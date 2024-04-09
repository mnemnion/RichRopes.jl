module RichRopes

export RichRope, AbstractRope, readinrope, cleave, delete, splice, rebuild, leaves, cursor

import AbstractTrees:  children, childtype, ischild, nodevalue, print_tree, printnode
import Base.Unicode: GraphemeIterator, isgraphemebreak!
import Unicode: Unicode, graphemes

using StringViews

# Compatibility shim

if VERSION ≤ v"1.9-"
    splat(f) = a -> f(a...)
end


"""
    AbstractRope <: AbstractString

Supertype for Rope data structures.
"""
abstract type AbstractRope{S<:AbstractString} <: AbstractString end

#  Smaller than usual, four cache lines?
#  Configurable in any case
const leaf_size = Ref(256)

struct RichRope{S<:AbstractString, T<:Union{AbstractRope{S},Nothing}} <: AbstractRope{S}
    sizeof::Int     # In bytes / codeunits
    depth::Int      # Depth of tree at Rope node
    length::Int     # Number of codepoints
    grapheme::Int   # Number of graphemes
    linenum::Int    # Number of '\n' under Rope node
    leaf::S         # A non-leaf node sets this to ""
    left::T         # Left child (nothing for leaf)
    right::T        # Right child (nothing for leaf)
    function RichRope(size::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S,
                      left::RichRope{S},
                      right::RichRope{S}) where {S<:AbstractString}
        new{S,RichRope{S}}(size, depth, length, grapheme, linenum, leaf, left, right)
    end
    function RichRope(size::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S) where {S<:AbstractString}
        new{S,Nothing}(size, depth, length, grapheme, linenum, leaf, nothing,nothing)
    end
    function RichRope(size::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S,
                      ::Nothing,
                      ::Nothing) where {S<:AbstractString}
        new{S,Nothing}(size, depth, length, grapheme, linenum, leaf, nothing,nothing)
    end
end

function RichRope(s::AbstractString)
    readinrope(s)
end

function RichRope(io::IO)
    readinrope(io)
end

# Builder methods

function readinrope(s::AbstractString, leafsize::Integer=leaf_size[])
    if sizeof(s) < leafsize
        stringtoleaf(s)
    else
        readinrope(IOBuffer(s), leafsize)
    end
end

function readinrope(io::IO, leafsize::Integer=leaf_size[])
    reading = true
    remain::Vector{UInt8} = UInt8[]
    leaves = RichRope{String,Nothing}[]
    first = true
    while reading
        l = length(remain)
        v = append!(remain, read(io, leafsize))
        if length(v) - l < leafsize
            reading = false
        end
        s = StringView(v)
        len, g, nl, last, valid, bad_end = string_metrics(s)
        if first
            nl += 1
            first = false
        end
        if !valid
            error("invalid UTF-8 on line $nl")
        end
        if reading # There will always be a remainder, for grapheme integrity
            len -= length(last)
            g -= bad_end ? 2 : 1
            nl -= count('\n', last)
            remain = collect(codeunits(last))
            resize!(v, length(v) - ncodeunits(last))
            s = String(v)
        end
        # This can leave an empty string (think Zalgotext), which we don't need
        if !isempty(s)
            rope = RichRope(sizeof(s), 0, len, g, nl, String(s))
            push!(leaves, rope)
        end
    end
    if length(leaves) == 1
        return only(leaves)
    end
    return mergeleaves(leaves)
end

readinrope(rope::RichRope) = rope

function mergeleaves(leaves)
    if length(leaves) == 1
        return only(leaves)
    elseif length(leaves) == 2
        return leaves[1] * leaves[2]
    else
        mid = length(leaves) ÷ 2
        return @views mergeleaves(leaves[begin:mid]) * mergeleaves(leaves[mid+1:end])
    end
end

function concatenate(left::RichRope{S}, right::RichRope{S}) where {S}
    isempty(left) && return right
    isempty(right) && return left

    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum
    depth = max(left.depth, right.depth) + 1
    RichRope(size, depth, len, g, nl, one(S), left, right)
end

function concatenate(left::RichRope{S}, right::RichRope{R}) where {S,R}
    # Left side seems like the sensible bias
    concatenate(left, convert(RichRope{S}, right))
end

stringtoleaf(s::RichRope) = s
function stringtoleaf(s::S) where {S<:AbstractString}
    len, g, nl, _, valid, bad_end = string_metrics(s)
    if !valid || bad_end
        error("invalid UTF-8 at index $len")
    end
    RichRope(sizeof(s), 0, len, g, nl, s)
end

function collectleaves(rope::RichRope{S,RichRope{S}},
                       leaves::Vector{RichRope{S,Nothing}}=RichRope{S,Nothing}[]) where S<:AbstractString
    collectleaves(rope.left, leaves)
    collectleaves(rope.right, leaves)
end

function collectleaves(rope::RichRope{S,Nothing}, leaves::Vector{RichRope{S,Nothing}}=RichRope{S,Nothing}[]) where S<:AbstractString
    push!(leaves,rope)
end

"""
    compactleaves!(leaves::Vector{RichRope{<:AbstractString,Nothing}}, leafsize=leaf_size[])

Compact a `Vector` of `RichRope` leaves, such that the returned Vector contains
leaves of approximately `leafsize`.

Note: this will never make a leaf smaller through splitting, so it cannot be used to
reduce the leaf size of a `RichRope`.  Presuming that the rope was created originally
of `leaf_size[]` leaves, the leaves resulting from `compactleaves!` will never be
larger than 1.75X `leaf_size[]`.

This is called by [`rebuild`](@ref).
"""
function compactleaves!(leaves::Vector{RichRope{S,Nothing}}, ls::Integer=leaf_size[]) where {S<:AbstractString}
    maxleaf = ls + (ls ÷ 4)
    minleaf = ls - (ls ÷ 4)
    io = IOBuffer()
    deadidx = length(leaves) ≤ typemax(UInt8) ? UInt8[] :
              length(leaves) ≤ typemax(UInt16) ? UInt16[] : Int[]  # above 64KiB we don't care lol
    bytes = 0
    for i in firstindex(leaves):lastindex(leaves)
        leaf = leaves[i]
        if sizeof(leaf) ≤ minleaf || bytes > 0
            bytes += write(io, leaf.leaf)
            if bytes ≥ maxleaf || i == lastindex(leaves)
                leaves[i] = stringtoleaf(S(String(take!(io))))
                bytes = 0
            else
                push!(deadidx, i)
            end
        end
    end
    if !isempty(deadidx)
        deleteat!(leaves, deadidx)
    end
    return leaves
end

"""
    rebuild(rope::RichRope{S,RichRope{S}} where {S}, leafsize::Integer=leaf_size[])

Rebuild a `RichRope`.  This will compact adjacent small leaves into correctly-sized
ones, and balance the tree.  Note that a value of `leafsize` smaller than that used
to create the rope will not result in any leaf nodes being split, but will affect the
target length of leaves which are compacted (see [`compactleaves!`](@ref)).
"""
function rebuild(rope::RichRope{S,RichRope{S}} where {S}, leafsize::Integer=leaf_size[])
    compactleaves!(collectleaves(rope), leafsize) |> mergeleaves
end
rebuild(rope::RichRope{<:AbstractString,Nothing}, ::Integer=0) = rope

# Interface

function cleave(rope::RichRope{S,Nothing}, index::Integer)  where {S<:AbstractString}
    @boundscheck 0 ≤ index ≤ length(rope) || throw(BoundsError(rope, index))
    if index == 0
        return one(RichRope{S}), rope
    elseif index == length(rope)
        return rope, one(RichRope{S})
    end
    if isascii(rope)
        left = stringtoleaf(@inbounds rope.leaf[begin:index])
        right = stringtoleaf(@inbounds rope.leaf[index+1:end])
        return left, right
    end
    left, right = @inbounds _cutstring(rope.leaf, index)::Tuple{S,S}
    return stringtoleaf(left), stringtoleaf(right)
end

"""
    cleave(rope::RichRope, index::Integer)

Return two ropes created by splitting `rope` at `index`, as indexed by
codepoints.
"""
function cleave(rope::RichRope{S,RichRope{S}}, index::Integer) where {S}
    @boundscheck 0 ≤ index ≤ length(rope) || throw(BoundsError(rope, index))
    if index == 0
        return one(RichRope{S}), rope
    elseif index == rope.length
        return rope, one(RichRope{S})
    elseif index < rope.left.length
        left, right = cleave(rope.left, index)
        return left, right * rope.right
    elseif index > rope.left.length
        left, right = cleave(rope.right, index - rope.left.length)
        return rope.left * left, right
    else  # right down the middle
        return rope.left, rope.right
    end
end

"""
    cleave(rope::RichRope, range::UnitRange{<:Integer})

Return two ropes which are the head and tail of `rope` with `range`
removed.  Range is measured in codepoints.
"""
function cleave(rope::RichRope, range::UnitRange{<:Integer})
    @boundscheck 0 < range.start || throw(BoundsError(rope, range.start))
    @boundscheck 0 ≤ range.stop ≤ length(rope) || throw(BoundsError(rope, range.stop))
    head, rest = cleave(rope, range.start - 1)
    _, tail = cleave(rest, 1 + range.stop - range.start)
    return head, tail
end

"""
    delete(rope::RichRope, range::UnitRange{<:Integer})

Return a rope with the characters in `range` deleted.
"""
function delete(rope::RichRope, range::UnitRange{<:Integer})
    range.stop < range.start && return rope
    left, right = cleave(rope, range)
    return left * right
end

"""
    delete(rope::RichRope, index::Integer, n::Integer)

Return a rope with `n` characters removed, beginning with the character
at `index`.
"""
function delete(rope::RichRope, index::Integer, n::Integer)
    delete(rope, index:(index + n - 1))
end

function _cutstring(s::S, i::Integer) where {S<:AbstractString}
    l = nthpoint(s, i)  # Checks bounds
    r = nextind(s, l)
    return s[begin:l], s[r:end]
end

"""
    splice(rope::RichRope, at::Union{Integer,UnitRange{<:Integer}}, str::AbstractString)

Return a new rope with `str` spliced into `rope`. If `at` is an `Integer`, the
rope is split at `at`, if `at` is a range, that range is removed.

# Example

```jldoctest
julia> String(splice(RichRope("aabbcc"), 3:4, "!!"))
"aa!!cc"

julia> String(splice(RichRope("aabbcc"), 3, "!!"))
"aab!!bcc"
```
"""
function splice(rope::RichRope, at::Union{Integer,UnitRange{<:Integer}}, str::AbstractString)
    left, right = cleave(rope, at)
    return left * str * right
end

struct LeafIterator{S}
    s::Vector{RichRope{S}}
    left::Vector{Bool}
end

Base.eltype(::Union{Type{LeafIterator{S}},LeafIterator{S}}) where {S} = RichRope{S,Nothing}
Base.IteratorSize(::Union{Type{LeafIterator},LeafIterator}) = Base.SizeUnknown()
Base.isdone(iter::LeafIterator) = isempty(iter.s)

function leaves(rope::RichRope{S}) where {S}
    s, left = [rope], [false]
    sizehint!(s, rope.depth)
    sizehint!(left, rope.depth)
    iter = LeafIterator{S}(s, left)
    while !isleaf(iter.s[end])
        push!(iter.s, iter.s[end].left)
        push!(iter.left, true)
    end
    return iter
end

function Base.iterate(iter::LeafIterator{S}, i::Int=0) where {S}
    if isempty(iter.s)
        return nothing
    end
    thisleaf = pop!(iter.s)::RichRope{S,Nothing}
    left = pop!(iter.left)
    if isempty(iter.s)
        return thisleaf, i + 1
    end
    while !left
        pop!(iter.s)
        left = pop!(iter.left)
        if isempty(iter.s)
            return thisleaf, i + 1
        end
    end
    push!(iter.s, iter.s[end].right)
    push!(iter.left, false)
    while !isleaf(iter.s[end])
        push!(iter.s, iter.s[end].left)
        push!(iter.left, true)
    end
    return thisleaf, i + 1
end


mutable struct RichRopeGraphemeIterator{S}
    leaves::LeafIterator{S}
    graphemes::GraphemeIterator{S}
    gstate::Tuple{Int,Int}
    length::Int
end

Base.IteratorSize(::Type{RichRopeGraphemeIterator}) = Base.HasLength()
Base.eltype(::Type{RichRopeGraphemeIterator{S}}) where {S} = SubString{S}
Base.eltype(::Type{RichRopeGraphemeIterator{SubString{S}}}) where {S} = SubString{S}
Base.length(giter::RichRopeGraphemeIterator) = giter.length

function Unicode.graphemes(rope::RichRope)
    leafiter = leaves(rope)
    leaf, = iterate(leafiter)
    graphiter = graphemes(leaf.leaf)
    RichRopeGraphemeIterator(leafiter, graphiter, (0,1), rope.grapheme)
end

Base.Unicode.graphemes(rope::RichRope) = Unicode.graphemes(rope)

function Base.iterate(g::RichRopeGraphemeIterator, i=0)
    next = iterate(g.graphemes, g.gstate)
    if next !== nothing
        g.gstate = next[2]
        return next[1], i + 1
    end
    if isempty(g.leaves)
        return nothing
    end
    leaf, = iterate(g.leaves)
    g.graphemes = graphemes(leaf.leaf)
    next = iterate(g.graphemes, (0,1))
    if next !== nothing
        g.gstate = next[2]
        return next[1], i + 1
    end
end

mutable struct RopeCharCursor{S<:AbstractString}
    stack::Vector{Union{RichRope{S,RichRope{S}},RichRope{S,Nothing}}}
    left::Vector{Bool}
    count::Int
    cursor::Int
end

Base.eltype(::Union{Type{RopeCharCursor{S}},RopeCharCursor{S}}) where {S} = Pair{Int64,eltype(S)}
Base.IteratorSize(::Union{Type{RopeCharCursor},RopeCharCursor}) = Base.HasLength()
Base.length(iter::RopeCharCursor) = isempty(iter.stack) ? 0 : iter.stack[1].length - iter.cursor - 1
Base.isdone(iter::RopeCharCursor) = isempty(iter.stack)

"""
    cursor(rope::RichRope, i::Integer=1)

Return an iterator of `Pair{Int,Char}` starting from the `i`th character.
"""
function cursor(rope::RichRope{S}, i::Integer=1) where {S}
    iter = RopeCharCursor(Union{RichRope{S,RichRope{S}},RichRope{S,Nothing}}[rope], [false], 0, i - 1)
    while !isleaf(iter.stack[end])
        r = iter.stack[end]
        if i > r.left.length
            push!(iter.left, false)
            push!(iter.stack, r.right)
            i -= r.left.length
        else
            push!(iter.left, true)
            push!(iter.stack, r.left)
        end
        iter.count = nextind(iter.stack[end].leaf, 0, i-1)
    end
    return iter
end

function Base.iterate(iter::RopeCharCursor{S}, idx::Integer=1) where {S<:AbstractString}
    isempty(iter.stack) && return nothing
    iter.cursor += 1
    iter.cursor > length(iter.stack[1]) && return nothing
    idx += 1
    stack = iter.stack
    T::Type = Union{RichRope{S,RichRope{S}},RichRope{S,Nothing}}
    ind = nextind((stack[end]::RichRope{S,Nothing}).leaf, iter.count)
    if ind ≤ stack[end].sizeof
        iter.count = ind
        return iter.cursor => (stack[end]::RichRope{S,Nothing}).leaf[ind], idx
    else
        this = pop!(stack)::T
        isempty(stack) && return nothing
        left = pop!(iter.left)
        while !left
            this = pop!(stack)::T
            left = pop!(iter.left)
            isempty(stack) && return nothing
        end
        push!(stack, (stack[end]::T).right::T)
        push!(iter.left, false)
        while !(stack[end] isa RichRope{S,Nothing})
            push!(stack, (stack[end]::T).left::T)
            push!(iter.left, true)
        end
        iter.count = 1
        return iter.cursor => (stack[end]::RichRope{S,Nothing})[1], idx
    end
end

# Indexing
#
# Base-native index methods are in the Base methods section

"""
    grapheme(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}

Return the `index`th grapheme of the rope.
"""
function grapheme(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}
    if index <= rope.left.grapheme
        grapheme(rope.left, index)
    else
        grapheme(rope.right, index - rope.left.grapheme)
    end
end

function grapheme(rope::RichRope{S,Nothing}, index::Integer) where {S<:AbstractString}
    nthgrapheme(rope.leaf, index)
end

"""
    graphemeindex(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}

Return the native (character) index of the `index`th grapheme.
"""
function graphemeindex(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}
    if index <= rope.left.grapheme
        graphemeindex(rope.left, index)
    else
        rope.left.length + graphemeindex(rope.right, index - rope.left.grapheme)
    end
end

function graphemeindex(rope::RichRope{S,Nothing}, index::Integer) where {S<:AbstractString}
    # No need for bounds check, will throw clean if not found
    nthgraphemeindex(rope.leaf, index)
end

#=  "codeunitindex

I'm going to wait until I implement the integration with StringUnits before returning to
this.

It's trickier than it looks, because everything else expects a character index, so it
has to handle "sub-unit" values.  By throwing an error, yes, but the circumstances of
this are easier to test with the StringUnits added.

"""
    codeunitindex(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}

Returns the character index (native index unit of `RichRopes`) for a given codeunit
offset (usual native index for Strings).
"""
function codeunitindex(rope::RichRope{S,RichRope{S}}, index::Integer) where {S<:AbstractString}
    if index <= rope.left.sizeof
        codeunitindex(rope.left, index)
    else
        rope.left.length + codeunitindex(rope.right, index - rope.left.sizeof)
    end
end

function codeunitindex(rope::RichRope{S,Nothing}, index::Integer) where {S<:AbstractString}
    point = 0
    for idx in eachindex(rope.leaf)
        point += 1
        if idx ≥ index
            return point
        end
    end
end

=#

# AbstractTrees interface

children(rope::RichRope{S,RichRope{S}} where {S}) = rope.left, rope.right
childtype(::Type{RichRope{S,C}}) where {S,C} = C
ischild(r1::RichRope, r2::RichRope{S,RichRope{S}} where {S}) = r2.left ≡ r1 || r2.right ≡ r1
ischild(r1::RichRope, r2::RichRope{S,Nothing} where {S}) = false
print_tree(rope::RichRope) = print_tree(rope, maxdepth=rope.depth)

function printnode(io::IO, rope::RichRope{S,RichRope{S}} where {S})
    str = ""
    if rope.length != rope.sizeof
        str *= "sizeof: $(rope.sizeof) length: $(rope.length)"
    else
        str *= "length: $(rope.length)"
    end
    if rope.length != rope.grapheme
        str *= " graphemes: $(rope.grapheme)"
    end
    printstyled(io, str, " lines: $(rope.linenum)", color=:light_black)
end


# Base methods

const ConcatFollow = Vararg{Union{AbstractChar,AbstractString,RichRope}}

Base.:*(a::RichRope{S}, b::RichRope{R}) where {S,R} = concatenate(a, b)
Base.:*(a::RichRope{S}, b::RichRope{R}, c::ConcatFollow) where {S,R} = *(a * b, c...)
# Favor the concrete type of the Rope
Base.:*(a::RichRope{S}, b::AbstractString) where {S} = concatenate(a, RichRope(convert(S, b)))
Base.:*(a::RichRope{S}, b::AbstractChar) where {S} = concatenate(a, RichRope(convert(S, string(b))))
Base.:*(a::AbstractString, b::RichRope{S}) where {S} = concatenate(RichRope(convert(S, a)), b)
Base.:*(a::AbstractChar, b::RichRope{S}) where {S} = concatenate(RichRope(convert(S, string(a))), b)
Base.:*(a::RichRope{S}, b::Union{AbstractChar,AbstractString}, c::ConcatFollow) where {S} = *(a * b, c...)
Base.:*(a::Union{AbstractChar,AbstractString}, b::RichRope{S}, c::ConcatFollow) where {S} = *(a * b, c...)
Base.one(::Union{RichRope{S},Type{RichRope{S}}}) where {S} = stringtoleaf(one(S))
Base.oneunit(::Union{RichRope{S},Type{RichRope{S}}}) where {S} = stringtoleaf(one(S))
Base.typemin(::Union{RichRope{S},Type{RichRope{S}}}) where {S} = stringtoleaf(typemin(S))

function Base.:^(a::RichRope, b::Integer)
    reps = [a for _ in 1:b]
    return mergeleaves(reps)
end

function Base.:(==)(a::RichRope{S,RichRope{S}}, b::RichRope{S}) where {S}
    lmatch = a.grapheme != b.grapheme ? false :
             a.length != b.length ? false :
             a.sizeof != b.sizeof ? false : true
    lmatch || return false

    for (c1, c2) in zip(a, b)
        if c1 != c2
           return false
        end
    end
    return true
end

function Base.:(==)(a::RichRope{S,RichRope{S}}, b::R) where {S,R<:AbstractString}
    if ncodeunits(a) != ncodeunits(b)
        return false
    end

    for (c1, c2) in zip(a, b)
        if c1 != c2
            return false
        end
    end
    return true
end

Base.:(==)(a::RichRope{S,Nothing}, b::RichRope{S,Nothing}) where {S} = a.leaf == b.leaf

Base.:(==)(a::RichRope{S,Nothing}, b::R) where {S,R<:AbstractString} = a.leaf == b

Base.ncodeunits(rope::RichRope) = rope.sizeof
Base.sizeof(rope::RichRope) = rope.sizeof
Base.length(rope::RichRope) = rope.length
Base.eltype(::Type{RichRope{S}}) where {S<:AbstractString} = eltype(S)

function Base.collect(rope::RichRope)
    chars = Char[]
    for c in rope
        push!(chars, c)
    end
    return chars
end

function Base.nextind(rope::RichRope, i::Int, n::Int=1)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    i + n > length(rope) + 1 && throw(BoundsError(rope, i+n))
    return i + n
end

function Base.prevind(rope::RichRope, i::Int, n::Int=1)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    i - n < 0 && throw(BoundsError(rope, i-n))
    return i - n
end

Base.thisind(rope::RichRope, i::Int) = 0 ≤ i ≤ length(rope) + 1 ? i : throw(BoundsError(rope, i))

function Base.getindex(rope::RichRope{S,RichRope{S}} where {S<:AbstractString}, index::Integer)
    @boundscheck 0 < index ≤ length(rope) || throw(BoundsError(rope, index))
    if index ≤ rope.left.length
        @inbounds getindex(rope.left, index)::Char
    else
        @inbounds getindex(rope.right, index - rope.left.length)::Char
    end
end

function Base.getindex(rope::RichRope{S,Nothing} where {S<:AbstractString}, index::Integer)::Char
    rope.leaf[nthpoint(rope.leaf, index)]
end

function Base.getindex(rope::RichRope{S,RichRope{S}} where {S}, range::UnitRange{<:Integer})
    @boundscheck (0 < range.start && range.stop ≤ length(rope)) || throw(BoundsError(rope, range))
    if range.stop ≤ rope.left.length
        return @inbounds getindex(rope.left, range)
    elseif range.start > rope.left.length
        return @inbounds getindex(rope.right, (range) .- (rope.left.length))
    end
    _, tail = @inbounds cleave(rope, range.start - 1)
    rest, _ = @inbounds cleave(tail, 1 + range.stop - range.start)
    return rest
end

function Base.getindex(rope::RichRope{S,Nothing} where {S}, range::UnitRange{<:Integer})
    @boundscheck (0 < range.start && range.stop ≤ length(rope)) || throw(BoundsError(rope, range))
    start = @inbounds nthpoint(rope.leaf,range.start)
    stop = @inbounds nthpoint(rope.leaf,range.stop)
    return stringtoleaf(rope.leaf[start:stop])
end

function Base.view(rope::RichRope{S,RichRope{S}}, range::UnitRange{<:Integer}) where {S}
    getindex(rope, range)
end

function Base.view(rope::RichRope{S,Nothing}, range::UnitRange{<:Integer}) where {S}
    RichRope(SubString{S}(rope.leaf, range.start, range.stop))
end

Base.eachindex(rope::RichRope) = 1:rope.length

function Base.codeunit(rope::RichRope{S,RichRope{S}} where {S<:AbstractString}, index::Integer)
    if index <= rope.left.sizeof
        codeunit(rope.left, index)::UInt8
    else
        codeunit(rope.right, index - rope.left.sizeof)::UInt8
    end
end

Base.codeunit(::RichRope{S}) where {S} = codeunit(one(S))

Base.codeunit(rope::RichRope{S,Nothing} where {S<:AbstractString}, index::Integer) = codeunit(rope.leaf, index)

Base.firstindex(rope::RichRope) =  1

Base.lastindex(rope::RichRope) = rope.length

# We always check validity building a string
Base.isvalid(::RichRope) = true
Base.isvalid(rope::RichRope, i::Integer) = 0 < i ≤ rope.length
Base.isascii(rope::RichRope) = rope.sizeof == rope.length

function Base.findnext(testf::Function, s::RichRope, i::Integer)
    i = Int(i)
    z = length(s) + 1
    1 ≤ i ≤ z || throw(BoundsError(s, i))
    for (idx, char) in cursor(s, i)
        testf(char) && return idx
    end
    return nothing
end

function _findincursor(λ::Function, cur::RopeCharCursor)
    for (idx, char) in cur
        # @assert cur.stack[1][idx] == char  "actual $(cur.stack[1][idx]), char $char"
        λ(char) && return idx
    end
    return nothing
end

function Base.findprev(testf::Function, s::RichRope, i::Integer)
    i = Int(i)
    z = length(s) + 1
    0 ≤ i ≤ z || throw(BoundsError(s, i))
    i == z && return nothing
    while i >= 1
        testf(@inbounds s[i]) &&  return i
        i = @inbounds prevind(s, i)
    end
    return nothing
end

function Base._rsearch(s::RichRope,
    t::Union{AbstractString,AbstractChar,AbstractVector{<:Union{Int8,UInt8}}},
    i::Integer)
    idx = Base._rsearchindex(s, t, i)
    if isempty(t)
        idx:idx-1
    elseif idx > firstindex(s) - 1
        idx:(idx+length(t)-1)
    else
        return nothing
    end
end

function Base._searchindex(s::RichRope, t::AbstractString, i::Int)
    if isempty(t)
        return 1 <= i <= nextind(s, lastindex(s))::Int ? i :
               throw(BoundsError(s, i))
    end
    t1 = t[begin]
    trest = Iterators.drop(t,1)
    cur = cursor(s, i)
    eq = isequal(t1)
    while true
        i = _findincursor(eq, cur)
        if i === nothing
            return 0
        end
        ii = nextind(s, i)::Int
        matched = true
        count = 0
        for (left, right) in zip(SubString(s, ii), trest)
            count += 1
            matched &= left == right
            !matched && break
        end
        matched && count == length(trest) && return i
        i = ii
    end
end



Base._searchindex(s::RichRope, t::AbstractChar, i::Integer) = something(findnext(isequal(t), s, i), 0)

function Base._search(s::RichRope,
    t::Union{AbstractString,AbstractChar,AbstractVector{<:Union{Int8,UInt8}}},
    i::Integer)
    idx = Base._searchindex(s, t, i)
    if isempty(t)
        idx:idx-1
    elseif idx >= firstindex(s)
        idx:(idx+length(t)-1)
    else
        return nothing
    end
end

Base.write(io::IO, rope::RichRope{S,Nothing}) where {S} = write(io, rope.leaf)
function Base.write(io::IO, rope::RichRope{S,RichRope{S}}) where {S}
    write(io, rope.left) + write(io, rope.right)
end

Base.print(io::IO, rope::RichRope) = (write(io, rope); return)

# Iteration Interface

mutable struct RichRopeCharIterator{S<:AbstractString}
    stack::Vector{Union{RichRope{S,RichRope{S}},RichRope{S,Nothing}}}
    count::Int
end

function Base.iterate(rope::RichRope{S,RichRope{S}}) where {S<:AbstractString}
    iter = RichRopeCharIterator(Union{RichRope{S,RichRope{S}},RichRope{S,Nothing}}[rope], 1)
    r = rope.left::RichRope{S}
    while !isleaf(r)
        push!(iter.stack, r)
        r = r.left::RichRope{S}
    end
    push!(iter.stack, r)
    return r.leaf[1], iter
end

function Base.iterate(::RichRope{S}, iter::RichRopeCharIterator) where {S<:AbstractString}
    stack, i = iter.stack, iter.count
    ind = nextind(stack[end].leaf, i)
    if ind ≤ stack[end].sizeof
        iter.count = ind
        return stack[end].leaf[ind], iter
    else
        pop!(stack) # drop the leaf
        if isempty(stack)
            return nothing
        end
        this = pop!(stack)
        push!(stack, this.right::RichRope)
        while !isleaf(stack[end])
            push!(stack, stack[end].left::RichRope)
        end
        iter.count = 1
        return stack[end].leaf[1], iter
    end
end

function Base.iterate(rope::RichRope{S}, i::Integer) where {S}
    i > length(rope) && return nothing
    return rope[i], i + 1
end

function Base.iterate(rope::RichRope{S,Nothing} where {S}, i::Integer=1)
    i > length(rope) && return nothing
    return rope.leaf[i], nextind(rope.leaf, i)
end

function Base.convert(::Type{String}, rope::RichRope)
   io = IOBuffer(sizehint=rope.sizeof)
   write(io, rope)
   String(take!(io))
end

function Base.convert(::Type{RichRope{S}}, rope::RichRope{R}) where {S<:AbstractString, R<:AbstractString}
    S == R && return rope

    left = rope.left === nothing ? nothing : convert(RichRope{S}, rope.left)
    right = rope.right === nothing ? nothing : convert(RichRope{S}, rope.right)
    RichRope(rope.length, rope.depth, rope.length, rope.grapheme, rope.linenum, convert(S, rope.leaf), left, right)
end

Base.isempty(rope::RichRope{S,Nothing}) where {S} = rope.leaf == one(S)
# Note: this relies on proper construction, the interface will prevent empty branch
Base.isempty(rope::RichRope{S,RichRope{S}}) where {S} = false

function Base.summary(io::IO, rope::RichRope)
    prefix = isempty(rope) ? "empty" : string(length(rope), "-character")
    print(io, prefix, " ", typeof(rope))
end

function Base.show(io::IO, ::MIME"text/plain", r::RichRope{S}) where {S}
    childtype = r.left === nothing ? "Nothing" : "RichRope{$S}"
    print(io, "RichRope{$S, $childtype}")
    if isleaf(r)
        println(io, " (leaf) $(repr(r.leaf))")
    else
        println(io, "\n   codeunits: $(r.sizeof)")
        println(io, "   length: $(r.length)")
        println(io, "   graphemes: $(r.grapheme)")
        println(io, "   lines: $(r.linenum)")
        println(io, "   max depth: $(r.depth)")
    end
end

function Base.show(io::IO, rope::RichRope)
    print(io, '"')
    show_rope(io, rope)
    print(io, '"')
end

function show_rope(io::IO, rope::RichRope{S,RichRope{S}}) where {S}
    show_rope(io, rope.left)
    show_rope(io, rope.right)
end
show_rope(io::IO, rope::RichRope{S,Nothing}) where {S} = print(io, escape_string(rope.leaf))

# Metrics

isleaf(a::RichRope{S}) where {S} = a.left === nothing

@inline
function nthpoint(s::AbstractString, i::Integer)
    @boundscheck i > length(s) && throw(BoundsError(s, i))
    @inbounds nextind(s, 0, i)
end

function nthgrapheme(s::S, i::Integer) where {S<:AbstractString}
    c0 = eltype(S)(0x00000000)
    state = Ref{Int32}(0)
    n = 0
    i0 = 0
    for (idx, c) in pairs(s)
        if isgraphemebreak!(state, c0, c)
            n += 1
            if n == i
                i0 = idx
            elseif i0 > 0
                return @view s[i0:prevind(s,idx)]
            end
        end
        c0 = c
    end
    if i0 > 0
        return @view s[i0:lastindex(s)]
    end
    error(lazy"Can't return grapheme $i of $(length(graphemes(s)))-grapheme string")
end


"""
    nthgraphemeindex(s::S, i::Integer) where {S<:AbstractString}

Return the (character) index of the `i`th grapheme
"""
function nthgraphemeindex(s::S, i::Integer) where {S<:AbstractString}
    c0 = eltype(S)(0x00000000)
    state = Ref{Int32}(0)
    n = 0
    point = 0
    for c in s
        point += 1
        if isgraphemebreak!(state, c0, c)
            n += 1
            n == i && return point
        end
        c0 = c
    end
    error(lazy"No index for grapheme $i in $(length(graphemes(s)))-grapheme string")
end

# awful shim

emptystring(::Union{Type{SubString{S}},SubString{S}}) where {S} = one(SubString{S})
emptystring(::Union{Type{S},S}) where {S<:AbstractString} = typemin(S)

function string_metrics(s::S) where {S<:AbstractString}
    if s == emptystring(S)
        return 0, 0, 0, s, true, false
    end
    nl, len = 0, 0
    malformed_end = false
    for (idx, char) in pairs(s)
        if isvalid(char)
            len += 1
        elseif idx + ncodeunits(char) != ncodeunits(s) + 1
            # These values are for reporting errors, note that
            # "length" is instead the index of the error
            return idx, 0, nl, 0, false, false
        else
            len += 1
            malformed_end = true
        end
        if char === '\n'
            nl += 1
        end
    end
    g, bad, good = grapheme_metrics(s)
    if malformed_end
        clip = bad
    else
        clip = good
    end
    last = clip == 0 ? s : @view s[clip:end]
    return len, g, nl, last, true, malformed_end
end

function grapheme_metrics(s::S) where {S<:AbstractString}
    c0 = eltype(S)(0x00000000)
    state = Ref{Int32}(0)
    n = 0
    pre = 0
    now = 0
    for (i, c) in pairs(s)
        if isgraphemebreak!(state, c0, c)
            n += 1
            pre = now
            now = i
        end
        c0 = c
    end
    return n, pre, now
end

end  # Module RichRopes
