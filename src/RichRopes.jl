module RichRopes

export RichRope, AbstractRope, readinrope, cleave, delete, splice

import Unicode: graphemes

"""
    AbstractRope <: AbstractString

Supertype for Rope data structures.
"""
abstract type AbstractRope{S<:AbstractString} <: AbstractString end

#  Smaller than usual, four cache lines?
#  Configurable in any case
const LEAF_SIZE = 256

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

function readinrope(s::AbstractString, leafsize=LEAF_SIZE)
    if sizeof(s) < leafsize
        stringtoleaf(s)
    else
        readinrope(IOBuffer(s), leafsize)
    end
end

function readinrope(io::IO, leafsize=LEAF_SIZE)
    reading = true
    remain::Vector{UInt8} = UInt8[]
    leaves = RichRope{String,Nothing}[]
    while reading
        v1 = read(io, leafsize)
        if length(v1) < leafsize
            reading = false
        end
        v = vcat(remain, v1)
        s = String(copy(v))
        len, g, nl, last, valid = string_metrics(s)
        if !valid
            error("Invalid UTF-8 on line $len")
        end
        if reading # There will always be a remainder, for grapheme integrity
            len -= length(last)
            g -= 1
            if last == "\n"
                nl -= 1
            end
            resize!(v, length(v) - ncodeunits(last))
            remain = collect(codeunits(last))
            s = String(v)
        end
        rope = RichRope(sizeof(s), 0, len, g, nl, s)

        push!(leaves, rope)
    end
    if length(leaves) == 1
        return only(leaves)
    end
    return mergeleaves(leaves)
end

function mergeleaves(leaves::Vector{RichRope{S,Nothing}}) where {S}
    tier = []
    # to get one-based line indexing, we handle the first two special-case.
    left, right = leaves[1], leaves[2]
    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum + 1
    push!(tier, RichRope(size, 1, len, g, nl, one(S), left, right))
    leaves = @views leaves[3:end]
    crunching = true
    while crunching
        for i in 1:2:length(leaves)
            if i < length(leaves)
                left, right = leaves[i], leaves[i+1]
                push!(tier, concatenate(left, right))
            else
                push!(tier, leaves[end])
            end
        end
        if length(tier) == 1
            crunching = false
        else
            leaves = tier
            tier = []
        end
    end
    return only(tier)
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

function stringtoleaf(s::S) where {S<:AbstractString}
    len, g, nl, _, valid = string_metrics(s)
    if !valid
        error("invalid UTF-8 at index $len")
    end
    RichRope(sizeof(s), 0, len, g, nl, s)
end
stringtoleaf(s::RichRope) = s

function collectleaves(rope::RichRope{S,RichRope{S}},
                       leaves::Vector{RichRope{S,Nothing}}=RichRope{S,Nothing}[]) where S<:AbstractString
    collectleaves(rope.left, leaves)
    collectleaves(rope.right, leaves)
end

function collectleaves(rope::RichRope{S,Nothing}, leaves::Vector{RichRope{S,Nothing}}=RichRope{S,Nothing}[]) where S<:AbstractString
    push!(leaves,rope)
end
# Interface

function cleave(rope::RichRope{S,Nothing}, index::Integer)  where {S<:AbstractString}
    @boundscheck 0 < index ≤ length(rope) || throw(BoundsError(rope, index))
    left, right = @inbounds _cutstring(rope.leaf, index)::Tuple{S,S}
    return stringtoleaf(left), stringtoleaf(right)
end

"""
    cleave(rope::RichRope, index::Integer)

Return two ropes created by splitting `rope` at `index`.
"""
function cleave(rope::RichRope{S,RichRope{S}}, index::Integer) where {S}
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
removed.
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
    if l ≥ sizeof(s)
        return s, one(S)
    end
    r = nextind(s,l)
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

# Base methods

Base.:*(a::RichRope, b::RichRope) = concatenate(a, b)
# Favor the concrete type of the Rope
Base.:*(a::RichRope{S}, b::AbstractString) where {S} = concatenate(a, RichRope(convert(S,b)))
Base.:*(a::AbstractString, b::RichRope{S}) where {S} = concatenate(RichRope(convert(S,a)), b)
Base.one(::Type{RichRope{S}}) where {S} = stringtoleaf(one(S))

function Base.:^(a::RichRope, b::Integer)
    reps = [a for _ in 1:b]
    return mergeleaves(reps)
end

function Base.:(==)(a::RichRope, b::RichRope)
    lmatch = a.grapheme != b.grapheme ? false :
             a.length != b.length ? false :
             a.sizeof != b.sizeof ? false : true
    lmatch || return false

    same = true
    for (c1, c2) in zip(a, b)
        if c1 != c2
            same = false
            break
        end
    end
    return same
end

function Base.:(==)(a::RichRope{S,RichRope{S}} where {S}, b::AbstractString)
    a.sizeof != sizeof(b) && return false

    same = true
    for (c1, c2) in zip(a, b)
        if c1 != c2
            same = false
            break
        end
    end
    return same
end

Base.:(==)(a::RichRope{S,Nothing} where {S}, b::AbstractString) = a.leaf == b

Base.:(==)(a::AbstractString, b::RichRope) = b == a

Base.ncodeunits(rope::RichRope) = rope.sizeof
Base.sizeof(rope::RichRope) = rope.sizeof
Base.length(rope::RichRope) = rope.length
Base.eltype(::Type{RichRope{S}}) where {S<:AbstractString} = eltype(S)

function Base.collect(rope::RichRope)
    chars = Char[]
    for c in rope
        push!(chars, c)
    end
    chars
end

function Base.getindex(rope::RichRope{S,RichRope{S}} where {S<:AbstractString}, index::Integer)
    if index <= rope.left.length
        getindex(rope.left, index)::Char
    else
        getindex(rope.right, index - rope.left.length)::Char
    end
end

function Base.getindex(rope::RichRope{S,Nothing} where {S<:AbstractString}, index::Integer)::Char
    rope.leaf[nthpoint(rope.leaf, index)]
end

function Base.getindex(rope::RichRope, range::UnitRange{<:Integer})
    _, tail = cleave(rope, range.start - 1)
    rest, _ = cleave(tail, 1 + range.stop - range.start)
    return rest
end

Base.eachindex(rope::RichRope) = 1:rope.length

function Base.codeunit(rope::RichRope{S,RichRope{S}} where {S<:AbstractString}, index::Integer)
    if index <= rope.left.sizeof
        codeunit(rope.left, index)::UInt8
    else
        codeunit(rope.right, index - rope.left.sizeof)::UInt8
    end
end

Base.codeunit(rope::RichRope{S,Nothing} where {S<:AbstractString}, index::Integer) = codeunit(rope.leaf, index)

Base.firstindex(rope::RichRope) =  1

Base.lastindex(rope::RichRope) = rope.length

# We always check validity building a string
Base.isvalid(::RichRope) = true
Base.isvalid(::RichRope, ::Integer) = true
Base.isascii(rope::RichRope) = rope.sizeof == rope.length

Base.write(io::IO, rope::RichRope{S,Nothing}) where {S} = write(io, rope.leaf)
function Base.write(io::IO, rope::RichRope{S,RichRope{S}}) where {S}
    write(io, rope.left) + write(io, rope.right)
end

Base.print(io::IO, rope::RichRope) = (write(io, rope); return)


# TODO make a struct to implement the iterator interface
function Base.iterate(rope::RichRope{S,RichRope{S}}) where {S<:AbstractString}
    stack = RichRope{S}[rope]
    r = rope.left::RichRope{S}
    while !isleaf(r)
        push!(stack, r)
        r = r.left::RichRope{S}
    end
    push!(stack, r)
    return r.leaf[1], (stack, 1)
end

function Base.iterate(::RichRope{S}, state::Tuple) where {S<:AbstractString}
    stack, i = state
    if !isleaf(stack[end])
        while !isleaf(stack[end])
            push!(stack, stack[end].left::RichRope{S})
        end
        return stack[end].leaf[1], (stack, 1)
    end
    ind = nextind(stack[end].leaf, i)
    if ind ≤ stack[end].sizeof
        return stack[end].leaf[ind], (stack, ind)
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
        return stack[end].leaf[1], (stack, 1)
    end
end

Base.iterate(rope::RichRope{S,Nothing} where {S}) = rope.leaf[1], 1

function Base.iterate(rope::RichRope{S,Nothing} where {S}, i::Integer)
    idx = nextind(rope.leaf, i)
    if idx > rope.sizeof
        return nothing
    else
        return rope.leaf[idx], idx
    end
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
    @boundscheck i > length(s) && throw(BoundsError("index out of bounds"))
    @inbounds nextind(s, 0, i)
end

function string_metrics(s::S) where {S<:AbstractString}
    if s == one(S)
        return 0, 0, 0, 0, true
    end
    nl, len = 0, 0
    for (idx, char) in pairs(s)
        if isvalid(char)
            len += 1
        elseif idx + ncodeunits(char) != ncodeunits(s) + 1
            # These values are for reporting errors, note that
            # "length" is instead the index of the error
            return idx, 0, nl, 0, false
        else
            len += 1
        end
        if char === '\n'
            nl += 1
        end
    end
    # Graphemes
    # To count graphemes correctly, we must always retain the last
    # grapheme for next time (until the end of the stream), because
    # in addition to malformed UTF-8 from clipping characters, we
    # might be between graphemes. A string can be a megabyte and one
    # grapheme, which we will handle correctly if inefficiently.
    #
    # TODO right now we care about correctness, but this allocates a bunch of SubStrings.
    # we'll want to rewrite it to use the low-level functions.
    # Note: this breaks long graphemes which it can't know about (because some are missing)
    # into smaller graphemes, backtracking at the end of a string, so it needs rewriting
    # using low-level grapheme break checks.
    g = 0
    last = nothing
    for glyph in graphemes(s)
        g += 1
        last = glyph
    end
    return len, g, nl, last, true # end may be malformed but string itself is valid
end

# Skipping the ones we don't use...
const Fib = Int[2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,
                2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418,
                317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465,
                14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296,
                433494437, 701408733, 1134903170, 1836311903, 2971215073, 4807526976,
                7778742049, 12586269025, 20365011074, 32951280099, 53316291173,
                86267571272, 139583862445, 225851433717, 365435296162, 591286729879,
                956722026041, 1548008755920, 2504730781961, 4052739537881,
                6557470319842, 10610209857723, 17167680177565]

end  # Module RichRopes
