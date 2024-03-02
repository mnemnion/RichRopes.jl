module RichRopes

export RichRope, AbstractRope, readinrope

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
    function RichRope(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S,
                      left::RichRope{S},
                      right::RichRope{S}) where {S<:AbstractString}
        new{S,RichRope{S}}(sizeof, depth, length, grapheme, linenum, leaf, left, right)
    end
    function RichRope(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S) where {S<:AbstractString}
        new{S,Nothing}(sizeof, depth, length, grapheme, linenum, leaf, nothing,nothing)
    end
    function RichRope(s,d,l,g,ln,str::S,left::Nothing,right::Nothing) where {S<:AbstractString}
        new{S,Nothing}(s,d,l,g,ln,str,left,right)
    end
end

function RichRope(s::AbstractString)
    readinrope(IOBuffer(s))
end

function RichRope(io::IO)
    readinrope(io)
end

# Builder methods

readinrope(s::AbstractString, leafsize=LEAF_SIZE) = readinrope(IOBuffer(s), leafsize)

function readinrope(io::IO, leafsize=LEAF_SIZE)
    reading = true
    remain::Vector{UInt8} = UInt8[]
    leaves = RichRope{String}[]
    while reading
        v1 = read(io, leafsize)
        if length(v1) < leafsize
            reading = false
        end
        v = vcat(remain, v1)
        s = String(v)
        len, g, nl, rem, valid = string_metrics(s)
        if !valid
            error("Invalid UTF-8 on line $len")
        end
        if reading # There will always be a remainder, for grapheme integrity
            s_tmp = s[1:end-rem]
            remain = codeunits(s[end-rem+1:end])
            s = s_tmp
        elseif !isempty(s)
            g += 1
            len += 1
            if s[end] == '\n'
                nl += 1
            end
        end
        # print(sizeof(s), " ")
        push!(leaves, RichRope(sizeof(s), 0, len, g, nl, s))
    end
    if length(leaves) == 1
        return only(leaves)
    end
    return mergeleaves(leaves)
end

function mergeleaves(leaves::Vector{RichRope{S}}) where {S<:AbstractString}
    tier = []
    # to get one-based line indexing, we handle the first two special-case.
    left, right = leaves[1], leaves[2]
    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum + 1
    push!(tier, RichRope(size, 1, len, g, nl, one(S), left, right))
    leaves = leaves[3:end]
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
    isempty(left) && return right || isempty(right) && return(left)
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

# Base methods

Base.:*(a::RichRope, b::RichRope) = concatenate(a, b)
# Favor the concrete type of the Rope
Base.:*(a::RichRope{S}, b::AbstractString) where {S} = concatenate(a, RichRope(convert(S,b)))
Base.:*(a::AbstractString, b::RichRope{S}) where {S} = concatenate(RichRope(convert(S,a)), b)

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

function Base.:(==)(a::RichRope, b::AbstractString)
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

Base.:(==)(a::AbstractString, b::RichRope) = b == a

Base.ncodeunits(rope::RichRope) = rope.sizeof
Base.sizeof(rope::RichRope) = rope.sizeof
Base.length(rope::RichRope) = rope.length

function Base.collect(rope::RichRope)
    chars = Char[]
    for c in rope
        push!(chars, c)
    end
    chars
end

Base.write(io::IO, rope::RichRope{S,Nothing}) where {S} = write(io, rope.leaf)
function Base.write(io::IO, rope::RichRope{S,RichRope{S}}) where {S}
    write(io, rope.left) + write(io, rope.right)
end

Base.print(io::IO, rope::RichRope) = (write(io, rope); return)

function Base.iterate(rope::RichRope)
    stack = RichRope[rope]
    if isleaf(rope)
        return rope.leaf[1], (stack, 1)
    end
    r = rope.left
    while !isleaf(r)
        push!(stack, r)
        r = r.left
    end
    push!(stack, r)
    return r.leaf[1], (stack, 1)
end

function Base.iterate(::RichRope, state)
    stack, i = state
    if !isleaf(stack[end])
        while !isleaf(stack[end])
            push!(stack, stack[end].left)
        end
        return stack[end].leaf[1], (stack, 1)
    end
    ind = nextind(stack[end].leaf, i)
    if ind > stack[end].sizeof
        pop!(stack) # drop the leaf
        if isempty(stack)
            return nothing
        end
        this = pop!(stack)
        push!(stack, this.right)
        while !isleaf(stack[end])
            push!(stack, stack[end].left)
        end
        return stack[end].leaf[1], (stack, 1)
    else
        return stack[end].leaf[ind], (stack, ind)
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

function Base.convert(::Type{String}, rope::RichRope)
   io = IOBuffer(sizehint=rope.sizeof)
   write(io, rope)
   String(take!(io))
end

function Base.convert(::Type{RichRope{S}}, rope::RichRope{R}) where {S<:AbstractString, R<:AbstractString}
    left = rope.left === nothing ? nothing : convert(RichRope{S}, rope.left)
    right = rope.right === nothing ? nothing : convert(RichRope{S}, rope.right)
    RichRope(rope.length, rope.depth, rope.length, rope.grapheme, rope.linenum, convert(S, rope.leaf), left, right)
end

Base.isempty(rope::RichRope{S,Nothing}) where {S} = rope.leaf == one(S)
Base.isempty(rope::RichRope{S,RichRope{S}}) where {S} = false

# Metrics

isleaf(a::RichRope{S,Nothing}) where {S} = true
isleaf(a::RichRope{S,RichRope{S}}) where {S} = false

function string_metrics(s::S) where {S<:AbstractString}
    if s == one(S)
        return 0, 0, 0, 0, true
    end
    nl, len = 0, 0
    for (idx, char) in pairs(s)
        if isvalid(char)
            len += 1
        elseif idx + ncodeunits(char) != ncodeunits(s)
            # We won't use most of the values so placeholders are fine
            return 0, 0, nl, 0, false
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
    g = -1
    last = nothing
    for glyph in graphemes(s)
        g += 1
        last = glyph
    end
    len -= length(last)
    rem = ncodeunits(last)
    if last == "\n"
        nl -= 1
    end
    return len, g, nl, rem, true # end may be malformed but string itself is valid
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

end  # Module RichRopes
