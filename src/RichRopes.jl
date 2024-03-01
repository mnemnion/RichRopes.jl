module RichRopes

export RichRope, readinrope

import Unicode: graphemes

"""
    AbstractRope <: AbstractString

Supertype for Rope data structures.
"""
abstract type AbstractRope <: AbstractString end

#  Smaller than usual, four cache lines?
#  Configurable in any case
const LEAF_SIZE = 256

struct RichRope{S<:AbstractString} <: AbstractRope
    sizeof::Int     # In bytes / codeunits
    depth::Int      # Depth of tree at Rope node
    length::Int     # Number of codepoints
    grapheme::Int   # Number of graphemes
    linenum::Int    # Number of '\n' under Rope node
    leaf::S         # A non-leaf node sets this to ""
    left::Union{RichRope{S},Nothing}   # Left child (nothing for leaf)
    right::Union{RichRope{S},Nothing}  # Right child (nothing for leaf)
    function RichRope{S}(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S,
                      left::RichRope{S},
                      right::RichRope{S}) where {S<:AbstractString}
        new{S}(sizeof, depth, length, grapheme, linenum, leaf, left, right)
    end
    function RichRope{S}(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S) where {S<:AbstractString}
        new{S}(sizeof, depth, length, grapheme, linenum, leaf, nothing,nothing)
    end
    function RichRope{S}(s,d,l,g,ln,str::S,left::Nothing,right::Nothing) where {S<:AbstractString}
        new{S}(s,d,l,g,ln,str,left,right)
    end
end

function RichRope(s::AbstractString)
    readinrope(IOBuffer(s))
end

function RichRope(io::IO)
    readinrope(io)
end

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
        push!(leaves, RichRope{String}(sizeof(s), 0, len, g, nl, s))
    end
    if length(leaves) == 1
        return only(leaves)
    end
    return mergeleaves(leaves)
end

function mergeleaves(leaves::Vector{RichRope{S}}) where {S<:AbstractString}
    println()
    tier = RichRope{S}[]
    # to get one-based line indexing, we handle the first two special-case.
    left, right = leaves[1], leaves[2]
    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum + 1
    push!(tier, RichRope{S}(size, 1, len, g, nl, "", left, right))
    leaves = leaves[3:end]
    crunching = true
    while crunching
        # println("leaf length: $(length(leaves))")
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
            tier = RichRope{S}[]
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
    RichRope{S}(size, depth, len, g, nl, one(S), left, right)
end

function concatenate(left::RichRope{S}, right::RichRope{R}) where {S,R}
    # Left side seems like the sensible bias
    concatenate(left, convert(RichRope{S}, right))
end

isleaf(a::RichRope{S}) where {S} = a.leaf !== one(S)

Base.:*(a::RichRope, b::RichRope) = concatenate(a, b)
# Favor the concrete type of the Rope
Base.:*(a::RichRope{S}, b::AbstractString) where {S} = concatenate(a, RichRope(convert(S,b)))
Base.:*(a::AbstractString, b::RichRope{S}) where {S} = concatenate(RichRope(convert(S,a)), b)

Base.ncodeunits(rope::RichRope) = rope.sizeof

function Base.collect(rope::RichRope)
    chars = Char[]
    for c in rope
        push!(chars, c)
    end
    chars
end

function Base.:^(a::RichRope, b::Integer)
    reps = [a for _ in 1:b]
    return mergeleaves(reps)
end

function Base.write(io::IO, rope::RichRope)
    if isleaf(rope)
        write(io, rope.leaf)
    else
        write(io, rope.left)
        write(io, rope.right)
    end
end

Base.print(io::IO, rope::RichRope) = write(io, rope)

function Base.iterate(rope::RichRope)
    stack = [rope]
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
    if isleaf(rope)
        print(io, escape_string(rope.leaf))
    else
        show_rope(io, rope.left)
        show_rope(io, rope.right)
    end
    print(io, '"')
end

function show_rope(io::IO, rope::RichRope)
    if isleaf(rope)
        print(io, escape_string(rope.leaf))
    else
        show_rope(io, rope.left)
        show_rope(io, rope.right)
    end
end

function Base.convert(::Type{String}, rope::RichRope)
   io = IOBuffer(sizehint=rope.sizeof)
   write(io, rope)
   String(take!(io))
end

function Base.convert(::Type{RichRope{S}}, rope::RichRope{R}) where {S<:AbstractString,R<:AbstractString}
    left = rope.left === nothing ? nothing : convert(RichRope{S}, rope.left)
    right = rope.right === nothing ? nothing : convert(RichRope{S}, rope.right)
    RichRope{S}(rope.length, rope.depth, rope.length, rope.grapheme, rope.linenum, convert(S, rope.leaf), left, right)
end

Base.isempty(rope::RichRope{S}) where {S} = rope.left === nothing && rope.right === nothing && rope.leaf == one(S)

function string_metrics(s::AbstractString)
    if s == ""
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
   print(io, "RichRope{$S}")
   if r.leaf == one(S)
        println(io, "\n   codeunits: $(r.sizeof)")
        println(io, "   length: $(r.length)")
        println(io, "   graphemes: $(r.grapheme)")
        println(io, "   lines: $(r.linenum)")
        println(io, "   max depth:  $(r.depth)")
    else
        println(io, " (leaf) $(repr(r.leaf))")
    end
end


end  # Module RichRopes
