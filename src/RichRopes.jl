module RichRopes

export RichRope

import Unicode: graphemes

"""
    AbstractRope <: AbstractString

Supertype for Rope data structures.
"""
abstract type AbstractRope <: AbstractString end

#  Smaller than usual, two cache lines?
const LEAF_SIZE = 128  # +1 because we keep (minimum) 1 ASCII byte

struct RichRope{S<:AbstractString} <: AbstractRope
    sizeof::Int     # In bytes / codeunits
    depth::Int      # Depth of tree at Rope node
    length::Int     # Number of codepoints
    grapheme::Int   # Number of graphemes
    linenum::Int    # Number of '\n' under Rope node
    leaf::S         # A non-leaf node sets this to ""
    left::Union{RichRope,Nothing}   # Left child (nothing for leaf)
    right::Union{RichRope,Nothing}  # Right child (nothing for leaf)
    function RichRope(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S,
                      left::RichRope,
                      right::RichRope) where {S}
        new{S}(sizeof, depth, length, grapheme, linenum, leaf, left, right)
    end
    function RichRope(sizeof::Int,
                      depth::Int,
                      length::Int,
                      grapheme::Int,
                      linenum::Int,
                      leaf::S) where {S}
        new{S}(sizeof, depth, length, grapheme, linenum, leaf, nothing,nothing)
    end
end

function RichRope(s::AbstractString)
    readinrope(IOBuffer(s))
end

function RichRope(io::IO)
    readinrope(io)
end

function readinrope(io::IO)
    reading = true
    remain::Vector{UInt8} = UInt8[]
    leaves = RichRope[]
    while reading
        v1 = read(io, LEAF_SIZE)
        if length(v1) < LEAF_SIZE
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
        else
            g += 1
            len += 1
            if s[end] == '\n'
                nl += 1
            end
        end
        print(sizeof(s), " ")
        push!(leaves, RichRope(sizeof(s), 0, len, g, nl, s))
    end
    if length(leaves) == 1
        return only(leaves)
    end
    return mergeleaves(leaves)
end

function mergeleaves(leaves::Vector{RichRope})
    println()
    tier = RichRope[]
    # to get one-based line indexing, we handle the first two special-case.
    left, right = leaves[1], leaves[2]
    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum + 1
    push!(tier, RichRope(size, 1, len, g, nl, "", left, right))
    leaves = leaves[3:end]
    crunching = true
    while crunching
        println("leaf length: $(length(leaves))")
        for i in 1:2:length(leaves)
            if i < length(leaves)
                left, right = leaves[i], leaves[i+1]
                push!(tier, concatenate(left, right))
            end
        end
        if isodd(length(leaves))
            push!(tier, leaves[end])
        end  # TODO does this do weird things to the semantics of depth?
        if length(tier) == 1
            crunching = false
        else
            leaves = tier
            tier = RichRope[]
        end
    end
    return only(tier)
end

function concatenate(left::RichRope, right::RichRope)
    size = left.sizeof + right.sizeof
    len = left.length + right.length
    g = left.grapheme + right.grapheme
    nl = left.linenum + right.linenum
    depth = max(left.depth, right.depth) + 1
    RichRope(size, depth, len, g, nl, "", left, right)
end

function string_metrics(s::String)
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

function Base.show(io::IO, ::MIME"text/plain", r::RichRope)
   print(io, "RichRope(TBD)")
end


end  # Module RichRopes
