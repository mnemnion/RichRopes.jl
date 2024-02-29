module RichRopes

import Unicode: graphemes

"""
    AbstractRope <: AbstractString

Supertype for Rope data structures.
"""
abstract type AbstractRope <: AbstractString end


const LEAF_SIZE = 128  # Smaller than usual, two cache lines?

struct RichRope{S<:AbstractString} <: AbstractRope
    sizeof::Int     # In bytes / codeunits
    # depth::Int      # Depth of tree at Rope node
    length::Int     # Number of codepoints
    grapheme::Int  # Number of graphemes
    linenum::Int       # Number of '\n' under Rope node
    leaf::S         # A non-leaf node sets this to ""
end

function RichRope(s::AbstractString)
    readinrope(IOBuffer(s))
end

function readinrope(io::IO)
    reading = true
    remain::Union{Vector{UInt8,Nothing}} = nothing
    top = RichRope(0, 0, 0, 0, "")
    rope = top
    while reading
        bytes = read(io, 128)
        if bytes < 128 - l
            reading = false
        end
        if remain !== nothing
            v = vcat(remain, v)
        end
        s = String(v)
        len, g, nl, rem, valid = string_metrics(s)
        len += rope.length
        if !valid
            error("Invalid UTF-8 on line $len")
        end
        g += rope.grapheme
        nl += rope.linenum
        if rem > 0  # We have grapheme or codepoint overhang
            s = String(v[1:end-rem])
            remain = v[end-rem:end]
        end
        rope = concatenate(rope, RichRope(sizeof(s), len, g, ln, s))
    end
end

function string_metrics(s::String)
    nl, len = 0, 0
    for (idx, char) in s
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
    len -= 1  # Because:
    # Graphemes
    # To count graphemes correctly, we must always retain the last
    # grapheme for next time (until the end of the stream), because
    # in addition to malformed UTF-8 from clipping characters, we
    # might be between graphemes. A string can be a megabyte and one
    # grapheme, which we will handle correctly if inefficiently.
    #
    # TODO this is for correctness, but allocates a bunch of SubStrings.
    # we'll want to rewrite it to use the low-level functions.
    g = 0
    last = nothing
    for glyph in graphemes(s)
        g += 1
        last = glyph
    end
    g -= 1
    if !malformed_end
        len -= 1
    end
    rem = ncodeunits(last)
    return len, g, nl, rem, true # end may be malformed but string itself is valid
end

end  # Module RichRopes
