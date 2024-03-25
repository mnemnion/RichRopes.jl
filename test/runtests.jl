using RichRopes
import RichRopes: stringtoleaf, collectleaves
using Test
using Aqua
import Base.Unicode: graphemes


RichRopes.leaf_size[] = rand(1:32)

println("Leaf Size: $(RichRopes.leaf_size[])")

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        # Aqua.test_all(RichRopes)
    end
    @testset "Reading" begin
        buf = IOBuffer("rope a dope dope")
        @test readinrope(buf) == "rope a dope dope"
        buf = IOBuffer("invalid \xff\xff")
        @test_throws ErrorException readinrope(buf)
        buf = IOBuffer("rope a dope dope")
        @test readinrope(buf, 69) == "rope a dope dope"
    end
    @testset "Cleave" begin
        ref = "abc👨🏻‍🌾δe∇g🍆h"^100
        rope = readinrope(ref)
        for i in 1:length(rope)
            left, right = cleave(rope, i)
            @test String(left) * String(right) == ref
        end
    end
    @testset "Metrics" begin
        ref = "abcδ👨🏻‍🌾e∇g🍆h"
        size = sizeof(ref)
        len = length(ref)
        graph = length(graphemes(ref))
        for i in 1:10
            rope = readinrope(ref^i)
            @test sizeof(rope) == size * i
            @test length(rope) == len * i
            @test rope.length == length(String(rope))
            @test rope.grapheme == graph * i
        end
    end
    @testset "Farmer Bob" begin
        ref = "👨🏻‍🌾"^30
        graphs = length(graphemes(ref))
        split = rand(7:21)
        println("splits at $split")
        rope = readinrope(ref, split)
        @test rope.grapheme == graphs
    end
    @testset "Codeunits" begin
        ref = "aδ∇🍆h"
        w = ncodeunits(ref)
        rope = readinrope(ref^23)
        for i in 1:ncodeunits(rope)
            I = (i - 1) % w + 1
            @test codeunit(rope, i) == codeunit(ref, I)
        end
    end
    @testset "getindex" begin
        ref = "aδ∇🍆h"
        w = length(ref)
        rope = readinrope(ref^23)
        for i in eachindex(rope)
            I = (i - 1) % w + 1
            @test rope[i] == ref[nextind(ref, 0, I)]
        end
    end
    @testset "Concat / * / == / ^ " begin
        for i in 1:10
            ref = "aδ∇🍆h"^i
            rope = readinrope(ref)
           @test rope * rope == ref * ref
        end
        ss = stringtoleaf(@view "abcdef"[1:6])
        st = stringtoleaf("abcdef")
        @test ss * st == "abcdefabcdef"
        @test ss * st isa RichRope{SubString{String}, RichRope{SubString{String}, T} where T<:Union{Nothing, AbstractRope{SubString{String}}}}
        @test st * ss == "abcdefabcdef"
        @test st * ss isa RichRope{String, RichRope{String, T} where T<:Union{Nothing, AbstractRope{String}}}
        @test st ^ 6 == "abcdefabcdefabcdefabcdefabcdefabcdef"
    end

    @testset "Equality" begin
        ref = "abcδ👨🏻‍🌾e∇g🍆h"^rand(5:20)
        rope = readinrope(ref)
        @test rope == ref
        @test rope != ref * "!"
        r2 = readinrope(ref * "?")
        @test rope != r2
        r3 = readinrope(ref[1:prevind(ref, lastindex(ref))])
        @test rope != r3
    end
    @testset "delete" begin
        str = "a"^10 * "b"^10 * "c"^10
        rope = readinrope(str)
        @test delete(rope, 1:10) == "b"^10 * "c"^10
        @test delete(rope, 1, 10) == "b"^10 * "c"^10
        @test delete(rope, 11:20) == "a"^10 * "c"^10
        @test delete(rope, 21:30) == "a"^10 * "b"^10
        @test delete(rope, 1:30) == ""
        @test_throws BoundsError delete(rope, 0:30)
        @test_throws BoundsError delete(rope, 1:31)
    end
    @testset "delete tests GPT edition" begin
        # Test for correct deletion
        let original_rope = RichRope("Hello World"), range = 7:11
            expected_output = RichRope("Hello ")
            @test delete(original_rope, range) == expected_output
        end

        # Test for empty range
        let original_rope = RichRope("Hello World"), range = 7:6
            expected_output = original_rope
            @test delete(original_rope, range) == expected_output
        end

        # Test for range out of bounds
        let original_rope = RichRope("Hello World"), range = 1:12
            @test_throws BoundsError delete(original_rope, range)
        end

        # Test for invalid range type
        let original_rope = RichRope("Hello World"), range = 5.0:10.0
            @test_throws MethodError delete(original_rope, range)
        end

        # Test for deletion at the beginning of the rope
        let original_rope = RichRope("Hello World"), range = 1:5
            expected_output = RichRope(" World")
            @test delete(original_rope, range) == expected_output
        end

        # Test for deletion at the end of the rope
        let original_rope = RichRope("Hello World"), range = 7:11
            expected_output = RichRope("Hello ")
            @test delete(original_rope, range) == expected_output
        end

        # Test for deletion of a single character
        let original_rope = RichRope("Hello World"), range = 6:6
            expected_output = RichRope("HelloWorld")
            @test delete(original_rope, range) == expected_output
        end
    end

    @testset "Basics" begin
        @test one(RichRope{String}) == one(RichRope("")) == one(String) == ""
        @test oneunit(RichRope{String}) == oneunit(RichRope("")) == oneunit(String) == ""
        @test typemin(RichRope{String}) == typemin(RichRope("")) == typemin(String) == ""
    end
end
