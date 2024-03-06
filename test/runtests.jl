using RichRopes
using Test
using Aqua
import Base.Unicode: graphemes


RichRopes.leaf_size[] = 1 # rand(3:31)

println("Leaf Size: $(RichRopes.leaf_size[])")

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        # Aqua.test_all(RichRopes)
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
    @testset "Concat / * / == " begin
        for i in 1:10
            ref = "aδ∇🍆h"^i
            rope = readinrope(ref)
           @test rope * rope == ref * ref
        end
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

end
