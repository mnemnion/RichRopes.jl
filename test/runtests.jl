using RichRopes
using Test
using Aqua
import Unicode: graphemes

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
       # Aqua.test_all(RichRopes)
    end
    @testset "Cleave" begin
        ref = "abc👨🏻‍🌾δe∇g🍆h"^100
        rope = readinrope(ref, 28)
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
            rope = readinrope(ref^i, 17)
            @test sizeof(rope) == size * i
            @test length(rope) == len * i
            @test rope.grapheme == graph * i
        end
    end
    @testset "Concat / * / == " begin
        for i in 1:10
            ref = "aδ∇🍆h"^i
            rope = readinrope(ref, 7)
           @test rope * rope == ref * ref
        end
    end
    @testset "deletenchars" begin
        az = String(collect('a':'z'))
        ref = az^3
        rope = readinrope(ref, 11)
        clip1 = deletenchars(rope, 1, 26)
        @test clip1 == az^2
        clip2 = deletenchars(rope, 27, 26)
        @test clip2 == az^2
        clip3 = deletenchars(rope, 53, 26)
        @test clip3 == az^2
        @test_throws BoundsError deletenchars(rope, 53, 27)
    end
end
