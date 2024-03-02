using RichRopes
using Test
using Aqua
import Unicode: graphemes

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
       # Aqua.test_all(RichRopes)
    end
    @testset "readinrope" begin
        ref = "abcδe∇g🍆h"^100
        rope = readinrope(ref, 27)
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
end
