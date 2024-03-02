using RichRopes
using Test
using Aqua

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
       # Aqua.test_all(RichRopes)
    end
    @testset "Cleave" begin
        ref = "abcdefg"^100
        rope = readinrope(ref, 28)
        for i in 1:sizeof(rope)
            left, right = cleave(rope, i)
            @test String(left) * String(right) == ref
        end
    end
    @testset "Concat / *" begin
        for i in 1:100
            ref = "a"^i
            rope = readinrope(ref, 7)
            @test rope * rope == ref * ref
        end
    end
end
