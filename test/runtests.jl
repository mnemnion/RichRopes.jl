using RichRopes
using Test
using Aqua

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        Aqua.test_all(RichRopes)
    end
    # Write your tests here.
end
