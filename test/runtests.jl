import AbstractTrees: NodeType, HasNodeType, children, childtype, ischild, nodevalue, print_tree, printnode
import Base.Unicode: graphemes
import RichRopes: collectleaves, mergeleaves, nthgrapheme, nthgraphemeindex, stringtoleaf

using Aqua
using RichRopes
using Test


RichRopes.leaf_size[] = rand(2:32)

println("Leaf Size: $(RichRopes.leaf_size[])")

@testset "RichRopes.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        # Aqua.test_all(RichRopes)
    end

    @testset "Reading" begin
        buf = IOBuffer("rope a dope dope")
        @test readinrope(buf) == "rope a dope dope"
        buf = IOBuffer("invalid \xff\xff")
        @test_throws "invalid UTF-8" readinrope(buf)
        @test_throws "invalid UTF-8" stringtoleaf("invalid \xff\xff")
        buf = IOBuffer("rope a dope dope")
        @test readinrope(buf, 69) == "rope a dope dope"
        buf = IOBuffer("rope a dope dope")
        @test RichRope(buf) == "rope a dope dope"
        rope = RichRope("rope a dope dope")
        @test stringtoleaf(rope) === rope
        @test readinrope(rope) === rope
    end

    @testset "Cleave" begin
        ref = "abc👨🏻‍🌾δe∇g🍆h"^100
        rope = readinrope(ref)
        for i in 1:length(rope)
            left, right = cleave(rope, i)
            @test String(left) * String(right) == ref
        end
        @test cleave(rope, 0) == ("", rope)
        @test cleave(rope, length(rope)) == (rope, "")
        r2 = stringtoleaf("abcdefghijklmnopqrstuvwxyz")
        for i in 0:length(r2)
            left, right = cleave(r2, i)
            @test String(left) * String(right) == r2
        end
    end

    @testset "Splice" begin
        sample = "aaaaaaaaaa"
        ref = sample^30
        rope = readinrope(ref)
        @test ((splice(rope, 4:4, "b") == rope)) == false
        @test ((splice(rope, 4:4, "b") == ref)) == false
        @test length(splice(rope, 4:4, "b")) == length(rope)
        @test splice(rope, 1:length(sample), "b"^length(sample)) == "b"^length(sample) * sample^29
        @test rope[1:51] == @views rope[1:51]
        r2 = stringtoleaf(sample)
        @test r2[1:5] == @views r2[1:5]
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
        @test rope[1:w] == ref
        @test rope[1:5w] == ref^5
    end

    @testset "Concat / * / == / ^ " begin
        for i in 1:10
            ref = "aδ∇🍆h"^i
            rope = readinrope(ref)
           @test rope * rope == ref * ref
           @test (rope * "a" * rope == ref * "b" * ref) == false
           @test rope * ref == ref * rope
        end
        ref = "abcdef"
        ss = stringtoleaf(@view ref[1:6])
        st = stringtoleaf(ref)
        @test ss * 'a' isa RichRope{SubString{String}}
        @test 'a' * ss isa RichRope{SubString{String}}
        @test ss * 'a' * ss isa RichRope{SubString{String}}
        @test 'a' * ss * 'a' * "a" isa RichRope{SubString{String}}
        @test "a" * ss * 'a' * 'a' * ss * ss isa RichRope{SubString{String}}
        @test (st * "!" == ss) == false
        @test (st == "abdcef") == false
        @test (st == stringtoleaf("abdcef")) == false
        @test ss * st == ref^2
        @test ss * st isa RichRope{SubString{String}}
        @test st * ss == "abcdefabcdef"
        @test st * ss isa RichRope{String}
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
        r4 = stringtoleaf(r3* "!")
        @test (rope == r4) == false
        @test (rope * "b" * rope == ref * "c" * ref) == false
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
    @testset "Reprs" begin
        ref = "abcδ👨🏻‍🌾e\n∇g🍆h"^20
        rope = readinrope(ref, 9)
        @test repr("text/plain", rope) == "RichRope{String, RichRope{String}}\n   codeunits: 620\n   length: 280\n   graphemes: 220\n   lines: 21\n   max depth: 6\n"
        io = IOBuffer()
        @test show(io, rope) === nothing
        @test String(take!(io)) == "\"abcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆habcδ👨🏻\\u200d🌾e\\n∇g🍆h\""
        @test repr("text/plain", stringtoleaf("text/plain")) == "RichRope{String, Nothing} (leaf) \"text/plain\"\n"
    end

    @testset "Iteration" begin
        @test collect("abcdefg") == collect(readinrope("abcdefg", 4))
        ref = "abcδ👨🏻‍🌾e\n∇g🍆h"^100
        rope = readinrope(ref)
        @test mergeleaves(collectleaves(rope)) == rope
    end

    @testset "Graphemes" begin
        ref = "👨🏻‍🌾"^30
        @test nthgrapheme(ref, 5) == "👨🏻‍🌾"
        @test nthgraphemeindex(ref, 5) ==  61
        @test_throws "Can't return grapheme" nthgrapheme(ref, 45)
        @test_throws "No index for grapheme" nthgraphemeindex(ref, 40)
    end

    @testset "AbstractTrees" begin
        ref = "abc"^60 * "abcδ👨🏻‍🌾e\n∇g🍆h"^100
        rope = readinrope(ref)
        @test children(rope) == (rope.left, rope.right)
        @test childtype(typeof(rope)) == RichRope{String, T} where T<:Union{Nothing, AbstractRope{String}}
        @test childtype(typeof(stringtoleaf("abc"))) == Nothing
        @test ischild(rope.left, rope) == true
        @test ischild(rope.right, rope) == true
        @test ischild(rope, stringtoleaf("abc")) == false
        @test ischild(rope, rope * "a") == true
        @test ischild(rope * "a", rope) == false
        @test NodeType(typeof(rope)) == HasNodeType()
        @test NodeType(typeof(stringtoleaf("abc"))) == HasNodeType()
        Base.redirect_stdio(stdout=devnull) do
            @test print_tree(rope) === nothing
        end
    end

    @testset "Basics" begin
        @test one(RichRope{String}) == one(RichRope("")) == one(String) == ""
        @test oneunit(RichRope{String}) == oneunit(RichRope("")) == oneunit(String) == ""
        @test typemin(RichRope{String}) == typemin(RichRope("")) == typemin(String) == ""
        @test eltype(RichRope("")) == Char
        @test eltype(RichRope(@view "abc"[1:3])) == Char
        @test eltype(RichRope{SubString}) == Char
        @test firstindex(RichRope("abcd")) == 1
        @test lastindex(RichRope("abcd")) == 4
        @test isvalid(RichRope("")) == true  # always true, otherwise constructor fails
        @test isvalid(RichRope("ααββ"), 2) == true # all in-bounds indices are valid
        @test isvalid(RichRope("a"), 5) == false
        @test isvalid(RichRope("a"), 0) == false
        @test convert(String, stringtoleaf("abcd")) isa String
    end
end
