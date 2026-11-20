# "Find regions like this one" helpers — docs/todo/LENS_QUERY_PLAN.md. Expected values come from the
# definitions (circle membership, a mean, cosine), worked by hand.
@testset "region lens query: pure helpers" begin
    # membership: centroid inside the circle, the edge included
    x = [0.0, 3.0, 5.0, 6.0]; y = [0.0, 4.0, 0.0, 0.0]
    @test Cecelia.lens_inside(x, y, (; x = 0.0, y = 0.0, r = 5.0)) == BitVector([1, 1, 1, 0])

    # a cell the region run gave no composition (NaN / missing) is zeroed and flagged unusable —
    # one NaN must not poison every score
    M = Union{Missing,Float64}[0.5 0.5; NaN 0.5; missing 1.0; 1.0 0.0]
    X, ok = Cecelia.composition_rows(M)
    @test ok == BitVector([1, 0, 0, 1])
    @test X == [0.5 0.5; 0.0 0.0; 0.0 0.0; 1.0 0.0]

    # signature = the mean row of the cells inside
    @test Cecelia.lens_signature(X, BitVector([1, 0, 0, 1])) == [0.75, 0.25]
    @test_throws ArgumentError Cecelia.lens_signature(X, falses(4))

    # cosine: identical direction 1, orthogonal 0, a zero row 0 (like nothing), a zero signature all 0
    s = Cecelia.cosine_scores([1.0 0.0; 0.0 1.0; 2.0 0.0; 0.0 0.0; 1.0 1.0], [1.0, 0.0])
    @test s[1:4] == [1.0, 0.0, 1.0, 0.0]
    @test s[5] ≈ 1 / sqrt(2)
    @test Cecelia.cosine_scores([1.0 2.0], [0.0, 0.0]) == [0.0]
end
