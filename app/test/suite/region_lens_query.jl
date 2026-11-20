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

@testset "region lens query: saved score helpers" begin
    @test Cecelia.lens_score_col("crypt rim") == "spatial.lens.crypt_rim"
    @test Cecelia.lens_score_col("lens2") == "spatial.lens.lens2"

    # similarity above the run's average cell: (cos − mu) / (1 − mu), clamped 0–1; no composition → NaN
    X = [1.0 0.0; 0.0 1.0; 1.0 1.0; 0.0 0.0]
    ok = BitVector([1, 1, 1, 0])
    sig = [1.0, 0.0]
    mu = Cecelia.lens_background([(; X, ok)], sig)
    @test mu ≈ (1 + 0 + 1 / sqrt(2)) / 3                # the unscored cell is not in the background
    sc = Cecelia.lens_similarity_scores(X, ok, sig, mu)
    @test sc[1] == 1.0                                  # identical to the lens
    @test sc[2] == 0.0                                  # less like it than the average cell: clamped
    @test sc[3] ≈ (1 / sqrt(2) - mu) / (1 - mu)
    @test isnan(sc[4])
    # every cell identical to the lens (mu 1): 0, not NaN
    @test Cecelia.lens_similarity_scores([1.0 0.0], trues(1), sig, 1.0) == [0.0]
    @test Cecelia.LENS_SCORE_RANGE == (0.0, 1.0)

    # coherence: the lens's own cells against its signature — one tissue ≈ 1, two tissues lower
    @test Cecelia.lens_coherence([1.0 0.0; 1.0 0.0], trues(2), [1.0, 0.0]) ≈ 1.0
    two = [1.0 0.0; 0.0 1.0]
    @test Cecelia.lens_coherence(two, trues(2), Cecelia.lens_signature(two, trues(2))) < Cecelia.LENS_QUERY_MIN_COHERENCE

    # QC: a lens whose mix is the tissue's average warns; a distinct one does not
    @test [f["code"] for f in Cecelia.lens_query_qc_findings(0.01)] == ["lensQuery.average_mix"]
    @test isempty(Cecelia.lens_query_qc_findings(0.4))
    @test [f["code"] for f in Cecelia.lens_query_qc_findings(0.4, 0.65)] == ["lensQuery.mixed_lens"]
    @test haskey(Cecelia.QC_TEXT, "lensQuery.mixed_lens")
end

@testset "clustRegions.lensQuery: dispatch and params" begin
    task = Cecelia._task_from_fun_name("clustRegions.lensQuery")
    @test task isa Cecelia.LensQuery
    @test Cecelia.task_scope(task) == "set"
    good = Dict{String,Any}("lensX" => 10.0, "lensY" => 20.0, "lensR" => 30.0)
    @test begin validate_params(task, good); true end
    @test_throws ParamValidationError validate_params(task, Dict{String,Any}("lensX" => 10.0, "lensY" => 20.0))
    @test_throws ParamValidationError validate_params(task, merge(good, Dict{String,Any}("lensR" => "wide")))
    p = Cecelia.parse_lens_query_params(merge(good, Dict{String,Any}("outputName" => "crypt")))
    @test (p.lensR, p.lensT, p.outputName) == (30.0, -1, "crypt")
end

@testset "clustRegions.lensQuery writes a score column on a region run" begin
    fx = fixture_for(:cells, :still, :regions, :real; need = "still-with-region-run")
    if fx === nothing
        @test_skip "no fixture with a region run (test-data/NEEDS.toml → still-with-region-run)"
    end
end

@testset "lens scores are a deletable run kind" begin
    k = only(filter(k -> k.kind == "lensScores", Cecelia.RUN_KINDS))
    @test k.funs == ["clustRegions.lensQuery"]
    @test Cecelia.lens_score_col("x") == Cecelia.LENS_SCORE_PREFIX * "x"
    # the region run's own composition columns are not lens scores, and vice versa
    @test Cecelia._obs_suffixes(["spatial.lens.lens1", "spatial.comp.cells_B.default", "regions.default"],
                                Cecelia.LENS_SCORE_PREFIX) == ["lens1"]
    drops, _ = Cecelia._clust_run_keys("regions", :cell, "default", ["regions.default", "spatial.lens.lens1"], false)
    @test !("spatial.lens.lens1" in drops)
end

@testset "lens scores draw on a fixed scale" begin
    # one colour = one score on every image: the lens scores keep 0 → 1, other columns their min–max
    @test Cecelia.fixed_colour_range("spatial.lens.lens1") == (0.0, 1.0)
    @test Cecelia.fixed_colour_range("spatial.comp.cells_B.default") === nothing
    @test Cecelia.fixed_colour_range("regions.default") === nothing
end
