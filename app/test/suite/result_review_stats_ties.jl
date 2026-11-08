# RESULT-REVIEW: result-52b1962a
# The replicate floor in `stats_qc_findings` binds only an EXACT Mann-Whitney. HypothesisTests switches
# a pair with >10 values and any ties to the tie-corrected normal approximation, whose p can go far
# below the exact floor — so a "can't go below p = …" warning there sits next to a significant bracket.
# One value per image (Datapoint = image), Kruskal + pairwise brackets over 2, 9 and 9 images, most
# images in a group sharing a value.

@testset "stats QC: no exact floor claimed beside an approximate rank test" begin
    mk(pre, vals) = DataFrame(uID = ["$pre$i" for i in eachindex(vals)], v = vals)
    g = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = mk("a", [1.0, 2.0])),
         (uid = "", vn = "B", sid = "B/b", grp = "", sub = mk("b", fill(10.0, 9))),
         (uid = "", vn = "B", sid = "B/c", grp = "", sub = mk("c", fill(20.0, 9)))]
    r = Cecelia._stats_from_series(g, "v", :auto)
    @test r["test"] == "kruskal"
    # the plot draws significant brackets for the 2-image group…
    small = r["groups"][1]                                  # the 2-image group's label
    @test any(p["pAdj"] < 0.05 for p in r["comparisonPairs"] if small in (p["a"], p["b"]))
    # …so QC must not say a bracket on it can't go below 0.05
    @test !any(f["code"] in ("stats.few_replicates", "stats.few_replicates_pairs") for f in r["findings"])
    @test !Cecelia.rank_tests_exact([[1.0, 2.0], fill(10.0, 9), fill(20.0, 9)])
    @test Cecelia.rank_tests_exact([[1.0, 2.0], [3.0, 4.0, 5.0]])
end
