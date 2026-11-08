# RESULT-REVIEW: result-d3ef9ed5
# The replicate floor speaks about the smallest group's brackets, so every one of them must have run
# exact. Here the 2-image group's bracket against a tied 9-image group runs on the tie-corrected
# approximation and is drawn significant — QC must not say a bracket on that group can't be.
# One value per image: A = 2 images, B = 3, C = 9 images all at the same value.

@testset "stats QC: the floor needs every bracket on the smallest group exact" begin
    mk(pre, vals) = DataFrame(uID = ["$pre$i" for i in eachindex(vals)], v = vals)
    g = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = mk("a", [1.0, 2.0])),
         (uid = "", vn = "B", sid = "B/b", grp = "", sub = mk("b", [3.0, 4.0, 5.0])),
         (uid = "", vn = "B", sid = "B/c", grp = "", sub = mk("c", fill(20.0, 9)))]
    r = Cecelia._stats_from_series(g, "v", :auto)
    small = r["groups"][1]
    @test any(p["pAdj"] < 0.05 for p in r["comparisonPairs"] if small in (p["a"], p["b"]))
    @test !any(f["code"] in ("stats.few_replicates", "stats.few_replicates_pairs") for f in r["findings"])
    @test !Cecelia.rank_tests_exact([[1.0, 2.0], [3.0, 4.0, 5.0], fill(20.0, 9)])
end
