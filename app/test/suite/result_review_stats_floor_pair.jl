# RESULT-REVIEW: result-a186a78c
# The exactness gate on the replicate floor must look at the pair the floor is about (the two smallest
# groups), not at every pair: an approximate test between two big tied groups says nothing about the
# exact brackets on the 2-image group, which still can't reach p < 0.05 after Bonferroni.
# One value per image: A = 2 images, B = C = the same 9 values (so B vs C is tied and approximate).

@testset "stats QC: an unrelated approximate pair doesn't switch the floor off" begin
    mk(pre, vals) = DataFrame(uID = ["$pre$i" for i in eachindex(vals)], v = vals)
    g = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = mk("a", [1.0, 2.0])),
         (uid = "", vn = "B", sid = "B/b", grp = "", sub = mk("b", collect(10.0:18.0))),
         (uid = "", vn = "B", sid = "B/c", grp = "", sub = mk("c", collect(10.0:18.0)))]
    r = Cecelia._stats_from_series(g, "v", :auto)
    small = r["groups"][1]
    # the exact brackets on the 2-image group can't be significant (best p_adj = 2/C(11,2) × 3 = 0.109)…
    @test all(p["pAdj"] >= 0.05 for p in r["comparisonPairs"] if small in (p["a"], p["b"]))
    # …and QC says so
    f = [x for x in r["findings"] if x["code"] == "stats.few_replicates_pairs"]
    @test length(f) == 1 && f[1]["subs"]["n"] == 2
    @test isapprox(f[1]["subs"]["p"], 0.109; atol = 1e-3)
end
