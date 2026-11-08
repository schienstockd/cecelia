# RESULT-REVIEW: result-6338c97d
# The between-group test's replicate floor (`stats_qc_findings`, app/src/plotting/stats.jl) must
# describe the test the plot actually shows: with 3+ groups the brackets are Bonferroni-adjusted, so a
# 2-image group beside two 8-image groups can never get a significant bracket (best p_adj 0.133) and
# must be flagged; the Kruskal-Wallis omnibus can still be significant, so the finding must speak of
# the pairs, not "the test"; and a Welch t-test has no exact floor, so none is claimed.

@testset "stats QC floor matches the brackets the plot draws" begin
    codes(fs) = [f["code"] for f in fs]
    # 8, 8, 2 images, one value per image: 2 vs 8 raw floor 0.044, ×3 pairs = 0.133 > 0.05
    f = Cecelia.stats_qc_findings([8, 8, 2], [8, 8, 2]; test = :kruskal)
    @test codes(f) == ["stats.few_replicates_pairs"]
    @test f[1]["subs"]["n"] == 2
    @test isapprox(f[1]["subs"]["p"], 0.133; atol = 1e-3)
    @test occursin("bracket", f[1]["short"])              # about the pairs, not the omnibus
    # the omnibus on 2, 2, 8 cleanly separated groups IS significant — the finding must not deny it
    r = Cecelia.run_stats(["a" => [1.0, 2.0], "b" => [10.0, 11.0], "c" => collect(20.0:27.0)]; test = :kruskal)
    @test r.p_value < 0.05
    @test codes(Cecelia.stats_qc_findings(r.n, r.n; test = :kruskal)) == ["stats.few_replicates_pairs"]
    # Welch 3 vs 3: no exact floor, so no "can't go below" claim beside a tiny p
    @test isempty(Cecelia.stats_qc_findings([3, 3], [3, 3]; test = :ttest))
    @test isempty(Cecelia.stats_qc_findings([3, 3, 3], [3, 3, 3]; test = :anova))
end
