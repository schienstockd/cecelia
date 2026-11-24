# RESULT-REVIEW: result-0f2920b4
# Both ends ruled; the low end finds a dip (6.3) above the stored high end (6) while the high end misses: the
# found end is not used (both stored ends stand) and the flags say so — both dashed, never a solid line on
# a position the rule did not place. Populations at 5.3 and 7.3 (sd 0.25) → dip at 6.3; windows [2, 9] and
# [4, 10] overlap on [4, 9], split at 6.5: lo searches [2, 6.5] (the dip), hi [6.5, 10] (a peak, no dip).
using Random: MersenneTwister

@testset "range valley ends: a found end that can't be used is flagged" begin
    rng = MersenneTwister(13)
    v = [5.3 .+ 0.25 .* randn(rng, 4000); 7.3 .+ 0.25 .* randn(rng, 4000)]
    g = RangeGate("c", 4.0, 6.0; lo_rule = ValleyRule(2.0, 9.0), hi_rule = ValleyRule(4.0, 10.0))
    @test abs(valley_cut(v, 2.0, 6.5) - 6.3) < 0.2 && valley_cut(v, 6.5, 10.0) === nothing   # the premise
    r = Cecelia.resolve_gate(g, v)
    @test (r.lo, r.hi) == (4.0, 6.0)
    @test Cecelia.rule_flags(g, v) == Dict("lo_rule_ok" => false, "hi_rule_ok" => false)
end
