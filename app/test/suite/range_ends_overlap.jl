# RESULT-REVIEW: result-7eb66191
# A range whose two ruled ends have overlapping windows (the GUI default on a range narrower than 2/5 of the
# view) on a parent with ONE dip: both ends must not land in that dip. Two populations at 3 and 9 (sd 0.6,
# equal halves) → the only valley is midway, at 6. The windows [2, 7] and [4, 8] overlap on [4, 7], split at
# 5.5: the low end searches [2, 5.5] (only the first peak's falling side — nothing), the high end [5.5, 8]
# (the dip). Expected positions from the mixture, not the code.
using Random: MersenneTwister

@testset "range valley ends: overlapping windows do not share one dip" begin
    rng = MersenneTwister(11)
    v = [3 .+ 0.6 .* randn(rng, 4000); 9 .+ 0.6 .* randn(rng, 4000)]
    g = RangeGate("c", 4.0, 6.0; lo_rule = ValleyRule(2.0, 7.0), hi_rule = ValleyRule(4.0, 8.0))
    r = Cecelia.resolve_gate(g, v)
    @test r.lo == 4.0                                   # no valley in [2, 5.5]: stored end stands
    @test abs(r.hi - 6.0) < 0.3
    @test Cecelia.rule_flags(g, v) == Dict("lo_rule_ok" => false, "hi_rule_ok" => true)
    @test r.hi - r.lo > 0.5                             # not collapsed onto the dip
    @test Cecelia.end_window(g, :lo) == (2.0, 5.5) && Cecelia.end_window(g, :hi) == (5.5, 8.0)
end
