# RESULT-REVIEW: result-ebfb39bf
# One ruled end beside a FIXED other end searches its whole window up to that end — a dip at 4 inside the
# hi window [2, 8] is found (the range's midpoint, 5, is not a limit). Populations at 2 and 6 → dip at 4.
using Random: MersenneTwister

@testset "range valley ends: one ruled end searches its whole window, up to the fixed end" begin
    rng = MersenneTwister(12)
    v = [2 .+ 0.5 .* randn(rng, 4000); 6 .+ 0.5 .* randn(rng, 4000)]
    g = RangeGate("c", 0.0, 10.0; hi_rule = ValleyRule(2.0, 8.0))
    @test abs(Cecelia.resolve_gate(g, v).hi - 4.0) < 0.3
    @test Cecelia.end_window(RangeGate("c", 3.0, 10.0; hi_rule = ValleyRule(2.0, 8.0)), :hi) == (3.0, 8.0)
end
