# RESULT-REVIEW: result-5485e121
# A follow shape saved where no cloud is found (here a rare population under 5 % of the tallest mode) must
# not snap onto a mode on other images — not even where the same, unmoved cloud is large enough to climb
# to. It cannot follow a cloud it was not anchored on, so it stays where it was drawn, flagged, on every
# image. Expected: no shift anywhere.
using Random: MersenneTwister

@testset "follow rule: saved with no cloud found → stays where drawn on every image" begin
    rng = MersenneTwister(5)
    big(n) = (8.0 .+ 0.3 .* randn(rng, n), 8.0 .+ 0.3 .* randn(rng, n))
    rare(n) = (2.0 .+ 0.3 .* randn(rng, n), 2.0 .+ 0.3 .* randn(rng, n))
    (bx, by), (rx, ry) = big(20_000), rare(300)                 # A: the rare cloud's peak is ~1.5 % of the big one's
    xa, ya = [bx; rx], [by; ry]
    rect = RectangleGate("x", "y", 1.0, 4.5, 1.0, 3.0; rule = FollowRule())   # off-centre: takes in x up to 4.5
    g = Cecelia.anchor_rule(rect, xa, ya)
    @test g.rule.anchor === nothing && Cecelia.needs_anchor(g)
    @test Cecelia.rule_flags(g, xa, ya) == Dict("rule_ok" => false)
    # B: the same rare cloud, unmoved, now 2000 cells — a mode the climb would reach
    (bx2, by2), (rx2, ry2) = big(20_000), rare(2000)
    xb, yb = [bx2; rx2], [by2; ry2]
    r = Cecelia.resolve_gate(g, xb, yb)
    @test (r.x_min, r.x_max, r.y_min, r.y_max) == (1.0, 4.5, 1.0, 3.0)
    @test Cecelia.rule_flags(g, xb, yb) == Dict("rule_ok" => false)
end
