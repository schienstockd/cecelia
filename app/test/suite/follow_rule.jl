# ── 2D rule: a rectangle / polygon that follows its cloud ──────────────────────
# app/src/gating/follow_rule.jl. Expected positions come from the Gaussian clouds the cells were drawn
# from (their centres and how far a cloud was moved), not from the code's output.
using Random: MersenneTwister
using Statistics: quantile, std

_cloud(rng, cx, cy, n; sd = 0.3) = (cx .+ sd .* randn(rng, n), cy .+ sd .* randn(rng, n))
function _two_clouds(rng, a, b; na = 2000, nb = 1000)
    xa, ya = _cloud(rng, a..., na); xb, yb = _cloud(rng, b..., nb)
    [xa; xb], [ya; yb]
end
const _FOLLOW_POLY = [(1.2, 1.0), (2.8, 1.2), (3.0, 3.0), (1.0, 2.8)]   # drawn around a cloud at (2, 2)

@testset "follow rule: anchored on the cloud it was drawn on, moves with it, misses when it is gone" begin
    rng = MersenneTwister(7)
    x1, y1 = _two_clouds(rng, (2.0, 2.0), (6.0, 6.0))
    g = Cecelia.anchor_rule(PolygonGate("x", "y", _FOLLOW_POLY; rule = FollowRule()), x1, y1)
    # the anchor is that cloud's centre (the mode of a Gaussian is its mean), to a grid step
    @test all(abs.(g.rule.anchor .- (2.0, 2.0)) .< 0.1)
    # on the image it was drawn on it does not move
    @test Cecelia.follow_shift(g, x1, y1) == (0.0, 0.0)
    @test Cecelia.resolve_gate(g, x1, y1).vertices == g.vertices
    # the cloud moved by (1.0, 0.5) on another image → the shape moves by that, its shape unchanged
    x2, y2 = _two_clouds(rng, (3.0, 2.5), (6.0, 6.0))
    s = Cecelia.follow_shift(g, x2, y2)
    @test s !== nothing && abs(s[1] - 1.0) < 0.15 && abs(s[2] - 0.5) < 0.15
    r = Cecelia.resolve_gate(g, x2, y2)
    @test r.vertices == [(x + s[1], y + s[2]) for (x, y) in g.vertices]
    # the flags name the peak it moved to: that image's cloud centre, to a grid step — the same density
    # as `resolve_gate`, so the shape and its peak agree
    f = Cecelia.rule_flags(g, x2, y2)
    @test f["rule_ok"] === true && all(abs.(f["rule_peak"] .- (3.0, 2.5)) .< 0.1)
    rg, rf = Cecelia.resolve_with_flags(g, x2, y2)
    @test rg.vertices == r.vertices && rf == f
    @test f["rule_peak"] ≈ collect(g.rule.anchor .+ s)
    # its cloud is gone: the nearest one is further than the shape's own size → it stays put, flagged
    xb, yb = _cloud(rng, 6.0, 6.0, 1000)
    @test Cecelia.follow_shift(g, xb, yb) === nothing
    @test Cecelia.resolve_gate(g, xb, yb).vertices == g.vertices
    @test Cecelia.rule_flags(g, xb, yb) == Dict("rule_ok" => false)
    # moved further than the shape is wide, along ONE axis only, is a miss too: (2.6, 0) for a shape 2.0
    # wide (x 1.0–3.0); a broad cloud, so the climb from the anchor does reach its mode
    x3, y3 = _cloud(rng, 4.6, 2.0, 3000; sd = 1.0)
    @test Cecelia.follow_shift(g, x3, y3) === nothing
    # too few cells: no density
    @test Cecelia.follow_shift(g, x2[1:5], y2[1:5]) === nothing
    # a rectangle anchors from its middle and moves the same way
    rect = Cecelia.anchor_rule(RectangleGate("x", "y", 1.0, 3.0, 1.0, 3.0; rule = FollowRule()), x1, y1)
    rr = Cecelia.resolve_gate(rect, x2, y2)
    @test abs(rr.x_min - 2.0) < 0.15 && abs(rr.y_max - 3.5) < 0.15 && rr.x_max - rr.x_min ≈ 2.0
    # not anchored yet (a Julia caller that skipped the save path): it stays where it was drawn
    u = PolygonGate("x", "y", _FOLLOW_POLY; rule = FollowRule())
    @test Cecelia.resolve_gate(u, x2, y2).vertices == u.vertices && Cecelia.needs_anchor(u)
    @test !Cecelia.needs_anchor(g) && !Cecelia.needs_parent_data(PolygonGate("x", "y", _FOLLOW_POLY))
end

@testset "follow rule: recompute! anchors on the parent's cells, then follows them per image" begin
    rng = MersenneTwister(11)
    # the parent keeps only x < 5: the cloud at (6, 6) is outside it, so the climb cannot reach it
    x1, y1 = _two_clouds(rng, (2.0, 2.0), (6.0, 6.0))
    df1 = DataFrame("label" => collect(1:length(x1)), "x" => x1, "y" => y1)
    m = PopulationTree(pop_type = "flow", value_name = "A")
    add_pop!(m, "left"; gate = RectangleGate("x", "y", -10.0, 5.0, -10.0, 10.0))
    add_pop!(m, "cloud"; parent = "/left", gate = PolygonGate("x", "y", _FOLLOW_POLY; rule = FollowRule()))
    recompute!(m, _ -> df1; anchor = true)
    a = pop_at(m, "/left/cloud").gate.rule.anchor
    @test a !== nothing && all(abs.(a .- (2.0, 2.0)) .< 0.1)
    # survives save / load and a column rename with its anchor
    td = mktempdir(); save_pop_tree!(m, td)
    m2 = load_pop_tree(td, "A")
    @test pop_at(m2, "/left/cloud").gate.rule == pop_at(m, "/left/cloud").gate.rule
    @test Cecelia.rename_pop_columns(m2, Dict("x" => "CD4")) |> r -> pop_at(r, "/left/cloud").gate.rule.anchor == a
    # another image: the cloud sits 0.8 further right → its cells are the pop, the drawn spot isn't
    x2, y2 = _two_clouds(rng, (2.8, 2.0), (6.0, 6.0))
    recompute!(m2, _ -> DataFrame("label" => collect(1:length(x2)), "x" => x2, "y" => y2))
    c = cells_in_pop(m2, "/left/cloud")
    # the template holds ~all of a cloud centred in it; moved with the cloud, it still does
    @test length(c) > 0.95 * 2000 && all(<(5.0), x2[c])
end

@testset "project_point: a gate-coordinate point onto a plot, either axis order and transform" begin
    lg = LogTransform()
    g = RectangleGate("a", "b", 0.0, 1.0, 0.0, 1.0)            # linear on its own axes
    @test project_point(g, "a", "b", LinearTransform(), LinearTransform(), (2.0, 3.0)) == [2.0, 3.0]
    @test project_point(g, "b", "a", LinearTransform(), LinearTransform(), (2.0, 3.0)) == [3.0, 2.0]
    # shown under log10 on x: raw 100 sits at 2
    @test project_point(g, "a", "b", lg, LinearTransform(), (100.0, 3.0)) ≈ [2.0, 3.0]
    @test project_point(g, "a", "c", LinearTransform(), LinearTransform(), (2.0, 3.0)) === nothing
end

@testset "follow rule: spec, projection, and what a rectangle / polygon's rule may be" begin
    g = PolygonGate("x", "y", _FOLLOW_POLY; rule = FollowRule(; anchor = (2.0, 2.0), adjust = 1.5))
    d = gate_spec(g)
    @test d["rule"] == Dict("kind" => "follow", "anchor" => [2.0, 2.0], "adjust" => 1.5)
    @test gate_from_spec(d).rule == g.rule
    # from the plot: no anchor (it is anchored again where it is saved)
    p = project_gate(g, "x", "y", LinearTransform(), LinearTransform())
    @test p["rule"] == Dict("kind" => "follow", "adjust" => 1.5)
    @test gate_from_spec(merge(d, Dict("rule" => p["rule"]))).rule.anchor === nothing
    @test !haskey(gate_spec(RectangleGate("x", "y", 0, 1, 0, 1)), "rule")
    # a valley rule is a 1D gate's, not a shape's
    @test_throws ErrorException gate_from_spec(merge(d, Dict("rule" => Dict("kind" => "valley", "lo" => 0, "hi" => 1))))
    @test_throws ErrorException FollowRule(; adjust = 0)
end

@testset "follow rule: a shape's centre and the density grid, from their definitions" begin
    # area centroid of a triangle = the mean of its corners; of an L of two unit-area-weighted boxes
    @test all(Cecelia.template_centre(PolygonGate("x", "y", [(0.0, 0.0), (3.0, 0.0), (0.0, 6.0)])) .≈ (1.0, 2.0))
    # L = box [0,2]×[0,1] (area 2, centre (1, 0.5)) ∪ box [0,1]×[1,3] (area 2, centre (0.5, 2)) → (0.75, 1.25)
    L = [(0.0, 0.0), (2.0, 0.0), (2.0, 1.0), (1.0, 1.0), (1.0, 3.0), (0.0, 3.0)]
    @test all(Cecelia.template_centre(PolygonGate("x", "y", L)) .≈ (0.75, 1.25))
    # no area (a sliver far from the origin, collinear to rounding): the vertex mean
    s = [(1e4, 1e4), (1e4 + 1, 1e4 + 1), (1e4 + 2, 1e4 + 2 + 1e-9), (1e4 + 9, 1e4 + 9)]
    @test Cecelia.template_centre(PolygonGate("x", "y", s)) == (sum(first, s) / 4, sum(last, s) / 4)
    @test Cecelia.template_size(PolygonGate("x", "y", L)) == (2.0, 3.0)

    # the grid: each axis from its 0.1 % quantile − 3 bandwidths to its 99.9 % quantile + 3 bandwidths, and
    # the density at every point the Gaussian product-kernel sum over the cells (binning error ≲ 1 %)
    rng = MersenneTwister(3)
    x = [1.0 .+ 0.5 .* randn(rng, 150); 4.0 .+ 0.8 .* randn(rng, 100)]
    y = [2.0 .+ 0.4 .* randn(rng, 150); -1.0 .+ 0.3 .* randn(rng, 100)]
    dens, (ax, dx), (ay, dy) = Cecelia._follow_density(x, y, 1.0)
    bw(v) = begin q1, q3 = quantile(v, (0.25, 0.75)); 0.9 * min(std(v), (q3 - q1) / 1.34) * length(v)^(-1 / 6) end
    hx, hy = bw(x), bw(y)
    @test ax ≈ quantile(x, 0.001) - 3hx && ax + (size(dens, 1) - 1) * dx ≈ quantile(x, 0.999) + 3hx
    @test ay ≈ quantile(y, 0.001) - 3hy && ay + (size(dens, 2) - 1) * dy ≈ quantile(y, 0.999) + 3hy
    kde(px, py) = sum(exp(-0.5 * (((px - a) / hx)^2 + ((py - b) / hy)^2)) for (a, b) in zip(x, y))
    idx = [(i, j) for i in 1:7:size(dens, 1), j in 1:7:size(dens, 2)]
    exact = [kde(ax + (i - 1) * dx, ay + (j - 1) * dy) for (i, j) in idx]
    @test maximum(abs.([dens[i, j] for (i, j) in idx] .- exact)) < 0.01 * maximum(exact)
    # a climb started off the grid is clamped to its edge: level with the first cloud it climbs to that
    # cloud's mode; at a corner no cell is within the kernel's reach, so the density is zero → no mode
    m = follow_mode(x, y, (-100.0, 2.0))
    @test m !== nothing && all(abs.(m .- (1.0, 2.0)) .< 0.3)
    @test follow_mode(x, y, (-100.0, -100.0)) === nothing
end
