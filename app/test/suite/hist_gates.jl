# ── 1D gates: range, bisector, and the valley rule ─────────────────────────────
# app/src/gating/hist_gates.jl. Expected cells come from the definitions (lo ≤ v ≤ hi; above / at-or-below
# the cut); expected valleys from the density the samples were drawn from, not from the code's output.
using Random: MersenneTwister

@testset "range + bisector: membership on one channel" begin
    v = [-1.0, 0.0, 0.5, 1.0, 2.0, NaN]
    r = RangeGate("c", 0.0, 1.0)
    @test inside(r, v, v) == BitVector([0, 1, 1, 1, 0, 0])
    @test RangeGate("c", 1.0, 0.0).lo == 0.0                    # ends in either order
    hi = BisectorGate("c", 0.5; side = 1); lo = BisectorGate("c", 0.5; side = -1)
    @test inside(hi, v, v) == BitVector([0, 0, 0, 1, 1, 0])
    @test inside(lo, v, v) == BitVector([1, 1, 1, 0, 0, 0])      # the cut itself is "-"
    @test all(inside(hi, v, v) .⊻ inside(lo, v, v) .== isfinite.(v))
    # the gate's own transform: a cut at asinh units
    t = AsinhTransform(cofactor = 1.0)
    g = BisectorGate("c", asinh(10.0); side = 1, x_transform = t)
    @test inside(g, [5.0, 20.0], [5.0, 20.0]) == BitVector([0, 1])
    @test_throws ErrorException BisectorGate("c", NaN)
    @test_throws ErrorException BisectorGate("c", 0.0; side = 0)
    @test_throws ErrorException ValleyRule(0.0, 1.0; adjust = 0)
end

@testset "1D gates: spec round-trip, one channel" begin
    for g in (RangeGate("c", 1.0, 3.0; x_transform = LogTransform(floor = 1.0)),
              BisectorGate("c", 2.0; side = -1, group = "g", rule = ValleyRule(1.0, 3.0; adjust = 1.5)))
        g2 = gate_from_spec(gate_spec(g))
        @test typeof(g2) == typeof(g) && gate_spec(g2) == gate_spec(g)
        @test g2.x_channel == g2.y_channel == "c"
    end
    @test gate_spec(BisectorGate("c", 0.0))["side"] == 1 && !haskey(gate_spec(BisectorGate("c", 0.0)), "rule")
    # a spec that names one channel, or the same twice, is a 1D gate; two different ones are refused
    @test gate_from_spec(Dict("kind" => "range", "x_channel" => "c", "lo" => 0, "hi" => 1)) isa RangeGate
    # a bisector spec without a side is the + half (side 1, above the cut)
    @test gate_from_spec(Dict("kind" => "bisector", "x_channel" => "c", "cut" => 0)).side == 1
    @test_throws ErrorException gate_from_spec(Dict("kind" => "bisector", "x_channel" => "a", "y_channel" => "b", "cut" => 0))
    @test_throws ErrorException gate_from_spec(Dict("kind" => "bisector", "x_channel" => "a", "cut" => 0,
                                                    "rule" => Dict("kind" => "peak", "lo" => 0, "hi" => 1)))
end

# a two-population sample: n1 cells N(μ1, σ1) and n2 cells N(μ2, σ2)
two_pops(rng, n1, μ1, σ1, n2, μ2, σ2) = [μ1 .+ σ1 .* randn(rng, n1); μ2 .+ σ2 .* randn(rng, n2)]
# the minimum of the TRUE mixture density between the two means (a fine grid search)
function true_valley(n1, μ1, σ1, n2, μ2, σ2)
    f(x) = n1 / σ1 * exp(-0.5 * ((x - μ1) / σ1)^2) + n2 / σ2 * exp(-0.5 * ((x - μ2) / σ2)^2)
    xs = range(μ1, μ2; length = 20_001)
    xs[argmin(f.(xs))]
end

@testset "valley_cut: the dip between two populations" begin
    rng = MersenneTwister(7)
    # equal halves: the valley is midway
    v = two_pops(rng, 4000, 0.0, 1.0, 4000, 6.0, 1.0)
    @test abs(valley_cut(v, -10, 10) - 3.0) < 0.25
    # unequal sizes and spreads: the valley moves toward the smaller, tighter population
    want = true_valley(6000, 0.0, 1.2, 1500, 5.0, 0.7)
    v = two_pops(rng, 6000, 0.0, 1.2, 1500, 5.0, 0.7)
    @test abs(valley_cut(v, -10, 10) - want) < 0.3 && want > 2.5
    # the window chooses among valleys: three populations, two valleys
    v = [two_pops(rng, 3000, 0.0, 0.6, 3000, 4.0, 0.6); 4.0 .+ 4.0 .+ 0.6 .* randn(rng, 3000)]
    @test abs(valley_cut(v, 0.5, 3.5) - 2.0) < 0.3
    @test abs(valley_cut(v, 4.5, 7.5) - 6.0) < 0.3
    # one population in the window: no valley → nothing (the caller keeps its fixed cut)
    @test valley_cut(0.0 .+ randn(rng, 5000), -3, 3) === nothing
    # one population with a sparse low tail (1 % of cells spread thinly below it): still one population
    v = [0.8 .+ 0.05 .* randn(rng, 9900); 0.2 .+ 0.5 .* rand(rng, 100)]
    @test valley_cut(v, 0.0, 1.2) === nothing
    # a shoulder (a dip under 20 % of the smaller peak) is not a valley; a clear one is
    @test valley_cut(two_pops(rng, 5000, 0.0, 1.0, 5000, 2.4, 1.0), -5, 7) === nothing
    @test valley_cut(two_pops(rng, 5000, 0.0, 1.0, 5000, 4.0, 1.0), -5, 7) !== nothing
    # a sparse far tail ripples, but the real dip wins (prominence, not depth)
    v = [two_pops(rng, 4000, 0.0, 1.0, 4000, 6.0, 1.0); [15.0, 18.0, 21.0, 24.0]]
    @test abs(valley_cut(v, -10, 30) - 3.0) < 0.3
    # an empty gap between two clouds: the middle of the gap
    v = [rand(rng, 2000); 10.0 .+ rand(rng, 2000)]
    @test abs(valley_cut(v, -5, 15) - 5.5) < 0.5
    # too few cells, NaNs ignored
    @test valley_cut([1.0, 2.0, NaN], 0, 3) === nothing
    @test abs(valley_cut([two_pops(rng, 2000, 0.0, 1.0, 2000, 6.0, 1.0); fill(NaN, 50)], -10, 10) - 3.0) < 0.3
end

# The cut is the docstring's recipe on the finite values: Silverman's bandwidth from their sd and IQR, a grid
# over their 0.1–99.9 % quantiles ∪ the window (± 3h), the binned density, its most prominent valley — the
# quantiles taken straight from `Statistics.quantile` on the values as given (in table order, NaN/Inf dropped).
@testset "valley_cut: the documented recipe, quantiles of the finite values" begin
    function recipe(v, lo, hi; adjust = 1.0)
        x = Float64[a for a in v if isfinite(a)]; n = length(x)
        q1, q3 = Cecelia.quantile(x, (0.25, 0.75)); sd = Cecelia.std(x)
        h = 0.9 * (q3 > q1 ? min(sd, (q3 - q1) / 1.34) : sd) * n^(-1 / 5) * adjust
        qa, qb = Cecelia.quantile(x, (Cecelia.VALLEY_RANGE_Q, 1 - Cecelia.VALLEY_RANGE_Q))
        a = max(minimum(x), min(qa, lo)) - 3h; b = min(maximum(x), max(qb, hi)) + 3h
        G = clamp(ceil(Int, 4 * (b - a) / h), Cecelia.VALLEY_GRID, Cecelia.VALLEY_GRID_MAX)
        d, a, dx = Cecelia._binned_kde(x, a, b, h, G)
        Cecelia._most_prominent_valley(d, a, dx, lo, hi)
    end
    rng = MersenneTwister(11)
    for k in 1:20
        v = Cecelia.Random.shuffle(rng, [two_pops(rng, 4000, 0.0, 1.0, 1000 * k, 4.0 + 0.1k, 0.8); NaN; Inf; -Inf; 1e4])
        lo, hi = -2.0 + 0.1k, 6.0 - 0.05k
        c = valley_cut(v, lo, hi; adjust = 0.5 + 0.1k)
        @test c !== nothing && isequal(c, recipe(v, lo, hi; adjust = 0.5 + 0.1k))
    end
end

@testset "rule bisector: the cut follows each parent's own valley" begin
    rng = MersenneTwister(11)
    rule = ValleyRule(-5.0, 15.0)
    m = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m, "pos"; gate = BisectorGate("c", 100.0; side = 1, group = "g", rule = rule))
    add_pop!(m, "neg"; gate = BisectorGate("c", 100.0; side = -1, group = "g", rule = rule))
    # image A: valley at 3; image B: the same populations shifted by +4 (a brighter acquisition) → 7
    for (shift, want) in ((0.0, 3.0), (4.0, 7.0))
        v = shift .+ two_pops(rng, 3000, 0.0, 1.0, 3000, 6.0, 1.0)
        df = DataFrame("label" => collect(1:length(v)), "c" => v)
        recompute!(m, _ -> df)
        pos = Set(cells_in_pop(m, "/pos")); neg = Set(cells_in_pop(m, "/neg"))
        @test isempty(intersect(pos, neg)) && length(pos) + length(neg) == length(v)
        cut = valley_cut(v, -5, 15)
        @test abs(cut - want) < 0.3
        @test pos == Set(findall(>(cut), v))           # exactly the cells above the resolved cut
    end
    # a parent with no valley in the window falls back to the stored cut (100 → nobody above it)
    df = DataFrame("label" => 1:3000, "c" => randn(rng, 3000))
    recompute!(m, _ -> df)
    @test isempty(cells_in_pop(m, "/pos")) && length(cells_in_pop(m, "/neg")) == 3000
    # the rule resolves on the PARENT's cells, not the whole table: under a gate that keeps only the
    # upper two populations, the valley is theirs
    m2 = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m2, "upper"; gate = RangeGate("c", 3.0, 100.0))
    add_pop!(m2, "hi"; parent = "/upper", gate = BisectorGate("c", 0.0; side = 1, group = "u", rule = ValleyRule(3.0, 100.0)))
    v = [two_pops(rng, 3000, 0.0, 0.6, 3000, 5.0, 0.6); 9.0 .+ 0.6 .* randn(rng, 3000)]
    recompute!(m2, _ -> DataFrame("label" => collect(1:length(v)), "c" => v))
    hi = cells_in_pop(m2, "/upper/hi")
    @test 2800 < length(hi) < 3200 && minimum(v[hi]) > 6.0
end

@testset "bisector halves move together; 1D gates survive save/load and a rename" begin
    m = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m, "pos"; gate = BisectorGate("c", 1.0; side = 1, group = "g"))
    add_pop!(m, "neg"; gate = BisectorGate("c", 1.0; side = -1, group = "g"))
    add_pop!(m, "mid"; gate = RangeGate("c", 0.0, 2.0))
    set_gate!(m, "/pos", BisectorGate("c", 2.0; side = 1, group = "g", rule = ValleyRule(0.0, 4.0)))
    @test pop_at(m, "/neg").gate.cut == 2.0 && pop_at(m, "/neg").gate.side == -1
    @test pop_at(m, "/neg").gate.rule == pop_at(m, "/pos").gate.rule
    td = mktempdir(); save_pop_tree!(m, td)
    m2 = load_pop_tree(td, "B")
    @test pop_at(m2, "/neg").gate isa BisectorGate && pop_at(m2, "/neg").gate.rule.hi == 4.0
    @test pop_at(m2, "/mid").gate isa RangeGate && pop_at(m2, "/mid").gate.hi == 2.0
    r = Cecelia.rename_pop_columns(m2, Dict("c" => "CD4"))
    @test pop_at(r, "/neg").gate.x_channel == pop_at(r, "/neg").gate.y_channel == "CD4"
    # the history hash ignores the group id, like a slice's
    @test Cecelia.gate_shape_spec(BisectorGate("c", 1.0; group = "a")) == Cecelia.gate_shape_spec(BisectorGate("c", 1.0; group = "b"))
    # on a histogram (x = y = c) the gate projects into the display transform; elsewhere it isn't shown
    pj = project_gate(BisectorGate("c", log10(100.0); side = -1, x_transform = LogTransform(floor = 1.0)), "c", "c",
                      LinearTransform(), LinearTransform())
    @test pj["kind"] == "bisector" && pj["cut"] ≈ 100.0 && pj["side"] == -1
    @test project_gate(RangeGate("c", 0.0, 1.0), "c", "d", LinearTransform(), LinearTransform()) === nothing
end

@testset "range with valley ends: a middle population between two dips, per image" begin
    rng = MersenneTwister(5)
    # three populations at 0, 4, 8 (sd 0.6): the middle one lies between valleys at 2 and 6
    three(shift) = shift .+ [0.6 .* randn(rng, 3000); 4 .+ 0.6 .* randn(rng, 3000); 8 .+ 0.6 .* randn(rng, 3000)]
    g = RangeGate("c", 1.0, 7.0; lo_rule = ValleyRule(0.5, 3.5), hi_rule = ValleyRule(4.5, 7.5))
    @test Cecelia.needs_parent_data(g) && !Cecelia.needs_parent_data(RangeGate("c", 0, 1))
    m = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m, "mid"; gate = g)
    v = three(0.0)
    recompute!(m, _ -> DataFrame("label" => collect(1:length(v)), "c" => v))
    r = Cecelia.resolve_gate(g, v)
    @test abs(r.lo - 2.0) < 0.3 && abs(r.hi - 6.0) < 0.3
    @test Set(cells_in_pop(m, "/mid")) == Set(findall(x -> r.lo <= x <= r.hi, v))
    @test 2800 < length(cells_in_pop(m, "/mid")) < 3200
    @test Cecelia.rule_flags(g, v) == Dict("lo_rule_ok" => true, "hi_rule_ok" => true)
    # only the upper end ruled; the lower stays where it was drawn
    g2 = RangeGate("c", 1.0, 7.0; hi_rule = ValleyRule(4.5, 7.5))
    r2 = Cecelia.resolve_gate(g2, v)
    @test r2.lo == 1.0 && abs(r2.hi - 6.0) < 0.3
    @test Cecelia.rule_flags(g2, v) == Dict("hi_rule_ok" => true)
    # an end whose window holds no valley keeps its stored value, flagged
    w = 4 .+ 0.6 .* randn(rng, 5000)
    @test Cecelia.resolve_gate(g, w).lo == 1.0 && Cecelia.rule_flags(g, w)["lo_rule_ok"] == false
    # spec round-trip + display projection keep the windows
    @test gate_spec(gate_from_spec(gate_spec(g))) == gate_spec(g)
    pj = project_gate(g, "c", "c", LinearTransform(), LinearTransform())
    @test pj["lo_rule"]["lo"] == 0.5 && pj["hi_rule"]["hi"] == 7.5
end

# A bisector group's two halves are one cut on one parent, so an evaluation searches for its valley once;
# a range's ruled ends search their own windows. Counted through `recompute!`'s `valley` (the search).
@testset "rule gates: one valley search per window per parent in an evaluation" begin
    rng = MersenneTwister(23)
    n = 6000
    df = DataFrame("label" => collect(1:n), "c" => two_pops(rng, 3000, 0.0, 1.0, 3000, 6.0, 1.0),
                   "d" => [two_pops(rng, 2000, 0.0, 0.7, 2000, 4.0, 0.7); 8.0 .+ 0.7 .* randn(rng, 2000)])
    function tree()
        m = PopulationTree(pop_type = "flow", value_name = "B")
        add_pop!(m, "hi"; gate = BisectorGate("c", 1.0; side = 1, group = "g", rule = ValleyRule(-10, 10)))
        add_pop!(m, "lo"; gate = BisectorGate("c", 1.0; side = -1, group = "g", rule = ValleyRule(-10, 10)))
        add_pop!(m, "mid"; gate = RangeGate("d", 1.0, 7.0; lo_rule = ValleyRule(0.5, 3.5), hi_rule = ValleyRule(4.5, 7.5)))
        add_pop!(m, "mid_hi"; parent = "/mid", gate = BisectorGate("c", 1.0; side = 1, group = "h", rule = ValleyRule(-10, 10)))
        add_pop!(m, "mid_lo"; parent = "/mid", gate = BisectorGate("c", 1.0; side = -1, group = "h", rule = ValleyRule(-10, 10)))
        m
    end
    calls = Ref(0)
    counted(v, lo, hi; adjust) = (calls[] += 1; valley_cut(v, lo, hi; adjust))
    m = recompute!(tree(), _ -> df; valley = counted)
    # root's group: 1, the range's two ends: 2, /mid's group (another parent, so its own search): 1
    @test calls[] == 4
    # membership as each gate resolved alone on its parent's raw values (the definition)
    for p in ("/hi", "/lo", "/mid", "/mid/mid_hi", "/mid/mid_lo")
        pop = m.pops[p]; par = pop.parent == ROOT ? trues(n) : pop_membership(m, pop.parent)
        g = Cecelia.resolve_gate(pop.gate, df[par, pop.gate.x_channel])
        @test pop_membership(m, p) == par .& inside(g, df[!, g.x_channel], df[!, g.y_channel])
    end
    @test count(pop_membership(m, "/hi")) + count(pop_membership(m, "/lo")) == n
    # rule_flags / resolve_gate are one resolution: the flags say which end's valley the gate used
    g = pop_at(m, "/mid").gate
    r, flags = Cecelia.resolve_rules(g, df.d)
    @test r.lo == Cecelia.resolve_gate(g, df.d).lo && flags == Cecelia.rule_flags(g, df.d) &&
          flags == Dict("lo_rule_ok" => true, "hi_rule_ok" => true) && abs(r.lo - 2.0) < 0.3 && abs(r.hi - 6.0) < 0.3
end
