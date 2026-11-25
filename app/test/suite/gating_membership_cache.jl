# Gate evaluation cost on large tables: `recompute!` transforms each (column, transform) once and tests a
# child only on its parent's rows; `recompute_cached!` keeps a map's membership across requests.
# Expected memberships come from the per-gate definition (`parent ∩ inside(gate, raw x, raw y)` over the
# whole table), never from the code path under test.
using Random: MersenneTwister

@testset "recompute!: shared transforms + parent rows give the per-gate answer" begin
    rng = MersenneTwister(7)
    n = 5_000
    df = DataFrame("label" => collect(1:n), "a" => 10 .^ (4 .* rand(rng, n)), "b" => 10 .^ (4 .* rand(rng, n)),
                   "c" => randn(rng, n) .* 50, "d" => rand(rng, n))
    df.a[17] = NaN; df.c[99] = NaN
    lg = LogicleTransform(T = 1e4); ash = AsinhTransform(cofactor = 5.0)
    m = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m, "big"; gate = RectangleGate("a", "b", 0.1, 0.9, 0.05, 1.0; x_transform = lg, y_transform = lg))
    add_pop!(m, "poly"; parent = "/big", gate = PolygonGate("a", "b", [(0.2, 0.2), (0.8, 0.3), (0.5, 0.9)];
                                                            x_transform = lg, y_transform = lg))
    # same axes, separately-parsed transforms with equal parameters — must share one transformed column
    add_pop!(m, "sib"; parent = "/big", gate = RectangleGate("a", "b", 0.5, 1.0, 0.0, 0.5;
             x_transform = LogicleTransform(T = 1e4), y_transform = LogicleTransform(T = 1e4)))
    # same column, a DIFFERENT transform — must not reuse the logicle column
    add_pop!(m, "lin"; parent = "/big/poly", gate = RectangleGate("a", "d", 100.0, 5000.0, 0.0, 0.5))
    add_pop!(m, "q"; parent = "/big", gate = QuadrantGate("c", "d", asinh(0.0), 0.5; quadrant = 1, x_transform = ash))
    add_pop!(m, "sl"; parent = "/big", gate = SliceGate("c", "d", [(-3.0, 0.0), (3.0, 1.0)]; side = 1, x_transform = ash))
    add_pop!(m, "rg"; parent = "/big/sib", gate = RangeGate("c", -1.0, 2.0; x_transform = ash))
    add_pop!(m, "bis"; parent = "/big/sib", gate = BisectorGate("c", 0.0; side = 1, x_transform = ash,
                                                               rule = ValleyRule(-2.0, 2.0)))
    recompute!(m, _ -> df)

    want = Dict{String,BitVector}("root" => trues(n))
    for p in topo_order(m)
        pop = m.pops[p]; par = want[pop.parent == ROOT ? "root" : pop.parent]
        g = Cecelia.needs_parent_data(pop.gate) ? Cecelia.resolve_gate(pop.gate, df[par, pop.gate.x_channel]) : pop.gate
        want[p] = par .& inside(g, df[!, g.x_channel], df[!, g.y_channel])
        @test pop_membership(m, p) == want[p]
    end
    @test all(p -> 0 < count(want[p]) < n, ["/big", "/big/poly", "/big/sib", "/big/q", "/big/sl"])
    # `inside` itself, against the scalar definition (transform, then the geometry)
    g = pop_at(m, "/big").gate
    @test inside(g, df.a, df.b) == BitVector(0.1 <= apply_transform(lg, x) <= 0.9 && 0.05 <= apply_transform(lg, y) <= 1.0
                                             for (x, y) in zip(df.a, df.b))
    pg = pop_at(m, "/big/poly").gate
    @test inside(pg, df.a, df.b) == BitVector(point_in_polygon(apply_transform(lg, x), apply_transform(lg, y), pg.vertices)
                                              for (x, y) in zip(df.a, df.b))
    @test transform_key(LogicleTransform(T = 1e4)) == transform_key(lg) != transform_key(LinearTransform())
    # the threaded path (a column past the split size) gives the per-value transform
    big = 10 .^ (4 .* rand(rng, 120_000))
    @test transform_column(lg, big) == [apply_transform(lg, float(v)) for v in big]
end

# membership by the definition, evaluated without any cache: what `cells_in_pop` must return
computed_with(img, vn, m) = cells_in_pop(recompute!(m, Cecelia.pop_membership_fetch(img, vn, "flow")), "/big/half")

@testset "membership cache: one evaluation per map + table" begin
    fx = fixture_for(:cells, :movie)
    td = mktempdir(); cp(fx.dir, joinpath(td, "img"))
    img = Cecelia._load_image(joinpath(td, "img")); vn = fx.value_name
    m0 = PopulationTree(pop_type = "flow", value_name = vn)
    add_pop!(m0, "big"; gate = RectangleGate("mean_intensity_0", "mean_intensity_1", -1e12, 1e12, -1e12, 1e12))
    add_pop!(m0, "half"; parent = "/big", gate = RangeGate("area", 0.0, 200.0))
    save_pop_tree!(m0, img)

    reads = Ref(0)
    src = Cecelia.pop_membership_fetch(img, vn, "flow")
    counting = cols -> (reads[] += 1; src(cols))
    ev(m) = recompute_cached!(m, img, vn, "flow", counting)
    load() = load_pop_tree(img; value_name = vn, pop_type = "flow")

    a = ev(load()); @test reads[] == 1
    b = ev(load()); @test reads[] == 1                     # same map, same table: no read
    @test b._labels === a._labels && pop_membership(b, "/big/half") == pop_membership(a, "/big/half")
    # the cached masks are shared, the map's dict is its own: replacing one entry stays local
    b._membership["/big"] = falses(length(b._labels))
    @test count(pop_membership(ev(load()), "/big")) > 0
    # a colour change changes no member: no evaluation (it used to cost the GUI one per population)
    m = load(); pop_at(m, "/big").colour = "#123456"; stamp_edit!(m, "/big"); save_pop_tree!(m, img)
    ev(load()); @test reads[] == 1

    # a moved gate is a new evaluation, and its answer is the moved gate's
    m = load(); set_gate!(m, "/big/half", RangeGate("area", 0.0, 50.0)); save_pop_tree!(m, img)
    c = ev(load()); @test reads[] == 2
    @test cells_in_pop(c, "/big/half") == computed_with(img, vn, load())

    # an in-memory edit (the API's pick-selection pop) is part of what is cached — a different
    # selection, differing in ONE label of thousands (a sampled `hash` would miss it), re-evaluates
    labs = collect(Int, c._labels)
    pick(ls) = (m = load(); add_pop!(m, "Pick selection"; explicit_labels = ls, transient = true); m)
    p1 = ev(pick(labs[1:1200])); @test reads[] == 3
    ev(pick(labs[1:1200])); @test reads[] == 3
    p2 = ev(pick([labs[1:600]; labs[602:1201]])); @test reads[] == 4
    @test cells_in_pop(p2, "/Pick selection") != cells_in_pop(p1, "/Pick selection")

    # a rewritten table is a new evaluation (mtime + size)
    h5 = Cecelia.label_props(img; value_name = vn).path
    sleep(0.01); touch(h5)
    ev(load()); @test reads[] == 5

    # a population holding NO explicit labels is not a population with no label list (a copy of its
    # parent): the two maps must not share an evaluation
    plain = load(); add_pop!(plain, "x"; parent = "/big")
    nolab = load(); add_pop!(nolab, "x"; parent = "/big", explicit_labels = Int[])
    @test count(pop_membership(ev(plain), "/big/x")) > 0
    @test count(pop_membership(ev(nolab), "/big/x")) == 0

    # the stamp follows the table the pop type is evaluated over: the per-track table moves a track
    # map's evaluation and not a cell map's
    ev(load()); n0 = reads[]                               # one slot per segmentation: refill it
    tp = Cecelia.img_track_props_path(img, vn); @test isfile(tp)
    ev(load()); @test reads[] == n0                         # cached
    tm = PopulationTree(pop_type = "track", value_name = vn)
    tsrc = Cecelia.pop_membership_fetch(img, vn, "track")
    tcount = Ref(0); tfetch = cols -> (tcount[] += 1; tsrc(cols))
    recompute_cached!(tm, img, vn, "track", tfetch); recompute_cached!(tm, img, vn, "track", tfetch)
    @test tcount[] == 1
    sleep(0.01); touch(tp)
    recompute_cached!(tm, img, vn, "track", tfetch); @test tcount[] == 2
    ev(load()); @test reads[] == n0                         # the cell map did not read the track table

    # the routes' entry point uses it: two computed maps of one document share their labels
    @test computed_pop_tree(img; value_name = vn)._labels === computed_pop_tree(img; value_name = vn)._labels
end

@testset "label_props: the cached label index is never the caller's column" begin
    fx = fixture_for(:cells, :movie)
    lp() = select_cols(Cecelia.label_props(fixture_table(fx)), ["area"])
    d1 = as_df(lp()); first_label = d1.label[1]
    d1.label[1] = -12345; sort!(d1, :area)               # a caller is free to edit and sort its frame
    @test as_df(lp()).label[1] == first_label
end

@testset "resolve_pops: the tracking columns are read once per table, not once per map write" begin
    # `_resolve_pops_compute` reruns on every gating-map write (its stamp hashes the map); the table's
    # track_id/track_source map only changes with the table, so it is cached on the table's stamp.
    fx = fixture_for(:cells, :movie)
    td = mktempdir(); cp(fx.dir, joinpath(td, "img"))
    img = Cecelia._load_image(joinpath(td, "img")); vn = fx.value_name
    a = Cecelia._label_track_sources_cached(img, vn)
    @test a == label_track_sources(img, vn)                 # the uncached reader's answer
    @test !isempty(a)                                       # the fixture is tracked
    @test Cecelia._label_track_sources_cached(img, vn) === a  # a map write later: no re-read
    sleep(0.01); touch(Cecelia.img_label_props_path(img, vn))
    b = Cecelia._label_track_sources_cached(img, vn)
    @test b !== a && b == a                                 # a rewritten table is read again
end
