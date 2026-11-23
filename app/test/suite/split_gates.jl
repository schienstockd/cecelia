# ── Split gates: slice (a dividing line, straight or bent) and quadrant ────────
# app/src/gating/split_gates.jl. Expected regions come from the geometry definition (left of the line's
# direction; sector from arm q to arm q+1 counter-clockwise), not from the code's output.

@testset "slice gate: sides of a straight line, extended past its ends" begin
    # the line y = 0 drawn left → right: its LEFT is above
    up = SliceGate("x", "y", [(-1.0, 0.0), (1.0, 0.0)]; side = 1)
    dn = SliceGate("x", "y", [(-1.0, 0.0), (1.0, 0.0)]; side = -1)
    x = [0.0, 0.0, 50.0, -50.0, 50.0, NaN]
    y = [1.0, -1.0, 3.0, -2.0, -0.1, 1.0]
    @test inside(up, x, y) == BitVector([1, 0, 1, 0, 0, 0])     # (50, 3): beyond the drawn end, still above
    @test inside(dn, x, y) == BitVector([0, 1, 0, 1, 1, 0])
    # the two sides tile every finite cell; a NaN cell is in neither (as for rectangle / polygon)
    @test all(inside(up, x, y) .⊻ inside(dn, x, y) .== isfinite.(x) .& isfinite.(y))
end

@testset "slice gate: a bent line" begin
    # right along y = 0 to the origin, then up x = 0: left of that is the upper-left quadrant
    verts = [(-1.0, 0.0), (0.0, 0.0), (0.0, 1.0)]
    left = SliceGate("x", "y", verts; side = 1)
    right = SliceGate("x", "y", verts; side = -1)
    x = [-1.0, 1.0, -1.0, 1.0, -30.0, -0.5]
    y = [1.0, 1.0, -1.0, -1.0, 40.0, 0.2]
    @test inside(left, x, y) == BitVector([1, 0, 0, 0, 1, 1])
    @test inside(right, x, y) == .!inside(left, x, y)
    # a line whose extension crosses itself still splits into exact complements
    zig = [(0.0, 0.0), (2.0, 0.0), (2.0, 1.0), (1.0, -1.0)]
    xs = collect(-3.0:0.25:3.0); ys = repeat([-0.5, 0.5, 2.0], length(xs) ÷ 3 + 1)[1:length(xs)]
    @test inside(SliceGate("x", "y", zig; side = 1), xs, ys) == .!inside(SliceGate("x", "y", zig; side = -1), xs, ys)
end

@testset "slice gate: refuses a line that is not one" begin
    @test_throws ErrorException SliceGate("x", "y", [(0.0, 0.0)])
    @test_throws ErrorException SliceGate("x", "y", [(1.0, 1.0), (1.0, 1.0)])
    @test_throws ErrorException SliceGate("x", "y", [(0.0, 0.0), (1.0, 0.0)]; side = 0)
end

@testset "quadrant gate: sectors, axis-aligned and tilted (spider)" begin
    x = [1.0, -1.0, -1.0, 1.0, 100.0, NaN]
    y = [1.0, 1.0, -1.0, -1.0, 0.5, 0.0]
    # arms right, up, left, down → 1 = ++, 2 = -+, 3 = --, 4 = +-
    for (q, want) in ((1, [1, 0, 0, 0, 1, 0]), (2, [0, 1, 0, 0, 0, 0]), (3, [0, 0, 1, 0, 0, 0]), (4, [0, 0, 0, 1, 0, 0]))
        @test inside(QuadrantGate("x", "y", 0.0, 0.0; quadrant = q), x, y) == BitVector(want)
    end
    # tilt arm 1 up to the diagonal: (100, 0.5) is now below it, in sector 4
    arms = [(1.0, 1.0), (0.0, 1.0), (-1.0, 0.0), (0.0, -1.0)]
    @test quadrant_of(QuadrantGate("x", "y", (0.0, 0.0), arms), 100.0, 0.5) == 4
    @test quadrant_of(QuadrantGate("x", "y", (0.0, 0.0), arms), 1.0, 3.0) == 1
    # every finite cell is in exactly one sector
    xs = randn(500) .* 3; ys = randn(500) .* 3
    @test all(sum(inside(QuadrantGate("x", "y", (0.5, -0.2), arms; quadrant = q), xs, ys) for q in 1:4) .== 1)
    # arms must go round counter-clockwise without crossing, and there must be four
    @test_throws ErrorException QuadrantGate("x", "y", (0.0, 0.0), [(1.0, 0.0), (-1.0, 0.0), (0.0, 1.0), (0.0, -1.0)])
    @test_throws ErrorException QuadrantGate("x", "y", (0.0, 0.0), [(1.0, 0.0), (0.0, 1.0), (-1.0, 0.0)])
end

@testset "split gates round-trip through their spec" begin
    s = SliceGate("a", "b", [(0.0, 1.0), (2.0, 3.0), (4.0, 1.0)]; side = -1, group = "g1",
                  x_transform = AsinhTransform(cofactor = 5.0))
    s2 = gate_from_spec(gate_spec(s))
    @test s2 isa SliceGate && s2.vertices == s.vertices && s2.side == -1 && s2.group == "g1"
    @test transform_spec(s2.x_transform) == transform_spec(s.x_transform)
    q = QuadrantGate("a", "b", (1.0, 2.0), [(3.0, 2.5), (1.0, 5.0), (-1.0, 2.0), (1.0, -1.0)]; quadrant = 3, group = "g2")
    q2 = gate_from_spec(gate_spec(q))
    @test q2 isa QuadrantGate && q2.centre == q.centre && q2.arms == q.arms && q2.quadrant == 3 && q2.group == "g2"
end

@testset "project_gate: an axis swap keeps every cell in its region" begin
    xs = randn(300) .* 2; ys = randn(300) .* 2
    sl = SliceGate("a", "b", [(-1.0, -0.5), (0.3, 0.2), (1.0, 1.5)]; side = 1)
    pj = project_gate(sl, "b", "a", LinearTransform(), LinearTransform())        # shown with a on Y
    back = SliceGate("b", "a", [(v[1], v[2]) for v in pj["vertices"]]; side = pj["side"])
    @test inside(back, ys, xs) == inside(sl, xs, ys)
    qg = QuadrantGate("a", "b", (0.2, 0.1), [(1.0, 0.4), (0.0, 1.0), (-1.0, -0.3), (0.5, -1.0)]; quadrant = 2)
    for q in 1:4
        g = QuadrantGate("a", "b", qg.centre, qg.arms; quadrant = q)
        pj = project_gate(g, "b", "a", LinearTransform(), LinearTransform())
        back = QuadrantGate("b", "a", Tuple(pj["centre"]), [Tuple(a) for a in pj["arms"]]; quadrant = pj["quadrant"])
        @test inside(back, ys, xs) == inside(g, xs, ys)
    end
end

@testset "set_gate! moves every region of a split together" begin
    m = PopulationTree(pop_type = "flow", value_name = "B")
    line = [(-1.0, 0.0), (1.0, 0.0)]
    add_pop!(m, "hi"; gate = SliceGate("x", "y", line; side = 1, group = "g"))
    add_pop!(m, "lo"; gate = SliceGate("x", "y", line; side = -1, group = "g"))
    add_pop!(m, "other"; gate = SliceGate("x", "y", line; side = -1, group = "h"))   # another split: untouched
    df = DataFrame("label" => [1, 2, 3], "x" => [0.0, 0.0, 0.0], "y" => [3.0, 1.0, -1.0])

    set_gate!(m, "/hi", SliceGate("x", "y", [(-1.0, 2.0), (1.0, 2.0)]; side = 1, group = "g"))   # line up to y = 2
    @test pop_at(m, "/lo").gate.vertices == [(-1.0, 2.0), (1.0, 2.0)] && pop_at(m, "/lo").gate.side == -1
    @test pop_at(m, "/other").gate.vertices == line
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/hi") == [1] && Set(cells_in_pop(m, "/lo")) == Set([2, 3])

    # an edit made on a plot with the axes swapped: the line y = 0 seen on (y, x) is u = 0 drawn upward,
    # and "hi" (y > 0) is to its RIGHT. "lo" must follow into the swapped frame and stay y < 0.
    set_gate!(m, "/hi", SliceGate("y", "x", [(0.0, -1.0), (0.0, 1.0)]; side = -1, group = "g"))
    @test pop_at(m, "/lo").gate.x_channel == "y" && pop_at(m, "/lo").gate.side == 1
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/hi")) == Set([1, 2]) && cells_in_pop(m, "/lo") == [3]

    # quadrants: the centre moves for all four, each keeps its sector; an axis swap mirrors the sector
    add_pop!(m, "pp"; gate = QuadrantGate("x", "y", 0.0, 0.0; quadrant = 1, group = "q"))
    add_pop!(m, "mp"; gate = QuadrantGate("x", "y", 0.0, 0.0; quadrant = 2, group = "q"))
    set_gate!(m, "/pp", QuadrantGate("x", "y", 5.0, 5.0; quadrant = 1, group = "q"))
    @test pop_at(m, "/mp").gate.centre == (5.0, 5.0) && pop_at(m, "/mp").gate.quadrant == 2
    # what a swapped plot sends back: project_gate's arms — mirrored AND reversed, so still counter-
    # clockwise ([left, down, right, up] on (y, x)) — and "pp" (x+y+) as sector 3 of that list
    pj = project_gate(pop_at(m, "/pp").gate, "y", "x", LinearTransform(), LinearTransform())
    @test pj["arms"] == [[4.0, 5.0], [5.0, 4.0], [6.0, 5.0], [5.0, 6.0]] && pj["quadrant"] == 3
    set_gate!(m, "/pp", QuadrantGate("y", "x", (5.0, 5.0), [Tuple(a) for a in pj["arms"]]; quadrant = 3, group = "q"))
    @test pop_at(m, "/mp").gate.x_channel == "y" && pop_at(m, "/mp").gate.quadrant == 2
    x = [0.0, 10.0]; y = [10.0, 10.0]             # cell 1 is x-y+ (relative to 5,5), cell 2 x+y+
    @test inside(pop_at(m, "/mp").gate, y, x) == BitVector([1, 0])
end

@testset "split gates survive a save/load round-trip" begin
    m = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(m, "hi"; gate = SliceGate("x", "y", [(0.0, 0.0), (1.0, 1.0), (2.0, 0.5)]; side = 1, group = "g"))
    add_pop!(m, "q3"; gate = QuadrantGate("x", "y", 1.0, 2.0; quadrant = 3, group = "k"))
    td = mktempdir(); save_pop_tree!(m, td)
    m2 = load_pop_tree(td, "B")
    @test pop_at(m2, "/hi").gate isa SliceGate && pop_at(m2, "/hi").gate.group == "g"
    @test pop_at(m2, "/q3").gate isa QuadrantGate && pop_at(m2, "/q3").gate.quadrant == 3
end

@testset "split gates: spec defaults, group scope, history hash, label points" begin
    # a spec without its region loads as region 1 (side left / first sector), never an invalid 0
    @test gate_from_spec(Dict("kind" => "slice", "x_channel" => "x", "y_channel" => "y",
                              "vertices" => [[0, 0], [1, 0]])).side == 1
    @test gate_from_spec(Dict("kind" => "quadrant", "x_channel" => "x", "y_channel" => "y", "centre" => [0, 0],
                              "arms" => [[1, 0], [0, 1], [-1, 0], [0, -1]])).quadrant == 1

    # a rectangle shown with swapped axes: x and y bounds trade places
    pj = project_gate(RectangleGate("a", "b", 1.0, 2.0, 5.0, 9.0), "b", "a", LinearTransform(), LinearTransform())
    @test (pj["x_min"], pj["x_max"], pj["y_min"], pj["y_max"]) == (5.0, 9.0, 1.0, 2.0)

    # set_gate! moves only the group members under the SAME parent: a member moved elsewhere keeps its line
    m = PopulationTree(pop_type = "flow", value_name = "B")
    line = [(-1.0, 0.0), (1.0, 0.0)]
    add_pop!(m, "p"; gate = RectangleGate("x", "y", -9.0, 9.0, -9.0, 9.0))
    add_pop!(m, "hi"; gate = SliceGate("x", "y", line; side = 1, group = "g"))
    add_pop!(m, "far"; parent = "/p", gate = SliceGate("x", "y", line; side = -1, group = "g"))
    set_gate!(m, "/hi", SliceGate("x", "y", [(-1.0, 3.0), (1.0, 3.0)]; side = 1, group = "g"))
    @test pop_at(m, "/p/far").gate.vertices == line

    # the gate record's hash ignores the group id: the same slice drawn twice hashes equal
    a = PopulationTree(pop_type = "flow", value_name = "B"); b = PopulationTree(pop_type = "flow", value_name = "B")
    add_pop!(a, "hi"; gate = SliceGate("x", "y", line; side = 1, group = "g1"))
    add_pop!(b, "hi"; gate = SliceGate("x", "y", line; side = 1, group = "g2"))
    @test pop_gate_hashes(a)["/hi"] == pop_gate_hashes(b)["/hi"]
    add_pop!(b, "lo"; gate = SliceGate("x", "y", line; side = -1, group = "g2"))
    @test pop_gate_hashes(b)["/lo"] != pop_gate_hashes(b)["/hi"]       # …but the side still counts

    # the gate-plot image names each region INSIDE its own region (a centred axis-aligned quadrant, a slice)
    box = (0.0, 10.0, 0.0, 10.0)
    for q in 1:4
        pj = Dict{String,Any}("kind" => "quadrant", "centre" => [5.0, 5.0],
                              "arms" => [[6.0, 5.0], [5.0, 6.0], [4.0, 5.0], [5.0, 4.0]], "quadrant" => q)
        at = Cecelia.split_label_point(pj, box)
        @test quadrant_of(QuadrantGate("x", "y", 5.0, 5.0), at...) == q
    end
    up = Cecelia.split_label_point(Dict{String,Any}("kind" => "slice", "vertices" => [[0.0, 5.0], [10.0, 5.0]], "side" => 1), box)
    dn = Cecelia.split_label_point(Dict{String,Any}("kind" => "slice", "vertices" => [[0.0, 5.0], [10.0, 5.0]], "side" => -1), box)
    @test up[2] > 5 && dn[2] < 5
    # a region wholly out of view has no label
    @test Cecelia.split_label_point(Dict{String,Any}("kind" => "quadrant", "centre" => [20.0, 20.0],
        "arms" => [[21.0, 20.0], [20.0, 21.0], [19.0, 20.0], [20.0, 19.0]], "quadrant" => 1), box) === nothing
end
