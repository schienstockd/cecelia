# ── Gating engine: transforms ─────────────────────────────────────────────
@testset "Transforms" begin
    # linear is identity
    @test apply_transform(LinearTransform(), 42.0) == 42.0
    @test invert_transform(LinearTransform(), 42.0) == 42.0

    # log / asinh round-trip
    lg = LogTransform()
    @test invert_transform(lg, apply_transform(lg, 1234.0)) ≈ 1234.0
    ah = AsinhTransform(cofactor=150.0)
    for x in (-500.0, 0.0, 37.0, 9000.0)
        @test invert_transform(ah, apply_transform(ah, x)) ≈ x atol=1e-6
    end

    # Logicle golden values — provenance:
    #   Generated once from FlowUtils' reference C implementation (`logicle_c`, the C port
    #   of Moore & Parks 2012), params T=262144 W=0.5 M=4.5 A=0, via:
    #     flowutils.transforms.logicle(x, channel_indices=[0], t=262144, m=4.5, w=0.5, a=0)
    #   flowutils was used transiently only to produce these numbers; it is NOT a runtime
    #   dependency. The values are baked in below so this test needs no Python at runtime.
    #   See app/src/gating/transforms.jl for the full citation.
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    golden = [(-1000.0, -0.2321153540), (-100.0, 0.0090411347), (0.0, 0.1111111111),
              (1.0, 0.1122315321), (10.0, 0.1223042757), (100.0, 0.2131810875),
              (1000.0, 0.4543375762), (10000.0, 0.6838326572),
              (100000.0, 0.9069275915), (262144.0, 1.0)]
    for (x, gy) in golden
        @test apply_transform(lc, x) ≈ gy atol=1e-6
    end
    # round-trip
    for x in (-1000.0, 0.0, 1.0, 1000.0, 262144.0)
        @test invert_transform(lc, apply_transform(lc, x)) ≈ x atol=1e-4
    end
    # vectorised
    @test apply_transform(lc, [0.0, 262144.0]) ≈ [0.1111111111, 1.0] atol=1e-6

    # Range-based auto-linearisation: logicle collapses a bounded 0–1 measure (morphology) but
    # spreads a real intensity range → effective_transform swaps to linear only for the former.
    @test transform_kind(lc) == "logicle"
    @test transform_collapses(lc, 0.02, 1.0)              # solidity ∈ [0,1] → collapses
    @test !transform_collapses(lc, 0.0, 262144.0)         # full intensity range → fine
    @test !transform_collapses(lc, 0.0, 5000.0)           # large-range morphology (area) → keep logicle
    @test effective_transform(lc, 0.02, 1.0) isa LinearTransform
    @test effective_transform(lc, 0.0, 262144.0) === lc   # untouched
    @test effective_transform(LinearTransform(), 0.02, 1.0) isa LinearTransform  # linear never coerces
    @test !transform_collapses(LinearTransform(), 0.02, 1.0)
    # log needs ≥1 decade above the floor; asinh coerces when all data is inside its ~linear core
    @test transform_collapses(LogTransform(floor=1.0), 0.5, 5.0)
    @test !transform_collapses(LogTransform(floor=1.0), 1.0, 1e5)
    @test transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 1.0)
    @test !transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 5000.0)
    # degenerate extent (single value / non-finite) → no coercion, keep requested
    @test effective_transform(lc, 1.0, 1.0) === lc
    @test effective_transform(lc, NaN, 1.0) === lc
end

# ── Gating engine: gates ──────────────────────────────────────────────────
@testset "Gates" begin
    # rectangle (linear): inclusive bounds
    rg = RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0)
    xin = inside(rg, [5.0, 11.0, -1.0, 0.0], [5.0, 5.0, 5.0, 0.0])
    @test xin == BitVector([true, false, false, true])

    # polygon point-in-polygon (unit square)
    sq = [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
    @test point_in_polygon(0.5, 0.5, sq)
    @test !point_in_polygon(1.5, 0.5, sq)
    @test !point_in_polygon(-0.1, 0.5, sq)
    pg = PolygonGate("x", "y", sq)
    @test inside(pg, [0.5, 2.0], [0.5, 2.0]) == BitVector([true, false])

    # transformed-space gate: a rectangle in logicle coords selects by raw value
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # logicle(1000)=0.4543, logicle(100000)=0.9069 → gate [0.45,0.95] keeps 1000 & 100000, not 10
    rgl = RectangleGate("x", "y", 0.45, 0.95, 0.45, 0.95;
                        x_transform=lc, y_transform=lc)
    keep = inside(rgl, [10.0, 1000.0, 100000.0], [1000.0, 1000.0, 100000.0])
    @test keep == BitVector([false, true, true])

    # JSON round-trip preserves membership
    for g in (rg, pg, rgl)
        g2 = gate_from_spec(gate_spec(g))
        @test inside(g2, [5.0, 1000.0], [5.0, 1000.0]) == inside(g, [5.0, 1000.0], [5.0, 1000.0])
    end

    # project_gate: re-express a gate's stored geometry into a DISPLAY transform so its outline
    # aligns with points drawn in that transform (the client has no transform math).
    lcp = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # a rectangle stored in logicle coords, projected onto a LINEAR display → raw bounds
    rgp = RectangleGate("cd4", "cd8", 0.4543375762, 0.9069275915, 0.4543375762, 0.9069275915;
                        x_transform=lcp, y_transform=lcp)
    lin = LinearTransform()
    pj = project_gate(rgp, "cd4", "cd8", lin, lin)
    @test pj["kind"] == "rectangle"
    @test pj["x_min"] ≈ 1000.0 atol=1e-2      # invert(logicle, 0.4543) ≈ 1000
    @test pj["x_max"] ≈ 100000.0 atol=1e-1    # invert(logicle, 0.9069) ≈ 100000
    # projecting onto the SAME transform is (near) identity
    same = project_gate(rgp, "cd4", "cd8", lcp, lcp)
    @test same["x_min"] ≈ 0.4543375762 atol=1e-4
    # swapped channel order → x/y transposed
    sw = project_gate(rgp, "cd8", "cd4", lin, lin)
    @test sw !== nothing
    @test sw["y_min"] ≈ 1000.0 atol=1e-2
    # not on this channel pair → nothing
    @test project_gate(rgp, "cd4", "cd19", lin, lin) === nothing
    # polygon vertices map pointwise — corner count is preserved under ANY display transform.
    # The client sends this list back on any handle drag, so densifying here would silently multiply
    # the vertex count on every cross-transform round-trip until the gate is un-editable.
    pgp = PolygonGate("cd4", "cd8", [(0.4543375762, 0.4543375762), (0.9069275915, 0.4543375762),
                                     (0.9069275915, 0.9069275915)]; x_transform=lcp, y_transform=lcp)
    pjp = project_gate(pgp, "cd4", "cd8", lin, lin)
    @test pjp["kind"] == "polygon"
    @test length(pjp["vertices"]) == 3                  # NEVER more than the stored corners
    @test pjp["vertices"][1][1] ≈ 1000.0 atol=1e-2       # first corner mapped through inverse logicle
    @test length(project_gate(pgp, "cd4", "cd8", lcp, lcp)["vertices"]) == 3
end

# ── Gating engine: density ────────────────────────────────────────────────
@testset "Density" begin
    x = collect(0.0:0.01:1.0)
    d = density_2d(x, x; bins=10)
    @test sum(d.counts) == length(x)          # every point counted once
    @test size(d.counts) == (10, 10)
    @test d.counts[1, 10] == 0                 # off-diagonal empty (x==y data)
    @test all(d.counts[i, i] >= 1 for i in 1:10)

    # NaN/Inf (object/morphology measures on degenerate objects) must be skipped, not throw —
    # extents come from the finite values, and a non-finite point contributes to no bin.
    xn = [0.0, 0.5, 1.0, NaN, Inf, -Inf]
    dn = density_2d(xn, xn; bins=10)
    @test sum(dn.counts) == 3                  # only the 3 finite points counted
    @test (dn.x_min, dn.x_max) == (0.0, 1.0)   # extents ignore NaN/Inf
    # all-non-finite → falls back to the default extent and empty counts (no throw)
    dz = density_2d([NaN, Inf], [NaN, Inf]; bins=4)
    @test sum(dz.counts) == 0
end

# ── Population manager: paths, tree, persistence ──────────────────────────
@testset "Population manager" begin
    @test pop_parent("/a/b") == "/a"
    @test pop_parent("/a") == ROOT
    @test pop_name("/a/b") == "b"
    @test pop_path("/a", "b") == "/a/b"
    @test pop_path(ROOT, "a") == "/a"
    @test is_root(ROOT) && is_root("/") && !is_root("/a")

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10), colour="#f00")
    add_pop!(m, "cd8"; parent="/cd4", gate=PolygonGate("x", "y", [(0.,0.),(0.,5.),(5.,5.),(5.,0.)]))
    @test pop_paths(m) == ["/cd4", "/cd4/cd8"]
    @test direct_children(m, ROOT) == ["/cd4"]
    @test descendants(m, "/cd4") == ["/cd4/cd8"]

    # save/load round-trip (tree + gate)
    td = mktempdir()
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"))
    # atomic write: the temp file is renamed into place, never left behind
    @test !isfile(gating_path(td, "B") * ".tmp")
    m2 = load_pop_map(td, "B")
    @test pop_paths(m2) == pop_paths(m)
    @test pop_at(m2, "/cd4").colour == "#f00"
    @test pop_at(m2, "/cd4").gate isa RectangleGate
    @test pop_at(m2, "/cd4/cd8").gate isa PolygonGate

    # cascade rename
    rename_pop!(m, "/cd4", "tcell")
    @test Set(pop_paths(m)) == Set(["/tcell", "/tcell/cd8"])
    @test pop_at(m, "/tcell/cd8").parent == "/tcell"
    # cascade delete
    del_pop!(m, "/tcell")
    @test isempty(pop_paths(m))
end

# ── Re-parenting a population: move_pop! ─────────────────────────────────────
# The UI (PopulationManager's ⋯ → "Move under…", pop/move) lifts a population out of the gate it was
# drawn under — the "I gated B under qc and now want it against all cells" case, which used to mean
# deleting the whole branch and redrawing it. The pop keeps its gate, name, colour and children; what
# changes is its PARENT, and with it its membership (a pop is its own gate ∩ its parent's).
@testset "move_pop! re-parents a population and its subtree" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 100.0, 0.0, 100.0), colour="#0f0")
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 100.0, 0.0, 100.0))

    # membership BEFORE: B is capped by qc's gate, so the far cell (20, 20) can't be in it
    df = DataFrame("label" => [1, 2, 3], "x" => [1.0, 7.0, 20.0], "y" => [1.0, 7.0, 20.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/qc/B")) == Set([1, 2])

    @test move_pop!(m, "/qc/B", ROOT) == "/B"
    @test Set(pop_paths(m)) == Set(["/qc", "/B", "/B/mem+"])
    @test pop_at(m, "/B").parent == ROOT
    @test pop_at(m, "/B").name == "B"
    @test pop_at(m, "/B").colour == "#0f0"                   # identity untouched
    @test pop_at(m, "/B").gate isa RectangleGate             # gate untouched
    @test pop_at(m, "/B/mem+").parent == "/B"                # the subtree came along
    # `order` keeps parents before children, so the moved subtree follows its new parent
    @test findfirst(==("/B"), pop_paths(m)) < findfirst(==("/B/mem+"), pop_paths(m))

    # membership AFTER: no longer capped by qc — the same gate now takes the far cell too
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/B")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/B/mem+")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/qc")) == Set([1, 2])         # the old parent is unaffected

    # and back under a gate again (the other direction)
    @test move_pop!(m, "/B", "/qc") == "/qc/B"
    @test pop_at(m, "/qc/B/mem+").parent == "/qc/B"
    @test move_pop!(m, "/qc/B", "/qc") == "/qc/B"            # already there → no-op, not an error

    # the move survives a save/load round-trip (the tree is nested, so the parent IS the position)
    td = mktempdir(); save_pop_map!(m, td)
    @test Set(pop_paths(load_pop_map(td, "B"))) == Set(["/qc", "/qc/B", "/qc/B/mem+"])
end

# ── Pop UIDs — the stable id every outside reference (obs columns, external notes) points at ─────
# Reintroduces the R version's `popID` (MULTI_POP_TRACKING_PLAN.md Decision 0). Path is the DISPLAY
# identity (what the UI shows); UID is the DURABLE one that outlives a rename. Two invariants matter
# and are pinned here: UID survives rename_pop!/move_pop! (rename ≠ new pop), and a legacy sidecar
# without any UIDs is one-shot-backfilled at load and saved so the next load gets the SAME uids.
@testset "population UIDs survive rename + move + save/load round-trips" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    add_pop!(m, "cd8"; parent="/cd4", gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid4 = pop_uid(m, "/cd4"); uid8 = pop_uid(m, "/cd4/cd8")
    @test length(uid4) == 6 && length(uid8) == 6 && uid4 != uid8
    # reverse lookup
    @test pop_path_by_uid(m, uid4) == "/cd4"
    @test pop_by_uid(m, uid4).path == "/cd4"
    @test pop_path_by_uid(m, "missing") === nothing
    # rename: uid stays, path shifts, index updates
    rename_pop!(m, "/cd4", "tcell")
    @test pop_uid(m, "/tcell") == uid4
    @test pop_uid(m, "/tcell/cd8") == uid8            # descendant's uid unchanged
    @test pop_path_by_uid(m, uid4) == "/tcell"
    # move: uid stays, path shifts, index updates
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    move_pop!(m, "/tcell", "/qc")
    @test pop_uid(m, "/qc/tcell") == uid4
    @test pop_path_by_uid(m, uid4) == "/qc/tcell"
    # save/load round-trip preserves uids AS-IS (no reroll)
    td = mktempdir(); save_pop_map!(m, td)
    m2 = load_pop_map(td, "B")
    @test pop_uid(m2, "/qc/tcell") == uid4
    @test pop_uid(m2, "/qc/tcell/cd8") == uid8
    # delete drops the reverse-lookup entry so a stale uid resolves to nothing rather than a valid path
    del_pop!(m2, "/qc/tcell")
    @test pop_path_by_uid(m2, uid4) === nothing
    @test pop_path_by_uid(m2, uid8) === nothing       # descendants dropped along with the parent
end

@testset "legacy sidecar without UIDs is backfilled and saved on first load" begin
    td = mktempdir()
    mkpath(joinpath(td, "gating"))
    # a hand-written sidecar carrying no `uid` on any node — what every project shipped before this
    write(joinpath(td, "gating", "B.json"), """
    {"pop_type":"flow","value_name":"B","populations":[
      {"name":"cd4","colour":"#f00","show":true,
       "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
               "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
               "x_min":0,"x_max":1,"y_min":0,"y_max":1},
       "children":[
         {"name":"cd8","colour":"#0f0","show":true,
          "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
                  "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
                  "x_min":0,"x_max":1,"y_min":0,"y_max":1},"children":[]}
       ]}
    ]}
    """)
    # first load backfills + saves; a second load reads back the SAME uids (stable across sessions)
    m1 = load_pop_map(td, "B")
    u4a = pop_uid(m1, "/cd4"); u8a = pop_uid(m1, "/cd4/cd8")
    @test length(u4a) == 6 && length(u8a) == 6
    m2 = load_pop_map(td, "B")
    @test pop_uid(m2, "/cd4") == u4a
    @test pop_uid(m2, "/cd4/cd8") == u8a
    @test m2._uid_backfilled == false                 # nothing to backfill this time
end

# ── Retired UIDs (MULTI_POP_TRACKING_ORPHANS_PLAN P2) ──────────────────────────────
# A pop's UID outlives the pop as long as the segmentation's h5ad still carries `track_source`
# rows stamped with it. Reusing that UID for a new pop would silently transfer the old orphan
# rows to the new pop on its first tracking run. `_del_paths!` retires the UID; `_fresh_pop_uid`
# refuses to reissue it; the set round-trips through the sidecar so retirement survives sessions.
@testset "deleted pop's UID is retired and never reissued" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid_a = pop_uid(m, "/a")
    @test uid_a ∉ m.retired_uids                       # live pops don't belong in the set

    del_pop!(m, "/a")
    @test uid_a in m.retired_uids                      # retirement is on delete

    # `_fresh_pop_uid` must refuse the retired UID even when explicitly preferred (a hand-edited
    # sidecar that resurrected it). Rerolls to something fresh instead of colliding.
    reused = Cecelia._fresh_pop_uid(m; preferred=uid_a)
    @test reused != uid_a
    @test reused ∉ m.retired_uids

    # And every future add_pop! is guaranteed to skip retired UIDs — driven statistically by the
    # birthday probability, guaranteed by the guard. Add 20 pops and none can land on `uid_a`.
    for i in 1:20
        add_pop!(m, "p$i"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
        @test pop_uid(m, "/p$i") != uid_a
    end
end

@testset "retired UIDs round-trip through the sidecar" begin
    td = mktempdir()
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    add_pop!(m, "b"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid_a = pop_uid(m, "/a"); uid_b = pop_uid(m, "/b")
    del_pop!(m, "/a")
    del_pop!(m, "/b")
    save_pop_map!(m, td)

    m2 = load_pop_map(td, "B")
    @test uid_a in m2.retired_uids
    @test uid_b in m2.retired_uids
    # The guard still applies AFTER a load — the retired set is authoritative in the fresh session.
    @test Cecelia._fresh_pop_uid(m2; preferred=uid_a) != uid_a
end

@testset "legacy sidecar without retired_uids loads with an empty set" begin
    td = mktempdir()
    mkpath(joinpath(td, "gating"))
    # An emitted sidecar from before the P2 ship — no `retired_uids` key at all.
    write(joinpath(td, "gating", "B.json"), """
    {"pop_type":"flow","value_name":"B","populations":[
      {"name":"a","uid":"abc123","colour":"#f00","show":true,
       "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
               "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
               "x_min":0,"x_max":1,"y_min":0,"y_max":1},"children":[]}
    ]}
    """)
    m = load_pop_map(td, "B")
    @test isempty(m.retired_uids)                      # legacy → empty, no exception
    @test pop_uid(m, "/a") == "abc123"                 # live UID untouched by the P2 path
end

@testset "retired_uids only appears in the sidecar when non-empty (visual cleanliness)" begin
    td = mktempdir()
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    save_pop_map!(m, td)
    # No deletions → no `retired_uids` key emitted. Byte-visible on the JSON.
    raw = read(joinpath(td, "gating", "B.json"), String)
    @test !occursin("retired_uids", raw)

    del_pop!(m, "/a")
    save_pop_map!(m, td)
    raw2 = read(joinpath(td, "gating", "B.json"), String)
    @test occursin("retired_uids", raw2)
end

@testset "move_pop! rejects a cycle, a collision and an unknown target" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "B";  parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))  # same NAME at root

    # a population cannot become its own descendant's child (that would orphan the whole subtree)
    @test_throws ErrorException move_pop!(m, "/qc/B", "/qc/B/mem+")
    @test_throws ErrorException move_pop!(m, "/qc/B", "/qc/B")
    # …nor land where a population of that name already sits
    @test_throws ErrorException move_pop!(m, "/qc/B", ROOT)
    @test_throws ErrorException move_pop!(m, "/qc/B", "/nope")
    @test_throws ErrorException move_pop!(m, "/nope", ROOT)
    # every rejection left the tree exactly as it was
    @test Set(pop_paths(m)) == Set(["/qc", "/qc/B", "/qc/B/mem+", "/B"])
end

# ── del_children!: prune BELOW a population, keep the population ──────────────
# The other half of del_pop! (⋯ → "Delete N below it"): re-gating from a gate you want to keep,
# without redrawing that gate. One call, so one undo step.
@testset "del_children! prunes the subtree and keeps the pop" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0))
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0))

    del_children!(m, "/qc")
    @test pop_paths(m) == ["/qc"]                            # the whole branch below it went
    @test pop_at(m, "/qc").colour == "#f00"                  # the pop itself survived intact
    del_children!(m, "/qc")                                  # already a leaf → no-op, not an error
    @test pop_paths(m) == ["/qc"]
    @test_throws ErrorException del_children!(m, "/nope")

    # membership of the kept pop is unchanged (nothing below it ever fed into it)
    df = DataFrame("label" => [1, 2], "x" => [1.0, 20.0], "y" => [1.0, 20.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/qc")) == Set([1])
end

# ── Boolean populations (Decision 16): a pop defined by combining OTHER pops ──
# The ask: "gate cells positive for nuc-GFP or mem-TOM", and "mem-TOM+ AND nuc-GFP+ but NOT
# CD169-Kat+" — combinations no single 2D gate can express. One form covers all of it: included
# terms combined with AND or OR, minus every excluded term, still ∩ the parent like any other pop.
@testset "boolean populations combine other populations" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    # three gates on the same axes, each admitting a different slice of the four cells below
    add_pop!(m, "gfp+"; parent=ROOT, gate=RectangleGate("g", "t", 1.0, 100.0, -100.0, 100.0))
    add_pop!(m, "tom+"; parent=ROOT, gate=RectangleGate("g", "t", -100.0, 100.0, 1.0, 100.0))
    add_pop!(m, "kat+"; parent=ROOT, gate=RectangleGate("k", "k", 1.0, 100.0, 1.0, 100.0))
    #                    label:      1        2        3        4
    df = DataFrame("label" => [1, 2, 3, 4],
                   "g" => [5.0, 0.0, 5.0, 0.0],      # gfp+: 1, 3
                   "t" => [0.0, 5.0, 5.0, 0.0],      # tom+: 2, 3
                   "k" => [0.0, 0.0, 5.0, 0.0])      # kat+: 3
    fetch = _ -> df

    # OR — the "positive for either marker" ask
    add_pop!(m, "gfp+ or tom+"; parent=ROOT, boolean_op="or", boolean_pops=["/gfp+", "/tom+"])
    # AND minus an exclusion — the "double positive but not CD169" ask
    add_pop!(m, "dp not kat"; parent=ROOT, boolean_op="and",
             boolean_pops=["/gfp+", "/tom+"], boolean_not=["/kat+"])
    # a plain NOT gate: no included terms ⇒ everything in the parent except the excluded one
    add_pop!(m, "not kat"; parent=ROOT, boolean_op="not", boolean_pops=["/kat+"])

    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/gfp+ or tom+")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/dp not kat")) == Set{Int}()      # only cell 3 is double+, and it is kat+
    @test Set(cells_in_pop(m, "/not kat")) == Set([1, 2, 4])
    # "not" is stored as an exclusion, not a third operator
    @test pop_at(m, "/not kat").boolean_op == Cecelia.BOOL_AND
    @test pop_at(m, "/not kat").boolean_pops == String[]
    @test pop_at(m, "/not kat").boolean_not == ["/kat+"]

    # ∩ parent still applies: the same combination under a gate is capped by that gate
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("g", "t", -100.0, 100.0, 1.0, 100.0))  # = tom+
    add_pop!(m, "either"; parent="/qc", boolean_op="or", boolean_pops=["/gfp+", "/tom+"])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/qc/either")) == Set([2, 3])

    # a boolean pop can combine boolean pops — and evaluation order is a DEPENDENCY, not depth: this
    # one sits at depth 1 and references "/qc/either" at depth 2, so plain depth order would derive
    # it before the thing it needs.
    add_pop!(m, "chained"; parent=ROOT, boolean_op="and", boolean_pops=["/qc/either", "/not kat"])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/chained")) == Set([2])

    # round-trips through the sidecar, references and all
    td = mktempdir(); save_pop_map!(m, td)
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/dp not kat").boolean_op == Cecelia.BOOL_AND
    @test pop_at(m2, "/dp not kat").boolean_pops == ["/gfp+", "/tom+"]
    @test pop_at(m2, "/dp not kat").boolean_not == ["/kat+"]
    recompute!(m2, fetch)
    @test Set(cells_in_pop(m2, "/gfp+ or tom+")) == Set([1, 2, 3])
end

@testset "BoolMembership enum — boundary coercion + JSON round-trip" begin
    # boolean_op is stored as a `BoolMembership` enum (BOOL_AND / BOOL_OR); string kwargs at the
    # add_pop!/set_boolean!/JSON-load boundaries are coerced through `_normalise_boolean`, and the
    # sidecar's `"boolean.op"` writes the lowercase wire string via `Base.string(::BoolMembership)`.
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))
    add_pop!(m, "b"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))

    # string kwarg → enum stored, both operators
    add_pop!(m, "and_pop"; parent=ROOT, boolean_op="and", boolean_pops=["/a", "/b"])
    add_pop!(m, "or_pop";  parent=ROOT, boolean_op="or",  boolean_pops=["/a", "/b"])
    @test pop_at(m, "/and_pop").boolean_op === Cecelia.BOOL_AND
    @test pop_at(m, "/or_pop").boolean_op  === Cecelia.BOOL_OR

    # passing the enum itself also works — accepts BoolMembership | AbstractString | Symbol
    add_pop!(m, "enum_pop"; parent=ROOT, boolean_op=Cecelia.BOOL_OR, boolean_pops=["/a"])
    @test pop_at(m, "/enum_pop").boolean_op === Cecelia.BOOL_OR

    # sidecar serialises the wire string, not the enum name (`"or"`, not `"BOOL_OR"`)
    td = mktempdir(); save_pop_map!(m, td)
    raw = JSON3.read(read(gating_path(td, "B"), String))
    ops = String[]
    walk = node -> begin
        b = get(node, :boolean, nothing)
        b === nothing || push!(ops, String(get(b, :op, "")))
        for c in get(node, :children, [])
            walk(c)
        end
    end
    for root in raw.populations
        walk(root)
    end
    @test Set(ops) == Set(["and", "or", "or"])
    @test all(o in ("and", "or") for o in ops)   # no "BOOL_AND"/"BOOL_OR" leaking to disk

    # and read back — the loader must coerce the sidecar string into the enum
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/and_pop").boolean_op === Cecelia.BOOL_AND
    @test pop_at(m2, "/or_pop").boolean_op  === Cecelia.BOOL_OR

    # parse_bool_membership rejects garbage with an ArgumentError
    @test Cecelia.parse_bool_membership("and") === Cecelia.BOOL_AND
    @test Cecelia.parse_bool_membership("or")  === Cecelia.BOOL_OR
    @test_throws ArgumentError Cecelia.parse_bool_membership("xor")

    # exported backward-compat surface: BOOLEAN_OPS still enumerates the wire strings
    @test Set(Cecelia.BOOLEAN_OPS) == Set(("and", "or"))
end

@testset "boolean references follow rename/move, and refuse to loop" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "gfp+"; parent="/qc", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "tom+"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "either"; parent=ROOT, boolean_op="or", boolean_pops=["/qc/gfp+", "/tom+"])

    # a reference is a PATH, so renaming or re-parenting a combined pop must rewrite it — otherwise
    # the combination silently loses a term the moment either gate is renamed.
    rename_pop!(m, "/qc/gfp+", "GFP+")
    @test pop_at(m, "/either").boolean_pops == ["/qc/GFP+", "/tom+"]
    move_pop!(m, "/qc/GFP+", ROOT)
    @test pop_at(m, "/either").boolean_pops == ["/GFP+", "/tom+"]

    # self-reference and loops are rejected, at creation and at edit
    @test_throws ErrorException add_pop!(m, "self"; parent=ROOT, boolean_op="not", boolean_pops=["/self"])
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/either"])
    add_pop!(m, "second"; parent=ROOT, boolean_op="and", boolean_pops=["/either"])
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/second"])   # A→B→A
    # …as are an unknown reference, an unknown operator and an empty term list
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="or", boolean_pops=["/nope"])
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="xor", boolean_pops=["/tom+"])
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="or", boolean_pops=String[])
    # a pop cannot combine its own descendant either (that descendant already depends on it)
    add_pop!(m, "child"; parent="/either", gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/either/child"])
    @test Set(pop_paths(m)) == Set(["/qc", "/GFP+", "/tom+", "/either", "/second", "/either/child"])

    # who would be left dangling by a delete — what the API refuses on (rename/move can't orphan)
    @test boolean_dependents(m, ["/tom+"]) == ["/either" => ["/tom+"]]
    @test isempty(boolean_dependents(m, ["/qc"]))
    # a dangling reference (only reachable by hand-editing a sidecar) degrades to an empty pop,
    # never a crash — the same rule as a missing column
    m.pops["/either"].boolean_pops = ["/gone"]
    df = DataFrame("label" => [1], "x" => [1.0], "y" => [1.0])
    recompute!(m, _ -> df)
    @test isempty(cells_in_pop(m, "/either"))
end

# ── Gate shape swap: rectangle ⇄ polygon on an EXISTING population ────────────
# The UI (PopulationManager's convert button, frontend plots/gateGeometry.ts) changes a gate's shape
# by pushing a new spec of the OTHER kind through pop/set-gate. `set_gate!` must therefore accept a
# gate of a different concrete type on a live pop — the population, its children and its identity
# survive; only the geometry and the derived membership change. Without this the only way to redraw
# a rectangle as a polygon is delete + redraw, which takes the children with it.
@testset "set_gate! swaps the gate KIND in place" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "cd8"; parent="/cd4", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0), colour="#0f0")

    # the four corners of the rectangle, as a polygon → the SAME region (what rect → poly does)
    corners = [(0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0)]
    set_gate!(m, "/cd4", PolygonGate("x", "y", corners))
    @test pop_at(m, "/cd4").gate isa PolygonGate
    @test pop_at(m, "/cd4").colour == "#f00"                 # identity untouched
    @test pop_paths(m) == ["/cd4", "/cd4/cd8"]               # child still hanging off it
    @test pop_at(m, "/cd4/cd8").gate isa RectangleGate       # and its own gate untouched

    # membership is re-derived, and the corner polygon selects what the rectangle did
    df = DataFrame("label" => [1, 2, 3, 4],
                   "x" => [1.0, 7.0, 20.0, 3.0], "y" => [1.0, 7.0, 20.0, 3.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/cd4")) == Set([1, 2, 4])     # 3 is outside
    @test Set(cells_in_pop(m, "/cd4/cd8")) == Set([1, 4])    # child gate still applies under it

    # and back the other way (poly → its bounding box): a WIDENING swap, still in place
    set_gate!(m, "/cd4", RectangleGate("x", "y", 0.0, 25.0, 0.0, 25.0))
    recompute!(m, _ -> df)
    @test pop_at(m, "/cd4").gate isa RectangleGate
    @test Set(cells_in_pop(m, "/cd4")) == Set([1, 2, 3, 4])  # bbox picked up the outlier

    # the swap survives a save/load round-trip (the sidecar carries "kind")
    td = mktempdir()
    set_gate!(m, "/cd4", PolygonGate("x", "y", corners)); save_pop_map!(m, td)
    @test pop_at(load_pop_map(td, "B"), "/cd4").gate isa PolygonGate
end

