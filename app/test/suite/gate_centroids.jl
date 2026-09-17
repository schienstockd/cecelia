# ── Gate + centroids + spatial-gate-units testsets ─────────────────────
# Four sections covering: recompute! + cells_in_pop, explicit-label (pick selection)
# membership, scale_centroids! maps each axis by name, and spatial gate units. Extracted
# from suite.jl to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).

@testset "recompute! + cells_in_pop" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9], track_id=[0, 5, 9, 0, 7])

    # flow: parent (x≥0) ∩ child (x≥5)
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 0.0, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p") == [1, 2, 3, 4, 5]
    @test cells_in_pop(m, "/p/c") == [2, 3, 4, 5]         # x≥5
    @test pop_stats(m, "/p/c").pct_parent == 80.0

    # filtered (tracked) pop: track_id > 0
    mt = PopulationMap(pop_type="live", value_name="T")
    add_pop!(mt, "tracked"; filter_measure="track_id", filter_fun="gt", filter_values=0)
    recompute!(mt, _ -> df)
    @test cells_in_pop(mt, "/tracked") == [2, 3, 5]

    @test_throws ErrorException cells_in_pop(PopulationMap(), "/x")  # not recomputed
end

# ── explicit-label membership (pick selection) + transient not persisted ─
@testset "explicit-label (pick selection) membership" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9])

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # x≥5 → 2,3,4,5
    # transient pick selection of labels {2,4,9} ∩ parent(x≥5) → {2,4}
    add_pop!(m, "pick"; parent="/p", explicit_labels=[2, 4, 9],
             colour="#22d3ee", transient=true)
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p/pick") == [2, 4]              # 9 absent, 3/5 not selected

    # root-level selection (no gate parent): exactly the labels present
    m2 = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m2, "sel"; explicit_labels=[1, 3, 99], transient=true)
    recompute!(m2, _ -> df)
    @test cells_in_pop(m2, "/sel") == [1, 3]

    # transient pops are NOT written to disk, but stay in the in-memory/broadcast tree
    td = mktempdir()
    save_pop_map!(m, td)
    reloaded = load_pop_map(td, "B")
    @test !has_pop(reloaded, "/p/pick")                     # dropped on persist
    @test has_pop(reloaded, "/p")                           # real pop kept
    @test "transient" in keys(Cecelia._node_dict(m, "/p/pick"))  # flagged in broadcast tree

    # explicit-label pops carry a membership signature in the broadcast tree (no gate/filter
    # to diff on) so the client refreshes plots when the selection's cell set changes.
    nd1 = Cecelia._node_dict(m, "/p/pick")
    @test haskey(nd1, "membership_sig")
    del_pop!(m, "/p/pick")
    add_pop!(m, "pick"; parent="/p", explicit_labels=[2, 9], colour="#22d3ee", transient=true)
    @test Cecelia._node_dict(m, "/p/pick")["membership_sig"] != nd1["membership_sig"]
end

# ── scale_centroids!: THE one pixel→µm conversion (pure, no fixture needed) ────────
# The Python mirror (`label_props_utils.scale_centroids`) is asserted on the SAME numbers in
# python/cecelia/tests/test_centroid_migrate.py, so the two languages cannot drift on which axis
# scales by what.
@testset "scale_centroids! maps each axis by name" begin
    phys = [3.0, 0.5, 0.25]        # [sz, sy, sx]
    mk(; with_z=true) = begin
        d = DataFrame("label" => [1, 2], "centroid_x" => [100.0, 200.0],
                      "centroid_y" => [10.0, 20.0], "centroid_t" => [0.0, 1.0],
                      "area" => [5.0, 6.0])
        with_z && (d[!, "centroid_z"] = [4.0, 8.0])
        d
    end

    d = scale_centroids!(mk(), phys)
    @test d.centroid_x == [25.0, 50.0]      # ×sx
    @test d.centroid_y == [5.0, 10.0]       # ×sy
    @test d.centroid_z == [12.0, 24.0]      # ×sz
    # time stays a FRAME index on purpose, and non-centroid columns are untouched
    @test d.centroid_t == [0.0, 1.0]
    @test d.area == [5.0, 6.0]
    @test d.label == [1, 2]

    # 2D: with no centroid_z, x must STILL use sx. A tail-aligned implementation would give x the
    # sy value here — the silent 2D bug the by-name contract exists to prevent.
    d2 = scale_centroids!(mk(with_z=false), phys)
    @test d2.centroid_x == [25.0, 50.0]
    @test d2.centroid_y == [5.0, 10.0]
    @test !("centroid_z" in names(d2))

    # a frame with no centroid columns is a no-op, not an error
    plain = DataFrame("label" => [1], "area" => [5.0])
    @test scale_centroids!(copy(plain), phys) == plain

    # the CciaImage form reads the sizes off `meta` — same numbers, one axis at a time
    with_meta(m) = (i = CciaImage(; uid="c1", name="cal", dir=""); i.meta = Dict{String,Any}(m); i)
    let img = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        d3 = scale_centroids!(mk(), img)
        @test d3.centroid_x == [25.0, 50.0]
        @test d3.centroid_z == [12.0, 24.0]
    end
    # uncalibrated → img_physical_sizes defaults to 1.0, so the frame comes back unchanged
    let img = with_meta(Dict{String,Any}())
        @test scale_centroids!(mk(), img).centroid_x == [100.0, 200.0]
        @test !img_is_calibrated(img)
    end
    # `_pop_df_finish` is the single conversion point every pop_df branch returns through.
    let cal = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        # :pixel leaves the values alone; :physical converts
        @test Cecelia._pop_df_finish(mk(), cal, :pixel).centroid_x == [100.0, 200.0]
        @test Cecelia._pop_df_finish(mk(), cal, :physical).centroid_x == [25.0, 50.0]
        @test Cecelia._pop_df_finish(mk(), cal, false).centroid_x == [100.0, 200.0]
        # a frame with NO cell coordinates (a track-grained or branch frame) warns rather than
        # silently ignoring the argument
        trackish = DataFrame("label" => [1], "live.track.speed" => [3.0])
        @test_logs (:warn, r"no centroid_x") Cecelia._pop_df_finish(trackish, cal, :physical)
        # …and an uncalibrated image warns instead of relabelling pixels as µm
        @test_logs (:warn, r"no physical pixel size") Cecelia._pop_df_finish(
            mk(), with_meta(Dict{String,Any}()), :physical)
    end

    # calibrated: X/Y present and > 0 (Z not required — a 2D image legitimately has none)
    @test img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "", "PhysicalSizeY" => "0.5")))
end

# ── spatial gates in µm: the stamp, the eval-time scale, the portability predicate ────────
# docs/todo/SPATIAL_GATE_UNITS_PLAN.md. A position gate is stored in µm and compared against data
# scaled with THIS image's µm/px, so one gate means one physical region on every image.
@testset "spatial gate units" begin
    # ── the stamp round-trips, and a legacy file (no stamp) reads as px ──
    @test PopulationMap().spatial_unit == SPATIAL_UNIT_PX
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM)
        @test to_tree(m)["spatial_unit"] == SPATIAL_UNIT_UM
        @test from_tree(to_tree(m)).spatial_unit == SPATIAL_UNIT_UM
    end
    # no stamp ⇒ px: every gating file written before this change holds pixel coordinates and must
    # keep evaluating as pixels, so the migration is optional rather than required
    @test from_tree(Dict("pop_type" => "flow", "value_name" => "B",
                         "populations" => [])).spatial_unit == SPATIAL_UNIT_PX

    # ── a map ADOPTS µm when there is nothing to reinterpret (this replaces the migration) ──
    # The stamp only constrains a file that already holds position coordinates, so a calibrated image
    # upgrades an intensity-only map — new or long-standing — and the first position gate anyone draws is
    # already physical. A map that DOES carry a position gate keeps its unit, or its numbers would move.
    let td = mktempdir()
        img = CciaImage(uid="CAL", dir=td)
        img.meta = Dict{String,Any}("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # brand-new map on a calibrated image → µm, with that image's sizes attached
        m0 = load_pop_map(img; value_name="B", pop_type="flow")
        @test m0.spatial_unit == SPATIAL_UNIT_UM
        @test m0.physical_sizes == [1.0, 0.5, 0.25]

        # an EXISTING intensity-only file (stamped px, as every pre-change file is) is upgraded
        mpx = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(mpx, "hi"; gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))
        save_pop_map!(mpx, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_UM

        # …but one that ALREADY holds a position gate keeps px — re-stamping would move its coordinates
        msp = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(msp, "pos"; gate=RectangleGate("centroid_x", "centroid_y", 0., 500., 0., 400.))
        save_pop_map!(msp, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_PX

        # an UNCALIBRATED image adopts nothing and carries no sizes (no µm to convert to)
        u = CciaImage(uid="UNCAL", dir=mktempdir())
        u.label_props["B"] = "B.h5ad"; u.label_props["_active"] = "B"
        let mu = load_pop_map(u; value_name="B", pop_type="flow")
            @test mu.spatial_unit == SPATIAL_UNIT_PX
            @test mu.physical_sizes === nothing
        end
    end

    # ── is_spatial_axis: centroid_t is NOT spatial (a frame index carries no pixel size) ──
    @test all(is_spatial_axis, ["centroid_x", "centroid_y", "centroid_z"])
    @test !any(is_spatial_axis, ["centroid_t", "area", "mean_intensity_0", "live.cell.speed"])

    # ── recompute! scales the DATA to the gate's unit, in one place ──
    # 3 cells at x = 100/200/300 px; sx = 0.5 µm/px ⇒ 50/100/150 µm. A gate over 40–110 µm selects
    # the first two; the SAME numbers read as pixels select only the first.
    cells = DataFrame("label" => [1, 2, 3], "centroid_x" => [100.0, 200.0, 300.0],
                      "centroid_y" => [0.0, 0.0, 0.0])
    fetch = _ -> cells
    mk(unit, sizes) = begin
        m = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=unit, physical_sizes=sizes)
        add_pop!(m, "sel"; gate=RectangleGate("centroid_x", "centroid_y", 40.0, 110.0, -1.0, 1.0))
        recompute!(m, fetch)
        m
    end
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, [1.0, 0.5, 0.5]), "/sel"))) == [1, 2]
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_PX, [1.0, 0.5, 0.5]), "/sel"))) == [1]
    # a µm map on an UNCALIBRATED image (no sizes) falls back to pixels rather than inventing a scale
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, nothing), "/sel"))) == [1]
    # the caller's frame is never mutated by the scaling (recompute! copies)
    @test cells.centroid_x == [100.0, 200.0, 300.0]
    # an intensity-only gate is untouched by any of this
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM, physical_sizes=[1.0, 0.5, 0.5])
        add_pop!(m, "hi"; gate=RectangleGate("area", "perim", 5.0, 15.0, 0.0, 100.0))
        recompute!(m, _ -> DataFrame("label" => [1, 2, 3], "area" => [1.0, 10.0, 20.0],
                                     "perim" => [1.0, 1.0, 1.0]))
        @test sort(collect(cells_in_pop(m, "/hi"))) == [2]
    end

    # ── has_spatial_gate: which strategies need the target image to be calibrated to copy ──
    g(f) = (m = PopulationMap(); f(m); m)
    @test has_spatial_gate(g(m -> add_pop!(m, "s";
        gate=RectangleGate("centroid_x", "centroid_y", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "m";      # y axis alone is enough
        gate=RectangleGate("area", "centroid_z", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "f";      # a filter on a position measure counts
        filter_measure="centroid_x", filter_fun="gt", filter_values=10)))
    @test has_spatial_gate(g(m -> add_pop!(m, "c"; filter_conditions=[
        (; measure="area", fun="gt", values=1), (; measure="centroid_y", fun="lt", values=99)])))
    @test !has_spatial_gate(g(m -> add_pop!(m, "i";
        gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "t";     # centroid_t is not spatial
        gate=RectangleGate("centroid_t", "area", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "n";
        filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)))
    @test !has_spatial_gate(PopulationMap())
end
