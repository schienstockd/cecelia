# ── The gate-plot read: `pop_plot_cols` / `computed_pop_map` / `pop_membership_fetch` ──
# The gating API's plots (plotdata/plotmeta/density/summary/plot-image) read through `pop_plot_cols`,
# which is `pop_df` plus gate units. These pin its contract on the real KDIeEm fixture against truth
# read straight off the table: table order, one aligned read, per-column empty, unknown pop → empty,
# the map hook, the labels-version pin, gate units, and track gates on aggregates nobody plotted.

function _ppc_image(; calibrated::Bool=false, tracks::Bool=false)
    lp = fixture_path("testpr", "1", "KDIeEm", "labelProps")
    td = mktempdir(); mkpath(joinpath(td, "labelProps"))
    cp(joinpath(lp, "B.h5ad"), joinpath(td, "labelProps", "B.h5ad"))
    tracks && cp(joinpath(lp, "B__tracks.h5ad"), joinpath(td, "labelProps", "B__tracks.h5ad"))
    img = CciaImage(uid="KDIeEm", dir=td)
    img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
    calibrated && (img.meta["PhysicalSizeX"] = "0.5"; img.meta["PhysicalSizeY"] = "0.7")
    img
end

@testset "pop_plot_cols — the gate-plot read (KDIeEm)" begin
    if !have_fixture(fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad"))
        @test_skip "pop_plot_cols (fixture missing)"
    else
        img = _ppc_image()
        full = label_props(img; value_name="B") |>
               select_cols(["mean_intensity_0", "area", "centroid_x"]) |> as_df
        @test nrow(full) == 1377

        # root: every row, in TABLE order, one vector per requested column (duplicates allowed)
        v = pop_plot_cols(img, "flow", ROOT, ["mean_intensity_0", "area", "mean_intensity_0", "label"];
                          value_name="B")
        @test v[1] == Float64.(full.mean_intensity_0) && v[3] == v[1]
        @test v[2] == Float64.(full.area) && v[4] == Float64.(full.label)
        # a column the table lacks is EMPTY, per column — it never blanks the others
        v = pop_plot_cols(img, "flow", ROOT, ["area", "not_a_col"]; value_name="B")
        @test length(v[1]) == 1377 && isempty(v[2])

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "dim"; gate=RectangleGate("mean_intensity_0", "area", 0.0, 1000.0, 0.0, 1e9))
        add_pop!(m, "none"; gate=RectangleGate("mean_intensity_0", "area", -2.0, -1.0, 0.0, 1e9))
        save_pop_map!(m, img)
        keep = (full.mean_intensity_0 .>= 0) .& (full.mean_intensity_0 .<= 1000) .& (full.area .<= 1e9)
        @test 0 < sum(keep) < 1377
        v = pop_plot_cols(img, "flow", "/dim", ["label", "area"]; value_name="B")
        @test v[1] == Float64.(full.label[keep])                    # members, table order
        @test v[2] == Float64.(full.area[keep])                     # aligned with the labels
        @test all(isempty, pop_plot_cols(img, "flow", "/nope", ["area"]; value_name="B"))
        @test all(isempty, pop_plot_cols(img, "flow", "/none", ["area", "label"]; value_name="B"))

        # the map hook: a transient explicit-label pop (the API's pick selection) reads like any pop
        pick = Int.(full.label[5:3:60])
        hook = mm -> add_pop!(mm, "Pick selection"; parent=ROOT, explicit_labels=reverse(pick), transient=true)
        v = pop_plot_cols(img, "flow", "/Pick selection", ["label"]; value_name="B", map_hook=hook)
        @test v[1] == Float64.(pick)                                # table order, not the given order
        @test isempty(pop_plot_cols(img, "flow", "/Pick selection", ["label"]; value_name="B")[1])
        cm = computed_pop_map(img; value_name="B", pop_type="flow", map_hook=hook, labels_version="v1")
        @test sort(cells_in_pop(cm, "/Pick selection")) == sort(pick)
        @test pop_stats(cm, "/dim").count == sum(keep) && cm.pinned_labels_version == "v1"
        # derived pops resolve the same way through the map as through pop_df / the plot read
        lm = computed_pop_map(img; value_name="B", pop_type="live")
        for p in ("/_tracked", "/dim/_tracked")
            n = nrow(pop_df(img, "live", [p]; value_name="B", pop_cols=["label"]))
            @test 0 < pop_stats(lm, p).count == n
            @test length(pop_plot_cols(img, "live", p, ["label"]; value_name="B")[1]) == n
        end

        # the membership source reads exactly what it is asked for — an empty ask is the labels alone
        @test names(pop_membership_fetch(img, "B", "flow")(String[])) == ["label"]
        # pop_cols = ["label"] reads the labels, not the whole table
        @test sort(names(pop_df(img, "flow", ["/dim"]; value_name="B", pop_cols=["label"]))) ==
              ["label", "pop", "value_name"]

        # labels-version pin reaches membership AND the column read: `_latest` points at a missing file
        vimg = _ppc_image()
        vimg.label_props["B"] = Dict{String,Any}("v1" => "B.h5ad", "v2" => "gone.h5ad", "_latest" => "v2")
        save_pop_map!(m, vimg)
        @test_throws Exception pop_plot_cols(vimg, "flow", "/dim", ["area"]; value_name="B")
        @test pop_plot_cols(vimg, "flow", "/dim", ["area"]; value_name="B", labels_version="v1")[1] ==
              Float64.(full.area[keep])
        @test_throws Exception pop_df(vimg, "labels", String[]; value_name="B", pop_cols=["area"])
        @test nrow(pop_df(vimg, "labels", String[]; value_name="B", pop_cols=["area"], labels_version="v1")) == 1377
        @test nrow(pop_df(vimg, "flow", ["/dim"]; value_name="B", pop_cols=["area"], centroids=:pixel,
                          labels_version="v1")) == sum(keep)

        # gate units: a µm-stamped map on a calibrated image shows centroids in µm; intensities stay
        cimg = _ppc_image(calibrated=true)
        um = PopulationMap(pop_type="flow", value_name="B")
        um.spatial_unit = "um"
        add_pop!(um, "left"; gate=RectangleGate("centroid_x", "centroid_y", 0.0, 30.0, -1e9, 1e9))
        save_pop_map!(um, cimg)
        cx = Float64.(full.centroid_x)
        v = pop_plot_cols(cimg, "flow", ROOT, ["centroid_x", "area"]; value_name="B")
        @test v[1] ≈ cx .* 0.5 && v[2] == Float64.(full.area)
        @test gate_axis_scale(load_pop_map(cimg; value_name="B"), "centroid_x") == 0.5
        @test gate_axis_scale(load_pop_map(cimg; value_name="B"), "centroid_t") == 1.0
        inleft = cx .* 0.5 .<= 30.0                                 # the gate is in µm too
        @test pop_plot_cols(cimg, "flow", "/left", ["label"]; value_name="B")[1] == Float64.(full.label[inleft])
        # the same map on an UNcalibrated image stays in pixels
        save_pop_map!(um, img)
        @test pop_plot_cols(img, "flow", ROOT, ["centroid_x"]; value_name="B")[1] == cx
    end
end

@testset "pop_plot_cols — track-grained (KDIeEm)" begin
    if !have_fixture(fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad"))
        @test_skip "pop_plot_cols track (fixture missing)"
    else
        img = _ppc_image(tracks=true)
        @test is_track_grained("track") && is_track_grained("trackclust") && !is_track_grained("flow")
        mot = track_table_cols(img, "B")
        speed = first(c for c in mot if c == "live.track.speed")
        tp = track_props(img; value_name="B", cell_measures=["area", "mean_intensity_0"])
        @test nrow(tp) == 62
        # root: one row per TRACK (never expanded to cells), aggregates resolved from the column names
        v = pop_plot_cols(img, "track", ROOT, [speed, "area.mean", "nope"]; value_name="B")
        @test v[1] == Float64.(tp[!, speed]) && v[2] == Float64.(tp[!, "area.mean"]) && isempty(v[3])
        # a gate on aggregates the plot does NOT name still evaluates (the membership source
        # aggregates what the gates need)
        thr = sort(tp[!, "mean_intensity_0.mean"])[31]
        m = PopulationMap(pop_type="track", value_name="B")
        add_pop!(m, "aggr"; gate=RectangleGate("mean_intensity_0.mean", "area.mean", -1e12, thr, -1e12, 1e12))
        save_pop_map!(m, img)
        keep = tp[!, "mean_intensity_0.mean"] .<= thr
        v = pop_plot_cols(img, "track", "/aggr", ["label", speed]; value_name="B")
        @test v[1] == Float64.(tp.label[keep]) && v[2] == Float64.(tp[!, speed][keep])
        @test 0 < length(v[1]) < 62
        # pop_df agrees when the caller names no cell measures at all
        @test nrow(pop_df(img, "track", ["/aggr"]; value_name="B", granularity=:track)) == sum(keep)
        # the pick-selection hook is the caller's to withhold on track maps; computed_pop_map evaluates
        # the per-track table
        @test count(identity, pop_membership(computed_pop_map(img; value_name="B", pop_type="track"), "/aggr")) == sum(keep)

        # the branching task's `refPops` → CELL labels, for every grain: a track gate is its tracks'
        # cells, a `_tracked` subset resolves, an unknown pop says so
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        kept = Set(Int.(tp.label[keep]))
        want = sort([Int(l) for (l, t) in zip(cells.label, cells.track_id) if t isa Real && isfinite(t) && Int(t) in kept])
        labs, err = Cecelia._ref_pop_cell_labels(img, "/aggr", "B")
        @test err === nothing && sort(labs) == want && !isempty(want)
        tracked = count(t -> t isa Real && isfinite(t) && t > 0, cells.track_id)
        labs, err = Cecelia._ref_pop_cell_labels(img, "B/_tracked", "B")
        @test err === nothing && length(labs) == tracked > 0
        labs, err = Cecelia._ref_pop_cell_labels(img, "/nope", "B")
        @test labs === nothing && occursin("not found", err)

        # the pop_df cache stamps the map file the read LOADS: a saved trackclust edit invalidates it
        # on a long-lived image (a REPL/notebook `img`), it doesn't serve the previous members
        if "clusters.movement" in names(tp)
            tc = PopulationMap(pop_type="trackclust", value_name="B")
            add_pop!(tc, "c"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0])
            save_pop_map!(tc, img)
            n0 = nrow(pop_df(img, "trackclust", ["/c"]; value_name="B", granularity=:track))
            sleep(0.05)
            tc = PopulationMap(pop_type="trackclust", value_name="B")
            add_pop!(tc, "c"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0, 1])
            save_pop_map!(tc, img)
            n1 = nrow(pop_df(img, "trackclust", ["/c"]; value_name="B", granularity=:track))
            @test n1 == count(v -> v in (0, 1), tp[!, "clusters.movement"]) > n0
        else
            @test_skip "fixture track table has no clusters.movement"
        end
    end
end
