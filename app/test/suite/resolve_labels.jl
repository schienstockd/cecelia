# ── resolve_pops + labels + track-table + pop_df:track testsets ───────
# Six sections covering: resolve_pops (KDIeEm), resolve_pops has_tracks (data flag,
# orthogonal to is_track), has_tracks attribution guard (pure predicate), labels pop_type
# + count (KDIeEm), track table helpers, and pop_df :track (KDIeEm B). Extracted from
# suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).

@testset "resolve_pops (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
        want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12), colour="#ef4444")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
        @test L.show === true && L.is_track === false
        @test sort(L.labels) == want                       # membership == the gate's cells

        # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
        again = resolve_pops(img, "flow"; value_name="B")
        @test again === layers
        @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
    end
end

# ── resolve_pops.has_tracks — DATA fact per pop, drives ribbon eligibility next to `is_track` ─────
# The `has_tracks` field (MULTI_POP_TRACKING_PLAN.md Decision 2) says whether a pop CURRENTLY holds
# any cell with `track_id > 0` — so a flow gate on cells that have since been tracked qualifies as
# ribbon-drawable without touching its `is_track` flag (which stays "was TYPED as a track pop"). The
# fixture h5ad `KDIeEm/B.h5ad` carries a `track_id` obs column (see the test at line 8837 above), so
# a broad flow gate lands on some tracked cells.
@testset "resolve_pops has_tracks: data flag, orthogonal to is_track" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops has_tracks (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # Broad gate: every measured cell (0 → 1e12 on both axes) → includes the tracked ones. A
        # `resolve_pops` layer for this flow pop should carry `has_tracks == true`.
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              -1e12, 1e12, -1e12, 1e12), colour="#3b82f6")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.is_track === false                # flow pop → typed flag stays false
        @test L.has_tracks === true               # data flag: at least one label has track_id > 0
    end
end

# ── has_tracks attribution guard (MULTI_POP_TRACKING_ORPHANS_PLAN P0) ─────────
# `_pop_has_authored_tracks` is the predicate that fixes the wrong-attribution bug where an orphan
# row (from a deleted pop) or a shared-label row (from an overlapping live pop) bled into a
# different pop's ribbon. Pure predicate so we can pin its four rules without a fixture that
# carries a categorical `track_source` column (which requires the Python writer). See docstring +
# the plan's decision 1.
@testset "has_tracks attribution guard (pure predicate)" begin
    UID_A = "aAaAaA"
    UID_B = "bBbBbB"
    WS    = Cecelia.WHOLE_SEG_TRACK_SOURCE
    src(d) = Dict{Int,Union{String,Nothing}}(k => v for (k, v) in d)

    # 1. Empty label_to_source → false regardless of pop.uid / labels.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], src(Dict())) === false

    # 2. A ↔ its own labels: authored by A → true.
    m1 = src(Dict(1 => UID_A, 2 => UID_A))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], m1) === true

    # 3. B on labels A authored → false. THIS is the bug the guard fixes: pre-guard, B.has_tracks
    #    would fire because B's labels overlap tracked rows; now B claims only rows IT authored.
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2, 3], m1) === false

    # 4. `whole_seg` sentinel → counts for everyone (the documented prime-everything mode).
    m_ws = src(Dict(1 => WS, 2 => WS))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_ws) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_ws) === true

    # 5. Legacy row (`nothing`) → counts for everyone. Preserves pre-P1 behaviour for h5ads written
    #    before the provenance ship (decision 1's legacy branch).
    m_leg = src(Dict(1 => nothing, 2 => nothing))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_leg) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_leg) === true

    # 6. Mixed set: label 1 authored by A, label 2 by B, label 3 legacy, label 4 whole_seg,
    #    label 5 orphaned (deleted pop's UID no longer live). Guard fires when ANY qualifying
    #    label sits in `labs` — attribution is per-pop, per-label.
    ORPHAN = "zZzZzZ"
    m_mix = src(Dict(1 => UID_A, 2 => UID_B, 3 => nothing, 4 => WS, 5 => ORPHAN))
    #   A owns 1 directly → true even in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1], m_mix) === true
    #   B owns 2 → true in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_B, [2], m_mix) === true
    #   Nobody but the orphan-source claims label 5. A does NOT get it.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5], m_mix) === false
    #   Legacy label 3 counts for A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [3], m_mix) === true
    #   whole_seg label 4 counts for B.
    @test Cecelia._pop_has_authored_tracks(UID_B, [4], m_mix) === true
    #   A over [5, 6] (all orphan or absent) → false. Confirms the ORPHAN row cannot bleed into A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5, 6], m_mix) === false

    # 7. Label absent from label_to_source (i.e. `track_id ≤ 0` or NaN) → not tracked, ignored.
    @test Cecelia._pop_has_authored_tracks(UID_A, [99, 100], m1) === false
end

# ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
# count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
# See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
# ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
@testset "labels pop_type + count (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "labels pop_type (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
        all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test nrow(all) > 0
        @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
        @test unique(all.pop) == ["/labels"]
        @test unique(all.value_name) == ["B"]

        # cell count via the summary aggregator over labels — one series, value == total.
        whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
        @test whole["chartType"] == "count"
        @test length(whole["series"]) == 1
        @test whole["series"][1]["value"] == Float64(nrow(all))

        # count per timepoint (group_by the temporal column) → counts partition the total.
        byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="centroid_t")
        @test byT["groupBy"] == "centroid_t"
        @test length(byT["series"]) > 1
        @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

        # a morphology distribution over labels, per timepoint
        area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                 measure="area", group_by="centroid_t")
        @test area["measure"] == "area"
        @test length(area["series"]) == length(byT["series"])

        # targets signature (the path the summary canvas + whiteboard QC row use: series =
        # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
        tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
        @test tg["chartType"] == "count"
        @test length(tg["series"]) == 1
        @test tg["series"][1]["value"] == Float64(nrow(all))
        @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
    end
end

# ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
@testset "track table helpers" begin
    td = mktempdir()
    img = CciaImage(uid="X", dir=td)
    # companion track table sits next to the cell labelProps with the __tracks suffix
    @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
    @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
    @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
    # reserved value-name suffix (a user segmentation may not end in __tracks)
    @test is_reserved_value_name("A__tracks")
    @test is_reserved_value_name("foo__tracks")
    @test !is_reserved_value_name("A")
    @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
    # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
    @test Cecelia._jsonsafe(NaN) === nothing
    @test Cecelia._jsonsafe(1.5) === 1.5
    @test Cecelia._jsonsafe(3)   === 3
    # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
    mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img.label_props["B"] = "B.h5ad"
    kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :cell, String[], String[])
    kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :track, String[], String[])
    @test kc != kt
end

# ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
@testset "pop_df :track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df :track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # track table layout: measures in X/var, lineage in obs, one row per track_id
        tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
        @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
        tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
        @test "track_root" in tobs

        # one row per track; carries measures + track_id + value_name
        tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
        @test nrow(tr) > 0
        @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
        @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
        @test unique(tr.value_name) == ["B"]

        # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
        ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
        ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
        @test nrow(tr) == ntracks_cells
        @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

        # pop_cols restriction returns just that measure (+ bookkeeping)
        sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                    pop_cols=["live.track.speed"])
        @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))

        # cell_measures aggregation (the clustTracks path): a per-cell measure is aggregated to
        # per-track feature column(s) via track_props, alongside motility — this is what lets
        # clustTracks cluster `_tracked` pops on HMM/intensity features, not just motility.
        cvars = col_names(label_props(img; value_name="B"); data_type=:vars)
        if !isempty(cvars)
            base = String(first(cvars))                        # a real per-cell measure
            ag = pop_df(img, "live", ["B/_tracked"]; granularity=:track, cell_measures=[base])
            @test any(startswith(c, base * ".") for c in names(ag))   # aggregated → {base}.…
            @test nrow(ag) == nrow(tr)                          # same tracks, extra feature cols
            @test "live.track.speed" in names(ag)               # motility still present
            @test "num_cells" in names(ag)                      # per-track cell count (minTracklength)
        end
    end
end
