# ── Plot specs + canvas + spatial-graph + neighbourStats-QC testsets ──
# Seven sections covering: plot specs live on the page that EXPLORES (not DEFINES), plot
# spec groupByOptions name current columns, every canvas host offers Close all, summary
# canvas is set-scoped / gating canvas is image-scoped, interaction matrix aggregates with
# NO population targets, spatial graph path accessor + discovery, and neighbourStats QC
# findings. Extracted from suite.jl to keep it small enough to merge without EOF conflicts
# on every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope (lexical include).
#
# The 7 path scans in the extracted range (plotDefinitions walks + frontend/src reads +
# api/src/server.jl read) are already pathof-anchored — no rewrites needed.

@testset "plot specs live on the page that EXPLORES, not the one that DEFINES" begin
    # Where a plot lives is a product decision worth pinning, because the drift is invisible: a new
    # pop type arrives, someone adds a `population_summary_<type>.json` pointed at the page that
    # produced it, and every population-DEFINING page slowly grows a summary canvas it has no use
    # for. Populations are DEFINED on gate / track / clust-cells / clust-tracks / regions, and
    # SUMMARISED on the Explore pages. Each summary follows its pop type:
    #     flow → phenotype ·  clust → phenotype ·  live/trackclust → behaviourAnalysis ·  region → spatialAnalysis
    root = joinpath(dirname(dirname(pathof(Cecelia))), "src", "plotDefinitions")
    @test isdir(root)
    specs = Dict{String,Any}()
    for f in readdir(root)
        endswith(f, ".json") || continue
        specs[f] = JSON3.read(read(joinpath(root, f), String), Dict{String,Any})
    end
    @test length(specs) > 5          # the walk found the registry (a floor, not a census)

    # The interaction matrix is a REGISTRY plot now, not a bespoke component + route: it was the one
    # violation of docs/PLOTS.md → *Hosting — ONE way*, which is why it sat in a fixed box below the
    # table and couldn't be duplicated, arranged, exported or put on the Analysis board.
    @test haskey(specs, "spatial_interactions.json")
    @test String(specs["spatial_interactions.json"]["dataSource"]["matrix"]["mode"]) == "interaction"
    @test String(specs["spatial_interactions.json"]["module"]) == "spatialAnalysis"
    # …and the bespoke surface is gone for good
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    @test !isfile(joinpath(fe, "modules", "spatial", "SpatialContactHeatmap.vue"))
    @test !isfile(joinpath(fe, "utils", "contactHeatmap.ts"))
    srv = read(joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "api", "src", "server.jl"), String)
    @test !occursin("/api/plots/contact_matrix", srv)

    # the population-DEFINING module pages carry no plot specs at all
    DEFINING = ("clustPops", "clustTracks", "clustRegions")
    stray = ["$f → module=$(get(s, "module", ""))" for (f, s) in specs
             if String(get(s, "module", "")) in DEFINING]
    @test isempty(stray)

    # There is now ONE population-summary spec offering every family, with the per-page curation in
    # its `modules` allow-list — the four per-popType copies are gone. Pin both halves: no copies
    # come back, and each page still offers exactly the families it should.
    for gone in ("population_summary_clust.json", "population_summary_trackclust.json",
                 "population_summary_tracks.json", "population_summary_region.json")
        @test !haskey(specs, gone)
    end
    ps = specs["population_summary.json"]
    @test !haskey(ps, "module")                       # multi-page specs use `modules`, not `module`
    offered = Dict(String(k) => Set(String(x) for x in v) for (k, v) in ps["modules"])
    @test offered["phenotype"]         == Set(["flow", "clust"])
    @test offered["behaviourAnalysis"] == Set(["live", "track", "trackclust"])
    @test offered["spatialAnalysis"]   == Set(["region"])

    # every family a page offers must actually be declared, WITH its own granularity — the one thing
    # that genuinely blocked a shared spec (sending the spec's single granularity asked for cell rows
    # under a track pop type). flow/clust/region are cell-grained, live/track/trackclust track-grained.
    pts = Dict(String(p["popType"]) => String(p["granularity"]) for p in ps["dataSource"]["popTypes"])
    @test Set(keys(pts)) == Set(["flow", "clust", "live", "track", "trackclust", "region"])
    @test pts["flow"] == "cell" && pts["clust"] == "cell" && pts["region"] == "cell"
    @test pts["live"] == "track" && pts["track"] == "track" && pts["trackclust"] == "track"
    for (_, fams) in offered, f in fams
        @test haskey(pts, f)                          # a page can't offer an undeclared family
    end

    # BEHAVIOUR PLOTS ARE NOT LIVE-ONLY. Every one of them shipped the legacy single
    # `popType: "live"`, so a gated-track population or a track cluster could not be plotted at all
    # — the family picker existed but these specs never opted into it. `pop_df` has always
    # supported `track`/`trackclust` at either granularity (`_pop_df_track_gating` expands track
    # membership to its member cells), so this was a spec omission, not a capability gap.
    BEHAVIOUR = ("cell_properties.json", "hmm_state_frequency.json", "state_signature.json",
                 "transition_matrix.json", "track_measures.json", "motif_class_frequency.json")
    for f in BEHAVIOUR
        ds = specs[f]["dataSource"]
        @test !haskey(ds, "popType")            # legacy single-family form is gone
        fams = Dict(String(p["popType"]) => String(p["granularity"]) for p in ds["popTypes"])
        @test Set(keys(fams)) == Set(["live", "track", "trackclust"])
        # granularity is the PLOT's, not the family's: per-track measures are track-grained, the
        # cell/HMM readouts cell-grained — and it must be the same for all three families, or one
        # pick would silently ask for a different table than another.
        want = f == "track_measures.json" ? "track" : "cell"
        @test all(g == want for g in values(fams))
    end

    # A plot's family list is CURATED in its spec (not derived from the data), because "which family
    # can this measure be sliced by" is a judgement the data can't make. The cost of curation is
    # silent drift, and it drifted: the spatial measures plot offered Gated/Cell clusters/Regions/
    # Tracked but not Track clusters — a family every spatial task happily accepts as input. So pin
    # the agreement to the PRODUCING tasks' own `accepts`, via the canonical token mapping
    # (`_accept_pop_types`) rather than a second hand-written list.
    producing = (CellNeighbours(), NeighbourStats(), CellContacts(), ContactsMeshes(),
                 DetectAggregates(), AggregatesMeshes(), ClustRegions())
    accepted = Set{String}()
    for t in producing
        spec = JSON3.read(read(Cecelia._spec_path(t), String))
        for p in get(spec, :params, [])
            String(get(p, :type, "")) == "popSelection" || continue
            acc = Cecelia._normalise_accepts(get(p, :accepts, String[]))
            union!(accepted, Cecelia._accept_pop_types(acc))
        end
    end
    @test accepted == Set(["live", "track", "clust", "trackclust", "region"])
    spat = Dict(String(p["popType"]) => String(p["granularity"])
                for p in specs["spatial_cell_properties.json"]["dataSource"]["popTypes"])
    @test isempty(setdiff(accepted, keys(spat)))   # every accepted family is offered for plotting
    # `flow` is offered ON TOP: _normalise_accepts folds flow→live (same gate map), but the plot
    # keeps them apart — "Gated" slices the cell gates, "Tracked" the derived `_tracked` sets.
    @test haskey(spat, "flow")
    # the spatial readouts are per-CELL columns, so every family is sliced at cell granularity —
    # including the track-grained ones (pop_df expands track membership to its member cells)
    @test all(g == "cell" for g in values(spat))

    # The manager follows the ACTIVE plot's family, which needs both hosts to pass activeSpecId AND
    # activePopType into useSummaryData. If that regresses the picker silently lists the wrong
    # family — invisible, so pin the wiring.
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    for host in ("SummaryCanvas.vue", "LayoutCanvas.vue")
        src = read(joinpath(fe, "components", "canvas", host), String)
        @test occursin("activeSpecId", src)
        @test occursin("activePopType", src)
        @test occursin("migrateSpecId", src)          # persisted canvases must not silently empty
    end
end

# ── a plot spec may only name CURRENT column names ────────────────────────────────
# `groupByOptions` is a HINT list: SummaryPanel keeps only the entries actually present on the data
# (`hints = groupByOptions.filter(c => present.has(c))`). So a spec naming a column that no longer
# exists does not error and does not warn — the option just never appears in the menu, and whatever
# view it unlocks is silently unreachable. That is exactly how the segmentation-QC per-timepoint plot
# (cell count / any label measure over time, the LOESS trend + CI ribbon) sat dark: the spec said
# `t`, the PRE-MIGRATION temporal column name that `centroid_migrate.py` renames to `centroid_t`
# (`uns/temporal_cols`), so the hint matched nothing, `timeSeries` never went true, and the chart menu
# never swapped to [trend, count]. The spec was right when written and rotted in place — which is why
# this is pinned statically rather than left to a data-dependent test.
@testset "plot spec groupByOptions name current columns" begin
    root = joinpath(dirname(dirname(pathof(Cecelia))), "src", "plotDefinitions")
    RETIRED = Set(["t"])          # pre-migration uns/temporal_cols spelling → now "centroid_t"
    for f in readdir(root)
        endswith(f, ".json") || continue
        spec = JSON3.read(read(joinpath(root, f), String), Dict{String,Any})
        ds = get(spec, "dataSource", nothing)
        ds isa AbstractDict || continue
        for c in String.(get(ds, "groupByOptions", String[]))
            @test !(c in RETIRED)                 # a retired name filters to nothing, silently
            @test !startswith(c, "centroid-")     # pre-migration positional centroid spelling
        end
    end
    # and the segmentation-QC spec still offers the per-timepoint view at all
    qc = JSON3.read(read(joinpath(root, "segmentation_qc.json"), String), Dict{String,Any})
    @test "centroid_t" in String.(qc["dataSource"]["groupByOptions"])
    @test "count" in String.(qc["chartTypes"])    # the count-over-time headline
end

# ── every plot canvas offers the same bulk close ──────────────────────────────────
# "Close all" has to be on EVERY canvas, not just the one it was asked for — a per-canvas answer is
# how the Tile/Cascade group ended up copied into four hosts in the first place. The shared halves are
# `useCanvasPanels.removeAll` (the workspace logic) and `CanvasArrangeButtons` (the toolbar group), so
# the drift to catch is a host that drives the composable but renders its own arrange buttons: it
# would silently lack the close, and nothing else would fail.
#
# Scanned from here rather than from vitest because the frontend suite is deliberately pure-logic only
# (docs/DEV.md → Tests) — `removeAll` is store mutation with no pure kernel to unit-test, and this is
# the same source-scanning guard the plot-host wiring above already uses.
@testset "every canvas host offers Close all" begin
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    comp = read(joinpath(fe, "composables", "useCanvasPanels.ts"), String)
    @test occursin("function removeAll", comp)
    @test occursin("removeAll,", comp)                  # …and it is actually returned to hosts
    @test isfile(joinpath(fe, "components", "canvas", "CanvasArrangeButtons.vue"))

    HOSTS = [joinpath("components", "canvas", "SummaryCanvas.vue"),
             joinpath("modules", "gate", "GatingPlots.vue"),
             joinpath("modules", "cluster", "ClusterPlots.vue"),
             joinpath("modules", "modelTraining", "ModelPlots.vue")]
    for h in HOSTS
        src = read(joinpath(fe, h), String)
        @test occursin("useCanvasPanels", src)
        @test occursin("CanvasArrangeButtons", src)     # the shared group, not a private copy
        @test occursin("close-all", src)                # …wired, not merely imported
        # the arrange buttons must not be re-inlined beside the shared component
        @test !occursin("'Tile in a grid'", src)
    end

    # Any OTHER host that starts driving the same workspace must adopt the shared group too — this is
    # the check that fails when a fifth canvas is added and quietly ships without Close all.
    for (root, _, files) in walkdir(fe), f in files
        endswith(f, ".vue") || continue
        p = joinpath(root, f)
        src = read(p, String)
        occursin("useCanvasPanels(", src) || continue   # calls it (not merely a type import)
        occursin("CanvasArrangeButtons", src) ||
            error("$(relpath(p, fe)) drives useCanvasPanels but renders no CanvasArrangeButtons — " *
                  "every plot canvas must offer Tile/Cascade/Close all")
    end
end

# ── a module summary canvas belongs to the SET, a gating canvas to the IMAGE ──────
# The summary canvas used to be keyed by the FIRST selected image, which quietly tied two unrelated
# things to the selection order: what got plotted, AND which saved layout you were looking at — so
# re-ticking swapped your whole canvas, and ticking five images showed the first one's. Summary plots
# are set-aware by design (the `compare` control is exactly the per-image/pooled/by-attribute choice),
# so the LAYOUT has no business being image-scoped as well. Gating is the opposite case and must stay
# per image: gates belong to one (image, value_name).
#
# Pinned by reading the key expressions, because both are one-liners in an SFC and the failure is
# silent either way — a wrongly-scoped canvas still renders, just not the one you saved.
@testset "summary canvas is set-scoped, gating canvas is image-scoped" begin
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    ckey_line(path) = begin
        ls = filter(l -> occursin("const ckey = computed", l),
                    split(read(joinpath(fe, path...), String), '\n'))
        @test length(ls) == 1                      # one key expression per canvas, or this is stale
        first(ls)
    end

    sc = ckey_line(("components", "canvas", "SummaryCanvas.vue"))
    @test occursin("setUid", sc)
    @test !occursin("imageUid", sc)                # the bug: first-selected image decided the canvas

    # gating stays per (image, value_name) — deliberate, not an oversight. The vn identifier
    # is `pageVn` (page-local, decoupled from the singleton `g.valueName` so cluster/trackclust
    # task-done can't rebind our ckey) — either substring is fine as long as SOME vn is there.
    gp = ckey_line(("modules", "gate", "GatingPlots.vue"))
    @test occursin("imageUid", gp) && (occursin("valueName", gp) || occursin("pageVn", gp))

    # cluster was already set-scoped; it is the precedent this follows (and `objectOf` persists a
    # set-keyed canvas to the SET's own moduleCanvases.json, so no new persistence path was needed)
    @test occursin("setUid", ckey_line(("modules", "cluster", "ClusterPlots.vue")))

    # …and ticking several images must actually PLOT several: `image` means "the first one only", so it
    # cannot be the default. `canCompare` already gates this on there being >1 image selected.
    usd = read(joinpath(fe, "composables", "useSummaryData.ts"), String)
    @test occursin("compareMode: 'per_image'", usd)
    @test !occursin("compareMode: 'image'", usd)
end

@testset "interaction matrix aggregates with NO population targets" begin
    # The path `api_plot_data`'s `precomputed` branch now takes. The panel sends no `series` (the
    # matrix's rows/columns come from the neighbourStats run), so the targets vector is EMPTY — and
    # the interception has to fire before anything touches pop_df. Previously the selector guard
    # rejected the body outright ("pops (or series) required" on a plot with no pops to pick), so
    # this dispatch was never exercised.
    td = mktempdir()
    try
        mkpath(joinpath(td, "spatialStats"))
        write(joinpath(td, "spatialStats", "run1.json"), """
            {"basis":["B/qc","T/qc"],"nCells":334,"nEdges":1200,"graphSuffix":"g1",
             "nPermutations":500,"coverage":0.9,"records":[
              {"popA":"B/qc","popB":"B/qc","observed":120,"expected":80,"logOdds":0.48,
               "zScore":15.8,"pValue":0.002,"significant":true,"association":"association"},
              {"popA":"B/qc","popB":"T/qc","observed":10,"expected":33,"logOdds":-1.19,
               "zScore":-30.4,"pValue":0.002,"significant":true,"association":"avoidance"},
              {"popA":"T/qc","popB":"T/qc","observed":90,"expected":60,"logOdds":0.58,
               "zScore":17.7,"pValue":0.002,"significant":true,"association":"association"}]}
            """)
        img = CciaImage(; dir = td)
        r = plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction", stats_suffix = "run1")
        @test r["chartType"] == "matrix" && r["matrixMode"] == "interaction"
        @test r["xLabels"] == ["B/qc", "T/qc"] && r["yLabels"] == r["xLabels"]
        @test r["suffixes"] == ["run1"] && r["suffix"] == "run1"
        @test isempty(r["series"])                       # nothing to overlay — it IS the matrix
        # symmetric fill: 2 populations → 4 cells, the off-diagonals sharing one record
        @test length(r["cells"]) == 4
        by = Dict((c["x"], c["y"]) => c for c in r["cells"])
        @test by[("B/qc", "B/qc")]["value"] == 0.48
        @test by[("B/qc", "T/qc")]["value"] == by[("T/qc", "B/qc")]["value"] == -1.19
        # z / p / observed ride along per cell so the renderer needs no second request
        @test by[("B/qc", "T/qc")]["zScore"] == -30.4
        @test by[("B/qc", "T/qc")]["pValue"] == 0.002
        @test by[("B/qc", "T/qc")]["count"] == 10
        # …plus the star ladder, from the SAME function the hypothesis tests use — a second ladder
        # in the renderer would be a fork waiting to disagree
        @test by[("B/qc", "T/qc")]["significance"] == Cecelia._significance(0.002)
        @test by[("B/qc", "T/qc")]["significance"] == "**"
        # the colour encoding is DIVERGING about 0, so the value must keep its sign as sent (the
        # renderer asserts the scale; here we pin that the payload isn't pre-normalised)
        @test by[("B/qc", "T/qc")]["value"] < 0 < by[("B/qc", "B/qc")]["value"]
        @test r["valueLabel"] == "log-odds"
        # an unknown suffix falls back to the first run rather than erroring
        @test plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                                matrix_mode = "interaction", stats_suffix = "nope")["suffix"] == "run1"
        # …and with NO run at all it's an empty matrix, not a throw (the panel shows its own hint)
        empty_img = CciaImage(; dir = mktempdir())
        e = plot_summary_data(empty_img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction")
        @test isempty(e["cells"]) && isempty(e["xLabels"])
    finally
        rm(td; recursive = true, force = true)
    end
end

@testset "spatial graph — path accessor + discovery" begin
    # The graph pools ACROSS segmentations, so it is keyed by run suffix under spatialGraph/, not by
    # value_name next to a cell table (which could not represent a cross-segmentation graph).
    # Discovery is a directory listing, like spatialStats/ — nothing in ccid.json.
    td = mktempdir()
    img = CciaImage(; dir = td)
    @test img_spatial_graph_suffixes(img) == String[]        # nothing built yet
    @test endswith(img_spatial_graph_path(img, "run1"), joinpath("spatialGraph", "run1.h5ad"))
    mkpath(img_spatial_graph_dir(img))
    for s in ("run2", "run1")
        touch(img_spatial_graph_path(img, s))
    end
    touch(joinpath(img_spatial_graph_dir(img), "notes.txt"))  # non-h5ad ignored
    @test img_spatial_graph_suffixes(img) == ["run1", "run2"]     # sorted
end

@testset "neighbourStats QC findings" begin
    # pure helper (docs/MODULES.md) — advisory findings only, never gates
    ids(fs) = Set(String(f["code"]) for f in fs)
    @test ids(Cecelia._neighbour_stats_findings(0, 0)) == Set(["spatial.no_cells"])
    @test ids(Cecelia._neighbour_stats_findings(10, 0)) == Set(["spatial.no_edges"])
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, 3))
    # a graph built over far more cells than the analysis selects → the counts rest on a slice of it
    @test "spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.02, 3))
    @test !("spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.5, 3)))
    # nothing beat chance → say so; -1 means the test was skipped (permutations = 0), so stay quiet
    @test "spatial.none_significant" in ids(Cecelia._neighbour_stats_findings(10, 5, 1.0, 0))
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, -1))
end
