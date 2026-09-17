# ── Population pickers + resolve_pop_type testsets ────────────────────
# Six sections covering pure pop-picker logic delegated by the API routes: plot population
# picker (plot_pop_types / plot_population_groups — the /api/plots/populations logic),
# popScope population picker, population accepts allow-list + category tags, branch
# pop_type wiring, ensure_filter_pop! auto-created population, and resolve_pop_type +
# pop_namespace (mixed-type pickers). Extracted from suite.jl to keep it small enough to
# merge without EOF conflicts on every append. The extracted file loads inside this file's
# aggregating testset scope, so any helpers defined earlier in suite.jl are still in scope
# (lexical include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

# ── Summary-canvas population picker (plot_pop_types / plot_population_groups) ──
# The logic the /api/plots/populations route delegates to — pure, so tested here (the route is a
# thin wrapper). Covers granularity→pop_type selection, cross-image + cross-pop_type union/dedup,
# derived-pop injection, and pop_type tagging (the track-pops-in-the-picker fix; docs/POPULATION.md).
@testset "plot population picker" begin
    # pop_type selection by granularity
    @test plot_pop_types("live", "cell") == ["live"]
    @test plot_pop_types("live", "")     == ["live"]
    @test plot_pop_types("live", "track") == ["live", "track"]
    @test plot_pop_types("track", "track") == ["track"]          # no duplicate

    # flatten_pop_tree: pre-order paths + colours
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    flat = flatten_pop_tree(to_tree(fm))
    @test [p for (p, _, _) in flat] == ["/qc", "/qc/sub"]
    @test flat[1][3] == "#ef4444"

    # a track-gated map (pop_type "track") with one pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")

    # loaders injected (as the API passes versioned_keys/load_pop_map closures)
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn == "C" ? (pt == "track" ? tm : (pt == "live" ? fm : nothing)) : nothing

    # CELL granularity → live pops only, with derived `_tracked` at root AND under each stored pop
    # (so /qc/_tracked is a selectable, indented child); a derived child directly follows its parent.
    cell = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell"))
    @test length(cell) == 1 && cell[1].value_name == "C"
    cpops = cell[1].populations
    @test [p.path for p in cpops] ==
          ["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked"]
    @test all(p.pop_type == "live" for p in cpops)
    @test !any(p.path == "/TEST" for p in cpops)
    # the nested derived pop is named by its leaf (indents under its parent in the UI)
    @test only(p for p in cpops if p.path == "/qc/_tracked").name == "_tracked"
    # a derived child inherits its parent pop's colour (so /qc/_tracked pairs with /qc visually —
    # the derived colour is read-only on the behaviour page, the parent's is editable on gating)
    @test only(p for p in cpops if p.path == "/qc/_tracked").colour == "#ef4444"      # = /qc
    @test only(p for p in cpops if p.path == "/qc/sub/_tracked").colour == "#abc"      # = /qc/sub
    @test only(p for p in cpops if p.path == "/_tracked").colour == "#7c93b8"          # root: no parent → grey

    # TRACK granularity → unions live (incl. nested /qc/_tracked) AND track (/TEST), each tagged
    trk = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "track"))
    tp = trk[1].populations
    @test Set(p.path for p in tp) ==
          Set(["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked", "/TEST"])
    test_pop = only(p for p in tp if p.path == "/TEST")
    @test test_pop.pop_type == "track" && test_pop.colour == "#f59e0b"
    @test only(p for p in tp if p.path == "/qc").pop_type == "live"
    @test !any(p.path == "/TEST/_tracked" for p in tp)          # no track-derived pop registered

    # derived_ok predicate, keyed on the PARENT path ("" = root): the API passes false where the
    # derived set is a copy of a deeper one, so tracking gated to /qc/sub hides /_tracked AND
    # /qc/_tracked while keeping /qc/sub/_tracked. Default (no predicate) offers all three —
    # asserted by `cell` above.
    gated = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell");
                                   derived_ok = (_v, _pt, parent, _d) -> parent == "/qc/sub")
    gpaths = [p.path for p in gated[1].populations]
    @test !("/_tracked" in gpaths) && !("/qc/_tracked" in gpaths)   # root + ancestor copies hidden
    @test "/qc/sub/_tracked" in gpaths                              # the population that was tracked
    @test "/qc" in gpaths && "/qc/sub" in gpaths                    # the stored gates are untouched

    # cross-image UNION + dedup: two images both expose "C" → each (pop_type, path) appears once
    dedup = plot_population_groups([:img1, :img2], names_for, load, ["live"])
    @test length(dedup) == 1
    @test length(dedup[1].populations) == length(cpops)         # no duplicates across images

    # LABELS (gateless): no gating map — one selectable pop per segmentation value_name, named by
    # the value_name, tagged pop_type "labels" (segmentation QC: B/T plot side by side).
    names2 = _ -> ["B", "T"]
    lab = plot_population_groups([:img1], names2, (args...) -> error("must not load a map for labels"),
                                 ["labels"])
    @test [g.value_name for g in lab] == ["B", "T"]
    @test all(g -> length(g.populations) == 1, lab)
    bp = only(lab[1].populations)
    @test bp.path == "/labels" && bp.name == "B" && bp.pop_type == "labels"
    @test only(lab[2].populations).name == "T"
end

@testset "popScope population picker" begin
    # is_track_pop: the sole cell-vs-track test (Julia parity of the R `isTrack` attribute)
    @test is_track_pop("live", "/qc") == false                  # plain cell gate
    @test is_track_pop("flow", "/qc/sub") == false
    @test is_track_pop("clust", "/myeloid") == false            # cell cluster
    @test is_track_pop("live", "/_tracked") == true             # derived tracked set (root)
    @test is_track_pop("live", "/qc/_tracked") == true          # derived tracked subset of a gate
    @test is_track_pop("track", "/TEST") == true                # per-track gate
    @test is_track_pop("trackclust", "/clusterA") == true       # track cluster

    # scope_pop_types: sources loaded per scope; clusters toggleable; unknown scope throws.
    # `cells` also loads `region` (spatial regions) alongside `clust` — both cluster-family.
    @test scope_pop_types("cells", true)  == ["live", "clust", "region"]
    @test scope_pop_types("cells", false) == ["live"]
    @test scope_pop_types("tracks", true)  == ["live", "track", "trackclust"]
    @test scope_pop_types("tracks", false) == ["live", "track"]
    @test_throws ErrorException scope_pop_types("bogus", true)

    # maps: flow gates (/qc, /qc/sub), a per-track gate (/TEST), a cell cluster (/myeloid),
    # a track cluster (/clusterA)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm : pt == "trackclust" ? tcm : nothing

    # CELLS scope: all-cells root ("/") + plain gates + cell clusters; NO derived _tracked sets
    cells = population_scope_groups([:img1], names_for, load, "cells")
    @test length(cells) == 1 && cells[1].value_name == "C"
    cpaths = [p.path for p in cells[1].populations]
    @test cpaths == ["/", "/qc", "/qc/sub", "/myeloid"]
    @test cells[1].populations[1].name == "all"                 # backend all-cells root
    @test !any(occursin("_tracked", p) for p in cpaths)         # cells never show tracked sets
    @test all(!is_track_pop(p.pop_type, p.path) for p in cells[1].populations if p.path != "/")

    # CELLS, clusters excluded → drops /myeloid
    cells_nc = population_scope_groups([:img1], names_for, load, "cells"; include_clusters=false)
    @test [p.path for p in cells_nc[1].populations] == ["/", "/qc", "/qc/sub"]

    # TRACKS scope: derived _tracked sets (root + per-gate) + per-track gate + track cluster;
    # NO plain cell gates (/qc, /qc/sub) and NO all-cells root ("/")
    trk = population_scope_groups([:img1], names_for, load, "tracks")
    tpaths = Set(p.path for p in trk[1].populations)
    @test tpaths == Set(["/_tracked", "/qc/_tracked", "/qc/sub/_tracked", "/TEST", "/clusterA"])
    @test !("/qc" in tpaths) && !("/qc/sub" in tpaths) && !("/" in tpaths)
    @test all(is_track_pop(p.pop_type, p.path) for p in trk[1].populations)
    # a derived tracked child keeps its parent gate's colour (visual pairing, read-only)
    @test only(p for p in trk[1].populations if p.path == "/qc/_tracked").colour == "#ef4444"

    # TRACKS, gated tracking → root /_tracked hidden (redundant with /qc/_tracked); children kept
    trk_g = population_scope_groups([:img1], names_for, load, "tracks";
                                    derived_ok=(_v, _pt, parent, _d) -> parent != "")
    gpaths = Set(p.path for p in trk_g[1].populations)
    @test !("/_tracked" in gpaths) && "/qc/_tracked" in gpaths

    # TRACKS, clusters excluded → drops /clusterA, keeps the per-track gate /TEST
    trk_nc = population_scope_groups([:img1], names_for, load, "tracks"; include_clusters=false)
    tncpaths = Set(p.path for p in trk_nc[1].populations)
    @test !("/clusterA" in tncpaths) && "/TEST" in tncpaths
end

# ── pop_category + population_accept_groups (Decision 14, accepts allow-list) ────────────────
@testset "population accepts allow-list + category tags" begin
    # pop_category: gated / clustered / region / tracked / aggregated from (pop_type, leaf).
    @test pop_category("live", "/qc")               == "gated"
    @test pop_category("track", "/TEST")             == "gated"
    @test pop_category("clust", "/myeloid")          == "clustered"
    @test pop_category("trackclust", "/clusterA")    == "clustered"
    @test pop_category("region", "/r0")              == "region"
    @test pop_category("live", "/qc/_tracked")       == "tracked"
    @test pop_category("live", "/qc/" * Cecelia.AGGREGATED_POP_NAME) == "aggregated"

    # same fixtures as the popScope testset above, plus a region map and an aggregated cell pop.
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, Cecelia.AGGREGATED_POP_NAME; parent="/qc", filter_measure="live.cell.is.aggregate",
             filter_fun="gt", filter_values=0, reserved_ok=true)   # auto-created aggregate pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5)
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    rm_ = PopulationMap(pop_type="region", value_name="C")
    add_pop!(rm_, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm :
        pt == "trackclust" ? tcm : pt == "region" ? rm_ : nothing

    # accepts=["live"] → all-cells root + cell gate + the aggregated cell pop; NO tracked sets,
    # NO clusters/regions. Each population carries granularity/category tags.
    g = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test [p.path for p in g] == ["/", "/qc", "/qc/" * Cecelia.AGGREGATED_POP_NAME]
    @test all(p.granularity == "cell" for p in g)
    @test only(p for p in g if p.path == "/qc").category == "gated"
    @test only(p for p in g if endswith(p.path, Cecelia.AGGREGATED_POP_NAME)).category == "aggregated"

    # "flow" is an alias for "live".
    @test [p.path for p in population_accept_groups([:img1], names_for, load, ["flow"])[1].populations] ==
          [p.path for p in g]

    # region basis: cells (gated+clustered+region) AND tracks (gated+clustered). One picker, both
    # granularities — the case popScope could not express.
    basis = population_accept_groups([:img1], names_for, load,
                ["live", "clust", "region", "track", "trackclust"])[1].populations
    bcats = Set((p.granularity, p.category) for p in basis)
    @test ("cell", "gated") in bcats && ("cell", "clustered") in bcats && ("cell", "region") in bcats
    @test ("track", "tracked") in bcats && ("track", "gated") in bcats && ("track", "clustered") in bcats
    @test "/r0" in [p.path for p in basis] && "/myeloid" in [p.path for p in basis]
    @test "/clusterA" in [p.path for p in basis] && "/TEST" in [p.path for p in basis]

    # accepts=["clust"] alone → only cell clusters, no all-cells root (live not accepted).
    cl = population_accept_groups([:img1], names_for, load, ["clust"])[1].populations
    @test [p.path for p in cl] == ["/myeloid"]

    # popScope shim must still produce identical paths to the direct accept call.
    @test [p.path for p in population_scope_groups([:img1], names_for, load, "cells")[1].populations] ==
          [p.path for p in population_accept_groups([:img1], names_for, load,
                                ["live", "clust", "region"])[1].populations]

    # unknown token / empty list throw loudly.
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, ["bogus"])
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, String[])
end

# ── branch pop_type (BRANCHING_PLAN.md Decision 2) ────────────────────────────
# Adding "branch" to the framework must extend POP_MAP_SUFFIX/ACCEPT_TOKENS/pop_category and
# route via population_accept_groups with granularity="branch". The framework was designed to
# take a third pop_type; this guards the wiring.
@testset "branch pop_type wiring" begin
    # POP_MAP_SUFFIX resolves the gating file suffix.
    @test Cecelia.POP_MAP_SUFFIX["branch"] == BRANCH_PROPS_SUFFIX
    # build the expected tail with joinpath — a literal "gating/..." fails on Windows, where the
    # path is "\\gating\\stroma__branch.json" (the product is fine; the assertion wasn't portable)
    @test endswith(gating_path("/tmp", "stroma"; pop_type="branch"),
                   joinpath("gating", "stroma__branch.json"))

    # ACCEPT_TOKENS + validators.
    @test "branch" in Cecelia.ACCEPT_TOKENS

    # pop_category: branch pops are gated (the ensure_filter_pop! per-branch-type case).
    @test pop_category("branch", "/endpoint-to-endpoint") == "gated"

    # population_accept_groups tags branch pops with granularity="branch" and only surfaces
    # them when "branch" is in accepts. A mixed request keeps cells + branches.
    bm = PopulationMap(pop_type="branch", value_name="C")
    add_pop!(bm, "endpoint-to-endpoint"; filter_measure="branch-type",
             filter_fun="eq", filter_values=0)
    add_pop!(bm, "junction-to-junction"; filter_measure="branch-type",
             filter_fun="eq", filter_values=2)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1))
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live"   ? fm :
        pt == "branch" ? bm : nothing

    # accepts=["branch"] → only branch pops, no cell root
    br = population_accept_groups([:img1], names_for, load, ["branch"])[1].populations
    @test Set(p.path for p in br) == Set(["/endpoint-to-endpoint", "/junction-to-junction"])
    @test all(p.granularity == "branch" for p in br)
    @test all(p.category    == "gated"  for p in br)
    @test all(p.pop_type    == "branch" for p in br)

    # accepts=["live","branch"] → all-cells root + cell gate + branches
    mix = population_accept_groups([:img1], names_for, load, ["live", "branch"])[1].populations
    gcats = Set((p.granularity, p.category) for p in mix)
    @test ("cell", "gated") in gcats
    @test ("branch", "gated") in gcats

    # accepts=["live"] must NOT include branches.
    only_cells = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test all(p.granularity == "cell" for p in only_cells)
end

# ── ensure_filter_pop! — a cutoff materialised as a reusable filter pop (Decision 14) ────────
@testset "ensure_filter_pop! auto-created population" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    m = PopulationMap(; pop_type="flow", value_name="B")
    add_pop!(m, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img)

    # a 0/1 flag column → aggregated pop under /qc (the generalisable `> 0`, not a baked TRUE/FALSE)
    created = ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created == ["/qc/" * AGGREGATED_POP_NAME]
    p = pop_at(load_pop_map(img; value_name="B", pop_type="flow"), "/qc/" * AGGREGATED_POP_NAME)
    @test p.filter_measure == "flow.cell.is.aggregate" && p.filter_fun == Cecelia.FILTER_GT && p.filter_values == 0
    @test pop_category(p.pop_type, p.path) == "aggregated" && !is_track_pop(p.pop_type, p.path)

    # idempotent: re-running REDEFINES (a probability cutoff — measure-agnostic), never duplicates
    ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.aggregate.score", filter_fun="gte", filter_values=0.5)
    m3 = load_pop_map(img; value_name="B", pop_type="flow")
    @test count(pp -> endswith(pp, AGGREGATED_POP_NAME), pop_paths(m3)) == 1
    @test pop_at(m3, "/qc/" * AGGREGATED_POP_NAME).filter_fun == Cecelia.FILTER_GTE

    # a parent absent from the map is skipped; the all-cells root ("/") maps to ROOT and is created
    created2 = ensure_filter_pop!(img, "flow", "B", ["/nonexistent", "/"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created2 == ["/" * AGGREGATED_POP_NAME]
    rm(td; recursive=true)
end

# ── Mixed-type pop resolution: resolve_pop_type / pop_namespace / pop_df_multi (module pickers) ──
@testset "resolve_pop_type + pop_namespace (mixed-type pickers)" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    # one stored map per type on disk (routed by m.pop_type), all under value_name "B"
    fm = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(fm, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0)); save_pop_map!(fm, img)
    cm = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2]); save_pop_map!(cm, img)
    rmp = PopulationMap(pop_type="region", value_name="B")
    add_pop!(rmp, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0]); save_pop_map!(rmp, img)
    tm = PopulationMap(pop_type="track", value_name="B")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5); save_pop_map!(tm, img)

    # each path resolves to the map that CONTAINS it; _tracked → live; root/unknown → flow
    @test resolve_pop_type(img, "B", "/qc")           == "flow"
    @test resolve_pop_type(img, "B", "/myeloid")      == "clust"
    @test resolve_pop_type(img, "B", "/r0")           == "region"
    @test resolve_pop_type(img, "B", "/TEST")         == "track"
    @test resolve_pop_type(img, "B", "/qc/_tracked")  == "live"   # derived leaf, not stored
    @test resolve_pop_type(img, "B", "/")             == "flow"   # all-cells root → cells
    @test resolve_pop_type(img, "B", "/nonexistent")  == "flow"   # unknown → default (empty downstream)

    # _split_pop_ref: prefix names the value_name; leading-slash/root stays in default
    @test Cecelia._split_pop_ref("B/qc", "default") == ("B", "/qc")
    @test Cecelia._split_pop_ref("/qc", "B")        == ("B", "/qc")
    @test Cecelia._split_pop_ref("qc", "B")         == ("B", "/qc")

    # pops_value_name: the spatial tasks derive their segmentation from the pick (no dropdown).
    # Value_name comes from the first ref's prefix; the all-cells root "B/" carries it too.
    @test pops_value_name(["B/qc"])              == "B"
    @test pops_value_name(["B/qc", "B/myeloid"]) == "B"       # single-segmentation set
    @test pops_value_name(["B/"])                == "B"       # "… all" root pick
    @test pops_value_name(String[])              == "default" # empty → default
    @test pops_value_name(String[]; default="C") == "C"
    # distinct value_names shouldn't reach a single-segmentation picker → warn, first still wins
    @test (@test_logs (:warn,) match_mode=:any pops_value_name(["B/qc", "T/qc"])) == "B"

    # grouping by discovered type preserves first-appearance order
    grp = Cecelia._group_pops_by_type(img, ["/qc", "/myeloid", "/qc/_tracked", "/r0"], "B")
    @test grp == ["flow" => ["/qc"], "clust" => ["/myeloid"], "live" => ["/qc/_tracked"], "region" => ["/r0"]]

    # pop_namespace: any TRACKED source → live, else flow (cluster/region are just cell selections)
    @test pop_namespace(img, ["/qc"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/r0"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/myeloid"]; value_name="B")       == "flow"
    @test pop_namespace(img, ["/qc/_tracked"]; value_name="B")   == "live"
    @test pop_namespace(img, ["/TEST"]; value_name="B")          == "live"   # track pop → live namespace
    @test pop_namespace(img, ["/qc", "B/TEST"]; value_name="B")  == "live"   # any tracked → live
    @test pop_namespace(img, String[])                           == "flow"

    # name-uniqueness guard (cross pop_type): a name already used by ANOTHER type in the segmentation
    @test pop_name_conflict(img, "B", "/qc"; pop_type="region")     == "flow"    # flow gate qc exists
    @test pop_name_conflict(img, "B", "/myeloid"; pop_type="flow")  == "clust"   # clust myeloid exists
    @test pop_name_conflict(img, "B", "/TEST"; pop_type="flow")     == "track"
    @test pop_name_conflict(img, "B", "/qc"; pop_type="flow")       === nothing   # same type → not a conflict
    @test pop_name_conflict(img, "B", "/qc"; pop_type="live")       === nothing   # live shares the flow map
    @test pop_name_conflict(img, "B", "/brandnew"; pop_type="flow") === nothing   # unused name → ok
    @test pop_name_conflict(img, "B", "/"; pop_type="clust")        === nothing   # root exempt

    # same-name guard: "/qc" now exists in BOTH the flow map (a gate) and the region map — an
    # ambiguous path. resolve by priority (flow first) AND @warn, never a silent mis-resolve.
    add_pop!(rmp, "qc"; filter_measure="regions.default", filter_fun="in", filter_values=[1]); save_pop_map!(rmp, img)
    @test (@test_logs (:warn,) match_mode=:any resolve_pop_type(img, "B", "/qc")) == "flow"
    rm(td; recursive=true)
end
