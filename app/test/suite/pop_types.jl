# ── Population types + cluster/region + spatial obs testsets ──────────
# 10 sections covering: GATING_POP_TYPES, img_has_value_name, clust/trackclust pop types,
# region pop type (spatial regions), contact_matrix (CODEX log-odds heatmap), region pop
# auto-share (co-clustered value_names, cell granularity), bare cluster/region pops (per
# request or run-wide), clustfeatures sidecar (families, labels, legacy layouts), spatial
# obs measures are numeric, and region 'other' column skipped when all-zero. Extracted
# from suite.jl to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

# ── clust / trackclust pop types (cluster-membership populations) ─────────────
# A cluster pop is a filter on the `clusters.{suffix}` column (clustPops/clustTracks output):
# filter_fun="in", filter_values=[ticked cluster ids]. Stored in its own sidecar so it never
# collides with flow gates. Headless — membership via a recompute! closure (no fixture).
# gating pop types = the hand-drawn ones (flow=cells, track=tracks); clust/trackclust are filters.
# Drives copy-to-images + the defining-plot view (one abstraction over both, no flow special-casing).
@testset "GATING_POP_TYPES" begin
    @test GATING_POP_TYPES == ("flow", "track")
    @test is_gating_pop_type("flow") && is_gating_pop_type("track")
    @test !is_gating_pop_type("clust") && !is_gating_pop_type("trackclust") && !is_gating_pop_type("live")
end

# generic value_name presence check on an image (drives copy-to-images target filtering).
# `_active` is a bookkeeping key, not a value_name → excluded by versioned_keys.
@testset "img_has_value_name" begin
    proj = create_project!(name="vn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "_active" => "A")
    @test Set(img_value_names(img)) == Set(["A", "B"])
    @test img_has_value_name(img, "A") && img_has_value_name(img, "B")
    @test !img_has_value_name(img, "C") && !img_has_value_name(img, "_active")
    rm(proj.root; recursive=true)
end

@testset "clust / trackclust pop types" begin
    td = mktempdir()
    # each clustering pop_type routes to its OWN gating sidecar (no collision with flow's {vn}.json)
    @test endswith(gating_path(td, "B"; pop_type="flow"),       joinpath("gating", "B.json"))
    @test endswith(gating_path(td, "B"; pop_type="clust"),      joinpath("gating", "B__clust.json"))
    @test endswith(gating_path(td, "B"; pop_type="trackclust"), joinpath("gating", "B__trackclust.json"))
    @test endswith(gating_path(td, "B"; pop_type="track"),      joinpath("gating", "B__tracks.json"))

    # cluster pop membership = filter "in" over the cluster code column
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "myeloid"; filter_measure="clusters.default", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "clusters.default" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/myeloid")) == Set([11, 13, 14])   # codes ∈ {1,3}

    # save/load round-trip → own file, filter fields preserved, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="clust"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="clust")
    @test pop_at(m2, "/myeloid").filter_measure == "clusters.default"
    @test pop_at(m2, "/myeloid").filter_fun == Cecelia.FILTER_IN
    @test Set(pop_at(m2, "/myeloid").filter_values) == Set([1, 3])
end

@testset "region pop type (spatial regions)" begin
    # region reuses the cluster-pop machinery with its OWN `regions.{suffix}` column prefix
    # (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5) — no duplicated logic, one generalisation.
    td = mktempdir()
    @test endswith(gating_path(td, "B"; pop_type="region"), joinpath("gating", "B__region.json"))
    @test Cecelia._is_cluster_pop_type("region")
    @test Cecelia._cluster_measure_prefix("region") == "regions."
    @test Cecelia._cluster_measure_prefix("clust") == "clusters."
    @test is_track_pop("region", "/tumour_zone") == false          # regions are per-cell, not per-track
    @test !is_gating_pop_type("region")                            # filter/membership pop, not a gate

    # region membership = filter "in" over the region code column (same engine path as clust)
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "tumour_zone"; filter_measure="regions.niches", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "regions.niches" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/tumour_zone")) == Set([11, 13, 14])   # region codes ∈ {1,3}

    # referenced-suffixes generalisation reads the region prefix from the map's own pop_type
    @test Cecelia._referenced_cluster_suffixes(m) == Set(["niches"])

    # save/load round-trip → own __region file, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="region"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="region")
    @test pop_at(m2, "/tumour_zone").filter_measure == "regions.niches"
    @test Set(pop_at(m2, "/tumour_zone").filter_values) == Set([1, 3])

    # categorical name-rule: `regions`/`regions.{suffix}` are always a code set, even past the level cap
    @test Cecelia._is_categorical_col(collect(0:50), "regions.niches")   # 51 int levels, name-rule wins
    @test Cecelia._is_categorical_col(collect(0:50), "regions")
    @test Cecelia._is_categorical_col([0.0, 1.5, 2.7], "regions.niches") # decimals irrelevant under name-rule

    # per-region heatmap matrix detection routes through the shared suffix extractor (regions. prefix)
    @test Cecelia._cluster_matrix_suffix("matrix", "regions.niches") == "niches"
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.default") == "default"
end

@testset "contact_matrix — CODEX log-odds heatmap matrix" begin
    # sidecar spatialStats/{suffix}.json → symmetric pop×pop log-odds matrix for the plot renderer
    td = mktempdir(); mkpath(joinpath(td, "spatialStats"))
    open(joinpath(td, "spatialStats", "default.json"), "w") do f
        write(f, """{"basis":["B/qc","T/qc"],"nCells":100,"nEdges":200,"records":[""" *
                 """{"popA":"B/qc","popB":"B/qc","observed":10,"expected":5,"logOdds":0.7,"association":"associated"},""" *
                 """{"popA":"B/qc","popB":"T/qc","observed":1,"expected":5,"logOdds":-1.1,"association":"avoided"},""" *
                 """{"popA":"T/qc","popB":"T/qc","observed":8,"expected":4,"logOdds":0.6,"association":"associated"}]}""")
    end
    m = contact_matrix(CciaImage(; dir=td))
    @test m.suffixes == ["default"] && m.suffix == "default"
    @test Set(m.basis) == Set(["B/qc", "T/qc"]) && m.nCells == 100 && m.nEdges == 200
    val(x, y) = only(c.value for c in m.cells if c.x == x && c.y == y)
    @test val("B/qc", "T/qc") ≈ -1.1 && val("T/qc", "B/qc") ≈ -1.1   # symmetric fill
    @test val("B/qc", "B/qc") ≈ 0.7 && val("T/qc", "T/qc") ≈ 0.6
    @test length(m.cells) == 4                                       # 2×2 fully filled
    # no sidecar → empty (route returns empty, UI shows "run contact stats first")
    m0 = contact_matrix(CciaImage(; dir=mktempdir()))
    @test isempty(m0.cells) && isempty(m0.suffixes)
end

@testset "region pop auto-share (co-clustered value_names, cell granularity)" begin
    # regions are a per-run column shared across co-clustered segmentations — the identical
    # auto-share/expand machinery as clust, exercised via the `regions.` prefix + cell granularity.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # A & B were region-clustered together (both CELL sidecars carry suffix "niches"); C was not.
    for vn in ("A", "B")
        open(joinpath(lpdir, "$(vn).clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("niches" => Dict("features" => ["flow.region.cd8"], "partOf" => ["u1"])))
        end
    end
    am = PopulationMap(pop_type="region", value_name="A")
    add_pop!(am, "TumourZone"; filter_measure="regions.niches", filter_fun="in", filter_values=[2], colour="#c061cb")
    save_pop_map!(am, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "C" => "C.h5ad", "_active" => "A")

    @test Set(Cecelia.co_clustered_value_names(img, "niches"; granularity=:cell)) == Set(["A", "B"])

    # B has no sidecar but IS co-clustered → borrows A's region pops, relabeled to B
    mb = load_pop_map(img; value_name="B", pop_type="region")
    @test Set(keys(mb.pops)) == Set(["/TumourZone"]) && mb.value_name == "B"
    @test all(p.value_name == "B" for p in values(mb.pops))
    # C was not in the run → no borrow
    @test isempty(load_pop_map(img; value_name="C", pop_type="region").pops)

    # bare region-pop ref expands across all co-clustered segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/TumourZone"], "region", "A")) ==
          Set(["A/TumourZone", "B/TumourZone"])
end

@testset "bare cluster/region pops: run-wide by default, per-segmentation on request" begin
    # A bare cluster-family ref spans every co-clustered segmentation (old-R popDT parity) — right
    # for "show me this run's cluster", WRONG for a plot series, where the picker already offered
    # each (segmentation, population) pair separately. Ticking 3 region pops under B plotted 6.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    for vn in ("B", "T")
        Cecelia._write_clust_features!(joinpath(lpdir, "$(vn).h5ad"), "immune",
                                       ["spatial.comp.x.immune"], ["u1"]; family = "regions")
    end
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "Population 1"; filter_measure="regions.immune", filter_fun="in", filter_values=[1])
    save_pop_map!(m, td)
    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "_active" => "B")

    # default: bare ref fans out across the run's segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/Population 1"], "region", "B")) ==
          Set(["B/Population 1", "T/Population 1"])
    # explicitly value_name-prefixed refs are untouched either way
    @test Cecelia._expand_cluster_pops(img, ["B/Population 1"], "region", "B") == ["B/Population 1"]
    # a non-cluster pop type never expands
    @test Cecelia._expand_cluster_pops(img, ["/gate"], "flow", "B") == ["/gate"]
    # and pop_df exposes the opt-out the series path uses (keyword present, both forms)
    @test :expand_cluster_pops in Base.kwarg_decl(
        only(methods(pop_df, (CciaImage, AbstractString, Any))))
end

@testset "clustfeatures sidecar — families, labels, legacy layouts" begin
    # The sidecar is keyed `{family}.{suffix}` so a cell clustering and a REGION clustering that
    # share a suffix coexist on one segmentation instead of clobbering each other. Three historical
    # layouts must all read back through the ONE shared reader (docs/todo/SPATIAL_REGIONS_PLAN.md).
    @test Cecelia._cluster_measure_family("region") == "regions"
    @test Cecelia._cluster_measure_family("clust")  == "clusters"
    @test Cecelia._cluster_measure_family("trackclust") == "clusters"
    @test Cecelia._clustfeatures_key("immune", "regions") == "regions.immune"
    @test Cecelia._clustfeatures_split_key("regions.immune") == ("immune", "regions")
    @test Cecelia._clustfeatures_split_key("clusters.a.b")   == ("a.b", "clusters")
    @test Cecelia._clustfeatures_split_key("immune")         == ("immune", nothing)   # legacy → any family

    td = mktempdir(); lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    props = joinpath(lpdir, "B.h5ad")

    # two runs, SAME suffix, different families — the collision that used to silently overwrite
    Cecelia._write_clust_features!(props, "immune", ["mean_intensity_0"], ["u1"]; family="clusters")
    Cecelia._write_clust_features!(props, "immune", ["spatial.comp.B_qc.immune"], ["u1", "u2"];
                                   family="regions",
                                   labels=Dict("spatial.comp.B_qc.immune" => "B/qc"))
    @test Cecelia._clustfeatures_features(props, "immune"; family="clusters") == ["mean_intensity_0"]
    @test Cecelia._clustfeatures_features(props, "immune"; family="regions") == ["spatial.comp.B_qc.immune"]
    @test Cecelia._clustfeatures_suffixes(props; family="clusters") == Set(["immune"])
    @test Cecelia._clustfeatures_suffixes(props; family="regions")  == Set(["immune"])
    # partOf stays per-family (the region run covered one more image)
    e_r = Cecelia._clustfeatures_entry(props, "immune"; family="regions")
    e_c = Cecelia._clustfeatures_entry(props, "immune"; family="clusters")
    @test length(get(e_r, "partOf", [])) == 2 && length(get(e_c, "partOf", [])) == 1
    @test String(get(e_r, "labels", Dict())["spatial.comp.B_qc.immune"]) == "B/qc"

    # LEGACY bare-suffix entry (pre-family) matches every family, so existing data keeps working
    legacy = joinpath(lpdir, "L.h5ad")
    open(replace(legacy, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("niches" => Dict("features" => ["x"], "partOf" => ["u1"])))
    end
    @test Cecelia._clustfeatures_suffixes(legacy; family="regions")  == Set(["niches"])
    @test Cecelia._clustfeatures_suffixes(legacy; family="clusters") == Set(["niches"])
    @test Cecelia._clustfeatures_features(legacy, "niches"; family="regions") == ["x"]

    # OLDEST layout: {suffix => [features]} (a bare array, no membership) normalises to the current shape
    oldest = joinpath(lpdir, "O.h5ad")
    open(replace(oldest, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("old" => ["f1", "f2"]))
    end
    @test Cecelia._clustfeatures_features(oldest, "old") == ["f1", "f2"]
    @test isempty(get(Cecelia._clustfeatures_entry(oldest, "old"), "partOf", ["nonempty"]))

    # absent run / absent file → empty, never a throw
    @test Cecelia._clustfeatures_features(props, "nosuchrun"; family="regions") == String[]
    @test Cecelia._clustfeatures_entry(joinpath(lpdir, "missing.h5ad"), "x") === nothing
end

@testset "spatial obs measures are NUMERIC, not integer code sets" begin
    # A 0/1 contact/aggregate flag has few integer levels, so the generic heuristic calls it a
    # categorical code set — and the plot panel then offers only count/bar and snaps the chart type
    # to `count`. Commit 16ead1d fixed exactly this for integer morphology by exempting `var`
    # columns; these are `obs`, so they need a name-rule instead.
    flag = [0, 1, 1, 0, 1]
    @test Cecelia._is_categorical_col(flag, "live.cell.contact#live.T_qc__tracked") == false
    @test Cecelia._is_categorical_col(flag, "flow.cell.is.aggregate") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.min_distance#live.T_qc") == false
    @test Cecelia._is_categorical_col([0, 0, 1], "spatial.comp.other.immune") == false
    # …while the IDENTIFIERS beside them stay categorical (they are label codes, not quantities)
    @test Cecelia._is_categorical_col([3, 7, 7], "live.cell.contact_id#live.T_qc__tracked") == true
    @test Cecelia._is_categorical_col([1, 2, 2], "live.cell.aggregate.id") == true
    # and the existing rules are untouched
    @test Cecelia._is_categorical_col([0, 1, 2], "regions.immune") == true
    @test Cecelia._is_categorical_col([0, 1, 2], "clusters.default") == true
    @test Cecelia._is_categorical_col([1.5, 2.5], "live.cell.speed") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.hmm.state.movement") == true
end

@testset "region 'other' column is skipped when it would be all-zero" begin
    # A graph built over the basis populations themselves contains nothing outside the basis, so the
    # "other" composition column is all-zero — not a measurement, just a flat row in the heatmap.
    # The runner drops it and flags that in the run QC; Julia must then not advertise it in the
    # clustfeatures sidecar, or the heatmap offers a column the table doesn't have.
    d = mktempdir()
    p = joinpath(d, "region_qc.json")
    @test Cecelia._region_other_all_zero(joinpath(d, "absent.json")) == false   # missing → written
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => true)); end
    @test Cecelia._region_other_all_zero(p) == true
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => false)); end
    @test Cecelia._region_other_all_zero(p) == false
    open(p, "w") do f; JSON3.write(f, Dict("nClusters" => 3)); end                # older run, no flag
    @test Cecelia._region_other_all_zero(p) == false
    write(p, "{ not json")                                                        # unreadable → written
    @test Cecelia._region_other_all_zero(p) == false
end
