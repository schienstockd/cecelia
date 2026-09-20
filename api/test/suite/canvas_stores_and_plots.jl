# Module canvas + image stores + plot-spec / plotmeta testsets — extracted from api/test/runtests.jl.
#
# Six testsets covering the module canvas / stores / plot-spec surface:
#  - `API: module-canvas persistence`
#  - `API: image stores (codec + on-disk size per version)`
#  - `API: plot-spec per-page popType narrowing`
#  - `API: interaction matrix needs no population selection`
#  - `API: cluster/region run resolution is family-aware`
#  - `API: plotmeta gate-autoscale helpers`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: module-canvas persistence" begin
    # Redirect projects_dir() → temp so we don't touch the dev projects dir.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTCANVAS"
        mkpath(joinpath(tmp, uid, "1", "IMG1"))   # the object (image) dir must exist
        write(joinpath(tmp, uid, "project.json"),
              JSON3.write((; uid = uid, name = "T", set_uids = String[])))
        entry = Dict("panels" => [], "activeId" => 0, "nextId" => 0, "arrangeSeq" => 0, "shared" => Dict())
        payload = Dict("projectUid" => uid, "objects" => Dict(
            "IMG1" => Dict("entries" => Dict("summary:behaviour:IMG1" => entry), "geom" => Dict())))
        # save writes 1/IMG1/moduleCanvases.json (with the object), verbatim
        @test _post(api_projects_canvases, payload)[1] == 200
        mc_file = joinpath(tmp, uid, "1", "IMG1", "moduleCanvases.json")
        @test isfile(mc_file)
        @test haskey(JSON3.read(read(mc_file, String)).entries, Symbol("summary:behaviour:IMG1"))
        # object dir absent → skipped (no crash, no stray file)
        @test _post(api_projects_canvases,
                    Dict("projectUid" => uid, "objects" => Dict("GHOST" => Dict("entries" => Dict(), "geom" => Dict()))))[1] == 200
        @test !isfile(joinpath(tmp, uid, "1", "GHOST", "moduleCanvases.json"))
        # load reassembles the per-object files into one keyed map
        st, body = api_projects_load(Vector{UInt8}(JSON3.write(Dict("uid" => uid))))
        @test st == 200
        mc = JSON3.read(body).moduleCanvases
        @test mc !== nothing && haskey(mc.entries, Symbol("summary:behaviour:IMG1"))
        # error paths
        @test _post(api_projects_canvases, Dict("objects" => Dict()))[1] == 400          # no projectUid
        @test _post(api_projects_canvases, Dict("projectUid" => "NOPE", "objects" => Dict()))[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: image stores (codec + on-disk size per version)" begin
    # The whole route over a hand-built project: what the metadata modal renders per stored file. The
    # shapes matter as much as the numbers — a version whose store is GONE must keep its row (bytes 0,
    # no codec ⇒ "—" in the modal) instead of dropping it or failing the call for the versions that
    # do read. Sizes are asserted as lower bounds: `_dir_bytes` reports disk BLOCKS, so the walked
    # total is legitimately larger than the bytes written.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        puid, iuid = "TESTSTORES", "IMG1"
        mkpath(joinpath(tmp, puid, "1", iuid))
        write(joinpath(tmp, puid, "project.json"),
              JSON3.write((; uid = puid, name = "T", set_uids = String[])))
        # one real store (flat layout: level-0 array is `0`), one registered but absent
        store = joinpath(tmp, puid, "0", iuid, "live.ome.zarr")
        mkpath(joinpath(store, "0"))
        write(joinpath(store, "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"zstd","clevel":3,"shuffle":1}}""")
        write(joinpath(store, "0", "0.0"), rand(UInt8, 20_000))
        # two label files under one value_name (base + nuc) — the row's size is their sum
        mkpath(joinpath(tmp, puid, "1", iuid, "labels"))
        write(joinpath(tmp, puid, "1", iuid, "labels", "A.zarr"), rand(UInt8, 8_000))
        write(joinpath(tmp, puid, "1", iuid, "labels", "A.nuc.zarr"), rand(UInt8, 4_000))
        write(state_file(joinpath(tmp, puid), iuid), JSON3.write(Dict{String,Any}(
            "class"    => "CciaImage",
            "filepath" => Dict{String,Any}("default" => "live.ome.zarr",
                                           "driftCorrected" => "gone.ome.zarr",
                                           "_active" => "default"),
            "labels"   => Dict{String,Any}("A" => ["A.zarr", "A.nuc.zarr"]))))

        st, body = api_image_stores(
            HTTP.Request("GET", "/api/images/stores?projectUid=$puid&imageUid=$iuid"))
        @test st == 200
        d = JSON3.read(body)
        # the store that reads: Settings' own label for that codec + a walked size
        @test d.versions.default.label == "zstd + shuffle"
        @test d.versions.default.bytes >= 20_000
        # the store that doesn't: row kept, size 0, codec fields absent (the modal shows "—")
        @test d.versions.driftCorrected.bytes == 0
        @test !haskey(d.versions.driftCorrected, :label)
        # label sets are sized too, summed across the value_name's files
        @test d.labels.A.bytes >= 12_000

        # Layout, not just codec: v2 and v3 stores coexist on disk permanently (no converter —
        # ZARR_V3_PLAN D7), so the modal has to be able to say which a store is and how it is chunked.
        # `shard` is present-and-null for an unsharded store rather than absent: "not sharded" and "we
        # could not read it" are different answers and the readout distinguishes them.
        # This fixture's `.zarray` is hand-written with only a `compressor` and no NGFF attrs, so
        # `ngffVersion`/`chunks` are legitimately empty here — the point asserted is that the fields are
        # REPORTED (the modal renders what it gets). Real values are asserted against the ZARRFMT
        # fixtures in *"API: zarr v2 and v3 read identically"*, which are real bioformats2raw stores.
        @test d.versions.default.zarrFormat == 2
        @test isnothing(d.versions.default.shard)
        @test haskey(d.versions.default, :ngffVersion)
        @test haskey(d.versions.default, :chunks)
        # the unreadable store carries none of them, same as its codec fields
        for k in (:zarrFormat, :ngffVersion, :chunks, :shard)
            @test !haskey(d.versions.driftCorrected, k)
        end

        @test api_image_stores(HTTP.Request("GET", "/api/images/stores"))[1] == 400
        @test api_image_stores(
            HTTP.Request("GET", "/api/images/stores?projectUid=NOPE&imageUid=NOPE"))[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: plot-spec per-page popType narrowing" begin
    # ONE spec serves several pages, each offering its own subset of the population families. The
    # narrowing happens server-side so the frontend needs no per-page knowledge — it renders a picker
    # over whatever list it was handed. See docs/PLOTS.md → *Which page a plot belongs to*.
    spec = Dict{String,Any}(
        "id" => "population_summary",
        "dataSource" => Dict{String,Any}("popTypes" => Any[
            Dict{String,Any}("popType" => "flow", "granularity" => "cell"),
            Dict{String,Any}("popType" => "clust", "granularity" => "cell"),
            Dict{String,Any}("popType" => "live", "granularity" => "track"),
            Dict{String,Any}("popType" => "trackclust", "granularity" => "track"),
            Dict{String,Any}("popType" => "region", "granularity" => "cell")]),
        "modules" => Dict{String,Any}("phenotype" => ["flow", "clust"],
                                      "behaviourAnalysis" => ["live", "trackclust"],
                                      "spatialAnalysis" => ["region"]))
    pts(s) = [String(p["popType"]) for p in s["dataSource"]["popTypes"]]

    # each page sees only its own families, in the SPEC's order (the spec decides the default = first)
    @test pts(_narrow_spec_poptypes(spec, "phenotype")) == ["flow", "clust"]
    @test pts(_narrow_spec_poptypes(spec, "behaviourAnalysis")) == ["live", "trackclust"]
    @test pts(_narrow_spec_poptypes(spec, "spatialAnalysis")) == ["region"]
    # granularity travels with the family, so the panel can send the right one per pick
    ph = _narrow_spec_poptypes(spec, "phenotype")["dataSource"]["popTypes"]
    @test all(String(p["granularity"]) == "cell" for p in ph)

    # the universal board (no module) gets the FULL list — it hosts every family at once
    @test length(pts(_narrow_spec_poptypes(spec, ""))) == 5
    # narrowing must not mutate the spec it was handed (specs are re-read per request, but a shared
    # in-memory spec would otherwise be progressively emptied by successive page queries)
    @test length(spec["dataSource"]["popTypes"]) == 5
    # a page not listed at all is left untouched rather than silently emptied
    @test length(pts(_narrow_spec_poptypes(spec, "segment"))) == 5
    # a legacy single-`module` spec has no `modules` and passes straight through
    legacy = Dict{String,Any}("module" => "phenotype",
                              "dataSource" => Dict{String,Any}("popType" => "flow", "granularity" => "cell"))
    @test _narrow_spec_poptypes(legacy, "phenotype") === legacy
end

@testset "API: interaction matrix needs no population selection" begin
    # The interaction matrix's rows/columns come from the `neighbourStats` run it reads, so the panel
    # sends NO `series`/`pops`. The generic selector guard rejected that body before `plot_summary_data`
    # could intercept on matrixMode — "pops (or series) required" on a plot that has no pops to pick.
    pops_required(r) = occursin("pops (or series) required", String(r[2]))
    base = Dict("projectUid" => "nope-not-a-project", "popType" => "flow", "granularity" => "cell")

    inter = _post(api_plot_data, merge(base, Dict("chartType" => "matrix", "matrixMode" => "interaction")))
    @test !pops_required(inter)          # gets past the guard (then fails on the bogus project, as it should)

    # the guard must still hold for every OTHER plot — including the other matrix modes, which DO
    # aggregate a pop_df frame and are meaningless without a selection.
    for mode in ("profile", "crosstab")
        r = _post(api_plot_data, merge(base, Dict("chartType" => "matrix", "matrixMode" => mode)))
        @test pops_required(r)
    end
    @test pops_required(_post(api_plot_data, merge(base, Dict("chartType" => "bar"))))
    # an explicitly EMPTY series list is a different mistake (the user unticked everything) and keeps
    # its own message rather than being waved through as "precomputed"
    empty_series = _post(api_plot_data, merge(base, Dict("chartType" => "bar", "series" => [])))
    @test occursin("series required", String(empty_series[2]))
end

@testset "API: cluster/region run resolution is family-aware" begin
    # The channels endpoint enumerates a pop_type's OWN obs column family. Hardcoding "clusters." here
    # is why the Region-clustering page showed an empty run list (falling back to "default") while
    # `regions.immune` sat in obs — see docs/todo/SPATIAL_REGIONS_PLAN.md.
    obs = ["label", "clusters.myeloid", "regions.immune", "regions.niches", "live.cell.speed"]
    @test _cluster_suffixes(obs, "clust")      == ["myeloid"]
    @test _cluster_suffixes(obs, "trackclust") == ["myeloid"]
    @test Set(_cluster_suffixes(obs, "region")) == Set(["immune", "niches"])
    @test _cluster_suffixes(obs) == ["myeloid"]          # default stays the clusters family
    @test isempty(_cluster_suffixes(["label"], "region"))

    # sidecar reads are family-scoped too, and a missing file/run is empty rather than a throw
    lpdir = mktempdir(); props = joinpath(lpdir, "B.h5ad")
    Cecelia._write_clust_features!(props, "immune", ["spatial.comp.B_qc.immune"], ["u1", "u2"];
                                   family = "regions", labels = Dict("spatial.comp.B_qc.immune" => "B/qc"))
    @test _clust_features(props, ["immune"], "regions")["immune"] == ["spatial.comp.B_qc.immune"]
    @test _clust_members(props, ["immune"], "regions")["immune"]  == ["u1", "u2"]
    @test _clust_feature_labels(props, ["immune"], "regions")["immune"]["spatial.comp.B_qc.immune"] == "B/qc"
    @test isempty(_clust_features(props, ["immune"], "clusters")["immune"])   # different family
    @test isempty(_clust_feature_labels(props, ["immune"], "clusters"))      # no labels → key omitted
    @test isempty(_clust_members(joinpath(lpdir, "gone.h5ad"), ["immune"], "regions")["immune"])

    # REGRESSION: an entry written BEFORE the `labels` field existed — i.e. any image not re-run since.
    # Reading it must not throw. `something(get(...), nothing)` did: with every argument `nothing`,
    # `something()` raises ArgumentError("No value arguments present"), which surfaced as repeated 500s
    # from /api/gating/channels?popType=region.
    legacy = joinpath(lpdir, "L.h5ad")
    open(replace(legacy, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("niches" => Dict("features" => ["B/qc"], "partOf" => ["u1"])))
    end
    @test _clust_feature_labels(legacy, ["niches"], "regions") == Dict{String,Any}()
    @test _clust_members(legacy, ["niches"], "regions")["niches"] == ["u1"]
    @test _clust_features(legacy, ["niches"], "regions")["niches"] == ["B/qc"]
    # …and the oldest layout of all: a bare feature ARRAY, with no partOf and no labels
    oldest = joinpath(lpdir, "O.h5ad")
    open(replace(oldest, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("old" => ["f1", "f2"]))
    end
    @test _clust_feature_labels(oldest, ["old"], "regions") == Dict{String,Any}()
    @test _clust_members(oldest, ["old"], "regions")["old"] == String[]
    # a suffix with no entry at all must also be safe (the obs column exists, the sidecar lags)
    @test _clust_feature_labels(props, ["nosuchrun"], "regions") == Dict{String,Any}()
    @test _clust_members(props, ["nosuchrun"], "regions")["nosuchrun"] == String[]
end

@testset "API: plotmeta gate-autoscale helpers" begin
    # _gates_bbox: display-space bbox over a mixed rectangle + polygon gate list
    @test _gates_bbox([]) == (Inf, -Inf, Inf, -Inf)          # nothing to enclose
    rect = Dict{String,Any}("kind" => "rectangle", "x_min" => 1.0, "x_max" => 3.0,
                            "y_min" => -2.0, "y_max" => 0.5)
    poly = Dict{String,Any}("kind" => "polygon", "vertices" => [[5.0, 1.0], [6.0, -4.0], [4.5, 2.0]])
    @test _gates_bbox([rect]) == (1.0, 3.0, -2.0, 0.5)
    bb = _gates_bbox([rect, poly])
    @test bb == (1.0, 6.0, -4.0, 2.0)                        # union across both gate kinds

    # _include_range: only the side a gate actually exceeds moves; margin = fraction of the span
    @test _include_range((0.0, 10.0), Inf, -Inf) == (0.0, 10.0)   # no finite gate → unchanged
    @test _include_range((0.0, 10.0), 2.0, 8.0)  == (0.0, 10.0)   # gate inside → unchanged
    lo, hi = _include_range((0.0, 10.0), -5.0, 20.0)              # exceeds both sides
    @test lo == -5.0 - 0.5 && hi == 20.0 + 0.5                    # margin = 0.05 * span(10) = 0.5
    lo2, hi2 = _include_range((0.0, 10.0), -5.0, 8.0)            # exceeds low side only
    @test lo2 == -5.5 && hi2 == 10.0
end

