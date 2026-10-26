# ── Spatial + neighbours + region-clustering testsets ─────────────────
# Six sections covering: Cohort metrics — branching anisotropy (Figure 4 panel D x-axis
# aggregation), cellNeighbours QC findings (pure helper), aggregate DBSCAN ids
# (Clustering.jl), cellContacts target-name sanitisation, neighbourStats spec (graph knobs
# live on the graph, not here), and clustRegions spec (graph knobs moved out with the
# graph). Extracted from suite.jl to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope, so
# any helpers defined earlier in suite.jl are still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

# Cohort QC must aggregate the per-image anisotropy readout — it is Figure 4 panel D's x-axis
# (SPATIAL_ANISOTROPY_PLAN Decision 6), so dropping it from COHORT_METRICS silently removes
# the plot's data source.
@testset "Cohort metrics — branching anisotropy" begin
    @test "anisotropy" in COHORT_METRICS["segment.branching"]
    @test "nBranches" in COHORT_METRICS["segment.branching"]

    # `anisotropy` is the first RATIO metric in a cohort list otherwise made of counts, so
    # check the outlier rule behaves on 0–1 values at the magnitudes real data produces
    # (EaMaVq measures ≈ 0.32). The modified-z path is scale-free, but the MAD==0 fallback
    # is a RELATIVE departure, so tiny numbers are where it would misbehave if anywhere.
    r = Cecelia._cohort_outliers(Dict("a" => 0.31, "b" => 0.33, "c" => 0.30, "d" => 0.09))
    @test haskey(r.outliers, "d") && !haskey(r.outliers, "a")
    # …and a cohort that merely spans the normal 0.1–0.4 band must NOT flag anything: real
    # tissue varies this much, and a false "outlier" on every low-anisotropy image is noise.
    @test isempty(Cecelia._cohort_outliers(
        Dict("a" => 0.12, "b" => 0.21, "c" => 0.30, "d" => 0.38)).outliers)
end

# ── Dispatch + param validation — ClustPops (clustPops.cluster, set-scope) ───


@testset "cellNeighbours QC findings (pure helper)" begin
    # objective graph metrics → advisory findings; only the unambiguous problems flag
    @test isempty(Cecelia._neighbours_qc_findings(100, 500, 0.1))        # healthy graph → no finding
    @test only(Cecelia._neighbours_qc_findings(0, 0, 0.0))["code"]   == "spatial.no_cells"
    @test only(Cecelia._neighbours_qc_findings(100, 0, 0.0))["code"] == "spatial.no_edges"
    @test only(Cecelia._neighbours_qc_findings(100, 40, 0.7))["code"] == "spatial.many_isolated"
    @test isempty(Cecelia._neighbours_qc_findings(100, 40, 0.3))         # some isolated, under half → fine
    # sparse graph: a radius-pruned Delaunay at mean degree 2.2 made region clustering meaningless
    sparse = only(Cecelia._neighbours_qc_findings(100, 110, 0.1, 2.2))
    @test sparse["code"] == "spatial.sparse_graph"
    @test sparse["level"] == "warn"
    @test occursin("2.2", sparse["long"])
    @test haskey(Cecelia.QC_TEXT, "spatial.sparse_graph")
    @test isempty(Cecelia._neighbours_qc_findings(100, 700, 0.0, 14.6))  # a validated graph → fine
    @test isempty(Cecelia._neighbours_qc_findings(100, 150, 0.0, 3.0))   # at the threshold → fine
    # the more specific findings win: no edges / mostly isolated are not also reported as sparse
    @test only(Cecelia._neighbours_qc_findings(100, 0, 0.0, 0.0))["code"]  == "spatial.no_edges"
    @test only(Cecelia._neighbours_qc_findings(100, 40, 0.7, 0.8))["code"] == "spatial.many_isolated"
end

@testset "clustRegions carries the neighbour graph's sparse-graph QC (#1771)" begin
    # the region runner banks per-image `graphMetrics` of the graph it loaded; the run's QC must flag a
    # sparse one with the graph task's own finding — per image, not pooled across the set
    d = mktempdir(); p = joinpath(d, "region_qc.json")
    open(p, "w") do io
        JSON3.write(io, Dict("nClusters" => 3, "perSegment" => [
            Dict("uID" => "sparse", "valueName" => "B", "n" => 50, "nClusters" => 3, "largestClusterFrac" => 0.4),
            Dict("uID" => "sparse", "valueName" => "T", "n" => 50, "nClusters" => 3, "largestClusterFrac" => 0.4),
            Dict("uID" => "dense",  "valueName" => "B", "n" => 50, "nClusters" => 3, "largestClusterFrac" => 0.4)],
            "graphMetrics" => Dict(
                "sparse" => Dict("nCells" => 100, "nEdges" => 110, "meanDegree" => 2.2, "isolatedFrac" => 0.1),
                "dense"  => Dict("nCells" => 100, "nEdges" => 700, "meanDegree" => 14.6, "isolatedFrac" => 0.0))))
    end
    gf = Cecelia._region_graph_findings(p, "r30")
    @test collect(keys(gf)) == ["sparse"]                              # the dense image is clean
    f = only(gf["sparse"])
    @test f["code"] == "spatial.sparse_graph"                          # the graph task's key + text
    @test f["detail"]["meanDegree"] == 2.2 && f["detail"]["graphSuffix"] == "r30"
    # an older run (no graphMetrics), a missing or an unreadable file → no findings, never a throw
    @test isempty(Cecelia._region_graph_findings(Dict{String,Any}("nClusters" => 3), "r30"))
    @test isempty(Cecelia._region_graph_findings(joinpath(d, "absent.json"), "r30"))
    bad = joinpath(d, "bad.json"); write(bad, "{ not json")
    @test isempty(Cecelia._region_graph_findings(bad, "r30"))

    # banked into the run's own QC: every segment doc of the sparse image carries it, the dense one not
    imgs = [CciaImage(; uid = "sparse", dir = mktempdir()), CciaImage(; uid = "dense", dir = mktempdir())]
    Cecelia.write_cluster_qc!(imgs, "clustRegions.cluster", p; suffix = "reg", image_findings = gf)
    codes(img, vn) = [String(x["code"]) for x in read_qc(img, "clustRegions.cluster", vn)["findings"]]
    @test "spatial.sparse_graph" in codes(imgs[1], "B.reg")
    @test "spatial.sparse_graph" in codes(imgs[1], "T.reg")
    @test !("spatial.sparse_graph" in codes(imgs[2], "B.reg"))
end


@testset "aggregate DBSCAN ids (Clustering.jl)" begin
    # two dense blobs + one far noise point → two aggregates, noise = id 0
    coords = [0.0 0.0; 0.1 0.1; 0.2 0.0; 5.0 5.0; 5.1 5.1; 5.2 5.0; 50.0 50.0]
    ids = Cecelia._aggregate_ids(coords, 0.5, 2)
    @test length(unique(ids[ids .> 0])) == 2                          # two aggregates
    @test ids[end] == 0                                               # far point is noise
    @test count(==(0), ids) == 1                                      # exactly one noise point
    # too-few points → all noise
    @test all(Cecelia._aggregate_ids([0.0 0.0; 0.1 0.1], 0.5, 5) .== 0)
end

# On a timecourse aggregates are found per frame. One stationary cell seen in 6 frames is 6 coincident
# points: pooled (`nothing` frames) they are an "aggregate" of one cell; per frame each is alone → noise.
# Two real 3-cell blobs in different frames keep distinct ids (offset across frames).
@testset "aggregate DBSCAN is per frame" begin
    track = zeros(6, 2); tT = Float64.(0:5)
    @test all(Cecelia._aggregate_ids_by_frame(track, nothing, 1.0, 3) .== 1)
    @test all(Cecelia._aggregate_ids_by_frame(track, tT, 1.0, 3) .== 0)
    blobs = [0.0 0.0; 0.1 0.0; 0.0 0.1; 0.0 0.0; 0.1 0.0; 0.0 0.1]; bT = [0.0, 0.0, 0.0, 1.0, 1.0, 1.0]
    ids = Cecelia._aggregate_ids_by_frame(blobs, bT, 1.0, 3)
    @test ids[1:3] == [1, 1, 1] && ids[4:6] == [2, 2, 2]
    # the DEFAULT run splits by frame — pooling is the opt-in
    @test Cecelia.parse_detect_aggregates_params(Dict{String,Any}()).perTimepoint === true
end

# End to end on a real tracked timecourse with DEFAULT params: every aggregate lies within one frame.
@testset "detectAggregates aggregates within a frame (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "detectAggregates per-frame (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", -1e12, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        res = Cecelia._run_task(DetectAggregates(), img,
                                Dict{String,Any}("pops" => ["B/all"], "clustDiameter" => 30.0, "minCells" => 3);
                                on_log = _ -> nothing)
        @test res !== nothing
        out = label_props(img; value_name="B") |> select_cols(["flow.cell.aggregate.id", "centroid_t"]) |> as_df
        frames_of = Dict{Float64,Set{Float64}}()
        for (a, t) in zip(out[!, "flow.cell.aggregate.id"], out.centroid_t)
            (ismissing(a) || a <= 0) && continue
            push!(get!(frames_of, a, Set{Float64}()), t)
        end
        @test !isempty(frames_of)
        @test all(length(ts) == 1 for ts in values(frames_of))
        rm(td; recursive=true)
    end
end



@testset "cellContacts target-name sanitisation" begin
    # obs column suffix — nothing to do with param validation, which is swept above
    @test Cecelia._contact_target("flow", ["T/qc"]) == "flow.T_qc"
    @test Cecelia._contact_target("flow", ["B/qc", "T/qc"]) == "flow.B_qc+T_qc"
end

# Several targets → one column set each (#1734); grouped by population path
# with the segmentation prefix stripped, so one population split across segmentations stays one group.
@testset "cellContacts target groups" begin
    g = Cecelia._contact_target_groups
    @test g(["B/T", "B/DC"]) == [["B/T"], ["B/DC"]]                      # two targets, first-picked order
    @test g(["B/T", "C/T", "B/DC"]) == [["B/T", "C/T"], ["B/DC"]]        # same pop, two segmentations
    @test g(["B/T", "B/DC"]; combine = true) == [["B/T", "B/DC"]]        # opt-in pooled form
    @test g(["B/T", "B/T"]) == [["B/T"]]                                 # duplicate pick
    @test Cecelia.parse_cell_contacts_params(Dict{String,Any}()).combineTargets === false
    @test Cecelia.parse_contacts_meshes_params(Dict{String,Any}()).combineTargets === false
    @test Cecelia.parse_cell_contacts_params(Dict{String,Any}("combineTargets" => true)).combineTargets
    for task in (CellContacts(), ContactsMeshes())
        good = Dict{String,Any}("popsA" => ["B/a"], "popsB" => ["B/b"], "maxContactDist" => 5.0,
                                "combineTargets" => true)
        @test begin validate_params(task, good); true end
        @test_throws ParamValidationError validate_params(task, merge(good, Dict{String,Any}("combineTargets" => "yes")))
    end
end

# cellContacts on a timecourse searches per frame. A at t=0 sits on top of where B is at t=1 but
# 5 µm from B at t=0: the pooled search (still-image path, `nothing` frames) pairs it across frames
# at 0 µm — the bug — while the per-frame search finds the same-frame B. An A frame with no B → NaN, 0.
@testset "cellContacts nearest B is per frame" begin
    aC = [0.0 0.0 0.0; 50.0 0.0 0.0];  aT = [0.0, 2.0]
    bC = [5.0 0.0 0.0; 0.0 0.0 0.0];   bL = [11, 22];  bT = [0.0, 1.0]
    d, id = Cecelia._nearest_contacts(aC, nothing, bC, bL, nothing)
    @test d[1] == 0.0 && id[1] == 22.0              # cross-frame pairing when frames are ignored
    d, id = Cecelia._nearest_contacts(aC, aT, bC, bL, bT)
    @test d[1] == 5.0 && id[1] == 11.0              # same-frame B only
    @test isnan(d[2]) && id[2] == 0.0               # no B in frame 2
end

# End to end on a real tracked timecourse: every contact pair must share a frame.
@testset "cellContacts pairs within a frame (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "cellContacts per-frame (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0", "centroid_t"]) |> as_df
        @test length(unique(full.centroid_t)) > 1
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "hi"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        add_pop!(m, "lo"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", -1e12, thr, -1e12, 1e12))
        save_pop_map!(m, img)
        res = Cecelia._run_task(CellContacts(), img,
                                Dict{String,Any}("popsA" => ["B/hi"], "popsB" => ["B/lo"], "maxContactDist" => 10.0);
                                on_log = _ -> nothing)
        @test res !== nothing
        @test res["targets"] == ["flow.B_lo"]
        idcol = "flow.cell.contact_id#$(only(res["targets"]))"
        out = label_props(img; value_name="B") |> select_cols([idcol, "centroid_t"]) |> as_df
        t_of = Dict(zip(full.label, full.centroid_t))
        paired = [(l, Int(c)) for (l, c) in zip(out.label, out[!, idcol]) if !ismissing(c) && isfinite(c) && c > 0]
        @test !isempty(paired)
        @test all(t_of[a] == t_of[b] for (a, b) in paired)
        rm(td; recursive=true)
    end
end

# End to end, several targets on a real image: per-target columns, each searching only its own target;
# `combineTargets` = today's pooled set (= the per-target minimum); the same population in two
# segmentations = one set. Intensity tertiles of KDIeEm's B give A ("lo") and two targets ("mid", "hi");
# segmentation "C" is a copy of B, so `B/mid` + `C/mid` must reproduce `B/mid`'s distances.
@testset "cellContacts one column set per target (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "cellContacts per target (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        for vn in ("B", "C"); cp(h5, joinpath(td, "labelProps", "$(vn).h5ad")); end
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["C"] = "C.h5ad"; img.label_props["_active"] = "B"
        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        v = sort(full.mean_intensity_0); t1 = v[cld(nrow(full), 3)]; t2 = v[cld(2 * nrow(full), 3)]
        for vn in ("B", "C")
            m = PopulationMap(pop_type="flow", value_name=vn)
            band(lo, hi) = RectangleGate("mean_intensity_0", "mean_intensity_1", lo, hi, -1e12, 1e12)
            add_pop!(m, "lo"; gate=band(-1e12, t1)); add_pop!(m, "mid"; gate=band(t1, t2))
            add_pop!(m, "hi"; gate=band(t2, 1e12)); add_pop!(m, "none"; gate=band(1e11, 1e12))
            save_pop_map!(m, img)
        end
        labs(pop) = Set(Int.(pop_df(img, "flow", ["B/$(pop)"]; value_name="B").label))
        run(popsB; kw...) = Cecelia._run_task(CellContacts(), img,
            Dict{String,Any}("popsA" => ["B/lo"], "popsB" => popsB, "maxContactDist" => 10.0, kw...);
            on_log = _ -> nothing)
        col(kind, t) = "flow.cell.$(kind)#$(t)"
        read_cols(ts) = label_props(img; value_name="B") |>
            select_cols([col(k, t) for t in ts for k in ("contact", "min_distance", "contact_id")]) |> as_df
        same(a, b) = all(x -> (isnan(x[1]) && isnan(x[2])) || x[1] == x[2], zip(a, b))

        # two targets → two column sets, each pairing only with its own target's cells
        res = run(["B/mid", "B/hi"])
        @test res["targets"] == ["flow.B_mid", "flow.B_hi"]
        per = read_cols(res["targets"]); aRows = per.label .∈ Ref(labs("lo")); per = per[aRows, :]
        for (t, pop) in (("flow.B_mid", "mid"), ("flow.B_hi", "hi"))
            ids = [Int(c) for c in per[!, col("contact_id", t)] if isfinite(c) && c > 0]
            @test !isempty(ids) && issubset(ids, labs(pop))
            @test per[!, col("contact", t)] == Float64.(per[!, col("min_distance", t)] .<= 10.0)
        end
        @test per[!, col("min_distance", "flow.B_mid")] != per[!, col("min_distance", "flow.B_hi")]

        # combineTargets → today's single pooled set = the nearer of the two per-target distances
        logs = String[]
        res = Cecelia._run_task(CellContacts(), img, Dict{String,Any}("popsA" => ["B/lo"],
            "popsB" => ["B/mid", "B/hi"], "maxContactDist" => 10.0, "combineTargets" => true); on_log = l -> push!(logs, l))
        @test res["targets"] == ["flow.B_hi+B_mid"]
        pooled = read_cols(res["targets"])[aRows, :]
        @test same(pooled[!, col("min_distance", "flow.B_hi+B_mid")],
                   min.(per[!, col("min_distance", "flow.B_mid")], per[!, col("min_distance", "flow.B_hi")]))
        # a later per-target run names the pooled columns it leaves beside its own
        logs = String[]
        Cecelia._run_task(CellContacts(), img, Dict{String,Any}("popsA" => ["B/lo"],
            "popsB" => ["B/mid", "B/hi"], "maxContactDist" => 10.0); on_log = l -> push!(logs, l))
        @test any(l -> occursin("earlier combined columns", l) && occursin("B_hi+B_mid", l), logs)

        # the same population from two segmentations → ONE set (pools within its group)
        res = run(["B/mid", "C/mid"])
        @test res["targets"] == ["flow.B_mid+C_mid"]
        cross = read_cols(res["targets"])[aRows, :]
        @test same(cross[!, col("min_distance", "flow.B_mid+C_mid")], per[!, col("min_distance", "flow.B_mid")])

        # a target that resolves no cells is skipped with a QC warning, not a failed run
        res = run(["B/mid", "B/none"])
        @test res["targets"] == ["flow.B_mid"]
        qc = Cecelia.read_qc(img, "spatialAnalysis.cellContacts", "B")
        @test qc !== nothing && qc["metrics"]["nTargets"] == 1
        @test any(f -> f["code"] == "contact.no_target_cells", qc["findings"])
        rm(td; recursive=true)
    end
end

# The MESH route end to end (#1772): Julia resolves the targets, Python meshes the label store and
# splits B per target. The CONTMESH fixture is the one with a label zarr — two rows of A / near / far
# cubes, ~2 µm and ~14 µm surface gaps (python/cecelia/tests/make_contacts_mesh_fixture.py).
@testset "contactsMeshes one column set per target (CONTMESH)" begin
    src = fixture_path("CONTMESH", "1", "CtMsh1", "labels", "seg.zarr")
    if !have_fixture(src)
        @test_skip "contactsMeshes per target (fixture missing)"
    else
        td = mktempdir()
        cp(fixture_path("CONTMESH"), joinpath(td, "CONTMESH"))
        img = Cecelia._load_image(joinpath(td, "CONTMESH", "1", "CtMsh1"))
        m = PopulationMap(pop_type="flow", value_name="seg")
        band(lo, hi) = RectangleGate("mean_intensity_0", "mean_intensity_1", lo, hi, -1e12, 1e12)
        add_pop!(m, "A"; gate=band(50, 150)); add_pop!(m, "near"; gate=band(150, 250))
        add_pop!(m, "far"; gate=band(250, 350))
        save_pop_map!(m, img)
        logs = String[]
        run(popsB; combine = false) = Cecelia._run_task(ContactsMeshes(), img,
            Dict{String,Any}("popsA" => ["seg/A"], "popsB" => popsB, "maxContactDist" => 5.0,
                             "combineTargets" => combine); on_log = l -> push!(logs, l))
        col(kind, t) = "flow.cell.$(kind)#$(t)"
        read_cols(ts) = sort(label_props(img; value_name="seg") |>
            select_cols([col(k, t) for t in ts for k in ("contact", "min_distance", "contact_id")]) |> as_df,
            :label)[[1, 4], :]                                          # the A cells

        # two targets → two column sets, each pairing only with its own target's cells
        res = run(["seg/near", "seg/far"])
        @test res !== nothing || error(join(logs, "\n"))
        @test res["targets"] == ["flow.seg_near", "flow.seg_far"]
        per = read_cols(res["targets"])
        @test per.label == [1, 4]
        @test per[!, col("contact_id", "flow.seg_near")] == [2.0, 5.0]
        @test per[!, col("contact_id", "flow.seg_far")] == [3.0, 6.0]
        dn = per[!, col("min_distance", "flow.seg_near")]; df = per[!, col("min_distance", "flow.seg_far")]
        @test all(d -> 1.0 < d < 3.0, dn) && all(d -> 12.0 < d < 15.0, df)    # µm, x/y scale 1
        @test per[!, col("contact", "flow.seg_near")] == [1.0, 1.0]
        @test per[!, col("contact", "flow.seg_far")] == [0.0, 0.0]
        qc = Cecelia.read_qc(img, "spatialAnalysis.contactsMeshes", "seg")
        @test qc !== nothing && qc["metrics"]["nTargets"] == 2 && qc["metrics"]["nContacts"] == 2

        # combineTargets → one pooled set = the nearer target's distance and id
        res = run(["seg/near", "seg/far"]; combine = true)
        @test res["targets"] == ["flow.seg_far+seg_near"]
        pooled = read_cols(res["targets"])
        @test pooled[!, col("min_distance", "flow.seg_far+seg_near")] == dn
        @test pooled[!, col("contact_id", "flow.seg_far+seg_near")] == [2.0, 5.0]
        @test Cecelia.read_qc(img, "spatialAnalysis.contactsMeshes", "seg")["metrics"]["nTargets"] == 1

        # a later per-target run names the pooled columns it leaves beside its own
        empty!(logs)
        run(["seg/near", "seg/far"])
        @test any(l -> occursin("earlier combined columns", l) && occursin("seg_far+seg_near", l), logs)
        rm(td; recursive=true)
    end
end

@testset "neighbourStats spec — graph knobs live on the graph, not here" begin
    # The graph parameters (method / radius / k) deliberately do NOT live here any more — they belong
    # to the graph this task consumes (`graphSuffix` → spatialAnalysis.cellNeighbours), so a
    # neighbourhood is defined once. (Ranges for what remains are swept above.)
    ns_spec = JSON3.read(read(Cecelia._spec_path(NeighbourStats()), String))
    ns_keys = Set(String(get(p, :key, "")) for p in get(ns_spec, :params, []))
    @test "graphSuffix" in ns_keys && "nPermutations" in ns_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod")
        @test !(gone in ns_keys)
    end
end

@testset "clustRegions spec — graph knobs moved out with the graph" begin
    # regions run ON a neighbour graph and no longer build their own, so the graph knobs moved to
    # cellNeighbours; `perTimepoint` went with them (whether neighbourhoods are per-frame is a
    # property of the graph, so behaviour regions come from choosing a per-timepoint graph).
    cr_keys = Set(String(get(p, :key, ""))
                  for p in get(JSON3.read(read(Cecelia._spec_path(ClustRegions()), String)), :params, []))
    @test "graphSuffix" in cr_keys && "includeOther" in cr_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod", "perTimepoint")
        @test !(gone in cr_keys)
    end
end
