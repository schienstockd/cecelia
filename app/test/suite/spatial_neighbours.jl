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



@testset "cellContacts target-name sanitisation" begin
    # obs column suffix — nothing to do with param validation, which is swept above
    @test Cecelia._contact_target("flow", ["T/qc"]) == "flow.T_qc"
    @test Cecelia._contact_target("flow", ["B/qc", "T/qc"]) == "flow.B_qc+T_qc"
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
        idcol = "flow.cell.contact_id#$(res["target"])"
        out = label_props(img; value_name="B") |> select_cols([idcol, "centroid_t"]) |> as_df
        t_of = Dict(zip(full.label, full.centroid_t))
        paired = [(l, Int(c)) for (l, c) in zip(out.label, out[!, idcol]) if !ismissing(c) && isfinite(c) && c > 0]
        @test !isempty(paired)
        @test all(t_of[a] == t_of[b] for (a, b) in paired)
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
