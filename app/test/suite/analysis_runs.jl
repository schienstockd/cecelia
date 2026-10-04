# ── Per-run delete (app/src/analysis_runs.jl, IMAGE_DELETE_PLAN Decision 14) ─────────────────

# The coverage ratchet: every registered task fun is either a run kind (it can be deleted from the
# Runs tab) or a reasoned NOT_RUN_TASKS entry. A new task that writes a new kind of output fails here
# until somebody decides how its run is deleted — the same KEEP-list discipline as ANALYSIS_KEEP.
@testset "run-kind coverage ratchet" begin
    registered = Set(keys(Cecelia._fun_name_map()))
    kinds      = Set(vcat([k.funs for k in RUN_KINDS]...))
    excused    = Set(keys(NOT_RUN_TASKS))
    @test isempty(setdiff(registered, union(kinds, excused)))   # an unclassified task
    @test isempty(intersect(kinds, excused))                    # classified twice
    @test isempty(setdiff(union(kinds, excused), registered))   # a stale entry for a removed task
    @test allunique([k.kind for k in RUN_KINDS])
end

@testset "drop_obsm removes an embedding, leaves the rest" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "drop_obsm (fixture missing)"
    else
        tmp = joinpath(mktempdir(), "B.h5ad"); cp(h5, tmp)
        keys0 = obsm_keys(label_props(tmp))
        @test "spatial" in keys0
        label_props(tmp) |> drop_obsm(["spatial", "never.existed"]) |> save!
        @test !("spatial" in obsm_keys(label_props(tmp)))
        @test length(obsm_keys(label_props(tmp))) == length(keys0) - 1
        @test nrow(label_props(tmp) |> select_cols(["label"]) |> as_df) > 0   # obs still readable
    end
end

# List → delete round trip over every obs/file-backed kind on one image. Each delete must take its
# own columns/files and nothing a sibling run owns (the shared `X_umap.{suffix}` is the trap: cell
# clusters and regions both write it).
@testset "analysis runs list and delete" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "analysis runs (fixture missing)"
    else
        proj = create_project!(name = "runs-test-$(rand(1000:9999))")
        s    = add_set!(proj; name = "s")
        img  = add_image!(s; name = "a")
        img.label_props = Dict("B" => "B.h5ad"); save!(img)
        props = Cecelia.img_label_props_path(img, "B")
        mkpath(dirname(props)); cp(h5, props)

        labs = (label_props(props) |> select_cols(["label"]) |> as_df).label
        n = length(labs)
        cols = ["clusters.alpha", "regions.alpha", "spatial.comp.B_qc.alpha",
                "live.cell.hmm.state.movement", "live.cell.hmm.transitions.movement",
                "flow.cell.contact#flow.T", "flow.cell.min_distance#flow.T", "flow.cell.contact_id#flow.T",
                "flow.cell.is.aggregate", "flow.cell.aggregate.id", "track_id"]
        df = DataFrame("label" => labs)
        for c in cols; df[!, c] = c == "track_id" ? [i <= 3 ? 1.0 : NaN for i in 1:n] : ones(n); end
        label_props(props) |> add_obs(df) |> save!
        write_json_atomic(replace(props, r"\.h5ad$" => ".clustfeatures.json"),
                          Dict("clusters.alpha" => Dict("features" => ["a"]),
                               "regions.alpha"  => Dict("features" => ["b"])))
        qcf = Cecelia.qc_path(img, "clustPops.cluster", "B.alpha"); mkpath(dirname(qcf)); write(qcf, "{}")
        mkpath(Cecelia.img_spatial_graph_dir(img)); write(Cecelia.img_spatial_graph_path(img, "g1"), "x")
        mkpath(Cecelia.img_stats_dir(img));         write(Cecelia.img_stats_path(img, "s1"), "{}")

        img = init_object(proj.uid, img.uid)
        runs = list_analysis_runs(img)
        has(kind, key, vn = "") = any(r -> r.kind == kind && r.key == key && r.value_name == vn, runs)
        @test has("clusters", "alpha") && has("regions", "alpha") && has("hmm", "movement")
        @test has("contacts", "flow#flow.T", "B") && has("aggregates", "flow", "B")
        @test has("graphs", "g1") && has("stats", "s1")
        @test has("tracks", "", "B")                         # legacy, unattributed tracks
        @test only(filter(r -> r.kind == "clusters", runs)).value_names == ["B"]

        obs() = col_names(label_props(props); data_type = :obs)

        # cell clusters: the column, its sidecar key and its QC go; the embedding STAYS (regions owns it too)
        @test delete_analysis_run!(img, "clusters", "alpha")
        @test !("clusters.alpha" in obs()) && "regions.alpha" in obs()
        sc = JSON3.read(read(replace(props, r"\.h5ad$" => ".clustfeatures.json"), String))
        @test !haskey(sc, Symbol("clusters.alpha")) && haskey(sc, Symbol("regions.alpha"))
        @test !isfile(qcf)

        # regions: its composition columns go with it, and the last sidecar key takes the file
        @test delete_analysis_run!(init_object(proj.uid, img.uid), "regions", "alpha")
        @test !any(c -> startswith(c, "spatial.comp.") || c == "regions.alpha", obs())
        @test !isfile(replace(props, r"\.h5ad$" => ".clustfeatures.json"))

        @test delete_analysis_run!(img, "hmm", "movement")
        @test !any(c -> startswith(c, "live.cell.hmm."), obs())
        @test delete_analysis_run!(img, "contacts", "flow#flow.T", "B")
        @test !any(c -> occursin("#flow.T", c), obs())
        @test delete_analysis_run!(img, "aggregates", "flow", "B")
        @test !any(c -> occursin("aggregate", c), obs())
        @test delete_analysis_run!(img, "graphs", "g1") && !isfile(Cecelia.img_spatial_graph_path(img, "g1"))
        @test delete_analysis_run!(img, "stats", "s1")  && !isfile(Cecelia.img_stats_path(img, "s1"))

        # untouched: the base table and the tracks nobody deleted
        @test "track_id" in obs()
        # absent run → false (a multi-image selection skips images without it), unknown kind → error
        @test !delete_analysis_run!(img, "hmm", "movement")
        @test_throws ArgumentError delete_analysis_run!(img, "nope", "x")

        rm(proj.root; recursive = true)
    end
end
