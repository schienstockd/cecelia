# Refine runs — gating with clustering (docs/todo/CLUSTER_REFINE_PLAN.md). A coarse run's cluster
# population is clustered again on its own; that run records what it refines, its populations land under
# the source population, and it reads stale once the source changes.
#
# The cell-table fixture has no cell clustering, so the test writes two through the obs writer:
# `clusters.coarse` (label mod 5, every cell) and `clusters.fine` (label mod 3, only the cells of the
# source population "Lymph" = coarse clusters 1 and 2; NaN elsewhere — what a restricted run writes).

@testset "API: refine run — lineage, nested populations, staleness" begin
  fx = fixture_for(:cells)
  if fx === nothing
    @test_skip "cell-table fixture missing"
  else
    dir = mktempdir()
    cp(fx.project, joinpath(dir, basename(fx.project)))
    puid, uid, vn = basename(fx.project), fx.uid, fx.value_name
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        a = Cecelia.init_object(puid, uid)
        p = img_label_props_path(a, vn)
        labs = (label_props(p) |> select_cols(["label"]) |> as_df)[!, "label"]
        coarse = Float64[mod(l, 5) for l in labs]
        in_lymph = [c in (1.0, 2.0) for c in coarse]
        fine = [in_lymph[i] ? Float64(mod(labs[i], 3)) : NaN for i in eachindex(labs)]
        label_props(p) |> add_obs(Cecelia.DataFrames.DataFrame("label" => labs,
            "clusters.coarse" => coarse, "clusters.fine" => fine)) |> save!
        at1 = "2026-11-24T10:00:00.000"
        record_coarse!(at) = Cecelia._write_clust_features!(p, "coarse", String["mean_intensity_0"], [uid];
                                                            at = at, params = Dict("resolution" => 0.5))

        record_coarse!(at1)
        base = Dict{String,Any}("projectUid" => puid, "imageUid" => uid, "valueName" => vn, "popType" => "clust")
        claude(obj) = handle_http(HTTP.Request("POST", "/api/gating/pop/add-cluster-pops", ["X-Cecelia-Client" => "claude"]),
                                  Vector{UInt8}(JSON3.write(merge(base, obj))))
        gui(f, obj) = _post(f, merge(base, obj))
        clustmap() = load_pop_tree(a; value_name = vn, pop_type = "clust")
        runs() = JSON3.read(api_gating_channels(HTTP.Request("GET",
            "/x?projectUid=$puid&imageUid=$uid&valueName=$vn&popType=clust"))[2]).clusterRuns

        # the coarse level: "Lymph" = clusters 1 + 2, at the root
        @test claude(Dict("run" => "coarse", "groups" => [Dict("name" => "Lymph", "clusters" => [1, 2])]))[1] == 200
        @test haskey(clustmap().pops, "/Lymph")

        # what a run over the population refines: one cluster population → its run + clusters + stamp;
        # anything else → nothing (a plain restricted run)
        src = refine_source([a], ["$vn/Lymph"])
        @test src == Dict{String,Any}("pop" => "/Lymph", "run" => "coarse", "clusters" => [1, 2], "runAt" => at1)
        @test refine_source([a], ["$vn/Lymph", "$vn/other"]) === nothing
        @test refine_source([a], ["$vn/root"]) === nothing
        @test refine_source([a], String[]) === nothing
        @test refine_source([a], ["$vn/Lymph"]; value_name = vn) == src                 # per segment
        @test refine_source([a], ["$vn/Lymph"]; value_name = "elsewhere") === nothing

        # the refine run records its lineage + params; the coarse run has params, no lineage
        Cecelia._write_clust_features!(p, "fine", String["mean_intensity_0"], [uid];
                                       at = "2026-11-24T11:00:00.000", params = Dict("resolution" => 2.0), refines = src)
        r = runs()
        @test r[:fine].params.resolution == 2.0 && r[:coarse].params.resolution == 0.5
        @test String(r[:fine].refines.pop) == "/Lymph" && String(r[:fine].refines.run) == "coarse"
        @test !haskey(r[:fine], :stale) && !haskey(r[:coarse], :refines)
        # …and the observer's cluster summary reads the same lineage
        lin = Cecelia._cluster_runs_lineage([a])
        @test lin["fine"]["refines"] == Dict{String,Any}("pop" => "/Lymph", "run" => "coarse")
        @test lin["fine"]["params"]["resolution"] == 2.0 && !haskey(lin["coarse"], "refines")

        # its populations go under the source: Claude's grouping and the GUI's add with no parent;
        # an explicit parent still wins
        st, body = claude(Dict("run" => "fine", "groups" => [Dict("name" => "T", "clusters" => [0])]))
        @test st == 200
        res = JSON3.read(body)
        @test String(res.populations[1].path) == "/Lymph/T"
        @test res.populations[1].count == count(i -> in_lymph[i] && fine[i] == 0.0, eachindex(labs))
        @test gui(api_gating_pop_add, Dict("name" => "B", "filter" => Dict("measure" => "clusters.fine",
                  "fun" => "in", "values" => [1])))[1] == 200
        @test gui(api_gating_pop_add, Dict("name" => "Loose", "parent" => "root", "filter" => Dict(
                  "measure" => "clusters.fine", "fun" => "in", "values" => [])))[1] == 200
        @test gui(api_gating_pop_add, Dict("name" => "Other", "filter" => Dict("measure" => "clusters.coarse",
                  "fun" => "in", "values" => [3])))[1] == 200
        @test Set(keys(clustmap().pops)) == Set(["/Lymph", "/Lymph/T", "/Lymph/B", "/Loose", "/Other"])

        # stale once the source changes: other clusters picked, its run re-run, its run deleted
        @test gui(api_gating_pop_update, Dict("path" => "/Lymph", "filter" => Dict("values" => [1])))[1] == 200
        @test occursin("other clusters", String(runs()[:fine].stale))
        @test gui(api_gating_pop_update, Dict("path" => "/Lymph", "filter" => Dict("values" => [1, 2])))[1] == 200
        @test !haskey(runs()[:fine], :stale)
        record_coarse!("2026-11-24T12:00:00.000")
        @test occursin("re-run", String(runs()[:fine].stale))
        @test occursin("re-run", Cecelia._cluster_runs_lineage([a])["fine"]["stale"])
        Cecelia.drop_props_sidecar_keys!(Cecelia._clustfeatures_path(p), Cecelia._clustfeatures_raw(p), ["clusters.coarse"])
        @test occursin("no longer exists", String(runs()[:fine].stale))
        # …and a source population that is gone
        record_coarse!(at1)
        @test _post(api_gating_pop_delete, merge(base, Dict{String,Any}("path" => "/Lymph")))[1] == 200
        @test occursin("no longer exists", String(runs()[:fine].stale))
        # an add with no parent on an image without the source refuses, rather than land at the root out
        # of step with the run's other images (the GUI replays one add to each); Claude's grouping too
        @test gui(api_gating_pop_add, Dict("name" => "Late", "filter" => Dict("measure" => "clusters.fine",
                  "fun" => "in", "values" => [])))[1] == 404
        @test claude(Dict("run" => "fine", "groups" => [Dict("name" => "Late", "clusters" => [2])]))[1] == 404
        @test !haskey(clustmap().pops, "/Late")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end
