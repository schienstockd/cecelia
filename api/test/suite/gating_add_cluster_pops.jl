# `POST /api/gating/pop/add-cluster-pops` — the observer's `add_cluster_pops`: Claude's GROUPING of a
# clustering run, several clusters per population, written to every image of the run. Plus what came with
# it for every cluster population: one population per cluster, enforced by the server on the GUI's tick
# too, and the run stamp (`runAt`) that marks a population picked on an earlier run under the same name.
#
# The testpr image's cell table has no cell clustering, so the test writes one — `clusters.immune`, five
# clusters by label — through the obs writer, onto the image and a second image derived from KDIeEx
# (TgtImg: a known third fewer cells, so a count that reads one image twice shows), and records the
# run's manifest on both: a run spanning two images, as a set-wide clustering does.

@testset "API: add-cluster-pops groups a run's clusters — set-wide, all or nothing" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    tgt = joinpath(proj, "1", "TgtImg")
    cp(joinpath(proj, "1", "KDIeEx"), tgt)    # disagrees with KDIeEm (test-data/fixtures.toml)
    rm(joinpath(tgt, "gating"); recursive = true, force = true)
    ccid = joinpath(tgt, "ccid.json")
    write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEx\"" => "\"uid\": \"TgtImg\""))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        a, b = Cecelia.init_object("testpr", "KDIeEm"), Cecelia.init_object("testpr", "TgtImg")
        for im in (a, b)
            p = img_label_props_path(im, "B")
            labs = (label_props(p) |> select_cols(["label"]) |> as_df)[!, "label"]
            label_props(p) |> add_obs(Cecelia.DataFrames.DataFrame("label" => labs,
                "clusters.immune" => Float64[mod(l, 5) for l in labs])) |> save!
        end
        record_run!(at) = for im in (a, b)
            Cecelia._write_clust_features!(img_label_props_path(im, "B"), "immune", String["mean_intensity_0"],
                                           ["KDIeEm", "TgtImg"]; at = at)
        end
        at1 = "2026-11-16T10:00:00.000"
        record_run!(at1)

        ids = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm", "valueName" => "B",
                               "popType" => "clust", "run" => "immune")
        claude(route, obj) = handle_http(HTTP.Request("POST", route, ["X-Cecelia-Client" => "claude"]),
                                         Vector{UInt8}(JSON3.write(merge(ids, obj))))
        add(obj) = claude("/api/gating/pop/add-cluster-pops", obj)
        gui(f, obj) = _post(f, merge(Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                                      "valueName" => "B", "popType" => "clust"), obj))
        channels() = JSON3.read(api_gating_channels(HTTP.Request("GET",
            "/x?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=clust"))[2])
        clustmap(im) = load_pop_map(im; value_name = "B", pop_type = "clust")

        # the run's clusters, as the cluster page offers them, and its stamp
        ch = channels()
        universe = sort!(Int.(collect(ch.clusterIds[:immune])))
        @test length(universe) >= 4
        @test String(ch.clusterRunAt[:immune]) == at1
        c1, c2, c3, c4 = universe[1:4]
        col = label_props(img_label_props_path(a, "B")) |> select_cols(["clusters.immune"]) |> as_df
        colb = label_props(img_label_props_path(b, "B")) |> select_cols(["clusters.immune"]) |> as_df
        n_of(c, cs) = count(x -> isfinite(x) && Int(round(x)) in cs, c[!, "clusters.immune"])
        n_in(cs) = n_of(col, cs) + n_of(colb, cs)      # each image's own cells, not one image twice
        @test length(colb.label) < length(col.label)
        grouping = [Dict("name" => "T", "clusters" => [c2, c1]),        # several clusters → one population
                    Dict("name" => "B cells", "clusters" => [c3])]

        # dry run: the counts over BOTH images of the run, nothing written
        st, body = add(Dict("groups" => grouping, "dryRun" => true))
        r = JSON3.read(body)
        @test st == 200 && r.dryRun
        @test Set(String.(r.images)) == Set(["KDIeEm", "TgtImg"]) && isempty(r.skipped)
        @test r.populations[1].count == n_in((c1, c2)) && r.populations[2].count == n_in((c3,))
        @test r.populations[1].clusters == [c1, c2]                   # sorted, as the manager stores them
        @test sort(Int.(r.unassigned)) == setdiff(universe, [c1, c2, c3])
        @test isempty(clustmap(a).pops) && isempty(clustmap(b).pops)

        # the write: one stamped cluster population per group, on every image of the run
        st, body = add(Dict("groups" => grouping))
        @test st == 200
        r = JSON3.read(body)
        @test String(r.runAt) == at1 && String(r.createdBy.via) == "claude"
        for im in (a, b)
            m = clustmap(im)
            @test Set(keys(m.pops)) == Set(["/T", "/B cells"])
            p = m.pops["/T"]
            @test p.filter_measure == "clusters.immune" && string(p.filter_fun) == "in" &&
                  sort(Int.(p.filter_values)) == [c1, c2]
            @test p.created_by["via"] == "claude" && p.run_at == at1
            @test m.pops["/T"].colour != m.pops["/B cells"].colour      # palette, one colour each
        end

        # all or nothing — every refusal before anything is written
        @test add(Dict("groups" => [Dict("name" => "T", "clusters" => [c4])]))[1] == 409          # name taken
        st, body = add(Dict("groups" => [Dict("name" => "NK", "clusters" => [c4]),
                                         Dict("name" => "More", "clusters" => [c1])]))
        @test st == 409 && JSON3.read(body).code == "cluster_taken"     # c1 is T's — and NK isn't written either
        @test add(Dict("groups" => [Dict("name" => "X", "clusters" => [c4]),
                                    Dict("name" => "Y", "clusters" => [c4])]))[1] == 422          # one cluster, two groups
        @test add(Dict("groups" => [Dict("name" => "X", "clusters" => [9999])]))[1] == 422        # not in the run
        @test add(Dict("groups" => [Dict("name" => "X", "clusters" => [])]))[1] == 422
        @test add(Dict("groups" => [Dict("name" => "X", "clusters" => [c4]),
                                    Dict("name" => "X", "clusters" => [universe[end]])]))[1] == 422
        @test add(Dict("run" => "nope", "groups" => grouping))[1] == 404
        @test add(Dict("popType" => "flow", "groups" => grouping))[1] == 422
        @test Set(keys(clustmap(a).pops)) == Set(["/T", "/B cells"]) &&
              Set(keys(clustmap(b).pops)) == Set(["/T", "/B cells"])

        # one population per cluster on the GUI's tick too: adding a held cluster is refused; the
        # manager's move (off the holder first, then on) goes through, and re-stamps the run
        st, body = gui(api_gating_pop_update, Dict("path" => "/B cells", "filter" => Dict("values" => [c3, c1])))
        @test st == 409 && JSON3.read(body).code == "cluster_taken"
        @test gui(api_gating_pop_update, Dict("path" => "/T", "filter" => Dict("values" => [c2])))[1] == 200
        @test gui(api_gating_pop_update, Dict("path" => "/B cells", "filter" => Dict("values" => [c3, c1])))[1] == 200
        # …and on the GUI's add, for a population created with ids
        @test gui(api_gating_pop_add, Dict("name" => "dup", "filter" => Dict("measure" => "clusters.immune",
                  "fun" => "in", "values" => [c3])))[1] == 409

        # stale: re-running under the same name moves the run's stamp; the populations keep theirs
        # until someone re-picks their clusters
        at2 = "2026-11-17T09:30:00.000"
        record_run!(at2)
        @test String(channels().clusterRunAt[:immune]) == at2
        @test clustmap(a).pops["/T"].run_at == at1
        # a tick on a stale population leaves its old-numbered ids, so it does NOT clear the warning…
        @test gui(api_gating_pop_update, Dict("path" => "/T", "filter" => Dict("values" => [c2, c4])))[1] == 200
        @test clustmap(a).pops["/T"].run_at == at1
        # …nor does the filter form re-sending the same column (a rename or recolour through it)
        @test gui(api_gating_pop_update, Dict("path" => "/T", "filter" => Dict("measure" => "clusters.immune",
                  "fun" => "in", "values" => [c2, c4])))[1] == 200
        @test clustmap(a).pops["/T"].run_at == at1
        # …only the user saying its clusters are checked does (the stale icon's click)
        @test gui(api_gating_pop_update, Dict("path" => "/T", "runChecked" => true))[1] == 200
        @test clustmap(a).pops["/T"].run_at == at2
        @test gui(api_gating_pop_update, Dict("path" => "/T", "filter" => Dict("values" => [c2])))[1] == 200
        # the GUI's new (empty) cluster pop is stamped with the run it was made on
        @test gui(api_gating_pop_add, Dict("name" => "Pop 3", "filter" => Dict("measure" => "clusters.immune",
                  "fun" => "in", "values" => [])))[1] == 200
        @test clustmap(a).pops["/Pop 3"].run_at == at2

        # an image re-clustered ALONE under the same name is not part of the run any more: skipped
        Cecelia._write_clust_features!(img_label_props_path(b, "B"), "immune", String["mean_intensity_0"],
                                       ["TgtImg"]; at = "2026-11-18T08:00:00.000")
        st, body = add(Dict("groups" => [Dict("name" => "Solo", "clusters" => [c4])], "dryRun" => true))
        r = JSON3.read(body)
        @test st == 200 && String.(r.images) == ["KDIeEm"] && String.(r.skipped) == ["TgtImg"]
        record_run!(at2)

        # delete-own: Claude's grouping goes from every image of the run; the user's edit made T theirs
        @test add(Dict("groups" => [Dict("name" => "NK", "clusters" => [c4])]))[1] == 200
        @test Set(keys(clustmap(b).pops)) ⊇ Set(["/NK"])
        del(path, extra = Dict{String,Any}()) = claude("/api/gating/pop/delete-own", merge(Dict{String,Any}("path" => path), extra))
        st, body = del("/T")
        @test st == 403 && JSON3.read(body).code == "not_claude_made"
        st, body = del("/NK", Dict{String,Any}("dryRun" => true))
        @test st == 200 && Set(String.(JSON3.read(body).images)) == Set(["KDIeEm", "TgtImg"])
        @test haskey(clustmap(a).pops, "/NK")
        @test del("/NK")[1] == 200
        @test !haskey(clustmap(a).pops, "/NK") && !haskey(clustmap(b).pops, "/NK")
        @test haskey(clustmap(b).pops, "/T")                             # the user's untouched
        # delete-own without popType is still flow-only — the cluster population isn't in that map
        @test claude("/api/gating/pop/delete-own", Dict{String,Any}("path" => "/T", "popType" => "flow"))[1] == 404
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: add-cluster-pops on a track run (trackclust)" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        ch = JSON3.read(api_gating_channels(HTTP.Request("GET",
            "/x?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=trackclust"))[2])
        universe = sort!(Int.(collect(ch.clusterIds[:movement])))
        @test length(universe) >= 2
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm", "valueName" => "B",
                                "popType" => "trackclust")
        add(groups) = handle_http(HTTP.Request("POST", "/api/gating/pop/add-cluster-pops", ["X-Cecelia-Client" => "claude"]),
            Vector{UInt8}(JSON3.write(merge(base, Dict{String,Any}("run" => "movement", "groups" => groups)))))
        fast = [Dict("name" => "fast", "clusters" => universe[1:2])]
        # the fixture's own Scanning / Directed already hold these — the user's, so refused
        st, body = add(fast)
        @test st == 409 && JSON3.read(body).code == "cluster_taken"
        for name in ("Scanning", "Directed")             # the user frees them, through the GUI's delete
            @test _post(api_gating_pop_delete, merge(base, Dict{String,Any}("path" => "/$name")))[1] == 200
        end
        st, body = add(fast)
        @test st == 200
        r = JSON3.read(body)
        @test r.images == ["KDIeEm"] && r.populations[1].count > 0       # tracks, not cells
        @test r.runAt === nothing                                         # a run from before the stamp
        img = Cecelia.init_object("testpr", "KDIeEm")
        p = load_pop_map(img; value_name = "B", pop_type = "trackclust").pops["/fast"]
        @test p.filter_measure == "clusters.movement" && p.run_at === nothing
        # set-cluster-pops on the track run: regroup + rename Claude's population, counted in tracks
        st, body = handle_http(HTTP.Request("POST", "/api/gating/pop/set-cluster-pops", ["X-Cecelia-Client" => "claude"]),
            Vector{UInt8}(JSON3.write(merge(base, Dict{String,Any}("run" => "movement",
                "groups" => [Dict("path" => "/fast", "clusters" => universe[1:1], "name" => "quick")])))))
        @test st == 200
        r = JSON3.read(body)
        @test String(r.populations[1].path) == "/quick" && 0 < r.populations[1].count < r.total
        m = load_pop_map(img; value_name = "B", pop_type = "trackclust")
        @test !haskey(m.pops, "/fast") && Int.(m.pops["/quick"].filter_values) == universe[1:1]
        @test Int(universe[2]) in Int.(r.unassigned)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# `POST /api/gating/pop/set-cluster-pops` — the observer's `set_cluster_pops`: Claude regroups, renames and
# recolours the cluster populations IT made, on every image of the run, under set-own-gate's ownership rules.
@testset "API: set-cluster-pops regroups Claude's own cluster populations — set-wide, all or nothing" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    tgt = joinpath(proj, "1", "TgtImg")
    cp(joinpath(proj, "1", "KDIeEx"), tgt)    # disagrees with KDIeEm (test-data/fixtures.toml)
    rm(joinpath(tgt, "gating"); recursive = true, force = true)
    ccid = joinpath(tgt, "ccid.json")
    write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEx\"" => "\"uid\": \"TgtImg\""))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        a, b = Cecelia.init_object("testpr", "KDIeEm"), Cecelia.init_object("testpr", "TgtImg")
        for im in (a, b)
            p = img_label_props_path(im, "B")
            labs = (label_props(p) |> select_cols(["label"]) |> as_df)[!, "label"]
            label_props(p) |> add_obs(Cecelia.DataFrames.DataFrame("label" => labs,
                "clusters.immune" => Float64[mod(l, 5) for l in labs])) |> save!
        end
        record_run!(at) = for im in (a, b)
            Cecelia._write_clust_features!(img_label_props_path(im, "B"), "immune", String["mean_intensity_0"],
                                           ["KDIeEm", "TgtImg"]; at = at)
        end
        at1, at2 = "2026-11-16T10:00:00.000", "2026-11-17T09:30:00.000"
        record_run!(at1)
        ids = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm", "valueName" => "B",
                               "popType" => "clust", "run" => "immune")
        claude(route, obj) = handle_http(HTTP.Request("POST", route, ["X-Cecelia-Client" => "claude"]),
                                         Vector{UInt8}(JSON3.write(merge(ids, obj))))
        add(groups) = claude("/api/gating/pop/add-cluster-pops", Dict{String,Any}("groups" => groups))
        set(groups, extra = Dict{String,Any}()) =
            claude("/api/gating/pop/set-cluster-pops", merge(Dict{String,Any}("groups" => groups), extra))
        gui(f, obj) = _post(f, merge(Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                                      "valueName" => "B", "popType" => "clust"), obj))
        clustmap(im) = load_pop_map(im; value_name = "B", pop_type = "clust")
        ids_of(im, path) = sort(Int.(clustmap(im).pops[path].filter_values))

        ch = JSON3.read(api_gating_channels(HTTP.Request("GET",
            "/x?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=clust"))[2])
        universe = sort!(Int.(collect(ch.clusterIds[:immune])))
        c1, c2, c3, c4 = universe[1:4]
        col = label_props(img_label_props_path(a, "B")) |> select_cols(["clusters.immune"]) |> as_df
        colb = label_props(img_label_props_path(b, "B")) |> select_cols(["clusters.immune"]) |> as_df
        n_of(c, cs) = count(x -> isfinite(x) && Int(round(x)) in cs, c[!, "clusters.immune"])
        n_in(cs) = n_of(col, cs) + n_of(colb, cs)      # each image's own cells, not one image twice
        @test length(colb.label) < length(col.label)
        @test add([Dict("name" => "T", "clusters" => [c1, c2]), Dict("name" => "B cells", "clusters" => [c3])])[1] == 200

        # moving c2 from T to B cells is ONE call: the grouping is checked as it will be after it.
        # Dry first — the counts, nothing written
        move = [Dict("path" => "/T", "clusters" => [c1]), Dict("path" => "/B cells", "clusters" => [c3, c2])]
        st, body = set(move, Dict{String,Any}("dryRun" => true))
        r = JSON3.read(body)
        @test st == 200 && r.dryRun
        @test r.populations[1].count == n_in((c1,)) && r.populations[2].count == n_in((c2, c3))
        @test Int.(r.populations[2].clusters) == [c2, c3]
        @test sort(Int.(r.unassigned)) == setdiff(universe, [c1, c2, c3])
        @test ids_of(a, "/T") == [c1, c2] && ids_of(b, "/T") == [c1, c2]
        st, body = set(move)
        @test st == 200
        for im in (a, b)
            @test ids_of(im, "/T") == [c1] && ids_of(im, "/B cells") == [c2, c3]
            p = clustmap(im).pops["/T"]
            @test p.updated_by["via"] == "claude" && claude_made(p)
        end

        # rename + recolour: the path moves on every image; a rename alone leaves the run stamp
        record_run!(at2)
        st, body = set([Dict("path" => "/T", "name" => "T cells", "colour" => "#112233")])
        @test st == 200 && String(JSON3.read(body).populations[1].path) == "/T cells"
        for im in (a, b)
            m = clustmap(im)
            @test !haskey(m.pops, "/T") && m.pops["/T cells"].colour == "#112233" && m.pops["/T cells"].run_at == at1
        end
        # re-picking the clusters is checking them against the run as it is now: the stamp moves
        @test set([Dict("path" => "/T cells", "clusters" => [c1])])[1] == 200
        @test clustmap(a).pops["/T cells"].run_at == at2 && clustmap(b).pops["/T cells"].run_at == at2

        # refusals — each before anything is written, on either image
        before = (clustmap(a).pops["/T cells"].filter_values, clustmap(b).pops["/B cells"].filter_values)
        @test gui(api_gating_pop_add, Dict("name" => "Mine", "filter" => Dict("measure" => "clusters.immune",
                  "fun" => "in", "values" => [c4])))[1] == 200                 # the user's population, on a only
        st, body = set([Dict("path" => "/T cells", "clusters" => [c1, c4])])
        @test st == 409 && JSON3.read(body).code == "cluster_taken"           # the user's cluster
        st, body = set([Dict("path" => "/Mine", "clusters" => [c4, c1])])
        @test st == 403 && JSON3.read(body).code == "not_claude_made"         # the user's population
        @test set([Dict("path" => "/T cells", "name" => "B cells")])[1] == 409          # name taken
        @test set([Dict("path" => "/T cells", "clusters" => [9999])])[1] == 422         # not in the run
        @test set([Dict("path" => "/T cells", "clusters" => [c1]),
                   Dict("path" => "/T cells", "colour" => "#000000")])[1] == 422        # one path twice
        @test set([Dict("path" => "/T cells", "clusters" => [c2]),
                   Dict("path" => "/B cells", "clusters" => [c2])])[1] == 422           # one cluster, two groups
        @test set([Dict("path" => "/T cells")])[1] == 422                               # changes nothing
        # a population renamed but not regrouped keeps its clusters — another may not take one
        st, body = set([Dict("path" => "/T cells", "name" => "T2"), Dict("path" => "/B cells", "clusters" => [c1, c2, c3])])
        @test st == 409 && JSON3.read(body).code == "cluster_taken"
        @test set([Dict("path" => "/nope", "clusters" => [c1])])[1] == 404
        @test set([Dict("path" => "/T cells", "clusters" => [c1])], Dict{String,Any}("run" => "nope"))[1] == 404
        @test (clustmap(a).pops["/T cells"].filter_values, clustmap(b).pops["/B cells"].filter_values) == before

        # the user's edit makes Claude's population theirs
        @test gui(api_gating_pop_update, Dict("path" => "/B cells", "colour" => "#abcdef"))[1] == 200
        st, body = set([Dict("path" => "/B cells", "clusters" => [c3])])
        @test st == 403 && JSON3.read(body).code == "not_claude_made"
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end
