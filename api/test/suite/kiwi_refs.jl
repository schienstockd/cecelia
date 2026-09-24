# ── Kiwi refs — shape check + resolver (docs/todo/KIWI_ASSISTANT_PLAN.md Phase 2) ──────────────────
# One testset for the schema shape check (pure), one for the resolver against the committed `testpr`
# fixture (KDIeEm: 1377 cells, 62 tracks, a trackclust gating sidecar). Each kind is pinned both ways —
# a real object resolves AND a near-miss fails — so a resolver that says "yes" to everything can't pass.
# The wrong-kind cases are the ones Phase 0 saw the model produce (an image uid filed as a track).

@testset "Kiwi refs — shape check (the shared schema)" begin
    @test Set(KIWI_REF_KINDS) == Set(["project", "set", "image", "population", "cells", "tracks",
        "viewer", "plot", "tile", "capture", "task", "ui", "blackboard"])
    ok(r) = kiwi_ref_shape_error(r) == ""
    @test ok(Dict("kind" => "image", "imageUid" => "KDIeEm"))
    @test ok(Dict("kind" => "tracks", "imageUid" => "KDIeEm", "valueName" => "B", "trackIds" => [1, 2]))
    @test ok(Dict("kind" => "plot", "plotId" => "p1", "u" => 0.5, "v" => 1))
    @test ok(Dict("kind" => "project"))
    # the Phase 0 failure: a flat {kind,id} is not a ref of any kind
    @test occursin("missing imageUid", kiwi_ref_shape_error(Dict("kind" => "tracks", "id" => "VJy1Nx")))
    @test occursin("missing", kiwi_ref_shape_error(Dict("kind" => "task", "id" => "get_project_info")))
    @test occursin("unknown field", kiwi_ref_shape_error(Dict("kind" => "image", "imageUid" => "a", "t" => 1)))
    @test occursin("unknown kind", kiwi_ref_shape_error(Dict("kind" => "gate")))
    @test kiwi_ref_shape_error(Dict("imageUid" => "a")) == "missing kind"
    @test kiwi_ref_shape_error("image") == "a ref must be an object"
    # types, ranges, empties
    @test occursin("trackIds must not be empty", kiwi_ref_shape_error(
        Dict("kind" => "tracks", "imageUid" => "a", "valueName" => "B", "trackIds" => Int[])))
    @test occursin("items must be a whole number", kiwi_ref_shape_error(
        Dict("kind" => "cells", "imageUid" => "a", "valueName" => "B", "labelIds" => [1.5])))
    @test occursin("must be a number", kiwi_ref_shape_error(Dict("kind" => "viewer", "imageUid" => "a", "t" => true)))
    @test occursin("≥ 0", kiwi_ref_shape_error(Dict("kind" => "viewer", "imageUid" => "a", "t" => -1)))
    @test occursin("≤ 1", kiwi_ref_shape_error(Dict("kind" => "plot", "plotId" => "p", "u" => 1.5)))
    @test occursin("wrong format", kiwi_ref_shape_error(
        Dict("kind" => "tile", "imageUid" => "a", "valueName" => "B", "cellId" => "b3")))
    @test occursin("must not be empty", kiwi_ref_shape_error(Dict("kind" => "image", "imageUid" => "")))
    # JSON3 bodies arrive with Symbol keys — the check must read those too
    @test kiwi_ref_shape_error(JSON3.read("""{"kind":"image","imageUid":"KDIeEm"}""")) == ""
end

@testset "Kiwi refs — resolver against the testpr fixture" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        res(r) = resolve_kiwi_ref("testpr", r)
        yes(r) = (x = res(r); x["ok"] || @info "unexpected failure" r x; x["ok"])
        no(r, needle) = (x = res(r); !x["ok"] && occursin(needle, x["error"]))
        img = Cecelia.init_object("testpr", "KDIeEm")

        # project / image / set — and the wrong-kind case
        @test res(Dict("kind" => "project")) == Dict("ok" => true, "check" => "exists",
                                                     "label" => "Cecelia test fixtures", "error" => "")
        @test yes(Dict("kind" => "image", "imageUid" => "KDIeEm"))
        @test res(Dict("kind" => "image", "imageUid" => "KDIeEm"))["label"] == img.name
        @test no(Dict("kind" => "image", "imageUid" => "zzzzzz"), "no image")
        @test no(Dict("kind" => "image", "imageUid" => "../../etc"), "invalid image id")
        @test no(Dict("kind" => "set", "setUid" => "KDIeEm"), "is an image, not a set")
        @test no(Dict("kind" => "set", "setUid" => "zzzzzz"), "no set")

        # cells: real label ids resolve; a missing one is NAMED in the error
        labels = sort!(collect(parse.(Int, string.(as_df(select_cols(label_props(img; value_name = "B"), ["label"])).label))))
        @test length(labels) == 1377
        @test yes(Dict("kind" => "cells", "imageUid" => "KDIeEm", "valueName" => "B", "labelIds" => labels[1:3]))
        @test no(Dict("kind" => "cells", "imageUid" => "KDIeEm", "valueName" => "B",
                      "labelIds" => [labels[1], 999_999_999]), "999999999")
        @test no(Dict("kind" => "cells", "imageUid" => "KDIeEm", "valueName" => "nope", "labelIds" => [1]),
                 "no segmentation")

        # tracks: ids from the per-track table; a cell label is not automatically a track id
        tids = sort!(collect(parse.(Int, string.(as_df(select_cols(label_props(img_track_props_path(img, "B")), ["label"])).label))))
        @test length(tids) == 62
        @test yes(Dict("kind" => "tracks", "imageUid" => "KDIeEm", "valueName" => "B", "trackIds" => tids[1:2]))
        @test no(Dict("kind" => "tracks", "imageUid" => "KDIeEm", "valueName" => "B", "trackIds" => [999_999]), "999999")
        # the Phase 0 citation, now expressible only as a shape error
        @test res(Dict("kind" => "tracks", "id" => "KDIeEm"))["check"] == "shape"

        # population: a path in the trackclust sidecar resolves, with its display name; others don't
        m = load_pop_map(img; value_name = "B", pop_type = "trackclust", backfill_save = false)
        p1 = first(sort!(collect(Cecelia.pop_paths(m))))
        r = res(Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => p1))
        @test r["ok"] && occursin(Cecelia.pop_at(m, p1).name, r["label"]) && occursin(" · B · ", r["label"])   # which segmentation
        @test yes(Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => "/_tracked"))
        @test no(Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => "/nope"),
                 "no population")
        # read-only: resolving touched no gating file
        gdir = joinpath(dir, "testpr", "1", "KDIeEm", "gating")
        @test readdir(gdir) == ["B__trackclust.json"]

        # a population's cells — what a click on it outlines in the viewer
        c = kiwi_population_cells("testpr", Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => p1))
        @test !(c isa String) && c.total > 0 && c.popType == "trackclust" && issorted(c.labelIds) && all(>(0), c.labelIds)
        small = kiwi_population_cells("testpr", Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => p1); limit = 1)
        @test length(small.labelIds) == 1 && small.truncated == (c.total > 1)
        @test occursin("no population", kiwi_population_cells("testpr", Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => "/nope")))
        @test kiwi_population_cells("testpr", Dict("kind" => "image", "imageUid" => "KDIeEm")) == "not a population ref"
        st, _ = api_kiwi_refs_cells(Vector{UInt8}("""{"projectUid":"testpr","ref":{"kind":"population","imageUid":"KDIeEm","valueName":"B","popPath":"/nope"}}"""))
        @test st == 404
        @test readdir(gdir) == ["B__trackclust.json"]                    # still read-only

        # viewer: an image with no pixels on disk has nothing to view — a failure, not a pass (testpr
        # ships no zarr; 2 of obWDNS's real images are unconverted the same way)
        @test no(Dict("kind" => "viewer", "imageUid" => "KDIeEm", "t" => 0), "no image data to view")
        # …give it real pixels (ZARRFMT's v2 store) and the range check reads the zarr's own extent
        v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
        if api_have_fixture(v2)
            zdst = joinpath(dir, "testpr", "0", "KDIeEm", "ccidImage.ome.zarr")
            mkpath(dirname(zdst)); cp(v2, zdst)
            g = image_geometry(zdst)
            @test yes(Dict("kind" => "viewer", "imageUid" => "KDIeEm"))
            @test yes(Dict("kind" => "viewer", "imageUid" => "KDIeEm", "t" => g.sizeT - 1, "z" => g.sizeZ - 1))
            @test no(Dict("kind" => "viewer", "imageUid" => "KDIeEm", "t" => g.sizeT), "outside")
            @test no(Dict("kind" => "viewer", "imageUid" => "KDIeEm", "z" => g.sizeZ), "outside")
            rm(zdst; recursive = true)
        end

        # plot + tile are LIVE: absent until the browser publishes, present after, gone when removed
        pref = Dict("kind" => "plot", "plotId" => "kiwi-test-plot")
        @test res(pref)["check"] == "live" && no(pref, "isn’t open")
        lock(_PLOTS_LOCK) do
            get!(_PLOTS_BY_PROJECT, "testpr", Dict{String,PlotEntry}())["kiwi-test-plot"] =
                PlotEntry("kiwi-test-plot", "c1", "summary", "Speed by pop", "/analysis", String[], nothing,
                          Dict{String,Any}(), time(), "testpr")
        end
        try
            @test res(pref) == Dict("ok" => true, "check" => "live", "label" => "Speed by pop", "error" => "", "detail" => "", "route" => "/analysis")
            # what the panel publishes becomes the label (measure) and the detail (series, grouping, images)
            lock(_PLOTS_LOCK) do
                _PLOTS_BY_PROJECT["testpr"]["kiwi-test-plot"] =
                    PlotEntry("kiwi-test-plot", "c1", "summary", "Track measures", "/analysis", String[], nothing,
                              Dict{String,Any}("measure" => "live.track.speed", "yLabel" => "speed",
                                               "series" => ["B/qc", "T/qc"], "groupBy" => "hmm.state",
                                               "imageUid" => "KDIeEm"), time(), "testpr")
            end
            r = res(pref)
            @test r["label"] == "Track measures · speed"
            @test startswith(r["detail"], "B/qc, T/qc · by hmm.state · ") && length(r["detail"]) > 30
        finally
            lock(_PLOTS_LOCK) do; delete!(_PLOTS_BY_PROJECT, "testpr") end
        end
        @test !res(pref)["ok"]
        tref = Dict("kind" => "tile", "imageUid" => "KDIeEm", "valueName" => "B", "cellId" => "B3", "t" => 0)
        @test no(tref, "no landscape")
        key = ("testpr", "KDIeEm", "B", 0, -1)
        lock(_LANDSCAPE_LOCK) do
            _LANDSCAPE_BY_KEY[key] = Landscape("testpr", "KDIeEm", "B", 0, -1, _now_epoch(),
                Dict{String,Any}("tiles" => [Dict("id" => "A1"), Dict("id" => "B3")]))
        end
        try
            @test res(tref)["ok"] && res(tref)["check"] == "live"
            @test no(merge(tref, Dict("cellId" => "C9")), "no tile C9")
        finally
            lock(_LANDSCAPE_LOCK) do; delete!(_LANDSCAPE_BY_KEY, key) end
        end

        # capture / blackboard: id format guarded, absent objects fail
        @test no(Dict("kind" => "capture", "captureId" => "cap-20260923T101010-abcdef"), "no capture")
        @test no(Dict("kind" => "capture", "captureId" => "../x"), "invalid capture id")
        @test no(Dict("kind" => "blackboard", "entryId" => "nope"), "invalid")

        # task: a real fun name resolves to its label; an MCP tool name does not (Phase 0's citation)
        some_fun = first(sort!(collect(keys(Cecelia._fun_name_map()))))
        @test yes(Dict("kind" => "task", "funName" => some_fun))
        @test no(Dict("kind" => "task", "funName" => "get_project_info"), "no task")

        # ui: format only — never claims existence
        @test res(Dict("kind" => "ui", "anchor" => "viewer.play"))["check"] == "format"
        @test yes(Dict("kind" => "ui", "anchor" => "nav:/analysis"))
        @test no(Dict("kind" => "ui", "anchor" => "click the play button"), "not a UI anchor")

        # the route: one result per ref, in order; bad bodies are client errors
        st, body = _post(api_kiwi_refs_resolve, Dict("projectUid" => "testpr", "refs" => [
            Dict("kind" => "image", "imageUid" => "KDIeEm"), Dict("kind" => "image", "imageUid" => "zzzzzz"),
            Dict("kind" => "gate")]))
        out = JSON3.read(body)
        @test st == 200 && [r.ok for r in out.results] == [true, false, false]
        @test out.results[3].check == "shape"
        @test _post(api_kiwi_refs_resolve, Dict("projectUid" => "nope", "refs" => []))[1] == 404
        @test _post(api_kiwi_refs_resolve, Dict("projectUid" => "testpr", "refs" => "x"))[1] == 400
        @test _post(api_kiwi_refs_resolve, Dict("refs" => []))[1] == 400
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end
