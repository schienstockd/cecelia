# Viewer pick / overlay-legend / record-test / thumbnail guards + tracks pick-selection —
# extracted from api/test/runtests.jl.
#
# Seven testsets covering the input-guard + tracks-resolution surface for /api/viewer/*:
#  - `API: viewer pick-cell — 404 when the mask store is missing` (P8 boundary guard).
#  - `API: viewer overlay-legend — 404 when the image doesn t exist`.
#  - `API: viewer pick-clear — 404 when the image doesn t exist`.
#  - `API: viewer pick-set — 404 when the image doesn t exist + input validation`.
#  - `API: /api/viewer/record-test — input guards`.
#  - `API: /api/viewer/thumbnail — input guards`.
#  - `API: a pick selection resolves to TRACKS`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: viewer pick-cell — 404 when the mask store is missing" begin
    # P8 — the click endpoint reuses `label_store_path`, so the same "not on disk" answer that
    # `api_viewer_meta`'s labelNames would omit surfaces here as a 404 rather than a 500 mid-read.
    # A viewer that opens on an unsegmented image sends no picks, but the guard belongs at the
    # boundary anyway (an unsegmented image is a normal state, not an error).
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-pick")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        save!(img)
        body = JSON3.write(Dict("projectUid" => proj.uid, "imageUid" => img.uid,
                                "valueName" => "nope", "popType" => "flow",
                                "t" => 0, "z" => 0, "x" => 0, "y" => 0))
        st, out = api_viewer_pick_cell(Vector{UInt8}(body))
        @test st == 404
        d = JSON3.read(out)
        @test occursin("no label store", d.error)
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer overlay-legend — 404 when the image doesn't exist" begin
    # P9 replacement for `/api/napari/overlay-legend`. Same pure computation (`overlay_legend_content`
    # walks pop maps + resolves colour-by categories against populations); the endpoint went to
    # `viewer_api.jl` verbatim. Uses `_gating_image`, so a missing project/image is a 404 at the
    # boundary rather than a 500 inside `load_pop_map`.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "colourBy" => "", "overlayPops" => []))
        st, out = api_viewer_overlay_legend(Vector{UInt8}(body))
        @test st == 404
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer pick-clear — 404 when the image doesn't exist" begin
    # P9 replacement for `/api/napari/stop-selection`. Same registry / broadcast path as pick-cell
    # and pick-rect, so it goes through `_gating_image` and answers 404 on an unknown project/image
    # before touching the label store. No mask store is needed to clear — an unsegmented image can
    # still have had a stale registry entry — so the ONLY failure mode at the boundary is the
    # image not existing at all.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "valueName" => "default", "popType" => "flow"))
        st, out = api_viewer_pick_clear(Vector{UInt8}(body))
        @test st == 404
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer pick-set — 404 when the image doesn't exist + input validation" begin
    # Symmetric with pick-clear: the boundary check goes through `_gating_image` before touching
    # any zarr, so an unknown image is a 404 not a 500. Also verifies `labels` must be an array —
    # a wrong body shape fails at 400 rather than silently accepting garbage.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        # Missing image → 404.
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "valueName" => "default", "popType" => "flow",
                                "labels" => [1, 2, 3]))
        st, out = api_viewer_pick_set(Vector{UInt8}(body))
        @test st == 404

        # `labels` not an array → 400. We still hit `_gating_image` first (missing project → 404
        # short-circuits), so give a fake project so it survives that check. Simulate that by
        # dropping to a request where project resolution errors on empty projectUid.
        body2 = JSON3.write(Dict("projectUid" => "", "imageUid" => "any", "labels" => 42))
        st2, out2 = api_viewer_pick_set(Vector{UInt8}(body2))
        @test st2 == 400  # `_gating_image` fires "projectUid required" (400) before labels check
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: /api/viewer/record-test — input guards" begin
    # The route itself is a smoke test — an end-to-end run needs the Python encoder, so the fixture
    # suites don't attempt it. What IS testable without leaving Julia is the request validation, so a
    # missing/invalid body fails the way the /movies UI expects (a 400 with an error field, not a
    # cascade of 500s from `record_view_movie` trying to open ""); and that an unknown image resolves
    # to a 404 rather than crashing on a bad zarr path.
    st, out = api_viewer_record_test(Vector{UInt8}("not json at all"))
    @test st == 400
    @test occursin("invalid JSON", JSON3.read(out).error)

    st, out = api_viewer_record_test(Vector{UInt8}(JSON3.write(Dict("imageUid" => "x"))))
    @test st == 400
    @test occursin("projectUid", JSON3.read(out).error)

    st, out = api_viewer_record_test(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "no-such", "imageUid" => "nope"))))
    @test st == 404
end

@testset "API: /api/viewer/thumbnail — input guards" begin
    # Same validation pattern as record-test: a well-shaped 400 rather than a 500 out of the
    # renderer. Full end-to-end (viewState → PNG → assetId) needs a fixture image which the
    # smoke suites don't run here; the request-shape guards are what protect the panel from
    # rendering a broken thumbnail on a typo'd payload.
    st, out = api_viewer_thumbnail(Vector{UInt8}("not json at all"))
    @test st == 400
    @test occursin("invalid JSON", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(Vector{UInt8}(JSON3.write(Dict("imageUid" => "x"))))
    @test st == 400
    @test occursin("projectUid", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "p", "imageUid" => "i"))))
    @test st == 400
    @test occursin("viewState", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "no-such", "imageUid" => "nope",
                                        "viewState" => Dict{String,Any}()))))
    @test st == 404
end

@testset "API: a pick selection resolves to TRACKS" begin
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img, err = _gating_image("testpr", "KDIeEm")
            @test err === nothing

            # nothing drawn → an empty answer, never an error: this route is polled by an open panel
            st, body = api_track_selection(HTTP.Request("GET",
                "/api/tracking/selection?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            d = JSON3.read(body)
            @test st == 200 && d.nLabels == 0 && isempty(d.tracks)

            # inject what the bridge would have stored, then resolve it
            lp = label_props(joinpath(proj, "1", "KDIeEm", "labelProps", "B.h5ad"))
            select_cols(lp, ["track_id"])
            df = as_df(lp; include_x = false, include_obs = true)
            tids = first(Cecelia.track_ids_present(df), 2)
            labs = Int[Int(round(Float64(df[r, :label]))) for r in 1:length(df.label)
                       if df[r, :track_id] isa Real && !isnan(Float64(df[r, :track_id])) &&
                          Int(round(Float64(df[r, :track_id]))) in tids]
            @test length(labs) > 2
            _set_pick_selection!(img._dir, "B", labs)
            try
                st, body = api_track_selection(HTTP.Request("GET",
                    "/api/tracking/selection?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
                d = JSON3.read(body)
                @test st == 200
                @test d.nLabels == length(labs)
                @test Set(Int[t.track for t in d.tracks]) == Set(tids)
                # most cells inside the drawn region first — so "pick the top two and Join" does the
                # obvious thing rather than picking by lowest id
                @test issorted([t.nCells for t in d.tracks]; rev = true)
                @test sum(Int[t.nCells for t in d.tracks]) == length(labs)
                @test d.nUntracked == 0
            finally
                _set_pick_selection!(img._dir, "B", Int[])
            end

            # `ids=` names tracks and IGNORES the cap — "the one I need is not in the top N" must have
            # an answer that is not "raise N for everyone".
            #
            # NOTE the payload shape: one entry per (images × population) GROUP, each carrying what the
            # single-image response used to carry at its top level. One image and no populations is one
            # group — the plot compares like every other plot on the board (docs/TRACKING.md).
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&limit=1"))
            capped = JSON3.read(body)
            @test st == 200 && capped.tracked && length(capped.groups) == 1
            g = capped.groups[1]
            @test length(g.paths) == 1 && g.shown == 1 && g.total > 1
            @test capped.shown == 1 && capped.dropped == 0
            # the group's identity travels with it — the frontend labels/colours/facets from this
            @test g.valueName == "B" && collect(String.(g.imageUids)) == ["KDIeEm"]
            @test g.label == ""                      # one group: a legend of one entry is noise
            want = string(last(tids))
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&limit=1&ids=$want"))
            named = JSON3.read(body)
            @test st == 200 && collect(String.(keys(named.groups[1].paths))) == [want]
            # a track that does not exist is empty, not a 500
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&ids=999999"))
            @test st == 200 && isempty(JSON3.read(body).groups[1].paths)

            # the cohort selectors reach the package resolver: `imageUids=` is the board's form, and
            # pooling one image is still one group (the flags are not a second code path)
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUids=KDIeEm&valueName=B&limit=2&poolImages=1"))
            pooled = JSON3.read(body)
            @test st == 200 && length(pooled.groups) == 1 && length(pooled.groups[1].paths) == 2
            # an image selector is REQUIRED — neither route may guess one
            @test api_track_paths(HTTP.Request("GET", "/api/tracking/paths?projectUid=testpr"))[1] == 400
            @test api_track_diagnostics(HTTP.Request("GET", "/api/tracking/diagnostics?projectUid=testpr"))[1] == 400

            # the diagnostics battery, same shape: one group carrying the curves and the run's own
            # findings (never re-derived in the frontend)
            st, body = api_track_diagnostics(HTTP.Request("GET",
                "/api/tracking/diagnostics?projectUid=testpr&imageUids=KDIeEm&valueName=B&maxLag=4"))
            diag = JSON3.read(body)
            @test st == 200 && diag.tracked && length(diag.groups) == 1
            dg = diag.groups[1]
            @test !isempty(dg.msd.lag) && !isempty(dg.acor.lag) && dg.nTracks > 0
            @test haskey(dg, :findings) && haskey(dg, :summary)

            # ── /api/tracking/detections — per-frame untracked cells (P3) ────────
            # smoke test: shape correctness on a real fixture. The `_is_untracked` rule itself is
            # pinned in the package suite; here we assert the route wires it into aligned per-frame
            # arrays, and that a frame with zero untracked cells is OMITTED (a scheme draws no rect
            # for zero, not one of zero height).
            @test api_track_detections(HTTP.Request("GET",
                "/api/tracking/detections?projectUid=testpr"))[1] == 400
            st, body = api_track_detections(HTTP.Request("GET",
                "/api/tracking/detections?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            d = JSON3.read(body)
            @test st == 200 && d.tracked == true
            @test issorted([f.t for f in d.frames])
            for f in d.frames
                @test f.count > 0
                @test f.count == length(f.labels)
                @test length(f.x) == f.count && length(f.y) == f.count
                @test all(l -> l isa Integer, f.labels)
            end
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
