# Viewer marks + capture + labels/ids testsets — extracted from api/test/runtests.jl.
#
# Three testsets covering the viewer-adjacent HTTP surface:
#  - `API: viewer/marks — track + cell POST publishes WS frame; list drops expired`
#    (BIDIR_CONTEXT_PLAN Part 3 point-out).
#  - `API: viewer/capture write + list + read round-trip` (BIDIR share-in — capture bag).
#  - `API: labels/ids — cells + tracks enumeration + stride sampling` (viewer data primitive).
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: viewer/marks — track + cell POST publishes WS frame; list drops expired" begin
    # BIDIR_CONTEXT_PLAN Part 3 (point-out). Every mark takes the same delivery path as `task:status`:
    # HTTP write → in-memory bag → WS broadcast. Frontend reads it and dispatches to the existing
    # setTrackHighlight / setPickHighlight setters (nothing image-render-specific here).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _reset_marks!()
    cap = Channel{String}(64); key = gensym("test-marks")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    try
        uid = "TESTMARK"; mkpath(joinpath(tmp, uid))
        # tracks
        t(b) = _post(api_viewer_marks_tracks, b)
        @test t(Dict("imageUid"=>"I", "valueName"=>"v", "trackIds"=>[1]))[1] == 400   # projectUid required
        @test t(Dict("projectUid"=>"NOPE", "imageUid"=>"I", "valueName"=>"v", "trackIds"=>[1]))[1] == 400
        @test t(Dict("projectUid"=>uid, "imageUid"=>"I", "valueName"=>"v"))[1] == 400 # trackIds required
        @test t(Dict("projectUid"=>uid, "imageUid"=>"I", "valueName"=>"v", "trackIds"=>[]))[1] == 400
        drain()

        st, body = t(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "valueName"=>"default",
                          "trackIds"=>[3, 7, 42], "focusId"=>7, "label"=>"two of interest", "ttl_s"=>60))
        @test st == 200
        r = JSON3.read(body)
        @test r.ok == true && startswith(String(r.markerId), "mark-")
        # WS frame — same shape TrackHighlight uses (trackIds), plus the marker envelope.
        frames = drain()
        @test length(frames) == 1
        f = frames[1]
        @test String(f.type) == "viewer:mark"
        @test String(f.kind) == "track"
        @test String(f.imageUid) == "IMG1"
        @test collect(f.trackIds) == [3, 7, 42]
        @test Int(f.focusId) == 7
        @test String(f.label) == "two of interest"

        # cells
        c(b) = _post(api_viewer_marks_cells, b)
        @test c(Dict("projectUid"=>uid, "imageUid"=>"I", "valueName"=>"v"))[1] == 400 # labelIds required
        drain()
        st2, body2 = c(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "valueName"=>"default",
                            "labelIds"=>[101, 202], "focusId"=>101))
        @test st2 == 200
        r2 = JSON3.read(body2)
        f2 = drain()[1]
        @test String(f2.kind) == "cell"
        @test collect(f2.labels) == [101, 202]
        @test Int(f2.focusId) == 101

        # list — both live entries
        st3, body3 = api_viewer_marks_list(HTTP.Request("GET", "/api/viewer/marks?projectUid=$uid"))
        @test st3 == 200
        items = JSON3.read(body3).items
        @test length(items) == 2

        # TTL: a mark with a 1-second TTL is skipped once it's aged past that.
        _reset_marks!()
        st4, body4 = t(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "valueName"=>"default",
                            "trackIds"=>[9], "ttl_s"=>1))
        @test st4 == 200
        # Force-age by rewriting the mark's createdAt via the bag. This is white-box on purpose —
        # sleeping 2s in a testset punishes CI, and the TTL branch is what needs proving.
        lock(_MARKS_LOCK) do
            for (_, m) in _MARKS_BY_PROJECT[uid]
                _MARKS_BY_PROJECT[uid][m.id] = Mark(m.id, m.kind, m.projectUid, m.imageUid,
                    m.valueName, m.ids, m.focusId, m.label, m.createdAt - 10.0, m.ttlSeconds)
            end
        end
        st5, body5 = api_viewer_marks_list(HTTP.Request("GET", "/api/viewer/marks?projectUid=$uid"))
        @test st5 == 200 && isempty(JSON3.read(body5).items)

        # UI marks (PR #5): anchor validation + WS frame carries the anchor string verbatim.
        _reset_marks!(); drain()
        u(b) = _post(api_viewer_marks_ui, b)
        @test u(Dict("projectUid"=>uid))[1] == 400                                       # anchor required
        @test u(Dict("projectUid"=>uid, "anchor"=>""))[1] == 400
        @test u(Dict("projectUid"=>"NOPE", "anchor"=>"viewer.movieSection"))[1] == 404
        stu, bodyu = u(Dict("projectUid"=>uid, "anchor"=>"nav:/segment", "label"=>"look here", "ttl_s"=>90))
        @test stu == 200
        fu = drain()[1]
        @test String(fu.type) == "viewer:mark" && String(fu.kind) == "ui"
        @test String(fu.anchor) == "nav:/segment"
        @test String(fu.label) == "look here"
        @test Int(fu.ttlSeconds) == 90

        # Freeform marks: target MUST be a captureId. live_viewer was removed — a live viewport
        # has no stable pixel frame Claude can author against without asking, so those marks
        # landed off-screen or clipped. The frozen shared frame (via CaptureViewSurface) is now
        # where every freeform mark paints.
        _reset_marks!(); drain()
        f(b) = _post(api_viewer_marks_freeform, b)
        @test f(Dict("projectUid"=>uid, "overlay"=>[Dict("kind"=>"rect")]))[1] == 400   # target required
        @test f(Dict("projectUid"=>uid, "target"=>"other", "overlay"=>[Dict("kind"=>"rect")]))[1] == 400
        @test f(Dict("projectUid"=>uid, "target"=>"live_viewer",
                 "overlay"=>[Dict("kind"=>"rect")]))[1] == 400   # live_viewer no longer accepted
        @test f(Dict("projectUid"=>uid, "target"=>"cap-20260918T175413-a1b2c3"))[1] == 400  # overlay required
        @test f(Dict("projectUid"=>uid, "target"=>"cap-20260918T175413-a1b2c3", "overlay"=>[]))[1] == 400
        # A well-formed captureId is the only valid target now. Unknown overlay kinds stripped.
        stf, bodyf = f(Dict("projectUid"=>uid,
            "target"=>"cap-20260918T175413-a1b2c3",
            "overlay"=>[
                Dict("kind"=>"circle", "geom"=>Dict("cx"=>0.5, "cy"=>0.5, "r"=>0.1)),
                Dict("kind"=>"nope", "geom"=>Dict())                                    # dropped
            ],
            "label"=>"this cell"))
        @test stf == 200
        ff = drain()[1]
        @test String(ff.kind) == "freeform"
        @test String(ff.target) == "cap-20260918T175413-a1b2c3"
        @test length(ff.overlay) == 1
        @test String(ff.overlay[1].kind) == "circle"

        # Plot point-out (PR #4b): family + plotId required, u/v finite in ~0..1, optional cell,
        # WS frame carries payload verbatim so per-family consumers filter by (family, plotId, cell).
        _reset_marks!(); drain()
        p(b) = _post(api_viewer_marks_plot, b)
        @test p(Dict("projectUid"=>uid))[1] == 400                                       # family required
        @test p(Dict("projectUid"=>uid, "family"=>"gate-scatter"))[1] == 400             # plotId required
        @test p(Dict("projectUid"=>uid, "family"=>"gate-scatter", "plotId"=>"pk"))[1] == 400   # u/v required
        @test p(Dict("projectUid"=>uid, "family"=>"gate-scatter", "plotId"=>"pk",
                     "u"=>0.5, "v"=>"nope"))[1] == 400                                   # v must be numeric
        @test p(Dict("projectUid"=>uid, "family"=>"gate-scatter", "plotId"=>"pk",
                     "u"=>0.5, "v"=>1500))[1] == 400                                     # page-off value rejected
        stp, bodyp = p(Dict("projectUid"=>uid, "family"=>"gate-scatter", "plotId"=>"gate:flow:IMG1:default:3",
                            "u"=>0.72, "v"=>0.35, "cell"=>"", "label"=>"outlier", "ttl_s"=>90))
        @test stp == 200
        fp = drain()[1]
        @test String(fp.type) == "viewer:mark" && String(fp.kind) == "plot"
        @test String(fp.family) == "gate-scatter"
        @test String(fp.plotId) == "gate:flow:IMG1:default:3"
        @test Float64(fp.u) == 0.72 && Float64(fp.v) == 0.35
        @test !haskey(fp, :cell)                                                         # omitted when empty
        @test String(fp.label) == "outlier"
        @test Int(fp.ttlSeconds) == 90
        # Multi-cell address: `cell` is carried through for subframe-addressing families.
        stp2, _ = p(Dict("projectUid"=>uid, "family"=>"image-strip", "plotId"=>"strip-abc",
                         "u"=>0.5, "v"=>0.5, "cell"=>"cell=3"))
        @test stp2 == 200
        fp2 = drain()[1]
        @test String(fp2.cell) == "cell=3"
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        _reset_marks!()
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: viewer/capture write + list + read round-trip" begin
    # BIDIR_CONTEXT_PLAN Part 2 (share-in). Envelope lands under <proj>/captures/<id>/{meta.json,
    # frame.png}; list returns newest-first with `address`; read inlines the frame as a data URL.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    # a 1×1 PNG (base64) — smallest legal payload
    png_b64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGNgYGD4DwABBAEAfbLI3wAAAABJRU5ErkJggg=="
    frame_data_url = "data:image/png;base64," * png_b64
    try
        uid = "TESTCAP"; mkpath(joinpath(tmp, uid))
        w(b) = _post(api_viewer_capture, b)
        # guards
        @test w(Dict("frames"=>[Dict("png"=>frame_data_url)]))[1] == 400              # projectUid required
        @test w(Dict("projectUid"=>"NOPE", "frames"=>[Dict("png"=>frame_data_url)]))[1] == 404
        @test w(Dict("projectUid"=>uid))[1] == 400                                     # frames required
        @test w(Dict("projectUid"=>uid, "frames"=>[Dict("png"=>"not-a-png")]))[1] == 400
        @test w(Dict("projectUid"=>uid, "frames"=>[Dict("png"=>"data:image/jpeg;base64,/9j/AA==")]))[1] == 400  # not a PNG

        addr = Dict("projectUid"=>uid, "imageUid"=>"IMG1", "valueName"=>"default", "t"=>3)
        # Overlay mixes: palette-safe colour (magenta), NO colour (older-shape), UNKNOWN kind (dropped),
        # UNKNOWN colour (dropped from the mark's payload, mark itself preserved). Round-trip below
        # asserts what actually landed on disk.
        overlay = [Dict("kind"=>"rect", "geom"=>Dict("x"=>0.1, "y"=>0.1, "w"=>0.2, "h"=>0.2),
                        "color"=>"magenta", "strokeWidth"=>"thick",
                        "rotate"=>45.0),                                   # safelisted rotation — survives
                   Dict("kind"=>"bogus", "geom"=>Dict()),               # unknown kind — dropped
                   Dict("kind"=>"stroke", "geom"=>Dict("pts"=>[[0.0,0.0],[1.0,1.0]]),
                        "label"=>"trail", "color"=>"chartreuse",
                        "strokeWidth"=>"chunky",
                        "rotate"=>9999.0),                                 # unknown preset + out-of-range rotate — both dropped
                   Dict("kind"=>"rect", "geom"=>Dict("x"=>0.5, "y"=>0.5, "w"=>0.1, "h"=>0.1),
                        "color"=>"black",
                        "rotate"=>0.0)]                                    # zero rotation kept off the wire
        st, body = w(Dict("projectUid"=>uid, "surface"=>"viewer_frame", "address"=>addr,
                          "frames"=>[Dict("png"=>frame_data_url)], "overlay"=>overlay))
        @test st == 200
        r = JSON3.read(body)
        @test r.ok == true && !isempty(String(r.captureId))
        cap_id = String(r.captureId)
        @test occursin(r"^cap-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$", cap_id)
        cap_dir = joinpath(tmp, uid, "captures", cap_id)
        @test isdir(cap_dir) && isfile(joinpath(cap_dir, "meta.json")) && isfile(joinpath(cap_dir, "frame.png"))

        # list — newest first, address preserved, item is the one we just wrote
        st2, body2 = api_viewer_captures_list(HTTP.Request("GET",
            "/api/viewer/captures?projectUid=$uid&limit=5"))
        @test st2 == 200
        items = JSON3.read(body2).items
        @test length(items) == 1
        @test String(items[1].captureId) == cap_id
        @test String(items[1].surface) == "viewer_frame"
        @test String(items[1].address.imageUid) == "IMG1"

        # get — envelope round-trips, unknown overlay kind is dropped, frame is inlined
        st3, body3 = api_viewer_capture_get(HTTP.Request("GET",
            "/api/viewer/capture?projectUid=$uid&captureId=$cap_id"))
        @test st3 == 200
        got = JSON3.read(body3)
        @test String(got.capture.captureId) == cap_id
        @test length(got.capture.overlay) == 3                # bogus kind stripped; black rect added
        @test startswith(String(got.frame), "data:image/png;base64,")
        # capturePath is the absolute meta.json path — the MCP layer threads it into the tool
        # response so a local Claude session can `Read(capturePath)` for the fat form when the
        # slim landscape isn't enough. Only emitted on a loopback-bound server (same gate the
        # debug REPL uses). Ends with the expected suffix and points at a real file.
        @test haskey(got, :capturePath)
        @test endswith(String(got.capturePath), joinpath(cap_id, "meta.json"))
        @test isfile(String(got.capturePath))

        # Same call under a network-exposed bind (`0.0.0.0`) omits capturePath — a remote
        # caller can't reach the local path and offering one they might `Read` is a foot-gun.
        # Flip `_BOUND_HOST` in a try/finally so no state leaks to other testsets. server.jl
        # is `include`d at top level so `_BOUND_HOST` lives in Main, not a `Cecelia.` module.
        prev_host = _BOUND_HOST[]
        try
            _BOUND_HOST[] = "0.0.0.0"
            st_p, body_p = api_viewer_capture_get(HTTP.Request("GET",
                "/api/viewer/capture?projectUid=$uid&captureId=$cap_id"))
            @test st_p == 200
            got_p = JSON3.read(body_p)
            @test !haskey(got_p, :capturePath)
            # Frame + envelope still round-trip — omitting capturePath is the ONLY difference.
            @test startswith(String(got_p.frame), "data:image/png;base64,")
            @test String(got_p.capture.captureId) == cap_id
        finally
            _BOUND_HOST[] = prev_host
        end
        # Colour safelist round-trip. Magenta survived on the rect; chartreuse was dropped from the
        # stroke's payload but the stroke itself survived (frontend resolver falls back to `white`).
        # Black safelisted for canvas-Share white-composite plots.
        @test String(got.capture.overlay[1].kind) == "rect"
        @test String(got.capture.overlay[1].color) == "magenta"
        @test String(got.capture.overlay[1].strokeWidth) == "thick"       # safelisted preset survives
        @test Float64(got.capture.overlay[1].rotate) ≈ 45.0               # in-range rotation round-trips
        @test String(got.capture.overlay[2].kind) == "stroke"
        @test !haskey(got.capture.overlay[2], :color)
        @test !haskey(got.capture.overlay[2], :strokeWidth)               # unknown preset dropped, mark preserved
        @test !haskey(got.capture.overlay[2], :rotate)                    # out-of-range rotate dropped, mark preserved
        @test String(got.capture.overlay[3].kind) == "rect"
        @test String(got.capture.overlay[3].color) == "black"
        @test !haskey(got.capture.overlay[3], :rotate)                    # zero rotation kept off the wire

        # guards on the read side
        @test api_viewer_capture_get(HTTP.Request("GET", "/api/viewer/capture"))[1] == 400
        @test api_viewer_capture_get(HTTP.Request("GET", "/api/viewer/capture?projectUid=$uid"))[1] == 400
        @test api_viewer_capture_get(HTTP.Request("GET", "/api/viewer/capture?projectUid=$uid&captureId=nope"))[1] == 400
        @test api_viewer_capture_get(HTTP.Request("GET", "/api/viewer/capture?projectUid=$uid&captureId=cap-20260101T000000-abcdef"))[1] == 404
        @test api_viewer_captures_list(HTTP.Request("GET", "/api/viewer/captures"))[1] == 400
        @test api_viewer_captures_list(HTTP.Request("GET", "/api/viewer/captures?projectUid=NOPE"))[1] == 404

        # Re-annotate lineage (Kiwi PR B). Post a second capture that references the first via
        # `previousCaptureId`; assert (a) the field round-trips into the get envelope and (b) the
        # list response surfaces it, so Kiwi can show a "refined" indicator without a per-row read.
        # Bogus previousCaptureId is silently dropped rather than stored.
        st_re, body_re = w(Dict("projectUid"=>uid, "surface"=>"viewer_frame", "address"=>addr,
                                "frames"=>[Dict("png"=>frame_data_url)], "overlay"=>[],
                                "previousCaptureId"=>cap_id))
        @test st_re == 200
        cap_id2 = String(JSON3.read(body_re).captureId)
        st_r2, body_r2 = api_viewer_capture_get(HTTP.Request("GET",
            "/api/viewer/capture?projectUid=$uid&captureId=$cap_id2"))
        @test st_r2 == 200
        @test String(JSON3.read(body_r2).capture.previousCaptureId) == cap_id
        st_l2, body_l2 = api_viewer_captures_list(HTTP.Request("GET",
            "/api/viewer/captures?projectUid=$uid&limit=5"))
        items_l2 = JSON3.read(body_l2).items
        refined = first(i for i in items_l2 if String(i.captureId) == cap_id2)
        @test String(refined.previousCaptureId) == cap_id
        # Bogus previousCaptureId (not a valid captureId shape) is dropped, not stored.
        st_re2, body_re2 = w(Dict("projectUid"=>uid, "surface"=>"viewer_frame", "address"=>addr,
                                  "frames"=>[Dict("png"=>frame_data_url)], "overlay"=>[],
                                  "previousCaptureId"=>"../../../etc/passwd"))
        @test st_re2 == 200
        cap_id3 = String(JSON3.read(body_re2).captureId)
        st_r3, body_r3 = api_viewer_capture_get(HTTP.Request("GET",
            "/api/viewer/capture?projectUid=$uid&captureId=$cap_id3"))
        @test !haskey(JSON3.read(body_r3).capture, :previousCaptureId)

        # empty list when the project has no captures dir yet
        uid2 = "TESTCAP2"; mkpath(joinpath(tmp, uid2))
        st4, body4 = api_viewer_captures_list(HTTP.Request("GET", "/api/viewer/captures?projectUid=$uid2"))
        @test st4 == 200 && isempty(JSON3.read(body4).items)

        # Multi-panel plot capture: `panels[]` round-trips, malformed entries are dropped, and the
        # list surfaces `panelCount` so Kiwi doesn't have to open the envelope for a glance row.
        panels_in = [
            Dict("panelId"=>"7", "position"=>Dict("x"=>0,   "y"=>0, "w"=>400, "h"=>300),
                 "plotRef"=>Dict("specId"=>"track_measures", "ui"=>Dict("measure"=>"live.track.speed",
                                                                          "chartType"=>"boxplot")),
                 "dataSlice"=>Dict("imageUids"=>["IMG1"], "scope"=>"per_image")),
            Dict("panelId"=>"9", "position"=>Dict("x"=>400, "y"=>0, "w"=>400, "h"=>300),
                 "plotRef"=>Dict("specId"=>"track_measures", "ui"=>Dict("measure"=>"live.track.displacement"))),
            # Malformed: no panelId → dropped. Malformed: non-numeric position → dropped.
            Dict("position"=>Dict("x"=>0, "y"=>300, "w"=>400, "h"=>300)),
            Dict("panelId"=>"11", "position"=>Dict("x"=>"nope", "y"=>0, "w"=>400, "h"=>300)),
        ]
        st_mp, body_mp = w(Dict("projectUid"=>uid, "surface"=>"plot",
                                "address"=>Dict("projectUid"=>uid,
                                                "plotSpec"=>Dict("specId"=>"multi-panel",
                                                                  "params"=>Dict("module"=>"behaviourAnalysis",
                                                                                  "panelCount"=>2))),
                                "panels"=>panels_in,
                                "workspaceOrigin"=>Dict("x"=>240, "y"=>360),
                                "frames"=>[Dict("png"=>frame_data_url)]))
        @test st_mp == 200
        cap_mp = String(JSON3.read(body_mp).captureId)
        st_mpr, body_mpr = api_viewer_capture_get(HTTP.Request("GET",
            "/api/viewer/capture?projectUid=$uid&captureId=$cap_mp"))
        @test st_mpr == 200
        got_mp = JSON3.read(body_mpr).capture
        @test length(got_mp.panels) == 2                              # two malformed entries dropped
        @test String(got_mp.panels[1].panelId) == "7"
        @test got_mp.panels[1].position.x == 0
        @test got_mp.panels[1].position.w == 400
        @test String(got_mp.panels[1].plotRef.specId) == "track_measures"
        @test String(got_mp.panels[1].plotRef.ui.measure) == "live.track.speed"
        @test String(got_mp.panels[2].plotRef.ui.measure) == "live.track.displacement"
        # workspaceOrigin round-trips numerically so a downstream "zoom to source" restores
        # panels at their exact original workspace pixels.
        @test got_mp.workspaceOrigin.x == 240
        @test got_mp.workspaceOrigin.y == 360
        # panelCount surfaces on the list row, not just in the envelope.
        st_lmp, body_lmp = api_viewer_captures_list(HTTP.Request("GET",
            "/api/viewer/captures?projectUid=$uid&limit=10"))
        items_lmp = JSON3.read(body_lmp).items
        row_mp = first(i for i in items_lmp if String(i.captureId) == cap_mp)
        @test row_mp.panelCount == 2

        # A single-plot capture with no `panels[]` field stays clean — no key added, no glitch.
        st_sp, body_sp = w(Dict("projectUid"=>uid, "surface"=>"plot",
                                "address"=>Dict("projectUid"=>uid,
                                                "plotSpec"=>Dict("specId"=>"track_measures")),
                                "frames"=>[Dict("png"=>frame_data_url)]))
        @test st_sp == 200
        cap_sp = String(JSON3.read(body_sp).captureId)
        st_spr, body_spr = api_viewer_capture_get(HTTP.Request("GET",
            "/api/viewer/capture?projectUid=$uid&captureId=$cap_sp"))
        @test !haskey(JSON3.read(body_spr).capture, :panels)
        @test !haskey(JSON3.read(body_spr).capture, :workspaceOrigin)
        # And the list row DOESN'T sprout panelCount for a single-plot capture.
        st_lsp, body_lsp = api_viewer_captures_list(HTTP.Request("GET",
            "/api/viewer/captures?projectUid=$uid&limit=10"))
        row_sp = first(i for i in JSON3.read(body_lsp).items if String(i.captureId) == cap_sp)
        @test !haskey(row_sp, :panelCount)

        # Kiwi capture management — user-driven delete + bulk clear (post-PR #3 follow-up).
        # Delete: round-trip via the handler; idempotent on a second call. Guarded regex/paths.
        del_body(id) = Vector{UInt8}(JSON3.write(Dict("projectUid"=>uid, "captureId"=>id)))
        st_d1, r_d1 = api_viewer_capture_delete(del_body(cap_id))
        @test st_d1 == 200
        @test JSON3.read(r_d1, Dict{String,Any})["deleted"] == true
        @test !isdir(cap_dir)
        st_d2, r_d2 = api_viewer_capture_delete(del_body(cap_id))
        @test st_d2 == 200
        @test JSON3.read(r_d2, Dict{String,Any})["deleted"] == false      # idempotent

        # Delete guards
        @test api_viewer_capture_delete(Vector{UInt8}(JSON3.write(Dict("projectUid"=>uid))))[1] == 400
        @test api_viewer_capture_delete(Vector{UInt8}(JSON3.write(Dict("captureId"=>cap_id))))[1] == 400
        # traversal attempt rejected by the captureId regex, not by path magic
        @test api_viewer_capture_delete(Vector{UInt8}(JSON3.write(Dict("projectUid"=>uid, "captureId"=>"../../etc/passwd"))))[1] == 400

        # Bulk clear — write a couple more captures, then clear all. Count includes the two
        # re-annotate captures written above (cap_id2, cap_id3) which weren't individually deleted,
        # plus the two multi-panel probes written for the panels[] round-trip (cap_mp, cap_sp).
        addr2 = Dict("projectUid"=>uid, "imageUid"=>"IMG2", "t"=>0)
        for _ in 1:3
            w(Dict("projectUid"=>uid, "surface"=>"viewer_frame", "address"=>addr2,
                   "frames"=>[Dict("png"=>frame_data_url)]))
        end
        st_c, r_c = api_viewer_captures_clear(Vector{UInt8}(JSON3.write(Dict("projectUid"=>uid))))
        @test st_c == 200
        @test JSON3.read(r_c, Dict{String,Any})["cleared"] == 7
        st_c2, r_c2 = api_viewer_captures_clear(Vector{UInt8}(JSON3.write(Dict("projectUid"=>uid))))
        @test st_c2 == 200
        @test JSON3.read(r_c2, Dict{String,Any})["cleared"] == 0
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: labels/ids — cells + tracks enumeration + stride sampling" begin
    # BIDIR follow-up. Claude's mark_cells / mark_tracks kept renderering nothing because the ids
    # were guessed; this endpoint hands over the real ones. Uses the same `_gating_image` loader
    # every gating endpoint uses, so a valid id here is a valid id there.
    fx = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(fx)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            g(qs) = api_labels_ids(HTTP.Request("GET", "/api/labels/ids?" * qs))

            # Guards
            @test g("")[1] == 400                                          # projectUid required
            @test g("projectUid=testpr")[1] == 400                          # imageUid required
            @test g("projectUid=testpr&imageUid=KDIeEm")[1] == 400          # valueName required
            @test g("projectUid=testpr&imageUid=KDIeEm&valueName=B&kind=nope")[1] == 400  # unknown kind
            @test g("projectUid=NOPE&imageUid=KDIeEm&valueName=B")[1] == 404
            @test g("projectUid=testpr&imageUid=NOPE&valueName=B")[1] == 404

            # Cells — real ids from the fixture. Ask for the FULL population (limit big enough)
            # so subsequent sample/limit checks have a real reference for "first" and "last".
            st, body = g("projectUid=testpr&imageUid=KDIeEm&valueName=B&kind=cells&limit=5000")
            @test st == 200
            r = JSON3.read(body)
            @test String(r.kind) == "cells"
            @test String(r.valueName) == "B"
            @test !isempty(r.ids)
            @test all(x -> x isa Integer, r.ids)
            @test Int(r.total) == length(r.ids)   # full population fetched
            @test r.sampled == false
            @test r.truncated == false

            # Limit is honoured — response ids don't exceed `limit` and `truncated` flips when the
            # population is larger.
            st2, body2 = g("projectUid=testpr&imageUid=KDIeEm&valueName=B&kind=cells&limit=3")
            r2 = JSON3.read(body2)
            @test st2 == 200
            @test length(r2.ids) <= 3
            if Int(r2.total) > 3
                @test r2.truncated == true
                @test r2.sampled == false
            end

            # Sampling — same length as limit, but sampled=true and truncated=false. The stride
            # sample is deterministic and preserves first + last of the FULL population.
            if Int(r.total) > 3
                st3, body3 = g("projectUid=testpr&imageUid=KDIeEm&valueName=B&kind=cells&limit=3&sample=true")
                r3 = JSON3.read(body3)
                @test st3 == 200
                @test length(r3.ids) == 3
                @test r3.sampled == true
                @test r3.truncated == false
                @test Int(r3.ids[1])   == Int(r.ids[1])
                @test Int(r3.ids[end]) == Int(r.ids[end])
            end

            # Tracks — no track_props on this fixture, so 404 rather than 500. Confirms the "no
            # tracks for this vn" branch reads as a not-found, not a server bug.
            st4, _ = g("projectUid=testpr&imageUid=KDIeEm&valueName=B&kind=tracks")
            @test st4 in (200, 404)  # tolerant: some fixtures may grow track_props later
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
            rm(dir; recursive = true, force = true)
        end
    end
end

