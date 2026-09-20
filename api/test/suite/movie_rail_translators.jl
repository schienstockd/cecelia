# Movie-rail input translators + keyframe helpers testsets — extracted from api/test/runtests.jl.
#
# Seven testsets covering the movie-rail input contract (viewer look/batch config → smoke
# route render args) and its per-frame helpers:
#  - `API: movie rail — offline overlay-config translator`
#  - `API: movie rail — viewstate → render args (keyframe rendering)`
#  - `API: crop_from_view_state — one-shot record uses the viewer rectangle`
#  - `API: z_from_view_state — one-shot record matches the viewer plane`
#  - `API: _max_px_from_view_state — blank size fields cap at the viewer canvas`
#  - `API: movie overlays — clock timestamp + scale-bar picker match the viewer`
#  - `API: interpolate_keyframes reaches the incoming keyframe exactly`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: movie rail — offline overlay-config translator" begin
    # `_overlays_raw_from_config` turns a viewer `look` / batch config into the smoke-route overlay
    # shape. If it drifts, the record button silently regresses to channels-only movies.
    @test _overlays_raw_from_config(Dict{String,Any}(), false) === nothing
    @test _overlays_raw_from_config(nothing, false) === nothing
    ov = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true, "popType" => "flow",
                                                     "pointsSize" => 8), false)
    @test ov isa AbstractDict
    @test ov["popType"] == "flow"
    @test ov["pointSizePx"] == 8
    @test ov["allTracks"] === false
    @test ov["includeTracks"] === false
    # showTracks = whole-segmentation tracks. Ribbon-eligible (`includeTracks`) — else the
    # `pi-directions` chip alone drew grey dots without ribbons.
    ov_all = _overlays_raw_from_config(Dict{String,Any}("showTracks" => true), false)
    @test ov_all["allTracks"] === true
    @test ov_all["includeTracks"] === true
    # showPops overrides showTracks — an explicit pop selection means "these pops", not "every cell
    # in the seg". Without this fix, `showPops + showTracks` painted every cell in default grey and
    # ignored the popsFilter entirely.
    ov_both = _overlays_raw_from_config(
        Dict{String,Any}("showTracks" => true, "showPopulations" => true), false)
    @test ov_both["allTracks"] === false
    @test ov_both["showPopulations"] === true
    # ribbons still push in the pops branch (via `include_tracks && (is_track || has_tracks)`)
    @test ov_both["includeTracks"] === true
    # A mask fills the mask branch AND flips `allCells` on when there are no pops/gated to filter by.
    ov_mask = _overlays_raw_from_config(Dict{String,Any}("labelContour" => 3), true)
    @test ov_mask["showMask"] === true
    @test ov_mask["maskContourPx"] == 3
    @test ov_mask["allCells"] === true
    # Gated tracks ON → the mask filters by those pops rather than showing every cell.
    ov_gated = _overlays_raw_from_config(Dict{String,Any}("showGatedTracks" => true), true)
    @test ov_gated["includeTracks"] === true
    @test ov_gated["allCells"] === false

    # `popsFilter` on the config surfaces as `popPaths` on the overlay dict — the batch picker's
    # per-image subset, forwarded to `build_overlays_for` / `build_mask_for` via
    # `_resolve_movie_overlays_mask`. Absent / empty = no filter (all pops rendered).
    ov_no_pf = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true), false)
    @test !haskey(ov_no_pf, "popPaths")
    ov_empty_pf = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popsFilter" => String[]), false)
    @test !haskey(ov_empty_pf, "popPaths")
    ov_pf = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popsFilter" => ["/A", "/B/c"]), false)
    @test ov_pf["popPaths"] == ["/A", "/B/c"]

    # `popValueName` surfaces as `valueName` on the overlay dict — the segmentation whose pop tree
    # `_resolve_movie_overlays_mask` looks up. Without it, pop paths from `flowTom` would silently
    # miss when the batch draws mask `default` (the resolver falls back to `vnn`).
    ov_no_vn = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true), false)
    @test !haskey(ov_no_vn, "valueName")
    ov_empty_vn = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popValueName" => ""), false)
    @test !haskey(ov_empty_vn, "valueName")
    ov_vn = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popValueName" => "flowTom"), false)
    @test ov_vn["valueName"] == "flowTom"

    # `trackSources` — multi-segmentation composition for showTracks && !showPops. When present +
    # non-empty, `_resolve_movie_overlays_mask` composes one overlay closure per source, each with
    # its own `all_tracks_colour`. Absent / empty → single-source `allTracks` grey (legacy).
    ov_no_ts = _overlays_raw_from_config(Dict{String,Any}("showTracks" => true), false)
    @test !haskey(ov_no_ts, "trackSources")
    ov_empty_ts = _overlays_raw_from_config(
        Dict{String,Any}("showTracks" => true, "trackSources" => []), false)
    @test !haskey(ov_empty_ts, "trackSources")
    ov_ts = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [
            Dict("valueName" => "cpSAM",   "colour" => "#ff6b6b"),
            Dict("valueName" => "flowTom", "colour" => "#4ecdc4"),
        ]), false)
    @test length(ov_ts["trackSources"]) == 2
    @test ov_ts["trackSources"][1]["valueName"] == "cpSAM"
    @test ov_ts["trackSources"][1]["colour"]    == "#ff6b6b"
    @test ov_ts["trackSources"][2]["valueName"] == "flowTom"
    # An entry with no colour falls back to the neutral grey, so a caller can send half-filled
    # entries without breaking the multi-source path.
    ov_ts_default = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [Dict("valueName" => "cpSAM")]), false)
    @test ov_ts_default["trackSources"][1]["colour"] == "#9ca3af"
    # A blank valueName is dropped (can't render tracks against no seg) — never sent to the author.
    ov_ts_blank = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [Dict("valueName" => "", "colour" => "#ff6b6b"),
                            Dict("valueName" => "cpSAM", "colour" => "#4ecdc4")]), false)
    @test length(ov_ts_blank["trackSources"]) == 1
    @test ov_ts_blank["trackSources"][1]["valueName"] == "cpSAM"
end

@testset "API: movie rail — viewstate → render args (keyframe rendering)" begin
    # `viewstate_to_render_args` is the single translator every keyframe of an offline animation
    # runs through — if a Layer entry's `visible` or `contrast_limits` stopped surfacing, the movie
    # would lose intent silently. Pin the four things a keyframe controls.
    args = viewstate_to_render_args(
        Dict{String,Any}("dims" => Dict("current_step" => [5, 2, 0, 0])),
        ["CH1", "CH2"], nothing, 100, 100)
    @test args.t == 5
    @test args.z == 2
    @test args.crop === nothing            # no canvas hints → no crop

    # Missing viewState fields fall back to defaults.
    args2 = viewstate_to_render_args(Dict{String,Any}(), ["CH1"],
                                      [(0.0, 100.0, "red", true)], 100, 100)
    @test args2.t == 0
    @test args2.z === nothing
    @test length(args2.specs) == 1
    @test args2.specs[1] == (0.0, 100.0, "red", true)

    # Layer entries overlay onto defaults (lookup by CHANNEL NAME, so a re-ordered image is safe).
    args3 = viewstate_to_render_args(
        Dict{String,Any}("layers" => Dict("CH2" => Dict("visible" => false,
                                                          "contrast_limits" => [10, 200],
                                                          "colormap" => "blue"))),
        ["CH1", "CH2"], [(0.0, 100.0, "red", true), (0.0, 100.0, "green", true)],
        100, 100)
    @test args3.specs[1] == (0.0, 100.0, "red", true)
    @test args3.specs[2] == (10.0, 200.0, "blue", false)

    # Camera → crop only when canvas hints are given.
    args_crop = viewstate_to_render_args(
        Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0)),
        ["CH1"], nothing, 100, 100; canvas_h = 40, canvas_w = 40)
    @test args_crop.crop !== nothing
    @test first(args_crop.crop.y) >= 0
    @test last(args_crop.crop.y)  <= 99

    # Snapshot's own `canvas` wins over the caller's `canvas_h/canvas_w` kwargs — the crop must
    # match the VIEWER'S visible rectangle at capture time, not the OUTPUT mp4 size. Bug (2026-08-31):
    # an animation recorded at 512×512 with a captured 656×831 canvas produced a
    # 126×126 mp4 (cropped to the OUTPUT size instead of the viewer's actual canvas), losing zoom
    # and aspect. Matches `crop_from_view_state`.
    vs_c = Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0),
                             "canvas" => Dict("height" => 80, "width" => 80),
                             "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]))
    args_snap = viewstate_to_render_args(vs_c, ["CH1"], nothing, 100, 100;
                                          canvas_h = 40, canvas_w = 40)
    @test args_snap.crop !== nothing
    # 80 / (2 × 2) = 20 half-width → x = 30:70; not 40:60 (which would use canvas_h/w).
    @test collect(args_snap.crop.x) == collect(30:70)
    @test collect(args_snap.crop.y) == collect(30:70)

    # No snapshot canvas → falls back to the caller's kwargs (legacy behaviour).
    vs_nocanv = Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0),
                                  "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]))
    args_fb = viewstate_to_render_args(vs_nocanv, ["CH1"], nothing, 100, 100;
                                        canvas_h = 40, canvas_w = 40)
    @test args_fb.crop !== nothing
    @test collect(args_fb.crop.x) == collect(40:60)
end

@testset "API: crop_from_view_state — one-shot record uses the viewer's rectangle" begin
    # The one-shot record needs the SAME visible rectangle the viewer is looking at, or it renders
    # a full-image movie at native aspect regardless of the user's zoom + pan (bug 2026-08-29). Pin
    # the crop maths in isolation — matches the crop half of `viewstate_to_render_args`, but the
    # one-shot path doesn't own per-frame arg resolution and only needs this piece.
    vs = Dict{String,Any}(
        "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]),
        "camera" => Dict("center" => [0.0, 50.0, 50.0], "zoom" => 2.0),
        "canvas" => Dict("width" => 40, "height" => 40),
    )
    c = crop_from_view_state(vs, 100, 100)
    @test c !== nothing
    @test first(c.x) >= 0
    @test last(c.x)  <= 99
    @test first(c.y) >= 0
    @test last(c.y)  <= 99
    # canvas 40 px / (2 × zoom 2) = 10 px half-width around cx = 50 → x = 40:60 (inclusive).
    @test collect(c.x) == collect(40:60)
    @test collect(c.y) == collect(40:60)

    # 3D → no 2D crop.
    vs3 = Dict{String,Any}("dims" => Dict("ndisplay" => 3),
                            "camera" => Dict("center" => [5.0, 50.0, 50.0], "zoom" => 2.0),
                            "canvas" => Dict("width" => 40, "height" => 40))
    @test crop_from_view_state(vs3, 100, 100) === nothing

    # Missing canvas / camera / zoom → nothing (falls through to whole-image behaviour).
    @test crop_from_view_state(nothing,                   100, 100) === nothing
    @test crop_from_view_state(Dict{String,Any}(),        100, 100) === nothing
    @test crop_from_view_state(
        Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0)),
        100, 100) === nothing                                                    # no canvas
end

@testset "API: z_from_view_state — one-shot record matches the viewer's plane" begin
    # A 2D browser viewer shows ONE z; when the request doesn't pin `zSlice`, the movie fell back to
    # an all-Z MIP of the same timepoint — which is a very different picture from what the user was
    # watching when they hit Record. Pin the plane pick here so a future drift is a failing test.
    vs2 = Dict{String,Any}(
        "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 7]),
        "camera" => Dict("center" => [0.0, 50.0, 50.0], "zoom" => 1.0),
        "canvas" => Dict("width" => 40, "height" => 40),
    )
    @test z_from_view_state(vs2) == 7

    # Non-integer plane (a Float32 landed in JSON) → rounded to the nearest int. The viewer's slider
    # is integer, but a snapshot can serialise as float — don't drop it on that.
    vs2f = Dict{String,Any}(
        "dims" => Dict("ndisplay" => 2, "current_step" => [0, 3.4]),
    )
    @test z_from_view_state(vs2f) == 3
    vs2fh = Dict{String,Any}(
        "dims" => Dict("ndisplay" => 2, "current_step" => [0, 3.6]),
    )
    @test z_from_view_state(vs2fh) == 4

    # 3D → nothing (whole volume is rendered, so no single plane to pick).
    vs3 = Dict{String,Any}("dims" => Dict("ndisplay" => 3, "current_step" => [0, 5]))
    @test z_from_view_state(vs3) === nothing

    # Nothing / empty / no dims / short current_step → nothing (falls through to previous behaviour).
    @test z_from_view_state(nothing) === nothing
    @test z_from_view_state(Dict{String,Any}()) === nothing
    @test z_from_view_state(Dict{String,Any}("dims" => Dict("ndisplay" => 2))) === nothing
    @test z_from_view_state(
        Dict{String,Any}("dims" => Dict("ndisplay" => 2, "current_step" => [0]))) === nothing
end

@testset "API: _max_px_from_view_state — blank size fields cap at the viewer canvas" begin
    # Blank size fields used to leave the mp4 at native crop resolution: tiny for a zoomed-in view,
    # huge for a zoomed-out one, neither matching what the viewer showed. Cap at the viewer canvas
    # long side. Aspect stays native — `max_px` is a stride cap, not an exact resize.
    vs = Dict{String,Any}("canvas" => Dict("width" => 900, "height" => 600))
    @test _max_px_from_view_state(vs) == 900
    # Portrait canvas → long side is the height.
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 400, "height" => 800))) == 800
    # Float canvas dims (JSON) → rounded, so a 799.6 canvas doesn't cap the mp4 at 799.
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 799.6, "height" => 600.0))) == 800

    # Nothing / no canvas / zero canvas → 0 (previous native-crop behaviour).
    @test _max_px_from_view_state(nothing) == 0
    @test _max_px_from_view_state(Dict{String,Any}()) == 0
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 0, "height" => 0))) == 0
end

@testset "API: movie overlays — clock timestamp + scale-bar picker match the viewer" begin
    # The Julia encoder-side overlays and the browser volume viewer's on-screen overlays draw the
    # SAME frame at the same time — if their formatters drift, a movie captured from the viewer
    # reads "7m 30s / 20 µm" while the viewer itself reads "0:07:30 / 50 µm". Pin the two policies
    # here so a future drift is a failing test, not a screenshot comparison.
    #
    # Timestamp: "H:MM:SS", zero-padded — matches `elapsedLabel(...,'clock')` in
    # `frontend/src/utils/stillOverlay.ts`.
    @test _format_ts(0,   0.5) == "0:00:00"
    @test _format_ts(15,  0.5) == "0:07:30"       # 15 frames × 30 s
    @test _format_ts(120, 1.0) == "2:00:00"       # 2 hours
    @test _format_ts(1,   1/60) == "0:00:01"      # 1 s

    # Scale bar: largest step ≤ 30 % of the frame's µm-extent, roll to mm at ≥ 1000 — matches
    # `niceScaleBar` in `frontend/src/utils/stillOverlay.ts`.
    #  600 px × 0.5 µm/px = 300 µm extent → 30 % = 90 µm → largest fitting step is 50 µm.
    sb = _pick_scale_bar(0.5, 600)
    @test sb !== nothing
    @test sb[1] == 50.0
    @test _scale_bar_label(sb[1]) == "50 µm"
    #  A big frame rolls up to mm.
    @test _scale_bar_label(1000.0) == "1 mm"
    @test _scale_bar_label(2000.0) == "2 mm"
    #  A tiny frame (too small for the smallest step) returns nothing.
    @test _pick_scale_bar(0.001, 10) === nothing
end

@testset "API: interpolate_keyframes reaches the incoming keyframe exactly" begin
    # The offline animation renderer relies on the last tween frame BEING the arrival state — not a
    # half-step short. The napari path had this contract already; the offline sibling has to match.
    kfs = [Dict("viewState" => Dict("dims" => Dict("current_step" => [0, 0])), "steps" => 1),
           Dict("viewState" => Dict("dims" => Dict("current_step" => [10, 0])), "steps" => 5)]
    frames = interpolate_keyframes(kfs)
    @test length(frames) == 6              # 1 + 5
    @test frames[end]["dims"]["current_step"][1] == 10
end
