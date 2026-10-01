# overlay_author 3D + movie-rail 3D + VIEWER_PARITY testsets — extracted from api/test/runtests.jl.
#
# Six testsets covering the 3D half of overlay_author and the movie-rail 3D pipeline:
#  - `API: overlay_author — build_overlays3d_for on the labelProps fixture` (3D analogue).
#  - `API: movie rail — overlay context resolver + JSON serialisation`.
#  - `API: overlay_author — colourBy + colourOverrides recolour via shared state`.
#  - `API: movie rail — 3D camera payload + scale bar follow the viewer's conventions`.
#  - `API: palette + track-mode JSON is the shared source of truth for overlay_author`
#    (VIEWER_PARITY phases 1 + 2).
#
# One path expression rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

@testset "API: overlay_author — build_overlays3d_for on the labelProps fixture" begin
    # 3D analogue of build_overlays_for. Same fixture, same wide-open pop, but NATIVE VOXEL coords
    # (no `PixelTransform`) and a `z` field on both points AND segments — the movie renderer's
    # shader projects them with the raycast's own camera, so they must arrive as positions.
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

            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "all3d", "colour" => "#00ff00", "gate" => gate)))))

            per_t = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                          include_tracks = false)
            pts0, segs0 = per_t(0)
            @test pts0 !== nothing
            @test length(pts0.x) > 0
            @test length(pts0.x) == length(pts0.y) == length(pts0.z)
            @test length(pts0.colour) == length(pts0.x)
            # Positions, not pixels: the same centroids the 2D author starts from (native voxels).
            lp = label_props(img; value_name = "B")
            view_centroid_cols(lp; order = [:x, :y, :z])
            df = as_df(lp)
            @test minimum(pts0.x) >= minimum(Float64.(df.centroid_x)) - 1e-9
            @test maximum(pts0.x) <= maximum(Float64.(df.centroid_x)) + 1e-9
            # Every point paints in the pop's colour — the resolver honoured the gate.
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts0.colour)
            # No tracks requested → no segments even if the fixture has `track_id`.
            @test segs0 === nothing
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: movie rail — overlay context resolver + JSON serialisation" begin
    # `_resolve_keyframe_overlay_builders` gates the whole overlay pipeline; `_overlays3d_state` is
    # the JSON contract the 3D renderer reads (positions; the shader projects). Pins the decision
    # points that could drift.

    # No image → no builders (channels-only movie).
    b2d, b3d = _resolve_keyframe_overlay_builders(nothing, nothing)
    @test b2d === nothing
    @test b3d === nothing
    # Image but empty config → still nothing (no draw-request flags).
    b2d2, b3d2 = _resolve_keyframe_overlay_builders(nothing,
        Dict{String,Any}("valueName" => "B", "popType" => "flow"))
    @test b2d2 === nothing && b3d2 === nothing
    # A mask asked for with no segmentation named → still the THREE-slot shape the recorder unpacks
    # (it returned two, so `build2d, per_t3d, build_mask = …` threw instead of recording channels-only).
    # `img` is never touched on this path, so any non-nothing stand-in works.
    @test _resolve_keyframe_overlay_builders(:img, Dict{String,Any}("showMask" => true)) ===
          (nothing, nothing, nothing)
    # Serialisation: a `nothing` closure → nothing, so the state dict stays terse.
    @test _overlays3d_state(nothing, 0) === nothing
    # Empty points-and-segments → nothing (skip the frame's overlay passes).
    @test _overlays3d_state(t -> (nothing, nothing), 5) === nothing
    # A non-empty payload → JSON-safe primitives (Vector{Float64}, no RGB objects at rest).
    pts = (; x = [50.5, 60.0], y = [40.0, 45.0], z = [3.0, 4.5],
             colour = [RGB{N0f8}(1, 0, 0), RGB{N0f8}(0, 1, 0)])
    segs = (; x0 = [50.5], y0 = [40.0], z0 = [3.0], x1 = [60.0], y1 = [45.0], z1 = [4.5],
              colour = [RGB{N0f8}(1, 0, 0)], t1 = [5])
    dct = _overlays3d_state(t -> (pts, segs), 5)
    @test dct isa AbstractDict
    @test dct["points"]["x"] == [50.5, 60.0]
    @test dct["points"]["z"] == [3.0, 4.5]
    @test dct["points"]["colour"][1] == Float64[1.0, 0.0, 0.0]
    @test dct["segments"]["z1"] == [4.5]
    @test !haskey(dct["segments"], "alpha")      # the viewer draws tails at one alpha; so does the movie
    # Round-trips through JSON3 — the actual over-the-wire test.
    j = JSON3.read(JSON3.write(dct))
    @test collect(j.points.x) == [50.5, 60.0]
    @test collect(j.segments.y1) == [45.0]
end

@testset "API: overlay_author — colourBy + colourOverrides recolour via shared state" begin
    # The drift-guarantee payoff: `colour_by` + `colour_overrides` plug in ONCE at
    # `_build_overlay_state`, so pointing 2D and 3D authors at the same column produces the same
    # per-vertex colours. Fixture is `testpr`/`KDIeEm` with a wide-open pop; we override the
    # `centroid_t` column so every cell falls to a known value → override wins uniformly.
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

            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "cb", "colour" => "#000000", "gate" => gate)))))

            # Which column to colour by — pick something guaranteed present, `centroid_t`. Discover
            # its values to build a total-override map, so EVERY dot gets a known colour.
            lp = label_props(img; value_name = "B")
            view_centroid_cols(lp; order = [:x, :y, :z])
            df = as_df(lp)
            ts = unique(Int[Int(round(Float64(v))) for v in df.centroid_t
                             if v isa Real && isfinite(Float64(v))])
            overrides = Dict{String,String}(string(t) => "#00ff00" for t in ts)

            # 2D author with colourBy = centroid_t + total overrides → all points paint green.
            H = ceil(Int, maximum(Float64.(df.centroid_y))) + 8
            W = ceil(Int, maximum(Float64.(df.centroid_x))) + 8
            tf = pixel_transform(H, W)
            per_t_2d = build_overlays_for(img; value_name = "B", pop_type = "flow", transform = tf,
                                           colour_by = "centroid_t",
                                           colour_overrides = overrides)
            pts, _ = per_t_2d(0)
            @test pts !== nothing
            @test length(pts.colour) > 0
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts.colour)

            # 3D author, same colourBy + overrides — same colours per vertex.
            per_t_3d = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                             colour_by = "centroid_t",
                                             colour_overrides = overrides)
            pts3d, _ = per_t_3d(0)
            @test pts3d !== nothing
            @test length(pts3d.colour) > 0
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts3d.colour)

            # Partial override — one value overridden, the rest fall to Okabe-Ito. The overridden
            # value's colour matches; some non-overridden values differ.
            partial = Dict{String,String}(string(first(ts)) => "#0000ff")
            per_t_2d_p = build_overlays_for(img; value_name = "B", pop_type = "flow", transform = tf,
                                             colour_by = "centroid_t",
                                             colour_overrides = partial)
            pts_p, _ = per_t_2d_p(first(ts))
            @test pts_p !== nothing
            @test all(c -> c == RGB{N0f8}(0, 0, 1), pts_p.colour)
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: movie rail — 3D camera payload + scale bar follow the viewer's conventions" begin
    # Julia forwards a 3D camera as the viewer stored it, plus the canvas the zoom was measured on;
    # the host applies it as `applyViewStateToBrowser` does (pinned by `shaders/golden.json` on both
    # sides), so there is no Julia-side conversion to keep in step.
    a = (; angles = (20.0, 45.0, 0.0), zoom = 2.3319, center3d = (5.0, 30.0, 60.0))
    st = Dict{String,Any}("canvas" => Dict{String,Any}("width" => 1186, "height" => 999),
                          "camera" => Dict{String,Any}("perspective" => 0))
    cam = _camera3d_payload(a, st)
    @test cam["angles"] == [20.0, 45.0, 0.0]
    @test cam["zoom"] == 2.3319
    @test cam["center"] == [5.0, 30.0, 60.0]
    @test cam["perspective"] == 0.0
    @test _snapshot_canvas_h(st) == 999.0
    # A batch camera: no centre (each image rotates about its own midpoint), no canvas.
    nb = _camera3d_payload((; angles = (0.0, 0.0, 0.0), zoom = nothing, center3d = nothing), Dict{String,Any}())
    @test !haskey(nb, "center") && nb["zoom"] == 1.0
    @test _snapshot_canvas_h(Dict{String,Any}()) === nothing

    # Scale bar: the viewer shows `captured_h / zoom` image rows across the canvas height, so the
    # bar's µm per output pixel scales with the snapshot canvas, not the output's.
    @test _um_per_px_3d(a, st, 0.5, 999) ≈ 0.5 / 2.3319
    @test _um_per_px_3d(a, st, 0.5, 512) ≈ 0.5 * (999 / 2.3319) / 512
    @test _um_per_px_3d(a, Dict{String,Any}(), 0.5, 512) ≈ 0.5 / 2.3319     # no canvas → its own
end
# ── VIEWER_PARITY phases 1 + 2: overlay_author reads the same JSON the browser reads ─────────────
# The house palette, the three track-colour-mode names, and the heat-ramp anchors used by the
# offline movie renderer all live in one JSON asset (`frontend/src/plots/palettes.json`) that the
# browser look ALSO reads. This testset pins that: the Julia constants must equal the JSON we would
# see the browser using — otherwise the two paths draw the same experiment differently.
# See docs/todo/VIEWER_PARITY_PLAN.md phases 1 + 2.
@testset "API: palette + track-mode JSON is the shared source of truth for overlay_author" begin
    palette_json = normpath(joinpath(API_TEST_DIR, "..", "..", "frontend", "src", "plots", "palettes.json"))
    @assert isfile(palette_json) "palettes.json missing — this test needs the checked-in file"
    doc = JSON3.read(read(palette_json, String))

    # Browser hex → Julia RGB the way overlay_author parses it (matches hex_to_rgb).
    function _parity_rgb(hex::AbstractString)
        h = strip(String(hex))
        startswith(h, "#") && (h = h[2:end])
        length(h) == 3 && (h = string(h[1], h[1], h[2], h[2], h[3], h[3]))
        r = parse(Int, h[1:2]; base = 16) / 255
        g = parse(Int, h[3:4]; base = 16) / 255
        b = parse(Int, h[5:6]; base = 16) / 255
        RGB{N0f8}(r, g, b)
    end

    # Palette equality: the JSON's `palettes.cecelia` block, parsed to RGB, is the Julia constant.
    json_palette = [_parity_rgb(String(h)) for h in doc.palettes.cecelia]
    @test length(CECELIA_TRACK_PALETTE) == length(json_palette) == 12
    @test CECELIA_TRACK_PALETTE == json_palette

    # Heat ramp: the JSON's five anchors are the Julia `_heat_stops()` — same order, same colours.
    json_heat = [_parity_rgb(String(h)) for h in doc.heatRamp]
    @test length(json_heat) == 5
    @test collect(_heat_stops()) == json_heat

    # Track-mode acceptance: every mode name the browser knows is accepted by build_overlays_for
    # (no fall-through to the `"track"` default warning inside the function).
    json_modes = [String(m) for m in doc.trackColorModes]
    @test Set(json_modes) == Set(TRACK_COLOR_MODES)
    for m in json_modes
        @test m in TRACK_COLOR_MODES
    end
end
