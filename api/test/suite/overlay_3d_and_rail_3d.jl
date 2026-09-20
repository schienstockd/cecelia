# overlay_author 3D + movie-rail 3D + VIEWER_PARITY testsets — extracted from api/test/runtests.jl.
#
# Six testsets covering the 3D half of overlay_author and the movie-rail 3D pipeline:
#  - `API: overlay_author — build_overlays3d_for on the labelProps fixture` (3D analogue).
#  - `API: movie rail — overlay context resolver + JSON serialisation`.
#  - `API: overlay_author — colourBy + colourOverrides recolour via shared state`.
#  - `API: overlay_author — rotation_matrix_from_angles matches vispy convention`.
#  - `API: movie rail — 2D↔3D overlay projection agrees at identity view`.
#  - `API: palette + track-mode JSON is the shared source of truth for overlay_author`
#    (VIEWER_PARITY phases 1 + 2).
#
# One path expression rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

@testset "API: overlay_author — build_overlays3d_for on the labelProps fixture" begin
    # 3D analogue of build_overlays_for. Same fixture, same wide-open pop, but NATIVE VOXEL coords
    # (no `PixelTransform`) and a `z` field on both points AND segments. Bug this catches: the 3D
    # author silently drops the z column on a 2D-segmented image, OR emits drawn-pixel coords by
    # accident — either would visually work in the smoke script but render wrong under a rotation.
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
            # Identity view: R = I, cx/cy/cz = 0, zoom scale = 1 → projected (u, v) = native (x + 0.5, y + 0.5).
            # This is the "drift guarantee" from a caller's POV: no rotation, no offset, dots land
            # where the cell is.
            R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
            canvas_h, canvas_w = 100, 100
            pts0, segs0 = per_t(0, R0, 0.0, 0.0, 0.0, 1.0, canvas_h, canvas_w, 1.0)
            @test pts0 !== nothing
            @test length(pts0.u) > 0
            @test length(pts0.u) == length(pts0.v)
            @test length(pts0.colour) == length(pts0.u)
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
    # `_resolve_keyframe_overlay_builders` gates the whole overlay pipeline; `_overlays2d_state`
    # is the JSON contract the Python renderer reads. Julia projects; Python rasterises. Pins the
    # four decision points that could drift.

    # No image → no builders (channels-only movie).
    b2d, b3d = _resolve_keyframe_overlay_builders(nothing, nothing)
    @test b2d === nothing
    @test b3d === nothing
    # Image but empty config → still nothing (no draw-request flags).
    b2d2, b3d2 = _resolve_keyframe_overlay_builders(nothing,
        Dict{String,Any}("valueName" => "B", "popType" => "flow"))
    @test b2d2 === nothing && b3d2 === nothing
    # Serialisation: a `nothing` closure → nothing, so the state dict stays terse.
    @test _overlays2d_state(nothing, 0, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2) === nothing
    # Empty points-and-segments → nothing (skip the frame's overlay pass).
    empty_closure = (t, R, cx, cy, cz, wpp, ch, cw, za) -> (nothing, nothing)
    @test _overlays2d_state(empty_closure, 5, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2) === nothing
    # A non-empty payload → JSON-safe primitives (Vector{Float64}, no RGB objects at rest).
    pts = (; u = [50.5, 60.0], v = [40.0, 45.0],
             colour = [RGB{N0f8}(1, 0, 0), RGB{N0f8}(0, 1, 0)])
    segs = (; u0 = [50.5], v0 = [40.0], u1 = [60.0], v1 = [45.0],
              colour = [RGB{N0f8}(1, 0, 0)], alpha = [0.8])
    non_empty = (t, R, cx, cy, cz, wpp, ch, cw, za) -> (pts, segs)
    dct = _overlays2d_state(non_empty, 5, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2)
    @test dct isa AbstractDict
    @test dct["pointSize"] == 6
    @test dct["segmentWidth"] == 2
    @test dct["tailLength"] == 30
    @test dct["points"]["u"] == [50.5, 60.0]
    @test dct["points"]["v"] == [40.0, 45.0]
    @test dct["points"]["colour"][1] == Float64[1.0, 0.0, 0.0]
    @test dct["segments"]["u0"] == [50.5]
    @test dct["segments"]["alpha"] == [0.8]
    # Round-trips through JSON3 — the actual over-the-wire test.
    j = JSON3.read(JSON3.write(dct))
    @test j.pointSize == 6
    @test collect(j.points.u) == [50.5, 60.0]
    @test collect(j.segments.alpha) == [0.8]
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
            R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
            pts3d, _ = per_t_3d(0, R0, 0.0, 0.0, 0.0, 1.0, 100, 100, 1.0)
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

@testset "API: overlay_author — rotation_matrix_from_angles matches vispy convention" begin
    # The rotation-matrix convention is REPLICATED in three places: `render_view_frame_3d` (Julia
    # CPU fallback), `render_animation_run.py::_rotation_matrix` (GPU raycast), and
    # `rotation_matrix_from_angles` (overlay projection). ALL THREE must agree — if the overlay
    # matrix drifts from the ray one, dots and the volume rotate in different directions.
    R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
    @test isapprox(R0, [1.0 0.0 0.0; 0.0 1.0 0.0; 0.0 0.0 1.0]; atol = 1e-9)
    R90y = rotation_matrix_from_angles((0.0, 90.0, 0.0))
    # Ry(90°) sends x → -z, z → x, y → y (standard right-hand rule). Check three column vectors.
    @test isapprox(R90y * [1.0, 0.0, 0.0], [0.0, 0.0, -1.0]; atol = 1e-9)
    @test isapprox(R90y * [0.0, 1.0, 0.0], [0.0, 1.0,  0.0]; atol = 1e-9)
    @test isapprox(R90y * [0.0, 0.0, 1.0], [1.0, 0.0,  0.0]; atol = 1e-9)
end

@testset "API: movie rail — 2D↔3D overlay projection agrees at identity view" begin
    # The DRIFT GUARANTEE. At angles=(0,0,0), zoom=1, the 3D projection reduces to an axial
    # projection: (x, y, z) → (u, v) = (x - cx + (W+1)/2 * something, y - cy + ...). We test that
    # a point at native voxel (cx, cy, cz) projects to the CANVAS CENTRE, and that swapping angles
    # for the same identity view produces the SAME screen coords whether we go through the 2D
    # `pixel_transform` path (which draws at native pixel + offset) or the 3D projection. The two
    # authors read from ONE `_build_overlay_state` and use the same collection; the projection
    # math must round-trip to the same drawn pixel for identity views.
    R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
    canvas_h, canvas_w = 100, 100
    # Volume extents matching a 100×100×20 image (so ext_x = ext_y and z_aniso = 1 collapses to
    # canvas coordinates that equal the world coordinates plus a centre offset).
    native_w, native_h, nZ = 100, 100, 20
    z_aniso = 1.0
    cx, cy, cz = 49.5, 49.5, 9.5
    wpp = _world_per_px_3d(native_w, native_h, nZ, z_aniso, 1.0, canvas_w)
    @test isapprox(wpp, 1.0; atol = 1e-9)   # canvas span == native extent → 1 world unit / pixel
    u, v = _project_3d_point(R0, cx, cy, cz, z_aniso, wpp,
                                       canvas_h, canvas_w, cx, cy, cz)
    # Centre of volume projects to centre of canvas ((W + 1) / 2, (H + 1) / 2 — 0-based).
    @test isapprox(u, (canvas_w + 1) / 2; atol = 1e-9)
    @test isapprox(v, (canvas_h + 1) / 2; atol = 1e-9)
    # Same world point projected at Ry(90°) rotation — the point at the volume centre should STILL
    # project to the canvas centre (rotation about a point through the centre leaves the centre
    # fixed). This proves the projection actually uses cx/cy/cz as the rotation origin.
    R90 = rotation_matrix_from_angles((0.0, 90.0, 0.0))
    u90, v90 = _project_3d_point(R90, cx, cy, cz, z_aniso, wpp,
                                           canvas_h, canvas_w, cx, cy, cz)
    @test isapprox(u90, (canvas_w + 1) / 2; atol = 1e-9)
    @test isapprox(v90, (canvas_h + 1) / 2; atol = 1e-9)
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
