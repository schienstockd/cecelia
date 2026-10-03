# Offline renderer plumbing testsets — extracted from api/test/runtests.jl.
#
# Four testsets pinning the wiring that feeds the offline movie renderer:
#  - `API: _resolve_movie_overlays_mask honours String-keyed ov_raw` (String-vs-Symbol
#    keying bug — silently returned defaults when raw came from movie_rail.jl).
#  - `API: record_view_movie — 2D through the shared renderer` (region, size, plane window, mask).
#  - `API: stills — the movie's frames as PNGs` (`render_view_stills`, `render_view_state_still`).
#  - `API: interpolate_keyframes — the offline renderer tween`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: _resolve_movie_overlays_mask honours String-keyed ov_raw" begin
    # The bug this pins: `_resolve_movie_overlays_mask` used `get(ov_raw, :symbol, default)` to read
    # every flag, but `_overlays_raw_from_config` (movie_rail.jl) hands over a `Dict{String,Any}`.
    # Symbol lookups against string keys silently returned the DEFAULTS, so `showPopulations=true`,
    # `showMask=false`, `allCellsColour="#9ca3af"` regardless of what the caller had set. Reported
    # 2026-08-31: compare-grid cpSAM-vs-flowTom rendered pop dots on flowTom (its flow
    # populations, painted because show_pops read as its default `true`) and NOTHING on cpSAM (which
    # has no flow pops); the rainbow mask outline never showed up on either cell because show_mask
    # read as its default `false`, skipping the whole mask branch. Julia's own key equality is what
    # made this silent: `get(Dict{String,Any}("k"=>false), :k, true)` returns `true`.
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

            # Minimal label store on disk: a movie mask needs its store (`movie_mask`).
            nt, nc, ny, nx = 3, 1, 8, 8
            labels_dir = joinpath(dir, "testpr", "1", "KDIeEm", "labels")
            mkpath(labels_dir)
            zp = joinpath(labels_dir, "B.zarr")
            axes_attr = Dict("multiscales" =>
                             [Dict("axes" => [Dict("name" => n) for n in ["t", "c", "y", "x"]])])
            g = zgroup(Zarr.DirectoryStore(zp); attrs = axes_attr)
            a = zcreate(UInt16, g, "0", nx, ny, nc, nt; chunks = (nx, ny, nc, nt))
            block = zeros(UInt16, nx, ny, nc, nt)
            block[1:3, 1:3, 1, :] .= UInt16(1)
            block[6:8, 1:3, 1, :] .= UInt16(2)
            block[1:8, 6:8, 1, :] .= UInt16(3)
            a[:, :, :, :] = block
            img.labels = Dict("B" => ["B.zarr"])
            save!(img)
            img, _ = _gating_image("testpr", "KDIeEm")

            arr, caxes = open_level0(String(img_labels_path(img, "B")))

            # STRING-keyed ov_raw (what `_overlays_raw_from_config` hands over): showPopulations=false
            # means "don't draw pop dots". Before the fix, symbol lookup returned `true`.
            ov_str = Dict{String,Any}(
                "showPopulations" => false, "popType" => "flow",
                "showMask" => true, "allCells" => true, "allCellsColour" => "rainbow",
                "maskContourPx" => 2)
            r = _resolve_movie_overlays_mask(img, nothing, arr, caxes, ov_str, "B")
            @test r.overlays3d_for === nothing
            @test r.mask !== nothing
            @test r.mask_diag["requested"] === true
            @test r.mask.contour_px == 2
            # "all cells" is every label in the viewer's palette (no colour table)
            @test r.mask.colours isa ViewerPalette
            # a population mask — not all cells — carries its label → colour table
            ov_pop = merge(ov_str, Dict{String,Any}("allCells" => false, "allCellsColour" => "#00ff00"))
            rp = _resolve_movie_overlays_mask(img, nothing, arr, caxes, ov_pop, "B")
            @test rp.mask === nothing || rp.mask.colours isa AbstractDict

            # Symbol-keyed ov_raw (what `record-test`'s JSON3 parse yields) must still work — the
            # helper tries symbol first, then string, so both wire shapes converge on the same
            # decision. Same asserts.
            ov_sym = Dict{Symbol,Any}(
                :showPopulations => false, :popType => "flow",
                :showMask => true, :allCells => true, :allCellsColour => "rainbow",
                :maskContourPx => 2)
            r2 = _resolve_movie_overlays_mask(img, nothing, arr, caxes, ov_sym, "B")
            @test r2.overlays3d_for === nothing
            @test r2.mask !== nothing && r2.mask.contour_px == 2
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: record_view_movie — 2D through the shared renderer" begin
    # The region / size contract the CPU renderer had (crop, then an integer stride, even sides),
    # now as a head-on camera on the viewer's pass; and the plane window the overlays are cut to.
    style = movie_overlay_style()
    @test (style.point_z_tol, style.track_z_tol, style.point_border_px) == (OVERLAY_Z_TOL, OVERLAY_Z_TOL, 0)
    @test movie_overlay_style(k -> k == "pointZTol" ? 0 : nothing).point_z_tol == 0
    zr, filt = _plane_window(5, 10, (; point_z_tol = 2, track_z_tol = 3))
    @test zr == [5, 5] && filt["points"] == [3, 7] && filt["tracks"] == [2, 8]
    zr, filt = _plane_window(nothing, 10, style)
    @test zr == [0, 9] && filt === nothing                       # the whole stack: every overlay
    @test _plane_window(2:4, 10, style)[1] == [2, 4]
    params = _mask_params!(Dict{String,Any}(), MovieMask("/x",
        Dict(7 => RGB{N0f8}(1, 0, 0), 3 => RGB{N0f8}(0, 0, 1)), 1, 0.5))
    @test params["labelColouring"] == "table"
    @test params["labelColours"]["ids"] == [3, 7]
    @test params["labelColours"]["colours"] == [[0.0, 0.0, 1.0], [1.0, 0.0, 0.0]]
    # a population with no cells here is an EMPTY table — it must still say "table", or the runner
    # would fall back to the palette and draw every label
    empty_tbl = _mask_params!(Dict{String,Any}(), MovieMask("/x", Dict{Int,RGB{N0f8}}(), 1, 0.5))
    @test empty_tbl["labelColouring"] == "table" && isempty(empty_tbl["labelColours"]["ids"])
    @test _mask_params!(Dict{String,Any}(), MovieMask("/x", ViewerPalette(), 1, 0.5))["labelColouring"] == "palette"

    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !api_have_fixture(v2)
        @test_skip "zarr format fixture missing"
    else
        specs = [(0.0, 4000.0, "red", true), (0.0, 4000.0, "green", true)]
        seen = Int[]
        ov3d = function (t::Int)
            push!(seen, t)
            ((; x = [4.0], y = [4.0], z = [1.0], colour = [RGB{N0f8}(0, 0, 1)]), nothing)
        end
        out = joinpath(mktempdir(), "rv.mp4")
        r = record_view_movie(v2, out; ts = [0, 1, 2], channels = 0:1, specs = specs,
                              overlays3d_for = ov3d, on_log = _ -> nothing)
        @test isfile(out) && r.frames == 3 && (r.width, r.height) == (64, 64)
        @test seen == [0, 1, 2]
        # an odd crop comes out even, the size the movie reports
        r_odd = record_view_movie(v2, joinpath(mktempdir(), "odd.mp4"); ts = [0], specs = specs,
                                  channels = 0:1, crop = (x = 0:62, y = 0:60), on_log = _ -> nothing)
        @test (r_odd.width, r_odd.height) == (62, 60)
        # a stride halves it
        r_half = record_view_movie(v2, joinpath(mktempdir(), "half.mp4"); ts = [0], specs = specs,
                                   channels = 0:1, max_px = 32, on_log = _ -> nothing)
        @test (r_half.width, r_half.height) == (32, 32)
        # a cancelled record writes nothing; a t range with nothing in it is a caller error
        rc = record_view_movie(v2, joinpath(mktempdir(), "c.mp4"); ts = [0], cancelled = () -> true)
        @test rc.cancelled && rc.frames == 0
        @test_throws ArgumentError record_view_movie(v2, joinpath(mktempdir(), "x.mp4"); ts = [99])
    end
end

@testset "API: stills — the movie's frames as PNGs" begin
    # `render_view_stills` is `record_view_movie`'s region and size, one PNG per timepoint; the test
    # suite renders them in a one-off process (`STILLS_VIA`), never through the preview worker.
    @test STILLS_VIA[] === _stills_one_off
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !api_have_fixture(v2)
        @test_skip "zarr format fixture missing"
    else
        specs = [(0.0, 4000.0, "red", true), (0.0, 4000.0, "green", true)]
        dir = mktempdir()
        paths = [joinpath(dir, "a.png"), joinpath(dir, "b.png"), joinpath(dir, "c.png")]
        # a timepoint may repeat (a card snaps several picks to one tracked t)
        r = render_view_stills(v2, paths, [1, 0, 1]; specs = specs, channels = 0:1,
                               crop = (x = 0:62, y = 0:60), on_log = _ -> nothing)
        @test (r.width, r.height) == (62, 60) && r.paths == paths
        imgs = [PNGFiles.load(p) for p in paths]
        @test all(i -> size(i) == (60, 62), imgs)
        @test imgs[1] == imgs[3] && imgs[1] != imgs[2]
        # a stride halves it, as in the movie
        r2 = render_view_stills(v2, [joinpath(dir, "h.png")], [0]; specs = specs, channels = 0:1,
                                max_px = 32, on_log = _ -> nothing)
        @test (r2.width, r2.height) == (32, 32)
        @test_throws ArgumentError render_view_stills(v2, paths[1:2], [0]; on_log = _ -> nothing)
        @test_throws ArgumentError render_view_stills(v2, paths[1:1], [99]; on_log = _ -> nothing)
        # a 3D view state renders its 3D view, at the canvas it was captured on
        vs = Dict{String,Any}("camera" => Dict{String,Any}("angles" => [0, 30, 0], "zoom" => 1.0),
                              "dims" => Dict{String,Any}("ndisplay" => 3, "current_step" => [1, 0]),
                              "canvas" => Dict{String,Any}("width" => 48, "height" => 40))
        s3 = render_view_state_still(v2, joinpath(dir, "k.png"), vs, ["CH1", "CH2"];
                                     default_specs = specs, on_log = _ -> nothing)
        @test (s3.width, s3.height) == (48, 40) && size(PNGFiles.load(s3.path)) == (40, 48)
    end
end

@testset "API: interpolate_keyframes — the offline renderer's tween" begin
    # napari-animation does this today, and it is the one part of that dependency worth keeping: a
    # keyframe is a saved view state plus the number of frames it takes to reach it. Every saved
    # animation config already means that, so C has to answer the same contract.
    kf(v, steps = 15) = Dict("viewState" => v, "steps" => steps)

    @test_throws ArgumentError interpolate_keyframes([kf(Dict("a" => 0))])

    # frame count: the first keyframe IS a frame, and every later one is the LAST frame of its own
    # transition — no duplicated frame at the joins.
    seq = interpolate_keyframes([kf(Dict("a" => 0.0), 99), kf(Dict("a" => 10.0), 5)])
    @test length(seq) == 6
    @test seq[1]["a"] == 0.0                    # starts exactly at keyframe 1
    @test seq[end]["a"] == 10.0                 # and ends exactly at keyframe 2
    @test seq[2]["a"] ≈ 2.0                     # evenly spaced in between
    @test issorted([f["a"] for f in seq])

    # three keyframes: each leg has its own step count, and the joins are the keyframes themselves
    tri = interpolate_keyframes([kf(Dict("a" => 0.0)), kf(Dict("a" => 1.0), 2), kf(Dict("a" => 5.0), 4)])
    @test length(tri) == 7
    @test tri[3]["a"] == 1.0                    # the middle keyframe lands on a frame exactly
    @test tri[end]["a"] == 5.0

    # A NON-NUMERIC value has no half-way point. It holds the outgoing keyframe's until the incoming
    # one is reached and changes exactly there — the alternative is erroring, or silently picking a
    # side one frame early, which reads as a colormap that flickers before the transition.
    cm = interpolate_keyframes([kf(Dict("colormap" => "red")), kf(Dict("colormap" => "green"), 4)])
    @test [f["colormap"] for f in cm] == ["red", "red", "red", "red", "green"]

    # `visible` is a Bool, which is an Integer in Julia — lerping it would produce 0.5 and then `true`
    # for every frame after the first. It has to step like a string does.
    vis = interpolate_keyframes([kf(Dict("visible" => false)), kf(Dict("visible" => true), 3)])
    @test [f["visible"] for f in vis] == [false, false, false, true]

    # `camera.perspective` is a 0/1 number but a switch: it flips on the keyframe, either direction,
    # rather than on the first in-between frame (where a 0.5 would already read as perspective).
    pa = Dict("camera" => Dict("perspective" => 0)); pb = Dict("camera" => Dict("perspective" => 1))
    @test [f["camera"]["perspective"] for f in interpolate_keyframes([kf(pa), kf(pb, 3)])] == [0, 0, 0, 1]
    @test [f["camera"]["perspective"] for f in interpolate_keyframes([kf(pb), kf(pa, 3)])] == [1, 1, 1, 0]

    # nested state (camera / dims / per-layer props) tweens all the way down
    a = Dict("camera" => Dict("zoom" => 1.0, "center" => [0.0, 0.0]), "dims" => Dict("current_step" => [0, 4]))
    b = Dict("camera" => Dict("zoom" => 3.0, "center" => [10.0, 20.0]), "dims" => Dict("current_step" => [8, 4]))
    nest = interpolate_keyframes([kf(a), kf(b, 4)])
    @test nest[3]["camera"]["zoom"] ≈ 2.0
    @test nest[3]["camera"]["center"] ≈ [5.0, 10.0]
    @test nest[3]["dims"]["current_step"] ≈ [4.0, 4.0]      # a slider sweeps; the caller rounds
    @test nest[end]["dims"]["current_step"] == [8, 4]

    # a key in one snapshot and not the other means "that layer was not in this snapshot", NOT zero —
    # tweening towards a zero that was never asked for fades a channel out for no reason
    part = interpolate_keyframes([kf(Dict("a" => 4.0)), kf(Dict("a" => 4.0, "b" => 9.0), 2)])
    @test part[2]["b"] == 9.0 && part[end]["b"] == 9.0

    # arrays of different length cannot be tweened elementwise, so they step
    len = interpolate_keyframes([kf(Dict("v" => [1.0, 2.0])), kf(Dict("v" => [1.0, 2.0, 3.0]), 2)])
    @test len[2]["v"] == [1.0, 2.0]
    @test len[end]["v"] == [1.0, 2.0, 3.0]

    # steps <= 0 would divide by zero or emit nothing; it means "get there next frame"
    z = interpolate_keyframes([kf(Dict("a" => 0.0)), kf(Dict("a" => 1.0), 0)])
    @test length(z) == 2 && z[end]["a"] == 1.0

    # the animation page's own shape, and a missing `steps` falling back to napari's default of 15
    nt = interpolate_keyframes([(; viewState = Dict("a" => 0.0)), (; viewState = Dict("a" => 1.0))])
    @test length(nt) == 16 && nt[end]["a"] == 1.0
end


@testset "API: _z_aniso — physical z over x, 1 when x is unknown" begin
    @test _z_aniso((2.0, 0.5, 0.5)) == 4.0
    @test _z_aniso((2.0, 0.5, 0.0)) == 1.0      # a zero x size would make the 3D render Inf-deep
    @test _z_aniso((2.0,)) == 1.0
end
