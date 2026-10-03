# Offline renderer plumbing testsets — extracted from api/test/runtests.jl.
#
# Four testsets pinning the wiring that feeds the offline movie renderer:
#  - `API: _resolve_movie_overlays_mask honours String-keyed ov_raw` (String-vs-Symbol
#    keying bug — silently returned defaults when raw came from movie_rail.jl).
#  - `API: render_view_frame — points and segments overlays` (offline renderer draws
#    both live overlays paths against a real fixture).
#  - `API: record_view_movie — 2D through the shared renderer` (region, size, plane window, mask).
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

@testset "API: render_view_frame — points and segments overlays" begin
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !api_have_fixture(v2)
        @test_skip "zarr format fixture missing"
    else
        specs = [(0.0, 4000.0, "red", true), (0.0, 4000.0, "green", true)]
        base  = render_view_frame(v2, 1; channels = 0:1, specs = specs)
        H, W = size(base)

        # A point marker changes the pixel — assertion is on DIFFERENCE against the un-overlayed frame,
        # so a fixture that happens to have blue at (10, 10) still passes.
        with_pt = render_view_frame(v2, 1; channels = 0:1, specs = specs,
                                    points = (; x = [10], y = [10],
                                              colour = [RGB{N0f8}(0, 0, 1)]),
                                    point_size_px = 3)
        @test size(with_pt) == (H, W)
        @test with_pt[10, 10] == RGB{N0f8}(0, 0, 1)
        @test with_pt != base

        # Same for segments.
        with_seg = render_view_frame(v2, 1; channels = 0:1, specs = specs,
                                     segments = (; x0 = [5], y0 = [5], x1 = [40], y1 = [5],
                                                 colour = [RGB{N0f8}(1, 1, 1)]),
                                     segment_width_px = 1)
        @test with_seg[5, 5]  == RGB{N0f8}(1, 1, 1)
        @test with_seg[5, 40] == RGB{N0f8}(1, 1, 1)

        # Segments draw BELOW points — a marker at a track endpoint reads as a marker, not as a
        # fatter tail.
        both = render_view_frame(v2, 1; channels = 0:1, specs = specs,
                                 points   = (; x = [20], y = [20],
                                             colour = [RGB{N0f8}(1, 0, 0)]),
                                 segments = (; x0 = [20], y0 = [20], x1 = [30], y1 = [20],
                                             colour = [RGB{N0f8}(0, 1, 0)]),
                                 point_size_px = 3, segment_width_px = 1)
        @test both[20, 20] == RGB{N0f8}(1, 0, 0)              # point wins at the shared pixel
        @test both[20, 30] == RGB{N0f8}(0, 1, 0)              # the tail's other end is untouched

        # A mask outline paints the outline colour on the rim of the labelled region — asserted on a
        # cell shape big enough to have an interior, so the "outline is a rim, not a fill" property
        # from the primitive testset carries through.
        mask = zeros(Int, H, W)
        mask[30:40, 30:40] .= 1
        with_mask = render_view_frame(v2, 1; channels = 0:1, specs = specs,
                                      mask = mask,
                                      mask_colours = Dict{Int,RGB{N0f8}}(1 => RGB{N0f8}(1, 0, 1)),
                                      mask_contour_px = 1)
        @test size(with_mask) == (H, W)
        @test with_mask[30, 30] == RGB{N0f8}(1, 0, 1)         # rim
        @test with_mask[35, 35] != RGB{N0f8}(1, 0, 1)         # interior — original pixel preserved

        # Layer order: mask below segments below points. A point AT a mask outline pixel still reads
        # as the point's colour.
        layered = render_view_frame(v2, 1; channels = 0:1, specs = specs,
                                    mask = mask,
                                    mask_colours = Dict{Int,RGB{N0f8}}(1 => RGB{N0f8}(1, 0, 1)),
                                    points = (; x = [30], y = [30],
                                              colour = [RGB{N0f8}(1, 1, 0)]),
                                    point_size_px = 3, mask_contour_px = 1)
        @test layered[30, 30] == RGB{N0f8}(1, 1, 0)           # point wins over the outline
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

