# Movie output params + zarr fmt + offline renderer frame + CPU overlays — extracted from
# api/test/runtests.jl.
#
# Five testsets covering the offline-renderer inputs and its per-frame CPU pipeline:
#  - `API: movie output size` (blank = canvas size; ONE reader for all three surfaces).
#  - `API: movie filename suffix` (two movies of one image → distinct names).
#  - `API: zarr v2 and v3 read identically` (fixture round-trip).
#  - `API: render_view_frame — the offline renderer movie frame`.
#  - `API: frame_overlays — CPU point + segment drawing`.
#
# No path expressions to rewrite in this extract (api_fixture uses the constant already).
# Extracted so runtests.jl contains only include lines + section-header comments — same
# shape as app/test/suite/*.jl.

@testset "API: movie output size (blank means the canvas size)" begin
    # `_movie_size_params` is the ONE reader of a requested movie size for all three surfaces (single
    # record, keyframe animation, batch). "Blank = the napari canvas size" has to be defined once: a
    # movie has always come out at canvas size, so an absent field is the default, not an error — and
    # `nothing` is what reaches `record_timelapse!`, which omits the size and lets napari use the canvas.
    # The pixel-level validation (clamp, even axes) lives in Python's movie_io.coerce_movie_size.
    p(json) = _movie_size_params(JSON3.read(json))

    @test p("""{"sizeX":1920,"sizeY":1080}""") == (1920, 1080)
    @test p("{}") == (nothing, nothing)                     # absent → canvas size
    @test p("""{"sizeX":1920}""") == (1920, nothing)         # one axis: the caller decides what half means
    # a blank field arrives as "" from a number input the user cleared — not a parse error, a default
    @test p("""{"sizeX":"","sizeY":""}""") == (nothing, nothing)
    @test p("""{"sizeX":"1920","sizeY":"1080"}""") == (1920, 1080)   # strings parse
    # zero/negative/junk are all "unset" rather than 500s — the size is advisory, never fatal
    @test p("""{"sizeX":0,"sizeY":0}""") == (nothing, nothing)
    @test p("""{"sizeX":-4,"sizeY":-4}""") == (nothing, nothing)
    @test p("""{"sizeX":"wide","sizeY":"tall"}""") == (nothing, nothing)
    @test p("""{"sizeX":null,"sizeY":null}""") == (nothing, nothing)
end

@testset "API: movie filename suffix (two movies of one image)" begin
    # A movie is named after the IMAGE, so recording the AF-corrected version and then the raw import
    # writes the same path twice — the second silently replaces the first. `_movie_suffix` is the
    # filename addition that keeps them apart; the frontend prefills it with the shown version, but it
    # is free text, so it must be sanitised HERE rather than trusted.
    @test _movie_suffix("corrected") == "_corrected"
    @test _movie_suffix("") == ""
    @test _movie_suffix(nothing) == ""
    @test _movie_suffix("   ") == ""
    @test _movie_suffix(" raw vs af ") == "_raw_vs_af"        # spaces are not filename material
    @test _movie_suffix("../../etc/passwd") == "_etc_passwd"   # no separators, no leading dots
    @test _movie_suffix("__x__") == "_x"                       # no doubled/trailing separators
    @test _movie_suffix("_") == ""                             # nothing left after stripping
    @test length(_movie_suffix("a"^200)) == MOVIE_SUFFIX_MAX + 1   # + the leading '_'

    # …and it lands BEFORE the extension in both naming schemes, or the file stops being an .mp4 to
    # every listing that filters on one.
    @test endswith(_movie_basename(Dict(), "abc", String[]; suffix = "_corrected"), "abc_corrected.mp4")
    @test endswith(_movie_basename(Dict("a" => "wt"), "abc", ["a"]; suffix = "_raw"), "wt_abc_raw.mp4")
end

@testset "API: zarr v2 and v3 read identically" begin
    # Two committed stores holding the SAME real pixels, written by bioformats2raw 0.12.1 as NGFF 0.4
    # (zarr v2) and NGFF 0.5 (zarr v3, SHARDED). See test-data/README.md + docs/todo/ZARR_V3_PLAN.md.
    #
    # Why real stores rather than hand-written metadata: NGFF 0.5 nests every attribute under `ome`, and
    # a reader that misses that does not error — axes come back EMPTY and `axis_dims` silently guesses
    # the order by rank, while scale comes back missing and becomes "1 um, 1 second per frame"
    # downstream. Both failures look like success. The fixture's calibration is deliberately NOT 1.0 so
    # a correct read is distinguishable from the fallback.
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = api_fixture("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(api_have_fixture(v2) && api_have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        a2, ax2 = open_level0(v2)
        a3, ax3 = open_level0(v3)

        # axes must be READ, not guessed. `String[]` here is the silent-failure signature.
        @test ax2 == ["t", "c", "z", "y", "x"]
        @test ax3 == ax2
        @test !isempty(ax3)
        @test axis_dims(ax3, ndims(a3)) == axis_dims(ax2, ndims(a2))

        @test image_geometry(v2) == image_geometry(v3)
        @test image_geometry(v2) == (sizeX = 64, sizeY = 64, sizeZ = 3, sizeT = 3)

        # identical pixels across formats — this is also what proves v3 needs no byte-order branch:
        # v3 keeps `endian` in the `bytes` codec INSIDE the pipeline Zarr.jl executes, unlike v2 where
        # the dtype string is metadata Zarr.jl parses for the eltype and then ignores.
        b2 = read_native(a2, :, :, :, :, :)
        b3 = read_native(a3, :, :, :, :, :)
        @test size(b2) == size(b3)
        @test b2 == b3
        @test maximum(b2) > 3000            # real intensity data, not a zeroed/garbled read

        # store_compression reports the format, and chunk-vs-shard the right way round. The v3 fixture
        # is sharded with shard != chunk ON PURPOSE — with equal values this assertion cannot fail.
        # the NGFF spec version each store declares — a different question from the zarr format, and
        # both are shown side by side in the metadata modal
        @test ngff_version(v2) == "0.4"
        @test ngff_version(v3) == "0.5"

        c2 = store_compression(v2); c3 = store_compression(v3)
        @test c2.zarrFormat == 2 && isnothing(c2.shard)
        @test c3.zarrFormat == 3
        @test c3.chunks == [1, 1, 1, 32, 32]      # inner chunk (from the sharding codec)
        @test c3.shard  == [1, 1, 1, 64, 64]      # outer grid = one file on disk
        @test c3.chunks != c3.shard

        # The chunk-key separator: "/" nests keys into a directory tree, "." keeps them flat. It is
        # most of a store's filesystem footprint (measured on a real 1.7 GB import: 20,933 directories
        # nested vs 4 flat) and all of its cost on a network share, so the modal states it. The DEFAULT
        # differs per format — "." for v2, "/" for v3 — so an absent key must not be read as one value.
        @test c2.separator in (".", "/")
        @test c3.separator in (".", "/")
        @test c2.separator == "/"      # bioformats2raw nests by default, in BOTH formats
        @test c3.separator == "/"
        # same codec asked for on both, so the describer must agree across formats (int shuffle in v2
        # metadata, NAME in v3 — normalised in one place)
        @test c2.codec == c3.codec == "zstd"
        @test c2.shuffle && c3.shuffle
        @test c2.label == c3.label

        # the preview renderer works on both (no props file → percentile auto-contrast)
        for p in (v2, v3)
            png = render_preview_frame(p, joinpath(p, "absent.json"), 1)
            @test length(png) > 100
            @test png[2:4] == UInt8['P', 'N', 'G']
        end
    end
end

@testset "API: render_view_frame — the offline renderer's movie frame" begin
    # The offline frame (WEB_VIEWER_PLAN.md → P5). Asserted on the committed real stores rather than a
    # phantom, because the three things that break here are all store-shaped: axis order, byte order,
    # and whether a z SELECTION means the same thing as it does on the browser's slab route.
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = api_fixture("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(api_have_fixture(v2) && api_have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        lum(px) = Float64(ColorTypes.red(px)) + Float64(ColorTypes.green(px)) + Float64(ColorTypes.blue(px))

        full = render_view_frame(v2, 1)
        @test size(full) == (64, 64)                       # (y, x), native — no silent downsample
        @test eltype(full) <: RGB
        @test maximum(lum, full) > 0.05                    # real pixels, not a black read

        # A MIP over the stack can only be BRIGHTER than any one plane of it, everywhere. That is the
        # assertion that catches a z selection reading the wrong axis: a transposed read still produces
        # a plausible picture, and still differs from the plane.
        specs = [(0.0, 4000.0, "red", true), (0.0, 4000.0, "green", true)]
        nch = 2
        mip   = render_view_frame(v2, 1; channels = 0:(nch - 1), specs = specs)
        plane = render_view_frame(v2, 1; z = 0, channels = 0:(nch - 1), specs = specs)
        @test size(mip) == size(plane)
        @test all(lum(mip[i]) >= lum(plane[i]) - 1e-6 for i in eachindex(mip))
        @test mip != plane                                 # the fixture has structure across z

        # A range that covers the whole stack IS the whole-stack projection — one vocabulary with the
        # slab route, where `nothing` / Int / UnitRange are the same three answers.
        @test render_view_frame(v2, 1; z = 0:2, channels = 0:(nch - 1), specs = specs) == mip
        @test render_view_frame(v2, 1; z = 0:0, channels = 0:(nch - 1), specs = specs) == plane

        # crop is 0-based inclusive, and clamps rather than producing a zero-size frame — an encoder
        # reports that as a corrupt movie, not as a bad crop.
        @test size(render_view_frame(v2, 1; crop = (x = 10:29, y = 4:13))) == (10, 20)
        @test size(render_view_frame(v2, 1; crop = (x = 900:999, y = 900:999))) == (1, 1)
        @test size(render_view_frame(v2, 1; crop = (y = 0:31,))) == (32, 64)

        # max_px strides the long side down; 0 leaves it alone
        @test size(render_view_frame(v2, 1; max_px = 32)) == (32, 32)
        @test size(render_view_frame(v2, 1; max_px = 0)) == (64, 64)

        # one channel through one LUT: nothing else may bleed in
        red1 = render_view_frame(v2, 1; channels = [0], specs = [(0.0, 4000.0, "red", true)])
        @test all(ColorTypes.green(px) == 0 && ColorTypes.blue(px) == 0 for px in red1)

        # the two formats hold the same pixels, so they must render the same frame
        @test render_view_frame(v3, 1; channels = 0:(nch - 1), specs = specs) == mip

        @test_throws ArgumentError render_view_frame(v2, 1; channels = Int[])
        @test_throws ArgumentError render_view_frame(v2, 1; channels = [99])
        @test_throws ArgumentError render_view_frame(v2, 1; channels = [0, 1], specs = specs[1:1])

        # opening ONCE and reading many slabs must answer exactly what the path form does — that
        # overload exists so a movie sweep is not nT*nC metadata round trips.
        a2, ax2 = open_level0(v2)
        @test render_view_frame(a2, ax2, 1; channels = 0:(nch - 1), specs = specs) == mip
        @test read_slab(a2, ax2, 1, 0; z = 1) == read_slab(v2, 1, 0; z = 1)
    end
end

@testset "API: frame_overlays — CPU point + segment drawing" begin
    # Pure drawing on an RGB frame, no zarr / no populations / no camera. What the caller has to have
    # done is column-oriented columns of `(x, y, colour)`; this asserts the drawing itself.
    black = fill(RGB{N0f8}(0, 0, 0), 20, 20)

    # A single red marker at (10, 5) — 1-based row/col, so column x=5 in row y=10.
    frame = copy(black)
    draw_points!(frame, (; x = [5], y = [10], colour = [RGB{N0f8}(1, 0, 0)]); size_px = 3)
    @test frame[10, 5] == RGB{N0f8}(1, 0, 0)
    # Non-marker pixels stay black — the marker is a disc, not a full-frame fill.
    @test frame[1, 1] == RGB{N0f8}(0, 0, 0)
    # A marker CENTRED at (5, 10) means the ROW is 10 and the COLUMN is 5. Getting the axes swapped
    # would put every cell at the reflected pixel — a movie that reads plausibly and is a bug.
    @test frame[5, 10] == RGB{N0f8}(0, 0, 0)

    # A marker whose centre lies OFF-frame still paints the pixels of its disc that are inside.
    edge = copy(black)
    draw_points!(edge, (; x = [1], y = [1], colour = [RGB{N0f8}(0, 1, 0)]); size_px = 3)
    @test edge[1, 1] == RGB{N0f8}(0, 1, 0)                     # centre inside
    off = copy(black)
    draw_points!(off, (; x = [0], y = [0], colour = [RGB{N0f8}(0, 1, 0)]); size_px = 3)
    @test off[1, 1] == RGB{N0f8}(0, 1, 0)                      # nearest inside pixel painted

    # Draw ORDER matters: the LAST point wins under overlap, so a caller who wants a foreground pop
    # over a background pop lists the foreground last.
    over = copy(black)
    draw_points!(over,
        (; x = [10, 10], y = [10, 10], colour = [RGB{N0f8}(1, 0, 0), RGB{N0f8}(0, 0, 1)]);
        size_px = 3)
    @test over[10, 10] == RGB{N0f8}(0, 0, 1)

    # Length mismatch is caught early — a silently-shorter colour column paints one pop's markers in
    # another pop's colour.
    @test_throws ArgumentError draw_points!(black,
        (; x = [1, 2], y = [1, 2], colour = [RGB{N0f8}(1, 0, 0)]); size_px = 3)

    # A segment paints its two endpoints and the pixels between them. Horizontal at row 10.
    seg = copy(black)
    draw_segments!(seg,
        (; x0 = [3], y0 = [10], x1 = [15], y1 = [10], colour = [RGB{N0f8}(1, 1, 1)]);
        width_px = 1)
    @test seg[10, 3] == RGB{N0f8}(1, 1, 1)
    @test seg[10, 9] == RGB{N0f8}(1, 1, 1)                     # mid
    @test seg[10, 15] == RGB{N0f8}(1, 1, 1)
    @test seg[10, 2] == RGB{N0f8}(0, 0, 0)                     # before start
    @test seg[9, 9]  == RGB{N0f8}(0, 0, 0)                     # off the row

    # width_px thickens perpendicularly — a width-3 horizontal line paints three rows.
    fat = copy(black)
    draw_segments!(fat,
        (; x0 = [3], y0 = [10], x1 = [15], y1 = [10], colour = [RGB{N0f8}(1, 1, 1)]);
        width_px = 3)
    @test fat[9, 9]  == RGB{N0f8}(1, 1, 1)
    @test fat[10, 9] == RGB{N0f8}(1, 1, 1)
    @test fat[11, 9] == RGB{N0f8}(1, 1, 1)
    @test fat[8, 9]  == RGB{N0f8}(0, 0, 0)                     # only ±1 away

    # A diagonal line — asserts Bresenham reaches both endpoints, not just one.
    diag = copy(black)
    draw_segments!(diag,
        (; x0 = [3], y0 = [3], x1 = [12], y1 = [12], colour = [RGB{N0f8}(1, 0, 1)]);
        width_px = 1)
    @test diag[3, 3]   == RGB{N0f8}(1, 0, 1)
    @test diag[12, 12] == RGB{N0f8}(1, 0, 1)
    @test diag[7, 7]   == RGB{N0f8}(1, 0, 1)                   # on the line

    # size_px <= 0 draws nothing rather than throwing — a caller with a config that resolved to 0 gets
    # a movie without markers, not a crash mid-render.
    zero_px = copy(black)
    draw_points!(zero_px, (; x = [10], y = [10], colour = [RGB{N0f8}(1, 0, 0)]); size_px = 0)
    @test zero_px == black

    # ── per-segment alpha — track-tail fade parity with the 3D PIL rasteriser.
    # A segment with alpha = 1 paints the pure colour (same as no alpha). alpha = 0.5 blends
    # 50/50 with the underlying frame. alpha = 0 leaves the frame untouched.
    bg = fill(RGB{N0f8}(0.2, 0.0, 0.0), 20, 20)   # dark red background
    fade = copy(bg)
    draw_segments!(fade,
        (; x0 = [3], y0 = [10], x1 = [15], y1 = [10],
           colour = [RGB{N0f8}(1, 1, 1)], alpha = [0.5]);
        width_px = 1)
    # 0.5 * white + 0.5 * (0.2, 0, 0) → (0.6, 0.5, 0.5). N0f8 rounds each channel to /255.
    r = Float64(fade[10, 9].r); g = Float64(fade[10, 9].g); b = Float64(fade[10, 9].b)
    @test isapprox(r, 0.6; atol = 0.01)
    @test isapprox(g, 0.5; atol = 0.01)
    @test isapprox(b, 0.5; atol = 0.01)
    # alpha = 0 → frame unchanged
    zero_alpha = copy(bg)
    draw_segments!(zero_alpha,
        (; x0 = [3], y0 = [10], x1 = [15], y1 = [10],
           colour = [RGB{N0f8}(1, 1, 1)], alpha = [0.0]);
        width_px = 1)
    @test zero_alpha == bg
    # Alpha length mismatch → error early
    @test_throws ArgumentError draw_segments!(bg,
        (; x0 = [3], y0 = [10], x1 = [15], y1 = [10],
           colour = [RGB{N0f8}(1, 1, 1)], alpha = [0.5, 0.5]);
        width_px = 1)

    # ── mask outlines ────────────────────────────────────────────────────────────
    # A 2x2 block of id=1 in the middle of an otherwise empty frame. Its outline is the whole 2x2
    # block: every pixel of the block has at least one background neighbour, so the outline IS the
    # block. Cells one pixel across always end up fully painted; that is what napari's `contour = 1`
    # produces too, and is not a bug.
    mask = zeros(Int, 20, 20)
    mask[10:11, 10:11] .= 1
    id_col = Dict{Int,RGB{N0f8}}(1 => RGB{N0f8}(1, 0, 0))
    mfr = copy(black)
    draw_mask_outline!(mfr, mask, id_col; contour_px = 1)
    @test mfr[10, 10] == RGB{N0f8}(1, 0, 0)
    @test mfr[11, 11] == RGB{N0f8}(1, 0, 0)
    @test mfr[10, 9]  == RGB{N0f8}(0, 0, 0)                    # background stays untouched
    @test mfr[12, 12] == RGB{N0f8}(0, 0, 0)

    # A larger cell: the INTERIOR (a pixel whose four neighbours are all the same id) is NOT painted —
    # the outline is a rim, not a fill. Bug this catches: neighbour-testing the just-painted array
    # would propagate the outline colour sideways across the cell as a filled band.
    big = zeros(Int, 20, 20)
    big[5:14, 5:14] .= 2
    id_big = Dict{Int,RGB{N0f8}}(2 => RGB{N0f8}(0, 1, 0))
    bfr = copy(black)
    draw_mask_outline!(bfr, big, id_big; contour_px = 1)
    @test bfr[5, 5]   == RGB{N0f8}(0, 1, 0)                    # corner: on the rim
    @test bfr[5, 10]  == RGB{N0f8}(0, 1, 0)                    # top edge
    @test bfr[9, 9]   == RGB{N0f8}(0, 0, 0)                    # interior: unpainted
    @test bfr[10, 10] == RGB{N0f8}(0, 0, 0)

    # contour_px thickens perpendicularly — a width-3 outline paints the two 1-pixel bands either
    # side of the rim as well as the rim itself.
    fat = copy(black)
    draw_mask_outline!(fat, big, id_big; contour_px = 3)
    @test fat[5, 5]   == RGB{N0f8}(0, 1, 0)                    # rim
    @test fat[4, 5]   == RGB{N0f8}(0, 1, 0)                    # one out
    @test fat[6, 6]   == RGB{N0f8}(0, 1, 0)                    # one in — thickens BOTH sides
    @test fat[3, 5]   == RGB{N0f8}(0, 0, 0)                    # two out: outside a width-3 outline

    # Ids absent from the map are skipped — a hidden pop leaves its labels untouched rather than
    # needing the caller to rewrite the mask.
    both = zeros(Int, 20, 20)
    both[5:8, 5:8] .= 3
    both[12:15, 12:15] .= 4
    only3 = Dict{Int,RGB{N0f8}}(3 => RGB{N0f8}(0, 0, 1))       # 4 absent
    ofr = copy(black)
    draw_mask_outline!(ofr, both, only3; contour_px = 1)
    @test ofr[5, 5]   == RGB{N0f8}(0, 0, 1)                    # id 3 painted
    @test ofr[12, 12] == RGB{N0f8}(0, 0, 0)                    # id 4 skipped

    # A cell touching the frame edge reads as CLOSED — the edge is treated as "different", so the
    # outline runs along the edge as well as the cell's interior boundary. Otherwise the outline
    # would open at the frame boundary and read like a track that walked off-screen.
    edge = zeros(Int, 20, 20)
    edge[1:3, 1:3] .= 5
    efr = copy(black)
    draw_mask_outline!(efr, edge, Dict{Int,RGB{N0f8}}(5 => RGB{N0f8}(1, 1, 0)); contour_px = 1)
    @test efr[1, 1] == RGB{N0f8}(1, 1, 0)
    @test efr[1, 3] == RGB{N0f8}(1, 1, 0)
    @test efr[3, 1] == RGB{N0f8}(1, 1, 0)

    # Two adjacent cells with different ids — the shared border is painted in BOTH colours, but only
    # each cell's OWN colour touches its own interior side. Bug this catches: writing across the
    # border would leak one cell's colour into the neighbour.
    adj = zeros(Int, 20, 20)
    adj[5:10, 5:9]  .= 6
    adj[5:10, 10:14] .= 7
    dfr = copy(black)
    draw_mask_outline!(dfr, adj,
        Dict{Int,RGB{N0f8}}(6 => RGB{N0f8}(1, 0, 0), 7 => RGB{N0f8}(0, 0, 1)); contour_px = 1)
    @test dfr[7, 9]  == RGB{N0f8}(1, 0, 0)                     # id 6's border pixel — red
    @test dfr[7, 10] == RGB{N0f8}(0, 0, 1)                     # id 7's border pixel — blue

    # Shape guard: mask and frame must agree.
    @test_throws ArgumentError draw_mask_outline!(black, zeros(Int, 10, 10),
        Dict{Int,RGB{N0f8}}(); contour_px = 1)

    # contour_px <= 0 draws nothing rather than throwing — same policy as size_px / width_px on the
    # other primitives.
    ncr = copy(black)
    draw_mask_outline!(ncr, big, id_big; contour_px = 0)
    @test ncr == black
end
