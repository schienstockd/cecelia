# Movie output params + zarr fmt + offline renderer frame + CPU overlays — extracted from
# api/test/runtests.jl.
#
# Testsets covering the offline-renderer inputs:
#  - `API: movie output size` (blank = canvas size; ONE reader for all three surfaces).
#  - `API: movie filename suffix` (two movies of one image → distinct names).
#  - `API: zarr v2 and v3 read identically` (fixture round-trip).
#  - `API: label_geometry_mismatch` (a mask from another image version is not drawn).
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

@testset "API: label_geometry_mismatch — a mask from another image version is not drawn" begin
    # Zarr.jl presents C-order axes reversed, so a ["t","c","z","y","x"] store is (x, y, z, c, t) here.
    img5  = zeros(UInt8, 12, 10, 3, 2, 2);  img_ax = ["t", "c", "z", "y", "x"]
    lab4  = zeros(UInt16, 12, 10, 3, 2);    lab_ax = ["t", "z", "y", "x"]
    @test label_geometry_mismatch(img5, img_ax, lab4, lab_ax) === nothing
    # drift correction padded the frame by a few pixels — the zolIMa case (420×441 mask, 427×448 frame)
    padded = zeros(UInt8, 14, 11, 3, 2, 2)
    @test label_geometry_mismatch(padded, img_ax, lab4, lab_ax) == "3×10×12 vs 3×11×14"
    # SMALLER frame (a crop) used to pass the old size check silently and draw the mask misaligned
    cropped = zeros(UInt8, 11, 9, 3, 2, 2)
    @test label_geometry_mismatch(cropped, img_ax, lab4, lab_ax) !== nothing
    # a Z-extent difference alone is a mismatch too (a 32-plane mask on a 31-plane version)
    @test label_geometry_mismatch(zeros(UInt8, 12, 10, 2, 2, 2), img_ax, lab4, lab_ax) !== nothing
    # 2D store without Z compares Y/X only
    @test label_geometry_mismatch(zeros(UInt8, 12, 10, 2, 2), ["t", "c", "y", "x"],
                                  zeros(UInt16, 12, 10, 2), ["t", "y", "x"]) === nothing
end
