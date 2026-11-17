# Image thumbnails + set montage (api/src/thumbnails_api.jl).

@testset "API: thumbnail_level picks the coarsest level that does not upscale" begin
    # C-order shapes (t, c, z, y, x), each level half the last — the long side is X.
    lv = [(; shape = [1, 2, 3, 1000, 2000]), (; shape = [1, 2, 3, 500, 1000]),
          (; shape = [1, 2, 3, 250, 500]),   (; shape = [1, 2, 3, 125, 250])]
    @test thumbnail_level(lv, 160) == 3        # 250 ≥ 160; nothing coarser exists
    @test thumbnail_level(lv, 300) == 2        # 500 ≥ 300 > 250
    @test thumbnail_level(lv, 1000) == 1       # exactly the long side still counts
    @test thumbnail_level(lv, 3000) == 0       # nothing is big enough → full resolution, never upscale
    @test thumbnail_level(nothing, 160) == 0   # unreadable pyramid metadata → level 0
    @test thumbnail_level(lv[1:1], 160) == 0   # a single-level store (AF-corrected default)
end

@testset "API: montage_layout is a near-square padded grid" begin
    pad = MONTAGE_PAD
    @test montage_layout(1, 100) == (1, 1, 100 + 2pad, 100 + 2pad)
    @test montage_layout(4, 100)[1:2] == (2, 2)
    @test montage_layout(5, 100)[1:2] == (3, 2)
    @test montage_layout(64, 100)[1:2] == (8, 8)
    cols, rows, W, H = montage_layout(10, 50)
    @test cols * rows >= 10 && W == cols * (50 + pad) + pad && H == rows * (50 + pad) + pad
end

@testset "API: shared_specs keeps brightness differences that per-image contrast hides" begin
    # Image B is image A at twice the intensity. Per-image percentile contrast draws them identically
    # (each stretched to its own range); the shared contrast must draw B brighter — the montage exists
    # to show exactly that difference.
    A = reshape(Float32.(1:400), 1, 20, 20)
    B = 2 .* A
    shared = shared_specs([A, B])
    @test length(shared) == 1
    lo, hi = shared[1][1], shared[1][2]
    @test lo < maximum(A) < hi                          # pooled range reaches past A into B

    lum(img) = sum(Float64(ColorTypes.red(p)) + Float64(ColorTypes.green(p)) + Float64(ColorTypes.blue(p))
                   for p in img)
    @test lum(composite_rgb(B, shared)) > 1.5 * lum(composite_rgb(A, shared))

    perA = [percentile_spec(view(A, 1, :, :), "red")]
    perB = [percentile_spec(view(B, 1, :, :), "red")]
    @test lum(composite_rgb(A, perA)) ≈ lum(composite_rgb(B, perB)) rtol = 0.02

    # mixed channel counts: a channel only some images have is pooled over those images only
    C3 = cat(A, A, A; dims = 1)
    s = shared_specs([A, C3])
    @test length(s) == 3
    @test s[2][1] == percentile_spec(view(C3, 2, :, :), "x")[1]
end

@testset "API: preview_chw reads a real store at thumbnail size" begin
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !api_have_fixture(v2)
        @test_skip "zarr format fixture missing"
    else
        chw = preview_chw(v2, 0; max_px = 32)
        @test size(chw) == (4, 32, 32)                     # 4 channels, 64×64 subsampled by 2
        @test maximum(chw) > 3000                          # real intensity, byte order applied
        # level choice against the real pyramid metadata: one 64-px level → level 0 for any size
        @test thumbnail_level(store_pyramid_levels(v2), 32) == 0
        png = png_bytes(srgb_encode(composite_rgb(chw, preview_specs(chw, "/no/props.json"))))
        @test png[1:4] == UInt8[0x89, 0x50, 0x4e, 0x47]    # a PNG
    end
end

@testset "API: thumbnail routes — input guards" begin
    req(t) = HTTP.Request("GET", t)
    st, out = api_image_thumbnail(req("/api/images/thumbnail?imageUid=x"))
    @test st == 400 && occursin("projectUid", JSON3.read(out).error)
    st, out = api_image_thumbnail(req("/api/images/thumbnail?projectUid=no-such&imageUid=nope"))
    @test st == 404

    st, out = api_set_thumbnails(req("/api/sets/thumbnails?projectUid=p"))
    @test st == 400 && occursin("setUid", JSON3.read(out).error)
    st, out = api_set_thumbnails(req("/api/sets/thumbnails?projectUid=p&setUid=s&contrast=auto"))
    @test st == 400 && occursin("contrast", JSON3.read(out).error)
    st, out = api_set_thumbnails(req("/api/sets/thumbnails?projectUid=no-such&setUid=nope"))
    @test st == 404
end

@testset "API: cached_render! holds any value and computes a key once" begin
    calls = Ref(0)
    k = "test-cache|$(time_ns())"
    a = cached_render!(k, () -> (calls[] += 1; reshape(Float32.(1:4), 1, 2, 2)))
    b = cached_render!(k, () -> (calls[] += 1; zeros(Float32, 1, 2, 2)))
    @test calls[] == 1 && a === b                 # second call is a hit — the block, not bytes
    @test cached_render!(k * "png", () -> UInt8[1, 2]) == UInt8[1, 2]
    # the do-block form — what `_thumb_chw` calls; a positional-only method failed it inside every tile
    c = cached_render!(k * "do") do
        calls[] += 1; UInt8[3]
    end
    @test c == UInt8[3] && calls[] == 2
end
