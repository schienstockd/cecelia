# VN version routing + rendering + storage codec testsets — extracted from api/test/runtests.jl.
#
# Five testsets covering the inner-axis (per-value_name) version routing plus the pixel
# pipeline that reads through it:
#  - `API: resolve_image_version — inner version axis` (VN P1c)
#  - `API: store compression (what a version is encoded with)`
#  - `API: image render composite`
#  - `API: sRGB encode matches the browser viewer canvas gamma`
#  - `API: zarr byte order`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── Inner-axis (per-value_name) version routing ──────────────────────────────
# P1c makes `resolve_image_version` version-aware without changing legacy behaviour: a bare-scalar
# filepath (today's shape) must unwrap to itself even when a `version` kwarg is passed, and a
# versioned-entry filepath (P2 forward) must default to the entry's `_latest` and honour an explicit
# `version = "vN"`. Struct field types are not widened in P1c — this testset constructs the
# versioned shape on disk directly (a Dict at the value_name key) which is what the reader will see
# once P2's writers land. See docs/todo/VN_VERSIONING_PLAN.md → P1c.
@testset "API: resolve_image_version — inner version axis" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        puid = "TESTVER"
        # Two images: one legacy (bare scalar), one versioned (Dict with `_latest`).
        iuid_legacy = "IMGLEG"; iuid_ver = "IMGVER"
        for iuid in (iuid_legacy, iuid_ver); mkpath(joinpath(tmp, puid, "1", iuid)); end
        write(joinpath(tmp, puid, "project.json"),
              JSON3.write((; uid = puid, name = "T", set_uids = String[])))

        # Legacy: `filepath[default]` is a bare scalar. Two versions of the store on disk, but the
        # scalar addresses only one — the `version` kwarg must have no effect.
        mkpath(joinpath(tmp, puid, "0", iuid_legacy, "legacy.ome.zarr"))
        write(state_file(joinpath(tmp, puid), iuid_legacy), JSON3.write(Dict{String,Any}(
            "class"    => "CciaImage",
            "filepath" => Dict{String,Any}("default" => "legacy.ome.zarr",
                                           "_active" => "default"))))

        # Versioned: the value_name entry is a Dict with `_latest` = "v2". Both stores exist.
        mkpath(joinpath(tmp, puid, "0", iuid_ver, "img_v1.ome.zarr"))
        mkpath(joinpath(tmp, puid, "0", iuid_ver, "img_v2.ome.zarr"))
        write(state_file(joinpath(tmp, puid), iuid_ver), JSON3.write(Dict{String,Any}(
            "class"    => "CciaImage",
            "filepath" => Dict{String,Any}(
                "default" => Dict{String,Any}("v1" => "img_v1.ome.zarr",
                                              "v2" => "img_v2.ome.zarr",
                                              "_latest" => "v2"),
                "_active" => "default"))))

        # Legacy: `version` is a no-op — every value routes to the one scalar.
        for ver in (nothing, "v1", "v99")
            zp, _, err = resolve_image_version(puid, iuid_legacy, "default"; version = ver)
            @test err === nothing
            @test zp == joinpath(tmp, puid, "0", iuid_legacy, "legacy.ome.zarr")
        end

        # Versioned: no `version` kwarg → walks `_latest` to `v2`.
        zp, _, err = resolve_image_version(puid, iuid_ver, "default")
        @test err === nothing
        @test zp == joinpath(tmp, puid, "0", iuid_ver, "img_v2.ome.zarr")

        # Explicit `version = "v1"` addresses the older store.
        zp, _, err = resolve_image_version(puid, iuid_ver, "default"; version = "v1")
        @test err === nothing
        @test zp == joinpath(tmp, puid, "0", iuid_ver, "img_v1.ome.zarr")

        # A version that doesn't exist reports a specific error instead of a generic miss.
        _, _, err = resolve_image_version(puid, iuid_ver, "default"; version = "v99")
        @test err !== nothing
        @test occursin("version", err)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: store compression (what a version is encoded with)" begin
    # The label a version shows in the metadata modal must be the SAME name Settings uses, or the two
    # surfaces describe one codec two ways.
    known = Dict(c.label => c for c in Cecelia.IMAGE_COMPRESSOR_CHOICES)

    d = _describe_compressor(Dict(:id => "blosc", :cname => "zstd", :clevel => 3, :shuffle => 1))
    @test haskey(known, d.label)                       # it resolved to a Settings choice, not a guess
    @test d.codec == "zstd" && d.level == 3 && d.shuffle

    # zstd level 0 IS the library default (3) — a store written at 0 must not read as a DIFFERENT
    # setting from one written at 3, or every store predating the explicit choice looks non-canonical.
    z0 = _describe_compressor(Dict(:id => "zstd", :level => 0))
    z3 = _describe_compressor(Dict(:id => "zstd", :level => 3))
    @test z0.level == 3 && z0.label == z3.label

    # The three codecs actually on disk today all happen to BE selectable choices, so they resolve to
    # Settings labels — verified against real stores (bioformats2raw/zarr-2 `lz4 + shuffle`, zarr-3
    # `zstd`, and the canonical `zstd + shuffle`).
    @test haskey(known, _describe_compressor(
        Dict(:id => "blosc", :cname => "lz4", :clevel => 5, :shuffle => 1)).label)

    # Something genuinely OUTSIDE the table must get an honest descriptive label rather than being
    # silently mapped onto the nearest option — a wrong name here would misreport what is on disk.
    for spec in (Dict(:id => "blosc", :cname => "lz4", :clevel => 9, :shuffle => 1),   # same codec, other level
                 Dict(:id => "zlib", :level => 6),                                     # not offered at all
                 Dict(:id => "blosc", :cname => "zstd", :clevel => 3, :shuffle => 0))  # blosc-wrapped, unshuffled
        d2 = _describe_compressor(spec)
        @test !haskey(known, d2.label)
        @test occursin(string(d2.level), d2.label)      # the label carries the level it found
    end

    # `compressor: null` is a real value — an uncompressed store, not an error
    @test _describe_compressor(nothing).label == "none"
    @test _describe_compressor("garbage") === nothing

    # store_compression: BOTH layouts, detected structurally (a flat store's level 0 is `0`, a
    # bioformats2raw series' is `0/0`, and both have a `0/` child so the path says nothing)
    mktempdir() do dir
        flat = joinpath(dir, "flat.ome.zarr"); mkpath(joinpath(flat, "0"))
        write(joinpath(flat, "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"zstd","clevel":3,"shuffle":1}}""")
        @test store_compression(flat).codec == "zstd"

        series = joinpath(dir, "series.ome.zarr"); mkpath(joinpath(series, "0", "0"))
        write(joinpath(series, "0", "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"lz4","clevel":5,"shuffle":1}}""")
        @test store_compression(series).codec == "lz4"

        # unreadable / absent / malformed → nothing, never a throw: this is display-only and the
        # caller is listing every version of an image
        @test store_compression(joinpath(dir, "nope.ome.zarr")) === nothing
        bad = joinpath(dir, "bad.ome.zarr"); mkpath(joinpath(bad, "0"))
        write(joinpath(bad, "0", ".zarray"), "{not json")
        @test store_compression(bad) === nothing
    end
end

@testset "API: image render composite" begin
    # Pure colourise/blend for the server-side preview render (image_render.jl) — no zarr/IO. (C,H,W) float +
    # per-channel (lo,hi,cmap,visible) → H×W RGB, clip-to-contrast + additive blend.
    r(x) = Float64(ColorTypes.red(x)); g(x) = Float64(ColorTypes.green(x)); b(x) = Float64(ColorTypes.blue(x))

    # one red channel, mid intensity, full-range contrast → mid red, no green/blue
    img = composite_rgb(fill(0.5f0, 1, 2, 2), [(0.0, 1.0, "red", true)])
    @test size(img) == (2, 2)
    @test isapprox(r(img[1, 1]), 0.5; atol = 0.01) && g(img[1, 1]) == 0 && b(img[1, 1]) == 0

    # contrast clip: value below lo → 0, above hi → 1
    chw = reshape(Float32[0.0 1.0; 0.2 0.8], 1, 2, 2)
    im2 = composite_rgb(chw, [(0.2, 0.8, "green", true)])
    @test isapprox(g(im2[1, 1]), 0.0; atol = 0.01)     # 0.0 < lo → 0
    @test isapprox(g(im2[1, 2]), 1.0; atol = 0.01)     # 1.0 > hi → 1

    # invisible channel contributes nothing
    dark = composite_rgb(fill(1.0f0, 1, 1, 1), [(0.0, 1.0, "red", false)])
    @test r(dark[1, 1]) == 0

    # additive blend: red + green channels → yellow-ish
    two = composite_rgb(cat(fill(1.0f0, 1, 1, 1), fill(1.0f0, 1, 1, 1); dims = 1),
                              [(0.0, 1.0, "red", true), (0.0, 1.0, "green", true)])
    @test r(two[1, 1]) > 0.9 && g(two[1, 1]) > 0.9 && b(two[1, 1]) == 0

    # ── Channel colour: napari's palette must NOT be guessed by name ──────────────────
    # `bop blue` was missing from CMAP_RGB and hit the unknown-name fallback (WHITE), which additively
    # washes the whole composite out — the SHG channel of every intravital image rendered white instead
    # of blue. Assert napari's own end colour (AVAILABLE_COLORMAPS["bop blue"].colors[-1]).
    bop = composite_rgb(fill(1.0f0, 1, 1, 1), [(0.0, 1.0, "bop blue", true)])
    @test isapprox(r(bop[1, 1]), 0.12549; atol = 0.01)
    @test isapprox(g(bop[1, 1]), 0.678431; atol = 0.01)
    @test isapprox(b(bop[1, 1]), 0.972549; atol = 0.01)
    @test b(bop[1, 1]) - r(bop[1, 1]) > 0.5            # unmistakably blue, not white

    # An explicit LUT (props `colormap_lut`) wins over any name table and is interpolated. A 2-stop
    # black→base ramp must reduce EXACTLY to `n .* base`, which is what additive primaries need.
    lut2 = [(0f0, 0f0, 0f0), (1f0, 0f0, 0f0)]
    half = composite_rgb(fill(0.5f0, 1, 1, 1), [(0.0, 1.0, lut2, true)])
    @test isapprox(r(half[1, 1]), 0.5; atol = 0.01) && g(half[1, 1]) == 0

    # 3-stop LUT: midpoint intensity lands on the middle stop, quarter interpolates into it
    lut3 = [(0f0, 0f0, 0f0), (0f0, 1f0, 0f0), (0f0, 0f0, 1f0)]
    mid = composite_rgb(fill(0.5f0, 1, 1, 1), [(0.0, 1.0, lut3, true)])
    @test isapprox(g(mid[1, 1]), 1.0; atol = 0.01) && isapprox(b(mid[1, 1]), 0.0; atol = 0.01)
    qtr = composite_rgb(fill(0.25f0, 1, 1, 1), [(0.0, 1.0, lut3, true)])
    @test isapprox(g(qtr[1, 1]), 0.5; atol = 0.01)
    # a white→colour LUT (napari's `I *` set) is honoured at zero intensity — no name table could do this
    inv = composite_rgb(fill(0.0f0, 1, 1, 1), [(0.0, 1.0, [(1f0, 1f0, 1f0), (0f0, 0f0, 1f0)], true)])
    @test r(inv[1, 1]) > 0.9 && g(inv[1, 1]) > 0.9

    # layer_display_specs prefers the saved LUT over the colormap NAME (same entry carries both)
    mktempdir() do d
        p = joinpath(d, "props.json")
        write(p, JSON3.write((; Image = [
            (; contrast_limits = [0.0, 10.0], colormap = "bop blue",
               colormap_lut = [[0.0, 0.0, 0.0], [1.0, 0.0, 0.0]], visible = true),
            (; contrast_limits = [1.0, 5.0], colormap = "magenta", visible = false),
        ])))
        specs = layer_display_specs(p)
        @test length(specs) == 2
        @test specs[1][3] isa AbstractVector && specs[1][3][2] == (1f0, 0f0, 0f0)  # LUT, not the name
        @test specs[2][3] == "magenta" && specs[2][4] == false                     # no LUT → name kept
        @test specs[2][1] == 1.0 && specs[2][2] == 5.0
    end
    @test layer_display_specs(joinpath(mktempdir(), "absent.json")) === nothing

    # Browser-viewer autosave shape (P5+) — the smoothed sidecar for zolIMa/fXgbTl has NO `Image`
    # array; only `layers` (name-keyed) + `webgpu.channels` (index-ordered). Before this fell back
    # to sampled contrast + `DEFAULT_CMAPS`, so the movie rendered red/green/blue/yellow instead of
    # the viewer's palette. Reader walks `webgpu.channels` in index order.
    mktempdir() do d
        p = joinpath(d, "props.json")
        write(p, JSON3.write((;
            webgpu = (; channels = [
                (; visible = false, hex = "#0000ff", lo = 0, hi =  29),   # SHG   (hidden)
                (; visible = true,  hex = "#00ffff", lo = 0, hi = 162),   # nuc-GFP (cyan)
                (; visible = true,  hex = "#ff00ff", lo = 0, hi = 310),   # mem-TOM (magenta)
                (; visible = true,  hex = "#ffff00", lo = 0, hi = 194),   # CD169-Kat (yellow)
            ]),
            layers = Dict("SHG" => (; visible = false)),                  # ignored: name-keyed
        )))
        specs = layer_display_specs(p)
        @test length(specs) == 4
        @test specs[1] == (0.0,  29.0, "#0000ff", false)
        @test specs[2] == (0.0, 162.0, "#00ffff", true)
        @test specs[3] == (0.0, 310.0, "#ff00ff", true)
        @test specs[4] == (0.0, 194.0, "#ffff00", true)
    end

    # A file with NEITHER shape returns nothing (fall through to sampled-contrast fallback).
    mktempdir() do d
        p = joinpath(d, "props.json")
        write(p, JSON3.write((; other = "unrelated")))
        @test layer_display_specs(p) === nothing
    end
end

@testset "API: sRGB encode matches the browser viewer's canvas gamma" begin
    # The offline movie renderer and the browser volume viewer both need to hand the display /
    # codec pixels in sRGB — otherwise the movie reads ~2× dimmer than the viewer at the same
    # specs (audit 2026-08-29). Pin the transfer function's known values + the whole pipeline's
    # mid-tone lift; a regression that silently reverts to linear output would drop these back.
    r = p -> Float64(red(p)); g = p -> Float64(green(p)); b = p -> Float64(blue(p))

    # `_linear_to_srgb` on canonical points (IEC 61966-2-1 piecewise curve).
    @test _linear_to_srgb(0f0) == 0f0
    @test isapprox(_linear_to_srgb(1f0), 1f0; atol = 1e-6)          # Float32 arithmetic loses 1 ulp
    # Mid-tone: 0.5 linear → ~0.735 sRGB. This is the entire visual difference between the linear
    # movie and the viewer, so keep the tolerance tight.
    @test isapprox(_linear_to_srgb(0.5f0), 0.7353569f0; atol = 5e-4)
    # Piecewise segment near zero (linear × 12.92), below the 0.0031308 threshold.
    @test isapprox(_linear_to_srgb(0.001f0), 0.001f0 * 12.92f0; atol = 1e-6)

    # `srgb_encode` on an already-composited frame — mid-grey lifts, white stays white.
    lut_r = [(0f0, 0f0, 0f0), (1f0, 0f0, 0f0)]
    half  = composite_rgb(fill(0.5f0, 1, 1, 1), [(0.0, 1.0, lut_r, true)])
    @test isapprox(r(half[1, 1]), 0.5; atol = 0.01)                # linear composite still 0.5
    lifted = srgb_encode(half)
    @test isapprox(r(lifted[1, 1]), 0.7353; atol = 0.01)           # sRGB-encoded, mid-tones up
    @test g(lifted[1, 1]) == 0.0 && b(lifted[1, 1]) == 0.0         # zero channels stay zero
    white = composite_rgb(fill(1.0f0, 1, 1, 1), [(0.0, 1.0, [(0f0,0f0,0f0),(1f0,1f0,1f0)], true)])
    @test all(c -> isapprox(c(srgb_encode(white)[1, 1]), 1.0; atol = 1e-3), (r, g, b))

    # `render_view_frame` bakes the encode in — the same specs the viewer draws with produce a
    # frame whose mid-tone channel reads ABOVE the linear composite. This is what closes the
    # visual gap between the movie and the on-screen canvas.
    mktempdir() do dir
        # A 1-t, 1-c, 1-z, 4-y, 4-x store filled at mid intensity; contrast 0..1 → composite 0.5.
        arr = fill(Float32(0.5), 1, 1, 1, 4, 4)
        caxes = ["t", "c", "z", "y", "x"]
        frame = render_view_frame(arr, caxes, 0; z = 0, channels = 0:0,
                                    specs = [(lo = 0.0, hi = 1.0,
                                                lut = [(0f0,0f0,0f0),(1f0,0f0,0f0)],
                                                visible = true)])
        @test isapprox(r(frame[1, 1]), 0.7353; atol = 0.01)         # sRGB, not 0.5
    end
end

# Minimal stand-in for a Zarr array: `read_native` only ever asks it for `arr[idx...]` and for
# `arr.metadata.dtype`, which is exactly enough to assert the no-copy path by object identity.
struct FakeZMeta; dtype::String; end
struct FakeZArray; block::Vector{UInt16}; metadata::FakeZMeta; end
FakeZArray(b, dt) = FakeZArray(b, FakeZMeta(dt))
Base.getindex(a::FakeZArray, ::Colon) = a.block

@testset "API: zarr byte order" begin
    # `read_native` must apply the STORED byte order. bioformats2raw writes big-endian (`>u2`) and
    # Zarr.jl parses that for the eltype but hands back the bytes UNSWAPPED — so a raw `default` image
    # version read with plain `arr[...]` is byte-swapped garbage that renders as saturated white noise
    # (a true 63 reads as 16128; 98% of a real frame exceeded a contrast ceiling that should clip none).
    # Silent, and invisible in Python, which honours the descriptor.
    # DETECTOR for the single Zarr.jl internal this depends on. `_zarr_byte_order` reads the raw numpy
    # dtype descriptor out of `arr.metadata.dtype`; if a Zarr.jl upgrade changes that field's shape the
    # guard falls back to '|' (never swap) and the big-endian bug returns. The swap assertions below DO
    # catch that — verified by mutating the guard to return '|', which fails 3 of them — but they fail as
    # an opaque UInt16 value mismatch. This one names the cause instead.
    mktempdir() do d
        a = zcreate(UInt16, Zarr.DirectoryStore(joinpath(d, "probe")), 2; chunks = (2,))
        dt = getfield(a.metadata, :dtype)
        @test dt isa AbstractString          # ← if THIS fails, read_native's byte-order guard is blind
        @test occursin(r"^[<>|]", String(dt))
    end

    # Re-stamp the dtype descriptor the way bioformats2raw would; Zarr.jl keeps it as the raw string.
    function stamp_order!(p, order)
        za = JSON3.read(read(joinpath(p, ".zarray"), String), Dict{String,Any})
        za["dtype"] = order
        write(joinpath(p, ".zarray"), JSON3.write(za))
    end
    vals = UInt16[0x0000, 0x003f, 0x00ff, 0x2800, 0xffff]      # 0, 63, 255, 10240, 65535
    mktempdir() do d
        for (i, (order, swaps)) in enumerate((">u2" => true, "<u2" => false))
            p = joinpath(d, "store$(i)")
            a = zcreate(UInt16, Zarr.DirectoryStore(p), length(vals); chunks = (length(vals),))
            a[:] = vals
            stamp_order!(p, order)
            got = read_native(zopen(p, "r"), :)
            @test got == (swaps ? ntoh.(vals) : vals)
            if order == ">u2"                  # the bug this pins: BE must NOT read as the raw bytes
                @test got != vals
                @test got[2] == 0x3f00         # 63 declared big-endian reads as 16128 if left unswapped
            end
        end
        # `|u1` (not-applicable, 1-byte) is passed through untouched — swapping a byte is a no-op, but
        # the descriptor must not be misread as an order either.
        # A matching order must return the block ITSELF, not a copy of it. `ltoh`/`ntoh` are no-ops
        # element-wise, so the old guard was correct — and it still broadcast over the whole block to
        # compute nothing, +65% on an 81.5 MB channel read (68 -> 112 ms) for every little-endian store,
        # which is every corrected/cropped version the writers produce. Identity is the only way to
        # assert "no copy" without measuring, so this uses a stand-in whose getindex returns a KNOWN
        # object; on a big-endian host the same test holds with the orders swapped.
        let block = UInt16[1, 2, 3]
            same_order = HOST_IS_LITTLE_ENDIAN ? "<u2" : ">u2"
            other_order = HOST_IS_LITTLE_ENDIAN ? ">u2" : "<u2"
            @test read_native(FakeZArray(block, same_order), :) === block    # ← the no-copy contract
            @test read_native(FakeZArray(block, other_order), :) !== block   # a real swap still copies
            @test read_native(FakeZArray(block, "|u1"), :) === block
        end

        p8 = joinpath(d, "store8")
        b = UInt8[0x00, 0x3f, 0xff]
        a8 = zcreate(UInt8, Zarr.DirectoryStore(p8), length(b); chunks = (length(b),))
        a8[:] = b
        stamp_order!(p8, "|u1")
        @test read_native(zopen(p8, "r"), :) == b
    end
end

