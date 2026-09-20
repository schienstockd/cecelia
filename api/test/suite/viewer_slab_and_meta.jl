# Viewer slab + viewer meta (spatial audit LOD + labelDims) testsets — extracted from
# api/test/runtests.jl.
#
# Four testsets covering the WebGPU-facing viewer endpoints:
#  - `API: viewer slab (voxels the GPU can upload without a transform)` — read_slab feeds
#    a WebGPU 3D texture directly; response body is copied to VRAM with no reshape.
#  - `API: viewer slab — XY tile + pyramid level (spatial audit Phase 2)`.
#  - `API: viewer meta — per-level shapes (spatial audit LOD)`.
#  - `API: viewer meta — labelDims lets the picker flag masks that dont fit`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: viewer slab (voxels the GPU can upload without a transform)" begin
    # `read_slab` feeds a WebGPU 3D texture directly: the response body is copied to VRAM with no
    # reshape, so its linear order MUST be x-fastest, then y, then z. Every assertion here is about a
    # failure that is SILENT — a transposed or byte-swapped volume renders a plausible-looking image of
    # the wrong thing, and neither the route nor the browser can tell. See docs/todo/WEB_VIEWER_PLAN.md.
    #
    # The pattern is `x + 10y + 100z + 1000c + 10000t`, so every axis has its own decimal digit and any
    # swap between two of them is visible in a single voxel.
    val(x, y, z, c, t) = UInt16(x + 10y + 100z + 1000c + 10000t)
    axes_attr(names) = Dict("multiscales" => [Dict("axes" => [Dict("name" => n) for n in names])])

    # Store declared (t,c,z,y,x) in C-order → Zarr.jl presents it REVERSED, so Julia dims are x,y,z,c,t.
    function make_store(dir, c_axes, jdims, fill!)
        g = zgroup(Zarr.DirectoryStore(dir); attrs = axes_attr(c_axes))
        a = zcreate(UInt16, g, "0", jdims...; chunks = jdims)
        buf = zeros(UInt16, jdims...)
        fill!(buf)
        a[fill(Colon(), length(jdims))...] = buf
        dir
    end

    nt, nc, nz, ny, nx = 2, 2, 3, 4, 5
    mktempdir() do d
        p = make_store(joinpath(d, "std.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)

        vol, sx, sy, sz = read_slab(p, 0, 1)          # t=0, c=1 (both 0-based)
        @test (sx, sy, sz) == (nx, ny, nz)
        @test vol[1, 1, 1] == val(1, 1, 1, 2, 1)
        @test vol[nx, ny, nz] == val(nx, ny, nz, 2, 1)
        # x and y are DIFFERENT lengths here on purpose — a square frame hides a transpose.
        @test vol[2, 3, 1] == val(2, 3, 1, 2, 1)

        # THE upload contract: the first `nx` elements of the flat body must walk x, not y or z.
        @test vec(vol)[1:nx] == [val(x, 1, 1, 2, 1) for x in 1:nx]
        @test vec(vol)[nx + 1] == val(1, 2, 1, 2, 1)              # then y
        @test vec(vol)[nx * ny + 1] == val(1, 1, 2, 2, 1)         # then z

        # …and the wire bytes are little-endian pairs of exactly that, nothing padded or reordered.
        bytes = slab_bytes(vol)
        @test length(bytes) == nx * ny * nz * 2
        @test bytes[1] == UInt8(val(1, 1, 1, 2, 1) % 256)
        @test bytes[2] == UInt8(val(1, 1, 1, 2, 1) ÷ 256)

        # t is honoured (0-based → 1-based) rather than silently always frame 1
        @test read_slab(p, 1, 0)[1][1, 1, 1] == val(1, 1, 1, 1, 2)

        # ── One z plane (the 2D view) ───────────────────────────────────────────────────
        # This is the view a timecourse is actually watched in, and the ONLY one that plays: on Dml3RG a
        # plane timepoint is 8.8 MB against 326 MB, so the whole 181-frame movie is 1.59 GB and fits in
        # VRAM. It shares `read_slab` with the volume deliberately — a scalar z drops the dim exactly as
        # t and c do — so there is no second reader to disagree about axis order.
        pv, px, py, pz = read_slab(p, 0, 1; z = 2)      # 0-based z → the THIRD plane
        @test (px, py, pz) == (nx, ny, 1)               # reports depth 1, not the stack's depth
        @test ndims(pv) == 2
        @test pv[2, 3] == val(2, 3, 3, 2, 1)            # ← the requested plane, not plane 1
        @test vec(pv)[1:nx] == [val(x, 1, 3, 2, 1) for x in 1:nx]   # still x-fastest on the wire
        # z=0 is a real plane, not "no plane" — an absent z means the whole stack and the two must not
        # collapse into each other.
        @test read_slab(p, 0, 0; z = 0)[1][2, 3] == val(2, 3, 1, 1, 1)
        @test size(read_slab(p, 0, 0)[1], 3) == nz      # z omitted → the whole stack
        # Out of range is clamped to a real plane rather than throwing: the slider's bound and the
        # store's depth can disagree for a moment after a version switch.
        @test read_slab(p, 0, 0; z = 999)[1][2, 3] == val(2, 3, nz, 1, 1)
        @test read_slab(p, 0, 0; z = -5)[1][2, 3] == val(2, 3, 1, 1, 1)

        # ── A RANGE of planes (the usable 3D view) ─────────────────────────────────────
        # Every cost here is linear in the plane count, so a few planes out of a deep stack is what
        # makes the volume view interactive: 8 of 37 is 70 MB rather than 326 MB. A range KEEPS the z
        # dim where a scalar drops it — that difference in rank is the whole contract, because the
        # client sizes its texture from it.
        rv, rx, ry, rz = read_slab(p, 0, 1; z = 1:2)    # 0-based → planes 2 and 3
        @test (rx, ry, rz) == (nx, ny, 2)
        @test ndims(rv) == 3
        @test rv[2, 3, 1] == val(2, 3, 2, 2, 1)         # ← starts at the range's low end
        @test rv[2, 3, 2] == val(2, 3, 3, 2, 1)
        @test vec(rv)[1:nx] == [val(x, 1, 2, 2, 1) for x in 1:nx]   # still x-fastest on the wire
        # A one-plane RANGE is not a scalar: same bytes, different rank, and the client's shape guard
        # rejects a slab whose depth disagrees with what it allocated.
        @test ndims(read_slab(p, 0, 1; z = 2:2)[1]) == 3
        @test read_slab(p, 0, 1; z = 2:2)[4] == 1
        @test read_slab(p, 0, 1; z = 2:2)[1][2, 3, 1] == val(2, 3, 3, 2, 1)
        # Clamped at both ends, and a reversed range is read the way round it was meant — these come
        # off a query string, where an out-of-range index is a 500 from inside Zarr.jl.
        @test read_slab(p, 0, 0; z = 0:999)[4] == nz
        @test read_slab(p, 0, 0; z = -5:1)[4] == 2
        # A backwards pair cannot even reach here as a range — `2:0` is normalised to the EMPTY `2:1` by
        # UnitRange's own constructor, which is why the route orders the two integers before building
        # one. An empty range that does arrive reads as the single plane at its start, never as zero
        # planes: a zero-thickness slab renders black (entry and exit distances coincide).
        @test read_slab(p, 0, 0; z = 2:0)[4] == 1
        @test read_slab(p, 0, 0; z = 2:0)[1][2, 3, 1] == val(2, 3, 3, 1, 1)
        @test ndims(read_slab(p, 0, 0; z = 2:0)[1]) == 3     # still a volume, not a plane

        # ── A RANGE of CHANNELS (brick atlas: all channels of one brick in ONE request) ────
        # The brick-atlas 3D viewer wants every channel of a spatial brick in a single response —
        # KILN_BRICK_PLAN.md → Decision 7. Serially at nC=38 measured 273 ms/brick, or ~2.5 s for a
        # 3x3 visible viewport; batched into one request drops to a single round trip. A range KEEPS
        # the c dim as the last axis, so `(x, y, z, c)` — the atlas can upload it as
        # `nc groups of nz consecutive planes` without a copy (KILN_BRICK_PLAN.md → Decision 4).
        rc, cx, cy, cz, cn = read_slab(p, 0, 0:1)
        @test (cx, cy, cz, cn) == (nx, ny, nz, 2)
        @test ndims(rc) == 4
        @test rc[2, 3, 1, 1] == val(2, 3, 1, 1, 1)          # first channel of the range
        @test rc[2, 3, 1, 2] == val(2, 3, 1, 2, 1)          # second channel of the range
        # Wire order stays x-fastest — the atlas uploads the buffer directly, no reshape.
        @test vec(rc)[1:nx] == [val(x, 1, 1, 1, 1) for x in 1:nx]
        # Length-1 RANGE keeps the c dim (rank 4); scalar-c drops it (rank 3). Same contract as z.
        @test read_slab(p, 0, 0:0)[5] == 1
        @test ndims(read_slab(p, 0, 0:0)[1]) == 4           # RANGE at length 1 keeps the dim
        @test ndims(read_slab(p, 0, 0)[1]) == 3             # scalar-c drops it — flat atlas path unchanged
        # Clamped both ends, same shape as z-range. Out-of-range c gets the closest existing channel.
        @test read_slab(p, 0, 0:999)[5] == nc
        @test read_slab(p, 0, -5:1)[5] == 2
        # A backwards pair reads as the single channel at its start (Julia normalises `1:0` to empty;
        # the route orders integers before building the range).
        @test read_slab(p, 0, 2:0)[5] == 1
        # A c-range combined with a z-range keeps both dims: (x, y, z, c), for a 4D brick payload.
        rcz, _, _, rz, rn = read_slab(p, 0, 0:1; z = 1:2)
        @test (rz, rn) == (2, 2)
        @test ndims(rcz) == 4
        @test rcz[2, 3, 1, 1] == val(2, 3, 2, 1, 1)         # first z, first c of the request
        @test rcz[2, 3, 2, 2] == val(2, 3, 3, 2, 1)         # last z, last c
    end

    # A store whose axes are NOT (t,c,z,y,x) must be PERMUTED to (x,y,z), not passed through. This is
    # the whole reason the permute is written out instead of relying on the usual layout: pass-through
    # would put z where x belongs and still render.
    mktempdir() do d
        # C-order (t,c,y,x,z) → Julia dims are z,x,y,c,t
        p = make_store(joinpath(d, "odd.ome.zarr"), ["t", "c", "y", "x", "z"],
                       (nz, nx, ny, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, y in 1:ny, x in 1:nx, z in 1:nz
                           b[z, x, y, c, t] = val(x, y, z, c, t)
                       end)
        vol, sx, sy, sz = read_slab(p, 0, 0)
        @test (sx, sy, sz) == (nx, ny, nz)             # reported as x,y,z whatever the store's order
        @test vol[2, 3, 1] == val(2, 3, 1, 1, 1)       # …and the voxels actually moved
        @test vec(vol)[1:nx] == [val(x, 1, 1, 1, 1) for x in 1:nx]
    end

    # Degenerate ranks: a 2D still and a single-channel stack answer the same shape of question, with
    # the missing axes counting as 1 — not an error, and not a silently dropped dimension.
    mktempdir() do d
        p2 = make_store(joinpath(d, "flat2d.ome.zarr"), ["y", "x"], (nx, ny),
                        b -> for y in 1:ny, x in 1:nx; b[x, y] = val(x, y, 1, 1, 1) end)
        vol, sx, sy, sz = read_slab(p2, 0, 0)
        @test (sx, sy, sz) == (nx, ny, 1)
        @test vol[2, 3] == val(2, 3, 1, 1, 1)
        # an image with no z axis: asking for a plane is a no-op, not an error
        @test read_slab(p2, 0, 0; z = 3)[1][2, 3] == val(2, 3, 1, 1, 1)

        p3 = make_store(joinpath(d, "zyx.ome.zarr"), ["z", "y", "x"], (nx, ny, nz),
                        b -> for z in 1:nz, y in 1:ny, x in 1:nx; b[x, y, z] = val(x, y, z, 1, 1) end)
        @test read_slab(p3, 0, 0)[2:4] == (nx, ny, nz)
    end

    # Big-endian: a raw bioformats2raw store is `>u2` and Zarr.jl hands the bytes back UNSWAPPED, so a
    # slab read with plain `arr[...]` is garbage that renders as saturated noise. `read_slab` must go
    # through `read_native`. Same stamp trick as the byte-order testset above.
    mktempdir() do d
        p = make_store(joinpath(d, "be.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)
        za = JSON3.read(read(joinpath(p, "0", ".zarray"), String), Dict{String,Any})
        za["dtype"] = ">u2"
        write(joinpath(p, "0", ".zarray"), JSON3.write(za))
        got = read_slab(p, 0, 0)[1]
        @test got[1, 1, 1] == ntoh(val(1, 1, 1, 1, 1))
        @test got[1, 1, 1] != val(1, 1, 1, 1, 1)       # ← fails if read_native was bypassed
    end

    # Cold-start contrast: one spec per channel, sampled from a FIXED (t, z) so playback cannot flicker
    # as the window chases each frame's own distribution (WEB_VIEWER_PLAN.md decision 5).
    mktempdir() do d
        p = make_store(joinpath(d, "c.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)
        specs = _sampled_specs(p, nc)
        @test length(specs) == nc
        @test all(s -> s[2] >= s[1], specs)
        @test _sampled_specs(p, nc) == specs           # same (t, z) every time, so stable
    end

    # `resolved_display_specs` is the ONE place a colormap name becomes RGB — the browser must not
    # re-derive napari's palette (a name table missing `bop blue` rendered a channel WHITE).
    mktempdir() do d
        pj = joinpath(d, "props.json")
        write(pj, JSON3.write((; Image = [
            (; contrast_limits = [0.0, 10.0], colormap = "bop blue", visible = true),
            (; contrast_limits = [1.0, 5.0], colormap = "green", visible = false),
        ])))
        r = resolved_display_specs(pj, 2)
        @test length(r) == 2
        @test r[1].lo == 0.0 && r[1].hi == 10.0 && r[1].visible
        @test r[1].lut[end] == (0.12549f0, 0.678431f0, 0.972549f0)   # resolved, not the string
        @test r[2].lut[end] == (0f0, 1f0, 0f0) && r[2].visible == false
        # Props describing FEWER channels than the store has → `nothing`, so the route falls back to
        # sampling instead of indexing off the end or shifting every channel's colour by one.
        @test resolved_display_specs(pj, 3) === nothing
        @test resolved_display_specs(joinpath(d, "absent.json"), 1) === nothing
    end
end

@testset "API: viewer slab — XY tile + pyramid level (spatial audit Phase 2)" begin
    # The pan/zoom viewer's access pattern: a rectangular TILE at a chosen pyramid LEVEL, out of a store
    # too big to slab whole. L0 of `f8gzA2` is 20329×16898 and 687 MB per channel; one 1024² chunk is
    # 2 MB. Same shape guard as the timecourse — the response body copies into a WebGPU 2D texture with
    # no transform, so a silent transpose or a level mismatch would render plausible-looking garbage.
    val(x, y, z, c, t) = UInt16(x + 10y + 100z + 1000c + 10000t)
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])

    # A multi-level FLAT store: `.zattrs` at root lists every level, arrays live at `"0"`, `"1"`, …
    # bioformats2raw's series layout wraps them in `0/` and open_level0 already tested that half — this
    # covers the level-index path, not the layout discrimination it inherits.
    function make_pyramid(dir, c_axes, jdims_per_level, filler)
        g = zgroup(Zarr.DirectoryStore(dir); attrs = axes_ms(c_axes, length(jdims_per_level)))
        for (i, jdims) in enumerate(jdims_per_level)
            a = zcreate(UInt16, g, string(i - 1), jdims...; chunks = jdims)
            buf = zeros(UInt16, jdims...)
            filler(buf, i - 1)
            a[fill(Colon(), length(jdims))...] = buf
        end
        dir
    end

    # 5D flat store, tiny — just enough to prove the axis walking is real. L0 is 8×6 XY; L1 is 4×3.
    nt, nc, nz = 1, 2, 2
    l0_nx, l0_ny = 8, 6
    l1_nx, l1_ny = 4, 3

    mktempdir() do d
        # Stamp values fit UInt16: val() maxes at ~12k for this shape, +30000 per level stays under
        # 65535 while making L0 and L1 obviously different (a bad open_level that returned L0's array
        # would fail these assertions loudly rather than passing on matching voxels).
        lvl_stamp = UInt16(30000)
        p = make_pyramid(joinpath(d, "pyr.ome.zarr"),
                         ["t", "c", "z", "y", "x"],
                         [(l0_nx, l0_ny, nz, nc, nt), (l1_nx, l1_ny, nz, nc, nt)],
                         (buf, lvl) -> for t in 1:size(buf, 5), c in 1:size(buf, 4),
                                          z in 1:size(buf, 3), y in 1:size(buf, 2), x in 1:size(buf, 1)
                             buf[x, y, z, c, t] = UInt16(val(x, y, z, c, t) + lvl_stamp * lvl)
                         end)

        # ── XY range: reads a tile, keeps the axes, clamps to the store ──────────────
        # x=2:4, y=1:2 are 0-BASED — store's 1-based coords are (x=3:5, y=2:3). The stamp uses store
        # coordinates, so the assertions read the returned voxel back in the store's own frame.
        vol, sx, sy, sz = read_slab(p, 0, 0; x = 2:4, y = 1:2)
        @test (sx, sy, sz) == (3, 2, nz)                # 3 wide, 2 tall, full stack
        @test ndims(vol) == 3
        @test vol[1, 1, 1] == val(3, 2, 1, 1, 1)        # tile origin in store coords
        @test vol[3, 2, 1] == val(5, 3, 1, 1, 1)        # tile's far corner
        # x-fastest on the wire even for a subset — the shape guard doesn't check the memory order
        @test vec(vol)[1:sx] == [val(x, 2, 1, 1, 1) for x in 3:5]

        # A whole-plane subset agrees with the untiled read — no accidental resample
        big = read_slab(p, 0, 0)[1]
        @test read_slab(p, 0, 0; x = 0:l0_nx-1, y = 0:l0_ny-1)[1] == big

        # Range CLAMPED to the store's edge, not a 500 from Zarr.jl — a viewport hanging off the frame
        # is a normal state, not a bad request. Same discipline `z` already has.
        clamped = read_slab(p, 0, 0; x = 6:99, y = 0:1)
        @test clamped[2:4] == (2, 2, nz)                # x clamped from 6:99 to 6:7 → 2 wide

        # A backwards pair (`x=5:3`) is what a swapped lo/hi arrives as after the route orders them, so
        # it should never REACH `read_slab` — but if it does, the empty range reads as the single column
        # at its start rather than as zero width. A zero-width tile has no pixels to display.
        @test read_slab(p, 0, 0; x = 5:3)[2] == 1
        @test read_slab(p, 0, 0; x = 5:3)[1][1, 1, 1] == val(6, 1, 1, 1, 1)

        # ── Pyramid level: a different array, not a resampled L0 ─────────────────────
        v0 = read_slab(p, 0, 0; level = 0)
        v1 = read_slab(p, 0, 0; level = 1)
        @test v0[2:4] == (l0_nx, l0_ny, nz)
        @test v1[2:4] == (l1_nx, l1_ny, nz)
        # The stamp said "+30000 per level", so a bad open_level would fail this loudly
        @test v1[1][1, 1, 1] == UInt16(val(1, 1, 1, 1, 1) + lvl_stamp)
        @test v0[1][1, 1, 1] == val(1, 1, 1, 1, 1)

        # XY range on a coarser level maps to that level's coordinates, not L0's — the client thinks in
        # level-space (that is the whole point of picking a level from meta's per-level shapes)
        tile = read_slab(p, 0, 0; level = 1, x = 0:1, y = 0:1)
        @test tile[2:4] == (2, 2, nz)
        @test tile[1][1, 1, 1] == UInt16(val(1, 1, 1, 1, 1) + lvl_stamp)

        # ── The (arr, caxes) form has NO `level`: a caller that pre-opened one array cannot ask a
        # different one, and the type system says so.
        a, ax = open_level(p, 1)
        v1b = read_slab(a, ax, 0, 0)
        @test v1b[2:4] == (l1_nx, l1_ny, nz)
        @test v1b[1] == v1[1]

        # ── HTTP round trip: `try_serve_slab` reads x/xTo/y/yTo/level off the query string ───
        qs(pairs...) = join(("$k=$v" for (k, v) in pairs), "&")
        # Reach the route the way the server does, but without spinning a real HTTP server: a
        # stream-handler takes a plain `HTTP.Stream`, and `IOStream_to_HTTP` is not part of the API — so
        # we just call `read_slab` through the same query-string parsing that `try_serve_slab` applies to
        # a request. The route header assertions (`X-Slab-Shape`, `X-Slab-Level`) belong in an
        # integration test; here the point is that a tile request lands on the same voxels.
        # (The route itself needs a project_uid to resolve, which is set up in the `pyramid QC` fixture.)
    end
end

@testset "API: viewer meta — per-level shapes (spatial audit LOD)" begin
    # `api_viewer_meta` grew a `levels` field so the CLIENT can pick a pyramid level from its viewport
    # zoom without asking the server. Same store the tile testset builds, exposed through the meta route
    # on a temporary project dir so the API layer is what's under test — not `store_pyramid_levels`,
    # which is covered where it lives.
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        # `resolve_image_version` reads projects_dir()/proj/1/img/ccid.json → `filepath` → the store
        proj = "PYRTST"; img = "IMGTST"
        proj_dir = joinpath(tmp, proj); mkpath(proj_dir)
        img_dir = joinpath(proj_dir, "1", img); mkpath(img_dir)
        state_target = joinpath(img_dir, "ccid.json")
        store_dir = joinpath(proj_dir, "0", img, "ccidImage.ome.zarr"); mkpath(dirname(store_dir))
        # Two-level flat pyramid: L0 8×6×2×2×1, L1 4×3×2×2×1. In Julia dims (x,y,z,c,t).
        # `_sampled_specs` reads a mid-stack plane for contrast, so both arrays need actual chunks on
        # disk — Zarr.jl throws `missing chunks and no fill_value` on a partially-empty store.
        g = zgroup(Zarr.DirectoryStore(store_dir); attrs = axes_ms(["t", "c", "z", "y", "x"], 2))
        a0 = zcreate(UInt16, g, "0", 8, 6, 2, 2, 1; chunks = (4, 4, 1, 1, 1))
        a1 = zcreate(UInt16, g, "1", 4, 3, 2, 2, 1; chunks = (4, 4, 1, 1, 1))
        a0[:, :, :, :, :] = zeros(UInt16, 8, 6, 2, 2, 1)
        a1[:, :, :, :, :] = zeros(UInt16, 4, 3, 2, 2, 1)
        write(state_target,
              JSON3.write(Dict("filepath" => Dict("default" => "ccidImage.ome.zarr",
                                                  "_active" => "default"))))
        st, body = api_viewer_meta(HTTP.Request("GET",
            "/api/viewer/meta?projectUid=$proj&imageUid=$img"))
        @test st == 200
        j = JSON3.read(body)
        @test haskey(j, :levels)
        lv = j[:levels]
        @test length(lv) == 2
        # `nX = shape[-1]`, `nY = shape[-2]` — Y×X reads the way the modal already renders shape
        @test (lv[1][:level], lv[1][:nX], lv[1][:nY]) == (0, 8, 6)
        @test (lv[2][:level], lv[2][:nX], lv[2][:nY]) == (1, 4, 3)
        @test (lv[1][:chunkX], lv[1][:chunkY]) == (4, 4)
        @test (lv[2][:chunkX], lv[2][:chunkY]) == (4, 4)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: viewer meta — labelDims lets the picker flag masks that don't fit" begin
    # A mask store segmented on a DIFFERENT image version keeps its old spatial dims (drift-expanded
    # / cropped — same class as commit 860da24b). The viewer's overlay path assumes label dims equal
    # image dims at the same level; a mismatch either mis-strides the label texture (silent wrong
    # render — see volumeRenderer.uploadFrame's `bytesPerRow = imageNX * LABEL_BPV`) or trips the
    # frontend shape guard. Meta now carries per-vn L0 dims so the sidebar picker can flag the
    # offending row rather than the user hitting the raw error at fetch time.
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "meta-labeldims")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        proj_id = proj.uid; img_id = img.uid
        # Image L0 8×6 (Julia dims x, y, z, c, t as elsewhere in this file).
        store_dir = joinpath(tmp, proj_id, "0", img_id, "ccidImage.ome.zarr")
        mkpath(dirname(store_dir))
        g = zgroup(Zarr.DirectoryStore(store_dir); attrs = axes_ms(["t","c","z","y","x"], 1))
        a = zcreate(UInt16, g, "0", 8, 6, 1, 1, 1; chunks = (4, 4, 1, 1, 1))
        a[:, :, :, :, :] = zeros(UInt16, 8, 6, 1, 1, 1)
        # Two label stores under the image's labels/ dir: `matching` 8×6 (fits),
        # `mismatched` 10×6 (nX differs → flagged). Register through `img.labels` + `save!` so the
        # ccid.json is written in the versioned shape the readers expect.
        labels_dir = joinpath(tmp, proj_id, "1", img_id, "labels"); mkpath(labels_dir)
        for (name, nx, ny) in (("matching", 8, 6), ("mismatched", 10, 6))
            lp = joinpath(labels_dir, "$(name).zarr")
            lg = zgroup(Zarr.DirectoryStore(lp);
                        attrs = axes_ms(["t", "c", "z", "y", "x"], 1))
            la = zcreate(UInt32, lg, "0", nx, ny, 1, 1, 1; chunks = (nx, ny, 1, 1, 1))
            la[:, :, :, :, :] = zeros(UInt32, nx, ny, 1, 1, 1)
        end
        img.filepath = Dict("default" => "ccidImage.ome.zarr", "_active" => "default")
        img.labels = Dict("matching" => ["matching.zarr"],
                          "mismatched" => ["mismatched.zarr"])
        save!(img)
        st, body = api_viewer_meta(HTTP.Request("GET",
            "/api/viewer/meta?projectUid=$(proj_id)&imageUid=$(img_id)"))
        @test st == 200
        j = JSON3.read(body)
        @test j[:nX] == 8 && j[:nY] == 6
        @test haskey(j, :labelDims)
        # Both registered stores must be reported. Absence would leave the client unable to check.
        @test haskey(j[:labelDims], :matching)
        @test haskey(j[:labelDims], :mismatched)
        @test (j[:labelDims][:matching][:nX],   j[:labelDims][:matching][:nY])   == (8,  6)
        @test (j[:labelDims][:mismatched][:nX], j[:labelDims][:mismatched][:nY]) == (10, 6)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
