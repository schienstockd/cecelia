# ── Browser viewer: volume slabs + display metadata ───────────────────────────────
# The server half of the in-browser WebGPU renderer (docs/todo/WEB_VIEWER_PLAN.md). Two routes:
#
#   GET /api/viewer/meta  → JSON: geometry, per-channel contrast + LUT stops
#   GET /api/viewer/slab  → raw voxels of ONE (t, c) volume, ready to `copyBufferToTexture`
#
# WHY A WHOLE (t, c) VOLUME AND NOT CHUNKS. The browser could fetch the store's chunks itself and
# assemble them — that was the obvious design, and it is 4.2x SLOWER than this one. Measured on
# `VJy1Nx` (38 z x 1046 x 1104, chunks 1x1x1x512x512): 1116 present chunks per timepoint took 5270 ms
# over HTTP and stopped improving past concurrency 8, against 737 ms for four assembled slabs. Per-
# request overhead dominates at that count, so the assembly belongs on the side that already has the
# store open. The browser decodes NOTHING — it gets voxels. See NAPARI_WEBGPU_AUDIT.md → G3.
#
# WHAT THE BYTE ORDER OF THE BODY IS, and why it is not negotiable. The body is the volume in
# C-order — x fastest, then y, then z — which is exactly the layout `GPUQueue.writeTexture` /
# `copyBufferToTexture` want for a 3D texture, so the client uploads the response with no transform.
# Zarr.jl hands back a column-major array whose dims are REVERSED (see `axis_dims`), so for the usual
# (t,c,z,y,x) store its linear memory is ALREADY x-fastest and the permute below is a no-op. It is
# still written explicitly, because a store with any other axis order would otherwise transpose
# silently — and a transposed volume renders as plausible-looking garbage, not as an error.
#
# This inherits `image_render.jl`'s SANCTIONED, NARROW carve-out of the one-canonical-reader rule:
# Julia touches the zarr directly (Zarr.jl) for display work only. Colours are NOT re-derived here —
# they come from `resolved_display_specs` in image_render.jl, which is the one place that knows how to
# turn a napari props file into RGB (a name table there once missed `bop blue` and rendered a channel
# WHITE). Do not add a second palette.

using ChunkCodecLibZstd: ZstdEncodeOptions
using ChunkCodecCore: encode

# ── Reading one volume ────────────────────────────────────────────────────────────

"""
    read_slab(zarr_path, t, c; z = nothing, x = nothing, y = nothing, level = 0) -> (vol, nx, ny, nz)

Voxels of timepoint `t`, channel `c` (both 0-based) as an `(x, y, z)` column-major array — i.e. one
whose linear memory is x-fastest, which is what a WebGPU 3D texture takes. Missing axes count as 1, so
a 2D single-channel still answers the same shape of question as a 5D movie.

`z` (0-based) selects the depth, and it is the difference between a timecourse you can watch and one
you wait on. It takes either kind of index, and WHICH KIND decides the rank of the answer:

  - an `Int` reads ONE PLANE and drops the z dim, exactly as `t` and `c` do → `nz == 1`;
  - a `UnitRange` reads a SLAB of that many planes and keeps it → a shorter volume.

Measured on `Dml3RG` (37 z, 4 ch, 181 t): a whole timepoint is 326 MB and ~400 ms of server read, one
plane is 8.8 MB and ~13-22 ms. More to the point, the whole 181-timepoint movie is 1.59 GB at one plane
against 59 GB at full depth — so it FITS in a VRAM budget, and the second pass through it is entirely
cache hits. This is the view Dominik actually uses for a timecourse (2026-08-24).

The RANGE is what makes the volume view usable, and it is Dominik's suggestion (2026-08-24): every cost
here is linear in the number of planes, so 8 of 37 is 70 MB rather than 326 MB — a ~0.25 s fetch instead
of ~1 s — and four times as many timepoints fit the same VRAM budget. Structure is usually in a few
planes, so a full-depth MIP is mostly paying for empty stack.

`x`/`y` (0-based `UnitRange`, or `nothing` for the whole axis) carve an XY TILE out of the plane, which
is what makes a big-XY tilescan pan/zoom viewer answerable at all — L0 of `f8gzA2` is 20329×16898 and
687 MB per channel as a whole slab, but a single 1024² chunk is 2 MB. Same shape as `z`'s range: the axis
is KEPT (it is a rectangle, not a column). Clamped to the store, so a viewport that hangs off the edge
of a frame gets a smaller tile back rather than a 500. Combined with `level`, this is the whole
"slippy-map" access pattern the temporal work never needed (spatial audit Phase 2, 2026-08-25).

`level` (0-based) selects the pyramid resolution. Level 0 is the highest resolution (what every caller
before the tile route used, so it stays the default). The client picks the level from the meta route's
per-level shapes — server-side does not know the viewport zoom.

Pixels go through `read_native`, never `arr[...]`: a raw `bioformats2raw` store is big-endian and
Zarr.jl does not swap it (see `image_geometry.jl`).

The `(arr, caxes)` form is for a caller that reads MANY slabs out of one store — a movie sweep asks for
`nT * nC` of them, and re-opening per read is `nT * nC` metadata round trips for a store whose geometry
cannot change mid-sweep. It does not take `level` because the caller has already opened a specific
level; passing a different one would silently disagree with the array in hand.
"""
read_slab(zarr_path::AbstractString, t::Int, c::Int;
          z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
          x::Union{AbstractUnitRange{Int},Nothing} = nothing,
          y::Union{AbstractUnitRange{Int},Nothing} = nothing,
          level::Int = 0) =
    read_slab(open_level(zarr_path, level)..., t, c; z = z, x = x, y = y)

function read_slab(arr, caxes, t::Int, c::Int;
                   z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
                   x::Union{AbstractUnitRange{Int},Nothing} = nothing,
                   y::Union{AbstractUnitRange{Int},Nothing} = nothing)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    names = caxes_or_fallback(caxes, nd)

    idx = Any[Colon() for _ in 1:nd]
    haskey(dims, "t") && (idx[dims["t"]] = t + 1)      # 0-based → 1-based; scalar, so the dim drops
    haskey(dims, "c") && (idx[dims["c"]] = c + 1)
    # A scalar z drops the z dim exactly as t and c do, so `kept` below excludes it and `nz` answers 1
    # — one code path serves a plane, a sub-slab and a whole volume rather than three that can disagree.
    # Ranges (x, y, and z-as-range) are CLAMPED to the store rather than trusted: these come off a query
    # string, and an out-of-range index is a 500 from deep inside Zarr.jl instead of an answer. An empty
    # range reads as the single plane/column at its start — that state cannot come from a caller asking
    # for nothing (Julia's `UnitRange` constructor normalises `2:0` to the empty `2:1`), so it is always
    # a lo/hi pair that arrived backwards and the ordering is fixed where the two numbers are still
    # separate (`try_serve_slab`). A zero-thickness slab would render BLACK: entry and exit coincide.
    _clamp_range(v::AbstractUnitRange{Int}, len::Int) = begin
        cl(i) = clamp(i, 0, len - 1) + 1
        lo = cl(first(v))
        lo:max(cl(isempty(v) ? first(v) : last(v)), lo)
    end
    if z !== nothing && haskey(dims, "z")
        nz_all = size(arr, dims["z"])
        idx[dims["z"]] = z isa Int ? clamp(z, 0, nz_all - 1) + 1 : _clamp_range(z, nz_all)
    end
    y === nothing || !haskey(dims, "y") ||
        (idx[dims["y"]] = _clamp_range(y, size(arr, dims["y"])))
    x === nothing || !haskey(dims, "x") ||
        (idx[dims["x"]] = _clamp_range(x, size(arr, dims["x"])))
    sub = read_native(arr, idx...)

    # Julia dim j carries the C-order axis at position nd-j+1. Scalar indexing dropped t and c, so
    # rebuild the surviving names in Julia dim order and permute them to exactly (x, y, z).
    kept = String[names[nd - j + 1] for j in 1:nd if !(idx[j] isa Int)]
    order = Int[]
    for want in ("x", "y", "z")
        k = findfirst(==(want), kept)
        k === nothing || push!(order, k)
    end
    # `order == 1:n` for every store we have (x IS Julia dim 1 there), and `permutedims` copies even
    # for the identity — 87 MB per slab on the real target. Skip it, but keep the general path.
    vol = (ndims(sub) != length(order) || order == collect(1:length(order))) ? sub :
          permutedims(sub, order)
    nx, ny = size(vol, 1), size(vol, 2)
    nz = ndims(vol) >= 3 ? size(vol, 3) : 1
    vol, nx, ny, nz
end

"""
    slab_bytes(vol) -> Vector{UInt8}

`vol`'s voxels as little-endian bytes. `read_native` has already put them in HOST order; this pins the
wire format to little-endian so the client never has to ask. A no-op copy on x86/ARM.
"""
function slab_bytes(vol)
    v = vec(vol)
    HOST_IS_LITTLE_ENDIAN || (v = htol.(v))  # a copy, so it is behind the branch, not unconditional
    reinterpret(UInt8, v)
end

# ── Contrast when napari has never opened the image ───────────────────────────────

# Percentile contrast from ONE z-plane of timepoint 0, xy-subsampled to ~`target` px per side. The
# viewer's own contrast (napari's props file) always wins; this is the cold-start answer for an image
# nobody has opened yet, and it is deliberately cheap: z chunks are size 1, so this reads a handful of
# chunks per channel rather than a whole volume.
#
# It is sampled from ONE FIXED (t, z) on purpose — decision 5 of WEB_VIEWER_PLAN.md. Contrast computed
# per timepoint makes playback flicker as the window chases each frame's own distribution.
function _sampled_specs(zarr_path::AbstractString, nc::Int; target::Int = 256)
    arr, caxes = open_level0(zarr_path)
    nd   = ndims(arr)
    dims = axis_dims(caxes, nd)
    jz   = get(dims, "z", 0)
    specs = Tuple{Float64,Float64,Any,Bool}[]
    for c in 0:(nc - 1)
        idx = Any[Colon() for _ in 1:nd]
        haskey(dims, "t") && (idx[dims["t"]] = 1)
        haskey(dims, "c") && (idx[dims["c"]] = c + 1)
        jz != 0 && (idx[jz] = cld(size(arr, jz), 2))            # mid-stack plane
        plane = read_native(arr, idx...)
        step  = max(1, cld(maximum(size(plane)), target))
        step > 1 && (plane = plane[(1:step:s for s in size(plane))...])
        push!(specs, percentile_spec(plane, DEFAULT_CMAPS[mod1(c + 1, 4)]))
    end
    specs
end

# ── GET /api/viewer/meta ──────────────────────────────────────────────────────────
# ?projectUid=&imageUid=&valueName= →
#   {nT, nC, nZ, nX, nY, bytesPerVoxel, slabBytes, contrastSource,
#    voxelUm: [x, y, z], spaceUnit, frameIntervalMin, calibrated: {xy, z, t},
#    channels: [{name, lo, hi, visible, lut: [[r,g,b], ...]}]}
#
# Everything the renderer needs BEFORE it asks for a single voxel: how many slabs exist, how big one
# is (the client sizes its VRAM cache from `slabBytes`), and how to colour each channel. `lut` is a
# black→colour (or white→colour) ramp the client uploads as a small 1D lookup texture; resolving it
# here rather than in TypeScript is what keeps napari's ~30 colormaps from being re-guessed by name.
#
# `frameIntervalMin` and `spaceUnit` are for the on-image overlays (scale bar + elapsed time), which
# are the napari ones the browser has to match. Both are NULLABLE and that is the point: the interval
# is minutes/frame from `img_physical_sizes`, which defaults a missing value to 1.0 — a real 1 min/frame
# and "no idea" are indistinguishable in the number, so the flag decides and the client falls back to a
# frame index rather than inventing a duration. `calibrated.t` comes from `img_scale_axes`, the same
# image-owned accessor the task gating uses, so the viewer cannot disagree with the rest of the app
# about whether this image has a real timecourse.
#
# `voxelUm` is load-bearing for a 3D view, not a nicety: z spacing is typically 3-10x the xy pitch, so
# a raycast through an unscaled voxel grid renders the stack squashed by that factor. It comes from
# `img_physical_sizes`, the image-owned accessor, and `calibrated` says whether each axis is a REAL
# measurement — that helper defaults a missing axis to 1.0 on purpose, indistinguishable from a genuine
# 1 um/px, so a consumer that displays a scale must ask separately (see its docstring).
function api_viewer_meta(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    try
        arr, caxes = open_level0(zp)
        d  = axis_dims(caxes, ndims(arr))
        dim(n) = haskey(d, n) ? size(arr, d[n]) : 1
        nx, ny, nz, nc, nt = dim("x"), dim("y"), dim("z"), dim("c"), dim("t")
        bpv = sizeof(eltype(arr))

        props = _props_path(td, zp)
        specs = resolved_display_specs(props, nc)
        src   = specs === nothing ? "sampled" : "viewer"
        specs === nothing && (specs = resolved_display_specs(_sampled_specs(zp, nc)))

        # One `init_object` for everything it can answer: the display names, the calibration, the
        # frame interval and the unit the scale bar is labelled in.
        names, vox, cal, unit, tmin = try
            img = init_object(pu, iu)
            sizes, ts = img_physical_sizes(img)        # [sz, sy, sx] um/px, minutes/frame
            ax = img_scale_axes(img)
            has_t = :T in ax
            (something(channel_names(img; value_name = vnn), String[]),
             [sizes[3], sizes[2], sizes[1]],           # → [x, y, z], the renderer's axis order
             (; xy = :XY in ax, z = :Z in ax, t = has_t),
             _meta_str(img.meta, "PhysicalSizeUnit"),
             has_t ? ts : nothing)
        catch
            (String[], [1.0, 1.0, 1.0], (; xy = false, z = false, t = false), nothing, nothing)
        end
        channels = [(; name = get(names, c, "Channel $(c - 1)"),
                       lo = s.lo, hi = s.hi, visible = s.visible,
                       lut = [[l[1], l[2], l[3]] for l in s.lut])
                    for (c, s) in enumerate(specs)]

        # Which segmentations have a MASK on disk, so the client can offer them without a probe per
        # name. `labels` and `label_props` are independent registries — an imported track set has a
        # measurement table and no mask — so this is not derivable from the overlay payload.
        label_names = try
            img = init_object(pu, iu)
            String[v for v in versioned_keys(img.labels)
                   if !is_reserved_value_name(v) && isdir(img_labels_path(img, v))]
        catch
            String[]
        end
        # Which VERSIONS this image has, and which one these numbers describe. The viewer window is a
        # pop-out with no project open, so it can look up neither — and without the second field a
        # version picker cannot show what it is already on. `resolve_image_version(.., nothing)` picks
        # the ACTIVE version, the one a task would run against, so this reports that rule's answer
        # rather than introducing a second notion of "active".
        value_names, active_vn = try
            raw = read_ccid_raw(state_file(joinpath(projects_dir(), pu), iu))
            fp  = get(raw, "filepath", nothing)
            fp isa AbstractDict ? (versioned_keys(fp), versioned_active(fp)) : (String[], "")
        catch
            (String[], "")
        end
        # Per-level shape + chunk shape, so the client can pick a pyramid LEVEL from its own viewport
        # zoom without asking the server. `store_pyramid_levels` is the same reader the metadata modal
        # uses (`api_image_stores`), so the two surfaces cannot disagree about what is on disk. `nothing`
        # means the store is not multiscales-shaped or its group metadata is unreadable — the client
        # sees an empty `levels` and falls back to L0 only, which is what a caller before the tile route
        # already did. Shape uses store row-major: `shape[-2]` is nY, `shape[-1]` is nX; a per-level XY
        # pair keeps the client from having to know the store's axis order.
        # Client LOD formula (spatial audit Phase 3): `level = clamp(floor(log2(zoom)), 0, nLevels-1)`,
        # where `zoom = imagePxPerDevicePx`. That formula assumes clean 2× steps — TRUE for every store
        # bioformats2raw or `create_multiscales` writes today — but the SHAPES are also carried here so a
        # non-2× pyramid (a future stitching writer) can be handled without re-encoding this contract:
        # the client computes the actual factor as `L0.nX / L[n].nX` and selects against that instead.
        lvls = try store_pyramid_levels(zp) catch; nothing end
        levels = lvls === nothing ? Any[] :
            [(; level = i - 1, nY = get(l.shape, length(l.shape) - 1, 0),
                nX = get(l.shape, length(l.shape),     0),
                chunkY = get(l.chunks, length(l.chunks) - 1, 0),
                chunkX = get(l.chunks, length(l.chunks),     0))
             for (i, l) in enumerate(lvls)]
        200, JSON3.write((; nT = nt, nC = nc, nZ = nz, nX = nx, nY = ny, labelNames = label_names,
                            valueNames = value_names,
                            valueName = vnn === nothing ? active_vn : vn,
                            # The ACTIVE one regardless of what was asked for, so a picker can say
                            # whether the version on screen is the one every task runs against. With
                            # only `valueName` an explicit request echoes itself and the comparison is
                            # impossible.
                            activeValueName = active_vn,
                            bytesPerVoxel = bpv, slabBytes = nx * ny * nz * bpv,
                            contrastSource = src, voxelUm = vox, spaceUnit = unit,
                            frameIntervalMin = tmin, calibrated = cal, channels, levels))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── GET /api/viewer/slab ──────────────────────────────────────────────────────────
# Served from the STREAM handler, not the route table, because it needs headers of its own —
# `Content-Encoding` and the shape guard. Same idiom as `try_serve_movie`. Returns false when this is
# not a slab request or the image cannot be resolved, and the caller falls through to the 404 path.
#
# ?projectUid=&imageUid=&valueName=&t=&c=&z=&zTo=&x=&xTo=&y=&yTo=&level=&enc=identity|zstd
#     → raw little-endian voxels.
#
# `z` omitted → the whole stack. `z=N` → that ONE plane (the 2D view), which is what makes a timecourse
# playable: 8.8 MB and ~13 ms against 326 MB and ~400 ms on `Dml3RG`. `z=N&zTo=M` → the planes N..M
# inclusive, which is what makes the 3D view usable — every cost is linear in the count.
#
# `x`/`xTo` and `y`/`yTo` carve out an XY TILE at 0-based inclusive `[lo,hi]`, omitted axes stay whole.
# Same pairing as `z`/`zTo` — a range in each field (`x=100:200`) is where "5:5" and "5" quietly diverge,
# so the two ends are separate. `level` (default 0) picks a pyramid resolution: the client asks the meta
# route for the per-level shapes and picks its LOD from those, so the server does not know the
# viewport zoom. All four are clamped to the store, so a viewport hanging off the edge of a frame gets
# a smaller tile back rather than a 500 from deep inside Zarr.jl. (Spatial audit Phase 2, 2026-08-25.)
#
# `zTo` is a separate parameter rather than a `z=lo:hi` string on purpose: the client already had a
# scalar `z`, the two cases mean different RANKS of answer, and parsing a range out of one field is a
# place for `z=5` and `z=5:5` to quietly diverge.
#
# `enc` is the CLIENT's choice and defaults to identity, deliberately — it is not `Accept-Encoding`
# negotiation. A browser always advertises zstd, so negotiating would compress unconditionally, and
# compression measured 195-208 ms per timepoint against a 330 ms read: a third of the server's time
# spent shrinking bytes that a loopback socket moves for free. It is the remote-VM case that wants it,
# and only the client knows which case it is in.
function try_serve_slab(stream::HTTP.Stream, target::AbstractString)::Bool
    q  = HTTP.queryparams(HTTP.URI(target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    # `labels=<value_name>` serves the MASK for that segmentation instead of the image. Same reader,
    # same headers, same shape guard — a mask is just another zarr of the same geometry, which is what
    # makes P4 cheap. The dtype differs (label ids, not intensities), and `X-Slab-Bpv` already says so.
    lbl = get(q, "labels", "")
    if !isempty(lbl)
        zp, lerr = label_store_path(get(q, "projectUid", ""), get(q, "imageUid", ""), lbl)
        lerr === nothing || return false
    else
        zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
        err === nothing || return false
    end
    t = something(tryparse(Int, get(q, "t", "0")), 0)
    c = something(tryparse(Int, get(q, "c", "0")), 0)
    z0 = haskey(q, "z") ? tryparse(Int, q["z"]) : nothing
    z1 = haskey(q, "zTo") ? tryparse(Int, q["zTo"]) : nothing
    # `zTo` present promotes the scalar to a range, which is what keeps the z dim (see `read_slab`).
    # Ordered HERE, while the two are still separate integers: `hi:lo` cannot be represented — Julia
    # normalises a backwards `UnitRange` to an empty one — so a swapped pair has to be caught before it
    # becomes a range at all.
    z = z0 === nothing ? nothing :
        (z1 === nothing ? z0 : min(z0, z1):max(z0, z1))
    # x/y always PROMOTE to a range — a single-column tile is not a shape the pan/zoom viewer asks for
    # and `read_slab` for x/y only takes ranges. Absent lo → 0; absent hi → typemax(Int), which
    # `read_slab` clamps to the axis length. Ordered here for the same reason `z` is (a backwards
    # `UnitRange` normalises to empty, so a swapped pair has to be caught before it becomes a range).
    xy_range(lo_key, hi_key) = begin
        lo = haskey(q, lo_key) ? tryparse(Int, q[lo_key]) : nothing
        hi = haskey(q, hi_key) ? tryparse(Int, q[hi_key]) : nothing
        lo === nothing && hi === nothing && return nothing
        lo_i = lo === nothing ? 0            : lo
        hi_i = hi === nothing ? typemax(Int) : hi
        min(lo_i, hi_i):max(lo_i, hi_i)
    end
    xr = xy_range("x", "xTo"); yr = xy_range("y", "yTo")
    # Level defaults to 0 (the highest resolution — every caller before the tile route asks for this).
    # Clamped against the multiscales datasets list so a hand-edited URL cannot reach `open_level`'s
    # KeyError path. `store_pyramid_levels` is metadata-only (JSON on disk); the read is cheap.
    lvl_req = something(tryparse(Int, get(q, "level", "0")), 0)
    nlvl    = something(let l = store_pyramid_levels(zp); l === nothing ? nothing : length(l) end, 1)
    level   = clamp(lvl_req, 0, nlvl - 1)
    enc = get(q, "enc", "identity")

    # A read failure has to arrive as a STATUS, not as an exception. This runs before `startwrite`, so
    # there is still a response to shape — once bytes are on the wire the only thing left is a broken
    # connection, which the client cannot tell from a network blip. An out-of-range `t` or `c` is the
    # ordinary case (a hand-edited URL, a stale slider bound) and answers 400 with the actual bound.
    local body, nx, ny, nz, bpv, read_ms, comp_ms
    try
        t0 = time()
        vol, nx, ny, nz = read_slab(zp, t, c; z = z, x = xr, y = yr, level = level)
        body = slab_bytes(vol)
        bpv  = sizeof(eltype(vol))
        read_ms = round(1000 * (time() - t0); digits = 1)

        comp_ms = 0.0
        if enc == "zstd"
            t1 = time()
            body = encode(ZstdEncodeOptions(; compression_level = 1), body)
            comp_ms = round(1000 * (time() - t1); digits = 1)
        end
    catch e
        msg = e isa BoundsError ? "t/c out of range for this image version" : sprint(showerror, e)
        @error "Slab read failed" zarr = zp t c exception = (e, catch_backtrace())
        HTTP.setstatus(stream, e isa BoundsError ? 400 : 500)
        HTTP.setheader(stream, "Content-Type" => "application/json")
        HTTP.setheader(stream, "Access-Control-Allow-Origin" => "*")
        HTTP.startwrite(stream)
        write(stream, JSON3.write((; error = msg)))
        return true
    end

    HTTP.setheader(stream, "Content-Type"   => "application/octet-stream")
    # nz,ny,nx — the client asserts this against `meta`, so a store whose axes are not what we think
    # fails LOUDLY. Silently transposed voxels still render; they just render the wrong thing.
    HTTP.setheader(stream, "X-Slab-Shape"   => "$nz,$ny,$nx")
    HTTP.setheader(stream, "X-Slab-Bpv"     => string(bpv))
    # The level actually served after clamping. A hand-edited URL asking level=99 gets the deepest
    # existing level, and the client's cache key needs to match what came back, not what it asked for.
    HTTP.setheader(stream, "X-Slab-Level"   => string(level))
    HTTP.setheader(stream, "X-Server-Read-Ms"     => string(read_ms))
    HTTP.setheader(stream, "X-Server-Compress-Ms" => string(comp_ms))
    HTTP.setheader(stream, "Access-Control-Allow-Origin"   => "*")
    HTTP.setheader(stream, "Access-Control-Expose-Headers" =>
                   "X-Slab-Shape, X-Slab-Bpv, X-Slab-Level, X-Server-Read-Ms, X-Server-Compress-Ms")
    # Slab URLs are content-addressed: every parameter that changes the bytes (t, c, z, zTo, x, xTo,
    # y, yTo, level, enc, labels, valueName) is in the query string, so the same URL always returns the
    # same bytes UNTIL the source store is reprocessed. `max-age=3600` lets the browser serve a slab a
    # second time without a round trip — the whole point of a plane switch that returns to a plane you
    # already loaded. Deliberately short: a reprocess would otherwise leave stale bytes cached for the
    # rest of the session (a hard reload clears it either way). Not `immutable` for the same reason.
    HTTP.setheader(stream, "Cache-Control"                 => "private, max-age=3600")
    enc == "zstd" && HTTP.setheader(stream, "Content-Encoding" => "zstd")
    HTTP.setheader(stream, "Content-Length" => string(length(body)))
    HTTP.setstatus(stream, 200)
    HTTP.startwrite(stream)
    write(stream, body)
    true
end

# ── Label (segmentation mask) stores ──────────────────────────────────────────────
"""
    label_store_path(project_uid, image_uid, value_name) -> (path, err)

The finished label store for one segmentation, through `img_labels_path` — the image-owned accessor —
so this cannot drift from where the tasks write. `err` is a message when the image, the segmentation or
the store on disk is missing; all three are normal states (an unsegmented image has none).

A `labels` value_name can carry SEVERAL files (a base mask and a nuclear one). `img_labels_path` answers
the FIRST, which is the base mask — the one napari shows as "(vn) Labels". Serving the others needs the
file to be named, which is a decision for when the client offers them.
"""
function label_store_path(project_uid::AbstractString, image_uid::AbstractString,
                          value_name::AbstractString)
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return (nothing, "image not found")
    vn = isempty(value_name) ? "" : String(value_name)
    haskey(img.labels, vn) || return (nothing, "no label store named '$vn'")
    zp = img_labels_path(img, vn)
    isdir(zp) || return (nothing, "label store not on disk: $(basename(zp))")
    (zp, nothing)
end

# ── GET /api/viewer/overlays ──────────────────────────────────────────────────────
# ?projectUid=&imageUid=&valueName=&popType=flow&colourBy= →
#   {nCells, axes, hasT, cells: {label, t, x, y, z, track}, pops: [...],
#    colourColumns: [...], colourBy, values}
#
# Everything the browser viewer needs to draw the h5ad-derived overlays napari draws: per-cell
# centroids, population membership, track ids, and one optional per-cell column to colour by
# (WEB_VIEWER_PLAN.md → P3).
#
# ONE REQUEST FOR THE WHOLE MOVIE, not one per timepoint, and the measurement is why. The largest cell
# table in the dev projects is 98,610 cells (`WIaUjL/p6t4mC/Tcell`); the typical one is 6,547. At five
# f32 columns that is 2.0 MB and 0.13 MB — comparable to a SINGLE 2D slab (8.8 MB), so the client
# fetches once per (image, value_name) and filters by t locally. That is the whole reason P3 needed no
# caching story of its own, and it was measured before the route was written rather than after.
#
# COLUMNAR, NOT PER-CELL OBJECTS. `{label: [...], x: [...]}` is ~5x smaller as JSON than
# `[{label:…, x:…}, …]` and lands in a `Float32Array` with one pass; a per-cell object array would also
# be ~40% of the parse time on the 98k case. If a dataset ever arrives that makes even this too big, the
# shape is already the one a binary body would have — see the note in the plan.
#
# MEMBERSHIP COMES FROM `resolve_pops`, the same cached resolver that feeds napari's points layers
# (`api_napari_show_populations`). Not a second membership path: pop membership is gating-engine
# business, it is cached against the gating-map and h5ad mtimes, and a viewer that computed its own
# would be a second answer to "which cells are in /A" that could disagree with the plots.
#
# Coordinates are in **µm**, through `scale_centroids!` — the one pixel→µm conversion for centroids, so
# the overlay lands in the same space as `extentUm` and nothing has to rescale it. `t` is deliberately
# NOT scaled: it stays a frame index, which is what the client filters on and what the column means on
# disk.
const OVERLAY_MAX_CELLS = 300_000

function api_viewer_overlays(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    _has_label_props(img) ||
        return 200, JSON3.write((; nCells = 0, axes = String[], hasT = false,
                                   cells = (;), pops = [], colourColumns = String[],
                                   colourBy = nothing, values = nothing,
                                   note = "not segmented"))
    vn  = _resolve_vn(img, get(q, "valueName", ""))
    pt  = get(q, "popType", "flow")
    cby = get(q, "colourBy", "")

    try
        lp   = label_props(img; value_name = vn)
        obs  = col_names(lp; data_type = :obs)
        # Ask for centroids, the track id and the colour column in ONE read. `select_cols` pushes the
        # selection into the file, so an unwanted 300-column feature matrix is never materialised.
        extra = String[c for c in (("track_id" in obs) ? ["track_id"] : String[])]
        (!isempty(cby) && cby in obs) && push!(extra, String(cby))
        view_centroid_cols(lp; order = [:x, :y, :z])
        isempty(extra) || select_cols(lp, extra)
        df = as_df(lp)
        # `size(df, 1)` and `hasproperty`, never `nrow`/`names` — `api/` does not import DataFrames, and
        # a test enforces it (those two would dispatch to Base and fail at runtime, not at load).
        n  = size(df, 1)
        has(c) = hasproperty(df, Symbol(c))
        n > OVERLAY_MAX_CELLS && return 413, JSON3.write((;
            error = "$n cells is past the overlay limit of $OVERLAY_MAX_CELLS — " *
                    "the JSON payload would be too large to parse in the browser"))
        scale_centroids!(df, img)                  # pixels → µm, per axis; `centroid_t` untouched

        axes = String[a for a in ("x", "y", "z") if has("centroid_$a")]
        # EVERY COORDINATE ARRAY IS FINITE, and the rows that cannot be are dropped here rather than
        # encoded. There is no sentinel available: JSON has no NaN literal (JSON3 refuses to write one
        # at all), and `null` is worse than useless in a coordinate array because `Float32Array.from`
        # turns it into 0 — a cell drawn at the origin instead of not drawn. A cell with no centroid is
        # not drawable, so it is not sent; `nDropped` says how many, because silently shipping fewer
        # cells than the table holds is the kind of thing that reads as a segmentation problem.
        fin(v) = !ismissing(v) && isfinite(Float64(v))
        need = vcat(String["centroid_$a" for a in axes], has("centroid_t") ? ["centroid_t"] : String[])
        keep = trues(n)
        for c in need
            v = df[!, c]
            for i in 1:n
                keep[i] = keep[i] && fin(v[i])
            end
        end
        idx = findall(keep)
        col(name) = has(name) ? Float64[Float64(df[i, name]) for i in idx] : Float64[]
        cells = (; label = Int[Int(df[i, :label]) for i in idx],
                   t     = col("centroid_t"),
                   x     = col("centroid_x"), y = col("centroid_y"), z = col("centroid_z"),
                   # -1 rather than 0/NaN for "not tracked": one sentinel the client tests, and it stays
                   # an integer, so it survives JSON without a float's rounding question.
                   track = has("track_id") ?
                           Int[(!fin(df[i, :track_id]) || Float64(df[i, :track_id]) <= 0) ?
                               -1 : Int(df[i, :track_id]) for i in idx] : Int[])

        # Populations, from the cached resolver. An image with no gating map answers an empty list
        # rather than an error — an unsegmented or ungated image is a normal state, not a failure.
        pops = try
            [(; path = p.path, name = p.name, colour = p.colour, show = p.show,
                isTrack = p.is_track, labels = p.labels)
             for p in resolve_pops(img, pt; value_name = vn)]
        catch e
            @warn "viewer overlays: populations unavailable" value_name = vn exception = e
            []
        end

        # A missing colour value IS meaningful — a cell that was not measured — so it is sent as
        # `null` rather than dropped. NaN has to become null too: it is what a float column actually
        # carries for "no value" (`live.cell.speed` on a cell's first frame), and JSON3 throws on it.
        # `colourBy` echoes back only when it was HONOURED, never the name that was asked for. A saved
        # view naming a column that has since gone must come back as "no colour-by" rather than as a
        # colour-by with no values — the client cannot tell those apart from the request.
        used = (!isempty(cby) && has(cby)) ? String(cby) : nothing
        vals = used === nothing ? nothing :
               Any[(v = df[i, used]; (ismissing(v) || (v isa Real && !isfinite(Float64(v)))) ?
                    nothing : v) for i in idx]

        # WHICH KIND OF COLUMN decides how the client colours it — a palette per level, or a ramp over a
        # range — and that question is already answered ONCE, by `_is_categorical_col`. It is not a
        # one-liner: strings are categorical, any fractional value makes a column continuous, a small
        # integer level set is a code set, and there are name carve-outs both ways (`clusters.*` is
        # always categorical however many levels; `min_distance#`/`contact#` are quantities even when
        # stored as 0/1). Re-deriving that in TypeScript would be a second answer that disagrees with
        # the plots about the same column, which is the class of bug this codebase keeps paying for.
        # Reaching for the underscore name is deliberate: same rule, one owner.
        kind = used === nothing ? nothing :
               (Cecelia._is_categorical_col(df[!, used], used) ? "categorical" : "numeric")
        # The levels (categorical) or the range (numeric) the client maps onto — computed here for the
        # same reason, so the legend agrees with what a plot of this column would show.
        levels = kind == "categorical" ?
                 sort(unique(Any[v for v in vals if v !== nothing]); by = string) : nothing
        finite = kind == "numeric" ? Float64[Float64(v) for v in vals if v isa Real] : Float64[]
        range_ = (kind == "numeric" && !isempty(finite)) ?
                 [minimum(finite), maximum(finite)] : nothing
        200, JSON3.write((; nCells = length(idx), nDropped = n - length(idx),
                            axes, hasT = has("centroid_t"), cells, pops,
                            colourColumns = obs, colourBy = used, valueKind = kind,
                            valueLevels = levels, valueRange = range_,
                            values = vals, valueName = vn, popType = pt))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── Per-image viewer layer props (PY) ─────────────────────────────────────────────
# The WebGPU viewer's autosaved per-image view state: contrast/colormap per channel, camera, T/Z.
# Written to the SAME file napari's autosave writes to (`<task_dir>/data/<basename(zarr)>.json`), so
# animation-card snapshots stay portable across the two viewers. Format is a superset of napari's
# schema: `camera`/`dims`/`layers` mirror what `capture_view_state` writes (so a movie recorder that
# reads the file keeps working), and a `webgpu` sub-block carries the round-trippable native state
# (channel index, orbit-camera pose, mode, zPlane, zRange) that napari's schema cannot represent.
#
# See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → PY.
_viewer_props_path(task_dir::AbstractString, zarr_path::AbstractString) =
    joinpath(task_dir, "data", basename(zarr_path) * ".json")

# ── GET /api/viewer/props ─────────────────────────────────────────────────────────
# Returns the saved viewState JSON for this image version, or 404 when nothing was saved yet.
function api_viewer_props_get(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    p = _viewer_props_path(td, zp)
    isfile(p) || return 404, JSON3.write((; error = "No saved viewer props"))
    try
        200, read(p)
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── POST /api/viewer/props ────────────────────────────────────────────────────────
# Body: `{projectUid, imageUid, valueName?, viewState}`. Written atomically so a crash mid-write
# leaves the previous save intact (not a truncated file). Called on any relevant viewer state change
# through the frontend's debouncedSave scheduler — one write per settle, not per input event.
function api_viewer_props_post(body_bytes::Vector{UInt8})
    body = JSON3.read(body_bytes, Dict{String,Any})
    pu = String(get(body, "projectUid", ""))
    iu = String(get(body, "imageUid", ""))
    vnr = get(body, "valueName", nothing)
    vnn = (vnr === nothing || (vnr isa AbstractString && isempty(vnr))) ? nothing : String(vnr)
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    vs = get(body, "viewState", nothing)
    vs === nothing && return 400, JSON3.write((; error = "viewState missing"))
    try
        p = _viewer_props_path(td, zp)
        mkpath(dirname(p))
        write_json_atomic(p, vs)
        200, JSON3.write((; ok = true, path = p))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── POST /api/viewer/pick-cell (P8) ───────────────────────────────────────────────
# Click a cell in the WebGPU viewer → transient population highlighted on the gating plots.
# Reuses the same _set_napari_selection! / _inject_napari_pop! bridge napari's shape selection
# uses (`gating_api.jl` → *Napari cell-selection registry*) so this behaves identically to the
# existing linked-brushing pop — same JSON tree, same broadcast, same colour, no client-side
# gating store changes needed.
#
# Body: {projectUid, imageUid, valueName, popType, t, z, x, y, mode?}
#   x, y are IMAGE PIXEL indices (0-based), computed client-side by `screenToImagePx`.
#   z is the plane the click landed on; t is the current timepoint.
#   mode = 'replace' (default) — the new label REPLACES the current selection. Ergonomic default
#          for pointer picking: click a cell to see it, click another to swap. Multi-select is a
#          shift-click follow-up (`mode='add'` / `mode='toggle'`), not shipped in this pass.
#
# Response: {label, cellUid?, nSelected}. `label = 0` when the click landed on background — the
# response is still 200; the selection is unchanged.
function api_viewer_pick_cell(body_bytes::Vector{UInt8})
    body = JSON3.read(body_bytes, Dict{String,Any})
    pu   = String(get(body, "projectUid", ""))
    iu   = String(get(body, "imageUid", ""))
    pt   = String(get(body, "popType", "flow"))
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    vn   = _resolve_vn(img, String(get(body, "valueName", "")))
    # Which mask on disk. `label_store_path` answers the base mask for that segmentation — the
    # same one shown as "(vn) Labels" in napari and rendered by the WebGPU viewer's mask slot.
    zp, lerr = label_store_path(pu, iu, vn)
    zp === nothing && return 404, JSON3.write((; error = lerr))
    tint = _to_int(get(body, "t", 0))
    zint = _to_int(get(body, "z", 0))
    xint = _to_int(get(body, "x", 0))
    yint = _to_int(get(body, "y", 0))
    # `level` matches the LOD the viewer is DISPLAYING (client sends `slabLevel.value`). Label
    # downsampling is nearest, so reading L0 while the user sees L1 picks a NEIGHBOUR of the visible
    # cell — reads as "the wrong cell was highlighted". Clamped against the store's pyramid depth so a
    # stale client cannot reach `open_level`'s KeyError path (mirrors `try_serve_slab`).
    lvl_req = _to_int(get(body, "level", 0))
    nlvl    = something(let l = store_pyramid_levels(String(zp)); l === nothing ? nothing : length(l) end, 1)
    lvl     = clamp(lvl_req, 0, nlvl - 1)
    label = try
        vol, _, _, _ = read_slab(String(zp), tint, 0; z = zint, x = xint:xint, y = yint:yint, level = lvl)
        Int(first(vol))
    catch e
        return 500, JSON3.write((; error = "pick read failed: " * sprint(showerror, e)))
    end
    # Label 0 is background. Report and leave the selection alone — resetting on a background
    # click would surprise the user who missed a cell by one pixel.
    label == 0 && return 200, JSON3.write((; label = 0, nSelected = 0))
    mode = String(get(body, "mode", "replace"))
    cur  = something(_get_napari_selection(img._dir, vn), Int[])
    labs = if mode == "add"
        label in cur ? cur : vcat(cur, label)
    elseif mode == "toggle"
        label in cur ? filter(!=(label), cur) : vcat(cur, label)
    else
        Int[label]
    end
    _set_napari_selection!(img._dir, vn, labs)
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_napari_pop!(m, img)
    _broadcast_popmap(pu, iu, vn, pt, m)
    200, JSON3.write((; label, nSelected = length(labs)))
end

# ── POST /api/viewer/pick-rect (P8 rectangle drag) ────────────────────────────────
# Drag a rectangle in the viewer → all cells whose mask intersects that XY box at (t, z) become the
# transient population. Same registry / broadcast path as `api_viewer_pick_cell`, so the two share
# the linked-brushing pop and can compose (a rect drag then a shift+click adds one more cell).
#
# Body: {projectUid, imageUid, valueName, popType, t, z, x1, y1, x2, y2, mode?}
#   (x1, y1) / (x2, y2) are the low/high corners in IMAGE PIXEL coords (client normalises before
#   POST). z is the plane the rect was drawn on; a future z-window feature will multi-plane it.
#   mode = 'replace' | 'add' | 'toggle' (as pick-cell).
#
# Reads the mask over the rect in ONE `read_slab` call — same reader the pixel path uses, so the
# rect obeys the same axis + level conventions. Labels are dedup'd server-side (a cell straddling
# many voxels contributes one label).
function api_viewer_pick_rect(body_bytes::Vector{UInt8})
    body = JSON3.read(body_bytes, Dict{String,Any})
    pu   = String(get(body, "projectUid", ""))
    iu   = String(get(body, "imageUid", ""))
    pt   = String(get(body, "popType", "flow"))
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    vn   = _resolve_vn(img, String(get(body, "valueName", "")))
    zp, lerr = label_store_path(pu, iu, vn)
    zp === nothing && return 404, JSON3.write((; error = lerr))
    tint = _to_int(get(body, "t", 0))
    zint = _to_int(get(body, "z", 0))
    x1   = _to_int(get(body, "x1", 0));  x2 = _to_int(get(body, "x2", 0))
    y1   = _to_int(get(body, "y1", 0));  y2 = _to_int(get(body, "y2", 0))
    # Normalise (client should already have done this, but a rect drawn from lower-right can arrive
    # inverted through JSON coercion / a client bug — one dedupe here is cheaper than debugging a
    # store read that returned zero rows).
    xlo = min(x1, x2); xhi = max(x1, x2)
    ylo = min(y1, y2); yhi = max(y1, y2)
    # `level` matches the LOD the viewer is DISPLAYING — see the pick-cell endpoint's note.
    lvl_req = _to_int(get(body, "level", 0))
    nlvl    = something(let l = store_pyramid_levels(String(zp)); l === nothing ? nothing : length(l) end, 1)
    lvl     = clamp(lvl_req, 0, nlvl - 1)
    labels_uniq = try
        vol, _, _, _ = read_slab(String(zp), tint, 0; z = zint, x = xlo:xhi, y = ylo:yhi, level = lvl)
        # `vol` is `(x, y, z)` column-major, one voxel per pixel of the rect (z drops because zint
        # is an Int). Flatten + unique + drop 0 (background). Keep as Int for JSON.
        Int[Int(l) for l in unique(vec(vol)) if l != 0]
    catch e
        return 500, JSON3.write((; error = "rect read failed: " * sprint(showerror, e)))
    end
    mode = String(get(body, "mode", "replace"))
    cur  = something(_get_napari_selection(img._dir, vn), Int[])
    labs = if mode == "add"
        collect(union(Set(cur), Set(labels_uniq)))
    elseif mode == "toggle"
        s = Set(cur); for l in labels_uniq; l in s ? delete!(s, l) : push!(s, l); end
        collect(s)
    else
        labels_uniq
    end
    _set_napari_selection!(img._dir, vn, labs)
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_napari_pop!(m, img)
    _broadcast_popmap(pu, iu, vn, pt, m)
    200, JSON3.write((; nLabels = length(labels_uniq), nSelected = length(labs)))
end

# ── POST /api/viewer/record-test ──────────────────────────────────────────────────
#
# A SMOKE-TEST route that produces an mp4 through renderer C so the pipeline can be eyeballed before
# the movie rail (`handle_movie_record` / `run_single_movie`) is migrated off napari. Blocking, not
# rail-integrated — the record runs on the request thread, cancellation is not offered and progress is
# not streamed. Kept out of the parity checklist for that reason.
#
# What it exercises:
#   * `resolve_image_version` → same active-vs-explicit rule the meta route uses,
#   * `resolved_display_specs` from the SAVED viewer props (sampled fallback), so the mp4's colours
#     match what the browser drew,
#   * `record_view_movie` end-to-end (frame render → raw temp → `encode_movie_run.py`),
#   * optional `title_card` — passes through to the shared prepend helper.
#
# `maxFrames` caps the sweep (default 30 — a smoke test should not wait minutes). Absent overlays: the
# author that resolves populations / centroids / tracks into the primitives' columnar shape is the
# next chunk, and its output will need Dominik's eyes on real cells rather than a green frame.
#
# `POST /api/viewer/record-test` — body: `{ projectUid, imageUid, valueName?, ts?: [start, end],
# z?: int, titleCard?, maxFrames?: 30, overlays?: { popType?: "flow", valueName?, popPaths?: [...],
# pointSizePx?: 6, segmentWidthPx?: 2, tailLength?: 30, includeTracks?: true } }`
# — response: `{ ok, path, filename, frames, width, height }`.
function api_viewer_record_test(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)) catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))
    pu = String(get(data, :projectUid, ""))
    iu = String(get(data, :imageUid, ""))
    (isempty(pu) || isempty(iu)) &&
        return 400, JSON3.write((; error = "projectUid and imageUid required"))
    vn_raw = get(data, :valueName, nothing)
    vnn = (vn_raw === nothing || String(vn_raw) == "") ? nothing : String(vn_raw)
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    # Specs — saved viewer props if present, else sampled. Sampled has the frame-flicker property
    # (decision 5), so a movie without saved props is louder than one with them; the test is a smoke
    # check, so this is a live limitation to flag not a defect to fix here.
    arr, caxes = open_level0(zp)
    nc  = try
        d = axis_dims(caxes, ndims(arr))
        haskey(d, "c") ? size(arr, d["c"]) : 1
    catch; 1 end
    props = _props_path(td, zp)
    specs = resolved_display_specs(props, nc)
    specs === nothing && (specs = resolved_display_specs(_sampled_specs(zp, nc)))
    # Timepoint range. `ts = [start, end]` inclusive, both 0-based to match the browser's slab route;
    # missing = all frames. Capped by `maxFrames` so a 200-frame movie is not the default surprise.
    ts_raw = get(data, :ts, nothing)
    ts = if ts_raw isa AbstractVector && length(ts_raw) == 2
        collect(Int(ts_raw[1]):Int(ts_raw[2]))
    else
        nothing
    end
    max_frames = Int(get(data, :maxFrames, 30))
    if ts !== nothing && length(ts) > max_frames
        ts = ts[1:max_frames]
    end
    z_raw = get(data, :z, nothing)
    z = z_raw === nothing ? nothing : Int(z_raw)
    tc_raw = get(data, :titleCard, nothing)
    title_card = tc_raw isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in tc_raw) : nothing

    # Optional P3 overlays. The author resolves populations/tracks into the primitives' columnar
    # shape once, and hands back a per-t closure — a movie of a gated experiment carries its
    # annotations rather than raw channels. `overlays.valueName` may differ from the frame's
    # `valueName` (a movie of processed image `A` with pops from segmentation `B`).
    ov_raw = get(data, :overlays, nothing)
    overlays_for = nothing
    point_size_px = 6; segment_width_px = 2
    # Diagnostics returned in the response so a smoke test can tell whether the AUTHOR engaged
    # and whether ANY frame carried a point/segment. A movie with no overlays is otherwise
    # indistinguishable from a movie whose overlays fell outside the drawn frame.
    ov_diag = Dict{String,Any}("requested" => ov_raw !== nothing, "reason" => "")
    if ov_raw isa AbstractDict
        ov_vn = String(get(ov_raw, :valueName, ""))
        # If the overlay caller did not name a segmentation, fall back to the ONE saved for the
        # frame (`vnn`) — a movie of the active segmentation is the smoke test's expected case.
        ov_vn = isempty(ov_vn) ? something(vnn, "") : ov_vn
        ov_pt = String(get(ov_raw, :popType, "flow"))
        ov_paths_raw = get(ov_raw, :popPaths, nothing)
        ov_paths = ov_paths_raw isa AbstractVector ?
                   String[String(p) for p in ov_paths_raw] : nothing
        include_tracks = Bool(get(ov_raw, :includeTracks, true))
        # `tailLength` in FRAMES — napari's `tail_length`, default 30, `0` hides tracks entirely
        # (same as `includeTracks = false`). Matches the browser's `viewerTailLength` setting so a
        # movie recorded from a look and a movie recorded from record-test read the same.
        tail_length      = Int(get(ov_raw, :tailLength, 30))
        point_size_px    = Int(get(ov_raw, :pointSizePx, point_size_px))
        segment_width_px = Int(get(ov_raw, :segmentWidthPx, segment_width_px))
        ov_diag["valueName"] = ov_vn
        ov_diag["popType"]   = ov_pt
        # Build the transform against the NATIVE frame size — same H/W the sweep will see.
        # `record_view_movie` here uses default crop/max_px (no crop, no downsample); if the smoke
        # route ever grows a crop, the transform picks it up automatically.
        img, gerr = _gating_image(pu, iu)
        if gerr !== nothing
            ov_diag["reason"] = "gating image lookup failed"
        elseif isempty(ov_vn)
            ov_diag["reason"] = "no valueName resolved"
        elseif !_has_label_props(img)
            ov_diag["reason"] = "image has no labelProps"
        else
            d = axis_dims(caxes, ndims(arr))
            H = haskey(d, "y") ? size(arr, d["y"]) : 0
            W = haskey(d, "x") ? size(arr, d["x"]) : 0
            ov_diag["frameH"] = H; ov_diag["frameW"] = W
            if H == 0 || W == 0
                ov_diag["reason"] = "could not resolve y/x axes from caxes ($(caxes))"
            else
                tf = pixel_transform(H, W; crop = nothing, max_px = 0)
                inner = try
                    build_overlays_for(img; value_name = ov_vn, pop_type = ov_pt,
                                       transform = tf, pops_filter = ov_paths,
                                       include_tracks = include_tracks,
                                       tail_length = tail_length)
                catch e
                    ov_diag["reason"] = "author threw: $(sprint(showerror, e))"
                    @warn "record-test: overlay author failed" value_name = ov_vn pop_type = ov_pt exception = e
                    nothing
                end
                if inner !== nothing
                    # Tally per-frame counts through a wrapper closure. The tally is cheap
                    # (one integer per frame) and answers "did overlays fire?" without a
                    # second inspection route.
                    pts_seen = Ref(0); segs_seen = Ref(0); frames_touched = Ref(0)
                    overlays_for = function(t::Int)
                        p, s = inner(t)
                        p === nothing || (pts_seen[]  += length(p.x))
                        s === nothing || (segs_seen[] += length(s.x0))
                        frames_touched[] += 1
                        (p, s)
                    end
                    ov_diag["_tally"] = (pts_seen, segs_seen, frames_touched)
                    isempty(ov_diag["reason"]) && (ov_diag["reason"] = "ok")
                end
            end
        end
    end

    # Filename picked here rather than by the caller: `_valid_movie_name` is what `/api/movies` filters
    # by, so a smoke movie has to sort with the others without a slash or a dot-tmp fragment.
    filename = "smoketest_" * iu * ".mp4"
    out_dir  = _movies_dir_for_project(pu)
    mkpath(out_dir)
    out_path = joinpath(out_dir, filename)
    result = try
        record_view_movie(zp, out_path; ts = ts, channels = 0:(nc - 1), specs = specs,
                          z = z, title_card = title_card,
                          overlays_for = overlays_for,
                          point_size_px = point_size_px,
                          segment_width_px = segment_width_px)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    # Bank it into the registry so it shows up on /movies with a producer tag — the same one-line
    # write the batch/animation paths do, so `startedAt`/`imageUid` are set. No config kind: this is a
    # smoke test, not something the movie editor should try to reopen for edit.
    register_movie!(pu, filename; produced_by = "smoketest", image_uid = iu)
    # Fold the tally back into the diagnostic block before returning — the Ref values are read
    # AFTER the sweep completes, so a caller can see how many points/segments the author actually
    # emitted across the whole movie.
    if haskey(ov_diag, "_tally")
        pts_ref, segs_ref, frames_ref = ov_diag["_tally"]
        ov_diag["pointsDrawn"]   = pts_ref[]
        ov_diag["segmentsDrawn"] = segs_ref[]
        ov_diag["framesCalled"]  = frames_ref[]
        delete!(ov_diag, "_tally")
    end
    200, JSON3.write((; ok = true, path = result.path, filename = filename,
                        frames = result.frames, width = result.width, height = result.height,
                        overlays = ov_diag))
end
