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
    read_slab(zarr_path, t, c; z = nothing) -> (vol, nx, ny, nz)

Voxels of timepoint `t`, channel `c` (both 0-based) as an `(x, y, z)` column-major array — i.e. one
whose linear memory is x-fastest, which is what a WebGPU 3D texture takes. Missing axes count as 1, so
a 2D single-channel still answers the same shape of question as a 5D movie.

`z` (0-based) reads ONE PLANE instead of the whole stack, and it is the difference between a timecourse
you can watch and one you wait on. Measured on `Dml3RG` (37 z, 4 ch, 181 t): a whole timepoint is
326 MB and ~400 ms of server read, one plane is 8.8 MB and ~13-22 ms. More to the point, the whole
181-timepoint movie is 1.59 GB at one plane against 59 GB at full depth — so it FITS in a VRAM budget,
and the second pass through it is entirely cache hits. This is the view Dominik actually uses for a
timecourse (2026-08-24); the volume MIP is for looking at structure, not for playing.

Pixels go through `read_native`, never `arr[...]`: a raw `bioformats2raw` store is big-endian and
Zarr.jl does not swap it (see `image_geometry.jl`).
"""
function read_slab(zarr_path::AbstractString, t::Int, c::Int; z::Union{Int,Nothing} = nothing)
    arr, caxes = open_level0(zarr_path)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    names = caxes_or_fallback(caxes, nd)

    idx = Any[Colon() for _ in 1:nd]
    haskey(dims, "t") && (idx[dims["t"]] = t + 1)      # 0-based → 1-based; scalar, so the dim drops
    haskey(dims, "c") && (idx[dims["c"]] = c + 1)
    # A scalar z drops the z dim exactly as t and c do, so `kept` below excludes it and `nz` answers 1
    # — one code path serves a plane and a volume rather than two that can disagree.
    if z !== nothing && haskey(dims, "z")
        nz_all = size(arr, dims["z"])
        idx[dims["z"]] = clamp(z, 0, nz_all - 1) + 1
    end
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
#    voxelUm: [x, y, z], calibrated: {xy, z},
#    channels: [{name, lo, hi, visible, lut: [[r,g,b], ...]}]}
#
# Everything the renderer needs BEFORE it asks for a single voxel: how many slabs exist, how big one
# is (the client sizes its VRAM cache from `slabBytes`), and how to colour each channel. `lut` is a
# black→colour (or white→colour) ramp the client uploads as a small 1D lookup texture; resolving it
# here rather than in TypeScript is what keeps napari's ~30 colormaps from being re-guessed by name.
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

        # One `init_object` for both facts it can answer: the display names and the calibration.
        names, vox, cal = try
            img = init_object(pu, iu)
            sizes, _ = img_physical_sizes(img)         # [sz, sy, sx] um/px
            ax = img_scale_axes(img)
            (something(channel_names(img; value_name = vnn), String[]),
             [sizes[3], sizes[2], sizes[1]],           # → [x, y, z], the renderer's axis order
             (; xy = :XY in ax, z = :Z in ax))
        catch
            (String[], [1.0, 1.0, 1.0], (; xy = false, z = false))
        end
        channels = [(; name = get(names, c, "Channel $(c - 1)"),
                       lo = s.lo, hi = s.hi, visible = s.visible,
                       lut = [[l[1], l[2], l[3]] for l in s.lut])
                    for (c, s) in enumerate(specs)]

        200, JSON3.write((; nT = nt, nC = nc, nZ = nz, nX = nx, nY = ny,
                            bytesPerVoxel = bpv, slabBytes = nx * ny * nz * bpv,
                            contrastSource = src, voxelUm = vox, calibrated = cal, channels))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── GET /api/viewer/slab ──────────────────────────────────────────────────────────
# Served from the STREAM handler, not the route table, because it needs headers of its own —
# `Content-Encoding` and the shape guard. Same idiom as `try_serve_movie`. Returns false when this is
# not a slab request or the image cannot be resolved, and the caller falls through to the 404 path.
#
# ?projectUid=&imageUid=&valueName=&t=&c=&z=&enc=identity|zstd → raw little-endian voxels.
#
# `z` omitted → the whole stack (the 3D MIP view). `z=N` → that one plane (the 2D view), which is what
# makes a timecourse playable: 8.8 MB and ~13 ms against 326 MB and ~400 ms on `Dml3RG`.
#
# `enc` is the CLIENT's choice and defaults to identity, deliberately — it is not `Accept-Encoding`
# negotiation. A browser always advertises zstd, so negotiating would compress unconditionally, and
# compression measured 195-208 ms per timepoint against a 330 ms read: a third of the server's time
# spent shrinking bytes that a loopback socket moves for free. It is the remote-VM case that wants it,
# and only the client knows which case it is in.
function try_serve_slab(stream::HTTP.Stream, target::AbstractString)::Bool
    q  = HTTP.queryparams(HTTP.URI(target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return false
    t = something(tryparse(Int, get(q, "t", "0")), 0)
    c = something(tryparse(Int, get(q, "c", "0")), 0)
    z = haskey(q, "z") ? tryparse(Int, q["z"]) : nothing
    enc = get(q, "enc", "identity")

    # A read failure has to arrive as a STATUS, not as an exception. This runs before `startwrite`, so
    # there is still a response to shape — once bytes are on the wire the only thing left is a broken
    # connection, which the client cannot tell from a network blip. An out-of-range `t` or `c` is the
    # ordinary case (a hand-edited URL, a stale slider bound) and answers 400 with the actual bound.
    local body, nx, ny, nz, bpv, read_ms, comp_ms
    try
        t0 = time()
        vol, nx, ny, nz = read_slab(zp, t, c; z = z)
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
    HTTP.setheader(stream, "X-Server-Read-Ms"     => string(read_ms))
    HTTP.setheader(stream, "X-Server-Compress-Ms" => string(comp_ms))
    HTTP.setheader(stream, "Access-Control-Allow-Origin"   => "*")
    HTTP.setheader(stream, "Access-Control-Expose-Headers" =>
                   "X-Slab-Shape, X-Slab-Bpv, X-Server-Read-Ms, X-Server-Compress-Ms")
    enc == "zstd" && HTTP.setheader(stream, "Content-Encoding" => "zstd")
    HTTP.setheader(stream, "Content-Length" => string(length(body)))
    HTTP.setstatus(stream, 200)
    HTTP.startwrite(stream)
    write(stream, body)
    true
end
