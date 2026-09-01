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
using PNGFiles

# ── Reading one volume ────────────────────────────────────────────────────────────

"""
    read_slab(zarr_path, t, c; z = nothing, x = nothing, y = nothing, level = 0) -> (vol, nx, ny, nz, nc)

Voxels of timepoint `t`, channel `c` (both 0-based) as an `(x, y, z)` — or `(x, y, z, c)` when `c` is
a range — column-major array. Linear memory is x-fastest, which is what a WebGPU 3D texture takes.
Missing axes count as 1, so a 2D single-channel still answers the same shape of question as a 5D movie.

`z` and `c` (both 0-based) select the depth and channel, and WHICH KIND decides the rank of the answer:

  - an `Int` reads ONE PLANE / one channel and drops the dim, exactly as `t` does → `nz == 1` /
    `nc == 1`;
  - a `UnitRange` reads a SLAB of that many planes / channels and keeps it → an extra output dim.

`c` as a range is what the brick-atlas viewer uses (`docs/todo/KILN_BRICK_PLAN.md` → Decision 7).
Measured cost of ONE brick × 38 channels serially over HTTP was 273 ms on SispLk; batching them
into one request drops the per-request overhead to a single round trip. Scalar `c` is the flat
atlas's path — kept for backward compat, unchanged output shape (`(vol, nx, ny, nz)` destructures
fine because the extra `nc` is on the end).

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
read_slab(zarr_path::AbstractString, t::Int, c::Union{Int,AbstractUnitRange{Int}};
          z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
          x::Union{AbstractUnitRange{Int},Nothing} = nothing,
          y::Union{AbstractUnitRange{Int},Nothing} = nothing,
          level::Int = 0) =
    read_slab(open_level(zarr_path, level)..., t, c; z = z, x = x, y = y)

function read_slab(arr, caxes, t::Int, c::Union{Int,AbstractUnitRange{Int}};
                   z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
                   x::Union{AbstractUnitRange{Int},Nothing} = nothing,
                   y::Union{AbstractUnitRange{Int},Nothing} = nothing)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    names = caxes_or_fallback(caxes, nd)

    idx = Any[Colon() for _ in 1:nd]
    haskey(dims, "t") && (idx[dims["t"]] = t + 1)      # 0-based → 1-based; scalar, so the dim drops
    # Scalar `c` drops the dim (flat atlas path); range set below with _clamp_range once defined.
    haskey(dims, "c") && c isa Int && (idx[dims["c"]] = c + 1)
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
    # `c` as a range: brick-atlas viewer wants all channels for one brick in ONE request. Clamped
    # here so the c handling reads the same as z's. Scalar c was already set above.
    if !(c isa Int) && haskey(dims, "c")
        idx[dims["c"]] = _clamp_range(c, size(arr, dims["c"]))
    end
    y === nothing || !haskey(dims, "y") ||
        (idx[dims["y"]] = _clamp_range(y, size(arr, dims["y"])))
    x === nothing || !haskey(dims, "x") ||
        (idx[dims["x"]] = _clamp_range(x, size(arr, dims["x"])))
    sub = read_native(arr, idx...)

    # Julia dim j carries the C-order axis at position nd-j+1. Scalar indexing dropped t (and c,
    # unless c is a range), so rebuild the surviving names in Julia dim order and permute them to
    # exactly (x, y, z, c). Scalar-c stores end at (x, y, z); range-c stores add c as the last dim
    # — the brick atlas expects channels stacked along z as a single texture, and the client can
    # reinterpret a (x, y, z*nc, 1) view over a (x, y, z, c) buffer without copying (nc groups of
    # nz consecutive planes, cache-friendly), so putting c LAST is the choice that matches Kiln's
    # z-stacked convention (KILN_BRICK_PLAN.md → Decision 4).
    kept = String[names[nd - j + 1] for j in 1:nd if !(idx[j] isa Int)]
    order = Int[]
    for want in ("x", "y", "z", "c")
        k = findfirst(==(want), kept)
        k === nothing || push!(order, k)
    end
    # `order == 1:n` for every store we have (x IS Julia dim 1 there), and `permutedims` copies even
    # for the identity — 87 MB per slab on the real target. Skip it, but keep the general path.
    vol = (ndims(sub) != length(order) || order == collect(1:length(order))) ? sub :
          permutedims(sub, order)
    nx = size(vol, 1)
    ny = ndims(vol) >= 2 ? size(vol, 2) : 1
    nz = ndims(vol) >= 3 ? size(vol, 3) : 1
    nc = ndims(vol) >= 4 ? size(vol, 4) : 1
    vol, nx, ny, nz, nc
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
        # frame interval, the unit the scale bar is labelled in, and the IMAGE NAME (for the window
        # title — the client used to carry `name` in the URL query but the server owns it, so meta
        # is the one source and the URL only carries identity: project + image + optional vn).
        names, vox, cal, unit, tmin, image_name = try
            img = init_object(pu, iu)
            sizes, ts = img_physical_sizes(img)        # [sz, sy, sx] um/px, minutes/frame
            ax = img_scale_axes(img)
            has_t = :T in ax
            (something(channel_names(img; value_name = vnn), String[]),
             [sizes[3], sizes[2], sizes[1]],           # → [x, y, z], the renderer's axis order
             (; xy = :XY in ax, z = :Z in ax, t = has_t),
             _meta_str(img.meta, "PhysicalSizeUnit"),
             has_t ? ts : nothing,
             img.name)
        catch
            (String[], [1.0, 1.0, 1.0], (; xy = false, z = false, t = false), nothing, nothing, "")
        end
        # The set this image belongs to, for per-set viewer prefs (contrast, colour-by, point size).
        # Used to travel in the URL as `set=…`; moved server-side so the URL shrinks to identity.
        # Picks the first set containing the image — an image can, in principle, live in multiple
        # sets, but the URL only ever carried one and the client picks the first one it sees.
        set_uid = try
            proj = load_project(pu)
            s = findfirst(s -> iu in s.image_uids, sets(proj))
            s === nothing ? "" : sets(proj)[s].uid
        catch
            ""
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
        200, JSON3.write((; nT = nt, nC = nc, nZ = nz, nX = nx, nY = ny,
                            name = image_name, setUid = set_uid,
                            labelNames = label_names,
                            valueNames = value_names,
                            valueName = vnn === nothing ? active_vn : vn,
                            # The ACTIVE one regardless of what was asked for, so a picker can say
                            # whether the version on screen is the one every task runs against. With
                            # only `valueName` an explicit request echoes itself and the comparison is
                            # impossible.
                            activeValueName = active_vn,
                            # The store the browser viewer is looking at and the image's meta dir.
                            # Body-carried into `/api/preview/run` (P7): the preview API uses these
                            # as source of truth for "what's on screen".
                            zarrPath = zp,
                            taskDir  = td,
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
    #
    # `preview=1` (P7) retargets a labels request to the scratch preview store the task-preview
    # worker just wrote — `<vn>__preview.ome.zarr` instead of `<vn>.ome.zarr` — riding the same
    # reader and headers as the real labels slab. The preview store is deliberately full-image
    # geometry, so the coordinate math the reader does is identical; only the file path differs.
    #
    # `preview_af=1&sourceChannel=N` (P7.1) is the same idea for an IMAGE channel: the AF worker
    # writes `{img_dir}/{vn}__preview_af_ch{N}.ome.zarr` per corrected channel, and this flag flips
    # the read for that one channel onto that store while the other channels keep coming from the
    # source image. `sourceChannel` is the source-image channel index the corrected store REPLACES —
    # the FE swaps channel-by-channel URLs based on which channels were corrected. `valueName` here
    # is the AF task's `outputValueName` (the write side's target version), NOT `c`.
    preview_labels = get(q, "preview", "") == "1"
    preview_af     = get(q, "preview_af", "") == "1"
    lbl = get(q, "labels", "")
    if !isempty(lbl)
        if preview_labels
            zpp, perr = preview_labels_store_path(
                get(q, "projectUid", ""), get(q, "imageUid", ""), lbl)
            perr === nothing || return false
            zp = zpp
        else
            zp, lerr = label_store_path(get(q, "projectUid", ""), get(q, "imageUid", ""), lbl)
            lerr === nothing || return false
        end
    else
        zp, meta_dir, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
        err === nothing || return false
        if preview_af
            # `sourceChannel` must be present (the store is per-channel) and `previewValueName` names
            # the AF TASK's `outputValueName` — the same string the worker's `_stage_af_image_store`
            # used. Kept separate from `valueName`: `valueName` is what `resolve_image_version` above
            # just used to find the SOURCE image; `previewValueName` names the AF write, which is
            # usually equal but need not be — a first-run AF task writes an unregistered
            # `outputValueName` that has no image version of its own.
            #
            # The scratch sits at `{img_meta_dir}/{af_vn}__preview_af_ch{N}.ome.zarr` — the SAME
            # `task_dir` the worker knows (`msg['taskDir']`). NOT `dirname(zp)`: the image data
            # lives under `{proj}/0/{uid}` while the meta dir is `{proj}/1/{uid}`, and `zp` here
            # resolves to the data side. `resolve_image_version` returns the meta dir directly, so
            # take it from there rather than reconstruct the path.
            src_ch = tryparse(Int, get(q, "sourceChannel", ""))
            src_ch === nothing && return false
            af_vn = get(q, "previewValueName", "")
            isempty(af_vn) && return false
            zp = joinpath(meta_dir, "$(af_vn)__preview_af_ch$(src_ch).ome.zarr")
            # A stale AF store from a prior preview is swept on cleanup; a missing store here is a
            # normal race (the FE fetched before the worker's promote landed) and 404s so the
            # browser can retry rather than serve the source image and hide the desync.
            isdir(zp) || return false
        end
    end
    t = something(tryparse(Int, get(q, "t", "0")), 0)
    c0 = something(tryparse(Int, get(q, "c", "0")), 0)
    c1 = haskey(q, "cTo") ? tryparse(Int, q["cTo"]) : nothing
    # `cTo` present promotes `c` to a range — the brick-atlas viewer fetches all channels for one
    # brick in one request (KILN_BRICK_PLAN.md → Decision 7). Ordered here, while the two ends
    # are still separate integers, for the same reason as z: Julia normalises a backwards
    # UnitRange to empty. Scalar `c` (no `cTo`) is the flat atlas's path, unchanged.
    c = c1 === nothing ? c0 : min(c0, c1):max(c0, c1)
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
    local body, nx, ny, nz, nc, bpv, read_ms, comp_ms
    try
        t0 = time()
        vol, nx, ny, nz, nc = read_slab(zp, t, c; z = z, x = xr, y = yr, level = level)
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
    # X-Slab-Shape: nc,nz,ny,nx when the request has a `cTo` (channels axis is kept), otherwise
    # nz,ny,nx (the legacy scalar-c shape, unchanged for the flat atlas). The client asserts this
    # against `meta`, so a store whose axes are not what we think fails LOUDLY. Silently
    # transposed voxels still render; they just render the wrong thing.
    HTTP.setheader(stream, "X-Slab-Shape"   =>
                   c isa Int ? "$nz,$ny,$nx" : "$nc,$nz,$ny,$nx")
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

"""
    preview_labels_store_path(project_uid, image_uid, value_name) -> (path, err)

The scratch preview labels store for one segmentation. Keyed only on the vn — the worker's convention
is fixed at `<img_labels_dir>/<vn>__preview.ome.zarr`, independent of ccid.json filename registration.

Deliberately NOT `label_store_path`: that helper enforces `haskey(img.labels, vn)`, which excludes a
FIRST-TIME segmentation preview — the exact case preview is most useful for. The scratch store lives
by convention, not by registration.
"""
function preview_labels_store_path(project_uid::AbstractString, image_uid::AbstractString,
                                   value_name::AbstractString)
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return (nothing, "image not found")
    vn = isempty(value_name) ? "" : String(value_name)
    isempty(vn) && return (nothing, "value_name required")
    zp = joinpath(img_labels_dir(img), "$(vn)__preview.ome.zarr")
    isdir(zp) || return (nothing, "preview labels store not on disk: $(basename(zp))")
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
            # `hasTracks` is a DATA fact ("does this pop hold any tracked cells right now") emitted
            # alongside the typed `isTrack` flag ("was this pop authored as a track-family pop"). The
            # WebGPU viewer treats a pop as ribbon-drawable when `isTrack || hasTracks` — see
            # MULTI_POP_TRACKING_PLAN.md Decision 2 + P3. Defaulting to `false` on legacy servers is
            # safe: the viewer falls back to today's isTrack-only behaviour.
            [(; path = p.path, name = p.name, colour = p.colour, show = p.show,
                isTrack = p.is_track, hasTracks = get(p, :has_tracks, false),
                labels = p.labels)
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
# Writes into the pick-selection registry (`gating_api.jl` → *Pick-selection registry*) which
# feeds the linked-brushing pop — same JSON tree, same broadcast, same colour, no client-side
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
    cur  = something(_get_pick_selection(img._dir, vn), Int[])
    labs = if mode == "add"
        label in cur ? cur : vcat(cur, label)
    elseif mode == "toggle"
        label in cur ? filter(!=(label), cur) : vcat(cur, label)
    else
        Int[label]
    end
    _set_pick_selection!(img._dir, vn, labs)
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_pick_pop!(m, img)
    _broadcast_popmap(pu, iu, vn, pt, m)
    200, JSON3.write((; label, nSelected = length(labs)))
end

# ── POST /api/viewer/pick-rect (P8 rectangle drag) ────────────────────────────────
# Drag a rectangle in the viewer → all cells whose mask intersects that XY box at (t, z) become the
# transient population. Same registry / broadcast path as `api_viewer_pick_cell`, so the two share
# the linked-brushing pop and can compose (a rect drag then a shift+click adds one more cell).
#
# Body: {projectUid, imageUid, valueName, popType, t, z, x1, y1, x2, y2, mode?, zLo?, zHi?}
#   (x1, y1) / (x2, y2) are the low/high corners in IMAGE PIXEL coords (client normalises before
#   POST). z is the plane the rect was drawn on. When BOTH `zLo` and `zHi` are supplied (`slice`
#   scope in the gating store's cell-selection tools), the read spans that inclusive z-range
#   instead of a single plane — the multi-plane path CellSelectionTools' "Z ±N" writes into. When
#   they are absent, the reader reads one plane (`z`), same as before.
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
    # Z range: an Int for one plane, or a UnitRange for `slice ± N`. Clamp to `[0, nZ - 1]` after
    # reading the store's z dim — a stale client sending a range past the top of the stack would
    # otherwise land on `read_slab`'s error path. `nZ` is available via `open_level` here but we
    # avoid opening twice: `read_slab` handles clipping when the range is inside; a caller-provided
    # range that goes negative or wraps is caller-side wrong and returns no rows either way.
    zsel_raw = get(body, "zLo", nothing)
    zsel = if zsel_raw !== nothing && get(body, "zHi", nothing) !== nothing
        zlo = _to_int(zsel_raw); zhi = _to_int(get(body, "zHi", 0))
        zlo <= zhi ? (zlo:zhi) : (zhi:zlo)
    else
        zint
    end
    labels_uniq = try
        vol, _, _, _ = read_slab(String(zp), tint, 0; z = zsel, x = xlo:xhi, y = ylo:yhi, level = lvl)
        # `vol` is `(x, y, z)` column-major, one voxel per pixel of the rect (z drops when `zsel` is
        # an Int; stays as a dim when `zsel` is a range). Flatten + unique + drop 0 (background).
        # Keep as Int for JSON.
        Int[Int(l) for l in unique(vec(vol)) if l != 0]
    catch e
        return 500, JSON3.write((; error = "rect read failed: " * sprint(showerror, e)))
    end
    mode = String(get(body, "mode", "replace"))
    cur  = something(_get_pick_selection(img._dir, vn), Int[])
    labs = if mode == "add"
        collect(union(Set(cur), Set(labels_uniq)))
    elseif mode == "toggle"
        s = Set(cur); for l in labels_uniq; l in s ? delete!(s, l) : push!(s, l); end
        collect(s)
    else
        labels_uniq
    end
    _set_pick_selection!(img._dir, vn, labs)
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_pick_pop!(m, img)
    _broadcast_popmap(pu, iu, vn, pt, m)
    200, JSON3.write((; nLabels = length(labels_uniq), nSelected = length(labs)))
end

# ── POST /api/viewer/overlay-legend (P9) ──────────────────────────────────────────
# Legend content for a captured viewState (analysis-board strip + movie title card). Replaces
# `/api/napari/overlay-legend` — the computation itself is pure Julia (walks pop maps, resolves
# colour-by categories against populations), no napari canvas required, so this reuses the same
# `overlay_legend_content` the napari route did.
#
# Body: `{projectUid, imageUid, colourBy?, overlayPops?, colourOverrides?}`
#   overlayPops = `[{valueName, popType, path}, …]` parsed from the snapshot's overlay layer names.
# Response: `{ok, colourBy, populations}` — same shape captureViewLegend consumes.
function api_viewer_overlay_legend(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    column      = String(get(data, :colourBy, ""))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    content = overlay_legend_content(img, column, get(data, :overlayPops, nothing),
                                     get(data, :colourOverrides, nothing))
    200, JSON3.write((; ok = true, colourBy = content.colourBy, populations = content.populations))
end

# ── POST /api/viewer/pick-clear (P9) ──────────────────────────────────────────────
# Empty the transient cell-selection pop for (image, valueName, popType). Same registry /
# broadcast path as pick-cell / pick-rect (they all `_set_pick_selection!` + `_inject_pick_pop!`
# + broadcast); clearing is `_set_pick_selection!` with an empty label list.
#
# Body: {projectUid, imageUid, valueName?, popType?} — same shape as pick-cell / pick-rect so a
# frontend caller can reuse its existing body builder. Response: {nSelected: 0}.
function api_viewer_pick_clear(body_bytes::Vector{UInt8})
    body = JSON3.read(body_bytes, Dict{String,Any})
    pu   = String(get(body, "projectUid", ""))
    iu   = String(get(body, "imageUid", ""))
    pt   = String(get(body, "popType", "flow"))
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    vn   = _resolve_vn(img, String(get(body, "valueName", "")))
    _set_pick_selection!(img._dir, vn, Int[])
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_pick_pop!(m, img)                               # no-op now (selection gone)
    _broadcast_popmap(pu, iu, vn, pt, m)
    200, JSON3.write((; nSelected = 0))
end

# ── Shared: request-overlay dict → (overlays_for, mask_for) closures ─────────────
#
# Every offline-renderer entry point — the smoke route below AND the movie rail (`run_single_offline` /
# `run_batch_offline` in `movie_rail.jl`) — reads the SAME overlay/mask spec off the request and hands
# it to the SAME `build_overlays_for` / `build_mask_for` authors. One mapping, so a movie recorded from
# the record button and a movie recorded from `/api/viewer/record-test` speak the same language.
#
# `img_err` is `_gating_image`'s error string (or `nothing`). Returns `(overlays_for, mask_for,
# point_size_px, segment_width_px, mask_contour_px, ov_diag, mask_diag)`. `ov_diag` / `mask_diag` carry
# the smoke test's diagnostic breadcrumbs (populated whether or not `tally` is on); with `tally = true`
# the returned closures additionally count points/segments/frames drawn into refs stashed under
# `ov_diag["_tally"]` / `mask_diag["_tally"]` — the smoke route unwraps them into its response body.
function _resolve_movie_overlays_mask(img, img_err, arr, caxes, ov_raw, vnn;
                                      z::Union{Int,Nothing} = nothing,
                                      crop = nothing, max_px::Int = 0,
                                      tally::Bool = false)
    ov_diag = Dict{String,Any}("requested" => ov_raw !== nothing, "reason" => "")
    mask_diag = Dict{String,Any}("requested" => false, "reason" => "")
    overlays_for = nothing
    mask_for = nothing
    point_size_px = 6; segment_width_px = 2; mask_contour_px = 1
    if !(ov_raw isa AbstractDict)
        return (; overlays_for, mask_for, point_size_px, segment_width_px, mask_contour_px,
                  ov_diag, mask_diag)
    end
    # `ov_raw` reaches us as either symbol- OR string-keyed depending on the caller:
    # `_overlays_raw_from_config` (`movie_rail.jl`) builds a `Dict{String,Any}`, while
    # `record-test`'s smoke route parses JSON via JSON3 which yields symbol keys. Before this helper,
    # every string-keyed field silently missed and returned the default — a `look`-derived movie
    # config would read `showPopulations=true, showMask=false, allCellsColour="#9ca3af"` regardless
    # of what the caller set. Reported 2026-08-31: cpSAM-vs-flowTom rendered pop dots on flowTom
    # (has flow pops) and nothing on cpSAM (no flow pops), with the compare-grid rainbow outline
    # never rendering because `showMask` was silently false. Try both key shapes; symbols first
    # since JSON3 keys arrive that way.
    _ov(d, k::Symbol, default) = begin
        v = get(d, k, nothing)
        v === nothing || return v
        get(d, String(k), default)
    end
    ov_vn = String(_ov(ov_raw, :valueName, ""))
    # If the overlay caller did not name a segmentation, fall back to the ONE saved for the
    # frame (`vnn`) — a movie of the active segmentation is the expected case.
    ov_vn = isempty(ov_vn) ? something(vnn, "") : ov_vn
    ov_pt = String(_ov(ov_raw, :popType, "flow"))
    ov_paths_raw = _ov(ov_raw, :popPaths, nothing)
    ov_paths = ov_paths_raw isa AbstractVector ?
               String[String(p) for p in ov_paths_raw] : nothing
    # `showPopulations` gates the pop-dot build. Absent = true, so `record-test`'s smoke overlays
    # block (which never carried this field) keeps painting pops the way it always has. A
    # `look`-derived dict from `_overlays_raw_from_config` sets it explicitly, so a movie that only
    # asked for a mask stops leaking pop dots the user didn't select (reported by Dominik).
    show_pops = Bool(_ov(ov_raw, :showPopulations, true))
    include_tracks = Bool(_ov(ov_raw, :includeTracks, true))
    # `tailLength` in FRAMES — napari's `tail_length`, default 30, `0` hides tracks entirely
    # (same as `includeTracks = false`). Matches the browser's `viewerTailLength` setting.
    tail_length      = Int(_ov(ov_raw, :tailLength, 30))
    # Whole-segmentation tracks: paint every tracked cell with one default colour, ignoring pops.
    all_tracks       = Bool(_ov(ov_raw, :allTracks, false))
    all_tracks_col   = String(_ov(ov_raw, :allTracksColour, "#9ca3af"))
    # Same three modes the browser's viewer setting exposes: "track" | "speed" | "solid".
    track_color_mode = String(_ov(ov_raw, :trackColorMode, "track"))
    point_size_px    = Int(_ov(ov_raw, :pointSizePx, point_size_px))
    segment_width_px = Int(_ov(ov_raw, :segmentWidthPx, segment_width_px))
    ov_diag["valueName"] = ov_vn
    ov_diag["popType"]   = ov_pt
    ov_diag["allTracks"] = all_tracks
    # `showMask` decides the mask-outline half INDEPENDENTLY of `showPopulations` / `allTracks`.
    # Before this hoist, mask reading lived INSIDE the `else` branch of the pop/track guard, so a
    # mask-only render (showPopulations=false, showMask=true, e.g. the compare-grid rainbow path)
    # never ran build_mask_for and the outline never showed. Reading it up here means the guard
    # covers all three flags, and the mask still fires when only it is on.
    show_mask = Bool(_ov(ov_raw, :showMask, false))
    all_cells = Bool(_ov(ov_raw, :allCells, false))
    mask_diag["requested"] = show_mask
    if img_err !== nothing
        ov_diag["reason"] = "gating image lookup failed"
    elseif isempty(ov_vn)
        ov_diag["reason"] = "no valueName resolved"
    elseif !_has_label_props(img)
        ov_diag["reason"] = "image has no labelProps"
    elseif !(show_pops || all_tracks || show_mask)
        # No overlay type asked for — skip the pop-dot / track / mask build entirely. Before this
        # gate, `build_overlays_for` painted every pop of `pop_type` regardless of `showPopulations`.
        ov_diag["reason"] = "no overlay type requested (showPopulations + allTracks + showMask all false)"
    else
        d = axis_dims(caxes, ndims(arr))
        H = haskey(d, "y") ? size(arr, d["y"]) : 0
        W = haskey(d, "x") ? size(arr, d["x"]) : 0
        ov_diag["frameH"] = H; ov_diag["frameW"] = W
        if H == 0 || W == 0
            ov_diag["reason"] = "could not resolve y/x axes from caxes ($(caxes))"
        else
            tf = pixel_transform(H, W; crop = crop, max_px = max_px)
            if show_pops || all_tracks
                inner = try
                    build_overlays_for(img; value_name = ov_vn, pop_type = ov_pt,
                                       transform = tf, pops_filter = ov_paths,
                                       include_tracks = include_tracks,
                                       tail_length = tail_length,
                                       all_tracks = all_tracks,
                                       all_tracks_colour = all_tracks_col,
                                       track_color_mode = track_color_mode)
                catch e
                    ov_diag["reason"] = "author threw: $(sprint(showerror, e))"
                    @warn "movie overlays: author failed" value_name = ov_vn pop_type = ov_pt exception = e
                    nothing
                end
                if inner !== nothing
                    if tally
                        # Tally per-frame counts through a wrapper closure. Cheap (one integer per
                        # frame) and answers "did overlays fire?" without a second inspection route.
                        pts_seen = Ref(0); segs_seen = Ref(0); frames_touched = Ref(0)
                        overlays_for = function(t::Int)
                            p, s = inner(t)
                            p === nothing || (pts_seen[]  += length(p.x))
                            s === nothing || (segs_seen[] += length(s.x0))
                            frames_touched[] += 1
                            (p, s)
                        end
                        ov_diag["_tally"] = (pts_seen, segs_seen, frames_touched)
                    else
                        overlays_for = inner
                    end
                    isempty(ov_diag["reason"]) && (ov_diag["reason"] = "ok")
                end
            else
                ov_diag["reason"] = "no overlay type requested (showPopulations + allTracks both false)"
            end
            # ── Optional P4 mask outlines. Same transform, same pops_filter, same
            # `allTracks/allCells` split (`allCells` is the mask counterpart). `showMask`
            # is the gate — off by default because it costs one label-store read per frame.
            if show_mask
                all_cells_col = String(_ov(ov_raw, :allCellsColour, "#9ca3af"))
                mask_contour_px = Int(_ov(ov_raw, :maskContourPx, mask_contour_px))
                # colourBy / colourOverrides ride on `ov_raw` (from `_overlays_raw_from_config`) —
                # nothing to do here beyond forwarding; the author does the actual recolour. Empty
                # / missing → pop-derived colours (pre-P5.5 behaviour).
                cb_raw_v = _ov(ov_raw, :colourBy, nothing)
                mask_cb = (cb_raw_v isa AbstractString && !isempty(String(cb_raw_v))) ?
                            String(cb_raw_v) : nothing
                co_raw_v = _ov(ov_raw, :colourOverrides, nothing)
                mask_co = co_raw_v isa AbstractDict ? co_raw_v : nothing
                mask_inner = try
                    build_mask_for(img; value_name = ov_vn, pop_type = ov_pt,
                                   transform = tf, pops_filter = ov_paths,
                                   z = z, all_cells = all_cells,
                                   all_cells_colour = all_cells_col,
                                   colour_by = mask_cb,
                                   colour_overrides = mask_co)
                catch e
                    mask_diag["reason"] = "author threw: $(sprint(showerror, e))"
                    @warn "movie mask: author failed" value_name = ov_vn pop_type = ov_pt exception = e
                    nothing
                end
                if mask_inner !== nothing
                    if tally
                        mask_frames = Ref(0); mask_ids = Ref(0)
                        mask_for = function(t::Int)
                            m, dict = mask_inner(t)
                            if m !== nothing && dict !== nothing
                                mask_frames[] += 1
                                mask_ids[] = length(dict)
                            end
                            (m, dict)
                        end
                        mask_diag["_tally"] = (mask_frames, mask_ids)
                    else
                        mask_for = mask_inner
                    end
                    isempty(mask_diag["reason"]) && (mask_diag["reason"] = "ok")
                    mask_diag["allCells"] = all_cells
                end
            end
        end
    end
    (; overlays_for, mask_for, point_size_px, segment_width_px, mask_contour_px, ov_diag, mask_diag)
end

# ── POST /api/viewer/record-test ──────────────────────────────────────────────────
#
# A SMOKE-TEST route that produces an mp4 through the offline renderer so the pipeline can be
# eyeballed independently of the movie rail (`handle_movie_record` → `run_single_offline` and its
# siblings in `movie_rail.jl`). Blocking, not rail-integrated — the record runs on the request thread,
# cancellation is not offered and progress is not streamed. Kept out of the parity checklist because
# the movie rail already exercises the same wiring end-to-end.
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
# pointSizePx?: 6, segmentWidthPx?: 2, tailLength?: 30, includeTracks?: true,
# allTracks?: false, allTracksColour?: "#9ca3af", trackColorMode?: "track",
# showMask?: false, allCells?: false, allCellsColour?: "#9ca3af", maskContourPx?: 1 } }`
# — response: `{ ok, path, filename, frames, width, height, overlays: {...}, mask: {...} }`.
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

    # Optional P3 overlays + P4 masks. Whole request-dict → closure mapping lives in
    # `_resolve_movie_overlays_mask` so the smoke route and the movie rail share ONE reader.
    # `tally = true` wraps the returned closures in per-frame counters so the smoke response can
    # report "did any point/segment/mask actually land in a frame?" — the movie rail asks with
    # `tally = false`.
    ov_raw = get(data, :overlays, nothing)
    img, gerr = _gating_image(pu, iu)
    ov = _resolve_movie_overlays_mask(img, gerr, arr, caxes, ov_raw, vnn; z = z,
                                       crop = nothing, max_px = 0, tally = true)
    overlays_for     = ov.overlays_for
    mask_for         = ov.mask_for
    point_size_px    = ov.point_size_px
    segment_width_px = ov.segment_width_px
    mask_contour_px  = ov.mask_contour_px
    ov_diag          = ov.ov_diag
    mask_diag        = ov.mask_diag

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
                          mask_for = mask_for,
                          point_size_px = point_size_px,
                          segment_width_px = segment_width_px,
                          mask_contour_px = mask_contour_px)
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
    if haskey(mask_diag, "_tally")
        mf, mi = mask_diag["_tally"]
        mask_diag["framesWithMask"] = mf[]
        mask_diag["idColoursCount"] = mi[]
        delete!(mask_diag, "_tally")
    end
    200, JSON3.write((; ok = true, path = result.path, filename = filename,
                        frames = result.frames, width = result.width, height = result.height,
                        overlays = ov_diag, mask = mask_diag))
end

# ── POST /api/viewer/thumbnail ────────────────────────────────────────────────────
#
# Render ONE frame from a captured viewState — the browser-viewer counterpart of napari's screenshot
# endpoint (`/api/napari/screenshot`) that the animation module page has been using to author
# keyframes. Same rendering path as the movie recorder (`viewstate_to_render_args` +
# `render_view_frame`), so the thumbnail matches what the movie will render — a keyframe strip that
# actually looks like the mp4 it will produce.
#
# **Channels only for MVP.** No overlays / no mask. The animation panel's thumbnail is for
# identifying a keyframe in the timeline strip, not for full-fidelity preview; adding overlays
# duplicates most of `run_single_offline` here and can land as a follow-up when the current shape
# is confirmed to work end-to-end.
#
# The PNG lands as a sidecar board asset — same storage the napari screenshot uses, so the animation
# panel's existing display + delete paths (`/api/board-assets/delete`, sidecar `<id>.png` under
# `settings/board-assets/`) work unchanged.
#
# `POST /api/viewer/thumbnail` — body: `{ projectUid, imageUid, valueName?, viewState }`,
# response: `{ ok, assetId, imageUid, width, height }`.
function api_viewer_thumbnail(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)) catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))
    pu = String(get(data, :projectUid, ""))
    iu = String(get(data, :imageUid, ""))
    (isempty(pu) || isempty(iu)) &&
        return 400, JSON3.write((; error = "projectUid and imageUid required"))
    vn_raw = get(data, :viewState, nothing)
    (vn_raw isa AbstractDict) ||
        return 400, JSON3.write((; error = "viewState (object) required"))
    vs_dict = vn_raw
    val_raw = get(data, :valueName, nothing)
    vnn = (val_raw === nothing || String(val_raw) == "") ? nothing : String(val_raw)
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    props = _props_path(td, zp)
    specs = resolved_display_specs(props, nc)
    specs === nothing && (specs = resolved_display_specs(_sampled_specs(zp, nc)))
    # The rendered thumbnail size — read from the snapshot's `canvas` (the browser recorded it
    # against a specific canvas so zoom + crop are consistent), else the viewState's canvas fields,
    # else fall back to a compact 512×384 placeholder. The renderer's crop is derived from
    # (center, zoom, canvas_h/w) so mismatched dimensions produce a wrong FoV; keeping this
    # aligned with what the viewer emitted is important.
    canvas_raw = get(vs_dict, :canvas, nothing)
    canvas_raw isa AbstractDict || (canvas_raw = get(vs_dict, "canvas", nothing))
    canvas_h = 384; canvas_w = 512
    if canvas_raw isa AbstractDict
        ch = get(canvas_raw, :height, get(canvas_raw, "height", nothing))
        cw = get(canvas_raw, :width,  get(canvas_raw, "width",  nothing))
        ch isa Real && ch > 0 && (canvas_h = Int(round(Float64(ch))))
        cw isa Real && cw > 0 && (canvas_w = Int(round(Float64(cw))))
    end
    # Native H/W for the args resolver — every branch needs them.
    native_h = haskey(d, "y") ? size(arr, d["y"]) : 0
    native_w = haskey(d, "x") ? size(arr, d["x"]) : 0
    (native_h == 0 || native_w == 0) &&
        return 400, JSON3.write((; error = "image has no y/x axes"))
    chan_names = try
        img, ierr = _gating_image(pu, iu)
        ierr === nothing ? something(channel_names(img; value_name = vnn), String[]) : String[]
    catch; String[] end
    args = try
        viewstate_to_render_args(vs_dict, chan_names, specs, native_h, native_w;
                                 canvas_h = canvas_h, canvas_w = canvas_w)
    catch e
        return 500, JSON3.write((; error = "viewState → args failed: $(sprint(showerror, e))"))
    end
    nT = haskey(d, "t") ? size(arr, d["t"]) : 1
    t_clamped = clamp(Int(args.t), 0, nT - 1)
    # 3D viewState (ndisplay = 3) needs the GPU rotation renderer, which isn't set up as a
    # one-shot here. For MVP, thumbnails render the equivalent 2D slice (the plane the viewState
    # was captured at) so a 3D animation still gets a keyframe strip — the movie itself will
    # rotate via the animation record path.
    frame = try
        render_view_frame(arr, caxes, t_clamped;
                          z = args.z, specs = args.specs, crop = args.crop)
    catch e
        return 500, JSON3.write((; error = "render failed: $(sprint(showerror, e))"))
    end
    tmp = tempname() * ".png"
    try
        PNGFiles.save(tmp, frame)
        asset_id = _save_board_asset_file(pu, tmp)
        h, w = size(frame)
        return 200, JSON3.write((; ok = true, assetId = asset_id, imageUid = iu,
                                    width = Int(w), height = Int(h)))
    catch e
        return 500, JSON3.write((; error = "png/save failed: $(sprint(showerror, e))"))
    finally
        isfile(tmp) && rm(tmp; force = true)
    end
end
