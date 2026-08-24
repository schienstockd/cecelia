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

Pixels go through `read_native`, never `arr[...]`: a raw `bioformats2raw` store is big-endian and
Zarr.jl does not swap it (see `image_geometry.jl`).
"""
function read_slab(zarr_path::AbstractString, t::Int, c::Int;
                   z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing)
    arr, caxes = open_level0(zarr_path)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    names = caxes_or_fallback(caxes, nd)

    idx = Any[Colon() for _ in 1:nd]
    haskey(dims, "t") && (idx[dims["t"]] = t + 1)      # 0-based → 1-based; scalar, so the dim drops
    haskey(dims, "c") && (idx[dims["c"]] = c + 1)
    # A scalar z drops the z dim exactly as t and c do, so `kept` below excludes it and `nz` answers 1
    # — one code path serves a plane, a sub-slab and a whole volume rather than three that can disagree.
    # Both forms are clamped to the store rather than trusted: these come off a query string, and an
    # out-of-range index is a 500 from deep inside Zarr.jl instead of an answer.
    if z !== nothing && haskey(dims, "z")
        nz_all = size(arr, dims["z"])
        cl(v) = clamp(v, 0, nz_all - 1) + 1
        # An EMPTY range reads as the single plane at its start rather than as nothing. It cannot be a
        # caller asking for zero planes — `2:0` never survives `UnitRange`'s constructor, which
        # normalises it to `2:1` — so it is always a lo/hi pair that arrived the wrong way round, and
        # the ordering has to be fixed where the two numbers are still separate (`try_serve_slab`).
        # A zero-thickness slab would render BLACK: the ray's entry and exit distances coincide.
        idx[dims["z"]] = if z isa Int
            cl(z)
        else
            lo = cl(first(z))
            lo:max(cl(isempty(z) ? first(z) : last(z)), lo)
        end
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

        200, JSON3.write((; nT = nt, nC = nc, nZ = nz, nX = nx, nY = ny,
                            bytesPerVoxel = bpv, slabBytes = nx * ny * nz * bpv,
                            contrastSource = src, voxelUm = vox, spaceUnit = unit,
                            frameIntervalMin = tmin, calibrated = cal, channels))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── GET /api/viewer/slab ──────────────────────────────────────────────────────────
# Served from the STREAM handler, not the route table, because it needs headers of its own —
# `Content-Encoding` and the shape guard. Same idiom as `try_serve_movie`. Returns false when this is
# not a slab request or the image cannot be resolved, and the caller falls through to the 404 path.
#
# ?projectUid=&imageUid=&valueName=&t=&c=&z=&zTo=&enc=identity|zstd → raw little-endian voxels.
#
# `z` omitted → the whole stack. `z=N` → that ONE plane (the 2D view), which is what makes a timecourse
# playable: 8.8 MB and ~13 ms against 326 MB and ~400 ms on `Dml3RG`. `z=N&zTo=M` → the planes N..M
# inclusive, which is what makes the 3D view usable — every cost is linear in the count.
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
    zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return false
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
