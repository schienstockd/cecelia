# ── Server-side image preview render (Julia, in-process) ──────────────────────────
# Renders a coloured, z-max-projected preview of one timepoint of an OME-ZARR image version: read
# planes → per-channel contrast + colour → additive blend → PNG bytes.
#
# THE ONLY server-side image renderer in the codebase. If you need a thumbnail or preview, use this;
# do not hand-roll a second compositor. It lived in the old `crop_render.jl`, named for its first consumer,
# until an audit found essentially nothing in it was crop-specific (Dominik, 2026-07-30) — the crop
# panel just asked first. `crop_api.jl` now holds only the crop ROUTES; the geometry helpers moved
# out earlier to `image_geometry.jl` for the same reason.
#
# SANCTIONED, NARROW carve-out of the "one canonical image reader" rule: Julia reads the zarr
# directly (Zarr.jl) ONLY for lightweight previews — Python `zarr_utils` stays canonical for anything
# that processes data. Do NOT grow this into a general image reader.
#
# Gotchas handled (proven in the CROP_PANEL_PLAN spike): Zarr.jl is column-major so it presents the
# array in REVERSED axis order (see `axis_dims` in image_geometry.jl); the store is one of two layouts
# (flat array at `/0`, or a bioformats2raw series group at `/0/0`); dtype is uint8 or uint16. Colours
# and contrast come from the viewer's JSON layer-props file; absent → default palette + percentile
# contrast.

using Zarr, JSON3, PNGFiles, ColorTypes, FixedPointNumbers

# ── Channel colour ────────────────────────────────────────────────────────────────
# A channel's colour comes from napari, as a LUT (`colormap_lut` in the props JSON: black→colour stops
# that this renderer interpolates). napari is the authority on its own palette, so we do not re-derive
# it — it has ~30 colormaps and the user can pick any of them, so a name table here can never be
# complete. It was not: `bop blue` was missing, hit the unknown-name fallback, and rendered napari's
# blue SHG channel as full WHITE — the worst possible fallback, because white adds to all three
# accumulators and washes the composite out.
#
# CMAP_RGB stays as the fallback for props files written BEFORE the LUT was saved, so existing images
# render correctly without being re-opened in napari. Values are napari's own LUT end colours
# (`napari.utils.colormaps.AVAILABLE_COLORMAPS[name].colors[-1]`) — all of these are linear ramps from
# black (verified: max deviation 0.007), so a 2-stop LUT reproduces them exactly. napari's remaining
# colormaps are NOT ramps from black — the perceptual ones (viridis/turbo/…) and the `I *` set, which
# run WHITE→colour — so they cannot be approximated from a name at all and fall back to gray here.
# Only a props file with `colormap_lut` renders those faithfully.
const CMAP_RGB = Dict(
    "red" => (1f0, 0f0, 0f0), "green" => (0f0, 1f0, 0f0), "blue" => (0f0, 0f0, 1f0),
    "cyan" => (0f0, 1f0, 1f0), "magenta" => (1f0, 0f0, 1f0), "yellow" => (1f0, 1f0, 0f0),
    "gray" => (1f0, 1f0, 1f0), "grey" => (1f0, 1f0, 1f0), "white" => (1f0, 1f0, 1f0),
    "hilo" => (1f0, 1f0, 1f0), "nan" => (1f0, 1f0, 1f0),
    "bop blue"   => (0.12549f0,  0.678431f0, 0.972549f0),
    "bop orange" => (0.972549f0, 0.678431f0, 0.12549f0),
    "bop purple" => (0.580392f0, 0.12549f0,  0.580392f0),
)
const DEFAULT_CMAPS = ["red", "green", "blue", "yellow"]

# One channel's colour ramp: `n` in [0,1] → RGB. Stop 1 is the colour at zero intensity.
const Lut = Vector{NTuple{3,Float32}}

# Resolve a spec's colour field to a LUT. A Vector of stops (from `colormap_lut`) is used as-is; a
# String is EITHER a colormap NAME resolved through CMAP_RGB, OR a `#rrggbb`/`#rgb` hex — the browser
# viewer sends hex when the user's live picker colour isn't in CMAP_RGB (see
# `frontend/src/utils/viewer/viewState.ts`). Either way it becomes a 2-stop black→base ramp.
_as_lut(v::Lut) = v
function _as_lut(s::AbstractString)
    rgb = _hex_to_rgb01(s)
    rgb === nothing || return NTuple{3,Float32}[(0f0, 0f0, 0f0), rgb]
    NTuple{3,Float32}[(0f0, 0f0, 0f0), get(CMAP_RGB, lowercase(s), (1f0, 1f0, 1f0))]
end
_as_lut(v::AbstractVector) = NTuple{3,Float32}[
    (Float32(s[1]), Float32(s[2]), Float32(s[3])) for s in v]

# `#rrggbb` / `#rgb` → 0-1 RGB. Nothing means it isn't a hex literal (`_as_lut` then tries CMAP_RGB).
function _hex_to_rgb01(s::AbstractString)
    m = match(r"^#?([0-9a-fA-F]{6}|[0-9a-fA-F]{3})$", strip(s))
    m === nothing && return nothing
    h = m.captures[1]
    if length(h) == 3
        r = parse(Int, string(h[1], h[1]); base = 16)
        g = parse(Int, string(h[2], h[2]); base = 16)
        b = parse(Int, string(h[3], h[3]); base = 16)
    else
        r = parse(Int, h[1:2]; base = 16)
        g = parse(Int, h[3:4]; base = 16)
        b = parse(Int, h[5:6]; base = 16)
    end
    (Float32(r) / 255f0, Float32(g) / 255f0, Float32(b) / 255f0)
end

# Sample a LUT at `n` ∈ [0,1] with linear interpolation between stops. A 2-stop black→base ramp reduces
# to exactly `n .* base`, which is what the additive channel primaries need.
@inline function _lut_at(lut::Lut, n::Float32)
    K = length(lut)
    K == 0 && return (0f0, 0f0, 0f0)
    K == 1 && return lut[1]
    p = n * (K - 1)
    i = min(floor(Int, p), K - 2)
    f = p - i
    a, b = lut[i + 1], lut[i + 2]
    (a[1] + f * (b[1] - a[1]), a[2] + f * (b[2] - a[2]), a[3] + f * (b[3] - a[3]))
end

# Read the viewer's per-channel display specs from the JSON layer-props file (Phase 0). Returns a vector
# of (lo, hi, colour, visible) in channel order, or `nothing` if the file is missing/unreadable.
# `colour` is the saved `colormap_lut` when present, else the `colormap` NAME (see CMAP_RGB above).
function layer_display_specs(props_path::AbstractString)
    isfile(props_path) || return nothing
    try
        d = JSON3.read(read(props_path, String))
        imgs = get(d, :Image, nothing)
        imgs === nothing && return nothing
        specs = Tuple{Float64,Float64,Any,Bool}[]
        for e in imgs
            cl  = get(e, :contrast_limits, [0.0, 1.0])
            lut = get(e, :colormap_lut, nothing)
            colour = (lut !== nothing && !isempty(lut)) ? _as_lut(lut) :
                     lowercase(String(get(e, :colormap, "gray")))
            push!(specs, (Float64(cl[1]), Float64(cl[2]), colour, Bool(get(e, :visible, true))))
        end
        isempty(specs) ? nothing : specs
    catch
        nothing
    end
end

"""
    resolved_display_specs(props_path, n_channels) -> Vector{NamedTuple} | Nothing
    resolved_display_specs(specs)                  -> Vector{NamedTuple}

The same per-channel display specs as `layer_display_specs`, but with the colour **already resolved to
LUT stops** — `(lo, hi, lut, visible)` where `lut` is a vector of `(r, g, b)`. `nothing` when the props
file is missing or describes fewer channels than the store has.

This exists for consumers that must SHIP the colours somewhere else rather than composite here — the
browser renderer serves them as JSON (`viewer_api.jl`) and uploads them as a lookup texture. They must
not resolve a colormap name themselves: that is a second copy of napari's palette, and the first copy
being incomplete is what rendered the SHG channel WHITE (see `CMAP_RGB` above). One resolver, two
consumers.
"""
function resolved_display_specs(props_path::AbstractString, n_channels::Int)
    specs = layer_display_specs(props_path)
    (specs === nothing || length(specs) < n_channels) && return nothing
    resolved_display_specs(specs[1:n_channels])
end

resolved_display_specs(specs::AbstractVector) =
    [(; lo = s[1], hi = s[2], lut = _as_lut(s[3]), visible = s[4]) for s in specs]

# Pure: composite a (C, H, W) float array + per-channel (lo, hi, colour, visible) specs → H×W RGB{N0f8}
# via clip-to-contrast, colourise through the channel's LUT, additive blend. `colour` is a colormap
# name or a LUT (see `_as_lut`). Unit-testable without any IO/zarr.
function composite_rgb(chw::AbstractArray{<:Real,3}, specs::AbstractVector)
    C, H, W = size(chw)
    acc = zeros(Float32, 3, H, W)
    @inbounds for c in 1:C
        lo, hi, colour, vis = specs[c]
        vis || continue
        lut = _as_lut(colour)
        rng = Float32(hi - lo); rng = rng == 0f0 ? 1f0 : rng
        for j in 1:W, i in 1:H
            n = clamp((Float32(chw[c, i, j]) - Float32(lo)) / rng, 0f0, 1f0)
            r, g, b = _lut_at(lut, n)
            acc[1, i, j] += r
            acc[2, i, j] += g
            acc[3, i, j] += b
        end
    end
    [RGB{N0f8}(clamp(acc[1, i, j], 0, 1), clamp(acc[2, i, j], 0, 1), clamp(acc[3, i, j], 0, 1))
     for i in 1:H, j in 1:W]
end

# Render timepoint `t` (0-based) of `zarr_path` to composite-MIP PNG bytes. z is max-projected (subsampled
# to ~≤`z_keep` planes for speed) and the frame is downsampled so its long side ≤ `max_px` (a crop
# footprint needs no more). Colours from `props_path` (JSON) if present. Returns the PNG as a byte vector.
function render_preview_frame(zarr_path::AbstractString, props_path::AbstractString, t::Int;
                           max_px::Int = 512, z_keep::Int = 12,
                           z_lo_frac::Real = 0.0, z_hi_frac::Real = 1.0)
    arr, caxes = open_level0(zarr_path)
    nd = ndims(arr)
    dims = axis_dims(caxes, nd)
    jy, jx = dims["y"], dims["x"]
    jz = get(dims, "z", 0); jc = get(dims, "c", 0); jt = get(dims, "t", 0)
    sz = size(arr)

    idx = Any[Colon() for _ in 1:nd]
    jt != 0 && (idx[jt] = t + 1)                 # 0-based t → 1-based
    if jz != 0
        # Project only over the KEPT z-range (so the slider previews what you'll keep), z-subsampled for
        # speed. Full range (0..1) ⇒ the whole stack. z chunks are size 1, so this also cuts IO.
        nz = sz[jz]
        lo = clamp(floor(Int, clamp(z_lo_frac, 0, 1) * nz) + 1, 1, nz)
        hi = clamp(ceil(Int,  clamp(z_hi_frac, 0, 1) * nz),     lo, nz)
        idx[jz] = lo:max(1, cld(hi - lo + 1, z_keep)):hi
    end
    # `read_native`, never `arr[idx...]` — a raw bioformats2raw store is big-endian and Zarr.jl does not
    # swap it (see image_geometry.jl). Reading it unswapped turned every `default` version into
    # saturated white noise.
    sub = read_native(arr, idx...)               # reads; the scalar t-dim is dropped

    # names of the REMAINING Julia dims (t dropped), so we can permute to canonical (c, y, x)
    names = [caxes_or_fallback(caxes, nd)[nd - j + 1] for j in 1:nd]
    rem_names = String[names[j] for j in 1:nd if j != jt]
    kz = findfirst(==("z"), rem_names)
    m = kz === nothing ? sub : dropdims(maximum(sub; dims = kz); dims = kz)   # MIP over z → (…c,y,x…)
    mnames = kz === nothing ? rem_names : [rem_names[k] for k in eachindex(rem_names) if k != kz]

    kc = findfirst(==("c"), mnames); ky = findfirst(==("y"), mnames); kx = findfirst(==("x"), mnames)
    if kc === nothing                            # single-channel image → add a channel axis
        m = reshape(m, 1, size(m)...); kc = 1; ky += 1; kx += 1
    end
    chw = permutedims(m, (kc, ky, kx))           # → (C, Y, X)

    # xy downsample so the long side ≤ max_px (drawing needs no full res)
    C, Y, X = size(chw)
    step = max(1, cld(max(Y, X), max_px))
    step > 1 && (chw = chw[:, 1:step:Y, 1:step:X])

    specs = layer_display_specs(props_path)
    if specs === nothing || length(specs) < size(chw, 1)
        specs = [percentile_spec(view(chw, c, :, :), DEFAULT_CMAPS[mod1(c, 4)]) for c in 1:size(chw, 1)]
    end
    img = composite_rgb(chw, specs)

    io = IOBuffer()
    PNGFiles.save(io, img)
    take!(io)
end

"""
    render_view_frame(zarr_path, t; kwargs...)     -> Matrix{RGB{N0f8}}
    render_view_frame(arr, caxes, t; kwargs...)    -> Matrix{RGB{N0f8}}

ONE movie-grade frame of timepoint `t` (0-based), composited from the channels through their LUTs —
renderer **C**'s frame, the offline half of the split in `docs/todo/WEB_VIEWER_PLAN.md` (P5).

Different from `render_preview_frame` in the three ways a movie differs from a thumbnail, and it is
worth naming them because "render a frame" sounds like one job:

  - **It returns pixels, not a PNG.** Half of C's warm frame was PNG encoding (49.5 ms of 117 ms), and
    a movie encoder wants raw frames — paying for a PNG per frame only to decode it again is most of
    the render. A caller that wants a still encodes one.
  - **It takes a z SELECTION rather than a fraction.** `nothing` projects the whole stack, an `Int` is
    one plane, a `UnitRange` projects that range — the same three answers, spelled the same way, as the
    browser's slab route, because they come from the same `read_slab`. A movie of "plane 12" and the
    2D view of plane 12 must be the same picture or one of them is lying.
  - **It does not subsample z, and only downsamples xy when asked.** A preview may drop planes for
    speed; a movie frame is the output, and quietly projecting 12 of 41 planes would change what the
    movie MEANS to make it faster.

`specs` are resolved display specs (`resolved_display_specs`) — the movie config's colours where there
is one, the saved napari props where there is not. Absent, each channel gets the percentile fallback,
which is per-frame and therefore FLICKERS across a timecourse: pass specs for anything but a still.

`crop` is `(x = x0:x1, y = y0:y1)` in 0-based pixels, clamped to the frame. `max_px` 0 keeps native
resolution; anything else downsamples so the long side fits, by striding (a movie writer's own scaler
is the better resampler when quality matters, and it has one).

`points` and `segments` paint the P3 overlays onto the composited frame — same content the browser
draws through WebGPU, so a recorded movie of a gated experiment shows the annotations that make the
pixels a result. Coordinates are 1-based row/column and MUST be pre-resolved to the frame's pixel grid
by the caller (µm → pixel, crop / stride honoured); this file is pure drawing. See
`frame_overlays.jl` for the shape.

`mask` is a same-shape `AbstractMatrix{<:Integer}` — the P4 label store's frame with whatever z
projection the caller applied. Present WITH `mask_colours::AbstractDict{<:Integer,<:RGB}` = label →
outline colour, or the mask is skipped. `mask_contour_px` is the outline width (napari's `contour`
setting). Ids absent from `mask_colours` are skipped, so hiding populations does not require
rewriting the mask.
"""
render_view_frame(zarr_path::AbstractString, t::Int; kwargs...) =
    render_view_frame(open_level0(zarr_path)..., t; kwargs...)

function render_view_frame(arr, caxes, t::Int;
                           z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
                           channels::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                           specs = nothing, crop = nothing, max_px::Int = 0,
                           points = nothing, point_size_px::Int = 6,
                           segments = nothing, segment_width_px::Int = 2,
                           mask = nothing, mask_colours = nothing, mask_contour_px::Int = 1)
    nd   = ndims(arr)
    dims = axis_dims(caxes, nd)
    nc   = haskey(dims, "c") ? size(arr, dims["c"]) : 1
    chans = channels === nothing ? collect(0:(nc - 1)) : collect(Int, channels)
    isempty(chans) && throw(ArgumentError("render_view_frame: no channels selected"))
    any(c -> c < 0 || c >= nc, chans) &&
        throw(ArgumentError("render_view_frame: channel out of range (image has $nc)"))

    # Which planes to project, as INDIVIDUAL reads. Not one `read_slab` over the range, deliberately:
    # a full-depth timepoint of the real target is 326 MB per channel, so projecting it in one read
    # holds 1.3 GB of transient at four channels for an answer that is 2.2 MB. z chunks are one plane
    # deep in every store here, so plane-at-a-time reads exactly the same bytes off disk — the whole
    # difference is the high-water mark.
    zs = if !haskey(dims, "z")
        nothing                                    # a 2D store: `z` has nothing to select
    else
        nz = size(arr, dims["z"])
        r = z === nothing ? (0:(nz - 1)) : (z isa Int ? (z:z) : z)
        clamp(first(r), 0, nz - 1):clamp(last(r), clamp(first(r), 0, nz - 1), nz - 1)
    end

    local chw
    for (k, c) in enumerate(chans)
        # `read_slab` answers (x, y) for a scalar z, and the max over the planes IS the projection.
        local m
        if zs === nothing
            m, = read_slab(arr, caxes, t, c)
            ndims(m) >= 3 && (m = dropdims(maximum(m; dims = 3); dims = 3))
        else
            for (i, zi) in enumerate(zs)
                pl, = read_slab(arr, caxes, t, c; z = zi)
                i == 1 ? (m = pl) : (m .= max.(m, pl))
            end
        end
        # Transpose to (y, x) — image row order, which is what `composite_rgb` and every encoder
        # downstream expects.
        plane = permutedims(m, (2, 1))
        if crop !== nothing
            H, W = size(plane)
            ys = _clamp_range(get(crop, :y, nothing), H)
            xs = _clamp_range(get(crop, :x, nothing), W)
            plane = plane[ys, xs]
        end
        if max_px > 0
            H, W = size(plane)
            step = max(1, cld(max(H, W), max_px))
            step > 1 && (plane = plane[1:step:H, 1:step:W])
        end
        # Allocated on the FIRST channel, once its final shape is known — crop and stride both change
        # it, and sizing this from the store would be a second derivation of the same number.
        k == 1 && (chw = Array{Float32,3}(undef, length(chans), size(plane)...))
        size(plane) == (size(chw, 2), size(chw, 3)) ||
            throw(ArgumentError("render_view_frame: channel $c is $(size(plane)), expected $(size(chw)[2:3])"))
        @inbounds chw[k, :, :] .= plane
    end

    sp = specs === nothing ?
        [percentile_spec(view(chw, k, :, :), DEFAULT_CMAPS[mod1(k, 4)]) for k in 1:length(chans)] :
        specs
    length(sp) >= length(chans) ||
        throw(ArgumentError("render_view_frame: $(length(sp)) specs for $(length(chans)) channels"))
    frame = composite_rgb(chw, sp)
    # Overlays paint on top of the composited channels. ORDER matters — the browser draws in the
    # same layering: mask outlines below tracks below points, so a marker at a track's endpoint reads
    # as a marker rather than a fatter tail, and a cell's outline reads as an outline rather than a
    # fatter marker. The caller has already resolved µm → pixel and honoured `crop`/`max_px`; this
    # file is pure drawing.
    if mask !== nothing && mask_colours !== nothing
        draw_mask_outline!(frame, mask, mask_colours; contour_px = mask_contour_px)
    end
    segments === nothing || draw_segments!(frame, segments; width_px = segment_width_px)
    points === nothing   || draw_points!(frame, points; size_px = point_size_px)
    frame
end

# ── 3D rotation-MIP renderer (P5-a Stage C / animation) ─────────────────────────
#
# `render_view_frame_3d` is the offline counterpart to `render_view_frame` for the 3D animation case.
# Instead of projecting the volume axially (`z = nothing`), it applies a camera rotation to the volume
# and MIPs along the rotated view-Z. NO Qt/vispy/GL: pure Julia trilinear interp + ray-cast MIP —
# what the toy demo of 2026-08-27 (scratchpad/rotate_demo.jl) proved viable on fXgbTl.
#
# Rotation is Euler `(rx, ry, rz)` in degrees, matching napari's `camera.angles` docstring
# (`vispy.scene.cameras.turntable`; the same field `capture_view_state` writes). The rotation matrix
# is `R = Rz * Ry * Rx` — rotate around X first, then Y, then Z — which is vispy's default when its
# `Base3DRotationCamera` applies the transform to the volume before projection.
#
# `z_aniso = physical_z / physical_x` corrects for anisotropic voxels. Without it, a rotated 90° view
# of a stack with `PhysicalSizeZ=2 µm`, `PhysicalSizeX=0.33 µm` would look ~6× squashed. The world is
# stretched so xy and (z × z_aniso) are isotropic BEFORE the rotation is applied.
#
# `render_quality` picks how many samples per ray we take along view-Z:
#   * `:draft`    — 0.5× the diagonal of the volume (fast previews)
#   * `:standard` — 1.0× (default)
#   * `:high`     — 2.0× (final render)
# Trilinear interp only — tricubic wasn't visibly better in testing and pays 8× the lookups.
#
# `zoom` = canvas-px-per-world-unit (napari `camera.zoom`). Higher is more zoomed in. `center = (cz,
# cy, cx)` in native voxels, or `nothing` for the volume midpoint.
#
# Volume load: one full (C, Y, X, Z) read per call. The keyframe renderer memoises on `(t, channels)`
# so consecutive same-t frames reuse the volume (a rotation animation typically holds t constant).
function render_view_frame_3d(arr, caxes, t::Int;
                              channels::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                              specs = nothing,
                              angles::NTuple{3,<:Real} = (0.0, 0.0, 0.0),
                              center::Union{Nothing,NTuple{3,<:Real}} = nothing,
                              zoom::Real = 1.0,
                              z_aniso::Real = 1.0,
                              canvas_h::Int = 512, canvas_w::Int = 512,
                              render_quality::Symbol = :standard,
                              volume_cache::Union{Nothing,Ref{Any}} = nothing)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    nc    = haskey(dims, "c") ? size(arr, dims["c"]) : 1
    chans = channels === nothing ? collect(0:(nc - 1)) : collect(Int, channels)
    isempty(chans) && throw(ArgumentError("render_view_frame_3d: no channels selected"))
    haskey(dims, "z") || throw(ArgumentError("render_view_frame_3d: image has no z axis (2D store — use render_view_frame)"))

    # Load full volume as (C, Y, X, Z) Float32. Memoise on (t, chans) if the caller passed a cache.
    ck = (t, Tuple(chans))
    V = if volume_cache !== nothing && volume_cache[] isa Tuple && volume_cache[][1] == ck
        volume_cache[][2]
    else
        nZ = size(arr, dims["z"]); nY = size(arr, dims["y"]); nX = size(arr, dims["x"])
        Vloc = Array{Float32,4}(undef, length(chans), nY, nX, nZ)
        for (k, c) in enumerate(chans)
            for zi in 0:(nZ - 1)
                pl, = read_slab(arr, caxes, t, c; z = zi)
                Vloc[k, :, :, zi + 1] .= Float32.(permutedims(pl, (2, 1)))
            end
        end
        volume_cache === nothing || (volume_cache[] = (ck, Vloc))
        Vloc
    end
    C, nY, nX, nZ = size(V)
    cz, cy, cx = center === nothing ? (Float64(nZ - 1) / 2, Float64(nY - 1) / 2, Float64(nX - 1) / 2) :
                                       (Float64(center[1]), Float64(center[2]), Float64(center[3]))

    # Rotation matrix R = Rz * Ry * Rx (vispy Base3DRotationCamera convention).
    rx, ry, rz = deg2rad(Float64(angles[1])), deg2rad(Float64(angles[2])), deg2rad(Float64(angles[3]))
    sx, cxs = sin(rx), cos(rx)
    sy, cys = sin(ry), cos(ry)
    sz, czs = sin(rz), cos(rz)
    # Composed R applied to a column vector (world = R * view).
    R11 = czs * cys;                 R12 = czs * sy * sx - sz * cxs;  R13 = czs * sy * cxs + sz * sx
    R21 = sz  * cys;                 R22 = sz  * sy * sx + czs * cxs; R23 = sz  * sy * cxs - czs * sx
    R31 = -sy;                       R32 = cys * sx;                  R33 = cys * cxs

    # Isotropic world extents. zoom scales canvas-px-per-world-unit; higher zoom = zoomed in.
    ext_y = Float64(nY)
    ext_x = Float64(nX)
    ext_z = Float64(nZ) * Float64(z_aniso)
    canvas_span = max(ext_x, ext_y)
    world_per_px = canvas_span / (Float64(zoom) * Float64(canvas_w))    # world units per canvas pixel

    # Sample density along the ray. Diagonal (X + Z) is what a 45° tilt sees end-to-end.
    diag = sqrt(ext_x^2 + ext_z^2)
    q_mult = render_quality === :draft ? 0.5 :
             render_quality === :high  ? 2.0 : 1.0
    n_samples = max(4, ceil(Int, diag * q_mult))
    step_v    = diag / n_samples                          # step in world units along view-Z

    out = zeros(Float32, C, canvas_h, canvas_w)
    @inbounds Threads.@threads for j in 1:canvas_w
        xv = (j - (canvas_w + 1) / 2) * world_per_px
        for i in 1:canvas_h
            yv = (i - (canvas_h + 1) / 2) * world_per_px
            for c in 1:C
                m = 0.0f0
                for s in 1:n_samples
                    zv = (s - (n_samples + 1) / 2) * step_v
                    # world = R * (xv, yv, zv)  (view-Y stays "up" because napari's canvas Y is world-Y).
                    xw = R11 * xv + R12 * yv + R13 * zv
                    yw = R21 * xv + R22 * yv + R23 * zv
                    zw = R31 * xv + R32 * yv + R33 * zv
                    vy = yw + cy
                    vx = xw + cx
                    vz = (zw / Float64(z_aniso)) + cz
                    (vy < 0 || vy > nY - 1 || vx < 0 || vx > nX - 1 || vz < 0 || vz > nZ - 1) && continue
                    y0 = floor(Int, vy); y1 = min(nY - 1, y0 + 1); fy = Float32(vy - y0)
                    x0 = floor(Int, vx); x1 = min(nX - 1, x0 + 1); fx = Float32(vx - x0)
                    z0 = floor(Int, vz); z1 = min(nZ - 1, z0 + 1); fz = Float32(vz - z0)
                    c000 = V[c, y0 + 1, x0 + 1, z0 + 1]
                    c100 = V[c, y1 + 1, x0 + 1, z0 + 1]
                    c010 = V[c, y0 + 1, x1 + 1, z0 + 1]
                    c110 = V[c, y1 + 1, x1 + 1, z0 + 1]
                    c001 = V[c, y0 + 1, x0 + 1, z1 + 1]
                    c101 = V[c, y1 + 1, x0 + 1, z1 + 1]
                    c011 = V[c, y0 + 1, x1 + 1, z1 + 1]
                    c111 = V[c, y1 + 1, x1 + 1, z1 + 1]
                    v = (1 - fy) * ((1 - fx) * ((1 - fz) * c000 + fz * c001) +
                                          fx  * ((1 - fz) * c010 + fz * c011)) +
                          fy   * ((1 - fx) * ((1 - fz) * c100 + fz * c101) +
                                          fx  * ((1 - fz) * c110 + fz * c111))
                    v > m && (m = v)
                end
                out[c, i, j] = m
            end
        end
    end

    sp = specs === nothing ?
        [percentile_spec(view(out, k, :, :), DEFAULT_CMAPS[mod1(k, 4)]) for k in 1:C] : specs
    composite_rgb(out, sp)
end

# A 0-based inclusive pixel range → the 1-based Julia range it selects, clamped to `n`. `nothing` (or a
# range entirely outside the frame) means the whole axis: a crop that selects nothing would render a
# zero-size frame, which an encoder reports as a corrupt movie rather than as a bad crop.
function _clamp_range(r, n::Int)
    r === nothing && return 1:n
    lo = clamp(first(r) + 1, 1, n)
    hi = clamp(last(r) + 1, lo, n)
    lo:hi
end

caxes_or_fallback(caxes, nd) = length(caxes) == nd ? caxes : ["t", "c", "z", "y", "x"][(end - nd + 1):end]

# Fallback per-channel spec when there's no viewer JSON: 1st/99.9th percentile contrast + default colour.
function percentile_spec(plane, cmap::String)
    v = sort(vec(Float64.(plane)))
    n = length(v)
    lo = v[clamp(floor(Int, 0.01 * n) + 1, 1, n)]
    hi = v[clamp(ceil(Int, 0.999 * n), 1, n)]
    (lo, hi, cmap, true)
end

# Small bounded in-memory frame cache (server-lifetime). Key includes the props-file mtime so changing
# the viewer's colours invalidates cached frames. FIFO eviction — a preview session touches few frames.
const _RENDER_CACHE       = Dict{String,Vector{UInt8}}()
const _RENDER_CACHE_ORDER = String[]
const _RENDER_CACHE_MAX   = 256
function cached_render!(key::String, produce)
    haskey(_RENDER_CACHE, key) && return _RENDER_CACHE[key]
    v = produce()
    _RENDER_CACHE[key] = v; push!(_RENDER_CACHE_ORDER, key)
    if length(_RENDER_CACHE_ORDER) > _RENDER_CACHE_MAX
        delete!(_RENDER_CACHE, popfirst!(_RENDER_CACHE_ORDER))
    end
    v
end

