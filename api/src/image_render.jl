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
# String is a colormap NAME resolved through CMAP_RGB into a 2-stop black→base ramp.
_as_lut(v::Lut) = v
_as_lut(name::AbstractString) =
    NTuple{3,Float32}[(0f0, 0f0, 0f0), get(CMAP_RGB, lowercase(name), (1f0, 1f0, 1f0))]
_as_lut(v::AbstractVector) = NTuple{3,Float32}[
    (Float32(s[1]), Float32(s[2]), Float32(s[3])) for s in v]

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
"""
render_view_frame(zarr_path::AbstractString, t::Int; kwargs...) =
    render_view_frame(open_level0(zarr_path)..., t; kwargs...)

function render_view_frame(arr, caxes, t::Int;
                           z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
                           channels::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                           specs = nothing, crop = nothing, max_px::Int = 0)
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
    composite_rgb(chw, sp)
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

