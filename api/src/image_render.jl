# ── Server-side image preview render (Julia, in-process) ──────────────────────────
# Renders a coloured, z-max-projected preview of one timepoint of an OME-ZARR image version: read
# planes → per-channel contrast + colour → additive blend → PNG bytes.
#
# THE ONLY server-side image renderer in the codebase. If you need a thumbnail or preview, use this;
# do not hand-roll a second compositor. It lived in the old `crop_render.jl`, named for its first consumer,
# until an audit found essentially nothing in it was crop-specific — the crop
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
# complete. It was not: `bop blue` was missing, hit the unknown-name fallback, and rendered the legacy viewer's
# blue SHG channel as full WHITE — the worst possible fallback, because white adds to all three
# accumulators and washes the composite out.
#
# CMAP_RGB stays as the fallback for props files written BEFORE the LUT was saved, so existing images
# render correctly without being re-opened in napari. Values are the legacy viewer's own LUT end colours
# (`napari.utils.colormaps.AVAILABLE_COLORMAPS[name].colors[-1]`) — all of these are linear ramps from
# black (verified: max deviation 0.007), so a 2-stop LUT reproduces them exactly. the legacy viewer's remaining
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
#
# Two on-disk shapes today, checked in preference order:
#  1. **Legacy napari**: `Image: [{contrast_limits, colormap, colormap_lut, visible}, …]` — one entry
#     per channel, INDEX-ORDERED. Written by the napari bridge; still the case for pre-P5 versions
#     and every image whose viewer state predates the browser viewer.
#  2. **Browser viewer**: `webgpu: {channels: [{visible, hex, lo, hi}, …]}` — same INDEX ORDER, but
#     the colour is a plain `#rrggbb` hex ramp (no LUT) and lo/hi live top-level rather than under
#     a `contrast_limits` pair. Written by the browser viewer's autosave (P5+). A sibling `layers`
#     dict carries the same fields keyed BY NAME, but this reader has no channel-name list so
#     `webgpu.channels` is what it can walk in order.
#
# If neither shape is present, return nothing — the caller (`_resolve_frame_for_record`) falls back
# to sampled contrast with `DEFAULT_CMAPS`, which is what a cold-start image shows in the viewer too.
function layer_display_specs(props_path::AbstractString)
    isfile(props_path) || return nothing
    try
        d = JSON3.read(read(props_path, String))
        imgs = get(d, :Image, nothing)
        if imgs !== nothing
            specs = Tuple{Float64,Float64,Any,Bool}[]
            for e in imgs
                cl  = get(e, :contrast_limits, [0.0, 1.0])
                lut = get(e, :colormap_lut, nothing)
                colour = (lut !== nothing && !isempty(lut)) ? _as_lut(lut) :
                         lowercase(String(get(e, :colormap, "gray")))
                push!(specs, (Float64(cl[1]), Float64(cl[2]), colour, Bool(get(e, :visible, true))))
            end
            isempty(specs) || return specs
        end
        # Browser-viewer shape (P5+). `webgpu.channels` is ordered by channel index — same order
        # the store's `c` axis walks — so no channel-name lookup is needed here.
        wg = get(d, :webgpu, nothing)
        if wg isa AbstractDict
            chs = get(wg, :channels, nothing)
            if chs isa AbstractVector && !isempty(chs)
                specs = Tuple{Float64,Float64,Any,Bool}[]
                for e in chs
                    lo  = get(e, :lo, 0.0)
                    hi  = get(e, :hi, 1.0)
                    hex = get(e, :hex, "gray")
                    vis = Bool(get(e, :visible, true))
                    push!(specs, (Float64(lo), Float64(hi), lowercase(String(hex)), vis))
                end
                return specs
            end
        end
        nothing
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
not resolve a colormap name themselves: that is a second copy of the legacy viewer's palette, and the first copy
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

# sRGB piecewise transfer function (IEC 61966-2-1): linear [0,1] → sRGB-encoded [0,1]. This is
# what the browser volume viewer's WebGPU canvas applies for us — the canvas is now pinned to an
# sRGB view (`frontend/src/lib/webgpu/canvasFormat.ts`), so the shader's linear accumulator gets
# gamma-encoded at write time. Every consumer here that hands a byte to a display device / codec
# has to do the same explicit encode, or the linear numbers would be interpreted AS sRGB — mid-
# tones ~2× dimmer than the viewer shows (audit 2026-08-29). Kept as a standalone helper so a
# caller that needs LINEAR pixels (a numeric readout, an image-processing step) can skip it.
@inline function _linear_to_srgb(x::Float32)
    x = clamp(x, 0f0, 1f0)
    x <= 0.0031308f0 && return 12.92f0 * x
    1.055f0 * x^(1f0 / 2.4f0) - 0.055f0
end

# Apply sRGB encoding to an already-composited RGB frame. Callers that write pixels to disk / into
# an mp4 must call this on the `composite_rgb` result; the tests on `composite_rgb` stay in the
# LINEAR space (which is what the LUT + blend policy asserts). One caller each way, one function
# each way — no in-place mutation, so the linear frame stays available if a caller wants both.
srgb_encode(img::AbstractMatrix{RGB{N0f8}}) =
    [RGB{N0f8}(_linear_to_srgb(Float32(red(p))),
               _linear_to_srgb(Float32(green(p))),
               _linear_to_srgb(Float32(blue(p))))
     for p in img]

# Pure: composite a (C, H, W) float array + per-channel (lo, hi, colour, visible) specs → H×W RGB{N0f8}
# via clip-to-contrast, colourise through the channel's LUT, additive blend. `colour` is a colormap
# name or a LUT (see `_as_lut`). Output is LINEAR — callers writing pixels for display / codec must
# wrap with `srgb_encode`. Unit-testable without any IO/zarr.
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
    # sRGB-encode before writing PNG bytes — every image viewer treats an 8-bit PNG as sRGB, so a
    # linear composite would read ~2× dimmer than the WebGPU volume viewer's canvas shows.
    img = srgb_encode(composite_rgb(chw, specs))

    io = IOBuffer()
    PNGFiles.save(io, img)
    take!(io)
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

