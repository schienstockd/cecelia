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

# Named colormap → base RGB for the additive channel blend. The additive primaries napari uses for
# multichannel display are linear ramps, so intensity × base-RGB reproduces them. Unknown/perceptual
# names fall back to gray (rare for raw channels; revisit with a LUT if needed).
const CMAP_RGB = Dict(
    "red" => (1f0, 0f0, 0f0), "green" => (0f0, 1f0, 0f0), "blue" => (0f0, 0f0, 1f0),
    "cyan" => (0f0, 1f0, 1f0), "magenta" => (1f0, 0f0, 1f0), "yellow" => (1f0, 1f0, 0f0),
    "gray" => (1f0, 1f0, 1f0), "grey" => (1f0, 1f0, 1f0), "white" => (1f0, 1f0, 1f0),
)
const DEFAULT_CMAPS = ["red", "green", "blue", "yellow"]

# Read the viewer's per-channel display specs from the JSON layer-props file (Phase 0). Returns a vector
# of (lo, hi, cmap_name, visible) in channel order, or `nothing` if the file is missing/unreadable.
function layer_display_specs(props_path::AbstractString)
    isfile(props_path) || return nothing
    try
        d = JSON3.read(read(props_path, String))
        imgs = get(d, :Image, nothing)
        imgs === nothing && return nothing
        specs = Tuple{Float64,Float64,String,Bool}[]
        for e in imgs
            cl = get(e, :contrast_limits, [0.0, 1.0])
            push!(specs, (Float64(cl[1]), Float64(cl[2]),
                          lowercase(String(get(e, :colormap, "gray"))),
                          Bool(get(e, :visible, true))))
        end
        isempty(specs) ? nothing : specs
    catch
        nothing
    end
end

# Pure: composite a (C, H, W) float array + per-channel (lo, hi, cmap, visible) specs → H×W RGB{N0f8}
# via clip-to-contrast, colourise, additive blend. Unit-testable without any IO/zarr.
function composite_rgb(chw::AbstractArray{<:Real,3}, specs::AbstractVector)
    C, H, W = size(chw)
    acc = zeros(Float32, 3, H, W)
    @inbounds for c in 1:C
        lo, hi, cmap, vis = specs[c]
        vis || continue
        base = get(CMAP_RGB, cmap, (1f0, 1f0, 1f0))
        rng = Float32(hi - lo); rng = rng == 0f0 ? 1f0 : rng
        r, gg, b = base
        for j in 1:W, i in 1:H
            n = clamp((Float32(chw[c, i, j]) - Float32(lo)) / rng, 0f0, 1f0)
            r  != 0f0 && (acc[1, i, j] += n * r)
            gg != 0f0 && (acc[2, i, j] += n * gg)
            b  != 0f0 && (acc[3, i, j] += n * b)
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
    sub = arr[idx...]                            # reads; the scalar t-dim is dropped

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

