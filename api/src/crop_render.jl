# ── In-app crop MIP render (Julia, in-process) ──────────────────────────────────
# Renders a coloured, z-max-projected preview of one timepoint of an OME-ZARR, for the in-app crop panel
# (docs/todo/CROP_PANEL_PLAN.md). This is a SANCTIONED, NARROW carve-out of the "one canonical image
# reader" rule: Julia reads the zarr directly (Zarr.jl) ONLY for this lightweight preview — Python
# `zarr_utils` stays canonical for all processing. Do NOT grow this into a general image reader.
#
# Gotchas handled (proven in the CROP_PANEL_PLAN spike): Zarr.jl is column-major so it presents the
# array in REVERSED axis order; the store is one of two layouts (flat array at `/0`, or a bioformats2raw
# series group at `/0/0`); dtype is uint8 or uint16. Colours/contrast come from the viewer's JSON
# layer-props file (Phase 0); absent → default per-channel palette + percentile contrast.

using Zarr, JSON3, PNGFiles, ColorTypes, FixedPointNumbers

# Named colormap → base RGB for the additive channel blend. The additive primaries napari uses for
# multichannel display are linear ramps, so intensity × base-RGB reproduces them. Unknown/perceptual
# names fall back to gray (rare for raw channels; revisit with a LUT if needed).
const _CROP_CMAP_RGB = Dict(
    "red" => (1f0, 0f0, 0f0), "green" => (0f0, 1f0, 0f0), "blue" => (0f0, 0f0, 1f0),
    "cyan" => (0f0, 1f0, 1f0), "magenta" => (1f0, 0f0, 1f0), "yellow" => (1f0, 1f0, 0f0),
    "gray" => (1f0, 1f0, 1f0), "grey" => (1f0, 1f0, 1f0), "white" => (1f0, 1f0, 1f0),
)
const _CROP_DEFAULT_CMAPS = ["red", "green", "blue", "yellow"]

# Open the level-0 array of a cecelia OME-ZARR + its NGFF axis names (C-order, e.g. ["t","c","z","y","x"]).
# Handles both on-disk layouts: flat (root group, array at "0") and the bioformats2raw series (group at
# "0", array at "0/0"). Axes come from the multiscales `.zattrs` on whichever group carries it.
function _crop_open_level0(zarr_path::AbstractString)
    g = zopen(zarr_path)
    node = g["0"]
    if node isa Zarr.ZArray                    # flat: root .zattrs has the multiscales; array is "0"
        arr, attrs_dir = node, zarr_path
    else                                       # series: "0" is a group, level-0 array is "0/0"
        arr, attrs_dir = node["0"], joinpath(zarr_path, "0")
    end
    caxes = _crop_read_axes(attrs_dir)
    arr, caxes
end

# NGFF axis names from a group's `.zattrs` (multiscales[0].axes[].name), lowercased C-order. Metadata
# only (JSON), not pixels — reading it here is fine. Falls back to a sensible order by ndims if absent.
function _crop_read_axes(attrs_dir::AbstractString)
    p = joinpath(attrs_dir, ".zattrs")
    if isfile(p)
        try
            d = JSON3.read(read(p, String))
            ms = get(d, :multiscales, nothing)
            if ms !== nothing && !isempty(ms)
                ax = get(ms[1], :axes, nothing)
                ax !== nothing && return String[lowercase(String(get(a, :name, ""))) for a in ax]
            end
        catch
        end
    end
    String[]
end

# Julia array dim (1-based) for each named axis. Zarr.jl reverses to column-major, so the C-order axis
# at position i (1-based) sits at Julia dim `ndims - i + 1`.
function _crop_axis_dims(caxes::Vector{String}, nd::Int)
    d = Dict{String,Int}()
    if length(caxes) == nd
        for (i, name) in enumerate(caxes)
            d[name] = nd - i + 1
        end
    else                                        # no axes metadata → assume (t,c,z,y,x) C-order tail
        fallback = ["t", "c", "z", "y", "x"][(end - nd + 1):end]
        for (i, name) in enumerate(fallback)
            d[name] = nd - i + 1
        end
    end
    d
end

# Read the viewer's per-channel display specs from the JSON layer-props file (Phase 0). Returns a vector
# of (lo, hi, cmap_name, visible) in channel order, or `nothing` if the file is missing/unreadable.
function _crop_props_specs(props_path::AbstractString)
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
function _crop_composite_rgb(chw::AbstractArray{<:Real,3}, specs::AbstractVector)
    C, H, W = size(chw)
    acc = zeros(Float32, 3, H, W)
    @inbounds for c in 1:C
        lo, hi, cmap, vis = specs[c]
        vis || continue
        base = get(_CROP_CMAP_RGB, cmap, (1f0, 1f0, 1f0))
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
function render_crop_frame(zarr_path::AbstractString, props_path::AbstractString, t::Int;
                           max_px::Int = 512, z_keep::Int = 12,
                           z_lo_frac::Real = 0.0, z_hi_frac::Real = 1.0)
    arr, caxes = _crop_open_level0(zarr_path)
    nd = ndims(arr)
    dims = _crop_axis_dims(caxes, nd)
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

    specs = _crop_props_specs(props_path)
    if specs === nothing || length(specs) < size(chw, 1)
        specs = [percentile_spec(view(chw, c, :, :), _CROP_DEFAULT_CMAPS[mod1(c, 4)]) for c in 1:size(chw, 1)]
    end
    img = _crop_composite_rgb(chw, specs)

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

# ── HTTP routes (GET) ────────────────────────────────────────────────────────────
# Resolve (projectUid, imageUid, valueName) → (zarr_path, task_dir) using the SAME ccid.json convention
# as napari open (`read_ccid_raw` + `versioned_get_field`). `value_name` empty ⇒ active/default.
function _crop_resolve(project_uid::AbstractString, image_uid::AbstractString, value_name)
    (isempty(project_uid) || isempty(image_uid)) && return (nothing, nothing, "projectUid + imageUid required")
    proj_dir = joinpath(projects_dir(), project_uid)
    meta = joinpath(proj_dir, "1", image_uid, "ccid.json")
    (isdir(proj_dir) && isfile(meta)) || return (nothing, nothing, "Image not found")
    raw = read_ccid_raw(meta)
    fn  = versioned_get_field(raw, "filepath", value_name)
    fn === nothing && return (nothing, nothing, "No filepath registered — run a conversion task first")
    zp = joinpath(proj_dir, "0", image_uid, string(fn))
    isdir(zp) || return (nothing, nothing, "Zarr not found on disk")
    (zp, joinpath(proj_dir, "1", image_uid), nothing)
end

# GET /api/crop/info?projectUid=&imageUid=&valueName=&maxPx= → {nT,nZ,fullW,fullH,frameW,frameH,maxPx}
# Dimensions the panel needs: the timepoint/slice counts for the scrubber + range sliders, the displayed
# frame size, and the full-res size (Phase 2 maps a drawn rectangle back to full px from these).
function api_crop_info(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, _, err = _crop_resolve(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    try
        arr, caxes = _crop_open_level0(zp)
        d  = _crop_axis_dims(caxes, ndims(arr))
        fx = size(arr, d["x"]); fy = size(arr, d["y"])
        nt = haskey(d, "t") ? size(arr, d["t"]) : 1
        nz = haskey(d, "z") ? size(arr, d["z"]) : 1
        max_px = parse(Int, get(q, "maxPx", "512"))
        step = max(1, cld(max(fx, fy), max_px))
        200, JSON3.write((; nT = nt, nZ = nz, fullW = fx, fullH = fy,
                            frameW = cld(fx, step), frameH = cld(fy, step), maxPx = max_px))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# Small bounded in-memory frame cache (server-lifetime). Key includes the props-file mtime so changing
# the viewer's colours invalidates cached frames. FIFO eviction — a crop session touches few frames.
const _CROP_CACHE       = Dict{String,Vector{UInt8}}()
const _CROP_CACHE_ORDER = String[]
const _CROP_CACHE_MAX   = 256
function _crop_cache!(key::String, produce)
    haskey(_CROP_CACHE, key) && return _CROP_CACHE[key]
    v = produce()
    _CROP_CACHE[key] = v; push!(_CROP_CACHE_ORDER, key)
    if length(_CROP_CACHE_ORDER) > _CROP_CACHE_MAX
        delete!(_CROP_CACHE, popfirst!(_CROP_CACHE_ORDER))
    end
    v
end

# GET /api/crop/frame?projectUid=&imageUid=&valueName=&t=&maxPx= → PNG bytes (coloured z-MIP of frame t).
# Served as application/octet-stream (the byte-body path); the frontend wraps it in an image/png blob.
function api_crop_frame(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, td, err = _crop_resolve(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    t = something(tryparse(Int, get(q, "t", "0")), 0)
    max_px = something(tryparse(Int, get(q, "maxPx", "512")), 512)
    zlo = clamp(something(tryparse(Float64, get(q, "zLo", "0")), 0.0), 0.0, 1.0)   # z-range fractions:
    zhi = clamp(something(tryparse(Float64, get(q, "zHi", "1")), 1.0), 0.0, 1.0)   # project only these z
    props = _props_path(td, zp)                       # JSON layer props (napari_api._props_path)
    key = string(zp, "|", vn, "|", t, "|", max_px, "|", zlo, "|", zhi, "|", isfile(props) ? mtime(props) : 0.0)
    try
        200, _crop_cache!(key, () -> render_crop_frame(zp, props, t; max_px = max_px, z_lo_frac = zlo, z_hi_frac = zhi))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
