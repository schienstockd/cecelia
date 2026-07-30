# ── Image geometry (Julia-side, metadata only) ────────────────────────────────────────────────────
#
# "How big is this image version, on which axes?" — resolved from the store itself, for a SPECIFIC
# image version. Everything here is metadata + array shape: no pixels are read.
#
# WHY IT IS PER-VERSION, and why a stored per-image size would be wrong: `filepath` is a versioned
# field (`default` / drift-corrected / AF-corrected / cropped …), and those versions do not share a
# frame extent. Drift correction EXPANDS the canvas (there is a whole QC finding for it,
# `output.canvas_expansion`) and a crop shrinks it. EaMaVq: 512×512 as imported, 544×548 once
# drift-corrected. So a consumer that needs the extent must name a version — or accept the ACTIVE
# one, which is what the tasks themselves run against.
#
# HISTORY: this was `_crop_*` inside `crop_render.jl`, private to the crop panel because that was the
# first consumer. None of it is crop-specific (Dominik, 2026-07-30) — the crop panel just happened to
# ask first. Pulled out here so the second consumer (the anisotropy grid-size advisory) reuses it
# instead of growing a parallel reader. `crop_render.jl` keeps only what IS about cropping: display
# scaling, colour specs, the RGB composite and the frame cache.
#
# This inherits `crop_render.jl`'s SANCTIONED, NARROW carve-out of the "one canonical image reader"
# rule: Julia touches the zarr directly (Zarr.jl) only for lightweight metadata/preview work; Python
# `zarr_utils` stays canonical for anything that processes data. Do not grow this into a general
# image reader.

"""
    open_level0(zarr_path) -> (array, c_order_axis_names)

Level-0 array of a cecelia OME-ZARR plus its NGFF axis names (C-order, e.g. `["t","c","z","y","x"]`).
Handles both on-disk layouts: flat (root group, array at `"0"`) and the bioformats2raw series (group
at `"0"`, array at `"0/0"`). Axes come from the multiscales `.zattrs` on whichever group carries it.
"""
function open_level0(zarr_path::AbstractString)
    g = zopen(zarr_path)
    node = g["0"]
    if node isa Zarr.ZArray                    # flat: root .zattrs has the multiscales; array is "0"
        arr, attrs_dir = node, zarr_path
    else                                       # series: "0" is a group, level-0 array is "0/0"
        arr, attrs_dir = node["0"], joinpath(zarr_path, "0")
    end
    arr, read_ngff_axes(attrs_dir)
end

"""
    read_ngff_axes(attrs_dir) -> Vector{String}

NGFF axis names from a group's `.zattrs` (`multiscales[0].axes[].name`), lowercased, C-order.
Metadata only (JSON), never pixels. Falls back to a sensible order by ndims when absent.
"""
function read_ngff_axes(attrs_dir::AbstractString)
    p = joinpath(attrs_dir, ".zattrs")
    if isfile(p)
        try
            d = JSON3.read(read(p, String))
            ms = get(d, :multiscales, nothing)
            if !isnothing(ms) && !isempty(ms)
                ax = get(first(ms), :axes, nothing)
                !isnothing(ax) && return [lowercase(string(get(a, :name, ""))) for a in ax]
            end
        catch
            # fall through to the by-rank default
        end
    end
    String[]
end

"""
    axis_dims(c_axes, ndims) -> Dict{String,Int}

Julia array dimension (1-based) for each named axis. Zarr.jl is column-major and so presents the
array in REVERSED axis order: the C-order axis at position `i` sits at Julia dim `ndims - i + 1`.
Falls back to the conventional order for the rank when `.zattrs` carried no axes.
"""
function axis_dims(c_axes::Vector{String}, nd::Int)
    axes = isempty(c_axes) ?
        (nd == 5 ? ["t", "c", "z", "y", "x"] :
         nd == 4 ? ["t", "c", "y", "x"] :
         nd == 3 ? ["c", "y", "x"] : ["y", "x"]) : c_axes
    Dict(a => nd - i + 1 for (i, a) in enumerate(axes) if !isempty(a))
end

"""
    resolve_image_version(project_uid, image_uid, value_name) -> (zarr_path, meta_dir, error)

On-disk path of ONE image version. `value_name === nothing` resolves the ACTIVE version (what a task
would run against). Returns `(nothing, nothing, message)` on any failure — the caller shapes the
HTTP status, so this stays usable off the request path.
"""
function resolve_image_version(project_uid::AbstractString, image_uid::AbstractString, value_name)
    (isempty(project_uid) || isempty(image_uid)) &&
        return (nothing, nothing, "projectUid + imageUid required")
    proj_dir = joinpath(projects_dir(), project_uid)
    meta = joinpath(proj_dir, "1", image_uid, "ccid.json")
    (isdir(proj_dir) && isfile(meta)) || return (nothing, nothing, "Image not found")
    raw = read_ccid_raw(meta)
    fn  = versioned_get_field(raw, "filepath", value_name)
    fn === nothing &&
        return (nothing, nothing, "No filepath registered — run a conversion task first")
    zp = joinpath(proj_dir, "0", image_uid, string(fn))
    isdir(zp) || return (nothing, nothing, "Zarr not found on disk")
    (zp, joinpath(proj_dir, "1", image_uid), nothing)
end

"""
    image_geometry(zarr_path) -> NamedTuple

`(sizeX, sizeY, sizeZ, sizeT)` of a version's level-0 array. Missing axes report 1, so a 2D still and
a 5D movie answer the same shape of question.
"""
function image_geometry(zarr_path::AbstractString)
    arr, caxes = open_level0(zarr_path)
    d = axis_dims(caxes, ndims(arr))
    dim(name) = haskey(d, name) ? size(arr, d[name]) : 1
    (sizeX = dim("x"), sizeY = dim("y"), sizeZ = dim("z"), sizeT = dim("t"))
end

# GET /api/images/geometry?projectUid=&imageUid=&valueName= → {sizeX,sizeY,sizeZ,sizeT,valueName}
#
# The frame extent of ONE image version. `valueName` omitted ⇒ the ACTIVE version, which is what a
# task will run against — the reason this exists rather than a stored per-image size (see the note at
# the top of this file).
function api_image_geometry(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", "")
    zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""),
                                       isempty(vn) ? nothing : vn)
    err === nothing || return 404, JSON3.write((; error = err))
    try
        g = image_geometry(zp)
        200, JSON3.write((; g.sizeX, g.sizeY, g.sizeZ, g.sizeT, valueName = vn))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
