using JSON3

# ── Versioned path dict helpers ────────────────────────────────────────────────

function active(d::Dict{String,String})::Union{String,Nothing}
    key = get(d, "_active", "default")
    get(d, key, nothing)
end

function set_active!(d::Dict{String,String}, filename::String, name::String="default")
    d[name]      = filename
    d["_active"] = name
    d
end

function value_names(d::Dict{String,String})::Vector{String}
    filter(k -> k != "_active", collect(keys(d)))
end

# ── CciaImage ──────────────────────────────────────────────────────────────────

mutable struct CciaImage
    uid::String
    name::String
    kind::String
    status::String                    # pending | converting | done | failed
    filepath::Dict{String,String}     # versioned filenames (relative to zero dir)
    labels::Dict{String,Vector{String}}  # valueName → [filename, ...] (e.g. labels.zarr, labels_cyto.zarr)
    label_props::Dict{String,String}
    im_channel_names::Dict{String,Any} # versioned: {value_name => [names], _active => value_name}
    attr::Dict{String,String}         # user-defined metadata attributes
    meta::Dict{String,Any}
    _dir::String                      # {proj}/1/{uid}/ — runtime only
    # runtime-only pop_df result cache (keyed by request hash; values are DataFrames).
    # Mirrors R cciaImage `private$filteredPopDT`. Not serialised; cleared via
    # `pop_df(...; flush_cache=true)`. Typed loosely because DataFrames is imported after
    # this file in the module include order.
    _pop_df_cache::Dict{String,Any}
end

function CciaImage(; uid=gen_uid(), name="", kind="static", status="pending", dir="")
    CciaImage(uid, name, kind, status,
              Dict{String,String}(), Dict{String,Vector{String}}(), Dict{String,String}(),
              Dict{String,Any}(),                 # im_channel_names (versioned)
              Dict{String,String}(), Dict{String,Any}(), dir,
              Dict{String,Any}())                 # _pop_df_cache (runtime only)
end

"""Image data directory — {proj}/0/{uid}/"""
function img_zero_dir(img::CciaImage)::String
    joinpath(dirname(dirname(img._dir)), "0", img.uid)
end

"""Absolute path to the active (or named) filepath version. Resolves into the 0 (image) dir."""
function img_filepath(img::CciaImage, name::Union{String,Nothing}=nothing)::Union{String,Nothing}
    filename = isnothing(name) ? active(img.filepath) : get(img.filepath, name, nothing)
    isnothing(filename) ? nothing : joinpath(img_zero_dir(img), filename)
end

"""The image's labelProps directory — `{proj}/1/{uid}/labelProps`."""
img_label_props_dir(img::CciaImage)::String = joinpath(img._dir, "labelProps")

"""
Absolute path to a labelProps `.h5ad` for a value_name (mirrors R `imLabelPropsFilepath`).
Uses the registered filename when present, else the conventional `{value_name}.h5ad`. This is
the single owner of the labelProps path convention — readers (`label_props`) and tasks
(segmentation/tracking) resolve here rather than joining `"labelProps"` inline.
"""
function img_label_props_path(img::CciaImage, value_name::AbstractString="default")::String
    filename = get(img.label_props, String(value_name), "$(value_name).h5ad")
    joinpath(img_label_props_dir(img), filename)
end

# Per-track table suffix. A tracked segmentation gets a companion `.h5ad` holding ONE row per
# track (track measures in X/var, lineage in obs) alongside the per-cell labelProps. The double
# underscore keeps it distinct from a segmentation literally named "{x}_tracks" and marks the
# name as reserved (a user segmentation may not end in `__tracks`).
const TRACK_PROPS_SUFFIX = "__tracks"

"""
Absolute path to the per-track labelProps `.h5ad` for a value_name:
`labelProps/{value_name}__tracks.h5ad`. Written by `tracking.track_measures` (one row per
`track_id`; measures in X/var so they are gateable, lineage in obs). Distinct from the per-cell
`img_label_props_path` so the cell table stays normalised (no broadcast track columns).
"""
img_track_props_path(img::CciaImage, value_name::AbstractString="default")::String =
    joinpath(img_label_props_dir(img), "$(value_name)$(TRACK_PROPS_SUFFIX).h5ad")

"""True if a value_name uses the reserved per-track suffix (`__tracks`) — not allowed for a
user-created segmentation (it names the companion track table for `{base}`)."""
is_reserved_value_name(value_name::AbstractString) =
    endswith(String(value_name), TRACK_PROPS_SUFFIX)

# ── Channel names ──────────────────────────────────────────────────────────────

"""
Set channel names, validating against SizeC from OME metadata.
Stored as a top-level versioned field (`imChannelNames`) — the same on-disk
convention used by tasks and the API — so `versioned_get_field`/`versioned_set_field!`
readers see them. value_name allows storing multiple sets (e.g. per population).
"""
function set_channel_names!(img::CciaImage, names::Vector{String};
                             value_name::String="default",
                             check_length::Bool=true)::CciaImage
    if check_length
        size_c = parse(Int, string(get(img.meta, "SizeC", "0")))
        if size_c > 0 && length(names) != size_c
            error("Expected $size_c channel names (SizeC), got $(length(names))")
        end
    end
    versioned_set!(img.im_channel_names, names, value_name)
    img
end

"""
Return channel names for a value_name (defaults to the active version). Channel names are
often stored only under a base version (e.g. `default`) while labelProps use other value_names
(e.g. `B`), so an explicit value_name with no own entry falls back to the active version.
"""
function channel_names(img::CciaImage; value_name=nothing)::Union{Vector{String},Nothing}
    isempty(img.im_channel_names) && return nothing
    v = versioned_get(img.im_channel_names, value_name)   # nothing → active
    isnothing(v) && !isnothing(value_name) && (v = versioned_get(img.im_channel_names, nothing))
    isnothing(v) ? nothing : collect(String, v)
end

function save!(img::CciaImage)
    d = Dict{String,Any}(
        "class"          => "CciaImage",
        "uid"            => img.uid,
        "name"           => img.name,
        "kind"           => img.kind,
        "status"         => img.status,
        "filepath"       => img.filepath,
        "labels"         => img.labels,
        "label_props"    => img.label_props,
        "imChannelNames" => img.im_channel_names,
        "attr"           => img.attr,
        "meta"           => img.meta,
    )
    open(joinpath(img._dir, "ccid.json"), "w") do f
        JSON3.pretty(f, d)
    end
end

"""
Physical pixel sizes for the image, read from `img.meta`.
Returns `(pixel_res, time_step)` where `pixel_res` is a `Vector{Float64}` of µm/px per
spatial axis in skimage order (z, y, x for 3-D), and `time_step` is **minutes/frame**.
Mirrors `cciaImage\$omeXMLPixelRes()` / `omeXMLTimelapseInfo()` from the old R version — which
reports the interval in minutes (`TimeIncrement / 60`), so track speed comes out in **µm/min**
(the celltrackR / T-cell convention, ~10 µm/min) rather than µm/s.

The values are persisted into `meta` at import time (the omezarr import reads them from the
OME-XML via `ome_types`/`dim_utils`); this accessor is pure-Julia and does no Python/XML I/O.
Any axis (or the time step) absent from `meta` falls back to `1.0` (pixel-space), so measures
remain correct in pixel/frame units when physical metadata is unavailable.
"""
function img_physical_sizes(img::CciaImage)::Tuple{Vector{Float64}, Float64}
    m = img.meta
    getf(key) = begin
        v = get(m, key, nothing)
        (isnothing(v) || v == "") ? 1.0 : something(tryparse_f64(v), 1.0)
    end
    sx = getf("PhysicalSizeX")
    sy = getf("PhysicalSizeY")
    sz = getf("PhysicalSizeZ")
    # OME TimeIncrement is in SECONDS; report minutes/frame (÷60) so speed is µm/min, matching the
    # old R omeXMLTimelapseInfo. Absent/blank → 1.0 min/frame fallback (parity with R interval=1).
    tiv = get(m, "TimeIncrement", nothing)
    ts = (isnothing(tiv) || tiv == "") ? 1.0 : something(tryparse_f64(tiv), 60.0) / 60.0
    ([sz, sy, sx], ts)   # skimage order: z, y, x
end

tryparse_f64(v::Real) = Float64(v)
tryparse_f64(v::AbstractString) = tryparse(Float64, v)
tryparse_f64(::Any) = nothing

function _load_image(dir::String)::CciaImage
    d = JSON3.read(read(joinpath(dir, "ccid.json"), String), Dict{String,Any})
    to_spaths(key) = Dict{String,String}(
        string(k) => string(v) for (k, v) in get(d, key, Dict{String,Any}()))
    # labels: Dict{String, Vector{String}} — value can be a list or a bare string (legacy)
    to_labels() = Dict{String,Vector{String}}(
        string(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(d, "labels", Dict{String,Any}()))
    icn = Dict{String,Any}(string(k) => v for (k, v) in get(d, "imChannelNames", Dict{String,Any}()))
    CciaImage(
        d["uid"], d["name"], get(d, "kind", "static"), get(d, "status", "pending"),
        to_spaths("filepath"), to_labels(), to_spaths("label_props"),
        icn,
        to_spaths("attr"),
        Dict{String,Any}(get(d, "meta", Dict{String,Any}())), dir,
        Dict{String,Any}(),                 # _pop_df_cache (runtime only)
    )
end
