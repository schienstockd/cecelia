using JSON3

mutable struct CciaSet
    uid::String
    name::String
    image_uids::Vector{String}
    meta::Dict{String,Any}
    _dir::String               # {proj}/1/{uid}/ — runtime only
    _images::Vector{CciaImage} # runtime only — not persisted
end

function CciaSet(; uid=gen_uid(), name="", dir="")
    CciaSet(uid, name, String[], Dict{String,Any}(), dir, CciaImage[])
end

state_file(s::CciaSet)::String = joinpath(s._dir, STATE_FILENAME)

function save!(s::CciaSet)
    d = Dict{String,Any}(
        "class"      => "CciaSet",
        "uid"        => s.uid,
        "name"       => s.name,
        "image_uids" => s.image_uids,
        "meta"       => s.meta,
    )
    write_json_atomic(state_file(s), d)
    for img in s._images
        save!(img)
    end
end

# proj_dir = {projects_dir}/{proj_uid}/
# Legacy ccid.jsons may still carry a `kind` key — silently ignored (project-wide static/live/flow
# distinction was dropped in favour of per-image axis gating; see Cecelia.task_applies).
function _load_set(proj_dir::String, set_uid::String)::CciaSet
    dir = joinpath(proj_dir, "1", set_uid)
    d = read_state_json(state_file(dir); as = Dict{String,Any})
    uids = String.(collect(d["image_uids"]))
    s = CciaSet(d["uid"], d["name"],
                uids, Dict{String,Any}(get(d, "meta", Dict{String,Any}())),
                dir, CciaImage[])
    for uid in uids
        push!(s._images, _load_image(joinpath(proj_dir, "1", uid)))
    end
    s
end

"""
Load a set or image by project UID + object UID.
Dispatches on the "class" field in ccid.json — no need to know the type in advance.
"""
function init_object(proj_uid::String, uid::String)
    proj_dir = joinpath(projects_dir(), proj_uid)
    dir = joinpath(proj_dir, "1", uid)
    d = read_state_json(state_file(dir); as = Dict{String,Any})
    if get(d, "class", "") == "CciaSet"
        _load_set(proj_dir, uid)
    else
        _load_image(dir)
    end
end

function add_image!(s::CciaSet;
    name::String,
    meta::Dict{String,Any}      = Dict{String,Any}(),
    attr::Dict{String,String}   = Dict{String,String}(),
    uid::String                 = gen_uid()   # override to preserve a UID (e.g. legacy migration)
)::CciaImage
    img      = CciaImage(uid=uid, name=name)
    # s._dir = {proj}/1/{set_uid}  →  dirname×2 = {proj}
    proj_dir = dirname(dirname(s._dir))
    meta_dir = joinpath(proj_dir, "1", img.uid)
    img_dir  = joinpath(proj_dir, "0", img.uid)
    mkpath(meta_dir)
    mkpath(img_dir)
    img._dir = meta_dir
    img.meta = meta
    # `attr` is the user-editable cohort/condition/mouse tag dict, distinct from the compute-derived
    # `meta`. Threading it here so a task that mints a NEW image (copy/crop/project/bin/resample) can
    # carry the source's tags forward — otherwise the derived image is invisible to
    # attr_value_counts / compare-by-attribute, and the user has to re-tag every copy. Empty default
    # keeps the import path (no source) unchanged.
    img.attr = copy(attr)
    save!(img)
    push!(s.image_uids, img.uid)
    push!(s._images, img)
    save!(s)
    img
end

function images(s::CciaSet)::Vector{CciaImage}
    s._images
end

"""
    attr_value_counts(imgs; included_only=false) -> Vector{Pair{String,Vector{Pair{String,Int}}}}

Tabulate the user-defined image ATTRIBUTES across `imgs`: for each attribute name, its distinct
values and how many images carry each. Attribute names come back in first-appearance order and
values sorted, so the shape is stable for both a picker and a diff. Empty values are skipped (an
unset attribute is not a level).

THE one tabulation of `img.attr` across a collection — `api_plot_attrs` (the compare-by-attribute
picker) and `la_gaps` (LabArchives cohort-vs-project, `ai/labarchives.jl`) both read it, so "which
levels exist" can't come out two different ways. Note what it measures: levels are derived from the
images PRESENT, so deleting an arm deletes the evidence that it existed — which is exactly why the
LabArchives sidecar has to carry the declared cohort separately.

`included_only=true` drops excluded images (`included == false`), i.e. counts what analysis will
actually see.
"""
function attr_value_counts(imgs; included_only::Bool = false)
    names = String[]
    vals  = Dict{String,Dict{String,Int}}()
    for im in imgs
        (included_only && !image_included(im)) && continue
        for (k, v) in im.attr
            ks, vs = string(k), string(v)
            isempty(vs) && continue
            haskey(vals, ks) || (push!(names, ks); vals[ks] = Dict{String,Int}())
            vals[ks][vs] = get(vals[ks], vs, 0) + 1
        end
    end
    [n => [v => vals[n][v] for v in sort(collect(keys(vals[n])))] for n in names]
end

"""
    image_attr_groups(imgs, uids, attrs) -> Dict{String,String}

uID → the COMBINED value of `attrs` on that image: each attribute's value in the order asked, empty
components dropped, joined with `"."` (mirroring the old R `paste0(axisX, ".", interaction)`). An image
with no value for any of them is ABSENT from the map — callers fall back to its uID, so a missing
attribute reads as "its own group" rather than as an empty label shared with every other gap.

THE one place "group these images by attribute" is answered. `POST /api/plot_data` built this inline for
the summary canvas, and the track plots (`/api/tracking/paths`, `/api/tracking/diagnostics`) need the
same grouping to put treatments side by side — three sites, so the join rule lives here instead of being
re-spelled per route. Pairs with [`attr_value_counts`], which answers *which levels exist*.
"""
function image_attr_groups(imgs, uids, attrs)::Dict{String,String}
    out = Dict{String,String}()
    isempty(attrs) && return out
    for (im, uid) in zip(imgs, uids)
        v = join(filter(!isempty, String[string(get(im.attr, a, "")) for a in attrs]), ".")
        isempty(v) || (out[string(uid)] = v)
    end
    out
end

"""
    image_by_uid(s::CciaSet; uid) -> Union{CciaImage,Nothing}

Look up one image by `uid` within the set (nothing if absent).
"""
function image_by_uid(s::CciaSet; uid::AbstractString)::Union{CciaImage,Nothing}
    idx = findfirst(i -> i.uid == uid, s._images)
    isnothing(idx) ? nothing : s._images[idx]
end


"""
Delete an image from the set: removes its data dir ({proj}/0/{uid}) and metadata
dir ({proj}/1/{uid}) from disk, drops it from the set manifest, and persists the set.
"""
function delete_image!(s::CciaSet, image_uid::String)::CciaSet
    proj_dir = dirname(dirname(s._dir))   # s._dir = {proj}/1/{set_uid}
    for sub in ("0", "1")
        d = joinpath(proj_dir, sub, image_uid)
        isdir(d) && rm(d; recursive = true)
    end
    filter!(u -> u != image_uid, s.image_uids)
    filter!(img -> img.uid != image_uid, s._images)
    save!(s)
    s
end
