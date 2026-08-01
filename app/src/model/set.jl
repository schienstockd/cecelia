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
    meta::Dict{String,Any} = Dict{String,Any}(),
    uid::String            = gen_uid()   # override to preserve a UID (e.g. legacy migration)
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
    image_by_uid(s::CciaSet; uid) -> Union{CciaImage,Nothing}

Look up one image by `uid` within the set (nothing if absent).
"""
function image_by_uid(s::CciaSet; uid::AbstractString)::Union{CciaImage,Nothing}
    idx = findfirst(i -> i.uid == uid, s._images)
    isnothing(idx) ? nothing : s._images[idx]
end

# ── Reference image ───────────────────────────────────────────────────────────────────────────────
#
# One image per set, nominated by the user as REPRESENTATIVE of the set. It is a property of the set,
# deliberately not of any task: the first consumer is the 8-bit import (deriving one intensity window
# for the whole set from the reference, so channels and images stay comparable), but "process this
# set the way this image says" is a shape that recurs — a normalisation baseline, a gating template,
# a parameter sweep's anchor. Any of those reads the same field instead of inventing its own.
#
# Why a nominated image rather than a derived value: the alternative is asking the user for a number
# (`rescaleFixedMax = 1500`), which they can only get by eyeballing a histogram. Picking "this movie
# looks typical" is a judgement they can actually make. The consuming task derives the number.
#
# Unset is the normal state and must stay usable — a consumer falls back to its per-image behaviour.

const REFERENCE_IMAGE_KEY = "referenceImage"

"""
    reference_image_uid(s::CciaSet) -> Union{String,Nothing}

The set's reference image uid, or `nothing` when none is set **or when the nominated image is no
longer in the set** (deleted, moved) — a caller gets one answer for "there is no usable reference"
rather than having to re-check membership itself.
"""
function reference_image_uid(s::CciaSet)::Union{String,Nothing}
    uid = get(s.meta, REFERENCE_IMAGE_KEY, nothing)
    uid isa AbstractString || return nothing
    String(uid) in s.image_uids ? String(uid) : nothing
end

"""
    set_reference_image!(s::CciaSet, uid) -> CciaSet

Nominate `uid` as the set's reference, or clear it with `nothing`. Errors if the image is not in the
set — a reference pointing outside it is a bug, not a state to persist.
"""
function set_reference_image!(s::CciaSet, uid::Union{AbstractString,Nothing})::CciaSet
    if isnothing(uid) || isempty(uid)
        delete!(s.meta, REFERENCE_IMAGE_KEY)
    else
        String(uid) in s.image_uids ||
            error("set_reference_image!: $uid is not in set $(s.uid)")
        s.meta[REFERENCE_IMAGE_KEY] = String(uid)
    end
    save!(s)
    s
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
