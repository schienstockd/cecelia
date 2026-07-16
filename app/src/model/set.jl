using JSON3

mutable struct CciaSet
    uid::String
    name::String
    kind::String
    image_uids::Vector{String}
    meta::Dict{String,Any}
    _dir::String               # {proj}/1/{uid}/ — runtime only
    _images::Vector{CciaImage} # runtime only — not persisted
end

function CciaSet(; uid=gen_uid(), name="", kind="static", dir="")
    CciaSet(uid, name, kind, String[], Dict{String,Any}(), dir, CciaImage[])
end

function save!(s::CciaSet)
    d = Dict{String,Any}(
        "class"      => "CciaSet",
        "uid"        => s.uid,
        "name"       => s.name,
        "kind"       => s.kind,
        "image_uids" => s.image_uids,
        "meta"       => s.meta,
    )
    open(joinpath(s._dir, "ccid.json"), "w") do f
        JSON3.pretty(f, d)
    end
    for img in s._images
        save!(img)
    end
end

# proj_dir = {projects_dir}/{proj_uid}/
function _load_set(proj_dir::String, set_uid::String)::CciaSet
    dir = joinpath(proj_dir, "1", set_uid)
    d = JSON3.read(read(joinpath(dir, "ccid.json"), String), Dict{String,Any})
    uids = String.(collect(d["image_uids"]))
    s = CciaSet(d["uid"], d["name"], get(d, "kind", "static"),
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
    d = JSON3.read(read(joinpath(dir, "ccid.json"), String), Dict{String,Any})
    if get(d, "class", "") == "CciaSet"
        _load_set(proj_dir, uid)
    else
        _load_image(dir)
    end
end

function add_image!(s::CciaSet;
    name::String,
    kind::String           = s.kind,
    meta::Dict{String,Any} = Dict{String,Any}(),
    uid::String            = gen_uid()   # override to preserve a UID (e.g. legacy migration)
)::CciaImage
    img      = CciaImage(uid=uid, name=name, kind=kind)
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
