# Data source routes — where a set's (or one image's) images came from (app/src/data_source.jl,
# docs/todo/DATA_SOURCE_PLAN.md). The set holds the source; an image may override it.

"""
    GET /api/data-source?projectUid&setUid

The set's own source and each image's own (its override, or null) and resolved one. Writes nothing.
"""
function api_data_source(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    s = _cohort_set(uid, set_uid)
    s isa Tuple && return s
    d = data_source(s)
    images = Any[]
    for iu in s.image_uids
        img = init_object(uid, iu)
        img isa CciaImage || continue
        own = data_source(img)
        push!(images, (; uid = img.uid, name = img.name,
                         own = own === nothing ? nothing : data_source_dict(own),
                         resolved = data_source_reply(img, s)))
    end
    200, JSON3.write((; source = d === nothing ? nothing : data_source_dict(d), images))
end

"""
    POST /api/data-source  { projectUid, setUid, imageUids?, source | null }

Write the set's source, or with `imageUids` those images' own (their override; null clears it so the
set's applies). A source the user writes is theirs and confirmed unless it says otherwise. 400 on a
shape error, naming it.
"""
function api_data_source_save(body_bytes::Vector{UInt8})
    data = try JSON3.read(body_bytes) catch; nothing end
    data isa AbstractDict || return 400, JSON3.write((; error = "JSON object required"))
    uid = string(get(data, :projectUid, "")); set_uid = string(get(data, :setUid, ""))
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    raw = get(data, :source, nothing)
    d = if raw === nothing
        nothing
    elseif raw isa AbstractDict
        try
            data_source_from(raw)
        catch e
            e isa ErrorException || rethrow()
            return 400, JSON3.write((; error = e.msg))
        end
    else
        return 400, JSON3.write((; error = "source must be an object or null"))
    end
    s = _cohort_set(uid, set_uid)
    s isa Tuple && return s
    img_uids = [string(x) for x in get(data, :imageUids, Any[])]
    if isempty(img_uids)
        set_data_source!(s, d)
    else
        for iu in img_uids
            iu in s.image_uids || return 400, JSON3.write((; error = "Image $iu is not in set $set_uid"))
        end
        for iu in img_uids
            img = init_object(uid, iu)
            img isa CciaImage && set_image_data_source!(img, d)
        end
    end
    200, JSON3.write((; ok = true))
end
