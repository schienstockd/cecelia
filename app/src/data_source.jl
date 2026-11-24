# Data source — where a set's (or one image's) images came from: the citation, DOI, URL and licence a
# figure, Methods draft or crate made from them has to carry. Plan: docs/todo/DATA_SOURCE_PLAN.md.
#
# Stored as `meta["data_source"]` on the set, and optionally on an image, where it overrides the set's
# (a set mixing two datasets). `data_source(proj, img)` is the one resolver: the image's own, else its
# set's. Empty = unknown, never an error. Not called `source` (the acquisition file format) nor
# `dataSource` (the plot definitions' key).

const DATA_SOURCE_KEY = "data_source"

"""Who filled a source in: import recognised a repository ID, or the user wrote it."""
@enum DataSourceBy SOURCE_BY_IMPORT SOURCE_BY_USER
const _SOURCE_BY_STR   = Dict(SOURCE_BY_IMPORT => "import", SOURCE_BY_USER => "user")
const _SOURCE_BY_PARSE = Dict(v => k for (k, v) in _SOURCE_BY_STR)
Base.string(b::DataSourceBy) = _SOURCE_BY_STR[b]

"""
One data source. Every text field may be empty. `repository` + `id` are set when import recognised a
repository ID ("hubmap", "HBM666.MCZF.973"); `confirmed` is false for an import suggestion until the
user accepts it, and true for anything the user wrote.
"""
struct DataSource
    citation::String
    doi::String
    url::String
    licence::String
    repository::String
    id::String
    by::DataSourceBy
    confirmed::Bool
end

const _SOURCE_TEXT_FIELDS = ("citation", "doi", "url", "licence", "repository", "id")

"""
    data_source_from(raw) -> Union{DataSource,Nothing}

Read a stored or posted source. Text fields are trimmed; a DOI given as a doi.org link or with a
`doi:` prefix is stored bare. A source with no citation, DOI, URL or repository ID says nothing and
reads as `nothing`. `by` defaults to "user" (confirmed); an unknown `by` is an error naming the
allowed values, because the value comes from a request body.
"""
function data_source_from(raw::AbstractDict)::Union{DataSource,Nothing}
    get_s(k) = strip(string(something(get(raw, k, get(raw, Symbol(k), "")), "")))
    t = Dict(k => String(get_s(k)) for k in _SOURCE_TEXT_FIELDS)
    t["doi"] = _bare_doi(t["doi"])
    all(isempty, (t["citation"], t["doi"], t["url"], t["id"])) && return nothing
    by_s = lowercase(get_s("by"))
    by = isempty(by_s) ? SOURCE_BY_USER :
         get(_SOURCE_BY_PARSE, by_s) do
             error("Unknown source 'by' \"$by_s\" (expected one of: $(join(sort(collect(keys(_SOURCE_BY_PARSE))), ", ")))")
         end
    conf_raw = get(raw, "confirmed", get(raw, :confirmed, nothing))
    confirmed = conf_raw === nothing ? by == SOURCE_BY_USER : conf_raw === true
    DataSource(t["citation"], t["doi"], t["url"], t["licence"], t["repository"], t["id"], by, confirmed)
end

# "https://doi.org/10.1/x", "doi:10.1/x" and "10.1/x" are one DOI; store the bare form.
_bare_doi(s::AbstractString)::String =
    String(replace(s, r"^(https?://(dx\.)?doi\.org/|doi:\s*)"i => ""))

"""The on-disk / on-wire form: every field, `by` by name."""
data_source_dict(d::DataSource)::Dict{String,Any} = Dict{String,Any}(
    "citation" => d.citation, "doi" => d.doi, "url" => d.url, "licence" => d.licence,
    "repository" => d.repository, "id" => d.id, "by" => string(d.by), "confirmed" => d.confirmed)

Base.:(==)(a::DataSource, b::DataSource) = data_source_dict(a) == data_source_dict(b)

_stored_source(meta::AbstractDict) = (raw = get(meta, DATA_SOURCE_KEY, nothing);
                                     raw isa AbstractDict ? data_source_from(raw) : nothing)

"""The set's own source, or `nothing`."""
data_source(s::CciaSet)::Union{DataSource,Nothing} = _stored_source(s.meta)

"""The image's own source (its override), or `nothing` — not the resolved one: see `data_source(proj, img)`."""
data_source(img::CciaImage)::Union{DataSource,Nothing} = _stored_source(img.meta)

"""
    image_set(proj, img_or_uid) -> Union{CciaSet,Nothing}

The set an image belongs to. A `CciaImage` has no pointer to its set; this is the one lookup.
`image_set_index` gives its position in `proj._sets` for callers that index.
"""
image_set_index(proj::CciaProject, uid::AbstractString)::Union{Int,Nothing} =
    findfirst(s -> uid in s.image_uids, proj._sets)
image_set_index(proj::CciaProject, img::CciaImage) = image_set_index(proj, img.uid)
function image_set(proj::CciaProject, img_or_uid)::Union{CciaSet,Nothing}
    i = image_set_index(proj, img_or_uid)
    i === nothing ? nothing : proj._sets[i]
end

"""
    data_source(proj, img) -> Union{DataSource,Nothing}

Where this image came from: its own source, else its set's, else `nothing` (unknown).
"""
function data_source(proj::CciaProject, img::CciaImage)::Union{DataSource,Nothing}
    own = data_source(img)
    own === nothing || return own
    s = image_set(proj, img)
    s === nothing ? nothing : data_source(s)
end

# Write `d` (nothing deletes) into a raw ccid dict's meta, String-keyed (nested values come back
# read-only with Symbol keys, as in set_experiment_design!).
function _write_source_meta!(raw, d::Union{DataSource,Nothing})
    m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    d === nothing ? delete!(m, DATA_SOURCE_KEY) : (m[DATA_SOURCE_KEY] = data_source_dict(d))
    raw["meta"] = m
end
_write_source_meta_mem!(meta::AbstractDict, d) =
    d === nothing ? delete!(meta, DATA_SOURCE_KEY) : (meta[DATA_SOURCE_KEY] = data_source_dict(d))

"""
    set_data_source!(set, d) / set_data_source!(proj, set_uid, d) -> CciaSet

Write the set's source (`nothing` clears it) inside the set's transaction.
"""
function set_data_source!(s::CciaSet, d::Union{DataSource,Nothing})::CciaSet
    commit_state!(raw -> _write_source_meta!(raw, d), s)
    _write_source_meta_mem!(s.meta, d)
    s
end
function set_data_source!(proj::CciaProject, set_uid::AbstractString, d::Union{DataSource,Nothing})::CciaSet
    idx = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(idx) && error("Set not found: $set_uid")
    set_data_source!(proj._sets[idx], d)
end

"""
    set_image_data_source!(img, d) -> CciaImage

Write one image's own source, its override of the set's (`nothing` clears it, so the set's applies).
"""
function set_image_data_source!(img::CciaImage, d::Union{DataSource,Nothing})::CciaImage
    commit_state!(raw -> _write_source_meta!(raw, d), img)
    _write_source_meta_mem!(img.meta, d)
    img
end

"""The resolved source as the wire form plus where it came from (`"image"` / `"set"`), or `nothing`.
The `(img, set)` form is for a caller already holding the image's set."""
data_source_reply(proj::CciaProject, img::CciaImage) = data_source_reply(img, image_set(proj, img))
function data_source_reply(img::CciaImage, s::Union{CciaSet,Nothing})
    own = data_source(img)
    own === nothing || return merge(data_source_dict(own), Dict{String,Any}("from" => "image"))
    d = s === nothing ? nothing : data_source(s)
    d === nothing ? nothing : merge(data_source_dict(d), Dict{String,Any}("from" => "set"))
end

"""
    inherit_data_source!(dst_meta, proj, src_img, dest_set) -> dst_meta

Give a derived image landing in ANOTHER set (a crop set) its source image's set-level source, when its
meta has none yet. `copy_source_meta!` already carries the image's own. A derived image that stays in
its source's set gets nothing here: it resolves to the set's source live, so a later correction of the
set's source reaches it too.
"""
function inherit_data_source!(dst::AbstractDict, proj::CciaProject, src::CciaImage, dest::CciaSet)
    haskey(dst, DATA_SOURCE_KEY) && return dst
    from = image_set(proj, src)
    (from === nothing || from.uid == dest.uid) && return dst
    d = data_source(from)
    d === nothing || (dst[DATA_SOURCE_KEY] = data_source_dict(d))
    dst
end

# ── Import suggestions ──────────────────────────────────────────────────────────────────────────────
# Offline only: what follows from an ID in the path or a record the importer already holds. Each is
# stored unconfirmed (`by = import`) for the user to accept. A repository is added here only once a
# real dataset from it confirms the pattern (DATA_SOURCE_PLAN, Decision 4).

# HuBMAP dataset IDs ("HBM666.MCZF.973"); a HuBMAP download folder is "<id>-<uuid>". HuBMAP builds its
# DOIs from the ID under the 10.35079 prefix (checked: doi:10.35079/HBM666.MCZF.973 resolves to the
# dataset). https://docs.hubmapconsortium.org/
const _HUBMAP_ID = r"(?<![A-Za-z0-9])(HBM\d{3}\.[A-Z]{4}\.\d{3})(?![A-Za-z0-9])"

"""
    suggest_data_source(path) -> Union{DataSource,Nothing}

A source suggested from an imported file's path, or `nothing`. Recognises a HuBMAP dataset ID in any
path component, giving the ID, its DOI and its portal search link. Pure: never goes online.
"""
function suggest_data_source(path::AbstractString)::Union{DataSource,Nothing}
    for part in Iterators.reverse(splitpath(path))
        m = match(_HUBMAP_ID, part)
        m === nothing && continue
        id = String(m.captures[1])
        return DataSource("HuBMAP $id", "10.35079/$id", "https://doi.org/10.35079/$id", "",
                          "hubmap", id, SOURCE_BY_IMPORT, false)
    end
    nothing
end

"""
    immunemap_data_source(im) -> Union{DataSource,Nothing}

A source suggested from the record Import from Immunemap keeps (`meta["immunemap"]`): the experiment's
name, link and first DOI (the experiment's, else the video's). `nothing` when it names no experiment.
"""
function immunemap_data_source(im::AbstractDict)::Union{DataSource,Nothing}
    g(k) = get(im, k, get(im, Symbol(k), nothing))
    name = strip(string(something(g("experimentName"), "")))
    url  = strip(string(something(g("experimentUrl"), "")))
    eid  = string(something(g("experimentId"), ""))
    dois = vcat(collect(something(g("experimentDois"), Any[])), collect(something(g("dois"), Any[])))
    doi  = isempty(dois) ? "" : _bare_doi(strip(string(first(dois))))
    isempty(name) && isempty(url) && isempty(doi) && return nothing
    DataSource(isempty(name) ? "Immunemap" : "Immunemap: $name", doi, url, "", "immunemap", eid,
               SOURCE_BY_IMPORT, false)
end
