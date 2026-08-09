# Movie registry — per-project metadata DECORATING the .mp4s under {proj}/movies/.
#
# The directory listing is the truth; this file is decoration (docs/todo/MOVIE_MANAGEMENT_PLAN.md
# Decision 1). Same shape as settings/notebooks.json, which is this exact problem already solved:
# artefacts that live as files on disk, with a per-project JSON keyed by filename holding what the
# filesystem can't. It sits under settings/, which `_mirror_tree!` copies verbatim into a .ccbundle, so
# it travels with the project — unlike the generation config, which lived in the browser's
# localStorage and went no further than one machine.
#
# `_movies_dir_for_project` / `_valid_movie_name` / `api_movies_list` live in routes.jl beside the other
# project-scoped paths; this file owns everything ABOUT a movie rather than the bytes of one.
using Dates

# Bumped when a saved config's SHAPE changes. It exists to report what could not be restored, never to
# reject an entry — a config is read field-by-field through defaults (Decision 6). The failure that
# actually bites is a dangling reference (a deleted segmentation), which no version number can catch.
const MOVIE_CONFIG_VERSION = 1

_movies_registry_path(uid::AbstractString) = joinpath(_settings_dir_for_project(uid), "movies.json")

function _read_movies_registry(uid::AbstractString)::Dict{String,Any}
    p = _movies_registry_path(uid)
    isfile(p) || return Dict{String,Any}()
    try
        Dict{String,Any}(String(k) => Dict{String,Any}(v)
                         for (k, v) in JSON3.read(read(p, String), Dict{String,Any}))
    catch
        # A corrupt registry must not take the Movies page down with it — the movies themselves are
        # still listable, which is the whole point of keeping this as decoration.
        @warn "movies registry unreadable — ignoring" path = p
        Dict{String,Any}()
    end
end

function _write_movies_registry!(uid::AbstractString, reg::AbstractDict)
    mkpath(_settings_dir_for_project(uid))
    write_json_atomic(_movies_registry_path(uid), reg)
end

# A display name is what the list SHOWS; the file on disk is never renamed (Decision 2). Capped so a
# name can't bloat the row, and newlines/tabs collapse — it is a label, not a note.
#
# Deliberately NOT `_safe_name_part` (napari_api.jl): that is the FILENAME rule and strips spaces,
# brackets and punctuation, which is exactly what a human label is allowed to have. Nothing here ever
# reaches a path — that is the whole point of Decision 2 — so the two must not be unified.
const MOVIE_NAME_MAX = 120
function _clean_movie_name(s)::String
    t = strip(replace(String(s === nothing ? "" : s), r"\s+" => " "))
    length(t) > MOVIE_NAME_MAX ? String(first(t, MOVIE_NAME_MAX)) : String(t)
end

# Free-form tags (Decision 3) — the growing taxonomy, with no code change per new kind. Deduped,
# order-preserving, blanks dropped, each one cleaned like a name.
const MOVIE_TAG_MAX  = 40
const MOVIE_TAGS_MAX = 20
function _clean_movie_tags(raw)::Vector{String}
    raw === nothing && return String[]
    out = String[]
    for t in raw
        s = strip(replace(String(t), r"\s+" => " "))
        isempty(s) && continue
        length(s) > MOVIE_TAG_MAX && (s = String(first(s, MOVIE_TAG_MAX)))
        s ∈ out || push!(out, String(s))
        length(out) >= MOVIE_TAGS_MAX && break
    end
    out
end

# Who recorded it (Decision 3). Written by the recorder, never by the user, so it needs no maintenance
# and gives the fixed axis that free-form tags deliberately don't.
const MOVIE_PRODUCERS = ("viewer", "animation", "batch")
_clean_producer(s)::String = (t = String(s === nothing ? "" : s); t ∈ MOVIE_PRODUCERS ? t : "")

"""
    register_movie!(project_uid, filename; produced_by, config, config_kind, config_version)

Record how a movie was made, right after its bytes land (Phase 4). MERGES into any existing entry, so
the user's display name / star / tags survive a re-record of the same filename — which happens by
design, since a movie is named after its image and re-recording replaces the file.

`recordedAt` is stamped here and is what the stale-config rule compares the file's mtime against
(Decision 5). Best-effort: a registry write must never fail a render that already succeeded.
"""
function register_movie!(project_uid::AbstractString, filename::AbstractString;
                         produced_by::AbstractString = "", config = nothing,
                         config_kind::AbstractString = "", config_version::Int = MOVIE_CONFIG_VERSION)
    try
        _valid_movie_name(filename) || return nothing
        reg = _read_movies_registry(project_uid)
        e   = get(reg, String(filename), Dict{String,Any}())
        e["producedBy"] = _clean_producer(produced_by)
        e["recordedAt"] = time()          # unix seconds — the same clock `mtime` uses, see _config_stale
        if config !== nothing && !isempty(config_kind)
            e["configKind"]    = String(config_kind)
            e["configVersion"] = config_version
            e["config"]        = config
        end
        reg[String(filename)] = e
        _write_movies_registry!(project_uid, reg)
    catch err
        @warn "could not register movie" file = filename exception = err
    end
    nothing
end

"""
    movies_with_meta(project_uid) -> Vector{NamedTuple}

The project's movies, newest first, each merged with its registry entry — and the registry reconciled
against the directory in the same pass (Decision 5):

  * an entry whose file is gone is DROPPED (a row that plays nothing is worse than no row), and
  * an entry older than its file is marked `configStale`: the metadata survived but the bytes under it
    came from a later run. That is the silent-overwrite case — a movie is named after its image, so
    re-recording it replaces the file while the entry stays put.

Reconciliation only ever REMOVES, and only writes when something actually changed, so listing a
project stays a read in the normal case.
"""
function movies_with_meta(project_uid::AbstractString)
    dir = _movies_dir_for_project(project_uid)
    reg = _read_movies_registry(project_uid)
    out = NamedTuple[]
    present = Set{String}()
    if isdir(dir)
        for name in readdir(dir)
            # skip in-progress temps: `prepend_title_to_movie` stages `{name}.mp4.tmp.mp4` beside the
            # real file and renames on success, so a run killed mid-encode leaves something a bare
            # `.mp4` filter would list as a real movie.
            (endswith(lowercase(name), ".mp4") && !occursin(".tmp.", name) &&
                isfile(joinpath(dir, name))) || continue
            push!(present, name)
            f = joinpath(dir, name)
            mt = mtime(f)
            e  = get(reg, name, Dict{String,Any}())
            push!(out, (; name, size = filesize(f), mtime = mt,
                          displayName = _clean_movie_name(get(e, "displayName", "")),
                          starred     = get(e, "starred", false) === true,
                          tags        = _clean_movie_tags(get(e, "tags", nothing)),
                          producedBy  = _clean_producer(get(e, "producedBy", "")),
                          hasConfig   = haskey(e, "config"),
                          configKind  = String(get(e, "configKind", "")),
                          configStale = _config_stale(e, mt)))
        end
        sort!(out; by = m -> m.mtime, rev = true)
    end
    orphans = setdiff(keys(reg), present)
    if !isempty(orphans)
        for k in orphans; delete!(reg, k); end
        try _write_movies_registry!(project_uid, reg) catch e
            @warn "could not prune movies registry" exception = e
        end
    end
    out
end

# An entry describes the file that was there when it was written. A newer file means a later run
# replaced those bytes, so whatever config the entry holds is no longer what produced them. A minute
# of slack: the stamp lands after the render, and a movie's mtime is whenever its last chunk was
# flushed, so the two legitimately differ by more than an instant.
#
# `recordedAt` is UNIX SECONDS, the same clock and units as `mtime`, deliberately — not the ISO string
# the notebooks registry uses for `updatedAt`. That one is only ever displayed; this one is COMPARED,
# and `string(Dates.now())` is naive LOCAL time while `datetime2unix` reads a DateTime as UTC. On
# UTC+10 that puts every stamp ten hours in the future, and nothing would ever read as stale.
const _CONFIG_STALE_SLACK_S = 60
function _config_stale(e::AbstractDict, file_mtime::Real)::Bool
    haskey(e, "config") || return false
    stamped = get(e, "recordedAt", nothing)
    stamped isa Real || return true          # absent, or an old ISO string — can't be vouched for
    file_mtime > stamped + _CONFIG_STALE_SLACK_S
end

# GET /api/movies/meta?projectUid=…&name=….mp4 → the full registry entry, INCLUDING the saved config
# (which the list route deliberately omits — a keyframe config is large and the list renders none of it).
function api_movies_meta_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    name  = get(query, "name", "")
    isempty(uid)  && return 400, JSON3.write((; error = "projectUid required"))
    _valid_movie_name(name) || return 400, JSON3.write((; error = "Invalid movie name"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    e = get(_read_movies_registry(uid), String(name), Dict{String,Any}())
    200, JSON3.write((; name, entry = e))
end

# POST /api/movies/meta {projectUid, name, displayName?, starred?, tags?}
# Set the user-owned metadata. Every field is OPTIONAL and absent means "leave alone", so the three
# controls on the page patch independently without reading each other's values first.
#
# Registers the movie even when no entry exists yet — every movie on disk predates this registry, so
# starring an old one has to be able to create its row.
function api_movies_meta_set(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    name = String(get(body, :name, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    _valid_movie_name(name) || return 400, JSON3.write((; error = "Invalid movie name"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    isfile(joinpath(_movies_dir_for_project(uid), name)) ||
        return 404, JSON3.write((; error = "Movie not found: $name"))

    reg = _read_movies_registry(uid)
    e   = get(reg, name, Dict{String,Any}())
    haskey(body, :displayName) && (e["displayName"] = _clean_movie_name(body[:displayName]))
    haskey(body, :starred)     && (e["starred"]     = Bool(body[:starred]))
    haskey(body, :tags)        && (e["tags"]        = _clean_movie_tags(body[:tags]))
    reg[name] = e
    _write_movies_registry!(uid, reg)
    200, JSON3.write((; ok = true, name,
                        displayName = _clean_movie_name(get(e, "displayName", "")),
                        starred = get(e, "starred", false) === true,
                        tags = _clean_movie_tags(get(e, "tags", nothing))))
end

# POST /api/movies/delete {projectUid, name} — remove ONE movie file and its registry entry.
# A movie is not an image: there are no version/label/analysis scopes to choose between
# (MOVIE_MANAGEMENT_PLAN.md → Not doing), so this is a plain delete behind the page's confirm.
function api_movies_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    name = String(get(body, :name, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    # The name guard is what keeps this route from deleting anything outside the movies dir: it admits
    # only [A-Za-z0-9._-]+.mp4, so no separator and no `..` can appear.
    _valid_movie_name(name) || return 400, JSON3.write((; error = "Invalid movie name"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    f = joinpath(_movies_dir_for_project(uid), name)
    isfile(f) || return 404, JSON3.write((; error = "Movie not found: $name"))
    rm(f; force = true)
    reg = _read_movies_registry(uid)
    if haskey(reg, name)
        delete!(reg, name)
        _write_movies_registry!(uid, reg)
    end
    @info "Deleted movie" name project = uid
    200, JSON3.write((; ok = true, name))
end
