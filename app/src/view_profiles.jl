# ── view_profiles.jl — read user drop-in sidebar profiles ─────────────────────────
#
# A **view profile** is a named, ordered subset of the sidebar pages that already exist, so a user
# doing narrow work isn't navigating a 20-item menu of pages they never touch. Same drop-in spirit as
# custom modules — a file in the per-user config dir, no rebuild:
#
#   <config_dir>/profiles/<id>.json   # { "label": "Gating + behaviour", "items": ["/gate", …] }
#
# The filename stem is the id (so a file can be renamed without editing it). A profile can only
# include/exclude/reorder EXISTING routes — it cannot invent a page, and it is NOT access control: a
# hidden page stays reachable by direct URL.
#
# This reader validates SHAPE ONLY. It deliberately does not know the route table — that lives in
# `frontend/src/main.ts` — so an item pointing at a route that no longer exists is resolved in the
# frontend against the live router, and reported there. See docs/todo/VIEW_PROFILES_PLAN.md.

"""
    view_profiles_dir([dev_dir]) -> String

The per-user view-profiles root, `<config_dir>/profiles` (see [`config_dir`](@ref)). Not created here:
reading a missing dir is the normal "no profiles" case.
"""
view_profiles_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "profiles")

"""
    parse_view_profile(id, raw) -> NamedTuple

Validate one decoded profile document. Returns `(; id, label, items)` on success; throws
`ArgumentError` with a user-facing message otherwise. `label` falls back to the id.

An empty `items` is an ERROR, not an empty profile — rendering a blank sidebar from a typo would look
like the app broke.
"""
function parse_view_profile(id::AbstractString, raw)::NamedTuple
    raw isa AbstractDict || throw(ArgumentError("profile must be a JSON object"))
    items_raw = get(raw, "items", get(raw, :items, nothing))
    items_raw === nothing && throw(ArgumentError("profile needs an `items` array of route paths"))
    items_raw isa AbstractVector || throw(ArgumentError("`items` must be an array of route paths"))
    items = String[]
    for it in items_raw
        it isa AbstractString || throw(ArgumentError("`items` entries must be strings, got $(typeof(it))"))
        s = strip(String(it))
        startswith(s, "/") || throw(ArgumentError("route path must start with '/': $(repr(s))"))
        s in items || push!(items, s)   # de-dup, keep first position
    end
    isempty(items) && throw(ArgumentError("`items` is empty — a profile must show at least one page"))
    label_raw = get(raw, "label", get(raw, :label, nothing))
    label = label_raw isa AbstractString && !isempty(strip(String(label_raw))) ?
        strip(String(label_raw)) : String(id)
    (; id = String(id), label = String(label), items)
end

"""
    read_view_profiles([; dev_dir]) -> (; dir, profiles, errors)

Scan `<config_dir>/profiles/*.json`. Returns the resolved `dir`, the valid `profiles`
(`(; id, label, items)`, sorted by label) and per-file `errors` (`(; file, error)`).

Never throws: a broken profile is reported, so the Settings panel can name it instead of the sidebar
silently shrinking.
"""
function read_view_profiles(; dev_dir::Union{String,Nothing} = nothing)
    dir = view_profiles_dir(dev_dir)
    profiles = NamedTuple[]
    errors   = NamedTuple[]
    isdir(dir) || return (; dir, profiles, errors)
    for f in sort(readdir(dir))
        (endswith(f, ".json") && !startswith(f, ".")) || continue
        path = joinpath(dir, f)
        try
            raw = JSON3.read(read(path, String))
            push!(profiles, parse_view_profile(splitext(f)[1], raw))
        catch e
            msg = e isa ArgumentError ? e.msg : sprint(showerror, e)
            push!(errors, (; file = f, error = msg))
            @warn "Invalid view profile" path error = msg
        end
    end
    sort!(profiles; by = p -> lowercase(p.label))
    (; dir, profiles, errors)
end

"""
    view_profile_id(label) -> String

The file-safe id derived from a user-typed label (`safe_name_part`). The user never types a filename.
Throws `ArgumentError` when nothing survives sanitising (e.g. a label of only punctuation).
"""
function view_profile_id(label::AbstractString)::String
    id = safe_name_part(lowercase(strip(String(label))))
    isempty(id) && throw(ArgumentError("profile name must contain at least one letter or digit"))
    id
end

"""
    write_view_profile(label, items; id = nothing, dev_dir = nothing) -> NamedTuple

Create or overwrite `<config_dir>/profiles/<id>.json` and return the stored
`(; id, label, items)`. `id` defaults to one derived from `label` — pass it explicitly to rename a
profile's label **without** changing its id, so an active selection doesn't break (Decision 4).

Validated through [`parse_view_profile`](@ref) before it touches disk, so the reader and the writer
agree on what a valid profile is, and written with `write_json_atomic` — a half-written profile would
be reported as broken on next read.
"""
function write_view_profile(label::AbstractString, items::AbstractVector;
                            id::Union{AbstractString,Nothing} = nothing,
                            dev_dir::Union{String,Nothing} = nothing)::NamedTuple
    pid  = id === nothing ? view_profile_id(label) : view_profile_id(id)
    prof = parse_view_profile(pid, Dict{String,Any}("label" => label, "items" => collect(items)))
    dir  = view_profiles_dir(dev_dir)
    mkpath(dir)
    write_json_atomic(joinpath(dir, "$(pid).json"),
                      Dict{String,Any}("label" => prof.label, "items" => prof.items))
    prof
end

"""
    delete_view_profile!(id; dev_dir = nothing) -> Bool

Remove `<config_dir>/profiles/<id>.json`. Returns `false` when it wasn't there (deleting an
already-gone profile is not an error). The id is sanitised, so a request cannot escape the dir.
"""
function delete_view_profile!(id::AbstractString; dev_dir::Union{String,Nothing} = nothing)::Bool
    path = joinpath(view_profiles_dir(dev_dir), "$(view_profile_id(id)).json")
    isfile(path) || return false
    rm(path)
    true
end
