# ── Per-profile settings store (USER_PROFILE_PLAN Phase 4) ─────────────────────────
#
# The user profile is the app-wide identity primitive (see docs/todo/USER_PROFILE_PLAN.md). The
# credential dir under `<config_dir>/user-profiles/<name>/` was introduced in Phase P2 of
# LOGIN_CREDENTIAL_ISOLATION_PLAN for `.credentials.json`. This file adds a sibling
# `settings.toml` under the SAME dir — no second layout to reason about, no collision with the
# View Profiles files that already live at `<config_dir>/view-profiles/`.
#
# Storage layout:
#
#   <config_dir>/user-profiles/<name>/settings.toml
#
# The `default` profile is special everywhere else (credentials fall back to `~/.claude`) — for
# SETTINGS we give it a settings dir like every other profile. The `default` profile's
# `.credentials.json` still lives at `~/.claude`; only `settings.toml` sits under
# `<config_dir>/user-profiles/default/`. Rationale: a single-seat install must have SOMEWHERE to
# park its preferences, and shoving them into `custom.toml` would blur the install-wide /
# per-profile boundary that the whole plan exists to draw.
#
# All keys are frontend-owned strings/numbers/booleans — the store is a bag the frontend
# hydrates on launch and PATCHes when the user flips a switch. See `docs/audit/user-profile-
# field-audit.md` for the classification (~25 keys move here in Phase 4 / P4b).

"""
    profile_settings_dir(name = active_profile_name()) -> String

Directory holding the profile's `settings.toml`. Always returns a path — including for
`default` — because settings need a home for every profile. PURE — does NOT create the
directory; see `profile_settings_path!` for the live-resolver form.
"""
profile_settings_dir(name::AbstractString = active_profile_name();
                     config_root::AbstractString = config_dir())::String =
    joinpath(String(config_root), "user-profiles", String(name))

"""
    profile_settings_path(name = active_profile_name()) -> String

Path to `settings.toml` for the profile. PURE — does not create anything.
"""
profile_settings_path(name::AbstractString = active_profile_name();
                      config_root::AbstractString = config_dir())::String =
    joinpath(profile_settings_dir(name; config_root = config_root), "settings.toml")

"""
    profile_settings_path!(name = active_profile_name()) -> String

Live resolver — `mkpath`s the profile dir if missing, then returns the settings-file path.
Use this from the writer; readers should use the pure `profile_settings_path` and treat a
missing file as an empty bag.
"""
function profile_settings_path!(name::AbstractString = active_profile_name();
                                config_root::AbstractString = config_dir())::String
    d = profile_settings_dir(name; config_root = config_root)
    isdir(d) || mkpath(d)
    joinpath(d, "settings.toml")
end

"""
    read_profile_settings(name = active_profile_name()) -> Dict{String,Any}

Parse the profile's `settings.toml`, returning an empty dict when the file is missing or
unparseable (the frontend then uses its own defaults). No caching — the file is small and
the read happens once per launch per window. Tolerant of a hand-edited or half-written
file: a parse error yields the empty bag rather than a 500 on the API round-trip.
"""
function read_profile_settings(name::AbstractString = active_profile_name();
                               config_root::AbstractString = config_dir())::Dict{String,Any}
    path = profile_settings_path(name; config_root = config_root)
    isfile(path) || return Dict{String,Any}()
    try
        return TOML.parsefile(path)
    catch
        return Dict{String,Any}()
    end
end

"""
    write_profile_settings!(dict, name = active_profile_name()) -> Dict{String,Any}

Atomically replace the profile's `settings.toml` with `dict`. Creates the profile dir if
missing. Returns the dict written (for a read-your-writes shape in the API layer). This is
the *replace* form; `patch_profile_settings!` is what the API PATCH uses.
"""
function write_profile_settings!(dict::AbstractDict,
                                 name::AbstractString = active_profile_name();
                                 config_root::AbstractString = config_dir())::Dict{String,Any}
    path = profile_settings_path!(name; config_root = config_root)
    write_atomic(io -> TOML.print(io, dict), path)
    Dict{String,Any}(String(k) => v for (k, v) in dict)
end

# Serialises the read-modify-write: the settings store and `utils/profileStorage.ts` PATCH on
# independent debounces, and two concurrent handlers would otherwise drop one side's keys.
const _PROFILE_SETTINGS_LOCK = ReentrantLock()

"""
    patch_profile_settings!(patch, name = active_profile_name()) -> Dict{String,Any}

Shallow-merge `patch` into the profile's existing settings and rewrite atomically. Missing
file is treated as empty. A key whose value in `patch` is `nothing` is DELETED from the
stored bag (so the frontend can reset a key to its default by PATCHing `null`). Returns
the merged settings the file now contains.
"""
function patch_profile_settings!(patch::AbstractDict,
                                 name::AbstractString = active_profile_name();
                                 config_root::AbstractString = config_dir())::Dict{String,Any}
    lock(() -> _patch_profile_settings!(patch, name, config_root), _PROFILE_SETTINGS_LOCK)
end

function _patch_profile_settings!(patch::AbstractDict, name::AbstractString,
                                  config_root::AbstractString)::Dict{String,Any}
    current = read_profile_settings(name; config_root = config_root)
    merged  = Dict{String,Any}(String(k) => v for (k, v) in current)
    for (k, v) in patch
        key = String(k)
        if v === nothing
            delete!(merged, key)
        else
            merged[key] = v
        end
    end
    write_profile_settings!(merged, name; config_root = config_root)
end

# ── Per-profile recent projects ────────────────────────────────────────────────────────────────
#
# `project.json lastOpenedAt` is the INSTALL's last open — on a shared machine it orders the project
# list by whoever opened something last. Each profile keeps its own `{projectUid = ISO time}` here,
# next to settings.toml, and the project list overlays it. A project this profile has never opened
# keeps the project's own timestamp and sorts below every project it has.
#
#   <config_dir>/user-profiles/<name>/recent-projects.toml

profile_recents_path(name::AbstractString = active_profile_name();
                     config_root::AbstractString = config_dir())::String =
    joinpath(profile_settings_dir(name; config_root = config_root), "recent-projects.toml")

"""
    read_profile_recents(name = active_profile_name()) -> Dict{String,String}

`projectUid => ISO timestamp` of this profile's last open. Empty when missing or unparseable.
"""
function read_profile_recents(name::AbstractString = active_profile_name();
                              config_root::AbstractString = config_dir())::Dict{String,String}
    path = profile_recents_path(name; config_root = config_root)
    isfile(path) || return Dict{String,String}()
    try
        Dict{String,String}(String(k) => string(v) for (k, v) in TOML.parsefile(path))
    catch
        Dict{String,String}()
    end
end

"""
    touch_profile_recent!(uid, at, name = active_profile_name())

Record that this profile opened project `uid` at `at` (ISO string).
"""
function touch_profile_recent!(uid::AbstractString, at::AbstractString,
                               name::AbstractString = active_profile_name();
                               config_root::AbstractString = config_dir())
    lock(_PROFILE_SETTINGS_LOCK) do
        recents = read_profile_recents(name; config_root = config_root)
        recents[String(uid)] = String(at)
        profile_settings_path!(name; config_root = config_root)   # mkpath the profile dir
        write_atomic(io -> TOML.print(io, recents), profile_recents_path(name; config_root = config_root))
    end
    nothing
end

"""
    overlay_profile_recents!(projects, recents) -> projects

Replace each project's `lastOpenedAt` with this profile's own open time where it has one, and sort:
projects this profile opened first (newest first), then the rest by the project's own timestamp.
"""
function overlay_profile_recents!(projects::Vector{Dict{String,Any}}, recents::AbstractDict)
    for p in projects
        t = get(recents, string(get(p, "uid", "")), nothing)
        t === nothing || (p["lastOpenedAt"] = t)
    end
    sort!(projects; rev = true,
          by = p -> (haskey(recents, string(get(p, "uid", ""))),
                     string(get(p, "lastOpenedAt", get(p, "createdAt", "")))))
end

# ── Former names ────────────────────────────────────────────────────────────────────────────────
#
# A rename moves the profile dir but leaves the old name stamped on things that record it on purpose
# (Kiwi turns) or key by it (lab-log hides). The dir keeps a list of the names it has had, so "is this
# record mine?" can answer yes across a rename.
#
#   <config_dir>/user-profiles/<name>/renamed-from   (one former name per line)

_profile_former_path(name::AbstractString; config_root::AbstractString = config_dir())::String =
    joinpath(profile_settings_dir(name; config_root = config_root), "renamed-from")

"""
    profile_names(name = active_profile_name()) -> Vector{String}

`name` followed by every name this profile has had before, newest first.
"""
function profile_names(name::AbstractString = active_profile_name();
                       config_root::AbstractString = config_dir())::Vector{String}
    p = _profile_former_path(name; config_root = config_root)
    former = isfile(p) ? filter(!isempty, strip.(readlines(p))) : String[]
    unique(String[String(name); String.(reverse(former))])
end

"""
    record_profile_rename!(old, new)

Call AFTER the dir moved to `new`: append `old` to its former-names list.
"""
function record_profile_rename!(old::AbstractString, new::AbstractString;
                                config_root::AbstractString = config_dir())
    p = _profile_former_path(new; config_root = config_root)
    isdir(dirname(p)) || return nothing
    open(io -> println(io, old), p, "a")
    nothing
end
