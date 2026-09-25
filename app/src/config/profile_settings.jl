# ── Per-profile settings store (USER_PROFILE_PLAN Phase 4) ─────────────────────────
#
# The user profile is the app-wide identity primitive (see docs/todo/USER_PROFILE_PLAN.md). The
# credential dir under `<config_dir>/kiwi-profiles/<name>/` was introduced in Phase P2 of
# LOGIN_CREDENTIAL_ISOLATION_PLAN for `.credentials.json`. This file adds a sibling
# `settings.toml` under the SAME dir — no second layout to reason about, no collision with the
# View Profiles files that already live at `<config_dir>/profiles/`.
#
# Storage layout:
#
#   <config_dir>/kiwi-profiles/<name>/settings.toml
#
# The `default` profile is special everywhere else (credentials fall back to `~/.claude`) — for
# SETTINGS we give it a settings dir like every other profile. The `default` profile's
# `.credentials.json` still lives at `~/.claude`; only `settings.toml` sits under
# `<config_dir>/kiwi-profiles/default/`. Rationale: a single-seat install must have SOMEWHERE to
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
    joinpath(String(config_root), "kiwi-profiles", String(name))

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
