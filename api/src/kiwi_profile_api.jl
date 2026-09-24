# ── Kiwi profile API — routes for LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6 ────────────────────────
#
# All routes are read-cheap or additive-write; no route ever spawns `claude login` itself. The
# profile picker (frontend, follow-up) drives these, and the "Open profile terminal" button uses
# the one-liner from GET /api/kiwi/terminal/command.
#
# Routes (see server.jl for wiring):
#
#   GET  /api/kiwi/profiles                   → { active, profiles: [{name, dir, isDefault}], legacyReserved }
#   POST /api/kiwi/profiles/select            → { name }               ; writes [ai].profile in custom.toml
#   POST /api/kiwi/profiles/create            → { name }               ; mkpath under kiwi-profiles/
#   GET  /api/kiwi/terminal/command?profile=N → { command, profile, profileDir }
#
# Server-active-profile model: `[ai].profile` in custom.toml is the single source of truth. All
# `claude` spawns pick it up via `kiwi_profile_name()`. Per-tab handoff (Q2 recommendation in the
# plan) is deferred — when it lands, the tab sends `X-Kiwi-Profile: <name>` and these routes read
# it in addition to (not instead of) the config default. `_apply_claude_env` already accepts a
# per-call profile_dir override to make that extension cheap.

import JSON3

# The `legacy` profile is reserved for the D10 read-time default on pass entries missing the
# profile field. It must never be creatable (would collide with the sentinel) or selectable (a
# user picking "legacy" as their identity is meaningless).
const _KIWI_RESERVED_PROFILE_NAMES = ("legacy",)

# Kiwi profile names must be usable as directory names AND look like git-author-shaped labels.
# Deliberately narrow: lower-ASCII alnum + `-` + `_`, 1..32 chars. Rejects `.` / `/` / whitespace
# / uppercase (which fails on case-insensitive filesystems), and every reserved name.
function _valid_kiwi_profile_name(name::AbstractString)::Bool
    s = String(name)
    (1 <= length(s) <= 32) || return false
    s in _KIWI_RESERVED_PROFILE_NAMES && return false
    all(c -> ('a' <= c <= 'z') || ('0' <= c <= '9') || c == '-' || c == '_', s)
end

# Enumerate the profile roster. `default` is always present (special-cased in `kiwi_profile_dir`
# to point at `~/.claude*` — see LOGIN_CREDENTIAL_ISOLATION_PLAN P2). Named profiles are the
# subdirectories under `<config_dir()>/kiwi-profiles/`.
function _kiwi_list_profiles()::Vector{Dict{String,Any}}
    out = Dict{String,Any}[Dict("name" => "default",
                                "dir"  => "",           # empty → CLI defaults per P2
                                "isDefault" => true)]
    root = joinpath(config_dir(), "kiwi-profiles")
    isdir(root) || return out
    for entry in sort!(readdir(root))
        p = joinpath(root, entry)
        isdir(p) || continue
        entry in _KIWI_RESERVED_PROFILE_NAMES && continue   # never surface a reserved name
        push!(out, Dict{String,Any}("name" => entry, "dir" => p, "isDefault" => false))
    end
    out
end

function api_kiwi_profiles_list(::HTTP.Request)
    body = Dict{String,Any}("active"         => kiwi_profile_name(),
                            "profiles"       => _kiwi_list_profiles(),
                            "legacyReserved" => collect(_KIWI_RESERVED_PROFILE_NAMES))
    200, JSON3.write(body)
end

function api_kiwi_profiles_create(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    name = body isa AbstractDict ? get(body, :name, "") : ""
    _valid_kiwi_profile_name(String(name)) || return 400, JSON3.write((;
        ok = false,
        error = "Profile name must be 1-32 chars, lower-ASCII alnum + `-` / `_`; " *
                "`legacy` and `default` are reserved."))
    # `default` is not creatable — it's a magic name that maps to `~/.claude`.
    String(name) == "default" && return 400, JSON3.write((;
        ok = false, error = "`default` already exists — it maps to `~/.claude`."))
    dir = joinpath(config_dir(), "kiwi-profiles", String(name))
    if isdir(dir)
        return 409, JSON3.write((; ok = false, error = "Profile `$(name)` already exists."))
    end
    mkpath(dir)
    200, JSON3.write((; ok = true, name = String(name), dir = dir,
                        terminalCommand = kiwi_terminal_command(dir)))
end

function api_kiwi_profiles_select(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    name = body isa AbstractDict ? String(get(body, :name, "")) : ""
    isempty(name) && return 400, JSON3.write((; ok = false, error = "Missing `name`."))
    if name != "default"
        # A named profile must exist on disk before we select it. `default` bypasses this check —
        # it's the magic name that maps to `~/.claude`, no dir under kiwi-profiles/ required.
        _valid_kiwi_profile_name(name) || return 400, JSON3.write((;
            ok = false, error = "Invalid profile name."))
        isdir(joinpath(config_dir(), "kiwi-profiles", name)) ||
            return 404, JSON3.write((; ok = false, error = "Profile `$(name)` not found."))
    end
    set_kiwi_profile!(name)
    200, JSON3.write((; ok = true, active = name))
end

function api_kiwi_terminal_command(req::HTTP.Request)
    # `profile` query param names which profile the terminal one-liner is for; absent → active.
    q = HTTP.queryparams(HTTP.URI(req.target))
    name = get(q, "profile", "")
    dir = if isempty(name)
        kiwi_profile_dir()             # active
    elseif name == "default"
        ""
    else
        _valid_kiwi_profile_name(name) ||
            return 400, JSON3.write((; ok = false, error = "Invalid profile name."))
        joinpath(config_dir(), "kiwi-profiles", name)
    end
    200, JSON3.write((; ok = true,
                        profile = isempty(name) ? kiwi_profile_name() : name,
                        profileDir = dir,
                        command = kiwi_terminal_command(dir)))
end
