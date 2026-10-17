# ── Kiwi profile API — routes for LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6 + P8 + D11 ─────────────
#
# Logging in runs here too (P9): `claude auth login` is spawned per profile and the dialog polls it,
# opening a private browser window and taking a pasted code as the fallback (`claude_auth.jl`). The
# terminal one-liner (GET /api/kiwi/terminal/command) is now only for terminal `claude` under a
# profile, not for logging in. `auth status` / `auth logout` drive the login label and Log out (P8).
#
# Routes (see server.jl for wiring):
#
#   GET  /api/kiwi/profiles                   → { active, profiles: [{name, dir, isDefault, retired}], legacyReserved }
#   POST /api/kiwi/profiles/select            → { name }               ; writes [ai].profile in custom.toml
#   POST /api/kiwi/profiles/create            → { name }               ; mkpath under user-profiles/
#   POST /api/kiwi/profiles/retire            → { name }               ; writes `.kiwi-retired` marker (D11)
#   GET  /api/kiwi/terminal/command?profile=N → { command, profile, profileDir }
#   GET  /api/kiwi/profiles/auth?profile=N    → { profile, state, email, method, org } ; Claude login
#   POST /api/kiwi/profiles/logout            → { name }               ; `claude auth logout` for it
#   POST /api/kiwi/profiles/login             → { name }               ; start `claude auth login` (P9)
#   GET  /api/kiwi/profiles/login?profile=N   → { state, url, browser, message } ; poll it
#   POST /api/kiwi/profiles/login/code        → { name, code }         ; the pasted fallback code
#   POST /api/kiwi/profiles/login/cancel      → { name }
#
# Server-active-profile model: `[ai].profile` in custom.toml is the single source of truth. All
# `claude` spawns pick it up via `active_profile_name()`. Per-tab handoff (Q2 recommendation in the
# plan) is deferred — when it lands, the tab sends `X-Kiwi-Profile: <name>` and these routes read
# it in addition to (not instead of) the config default. `_apply_claude_env` already accepts a
# per-call profile_dir override to make that extension cheap.

import JSON3

# Reserved profile names — never creatable, never listed in the roster:
#   `legacy` — D10 read-time default for pass entries missing the profile field. A user picking
#              "legacy" as their identity would collide with the sentinel and is meaningless.
#   `peanut` — the frontend's display alias for `default` (mirror of `DEFAULT_PROFILE_DISPLAY_NAME`
#              in `frontend/src/utils/profileApi.ts`). Blocked here so no new profile can visually
#              collide with the picker's synthetic default row.
const _KIWI_RESERVED_PROFILE_NAMES = ("legacy", "peanut")

# Kiwi profile names must be usable as directory names AND look like git-author-shaped labels.
# Deliberately narrow: lower-ASCII alnum + `-` + `_`, 1..32 chars. Rejects `.` / `/` / whitespace
# / uppercase (which fails on case-insensitive filesystems), and every reserved name.
function _valid_kiwi_profile_name(name::AbstractString)::Bool
    s = String(name)
    (1 <= length(s) <= 32) || return false
    s in _KIWI_RESERVED_PROFILE_NAMES && return false
    all(c -> ('a' <= c <= 'z') || ('0' <= c <= '9') || c == '-' || c == '_', s)
end

# A retired profile keeps its data on disk (D11 immutable-name lifecycle — pass records that
# already carry the name must stay resolvable) but is marked non-selectable via a sentinel file.
# The name is preserved so `default` stays a magic name and nothing collides on re-creation.
const _KIWI_RETIRED_MARKER = ".kiwi-retired"

_kiwi_retired_marker_path(profile_dir::AbstractString)::String =
    joinpath(String(profile_dir), _KIWI_RETIRED_MARKER)

_kiwi_profile_retired(profile_dir::AbstractString)::Bool =
    isfile(_kiwi_retired_marker_path(profile_dir))

# Enumerate the profile roster. `default` is always present (special-cased in `active_profile_dir`
# to point at `~/.claude*` — see LOGIN_CREDENTIAL_ISOLATION_PLAN P2) and is prepended here
# SYNTHETICALLY. USER_PROFILE_PLAN Phase 4 introduced `<config_dir>/user-profiles/default/` as a
# real on-disk directory (to hold the default profile's `settings.toml`), so the scan below has
# to SKIP the literal name `default` — otherwise the picker shows two "default" entries, one
# synthetic + one from disk. Same treatment for the reserved `legacy` sentinel. Retired named
# profiles stay in the list so a picker can show them grayed out and pass logs stay resolvable —
# the `retired` flag is what the UI keys off.
function _kiwi_list_profiles()::Vector{Dict{String,Any}}
    out = Dict{String,Any}[Dict("name" => "default",
                                "dir"  => "",           # empty → CLI defaults per P2
                                "isDefault" => true,
                                "retired" => false)]
    root = joinpath(config_dir(), "user-profiles")
    isdir(root) || return out
    for entry in sort!(readdir(root))
        p = joinpath(root, entry)
        isdir(p) || continue
        entry == "default" && continue                       # already prepended above
        entry in _KIWI_RESERVED_PROFILE_NAMES && continue   # never surface a reserved name
        push!(out, Dict{String,Any}("name" => entry, "dir" => p, "isDefault" => false,
                                    "retired" => _kiwi_profile_retired(p)))
    end
    out
end

function api_kiwi_profiles_list(::HTTP.Request)
    body = Dict{String,Any}("active"         => active_profile_name(),
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
                "`legacy`, `default` and `peanut` are reserved."))
    # `default` is not creatable — it's a magic name that maps to `~/.claude`.
    String(name) == "default" && return 400, JSON3.write((;
        ok = false, error = "`default` already exists — it maps to `~/.claude`."))
    dir = joinpath(config_dir(), "user-profiles", String(name))
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
        # it's the magic name that maps to `~/.claude`, no dir under user-profiles/ required.
        _valid_kiwi_profile_name(name) || return 400, JSON3.write((;
            ok = false, error = "Invalid profile name."))
        dir = joinpath(config_dir(), "user-profiles", name)
        isdir(dir) ||
            return 404, JSON3.write((; ok = false, error = "Profile `$(name)` not found."))
        # A retired profile is non-selectable — the whole point of the marker is that new Kiwi
        # turns must not run under a retired identity. The record itself is preserved (D11).
        _kiwi_profile_retired(dir) &&
            return 409, JSON3.write((; ok = false,
                error = "Profile `$(name)` is retired and can't be selected."))
    end
    set_active_profile!(name)
    200, JSON3.write((; ok = true, active = name, loggingOut = _enforce_one_claude_login(name)))
end

# Shared workstation → "One Claude login at a time": picking a profile logs every OTHER profile out
# of Claude, so the previous person's login doesn't outlive the next pick (the launch picker selects
# on every reload — that is the hand-over moment). Includes `default`, whose login is `~/.claude`.
# In the background: each logout is a ~1 s CLI spawn and the picker is waiting. Returns how many
# were queued.
function _enforce_one_claude_login(keep::AbstractString)::Int
    dirs = _one_claude_login_targets(keep)
    (isempty(dirs) || isnothing(Cecelia.agent_bin_path(Cecelia.observer_agent_bin()))) && return 0
    Threads.@spawn for d in dirs
        ok, msg = claude_auth_logout!(d)
        ok || @warn "Shared workstation: could not log a profile out of Claude" dir = d message = msg
    end
    length(dirs)
end

# The profile dirs that policy logs out when `keep` is picked — every other profile, retired ones
# too (their credentials are still on disk), `""` being the default profile. Empty when the policy
# is off. Split out so a test can check the list without logging anything out.
_one_claude_login_targets(keep::AbstractString)::Vector{String} =
    Cecelia.shared_workstation("oneClaudeLogin") ?
        String[p["dir"] for p in _kiwi_list_profiles() if p["name"] != keep] : String[]

# Retire a profile — write the sentinel marker so the picker grays it out and select rejects it.
# The dir + credentials stay on disk (D11 immutable-name lifecycle). If the retired profile is
# the currently-active one, we also snap the active back to `default` so the next spawn doesn't
# silently keep using retired credentials.
function api_kiwi_profiles_retire(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    name = body isa AbstractDict ? String(get(body, :name, "")) : ""
    isempty(name) && return 400, JSON3.write((; ok = false, error = "Missing `name`."))
    name == "default" && return 400, JSON3.write((; ok = false,
        error = "`default` can't be retired — it maps to `~/.claude`."))
    _valid_kiwi_profile_name(name) || return 400, JSON3.write((;
        ok = false, error = "Invalid profile name."))
    dir = joinpath(config_dir(), "user-profiles", name)
    isdir(dir) || return 404, JSON3.write((; ok = false, error = "Profile `$(name)` not found."))
    _kiwi_profile_retired(dir) &&
        return 200, JSON3.write((; ok = true, name = name, active = active_profile_name(),
                                    alreadyRetired = true))
    write(_kiwi_retired_marker_path(dir), string(now()))
    snapped = false
    if active_profile_name() == name
        set_active_profile!("default")
        snapped = true
    end
    200, JSON3.write((; ok = true, name = name,
                        active = active_profile_name(), snappedToDefault = snapped))
end

# USER_PROFILE_PLAN Decision 11 amendment: rename + delete alongside retire. Rename moves the
# on-disk dir + updates [ai].profile if active; past turn logs pin the OLD name (documented at
# the callsite). Delete removes credentials + settings + retired marker; refuse if active.
function api_kiwi_profiles_rename(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    body isa AbstractDict || return 400, JSON3.write((; ok = false, error = "Missing body."))
    old_name = String(get(body, :oldName, ""))
    new_name = String(get(body, :newName, ""))
    isempty(old_name) && return 400, JSON3.write((; ok = false, error = "Missing `oldName`."))
    isempty(new_name) && return 400, JSON3.write((; ok = false, error = "Missing `newName`."))
    old_name == "default" && return 400, JSON3.write((;
        ok = false, error = "`default` can't be renamed — it maps to `~/.claude`."))
    new_name == "default" && return 400, JSON3.write((;
        ok = false, error = "`default` already exists — it maps to `~/.claude`."))
    _valid_kiwi_profile_name(old_name) || return 400, JSON3.write((;
        ok = false, error = "Invalid old profile name."))
    _valid_kiwi_profile_name(new_name) || return 400, JSON3.write((;
        ok = false, error = "New profile name must be 1-32 chars, lower-ASCII alnum + `-` / `_`; " *
                            "`legacy`, `default` and `peanut` are reserved."))
    old_name == new_name && return 400, JSON3.write((;
        ok = false, error = "New name matches the old name."))
    root = joinpath(config_dir(), "user-profiles")
    old_dir = joinpath(root, old_name)
    new_dir = joinpath(root, new_name)
    isdir(old_dir) || return 404, JSON3.write((; ok = false, error = "Profile `$(old_name)` not found."))
    isdir(new_dir) && return 409, JSON3.write((;
        ok = false, error = "A profile named `$(new_name)` already exists."))
    # Refuse a rename of a retired profile — the whole point of the retired sentinel is that the
    # name is a stable reference for past turn logs. Rename would silently invalidate that plus
    # the sentinel semantics; wrong shape.
    _kiwi_profile_retired(old_dir) && return 409, JSON3.write((;
        ok = false, error = "Retired profiles can't be renamed — the retired marker is a stable reference."))
    try
        mv(old_dir, new_dir)
    catch e
        return 500, JSON3.write((; ok = false,
            error = "Failed to rename profile dir: " * sprint(showerror, e)))
    end
    # Update [ai].profile if the renamed profile was active — otherwise the next spawn silently
    # falls back to "default" (which fabricates ~/.claude access under a fresh env).
    if active_profile_name() == old_name
        set_active_profile!(new_name)
    end
    record_profile_rename!(old_name, new_name)   # turns + lab-log hides stamped `old_name` stay theirs
    # Same person, new name: the projects they own stay theirs. (Kiwi turn stamps keep the old
    # name on purpose — see the comment above this function.)
    rewrite_project_owners!(os -> String[o == old_name ? new_name : o for o in os])
    200, JSON3.write((; ok = true, oldName = old_name, newName = new_name,
                        active = active_profile_name()))
end

function api_kiwi_profiles_delete(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    body isa AbstractDict || return 400, JSON3.write((; ok = false, error = "Missing body."))
    name = String(get(body, :name, ""))
    isempty(name) && return 400, JSON3.write((; ok = false, error = "Missing `name`."))
    name == "default" && return 400, JSON3.write((;
        ok = false, error = "`default` can't be deleted — it maps to `~/.claude`."))
    _valid_kiwi_profile_name(name) || return 400, JSON3.write((;
        ok = false, error = "Invalid profile name."))
    # A user deleting the profile they're currently logged in as would strand every subsequent
    # `claude` spawn with a scrubbed env and no credential dir — enforce the switch-first flow.
    active_profile_name() == name && return 409, JSON3.write((;
        ok = false,
        error = "Can't delete the active profile — switch to another profile first."))
    dir = joinpath(config_dir(), "user-profiles", name)
    isdir(dir) || return 404, JSON3.write((; ok = false, error = "Profile `$(name)` not found."))
    try
        rm(dir; recursive = true, force = true)
    catch e
        # A stale sub-path can throw mid-walk even after the top-level tore down; if it's gone,
        # the delete succeeded (mirrors `api_projects_delete`'s handling of the same case).
        isdir(dir) && return 500, JSON3.write((; ok = false,
            error = "Failed to delete profile dir: " * sprint(showerror, e)))
    end
    # Drop the deleted profile from every project's owners — a project only it owned becomes
    # visible to all instead of hidden behind a name nobody can pick.
    rewrite_project_owners!(os -> filter(!=(name), os))
    200, JSON3.write((; ok = true, name = name))
end

function api_kiwi_terminal_command(req::HTTP.Request)
    # `profile` query param names which profile the terminal one-liner is for; absent → active.
    q = HTTP.queryparams(HTTP.URI(req.target))
    name = get(q, "profile", "")
    dir = _kiwi_claude_dir(name; must_exist = false)
    dir isa Tuple && return dir
    200, JSON3.write((; ok = true,
                        profile = isempty(name) ? active_profile_name() : name,
                        profileDir = dir,
                        command = kiwi_terminal_command(dir)))
end

# A profile name → its `CLAUDE_CONFIG_DIR` (`""` for `default`), or an error response tuple. An empty
# name means the active profile. `must_exist`: a named profile's dir must already be on disk — the
# CLI creates a missing one, so logging out or probing a typo would mint a stray profile.
function _kiwi_claude_dir(name::AbstractString; must_exist::Bool = true)
    isempty(name) && return active_profile_dir()
    name == "default" && return active_profile_dir(name)
    _valid_kiwi_profile_name(name) ||
        return (400, JSON3.write((; ok = false, error = "Invalid profile name.")))
    dir = active_profile_dir(name)
    must_exist && !isdir(dir) &&
        return (404, JSON3.write((; ok = false, error = "Profile `$(name)` not found.")))
    dir
end

# Who a profile is logged in to Claude as. `fresh=1` skips the short status cache (the Preferences
# refresh button). `state` is `loggedIn` / `loggedOut` / `unknown` (no CLI, or it failed).
function api_kiwi_profiles_auth(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    name = get(q, "profile", "")
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    a = claude_auth_status(dir; fresh = get(q, "fresh", "") == "1")
    200, JSON3.write((; ok = true, profile = isempty(name) ? active_profile_name() : name,
                        state = String(a.state), email = a.email, method = a.method, org = a.org))
end

function api_kiwi_profiles_logout(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes)); catch; nothing; end
    name = body isa AbstractDict ? String(get(body, :name, "")) : ""
    isempty(name) && return 400, JSON3.write((; ok = false, error = "Missing `name`."))
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    ok, msg = claude_auth_logout!(dir)
    ok || return 500, JSON3.write((; ok = false, error = isempty(msg) ? "Logout failed." : msg))
    200, JSON3.write((; ok = true, name = name))
end

# ── Logging in (P9) — start, poll, paste a code, cancel. The writes need an explicit `name`, like
# logout: an empty body must never start a login for whichever profile happens to be active. ──

_kiwi_body_name(body_bytes) =
    (b = try; JSON3.read(String(body_bytes)); catch; nothing; end;
     b isa AbstractDict ? String(get(b, :name, "")) : "")

const _MISSING_NAME = (400, JSON3.write((; ok = false, error = "Missing `name`.")))

function api_kiwi_profiles_login_start(body_bytes::Vector{UInt8})
    name = _kiwi_body_name(body_bytes)
    isempty(name) && return _MISSING_NAME
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    ok, msg = claude_login_start!(dir)
    ok || return 500, JSON3.write((; ok = false, error = isempty(msg) ? "Could not start the login." : msg))
    200, JSON3.write((; ok = true, claude_login_status(dir)...))
end

function api_kiwi_profiles_login_status(req::HTTP.Request)
    name = get(HTTP.queryparams(HTTP.URI(req.target)), "profile", "")
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    200, JSON3.write((; ok = true, claude_login_status(dir)...))
end

function api_kiwi_profiles_login_code(body_bytes::Vector{UInt8})
    b = try; JSON3.read(String(body_bytes)); catch; nothing; end
    name = b isa AbstractDict ? String(get(b, :name, "")) : ""
    isempty(name) && return _MISSING_NAME
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    ok, msg = claude_login_code!(dir, String(get(b, :code, "")))
    ok || return 409, JSON3.write((; ok = false, error = msg))
    200, JSON3.write((; ok = true))
end

function api_kiwi_profiles_login_cancel(body_bytes::Vector{UInt8})
    name = _kiwi_body_name(body_bytes)
    isempty(name) && return _MISSING_NAME
    dir = _kiwi_claude_dir(name)
    dir isa Tuple && return dir
    200, JSON3.write((; ok = true, cancelled = claude_login_cancel!(dir)))
end
