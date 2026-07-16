# ── First-launch setup wizard ───────────────────────────────────────────────────
# Removes the custom.toml barrier for non-terminal users: the wizard picks a projects dir and
# writes it to the user's config, in the browser. `setup_required` (Cecelia.config) drives the
# frontend redirect to /setup; these endpoints supply the default, validate a path, and commit it.
# The config path itself is resolved by Cecelia.config_dir (dev = CECELIA_DEV_DIR/.env, prod =
# ~/.cecelia) — this layer never re-derives it. See docs/todo/ONBOARDING_PLAN.md.

# Best-effort writability probe: create + drop a temp file in `dir`. Cross-platform (no stat-mode
# bit-twiddling, which is unreliable on Windows) — the only sure test is "can we actually write".
_dir_writable(dir::AbstractString)::Bool =
    try; mktemp((_, _) -> nothing, dir); true; catch; false; end

# Can `dir` serve as the projects directory? → (ok, message, willCreate). Pure check, no side
# effects (the actual mkpath happens in /init), so it's safe to call live on every keystroke.
function _check_projects_dir(dir::AbstractString)
    path = expanduser(strip(String(dir)))
    isempty(path)      && return (false, "Enter a folder path.", false)
    isabspath(path)    || return (false, "Use an absolute path (or one starting with ~).", false)
    if ispath(path)
        isdir(path) || return (false, "That path exists but is not a folder.", false)
        return _dir_writable(path) ? (true, "Folder exists and is writable.", false) :
                                     (false, "That folder is not writable.", false)
    end
    # doesn't exist yet — walk up to the nearest existing ancestor and check we could create under it
    parent = dirname(path)
    while !isempty(parent) && !ispath(parent)
        np = dirname(parent); np == parent && break; parent = np
    end
    (isdir(parent) && _dir_writable(parent)) ?
        (true, "Folder will be created.", true) :
        (false, "Can't create a folder there — the parent isn't writable.", false)
end

# GET /api/setup/defaults → { projectsDir } — the wizard pre-fill. A leading `~` is kept literal so it
# stays per-user portable when stored (D1); `expanduser` resolves it (incl. to %USERPROFILE% on
# Windows) on validate/create/read. `homedir()` fallback only if `~` somehow won't expand here.
function api_setup_defaults(::HTTP.Request)
    default = expanduser("~") == "~" ? joinpath(homedir(), "cecelia-projects") : "~/cecelia-projects"
    200, JSON3.write((; projectsDir = default))
end

# GET /api/setup/validate?path=... → { ok, message, willCreate } — live field feedback.
function api_setup_validate(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    ok, message, willCreate = _check_projects_dir(get(q, "path", ""))
    200, JSON3.write((; ok, message, willCreate))
end

# POST /api/setup/init { projectsDir } → { ok, projectsDir, restartRequired } | 400
# Validate → create the dir → persist to custom.toml → hot-reload. `restartRequired` stays in the
# contract as a fallback (D3), but config reloads in place so it's false on the normal path.
function api_setup_init(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    raw = strip(String(get(body, "projectsDir", "")))
    isempty(raw) && return 400, JSON3.write((; error = "projectsDir required"))
    ok, message, _ = _check_projects_dir(raw)
    ok || return 400, JSON3.write((; error = message))
    try
        mkpath(expanduser(raw))
    catch e
        return 400, JSON3.write((; error = "Could not create folder: $(sprint(showerror, e))"))
    end
    set_projects_dir!(raw)   # store the literal (a leading ~ stays per-user portable), reload config
    @info "Setup complete via /api/setup/init" projectsDir = projects_dir()
    200, JSON3.write((; ok = true, projectsDir = projects_dir(), restartRequired = false))
end
