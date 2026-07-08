# ── Notebook Playground (API layer) ─────────────────────────────────────────────
# Launch / probe the Pluto notebook server (port 7660) and list a project's notebooks. The Pluto
# server runs as its OWN Julia process (the pluto/ env, which path-sources Cecelia) — NOT this API
# server. Lifecycle mirrors the napari bridge: lazy-launch on first request, adopt an already-running
# server (e.g. one from `pixi run notebooks` or a survivor of a server restart) instead of spawning a
# duplicate. Secret auth is disabled in pluto/launch.jl (localhost dev tool), so the URL is a plain
# http://localhost:7660/. See docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md.

const NOTEBOOKS_PORT = 7660
const NOTEBOOKS_URL  = "http://localhost:$(NOTEBOOKS_PORT)/"

# api/src → repo root → pluto/ (engine env + launch.jl) and notebooks/ (shipped examples).
_pluto_root()          = abspath(joinpath(@__DIR__, "..", "..", "pluto"))
_repo_notebooks_dir()  = abspath(joinpath(@__DIR__, "..", "..", "notebooks"))
_project_notebooks_dir(uid::AbstractString) = joinpath(projects_dir(), uid, "notebooks")

# Pluto's secret protection stays ON (see launch.jl). launch.jl writes the running session's secret
# here; the frontend appends it to URLs (…/?secret=… and …/open?path=…&secret=…) so only this app can
# drive the server. "" if not running / not yet written (frontend only uses URLs once running).
_secret_path()      = joinpath(_pluto_root(), ".plutosecret")
_notebook_secret()  = isfile(_secret_path()) ? String(strip(read(_secret_path(), String))) : ""

const _nb_proc_ref  = Ref{Union{Base.Process,Nothing}}(nothing)
const _nb_lock      = ReentrantLock()
const _nb_starting  = Ref(false)
const _nb_error     = Ref{Union{String,Nothing}}(nothing)   # last launch failure, surfaced in status

const _SETUP_HINT = "Run `pixi run notebooks-instantiate` once to set up the notebook environment " *
                    "(and optionally `pixi run notebooks-sysimage` for fast plots), then try again."

# Is the pluto/ env provisioned? Heuristic: a resolved Manifest exists. A fresh clone has the committed
# Manifest.toml but no downloaded/precompiled packages — that surfaces as a fast launch failure caught
# below; this pre-check just gives the friendly hint before we even spawn when the Manifest is absent.
_pluto_env_ready() = isfile(joinpath(_pluto_root(), "Manifest.toml"))

# Alive = the Pluto HTTP server answers on the port. Any HTTP response (200 with secret disabled)
# counts; a refused connection throws → not alive.
function _notebook_server_alive()::Bool
    try
        HTTP.get(NOTEBOOKS_URL; retry = false, redirect = false,
                 connect_timeout = 2, read_idle_timeout = 3, status_exception = false)
        true
    catch
        false
    end
end

# Ensure the Pluto server is up. Returns true if already serving, false if a launch was just kicked
# off (still starting). `notebooks_dir` points Pluto's file picker at the active project's notebooks.
function _ensure_notebook_server!(notebooks_dir::AbstractString)::Bool
    lock(_nb_lock) do
        _notebook_server_alive() && return true
        _nb_starting[] && return false

        pluto_root    = _pluto_root()
        launch_script = joinpath(pluto_root, "launch.jl")
        isfile(launch_script) || error("pluto/launch.jl not found at $launch_script")
        # Friendly first-run guidance before spawning if the env clearly isn't set up.
        _pluto_env_ready() || error("The notebook environment is not set up. $_SETUP_HINT")

        julia_exe = joinpath(Sys.BINDIR, Base.julia_exename())
        cmd = Cmd(`$julia_exe --project=$pluto_root $launch_script`)
        cmd = addenv(cmd,
            "CECELIA_NOTEBOOKS_DIR" => abspath(notebooks_dir),
            "CECELIA_PLUTO_BROWSER" => "false")

        @info "Launching Pluto notebook server..." notebooks_dir port = NOTEBOOKS_PORT
        _nb_error[] = nothing
        proc = run(pipeline(cmd; stdout = stdout, stderr = stderr), wait = false)
        _nb_proc_ref[] = proc
        _nb_starting[] = true
        # Off the request path: wait for the port, OR detect the process dying during startup (the
        # usual cause: env not instantiated → `using Pluto` fails) and surface a friendly hint.
        @async begin
            try
                for _ in 1:120   # up to ~120 s cold start (first Pluto boot precompiles)
                    _notebook_server_alive() && break
                    if process_exited(proc) && !_notebook_server_alive()
                        _nb_error[] = "The notebook server exited during startup. $_SETUP_HINT"
                        break
                    end
                    sleep(1)
                end
            finally
                lock(_nb_lock) do; _nb_starting[] = false; end
            end
        end
        false
    end
end

# POST /api/notebooks/launch  { projectUid }  → { url, starting }
function api_notebooks_launch(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    nb_dir = _project_notebooks_dir(uid)
    mkpath(nb_dir)
    ready = try
        _ensure_notebook_server!(nb_dir)
    catch e
        @warn "Pluto launch failed" exception = e
        return 500, JSON3.write((; error = "Could not launch notebook server: $(sprint(showerror, e))"))
    end
    # 202 while starting (mirrors napari's starting response), 200 once serving.
    (ready ? 200 : 202), JSON3.write((; url = NOTEBOOKS_URL, secret = _notebook_secret(), starting = !ready))
end

# GET /api/notebooks/status  → { running, starting, url, error }
function api_notebooks_status(req::HTTP.Request)
    running = _notebook_server_alive()
    200, JSON3.write((; running = running, starting = _nb_starting[], url = NOTEBOOKS_URL,
                        secret = _notebook_secret(), error = running ? nothing : _nb_error[]))
end

# Stop the Pluto server. We can only kill a server THIS process spawned (we hold its handle); killing
# the launcher process terminates Pluto and its Malt workers follow (they exit when the parent drops).
# An adopted/externally-launched one (e.g. `pixi run notebooks`) isn't ours → direct to stop-by-port.
function _shutdown_notebook_server!()::Tuple{Bool,String}
    lock(_nb_lock) do
        proc = _nb_proc_ref[]
        _nb_proc_ref[] = nothing
        _nb_starting[] = false
        _nb_error[]    = nothing
        if proc !== nothing && process_running(proc)
            kill(proc)
            return true, "stopped"
        end
        _notebook_server_alive() &&
            return false, "The notebook server is running but wasn't started by this app — stop it with `pixi run stop-notebooks`."
        true, "not running"
    end
end

# POST /api/notebooks/shutdown  → { ok, message }
function api_notebooks_shutdown(body_bytes::Vector{UInt8})
    ok, msg = _shutdown_notebook_server!()
    (ok ? 200 : 409), JSON3.write((; ok = ok, message = msg))
end

# POST /api/notebooks/restart  { projectUid }  → { url, starting }  (relaunches after stopping)
function api_notebooks_restart(body_bytes::Vector{UInt8})
    ok, msg = _shutdown_notebook_server!()
    ok || return 409, JSON3.write((; error = msg))
    for _ in 1:20                        # let the port free before rebinding
        _notebook_server_alive() || break
        sleep(0.25)
    end
    api_notebooks_launch(body_bytes)
end

# Best-effort: take a server WE spawned down when this API process exits cleanly (so a normal server
# shutdown doesn't orphan Pluto on :7660). Won't fire on SIGKILL — `pixi run stop` also kills :7660.
atexit() do
    try
        p = _nb_proc_ref[]
        p !== nothing && process_running(p) && kill(p)
    catch
    end
end

# ── Registry (settings/notebooks.json) ──────────────────────────────────────────
# Per-project notebook metadata (description + version), mirroring how chains/boards persist under
# settings/ (routes.jl `_settings_dir_for_project`). Keyed by filename; only PROJECT-scope notebooks
# are registered — shipped examples are read-only (versioned with the code).
using Dates

_registry_path(uid::AbstractString) = joinpath(_settings_dir_for_project(uid), "notebooks.json")

function _read_registry(uid::AbstractString)::Dict{String,Any}
    p = _registry_path(uid)
    isfile(p) || return Dict{String,Any}()
    try
        Dict{String,Any}(String(k) => Dict{String,Any}(v)
                         for (k, v) in JSON3.read(read(p, String), Dict{String,Any}))
    catch
        Dict{String,Any}()
    end
end

function _write_registry!(uid::AbstractString, reg::AbstractDict)
    mkpath(_settings_dir_for_project(uid))
    open(_registry_path(uid), "w") do io; JSON3.write(io, reg); end
end

# Sanitise a user-supplied notebook name → a safe `*.jl` basename. Rejects (returns nothing) any
# path-like input — separators or `..` — rather than silently stripping it, plus dotfiles.
function _safe_nb_file(name)::Union{String,Nothing}
    n = strip(String(name))
    isempty(n) && return nothing
    (occursin('/', n) || occursin('\\', n) || occursin("..", n)) && return nothing
    endswith(n, ".jl") || (n = n * ".jl")
    (startswith(n, ".") || !occursin(r"^[A-Za-z0-9._ -]+\.jl$", n)) && return nothing
    n
end

_reg_desc(e) = String(get(e, "description", ""))
# `current` = which snapshot version the LIVE notebook currently reflects (0 = never snapshotted).
# This is what the UI shows, so a restore to v3 reads back as "v3" (not a monotonic counter).
_reg_current(e) = Int(get(e, "current", 0))

# Snapshot versions present on disk for a notebook (from .snapshots/<stem>@v<N>.jl). Names are ASCII
# (_safe_nb_file), so the byte-slice between the "…@v" prefix and ".jl" is safe.
function _snapshot_versions(uid::AbstractString, file::AbstractString)::Vector{Int}
    snapdir = joinpath(_project_notebooks_dir(uid), ".snapshots")
    prefix  = "$(splitext(file)[1])@v"
    vs = Int[]
    isdir(snapdir) || return vs
    for f in readdir(snapdir)
        (startswith(f, prefix) && endswith(f, ".jl")) || continue
        v = tryparse(Int, f[(length(prefix) + 1):(length(f) - 3)])
        v === nothing || push!(vs, v)
    end
    vs
end

# Next snapshot number = one past the highest on disk (robust to manual snapshot edits).
_next_snapshot_version(uid, file) = (vs = _snapshot_versions(uid, file); isempty(vs) ? 1 : maximum(vs) + 1)

# GET /api/notebooks?projectUid=…  → { notebooks: [{ name, file, scope, description, version }] }
# scope: "project" (this project's notebooks/, registry-tracked) or "example" (shipped, read-only).
function api_notebooks_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    reg = _read_registry(uid)
    _files(dir) = isdir(dir) ?
        sort([f for f in readdir(dir) if endswith(f, ".jl") && !startswith(f, ".")]) : String[]

    pdir = _project_notebooks_dir(uid); edir = _repo_notebooks_dir()
    proj = [(; name = splitext(f)[1], file = f, scope = "project", path = joinpath(pdir, f),
             description = _reg_desc(get(reg, f, Dict())), version = _reg_current(get(reg, f, Dict())))
            for f in _files(pdir)]
    examples = [(; name = splitext(f)[1], file = f, scope = "example", path = joinpath(edir, f),
                 description = "", version = 0)
                for f in _files(edir)]
    200, JSON3.write((; notebooks = vcat(proj, examples)))
end

# ── Notebook CRUD (project scope) ────────────────────────────────────────────────
# All resolve the target strictly under {proj}/notebooks/ via _safe_nb_file, so examples (which live
# in the repo, not the project dir) can't be mutated/deleted here — only duplicated into the project.

# POST /api/notebooks/create  { projectUid, name, description? }  → { file }
function api_notebooks_create(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    file = _safe_nb_file(get(body, :name, ""))
    file === nothing && return 400, JSON3.write((; error = "Invalid notebook name"))

    dir = _project_notebooks_dir(uid); mkpath(dir)
    dest = joinpath(dir, file)
    isfile(dest) && return 409, JSON3.write((; error = "Notebook already exists: $file"))
    template = joinpath(_pluto_root(), "notebook_template.jl")
    isfile(template) || return 500, JSON3.write((; error = "Template missing: $template"))
    cp(template, dest)

    reg = _read_registry(uid)
    reg[file] = Dict{String,Any}("description" => String(get(body, :description, "")),
                                 "current" => 0, "updatedAt" => string(Dates.now()))
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true, file = file))
end

# POST /api/notebooks/describe  { projectUid, file, description }  → { ok }
function api_notebooks_describe(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    file = _safe_nb_file(get(body, :file, ""))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))
    isfile(joinpath(_project_notebooks_dir(uid), file)) || return 404, JSON3.write((; error = "Notebook not found"))

    reg = _read_registry(uid)
    e = get(reg, file, Dict{String,Any}("current" => 0))
    e["description"] = String(get(body, :description, ""))
    e["updatedAt"]   = string(Dates.now())
    reg[file] = e
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true))
end

# POST /api/notebooks/delete  { projectUid, file }  → { ok }
function api_notebooks_delete(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    file = _safe_nb_file(get(body, :file, ""))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))
    # Guard: Pluto owns an open notebook's file and can re-create it after deletion. If the server is
    # up, require an explicit `force` (the UI's confirm supplies it) and tell the user to close it.
    if get(body, :force, false) !== true && _notebook_server_alive()
        return 409, JSON3.write((; error = "The notebook server is running — if \"$file\" is open there, close that tab first (an open notebook can be re-created after deletion), then confirm.", serverRunning = true))
    end
    dest = joinpath(_project_notebooks_dir(uid), file)
    isfile(dest) && rm(dest)
    # Also drop this notebook's snapshots (.snapshots/<stem>@v<N>.jl) — otherwise deleting the
    # notebook orphans its whole version history on disk.
    snapdir = joinpath(_project_notebooks_dir(uid), ".snapshots")
    prefix  = "$(splitext(file)[1])@v"
    if isdir(snapdir)
        for f in readdir(snapdir)
            (startswith(f, prefix) && endswith(f, ".jl")) || continue
            tryparse(Int, f[(length(prefix) + 1):(length(f) - 3)]) === nothing && continue
            rm(joinpath(snapdir, f))
        end
    end
    reg = _read_registry(uid)
    delete!(reg, file)
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true))
end

# POST /api/notebooks/duplicate  { projectUid, file, scope, newName? }  → { file }
# Copies a project OR example notebook into this project's notebooks/ under a fresh name.
function api_notebooks_duplicate(body_bytes::Vector{UInt8})
    body  = JSON3.read(String(body_bytes))
    uid   = String(get(body, :projectUid, ""))
    file  = _safe_nb_file(get(body, :file, ""))
    scope = String(get(body, :scope, "project"))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    src = scope == "example" ? joinpath(_repo_notebooks_dir(), file) : joinpath(_project_notebooks_dir(uid), file)
    isfile(src) || return 404, JSON3.write((; error = "Source notebook not found"))

    dir = _project_notebooks_dir(uid); mkpath(dir)
    base = something(_safe_nb_file(get(body, :newName, "")), splitext(file)[1] * "-copy.jl")
    # Ensure uniqueness: name, name-2, name-3, …
    stem, ext = splitext(base); dest = joinpath(dir, base); n = 2
    while isfile(dest); dest = joinpath(dir, "$(stem)-$(n)$(ext)"); n += 1; end
    cp(src, dest)

    newfile = basename(dest)
    reg = _read_registry(uid)
    reg[newfile] = Dict{String,Any}("description" => "Copied from $(scope)/$(file)",
                                    "current" => 0, "updatedAt" => string(Dates.now()))
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true, file = newfile))
end

# POST /api/notebooks/snapshot  { projectUid, file }  → { snapshot, version }
# Freeze an immutable copy to notebooks/.snapshots/<name>@v<N>.jl (N = next number on disk) and set
# the notebook's `current` to N. Answers "which version made Figure 3" without git/file-watching.
function api_notebooks_snapshot(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    file = _safe_nb_file(get(body, :file, ""))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))
    dir  = _project_notebooks_dir(uid)
    src  = joinpath(dir, file)
    isfile(src) || return 404, JSON3.write((; error = "Notebook not found"))

    v = _next_snapshot_version(uid, file)
    snapdir = joinpath(dir, ".snapshots"); mkpath(snapdir)
    snapname = "$(splitext(file)[1])@v$(v).jl"
    cp(src, joinpath(snapdir, snapname); force = true)

    reg = _read_registry(uid)
    e   = get(reg, file, Dict{String,Any}())
    e["current"]   = v                 # the live notebook now IS this snapshot
    e["updatedAt"] = string(Dates.now())
    reg[file] = e
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true, snapshot = snapname, version = v))
end

# List a notebook's snapshots (newest version first). GET /api/notebooks/snapshots?projectUid=&file=
function api_notebooks_snapshots(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    file  = _safe_nb_file(get(query, "file", ""))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))

    stem  = splitext(file)[1]
    snaps = sort(_snapshot_versions(uid, file); rev = true)
    200, JSON3.write((; snapshots = [(; version = v, file = "$(stem)@v$(v).jl") for v in snaps]))
end

# Restore a snapshot into the live notebook. POST /api/notebooks/restore { projectUid, file, version }
# Plain overwrite of the live .jl with the chosen snapshot — it does NOT create a snapshot and does
# NOT bump the version (Snapshot is the explicit way to freeze a version; restore just goes back).
# Pluto auto-reloads the file (launch.jl `auto_reload_from_file`). The UI two-click-confirms to guard
# against losing un-snapshotted edits.
function api_notebooks_restore(body_bytes::Vector{UInt8})
    body    = JSON3.read(String(body_bytes))
    uid     = String(get(body, :projectUid, ""))
    file    = _safe_nb_file(get(body, :file, ""))
    version = get(body, :version, nothing)
    (isempty(uid) || file === nothing || version === nothing) &&
        return 400, JSON3.write((; error = "projectUid + file + version required"))
    ver = version isa Integer ? Int(version) : tryparse(Int, string(version))
    ver === nothing && return 400, JSON3.write((; error = "version must be an integer"))

    dir    = _project_notebooks_dir(uid)
    live   = joinpath(dir, file)
    isfile(live) || return 404, JSON3.write((; error = "Notebook not found"))
    chosen = joinpath(dir, ".snapshots", "$(splitext(file)[1])@v$(ver).jl")
    isfile(chosen) || return 404, JSON3.write((; error = "Snapshot v$ver not found"))
    # Guard: restoring overwrites the live file, which Pluto may have open (and could re-save its own
    # copy over). If the server is up, require explicit `force` (UI confirm) and warn to close it.
    if get(body, :force, false) !== true && _notebook_server_alive()
        return 409, JSON3.write((; error = "The notebook server is running — if \"$file\" is open there, close that tab first (Pluto may re-save its copy over the restore), then confirm.", serverRunning = true))
    end

    cp(chosen, live; force = true)   # overwrite live with the chosen snapshot — no implicit snapshot
    reg = _read_registry(uid)
    e = get(reg, file, Dict{String,Any}())
    e["current"]   = ver             # the live notebook now IS version `ver` → the table shows "v{ver}"
    e["updatedAt"] = string(Dates.now())
    reg[file] = e
    _write_registry!(uid, reg)
    200, JSON3.write((; ok = true, restoredFrom = ver, version = ver))
end
