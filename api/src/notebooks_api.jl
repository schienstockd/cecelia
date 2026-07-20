# ── Notebook Playground (API layer) ─────────────────────────────────────────────
# Launch / probe the Pluto notebook server (port 7660) and list a project's notebooks. The Pluto
# server runs as its OWN Julia process (the pluto/ env, which path-sources Cecelia) — NOT this API
# server. Lifecycle mirrors the napari bridge: lazy-launch on first request, adopt an already-running
# server (e.g. one from `pixi run notebooks` or a survivor of a server restart) instead of spawning a
# duplicate. Secret auth stays ON (see launch.jl); launch.jl publishes the session secret to
# pluto/.plutosecret and the frontend appends it to URLs. See docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md.

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

# GET /api/notebooks/status  → { running, starting, url, secret, sysimage, error }
# sysimage: "ready" | "stale" | "building" | "error" | "absent" (see _classify_sysimage below).
function api_notebooks_status(req::HTTP.Request)
    running = _notebook_server_alive()
    200, JSON3.write((; running = running, starting = _nb_starting[], url = NOTEBOOKS_URL,
                        secret = _notebook_secret(), sysimage = _sysimage_status(),
                        error = running ? nothing : _nb_error[]))
end

# ── Fast-plot sysimage (pluto/deps.so), built on first run ───────────────────────
# The deps-only sysimage (PackageCompiler) cuts Makie's ~20 s time-to-first-plot to a few seconds. It
# can't be shipped prebuilt (native code tied to this platform + Julia/package versions), so on first
# use we build it here in the BACKGROUND while notebooks stay usable (slow first plot until it lands).
# The freshly-built image is picked up by launch.jl on the NEXT server launch — we don't restart a
# running server out from under an open session. Mirrors the server lifecycle above (one tracked proc,
# atexit cleanup). See docs/NOTEBOOKS.md and TODO #00070.
_sysimage_path()  = joinpath(_pluto_root(), "deps.so")
_sysimage_stamp() = _sysimage_path() * ".stamp"
# Fingerprint of the resolved pluto deps — mirrors pluto/sysimage_stamp.jl (`hash(Manifest.toml)`), so
# a package update that re-resolves the Manifest invalidates the stamp.
_manifest_hash() = (m = joinpath(_pluto_root(), "Manifest.toml"); isfile(m) ? string(hash(read(m, String))) : "")

const _nb_build_proc  = Ref{Union{Base.Process,Nothing}}(nothing)
const _nb_build_error = Ref{Union{String,Nothing}}(nothing)

# Does the on-disk stamp match this Julia + the current Manifest? (Same two fields the build writes.)
function _stamp_matches(stamp::Union{String,Nothing}, julia::AbstractString, manifest::AbstractString)::Bool
    stamp === nothing && return false
    try
        d = JSON3.read(stamp)
        String(get(d, :julia, "")) == julia && String(get(d, :manifest, "")) == manifest
    catch
        false
    end
end

# Pure classifier (testable, no IO): given whether deps.so exists, its stamp contents (or nothing),
# whether a build is running, whether the last build errored, and the current Julia + Manifest hash →
# one of: "ready" (fresh image), "stale" (image exists but built for a different Julia/package set —
# rebuild), "building", "error", "absent". The frontend auto-rebuilds on "absent" OR "stale".
function _classify_sysimage(exists::Bool, stamp::Union{String,Nothing}, building::Bool, errored::Bool,
                            julia::AbstractString, manifest::AbstractString)::String
    if exists
        _stamp_matches(stamp, julia, manifest) && return "ready"
        return building ? "building" : "stale"
    end
    building && return "building"
    errored  && return "error"
    "absent"
end

function _sysimage_status()::String
    p = _nb_build_proc[]
    building = p !== nothing && process_running(p)
    stamp = isfile(_sysimage_stamp()) ? read(_sysimage_stamp(), String) : nothing
    _classify_sysimage(isfile(_sysimage_path()), stamp, building, _nb_build_error[] !== nothing,
                       string(VERSION), _manifest_hash())
end

# Kick off the background build if the image isn't already fresh or being built. Idempotent; returns
# the resulting status. Rebuilds over a STALE image too (create_sysimage overwrites deps.so). Errors
# (env not set up / script missing) propagate to the caller as a 500.
function _ensure_sysimage_build!()::String
    lock(_nb_lock) do
        _sysimage_status() == "ready" && return "ready"
        p = _nb_build_proc[]
        (p !== nothing && process_running(p)) && return "building"

        pluto_root   = _pluto_root()
        build_script = joinpath(pluto_root, "build_sysimage.jl")
        isfile(build_script) || error("pluto/build_sysimage.jl not found at $build_script")
        _pluto_env_ready() || error("The notebook environment is not set up. $_SETUP_HINT")

        julia_exe = joinpath(Sys.BINDIR, Base.julia_exename())
        cmd = Cmd(`$julia_exe --project=$pluto_root $build_script`)
        @info "Building the notebook fast-plot sysimage in the background (first run, ~10 min)..." out = _sysimage_path()
        _nb_build_error[] = nothing
        proc = run(pipeline(cmd; stdout = stdout, stderr = stderr), wait = false)
        _nb_build_proc[] = proc
        # Watch for completion off the request path: success = the file now exists (PackageCompiler
        # can exit 0 without writing on some failures, so trust the file, not the code).
        @async begin
            wait(proc)
            lock(_nb_lock) do
                _nb_build_error[] = isfile(_sysimage_path()) ? nothing :
                    "The fast-plot sysimage build failed — notebooks still work (slower first plot). Retry, or run `pixi run notebooks-sysimage`."
            end
        end
        "building"
    end
end

# POST /api/notebooks/build-sysimage  → { status }   (status as _sysimage_status)
function api_notebooks_build_sysimage(body_bytes::Vector{UInt8})
    status = try
        _ensure_sysimage_build!()
    catch e
        @warn "Could not start sysimage build" exception = e
        return 500, JSON3.write((; error = "Could not start the fast-plot build: $(sprint(showerror, e))"))
    end
    200, JSON3.write((; status = status))
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
    for r in (_nb_proc_ref, _nb_build_proc)
        try
            p = r[]
            p !== nothing && process_running(p) && kill(p)
        catch
        end
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

# GET /api/notebooks/content?projectUid=&file=  → { file, scope, content }
# Read a notebook's CURRENT source (including the user's own edits) so the observer can "have a look"
# when the user is stuck — the Phase 2 teaching flow: read, explain, suggest corrected cells to paste;
# never overwrite (a revision becomes a NEW notebook via create_notebook). Resolves under the project's
# notebooks/ dir, then the shipped examples/ dir. `_safe_nb_file` blocks path traversal. Read-only.
function api_notebooks_content(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    file  = _safe_nb_file(get(query, "file", ""))
    (isempty(uid) || file === nothing) && return 400, JSON3.write((; error = "projectUid + file required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    ppath = joinpath(_project_notebooks_dir(uid), file)
    epath = joinpath(_repo_notebooks_dir(), file)
    path, scope = isfile(ppath) ? (ppath, "project") :
                  isfile(epath) ? (epath, "example") : (nothing, "")
    path === nothing && return 404, JSON3.write((; error = "Notebook not found"))
    200, JSON3.write((; file = file, scope = scope, content = read(path, String)))
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

# The mandatory first cell — activates the Notebooks engine env so the dev `Cecelia`/`CeceliaNb`
# resolve (mirrors notebook_template.jl). The write route ALWAYS prepends this, so a generated
# notebook is self-contained and runnable regardless of what the caller supplied.
const _NB_ACTIVATION_CELL = """# Activate the Notebooks env (path-sources the dev Cecelia). Keep this as the first cell.
begin
    import Pkg
    Pkg.activate(get(ENV, \"CECELIA_PLUTO_ENV\", joinpath(@__DIR__, \"..\", \"pluto\")))
end"""

# A valid v4-UUID string for a Pluto cell id — built from Base rand/bytes2hex so we don't pull the
# UUIDs stdlib into the api env. (Pluto parses cell ids as UUIDs, so they must be well-formed.)
function _cell_uuid()::String
    b = rand(UInt8, 16)
    b[7] = (b[7] & 0x0f) | 0x40   # version 4
    b[9] = (b[9] & 0x3f) | 0x80   # variant 10xx
    h = bytes2hex(b)
    string(h[1:8], "-", h[9:12], "-", h[13:16], "-", h[17:20], "-", h[21:32])
end

# Serialise a list of Julia cell sources into a valid Pluto notebook `.jl` (the format
# notebook_template.jl uses: a header, one `# ╔═╡ <uuid>` marker per cell, then a `# ╔═╡ Cell order:`
# block). The activation cell is prepended and pinned to the template's fixed id; caller cells get
# fresh uuids. Pure — unit-tested in api/test.
function _pluto_notebook_source(cells::AbstractVector)::String
    all_cells = vcat(Any[_NB_ACTIVATION_CELL], collect(cells))
    ids  = String[]
    body = IOBuffer()
    for (i, code) in enumerate(all_cells)
        id = i == 1 ? "10000000-0000-0000-0000-000000000001" : _cell_uuid()
        push!(ids, id)
        print(body, "# ╔═╡ ", id, "\n", rstrip(String(code)), "\n\n")
    end
    io = IOBuffer()
    print(io, "### A Pluto.jl notebook ###\n# v1.0.3\n\n")
    print(io, "using Markdown\nusing InteractiveUtils\n\n")
    print(io, String(take!(body)))
    print(io, "# ╔═╡ Cell order:\n")
    for id in ids
        print(io, "# ╠═", id, "\n")
    end
    String(take!(io))
end

# POST /api/notebooks/write  { projectUid, name, cells:[code…], description }  → { ok, file }
# Create a NEW notebook FROM CELLS — the observer's "generate a notebook" path, distinct from /create
# (which stamps a blank template). Serialises `cells` to valid Pluto format (env-activation cell
# prepended), registers it, and snapshots v1 so there's an immediate restore point. CREATE-ONLY: 409 if
# the name exists, so it never clobbers a notebook the user may have edited — Claude picks a new name or
# the user iterates in Pluto. The user then owns/edits it freely. Backs the create_notebook MCP tool.
function api_notebooks_write(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    uid  = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    file = _safe_nb_file(get(body, :name, ""))
    file === nothing && return 400, JSON3.write((; error = "Invalid notebook name"))
    cells = get(body, :cells, nothing)
    (cells === nothing || isempty(cells)) && return 400, JSON3.write((; error = "cells required (a non-empty list of Julia cell sources)"))

    dir = _project_notebooks_dir(uid); mkpath(dir)
    dest = joinpath(dir, file)
    isfile(dest) && return 409, JSON3.write((; error = "Notebook already exists: $file (pick a new name; create-only)"))
    open(dest, "w") do io; write(io, _pluto_notebook_source(collect(cells))); end

    reg = _read_registry(uid)
    reg[file] = Dict{String,Any}("description" => String(get(body, :description, "")),
                                 "current" => 0, "updatedAt" => string(Dates.now()))
    _write_registry!(uid, reg)
    # snapshot v1 — an immediate restore point before the user starts editing in Pluto
    api_notebooks_snapshot(Vector{UInt8}(JSON3.write((; projectUid = uid, file = file))))
    # nudge an open Notebooks page to refresh (it has no per-notebook poll); harmless if none is open.
    broadcast_ws(Dict{String,Any}("type" => "notebooks_changed", "projectUid" => uid, "file" => file))
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
