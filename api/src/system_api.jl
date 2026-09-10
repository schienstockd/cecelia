# System envs — probe + install opt-in pixi environments.
#
# One consumer today: `cellpose-v3` on macOS. See docs/todo/CELLPOSE_V3_OPTIN_PLAN.md.
# Not a general "install any package" surface — the catalog is a small, static set of extras the app
# knows about; adding a new one = one row in `_OPT_IN_ENVS` and a `[feature.<name>]` block in
# pixi.toml.
#
# Endpoints:
#   GET  /api/system/envs                   → { platform, envs: {name: {installed, supported, ...}} }
#   POST /api/system/envs/install {env}     → 202 { started, jobId, env } — streams progress over WS
#                                              (`system:env-install-log`, `system:env-install-complete`)
#
# Refused in a git checkout only when the caller asks; the install itself is fine in dev
# (pixi lives on PATH via `pixi run dev`), but shipped-app resolvability of `pixi` is the real gate.
# See docs/SHIPPING.md → bundled `pixi` lookup once that's decided.

using JSON3

# Repo root, mirrors update_api.jl's constant of the same name. Own definition, so this file stays
# self-contained (include order-independent).
const _SYSTEM_APP_ROOT = abspath(joinpath(@__DIR__, "..", ".."))

# Static catalog of known opt-in envs.
const _OPT_IN_ENVS = Dict(
    "cellpose-v3" => (
        supported_platforms = ["osx-arm64"],
        approx_size_mb      = 500,
        description         = "Cellpose 3 (cyto2/cyto3) — fast on Apple Silicon MPS.",
    ),
)

# Platform label matching pixi.toml `[target.<label>]` and `[feature.*.platforms]`.
function _current_pixi_platform()::String
    if Sys.isapple()
        Sys.ARCH === :aarch64 ? "osx-arm64" : "osx-64"
    elseif Sys.iswindows()
        "win-64"
    elseif Sys.islinux()
        "linux-64"
    else
        "unknown"
    end
end

_pixi_env_dir(name::AbstractString)::String = joinpath(_SYSTEM_APP_ROOT, ".pixi", "envs", String(name))

# Presence of the env's python interpreter is the only reliable "installed" signal — an empty
# directory is not enough (a half-cancelled install can leave one), and pixi has no cheap query.
_env_installed(name::AbstractString)::Bool =
    isfile(joinpath(_pixi_env_dir(name), Sys.iswindows() ? "python.exe" : joinpath("bin", "python")))

# Locate the pixi binary. `PIXI` env var wins (bundle-time override), else PATH, else "".
function _pixi_bin_path()::String
    p = strip(get(ENV, "PIXI", ""))
    !isempty(p) && isfile(p) && return p
    found = Sys.which("pixi")
    isnothing(found) ? "" : String(found)
end

function api_system_envs(req)
    plat = _current_pixi_platform()
    envs = Dict{String,Any}()
    for (name, meta) in _OPT_IN_ENVS
        envs[String(name)] = Dict{String,Any}(
            "installed"    => _env_installed(name),
            "supported"    => plat in meta.supported_platforms,
            "approxSizeMb" => meta.approx_size_mb,
            "description"  => meta.description,
        )
    end
    200, JSON3.write((; platform = plat, envs))
end

const _ENV_INSTALL_JOB_PREFIX = "system-env-install:"

_env_install_job_id(name::AbstractString)::String = _ENV_INSTALL_JOB_PREFIX * String(name)

function api_system_envs_install(body_bytes)
    body = try
        JSON3.read(body_bytes)
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    name = String(get(body, :env, ""))
    haskey(_OPT_IN_ENVS, name) || return 400, JSON3.write((; error = "unknown env: $name"))
    meta = _OPT_IN_ENVS[name]
    plat = _current_pixi_platform()
    plat in meta.supported_platforms || return 400, JSON3.write((;
        error = "env $name is not supported on this platform ($plat) — supported: $(join(meta.supported_platforms, ", "))"))

    _env_installed(name) && return 200, JSON3.write((; installed = true, alreadyPresent = true, env = name))

    pixi = _pixi_bin_path()
    isempty(pixi) && return 500, JSON3.write((;
        error = "pixi executable not found on PATH — cannot install env from within the app"))

    job_id = _env_install_job_id(name)
    start_job!(job_id)
    Threads.@spawn _run_env_install(name, pixi, meta, job_id)
    202, JSON3.write((; started = true, jobId = job_id, env = name))
end

# Shell `pixi install -e <name>` in the app root, stream lines through the task rail (task:log,
# task:progress, task:status) so the Task Manager row + log rail show what pixi is doing — a
# ~500 MB install with no visible output looks like a hang. Register the subprocess with the job
# registry so a cancel reaches it. Best-effort pre-warm of cellpose 3 built-in weights so the
# first segmentation doesn't stall on cellpose's server.
#
# Task-rail semantics — the same pattern movie_rail.jl uses for background jobs:
#   ws_status(..., "running", ...; fun = "system:install-env:<name>", pool = "job")
#   ws_log     for each pixi/warm line
#   ws_progress in coarse phases (start → install done → warm done)
#   ws_status(..., "done"|"failed", ...)
# `pool = "job"` marks it as a background job, not a scheduler task, in the rail.
function _run_env_install(name::AbstractString, pixi::AbstractString, meta::NamedTuple,
                          job_id::AbstractString)
    fun = "system:install-env:$name"
    _log(msg)  = ws_log(nothing, job_id, msg)
    _prog(frac) = ws_progress(nothing, job_id, Float64(frac))
    ok = false
    ws_status(nothing, job_id, "running", ""; fun = fun, pool = "job")
    try
        _log("pixi install -e $name  (~$(meta.approx_size_mb) MB)")
        _prog(0.02)
        cmd = Cmd(`$pixi install -e $name`; dir = _SYSTEM_APP_ROOT)
        out = Pipe(); err = Pipe()
        proc = run(pipeline(cmd; stdout = out, stderr = err); wait = false)
        close(out.in); close(err.in)
        track_job!(job_id, proc)
        # Pixi writes progress mostly to stderr (one "Downloading …" line per package + the tally);
        # drain both so nothing is dropped. Each line lands as a `task:log` frame under this job's
        # taskId — the Task Manager row and the log rail both show them.
        @async try; for line in eachline(err); _log(line); end; catch; end
        @async try; for line in eachline(out); _log(line); end; catch; end
        wait(proc)
        install_ok = proc.exitcode == 0 && proc.termsignal == 0 && _env_installed(name)
        if !install_ok
            _log("[ERROR] pixi install exited with status $(proc.exitcode) / signal $(proc.termsignal)")
            ws_status(nothing, job_id, "failed", ""; fun = fun, pool = "job")
            return
        end
        _prog(0.85)
        if name == "cellpose-v3"
            _log("pre-warming cyto2 / cyto3 model weights (~50 MB, one-time)…")
            warm = Cmd(`$pixi run -e $name python -c "from cellpose import models; models.CellposeModel(model_type='cyto2'); models.CellposeModel(model_type='cyto3')"`;
                       dir = _SYSTEM_APP_ROOT)
            warm_out = Pipe()
            try
                wproc = run(pipeline(warm; stdout = warm_out, stderr = warm_out); wait = false)
                close(warm_out.in)
                track_job!(job_id, wproc)
                @async try; for line in eachline(warm_out); _log(line); end; catch; end
                wait(wproc)
                if !(wproc.exitcode == 0 && wproc.termsignal == 0)
                    _log("[WARN] weight pre-warm failed — models will download on first run")
                end
            catch e
                _log("[WARN] weight pre-warm errored ($(sprint(showerror, e))) — models will download on first run")
            end
        end
        _prog(1.0)
        _log("[DONE] $name env ready")
        ok = _env_installed(name)
        ws_status(nothing, job_id, ok ? "done" : "failed", ""; fun = fun, pool = "job")
    catch e
        _log("[ERROR] $(sprint(showerror, e))")
        ws_status(nothing, job_id, "failed", ""; fun = fun, pool = "job")
    finally
        finish_job!(job_id)
    end
end
