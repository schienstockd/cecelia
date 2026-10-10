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
#   GET  /api/system/weights                → { models: {name: {present, fetching, label, approxSizeMb}} }
#   POST /api/system/weights/fetch {model}  → 202 { started, jobId } | 200 { present: true }
#
# pixi is resolved by `_find_pixi` (pixi_bin.jl) — the shipped app has no pixi on PATH.

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
        weights             = [m for (m, _, b) in Cecelia.BUILTIN_CELLPOSE_MODELS if b === :v3],
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
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    name = _wstr(body, :env)
    haskey(_OPT_IN_ENVS, name) || return 400, JSON3.write((; error = "unknown env: $name"))
    meta = _OPT_IN_ENVS[name]
    plat = _current_pixi_platform()
    plat in meta.supported_platforms || return 400, JSON3.write((;
        error = "env $name is not supported on this platform ($plat) — supported: $(join(meta.supported_platforms, ", "))"))

    _env_installed(name) && return 200, JSON3.write((; installed = true, alreadyPresent = true, env = name))
    # A shared (system-scope) install is read-only to every account but the one that installed it,
    # and `pixi install` dies on the env lock there — refuse up front instead of streaming that error.
    _dir_writable(_SYSTEM_APP_ROOT) || return 403, JSON3.write((;
        error = "Shared installation — only the account that installed Cecelia can add $name."))

    pixi = _find_pixi()
    isempty(pixi) && return 500, JSON3.write((;
        error = "pixi not found — cannot install $name from within the app"))

    job_id = _env_install_job_id(name)
    # a second click (or tab) joins the install in flight rather than racing it
    claim_job!(job_id) && Threads.@spawn _run_env_install(name, pixi, meta, job_id)
    202, JSON3.write((; started = true, jobId = job_id, env = name))
end

# Shell `pixi install -e <name>` in the app root, stream lines through the task rail (task:log,
# task:progress, task:status) so the Task Manager row + log rail show what pixi is doing — a
# ~500 MB install with no visible output looks like a hang. Register the subprocess with the job
# registry so a cancel reaches it. Then a best-effort fetch of the env's built-in weights
# (`_fetch_weights`) so the first segmentation doesn't stall on cellpose's server.
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
        if !isempty(meta.weights)
            _log("fetching $(join(meta.weights, " / ")) model weights (one-time)…")
            _fetch_weights(job_id, meta.weights; env = Symbol(replace(name, "-" => "_")), on_log = _log) ||
                _log("[WARN] weight fetch failed — models will download on first run")
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

# ── Model weights ────────────────────────────────────────────────────────────────────────────────
#
# Cellpose downloads a built-in model's weights the first time it loads it. For `cpsam_v2` that is
# ~1.2 GB, which inside a preview request outlasts the browser's timeout. So weights are fetched by
# ONE job — `model-weights:<model>`, visible in the Task Manager and retryable — started from three
# places, and never inside a request:
#   1. the installers (install.sh / install.ps1 run `python -m cecelia.utils.model_weights`);
#   2. app start, when missing (`fetch_missing_weights_at_boot!`, installed apps only);
#   3. a preview that needs them (`missing_weights_job`, preview_api.jl) or the route below.
#
# The cellpose 4 built-ins of `BUILTIN_CELLPOSE_MODELS`: their file is `<cellpose model dir>/<name>`,
# so presence is a stat. One architecture, so one size. Cellpose 3 weights (cyto2/cyto3) are fetched
# by the `cellpose-v3` install job instead (`_OPT_IN_ENVS` → `weights`).
const _V4_WEIGHTS_MB = 1200
const _MODEL_WEIGHTS = Dict(
    m => (label = "$label weights", approx_size_mb = _V4_WEIGHTS_MB)
    for (m, label, backend) in Cecelia.BUILTIN_CELLPOSE_MODELS if backend === :v4)

const _WEIGHTS_JOB_PREFIX = "model-weights:"
_weights_job_id(name::AbstractString)::String = _WEIGHTS_JOB_PREFIX * String(name)

# Cellpose 4's own cache (`cellpose.models.MODEL_DIR`): `CELLPOSE_LOCAL_MODELS_PATH`, else
# `~/.cellpose/models`. The fetch writes there through cellpose's own constants; this only stats.
function _cellpose_weights_dir()::String
    d = strip(get(ENV, "CELLPOSE_LOCAL_MODELS_PATH", ""))
    isempty(d) ? joinpath(expand_user("~"), ".cellpose", "models") : String(d)
end

# The fetch writes atomically, so a file on disk is complete weights.
_weights_present(name::AbstractString)::Bool = isfile(joinpath(_cellpose_weights_dir(), String(name)))

# Run `cecelia.utils.model_weights` for `models` in `env` (`nothing` = the default env) under job
# `job_id`: byte progress to the job's bar, other lines to its log, subprocess registered for cancel.
function _fetch_weights(job_id::AbstractString, models::AbstractVector;
                        env::Union{Symbol,Nothing} = nothing, on_log::Function)::Bool
    Cecelia.run_py("utils/model_weights.py", Dict("models" => collect(String, models)), mktempdir();
                   env         = env,
                   on_log      = on_log,
                   on_progress = (n, t) -> t > 0 && ws_progress(nothing, job_id, n / t),
                   on_process  = p -> track_job!(String(job_id), p))
end

function _run_weights_fetch(name::AbstractString, job_id::AbstractString)
    meta = _MODEL_WEIGHTS[name]
    fun  = "system:fetch-weights:$name"
    _log(msg) = ws_log(nothing, job_id, msg)
    ws_status(nothing, job_id, "running", ""; fun = fun, pool = "job")
    ok = false
    try
        _log("Downloading $(meta.label) (~$(round(meta.approx_size_mb / 1000; digits = 1)) GB)…")
        ok = _fetch_weights(job_id, [name]; on_log = _log) && _weights_present(name)
        ok || _log("[ERROR] $(meta.label) download failed — retry from the preview or restart the app")
    catch e
        _log("[ERROR] $(sprint(showerror, e))")
    finally
        ws_status(nothing, job_id, ok ? "done" : "failed", ""; fun = fun, pool = "job")
        finish_job!(job_id)
    end
end

"""
    start_weights_fetch!(name) -> Union{String,Nothing}

Start the weights job for the built-in model `name`, or join the one already running. Returns the job
id, or `nothing` when the weights are on disk already (or `name` is not a listed built-in)."""
function start_weights_fetch!(name::AbstractString)::Union{String,Nothing}
    haskey(_MODEL_WEIGHTS, name) || return nothing
    _weights_present(name) && return nothing
    job_id = _weights_job_id(name)
    claim_job!(job_id) && Threads.@spawn _run_weights_fetch(String(name), job_id)
    job_id
end

"""
    missing_weights_job(params) -> Union{NamedTuple,Nothing}

For a preview: if `params` (as prepared for the run) names a built-in model whose weights are not on
disk, start or join their fetch and return `(; job_id, label, approx_size_mb)`. `nothing` = go ahead.
Reads every `params["models"][*]["model"]`, the shape cellpose-style tasks share."""
function missing_weights_job(params)::Union{NamedTuple,Nothing}
    models = params isa AbstractDict ? get(params, "models", nothing) : nothing
    models isa AbstractDict || return nothing
    for m in values(models)
        m isa AbstractDict || continue
        name = string(get(m, "model", ""))
        job = start_weights_fetch!(name)
        job === nothing && continue
        meta = _MODEL_WEIGHTS[name]
        return (; job_id = job, label = meta.label, approx_size_mb = meta.approx_size_mb)
    end
    nothing
end

# App start: fetch the default model's weights if they are missing, as a visible job — covers an
# offline install, a failed installer download, and installs that predate the installer step.
# Installed apps only: a dev checkout or CI run would start a 1.2 GB download on every boot.
# `CECELIA_SKIP_MODEL_WEIGHTS=1` opts out, as in the installers.
function fetch_missing_weights_at_boot!()
    get(ENV, "CECELIA_SKIP_MODEL_WEIGHTS", "") == "1" && return nothing
    _is_installed() || return nothing
    start_weights_fetch!(first(first(Cecelia.BUILTIN_CELLPOSE_MODELS)))
end

function api_system_weights(req)
    models = Dict{String,Any}(
        name => Dict{String,Any}(
            "present"      => _weights_present(name),
            "fetching"     => job_active(_weights_job_id(name)),
            "label"        => meta.label,
            "approxSizeMb" => meta.approx_size_mb,
        ) for (name, meta) in _MODEL_WEIGHTS)
    200, JSON3.write((; models))
end

function api_system_weights_fetch(body_bytes)
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    name = _wstr(body, :model)
    haskey(_MODEL_WEIGHTS, name) || return 400, JSON3.write((; error = "unknown model: $name"))
    job = start_weights_fetch!(name)
    job === nothing && return 200, JSON3.write((; present = true, model = name))
    202, JSON3.write((; started = true, jobId = job, model = name))
end
