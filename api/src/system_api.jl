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
#
# TESTING (2026-09-10): `supported_platforms` temporarily includes linux-64 + win-64 so the whole
# install/probe flow can be exercised off a Mac. Ship-time: narrow back to `["osx-arm64"]` — the
# advisor and pixi feature must both be tightened in the same change (see pixi.toml).
const _OPT_IN_ENVS = Dict(
    "cellpose-v3" => (
        supported_platforms = ["osx-arm64", "linux-64", "win-64"],   # TESTING — see note above
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

# Shell `pixi install -e <name>` in the app root, stream lines over the WS rail, register the
# subprocess with the job registry so a cancel reaches it. Best-effort pre-warm of cellpose 3
# built-in weights so the first segmentation doesn't stall on cellpose's server.
function _run_env_install(name::AbstractString, pixi::AbstractString, meta::NamedTuple,
                          job_id::AbstractString)
    _emit(msg) = broadcast_ws(Dict{String,Any}("type" => "system:env-install-log",
                                                "env" => String(name), "line" => msg))
    ok = false
    try
        _emit("pixi install -e $name  (~$(meta.approx_size_mb) MB)")
        cmd = Cmd(`$pixi install -e $name`; dir = _SYSTEM_APP_ROOT)
        out = Pipe(); err = Pipe()
        proc = run(pipeline(cmd; stdout = out, stderr = err); wait = false)
        close(out.in); close(err.in)
        track_job!(job_id, proc)
        # pixi writes progress mostly to stderr; drain both.
        @async try; for line in eachline(err); _emit(line); end; catch; end
        @async try; for line in eachline(out); _emit(line); end; catch; end
        wait(proc)
        ok = proc.exitcode == 0 && proc.termsignal == 0 && _env_installed(name)
        if ok && name == "cellpose-v3"
            _emit("pre-warming cyto2 / cyto3 model weights (~50 MB, one-time)…")
            warm = Cmd(`$pixi run -e $name python -c "from cellpose import models; models.CellposeModel(model_type='cyto2'); models.CellposeModel(model_type='cyto3')"`;
                       dir = _SYSTEM_APP_ROOT)
            try
                wproc = run(pipeline(warm; stdout = devnull, stderr = devnull); wait = false)
                track_job!(job_id, wproc)
                wait(wproc)
            catch
                _emit("[WARN] weight pre-warm failed — models will download on first run")
            end
        end
    catch e
        _emit("[ERROR] $(sprint(showerror, e))")
    finally
        broadcast_ws(Dict{String,Any}("type" => "system:env-install-complete",
                                       "env" => String(name), "ok" => ok,
                                       "installed" => _env_installed(name)))
        finish_job!(job_id)
    end
end
