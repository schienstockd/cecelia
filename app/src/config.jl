import TOML

const _DEFAULT_CONF_PATH = joinpath(@__DIR__, "..", "config.toml")
const _DOTENV_PATH       = joinpath(@__DIR__, "..", "..", ".env")
const _CONF = Ref(Dict{String,Any}())

function _deep_merge(a::Dict, b::Dict)::Dict
    result = copy(a)
    for (k, v) in b
        result[k] = (v isa Dict && get(a, k, nothing) isa Dict) ?
            _deep_merge(a[k], v) : v
    end
    result
end

# Read KEY=value pairs from a .env file. Skips comments and blank lines.
# Values are NOT shell-expanded; use expanduser() on the result.
function _read_dotenv(path::String)::Dict{String,String}
    out = Dict{String,String}()
    isfile(path) || return out
    for line in eachline(path)
        line = strip(line)
        (isempty(line) || startswith(line, '#')) && continue
        m = match(r"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$", line)
        isnothing(m) || (out[m[1]] = strip(m[2]))
    end
    out
end

# Pure resolver (unit-testable, no env/file reads): given the three ordered signals, pick the dir.
# Order: explicit arg → CECELIA_DEV_DIR env → CECELIA_DEV_DIR in .env → ~/.cecelia default.
function _resolve_config_dir(dev_dir::Union{AbstractString,Nothing},
                             env_val::Union{AbstractString,Nothing},
                             dotenv_val::Union{AbstractString,Nothing})::String
    isnothing(dev_dir)    || return expanduser(String(dev_dir))
    isnothing(env_val)    || return expanduser(String(env_val))
    isnothing(dotenv_val) || return expanduser(String(dotenv_val))
    expanduser("~/.cecelia")
end

"""
    config_dir([dev_dir]) -> String

The per-user directory that holds `custom.toml`. Resolution order (first wins):

  1. Explicit argument: `config_dir("~/cecelia-pineapple/dev")`  — tests / REPL
  2. `CECELIA_DEV_DIR` environment variable                       — dev, CI
  3. `CECELIA_DEV_DIR` in `cecelia-pineapple/.env` (gitignored)   — dev checkout
  4. Default: `~/.cecelia`                                        — the installed app

The presence of `.env` / `CECELIA_DEV_DIR` **is** the dev signal: an installed app has neither
and falls through to `~/.cecelia`, so a developer's real `~/.cecelia` is never touched by a dev
run, and the config path never depends on install scope (user vs system-wide). This is the single
resolver both the reader (`init_cecelia!`) and the writer (`set_projects_dir!`) share, so they can
never disagree. See `docs/todo/ONBOARDING_PLAN.md` (D1). Mirrors R's `cciaUse(path)` pattern.
"""
function config_dir(dev_dir::Union{String,Nothing} = nothing)::String
    dotenv = _read_dotenv(_DOTENV_PATH)
    _resolve_config_dir(dev_dir,
                        get(ENV, "CECELIA_DEV_DIR", nothing),
                        get(dotenv, "CECELIA_DEV_DIR", nothing))
end

"""
    custom_toml_path([dev_dir]) -> String

Absolute path to the user's `custom.toml`, inside [`config_dir`](@ref). The one path the setup
wizard writes and `init_cecelia!` reads.
"""
custom_toml_path(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "custom.toml")

"""
Initialise Cecelia configuration. Merges the bundled `config.toml` with the user `custom.toml`
found at [`custom_toml_path`](@ref) (see [`config_dir`](@ref) for how the location is resolved).
"""
function init_cecelia!(dev_dir::Union{String,Nothing} = nothing)
    resolved = config_dir(dev_dir)

    cfg = if isfile(_DEFAULT_CONF_PATH)
        @info "Loaded default config" path = _DEFAULT_CONF_PATH
        TOML.parsefile(_DEFAULT_CONF_PATH)
    else
        @warn "Default config not found" path = _DEFAULT_CONF_PATH
        Dict{String,Any}()
    end

    custom = joinpath(resolved, "custom.toml")
    if isfile(custom)
        @info "Merging custom config" path = custom
        cfg = _deep_merge(cfg, TOML.parsefile(custom))
    else
        @warn "Custom config not found, using defaults only" path = custom
    end

    _CONF[] = cfg
    nothing
end

function cecelia_conf()::Dict{String,Any}
    isempty(_CONF[]) && init_cecelia!()
    _CONF[]
end

function _cfg_dir(key::String, default::String)::String
    d = get(cecelia_conf(), "dirs", Dict{String,Any}())
    expanduser(string(get(d, key, default)))
end

const _PROJECTS_DIR_PLACEHOLDER = "/path/to/projects"

projects_dir()::String = _cfg_dir("projects", _PROJECTS_DIR_PLACEHOLDER)

"""
    setup_required() -> Bool

`true` when first-launch setup is still needed: no `custom.toml` yet, or the projects dir is
unconfigured / still the placeholder / not an existing directory. The API exposes this as
`setup_required` so the frontend can route to `/setup`. See `docs/todo/ONBOARDING_PLAN.md`.
"""
function setup_required()::Bool
    isfile(custom_toml_path()) || return true
    p = projects_dir()
    isempty(p) || p == _PROJECTS_DIR_PLACEHOLDER || !isdir(p)
end

"""
    set_projects_dir!(path) -> String

Persist `path` as `dirs.projects` in the user's `custom.toml` (creating the file/dir if needed,
**merging** so other keys survive) and hot-reload config. Writer half of the config pair — it
targets the same [`custom_toml_path`](@ref) the reader uses. The literal string is stored (so a
leading `~` stays portable across users); `expanduser` happens on read in `_cfg_dir`. Returns the
stored path. Creating/validating the projects directory itself is the caller's job (the setup
endpoint). See `docs/todo/ONBOARDING_PLAN.md` (D1/D3).
"""
function set_projects_dir!(path::AbstractString)::String
    stored   = strip(String(path))
    cfg_path = custom_toml_path()
    mkpath(dirname(cfg_path))
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    dirs = get(cfg, "dirs", Dict{String,Any}())
    dirs["projects"] = stored
    cfg["dirs"] = dirs
    open(cfg_path, "w") do io
        TOML.print(io, cfg)
    end
    init_cecelia!()   # hot-reload: _CONF[] refreshed in place, accessors read it live (D3)
    stored
end

# Resolve the bioformats2raw launcher: explicit config override → the copy the installer fetched
# alongside the app (`<install>/bioformats2raw/`; ~190 MB, so downloaded at install time rather than
# shipped in the release bundle — Java comes from the Pixi env) → PATH → the (likely-missing)
# default. Run via `pixi run` so the `bioformats2raw` script finds `java`. See docs/SHIPPING.md.
function bioformats2raw_bin()::String
    exe = Sys.iswindows() ? "bioformats2raw.bat" : "bioformats2raw"
    d   = get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "bioformats2raw", "")
    if !isempty(string(d)) && string(d) != "/path/to/bioformats2raw"
        return joinpath(expanduser(string(d)), "bin", exe)
    end
    bundled = joinpath(@__DIR__, "..", "..", "bioformats2raw", "bin", exe)   # repo/install root
    isfile(bundled) && return bundled
    found = Sys.which(exe)
    found === nothing || return string(found)
    joinpath(_cfg_dir("bioformats2raw", "/path/to/bioformats2raw"), "bin", exe)
end

python_bin_path()::String = _cfg_dir("python", "python3")

# Default for launching the napari bridge on the discrete GPU (hybrid-graphics machines). Reads
# `[napari].discreteGpu`; the api layer holds the runtime toggle (Settings) and seeds it from this.
napari_discrete_gpu()::Bool =
    Bool(get(get(cecelia_conf(), "napari", Dict{String,Any}()), "discreteGpu", false))

tasks_concurrent_limit()::Int =
    Int(get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "concurrentLimit", 4))
