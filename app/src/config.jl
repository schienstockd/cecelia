import TOML
# config.jl is included before utils.jl (which also imports it), so declare it here rather than
# relying on a later include having bound the name by the time a body first runs.
import JSON3

# Bootstrap: config-dir resolution + custom.toml merge + hot-reload. The domain-specific accessors
# and mutators live in `config/*.jl`, included at the bottom of this file — split out to keep this
# file scoped to the runtime state and its lifecycle (top-churned in git history; no invariants at
# stake in the split).

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
# Values are NOT shell-expanded; use expand_user() on the result.
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

"""
    expand_user(path) -> String

Replace a leading `~` with the user's home directory, **on every platform**.

Use this instead of `Base.expanduser`, which is documented as Unix-only ("On Unix systems, replace
a tilde character…") and is a **silent no-op on Windows** — a `~`-prefixed path then survives
verbatim into `joinpath`/`open`, producing paths like `~/.cecelia\\observer-mcp.json` that no
Windows API resolves. Every stored path in `custom.toml`/`.env` may legitimately start with `~`
(that is what keeps them portable across users), so this is the one expansion helper they all
go through. `homedir()` is correct on Windows (it honours `USERPROFILE`).
"""
function expand_user(path::AbstractString)::String
    s = String(path)
    s == "~" && return homedir()
    # Windows users may type either separator
    if startswith(s, "~/") || (Sys.iswindows() && startswith(s, "~\\"))
        # Split the remainder into components rather than pasting it on, so the result is a canonical
        # path: `joinpath(homedir(), "foo/bar")` on Windows yields `C:\Users\x\foo/bar` — mixed
        # separators, which Windows tolerates but which makes every path comparison unreliable.
        # Only treat `\` as a separator ON Windows; on Unix it is a legal filename character.
        seps = Sys.iswindows() ? ('/', '\\') : ('/',)
        parts = split(s[3:end], seps; keepempty = false)
        return isempty(parts) ? homedir() : joinpath(homedir(), parts...)
    end
    s
end

# Pure resolver (unit-testable, no env/file reads): given the three ordered signals, pick the dir.
# Order: explicit arg → CECELIA_DEV_DIR env → CECELIA_DEV_DIR in .env → ~/.cecelia default.
function _resolve_config_dir(dev_dir::Union{AbstractString,Nothing},
                             env_val::Union{AbstractString,Nothing},
                             dotenv_val::Union{AbstractString,Nothing})::String
    isnothing(dev_dir)    || return expand_user(String(dev_dir))
    isnothing(env_val)    || return expand_user(String(env_val))
    isnothing(dotenv_val) || return expand_user(String(dotenv_val))
    expand_user("~/.cecelia")
end

"""
    config_dir([dev_dir]) -> String

The per-user directory that holds `custom.toml`. Resolution order (first wins):

  1. Explicit argument: `config_dir("~/cecelia-feijoa/dev")`     — tests / REPL
  2. `CECELIA_DEV_DIR` environment variable                       — dev, CI
  3. `CECELIA_DEV_DIR` in `cecelia-feijoa/.env` (gitignored)      — dev checkout
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
    ensure_config_dir([dev_dir]) -> String

[`config_dir`](@ref), created if it does not exist yet. Use this — not bare `config_dir()` — before
**writing** anything into it.

`config_dir()` is a pure path computation: on a machine that has never run the setup wizard the
directory genuinely does not exist, so `open(joinpath(config_dir(), …), "w")` fails with
`SystemError: No such file or directory`. That is not hypothetical — it broke CI on all three
platforms once the observer wrote its MCP config on every status call.
"""
function ensure_config_dir(dev_dir::Union{String,Nothing} = nothing)::String
    d = config_dir(dev_dir)
    mkpath(d)
    d
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

"""
    cecelia_version() -> String

The package version, from `app/Project.toml`. **The one runtime reader.** `CITATION.cff` and
`frontend/package.json` carry the same string for user-facing tools that cannot see Julia; the
release-cutting checklist (`docs/RELEASING.md`) bumps all three together, and a testset in
`app/test/suite.jl` fails the suite if they diverge.
"""
cecelia_version()::String = string(pkgversion(@__MODULE__))

function _cfg_dir(key::String, default::String)::String
    d = get(cecelia_conf(), "dirs", Dict{String,Any}())
    expand_user(string(get(d, key, default)))
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
leading `~` stays portable across users); `expand_user` happens on read in `_cfg_dir`. Returns the
stored path. Creating/validating the projects directory itself is the caller's job (the setup
endpoint). See `docs/todo/ONBOARDING_PLAN.md` (D1/D3).
"""
function set_projects_dir!(path::AbstractString)::String
    stored   = strip(String(path))
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    dirs = get(cfg, "dirs", Dict{String,Any}())
    dirs["projects"] = stored
    cfg["dirs"] = dirs
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()   # hot-reload: _CONF[] refreshed in place, accessors read it live (D3)
    stored
end

# ── Domain-specific config surfaces (all in the Cecelia module namespace) ──────
# Order: models → binaries → throttle → image_format. `binaries.jl` uses `_cfg_dir`/`expand_user`
# from above; `throttle.jl` and `image_format.jl` use `custom_toml_path`/`ensure_config_dir`/
# `init_cecelia!`/`write_atomic` (the last comes from utils.jl and is call-time, not load-time).
# `image_format.jl`'s `_bf2raw_lib_dir` default uses `bioformats2raw_bin` from binaries.jl.
include("config/models.jl")
include("config/binaries.jl")
include("config/throttle.jl")
include("config/image_format.jl")
include("config/tls.jl")
