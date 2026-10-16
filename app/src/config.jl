import TOML
# config.jl is included before utils.jl (which also imports it), so declare it here rather than
# relying on a later include having bound the name by the time a body first runs.
import JSON3

# Bootstrap: config-dir resolution + custom.toml merge + hot-reload. The domain-specific accessors
# and mutators live in `config/*.jl`, included at the bottom of this file — split out to keep this
# file scoped to the runtime state and its lifecycle (top-churned in git history; no invariants at
# stake in the split).

const _DEFAULT_CONF_PATH = joinpath(@__DIR__, "..", "config.toml")
const _CONF = Ref(Dict{String,Any}())

# `config_dir` / `ensure_config_dir` / `expand_user` live in `config_dir.jl` (included before this file):
# Base-only, so the dev supervisor and `pixi run stop` resolve the same dir without loading Cecelia.

function _deep_merge(a::Dict, b::Dict)::Dict
    result = copy(a)
    for (k, v) in b
        result[k] = (v isa Dict && get(a, k, nothing) isa Dict) ?
            _deep_merge(a[k], v) : v
    end
    result
end

# custom.toml keys that were RENAMED, as `(table, old, new)`. A user who set the old key keeps their
# value under the new name; `init_cecelia!` rewrites the file once so the old key does not linger
# and later writers (`set_pool_limit!`) do not have two names for one setting.
const _CUSTOM_CONF_RENAMES = (
    ("pools", "network", "hpc"),   # the unused `network` pool became the HPC lane (HPC_PLAN.md D6)
)

"""
    migrate_custom_conf!(cfg) -> Vector{String}

Apply [`_CUSTOM_CONF_RENAMES`](@ref) to a parsed `custom.toml` in place. Returns one note per change,
empty when nothing needed migrating. When both names are present the NEW one wins and the old is
dropped: it was written later, by a version that already knew the new name.
"""
function migrate_custom_conf!(cfg::AbstractDict)::Vector{String}
    notes = String[]
    for (tbl, old, new) in _CUSTOM_CONF_RENAMES
        t = get(cfg, tbl, nothing)
        (t isa AbstractDict && haskey(t, old)) || continue
        v = pop!(t, old)
        if haskey(t, new)
            push!(notes, "[$tbl] $old dropped: $new is already set")
        else
            t[new] = v
            push!(notes, "[$tbl] $old renamed to $new")
        end
    end
    notes
end

"""
    _renamed_pool(name) -> String

`name`, or the pool it was renamed to. For pool names stored OUTSIDE custom.toml, which
`migrate_custom_conf!` cannot rewrite: a saved chain node's override, a drop-in module's spec.
"""
function _renamed_pool(name::String)::String
    for (tbl, old, new) in _CUSTOM_CONF_RENAMES
        tbl == "pools" && old == name && return new
    end
    name
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
        user = TOML.parsefile(custom)
        notes = migrate_custom_conf!(user)
        if !isempty(notes)
            # The merge below uses the migrated dict either way; a failed rewrite only means the
            # migration runs again next launch.
            try
                write_atomic(io -> TOML.print(io, user), custom)
                @info "Migrated custom config" path = custom changes = notes
            catch e
                @warn "Could not rewrite migrated custom config; applied in memory" path = custom exception = e
            end
        end
        cfg = _deep_merge(cfg, user)
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
include("config/server_threads.jl")
include("config/viewer_cache.jl")
include("config/shared_workstation.jl")
# ── Per-profile settings — after agent_runner.jl so `active_profile_name()` is available ──
# `agent_runner.jl` is loaded from Cecelia.jl AFTER config.jl, so profile_settings.jl can
# only be included from there. That include statement lives near the AI block, not here.
