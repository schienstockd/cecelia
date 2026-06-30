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

"""
Initialise Cecelia configuration.

Merges the bundled `config.toml` with a user `custom.toml` found in `dev_dir`.

`dev_dir` resolution order (first wins):
  1. Explicit argument: `init_cecelia!("~/cecelia-pineapple/dev")`
  2. `CECELIA_DEV_DIR` environment variable
  3. `CECELIA_DEV_DIR` in a `.env` file at the project root (`cecelia-pineapple/.env`)
  4. Default: `~/cecelia`

Mirrors R's `cciaUse(path)` pattern.
"""
function init_cecelia!(dev_dir::Union{String,Nothing} = nothing)
    dotenv = _read_dotenv(_DOTENV_PATH)
    resolved = if !isnothing(dev_dir)
        expanduser(dev_dir)
    elseif haskey(ENV, "CECELIA_DEV_DIR")
        expanduser(ENV["CECELIA_DEV_DIR"])
    elseif haskey(dotenv, "CECELIA_DEV_DIR")
        expanduser(dotenv["CECELIA_DEV_DIR"])
    else
        expanduser("~/cecelia")
    end

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

projects_dir()::String = _cfg_dir("projects", "/path/to/projects")

# Resolve the bioformats2raw launcher: explicit config override → bundled copy (vendored into
# release bundles; Java comes from the Pixi env) → PATH → the (likely-missing) default. Run via
# `pixi run` so the bundled `bioformats2raw` script finds `java`. See docs/SHIPPING.md.
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

tasks_concurrent_limit()::Int =
    Int(get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "concurrentLimit", 4))
