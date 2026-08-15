import TOML
# config.jl is included before utils.jl (which also imports it), so declare it here rather than
# relying on a later include having bound the name by the time a body first runs.
import JSON3

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

# ── User-supplied model checkpoints ────────────────────────────────────────────
# Custom DL checkpoints live under `<config_dir>/models/{family}/{name}` (mirroring the old R
# version's `cciaModels()` layout). Cellpose's `cellposeModels/` subfolder holds `.pt` / no-ext
# files that cellpose's `CellposeModel(pretrained_model=path)` can load — e.g. `ccia.fluo`, the
# custom fluorescence model that segments dendritic / SHG stroma (upstream of the branching task).
# The Julia handler resolves a user-selected model NAME to its FILE PATH before calling the
# Python runner, so cellpose's own `os.path.isfile(model_type)` branch (`cellpose_utils.py`) picks
# the custom path up automatically. See docs/SEGMENTATION.md → *Custom cellpose checkpoints*.

"""Absolute directory for user cellpose checkpoints. Just a path — no I/O, no side-effects."""
cellpose_models_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "models", "cellposeModels")

"""
    cellpose_model_path(name) -> String | Nothing

Absolute path to a custom cellpose checkpoint by filename, or `nothing` if the file doesn't
exist. Empty/whitespace name → `nothing` (no false positive on directory-only entries).

Two locations are checked, in order — mirrors `bioformats2raw_bin()`'s **explicit override →
bundled** shape: a user's config-dir drop-in takes precedence over the bundled copy of the same
filename. That's what lets someone replace `ccia.fluo` with a fine-tuned version without
touching the repo/install.

  1. `<config_dir>/models/cellposeModels/{name}` — user drop-in slot. Same convention as
     custom modules under `<config_dir>/modules/` (see `docs/CUSTOM_MODULES.md`): the file
     appears in the cellpose task's Model picker without a rebuild.
  2. `<install root>/models/cellposeModels/{name}` — the bundled set, populated by
     `install.sh` / `install.ps1` / `pixi run models-fetch` from
     `schienstockd/ceceliaModels`.
"""
function cellpose_model_path(name::AbstractString,
                             dev_dir::Union{String,Nothing} = nothing)::Union{String,Nothing}
    s = strip(String(name))
    isempty(s) && return nothing
    user = joinpath(cellpose_models_dir(dev_dir), s)
    isfile(user) && return user
    # `@__DIR__` = `<repo>/app/src` → `..`/`..` = repo (install) root.
    bundled = joinpath(@__DIR__, "..", "..", "models", "cellposeModels", s)
    isfile(bundled) ? bundled : nothing
end

# Cellpose's built-in model names (documented in the cellpose 3 CLI + Python API). Enumerated
# separately from filesystem checkpoints so the picker always offers them even before any
# checkpoint file is installed.
const _BUILTIN_CELLPOSE_MODELS = ("cyto3", "cyto2", "cyto", "nuclei")

"""
    list_cellpose_models() -> Vector{NamedTuple}

Every cellpose model the picker should offer: the four built-ins, then any filenames present
in the bundled `<install>/models/cellposeModels/` and the user drop-in
`<config_dir>/models/cellposeModels/`. Deduped by name; a user drop-in shadows a bundled file
of the same name (matches the resolver's precedence). Each entry is `(name, label, source)`,
where `source ∈ {"builtin", "bundled", "user"}` and `label` is what the picker displays.

This is the enumeration the `/api/tasks/definitions` route uses to REPLACE the static options
list in `cellpose.json`'s Model select, so a user's newly-dropped checkpoint appears without a
rebuild. See `docs/SEGMENTATION.md` → *Custom cellpose checkpoints*.
"""
function list_cellpose_models(dev_dir::Union{String,Nothing} = nothing)::Vector{NamedTuple}
    out = NamedTuple[]
    for m in _BUILTIN_CELLPOSE_MODELS
        push!(out, (name = m, label = uppercasefirst(m), source = "builtin"))
    end
    seen = Set{String}(String(m.name) for m in out)
    # user drop-ins first so they shadow bundled files of the same name (matches resolver order)
    user_dir    = cellpose_models_dir(dev_dir)
    bundled_dir = joinpath(@__DIR__, "..", "..", "models", "cellposeModels")
    for (dir, tag) in ((user_dir, "user"), (bundled_dir, "bundled"))
        isdir(dir) || continue
        for name in sort!(readdir(dir))
            startswith(name, ".") && continue
            isfile(joinpath(dir, name)) || continue
            name in seen && continue
            push!(out, (name = name, label = "$(name) ($(tag))", source = tag))
            push!(seen, name)
        end
    end
    out
end

# ── Coastal (optical-flow) models ──────────────────────────────────────────────
# The same drop-in vault as cellpose above, one directory over: `<config_dir>/models/coastalModels/`.
# It is deliberately NOT a per-project store — a model trained on one movie is meant to be applied
# across projects (Dominik: *"in config like the cellpose vault. to use it across projects"*).
#
# Two differences from cellpose, both consequences of coastal having no built-in models:
#   * there is nothing bundled and nothing built in, so an empty vault means an empty picker — the
#     user must train a model on the Optical Flow page first;
#   * a model is a PAIR: `<name>.pt` plus a `<name>.json` manifest recording the metric set,
#     temporal scales, cumulative window, source image and channel. Inference must use the metric
#     set the model was trained on, and coastal fails SILENTLY when it does not (channels shift and
#     the zero-fill lands at the end), so the manifest is not documentation — it is what
#     `CoastalUtils` configures itself from.
#
# Consequence to accept knowingly: config-dir models do not travel with a `.ccbundle` export, so a
# shared project references a model the recipient does not have. The task fails loudly on a missing
# model rather than falling back to an untrained one.
# See `docs/todo/COASTAL_SEGMENTATION_PLAN.md` decision 7.

"""Absolute directory for user coastal models. Just a path — no I/O, no side-effects."""
coastal_models_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "models", "coastalModels")

"""
    coastal_model_path(name) -> String | Nothing

Absolute path to a coastal checkpoint by filename, or `nothing` if it doesn't exist. Unlike
[`cellpose_model_path`](@ref) there is no bundled fallback: coastal ships no models, so the user
vault is the only location.
"""
function coastal_model_path(name::AbstractString,
                            dev_dir::Union{String,Nothing} = nothing)::Union{String,Nothing}
    s = strip(String(name))
    isempty(s) && return nothing
    isabspath(s) && isfile(s) && return s
    p = joinpath(coastal_models_dir(dev_dir), s)
    isfile(p) ? p : nothing
end

"""
    coastal_model_manifest(name) -> Dict{String,Any}

The `<name>.json` sidecar beside a checkpoint, or an empty Dict when there is none (a hand-dropped
`.pt`). Parsed here rather than in Python because the picker label and `list_coastal_models` need
it, and because the vault manager shows it without loading torch.
"""
function coastal_model_manifest(name::AbstractString,
                                dev_dir::Union{String,Nothing} = nothing)::Dict{String,Any}
    path = coastal_model_path(name, dev_dir)
    isnothing(path) && return Dict{String,Any}()
    sidecar = string(first(splitext(path)), ".json")
    isfile(sidecar) || return Dict{String,Any}()
    try
        Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(sidecar, String)))
    catch
        # A corrupt manifest must not take the picker down with it; the model still lists, and
        # `CoastalUtils` falls back to coastal's training defaults (and says so).
        Dict{String,Any}()
    end
end

"""
    list_coastal_models() -> Vector{NamedTuple}

Every coastal model in `<config_dir>/models/coastalModels/`, as `(name, label, source, manifest)`.
Only `.pt` files count — the `.json` manifests sit beside them and are not separate entries.

This is the enumeration `/api/tasks/definitions` uses to replace the (empty) static options list in
`coastal.json`'s Model select, and the same list the Optical Flow page's vault manager renders.
"""
function list_coastal_models(dev_dir::Union{String,Nothing} = nothing)::Vector{NamedTuple}
    out = NamedTuple[]
    dir = coastal_models_dir(dev_dir)
    isdir(dir) || return out
    for name in sort!(readdir(dir))
        startswith(name, ".") && continue
        last(splitext(name)) == ".pt" || continue
        isfile(joinpath(dir, name)) || continue
        manifest = coastal_model_manifest(name, dev_dir)
        # Label carries the one thing that decides whether a model fits an image: what it was
        # trained on. Kept to a phrase — see docs/UI.md → *UI copy*.
        ch = get(manifest, "channelName", nothing)
        stem = first(splitext(name))
        label = isnothing(ch) || isempty(string(ch)) ? stem : "$(stem) ($(ch))"
        push!(out, (name = name, label = label, source = "user", manifest = manifest))
    end
    out
end

"""
    flow_model_names(dev_dir = nothing) -> Vector{String}

The model names already in the vault, as the user TYPES them — stems, no `.pt`, because that is what
`flow_model_target` takes and what `opticalFlow.train`'s `modelName` field holds.

Built on `list_coastal_models` rather than listing the directory again: that is the one enumeration of
the vault, and it already knows to skip dotfiles and the `.json` manifests sitting beside each model.

The `models` namespace is the odd one out — **global**, not per image (VALUE_NAME_INPUT_PLAN → D6), so
this takes no image and its suggestions cannot ride the image payload.
"""
flow_model_names(dev_dir::Union{String,Nothing} = nothing)::Vector{String} =
    String[first(splitext(m.name)) for m in list_coastal_models(dev_dir)]

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

# Resolve the bioformats2raw launcher: explicit config override → the copy the installer fetched
# alongside the app (`<install>/bioformats2raw/`; ~190 MB, so downloaded at install time rather than
# shipped in the release bundle — Java comes from the Pixi env) → PATH → the (likely-missing)
# default. Run via `pixi run` so the `bioformats2raw` script finds `java`. See docs/SHIPPING.md.
function bioformats2raw_bin()::String
    exe = Sys.iswindows() ? "bioformats2raw.bat" : "bioformats2raw"
    d   = get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "bioformats2raw", "")
    if !isempty(string(d)) && string(d) != "/path/to/bioformats2raw"
        return joinpath(expand_user(string(d)), "bin", exe)
    end
    bundled = joinpath(@__DIR__, "..", "..", "bioformats2raw", "bin", exe)   # repo/install root
    isfile(bundled) && return bundled
    found = Sys.which(exe)
    found === nothing || return string(found)
    joinpath(_cfg_dir("bioformats2raw", "/path/to/bioformats2raw"), "bin", exe)
end

# The shipped `[dirs] python` value. Must match `app/config.toml` — it is the sentinel for "nobody
# chose this", the same role `_PROJECTS_DIR_PLACEHOLDER` plays for the projects dir.
const _PYTHON_BIN_DEFAULT = "python3"

# Interpreter names to try, in order. PURE and parameterised on `iswin` so BOTH platforms' behaviour is
# testable from any host. Windows conda/pixi envs ship `python.exe` and frequently no `python3` at all,
# so `python` must be tried there; on Unix `python3` is the unambiguous one.
#
# A name the user DELIBERATELY configured is the only candidate: resolve it to an absolute path if we
# can, but never silently substitute a different interpreter. Falling back would run tasks under an
# interpreter that lacks the analysis deps and report nothing about why — worse than failing on the
# name they asked for. The shipped default is not a deliberate choice, so it does get the fallbacks.
_python_bin_candidates(configured::AbstractString, iswin::Bool)::Vector{String} =
    let c = String(strip(String(configured)))
        (isempty(c) || c == _PYTHON_BIN_DEFAULT) ?
            (iswin ? String["python", "python3"] : String["python3", "python"]) :
            String[c]
    end

"""
    python_bin_path() -> String

The Python interpreter the engine's subprocesses run — **resolved to an absolute path** whenever it
can be found on `PATH`.

Absolute, not the bare `"python3"` it used to return, because the string escapes the activated
environment. `pixi run` puts the Pixi env first on `PATH`, so a bare name resolves correctly for
anything *Julia* spawns (`run_py`, the napari bridge) — but the observer's MCP spec registers this
value into the user's **own** Claude Code config, where it is launched from a plain shell with no Pixi
activation. There, a bare `python3` is the *system* python, which has neither `mcp` nor `websockets`,
so the observer's tools failed to start in exactly the sessions the one-click setup was meant to
enable. It also could not work on Windows at all, where `python3` frequently does not exist.

Resolution: an explicitly configured `dirs.python` **path** (anything with a directory component) is
used verbatim — the user has said precisely which interpreter. A bare *name* (including the shipped
default `"python3"`) is resolved through `PATH`, falling back to the platform's other spellings. If
nothing resolves, the configured/legacy bare name is returned unchanged, so behaviour never gets
worse than before.
"""
function python_bin_path()::String
    raw  = strip(string(get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "python", "")))
    conf = isempty(raw) ? "" : expand_user(String(raw))
    # An explicit PATH wins verbatim; a bare NAME falls through to resolution below.
    isempty(conf) || isempty(dirname(conf)) || return conf
    for cand in _python_bin_candidates(conf, Sys.iswindows())
        p = Sys.which(cand)
        isnothing(p) || return String(p)
    end
    isempty(conf) ? "python3" : conf
end

# Default for launching the napari bridge on the discrete GPU (hybrid-graphics machines). Reads
# `[napari].discreteGpu`; the api layer holds the runtime toggle (Settings) and seeds it from this.
napari_discrete_gpu()::Bool =
    Bool(get(get(cecelia_conf(), "napari", Dict{String,Any}()), "discreteGpu", false))

tasks_concurrent_limit()::Int =
    Int(get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "concurrentLimit", 4))

# ── The detached task runner (Settings → System) ──────────────────────────────────────────────────
#
# Whether tasks execute in a SEPARATE process, so restarting the backend does not kill work in flight.
# `[runner].enabled`, default **false** while the design has known gaps (no on-disk spool; chains and
# background jobs still run in-process — docs/todo/TASK_RUNNER_PLAN.md).
#
# A SETTING, not just an env var, because "opt-in" has to mean something to a user. It began as
# `CECELIA_RUNNER=1`, which exists only as a pixi task — a packaged install had no way to turn it on
# at all, so what was called opt-in was really "dev-only". The env var is kept as an OVERRIDE (it
# wins, and is how `pixi run dev-runner` and CI ask for it without touching a config file); the
# setting is what a user actually has.
_env_flag(name) = lowercase(strip(get(ENV, name, ""))) in ("1", "true", "yes", "on")

"""
    is_dev_session() -> Bool

Whether this is a development run (`pixi run dev` sets `CECELIA_DEV`; `prod`, `app.py` and the
packaged launcher never do). Mirrors the API layer's `_is_dev`, in the package so the runner and its
gating can read it too.
"""
is_dev_session()::Bool = _env_flag("CECELIA_DEV")

"""
    runner_enabled() -> Bool

**Dev only.** A production install has no Restart button, so the runner's whole benefit — a backend
restart not costing a running task — is unreachable there, while every one of its failure modes
(an idle process with no window, no cancel, nothing to find it by) lands squarely on the user. And a
prod user does not need it: they leave the app running, and closing the browser tab was never what
stopped a task.

So this is deliberately not a thing a user can switch on. The process side of "quit and keep
processing" (`detach = true`) is already built if that ever becomes something someone asks for —
see docs/todo/TASK_RUNNER_PLAN.md → Decision 3b — but it is not built on a guess.
"""
function runner_enabled()::Bool
    is_dev_session() || return false
    haskey(ENV, "CECELIA_RUNNER") && return _env_flag("CECELIA_RUNNER")
    Bool(get(get(cecelia_conf(), "runner", Dict{String,Any}()), "enabled", false))
end

"""
    set_runner_enabled!(on) -> Bool

Persist `[runner].enabled` and hot-reload config. Mirrors `set_image_compressor!` — merged write, so
an unrelated key in `custom.toml` survives.

Returns the value now in EFFECT, which is not always what was asked: `CECELIA_RUNNER` overrides the
file, so a dev session started with the env var reports `true` however the toggle is set. Returning
the effective value rather than the written one is what lets the UI show the truth instead of the
request.
"""
function set_runner_enabled!(on::Bool)::Bool
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    r   = get(cfg, "runner", Dict{String,Any}())
    r["enabled"] = on
    cfg["runner"] = r
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    runner_enabled()
end

# ── Image store compression ──────────────────────────────────────────────────────────────────────

"""
Compressor choices for an IMAGE store, mirroring `zarr_utils.IMAGE_COMPRESSOR_CHOICES`.

Julia needs its own copy for two reasons: bioformats2raw is handed flags on a command line (it cannot
read a Python constant), and the API serves the list to Settings. Kept in the SAME order as the Python
dict, and asserted equal by `app/test/suite.jl` → *"import metrics"* — same arrangement as the
calibration writers (CLAUDE.md → *Calibration — three copies, one stamp*). If you change one, that
test is the contract.

Each entry carries its MEASURED numbers as display strings (`size`/`write`/`read`), because Settings
renders them as a comparison table rather than a bare dropdown — the trade-off is the whole reason
there is a choice, so hiding it behind a name would make the control decorative. Serving them
pre-formatted keeps the numbers stated in exactly one place; the frontend never computes or restates
them.

Measured identically for every row: `4kS67f/LUkCpP` (64x4x13x512x512, 12-bit data in 16-bit words,
1.745 GB raw), whole store rewritten. `read` is a warm per-plane read, the only read difference that
survived three repeat runs — cold and sequential reads were inside run-to-run noise, because a smaller
store offsets the extra CPU. `url` is the codec's own site so a user can check what they're choosing.

**Held fixed while measuring:** the store LAYOUT — zarr v2, nested chunk keys (bioformats2raw's own
default, which is what wrote that store), 1x1x1x512x512 chunks, unsharded. It has to be stated because
the other table on that Settings page varies exactly those, and a size in one table is only comparable
to a size in the other if you know which layout each was taken in. Chunk shape is the part that could
actually move these numbers: the codec compresses one chunk at a time, so a different chunk size gives
it a different amount of context. The two variables are not independent, and neither table's rows were
re-measured across the other's — each varies one thing with the other pinned.
"""
const IMAGE_COMPRESSOR_CHOICES = [
    (name = "zstd-shuffle", cname = "zstd", clevel = 3, shuffle = true,
     label = "zstd + shuffle", size = "0.64 GB", ratio = "2.74x", write = "5.1 s", read = "1.7 ms",
     url = "https://facebook.github.io/zstd/"),
    (name = "zstd",         cname = "zstd", clevel = 3, shuffle = false,
     label = "zstd",          size = "0.77 GB", ratio = "2.27x", write = "5.0 s", read = "1.7 ms",
     url = "https://facebook.github.io/zstd/"),
    (name = "lz4-shuffle",  cname = "lz4",  clevel = 5, shuffle = true,
     label = "lz4 + shuffle", size = "0.94 GB", ratio = "1.85x", write = "4.6 s", read = "1.35 ms",
     url = "https://lz4.org/"),
    (name = "zstd-max",     cname = "zstd", clevel = 9, shuffle = true,
     label = "zstd level 9",  size = "0.60 GB", ratio = "2.90x", write = "36.9 s", read = "1.6 ms",
     url = "https://facebook.github.io/zstd/"),
]

#: What every row was measured on, shown as the table's caption. One line, no prose. Names the LAYOUT
#: it was measured in (see the docstring) — the layout table below states its codec, symmetrically, so
#: neither set of numbers is readable as "the" size without knowing what the other variable was.
const IMAGE_COMPRESSOR_MEASURED_ON =
    "1.7 GB 16-bit timecourse, whole store · zarr v2, nested keys, 512×512 chunks"

const IMAGE_COMPRESSOR_DEFAULT = "zstd-shuffle"

"""
    image_compressor() -> String

The configured image-store compressor choice (`[zarr].imageCompressor`), or the default when unset or
unrecognised. Falls back rather than erroring: a typo in `custom.toml` must not fail every write.

Label stores are deliberately not configurable — see `zarr_utils.LABEL_COMPRESSOR`.
"""
function image_compressor()::String
    name = string(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "imageCompressor",
                      IMAGE_COMPRESSOR_DEFAULT))
    any(c -> c.name == name, IMAGE_COMPRESSOR_CHOICES) ? name : IMAGE_COMPRESSOR_DEFAULT
end

"""
    set_image_compressor!(name) -> String

Persist `name` as `[zarr].imageCompressor` in the user's `custom.toml` and hot-reload config, so the
next task writes with it — no restart. Rejects an unknown name (the caller validates for the API's
400). Existing stores are untouched; `python/cecelia/utils/rechunk_zarr.py` re-lands them.
"""
function set_image_compressor!(name::AbstractString)::String
    nm = strip(String(name))
    any(c -> c.name == nm, IMAGE_COMPRESSOR_CHOICES) ||
        throw(ArgumentError("unknown compressor '$nm'"))
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    z   = get(cfg, "zarr", Dict{String,Any}())
    z["imageCompressor"] = nm
    cfg["zarr"] = z
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    nm
end

# ── Store LAYOUT defaults (Settings → Storage; the import pre-fills from these) ────────────────────
#
# The format and the chunk-key separator are decided PER IMAGE, at import — an existing v2 image cannot
# become v3 (no converter, ZARR_V3_PLAN D7) and derived stores inherit from their source (D9/D11). So
# unlike the compressor, which applies to whatever is written next, these are **defaults** that the
# import form pre-fills, not a switch that changes existing behaviour. `docs/todo/ZARR_V3_PLAN.md` D10.

#: Store LAYOUTS a new import can be written in, as the three VIABLE combinations of NGFF version and
#: chunk-key separator — not two independent controls. Flat keys and NGFF 0.5 cannot be combined
#: (bioformats2raw silently writes zarr v2 for that pair, verified in both flag orders), so presenting
#: them as one choice makes the impossible state unrepresentable instead of warning about it.
#:
#: Rendered as a TABLE in Settings, like the compressor, because the trade-off is the only reason there
#: is a choice. Every number is MEASURED on one source (`M3c-CD8-GFP-CD20-Tom_MAX.tif`, 2 pyramid
#: levels), with `read` the median of 9 interleaved full-level reads so drift hits each store equally.
#: All three decode to identical pixels.
#:
#: **Held fixed while measuring:** the CODEC — blosc/zstd level 3 + byte shuffle (`zstd-shuffle`, the
#: default of the compressor table above) with 1x1x1x512x512 chunks, verified identical in all three
#: stores' array metadata. Stated for the same reason that table names its layout: these are two
#: settings on one page and each table pins the other's variable, so `size` here is "this layout at the
#: default codec", not the size of the layout as such. A different codec shifts all three rows together
#: — the DIFFERENCE between them is a directory-inode cost and does not depend on the codec at all.
#:
#: **`flat` was removed (Dominik, 2026-08-14).** It was the default, on 10 MB of 81 MB (~14%) saved
#: directory inodes at identical read time. Two things undid it. First, re-measured on a real 3.5 GB
#: movie instead of this 81 MB fixture, the saving is **~171 MB, ~5%** — the fixture flattered it
#: because its data was small relative to its directory count. Second, and decisive: a flat store
#: **conforms to no published NGFF version.** Nested storage is what 0.2 introduced (ome-zarr-py
#: `FormatV02`, "Changelog: move to nested storage") and 0.3/0.4/0.5 inherit it, so flat keys are 0.1
#: storage carrying 0.4-shaped metadata that 0.1 does not define. The two writers each labelled a
#: different half and disagreed on disk — bioformats2raw stamped `0.1` for `--no-nested`, our own
#: writers stamped `0.4` for the identical layout. 5% of a movie does not buy a store no conforming
#: reader can name. Existing flat stores still READ (the separator is self-describing); nothing needs
#: re-importing, and derived stores of a legacy flat source now come out nested.
#:
#: `v3` is offered and NOT default: same size, ~40% slower to read (263 vs 188 ms, its whole range above
#: every other median). bioformats2raw ALWAYS shards v3, so this measures "v3 as we can actually produce
#: it"; the shard-index indirection is the likely cause. See docs/todo/ZARR_V3_PLAN.md.
const STORE_LAYOUT_CHOICES = [
    (name = "nested", ngffVersion = "0.4", chunkSeparator = "nested",
     label = "zarr v2 · nested keys", keys = "0/0/36/0/8", dirs = "2,474",
     size = "81.2 MB", read = "188 ms",
     detail = "NGFF 0.4, dimension_separator '/' — bioformats2raw's own default"),
    (name = "v3", ngffVersion = "0.5", chunkSeparator = "nested",
     label = "zarr v3 · sharded", keys = "c/0/0/36/0/8", dirs = "2,476",
     size = "81.2 MB", read = "263 ms",
     detail = "NGFF 0.5, zarr.json + sharding_indexed (--ngff-version 0.5)"),
]
const STORE_LAYOUT_DEFAULT = "nested"

#: What every row was measured on — shown as the table's caption, one line, like the compressor's, and
#: naming the codec for the same reason that one names the layout.
const STORE_LAYOUT_MEASURED_ON =
    "0.5 GB 16-bit stack, 2 levels, zstd + shuffle · read = median of 9 interleaved"

"""The configured store layout, as a `STORE_LAYOUT_CHOICES` entry.

Resolved from the two persisted `[zarr]` keys rather than a layout name, so a config written before
this table existed still resolves, and so the two remain independently settable from the API. An
unrecognised pair falls back to the default rather than erroring — this is read on the write path of a
long import."""
function store_layout()
    v, sep = ngff_version(), chunk_separator()
    i = findfirst(c -> c.ngffVersion == v && c.chunkSeparator == sep, STORE_LAYOUT_CHOICES)
    STORE_LAYOUT_CHOICES[isnothing(i) ? findfirst(c -> c.name == STORE_LAYOUT_DEFAULT, STORE_LAYOUT_CHOICES) : i]
end

ngff_version()::String =
    (v = String(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "ngffVersion", NGFF_VERSION_DEFAULT));
     v in ("0.4", "0.5") ? v : NGFF_VERSION_DEFAULT)

chunk_separator()::String =
    (v = String(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "chunkSeparator", CHUNK_SEPARATOR_DEFAULT));
     v == "nested" ? v : CHUNK_SEPARATOR_DEFAULT)   # "flat" is no longer offered; see CHUNK_SEPARATORS

#: Defaults for the two underlying keys — derived from the default LAYOUT so they cannot drift apart.
const NGFF_VERSION_DEFAULT = "0.4"
const CHUNK_SEPARATOR_DEFAULT = "nested"

"""Persist a whole LAYOUT by name (both keys at once) and hot-reload. Setting the pair together is what
keeps the impossible combination unreachable from the UI."""
function set_store_layout!(name::AbstractString)::String
    nm = strip(String(name))
    i  = findfirst(c -> c.name == nm, STORE_LAYOUT_CHOICES)
    isnothing(i) && throw(ArgumentError("unknown store layout '$nm'"))
    c = STORE_LAYOUT_CHOICES[i]
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    z   = get(cfg, "zarr", Dict{String,Any}())
    z["ngffVersion"]    = c.ngffVersion
    z["chunkSeparator"] = c.chunkSeparator
    cfg["zarr"] = z
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    c.name
end

"""
    bf2raw_shuffle_values(lib_dir) -> (on, off)

How THIS bioformats2raw spells the blosc `shuffle` property. Returns the value for byte-shuffle-on and
for shuffle-off.

bioformats2raw 0.12.0 swapped its zarr library (jzarr → zarr-java, upstream PR #302) and with it the
spelling of this one property. The two are mutually exclusive — each version **hard-fails** on the
other's, so the flag has to follow the binary:

| passed              | 0.11.x (jzarr) | 0.12.x (zarr-java)   |
|---------------------|----------------|----------------------|
| `shuffle=1`         | byte shuffle   | NullPointerException |
| `shuffle=0`         | no shuffle     | NullPointerException |
| `shuffle=shuffle`   | invalid option | byte shuffle         |
| `shuffle=noshuffle` | invalid option | no shuffle           |

**`byteshuffle` — the alias 0.12's README documents for byte shuffle — is BROKEN upstream.** It
reaches blosc-java as a null enum (`NullPointerException: Cannot read field "shuffle" because "x0" is
null`, in `Blosc.Shuffle.access\$000`) and every chunk write throws `ZarrException: Error in encoding
blosc`. `shuffle` is the spelling that works; measured against 0.12.1 for cname ∈
{lz4, zstd, zlib, blosclz} and both NGFF 0.4 and 0.5.

Detected from the BUNDLED ZARR LIBRARY, not by parsing `--version`: the library *is* the cause, and a
directory listing costs nothing where `--version` pays a JVM start on every import. An unrecognised
install gets the current spelling — a wrong guess fails loudly (non-zero exit, which `_run_task`
already checks), it cannot silently write the wrong codec.
"""
function bf2raw_shuffle_values(lib_dir::AbstractString)
    legacy = isdir(lib_dir) && any(startswith(f, "jzarr-") for f in readdir(lib_dir))
    legacy ? ("1", "0") : ("shuffle", "noshuffle")
end

# lib/ sits next to bin/ in every bioformats2raw distribution: <install>/bin/bioformats2raw + <install>/lib
_bf2raw_lib_dir(bin::AbstractString = bioformats2raw_bin()) = joinpath(dirname(dirname(bin)), "lib")

"""
    bf2raw_chunk_flags(value) -> Vector{String}

bioformats2raw `--tile-width`/`--tile-height` flags for the configured chunk size, or **empty for
`"auto"`**.

Auto deliberately passes NOTHING and lets bioformats2raw apply its own default of 1024 — because that
default is already *capped to the frame*: a 512×512 acquisition gets 512×512 chunks, a 1024×1024 one
gets 1024×1024. That is exactly the rule we want (one chunk per plane, up to 1024) and it needs no
knowledge of the source dimensions, which we do not have at this point anyway — the image has not been
converted yet, so there is no store to measure.

Why one chunk per plane is the target rather than something smaller: napari slices per (t,c,z) and
draws whole planes, so a plane that is one chunk is one read. The same reasoning is written down in
`zarr_utils.plane_chunks`, which chunks our OWN writes that way. Smaller chunks only pay off for
routine sub-region reads, which nothing in the app does — segmentation reads tiles that are at least
its own block size.

A 1024×1024 `uint16` chunk is 2 MB. 2048 is 8 MB, which is a lot to fetch for a viewport showing far
less; it is offered for the rare very large frame, not as an upgrade.

Anything unparseable falls back to auto rather than raising — the same call as
`bf2raw_compression_flags`: a typo must not fail an hour-long import.
"""
function bf2raw_chunk_flags(value)::Vector{String}
    s = lowercase(strip(string(value)))
    (isempty(s) || s == "auto") && return String[]
    n = tryparse(Int, s)
    (isnothing(n) || n < 32) && return String[]
    ["--tile-width", string(n), "--tile-height", string(n)]
end

"""
    bf2raw_format_flags(ngff_version, shard_size) -> Vector{String}

bioformats2raw `--ngff-version` / `--shard-*` flags. Empty for the default (`"0.4"`, no shard), so an
unchanged import produces the exact command it always did.

The import is the ONLY place the store format is chosen; every derived store inherits it from its
source (`docs/todo/ZARR_V3_PLAN.md` D9). That is what keeps this answerable — the user decides once
per image, not again on every correction, crop and label set.

Returns `(flags, conflict)` — see the conflict note below.

**Sharding applies only to NGFF 0.5** and is silently dropped for 0.4 rather than raising: the two are
separate controls in the UI, and a user who sets a shard size and then switches back to 0.4 should get
a working import, not an error.

**`--no-nested` and `--ngff-version 0.5` are mutually exclusive** — together, bioformats2raw silently
writes zarr **v2** (verified in both flag orders: the root carries `.zgroup`, not `zarr.json`). Flat
wins, because flat has a measured benefit (56x fewer directories) and 0.5 on its own currently buys
nothing. `conflict` is returned so the caller can say so instead of leaving the user to notice in the
metadata modal.

**There is no "off".** `--shard-width` defaults to 1024 and cannot be disabled, so bioformats2raw shards
EVERY v3 store — verified against 0.12.1: a 0.5 import with no shard flag still produces a
`sharding_indexed` codec. The control therefore sets the shard SIZE, and `"auto"` means "pass nothing,
take the 1024 default" (itself capped to the frame). An option claiming to turn sharding off would be a
lie, which is why there is not one.

Which size is *best* is unmeasured — Phase 4's job. A shard is one file holding many chunks, so writing
one chunk rewrites the whole shard: safe for an import (written once, sequentially) and potentially
expensive for anything filling a store incrementally (D8). `"auto"` defers to upstream rather than
naming a number chosen to look decisive.
"""
function bf2raw_format_flags(ngff_version, shard_size;
                            shard_depth = "1", z_planes::Int = 0)
    v     = strip(string(ngff_version))
    flags = String[]

    # `--no-nested` is never emitted: we write NESTED keys in every format (see CHUNK_SEPARATORS in
    # zarr_utils.py for why flat was dropped — it conforms to no NGFF version). That also retires the
    # old flat-vs-0.5 conflict this function used to report: `--no-nested` combined with
    # `--ngff-version 0.5` silently produced a zarr v2 store, which was the reason a `conflict` flag
    # existed at all. With flat gone the impossible state is unrepresentable rather than warned about.
    if !(isempty(v) || v == "0.4")
        append!(flags, ["--ngff-version", v])
    end
    # sharding is NGFF 0.5 only
    v == "0.5" || return flags

    sh = lowercase(strip(string(shard_size)))
    if !(isempty(sh) || sh == "auto")
        n = tryparse(Int, sh)
        (isnothing(n) || n < 32) || append!(flags, ["--shard-width", string(n), "--shard-height", string(n)])
    end
    # Shard DEPTH is the only axis that actually reduces the file count on a 512x512 frame — width and
    # height cap to the frame, so the shard equals the chunk and packs nothing. Measured: depth 13 gave
    # 13 files where the default gave 109. It is also the axis D8 warns about: a shard spanning z is
    # rewritten in full for every plane write, so this is opt-in and warned, never a default.
    d = lowercase(strip(string(shard_depth)))
    if d == "all"
        z_planes > 1 && append!(flags, ["--shard-depth", string(z_planes)])
    else
        n = tryparse(Int, d)
        (isnothing(n) || n <= 1) || append!(flags, ["--shard-depth", string(n)])
    end
    flags
end

"""
    bf2raw_compression_flags([name]) -> Vector{String}

bioformats2raw CLI flags that make it write the SAME compressor our own Python writers use.

bioformats2raw defaults to `blosc/lz4-5`, which on real 16-bit acquisition data is 33% larger than the
default choice for no read-speed benefit that survives measurement. The import is the one store we do
NOT write through `zarr_utils`, so it is the one that has to be told explicitly — otherwise an
imported original and every correction derived from it are encoded differently.

The `shuffle` value's spelling depends on the installed version — see `bf2raw_shuffle_values`.
"""
function bf2raw_compression_flags(name::AbstractString = image_compressor())::Vector{String}
    i = findfirst(c -> c.name == name, IMAGE_COMPRESSOR_CHOICES)
    c = IMAGE_COMPRESSOR_CHOICES[isnothing(i) ? 1 : i]
    shuf_on, shuf_off = bf2raw_shuffle_values(_bf2raw_lib_dir())
    ["--compression", "blosc",
     "--compression-properties", "cname=$(c.cname)",
     "--compression-properties", "clevel=$(c.clevel)",
     "--compression-properties", "shuffle=$(c.shuffle ? shuf_on : shuf_off)"]
end
