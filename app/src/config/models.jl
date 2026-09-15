# Model vaults (custom-checkpoint stores) — cellpose + coastal + denoise. Split out of config.jl.
# Names live in the top-level `Cecelia` module; included from config.jl after the bootstrap.

# ── User-supplied model checkpoints ────────────────────────────────────────────
# Custom DL checkpoints live under `<config_dir>/models/{family}/{name}` (mirroring the old R
# version's `cciaModels()` layout). Cellpose's `cellposeModels/` subfolder holds `.pt` / no-ext
# files that cellpose's `CellposeModel(pretrained_model=path)` can load. The Julia handler resolves
# a user-selected model NAME to its FILE PATH before calling the Python runner, so the runner's
# `os.path.isfile` branch (`cellpose_utils.py`) picks the custom path up automatically.
#
# The checkpoint must be a **Cellpose 4** file. Cellpose 4 rejects a v3 checkpoint outright
# (`ValueError: This model does not appear to be a CP4 model`), which is why the bundled v3
# `ccia.fluo` was dropped in the v4 migration — see docs/todo/CELLPOSE_V4_PLAN.md. The slot itself
# is unchanged and takes v4 fine-tunes. See docs/SEGMENTATION.md → *Custom cellpose checkpoints*.

"""Absolute directory for user cellpose checkpoints. Just a path — no I/O, no side-effects."""
cellpose_models_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "models", "cellposeModels")

"""
    cellpose_model_path(name) -> String | Nothing

Absolute path to a custom cellpose checkpoint by filename, or `nothing` if the file doesn't
exist. Empty/whitespace name → `nothing` (no false positive on directory-only entries).

Two locations are checked, in order — mirrors `bioformats2raw_bin()`'s **explicit override →
bundled** shape: a user's config-dir drop-in takes precedence over the bundled copy of the same
filename. That's what lets someone shadow a bundled checkpoint with a fine-tuned version without
touching the repo/install. Nothing is bundled since the v4 migration, so in practice slot 1 is the
only one that hits — slot 2 stays because a distribution may still ship checkpoints.

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
    # `@__DIR__` = `<repo>/app/src/config` → `..`/`..`/`..` = repo (install) root.
    bundled = joinpath(@__DIR__, "..", "..", "..", "models", "cellposeModels", s)
    isfile(bundled) ? bundled : nothing
end

# Cellpose's built-in model names, and the labels the picker shows for them. Enumerated separately
# from filesystem checkpoints so the picker always offers them even before any checkpoint file is
# installed.
#
# **This is the one copy.** `cellpose.jl` reads it for the builtin-vs-custom fork rather than
# keeping a second tuple (it did, and the two could drift). Cellpose 4 has one architecture and
# publishes its weights on HuggingFace; `cpsam_v2` is the current default, `cpsam` the v1 release,
# kept so a run recorded against it stays reproducible. `cpdino*` is deliberately absent — it needs
# `dinov3` from git, which we do not ship.
#
# Fourth field is `backend :: Symbol` — `:v4` for the default env (`cellpose>=4.2`), `:v3` for the
# opt-in Mac-only `cellpose-v3` env. See docs/todo/CELLPOSE_V3_OPTIN_PLAN.md.
const BUILTIN_CELLPOSE_MODELS = (
    ("cpsam_v2", "Cellpose-SAM v2",       :v4),
    ("cpsam",    "Cellpose-SAM v1",       :v4),
    ("cyto3",    "cyto3 (cellpose 3)",    :v3),
    ("cyto2",    "cyto2 (cellpose 3)",    :v3),
)

# Cellpose 3 checkpoint filenames that never got a v4 equivalent AND that our v3 env doesn't ship
# either. `cyto`/`nuclei` were the pre-v3 zoo; the `*torch_0` suffixes are v3-era model filenames.
# Kept as an explicit reject list because v4 doesn't error on them — it logs a warning and silently
# loads `cpsam_v2`, so a saved run under one of these names would return a different segmentation
# with nothing in the log. `cyto2`/`cyto3` are NO LONGER retired — they route to the v3 env now.
# See `cellpose_models_for_python` in tasks/segment/cellpose.jl.
const RETIRED_CELLPOSE_MODELS = ("cyto", "nuclei",
                                 "cyto3torch_0", "cyto2torch_0", "cytotorch_0", "nucleitorch_0")

"""
    list_cellpose_models() -> Vector{NamedTuple}

Every cellpose model the picker should offer: the built-ins, then any filenames present
in the bundled `<install>/models/cellposeModels/` and the user drop-in
`<config_dir>/models/cellposeModels/`. Deduped by name; a user drop-in shadows a bundled file
of the same name (matches the resolver's precedence). Each entry is `(name, label, source)`,
where `source ∈ {"builtin", "bundled", "user"}` and `label` is what the picker displays.

This is the enumeration the `/api/tasks/definitions` route uses to REPLACE the static options
list in `cellpose.json`'s Model select, so a user's newly-dropped checkpoint appears without a
rebuild. See `docs/SEGMENTATION.md` → *Custom cellpose checkpoints*.
"""
# Is the opt-in `cellpose-v3` pixi env installed? Presence of its python interpreter is the
# only reliable signal — an empty directory can be left by a half-cancelled install. Mirrors
# `_env_installed` in `api/src/system_api.jl` (kept local because config.jl is loaded before the
# api layer). Called per `/api/tasks/definitions` request; that's a stat() on a known path — cheap.
function _cellpose_v3_env_installed()::Bool
    root = abspath(joinpath(@__DIR__, "..", "..", ".."))   # app/src/config → repo root
    isfile(joinpath(root, ".pixi", "envs", "cellpose-v3",
                    Sys.iswindows() ? "python.exe" : joinpath("bin", "python")))
end

function list_cellpose_models(dev_dir::Union{String,Nothing} = nothing)::Vector{NamedTuple}
    out = NamedTuple[]
    v3_ok = _cellpose_v3_env_installed()
    for (m, label, backend) in BUILTIN_CELLPOSE_MODELS
        # Hide v3 built-ins from the picker when the v3 env isn't installed — a user picking one
        # of those and hitting Run would only get `run_py`'s missing-env error, which is worse
        # than not showing them at all. The InlineNote advisor on this dropdown still fires with
        # an Install button, so discoverability is preserved. When the env lands, the next
        # `/api/tasks/definitions` request re-runs this filter and the two names appear.
        (backend === :v3 && !v3_ok) && continue
        push!(out, (name = m, stem = vault_model_stem(m), label = label,
                    source = "builtin", backend = backend))
    end
    seen = Set{String}(String(m.name) for m in out)
    user_dir    = cellpose_models_dir(dev_dir)
    bundled_dir = joinpath(@__DIR__, "..", "..", "..", "models", "cellposeModels")
    for (dir, tag) in ((user_dir, "user"), (bundled_dir, "bundled"))
        isdir(dir) || continue
        for name in sort!(readdir(dir))
            startswith(name, ".") && continue
            isfile(joinpath(dir, name)) || continue
            name in seen && continue
            # Custom checkpoints are treated as v4 by default (a user-dropped file for the v3 env is
            # not on the current drop path — those two model names are the built-in cyto2/cyto3).
            push!(out, (name = name, stem = vault_model_stem(name),
                        label = "$(name) ($(tag))", source = tag, backend = :v4))
            push!(seen, name)
        end
    end
    out
end

"""
    cellpose_model_backend(name) -> Symbol

`:v3` if `name` is a built-in v3 model, `:v4` otherwise. Custom checkpoints are treated as v4
(they load through `CellposeModel(pretrained_model=<path>)` and cellpose 4 rejects a v3 file up
front — see `cellpose_models_for_python`). Case-sensitive; the picker's `optionsFrom` builds from
`list_cellpose_models`, which uses the canonical spelling.
"""
function cellpose_model_backend(name::AbstractString)::Symbol
    s = String(name)
    for (m, _, backend) in BUILTIN_CELLPOSE_MODELS
        m == s && return backend
    end
    :v4
end

# ── Model-vault helpers (shared by coastal and denoise) ───────────────────────
# One generic lookup used by every user vault (`.pt` + sibling `.json`). Pickers may hand us a bare
# stem OR a full filename — `flowModels` sends stems, `coastalModels` sends `<name>.pt`, and #828
# flipped `denoiseModels` from full to stem. Accepting BOTH is what stops that shape flip from
# silently breaking segmentation again; before this, `denoise_model_path("supp.MERTK")` returned
# `nothing` because the file on disk is `supp.MERTK.pt`.
#
# Absolute paths pass through (a REPL/test caller can point at a checkpoint outside the vault).

# Strip ONLY the `.pt` suffix — `splitext` would split at every internal dot, collapsing e.g. a
# perChannel bundle folder named `supp.small` to `supp` and losing the picker → resolver round-trip
# (2026-09-08 report). Bundle names have no extension and pass through unchanged. The `api` layer's
# `vault_model_stem` in `api/src/vault_api.jl` is the identical helper — that module isn't in this
# Julia module's namespace, so a two-line duplicate on the app side is cheaper than a cross-module
# dep just for a stem strip. Same rule in both; do NOT diverge them.
vault_model_stem(name::AbstractString) =
    endswith(String(name), ".pt") ? String(name)[1:end-3] : String(name)

function vault_model_path(dir::AbstractString, name::AbstractString)::Union{String,Nothing}
    s = strip(String(name))
    isempty(s) && return nothing
    isabspath(s) && isfile(s) && return s
    for p in (joinpath(dir, s), joinpath(dir, "$(s).pt"))
        isfile(p) && return p
    end
    nothing
end

function vault_model_manifest(dir::AbstractString, name::AbstractString)::Dict{String,Any}
    path = vault_model_path(dir, name)
    isnothing(path) && return Dict{String,Any}()
    sidecar = string(first(splitext(path)), ".json")
    isfile(sidecar) || return Dict{String,Any}()
    try
        # A corrupt manifest must not take the picker down with it: the model still lists, and the
        # runner decides what to do without one (coastal falls back to defaults; SUPPORT errors).
        Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(sidecar, String)))
    catch
        Dict{String,Any}()
    end
end

# ── Coastal (optical-flow) models ──────────────────────────────────────────────
# The same drop-in vault as cellpose above, one directory over: `<config_dir>/models/coastalModels/`.
# It is deliberately NOT a per-project store — a model trained on one movie is meant to be applied
# across projects (*"in config like the cellpose vault. to use it across projects"*).
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

Absolute path to a coastal checkpoint. Accepts a bare stem or a `<name>.pt`; unlike
[`cellpose_model_path`](@ref) there is no bundled fallback (coastal ships no models).
"""
coastal_model_path(name::AbstractString,
                   dev_dir::Union{String,Nothing} = nothing)::Union{String,Nothing} =
    vault_model_path(coastal_models_dir(dev_dir), name)

"""
    coastal_model_manifest(name) -> Dict{String,Any}

The `<name>.json` sidecar beside a checkpoint, or an empty Dict when there is none (a hand-dropped
`.pt`). Parsed here rather than in Python because the picker label and `list_coastal_models` need
it, and because the vault manager shows it without loading torch.
"""
coastal_model_manifest(name::AbstractString,
                       dev_dir::Union{String,Nothing} = nothing)::Dict{String,Any} =
    vault_model_manifest(coastal_models_dir(dev_dir), name)

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
        # trained on. Kept to a phrase — see docs/ui/COPY.md.
        ch = get(manifest, "channelName", nothing)
        stem = vault_model_stem(name)
        label = isnothing(ch) || isempty(string(ch)) ? stem : "$(stem) ($(ch))"
        push!(out, (name = name, stem = stem, label = label, source = "user", manifest = manifest))
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
    String[m.stem for m in list_coastal_models(dev_dir)]

# ── Denoise (SUPPORT) models ───────────────────────────────────────────────────
# The same drop-in vault as coastal, one directory over: `<config_dir>/models/denoiseModels/`.
# **Per-kind directory, NOT a shared `models/`** — DENOISE_INTEGRATION_PLAN.md D2. Each kind carries
# its own manifest schema (denoise records `arch.inputFrames` / `training.framesPerImage`, coastal
# records `metricSet` / `temporalScales`); mixing them behind a `kind` field would force branchy
# consumers and couple schema evolution across unrelated engines.
#
# A model is a PAIR: `<name>.pt` (torch weights) plus `<name>.json` (the manifest that the runner
# needs to reconstruct the exact `SUPPORT(...)` call — `mid_channels`, `depth`, `blind_conv_channels`,
# `input_frames`). SUPPORT does not encode its own architecture in the checkpoint; inference with the
# wrong shape errors on the first `load_state_dict`. The manifest is not documentation.
#
# The vault ships nothing bundled — a model has to be trained on a real acquisition (Phase B). See
# `docs/todo/DENOISE_INTEGRATION_PLAN.md`.

"""Absolute directory for user denoise models. Just a path — no I/O, no side-effects."""
denoise_models_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "models", "denoiseModels")

"""
    denoise_model_path(name) -> String | Nothing

Absolute path to a denoise checkpoint. Accepts a bare stem or a `<name>.pt`; no bundled fallback.
"""
denoise_model_path(name::AbstractString,
                   dev_dir::Union{String,Nothing} = nothing)::Union{String,Nothing} =
    vault_model_path(denoise_models_dir(dev_dir), name)

"""
    denoise_model_manifest(name) -> Dict{String,Any}

The `<name>.json` sidecar beside a denoise checkpoint, or an empty Dict when there is none. Parsed
here rather than in Python because the picker label and `list_denoise_models` need it, and because
the vault manager shows it without loading torch.
"""
denoise_model_manifest(name::AbstractString,
                       dev_dir::Union{String,Nothing} = nothing)::Dict{String,Any} =
    vault_model_manifest(denoise_models_dir(dev_dir), name)

"""
    list_denoise_models() -> Vector{NamedTuple}

Every denoise model in `<config_dir>/models/denoiseModels/`, as `(name, label, source, manifest)`.
Only `.pt` files count — the `.json` manifests sit beside them and are not separate entries.

The label carries the acquisition the model was trained on (channel + set) — the one thing that
decides whether a model fits an image (a SUPPORT model trained on mem-TOM at 45 t frame rate is not
what an SHG channel needs). Same shape as `list_coastal_models`.
"""
function list_denoise_models(dev_dir::Union{String,Nothing} = nothing)::Vector{NamedTuple}
    out = NamedTuple[]
    dir = denoise_models_dir(dev_dir)
    isdir(dir) || return out
    for name in sort!(readdir(dir))
        startswith(name, ".") && continue
        full = joinpath(dir, name)

        # A denoise model is EITHER a pooled `.pt` file OR a perChannel bundle folder
        # (SUPPORT_PERCHANNEL_PLAN.md → D2). `.name` is the ON-DISK name in both cases —
        # `<stem>.pt` for pooled, `<stem>` for a bundle folder — so every consumer that does
        # `joinpath(dir, m.name)` still finds the right thing without a kind-aware branch.
        # `.kind` is exposed so consumers that DO need to differentiate (delete, rename, size)
        # do not have to re-inspect the disk.
        stem, mode, manifest = if isfile(full) && endswith(name, ".pt")
            (vault_model_stem(name), :pooled, denoise_model_manifest(name, dev_dir))
        elseif isdir(full)
            resolved = denoise_model_resolve(name, dev_dir)
            isnothing(resolved) && continue
            resolved.kind === :perChannel || continue
            (name, :perChannel, resolved.manifest)
        else
            continue
        end

        chs = get(manifest, "channels", nothing)
        joined = chs isa AbstractVector && !isempty(chs) ?
                 join((string(c) for c in chs), "+") : nothing
        # Only label the exception (perChannel bundle). Pooled is the default — surfacing "pooled"
        # on every picker entry is noise until the perChannel case exists.
        label = if mode === :perChannel
            isnothing(joined) ? "$(stem) (per-channel)" : "$(stem) ($(joined), per-channel)"
        else
            isnothing(joined) ? stem : "$(stem) ($(joined))"
        end
        push!(out, (name = name, stem = stem, label = label, source = "user",
                    manifest = manifest, kind = mode))
    end
    out
end

"""
    denoise_model_names(dev_dir = nothing) -> Vector{String}

The denoise model names already in the vault, as stems — the value the training task's `modelName`
field holds. Built on `list_denoise_models` so there is one enumeration of the vault. Uses
`vault_model_stem` so a bundle folder with an internal dot (e.g. `supp.small`) round-trips through
the picker instead of collapsing to `supp`.
"""
denoise_model_names(dev_dir::Union{String,Nothing} = nothing)::Vector{String} =
    String[m.stem for m in list_denoise_models(dev_dir)]

"""
    denoise_model_target(name; overwrite, want_bundle=false) -> String | (String, String)

Absolute `.pt` path in the denoise vault for a new model, after checking the name is a plain
filename and that nothing is being clobbered. Creates the vault directory. Mirror of
[`flow_model_target`](@ref) — same guards, different vault.

`want_bundle = true` returns `(pt_path, bundle_dir)` — both target paths a SUPPORT training run may
write to. The pooled path is `<name>.pt`; the perChannel bundle path is `<name>/`. The overwrite
check refuses if EITHER exists (unless overwrite=true). With overwrite=true, BOTH sibling shapes
are cleared before the run — one name is one model, so a pooled → perChannel retrain (or vice
versa) does not leave the old shape orphaned next to the new one. See SUPPORT_PERCHANNEL_PLAN.md D2.
"""
function denoise_model_target(name::AbstractString; overwrite::Bool = false,
                              want_bundle::Bool = false,
                              dev_dir::Union{String,Nothing} = nothing)
    stem = strip(String(name))
    isempty(stem) && error("Give the model a name — it is how you will pick it in the denoiser.")
    occursin(r"[/\\]", stem) && error("Model name cannot contain a path separator: '$stem'")
    stem in (".", "..") && error("Model name cannot be '$stem'")
    endswith(stem, ".pt") && (stem = first(splitext(stem)))

    dir = denoise_models_dir(dev_dir)
    mkpath(dir)
    pt_target     = joinpath(dir, "$(stem).pt")
    json_target   = joinpath(dir, "$(stem).json")
    bundle_target = joinpath(dir, stem)
    if !overwrite
        isfile(pt_target) && error(
            "A model named '$stem' already exists. Choose another name, or tick Overwrite existing.")
        isdir(bundle_target) && error(
            "A per-channel bundle named '$stem' already exists. Choose another name, or tick Overwrite existing.")
    else
        # Clear BOTH shapes at this stem — a pooled `<stem>.pt` sibling of a perChannel `<stem>/` (or
        # vice versa) shows up twice in the picker and is the state a `pooled → perChannel` retrain
        # would otherwise leave behind. Same reflex as `vault_delete`.
        isfile(pt_target)     && rm(pt_target)
        isfile(json_target)   && rm(json_target)
        isdir(bundle_target)  && rm(bundle_target; recursive = true)
    end
    want_bundle ? (pt_target, bundle_target) : pt_target
end

"""
    denoise_model_resolve(name) -> NamedTuple | Nothing

Resolve a picker's `<name>` into a concrete model on disk — either a pooled `.pt` OR a perChannel
bundle folder. Returns a NamedTuple `(kind, rootPath, manifest, perChannel)`:

  * `kind = :pooled`     — `rootPath` is the `.pt`, `manifest` is its `<name>.json` sidecar,
                           `perChannel` is empty.
  * `kind = :perChannel` — `rootPath` is the bundle directory, `manifest` is its top-level
                           `manifest.json`, `perChannel` is `Dict("<channelName>" =>
                           (ptPath, subManifest))` — one per trained channel.
  * `nothing`            — no such model in the vault.

Bundle takes precedence over a file of the same name (they cannot both exist because
[`denoise_model_target`](@ref)'s overwrite guard refuses either colliding shape).
See SUPPORT_PERCHANNEL_PLAN.md → D3.
"""
function denoise_model_resolve(name::AbstractString,
                               dev_dir::Union{String,Nothing} = nothing)
    s = strip(String(name))
    isempty(s) && return nothing
    dir = denoise_models_dir(dev_dir)

    # Bundle first: a directory in the vault (or an absolute directory path from a REPL caller).
    # A bundle MUST carry `manifest.json` with `mode:"perChannel"` — the trainer writes both; a
    # stray directory without that marker is not a bundle and must not shadow a pooled `.pt` of
    # the same name.
    for candidate in (isabspath(s) ? [s] : [joinpath(dir, s)])
        isdir(candidate) || continue
        top_manifest_path = joinpath(candidate, "manifest.json")
        isfile(top_manifest_path) || continue
        top_manifest = try
            Dict{String,Any}(String(k) => v for (k, v) in
                JSON3.read(read(top_manifest_path, String)))
        catch
            continue
        end
        string(get(top_manifest, "mode", "")) == "perChannel" || continue
        per_ch = Dict{String,Any}()
        entries = get(top_manifest, "perChannel", Any[])
        if entries isa AbstractVector
            for e in entries
                e isa AbstractDict || continue
                ch_name = string(get(e, :name, get(e, "name", "")))
                slug    = string(get(e, :slug, get(e, "slug", "")))
                pt_rel  = string(get(e, :pt,   get(e, "pt",   isempty(slug) ? "" : "$(slug).pt")))
                isempty(ch_name) && continue
                sub_pt  = joinpath(candidate, pt_rel)
                isfile(sub_pt) || continue
                sub_json = string(first(splitext(sub_pt)), ".json")
                sub_manifest = if isfile(sub_json)
                    try
                        Dict{String,Any}(String(k) => v for (k, v) in
                            JSON3.read(read(sub_json, String)))
                    catch
                        Dict{String,Any}()
                    end
                else
                    Dict{String,Any}()
                end
                per_ch[ch_name] = (ptPath = sub_pt, manifest = sub_manifest)
            end
        end
        return (kind = :perChannel, rootPath = candidate,
                manifest = top_manifest, perChannel = per_ch)
    end

    # Fall through to the pooled `.pt` — unchanged behaviour.
    pt = vault_model_path(dir, s)
    isnothing(pt) && return nothing
    return (kind = :pooled, rootPath = pt,
            manifest = vault_model_manifest(dir, s),
            perChannel = Dict{String,Any}())
end

"""
    flow_model_filename(stem) -> String

The vault FILENAME for a model stem — i.e. what a consumer's `model` select carries, built from the
stem `opticalFlow.train`'s `modelName` holds. The inverse of [`flow_model_names`](@ref).

Exists because those two spellings meet whenever one chain node trains a model and a later node
segments with it, and appending `.pt` at each such site is how they drift apart. Idempotent, so it is
safe on a value that is already a filename.
"""
flow_model_filename(stem::AbstractString)::String =
    endswith(stem, ".pt") ? String(stem) : "$(stem).pt"
