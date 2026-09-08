# ── Shared model-vault API primitives ──────────────────────────────────────────
# One implementation of the read/rename/delete surface that both `denoise_api.jl` and
# `optical_flow_api.jl` use — the two files were copy-paste siblings (`_flow_model_stem` = literal
# clone of `_denoise_model_stem`, etc.), and the SUPPORT perChannel change (bundle folders alongside
# pooled `.pt` files) would have forced a THIRD copy or forked them further. Consolidated 2026-09-07.
#
# Vault contents come in two shapes today, both handled here:
#   * pooled  — `<vault>/<name>.pt` + sibling `<vault>/<name>.json` manifest
#   * bundle  — `<vault>/<name>/` directory containing `<slug>.pt` + `<slug>.json` per channel and a
#               top-level `manifest.json`. Only denoise writes bundles today (perChannel mode,
#               SUPPORT_PERCHANNEL_PLAN.md → D2/D3); optical-flow only writes pooled. The helpers
#               below dispatch on `.kind` from the `list_*_models` row so if flow ever grows a
#               bundle case, no divergent re-implementation.
#
# Contract with the `list_*_models` producers (`config.jl`):
#   * `.name`     — the on-disk name inside the vault directory (filename OR bundle folder name).
#   * `.label`    — the picker label.
#   * `.manifest` — parsed sidecar (empty Dict when absent).
#   * `.kind`     — `:pooled` | `:perChannel`. Optional; defaults to `:pooled`.

using Dates

# `vault_model_stem` lives in Cecelia (`app/src/config.jl`, exported) — the twin copy that used
# to sit here was independently rediscovered as a bug when `denoise_model_names` collapsed
# `supp.small` to `supp`. Do NOT re-add it here.

"""A vault name that is provably a leaf inside the vault (a pooled `.pt` OR a bundle directory),
or `nothing`. Path separators and `..` still refused."""
function safe_vault_model_name(name)::Union{String,Nothing}
    s = String(strip(String(name)))
    (isempty(s) || occursin(r"[/\\]", s) || s in (".", "..")) && return nothing
    s
end

# Bundle size = the whole folder recursively; pooled size = `.pt` alone (the sidecar `.json` is a
# tiny detail the manager already shows via `hasManifest`, not worth adding to the bytes readout).
# Bundle mtime = the LATEST mtime under it — the top-level `manifest.json` lands last when the
# trainer finishes writing sub-models, so it approximates that anyway, but latest-of-tree is honest
# for a hand-edited bundle too. Cheap on a bundle of ≤4 files.
function vault_model_bytes(path::AbstractString, kind::Symbol)::Int
    kind === :perChannel || return isfile(path) ? Int(filesize(path)) : 0
    isdir(path) || return 0
    total = 0
    for (root, _, files) in walkdir(path), f in files
        total += filesize(joinpath(root, f))
    end
    total
end

function vault_model_mtime(path::AbstractString, kind::Symbol)::Float64
    kind === :perChannel || return isfile(path) ? mtime(path) : 0.0
    isdir(path) || return 0.0
    latest = 0.0
    for (root, _, files) in walkdir(path), f in files
        m = mtime(joinpath(root, f))
        m > latest && (latest = m)
    end
    latest
end

"""Augment a `list_*_models()` row with the fields the vault manager renders. Reads `m.stem` if the
list function already populated it (all three do today); falls back to `vault_model_stem(m.name)`
for a caller that hasn't been updated. Same rule, one place."""
function vault_model_row(dir::AbstractString, m::NamedTuple)
    kind = Symbol(get(m, :kind, :pooled))
    path = joinpath(dir, m.name)
    bytes = vault_model_bytes(path, kind)
    mt    = vault_model_mtime(path, kind)
    stem  = get(m, :stem, vault_model_stem(m.name))
    (; name = m.name, label = m.label, stem = stem, kind = String(kind),
       bytes = bytes,
       modified = mt > 0 ? Dates.format(Dates.unix2datetime(mt), "yyyy-mm-dd") : "",
       hasManifest = !isempty(m.manifest),
       manifest = m.manifest)
end

"""Rename either a pooled pair (`<from>.pt` + `<from>.json` → `<to>.pt` + `<to>.json`) or a bundle
folder (`<from>/` → `<to>/`). Returns `(status, body_json)`."""
function vault_rename(dir::AbstractString, from::AbstractString, to_raw::AbstractString)
    to_stem = String(strip(to_raw))
    (isempty(to_stem) || occursin(r"[/\\]", to_stem) || to_stem in (".", "..")) &&
        return 400, JSON3.write((; error = "New name cannot be empty or contain a path separator"))
    to_stem = vault_model_stem(to_stem)

    src = joinpath(dir, from)
    if isdir(src)
        dest = joinpath(dir, to_stem)
        src == dest && return 200, JSON3.write((; ok = true, name = to_stem))
        ispath(dest) && return 409, JSON3.write((;
            error = "A model named '$to_stem' already exists"))
        mv(src, dest)
        return 200, JSON3.write((; ok = true, name = to_stem))
    elseif isfile(src)
        dest = joinpath(dir, "$(to_stem).pt")
        src == dest && return 200, JSON3.write((; ok = true, name = basename(dest)))
        isfile(dest) && return 409, JSON3.write((;
            error = "A model named '$to_stem' already exists"))
        mv(src, dest)
        # The manifest moves with the weights or the pair breaks apart silently.
        src_manifest = joinpath(dir, "$(vault_model_stem(from)).json")
        isfile(src_manifest) && mv(src_manifest, joinpath(dir, "$(to_stem).json"); force = true)
        return 200, JSON3.write((; ok = true, name = basename(dest)))
    else
        return 404, JSON3.write((; error = "Model not found: $from"))
    end
end

"""Delete either a pooled pair (`.pt` + `.json`) or a bundle folder (recursively). Returns
`(status, body_json)`."""
function vault_delete(dir::AbstractString, name::AbstractString)
    path = joinpath(dir, name)
    if isdir(path)
        rm(path; recursive = true)
        return 200, JSON3.write((; ok = true))
    elseif isfile(path)
        rm(path)
        manifest = joinpath(dir, "$(vault_model_stem(name)).json")
        isfile(manifest) && rm(manifest)
        return 200, JSON3.write((; ok = true))
    else
        return 404, JSON3.write((; error = "Model not found: $name"))
    end
end
