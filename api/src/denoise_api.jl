# ── Denoise (SUPPORT) model vault (API layer) ───────────────────────────────────
# Read/rename/delete for `<config_dir>/models/denoiseModels/`. Mirror of `optical_flow_api.jl` —
# the vault is MACHINE-LOCAL, shared across projects, and a model is a PAIR (`<name>.pt` +
# `<name>.json`), so rename/delete move or remove both. An orphaned `.pt` is not harmless: the
# runner reads architecture (`inputFrames`, `midChannels`, `depth`, `blindConvChannels`, `bsSize`)
# from the manifest before it calls `SUPPORT(...)`; a missing manifest fails loud rather than
# silently rebuilding the wrong shape.
#
# Enumeration lives in `Cecelia.list_denoise_models` (config.jl), which the task's picker uses via
# `optionsFrom: "denoiseModels"` — the manager must not grow a second listing that can disagree
# with the dropdown.
# See docs/todo/DENOISE_INTEGRATION_PLAN.md → Phase A.

using Dates

_denoise_model_stem(name::AbstractString) = String(first(splitext(String(name))))

"""A vault filename that is provably a leaf inside the vault, or `nothing`."""
function _safe_denoise_model(name)::Union{String,Nothing}
    s = String(strip(String(name)))
    (isempty(s) || occursin(r"[/\\]", s) || s in (".", "..")) && return nothing
    endswith(s, ".pt") || return nothing
    s
end

function api_denoise_models(::HTTP.Request)
    dir = Cecelia.denoise_models_dir()
    models = map(Cecelia.list_denoise_models()) do m
        path = joinpath(dir, m.name)
        (; name = m.name, label = m.label, stem = _denoise_model_stem(m.name),
           bytes = isfile(path) ? filesize(path) : 0,
           modified = isfile(path) ? Dates.format(Dates.unix2datetime(mtime(path)),
                                                  "yyyy-mm-dd") : "",
           hasManifest = !isempty(m.manifest),
           manifest = m.manifest)
    end
    200, JSON3.write((; dir = dir, models = models))
end

function api_denoise_rename(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    from = _safe_denoise_model(get(body, :name, ""))
    to_stem = String(strip(String(get(body, :newName, ""))))
    isnothing(from) && return 400, JSON3.write((; error = "name required (a .pt in the vault)"))
    (isempty(to_stem) || occursin(r"[/\\]", to_stem) || to_stem in (".", "..")) &&
        return 400, JSON3.write((; error = "New name cannot be empty or contain a path separator"))
    to_stem = _denoise_model_stem(to_stem)

    dir = Cecelia.denoise_models_dir()
    src = joinpath(dir, from)
    isfile(src) || return 404, JSON3.write((; error = "Model not found: $from"))
    dest = joinpath(dir, "$(to_stem).pt")
    src == dest && return 200, JSON3.write((; ok = true, name = basename(dest)))
    isfile(dest) && return 409, JSON3.write((; error = "A model named '$to_stem' already exists"))

    mv(src, dest)
    src_manifest = joinpath(dir, "$(_denoise_model_stem(from)).json")
    isfile(src_manifest) && mv(src_manifest, joinpath(dir, "$(to_stem).json"); force = true)
    200, JSON3.write((; ok = true, name = basename(dest)))
end

function api_denoise_delete(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    name = _safe_denoise_model(get(body, :name, ""))
    isnothing(name) && return 400, JSON3.write((; error = "name required (a .pt in the vault)"))

    dir = Cecelia.denoise_models_dir()
    path = joinpath(dir, name)
    isfile(path) || return 404, JSON3.write((; error = "Model not found: $name"))
    rm(path)
    manifest = joinpath(dir, "$(_denoise_model_stem(name)).json")
    isfile(manifest) && rm(manifest)
    200, JSON3.write((; ok = true))
end
