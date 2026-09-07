# ── Denoise (SUPPORT) model vault (API layer) ───────────────────────────────────
# Thin wrapper — the read/rename/delete surface is shared with `optical_flow_api.jl` via
# `vault_api.jl`. What's specific to denoise is the vault directory and the listing function; both
# are one-line calls into `Cecelia.*`. The vault is MACHINE-LOCAL, shared across projects, and a
# model is either a `<name>.pt` + `<name>.json` pair (pooled) or a `<name>/` folder holding one
# `.pt` + `.json` per channel (perChannel bundle, SUPPORT_PERCHANNEL_PLAN.md → D2/D3).
#
# Enumeration lives in `Cecelia.list_denoise_models` (config.jl) — the manager must not grow a
# second listing that can disagree with the task's picker (`optionsFrom: "denoiseModels"`).
# See docs/todo/DENOISE_INTEGRATION_PLAN.md → Phase A.

function api_denoise_models(::HTTP.Request)
    dir = Cecelia.denoise_models_dir()
    models = [vault_model_row(dir, m) for m in Cecelia.list_denoise_models()]
    200, JSON3.write((; dir = dir, models = models))
end

function api_denoise_rename(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    from = safe_vault_model_name(get(body, :name, ""))
    isnothing(from) && return 400, JSON3.write((; error = "name required"))
    vault_rename(Cecelia.denoise_models_dir(), from, String(get(body, :newName, "")))
end

function api_denoise_delete(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    name = safe_vault_model_name(get(body, :name, ""))
    isnothing(name) && return 400, JSON3.write((; error = "name required"))
    vault_delete(Cecelia.denoise_models_dir(), name)
end
