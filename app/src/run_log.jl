# Per-image RUN LOG — an append-only record of which task functions ran on an image, with what params,
# and when, so the UI can show a provenance history ("ran segment.cellpose on 2026-07-11", …) and the
# AI observer can see the *tuning trail* — which param sets were tried across re-runs. Each entry is
# {fun, valueName, status, params, at}; `params` is the sanitised task params (internal `_…` keys and
# the redundant `valueName` dropped — see `_run_log_params`). Stored as a sidecar `{1/uid}/runlog.json`
# (a JSON array), mirroring the QC sidecars. The scheduler appends an entry when a task finishes
# (:done or :failed); see tasks/scheduler.jl. Capped to the most recent RUN_LOG_CAP entries.
#
# IMPORTANT — this is a TRAIL, not a param↔outcome correlation. QC-at-the-time is deliberately NOT
# snapshotted here: a re-run overwrites the h5ad, so a superseded run's metrics are gone, and real
# projects produce only a few confounded re-runs (the user nudges params *because* the last result was
# off) — not a fittable dataset. Readers (incl. Claude) must treat `params` as "what was tried", never
# as a relationship to extrapolate from. See docs/todo/OBSERVER_PHASE2_PLAN.md §1 + the tier-3 note.
const RUN_LOG_CAP = 200

run_log_path(img::CciaImage) = joinpath(img._dir, "runlog.json")

# the run log as a Vector of Dicts ({fun, valueName, status, params, at}), oldest→newest; [] when none.
# Legacy entries may lack `status`/`params`; readers treat missing status as "done" and missing params as {}.
function read_run_log(img::CciaImage)::Vector{Any}
    p = run_log_path(img)
    isfile(p) || return Any[]
    try
        collect(JSON3.read(read(p, String), Vector{Any}))
    catch
        Any[]   # corrupt/legacy file → treat as empty rather than throwing on image load
    end
end

# Sanitise task params for the run-log trail: drop internal keys (any leading-underscore key, e.g. the
# injected `_task_id`) and the redundant `valueName` (already its own field). Keeps the real tuning
# knobs. Returns a String-keyed Dict; `nothing`/empty → an empty Dict (so entries are shape-stable).
function _run_log_params(params)::Dict{String,Any}
    out = Dict{String,Any}()
    params === nothing && return out
    for (k, v) in params
        ks = string(k)
        (startswith(ks, "_") || ks == "valueName") && continue
        out[ks] = v
    end
    out
end

# append one {fun, valueName, status, params, at} entry (ISO-ish local timestamp) and persist; returns
# the new log. `status` is "done" (success) or "failed" — recording failures too so the run history
# (and the AI observer reading it) can see repeated failures, not just successes. `params` is the raw
# task params (sanitised via `_run_log_params`); pass `nothing` (default) when there are none.
function append_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                         status::AbstractString = "done", params = nothing)
    entries = read_run_log(img)
    push!(entries, Dict{String,Any}(
        "fun" => string(fun_name), "valueName" => string(value_name), "status" => string(status),
        "params" => _run_log_params(params),
        "at" => Dates.format(Dates.now(), "yyyy-mm-ddTHH:MM:SS")))
    length(entries) > RUN_LOG_CAP && (entries = entries[(end - RUN_LOG_CAP + 1):end])
    open(run_log_path(img), "w") do io
        JSON3.write(io, entries)
    end
    entries
end
