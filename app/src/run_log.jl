# Per-image RUN LOG — an append-only record of which task functions ran on an image and when, so the
# UI can show a provenance history ("ran segment.cellpose on 2026-07-11", …). No params are stored here
# (the last-run params live in the task config); this is just fun + value_name + timestamp. Stored as a
# sidecar `{1/uid}/runlog.json` (a JSON array), mirroring the QC sidecars. The scheduler appends an
# entry when a task finishes successfully (:done); see tasks/scheduler.jl. Capped to the most recent
# RUN_LOG_CAP entries so it can't grow unbounded.
const RUN_LOG_CAP = 200

run_log_path(img::CciaImage) = joinpath(img._dir, "runlog.json")

# the run log as a Vector of Dicts ({fun, valueName, at}), oldest→newest; [] when none.
function read_run_log(img::CciaImage)::Vector{Any}
    p = run_log_path(img)
    isfile(p) || return Any[]
    try
        collect(JSON3.read(read(p, String), Vector{Any}))
    catch
        Any[]   # corrupt/legacy file → treat as empty rather than throwing on image load
    end
end

# append one {fun, valueName, status, at} entry (ISO-ish local timestamp) and persist; returns the new
# log. `status` is "done" (success) or "failed" — recording failures too so the run history (and the
# AI observer reading it) can see repeated failures, not just successes. Legacy entries lack `status`;
# readers should treat a missing status as "done".
function append_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                         status::AbstractString = "done")
    entries = read_run_log(img)
    push!(entries, Dict{String,Any}(
        "fun" => string(fun_name), "valueName" => string(value_name), "status" => string(status),
        "at" => Dates.format(Dates.now(), "yyyy-mm-ddTHH:MM:SS")))
    length(entries) > RUN_LOG_CAP && (entries = entries[(end - RUN_LOG_CAP + 1):end])
    open(run_log_path(img), "w") do io
        JSON3.write(io, entries)
    end
    entries
end
