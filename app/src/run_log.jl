# Per-image RUN LOG — a record of which task functions ran on an image, with what params, and when, so
# the UI can show a provenance history ("ran segment.cellpose on 2026-07-11", …) and the AI observer can
# see the *tuning trail* — which param sets were tried across re-runs. Each entry is
# {fun, valueName, status, params, at, taskId, finishedAt}; `params` is the sanitised task params
# (internal `_…` keys and the redundant `valueName` dropped — see `_run_log_params`). Stored as a
# sidecar `{1/uid}/runlog.json` (a JSON array), mirroring the QC sidecars. Capped to RUN_LOG_CAP.
#
# ── A run is written TWICE: opened when it starts, closed when it ends ─────────────────────────────
#
# It used to be appended once, on finish, and ONLY for :done/:failed — `:cancelled` was skipped on the
# reasoning that "the user aborted, not an outcome worth logging". That reasoning holds for a task
# aborted five seconds in and fails badly for everything else: a cancelled run is also how a task ends
# when its process is killed, so 22 minutes of GPU segmentation could vanish leaving NOTHING on disk —
# no entry, no outcome, no trace. The user's question afterwards ("I started six, three are running,
# what happened to the other three?") was then unanswerable from the project itself.
#
# Worse, an append-on-finish log cannot record a run that never reaches its own finish at all. The
# detached task runner holds its queue in memory with no spool (docs/RUNNER.md), so a Ctrl-C or a
# runner crash takes every in-flight task with it — no Julia code runs, so no append ever happens.
# The only way to record that is to have already written something when the run STARTED.
#
# Hence the pair: `open_run_log!` writes the entry at `:running`, `close_run_log!` patches it in place
# with the terminal status. A run whose process died leaves a stale `"running"` entry, which
# `reap_run_log!` converts to `"interrupted"` once it can prove no live task still owns it. Statuses:
#
#   running · done · failed · cancelled · interrupted        (legacy entries carry none → read "done")
#
# Readers must therefore treat `status` as an open set and NOT assume a terminal one — `"running"` on
# disk means "started, outcome not yet known", which is a real state, not a corrupt entry.
#
# IMPORTANT — this is a TRAIL, not a param↔outcome correlation. QC-at-the-time is deliberately NOT
# snapshotted here: a re-run overwrites the h5ad, so a superseded run's metrics are gone, and real
# projects produce only a few confounded re-runs (the user nudges params *because* the last result was
# off) — not a fittable dataset. Readers (incl. Claude) must treat `params` as "what was tried", never
# as a relationship to extrapolate from. See docs/ai-assist/OBSERVER.md → Phase 2 (param-suggestion
# boundary + the deferred tier-3 per-run QC snapshot).
const RUN_LOG_CAP = 200

# The one non-terminal status, and the one a reaped run gets. Named because three files test for them.
const RUN_LOG_RUNNING     = "running"
const RUN_LOG_INTERRUPTED = "interrupted"

# Serialises the read-modify-write in `_update_run_log!`. Two tasks CAN target one image at once (they
# sit in different pools), and both `open_run_log!` and `close_run_log!` rewrite the whole file, so
# without this the second write is built from a list read before the first one landed.
#
# This is a WITHIN-process lock. When the detached runner is enabled the runner writes these entries
# and the backend only ever reaps at project open, so the two processes touch disjoint entries; the
# writes are whole-file though, so a reap landing in the microseconds between a runner read and its
# write would be lost. `write_json_atomic` still guarantees the file is never torn — the failure mode
# is a re-reaped entry on the next open, not corruption. A lock file for that window would cost more
# than it buys.
const _RUN_LOG_LOCK = ReentrantLock()

run_log_path(ccid_dir::AbstractString) = joinpath(String(ccid_dir), "runlog.json")
run_log_path(img::CciaImage) = run_log_path(img._dir)

# the run log as a Vector of Dicts ({fun, valueName, status, params, at, taskId, finishedAt}),
# oldest→newest; [] when none. Legacy entries may lack `status`/`params`/`taskId`; readers treat a
# missing status as "done", missing params as {} and a missing taskId as "".
#
# Dir-based as well as image-based: the funParams reader (`read_module_fun_params_by_name`) is
# dir-based by design — the task form asks for a name's params without loading an object — and it
# backfills from this log. Same file, one reader.
read_run_log(img::CciaImage)::Vector{Any} = read_run_log(img._dir)
function read_run_log(ccid_dir::AbstractString)::Vector{Any}
    p = run_log_path(ccid_dir)
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

_run_log_now() = Dates.format(Dates.now(), "yyyy-mm-ddTHH:MM:SS")

# read → mutate → persist under the lock. `f` takes the entry vector and returns the one to write.
# Never throws on a log write: a task must not fail because its provenance line could not be saved.
function _update_run_log!(f::Function, img::CciaImage)
    lock(_RUN_LOG_LOCK) do
        entries = read_run_log(img)
        entries = f(entries)
        length(entries) > RUN_LOG_CAP && (entries = entries[(end - RUN_LOG_CAP + 1):end])
        write_json_atomic(run_log_path(img), entries)
        entries
    end
end

# a run-log entry's field, tolerating the String/Symbol key split JSON3 can hand back.
_rl_get(e, k, dflt = "") = (v = get(e, k, get(e, Symbol(k), nothing)); v === nothing ? dflt : v)
_rl_str(e, k) = string(_rl_get(e, k, ""))

"""
    open_run_log!(img, fun_name, value_name, params; task_id, at) -> entries

Record that a run has STARTED, with status `"running"`. Pair with `close_run_log!` on every exit path.

The entry is written before the work, so a run whose process is killed outright still leaves a trace —
that is the whole reason the log is not append-on-finish (see the header). `task_id` is what
`close_run_log!` and `reap_run_log!` match on; pass the scheduler's job id.
"""
function open_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                       params = nothing; task_id::AbstractString = "",
                       at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        push!(entries, Dict{String,Any}(
            "fun" => string(fun_name), "valueName" => string(value_name),
            "status" => RUN_LOG_RUNNING, "params" => _run_log_params(params),
            "at" => String(at), "taskId" => string(task_id)))
        entries
    end
end

"""
    close_run_log!(img, task_id, status; fun_name, value_name, params, at) -> entries

Stamp a run's terminal outcome onto the entry `open_run_log!` wrote, matched by `task_id`.

Records **every** terminal status — `"done"`, `"failed"` AND `"cancelled"` — so repeated failures and
abandoned work are both visible rather than only successes. If no open entry is found (a run opened
before this process started, or a caller that never opened one) the outcome is appended instead, so a
terminal status is never silently dropped; that is what makes `append_run_log!` below just a
convenience wrapper rather than a second storage path.
"""
function close_run_log!(img::CciaImage, task_id::AbstractString, status::AbstractString;
                        fun_name::AbstractString = "", value_name::AbstractString = "",
                        params = nothing, at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        # newest-first: a re-run of the same task id should close its own entry, not an older one
        i = findlast(e -> _rl_str(e, "taskId") == string(task_id) &&
                          _rl_str(e, "status") == RUN_LOG_RUNNING, entries)
        if isnothing(i)
            push!(entries, Dict{String,Any}(
                "fun" => string(fun_name), "valueName" => string(value_name),
                "status" => string(status), "params" => _run_log_params(params),
                "at" => String(at), "taskId" => string(task_id)))
        else
            e = Dict{String,Any}(String(k) => v for (k, v) in pairs(entries[i]))
            e["status"] = string(status)
            e["finishedAt"] = String(at)
            entries[i] = e
        end
        entries
    end
end

"""
    reap_run_log!(img, live_task_ids) -> n

Convert this image's stale `"running"` entries to `"interrupted"`, returning how many were reaped.

An entry is stale when nothing is executing it any more — its process died without closing it (a
runner Ctrl-C or crash; the runner keeps its queue in memory with no spool). `live_task_ids` is the
set of task ids that ARE still executing somewhere, and it is load-bearing in one direction: a backend
restart while the detached runner keeps segmenting is the runner's whole purpose, so reaping a task
the runner is still running would report live work as lost. When the runner is unreachable its tasks
are genuinely gone, so an empty set is the correct answer then, not a reason to skip the reap.
"""
function reap_run_log!(img::CciaImage, live_task_ids = String[])::Int
    live = Set(string.(live_task_ids))
    n = 0
    _update_run_log!(img) do entries
        for (i, e) in pairs(entries)
            (_rl_str(e, "status") == RUN_LOG_RUNNING && !(_rl_str(e, "taskId") in live)) || continue
            d = Dict{String,Any}(String(k) => v for (k, v) in pairs(e))
            d["status"] = RUN_LOG_INTERRUPTED
            entries[i] = d
            n += 1
        end
        entries
    end
    n
end

"""
    run_log_params_for_output(ccid_dir, fun, value_name) -> Dict | nothing

The params of the most recent run of `fun` that wrote its output under `value_name`, or `nothing`.

**This is what makes "pick a name, get its settings back" work on work that already exists.** Params
are banked per output name in `ccid.json` (`meta.funParamsByName`) only from the run that banks them
onwards — so on every project segmented before that existed, every name resolved to "nothing banked"
and the form restored nothing. Nobody re-runs six segmentations to seed a convenience feature.

The history was already on disk: this log records every run's params, and a run's output name is
recoverable from them (`task_output_name`, via the spec's `namespace`). So the by-name record is a
fast index over this, not the only copy — and a name that predates it is found here instead.

Two deliberate choices:
  • **Only a run that finished `"done"` counts.** A failed run's params are "what was tried", not what
    the name was made with. More to the point, the two halves of this feature have to agree on one set
    of names: the picker offers what EXISTS in the namespace, and a failed run wrote nothing — so
    honouring one would restore settings for a name the list will never offer. (It first fell back to
    a failed run when no successful one existed, which is exactly that mismatch.) The alternative —
    offering failed names in the picker — cannot be had cheaply: that list is the image payload's
    per-namespace listing, shared with `valueNameSelection`, where a name that was never written is a
    task that fails on read.
  • **`valueName` is put back.** `_run_log_params` strips it because the entry carries it as its own
    field — but it is a real form param (the INPUT version a task reads), so a record restored
    without it would silently reset which version the task runs on.

The reverse mismatch stays possible and is the harmless direction: a name in the picker with nothing
to restore (written by a different task, or by a run older than `RUN_LOG_CAP`) answers `nothing`, and
the form is left alone rather than overwritten.
"""
function run_log_params_for_output(ccid_dir::AbstractString, fun::AbstractString,
                                   value_name::AbstractString)::Union{Dict{String,Any},Nothing}
    (isempty(fun) || isempty(value_name)) && return nothing
    entries = read_run_log(ccid_dir)
    isempty(entries) && return nothing

    # Resolved once so an entry for a task that no longer exists costs one lookup, not one per entry.
    # The per-entry call stays `task_output_name` rather than its pure `_spec_output_name` half: that
    # would skip the `::CompositeTask` method and answer `""` for exactly the tasks the module pages
    # run. Re-entering `_task_spec` per entry is the price (a cached read, plus a deepcopy + a model-dir
    # scan for a task with dynamic options) and it is paid only while resolving a name.
    task = try
        _task_from_fun_name(String(fun))
    catch
        nothing
    end
    isnothing(task) && return nothing

    for e in Iterators.reverse(entries)          # newest first — the most recent run under the name
        # legacy entries carry no status and are historical successes (the log was append-on-finish
        # for :done/:failed only — see the header). `running` is skipped with the rest: a run in
        # flight has not written its output yet, so the picker is not offering that name either.
        st = _rl_str(e, "status")
        (isempty(st) || st == "done") || continue
        _rl_str(e, "fun") == String(fun) || continue
        p = _rl_get(e, "params", nothing)
        p isa AbstractDict || continue
        params = Dict{String,Any}(String(k) => v for (k, v) in p)
        task_output_name(task, params) == String(value_name) || continue
        vn = _rl_str(e, "valueName")
        isempty(vn) || (params["valueName"] = vn)
        return params
    end
    nothing
end

# open+close in one call, for a run whose outcome is already known — the REPL, tests, and anything
# replaying history. Same storage path as the pair above, so there is only one entry shape to reason
# about. `status` is "done" (success), "failed", or "cancelled".
function append_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                         status::AbstractString = "done", params = nothing;
                         at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        push!(entries, Dict{String,Any}(
            "fun" => string(fun_name), "valueName" => string(value_name), "status" => string(status),
            "params" => _run_log_params(params),
            "at" => String(at), "taskId" => ""))
        entries
    end
end
