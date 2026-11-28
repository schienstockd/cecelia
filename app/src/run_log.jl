# Per-image RUN LOG — a record of which task functions ran on an image, with what params, and when, so
# the UI can show a provenance history ("ran segment.cellpose on 2026-07-11", …) and the AI observer can
# see the *tuning trail* — which param sets were tried across re-runs. Each entry is
# {fun, valueName, status, params, at, taskId, pool, by, finishedAt}; `params` is the sanitised task params
# (internal `_…` keys and the redundant `valueName` dropped — see `_run_log_params`); `pool` is the
# resource pool it ran in (`effective_pool_name`; absent on entries written before it was recorded); `by`
# is the profile that launched it (absent on older entries and when unknown). Stored as a
# sidecar `{1/uid}/runlog.json` (a JSON array), mirroring the QC sidecars. Holds the newest
# RUN_LOG_CAP entries; older ones are MOVED to `runlog.archive.jsonl`, never dropped (see *Archive*).
#
# ── What it read, and what it replaced: `inputs`, `replaces` ───────────────────────────────────────
# Stamped at open by `run_provenance` (run_provenance.jl — the readers that interpret these fields live
# there), from the task spec alone (one place, every task):
#   inputs   — one {param, name, ns?, version?, pinned?} per name the run READ: every `valueNameSelection`
#              param that does not write. An empty picker (= "the active one") is resolved to the name
#              it meant. `version` is the `vN` it read — the pin in `params["version"]` for `valueName`,
#              else `_latest` — on image versions (ns absent) and label sets (`ns: "labels"`); other
#              namespaces carry the name only. Populations are not repeated here: they are in `params`
#              and `run_pop_reads` derives them.
#   replaces — present only when the run's output already existed and the run overwrites it
#              (re-runs overwrite BY DESIGN; `keep_previous_version()` is the opt-in that keeps image
#              versions instead, and then nothing is replaced). {name, ns?, version?} plus the
#              {taskId, at, fun} of the newest `done` run that made it, when the log (or its archive)
#              has one; without them the output predates the log or came from outside it.
#   gates    — one {valueName, path, hash} per gated population the run READ (`run_pop_reads`' refs,
#              `gated_pop_refs`): the content hash of that population's gate as it was when the run
#              started (`pop_gate_hashes`, gating/popmanager/history.jl — it covers the whole parent
#              chain). Gates are edited in place; `gate_state_for_hash` finds the gate a hash names in
#              the sidecar or its history (`gating/{vn}.history.jsonl`).
# All three are ABSENT on entries written before they were recorded, and on runs that read/replace nothing.
#   unchanged — `true` on a `done` run that wrote nothing (`run_unchanged`: its result says `"unchanged"`,
#              e.g. a label correction whose every op was a no-op). Stamped at close, which also DROPS the
#              `replaces` stamped at open: the output is byte-identical, nothing was overwritten. Such a run
#              made nothing — `_run_maker` and the staleness walk (`stale_outputs_for`) pass over it.
#
# ── What made it: `ceceliaVersion`, `ceceliaCommit`, `source`, `author`, `env` ────────────────────────────────────────────────
# So a result can say which code, which launcher and which machine made it. All three are ABSENT on
# entries written before they were recorded, and absent whenever the value is not known — never a guess:
#   ceceliaVersion — `cecelia_version()` of the process that RAN it (the detached runner can lag the
#                    backend by a version), stamped at open.
#   ceceliaCommit  — `cecelia_commit()`: the git commit of a checkout or dev install; absent for a release,
#                    whose version already names the code. Stamped at open.
#   source         — where a NON-built-in task came from (`task_source_stamp`): `{tier: "plugin", plugin,
#                    version, ref?}` or `{tier: "hand-dropped", file}`. Absent for a built-in, whose code is
#                    named by the two fields above. Stamped at open, from the `source` the caller passes.
#   author         — the `author_stamp()` shape {profile, via?, model?, session?}: `by` plus the caller
#                    the asking side captured (`caller_stamp`). No `via` = the asker did not say (a REPL
#                    call, a chain node — see `run_author`).
#   env            — facts decided at run time (device, engine version, seed), stamped at close from the
#                    task result's `"runEnv"` (`run_env_of`). A task that reports none gets none.
#   usage          — not what made it but what it USED, by the same rule: `{source, host?,
#                    peakRssBytes?, runs, …}` — a local run's peak Python memory and host, a cluster
#                    job's scheduler accounting (shape and merge rules: app/src/run_usage.jl). Stamped
#                    at close from the accumulator the scheduler binds around the task; absent when
#                    nothing reported (a task with no Python, a runner killed before exit, a site with
#                    no accounting).
#
# ── Format: `format` ───────────────────────────────────────────────────────────────────────────────
# Every entry written since formats were numbered carries `format` = RUN_LOG_FORMAT, stamped on the path
# that CREATES it (open, a close with nothing to patch, a skip, `append_run_log!`); a close keeps the
# number its open wrote. ABSENT = format 0 = written before this field existed. Migration rule: entries
# are never rewritten to a newer format — readers stay tolerant of every older one (missing fields read
# as "not recorded", never as a guess), and a reader that needs a field only a newer format guarantees
# asks `run_entry_format(e)` first. Bump the number when an entry field changes MEANING or a reader
# would otherwise misread an older entry, not for a new optional field. History:
#   0 — before `format` (all fields optional; legacy entries may lack status/params/taskId)
#   1 — `format` added; same fields as 0
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
#   running · done · failed · cancelled · interrupted · skipped   (legacy entries carry none → read "done")
#
# `skipped` is the one entry with no run behind it: the run was ASKED for on this image and dropped
# before dispatch (today: the image is excluded), written once by `skip_run_log!` with a `reason`. Its
# `taskId` is the one the rest of the selection ran under, so a set-level result that covers fewer
# images can be traced to the images it left out. Like cancelled/interrupted it produced no output.
# A chain node that REUSED a past run ("Reuse unchanged steps", tasks/chain/reuse.jl) is skipped too,
# with reason `reused <taskId>` (`RUN_LOG_REUSED_PREFIX`) naming the `done` run whose output it kept and
# an empty `taskId`: that run stays the output's maker for every reader (`_run_maker`, the staleness
# walk, the history table), which pass over skipped entries.
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
#
# ── Archive: the cap moves entries, it never drops them ────────────────────────────────────────────
# `runlog.json` is rewritten whole on every open/close, so it is kept to the newest RUN_LOG_CAP
# entries. The overflow is APPENDED to `runlog.archive.jsonl` beside it (one JSON entry per line,
# oldest first) before the main file is rewritten — so a kill between the two duplicates an entry in
# the archive rather than losing it, and a failed append keeps the entries in the main file instead.
# Readers of a torn last line skip it. Append-only, except that a label-set rename rewrites names in it
# exactly as it does in the main file (`_rename_run_log!`).
#
# Who reads it: the readers that answer "what made this output" — `run_log_params_for_output`, the
# `replaces` lookup, the claim checks' lineage, the last track-measure params, Kiwi's task refs — and
# `run_pop_reads`, the guard on editing a gate runs depend on — read it too (`read_run_log_history`). The
# history views (the task-history dialog, lineage, the lab-log context) show the recent window by design.
const RUN_LOG_CAP = 200
const RUN_LOG_FORMAT = 1      # see *Format* above; absent on an entry = 0
const RUN_LOG_ARCHIVE_FILE = "runlog.archive.jsonl"

# The one non-terminal status, and the one a reaped run gets. Named because three files test for them.
const RUN_LOG_RUNNING     = "running"
const RUN_LOG_INTERRUPTED = "interrupted"
const RUN_LOG_SKIPPED     = "skipped"     # asked for, never dispatched — see `skip_run_log!`
const RUN_LOG_REUSED_PREFIX = "reused "   # a skip's reason when a chain node reused a past run (header)

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
run_log_archive_path(ccid_dir::AbstractString) = joinpath(String(ccid_dir), RUN_LOG_ARCHIVE_FILE)
run_log_archive_path(img::CciaImage) = run_log_archive_path(img._dir)

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

"""
    read_run_log_archive(ccid_dir | img) -> Vector

The entries rotated out of `runlog.json` (see *Archive* in the header), oldest→newest; [] when none.
A line that does not parse (a torn last append) is skipped, not fatal.
"""
read_run_log_archive(img::CciaImage)::Vector{Any} = read_run_log_archive(img._dir)
read_run_log_archive(ccid_dir::AbstractString)::Vector{Any} = read_jsonl(run_log_archive_path(ccid_dir))

"""
    read_run_log_history(ccid_dir | img) -> Vector

The whole record, archive then main file, oldest→newest. For a reader that must find an old run; a
view of recent runs reads `read_run_log`.
"""
read_run_log_history(img::CciaImage)::Vector{Any} = read_run_log_history(img._dir)
read_run_log_history(ccid_dir::AbstractString)::Vector{Any} =
    vcat(read_run_log_archive(ccid_dir), read_run_log(ccid_dir))

# Append rotated-out entries to the archive in ONE write (`append_jsonl!`). Called under `_RUN_LOG_LOCK`.
_append_run_log_archive!(ccid_dir::AbstractString, overflow::AbstractVector) =
    append_jsonl!(run_log_archive_path(ccid_dir), overflow)

# One run-log entry as plain values: `params` with the entry's `valueName` put back in (the log keeps
# it beside the params), `legacy` = logged before the run log recorded params, `status` per
# `run_entry_status` (a legacy entry reads "done"). Other missing fields read "".
function run_log_row(e)
    raw = _rl_get(e, "params", nothing)
    p = raw isa AbstractDict ? Dict{String,Any}(string(k) => v for (k, v) in raw) : Dict{String,Any}()
    legacy = isempty(p)
    vn = _rl_str(e, "valueName")
    isempty(vn) || (p["valueName"] = vn)
    (; fn = _rl_str(e, "fun"), params = p, at = _rl_str(e, "at"), status = run_entry_status(e), legacy,
       taskId = _rl_str(e, "taskId"))
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
        if length(entries) > RUN_LOG_CAP
            # archive FIRST: a kill before the rewrite below duplicates, never loses (see *Archive*)
            n = length(entries) - RUN_LOG_CAP
            try
                _append_run_log_archive!(img._dir, entries[1:n])
                entries = entries[(n + 1):end]
            catch e
                @warn "run-log archive append failed; keeping the entries in runlog.json" exception = e
            end
        end
        write_json_atomic(run_log_path(img), entries)
        entries
    end
end

# a run-log entry's field, tolerating the String/Symbol key split JSON3 can hand back.
_rl_get(e, k, dflt = "") = (v = get(e, k, get(e, Symbol(k), nothing)); v === nothing ? dflt : v)
_rl_str(e, k) = string(_rl_get(e, k, ""))

# An entry's status, with a legacy entry (no status: the log was append-on-finish for done/failed only,
# see the header) read as the historical success it is. The one rule every "did it succeed" reader uses.
run_entry_status(e)::String = (s = _rl_str(e, "status"); isempty(s) ? "done" : s)

# Whether an entry is a run at all — false for a `skipped` one (asked for, never dispatched; see
# `skip_run_log!`). The one rule every "what ran on this image" reader passes over skips with.
run_entry_ran(e)::Bool = run_entry_status(e) != RUN_LOG_SKIPPED

# An entry's format number (see *Format* in the header): absent → 0, written before formats were numbered.
run_entry_format(e)::Int = (v = _rl_get(e, "format", 0); v isa Integer ? Int(v) : something(tryparse(Int, string(v)), 0))

"""
    run_author(by, caller) -> Dict{String,Any}

The run's author stamp, `author_stamp()`-shaped: the `caller` the asking side captured
(`caller_stamp()`: `{via, model?, session?}`) with `by` — the launching profile the scheduler threads
— as its `profile`. Either part may be missing and its fields are then absent, not filled in: a REPL
`run_task` has no caller, so its stamp is `{profile}` and says nothing about `via`.
"""
function run_author(by::AbstractString, caller::AbstractDict = Dict{String,Any}())::Dict{String,Any}
    s = Dict{String,Any}(String(k) => string(v) for (k, v) in caller
                         if String(k) in ("via", "model", "session") && !isempty(string(v)))
    isempty(by) || (s["profile"] = String(by))
    s
end

"""
    run_env_of(result) -> Dict{String,Any}

The run-time environment a task reported in its result as `"runEnv"` (e.g. `{device, cellpose, seed}`),
or an empty Dict. A task opts in by returning it; the scheduler stamps it onto the run-log entry.
"""
function run_env_of(result)::Dict{String,Any}
    result isa AbstractDict || return Dict{String,Any}()
    env = get(result, "runEnv", get(result, :runEnv, nothing))
    env isa AbstractDict || return Dict{String,Any}()
    Dict{String,Any}(String(k) => v for (k, v) in env if v !== nothing)
end

"""
    run_unchanged(result) -> Bool

Did the run leave its output exactly as it was? A task says so by returning `"unchanged" => true` (a
`segment.correct` whose ops rewrote no pixel); a composite hands back its last step's result. The
scheduler passes it to `close_run_log!`, which stamps `unchanged` and drops `replaces` (see the header).
"""
run_unchanged(result)::Bool =
    result isa AbstractDict && get(result, "unchanged", get(result, :unchanged, false)) === true

# a `done` run that wrote nothing — never the maker of anything (see `unchanged` in the header)
run_entry_unchanged(e)::Bool = _rl_get(e, "unchanged", false) === true

"""
    read_run_env(sidecar_path) -> Dict{String,Any}

The `"runEnv"` a Python runner wrote into the JSON sidecar it already hands back (its `qcOutPath`),
or an empty Dict when the file or the key is missing. This is the Python → Julia half of `run_env_of`:
the runner knows the device it got; the handler copies this into its result.
"""
function read_run_env(path::AbstractString)::Dict{String,Any}
    isfile(path) || return Dict{String,Any}()
    try
        run_env_of(Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(path, String))))
    catch
        Dict{String,Any}()
    end
end

# The fields every entry gets on the path that CREATES it (open, or a close with nothing to patch).
# `source` is the caller's `task_source_stamp(fun)` (the scheduler and `run_task_logged!` pass it):
# where a task came from is the task registry's to say, so the log is handed it, never asks.
function _run_log_stamp!(e::Dict{String,Any}, by::AbstractString, author::AbstractDict; source = nothing)
    e["format"] = RUN_LOG_FORMAT
    isempty(by) || (e["by"] = String(by))
    a = run_author(by, author)
    isempty(a) || (e["author"] = a)
    v = try cecelia_version() catch; "" end
    isempty(v) || (e["ceceliaVersion"] = v)
    c = try cecelia_commit() catch; "" end
    isempty(c) || (e["ceceliaCommit"] = c)
    source === nothing || (e["source"] = source)
    e
end

"""
    open_run_log!(img, fun_name, value_name, params; task_id, pool, by, caller, at) -> entries

Record that a run has STARTED, with status `"running"`. Pair with `close_run_log!` on every exit path.
`inputs` / `replaces` / `gates` are `run_provenance`'s answer for this image (see the header); empty = not stamped.
`source` is `task_source_stamp(fun_name)` (`nothing` = a built-in: not stamped).

The entry is written before the work, so a run whose process is killed outright still leaves a trace —
that is the whole reason the log is not append-on-finish (see the header). `task_id` is what
`close_run_log!` and `reap_run_log!` match on; pass the scheduler's job id.
"""
function open_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                       params = nothing; task_id::AbstractString = "", pool::AbstractString = "",
                       by::AbstractString = "", caller::AbstractDict = Dict{String,Any}(),
                       inputs::AbstractVector = Any[], replaces = nothing,
                       gates::AbstractVector = Any[], source = nothing, at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        e = Dict{String,Any}(
            "fun" => string(fun_name), "valueName" => string(value_name),
            "status" => RUN_LOG_RUNNING, "params" => _run_log_params(params),
            "at" => String(at), "taskId" => string(task_id), "pool" => String(pool))
        isempty(inputs) || (e["inputs"] = collect(Any, inputs))
        replaces isa AbstractDict && (e["replaces"] = replaces)
        isempty(gates) || (e["gates"] = collect(Any, gates))
        push!(entries, _run_log_stamp!(e, by, caller; source))
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

`env` (see `run_env_of`) is stamped as the entry's `env` when non-empty, and `usage` (the run's
`RunUsage` total, app/src/run_usage.jl) as its `usage`.
"""
function close_run_log!(img::CciaImage, task_id::AbstractString, status::AbstractString;
                        fun_name::AbstractString = "", value_name::AbstractString = "",
                        params = nothing, pool::AbstractString = "", by::AbstractString = "",
                        caller::AbstractDict = Dict{String,Any}(), env::AbstractDict = Dict{String,Any}(),
                        usage::AbstractDict = Dict{String,Any}(),
                        unchanged::Bool = false, source = nothing, at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        # newest-first: a re-run of the same task id should close its own entry, not an older one
        i = findlast(e -> _rl_str(e, "taskId") == string(task_id) &&
                          _rl_str(e, "status") == RUN_LOG_RUNNING, entries)
        if isnothing(i)
            e = Dict{String,Any}(
                "fun" => string(fun_name), "valueName" => string(value_name),
                "status" => string(status), "params" => _run_log_params(params),
                "at" => String(at), "taskId" => string(task_id), "pool" => String(pool))
            _run_log_stamp!(e, by, caller; source)
            isempty(env) || (e["env"] = Dict{String,Any}(String(k) => v for (k, v) in env))
            isempty(usage) || (e["usage"] = Dict{String,Any}(String(k) => v for (k, v) in usage))
            unchanged && (e["unchanged"] = true)
            push!(entries, e)
        else
            e = Dict{String,Any}(String(k) => v for (k, v) in pairs(entries[i]))
            e["status"] = string(status)
            e["finishedAt"] = String(at)
            isempty(env) || (e["env"] = Dict{String,Any}(String(k) => v for (k, v) in env))
            isempty(usage) || (e["usage"] = Dict{String,Any}(String(k) => v for (k, v) in usage))
            # nothing was overwritten: the `replaces` stamped at open would claim otherwise
            unchanged && (e["unchanged"] = true; delete!(e, "replaces"))
            entries[i] = e
        end
        entries
    end
end

"""
    skip_run_log!(img, fun_name, reason; value_name, params, task_id, by, at) -> entries

Record that a run was asked for on this image and dropped before dispatch — status `"skipped"` plus
the `reason`. One write, no open/close pair: nothing ran, so there is no outcome to wait for. Pass the
`task_id` the rest of the selection runs under, so the entry links to that run's entries.
"""
function skip_run_log!(img::CciaImage, fun_name::AbstractString, reason::AbstractString;
                       value_name::AbstractString = "", params = nothing,
                       task_id::AbstractString = "", by::AbstractString = "",
                       at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        e = Dict{String,Any}(
            "fun" => string(fun_name), "valueName" => string(value_name),
            "status" => RUN_LOG_SKIPPED, "reason" => string(reason),
            "params" => _run_log_params(params), "at" => String(at), "taskId" => string(task_id),
            "format" => RUN_LOG_FORMAT)
        isempty(by) || (e["by"] = String(by))
        push!(entries, e)
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
recoverable from them (`task_params_name`: the spec's `namespace`, or the input a task writes into). So the by-name record is a
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
to restore (written by a different task, or before the run log existed) answers `nothing`, and the
form is left alone rather than overwritten. A run older than `RUN_LOG_CAP` is still found: it was
rotated into the archive, which is searched when the main file has no answer.
"""
function run_log_params_for_output(ccid_dir::AbstractString, fun::AbstractString,
                                   value_name::AbstractString)::Union{Dict{String,Any},Nothing}
    (isempty(fun) || isempty(value_name)) && return nothing
    entries = read_run_log(ccid_dir)
    isempty(entries) && !isfile(run_log_archive_path(ccid_dir)) && return nothing

    # Resolved once so an entry for a task that no longer exists costs one lookup, not one per entry.
    # The per-entry call stays `task_params_name` rather than its pure `_spec_output_name` half: that
    # would skip the `::CompositeTask` method and answer `""` for exactly the tasks the module pages
    # run. Re-entering `_task_spec` per entry is the price (a cached read, plus a deepcopy + a model-dir
    # scan for a task with dynamic options) and it is paid only while resolving a name.
    task = try
        _task_from_fun_name(String(fun))
    catch
        nothing
    end
    isnothing(task) && return nothing

    hit = _params_for_output_in(entries, task, fun, value_name)
    isnothing(hit) || return hit
    # not in the recent window: the run may have been rotated into the archive (see *Archive*)
    _params_for_output_in(read_run_log_archive(ccid_dir), task, fun, value_name)
end

function _params_for_output_in(entries, task, fun::AbstractString, value_name::AbstractString)
    for e in Iterators.reverse(entries)          # newest first — the most recent run under the name
        # legacy entries carry no status and are historical successes (the log was append-on-finish
        # for :done/:failed only — see the header). `running` is skipped with the rest: a run in
        # flight has not written its output yet, so the picker is not offering that name either.
        run_entry_status(e) == "done" || continue
        run_entry_unchanged(e) && continue       # wrote nothing, so it did not produce the output
        _rl_str(e, "fun") == String(fun) || continue
        p = _rl_get(e, "params", nothing)
        p isa AbstractDict || continue
        params = Dict{String,Any}(String(k) => v for (k, v) in p)
        # put `valueName` back first: for a task that writes into its input (tracking) it IS the name
        vn = _rl_str(e, "valueName")
        isempty(vn) || (params["valueName"] = vn)
        task_params_name(task, params) == String(value_name) || continue
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
            "at" => String(at), "taskId" => "", "format" => RUN_LOG_FORMAT))
        entries
    end
end
