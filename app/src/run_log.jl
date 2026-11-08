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
# Stamped at open by `run_provenance`, from the task spec alone (one place, every task):
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
#
# ── What made it: `ceceliaVersion`, `author`, `env` ────────────────────────────────────────────────
# So a result can say which code, which launcher and which machine made it. All three are ABSENT on
# entries written before they were recorded, and absent whenever the value is not known — never a guess:
#   ceceliaVersion — `cecelia_version()` of the process that RAN it (the detached runner can lag the
#                    backend by a version), stamped at open.
#   author         — the `author_stamp()` shape {profile, via?, model?, session?}: `by` plus the caller
#                    the asking side captured (`caller_stamp`). No `via` = the asker did not say (a REPL
#                    call, a chain node — see `run_author`).
#   env            — facts decided at run time (device, engine version, seed), stamped at close from the
#                    task result's `"runEnv"` (`run_env_of`). A task that reports none gets none.
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
const RUN_LOG_ARCHIVE_FILE = "runlog.archive.jsonl"

# The one non-terminal status, and the one a reaped run gets. Named because three files test for them.
const RUN_LOG_RUNNING     = "running"
const RUN_LOG_INTERRUPTED = "interrupted"
const RUN_LOG_SKIPPED     = "skipped"     # asked for, never dispatched — see `skip_run_log!`

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
    (; fn = _rl_str(e, "fun"), params = p, at = _rl_str(e, "at"), status = run_entry_status(e), legacy)
end

"""
    run_pop_reads(img) -> Dict{(value_name, path) => [fun, …]}

The gated populations this image's runs READ, each with the tasks that read it — a clustering on
`B/qc/_tracked` reads `("B", "/qc")`. Runs that finished, or are still running; a failed or cancelled
run left nothing that depends on its input. Read through the task specs' population pickers
(`gated_pop_refs`), so a task that no longer exists is skipped.
"""
function run_pop_reads(img::CciaImage)::Dict{Tuple{String,String},Vector{String}}
    out = Dict{Tuple{String,String},Vector{String}}()
    leaves = Dict{String,Any}()
    # the whole history: this guards a gate edit against runs that depend on it, however old
    for e in read_run_log_history(img)
        r = run_log_row(e)
        r.status in ("done", RUN_LOG_RUNNING) || continue
        lv = get!(() -> _history_leaves(r.fn), leaves, r.fn)
        lv === nothing && continue
        # every population picker: a track analysis on `B/qc/_tracked` reads the cell gate `/qc` too
        for ref in gated_pop_refs(r.params, lv; all_pickers = true)
            fns = get!(out, ref, String[])
            r.fn in fns || push!(fns, r.fn)
        end
    end
    out
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
function _run_log_stamp!(e::Dict{String,Any}, by::AbstractString, author::AbstractDict)
    isempty(by) || (e["by"] = String(by))
    a = run_author(by, author)
    isempty(a) || (e["author"] = a)
    v = try cecelia_version() catch; "" end
    isempty(v) || (e["ceceliaVersion"] = v)
    e
end

# ── Provenance stamped at open: what the run reads, and what it replaces ─────────────────────────
# Resolved from the task spec's declared params (`_history_leaves` — the same leaf reading the chain
# pre-flight and chain-from-history use), so every task gets it with no per-task code.

# The image field a name in namespace `ns` lives in, for a version lookup; `nothing` = no versions
# there. The namespace → field table is the chain pre-flight's (`_preflight_fields`).
_run_input_field(ns::AbstractString) = (fs = _preflight_fields(String(ns)); fs === nothing ? nothing : first(fs))

# Does `img` hold `name` in namespace `ns`? `nothing` = not checkable from the image (models, clusters, …).
_run_has_output(img::CciaImage, name::AbstractString, ns::AbstractString) =
    _preflight_fields(String(ns)) === nothing ? nothing : _preflight_has(img, String(name), String(ns))

# The version `img` holds `name` at, or "" when it does not hold it / the namespace has none.
function _run_name_version(img::CciaImage, name::AbstractString, ns::AbstractString)::String
    f = _run_input_field(ns)
    (f === nothing || !img_has_value_name(img, name; field = f)) && return ""
    resolve_version(img, f, name)
end

"""
    run_inputs(fun_name, img, params) -> Vector{Dict}

The names a run of `fun_name` with `params` reads on `img` — one `{param, name, ns?, version?, pinned?}`
per non-writing `valueNameSelection` value (nested groups and multi-selects included). See the header.
"""
function run_inputs(fun_name::AbstractString, img::CciaImage, params::AbstractDict)::Vector{Any}
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return Any[]
    # a renamed param's saved key (`formerKeys`) reads as the leaf it now is
    bykey = Dict{String,Tuple{String,Any}}()
    for (k, l) in leaves
        bykey[k] = (k, l)
        for f in l.former; haskey(bykey, f) || (bykey[f] = (k, l)); end
    end
    pin = string(something(get(params, "version", nothing), ""))
    out = Any[]
    seen = Set{Tuple{String,String}}()
    for (sk, v) in _keyed_strings(params)
        hit = get(bykey, sk, nothing)
        hit === nothing && continue
        k, l = hit
        (l.type == "valueNameSelection" && !l.writes) || continue
        name = String(strip(v))
        if isempty(name)              # an empty picker means the active one — name it
            name = l.ns == "" ? versioned_active(img.filepath) :
                   l.ns == "labels" ? resolve_value_name(img) : ""
            isempty(name) && continue
        end
        (k, name) in seen && continue
        push!(seen, (k, name))
        d = Dict{String,Any}("param" => k, "name" => name)
        isempty(l.ns) || (d["ns"] = l.ns)
        if k == "valueName" && l.ns == "" && !isempty(pin)
            d["version"] = pin; d["pinned"] = true    # the read is pinned (chain pinning, P3)
        else
            ver = _run_name_version(img, name, l.ns)
            isempty(ver) || (d["version"] = ver)
        end
        push!(out, d)
    end
    out
end

# (name, ns, into) a run writes: its named or fixed output (`_history_output`), else — for a task that
# writes INTO its input (`writesInto`, tracking) — that input, with `into = true`. ("", "", false) = none.
function _run_output_identity(fn::AbstractString, params::Dict{String,Any}, leaves)
    name, ns = _history_output(String(fn), params, leaves)
    isempty(name) || return (name, ns, false)
    into = task_params_name(String(fn), params)
    isempty(into) && return ("", "", false)
    key = try string(something(get(_task_spec(_task_from_fun_name(String(fn))), "writesInto", nothing), "")) catch; "" end
    l = get(leaves, key, nothing)
    (into, l === nothing ? "" : l.ns, true)
end

# The newest `done` entry in `entries` that wrote (name, ns) — for an `into` output, a run of the same
# task on it (the segmentation that made the label set did not make its tracks).
function _run_maker(entries, fun_name::AbstractString, name::AbstractString, ns::AbstractString, into::Bool,
                    leaves_cache::Dict{String,Any})
    for e in Iterators.reverse(entries)
        run_entry_status(e) == "done" || continue
        # no params at all = logged before params were recorded: its output name is unknowable. (Not
        # `r.legacy`, which also reads a run whose only param was its `valueName` as legacy.)
        _rl_get(e, "params", nothing) isa AbstractDict || continue
        r = run_log_row(e)
        into && r.fn != fun_name && continue
        lv = get!(() -> _history_leaves(r.fn), leaves_cache, r.fn)
        lv === nothing && continue
        (n2, ns2, into2) = try _run_output_identity(r.fn, r.params, lv) catch; ("", "", false) end
        (n2 == name && ns2 == ns && into2 == into) && return e
    end
    nothing
end

"""
    run_replaces(fun_name, img, params) -> Dict | nothing

What a run of `fun_name` with `params` will overwrite on `img`, or `nothing`. See the header.
"""
function run_replaces(fun_name::AbstractString, img::CciaImage, params::AbstractDict)
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return nothing
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    name, ns, into = _run_output_identity(fun_name, p, leaves)
    isempty(name) && return nothing
    # a fixed `default` output is a NEW image's (bin, crop, the projections) or a file (export) — the
    # source image's `default` is never written over. Same exclusion as the chain pre-flight.
    (ns == "" && !into && name == VERSIONED_DEFAULT_VAL) && return nothing
    # an image version under keepPreviousVersion is minted beside the old one, not over it
    (ns == "" && !into && keep_previous_version()) && return nothing
    # existence: read off the image where the namespace allows it; else (and for a write-into) a run made it
    on_img = into ? nothing : _run_has_output(img, name, ns)
    on_img === false && return nothing          # a new output: no log walk at all
    cache = Dict{String,Any}()
    maker = _run_maker(read_run_log(img), fun_name, name, ns, into, cache)
    maker === nothing && (maker = _run_maker(read_run_log_archive(img), fun_name, name, ns, into, cache))
    (on_img === true || maker !== nothing) || return nothing
    d = Dict{String,Any}("name" => name)
    isempty(ns) || (d["ns"] = ns)
    ver = into ? "" : _run_name_version(img, name, ns)
    isempty(ver) || (d["version"] = ver)
    if maker !== nothing
        d["taskId"] = _rl_str(maker, "taskId")
        d["at"]     = _rl_str(maker, "at")
        d["fun"]    = _rl_str(maker, "fun")
    end
    d
end

"""
    run_gates(fun_name, img, params) -> Vector{Dict}

The gated populations a run of `fun_name` with `params` reads on `img`, each `{valueName, path, hash}`
(+ `popType: "track"` for a track gate) with the content hash of its gate now (`pop_gate_hashes`). Every
population picker counts, track pickers too: `B/qc/_tracked` is read through the cell gate `/qc`. A path
is looked up in the sidecar `resolve_pop_type` finds it in; cluster/region pops are not gates and a
population in no sidecar is left out.
"""
function run_gates(fun_name::AbstractString, img::CciaImage, params::AbstractDict)::Vector{Any}
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return Any[]
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    hashes = Dict{Tuple{String,String},Dict{String,String}}()
    out = Any[]
    for (vn, path) in gated_pop_refs(p, leaves; all_pickers = true)
        pt = resolve_pop_type(img, vn, path)
        is_gating_pop_type(pt) || continue
        hs = get!(() -> pop_gate_hashes(load_pop_map(img._dir, vn; pop_type = pt, backfill_save = false)),
                  hashes, (vn, pt))
        h = get(hs, path, nothing)
        h === nothing && continue
        d = Dict{String,Any}("valueName" => vn, "path" => path, "hash" => h)
        pt == "flow" || (d["popType"] = pt)
        push!(out, d)
    end
    out
end

"""
    run_provenance(fun_name, img, params) -> (; inputs, replaces, gates)

`run_inputs` + `run_replaces` + `run_gates` for one image, read from a FRESH copy of its `ccid.json` (an earlier step
of the same chain may have written a version since `img` was loaded). Never throws: provenance is not
worth failing a run over, so a lookup that errors answers "nothing recorded".
"""
function run_provenance(fun_name::AbstractString, img::CciaImage, params::AbstractDict)
    fresh = try _load_image(img._dir) catch; img end
    inputs = try run_inputs(fun_name, fresh, params) catch e
        @warn "run-log inputs lookup failed" fun_name exception = e
        Any[]
    end
    replaces = try run_replaces(fun_name, fresh, params) catch e
        @warn "run-log replaces lookup failed" fun_name exception = e
        nothing
    end
    gates = try run_gates(fun_name, fresh, params) catch e
        @warn "run-log gate lookup failed" fun_name exception = e
        Any[]
    end
    (; inputs, replaces, gates)
end

"""
    open_run_log!(img, fun_name, value_name, params; task_id, pool, by, caller, at) -> entries

Record that a run has STARTED, with status `"running"`. Pair with `close_run_log!` on every exit path.
`inputs` / `replaces` / `gates` are `run_provenance`'s answer for this image (see the header); empty = not stamped.

The entry is written before the work, so a run whose process is killed outright still leaves a trace —
that is the whole reason the log is not append-on-finish (see the header). `task_id` is what
`close_run_log!` and `reap_run_log!` match on; pass the scheduler's job id.
"""
function open_run_log!(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = "",
                       params = nothing; task_id::AbstractString = "", pool::AbstractString = "",
                       by::AbstractString = "", caller::AbstractDict = Dict{String,Any}(),
                       inputs::AbstractVector = Any[], replaces = nothing,
                       gates::AbstractVector = Any[], at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        e = Dict{String,Any}(
            "fun" => string(fun_name), "valueName" => string(value_name),
            "status" => RUN_LOG_RUNNING, "params" => _run_log_params(params),
            "at" => String(at), "taskId" => string(task_id), "pool" => String(pool))
        isempty(inputs) || (e["inputs"] = collect(Any, inputs))
        replaces isa AbstractDict && (e["replaces"] = replaces)
        isempty(gates) || (e["gates"] = collect(Any, gates))
        push!(entries, _run_log_stamp!(e, by, caller))
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

`env` (see `run_env_of`) is stamped as the entry's `env` when non-empty.
"""
function close_run_log!(img::CciaImage, task_id::AbstractString, status::AbstractString;
                        fun_name::AbstractString = "", value_name::AbstractString = "",
                        params = nothing, pool::AbstractString = "", by::AbstractString = "",
                        caller::AbstractDict = Dict{String,Any}(), env::AbstractDict = Dict{String,Any}(),
                        at::AbstractString = _run_log_now())
    _update_run_log!(img) do entries
        # newest-first: a re-run of the same task id should close its own entry, not an older one
        i = findlast(e -> _rl_str(e, "taskId") == string(task_id) &&
                          _rl_str(e, "status") == RUN_LOG_RUNNING, entries)
        if isnothing(i)
            e = Dict{String,Any}(
                "fun" => string(fun_name), "valueName" => string(value_name),
                "status" => string(status), "params" => _run_log_params(params),
                "at" => String(at), "taskId" => string(task_id), "pool" => String(pool))
            _run_log_stamp!(e, by, caller)
            isempty(env) || (e["env"] = Dict{String,Any}(String(k) => v for (k, v) in env))
            push!(entries, e)
        else
            e = Dict{String,Any}(String(k) => v for (k, v) in pairs(entries[i]))
            e["status"] = string(status)
            e["finishedAt"] = String(at)
            isempty(env) || (e["env"] = Dict{String,Any}(String(k) => v for (k, v) in env))
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
            "params" => _run_log_params(params), "at" => String(at), "taskId" => string(task_id))
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
            "at" => String(at), "taskId" => ""))
        entries
    end
end
