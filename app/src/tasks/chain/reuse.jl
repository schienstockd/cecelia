# ── Reuse unchanged steps: a chain node marked `reused` instead of dispatched ─────────────────────────
#
# Opt-in per run (`run_chain(...; reuse = true)`, `ChainRequest.reuse`, the Chain page's toggle). A past
# run is reused when it ran the same task, on the same code, with the same params, read the same inputs
# and gates, and what it made is still there and still current — identity, never a content hash. Never
# silent: the node says `reused <taskId>` in run.json and the Live view, and the image's run log gets a
# `skipped` entry with reason `reused <taskId>` (`RUN_LOG_REUSED_PREFIX`). Where the rule comes from:
# docs/todo/GALAXY_LESSONS_PLAN.md → L3 / Decision 3. Design and limits: docs/SCHEDULER.md → *Reuse
# unchanged steps*.
#
# The key, all of it against the NEWEST `done` run that made the output (`_run_maker`):
#   task        — the same `fun` (a composite sharing a step is not the same task)
#   code        — the same `ceceliaVersion`, recorded on both sides; an entry without one never matches
#   params      — the sanitised params + `valueName` as `run_task` would log them (`resolve_run_params`,
#                 `_run_log_params`), compared as canonical JSON (`_gate_canon`: keys sorted at every
#                 level, since a stored entry comes back in file order)
#   inputs      — every name it reads at the same name + vN (`run_inputs`)
#   gates       — every gate it reads at the same content hash (`run_gates`)
# and "unchanged since": the output still exists on the image, no later `done` run made it (that run
# would be the maker), and nothing it read has been replaced since, directly or upstream
# (`stale_outputs_for` lists it neither stale nor unknown) — the staleness walk run in reverse.
# Never reused, because the key cannot see what changed: a task with a model picker (a model retrained
# under the same name is a new model with an old name), and a run that assumed a scale or would now
# (`env.scaleDefaulted` / `run_scale_defaults` — a calibration set since is not a run).

_reuse_params_key(params::AbstractDict, value_name::AbstractString) =
    _gate_canon(merge(_run_log_params(params), Dict{String,Any}("valueName" => String(value_name))))

# One recorded `{…}` (an input or a gate) as a comparable tuple of the named fields.
_reuse_record(d, fields) = Tuple(_rl_str(d, f) for f in fields)
_reuse_records(xs, fields) =
    sort!([_reuse_record(d, fields) for d in (xs isa AbstractVector ? xs : Any[]) if d isa AbstractDict])

const _REUSE_INPUT_FIELDS = ("param", "name", "ns", "version")
const _REUSE_GATE_FIELDS  = ("valueName", "path", "popType", "hash")

"""
    reusable_run(img, task, fun_name, run_params) -> (; entry, params, value_name) | nothing

The run-log entry of a past run on `img` that a chain node of `fun_name` with `run_params` may reuse
instead of running, or `nothing`. See the file header for the key. `params` / `value_name` are what the
run would have logged. Never throws: a lookup that errors answers "run it".
"""
function reusable_run(img::CciaImage, task, fun_name::AbstractString, run_params::Dict{String,Any})
    try
        _reusable_run(img, task, String(fun_name), run_params)
    catch e
        @warn "reuse lookup failed; running the step" fun_name exception = e
        nothing
    end
end

function _reusable_run(img::CciaImage, task, fn::String, run_params::Dict{String,Any})
    leaves = _history_leaves(fn)
    leaves === nothing && return nothing
    any(l -> l.ns == "models", values(leaves)) && return nothing
    fresh = try _load_image(img._dir) catch; img end     # an earlier step may have written since
    p = resolve_run_params(task, run_params, fresh)      # what `run_task` would run, and log, with
    vn = string(get(p, "valueName", ""))
    name, ns, into = _run_output_identity(fn, Dict{String,Any}(p), leaves)
    isempty(name) && return nothing                      # writes no named output: nothing to find
    (ns == "" && !into && name == VERSIONED_DEFAULT_VAL) && return nothing   # a NEW image's `default`
    _run_has_output(fresh, name, ns) === true || return nothing   # gone, or not checkable here
    e = _run_maker(read_run_log_history(fresh), fn, name, ns, into, Dict{String,Any}())
    e === nothing && return nothing
    tid = _rl_str(e, "taskId")
    (_rl_str(e, "fun") == fn && !isempty(tid)) || return nothing
    v = _rl_str(e, "ceceliaVersion")
    (!isempty(v) && v == (try cecelia_version() catch; "" end)) || return nothing
    env = _rl_get(e, "env", nothing)
    (env isa AbstractDict && _rl_get(env, "scaleDefaulted", nothing) !== nothing) && return nothing
    isempty(run_scale_defaults(fresh, task_requires_scale(task))) || return nothing
    _reuse_params_key(Dict{String,Any}(String(k) => x for (k, x) in _rl_get(e, "params", Dict{String,Any}())),
                      _rl_str(e, "valueName")) == _reuse_params_key(p, vn) || return nothing
    # inputs not recorded on a task that reads names = unknown, never a match
    ins = _rl_get(e, "inputs", nothing)
    (ins isa AbstractVector || !_stale_reads_names(leaves)) || return nothing
    _reuse_records(ins, _REUSE_INPUT_FIELDS) ==
        _reuse_records(run_inputs(fn, fresh, p), _REUSE_INPUT_FIELDS) || return nothing
    _reuse_records(_rl_get(e, "gates", nothing), _REUSE_GATE_FIELDS) ==
        _reuse_records(run_gates(fn, fresh, p), _REUSE_GATE_FIELDS) || return nothing
    st = stale_outputs_for(fresh)
    any(d -> _rl_str(d["run"], "taskId") == tid, vcat(st.stale, st.unknown)) && return nothing
    (; entry = e, params = p, value_name = vn)
end

# A node may be reused only when every predecessor it has in this run (incremental plot nodes aside)
# was reused too: a predecessor that ran wrote its output anew, so what this node would read is not
# what its past run read. Set-scope and "Queue after" predecessors are never reused, so their
# successors always run.
_reuse_preds_ok(states, preds, incremental_ids) =
    all(p -> p in incremental_ids || (haskey(states, p) && states[p].reused !== nothing), preds)

# Record the reuse: one `skipped` run-log entry (`reused <taskId>`) and the node :done with `reused`.
function _mark_reused!(run::ChainRun, img::CciaImage, node::ChainNode, hit, run_params::Dict{String,Any},
                       effective_params::Dict{String,Any}, on_log::Function)
    tid = _rl_str(hit.entry, "taskId")
    try
        skip_run_log!(img, node.fn, RUN_LOG_REUSED_PREFIX * tid; value_name = hit.value_name,
                      params = hit.params, by = chain_run_as(run))
    catch e
        @warn "run-log reuse entry failed" fun_name = node.fn exception = e
    end
    at = _rl_str(hit.entry, "finishedAt")
    Base.invokelatest(on_log, "REUSE [$(img.uid)/$(node.id)] $(node.fn): unchanged since run $tid" *
                              (isempty(at) ? "" : " ($at)"))
    _update_node_state!(run, img.uid, node.id; status = NODE_DONE, fn = node.fn, node_params = run_params,
                        params_hash = _params_hash(effective_params), reused = tid)
end
