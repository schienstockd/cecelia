# ── Manual steps: a chain node that waits for the user ────────────────────────
#
# A task whose spec carries a `"manual"` block (`task_manual`) runs its `_run_task` as PREPARE, then the
# node parks at NODE_WAITING until someone continues it. Scope decides the grain: an image-scope node
# parks and continues per image; a set-scope node parks once, after the barrier, for all of them.
# Design: docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part A.
#
# The live half mirrors chain cancel (`scheduler/reporting.jl`): a process-wide registry keyed by run id,
# so `continue_chain_node!` works from the REPL, the WS handler and the runner's HTTP route alike, with
# no handle on the run. The durable half is NODE_WAITING in run.json: a parked node outlives its
# process, and resume parks it again WITHOUT re-running prepare (`_execute_image_chain!`).

# One continue attempt: the parked thread runs the check and answers on `reply` — `nothing` = continued,
# a string = refused (the node stays parked) and why.
struct _ContinueRequest
    reply::Channel{Union{String,Nothing}}
end

# (run id, node id, image uid) → the parked thread's inbox. A set-scope node parks under image uid "".
const _CHAIN_WAITERS      = Dict{Tuple{String,String,String},Channel{_ContinueRequest}}()
const _CHAIN_WAITERS_LOCK = ReentrantLock()

const _SET_WAITER = ""

"""
Park the calling chain thread until a continue passes `check()` (→ `true`) or the run is cancelled
(→ `false`). A refused continue answers its caller and keeps waiting. Polls the cancel flag because
cancel is a flag, not a signal (`is_chain_cancelled`); a parked Julia task costs no OS thread.
"""
function _park_manual!(run_id::String, node_id::String, uid::String,
                       check::Function, is_cancelled::Function)::Bool
    key   = (run_id, node_id, uid)
    inbox = Channel{_ContinueRequest}(Inf)
    lock(_CHAIN_WAITERS_LOCK) do; _CHAIN_WAITERS[key] = inbox; end
    try
        while true
            if isready(inbox)
                req = take!(inbox)
                msg = try
                    check()
                catch e
                    "check failed: $(sprint(showerror, e))"
                end
                put!(req.reply, msg)
                isnothing(msg) && return true
            elseif is_cancelled()
                return false
            else
                sleep(0.2)
            end
        end
    finally
        lock(_CHAIN_WAITERS_LOCK) do; delete!(_CHAIN_WAITERS, key); end
    end
end

"""
    continue_chain_node!(run_id, node_id; image_uids = nothing, timeout = 60.0) -> Dict

Continue a manual step parked in THIS process. `image_uids = nothing` continues every image parked at
the node (and a set-scope node, which parks once for all). Each parked thread runs its check first:

    Dict("continued" => [uids…], "refused" => Dict(uid => reason))

A set-scope node reports under its own id rather than an image uid. Both empty = nothing of that run is
parked here — the run may be on the other process, or not live at all (`continue_parked_on_disk!`).
"""
function continue_chain_node!(run_id::AbstractString, node_id::AbstractString;
                              image_uids::Union{Nothing,AbstractVector} = nothing,
                              timeout::Real = 60.0)::Dict{String,Any}
    want = isnothing(image_uids) ? nothing : Set(String.(image_uids))
    targets = lock(_CHAIN_WAITERS_LOCK) do
        [(k[3], ch) for (k, ch) in _CHAIN_WAITERS
         if k[1] == run_id && k[2] == node_id &&
            (k[3] == _SET_WAITER || isnothing(want) || k[3] in want)]
    end
    continued = String[]
    refused   = Dict{String,String}()
    for (uid, inbox) in targets
        reply = Channel{Union{String,Nothing}}(1)
        put!(inbox, _ContinueRequest(reply))
        label = uid == _SET_WAITER ? String(node_id) : uid
        if timedwait(() -> isready(reply), float(timeout)) === :timed_out
            refused[label] = "no answer from the parked step"
            continue
        end
        msg = take!(reply)
        isnothing(msg) ? push!(continued, label) : (refused[label] = msg)
    end
    Dict{String,Any}("continued" => continued, "refused" => refused)
end

"""True when some step of `run_id` is parked in this process (so a continue must go here)."""
chain_run_parked_here(run_id::AbstractString)::Bool =
    lock(_CHAIN_WAITERS_LOCK) do; any(k -> k[1] == run_id, keys(_CHAIN_WAITERS)); end

# The check for one image of a manual node: the task's own `manual_check`, on the params prepare ran with.
function _manual_check_image(run::ChainRun, task, node::ChainNode, uid::String,
                             params::Dict{String,Any})::Union{String,Nothing}
    img = init_object(run.project_uid, uid)
    img isa CciaImage || return "not an image"
    manual_check(task, img, params)
end

"""
Image scope: mark the node waiting, park, then record the user's answer. `params` are what prepare ran
with (the check sees the same); the hash is on `effective_params`, as for every node (resume skips on it).
"""
function _await_manual_image!(run::ChainRun, node::ChainNode, uid::String, task,
                              params::Dict{String,Any}, effective_params::Dict{String,Any},
                              is_cancelled::Function)
    ph = _params_hash(effective_params)
    _update_node_state!(run, uid, node.id; status = NODE_WAITING, fn = node.fn,
                        node_params = effective_params, params_hash = ph)
    ok = _park_manual!(run.id, node.id, uid,
                       () -> _manual_check_image(run, task, node, uid, params), is_cancelled)
    _update_node_state!(run, uid, node.id; status = ok ? NODE_DONE : NODE_CANCELLED, fn = node.fn,
                        node_params = effective_params, waited = true)
end

"""Set scope: every member waits on ONE park; a continue checks each member and refuses if any fails."""
function _await_manual_set!(run::ChainRun, node::ChainNode, uids::Vector{String}, task,
                            params::Dict{String,Any}, effective_params::Dict{String,Any},
                            is_cancelled::Function)
    ph = _params_hash(effective_params)
    for uid in uids
        _update_node_state!(run, uid, node.id; status = NODE_WAITING, fn = node.fn,
                            node_params = effective_params, params_hash = ph)
    end
    check = function()
        bad = String[]
        for uid in uids
            msg = _manual_check_image(run, task, node, uid, params)
            isnothing(msg) || push!(bad, "$uid: $msg")
        end
        isempty(bad) ? nothing : join(bad, "; ")
    end
    ok = _park_manual!(run.id, node.id, _SET_WAITER, check, is_cancelled)
    for uid in uids
        _update_node_state!(run, uid, node.id; status = ok ? NODE_DONE : NODE_CANCELLED, fn = node.fn,
                            node_params = effective_params, waited = true)
    end
end

"""
    continue_parked_on_disk!(proj, run_id, node_id; image_uids = nothing) -> Dict

Continue a manual step of a run that is NOT live (its process restarted while it was parked): run each
image's check against the frozen template and mark the passing ones done in run.json. Same reply shape
as `continue_chain_node!`. The caller resumes the run afterwards; the continued step is then skipped
like any done node. A set-scope node continues all its waiting images or none.
"""
function continue_parked_on_disk!(proj::CciaProject, run_id::AbstractString, node_id::AbstractString;
                                  image_uids::Union{Nothing,AbstractVector} = nothing)::Dict{String,Any}
    run  = load_chain_run(proj, String(run_id))
    i    = findfirst(n -> n.id == node_id, run.template_snapshot.nodes)
    isnothing(i) && error("run $run_id has no node '$node_id'")
    node = run.template_snapshot.nodes[i]
    task = _task_from_fun_name(node.fn)
    params = _node_run_params(task, node, Dict{String,Any}(), _ -> nothing)
    ph     = _params_hash(_apply_overrides(node.params, node.id, Dict{String,Any}()))
    waiting = [uid for uid in run.image_uids if run.image_states[uid][node.id].status == NODE_WAITING]
    isnothing(image_uids) || node.scope == CHAIN_SET ||
        (waiting = [u for u in waiting if u in Set(String.(image_uids))])
    continued = String[]
    refused   = Dict{String,String}()
    for uid in waiting
        msg = try
            _manual_check_image(run, task, node, uid, params)
        catch e
            "check failed: $(sprint(showerror, e))"
        end
        isnothing(msg) ? push!(continued, uid) : (refused[uid] = msg)
    end
    # set scope: one member failing holds them all, exactly as the live park does
    node.scope == CHAIN_SET && !isempty(refused) && (continued = String[])
    lock(run._lock) do
        for uid in continued
            st = run.image_states[uid][node.id]
            st.status = NODE_DONE; st.params_hash = ph
        end
        isempty(continued) || _save_run!(run)
    end
    Dict{String,Any}("continued" => continued, "refused" => refused)
end
