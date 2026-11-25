# ── "Queue after": a chain node that waits for a run already going ────────────
#
# The ad-hoc case a template can't express: segmentation is already running on some images, and the
# user wants clustering to start when it finishes, without waiting by the screen to press Run. The
# runner sends a two-node chain built on the spot (never saved): an `after_live` node for the running
# function, then the follow-up. The barrier, failure propagation, cancel and the run record are the
# chain executor's own, so none of that is re-implemented here.
#
# An `after_live` node starts nothing. Per image it waits for two sets of runs of its `fn`:
#   - the ones queued or running in THIS process (the scheduler's `_TASKS`, the records the duplicate-run
#     guard reads), and
#   - the task ids the asker saw live on that image (`params["waitFor"]`: uid => ids), until each has a
#     banked outcome (`banked_task_outcome`). This covers the run this process cannot see — it is on the
#     task runner while the chain runs in the backend, or the other way round — and the run that ended
#     between the click and this check: both processes bank every task's outcome.
# Then it takes their outcome: any cancelled → NODE_CANCELLED, else any failed → NODE_FAILED, else
# NODE_DONE, so the follow-up is skipped on that image exactly as after an upstream step failed inside
# the chain. An image with nothing to wait for, here or on the chain's other images, is done at once.
#
# Once everything it waits for is over, it holds while a run of `fn` is still going on any OTHER image of
# the chain (local, or an id the asker saw there), then looks again for `grace` seconds: a sequential
# sweep sends its next value only after the previous one has ended on EVERY image, so an image that
# finishes a value early sits idle for as long as the slowest one takes — far longer than any grace — and
# the follow-up belongs after the last value. That includes an image with nothing of its own live at the
# click — it may be the one that finished the current value first. The outcome is still its own runs'
# only (none → done).
#
# The node fires NO chain events (`quiet`): the run it waits for already has its own row on the task
# rail; a `chain:node:*` frame under its task id would list that work twice and bank its outcome twice.
# What the node did is in run.json.

const _AFTER_LIVE_GRACE = 2.0   # seconds; a sequential sweep's next value lands well within it

# The runs of `fun` queued or running on `uid` in this process, oldest first.
function _live_runs_of(fun::AbstractString, uid::AbstractString)::Vector{TaskRecord}
    rs = lock(_TASKS_LOCK) do
        TaskRecord[r for r in values(_TASKS)
                   if _is_active(r) && r.fun_name == fun && uid in r.image_uids]
    end
    sort!(rs; by = r -> r.queued_at)
end

# The task ids the asker saw live on `uid` (`params["waitFor"]`).
function _after_live_expected(params::AbstractDict, uid::AbstractString)::Vector{String}
    w = get(params, "waitFor", nothing)
    w isa AbstractDict || return String[]
    ids = get(w, uid, get(w, Symbol(uid), String[]))
    String[string(i) for i in ids]
end

# The node's outcome from the terminal statuses ("done" | "failed" | "cancelled") it waited for.
function _after_live_status(statuses)::ChainNodeStatus
    any(==("cancelled"), statuses) && return NODE_CANCELLED
    any(==("failed"), statuses)    && return NODE_FAILED
    NODE_DONE
end

"""
Wait for the runs of `node.fn` on `image_uid` (see the header) and record their outcome as the node's.
Polls: a `TaskRecord` has no condition to wait on, and an outcome arrives as a banked row. A chain cancel
stops the WAIT, never the run waited for, which is not the chain's to kill.
"""
function _await_live_runs!(run::ChainRun, node::ChainNode, image_uid::String,
                           effective_params::Dict{String,Any};
                           on_log::Function, is_cancelled::Function, pollint::Real = 0.5,
                           grace::Real = _AFTER_LIVE_GRACE)
    fun      = try _fun_name_from_task(_task_from_fun_name(node.fn)) catch; node.fn end
    expected = _after_live_expected(effective_params, image_uid)
    say(line) = Base.invokelatest(on_log, "[$image_uid/$(node.id)] $line")
    seen     = Dict{String,TaskRecord}()        # local runs, by id
    unseen() = String[i for i in expected if !haskey(seen, i) && isnothing(banked_task_outcome(i))]
    look!()  = (for r in _live_runs_of(fun, image_uid); seen[r.id] = r; end)
    busy()   = any(_is_active, values(seen)) || !isempty(unseen())
    peers    = String[u for u in run.image_uids if u != image_uid]
    peer_ids = unique!(reduce(vcat, (_after_live_expected(effective_params, u) for u in peers); init = String[]))
    peer_busy() = any(u -> !isempty(_live_runs_of(fun, u)), peers) ||
                  any(i -> isnothing(banked_task_outcome(i)), peer_ids)
    holding() = busy() || peer_busy()

    look!()
    if !holding()
        isempty(expected) && say("[INFO] Nothing of $(node.fn) running on this image — going on")
    else
        local_ids = [r.id for r in sort!(collect(values(seen)); by = r -> r.queued_at)]
        pending   = unseen()
        _update_node_state!(run, image_uid, node.id; status = NODE_RUNNING, quiet = true,
                            task_id = !isempty(local_ids) ? first(local_ids) :
                                      !isempty(pending)   ? first(pending)   : nothing)
        say(busy() ? "[INFO] Waiting for $(node.fn) already going on this image" :
                     "[INFO] Waiting for $(node.fn) still going on other images")
        while !is_cancelled()
            while holding() && !is_cancelled()
                sleep(pollint); look!()
            end
            t0 = time()   # a sequential sweep's next value starts only once the previous ends everywhere
            while time() - t0 < grace && !is_cancelled() && !holding()
                sleep(pollint); look!()
            end
            holding() || break
        end
    end
    # an expected id this process never saw and has no outcome for can only be one cut short by a cancel
    statuses = vcat(String[string(r.status) for r in values(seen)],
                    String[something(banked_task_outcome(i), "cancelled") for i in expected if !haskey(seen, i)])
    status = is_cancelled() ? NODE_CANCELLED : _after_live_status(statuses)
    isempty(statuses) || say("[INFO] $(node.fn) finished: $(string(status))")
    _update_node_state!(run, image_uid, node.id; status, quiet = true,
                        params_hash = status == NODE_DONE ? _params_hash(effective_params) : nothing)
    status
end
