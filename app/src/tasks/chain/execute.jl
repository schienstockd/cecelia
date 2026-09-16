function _update_node_state!(run::ChainRun, image_uid::String, node_id::String;
                              status::ChainNodeStatus,
                              fn::String               = "",
                              node_params::Dict{String,Any} = Dict{String,Any}(),
                              task_id     = nothing,
                              result      = nothing,
                              params_hash = nothing)
    captured_result = Ref{Any}(nothing)
    # The scheduler task id this node ran as, captured under the lock alongside the result. It goes out
    # on every event so a consumer can correlate a node with its `TaskRecord` / `GET /api/tasks` row —
    # the task console needs it to attribute a chain node's real outcome (chain runs emit no
    # `task:status` frames, so without this a finished node can only be reported as "outcome unseen").
    # "" when the node has no task id yet (skipped/cancelled before submission, set-scope/incremental
    # nodes that bypass `run_task`): consumers must treat it as "no correlation available", never assume.
    captured_task_id = Ref("")
    lock(run._lock) do
        st = run.image_states[image_uid][node_id]
        st.status = status
        isnothing(task_id)     || (st.task_id     = task_id)
        isnothing(result)      || (st.result      = result)
        isnothing(params_hash) || (st.params_hash = params_hash)
        captured_result[]  = st.result
        captured_task_id[] = something(st.task_id, "")
        _save_run!(run)
    end
    # Fire events outside the lock — handlers must not re-enter run._lock
    if status == NODE_QUEUED
        _fire_chain_event!("node:queued", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
            task_id     = captured_task_id[],
        ))
    elseif status == NODE_RUNNING
        _fire_chain_event!("node:running", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
            task_id     = captured_task_id[],
        ))
    elseif status == NODE_DONE
        _fire_chain_event!("node:done", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
            result      = captured_result[],
            task_id     = captured_task_id[],
        ))
    elseif status ∈ (NODE_FAILED, NODE_SKIPPED, NODE_CANCELLED)
        _fire_chain_event!("node:failed", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            status      = string(status),
            task_id     = captured_task_id[],
        ))
    end
end

# ── Topological sort ──────────────────────────────────────────────────────────

function _topo_sort(template::ChainTemplate)::Vector{ChainNode}
    node_map   = Dict(n.id => n for n in template.nodes)
    in_degree  = Dict(n.id => 0 for n in template.nodes)
    successors = Dict(n.id => String[] for n in template.nodes)
    for e in template.edges
        in_degree[e.to] = get(in_degree, e.to, 0) + 1
        push!(successors[e.from], e.to)
    end
    queue  = [id for (id, d) in in_degree if d == 0]
    result = ChainNode[]
    while !isempty(queue)
        id = popfirst!(queue)
        push!(result, node_map[id])
        for child in successors[id]
            in_degree[child] -= 1
            in_degree[child] == 0 && push!(queue, child)
        end
    end
    length(result) == length(template.nodes) ||
        error("Chain '$(template.name)' contains a cycle")
    result
end

# ── Barrier primitive ─────────────────────────────────────────────────────────
# Shared by Step 3 (picnic/set-scope nodes) and Step 6 (final plot nodes).
# One implementation, two consumers — not two separate barrier mechanisms.
#
# Protocol:
#   Image threads: _barrier_arrive!(run, node_id)     → signals readiness
#                  _barrier_wait_done!(run, node_id)  → waits for node to finish
#   Set-scope runner: _barrier_wait_all!(run, node_id)  → waits for N arrivals
#                     _barrier_signal_done!(run, node_id) → unblocks all images

function _barrier_arrive!(run::ChainRun, node_id::String)
    ch = get(run._barriers, node_id, nothing)
    isnothing(ch) || put!(ch, nothing)
end

function _barrier_wait_all!(run::ChainRun, node_id::String)
    ch = get(run._barriers, node_id, nothing)
    isnothing(ch) && return
    for _ in 1:length(run.image_uids)
        take!(ch)
    end
end

function _barrier_wait_done!(run::ChainRun, node_id::String)
    ch = get(run._barriers_done, node_id, nothing)
    isnothing(ch) || take!(ch)
end

function _barrier_signal_done!(run::ChainRun, node_id::String)
    ch = get(run._barriers_done, node_id, nothing)
    isnothing(ch) && return
    for _ in 1:length(run.image_uids)
        put!(ch, nothing)
    end
end

# ── Per-node progress ─────────────────────────────────────────────────────────
# A node's progress reaches a client over the chain EVENT BUS, the same carrier as `node:queued/…`,
# and is shaped into a `task:progress` frame by the one builder in `runner/chain_frames.jl`. That is
# the whole point: both processes already subscribe to that builder, so the API server and the
# detached runner emit an identical frame without either learning anything new.
#
# It is fired here rather than wired per call site because that wiring is exactly what drifted. The
# standalone path passes `on_progress` to `run_task` in TWO places (`sockets.jl` → `execute_task` and
# `runner/server.jl`), and the chain — which does not go through `execute_task` — passed it in none, so
# no chain node had ever reported progress, of any scope. The Python side emits `[PROGRESS]` and
# `run_py` routes it; only the last hop was missing.
#
# `task:status` deliberately stays off this path: a chain node emits none, or the Task Manager would
# show a second row for every node (see `subscribe_chain_frames!`). Progress attaches to the row the
# snapshot already publishes, so it adds telemetry without adding rows.
function _fire_node_progress!(run::ChainRun, node::ChainNode, image_uid::String,
                              task_id::String, n::Integer, total::Integer)
    _fire_chain_event!("node:progress", (
        run_id      = run.id,
        chain_name  = run.chain_name,
        project_uid = run.project_uid,
        image_uid   = image_uid,
        node_id     = node.id,
        fn          = node.fn,
        task_id     = task_id,
        n           = Int(n),
        total       = Int(total),
    ))
end

# ── Per-image chain execution (runs in its own OS thread) ─────────────────────

function _apply_overrides(params::Dict{String,Any}, node_id::String,
                          overrides::Dict{String,Any})::Dict{String,Any}
    node_ov = get(overrides, node_id, nothing)
    isnothing(node_ov) && return params
    merge(params, Dict{String,Any}(string(k) => v for (k, v) in node_ov))
end

function _execute_image_chain!(run::ChainRun, image_uid::String,
                                ordered_nodes::Vector{ChainNode},
                                overrides::Dict{String,Any};
                                on_log::Function,
                                is_cancelled::Function = () -> false)
    img = try
        obj = init_object(run.project_uid, image_uid)
        obj isa CciaImage || error("UID is not an image")
        obj
    catch e
        @warn "Could not load image for chain" uid=image_uid exception=e
        for node in ordered_nodes
            _update_node_state!(run, image_uid, node.id; status=NODE_FAILED, fn=node.fn)
        end
        return
    end

    # Incremental plot nodes are driven by the dedicated watcher task — not image threads.
    # Exclude them from fault-isolation checks so a failed plot never kills the pipeline.
    incremental_ids = Set(n.id for n in ordered_nodes if n.scope == CHAIN_INCREMENTAL)

    # Direct predecessors per node — for predecessor-based fault isolation. In a fan-out
    # (driftCorrect → two independent segmentations) a failed sibling must NOT skip the other
    # branch, so we gate on a node's OWN predecessors, not on "did any node anywhere fail".
    preds = Dict{String,Vector{String}}(n.id => String[] for n in ordered_nodes)
    for e in run.template_snapshot.edges
        haskey(preds, e.to) && push!(preds[e.to], e.from)
    end

    for node in ordered_nodes
        # Set-scope (picnic) node: always arrive at barrier (avoids deadlock even when
        # cancelled). If cancelled, skip waiting for the set-scope runner to finish.
        if node.scope == CHAIN_SET
            _barrier_arrive!(run, node.id)
            is_cancelled() || _barrier_wait_done!(run, node.id)
            continue
        end

        # Incremental plot node: handled by dedicated watcher task — skip here.
        if node.scope == CHAIN_INCREMENTAL
            continue
        end

        # Cancel check: mark remaining image-scope nodes and stop processing.
        if is_cancelled()
            _update_node_state!(run, image_uid, node.id; status=NODE_CANCELLED, fn=node.fn)
            continue
        end

        # Fault isolation: skip this node only if one of ITS OWN predecessors failed/was skipped —
        # so independent branches in a fan-out stay independent (a failed sibling doesn't skip us).
        # `:skipped` is in the trigger set so a failure propagates transitively down a branch
        # (pred failed → this node skipped → its successor sees a skipped pred → also skipped).
        # Topo order guarantees every predecessor's status is already set when we reach this node.
        # Incremental (plot) predecessors never gate.
        let states = run.image_states[image_uid]
            if any(p -> p ∉ incremental_ids && haskey(states, p) &&
                        states[p].status ∈ (NODE_FAILED, NODE_CANCELLED, NODE_SKIPPED),
                   preds[node.id])
                _update_node_state!(run, image_uid, node.id; status=NODE_SKIPPED, fn=node.fn)
                continue
            end
        end

        effective_params = _apply_overrides(node.params, node.id, overrides)

        # Resume: skip already-completed nodes when params are unchanged
        let st = run.image_states[image_uid][node.id]
            if st.status == NODE_DONE && st.params_hash == _params_hash(effective_params)
                continue
            end
        end

        # Resolve the task struct BEFORE announcing :queued so we can also check axis
        # applicability up-front (task requires T but this image is static → skip cleanly, don't
        # occupy a pool slot). Independent branches stay independent — :skipped is a fault-isolation
        # trigger, so downstream nodes on the same branch skip too (which is what we want: a
        # tracking-dependent branch shouldn't run on a static image).
        task_struct = try
            _task_from_fun_name(node.fn)
        catch e
            @warn "Unknown task fn in chain" fn=node.fn exception=e
            Base.invokelatest(on_log, "ERROR [$image_uid/$(node.id)] Unknown function: $(node.fn) — $(sprint(showerror, e))")
            _update_node_state!(run, image_uid, node.id;
                                status=NODE_FAILED, fn=node.fn, node_params=effective_params)
            continue
        end

        if !task_applies(task_struct, img)
            reason = task_applicability_reason(task_struct, img)
            Base.invokelatest(on_log, "SKIP [$image_uid/$(node.id)] $reason")
            _update_node_state!(run, image_uid, node.id;
                                status=NODE_SKIPPED, fn=node.fn, node_params=effective_params)
            continue
        end

        tid = gen_uid()
        # Mark :queued, not :running. Concurrency is enforced by the global scheduler
        # pool (run_task → _pool, sized from config [pools]); a node whose resource_pool
        # is saturated blocks inside run_task. The node flips to :running only when a pool
        # worker actually picks it up (`execute_task`'s `on_status`) — so the live view
        # distinguishes "waiting for a GPU slot" from "running on the GPU", and elapsed
        # time counts from the real start, not from when the image thread reached here.
        _update_node_state!(run, image_uid, node.id;
                            status=NODE_QUEUED, task_id=tid,
                            fn=node.fn, node_params=effective_params)

        # THROUGH `execute_task`, the canonical single-task pathway — the same one `handle_task_run`
        # and the runner's own task handler use. A chain node is a task; the only thing it needs that
        # a standalone run does not is the chain correlation pair, and that is a field on the shared
        # `TaskRequest` rather than a second implementation here. See docs/SCHEDULER.md.
        result = Ref{Any}(nothing)
        status::ChainNodeStatus = try
            parse_chain_node_status(string(execute_task(
                TaskRequest(; task_id      = tid,
                              fun_name     = node.fn,
                              project_uid  = run.project_uid,
                              image_uid    = image_uid,
                              pool_name    = node.resource_pool,
                              params       = effective_params,
                              chain_run_id = run.id,
                              chain_node_id = node.id);
                on_log      = line -> Base.invokelatest(on_log, "[$image_uid/$(node.id)] $line"),
                on_progress = (n, t) -> _fire_node_progress!(run, node, image_uid, tid, n, t),
                # Only `running` is mirrored into node state: `queued` is already set above, and the
                # terminal one is decided below (the chain's cancel check outranks the task's).
                on_status   = (st, _uid, _uids) -> st == "running" &&
                    _update_node_state!(run, image_uid, node.id;
                                        status=NODE_RUNNING, fn=node.fn,
                                        node_params=effective_params),
                on_result   = (_uid, meta) -> (result[] = meta))))
        catch e
            @warn "Task error in chain" uid=image_uid node=node.id exception=e
            Base.invokelatest(on_log, "ERROR [$image_uid/$(node.id)] $(sprint(showerror, e))")
            NODE_FAILED
        end

        # The CHAIN's cancel check, not the task registry's: `cancel_chain_run!` sets a chain flag, so a
        # node killed that way comes back NODE_FAILED from the task's own accounting. Keeping this
        # override is why routing through `execute_task` is behaviour-preserving here.
        final_status = is_cancelled() ? NODE_CANCELLED : status
        result = result[]
        _update_node_state!(run, image_uid, node.id;
                            fn          = node.fn,
                            node_params = effective_params,
                            status      = final_status,
                            result      = result,
                            params_hash = final_status == NODE_DONE ? _params_hash(effective_params) : nothing)
    end
end

# ── Set-scope node runner ─────────────────────────────────────────────────────
# Runs in its own OS thread (Threads.@spawn from run_chain).
# Waits until every image thread has arrived at this barrier node, then executes
# the task ONCE over the full image set, then unblocks all image threads.

function _run_set_scope_node!(run::ChainRun, node::ChainNode,
                               overrides::Dict{String,Any};
                               on_log::Function,
                               is_cancelled::Function = () -> false)
    # Block until every image thread signals arrival
    _barrier_wait_all!(run, node.id)

    if is_cancelled()
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=NODE_CANCELLED, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    effective_params = _apply_overrides(node.params, node.id, overrides)
    ph = _params_hash(effective_params)

    # Resume: skip if all images already completed this node with matching params
    if all(run.image_states[uid][node.id].status == NODE_DONE &&
           run.image_states[uid][node.id].params_hash == ph
           for uid in run.image_uids)
        _barrier_signal_done!(run, node.id)
        return
    end

    # Categorize images by upstream failure status (check before this node's state changes)
    failed_uids = Set(uid for uid in run.image_uids
                      if any(s.status ∈ (NODE_FAILED, NODE_CANCELLED)
                             for (nid, s) in run.image_states[uid] if nid != node.id))
    ok_uids = [uid for uid in run.image_uids if uid ∉ failed_uids]

    policy = node.barrier_policy

    # require_all: abort if any image failed upstream
    if policy == BARRIER_REQUIRE_ALL && !isempty(failed_uids)
        @warn "Set-scope node aborted: upstream failures under require_all policy" node=node.id failed=length(failed_uids)
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=NODE_FAILED, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    # successful_only: exclude failed images; abort if none remain
    participating_uids = policy == BARRIER_SUCCESSFUL_ONLY ? ok_uids : collect(run.image_uids)
    if isempty(participating_uids)
        @warn "Set-scope node aborted: no eligible images" node=node.id policy=policy
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=NODE_FAILED, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    # Mark excluded images :skipped (successful_only policy only)
    for uid in setdiff(run.image_uids, participating_uids)
        _update_node_state!(run, uid, node.id; status=NODE_SKIPPED, fn=node.fn)
    end

    # Load participating images
    imgs = CciaImage[]
    for uid in participating_uids
        try
            obj = init_object(run.project_uid, uid)
            obj isa CciaImage && push!(imgs, obj)
        catch e
            @warn "Could not load image for set-scope node" uid=uid node=node.id exception=e
        end
    end

    tid = gen_uid()
    # :queued, not :running — the same distinction the image-scope path makes. This node goes through
    # `run_task` now, so it waits for a slot in its `resource_pool` like any other task, and only a
    # pool worker picking it up means "running" (`execute_task`'s `on_status`, below).
    for uid in participating_uids
        _update_node_state!(run, uid, node.id;
                            status=NODE_QUEUED, task_id=tid,
                            fn=node.fn, node_params=effective_params)
    end

    task_struct = try
        _task_from_fun_name(node.fn)
    catch e
        @warn "Unknown task fn in set-scope node" fn=node.fn exception=e
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=NODE_FAILED, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    # Axis gating (set-scope): drop images that don't satisfy the task's `requires.axes` and mark
    # them :skipped. Set-scope tasks fit jointly across the vector, so a static image inside a
    # T-requiring HMM would break the fit — better to run it on the applicable subset.
    if !isempty(task_requires_axes(task_struct))
        keep_imgs = CciaImage[]
        keep_uids = String[]
        for (uid, img) in zip(participating_uids, imgs)
            if task_applies(task_struct, img)
                push!(keep_imgs, img); push!(keep_uids, uid)
            else
                Base.invokelatest(on_log, "SKIP [$uid/$(node.id)] $(task_applicability_reason(task_struct, img))")
                _update_node_state!(run, uid, node.id; status=NODE_SKIPPED, fn=node.fn, node_params=effective_params)
            end
        end
        if isempty(keep_imgs)
            axs = join(sort!(collect(task_requires_axes(task_struct))), ", ")
            Base.invokelatest(on_log, "SKIP [$(first(imgs).uid)/$(node.id)] no images satisfy required axes: $axs")
            _barrier_signal_done!(run, node.id)
            return
        end
        imgs               = keep_imgs
        participating_uids = keep_uids
    end

    # THROUGH `execute_task`, exactly like the image-scope path — it dispatches on the task's own
    # `scope`, so passing the image VECTOR lands in `_execute_set_task` and the members still get one
    # joint fit. This used to call `_run_task` directly and skipped every wrapper `run_task` provides:
    # no `<img>/logs/<fun_name>.log`, no `TaskRecord` (so nothing for the console or task-log view to
    # attach to, and the output fell through to the server's stdout), no run-log entry, no
    # `on_process` registration for cancel, and no `put!(pool.queue, job)` — so a node declaring
    # `resource_pool: "gpu"` ran UNQUEUED. Four bugs from one shortcut; the point of routing through
    # the shared executor is that there is no longer a place to take it.
    result = Ref{Any}(nothing)
    status::ChainNodeStatus = try
        parse_chain_node_status(string(execute_task(
            TaskRequest(; task_id      = tid,
                          fun_name     = node.fn,
                          project_uid  = run.project_uid,
                          image_uid    = first(imgs).uid,
                          image_uids   = participating_uids,
                          pool_name    = node.resource_pool,
                          params       = effective_params,
                          chain_run_id = run.id,
                          chain_node_id = node.id);
            on_log      = line -> Base.invokelatest(on_log, "[$(first(imgs).uid)/$(node.id)] $line"),
            on_progress = (n, t) -> _fire_node_progress!(run, node, first(imgs).uid, tid, n, t),
            # One task, N images: mirror the pool pick-up onto every participating image, so the whole
            # barrier row flips NODE_QUEUED → NODE_RUNNING together.
            on_status   = (st, _uid, _uids) -> st == "running" && for uid in participating_uids
                _update_node_state!(run, uid, node.id;
                                    status=NODE_RUNNING, fn=node.fn, node_params=effective_params)
            end,
            on_result   = (_uid, meta) -> (result[] = meta))))
    catch e
        @warn "Set-scope task error" node=node.id fn=node.fn exception=e
        Base.invokelatest(on_log, "ERROR [$(first(imgs).uid)/$(node.id)] $(sprint(showerror, e))")
        NODE_FAILED
    end
    result = result[]

    # Same rule as the image-scope path, including the chain's own cancel check outranking the task's:
    # `cancel_chain_run!` sets a chain flag, so a node killed that way comes back NODE_FAILED from the
    # task's accounting alone. This path previously had no cancel branch at all — a cancelled set node
    # was recorded as a failure.
    final_status = is_cancelled() ? NODE_CANCELLED : status
    for uid in participating_uids
        _update_node_state!(run, uid, node.id;
                            fn          = node.fn,
                            node_params = effective_params,
                            status      = final_status,
                            result      = result,
                            params_hash = final_status == NODE_DONE ? ph : nothing)
    end

    # Unblock all image threads so they can continue to downstream nodes
    _barrier_signal_done!(run, node.id)
end

# ── Incremental plot node runner ──────────────────────────────────────────────
# Runs in its own OS thread (Threads.@spawn from run_chain).
# Subscribes to "node:done" events from the upstream node, debounces, and calls
# the plot task with the current batch of completed images.  Image threads skip
# incremental nodes entirely — this watcher is the only one that updates their state.

function _run_incremental_node!(run::ChainRun, node::ChainNode,
                                 upstream_id::String,
                                 overrides::Dict{String,Any};
                                 on_log::Function,
                                 is_cancelled::Function = () -> false)
    effective_params = _apply_overrides(node.params, node.id, overrides)
    ph               = _params_hash(effective_params)

    # Skip if all images already done with matching params (resume scenario)
    if all(run.image_states[uid][node.id].status == NODE_DONE &&
           run.image_states[uid][node.id].params_hash == ph
           for uid in run.image_uids)
        return
    end

    task_struct = try
        _task_from_fun_name(node.fn)
    catch e
        @warn "Unknown fn in incremental node" fn=node.fn exception=e
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=NODE_FAILED, fn=node.fn)
        end
        return
    end

    debounce_s = Float64(get(node.params, "debounce_ms", 500)) / 1000.0
    n_total    = length(run.image_uids)
    comp_lock  = ReentrantLock()
    completed  = Dict{String, CciaImage}()   # uid => loaded image

    function run_plot!(imgs_snap::Vector{CciaImage})
        result = try
            _run_task(task_struct, imgs_snap, _flatten_sections(task_struct, effective_params);
                      on_log     = line -> Base.invokelatest(on_log, "[incr/$(node.id)] $line"),
                      on_process = _ -> nothing)
        catch e
            @warn "Incremental plot task error" node=node.id exception=e
            nothing
        end
        st = isnothing(result) ? NODE_FAILED : NODE_DONE
        ph_val = isnothing(result) ? nothing : ph
        for img in imgs_snap
            _update_node_state!(run, img.uid, node.id;
                                fn=node.fn, node_params=effective_params,
                                status=st, result=result, params_hash=ph_val)
        end
    end

    # Pre-populate with images whose upstream is already :done (resume scenario)
    for uid in run.image_uids
        if run.image_states[uid][upstream_id].status == NODE_DONE
            img = try
                o = init_object(run.project_uid, uid)
                o isa CciaImage ? o : nothing
            catch; nothing; end
            isnothing(img) || (completed[uid] = img)
        end
    end

    if length(completed) == n_total
        # All upstream already done — fire once and exit
        run_plot!(collect(values(completed)))
        return
    end

    # Subscribe to upstream completions and collect via Channel
    event_ch    = Channel{Pair{String,CciaImage}}(n_total + 1)
    all_done_ch = Channel{Nothing}(1)

    handler = payload -> begin
        payload.run_id   == run.id      || return
        payload.node_id  == upstream_id || return
        uid = payload.image_uid
        uid ∈ run.image_uids                         || return
        lock(comp_lock) do; uid ∈ keys(completed); end && return  # dedup
        img = try
            o = init_object(run.project_uid, uid)
            o isa CciaImage ? o : nothing
        catch; nothing; end
        isnothing(img) && return
        put!(event_ch, uid => img)
    end

    subscribe_chain_events!("node:done", handler)

    # Drain loop — collect events with debounce-style windowing
    last_fired_n = length(completed)
    while true
        is_cancelled() && break
        got = timedwait(() -> isready(event_ch),
                        max(debounce_s, 0.002); pollint=0.005)
        if got == :ok
            while isready(event_ch)
                uid, img = take!(event_ch)
                lock(comp_lock) do; completed[uid] = img; end
            end
        else
            # Debounce window closed — fire if we have unfired completions
            n_now = lock(comp_lock) do; length(completed); end
            if n_now > last_fired_n
                run_plot!(lock(comp_lock) do; collect(values(completed)); end)
                last_fired_n = n_now
            end
        end
        lock(comp_lock) do; length(completed); end == n_total && break
    end

    # Final fire if last batch arrived before debounce timeout
    n_final = lock(comp_lock) do; length(completed); end
    if n_final > last_fired_n
        run_plot!(lock(comp_lock) do; collect(values(completed)); end)
    end

    unsubscribe_chain_events!("node:done", handler)
end

# ── Resume helpers ────────────────────────────────────────────────────────────

# ── Forward references inside one template ────────────────────────────────────
# A chain that TRAINS a model and then segments with it names something that is not in the vault at
# author time. Per-node param validation only knows the vault — the `model` select's options are
# injected from it by `_inject_dynamic_options!` — so it read a forward reference as a typo and
# rejected the whole template. Only the template can tell the two apart, which is why this lives here
# and is handed down as `validate_params(...; extra_options)`.
#
# ANCESTORS only, deliberately, not the whole template: a node segmenting with a model trained LATER
# (or on a parallel branch that has not joined yet) is a real wiring mistake, and accepting it here
# would only defer it to a mid-run failure with the GPU already occupied.
#
# `models` is the only namespace that needs this. The others are consumed by `valueNameSelection`
# params, which carry no fixed option list to fail against.

function _ancestors(template::ChainTemplate, node_id::String)::Set{String}
    pred = Dict{String,Vector{String}}()
    for e in template.edges
        push!(get!(pred, e.to, String[]), e.from)
    end
    out   = Set{String}()
    queue = String[node_id]
    while !isempty(queue)
        n = popfirst!(queue)
        for pr in get(pred, n, String[])
            if pr ∉ out
                push!(out, pr)
                push!(queue, pr)
            end
        end
    end
    out
end

function _chain_produced_names(template::ChainTemplate, node_id::String)::Set{String}
    ups = _ancestors(template, node_id)
    out = Set{String}()
    isempty(ups) && return out
    for n in template.nodes
        n.id in ups || continue
        task = try
            _task_from_fun_name(n.fn)
        catch
            continue        # unknown fn is reported by validate_chain_template itself
        end
        spec = _task_spec(task)
        isnothing(spec) && continue
        for pspec in get(spec, "params", [])
            pspec isa AbstractDict || continue
            string(get(pspec, "namespace", "")) == "models" || continue
            key  = string(get(pspec, "key", ""))
            stem = strip(string(get(n.params, key, get(pspec, "default", ""))))
            isempty(stem) && continue
            # The stem the producer holds vs the filename the consumer's select carries — one helper,
            # never an inline `* ".pt"`.
            push!(out, flow_model_filename(stem))
        end
    end
    out
end

# All nodes reachable downstream of `node_id` (successors, transitively) — NOT including itself.
function _descendants(template::ChainTemplate, node_id::String)::Set{String}
    succ = Dict{String,Vector{String}}()
    for e in template.edges
        push!(get!(succ, e.from, String[]), e.to)
    end
    out   = Set{String}()
    queue = String[node_id]
    while !isempty(queue)
        n = popfirst!(queue)
        for c in get(succ, n, String[])
            if c ∉ out
                push!(out, c)
                push!(queue, c)
            end
        end
    end
    out
end

# UML start-dot pruning: restrict a template to the nodes reachable from its `start_targets` (their
# inclusive descendants). Targets become effective roots — edges into them from now-excluded nodes are
# dropped, so a mid-chain start runs from there on, and disconnected branches (unreachable from the
# start dot) drop out as drafts. No start targets ⇒ the template is returned unchanged (run all).
function _prune_to_start(template::ChainTemplate)::ChainTemplate
    isempty(template.start_targets) && return template
    ids  = Set(n.id for n in template.nodes)
    exec = Set{String}()
    for t in template.start_targets
        t in ids || continue                       # stale target (node since deleted) — ignore
        push!(exec, t)
        union!(exec, _descendants(template, t))
    end
    # All targets stale ⇒ fall back to running the whole chain rather than erroring on an empty run
    # (a dangling start dot shouldn't brick the run — better to run everything and let the user notice).
    if isempty(exec)
        @warn "Chain start dot targets no existing nodes — running the whole chain" targets=template.start_targets
        return ChainTemplate(template.name, template.nodes, template.edges, String[])
    end
    nodes = [n for n in template.nodes if n.id in exec]
    edges = [e for e in template.edges if e.from in exec && e.to in exec]
    ChainTemplate(template.name, nodes, edges, String[])   # start consumed into the node set
end

# Explicit "start node" for resume: force `start_node` and everything downstream back to :pending
# across ALL images, even nodes that are :done with matching params (which `_reset_stale_nodes!`
# would otherwise keep and skip). This is the whiteboard "resume from here" — re-run a completed
# section (e.g. measurements) without touching upstream nodes, which stay :done and are skipped.
function _force_restart_from!(run::ChainRun, start_node::String)
    targets = _descendants(run.template_snapshot, start_node)
    push!(targets, start_node)
    changed = false
    for uid in run.image_uids, nid in targets
        haskey(run.image_states[uid], nid) || continue
        st = run.image_states[uid][nid]
        if st.status != NODE_PENDING
            st.status=NODE_PENDING; st.params_hash = nothing
            st.result = nothing;  st.task_id     = nothing
            changed   = true
        end
    end
    changed && _save_run!(run)
end

# Pre-pass before re-running a chain: resets nodes that need re-execution.
# Handles crash recovery (:running → :failed), retries (:failed/:skipped/:cancelled → :pending),
# params staleness (:done with changed params → :pending), and propagates dirtiness downstream.
function _reset_stale_nodes!(run::ChainRun, overrides::Dict{String,Any},
                              ordered_nodes::Vector{ChainNode})
    preds = Dict{String,Vector{String}}(n.id => String[] for n in ordered_nodes)
    for e in run.template_snapshot.edges
        push!(preds[e.to], e.from)
    end

    stale_set   = Set{Tuple{String,String}}()  # (node_id, uid)
    any_changed = false

    for node in ordered_nodes
        effective_params = _apply_overrides(node.params, node.id, overrides)
        ph = _params_hash(effective_params)
        for uid in run.image_uids
            st = run.image_states[uid][node.id]
            if st.status ∈ (NODE_RUNNING, NODE_QUEUED)   # crash recovery — never finished
                st.status=NODE_FAILED
                any_changed = true
            end
            retry        = st.status ∈ (NODE_FAILED, NODE_SKIPPED, NODE_CANCELLED)
            params_stale = st.status == NODE_DONE && st.params_hash != ph
            pred_stale   = any((p, uid) ∈ stale_set for p in preds[node.id])
            if retry || params_stale || pred_stale
                push!(stale_set, (node.id, uid))
                if st.status != NODE_PENDING
                    st.status=NODE_PENDING
                    st.params_hash = nothing
                    st.result      = nothing
                    st.task_id     = nothing
                    any_changed    = true
                end
            end
        end
    end

    any_changed && _save_run!(run)
end
