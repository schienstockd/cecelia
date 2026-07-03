import SHA

# ── Chain data model: template vs. run ────────────────────────────────────────
#
# Two distinct artifacts:
#
#   ChainTemplate  — reusable, no images baked in. Lives at
#                    <project>/chains/<name>.json. Editing a template never
#                    retroactively changes a completed run.
#
#   ChainRun       — created when a template is applied to a set of images.
#                    Stores a FROZEN COPY of the template at run time (not a
#                    pointer to the template file). Per-image per-node state
#                    is persisted to <project>/chains/runs/<run_id>/run.json
#                    after every node completion.

# ── Template ──────────────────────────────────────────────────────────────────

struct ChainNode
    id::String
    fn::String              # "category.taskName" — same key as the task registry
    scope::String           # "image" (default) | "set" (picnic) | "incremental" (plot watcher)
    params::Dict{String,Any}
    # barrier_policy — only meaningful for scope="set" nodes:
    #   "all"             run with every image regardless of upstream failures (default)
    #   "require_all"     abort if any image failed upstream; all images → :failed
    #   "successful_only" run with only upstream-successful images; failed ones → :skipped
    barrier_policy::String
    # resource_pool — name of the global scheduler pool this node runs in (see scheduler.jl
    # _POOLS, sized from config.toml [pools]). "" falls back to the task JSON's resource_pool,
    # else "default". A pool with limit 1 (e.g. "gpu") serialises that node across the process.
    resource_pool::String
end

# Default scope for a task fun_name, read from its JSON spec's "scope" field. The task JSON is
# the single source of truth for scope: set-scope (picnic) tasks like behaviour.hmm and
# clustTracks.cluster declare "scope": "set" there, so a node built from them — in the REPL or
# dragged onto the whiteboard — becomes a picnic node without the author restating it. Unknown
# fn or specless task → "image".
function _task_default_scope(fn::String)::String
    try
        task_scope(_task_from_fun_name(fn))   # reads the spec's "scope" field (task.jl)
    catch
        "image"
    end
end

# An empty scope means "inherit from the task spec" (see _task_default_scope). An explicit
# non-empty scope always wins, so a caller can still force image-scope on a set task if needed.
ChainNode(; id, fn, scope="", params=Dict{String,Any}(),
            barrier_policy="all", resource_pool="") =
    ChainNode(id, fn, isempty(scope) ? _task_default_scope(fn) : scope,
              params, barrier_policy, resource_pool)

struct ChainEdge
    from::String
    to::String
end

struct ChainTemplate
    name::String
    nodes::Vector{ChainNode}
    edges::Vector{ChainEdge}
end

# ── Run record ────────────────────────────────────────────────────────────────

mutable struct ImageNodeState
    status::Symbol                          # :pending | :running | :done | :failed | :cancelled | :skipped
    task_id::Union{String,Nothing}
    result::Union{Dict{String,Any},Nothing}
    params_hash::Union{String,Nothing}      # sha256 of effective params — set on :done, used for resume skip
end

ImageNodeState() = ImageNodeState(:pending, nothing, nothing, nothing)

mutable struct ChainRun
    id::String
    chain_name::String
    project_uid::String
    image_uids::Vector{String}
    template_snapshot::ChainTemplate        # in-memory only — needed during execution
    template_hash::String                   # sha256 hex — pointer to cache entry on disk
    image_states::Dict{String,Dict{String,ImageNodeState}}  # uid => node_id => state
    created_at::Float64
    _dir::String                            # <project>/chains/runs/<run_id>/
    _lock::ReentrantLock                    # guards image_states + disk writes
    _barriers::Dict{String,Channel{Nothing}} # node_id => arrive channel (Step 3)
    _barriers_done::Dict{String,Channel{Nothing}} # node_id => done channel (Step 3)
end
# NOTE: resource-pool concurrency is NOT a per-run concern. Every node runs via
# `run_task`, which routes through the global scheduler pools (`_POOLS` in
# scheduler.jl, sized from the [pools] section of config.toml). A `gpu` pool with
# limit 1 serialises GPU work across the whole process — chain nodes and module-page
# tasks alike. There is intentionally no second, per-run pool layer.

# ── Filesystem helpers ────────────────────────────────────────────────────────

_chains_dir(proj::CciaProject)::String         = joinpath(proj.root, "chains")
_template_path(proj::CciaProject, name::String) = joinpath(_chains_dir(proj), "$name.json")
_runs_dir(proj::CciaProject)::String           = joinpath(_chains_dir(proj), "runs")
_cache_dir(proj::CciaProject)::String          = joinpath(_chains_dir(proj), ".cache")

# ── Template I/O ──────────────────────────────────────────────────────────────

function _node_from_dict(d)::ChainNode
    ChainNode(
        string(get(d, "id", get(d, :id, ""))),
        string(get(d, "fn", get(d, :fn, ""))),
        let sc = string(get(d, "scope", get(d, :scope, "")))
            isempty(sc) ? _task_default_scope(string(get(d, "fn", get(d, :fn, "")))) : sc
        end,
        Dict{String,Any}(string(k) => v
                         for (k, v) in get(d, "params", get(d, :params, Dict()))),
        string(get(d, "barrier_policy", get(d, :barrier_policy, "all"))),
        string(get(d, "resource_pool", get(d, :resource_pool, ""))),
    )
end

function _edge_from_dict(d)::ChainEdge
    ChainEdge(
        string(get(d, "from", get(d, :from, ""))),
        string(get(d, "to", get(d, :to, ""))),
    )
end

"""
Load a chain template from `<project>/chains/<name>.json`.
"""
function load_chain_template(proj::CciaProject, name::String)::ChainTemplate
    path = _template_path(proj, name)
    isfile(path) || error("Chain template not found: $path")
    raw  = JSON3.read(read(path, String))
    ChainTemplate(
        string(get(raw, :name, name)),
        [_node_from_dict(n) for n in get(raw, :nodes, [])],
        [_edge_from_dict(e) for e in get(raw, :edges, [])],
    )
end

"""
Write a chain template to `<project>/chains/<name>.json`.
Creates the chains/ directory if needed.
"""
function save_chain_template!(proj::CciaProject, t::ChainTemplate)::ChainTemplate
    mkpath(_chains_dir(proj))
    open(_template_path(proj, t.name), "w") do io
        JSON3.write(io, (;
            name  = t.name,
            nodes = [(; id=n.id, fn=n.fn, scope=n.scope, params=n.params,
                       barrier_policy=n.barrier_policy, resource_pool=n.resource_pool)
                     for n in t.nodes],
            edges = [(; from=e.from, to=e.to) for e in t.edges],
        ))
    end
    t
end

# ── Template content cache ─────────────────────────────────────────────────────
# Templates are stored once under chains/.cache/<sha256>.json.
# Run records reference the hash — editing a template after a run has started
# produces a new hash, leaving the old cache entry (and run records) untouched.

function _template_json(t::ChainTemplate)::String
    JSON3.write((;
        name  = t.name,
        nodes = [(; id=n.id, fn=n.fn, scope=n.scope, params=n.params,
                   barrier_policy=n.barrier_policy, resource_pool=n.resource_pool)
                 for n in t.nodes],
        edges = [(; from=e.from, to=e.to) for e in t.edges],
    ))
end

function _template_hash(t::ChainTemplate)::String
    bytes2hex(SHA.sha256(_template_json(t)))
end

function _params_hash(params::Dict{String,Any})::String
    sorted_pairs = [(k, params[k]) for k in sort(collect(keys(params)))]
    bytes2hex(SHA.sha256(JSON3.write(sorted_pairs)))
end

"""
Write a template to the content cache if not already present.
Returns the sha256 hex hash.
"""
function _cache_template!(proj::CciaProject, t::ChainTemplate)::String
    hash = _template_hash(t)
    path = joinpath(_cache_dir(proj), "$hash.json")
    if !isfile(path)
        mkpath(_cache_dir(proj))
        write(path, _template_json(t))
    end
    hash
end

"""
Load a template from the content cache by hash.
Throws if the cache entry is missing.
"""
function load_template_from_cache(proj::CciaProject, hash::String)::ChainTemplate
    path = joinpath(_cache_dir(proj), "$hash.json")
    isfile(path) || error("Template cache entry not found: $hash")
    raw = JSON3.read(read(path, String))
    ChainTemplate(
        string(get(raw, :name, "")),
        [_node_from_dict(n) for n in get(raw, :nodes, [])],
        [_edge_from_dict(e) for e in get(raw, :edges, [])],
    )
end

# ── Run record I/O ────────────────────────────────────────────────────────────

function _save_run!(run::ChainRun)
    mkpath(run._dir)
    states = Dict{String,Any}(
        uid => Dict{String,Any}(
            nid => Dict{String,Any}(
                "status"      => string(st.status),
                "task_id"     => st.task_id,
                "result"      => st.result,
                "params_hash" => st.params_hash,
            )
            for (nid, st) in node_map
        )
        for (uid, node_map) in run.image_states
    )
    open(joinpath(run._dir, "run.json"), "w") do io
        JSON3.write(io, (;
            id             = run.id,
            chain_name     = run.chain_name,
            project_uid    = run.project_uid,
            image_uids     = run.image_uids,
            template_hash  = run.template_hash,
            image_states   = states,
            created_at     = run.created_at,
        ))
    end
end

function _update_node_state!(run::ChainRun, image_uid::String, node_id::String;
                              status::Symbol,
                              fn::String               = "",
                              node_params::Dict{String,Any} = Dict{String,Any}(),
                              task_id     = nothing,
                              result      = nothing,
                              params_hash = nothing)
    captured_result = Ref{Any}(nothing)
    lock(run._lock) do
        st = run.image_states[image_uid][node_id]
        st.status = status
        isnothing(task_id)     || (st.task_id     = task_id)
        isnothing(result)      || (st.result      = result)
        isnothing(params_hash) || (st.params_hash = params_hash)
        captured_result[] = st.result
        _save_run!(run)
    end
    # Fire events outside the lock — handlers must not re-enter run._lock
    if status == :queued
        _fire_chain_event!("node:queued", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
        ))
    elseif status == :running
        _fire_chain_event!("node:running", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
        ))
    elseif status == :done
        _fire_chain_event!("node:done", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            params      = node_params,
            result      = captured_result[],
        ))
    elseif status ∈ (:failed, :skipped, :cancelled)
        _fire_chain_event!("node:failed", (
            run_id      = run.id,
            chain_name  = run.chain_name,
            project_uid = run.project_uid,
            image_uid   = image_uid,
            node_id     = node_id,
            fn          = fn,
            status      = string(status),
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
                                on_status_change::Function,
                                is_cancelled::Function = () -> false)
    img = try
        obj = init_object(run.project_uid, image_uid)
        obj isa CciaImage || error("UID is not an image")
        obj
    catch e
        @warn "Could not load image for chain" uid=image_uid exception=e
        for node in ordered_nodes
            _update_node_state!(run, image_uid, node.id; status=:failed, fn=node.fn)
        end
        return
    end

    # Incremental plot nodes are driven by the dedicated watcher task — not image threads.
    # Exclude them from fault-isolation checks so a failed plot never kills the pipeline.
    incremental_ids = Set(n.id for n in ordered_nodes if n.scope == "incremental")

    for node in ordered_nodes
        # Set-scope (picnic) node: always arrive at barrier (avoids deadlock even when
        # cancelled). If cancelled, skip waiting for the set-scope runner to finish.
        if node.scope == "set"
            _barrier_arrive!(run, node.id)
            is_cancelled() || _barrier_wait_done!(run, node.id)
            continue
        end

        # Incremental plot node: handled by dedicated watcher task — skip here.
        if node.scope == "incremental"
            continue
        end

        # Cancel check: mark remaining image-scope nodes and stop processing.
        if is_cancelled()
            _update_node_state!(run, image_uid, node.id; status=:cancelled, fn=node.fn)
            continue
        end

        # Fault isolation: skip downstream nodes if any earlier non-incremental node failed.
        if any(st.status ∈ (:failed, :cancelled)
               for (nid, st) in run.image_states[image_uid] if nid ∉ incremental_ids)
            _update_node_state!(run, image_uid, node.id; status=:skipped, fn=node.fn)
            continue
        end

        effective_params = _apply_overrides(node.params, node.id, overrides)

        # Resume: skip already-completed nodes when params are unchanged
        let st = run.image_states[image_uid][node.id]
            if st.status == :done && st.params_hash == _params_hash(effective_params)
                continue
            end
        end

        tid = gen_uid()
        # Mark :queued, not :running. Concurrency is enforced by the global scheduler
        # pool (run_task → _pool, sized from config [pools]); a node whose resource_pool
        # is saturated blocks inside run_task. The node flips to :running only when a pool
        # worker actually picks it up (via on_status_change below) — so the live view
        # distinguishes "waiting for a GPU slot" from "running on the GPU", and elapsed
        # time counts from the real start, not from when the image thread reached here.
        _update_node_state!(run, image_uid, node.id;
                            status=:queued, task_id=tid,
                            fn=node.fn, node_params=effective_params)

        task_struct = try
            _task_from_fun_name(node.fn)
        catch e
            @warn "Unknown task fn in chain" fn=node.fn exception=e
            Base.invokelatest(on_log, "ERROR [$image_uid/$(node.id)] Unknown function: $(node.fn) — $(sprint(showerror, e))")
            _update_node_state!(run, image_uid, node.id;
                                status=:failed, fn=node.fn)
            continue
        end

        result = try
            run_task(task_struct, img, effective_params;
                     task_id          = tid,
                     pool_name        = node.resource_pool,
                     chain_run_id     = run.id,
                     on_log           = line -> Base.invokelatest(on_log, "[$image_uid/$(node.id)] $line"),
                     on_status_change = rec -> begin
                         # Mirror the pool worker picking up the job into the node state,
                         # so :queued → :running reflects the real GPU-slot acquisition.
                         if rec.status === :running
                             _update_node_state!(run, image_uid, node.id;
                                                 status=:running, fn=node.fn,
                                                 node_params=effective_params)
                         end
                         Base.invokelatest(on_status_change, rec)
                     end)
        catch e
            @warn "Task error in chain" uid=image_uid node=node.id exception=e
            Base.invokelatest(on_log, "ERROR [$image_uid/$(node.id)] $(sprint(showerror, e))")
            nothing
        end

        # Cancelled mid-run (subprocess killed by cancel_chain_run!) → :cancelled, not :failed.
        final_status = is_cancelled()   ? :cancelled :
                       isnothing(result) ? :failed    : :done
        _update_node_state!(run, image_uid, node.id;
                            fn          = node.fn,
                            node_params = effective_params,
                            status      = final_status,
                            result      = result,
                            params_hash = final_status == :done ? _params_hash(effective_params) : nothing)
    end
end

# ── Set-scope node runner ─────────────────────────────────────────────────────
# Runs in its own OS thread (Threads.@spawn from run_chain).
# Waits until every image thread has arrived at this barrier node, then executes
# the task ONCE over the full image set, then unblocks all image threads.

function _run_set_scope_node!(run::ChainRun, node::ChainNode,
                               overrides::Dict{String,Any};
                               on_log::Function,
                               on_status_change::Function,
                               is_cancelled::Function = () -> false)
    # Block until every image thread signals arrival
    _barrier_wait_all!(run, node.id)

    if is_cancelled()
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=:cancelled, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    effective_params = _apply_overrides(node.params, node.id, overrides)
    ph = _params_hash(effective_params)

    # Resume: skip if all images already completed this node with matching params
    if all(run.image_states[uid][node.id].status == :done &&
           run.image_states[uid][node.id].params_hash == ph
           for uid in run.image_uids)
        _barrier_signal_done!(run, node.id)
        return
    end

    # Categorize images by upstream failure status (check before this node's state changes)
    failed_uids = Set(uid for uid in run.image_uids
                      if any(s.status ∈ (:failed, :cancelled)
                             for (nid, s) in run.image_states[uid] if nid != node.id))
    ok_uids = [uid for uid in run.image_uids if uid ∉ failed_uids]

    policy = node.barrier_policy

    # require_all: abort if any image failed upstream
    if policy == "require_all" && !isempty(failed_uids)
        @warn "Set-scope node aborted: upstream failures under require_all policy" node=node.id failed=length(failed_uids)
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=:failed, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    # successful_only: exclude failed images; abort if none remain
    participating_uids = policy == "successful_only" ? ok_uids : collect(run.image_uids)
    if isempty(participating_uids)
        @warn "Set-scope node aborted: no eligible images" node=node.id policy=policy
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=:failed, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    # Mark excluded images :skipped (successful_only policy only)
    for uid in setdiff(run.image_uids, participating_uids)
        _update_node_state!(run, uid, node.id; status=:skipped, fn=node.fn)
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
    for uid in participating_uids
        _update_node_state!(run, uid, node.id;
                            status=:running, task_id=tid,
                            fn=node.fn, node_params=effective_params)
    end

    task_struct = try
        _task_from_fun_name(node.fn)
    catch e
        @warn "Unknown task fn in set-scope node" fn=node.fn exception=e
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=:failed, fn=node.fn)
        end
        _barrier_signal_done!(run, node.id)
        return
    end

    result = try
        _run_task(task_struct, imgs,
                  merge(effective_params, Dict("_task_id" => tid));
                  on_log      = line -> Base.invokelatest(on_log, "[set/$(node.id)] $line"),
                  on_process  = _ -> nothing)
    catch e
        @warn "Set-scope task error" node=node.id fn=node.fn exception=e
        nothing
    end

    final_status = isnothing(result) ? :failed : :done
    for uid in participating_uids
        _update_node_state!(run, uid, node.id;
                            fn          = node.fn,
                            node_params = effective_params,
                            status      = final_status,
                            result      = result,
                            params_hash = isnothing(result) ? nothing : ph)
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
                                 on_status_change::Function,
                                 is_cancelled::Function = () -> false)
    effective_params = _apply_overrides(node.params, node.id, overrides)
    ph               = _params_hash(effective_params)

    # Skip if all images already done with matching params (resume scenario)
    if all(run.image_states[uid][node.id].status == :done &&
           run.image_states[uid][node.id].params_hash == ph
           for uid in run.image_uids)
        return
    end

    task_struct = try
        _task_from_fun_name(node.fn)
    catch e
        @warn "Unknown fn in incremental node" fn=node.fn exception=e
        for uid in run.image_uids
            _update_node_state!(run, uid, node.id; status=:failed, fn=node.fn)
        end
        return
    end

    debounce_s = Float64(get(node.params, "debounce_ms", 500)) / 1000.0
    n_total    = length(run.image_uids)
    comp_lock  = ReentrantLock()
    completed  = Dict{String, CciaImage}()   # uid => loaded image

    function run_plot!(imgs_snap::Vector{CciaImage})
        result = try
            _run_task(task_struct, imgs_snap, effective_params;
                      on_log     = line -> Base.invokelatest(on_log, "[incr/$(node.id)] $line"),
                      on_process = _ -> nothing)
        catch e
            @warn "Incremental plot task error" node=node.id exception=e
            nothing
        end
        st = isnothing(result) ? :failed : :done
        ph_val = isnothing(result) ? nothing : ph
        for img in imgs_snap
            _update_node_state!(run, img.uid, node.id;
                                fn=node.fn, node_params=effective_params,
                                status=st, result=result, params_hash=ph_val)
        end
    end

    # Pre-populate with images whose upstream is already :done (resume scenario)
    for uid in run.image_uids
        if run.image_states[uid][upstream_id].status == :done
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
            if st.status ∈ (:running, :queued)   # crash recovery — never finished
                st.status   = :failed
                any_changed = true
            end
            retry        = st.status ∈ (:failed, :skipped, :cancelled)
            params_stale = st.status == :done && st.params_hash != ph
            pred_stale   = any((p, uid) ∈ stale_set for p in preds[node.id])
            if retry || params_stale || pred_stale
                push!(stale_set, (node.id, uid))
                if st.status != :pending
                    st.status      = :pending
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

"""
Load a previously-created chain run from disk by its run ID.
The associated template is restored from the content cache.
"""
function load_chain_run(proj::CciaProject, run_id::String)::ChainRun
    run_dir = joinpath(_runs_dir(proj), run_id)
    path    = joinpath(run_dir, "run.json")
    isfile(path) || error("Chain run not found: $run_id")

    raw           = JSON3.read(read(path, String))
    template_hash = string(raw[:template_hash])
    template      = load_template_from_cache(proj, template_hash)
    image_uids    = [string(u) for u in raw[:image_uids]]

    image_states = Dict{String,Dict{String,ImageNodeState}}()
    for (uid_sym, node_map) in raw[:image_states]
        uid = string(uid_sym)
        image_states[uid] = Dict{String,ImageNodeState}()
        for (nid_sym, st_raw) in node_map
            nid = string(nid_sym)
            image_states[uid][nid] = ImageNodeState(
                Symbol(string(st_raw[:status])),
                !isnothing(get(st_raw, :task_id,     nothing)) ? string(st_raw[:task_id])     : nothing,
                !isnothing(get(st_raw, :result,      nothing)) ?
                    Dict{String,Any}(string(k) => v for (k, v) in st_raw[:result]) : nothing,
                !isnothing(get(st_raw, :params_hash, nothing)) ? string(st_raw[:params_hash]) : nothing,
            )
        end
    end

    n             = length(image_uids)
    barriers      = Dict{String,Channel{Nothing}}()
    barriers_done = Dict{String,Channel{Nothing}}()
    for node in template.nodes
        if node.scope == "set"
            barriers[node.id]      = Channel{Nothing}(n)
            barriers_done[node.id] = Channel{Nothing}(n)
        end
    end

    ChainRun(
        string(raw[:id]),
        string(raw[:chain_name]),
        string(raw[:project_uid]),
        image_uids,
        template,
        template_hash,
        image_states,
        Float64(raw[:created_at]),
        run_dir,
        ReentrantLock(),
        barriers,
        barriers_done,
    )
end

# ── run_chain — the public REPL entrypoint ────────────────────────────────────

"""
Run a named chain against a list of image UIDs.

Each image progresses through the chain independently on its own OS thread.
A pool worker slot is the only gate between nodes — image A can be on node 2
while image B is still waiting for a pool slot on node 1.

One image erroring at any node does not affect other images (fault isolation).

Resource-pool concurrency is enforced by the global scheduler pools (`_POOLS` in
scheduler.jl, sized from config.toml `[pools]`) — `run_chain` takes no pool argument.
A pool with limit 1 (e.g. `gpu`) serialises that node's work across the whole process.

## Fresh run
Template is loaded from `<project>/chains/<name>.json` and `chain` must be given.
A frozen copy is stored in the run record — editing the template after the run
has started does not change what that run is understood to have done.

    run_chain(proj, ["uid1", "uid2"]; chain="my-chain")

## Resume
Pass an existing `run_id` instead of a `chain` name. The run is loaded from disk,
stale/failed nodes are reset to `:pending`, and already-done nodes with unchanged
params are skipped. `image_uids` is ignored in resume mode.

    run_chain(proj, String[]; run_id="abc12345")

overrides: node_id => param overrides merged on top of template params.
"""
function run_chain(proj::CciaProject, image_uids::Vector{String};
                   chain::String                  = "",
                   run_id::Union{String,Nothing}  = nothing,
                   overrides::Dict{String,Any}    = Dict{String,Any}(),
                   on_log::Function               = line -> println(line),
                   on_status_change::Function     = _ -> nothing,
                   on_cancel_check::Function      = _ -> false)::ChainRun

    if !isnothing(run_id)
        # Resume: restore run from disk, reset stale/failed nodes, keep :done ones
        run           = load_chain_run(proj, run_id)
        ordered_nodes = _topo_sort(run.template_snapshot)
        _reset_stale_nodes!(run, overrides, ordered_nodes)
    else
        isempty(chain) && error("run_chain requires `chain` name when `run_id` is not given")
        isempty(image_uids) && error("run_chain requires at least one image UID")

        template      = load_chain_template(proj, chain)
        ordered_nodes = _topo_sort(template)

        image_states  = Dict{String,Dict{String,ImageNodeState}}(
            uid => Dict{String,ImageNodeState}(n.id => ImageNodeState()
                                               for n in template.nodes)
            for uid in image_uids
        )

        n             = length(image_uids)
        barriers      = Dict{String,Channel{Nothing}}()
        barriers_done = Dict{String,Channel{Nothing}}()
        for node in template.nodes
            if node.scope == "set"
                barriers[node.id]      = Channel{Nothing}(n)
                barriers_done[node.id] = Channel{Nothing}(n)
            end
        end

        hash    = _cache_template!(proj, template)
        new_id  = gen_uid()
        run_dir = joinpath(_runs_dir(proj), new_id)
        run     = ChainRun(
            new_id, template.name, proj.uid, image_uids, template, hash,
            image_states, time(), run_dir, ReentrantLock(),
            barriers, barriers_done,
        )
        _save_run!(run)
    end

    # Build predecessor map for incremental nodes (upstream node_id per incremental node)
    direct_preds = Dict{String, Vector{String}}(n.id => String[] for n in ordered_nodes)
    for e in run.template_snapshot.edges
        push!(direct_preds[e.to], e.from)
    end
    incr_upstream = Dict{String, String}(
        n.id => first(direct_preds[n.id])
        for n in ordered_nodes
        if n.scope == "incremental" && !isempty(direct_preds[n.id])
    )

    # Bind cancel check to this run's ID so callers only pass a run_id → bool function.
    _is_cancelled() = on_cancel_check(run.id)

    # One OS thread per image — each progresses through nodes independently
    image_tasks = [
        Threads.@spawn _execute_image_chain!(
            run, uid, ordered_nodes, overrides;
            on_log, on_status_change, is_cancelled=_is_cancelled)
        for uid in run.image_uids
    ]

    # One set-scope runner per picnic node — waits for all images then runs once
    set_tasks = [
        Threads.@spawn _run_set_scope_node!(run, node, overrides;
            on_log, on_status_change, is_cancelled=_is_cancelled)
        for node in ordered_nodes if node.scope == "set"
    ]

    # One incremental watcher per incremental plot node — event-driven, debounced
    incr_tasks = [
        Threads.@spawn _run_incremental_node!(
            run, node, incr_upstream[node.id], overrides;
            on_log, on_status_change, is_cancelled=_is_cancelled)
        for node in ordered_nodes
        if node.scope == "incremental" && haskey(incr_upstream, node.id)
    ]

    foreach(fetch, image_tasks)
    foreach(fetch, set_tasks)
    foreach(fetch, incr_tasks)

    run
end

"""Convenience overload: run a chain over all images in a set."""
function run_chain(proj::CciaProject, s::CciaSet; kwargs...)
    run_chain(proj, s.image_uids; kwargs...)
end

# ── REPL chain-building helpers ───────────────────────────────────────────────

"""
Thin constructor for ChainNode with auto-generated id.

    n = chain_node("importImages.omezarr")
    n = chain_node("cleanupImages.cellpose"; scope="image", resource_pool="gpu",
                   params=Dict("model" => "cyto2"))
"""
function chain_node(fn::String;
                    id::String             = gen_uid(),
                    scope::String          = "",   # "" → inherit from the task spec (see _task_default_scope)
                    params::Dict{String,Any} = Dict{String,Any}(),
                    barrier_policy::String = "all",
                    resource_pool::String  = "")::ChainNode
    ChainNode(; id, fn, scope, params, barrier_policy, resource_pool)
end

"""
Build and save a linear chain from a name and an ordered vector of ChainNodes.
Edges are added left-to-right between consecutive nodes.

    t = make_chain(proj, "my-pipeline", [
        chain_node("importImages.omezarr"),
        chain_node("cleanupImages.cellpose"; resource_pool="gpu"),
        chain_node("segmentation.cellpose"),
    ])
"""
function make_chain(proj::CciaProject, name::String, nodes::Vector{ChainNode})::ChainTemplate
    edges = [ChainEdge(nodes[i].id, nodes[i+1].id) for i in 1:length(nodes)-1]
    t     = ChainTemplate(name, nodes, edges)
    save_chain_template!(proj, t)
    t
end
