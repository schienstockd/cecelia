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
                parse_chain_node_status(string(st_raw[:status])),
                !isnothing(get(st_raw, :task_id,     nothing)) ? string(st_raw[:task_id])     : nothing,
                !isnothing(get(st_raw, :result,      nothing)) ?
                    Dict{String,Any}(string(k) => v for (k, v) in st_raw[:result]) : nothing,
                !isnothing(get(st_raw, :params_hash, nothing)) ? string(st_raw[:params_hash]) : nothing,
                !isnothing(get(st_raw, :reason,      nothing)) ? string(st_raw[:reason])      : nothing,
            )
        end
    end

    n             = length(image_uids)
    barriers      = Dict{String,Channel{Nothing}}()
    barriers_done = Dict{String,Channel{Nothing}}()
    for node in template.nodes
        if node.scope == CHAIN_SET
            barriers[node.id]      = Channel{Nothing}(n)
            barriers_done[node.id] = Channel{Nothing}(n)
        end
    end

    ChainRun(
        string(raw[:id]),
        string(raw[:chain_name]),
        # Identity from LOCATION, not the stored field: run.json lives inside `proj`, so resume uses
        # `proj.uid`. The persisted `project_uid` is advisory — a run.json that travelled with a project
        # imported under a new uid then resumes against the CURRENT project, not the stale stored one.
        proj.uid,
        image_uids,
        template,
        template_hash,
        image_states,
        Float64(raw[:created_at]),
        string(get(raw, :by, "")),
        string(get(raw, :resumed_by, "")),
        Dict{String,String}(string(k) => string(v) for (k, v) in get(raw, :excluded_images, Dict())),
        run_dir,
        ReentrantLock(),
        barriers,
        barriers_done,
        Dict{String,Any}(),
        _node_dicts(get(raw, :overrides, nothing)),     # absent = unknown (`nothing`), not "none"
        _node_dicts(get(raw, :node_params, nothing)),
        string(something(get(raw, :from_run, nothing), "")),
    )
end

# A stored node_id => params map as plain Dicts (JSON3 hands back Symbol-keyed views — `_spec_value`
# materialises them); `nothing` stays `nothing` — the field was not recorded.
_node_dicts(::Nothing) = nothing
_node_dicts(raw::AbstractDict)::Dict{String,Any} = _spec_value(raw)

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
Template is loaded from `<project>/settings/chains/<name>.json` and `chain` must be given.
A frozen copy is stored in the run record — editing the template after the run
has started does not change what that run is understood to have done.

    run_chain(proj, ["uid1", "uid2"]; chain="my-chain")

## Resume
Pass an existing `run_id` instead of a `chain` name. The run is loaded from disk,
stale/failed nodes are reset to `:pending`, and already-done nodes with unchanged
params are skipped. `image_uids` is ignored in resume mode. The run's recorded `overrides` are kept,
with any given here on top.

    run_chain(proj, String[]; run_id="abc12345")

overrides: node_id => param overrides merged on top of template params.

excluded: images the caller dropped from the selection before the run (uid => reason), recorded in
run.json as `excluded_images` so the run says its selection was larger. Fresh runs only; a resume keeps
the record it was created with.

## Replay
Pass `from_run` (a past run's id) instead of a `chain` name: a FRESH run of that run's frozen template
(not the current one) with the overrides it recorded, `overrides` given here merged on top per node.
Empty `image_uids` = the past run's images. The new run.json names its source as `from_run`. A source
run from before overrides were recorded replays its template alone, and says so in the log.

    run_chain(proj, String[]; from_run="abc12345")

Every execution records its `overrides` and each node's params (`node_params`) in run.json.
"""
function run_chain(proj::CciaProject, image_uids::Vector{String};
                   chain::String                  = "",
                   run_id::Union{String,Nothing}  = nothing,
                   from_run::Union{String,Nothing} = nothing,
                   start_node::Union{String,Nothing} = nothing,
                   overrides::Dict{String,Any}    = Dict{String,Any}(),
                   excluded::AbstractDict         = Dict{String,String}(),
                   on_log::Function               = line -> println(line),
                   on_cancel_check::Function      = _ -> false,
                   by::String                     = active_profile_name(),
                   caller::AbstractDict           = Dict{String,Any}())::ChainRun

    isnothing(run_id) || isnothing(from_run) ||
        error("run_chain: `run_id` resumes a run and `from_run` starts a new one from it — pass one")
    if !isnothing(run_id)
        # Resume: restore run from disk, reset stale/failed nodes, keep :done ones. An explicit
        # `start_node` additionally force-restarts that node + everything downstream (re-run from here).
        run           = load_chain_run(proj, run_id)
        # A resume continues the SAME run, so it keeps the overrides the run recorded, the caller's on
        # top. Without this a GUI Resume (which sends none) of a run started with overrides — a REPL run,
        # or a replay (↻) — would count every :done node stale and re-run it without them. A run.json
        # from before overrides were recorded has none to keep (`nothing`), as before.
        overrides     = _merge_overrides(something(run.overrides, Dict{String,Any}()), overrides)
        ordered_nodes = _topo_sort(run.template_snapshot)
        _reset_stale_nodes!(run, overrides, ordered_nodes)
        isnothing(start_node) || _force_restart_from!(run, start_node)
        kept = Set(n.id for n in ordered_nodes
                   if all(s[n.id].status == NODE_DONE for s in values(run.image_states)))
        _validate_run_params(run.template_snapshot, overrides; skip = kept)
        run.resumed_by = by
        run.caller     = Dict{String,Any}(String(k) => v for (k, v) in caller)   # the resumer's, not persisted
        run.overrides  = _copy_overrides(overrides)
        # A step this resume will not re-run (:done on every image) keeps the params it RAN with: its
        # resume hash ignores spec defaults and clamps, so re-resolving it against today's specs could
        # record a default that changed since its outputs were made. Re-run steps get fresh params.
        fresh = _record_node_params(run.template_snapshot, overrides)
        prev  = something(run.node_params, Dict{String,Any}())
        run.node_params = Dict{String,Any}(nid => (nid in kept && haskey(prev, nid) ? prev[nid] : p)
                                           for (nid, p) in fresh)
        _save_run!(run)
    else
        src = nothing
        if !isnothing(from_run)
            # Replay: the source's frozen template (already pruned to its start dot when it ran) and the
            # overrides it recorded, with this call's on top.
            src = load_chain_run(proj, from_run)
            template = src.template_snapshot
            isempty(image_uids) && (image_uids = copy(src.image_uids))
            if isnothing(src.overrides)
                Base.invokelatest(on_log, "[INFO] Replaying run $from_run: it predates recorded overrides, " *
                                          "so any it ran with are unknown — replaying its frozen template only")
            end
            overrides = _merge_overrides(something(src.overrides, Dict{String,Any}()), overrides)
        else
            isempty(chain) && error("run_chain requires `chain` name when `run_id` is not given")
            # UML start dot: if the template has start targets, run ONLY the reachable subgraph (the rest
            # are drafts). Pruning here means every downstream stage (topo, states, resume) sees one clean
            # effective template — targets are roots, unreachable branches simply don't exist for this run.
            template = _prune_to_start(load_chain_template(proj, chain))
        end
        isempty(image_uids) && error("run_chain requires at least one image UID")
        isempty(template.nodes) && error("run_chain: start dot reaches no nodes (nothing to run)")
        _validate_run_params(template, overrides)
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
            if node.scope == CHAIN_SET
                barriers[node.id]      = Channel{Nothing}(n)
                barriers_done[node.id] = Channel{Nothing}(n)
            end
        end

        hash    = _cache_template!(proj, template)
        new_id  = gen_uid()
        run_dir = joinpath(_runs_dir(proj), new_id)
        run     = ChainRun(
            new_id, template.name, proj.uid, image_uids, template, hash,
            image_states, time(), by, "",
            Dict{String,String}(string(k) => string(v) for (k, v) in excluded),
            run_dir, ReentrantLock(),
            barriers, barriers_done, Dict{String,Any}(String(k) => v for (k, v) in caller),
            _copy_overrides(overrides), _record_node_params(template, overrides),
            isnothing(from_run) ? "" : from_run,
        )
        isnothing(src) || _log_replay_drift(src, run, on_log)
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
        if n.scope == CHAIN_INCREMENTAL && !isempty(direct_preds[n.id])
    )

    # Bind cancel check to this run's ID so callers only pass a run_id → bool function.
    _is_cancelled() = on_cancel_check(run.id)

    # One OS thread per image — each progresses through nodes independently
    image_tasks = [
        Threads.@spawn _execute_image_chain!(
            run, uid, ordered_nodes, overrides;
            on_log, is_cancelled=_is_cancelled)
        for uid in run.image_uids
    ]

    # One set-scope runner per picnic node — waits for all images then runs once
    set_tasks = [
        Threads.@spawn _run_set_scope_node!(run, node, overrides;
            on_log, is_cancelled=_is_cancelled)
        for node in ordered_nodes if node.scope == CHAIN_SET
    ]

    # One incremental watcher per incremental plot node — event-driven, debounced
    incr_tasks = [
        Threads.@spawn _run_incremental_node!(
            run, node, incr_upstream[node.id], overrides;
            on_log, is_cancelled=_is_cancelled)
        for node in ordered_nodes
        if node.scope == CHAIN_INCREMENTAL && haskey(incr_upstream, node.id)
    ]

    foreach(fetch, image_tasks)
    foreach(fetch, set_tasks)
    foreach(fetch, incr_tasks)

    run
end

# The overrides as recorded: node_id => a plain params Dict (the caller's dict is never aliased).
_copy_overrides(ov::AbstractDict)::Dict{String,Any} =
    Dict{String,Any}(string(k) => _spec_value(v) for (k, v) in ov)

# A replay's overrides: the source run's, then the caller's on top, merged per node (a caller override
# for one param of a node keeps the source's other overrides for that node).
function _merge_overrides(base::AbstractDict, top::AbstractDict)::Dict{String,Any}
    out = _copy_overrides(base)
    for (nid, ov) in _copy_overrides(top)
        prev = get(out, nid, nothing)
        out[nid] = prev isa AbstractDict && ov isa AbstractDict ? merge(prev, ov) : ov
    end
    out
end

# A replay re-resolves params against TODAY's specs: a default that changed since, or a stored value now
# clamped, makes a step run with something other than the source recorded. Say so, one line per value,
# so a replay that is not a faithful one is visible in the log (both records are in the two run.json).
function _log_replay_drift(src::ChainRun, run::ChainRun, on_log::Function)
    isnothing(src.node_params) && return
    for (nid, now) in something(run.node_params, Dict{String,Any}())
        was = get(src.node_params, nid, nothing)
        (was isa AbstractDict && now isa AbstractDict) || continue
        for k in sort!(collect(union(keys(was), keys(now))))
            a, b = get(was, k, nothing), get(now, k, nothing)
            isequal(a, b) && continue
            Base.invokelatest(on_log, "[INFO] Replaying run $(src.id): step $nid '$k' was " *
                                      "$(repr(a)) there, now $(repr(b))")
        end
    end
end

"""Convenience overload: run a chain over all images in a set."""
function run_chain(proj::CciaProject, s::CciaSet; kwargs...)
    run_chain(proj, s.image_uids; kwargs...)
end

# ── REPL chain-building helpers ───────────────────────────────────────────────

"""
Thin constructor for ChainNode with auto-generated id.

    n = chain_node("importImages.omezarr")
    n = chain_node("segment.cellpose"; scope="image", resource_pool="gpu",
                   params=Dict("models" => Dict("0" => Dict("model" => "cpsam_v2"))))
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
    # the start dot on the roots, as the create/save routes repair it — the whiteboard draws it from this
    t     = ChainTemplate(name, nodes, edges, chain_root_ids(t))
    save_chain_template!(proj, t)
    t
end
