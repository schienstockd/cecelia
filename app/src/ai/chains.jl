# Chain (whiteboard pipeline) summary for the read-only observer (Slice E of OBSERVER_DATA_ACCESS_PLAN.md).
# The lineage's per-image `steps` come from the run log — a recent, capped window — so a pipeline that
# ran earlier leaves no dated steps and the observer has to reconstruct it from artifacts. Chains are the
# INTENDED pipeline, persisted as structured templates: this exposes the wired DAG (which task feeds which)
# + the actual chain RUNS (which pipeline was executed, on how many images, node outcomes) — so the
# observer can tell chain-orchestrated work from ad-hoc runs. Unlike boards (opaque frontend JSON), chain
# templates/runs are fully structured, so this reads them directly. Project-level; READ-ONLY.

const _CHAIN_RUN_CAP = 20   # most-recent chain runs returned (hard cap)

# The wired chain templates: each a name + its node DAG (task fn + scope per node) + edges.
function _chain_templates(proj::CciaProject)
    dir = _chains_dir(proj); isdir(dir) || return Any[]
    out = Any[]
    for f in sort(readdir(dir))
        endswith(f, ".json") || continue
        t = try load_chain_template(proj, f[1:end-5]) catch; continue end
        push!(out, (; name = t.name,
                     nodes = [(; id = n.id, fun = n.fn, scope = n.scope) for n in t.nodes],
                     edges = [(; from = e.from, to = e.to) for e in t.edges],
                     startTargets = t.start_targets))
    end
    out
end

_chain_run_at(t::Real) = try Dates.format(Dates.unix2datetime(t), "yyyy-mm-ddTHH:MM:SS") catch; "" end

# The recorded chain RUNS (executions), newest first, capped. Per run: which chain, when, how many images,
# and a roll-up of node outcomes across images (:done/:failed/:skipped/…) — enough to spot a chain that
# failed partway without dumping every node's state.
function _chain_runs(proj::CciaProject)
    dir = _runs_dir(proj); isdir(dir) || return Any[]
    acc = Tuple{Float64,Any}[]
    for rid in readdir(dir)
        isdir(joinpath(dir, rid)) || continue
        r = try load_chain_run(proj, rid) catch; continue end
        counts = Dict{String,Int}()
        for (_uid, nodemap) in r.image_states, (_nid, st) in nodemap
            k = string(st.status); counts[k] = get(counts, k, 0) + 1
        end
        e = (; id = r.id, chainName = r.chain_name, at = _chain_run_at(r.created_at),
              imageCount = length(r.image_uids), nodeStatus = counts)
        push!(acc, (r.created_at, e))
    end
    sort!(acc; by = first, rev = true)
    [e for (_, e) in acc[1:min(end, _CHAIN_RUN_CAP)]]
end

"""
    chains_summary(proj) -> NamedTuple

The project's whiteboard chains: the wired `templates` (node DAG + task fns) and the recent `runs`
(executions with node-outcome roll-ups). Project-level, READ-ONLY. Slice E of OBSERVER_DATA_ACCESS_PLAN.md.
"""
chains_summary(proj::CciaProject) =
    (; projectUid = proj.uid, templates = _chain_templates(proj), runs = _chain_runs(proj))
