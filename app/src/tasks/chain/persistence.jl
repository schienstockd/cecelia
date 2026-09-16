# ── Filesystem helpers ────────────────────────────────────────────────────────

# Chains live under `<proj>/settings/chains/` — the same location the API layer reads/writes
# (`_chains_dir_for_project` in api/src/routes.jl). These MUST agree: the whiteboard saves a
# template through the API, then `run_chain` loads it through here. They diverged once (API moved
# chains into settings/, this stayed at `<proj>/chains/`) and every chain run failed with
# "template not found" — keep them in sync. Mirrors the API's legacy-location migration so a
# REPL-first project (or one predating settings/) is picked up too.
function _chains_dir(proj::CciaProject)::String
    newdir = joinpath(proj.root, "settings", "chains")
    olddir = joinpath(proj.root, "chains")   # legacy location (pre-settings/)
    if isdir(olddir) && !isdir(newdir)
        try
            mkpath(joinpath(proj.root, "settings"))
            mv(olddir, newdir)
        catch e
            @warn "Could not migrate chains into settings/" project=proj.uid exception=e
        end
    end
    newdir
end
_template_path(proj::CciaProject, name::String) = joinpath(_chains_dir(proj), "$name.json")
_runs_dir(proj::CciaProject)::String           = joinpath(_chains_dir(proj), "runs")
_cache_dir(proj::CciaProject)::String          = joinpath(_chains_dir(proj), ".cache")

# ── Template I/O ──────────────────────────────────────────────────────────────

function _node_from_dict(d)::ChainNode
    fn = string(get(d, "fn", get(d, :fn, "")))
    sc = string(get(d, "scope", get(d, :scope, "")))
    ChainNode(
        string(get(d, "id", get(d, :id, ""))),
        fn,
        isempty(sc) ? _task_default_scope(fn) : parse_chain_scope(sc),
        Dict{String,Any}(string(k) => v
                         for (k, v) in get(d, "params", get(d, :params, Dict()))),
        parse_chain_barrier_policy(string(get(d, "barrier_policy", get(d, :barrier_policy, "all")))),
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
    chain_template_from_raw(raw; name="")

Build a `ChainTemplate` from a parsed template document (a `JSON3.Object` or any `AbstractDict`),
defaulting its name to `name` when the document has none. Extra fields the whiteboard adds (e.g.
`positions`) are ignored.

This is the ONE parse — `load_chain_template` reads a file through it, and the API's create route
runs an author's posted document through it before validating, so what gets validated is exactly what
`run_chain` will later load.
"""
chain_template_from_raw(raw; name::AbstractString = "")::ChainTemplate =
    ChainTemplate(
        string(get(raw, :name, get(raw, "name", name))),
        [_node_from_dict(n) for n in get(raw, :nodes, get(raw, "nodes", []))],
        [_edge_from_dict(e) for e in get(raw, :edges, get(raw, "edges", []))],
        String[string(s) for s in get(raw, :startTargets,
                       get(raw, "startTargets", get(raw, :start_targets,
                       get(raw, "start_targets", []))))],
    )

"""
Load a chain template from `<project>/settings/chains/<name>.json`.
"""
function load_chain_template(proj::CciaProject, name::String)::ChainTemplate
    path = _template_path(proj, name)
    isfile(path) || error("Chain template not found: $path")
    chain_template_from_raw(JSON3.read(read(path, String)); name = name)
end

"""
Write a chain template to `<project>/settings/chains/<name>.json`.
Creates the chains/ directory if needed.
"""
function save_chain_template!(proj::CciaProject, t::ChainTemplate)::ChainTemplate
    mkpath(_chains_dir(proj))
    write_atomic(_template_path(proj, t.name)) do io
        JSON3.pretty(io, (;
            name  = t.name,
            nodes = [(; id=n.id, fn=n.fn, scope=string(n.scope), params=n.params,
                       barrier_policy=string(n.barrier_policy), resource_pool=n.resource_pool)
                     for n in t.nodes],
            edges = [(; from=e.from, to=e.to) for e in t.edges],
            startTargets = t.start_targets,
        ))
    end
    t
end

# ── Template validation ───────────────────────────────────────────────────────
#
# The whiteboard cannot author an invalid template — it only offers real task defs, and VueFlow
# cannot draw an edge to a node that isn't there. Every OTHER author can: the REPL, a hand-edited
# file, and (since it can author chains over the MCP) Claude. For those, nothing checked anything
# until `run_chain`, so a typo surfaced as a mid-run `_task_from_fun_name` throw or a `KeyError` in
# `_topo_sort` — after the user pressed Run on a chain they did not write. This validates at AUTHOR
# time instead, so the error lands on whoever wrote it.
#
# Advisory scope, deliberately: it checks what is knowable from the template + the task specs. It
# cannot check intent (tracking wired before segmentation) or anything per-image — `requires`/axis
# gating is evaluated against a real image at run time, and selection params (`valueNameSelection`,
# `popSelection`) name project state that does not exist at author time. A valid template is a
# well-formed one, not a sensible one; the user reviewing the graph before Run stays load-bearing.


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
        String[string(s) for s in get(raw, :startTargets, get(raw, :start_targets, []))],
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
    write_atomic(joinpath(run._dir, "run.json")) do io
        JSON3.pretty(io, (;
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
