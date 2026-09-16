# Kept for backward compatibility — internal validation now goes through the type system (a bad
# value can't survive `parse_chain_scope`/`parse_chain_barrier_policy` in `_node_from_dict`).
const CHAIN_SCOPES           = Tuple(string(s) for s in instances(ChainScope))
const CHAIN_BARRIER_POLICIES = Tuple(string(s) for s in instances(ChainBarrierPolicy))

struct ChainTemplateError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ChainTemplateError) = print(io, "ChainTemplateError: ", e.msg)

# Pool names come from the CONFIG table `_pools_init!` reads, not from `_POOLS` — validating must not
# spin up pool dispatchers as a side effect. `""` means "inherit from the task JSON" (see ChainNode).
# An unknown name is worth rejecting: `_pool` only warns and falls back to the wide `cpu` pool, which
# would silently run a GPU node unbounded.
function _known_pool_names()::Set{String}
    names = Set{String}(string(k) for k in keys(get(cecelia_conf(), "pools", Dict{String,Any}())))
    push!(names, "cpu")   # _pools_init! guarantees cpu even if the config omits it
    names
end

"""
    chain_root_ids(t::ChainTemplate) -> Vector{String}

Node ids with no incoming edge — where a run naturally begins — in template order.

Used to fill `start_targets` for a template authored outside the whiteboard. An empty `start_targets`
runs the whole chain (see `_prune_to_start`), so this changes nothing about execution; it exists for
the EDITOR. The whiteboard only draws the UML start dot when it has a target or a saved position
(`buildStartGraph` in frontend/src/utils/startDot.ts returns `nothing` otherwise), so a template with
neither opens with no start dot at all and the user has to add and wire one by hand.
"""
function chain_root_ids(t::ChainTemplate)::Vector{String}
    has_incoming = Set{String}(e.to for e in t.edges)
    String[n.id for n in t.nodes if !(n.id in has_incoming)]
end

"""
    validate_chain_template(t::ChainTemplate)

Throw `ChainTemplateError` if what `t` DOES describe is malformed. Checks, in the order a reader
would: node ids (present, unique), `fn` resolves in the task registry, `scope` / `barrier_policy` /
`resource_pool` are known values, both endpoints of every edge exist, the graph is acyclic, every
`startTargets` entry is a real node, and each node's params satisfy its task's JSON spec.

An empty template (no nodes) is intentionally allowed here — the whiteboard saves that as its very
first step when the user clicks "new" and hasn't wired anything yet. The "cannot actually run"
check lives in `run_chain` itself, which is the point that matters.

Returns `nothing` on success. Pure — reads the task specs and the config, writes nothing.
"""
function validate_chain_template(t::ChainTemplate)
    ids   = Set{String}()
    pools = _known_pool_names()
    for n in t.nodes
        isempty(n.id) && throw(ChainTemplateError("a node has an empty id"))
        n.id in ids && throw(ChainTemplateError("duplicate node id '$(n.id)'"))
        push!(ids, n.id)

        task = try
            _task_from_fun_name(n.fn)
        catch
            throw(ChainTemplateError("node '$(n.id)': unknown task '$(n.fn)' — " *
                                     "fn must be a registered fun_name like \"segment.cellpose\""))
        end
        # scope + barrier_policy are type-checked by ChainNode's constructor
        # (`parse_chain_scope`/`parse_chain_barrier_policy` in `_node_from_dict` throw
        # ArgumentError on an unknown value, which surfaces at template load time).
        (isempty(n.resource_pool) || n.resource_pool in pools) ||
            throw(ChainTemplateError("node '$(n.id)': resource_pool '$(n.resource_pool)' is not " *
                                     "configured — known pools: " * join(sort(collect(pools)), ", ")))
        try
            validate_params(task, n.params;
                            extra_options = _chain_produced_names(t, n.id))
        catch e
            e isa ParamValidationError || rethrow()
            throw(ChainTemplateError("node '$(n.id)' ($(n.fn)): $(e.msg)"))
        end
    end

    # Both endpoints must exist. A dangling `from` is a run-time KeyError in `_topo_sort`; a dangling
    # `to` is worse — it never reaches in-degree 0, so that edge silently does nothing.
    for e in t.edges
        e.from in ids || throw(ChainTemplateError("edge '$(e.from)' → '$(e.to)': no node '$(e.from)'"))
        e.to   in ids || throw(ChainTemplateError("edge '$(e.from)' → '$(e.to)': no node '$(e.to)'"))
        e.from == e.to && throw(ChainTemplateError("edge '$(e.from)' → '$(e.to)': a node cannot " *
                                                  "depend on itself"))
    end

    for s in t.start_targets
        s in ids || throw(ChainTemplateError("startTargets names '$s', which is not a node"))
    end

    # Reuse the executor's own sort so "valid" means exactly "the executor can order it".
    try
        _topo_sort(t)
    catch e
        throw(ChainTemplateError(sprint(showerror, e)))
    end
    nothing
end

# ── Template content cache ─────────────────────────────────────────────────────
# Templates are stored once under settings/chains/.cache/<sha256>.json.
# Run records reference the hash — editing a template after a run has started
# produces a new hash, leaving the old cache entry (and run records) untouched.

function _template_json(t::ChainTemplate)::String
    JSON3.write((;
        name  = t.name,
        nodes = [(; id=n.id, fn=n.fn, scope=string(n.scope), params=n.params,
                   barrier_policy=string(n.barrier_policy), resource_pool=n.resource_pool)
                 for n in t.nodes],
        edges = [(; from=e.from, to=e.to) for e in t.edges],
        startTargets = t.start_targets,
    ))
end

function _template_hash(t::ChainTemplate)::String
    bytes2hex(SHA.sha256(_template_json(t)))
end

function _params_hash(params::Dict{String,Any})::String
    sorted_pairs = [(k, params[k]) for k in sort(collect(keys(params)))]
    bytes2hex(SHA.sha256(JSON3.write(sorted_pairs)))
end
