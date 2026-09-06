# Correction plan → ChainTemplate mount (Phase E of docs/todo/CORRECTION_QC_PLAN.md).
#
# Splits from `correction_plan.jl` because `ChainTemplate` / `ChainNode` / `ChainEdge` live in
# `tasks/chain.jl`, which loads AFTER correction_plan.jl in `Cecelia.jl`. Keeping the mount here
# makes the dependency direction explicit — chain.jl compiles standalone; correction_plan_chain.jl
# is the bridge one direction only (plan → chain, never the other way).


"""
    plan_to_chain_template(plan; name = nothing) -> ChainTemplate

Turn a `CorrectionPlan` into a runnable `ChainTemplate`. **Pure** — no disk write, no executor
mount. The caller decides whether to `save_chain_template!` (persist to the project's chains dir)
and/or `run_chain` (execute).

Shape:
- **One `ChainNode` per included step**, in `plan.included` order (already `_order_weight`-sorted
  in `apply_rules`). `id` = short-form fun_name (`"driftCorrect"`, `"smooth"`, …) — stable across
  re-plans and human-readable in the whiteboard.
- **Linear edges** — `a → b → c …` in plan order. Bucketed order weights collapse to a linear DAG
  for one image; picnic set-scope tasks (not currently emitted by `apply_rules`) would need edges
  extended when they land.
- **Params passed through unchanged** — the plan already resolved them from card + wizard + score
  precedence. Validation happens at chain-save time via `validate_template` (which calls
  `validate_params` per node), not here — so bad card params surface at the same natural point
  they would for a hand-authored chain.
- **Excluded steps dropped.** `plan.excluded` is an audit surface, not a runnable list; the chain
  executor has no concept of "considered and dropped."

`name` defaults to `"correction-plan-{image_uid}"` so re-mounting one image's plan replaces its
prior template on `save_chain_template!` (canonical per-image, not per-run).

**Not populated in Phase E:** the `upstream_value_names` provenance (§8) — the chain executor
knows which `value_name` each node reads via its own `execute_task` wiring, and duplicating that
into the plan sidecar would be a second source of truth. Deferred until a Phase F needs it in
the UI without loading the chain state.
"""
function plan_to_chain_template(plan::CorrectionPlan;
                                name::Union{AbstractString,Nothing} = nothing)::ChainTemplate
    tmpl_name = name === nothing ? "correction-plan-$(plan.image_uid)" : String(name)
    nodes = ChainNode[]
    for step in plan.included
        push!(nodes, ChainNode(; id = _plan_node_id(step.fun_name),
                                 fn = step.fun_name,
                                 params = copy(step.params)))
    end
    edges = ChainEdge[]
    for i in 1:(length(nodes) - 1)
        push!(edges, ChainEdge(nodes[i].id, nodes[i + 1].id))
    end
    return ChainTemplate(tmpl_name, nodes, edges)
end

# Short-form fun_name for stable, human-readable node ids. `"cleanupImages.driftCorrect"` becomes
# `"driftCorrect"`. Guaranteed unique per plan because `apply_rules` keys `steps_by_fn` by fun_name;
# an unqualified fun_name (no dot) is left alone.
_plan_node_id(fn::AbstractString)::String =
    (i = findlast('.', fn); i === nothing ? String(fn) : String(fn[nextind(fn, i):end]))
