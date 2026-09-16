# ── Template ──────────────────────────────────────────────────────────────────

# Chain-node scope. Terminal set of values enforced by the enum; the on-disk (template JSON) and
# on-wire (whiteboard) form is the lowercase name via `Base.string(::ChainScope)`.
# `parse_chain_scope` reads it back and throws a friendly ArgumentError on an unknown value —
# `chain_template_from_raw` propagates it as the template-load error (validation would fire otherwise).
@enum ChainScope CHAIN_IMAGE CHAIN_SET CHAIN_INCREMENTAL
const _CHAIN_SCOPE_STR = Dict(
    CHAIN_IMAGE       => "image",
    CHAIN_SET         => "set",
    CHAIN_INCREMENTAL => "incremental",
)
const _CHAIN_SCOPE_PARSE = Dict(v => k for (k, v) in _CHAIN_SCOPE_STR)
Base.string(s::ChainScope) = _CHAIN_SCOPE_STR[s]
function parse_chain_scope(s::AbstractString)::ChainScope
    haskey(_CHAIN_SCOPE_PARSE, s) || throw(ArgumentError(
        "unknown chain scope: '$s' (must be one of $(join(sort(collect(keys(_CHAIN_SCOPE_PARSE))), ", ")))"))
    _CHAIN_SCOPE_PARSE[s]
end

# Chain-node barrier policy — only meaningful for CHAIN_SET nodes:
#   BARRIER_ALL              run with every image regardless of upstream failures (default)
#   BARRIER_REQUIRE_ALL      abort if any image failed upstream; all images → NODE_FAILED
#   BARRIER_SUCCESSFUL_ONLY  run with only upstream-successful images; failed ones → NODE_SKIPPED
@enum ChainBarrierPolicy BARRIER_ALL BARRIER_REQUIRE_ALL BARRIER_SUCCESSFUL_ONLY
const _BARRIER_POLICY_STR = Dict(
    BARRIER_ALL             => "all",
    BARRIER_REQUIRE_ALL     => "require_all",
    BARRIER_SUCCESSFUL_ONLY => "successful_only",
)
const _BARRIER_POLICY_PARSE = Dict(v => k for (k, v) in _BARRIER_POLICY_STR)
Base.string(s::ChainBarrierPolicy) = _BARRIER_POLICY_STR[s]
function parse_chain_barrier_policy(s::AbstractString)::ChainBarrierPolicy
    haskey(_BARRIER_POLICY_PARSE, s) || throw(ArgumentError(
        "unknown chain barrier policy: '$s' (must be one of $(join(sort(collect(keys(_BARRIER_POLICY_PARSE))), ", ")))"))
    _BARRIER_POLICY_PARSE[s]
end

struct ChainNode
    id::String
    fn::String              # "category.taskName" — same key as the task registry
    scope::ChainScope       # see @enum ChainScope above
    params::Dict{String,Any}
    barrier_policy::ChainBarrierPolicy  # see @enum ChainBarrierPolicy above
    # resource_pool — name of the global scheduler pool this node runs in (see scheduler.jl
    # _POOLS, sized from config.toml [pools]). "" falls back to the task JSON's resource_pool,
    # else "cpu". A pool with limit 1 (e.g. "gpu") serialises that node across the process.
    resource_pool::String
end

# Default scope for a task fun_name, read from its JSON spec's "scope" field. The task JSON is
# the single source of truth for scope: set-scope (picnic) tasks like behaviour.hmm and
# clustTracks.cluster declare "scope": "set" there, so a node built from them — in the REPL or
# dragged onto the whiteboard — becomes a picnic node without the author restating it. Unknown
# fn or specless task → CHAIN_IMAGE.
function _task_default_scope(fn::String)::ChainScope
    try
        parse_chain_scope(task_scope(_task_from_fun_name(fn)))   # spec's "scope" field (task.jl)
    catch
        CHAIN_IMAGE
    end
end

# `scope`/`barrier_policy` accept String OR the enum: string form for the whiteboard/REPL author
# and the JSON deserializer; enum form for internal code. An empty scope means "inherit from the
# task spec" (see _task_default_scope). An explicit non-empty scope always wins, so a caller can
# still force image-scope on a set task if needed.
_coerce_scope(s::ChainScope, fn) = s
_coerce_scope(s::AbstractString, fn) = isempty(s) ? _task_default_scope(fn) : parse_chain_scope(s)
_coerce_barrier(p::ChainBarrierPolicy) = p
_coerce_barrier(p::AbstractString) = parse_chain_barrier_policy(p)

ChainNode(; id, fn, scope="", params=Dict{String,Any}(),
            barrier_policy="all", resource_pool="") =
    ChainNode(id, fn, _coerce_scope(scope, fn), params, _coerce_barrier(barrier_policy), resource_pool)

struct ChainEdge
    from::String
    to::String
end

struct ChainTemplate
    name::String
    nodes::Vector{ChainNode}
    edges::Vector{ChainEdge}
    # UML "start" dot targets: node ids the start dot links to. When non-empty, a run executes ONLY the
    # nodes reachable from these (their inclusive descendants) — everything else is a draft, left in the
    # editor but skipped. Empty ⇒ run the whole chain from its natural roots (backward-compatible).
    start_targets::Vector{String}
end
ChainTemplate(name, nodes, edges) = ChainTemplate(name, nodes, edges, String[])

# ── Run record ────────────────────────────────────────────────────────────────

# Chain-node lifecycle. Terminal states are NODE_DONE, NODE_FAILED, NODE_CANCELLED, NODE_SKIPPED
# (enforced by convention in `_update_node_state!` — a re-fire of a terminal state is not a bug).
# On-disk (run.json) and on-wire (chain events) form is the lowercase name via
# `Base.string(::ChainNodeStatus)`; `parse_chain_node_status` reads it back from disk.
@enum ChainNodeStatus NODE_PENDING NODE_QUEUED NODE_RUNNING NODE_DONE NODE_FAILED NODE_CANCELLED NODE_SKIPPED
const _NODE_STATUS_STR = Dict(
    NODE_PENDING   => "pending",
    NODE_QUEUED    => "queued",
    NODE_RUNNING   => "running",
    NODE_DONE      => "done",
    NODE_FAILED    => "failed",
    NODE_CANCELLED => "cancelled",
    NODE_SKIPPED   => "skipped",
)
const _NODE_STATUS_PARSE = Dict(v => k for (k, v) in _NODE_STATUS_STR)
Base.string(s::ChainNodeStatus) = _NODE_STATUS_STR[s]
parse_chain_node_status(s::AbstractString)::ChainNodeStatus = _NODE_STATUS_PARSE[s]

mutable struct ImageNodeState
    status::ChainNodeStatus                 # see @enum ChainNodeStatus above
    task_id::Union{String,Nothing}
    result::Union{Dict{String,Any},Nothing}
    params_hash::Union{String,Nothing}      # sha256 of effective params — set on NODE_DONE, used for resume skip
end

ImageNodeState() = ImageNodeState(NODE_PENDING, nothing, nothing, nothing)

mutable struct ChainRun
    id::String
    chain_name::String
    project_uid::String
    image_uids::Vector{String}
    template_snapshot::ChainTemplate        # in-memory only — needed during execution
    template_hash::String                   # sha256 hex — pointer to cache entry on disk
    image_states::Dict{String,Dict{String,ImageNodeState}}  # uid => node_id => state
    created_at::Float64
    _dir::String                            # <project>/settings/chains/runs/<run_id>/
    _lock::ReentrantLock                    # guards image_states + disk writes
    _barriers::Dict{String,Channel{Nothing}}      # node_id => arrive channel (set-scope barrier)
    _barriers_done::Dict{String,Channel{Nothing}} # node_id => done channel   (set-scope barrier)
end
# NOTE: resource-pool concurrency is NOT a per-run concern. Every node runs via
# `run_task`, which routes through the global scheduler pools (`_POOLS` in
# scheduler.jl, sized from the [pools] section of config.toml). A `gpu` pool with
# limit 1 serialises GPU work across the whole process — chain nodes and module-page
# tasks alike. There is intentionally no second, per-run pool layer.
