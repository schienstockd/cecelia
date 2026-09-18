# Path helpers + the three population-shape enums (PopType, BoolMembership, FilterFun) and
# FilterCondition. Loaded first because every other popmanager fragment references at least one:
# Population/PopulationMap structs embed PopType, boolean-pop logic dispatches on BoolMembership,
# and picker/allow-list rules gate on pop_type. The root-path convention (root = `"root"`, all
# pop paths start with `/`) is fixed here so downstream code never re-derives it.

# ── Population manager ───────────────────────────────────────────────────────────
#
# The generic abstraction for all population types (flow/clust/live). A population is a
# node in a tree: it has a name, hierarchical path, colour, and a membership definition —
# a `gate` (flow) or a filter spec (clust/live; see docs/POPULATION.md). Membership is
# derived (gating_engine.jl), never stored in the H5AD.
#
# Persistence: per-segmentation sidecar `{task_dir}/gating/{value_name}.json` (NOT ccid.json).

const ROOT = "root"

# `track_source` sentinel written by `_write_back` when tracking runs on the whole segmentation, not
# on a gated pop. Bypasses the pop→pop conflict/attribution rules (whole-seg is the documented "prime
# everything" mode, MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 1). Kept as a Julia const so the
# guard in `resolve_pops` and the writer in `tasks/tracking/bayesian_tracking.jl` can't drift apart.
const WHOLE_SEG_TRACK_SOURCE = "whole_seg"

# ── Path helpers (root convention: "root"; pop paths start with "/") ─────────────
is_root(p::AbstractString) = p == ROOT || p == "/" || isempty(p)

"""Parent path of a pop path. `/a/b` → `/a`; `/a` → `root`."""
function pop_parent(path::AbstractString)::String
    is_root(path) && return ROOT
    idx = findlast('/', path)
    (idx === nothing || idx == 1) ? ROOT : path[1:idx-1]
end

"""Leaf name of a pop path. `/a/b` → `b`."""
function pop_name(path::AbstractString)::String
    is_root(path) && return ROOT
    idx = findlast('/', path)
    idx === nothing ? path : path[(idx+1):end]
end

"""Join a parent path + leaf name into a pop path."""
pop_path(parent::AbstractString, name::AbstractString) =
    is_root(parent) ? "/" * name : rstrip(parent, '/') * "/" * name

# replace the `old` path prefix with `new` (boundary-aware: matches `old` or `old/…`)
function _replace_prefix(path::AbstractString, old::AbstractString, new::AbstractString)::String
    path == old && return new
    startswith(path, old * "/") && return new * path[(length(old)+1):end]
    path
end

# ── PopType enum ─────────────────────────────────────────────────────────────────
# Enumerates the population kinds Cecelia recognises internally. The on-wire (API `popType`) and
# on-disk (gating sidecar `"pop_type"` value, and the `{vn}__<pt>.json` filename discriminator)
# forms stay lowercase strings via `Base.string(::PopType)`; `parse_pop_type` reads them back and
# throws `ArgumentError` on an unknown value.
#
# The set is not a free vocabulary — every value listed here is one that some code path routes on
# (`accepts` allow-lists in `is_gating_pop_type`/`_is_cluster_pop_type`, filename derivation in
# `gating_path`, granularity/category resolution). A new pop_type value is a new set of dispatch
# entries; add it here first so the compiler surfaces the reachable sites.
@enum PopType POP_FLOW POP_CLUST POP_TRACK POP_TRACKCLUST POP_BRANCH POP_LIVE POP_REGION POP_LABELS
const _POP_TYPE_STR = Dict(
    POP_FLOW       => "flow",
    POP_CLUST      => "clust",
    POP_TRACK      => "track",
    POP_TRACKCLUST => "trackclust",
    POP_BRANCH     => "branch",
    POP_LIVE       => "live",
    POP_REGION     => "region",
    POP_LABELS     => "labels",
)
const _POP_TYPE_PARSE = Dict(v => k for (k, v) in _POP_TYPE_STR)
Base.string(s::PopType) = _POP_TYPE_STR[s]
# `String(pt)` also works — the many `String[pop_type, ...]` / `String(pop_type)` idioms across the
# codebase (already covering the String case as identity) round-trip an enum via this converter.
Base.String(s::PopType) = _POP_TYPE_STR[s]
# String interpolation (`"$(pt)"`) goes through `print`, which for `@enum` types defaults to the
# UPPERCASE symbol name ("POP_FLOW"), not the wire lowercase. Override so interpolation stays
# wire-compatible — `lab_log_context.jl:300` builds a `|`-delimited key with the pop_type value
# baked in and then splits it back apart to route on `is_gating_pop_type`.
Base.print(io::IO, s::PopType) = print(io, _POP_TYPE_STR[s])
function parse_pop_type(s::AbstractString)::PopType
    haskey(_POP_TYPE_PARSE, s) || throw(ArgumentError(
        "unknown pop_type: '$s' (must be one of $(join(sort(collect(keys(_POP_TYPE_PARSE))), ", ")))"))
    _POP_TYPE_PARSE[s]
end
# Accept enum or String at construction/boundary sites — kw ctors, `_pop_from_dict`, and function
# kwargs that used to take `AbstractString` all go through `_coerce_pop_type`.
_coerce_pop_type(x::PopType) = x
_coerce_pop_type(x::AbstractString) = parse_pop_type(x)
# Union alias for API-boundary function kwargs: accept enum OR string, coerce internally
# with `string(pop_type)` (already what most helpers do). Signature stays permissive.
const PopTypeArg = Union{PopType,AbstractString}

# ── BoolMembership enum ──────────────────────────────────────────────────────────
# The operator on a boolean population — the two ways `boolean_pops` combine before `boolean_not`
# is subtracted. On-wire (API `"op"`) and on-disk (gating sidecar `"boolean.op"`) stays the lowercase
# string via `Base.string(::BoolMembership)`. Field type upgrade from `Union{String,Nothing}` so an
# assignment of "xor" or a typo fails at the type boundary rather than surfacing later in
# `_normalise_boolean` — same reasoning as the enum sweep in #900/#901 (PopType/ChainScope).
#
# `"not"` is deliberately NOT a value: `_normalise_boolean` accepts it as an input alias, canonicalises
# to `BOOL_AND` with the terms moved to the exclusion list, and only then stores it.
@enum BoolMembership BOOL_AND BOOL_OR
const _BOOL_MEMBERSHIP_STR = Dict(BOOL_AND => "and", BOOL_OR => "or")
const _BOOL_MEMBERSHIP_PARSE = Dict(v => k for (k, v) in _BOOL_MEMBERSHIP_STR)
Base.string(s::BoolMembership) = _BOOL_MEMBERSHIP_STR[s]
Base.String(s::BoolMembership) = _BOOL_MEMBERSHIP_STR[s]
Base.print(io::IO, s::BoolMembership) = print(io, _BOOL_MEMBERSHIP_STR[s])
function parse_bool_membership(s::AbstractString)::BoolMembership
    haskey(_BOOL_MEMBERSHIP_PARSE, s) || throw(ArgumentError(
        "unknown boolean_op: '$s' (must be one of $(join(sort(collect(keys(_BOOL_MEMBERSHIP_PARSE))), ", ")))"))
    _BOOL_MEMBERSHIP_PARSE[s]
end

# ── FilterFun enum + FilterCondition ─────────────────────────────────────────────
# `filter_fun` names one of seven comparison operators the gating engine understands
# (`_filter_mask` in gating_engine.jl). Same wire-form / on-disk pattern as PopType and
# BoolMembership: the lowercase string via `Base.string(::FilterFun)`, a typo fails at the type
# boundary rather than at `_filter_mask("Unknown filter_fun: ...")`.
@enum FilterFun FILTER_GT FILTER_GTE FILTER_LT FILTER_LTE FILTER_EQ FILTER_NEQ FILTER_IN
const _FILTER_FUN_STR = Dict(
    FILTER_GT  => "gt",  FILTER_GTE => "gte",
    FILTER_LT  => "lt",  FILTER_LTE => "lte",
    FILTER_EQ  => "eq",  FILTER_NEQ => "neq",
    FILTER_IN  => "in",
)
const _FILTER_FUN_PARSE = Dict(v => k for (k, v) in _FILTER_FUN_STR)
Base.string(f::FilterFun) = _FILTER_FUN_STR[f]
Base.String(f::FilterFun) = _FILTER_FUN_STR[f]
Base.print(io::IO, f::FilterFun) = print(io, _FILTER_FUN_STR[f])
function parse_filter_fun(s::AbstractString)::FilterFun
    haskey(_FILTER_FUN_PARSE, s) || throw(ArgumentError(
        "unknown filter_fun: '$s' (must be one of $(join(sort(collect(keys(_FILTER_FUN_PARSE))), ", ")))"))
    _FILTER_FUN_PARSE[s]
end
_coerce_filter_fun(x::FilterFun) = x
_coerce_filter_fun(x::AbstractString) = parse_filter_fun(String(x))
_coerce_filter_fun(x::Symbol) = parse_filter_fun(String(x))

# One entry of a compound filter (Decision 15). Multiple conditions AND to give a compound filter
# pop: e.g. `CD4 > 0.5 AND speed > 5`. Replaces the earlier `NamedTuple{(measure, fun, values)}`
# — typing at the boundary means `_normalise_conditions` is the ONLY place that has to accept a
# string `fun` value; everything downstream already has a `FilterFun`.
struct FilterCondition
    measure::String
    fun::FilterFun
    values::Any
end

