# Boolean populations (Decision 16) — the AND/OR/NOT combinators over other populations. A
# boolean pop's cell membership is derived at read time from `filter` (its BoolMembership operator
# + operand pop paths), not stored; that keeps re-gating cheap and prevents a booleanised copy
# from drifting when the operands change. See docs/POPULATION.md → Boolean populations.

# ── Boolean populations (Decision 16) ────────────────────────────────────────────
# A pop whose membership is a set operation over OTHER pops in the same map, rather than a gate or a
# filter: "nuc-GFP+ OR mem-TOM+". The tree is still the tree (∩ parent as always) — these add a
# second kind of edge on top of it, which is why dependency order below is no longer just depth.

# Retained as a derived tuple for the exported public surface (`Cecelia.BOOLEAN_OPS`) — the source
# of truth is `@enum BoolMembership`, this just re-exposes the wire-form strings for consumers that
# were coded against the pre-enum name.
const BOOLEAN_OPS = Tuple(_BOOL_MEMBERSHIP_STR[s] for s in instances(BoolMembership))

"""
    _normalise_boolean(op, pops, nots; self) -> (op, pops, nots) | (nothing, nothing, nothing)

Validate + canonicalise a boolean spec. Empty/`nothing` op ⇒ not a boolean pop. `"not"` is accepted
as an operator and normalised into the exclusion list (`op="not", pops=[A]` ≡ `op="and", not=[A]`),
so a hand-written sidecar — and the one-click "everything except this" — say the obvious thing. At
least one term is required, and `self` (the path the spec is attached to) may not be one of them.

Returned `op` is a `BoolMembership` enum value (`BOOL_AND` / `BOOL_OR`); the caller stores it as
the canonical form on `Population.boolean_op`.
"""
function _normalise_boolean(op, pops, nots; self::Union{AbstractString,Nothing}=nothing)
    (op === nothing || (op isa AbstractString && isempty(op))) && return (nothing, nothing, nothing)
    # Accept enum, String, or Symbol at the boundary; error on anything else via `String(op)`.
    raw = op isa BoolMembership ? _BOOL_MEMBERSHIP_STR[op] : lowercase(String(op))
    lst(x) = unique(String[String(v) for v in (x === nothing ? () : x)])
    ps, ns = lst(pops), lst(nots)
    if raw == "not"                   # alias: everything of the parent except these
        raw = "and"; ns = unique([ns; ps]); ps = String[]
    end
    haskey(_BOOL_MEMBERSHIP_PARSE, raw) ||
        error("boolean pop: unknown operator \"$op\" — one of $(join(sort(collect(keys(_BOOL_MEMBERSHIP_PARSE))), ", ")) or \"not\"")
    (isempty(ps) && isempty(ns)) &&
        error("boolean pop: pick at least one population to combine")
    (self !== nothing && (String(self) in ps || String(self) in ns)) &&
        error("boolean pop: a population cannot reference itself")
    (_BOOL_MEMBERSHIP_PARSE[raw], ps, ns)
end

"""Populations `path` needs before its own membership can be derived: its parent, plus (boolean) the
populations it combines. The edge set `topo_order` sorts over."""
function _pop_deps(m::PopulationMap, path::AbstractString)::Vector{String}
    p = m.pops[String(path)]
    deps = String[]
    is_root(p.parent) || push!(deps, p.parent)
    p.boolean_pops === nothing || append!(deps, p.boolean_pops)
    p.boolean_not  === nothing || append!(deps, p.boolean_not)
    deps
end

"""Would referencing `refs` from `path` close a dependency loop (`A = not B`, `B = not A`, or a
reference to one of `path`'s own descendants, which depend on it through their parent)?"""
function boolean_cycle(m::PopulationMap, path::AbstractString, refs)::Bool
    target = String(path)
    seen = Set{String}()
    stack = String[String(r) for r in refs]
    while !isempty(stack)
        cur = pop!(stack)
        cur == target && return true
        (cur in seen || !has_pop(m, cur)) && continue
        push!(seen, cur)
        append!(stack, _pop_deps(m, cur))
    end
    false
end

"""Populations OUTSIDE `targets` whose boolean definition references something inside it — i.e. what
would be left dangling by deleting `targets`. Returns `dependent path => referenced paths`."""
function boolean_dependents(m::PopulationMap, targets)::Vector{Pair{String,Vector{String}}}
    tset = Set(String[String(t) for t in targets])
    out = Pair{String,Vector{String}}[]
    for path in m.order
        path in tset && continue
        pop = m.pops[path]
        pop.boolean_op === nothing && continue
        refs = [something(pop.boolean_pops, String[]); something(pop.boolean_not, String[])]
        hit = [r for r in refs if r in tset]
        isempty(hit) || push!(out, path => hit)
    end
    out
end

"""
Paths in dependency order: every population after the ones its membership needs (its parent, and for
a boolean pop the populations it combines). Depth alone is enough for a pure tree — a boolean pop can
reference a DEEPER pop than itself, so the graph needs a real topological sort. Anything left in a
cycle (only reachable by hand-editing a sidecar) is appended as-is rather than raising: `recompute!`
degrades a pop with an unresolved reference to empty + a warning, and a broken file must not take the
whole map down.
"""
function topo_order(m::PopulationMap)::Vector{String}
    base = sort(m.order; by = p -> count(==('/'), p), alg = MergeSort)
    any(m.pops[p].boolean_op !== nothing for p in base) || return base
    done = Set{String}()
    out = String[]
    remaining = base
    while !isempty(remaining)
        keep = String[]
        for p in remaining
            if all(d -> !has_pop(m, d) || d in done, _pop_deps(m, p))
                push!(out, p); push!(done, p)
            else
                push!(keep, p)
            end
        end
        length(keep) == length(remaining) && (append!(out, keep); break)   # no progress ⇒ cycle
        remaining = keep
    end
    out
end

# Normalise a compound-filter spec (Decision 15): `nothing` | a list of `{measure, fun, values}`
# (dicts, from JSON, NamedTuples, or already-typed `FilterCondition`s) → `Vector{FilterCondition}`,
# dropping entries missing a measure/fun. Empty → `nothing` (treated as no compound filter). The
# `fun` field is coerced string/symbol → `FilterFun` here, so every downstream reader gets an enum.
function _normalise_conditions(conds)
    conds === nothing && return nothing
    out = FilterCondition[]
    for c in conds
        c isa FilterCondition && (push!(out, c); continue)
        gc(k) = c isa AbstractDict ? get(c, k, get(c, Symbol(k), nothing)) : getproperty(c, Symbol(k))
        mz = gc("measure"); fz = gc("fun")
        (mz === nothing || fz === nothing) && continue
        push!(out, FilterCondition(String(mz), _coerce_filter_fun(fz), gc("values")))
    end
    isempty(out) ? nothing : out
end

