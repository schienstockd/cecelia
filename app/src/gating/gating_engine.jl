# ── Gating engine: recompute, membership ─────────────────────────────────────────
#
# Walks a PopulationMap and derives membership (child = parent ∩ own definition) in
# topological order, caching the result on the map. Membership is derived here, never
# stored in the H5AD (docs/POPULATION.md). The pop_type-agnostic `pop_df` accessor that
# builds on this lives with the pop-map infrastructure in population_manager.jl.

# ── Filter mask for filtered populations (clust/live; e.g. tracked = track_id > 0) ─
function _filter_mask(col::AbstractVector, fun, vals, default_all::Bool)::BitVector
    n = length(col)
    fun === nothing && return default_all ? trues(n) : falses(n)
    fun = String(fun)
    if fun == "in"
        s = Set(vals isa AbstractVector ? vals : [vals])
        return BitVector(!ismissing(x) && (x in s) for x in col)
    end
    cmp = fun == "gt"  ? (>)  : fun == "gte" ? (>=) :
          fun == "lt"  ? (<)  : fun == "lte" ? (<=) :
          fun == "eq"  ? (==) : fun == "neq" ? (!=) :
          error("Unknown filter_fun: $fun")
    BitVector(!ismissing(x) && cmp(x, vals) for x in col)
end

# A gate/filter column the map references isn't in the fetched frame → the population resolves to no
# members (empty mask) rather than crashing the whole recompute. Warns once per (pop, column) so the
# cause is visible in the server log without spamming. See the call sites in `recompute!`.
function _missing_col_mask(n::Int, path::AbstractString, col::AbstractString)::BitVector
    @warn "gating: column '$col' absent from the fetched data for population '$path' — treating as \
           empty (no members). For a cluster pop this usually means it's being evaluated against a \
           segmentation that didn't take part in its clustering run."
    falses(n)
end

# columns the gates + filters in this map need from the H5AD
function _needed_columns(m::PopulationMap)::Vector{String}
    cols = String[]
    for path in m.order
        p = m.pops[path]
        if p.gate !== nothing
            push!(cols, p.gate.x_channel, p.gate.y_channel)
        end
        p.filter_measure !== nothing && push!(cols, p.filter_measure)
    end
    unique(cols)
end

"""
    recompute!(m, fetch_cols) -> m

Derive membership for every population. `fetch_cols(cols::Vector{String})` must return a
`DataFrame` with a `label` column plus the requested columns (production wraps
`label_props`; tests pass an in-memory closure). Result cached on the map.
"""
function recompute!(m::PopulationMap, fetch_cols::Function)
    df = fetch_cols(_needed_columns(m))
    "label" in names(df) || error("recompute!: fetch_cols must return a `label` column")
    labels = df.label
    n = length(labels)
    cols = Set(names(df))
    memb = Dict{String,BitVector}()
    memb[ROOT] = trues(n)
    for path in topo_order(m)
        p = m.pops[path]
        parent_mask = get(memb, p.parent, trues(n))
        mask = if p.explicit_labels !== nothing
            # membership IS this label set (∩ parent) — the napari selection (docs/POPULATION.md)
            sel = Set(p.explicit_labels)
            parent_mask .& BitVector(l in sel for l in labels)
        elseif p.gate !== nothing
            # a gate whose axis column is absent from the fetched frame → no members, not a crash. See
            # the filter case below for the rationale (missing column ≠ hard 500).
            (p.gate.x_channel in cols && p.gate.y_channel in cols) ?
                parent_mask .& inside(p.gate, df[!, p.gate.x_channel], df[!, p.gate.y_channel]) :
                _missing_col_mask(n, path, string(p.gate.x_channel, " / ", p.gate.y_channel))
        elseif p.filter_measure !== nothing
            # A filter's column can legitimately be absent from THIS frame — e.g. a cluster pop
            # (`clusters.{suffix}`) evaluated against a segmentation that didn't take part in that
            # clustering run. `fetch_cols` intersects with the table's columns and silently drops the
            # missing one, so an unguarded `df[!, col]` would raise `ArgumentError: column name … not
            # found` and 500 the whole plot. Degrade to empty membership + a warning instead: the pop
            # shows no members rather than taking down every other population on the map.
            (p.filter_measure in cols) ?
                parent_mask .& _filter_mask(df[!, p.filter_measure], p.filter_fun, p.filter_values, p.filter_default_all) :
                _missing_col_mask(n, path, string(p.filter_measure))
        else
            copy(parent_mask)
        end
        memb[path] = mask
    end
    m._labels = labels
    m._membership = memb
    m
end

_check_recomputed(m::PopulationMap) =
    m._membership === nothing && error("PopulationMap not computed — call recompute! first")

"""Boolean membership mask for a population (over the recompute row order)."""
function pop_membership(m::PopulationMap, path::AbstractString)::BitVector
    _check_recomputed(m)
    is_root(path) && return trues(length(m._labels))
    has_pop(m, path) || error("pop_membership: not found: $path")
    m._membership[String(path)]
end

"""Label IDs belonging to a population."""
cells_in_pop(m::PopulationMap, path::AbstractString) = m._labels[pop_membership(m, path)]

"""Count + percent-of-parent for a population."""
function pop_stats(m::PopulationMap, path::AbstractString)
    cnt = sum(pop_membership(m, path))
    parent = is_root(path) ? ROOT : m.pops[String(path)].parent
    pcnt = sum(pop_membership(m, parent))
    (count = cnt, parent_count = pcnt, pct_parent = pcnt > 0 ? 100 * cnt / pcnt : 0.0)
end
