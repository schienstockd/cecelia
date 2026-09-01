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

# A boolean pop references a population that isn't in this map (deleted behind its back, or a
# hand-edited sidecar) → empty membership + a warning, the same degrade as a missing column. Never a
# 500: one broken reference must not take down every other population on the map.
function _missing_ref_mask(n::Int, path::AbstractString, ref::AbstractString)::BitVector
    @warn "gating: population '$path' combines '$ref', which no longer exists — treating as empty \
           (no members). Edit it to point at a population that is still there."
    falses(n)
end

# ── Boolean membership (Decision 16) ──────────────────────────────────────────────
# Included terms combined with AND or OR, then every excluded term subtracted:
#   op="or",  pops=[GFP,TOM]              → GFP ∪ TOM
#   op="and", pops=[TOM,GFP], not=[CD169] → (TOM ∩ GFP) ∖ CD169
#   op="and", pops=[],        not=[CD169] → ∖ CD169 alone, i.e. "everything here except CD169"
# The referenced masks are each pop's OWN membership, which already includes its ancestors — so a
# combination of two gates in different branches means what it reads as. `∩ parent` is applied by the
# caller, as for every other population.
function _boolean_mask(p, memb::Dict{String,BitVector}, n::Int, path::AbstractString)::BitVector
    inc = p.boolean_pops === nothing ? String[] : p.boolean_pops
    exc = p.boolean_not  === nothing ? String[] : p.boolean_not
    orop = p.boolean_op == "or"
    mask = trues(n)                        # no included term ⇒ start from the parent's cells
    if !isempty(inc)
        mask = orop ? falses(n) : trues(n)
        for r in inc
            haskey(memb, r) || return _missing_ref_mask(n, path, r)
            mask = orop ? (mask .| memb[r]) : (mask .& memb[r])
        end
    end
    for r in exc
        haskey(memb, r) || return _missing_ref_mask(n, path, r)
        mask = mask .& .!memb[r]
    end
    mask
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
        p.filter_conditions === nothing || append!(cols, (c.measure for c in p.filter_conditions))
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
    # ── spatial axes: put the DATA in the same unit as the GATE, here and nowhere else ──
    # Gate coordinates on a `centroid_x`/`_y`/`_z` axis are stored in µm (once the map says so), while
    # the cell table always holds pixels (docs/DATAMODEL.md). Converting here — not in each caller's
    # fetch closure — is deliberate: `recompute!` has SIX call sites, five of which build their own
    # closure (gating_api, _pop_df, _pop_df_tracks, resolve_pops, bayesian_tracking, branching), so a
    # per-caller conversion would drift and a population's members would depend on which code path
    # resolved it. See docs/todo/SPATIAL_GATE_UNITS_PLAN.md decision 3.
    #
    # Only when the map is stamped "um" AND its image supplied a real pixel size: a legacy (unstamped)
    # file holds pixel coordinates and must keep comparing against pixels, and an uncalibrated image has
    # no µm to convert to. `scale_centroids!` is the one shared conversion; it is a no-op on a frame
    # with no centroid columns, which is every intensity-only gate.
    if m.spatial_unit == SPATIAL_UNIT_UM && m.physical_sizes !== nothing
        df = scale_centroids!(copy(df), m.physical_sizes)   # copy: never mutate the caller's frame
    end
    labels = df.label
    n = length(labels)
    cols = Set(names(df))
    memb = Dict{String,BitVector}()
    memb[ROOT] = trues(n)
    for path in topo_order(m)
        p = m.pops[path]
        parent_mask = get(memb, p.parent, trues(n))
        mask = if p.explicit_labels !== nothing
            # membership IS this label set (∩ parent) — the pick selection (docs/POPULATION.md)
            sel = Set(p.explicit_labels)
            parent_mask .& BitVector(l in sel for l in labels)
        elseif p.boolean_op !== nothing
            # a set operation over other populations — no data column of its own (Decision 16)
            parent_mask .& _boolean_mask(p, memb, n, path)
        elseif p.gate !== nothing
            # a gate whose axis column is absent from the fetched frame → no members, not a crash. See
            # the filter case below for the rationale (missing column ≠ hard 500).
            (p.gate.x_channel in cols && p.gate.y_channel in cols) ?
                parent_mask .& inside(p.gate, df[!, p.gate.x_channel], df[!, p.gate.y_channel]) :
                _missing_col_mask(n, path, string(p.gate.x_channel, " / ", p.gate.y_channel))
        elseif p.filter_conditions !== nothing
            # compound filter (Decision 15): AND every condition's mask. A missing column degrades the
            # WHOLE pop to empty + a warning (same rationale as the single-filter case below), never a 500.
            cmask = trues(n)
            for c in p.filter_conditions
                if c.measure in cols
                    cmask .&= _filter_mask(df[!, c.measure], c.fun, c.values, false)
                else
                    cmask = _missing_col_mask(n, path, String(c.measure)); break
                end
            end
            parent_mask .& cmask
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
