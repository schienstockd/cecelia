# ── Track class — one sub-population call per track (compute-on-read) ─────────────
#
# A tracked population's cells flicker between its sub-populations from frame to frame (a cell just
# under a quadrant line one frame, just over it the next). The class gives each TRACK one call:
# the fraction of its cells in each sub-population, and the sub-population holding a STRICT majority
# above the threshold — else `mixed`. docs/todo/TRACK_SUBPOP_GATING_PLAN.md Decisions 5 and 8.
#
# - Sub-populations: the parent's children `level` levels down (a leaf above that level stands for
#   itself). Its `tracked` child (`track_id > 0`, "Add tracked cells") is the parent again, not a
#   sub-population, and is skipped.
# - Tracks: every track with at least one cell in the parent ("any cell", as track gating).
# - Denominator: every cell of the track, wherever it now sits — a cell outside every sub-population
#   (a gate moved after tracking) counts against purity. Two cells of one track in one frame (a
#   merge) count once each; a gap adds nothing.
# - Overlapping sub-populations (two gates that share cells) are each counted on their own; the class
#   is the one largest fraction above the threshold, a tie is `mixed`.
#
# Nothing is stored but the threshold and level, on the parent (`Population.track_class`), so a change
# re-stamps the map and every cached read.

const TRACK_CLASS_THRESHOLD = 0.5
const TRACK_CLASS_MIXED = "mixed"

"""
    track_class_settings(p) -> (; threshold, level)

The class settings stored on population `p` (`trackClass`), else the defaults (a strict majority,
the children one level down). `p === nothing` (root) gets the defaults.
"""
function track_class_settings(p)
    tc = p === nothing ? nothing : p.track_class
    th = tc === nothing ? TRACK_CLASS_THRESHOLD : Float64(get(tc, "threshold", TRACK_CLASS_THRESHOLD))
    lv = tc === nothing ? 1 : Int(get(tc, "level", 1))
    (; threshold = th, level = lv)
end

"""Is `(threshold, level)` a usable class setting? → `nothing`, or the reason it is not."""
function track_class_setting_error(threshold, level)
    (threshold isa Real && 0 <= threshold < 1) || return "threshold must be at least 0 and below 1"
    (level isa Integer && level >= 1) || return "level must be 1 or more"
    nothing
end

# `P/tracked` ("Add tracked cells") holds the parent's tracked cells — the parent again, not a class.
# It carries the derived `_tracked`'s rule (`_DERIVED_POPS`), so that spec is what it is matched on.
function _is_tracked_child(p)
    spec = _DERIVED_POPS["_tracked"]
    p.filter_measure == spec.filter_measure && p.filter_fun == _coerce_filter_fun(spec.filter_fun) &&
        (p.filter_conditions === nothing || length(p.filter_conditions) == 1) &&
        length(vcat(p.filter_values)) == 1 && Float64(only(vcat(p.filter_values))) == spec.filter_values
end

_class_kids(m::PopulationTree, path) =
    [c for c in direct_children(m, path) if !m.pops[c].transient && !_is_tracked_child(m.pops[c])]

"""
    track_class_children(m, parent; level = 1) -> Vector{String}

The sub-populations a track of `parent` is called among: its children `level` levels down, a leaf
above that level standing for itself. Transient pops and the `tracked` child are skipped.
"""
function track_class_children(m::PopulationTree, parent::AbstractString; level::Integer = 1)::Vector{String}
    front = _class_kids(m, parent)
    for _ in 2:level
        front = reduce(vcat, [(k = _class_kids(m, c); isempty(k) ? [c] : k) for c in front]; init = String[])
    end
    front
end

"""How many levels of sub-populations sit under `parent` (0 = none) — the level picker's range."""
function track_class_max_level(m::PopulationTree, parent::AbstractString)::Int
    kids = _class_kids(m, parent)
    isempty(kids) ? 0 : 1 + maximum(track_class_max_level(m, c) for c in kids)
end

"""
    track_class_table(track_ids, members, children; threshold) -> DataFrame

The pure core. `track_ids[i]` is cell i's track, `members[j][i]` whether cell i is in sub-population
`children[j]`. One row per track (sorted): `track_id`, `num_cells`, one `frac.<child>` column per
sub-population (its cells over all the track's cells), and `class` — the child with the single largest
fraction when that fraction is **above** `threshold`, else `TRACK_CLASS_MIXED`.
"""
function track_class_table(track_ids::AbstractVector{<:Integer}, members::AbstractVector,
                           children::AbstractVector{<:AbstractString};
                           threshold::Real = TRACK_CLASS_THRESHOLD)::DataFrame
    length(members) == length(children) || error("track_class_table: one member vector per child")
    ids = sort(unique(track_ids))
    row = Dict(t => i for (i, t) in enumerate(ids))
    n = zeros(Int, length(ids)); cnt = zeros(Int, length(ids), length(children))
    for (c, t) in enumerate(track_ids)
        i = row[t]; n[i] += 1
        for j in eachindex(members)
            members[j][c] && (cnt[i, j] += 1)
        end
    end
    frac = cnt ./ max.(n, 1)
    class = map(eachindex(ids)) do i
        isempty(children) && return TRACK_CLASS_MIXED
        best = maximum(@view frac[i, :])
        top = findall(==(best), @view frac[i, :])
        length(top) == 1 && best > threshold ? String(children[top[1]]) : TRACK_CLASS_MIXED
    end
    df = DataFrame(track_id = collect(Int, ids), num_cells = n)
    for (j, c) in enumerate(children)
        df[!, "frac.$c"] = frac[:, j]
    end
    df[!, :class] = class
    df
end

"""
    track_classes(img; value_name, parent, threshold = nothing, level = nothing)
        -> (; parent, children, threshold, level, max_level, table)

The class of every track of `parent` (a `flow` population) — `track_class_table` over the image's
cells. `threshold` / `level` default to what `parent` stores (`track_class_settings`). An untracked
segmentation, or a parent with no sub-populations, gives an empty table.
"""
function track_classes(img::CciaImage; value_name::Union{AbstractString,Nothing} = nothing,
                       parent::AbstractString, threshold::Union{Real,Nothing} = nothing,
                       level::Union{Integer,Nothing} = nothing)
    vn = resolve_value_name(img, value_name)
    m = load_pop_tree(img; value_name = vn, pop_type = "flow")
    par = is_root(parent) ? ROOT : String(parent)
    (par == ROOT || has_pop(m, par)) || error("track_classes: population not found: $parent")
    s = track_class_settings(par == ROOT ? nothing : m.pops[par])
    th = something(threshold, s.threshold); lv = something(level, s.level)
    kids = track_class_children(m, par; level = lv)
    maxl = track_class_max_level(m, par)
    empty_out = (; parent = par, children = kids, threshold = th, level = lv, max_level = maxl,
                 table = track_class_table(Int[], [Bool[] for _ in kids], kids; threshold = th))
    (isempty(kids) || !is_tracked(img; value_name = vn)) && return empty_out
    df = pop_df(img, "flow", unique(vcat(ROOT, par, kids)); value_name = vn, pop_cols = ["track_id"],
                unique_labels = false)
    nrow(df) == 0 && return empty_out
    labels_of(p) = Set(df.label[df.pop .== p])
    all_rows = findall(==(ROOT), df.pop)
    tracked = [i for i in all_rows if (t = df.track_id[i]; t isa Real && isfinite(t) && t > 0)]
    tid = Int[Int(df.track_id[i]) for i in tracked]
    lab = df.label[tracked]
    # the parent's tracks: any cell in it
    par_labels = par == ROOT ? nothing : labels_of(par)
    in_par = par == ROOT ? Set(tid) : Set(t for (t, l) in zip(tid, lab) if l in par_labels)
    keep = [t in in_par for t in tid]
    members = [(s = labels_of(k); BitVector(l in s for l in lab[keep])) for k in kids]
    (; parent = par, children = kids, threshold = th, level = lv, max_level = maxl,
       table = track_class_table(tid[keep], members, kids; threshold = th))
end

"""Tracks per class (`children` order, then `mixed`) and the share that is mixed (0–100; NaN with no tracks)."""
function track_class_summary(r)
    counts = [count(==(c), r.table.class) for c in r.children]
    mixed = count(==(TRACK_CLASS_MIXED), r.table.class)
    total = nrow(r.table)
    (; counts, mixed, total, mixed_pct = total == 0 ? NaN : 100 * mixed / total)
end

"""
    track_step_children(img; value_name, parent, level = nothing) -> (; children, step)

Each tracked cell of `parent`'s sub-populations, keyed `(track_id, frame)` → the index (into
`children`) of the first sub-population holding it — the per-step ("sub-population") ribbon colour.
A step whose cell is in none is absent; a step two sub-populations share (overlapping gates, or two
cells of one track in one frame) takes the first in `children` order.
"""
function track_step_children(img::CciaImage; value_name::Union{AbstractString,Nothing} = nothing,
                             parent::AbstractString, level::Union{Integer,Nothing} = nothing)
    vn = resolve_value_name(img, value_name)
    m = load_pop_tree(img; value_name = vn, pop_type = "flow")
    par = is_root(parent) ? ROOT : String(parent)
    (par == ROOT || has_pop(m, par)) || error("track_step_children: population not found: $parent")
    lv = something(level, track_class_settings(par == ROOT ? nothing : m.pops[par]).level)
    kids = track_class_children(m, par; level = lv)
    step = Dict{Tuple{Int,Int},Int}()
    (isempty(kids) || !is_tracked(img; value_name = vn)) && return (; children = kids, step)
    df = pop_df(img, "flow", kids; value_name = vn, pop_cols = ["track_id", "centroid_t"], unique_labels = false)
    nrow(df) == 0 && return (; children = kids, step)
    order = Dict(k => j for (j, k) in enumerate(kids))
    for i in 1:nrow(df)
        t = df.track_id[i]; f = df.centroid_t[i]
        (t isa Real && isfinite(t) && t > 0 && f isa Real && isfinite(f)) || continue
        key = (Int(t), Int(round(f))); j = order[df.pop[i]]
        step[key] = min(get(step, key, j), j)
    end
    (; children = kids, step)
end
