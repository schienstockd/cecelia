# Population + PopulationMap — the tree per value_name. Population is the leaf record (name,
# gate, filter, pop_type, colour, children path); PopulationMap holds one image's tree with a
# path→Population lookup and the traversal primitives (`walk`, `descendants`, `siblings`,
# `find_by_path`). Loaded after types.jl (embeds PopType) and before boolean/mutations/persistence,
# which all mutate this tree.

# ── Population ───────────────────────────────────────────────────────────────────
mutable struct Population
    # Stable identity — 6-char `gen_uid`, assigned at creation, persisted in the gating JSON, unchanged
    # by rename/move. `path` is the display identity (what the UI shows, what boolean references cite);
    # `uid` is the durable one that outlives a rename — the R version had this as `popID` and the port
    # dropped it, which is why per-pop obs like `track_source` (docs/todo/MULTI_POP_TRACKING_PLAN.md
    # Decision 0) couldn't be kept coherent across a rename by any amount of `_repath!` cascading.
    uid::String
    name::String
    path::String
    parent::String
    colour::String
    show::Bool
    pop_type::PopType             # see @enum PopType above
    value_name::String
    gate::Union{Gate,Nothing}              # flow
    # filtered-pop spec (clust/live; e.g. _tracked = filter_measure="track_id", fun=FILTER_GT, values=0)
    filter_measure::Union{String,Nothing}
    filter_fun::Union{FilterFun,Nothing}       # see @enum FilterFun above
    filter_values::Any
    filter_default_all::Bool
    # compound filter (Decision 15): a list of AND-ed `FilterCondition`s. `nothing` → the single
    # filter_measure/fun/values above (back-compat: existing sidecars). When set (non-empty), it is
    # the source of truth and the single fields mirror conditions[1] for readers that only look at
    # one. Lets a user-defined filter pop combine e.g. CD4>0.5 AND speed>5 in ONE pop.
    filter_conditions::Union{Vector{FilterCondition},Nothing}
    is_track::Bool
    # boolean membership (Decision 16): this pop's cells are a set operation over OTHER populations
    # in the same map, not a gate or a column — it LINKS existing gates. One form covers all three
    # cases the manager offers: included terms combined with AND or OR, minus every excluded term.
    #   nuc-GFP+ OR mem-TOM+                → op="or",  pops=[GFP,TOM]
    #   mem-TOM+ AND nuc-GFP+ BUT NOT CD169 → op="and", pops=[TOM,GFP], not=[CD169]
    #   NOT CD169 (a plain "not gate")      → op="and", pops=[],        not=[CD169]
    # Still ∩ parent like every other population, so an empty include list means "the parent's cells".
    boolean_op::Union{BoolMembership,Nothing}           # how `boolean_pops` combine — see @enum BoolMembership above
    boolean_pops::Union{Vector{String},Nothing}         # included terms (empty ⇒ all of the parent)
    boolean_not::Union{Vector{String},Nothing}          # excluded terms, always subtracted
    # explicit-label membership: when set, this pop's cells ARE these label IDs (∩ parent),
    # bypassing gate/filter. Used by the transient napari selection (docs/POPULATION.md) so a
    # spatial selection in napari lights up the same cells on the flow plots. Not persisted.
    # Label IDs are integers on disk (`label_props.jl` obs `label`, `_pick_sel::Dict{...,Vector{Int}}`
    # in `api/src/gating_api.jl`); typing here as `Vector{Int}` closes the last untyped hop between
    # the boundary and the engine, so `Set(p.explicit_labels)` and `Int.(p.explicit_labels)` need
    # no runtime element-type check.
    explicit_labels::Union{Vector{Int},Nothing}
    transient::Bool                        # ephemeral (napari selection) — never written to disk
end

# ── PopulationMap (a tree for one value_name) ────────────────────────────────────
mutable struct PopulationMap
    pop_type::PopType             # see @enum PopType above
    value_name::String
    pops::Dict{String,Population}          # path → Population
    order::Vector{String}                  # insertion order (parents before children)
    # uid → path lookup: kept in sync with `pops` by add_pop!/rename_pop!/move_pop!/del_pop! so a
    # per-pop obs column (`track_source`, docs/todo/MULTI_POP_TRACKING_PLAN.md) can resolve back to the
    # currently-named pop after a rename. The primary map stays keyed by path — the UI still reads by
    # path — this is the reverse index for outside references.
    uid_index::Dict{String,String}
    # UIDs of previously-deleted pops, appended by `_del_paths!` and honoured by `_fresh_pop_uid` so a
    # freshly-generated UID can never collide with a pop the user has already retired. The 6-char UID
    # space (56.8B) makes a live-map collision negligible in isolation, but a `track_source` obs value
    # in the h5ad outlives the pop that stamped it — reissuing the same UID would silently transfer
    # the old pop's lineage rows to a new pop on its first tracking run (MULTI_POP_TRACKING_ORPHANS_PLAN
    # decision 4). Persisted in the gating JSON; a legacy sidecar without the key loads with an empty
    # set and marks the map dirty on the next mutation so the field appears on the next save.
    retired_uids::Set{String}
    # Load-time backfill flag: `true` if any pop's uid was auto-generated during `from_tree` because
    # the source dict didn't carry one. `load_pop_map(task_dir, …)` uses this to `save_pop_map!`
    # immediately so the freshly-assigned uids become the STABLE ids across sessions rather than being
    # rerolled on every load. See MULTI_POP_TRACKING_PLAN.md Decision 0 (migration).
    _uid_backfilled::Bool
    # Which unit the SPATIAL gate coordinates (on `centroid_x`/`_y`/`_z` axes) are stored in:
    # "um" or "px". Persisted in the gating file; **absent ⇒ "px"**, which is every file written
    # before spatial gates moved to µm, so an unmigrated project keeps evaluating correctly
    # (docs/todo/SPATIAL_GATE_UNITS_PLAN.md, decision 4). `recompute!` scales the data to µm ONLY
    # when this says "um". Non-spatial gates are unaffected — an intensity gate has no pixel scale.
    spatial_unit::String
    # µm/px for this map's image, `[sz, sy, sx]` (`img_physical_sizes`), stamped by
    # `load_pop_map(img; …)`. NOT persisted: the scale belongs to the IMAGE, not the gate — storing it
    # would bake one image's pixel size into a gate and break copying it to another image (decision 2).
    # `nothing` = unknown (the task_dir form, or a hand-built map in a test) ⇒ no scaling.
    physical_sizes::Union{Vector{Float64},Nothing}
    # Labels vN that was current for this map's value_name at the last save (P3b breadcrumb, `docs/todo/
    # VN_VERSIONING_PLAN.md`). Stamped by `save_pop_map!(m, img)` from `resolve_version(img, :label_props,
    # m.value_name)`; persisted; `nothing` on legacy files and on the task_dir save form (no image →
    # nothing to resolve). Used by the API to detect drift against the image's current `_latest` and
    # surface the drift banner in the gating module.
    authored_labels_version::Union{String,Nothing}
    # SESSION-only pin: when set, every `label_props(img; value_name, version)` read for this map's
    # eval reads at this version instead of `_latest`. Set from the gating API's optional
    # `labelsVersion` query/body param (the "Use pinned vN" banner action). NOT persisted — a new
    # session opens with drift detected and the user re-picks.
    pinned_labels_version::Union{String,Nothing}
    # recompute cache (populated by gating_engine.recompute!)
    _labels::Union{Vector,Nothing}
    _membership::Union{Dict{String,BitVector},Nothing}
end

# The two spatial-unit values. "px" is the legacy default for a file with no stamp.
const SPATIAL_UNIT_PX = "px"
const SPATIAL_UNIT_UM = "um"

PopulationMap(; pop_type::Union{PopType,AbstractString}=POP_FLOW,
              value_name::AbstractString="default",
              spatial_unit::AbstractString=SPATIAL_UNIT_PX,
              physical_sizes::Union{AbstractVector{<:Real},Nothing}=nothing,
              authored_labels_version::Union{AbstractString,Nothing}=nothing,
              pinned_labels_version::Union{AbstractString,Nothing}=nothing) =
    PopulationMap(_coerce_pop_type(pop_type), String(value_name),
                  Dict{String,Population}(), String[],
                  Dict{String,String}(), Set{String}(), false,
                  String(spatial_unit),
                  physical_sizes === nothing ? nothing : Vector{Float64}(physical_sizes),
                  authored_labels_version === nothing ? nothing : String(authored_labels_version),
                  pinned_labels_version === nothing ? nothing : String(pinned_labels_version),
                  nothing, nothing)

Base.length(m::PopulationMap) = length(m.order)
pop_at(m::PopulationMap, path::AbstractString) = m.pops[String(path)]
has_pop(m::PopulationMap, path::AbstractString) = haskey(m.pops, String(path))
pop_paths(m::PopulationMap) = copy(m.order)

"""The stable UID of the population at `path` (see the `Population.uid` docstring)."""
pop_uid(m::PopulationMap, path::AbstractString) = m.pops[String(path)].uid

"""The path currently held by the population with the given UID, or `nothing` if unknown.
Use this to resolve a UID stored in an outside table (an obs column, another sidecar) back to the
currently-named pop after a rename/move."""
pop_path_by_uid(m::PopulationMap, uid::AbstractString) = get(m.uid_index, String(uid), nothing)

"""The `Population` with the given UID, or `nothing` if unknown."""
function pop_by_uid(m::PopulationMap, uid::AbstractString)
    p = pop_path_by_uid(m, uid)
    p === nothing ? nothing : m.pops[p]
end

# Pick a UID not already used in this map. Callers can pass an explicit `preferred` (deserialisation
# from disk) — that is honoured when free; a collision reroll runs the same generator until a fresh
# one lands. UIDs are 6-char `[A-Za-z0-9]` (see `gen_uid` in `app/src/utils.jl`); with the map's
# active-pop counts (single- to low-hundreds), the birthday probability of ever needing a reroll is
# vanishing — this exists so a hand-edited sidecar with two identical UIDs doesn't shadow one pop.
function _fresh_pop_uid(m::PopulationMap; preferred::AbstractString="")::String
    # `preferred` (a UID from a sidecar being deserialised) is honoured when it collides with
    # neither a live pop nor a retired one. A conflict with `retired_uids` shouldn't happen on a
    # well-formed sidecar — the writer keeps a UID either in `uid_index` XOR in `retired_uids` — but
    # a hand-edited file that resurrected a retired UID gets rerolled here rather than silently
    # inheriting the old pop's `track_source` lineage.
    if !isempty(preferred) && !haskey(m.uid_index, String(preferred)) &&
                              !(String(preferred) in m.retired_uids)
        return String(preferred)
    end
    while true
        candidate = gen_uid()
        (haskey(m.uid_index, candidate) || candidate in m.retired_uids) || return candidate
    end
end

"""
    has_spatial_gate(m) -> Bool

Does any population in `m` gate on a SPATIAL axis (`centroid_x`/`_y`/`_z`) — as a gate axis or as a
filter measure? Those are the only gates whose meaning depends on the image's pixel size, so this is
the predicate for "does copying this map to another image need that image to be calibrated"
(docs/todo/SPATIAL_GATE_UNITS_PLAN.md decision 7). A map of intensity/morphology gates is portable
regardless of calibration.
"""
function has_spatial_gate(m::PopulationMap)::Bool
    for path in m.order
        p = m.pops[path]
        if p.gate !== nothing
            (is_spatial_axis(String(p.gate.x_channel)) || is_spatial_axis(String(p.gate.y_channel))) &&
                return true
        end
        p.filter_measure === nothing ||
            (is_spatial_axis(String(p.filter_measure)) && return true)
        p.filter_conditions === nothing && continue
        # conditions are normalised NamedTuples (`_normalise_conditions`), so `.measure` is a String
        for c in p.filter_conditions
            is_spatial_axis(String(c.measure)) && return true
        end
    end
    false
end

"""Direct children of `parent` (insertion order)."""
direct_children(m::PopulationMap, parent::AbstractString) =
    [p for p in m.order if m.pops[p].parent == String(parent)]

"""All descendants of `path` (any depth)."""
descendants(m::PopulationMap, path::AbstractString) =
    [p for p in m.order if startswith(p, String(path) * "/")]

