# The population-type registry — ONE row per pop type, holding the properties code branches on.
#
# Every list of pop types the rest of the code needs is DERIVED from this table: the sidecar suffixes
# (`POP_MAP_SUFFIX`), the hand-drawn types (`GATING_POP_TYPES`), the stored types a walk of the
# gating maps covers (`STORED_POP_TYPES`), the track-grained / cluster-family predicates, the picker
# category, the board's track families. Hand-written lists drifted apart (one walk lacked `branch`,
# a legend lacked `region`), so a new list of pop-type literals outside this file fails the
# `pop-type registry ratchet` (app/test/suite/ratchets.jl); a deliberate subset says why on its line
# with `POP-TYPES-OK: <reason>`.
#
# The frontend reads the same table from `frontend/src/plots/popTypeRegistry.json`, which is
# `pop_type_registry_wire()` written out (`write_pop_type_registry_json()`); a test fails when the two
# differ. See docs/POPULATION.md → *Population types*.

struct PopTypeInfo
    type::PopType
    label::String                       # user-facing name in plot pickers ("Gated", not "flow")
    suffix::Union{String,Nothing}       # own sidecar `gating/{vn}<suffix>.json`; nothing = no own file
    map_type::Union{PopType,Nothing}    # the stored map its pops live in (live → flow); nothing = none
    gating::Bool                        # pops are hand-drawn gate geometry (undo/redo, copy, gating QC)
    cluster::Bool                       # pops are `filter_fun="in"` filters on a per-run cluster column
    cluster_prefix::String              # that column's family prefix (`clusters.` / `regions.`), else ""
    grain::Symbol                       # rows its pops are evaluated over: :cell | :track | :branch
    category::String                    # picker category: "gated" | "clustered" | "region"
    track_family::Bool                  # a population of tracks on the board's track plots
end

# Order is load-bearing only where noted: `STORED_POP_TYPES` keeps flow, track, clust, trackclust,
# region, branch (the order every gating-map walk used), and `TRACK_FAMILY_POP_TYPES` keeps live,
# track, trackclust (the track plots' picker order; the first is the default).
const POP_TYPE_REGISTRY = PopTypeInfo[
    PopTypeInfo(POP_FLOW,       "Gated",           "",                 POP_FLOW,       true,  false, "",          :cell,   "gated",     false),
    PopTypeInfo(POP_LIVE,       "Tracked",         nothing,            POP_FLOW,       false, false, "",          :cell,   "gated",     true),
    PopTypeInfo(POP_TRACK,      "Tracked (gated)", TRACK_PROPS_SUFFIX, POP_TRACK,      true,  false, "",          :track,  "gated",     true),
    PopTypeInfo(POP_CLUST,      "Cell clusters",   "__clust",          POP_CLUST,      false, true,  "clusters.", :cell,   "clustered", false),
    PopTypeInfo(POP_TRACKCLUST, "Track clusters",  "__trackclust",     POP_TRACKCLUST, false, true,  "clusters.", :track,  "clustered", true),
    PopTypeInfo(POP_REGION,     "Regions",         "__region",         POP_REGION,     false, true,  "regions.",  :cell,   "region",    false),
    PopTypeInfo(POP_BRANCH,     "Branches",        BRANCH_PROPS_SUFFIX, POP_BRANCH,    false, false, "",          :branch, "gated",     false),
    PopTypeInfo(POP_LABELS,     "All cells",       nothing,            nothing,        false, false, "",          :cell,   "gated",     false),
]
const _POP_TYPE_INFO = Dict{String,PopTypeInfo}(string(i.type) => i for i in POP_TYPE_REGISTRY)

"""`pop_type_info(pt)` → the registry row for `pt` (enum or wire string), or `nothing` for an unknown
string — callers keep their old "unknown → not that kind" answer instead of throwing."""
pop_type_info(pt::PopType)::PopTypeInfo = _POP_TYPE_INFO[string(pt)]
pop_type_info(pt)::Union{PopTypeInfo,Nothing} = get(_POP_TYPE_INFO, string(pt), nothing)

_pop_type_names(pred) = Tuple(string(i.type) for i in POP_TYPE_REGISTRY if pred(i))

"""Every pop type with its own gating sidecar — what a walk of an image's persisted maps covers."""
const STORED_POP_TYPES = _pop_type_names(i -> i.suffix !== nothing)
"""The hand-drawn gating types (a gate geometry per pop): `flow` gates cells, `track` gates tracks."""
const GATING_POP_TYPES = _pop_type_names(i -> i.gating)
"""The cluster-filter families (`clust`, `trackclust`, `region`)."""
const CLUSTER_POP_TYPES = _pop_type_names(i -> i.cluster)
"""The board's populations of tracks (`live`, `track`, `trackclust`), in picker order."""
const TRACK_FAMILY_POP_TYPES = _pop_type_names(i -> i.track_family)
"""`gating/{vn}<suffix>.json` per type that has a non-empty suffix (flow's file has none; `live` reads
flow's file; `labels` has none). `gating_path` falls back to `""`."""
const POP_MAP_SUFFIX = Dict{String,String}(string(i.type) => i.suffix for i in POP_TYPE_REGISTRY
                                           if i.suffix !== nothing && !isempty(i.suffix))

# The type `resolve_pop_type` probes when a picker value carries only a path: priority, not
# membership. `flow` first so a plain gate resolves to cells, not to a like-named cluster/region pop.
const POP_TYPE_PROBE_ORDER = ("flow", "clust", "region", "track", "trackclust", "branch")
Set(POP_TYPE_PROBE_ORDER) == Set(STORED_POP_TYPES) ||
    error("POP_TYPE_PROBE_ORDER must order exactly the STORED_POP_TYPES")

is_gating_pop_type(pop_type)::Bool = (i = pop_type_info(pop_type); i !== nothing && i.gating)
_is_cluster_pop_type(pop_type)::Bool = (i = pop_type_info(pop_type); i !== nothing && i.cluster)

"""True when a pop_type's populations are evaluated over the PER-TRACK table (one row per track,
`label == track_id`): `track` (hand-drawn track gates) and `trackclust` (a `clusters.{suffix}` filter).
Every other pop_type's rows are cells (or branches)."""
is_track_grained(pop_type)::Bool = (i = pop_type_info(pop_type); i !== nothing && i.grain === :track)

"""The stored map a pop type's populations live in, as a wire string (`live` → `"flow"`), or
`nothing` (`labels`, unknown)."""
function pop_map_type(pop_type)::Union{String,Nothing}
    i = pop_type_info(pop_type)
    (i === nothing || i.map_type === nothing) ? nothing : string(i.map_type)
end

"""The registry as JSON-ready rows (field order fixed) — what the frontend snapshot holds."""
pop_type_registry_wire() = [(;
        name          = string(i.type),
        label         = i.label,
        suffix        = i.suffix,
        mapType       = i.map_type === nothing ? nothing : string(i.map_type),
        gating        = i.gating,
        cluster       = i.cluster,
        clusterPrefix = i.cluster_prefix,
        grain         = string(i.grain),
        category      = i.category,
        trackFamily   = i.track_family,
    ) for i in POP_TYPE_REGISTRY]

const POP_TYPE_REGISTRY_JSON = normpath(joinpath(@__DIR__, "..", "..", "..", "..", "frontend", "src",
                                                 "plots", "popTypeRegistry.json"))

"""Write the frontend's copy of the registry. Run after editing `POP_TYPE_REGISTRY`:
`pixi run julia --project=app -e 'using Cecelia; Cecelia.write_pop_type_registry_json()'`."""
write_pop_type_registry_json(path::AbstractString = POP_TYPE_REGISTRY_JSON) =
    write_atomic(path) do io
        JSON3.pretty(io, pop_type_registry_wire(), JSON3.AlignmentContext(indent = 2)); println(io)
    end
