# (De)serialisation + on-disk persistence. The wire shape is a nested tree — {name, gate, filter,
# children} — Python-readable (docs/POPULATION.md → Format). Files live at
# `{task_dir}/gating/{value_name}.json` and (for track populations) `{task_dir}/gating/{value_name}__tracks.json`;
# atomic write goes through `write_json_atomic`. Reads reconstruct a PopulationMap; writes flatten
# it back. Kept separate from mutations so a caller can serialise without pulling in the mutation API.

# ── (De)serialisation — nested tree {name, gate, filter, children}, Python-readable ─
# `include_transient=false` drops ephemeral pops (napari selection) — used for persistence so
# they never reach disk; the broadcast/serve path keeps them so the client stays in sync.
function _node_dict(m::PopulationMap, path::AbstractString; include_transient::Bool=true)::Dict{String,Any}
    p = m.pops[path]
    # UID is emitted BEFORE `colour`/`show` so a diff of the sidecar names the identity of each node
    # up front rather than buried after the tree ornaments.
    d = Dict{String,Any}("name" => p.name, "uid" => p.uid, "colour" => p.colour, "show" => p.show)
    p.gate !== nothing && (d["gate"] = gate_spec(p.gate))
    if p.filter_measure !== nothing
        d["filter"] = Dict{String,Any}("measure" => p.filter_measure,
                                       "fun"     => p.filter_fun === nothing ? nothing : string(p.filter_fun),
                                       "values"  => p.filter_values,
                                       "default_all" => p.filter_default_all)
        # compound filter (Decision 15): emit the AND-ed conditions so a multi-condition pop round-trips.
        p.filter_conditions === nothing ||
            (d["filter"]["conditions"] = [Dict{String,Any}("measure" => c.measure, "fun" => string(c.fun),
                                                           "values" => c.values) for c in p.filter_conditions])
    end
    p.boolean_op === nothing ||
        (d["boolean"] = Dict{String,Any}("op" => string(p.boolean_op), "pops" => p.boolean_pops,
                                         "not" => p.boolean_not))
    p.is_track && (d["is_track"] = true)
    p.transient && (d["transient"] = true)
    # explicit-label pops (the napari selection) have no gate/filter, so the client can't tell
    # from gate/filter alone that membership changed (e.g. the user resizes the selection shape).
    # Emit a membership signature so the client bumps its per-pop version and refreshes the plots.
    p.explicit_labels === nothing ||
        (d["membership_sig"] = string(hash(sort(p.explicit_labels))))
    children = direct_children(m, path)
    include_transient || (children = [c for c in children if !m.pops[c].transient])
    d["children"] = [_node_dict(m, c; include_transient = include_transient) for c in children]
    d
end

"""Serialise the map to a nested-tree dict. Pass `include_transient=false` to omit
ephemeral (napari-selection) pops — persistence uses this so they never hit disk."""
function to_tree(m::PopulationMap; include_transient::Bool=true)::Dict{String,Any}
    roots = direct_children(m, ROOT)
    include_transient || (roots = [r for r in roots if !m.pops[r].transient])
    out = Dict{String,Any}(
        "value_name" => m.value_name,
        "pop_type" => string(m.pop_type),
        # Which unit this file's SPATIAL gate coordinates are in. Written always (so a file this code
        # saves is self-describing); read back with a "px" default for pre-existing files.
        "spatial_unit" => m.spatial_unit,
        "populations" => [_node_dict(m, p; include_transient = include_transient) for p in roots],
    )
    # Only serialise the retired-UID set when non-empty — a project that has never had a pop deleted
    # keeps its sidecar visually clean, matching the pre-P2 file shape byte-for-byte.
    isempty(m.retired_uids) ||
        (out["retired_uids"] = sort!(collect(m.retired_uids)))
    # P3b breadcrumb (docs/todo/VN_VERSIONING_PLAN.md → P3b). Only emitted when set — a legacy
    # sidecar's shape is byte-for-byte preserved through a load/save round-trip that predates the
    # first labels-versioned write.
    m.authored_labels_version === nothing ||
        (out["authored_labels_version"] = m.authored_labels_version)
    out
end

function _add_node!(m::PopulationMap, node::AbstractDict, parent::AbstractString)
    g(k, default=nothing) = get(node, k, get(node, Symbol(k), default))
    gate = g("gate") === nothing ? nothing : gate_from_spec(g("gate"))
    flt = g("filter")
    bl = g("boolean")
    # UID pass-through: honour the sidecar's own uid when present (stable across sessions), else
    # let `add_pop!` generate one AND record that this map needs saving so the freshly-picked uid
    # doesn't get rerolled on every load. See MULTI_POP_TRACKING_PLAN.md Decision 0 (migration).
    raw_uid = String(g("uid", ""))
    isempty(raw_uid) && (m._uid_backfilled = true)
    path = add_pop!(m, String(g("name")); parent=parent, gate=gate, reserved_ok=true,
                    uid=raw_uid,
                    colour=String(g("colour", "#ffffff")), show=Bool(g("show", true)),
                    filter_measure = flt === nothing ? nothing : get(flt, "measure", get(flt, :measure, nothing)),
                    filter_fun     = flt === nothing ? nothing : get(flt, "fun", get(flt, :fun, nothing)),
                    filter_values  = flt === nothing ? nothing : get(flt, "values", get(flt, :values, nothing)),
                    filter_default_all = flt === nothing ? false : Bool(get(flt, "default_all", get(flt, :default_all, false))),
                    filter_conditions = flt === nothing ? nothing : get(flt, "conditions", get(flt, :conditions, nothing)),
                    is_track = Bool(g("is_track", false)), validate_refs = false,
                    boolean_op   = bl === nothing ? nothing : get(bl, "op", get(bl, :op, nothing)),
                    boolean_pops = bl === nothing ? nothing : get(bl, "pops", get(bl, :pops, nothing)),
                    boolean_not  = bl === nothing ? nothing : get(bl, "not", get(bl, :not, nothing)))
    for child in g("children", [])
        _add_node!(m, child, path)
    end
end

"""Build a map from a nested-tree dict."""
function from_tree(tree::AbstractDict)::PopulationMap
    g(k, default=nothing) = get(tree, k, get(tree, Symbol(k), default))
    # No stamp ⇒ "px": every gating file written before spatial gates moved to µm holds pixel
    # coordinates, and must keep evaluating as pixels (SPATIAL_GATE_UNITS_PLAN.md decision 4).
    m = PopulationMap(; pop_type=String(g("pop_type", "flow")), value_name=String(g("value_name", "default")),
                      spatial_unit=String(g("spatial_unit", SPATIAL_UNIT_PX)),
                      # P3b: absent on any file predating the breadcrumb (legacy default) — stays
                      # `nothing`, drift detection skips, `_latest` semantics unchanged.
                      authored_labels_version = let v = g("authored_labels_version", nothing)
                          v === nothing ? nothing : String(v)
                      end)
    # Retired-UID set (MULTI_POP_TRACKING_ORPHANS_PLAN P2): absent on a legacy sidecar → empty set,
    # unchanged behaviour for a project that has never had a pop deleted. Present entries seed the
    # collision guard in `_fresh_pop_uid` — the freshly-picked UID for a newly added pop is
    # guaranteed not to be one previously retired.
    for u in g("retired_uids", String[])
        push!(m.retired_uids, String(u))
    end
    for node in g("populations", [])
        _add_node!(m, node, ROOT)
    end
    m
end

# ── Persistence: {task_dir}/gating/{value_name}[__tracks].json ───────────────────
# Each pop_type with its OWN stored map gets a distinct sidecar suffix, mirroring the data source
# it gates over: `flow` → `gating/{vn}.json` (cell gates); `track` → `gating/{vn}__tracks.json`
# (per-track gates); `clust`/`trackclust` → `gating/{vn}__clust.json` / `__trackclust.json`
# (cluster-membership pops — a filter on the `clusters.{suffix}` column written by clustPops/
# clustTracks). The tree format + engine are identical across types (only the data source +
# membership rule differ). `live` is NOT here: it is derived off the `flow` map (no own file).
const POP_MAP_SUFFIX = Dict{String,String}(
    "track"      => TRACK_PROPS_SUFFIX,   # "__tracks"
    "clust"      => "__clust",
    "trackclust" => "__trackclust",
    "region"     => "__region",           # spatial region pops — a filter on `regions.{suffix}`
    "branch"     => BRANCH_PROPS_SUFFIX,  # "__branch" — skeleton branch pops (docs/todo/BRANCHING_PLAN.md)
)

# The pop types that are hand-drawn *gating* (a gate geometry per pop), as opposed to the
# filter/membership pop types (clust/trackclust) and the derived `live`. `flow` gates cells, `track`
# gates tracks — one abstraction over both so gating features (e.g. copy-to-images, the defining-plot
# view) treat them uniformly instead of special-casing flow. Single source of truth.
const GATING_POP_TYPES = ("flow", "track")
is_gating_pop_type(pop_type) = string(pop_type) in GATING_POP_TYPES
gating_dir(task_dir::AbstractString) = joinpath(task_dir, "gating")
gating_path(task_dir::AbstractString, value_name::AbstractString; pop_type::PopTypeArg="flow") =
    joinpath(gating_dir(task_dir), value_name * get(POP_MAP_SUFFIX, string(pop_type), "") * ".json")

"""Write the map to `{task_dir}/gating/{value_name}[__tracks].json` (by `m.pop_type`)."""
function save_pop_map!(m::PopulationMap, task_dir::AbstractString)
    dir = gating_dir(task_dir)
    isdir(dir) || mkpath(dir)
    path = gating_path(task_dir, m.value_name; pop_type=m.pop_type)
    # `write_json_atomic` (app/src/utils.jl) so a concurrent reader never observes a half-written
    # (truncated) JSON, and an interrupted write keeps the previous gates. This file was where that
    # tmp-then-rename pattern was first written by hand; it is now the shared mechanism every state
    # write uses. The load→mutate→save critical section is separately serialised by `_POPMAP_LOCK`
    # in the gating API handlers (against lost updates); this guards the file itself.
    write_json_atomic(path, to_tree(m; include_transient = false))
    m._uid_backfilled = false   # the just-saved sidecar now carries every uid
    m
end

"""Load the map from `{task_dir}/gating/{value_name}[__tracks].json` (empty map if absent).

**One-shot UID backfill**: a sidecar written before UIDs shipped (MULTI_POP_TRACKING_PLAN.md
Decision 0) has no `uid` on any node — `_add_node!` generates them and flips
`m._uid_backfilled = true`. This function then runs a `save_pop_map!` before returning so the
freshly-picked UIDs are stable across sessions (a next load would otherwise reroll them and any
outside reference — a `track_source` obs value, a lab-log capture — would drift). Once the sidecar
is UID-complete this branch never fires again.

`backfill_save = false` skips that write, for a caller that must stay read-only (the Kiwi ref resolver,
`api/src/kiwi_refs.jl`, which only asks whether a path exists — the rerolled uids are never used)."""
function load_pop_map(task_dir::AbstractString, value_name::AbstractString;
                      pop_type::PopTypeArg="flow", backfill_save::Bool=true)::PopulationMap
    path = gating_path(task_dir, value_name; pop_type=pop_type)
    isfile(path) || return PopulationMap(; pop_type=pop_type, value_name=value_name)
    m = from_tree(JSON3.read(read(path, String), Dict{String,Any}))
    (backfill_save && m._uid_backfilled) && save_pop_map!(m, task_dir)
    m
end

# CciaImage convenience (task_dir = img._dir). Also stamps the P3b `authored_labels_version`
# breadcrumb from the image's current `_latest` labels version at save time — the task_dir form
# has no image to resolve against and leaves the field alone (tests exercise both paths).
function save_pop_map!(m::PopulationMap, img::CciaImage)
    # Only stamp when a label_props entry actually exists for this value_name — a first-save on an
    # image that never ran segmentation has no version to point at (the breadcrumb stays `nothing`
    # and the drift banner never fires because there is no drift to detect).
    if haskey(img.label_props, m.value_name)
        m.authored_labels_version = resolve_version(img, :label_props, m.value_name)
    end
    save_pop_map!(m, img._dir)
end

