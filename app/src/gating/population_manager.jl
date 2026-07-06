using JSON3

# ── Population manager ───────────────────────────────────────────────────────────
#
# The generic abstraction for all population types (flow/clust/live). A population is a
# node in a tree: it has a name, hierarchical path, colour, and a membership definition —
# a `gate` (flow) or a filter spec (clust/live; see docs/POPULATION.md). Membership is
# derived (gating_engine.jl), never stored in the H5AD.
#
# Persistence: per-segmentation sidecar `{task_dir}/gating/{value_name}.json` (NOT ccid.json).

const ROOT = "root"

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

# ── Population ───────────────────────────────────────────────────────────────────
mutable struct Population
    name::String
    path::String
    parent::String
    colour::String
    show::Bool
    pop_type::String
    value_name::String
    gate::Union{Gate,Nothing}              # flow
    # filtered-pop spec (clust/live; e.g. _tracked = filter_measure="track_id", fun="gt", values=0)
    filter_measure::Union{String,Nothing}
    filter_fun::Union{String,Nothing}      # gt|gte|lt|lte|eq|neq|in
    filter_values::Any
    filter_default_all::Bool
    is_track::Bool
    # explicit-label membership: when set, this pop's cells ARE these label IDs (∩ parent),
    # bypassing gate/filter. Used by the transient napari selection (docs/POPULATION.md) so a
    # spatial selection in napari lights up the same cells on the flow plots. Not persisted.
    explicit_labels::Union{Vector,Nothing}
    transient::Bool                        # ephemeral (napari selection) — never written to disk
end

# ── PopulationMap (a tree for one value_name) ────────────────────────────────────
mutable struct PopulationMap
    pop_type::String
    value_name::String
    pops::Dict{String,Population}          # path → Population
    order::Vector{String}                  # insertion order (parents before children)
    # recompute cache (populated by gating_engine.recompute!)
    _labels::Union{Vector,Nothing}
    _membership::Union{Dict{String,BitVector},Nothing}
end

PopulationMap(; pop_type::AbstractString="flow", value_name::AbstractString="default") =
    PopulationMap(String(pop_type), String(value_name),
                  Dict{String,Population}(), String[], nothing, nothing)

Base.length(m::PopulationMap) = length(m.order)
pop_at(m::PopulationMap, path::AbstractString) = m.pops[String(path)]
has_pop(m::PopulationMap, path::AbstractString) = haskey(m.pops, String(path))
pop_paths(m::PopulationMap) = copy(m.order)

"""Direct children of `parent` (insertion order)."""
direct_children(m::PopulationMap, parent::AbstractString) =
    [p for p in m.order if m.pops[p].parent == String(parent)]

"""All descendants of `path` (any depth)."""
descendants(m::PopulationMap, path::AbstractString) =
    [p for p in m.order if startswith(p, String(path) * "/")]

"""Paths in parent-before-child order (stable by depth)."""
topo_order(m::PopulationMap) = sort(m.order; by = p -> count(==('/'), p), alg = MergeSort)

# ── Mutations ────────────────────────────────────────────────────────────────────
function add_pop!(m::PopulationMap, name::AbstractString;
                  parent::AbstractString=ROOT, gate::Union{Gate,Nothing}=nothing,
                  colour::AbstractString="#ffffff", show::Bool=true,
                  filter_measure=nothing, filter_fun=nothing, filter_values=nothing,
                  filter_default_all::Bool=false, is_track::Bool=false,
                  explicit_labels=nothing, transient::Bool=false,
                  reserved_ok::Bool=false)::String
    # `_`-prefixed names are reserved for derived populations (e.g. _tracked); only the derived
    # injection (reserved_ok=true) may create them, so a hand-drawn gate can't shadow one.
    (reserved_ok || !is_reserved_pop_name(name)) ||
        error("add_pop!: names beginning with \"$DERIVED_POP_PREFIX\" are reserved for derived " *
              "populations (e.g. _tracked) — choose another name")
    parent = is_root(parent) ? ROOT : String(parent)
    (parent == ROOT || has_pop(m, parent)) || error("add_pop!: parent not found: $parent")
    path = pop_path(parent, name)
    has_pop(m, path) && error("add_pop!: population already exists: $path")
    m.pops[path] = Population(String(name), path, parent, String(colour), show,
                              m.pop_type, m.value_name, gate,
                              filter_measure === nothing ? nothing : String(filter_measure),
                              filter_fun === nothing ? nothing : String(filter_fun),
                              filter_values, filter_default_all, is_track,
                              explicit_labels === nothing ? nothing : collect(explicit_labels), transient)
    push!(m.order, path)
    _invalidate!(m)
    path
end

function set_gate!(m::PopulationMap, path::AbstractString, gate::Gate)
    has_pop(m, path) || error("set_gate!: not found: $path")
    m.pops[String(path)].gate = gate
    _invalidate!(m)
    m
end

"""Rename a population, cascading the path change to all descendants."""
function rename_pop!(m::PopulationMap, path::AbstractString, newname::AbstractString)::String
    has_pop(m, path) || error("rename_pop!: not found: $path")
    !is_reserved_pop_name(newname) ||
        error("rename_pop!: names beginning with \"$DERIVED_POP_PREFIX\" are reserved for " *
              "derived populations (e.g. _tracked) — choose another name")
    path = String(path)
    p = m.pops[path]
    newpath = pop_path(p.parent, newname)
    newpath == path && return path
    has_pop(m, newpath) && error("rename_pop!: target exists: $newpath")
    affected = [path; descendants(m, path)]
    for old in affected
        np = _replace_prefix(old, path, newpath)
        pop = m.pops[old]
        pop.path = np
        pop.parent = _replace_prefix(pop.parent, path, newpath)
        pop.name = pop_name(np)
        pop.value_name = m.value_name
        if np != old
            m.pops[np] = pop
            delete!(m.pops, old)
        end
    end
    m.order = [_replace_prefix(o, path, newpath) for o in m.order]
    _invalidate!(m)
    newpath
end

"""Delete a population and all its descendants (cascade)."""
function del_pop!(m::PopulationMap, path::AbstractString)
    has_pop(m, path) || error("del_pop!: not found: $path")
    path = String(path)
    targets = Set([path; descendants(m, path)])
    for t in targets
        delete!(m.pops, t)
    end
    filter!(p -> !(p in targets), m.order)
    _invalidate!(m)
    m
end

_invalidate!(m::PopulationMap) = (m._labels = nothing; m._membership = nothing; m)

# ── (De)serialisation — nested tree {name, gate, filter, children}, Python-readable ─
# `include_transient=false` drops ephemeral pops (napari selection) — used for persistence so
# they never reach disk; the broadcast/serve path keeps them so the client stays in sync.
function _node_dict(m::PopulationMap, path::AbstractString; include_transient::Bool=true)::Dict{String,Any}
    p = m.pops[path]
    d = Dict{String,Any}("name" => p.name, "colour" => p.colour, "show" => p.show)
    p.gate !== nothing && (d["gate"] = gate_spec(p.gate))
    if p.filter_measure !== nothing
        d["filter"] = Dict{String,Any}("measure" => p.filter_measure, "fun" => p.filter_fun,
                                       "values" => p.filter_values, "default_all" => p.filter_default_all)
    end
    p.is_track && (d["is_track"] = true)
    p.transient && (d["transient"] = true)
    # explicit-label pops (the napari selection) have no gate/filter, so the client can't tell
    # from gate/filter alone that membership changed (e.g. the user resizes the selection shape).
    # Emit a membership signature so the client bumps its per-pop version and refreshes the plots.
    p.explicit_labels === nothing ||
        (d["membership_sig"] = string(hash(sort(collect(Int, p.explicit_labels)))))
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
    Dict{String,Any}(
        "value_name" => m.value_name,
        "pop_type" => m.pop_type,
        "populations" => [_node_dict(m, p; include_transient = include_transient) for p in roots],
    )
end

function _add_node!(m::PopulationMap, node::AbstractDict, parent::AbstractString)
    g(k, default=nothing) = get(node, k, get(node, Symbol(k), default))
    gate = g("gate") === nothing ? nothing : gate_from_spec(g("gate"))
    flt = g("filter")
    path = add_pop!(m, String(g("name")); parent=parent, gate=gate, reserved_ok=true,
                    colour=String(g("colour", "#ffffff")), show=Bool(g("show", true)),
                    filter_measure = flt === nothing ? nothing : get(flt, "measure", get(flt, :measure, nothing)),
                    filter_fun     = flt === nothing ? nothing : get(flt, "fun", get(flt, :fun, nothing)),
                    filter_values  = flt === nothing ? nothing : get(flt, "values", get(flt, :values, nothing)),
                    filter_default_all = flt === nothing ? false : Bool(get(flt, "default_all", get(flt, :default_all, false))),
                    is_track = Bool(g("is_track", false)))
    for child in g("children", [])
        _add_node!(m, child, path)
    end
end

"""Build a map from a nested-tree dict."""
function from_tree(tree::AbstractDict)::PopulationMap
    g(k, default=nothing) = get(tree, k, get(tree, Symbol(k), default))
    m = PopulationMap(; pop_type=String(g("pop_type", "flow")), value_name=String(g("value_name", "default")))
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
)
gating_dir(task_dir::AbstractString) = joinpath(task_dir, "gating")
gating_path(task_dir::AbstractString, value_name::AbstractString; pop_type::AbstractString="flow") =
    joinpath(gating_dir(task_dir), value_name * get(POP_MAP_SUFFIX, pop_type, "") * ".json")

"""Write the map to `{task_dir}/gating/{value_name}[__tracks].json` (by `m.pop_type`)."""
function save_pop_map!(m::PopulationMap, task_dir::AbstractString)
    dir = gating_dir(task_dir)
    isdir(dir) || mkpath(dir)
    path = gating_path(task_dir, m.value_name; pop_type=m.pop_type)
    # Write to a temp file then atomically rename, so a concurrent reader never observes a
    # half-written (truncated) JSON. The load→mutate→save critical section is itself serialised
    # by `_POPMAP_LOCK` in the gating API handlers (against lost updates); this guards the file.
    tmp = path * ".tmp"
    open(tmp, "w") do f
        JSON3.pretty(f, to_tree(m; include_transient = false))
    end
    mv(tmp, path; force = true)
    m
end

"""Load the map from `{task_dir}/gating/{value_name}[__tracks].json` (empty map if absent)."""
function load_pop_map(task_dir::AbstractString, value_name::AbstractString;
                      pop_type::AbstractString="flow")::PopulationMap
    path = gating_path(task_dir, value_name; pop_type=pop_type)
    isfile(path) || return PopulationMap(; pop_type=pop_type, value_name=value_name)
    from_tree(JSON3.read(read(path, String), Dict{String,Any}))
end

# CciaImage convenience (task_dir = img._dir)
save_pop_map!(m::PopulationMap, img::CciaImage) = save_pop_map!(m, img._dir)
load_pop_map(img::CciaImage; value_name::AbstractString="default", pop_type::AbstractString="flow") =
    load_pop_map(img._dir, value_name; pop_type=pop_type)

# ── pop_df — unified population accessor (pop_type-agnostic) ─────────────────────
#
# `pop_df` is the single accessor for the cells of any population, across ALL pop_types
# (flow, live, clust, transient — docs/POPULATION.md), pooling across populations and
# value_names. It lives here with the pop-map infrastructure (generic, pop_type-neutral),
# NOT in the gating engine — gating is only one membership source. It builds on the gate-
# evaluation internals (`recompute!`, `cells_in_pop`) defined in gating_engine.jl (resolved
# at call time) and reads cell tables via `label_props`.
#
# Image-owned data-access API (functions dispatching on ::CciaImage): `label_props` (H5AD
# tables), `pop_df` (population cells), `load_pop_map`/`save_pop_map!` (population maps) —
# mirroring the R cciaImage accessors but kept in subsystem files, not a single god-object.

# A pop reference may name its value_name as a prefix ("Tcells/_tracked") or use a
# leading-slash path within the default value_name ("/cd4/cd8"). Group accordingly.
function _group_pops_by_value_name(pops, default_vn::AbstractString)
    groups = Dict{String,Vector{String}}()
    for p in pops
        p = String(p)
        if startswith(p, "/") || is_root(p)
            vn, path = default_vn, p
        else
            idx = findfirst('/', p)
            idx === nothing ? (vn = default_vn; path = "/" * p) :
                              (vn = p[1:idx-1]; path = p[idx:end])
        end
        push!(get!(groups, vn, String[]), path)
    end
    groups
end

# Drop rows that are NA/NaN in any requested measure column (mirrors R popDT `dropNA`).
# Only `pop_cols` are considered (the user-requested measures), not bookkeeping columns.
function _drop_na_rows(df::DataFrame, pop_cols)::DataFrame
    pop_cols === nothing && return df
    cols = intersect(String.(pop_cols), names(df))
    isempty(cols) && return df
    keep = trues(nrow(df))
    for c in cols, i in 1:nrow(df)
        v = df[i, c]
        (ismissing(v) || (v isa AbstractFloat && isnan(v))) && (keep[i] = false)
    end
    df[keep, :]
end

# core pop_df over injectable providers (testable headless):
#   load_map(vn)            -> PopulationMap for value_name vn
#   fetch(vn, cols)         -> DataFrame(label + cols) for value_name vn
function _pop_df(load_map::Function, fetch::Function, pop_type::AbstractString, pops;
                 default_vn::AbstractString="default", pop_cols=nothing,
                 unique_labels::Bool=true, drop_na::Bool=false,
                 membership_fetch::Function=fetch)::DataFrame
    frames = DataFrame[]
    for (vn, vpops) in _group_pops_by_value_name(pops, default_vn)
        m = load_map(vn)
        # gate eval must read RAW channel columns (gates store {measure}_intensity_{i});
        # `fetch` may rename output columns to channel names, so keep them separate.
        recompute!(m, cols -> membership_fetch(vn, cols))
        cols = pop_cols === nothing ? nothing : unique(vcat("label", String.(pop_cols)))
        base = fetch(vn, cols === nothing ? String[] : filter(!=("label"), cols))  # label + requested cols
        byrow = Dict(lab => i for (i, lab) in enumerate(base.label))
        for pop in vpops
            # a pop may be defined on only some images of a pooled set (e.g. a cluster pop written
            # to the run's `partOf` images): skip it where absent rather than erroring the whole set.
            (is_root(pop) || has_pop(m, pop)) || continue
            labs = cells_in_pop(m, pop)
            isempty(labs) && continue
            rows = [byrow[l] for l in labs if haskey(byrow, l)]
            sub = base[rows, :]
            sub[!, :pop] = fill(String(pop), nrow(sub))
            sub[!, :value_name] = fill(vn, nrow(sub))
            push!(frames, sub)
        end
    end
    isempty(frames) && return DataFrame()
    df = reduce((a, b) -> vcat(a, b; cols=:union), frames)
    if unique_labels && nrow(df) > 0
        # collapse to one row per cell, most-specific pop wins. Dedup key mirrors R popDT's
        # merge cols — intersect(["uID","value_name","label","track_id"], names) — so uID
        # (set-level pooling) and track_id only participate when those columns are present.
        key = intersect([:uID, :value_name, :label, :track_id], propertynames(df))
        df[!, :__depth] = [count(==('/'), p) for p in df.pop]
        sort!(df, [key..., :__depth]; rev=vcat(falses(length(key)), true))
        df = unique(df, key)
        select!(df, Not(:__depth))
    end
    drop_na && (df = _drop_na_rows(df, pop_cols))
    df
end

# Per-TRACK assembly (`granularity=:track`): ONE row per track instead of one per cell.
# Cell-level gate membership is evaluated as usual; the member cells' `track_id`s select rows
# from the companion per-track table `{vn}__tracks.h5ad` (measures in X/var, lineage in obs). A
# track belongs to a pop if any of its cells are in that pop. Mirrors the `_pop_df` dedup, but
# the row key is the track (value_name, track_id), not the cell label.
function _pop_df_tracks(img::CciaImage, load_map::Function, pops, default_vn::AbstractString;
                        pop_cols=nothing, unique_labels::Bool=true, drop_na::Bool=false)::DataFrame
    frames = DataFrame[]
    for (vn, vpops) in _group_pops_by_value_name(pops, default_vn)
        tpath = img_track_props_path(img, vn)
        isfile(tpath) || begin
            @warn "pop_df(:track): no track table for value_name=$vn — run tracking.track_measures" tpath
            continue
        end

        # cell-level gate membership (Julia is the evaluator), then cell label → track_id
        m = load_map(vn)
        recompute!(m, cols -> (label_props(img; value_name=vn) |>
                               lp -> select_cols(lp, cols) |> as_df))
        cellobs = label_props(img; value_name=vn) |> lp -> select_cols(lp, ["track_id"]) |> as_df
        cell_tid = Dict{Int,Int}()
        for r in eachrow(cellobs)
            (r.track_id isa Number && !isnan(r.track_id)) || continue
            cell_tid[Int(r.label)] = Int(r.track_id)
        end

        # per-track table (label == track_id); select requested measures when given
        tview = label_props(tpath)
        pop_cols === nothing || select_cols(tview, String.(pop_cols))
        ttab = as_df(tview)
        trow = Dict(Int(t) => i for (i, t) in enumerate(ttab.label))

        for pop in vpops
            tids = unique(cell_tid[l] for l in cells_in_pop(m, pop) if haskey(cell_tid, l))
            rows = [trow[t] for t in tids if haskey(trow, t)]
            isempty(rows) && continue
            sub = ttab[rows, :]
            sub[!, :track_id]   = sub.label
            sub[!, :pop]        = fill(String(pop), nrow(sub))
            sub[!, :value_name] = fill(vn, nrow(sub))
            push!(frames, sub)
        end
    end
    isempty(frames) && return DataFrame()
    df = reduce((a, b) -> vcat(a, b; cols=:union), frames)
    if unique_labels && nrow(df) > 0
        key = intersect([:uID, :value_name, :track_id], propertynames(df))
        df[!, :__depth] = [count(==('/'), p) for p in df.pop]
        sort!(df, [key..., :__depth]; rev=vcat(falses(length(key)), true))
        df = unique(df, key)
        select!(df, Not(:__depth))
    end
    drop_na && (df = _drop_na_rows(df, pop_cols))
    df
end

# Expand a per-track result (one row per `(value_name, track_id)`) to its member cells: one row
# per cell with `label` (cell), `track_id`, `pop`, `value_name`. Used by `pop_df(pop_type="track",
# granularity=:cell)` so a track gate can hand napari / downstream the cells of the gated tracks
# (the "selecting a track pulls in all its cells" behaviour). Cell measures are NOT re-attached —
# the gate is over track properties; callers wanting cell measures read them via a `:cell` pop_df.
function _expand_tracks_to_cells(img::CciaImage, trackdf::DataFrame; cell_cols=String[])::DataFrame
    # the per-track frame identifies each track by `label` (label == track_id on the track table);
    # older callers may pass an explicit `track_id` column. Accept either.
    tidcol = "track_id" in names(trackdf) ? "track_id" : ("label" in names(trackdf) ? "label" : nothing)
    (isempty(trackdf) || tidcol === nothing || !("value_name" in names(trackdf))) && return DataFrame()
    ccols = String.(collect(cell_cols))
    frames = DataFrame[]
    for vn in unique(trackdf.value_name)
        sub = trackdf[trackdf.value_name .== vn, :]
        # read member cells' track_id (+ any requested CELL columns, e.g. `live.cell.hmm.state.*`
        # for the HMM plots — these are per-cell obs, not on the per-track table, so they must be
        # carried here); unknown columns are ignored by the reader.
        co = label_props(img; value_name=vn) |> lp -> select_cols(lp, unique(vcat("track_id", ccols))) |> as_df
        keep = [r isa Number && !isnan(r) && Int(r) > 0 for r in co.track_id]
        co = co[keep, :]; nrow(co) == 0 && continue
        co[!, :track_id] = Int.(co.track_id)
        tid_rows = Dict{Int,Vector{Int}}()
        for (i, t) in enumerate(co.track_id); push!(get!(tid_rows, t, Int[]), i); end
        present = intersect(ccols, names(co))                 # requested cols living on the CELL table
        # requested cols that live on the TRACK table instead (e.g. `clusters.{suffix}` for the HMM
        # plots' per-cluster mode) — carried onto every member cell as a per-track constant.
        track_cols = intersect(setdiff(ccols, present), names(sub))
        for r in eachrow(sub)
            tid = Int(r[tidcol])
            rows = get(tid_rows, tid, Int[])
            isempty(rows) && continue
            cdf = co[rows, :]
            out = DataFrame(label=cdf.label, track_id=tid,
                            pop=String(r.pop), value_name=vn)
            for c in present;    out[!, c] = cdf[!, c]; end       # cell columns, per member cell
            for c in track_cols; out[!, c] = fill(r[c], nrow(out)); end  # track columns, per-track constant
            push!(frames, out)
        end
    end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols=:union), frames)
end

# Per-TRACK GATING (`pop_type="track"`): gates are defined on TRACK properties — one point per
# track — NOT on cell properties (contrast `_pop_df_tracks`, which gates cells then aggregates).
# Data source is `track_props` (label == track_id, one row per track: motility from
# `{vn}__tracks.h5ad` + on-read aggregates of `cell_measures`/`categorical`). The gate map loads
# from `gating/{vn}__tracks.json` and is evaluated directly over that per-track table by the
# generic `_pop_df` core (membership is by `label`, which here IS the track_id). `granularity=:track`
# returns the gated track rows; `granularity=:cell` expands them to member cells.
function _pop_df_track_gating(img::CciaImage, pops, default_vn::AbstractString;
                              pop_type::AbstractString="track",
                              cell_measures=String[], categorical=String[], pop_cols=nothing,
                              unique_labels::Bool=true, drop_na::Bool=false,
                              granularity::Symbol=:track)::DataFrame
    # one track_props table per value_name (label == track_id); cache within this call. The
    # cluster column (`clusters.{suffix}`, written by clustTracks into the track table obs) comes
    # free via track_props' motility leftjoin, so `trackclust` membership needs no cell_measures.
    tp_cache = Dict{String,DataFrame}()
    get_tp(vn) = get!(tp_cache, vn) do
        track_props(img; value_name=vn, cell_measures=cell_measures, categorical=categorical)
    end
    load_map = vn -> load_pop_map(img; value_name=vn, pop_type=pop_type)
    fetch = function (vn, cols)
        tp = get_tp(vn)
        isempty(tp) && return DataFrame(label=Int[])
        isempty(cols) && return tp
        keep = intersect(unique(vcat("label", String.(cols))), names(tp))
        select(tp, keep)
    end

    trackdf = _pop_df(load_map, fetch, pop_type, pops; default_vn=default_vn,
                      pop_cols=pop_cols, unique_labels=unique_labels, drop_na=drop_na)
    # :cell expansion carries the requested cell columns (pop_cols that are per-cell obs, e.g. the
    # HMM state/transition columns for the HMM plots) onto the member cells.
    granularity === :track ? trackdf :
        _expand_tracks_to_cells(img, trackdf; cell_cols=(pop_cols === nothing ? String[] : pop_cols))
end

# ── Derived populations ──────────────────────────────────────────────────────────
# A *derived* population's membership comes from a column rule (a filter on an obs/measure
# column), not a hand-drawn gate. It is materialised transiently at read time as a filtered
# child of its parent and is NEVER stored in a gating file (the gates live under `flow`; a
# derived pop layers a filter on top). This is the shared mechanism behind `live` tracked pops
# (`_tracked` = track_id > 0) and, ahead, population clustering (cluster-membership pops).
#
# Derived pops live in a **reserved namespace: leaf names beginning with `_`**. Hand-drawn gates
# may not use that prefix (enforced in `add_pop!`/`rename_pop!`), so a derived name can never be
# shadowed by — or collide with — a real gate. To add a new derived population, register its
# `_`-prefixed leaf name here; the reservation then applies automatically.
const DERIVED_POP_PREFIX = "_"

struct DerivedPopSpec
    pop_type::String        # the pop_type it is derived under (e.g. "live")
    filter_measure::String  # obs/measure column the membership filters on
    filter_fun::String      # gt|gte|lt|lte|eq|neq|in
    filter_values::Any
    is_track::Bool
end

# reserved leaf name (incl. the `_` prefix) → how to derive it
const _DERIVED_POPS = Dict{String,DerivedPopSpec}(
    "_tracked" => DerivedPopSpec("live", "track_id", "gt", 0, true),
)

"""True if `name` is in the reserved derived-population namespace (leaf begins with `_`)."""
is_reserved_pop_name(name::AbstractString) = startswith(String(name), DERIVED_POP_PREFIX)

"""
    derived_pop_paths(pop_type) -> Vector{String}

Root-level paths of the derived populations registered for `pop_type` (e.g. `/_tracked` under
`live`). Derived pops are injected at query time rather than stored in the gating map, so callers
that enumerate selectable populations (e.g. the summary-canvas population picker) need this to
surface them. Generic over `_DERIVED_POPS`, so future reserved pops appear automatically.
"""
derived_pop_paths(pop_type::AbstractString)::Vector{String} =
    ["/" * name for (name, spec) in _DERIVED_POPS if spec.pop_type == String(pop_type)]

# ── Summary-canvas population picker (logic lives here, NOT in the API — api/plotting_api.jl is a
#    thin wrapper; this is Revise-tracked + headless-testable per docs/ARCHITECTURE.md) ────────────

"""
    flatten_pop_tree(tree) -> Vector{Tuple{String,String,String}}

Flatten a `to_tree` population tree into `(path, name, colour)` in tree (pre-order) order. Accepts
String or Symbol keys (JSON3 round-trips yield Symbols).
"""
function flatten_pop_tree(tree)::Vector{Tuple{String,String,String}}
    out = Tuple{String,String,String}[]
    walk(nodes, parent) = for n in nodes
        name = string(get(n, "name", get(n, :name, "")))
        path = parent == "" ? "/" * name : parent * "/" * name
        push!(out, (path, name, string(get(n, "colour", get(n, :colour, "#7c93b8")))))
        walk(get(n, "children", get(n, :children, [])), path)
    end
    walk(get(tree, "populations", get(tree, :populations, [])), "")
    out
end

"""
    plot_pop_types(pop_type, granularity) -> Vector{String}

Pop types the summary picker surfaces for a plot's granularity. A **track**-granularity plot unions
`live` pops (cell gates + the derived `/_tracked`, aggregated one-point-per-track) with `track` pops
(gated directly on per-track measures, from `{vn}__tracks.json`); a cell plot uses just `pop_type`.
"""
plot_pop_types(pop_type::AbstractString, granularity::AbstractString)::Vector{String} =
    granularity == "track" ? unique(String[pop_type, "track"]) : String[pop_type]

"""
    plot_population_groups(imgs, value_names_for, load_map, pop_types) -> Vector{NamedTuple}

Build the summary-canvas picker list: populations available across `imgs`, grouped by segmentation
(value_name). Unioned across images (dedup by `(pop_type, path)`, first image wins name/colour) and
across `pop_types`, with derived pops (`derived_pop_paths`) prepended per pop_type. Each population is
tagged with the `pop_type` it must be fetched under. `value_names_for(img)` and `load_map(img, vn,
pt)` are injected (the API passes `versioned_keys` / `load_pop_map` closures) so this is pure and
headless-testable. Returns `[(value_name, populations=[(path, name, colour, pop_type)])]` in
first-appearance order; `load_map` returning `nothing`/throwing for a missing (vn, pop_type) is
skipped.
"""
function plot_population_groups(imgs, value_names_for::Function, load_map::Function,
                                pop_types::Vector{String})
    # Gateless pop_type (`labels` = ungated all-cells, R parity): there is no gating map to flatten —
    # each segmentation IS its own population, named by its value_name. One selectable entry per vn, so
    # the user overlays whole segmentations (B, T, …) side by side. The path is the fixed "/labels" tag
    # the `labels` pop_df branch stamps (matched by `_series_groups` at plot time); it must start with
    # "/" like any pop path so the manager-form id (`value_name + path`) round-trips through the
    # frontend's `tkey`/`parseTkey` and colour map.
    if all(==("labels"), pop_types)
        vn_order = String[]
        for img in imgs, vn in value_names_for(img)
            v = String(vn); v in vn_order || push!(vn_order, v)
        end
        return [(value_name = v,
                 populations = [(path = "/labels", name = v, colour = "#7c93b8", pop_type = "labels")])
                for v in vn_order]
    end
    vn_order = String[]
    order = Dict{String,Vector{Tuple{String,String}}}()                       # vn → ordered (pt, path)
    meta  = Dict{String,Dict{Tuple{String,String},Tuple{String,String,String}}}()  # vn → key → (name,colour,pt)
    for img in imgs
        for vn in value_names_for(img)
            v = String(vn)
            haskey(order, v) || (push!(vn_order, v); order[v] = Tuple{String,String}[];
                                  meta[v] = Dict{Tuple{String,String},Tuple{String,String,String}}())
            for pt in pop_types
                m = try; load_map(img, v, pt); catch; nothing; end
                m === nothing && continue
                for (path, name, colour) in flatten_pop_tree(to_tree(m))
                    key = (pt, path); haskey(meta[v], key) && continue
                    push!(order[v], key); meta[v][key] = (name, colour, pt)
                end
            end
        end
    end
    # Derived pops (e.g. `_tracked` = track_id>0) are injected at query time, not stored. Offer each
    # BOTH at root (`/_tracked` = all tracked cells) AND as a child of every stored pop
    # (`/qc/_tracked` = qc's tracked subset) — so the tracked subset of any population is selectable
    # and the picker shows the hierarchy. Rebuild the order so a derived child directly follows its
    # parent (root-level derived first).
    for v in vn_order
        rebuilt = Tuple{String,String}[]
        for pt in pop_types, dpath in derived_pop_paths(pt)              # root-level derived, at the top
            key = (pt, dpath)
            haskey(meta[v], key) || (meta[v][key] = (pop_name(dpath), "#7c93b8", pt))
            key in rebuilt || push!(rebuilt, key)
        end
        for (pt, path) in order[v]                                       # each stored pop, then its derived children
            push!(rebuilt, (pt, path))
            for dpath in derived_pop_paths(pt)
                cpath = path * dpath                                     # dpath starts with "/" → "/qc" * "/_tracked"
                key = (pt, cpath); haskey(meta[v], key) && continue
                meta[v][key] = (pop_name(dpath), "#7c93b8", pt); push!(rebuilt, key)
            end
        end
        order[v] = rebuilt
    end
    [(value_name = v,
      populations = [(path = p, name = meta[v][(pt, p)][1], colour = meta[v][(pt, p)][2], pop_type = pt)
                     for (pt, p) in order[v]])
     for v in vn_order]
end

# Inject the derived pop(s) for the requested paths into a (flow) map, transiently. A path whose
# leaf is a registered derived pop for this `pop_type` (e.g. ".../_tracked" under `live`) becomes
# a filtered child of its parent; recompute! then composes parent ∩ filter. Unknown/foreign-type
# leaves are left untouched. No-op if the pop already exists or the parent is absent.
function _inject_derived_pops!(m::PopulationMap, paths, pop_type::AbstractString)::PopulationMap
    for path in paths
        path = String(path)
        has_pop(m, path) && continue
        spec = get(_DERIVED_POPS, pop_name(path), nothing)
        (spec === nothing || spec.pop_type != String(pop_type)) && continue
        parent = pop_parent(path)
        (is_root(parent) || has_pop(m, parent)) || continue
        add_pop!(m, pop_name(path); parent=parent, filter_measure=spec.filter_measure,
                 filter_fun=spec.filter_fun, filter_values=spec.filter_values,
                 is_track=spec.is_track, transient=true, reserved_ok=true)
    end
    m
end

_pop_df_mtime(p::AbstractString) = isfile(p) ? string(mtime(p)) : "∅"

# Stable cache key for a pop_df request (mirrors R popDT's md5 of the request signature). Folds in
# the on-disk mtimes of each involved value_name's gating map + h5ad so a saved gate edit or a
# re-tracked h5ad auto-invalidates the cache: pop_df always reflects the on-disk state (it reloads
# the map and reads the h5ad fresh), so a changed stamp == a real change. `flush_cache` remains as
# a manual override for in-memory edits that were never written to disk.
function _pop_df_cache_key(img::CciaImage, pop_type, value_name, pops, pop_cols, include_x,
                           include_obs, unique_labels, drop_na, raw_channel_names,
                           granularity, cell_measures, categorical)::String
    is_track = String(pop_type) == "track"
    stamps = String[]
    for vn in sort(collect(keys(_group_pops_by_value_name(pops, value_name))))
        # track gating reads the per-track gate map `{vn}__tracks.json`; cell-level pop_types read
        # the flow map. Always fold the cell h5ad (track_props derives from it).
        gmtime = _pop_df_mtime(gating_path(img._dir, vn; pop_type=is_track ? "track" : "flow"))
        s = string(vn, "@", gmtime, "/", _pop_df_mtime(img_label_props_path(img, vn)))
        # :track granularity and track-gating both read the companion track table — fold its mtime
        # so a re-run (or a saved track gate) auto-invalidates.
        (granularity === :track || is_track) &&
            (s *= "/" * _pop_df_mtime(img_track_props_path(img, vn)))
        push!(stamps, s)
    end
    parts = (pop_type, value_name, join(sort(String.(collect(pops))), "&"),
             pop_cols === nothing ? "" : join(sort(String.(collect(pop_cols))), "&"),
             include_x, include_obs, unique_labels, drop_na, raw_channel_names, granularity,
             join(sort(String.(collect(cell_measures))), "&"),
             join(sort(String.(collect(categorical))), "&"),
             join(stamps, "|"))
    string(hash(parts))
end

"""
    pop_df(img, pop_type, pops; value_name=nothing, pop_cols=nothing, include_x=false,
           include_obs=true, unique_labels=true, drop_na=false, flush_cache=false,
           raw_channel_names=false) -> DataFrame

Unified population accessor. Returns the cells of `pops` with a `pop` + `value_name`
column and the requested `pop_cols` (read from the H5AD via `label_props`). Pools across
populations and across value_names: a pop path may name its value_name as a **prefix**
(`"A/qc"` → value_name `A`, path `/qc`), while a **leading-slash** path stays within the
given/active value_name (`pop_df(img, "flow", ["/qc"]; value_name="A")`). So a single call
can pool one population from several segmentations — `pop_df(img, "live",
["A/qc/_tracked", "B/qc/_tracked", "C/qc/_tracked"])` — but a leading-slash path cannot reach a
different value_name than the one passed.

- For `flow`, membership comes from gate `recompute!`. For derived pop_types (`live`, later
  `clust`), the gates still live in the `flow` map; a **derived population** is layered on top
  and is *not* stored in any gating file. Derived pops use a reserved namespace — **leaf names
  beginning with `_`** (e.g. `"_tracked"` → `track_id > 0`, see `_DERIVED_POPS`); the path's leaf
  becomes a transient filtered child of its parent and `recompute!` composes parent ∩ filter.
  Hand-drawn gates may not use the `_` prefix, so a derived name can never be shadowed by a gate.
- `granularity=:cell` (default) returns one row per cell. `granularity=:track` returns one row
  per **track**: for cell-level pop_types (`live`) cell membership still drives selection, but the
  member cells' `track_id`s pick rows from the companion per-track table `{vn}__tracks.h5ad` (track
  measures in X/var, lineage in obs, written by `tracking.track_measures`). A track belongs to a
  pop if any of its cells are in that pop; the row key is `(value_name, track_id)`.
- `pop_type="track"` gates **directly on per-track properties** (one point per track): the gate
  map loads from `gating/{vn}__tracks.json` and is evaluated over the `track_props` table (motility
  from `{vn}__tracks.h5ad` + on-read aggregates of `cell_measures`/`categorical`, keyed by
  track_id). `granularity=:track` returns the gated track rows; `granularity=:cell` expands them to
  member cells (label/track_id/pop/value_name) — selecting a track pulls in all its cells.
  `cell_measures`/`categorical` are the *base* cell columns to aggregate into track properties
  (numeric → `.mean/.median/.sum/.qUp/.qLow/.sd`, categorical → per-category frequency `{m}.{cat}`);
  pass the base names being gated/plotted (mirrors R `tracksInfo`'s `trackStatsNames`).
- `value_name=nothing` resolves to the image's **active** segmentation (parity with
  `label_props(img)`); pass a name to override the default value_name for unprefixed pops.
- `drop_na=true` drops cells that are NA/NaN in any requested `pop_cols` (mirrors R popDT
  `dropNA`).
- Results are cached per image keyed by the request signature **plus the on-disk mtimes of the
  involved gating maps + h5ads**, so a saved gate edit or a re-tracked h5ad **auto-invalidates**
  the cache. `flush_cache=true` is a manual override for in-memory edits never written to disk.

By default intensity columns are returned under their **channel names** (e.g.
`mean_intensity_0` → `"CD4"`), mirroring R `popDT` / Python `change_channel_names` — pass
`raw_channel_names=true` to keep the raw `{measure}_intensity_{i}` column names instead.
"""
function pop_df(img::CciaImage, pop_type::AbstractString, pops;
                value_name::Union{AbstractString,Nothing}=nothing, pop_cols=nothing,
                include_x::Bool=false, include_obs::Bool=true, unique_labels::Bool=true,
                drop_na::Bool=false, flush_cache::Bool=false,
                raw_channel_names::Bool=false, granularity::Symbol=:cell,
                cell_measures=String[], categorical=String[])::DataFrame
    granularity in (:cell, :track) ||
        error("pop_df: granularity must be :cell or :track (got :$granularity)")
    # value_name=nothing → active segmentation key (same resolution as label_props(img))
    resolved_vn = something(value_name, get(img.label_props, "_active", "default"))

    ckey = _pop_df_cache_key(img, pop_type, resolved_vn, pops, pop_cols, include_x, include_obs,
                             unique_labels, drop_na, raw_channel_names, granularity,
                             cell_measures, categorical)
    flush_cache && delete!(img._pop_df_cache, ckey)
    haskey(img._pop_df_cache, ckey) && return copy(img._pop_df_cache[ckey]::DataFrame)

    # `track` / `trackclust` pop_types: membership is defined directly on per-track properties
    # (one point per track), evaluated over the `track_props` table — `track` via hand-drawn gates
    # in `gating/{vn}__tracks.json`, `trackclust` via a `clusters.{suffix}` filter in
    # `gating/{vn}__trackclust.json`. `granularity` selects the return shape (:track rows, or
    # :cell-expanded member cells). Distinct from the `live`+:track path below, which gates CELL
    # properties and then aggregates to tracks.
    if String(pop_type) in ("track", "trackclust")
        df = _pop_df_track_gating(img, pops, resolved_vn; pop_type=String(pop_type),
                                  cell_measures=cell_measures,
                                  categorical=categorical, pop_cols=pop_cols,
                                  unique_labels=unique_labels, drop_na=drop_na,
                                  granularity=granularity)
        img._pop_df_cache[ckey] = df
        return copy(df)
    end

    # `labels` pop_type: ALL cells of the segmentation's labelProps, UNGATED — no gating map, no
    # membership eval. Mirrors the old R popType "labels" (`labelsPopUtils`). This is the raw
    # segmentation-output data source (e.g. the segmentation-integrity QC canvas): every measured
    # object of `resolved_vn`, tagged with a single "/labels" pop path so the summary framework groups
    # it like any other population (path starts with "/", matching the picker + manager-form id).
    # `pops` is ignored (there are no sub-populations).
    if String(pop_type) == "labels"
        lp = label_props(img; value_name=resolved_vn) |> v -> rename_channels!(v, !raw_channel_names)
        isnothing(pop_cols) || select_cols(lp, String.(pop_cols))
        # When no columns are requested (e.g. a bare cell COUNT), read neither X nor obs — just
        # label/centroids. Reading every obs measure for millions of cells only to count rows needlessly
        # loads the (single-process) API and would stall e.g. a queued napari open. With pop_cols set,
        # `select_cols` already narrows the read to exactly those columns.
        df = as_df(lp; include_x=(pop_cols === nothing ? include_x : true),
                       include_obs=(pop_cols === nothing ? false : include_obs))
        df[!, "value_name"] .= resolved_vn
        df[!, "pop"]        .= "/labels"
        img._pop_df_cache[ckey] = df
        return copy(df)
    end

    # Derived pop_types (e.g. `live`): gates are stored under `flow`; layer the derived pops
    # (e.g. _tracked) on top, transiently. Pop_types with no registered derived specs load normally.
    groups = _group_pops_by_value_name(pops, resolved_vn)
    has_derived = any(s -> s.pop_type == pop_type, values(_DERIVED_POPS))
    load_map = function (vn)
        if has_derived
            m = load_pop_map(img; value_name=vn, pop_type="flow")
            _inject_derived_pops!(m, get(groups, vn, String[]), pop_type)
        else
            load_pop_map(img; value_name=vn, pop_type=pop_type)
        end
    end

    # :track → one row per track from the companion track table (membership still cell-level).
    if granularity === :track
        df = _pop_df_tracks(img, load_map, pops, resolved_vn;
                            pop_cols=pop_cols, unique_labels=unique_labels, drop_na=drop_na)
        img._pop_df_cache[ckey] = df
        return copy(df)
    end
    # label_props chain idiom (docs/DATAMODEL.md). Output columns resolve channel names by
    # default (raw_channel_names=true keeps raw); the conditional select + as_df kwargs keep
    # this as fluent statements rather than a single pipe.
    fetch = function (vn, cols)
        lp = label_props(img; value_name=vn) |> v -> rename_channels!(v, !raw_channel_names)
        isempty(cols) || select_cols(lp, cols)
        as_df(lp; include_x=(isempty(cols) ? include_x : true), include_obs=include_obs)
    end
    # membership/gate eval: always raw column names (gates store raw intensity column names)
    membership_fetch = function (vn, cols)
        lp = label_props(img; value_name=vn)
        isempty(cols) || select_cols(lp, cols)
        as_df(lp; include_x=(isempty(cols) ? include_x : true), include_obs=include_obs)
    end
    df = _pop_df(load_map, fetch, pop_type, pops;
                 default_vn=resolved_vn, pop_cols=pop_cols, unique_labels=unique_labels,
                 drop_na=drop_na, membership_fetch=membership_fetch)
    img._pop_df_cache[ckey] = df
    copy(df)   # return a copy so callers never mutate the cached frame
end

"""
    pop_df(imgs::Vector{CciaImage}, uids, pop_type, pops; kwargs...) -> DataFrame

Set-level pooling: run `pop_df` on each image and stack the results with a **`uID` column** tagging
each row's source image. This is the cross-image analogue of pooling across value_names — it lets a
summary plot compare the *same* population across images (e.g. mean `live.track.speed` of `A/_tracked`
in images X/Y/Z). `uids` is parallel to `imgs` (the API passes a `CciaSet`'s `_images` + `image_uids`).
Per-image rows are already deduped within their image; the `uID` column keeps same-label cells from
different images distinct (the `uID` is part of `pop_df`'s dedup key — see `_pop_df`). All `kwargs`
(`value_name`/`pop_cols`/`granularity`/…) pass straight through to the per-image `pop_df`.
"""
function pop_df(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector, pop_type::AbstractString,
                pops; kwargs...)::DataFrame
    length(imgs) == length(uids) ||
        error("pop_df: imgs and uids must be parallel (got $(length(imgs)) vs $(length(uids)))")
    frames = DataFrame[]
    for (img, uid) in zip(imgs, uids)
        d = pop_df(img, pop_type, pops; kwargs...)
        nrow(d) == 0 && continue
        d[!, :uID] = fill(String(uid), nrow(d))
        push!(frames, d)
    end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols=:union), frames)
end
