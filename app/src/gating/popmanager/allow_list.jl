# accepts allow-list — a function declares the exact pop_types its popSelection takes (Decision
# 14, docs/POPULATION.md → accepts allow-list). One test at plot-authoring time trumps three
# runtime bugs: a cluster pop offered to a gate that can only take a leaf, a region pop offered to
# a plot that filters by centroid, a track pop offered to a per-cell measure. The allow-list is
# authoritative — a task adding a new pop_type without extending the allow-list is what breaks the
# picker silently. Loaded after picker.jl because the picker calls the allow-list to gate rows.

# ── accepts allow-list: a function declares the exact pop_types its popSelection takes (Decision 14,
#    SPATIAL_REGIONS_PLAN.md). This SUPERSEDES the coarse `popScope` (cells|tracks) — the region
#    basis, for one, needs cells AND tracks — while the two-scope helpers below stay working as a
#    thin shim over it. `accepts` is a list of pop_type tokens (R per-widget popType parity):
#      live/flow — cell gates      clust — cell clusters       region — spatial regions
#      track     — per-track gates + the derived `_tracked` sets   trackclust — track clusters
#    "flow" is an alias for "live" (both read the same flow gate map). Each surviving population is
#    tagged (granularity, category) via `is_track_pop`/`pop_category` so the picker groups it. ──────

# every stored type names a map to load; `live` is the derived view of flow's map (registry.jl).
const ACCEPT_TOKENS = ("live", STORED_POP_TYPES...)

# canonicalise: fold "flow"→"live", dedup, preserve order.
_normalise_accepts(accepts) = unique(String[String(a) == "flow" ? "live" : String(a) for a in accepts])

# Which (granularity, category) pairs a normalised accept token admits. Aggregated pops ride with
# their cell/track input (an aggregate is a spatial grouping of the accepted cells), so a gated
# token also admits its own granularity's aggregated pop.
function _accept_permits(accepts::Vector{String}, granularity::AbstractString, category::AbstractString)::Bool
    g = String(granularity); c = String(category)
    for a in accepts
        if a == "live"
            (g == "cell"  && c in ("gated", "aggregated")) && return true
        elseif a == "clust"
            (g == "cell"  && c == "clustered")             && return true
        elseif a == "region"
            (g == "cell"  && c == "region")                && return true
        elseif a == "track"
            (g == "track" && c in ("gated", "tracked", "aggregated")) && return true
        elseif a == "trackclust"
            (g == "track" && c == "clustered")             && return true
        elseif a == "branch"
            (g == "branch" && c == "gated") && return true
        end
    end
    false
end

# The pop_types `plot_population_groups` must LOAD to cover `accepts`. `track` also pulls in `live`:
# the derived `_tracked` per-track sets are enumerated as children of the (cell) `live` gates.
function _accept_pop_types(accepts::Vector{String})::Vector{String}
    pts = String[]
    ("track" in accepts || "live" in accepts) && push!(pts, "live")
    ("track" in accepts)      && push!(pts, "track")
    ("clust" in accepts)      && push!(pts, "clust")
    ("trackclust" in accepts) && push!(pts, "trackclust")
    ("region" in accepts)     && push!(pts, "region")
    ("branch" in accepts)     && push!(pts, "branch")
    unique(pts)
end

"""
    population_accept_groups(imgs, value_names_for, load_map, accepts; kwargs...) -> Vector{NamedTuple}

The MODULE-FUNCTION population picker (Decision 14): `plot_population_groups` restricted to the
pop_types a function's popSelection declares it `accepts`, with every surviving population tagged
`granularity` (`"cell"`|`"track"`) and `category` (`"gated"`|`"clustered"`|`"region"`|`"tracked"`|
`"aggregated"`) so the frontend groups it under a *"<granularity> · <category>"* header. When cell
gates are accepted (`live`/`flow`) an all-cells root (`/`, always real — the segmentation has label
props) is prepended per segmentation. The all-TRACKS root is the derived `/_tracked` root
`plot_population_groups` already produces, guarded by `derived_ok` so a bogus "all tracks" never
appears with no tracking. Same injected closures as `plot_population_groups`. Unknown token → throws
(a spec typo should fail loudly, not silently empty the picker).

Returns `[(value_name, populations=[(path, name, colour, pop_type, granularity, category)])]`.
"""
function population_accept_groups(imgs, value_names_for::Function, load_map::Function,
                                  accepts::AbstractVector; include_all_cells::Bool = true,
                                  derived_ok::Function = (_v, _pt, _parent, _dpath) -> true)
    acc = _normalise_accepts(accepts)
    isempty(acc) && error("accepts is empty — a popSelection must declare at least one pop_type")
    bad = setdiff(acc, ACCEPT_TOKENS)
    isempty(bad) || error("unknown accepts token(s): $(join(bad, ", ")) " *
                          "(expected any of live/flow, clust, trackclust, region, track, branch)")
    groups = plot_population_groups(imgs, value_names_for, load_map, _accept_pop_types(acc);
                                    derived_ok = derived_ok)
    want_all_cells = include_all_cells && ("live" in acc)
    [(value_name = g.value_name,
      populations = begin
          kept = NamedTuple[]
          # The synthetic "all cells" root ("/") has no gating-map identity — uid is empty; a
          # capture that references it restores by (value_name, "/") alone.
          want_all_cells && push!(kept, (path = "/", name = "all", colour = "#7c93b8",
                                         pop_type = "live", uid = "",
                                         granularity = "cell", category = "gated"))
          for p in g.populations
              # branch is a THIRD granularity distinct from cell/track (BRANCHING_PLAN Decision 2);
              # detect it explicitly before falling back to the cell/track binary.
              gran = string(p.pop_type) == "branch" ? "branch" :
                     is_track_pop(p.pop_type, p.path) ? "track" : "cell"
              cat  = pop_category(p.pop_type, p.path)
              _accept_permits(acc, gran, cat) || continue
              push!(kept, (path = p.path, name = p.name, colour = p.colour,
                           pop_type = string(p.pop_type), uid = p.uid,
                           granularity = gran, category = cat))
          end
          kept
      end)
     for g in groups]
end

# scope → accepts tokens, for the back-compatible `popScope` shim. No built-in task spec uses
# `popScope` any more (pinned by task_spec_ratchets.jl) — it stays for two callers that are not built-in
# specs: user custom modules / plugins written against it (docs/CUSTOM_MODULES.md; their specs are not
# ours to migrate), and the UMAP view's colour-by picker (`ClusterMapView.vue`, `popScope` +
# `includeClusters=false` on /api/plots/populations). Delete it only when both are gone.
function _scope_accepts(scope::AbstractString, include_clusters::Bool)::Vector{String}
    # the scope's cluster tokens = the registry's cluster types of that grain (cells: clust, region;
    # tracks: trackclust)
    clusters_of(grain) = String[pt for pt in CLUSTER_POP_TYPES if pop_type_info(pt).grain === grain]
    if String(scope) == "tracks"
        acc = ["track"]; include_clusters && append!(acc, clusters_of(:track))
    elseif String(scope) == "cells"
        acc = ["live"]; include_clusters && append!(acc, clusters_of(:cell))
    else
        error("unknown popScope: $scope (expected \"cells\" or \"tracks\")")
    end
    acc
end

"""
    population_scope_groups(imgs, value_names_for, load_map, scope; kwargs...) -> Vector{NamedTuple}

Back-compatible two-scope picker (`popScope` = `"cells"`|`"tracks"`), now a thin shim over
`population_accept_groups`: `cells` = `["live", "clust", "region"]` (+ all-cells root), `tracks` =
`["track", "trackclust"]` (no all-cells root; `track` pulls in `live` for the derived `_tracked`
sets). `include_clusters=false` drops the cluster token. Output carries the same `granularity`/
`category` tags. Superseded by `accepts` for new specs — kept so existing `popScope` JSONs keep working.
"""
function population_scope_groups(imgs, value_names_for::Function, load_map::Function,
                                 scope::AbstractString; include_clusters::Bool = true,
                                 derived_ok::Function = (_v, _pt, _parent, _dpath) -> true)
    population_accept_groups(imgs, value_names_for, load_map,
                             _scope_accepts(scope, include_clusters);
                             include_all_cells = (String(scope) == "cells"),
                             derived_ok = derived_ok)
end

# Inject the derived pop(s) for the requested paths into a (flow) map, transiently. A path whose
# leaf is a registered derived pop for this `pop_type` (e.g. ".../_tracked" under `live`) becomes
# a filtered child of its parent; recompute! then composes parent ∩ filter. Unknown/foreign-type
# leaves are left untouched. No-op if the pop already exists or the parent is absent.
function _inject_derived_pops!(m::PopulationTree, paths, pop_type::PopTypeArg)::PopulationTree
    for path in paths
        path = String(path)
        has_pop(m, path) && continue
        spec = get(_DERIVED_POPS, pop_name(path), nothing)
        (spec === nothing || spec.pop_type != _coerce_pop_type(pop_type)) && continue
        parent = pop_parent(path)
        (is_root(parent) || has_pop(m, parent)) || continue
        add_pop!(m, pop_name(path); parent=parent, filter_measure=spec.filter_measure,
                 filter_fun=spec.filter_fun, filter_values=spec.filter_values,
                 is_track=spec.is_track, transient=true, reserved_ok=true)
    end
    m
end


# Cache slot + stamp for a pop_df request. The
# KEY is the request — the image's directory plus every argument that changes the frame — so a gate
# edit REPLACES its slot instead of adding one. The STAMP is what the frame was read from:
# `_gating_inputs_stamp` per involved value_name (every gating map's digest — a cluster pop type
# borrows a sibling's map — the sidecars, the cell h5ad, the µm/px) plus the files only some reads
# touch: the per-track and per-branch tables. pop_df always reflects the on-disk state, so a changed
# stamp == a real change; `flush_cache` remains as a manual override for in-memory edits never written.
function _pop_df_cache_key(img::CciaImage, pop_type, value_name, pops, pop_cols, include_x,
                           include_obs, unique_labels, drop_na, raw_channel_names,
                           granularity, cell_measures, categorical, centroids = false,
                           labels_version = nothing)::@NamedTuple{key::String, stamp::String}
    is_track  = is_track_grained(pop_type)
    is_branch = string(pop_type) == "branch"
    stamps = String[]
    for vn in sort(collect(keys(_group_pops_by_value_name(pops, value_name))))
        s = string(vn, "@", _gating_inputs_stamp(img, vn))
        # channel names decide which column a NAME reads and what an intensity column is called — a
        # rename (ccid.json, not the h5ad) must not serve the old column (docs/todo/CHANNEL_NAMES_PLAN.md)
        s *= "#" * join(something(channel_names(img; value_name = vn), String[]), "\u1f")
        # :track granularity and track-gating both read the companion track table — fold its mtime
        # so a re-run (or a saved track gate) auto-invalidates.
        (granularity === :track || is_track) &&
            (s *= "/" * file_stamp(img_track_props_path(img, vn)))
        # branch pop_type reads the per-branch sidecar; fold its mtime for cache invalidation.
        is_branch && (s *= "/" * file_stamp(img_branch_props_path(img, vn)))
        push!(stamps, s)
    end
    parts = (img._dir, pop_type, value_name, join(sort(String.(collect(pops))), "&"),
             pop_cols === nothing ? "" : join(sort(String.(collect(pop_cols))), "&"),
             include_x, include_obs, unique_labels, drop_na, raw_channel_names, granularity,
             join(sort(String.(collect(cell_measures))), "&"),
             join(sort(String.(collect(categorical))), "&"),
             # only WHETHER centroids were read belongs in the key, not the unit: the cache holds the
             # frame as read (pixels) and `:physical` scales the returned copy, so `:pixel` and
             # `:physical` legitimately share one cached read instead of doubling the entries.
             centroids !== false,
             # the pin changes which cell table is read — a pinned and an unpinned read never share
             something(labels_version, ""))
    (key = bytes2hex(SHA.sha256(join(string.(parts), "\u1e"))), stamp = join(stamps, "|"))
end


"""
    _pop_has_authored_tracks(pop_uid, pop_labels, label_to_source) -> Bool

The attribution predicate that drives `has_tracks` in `resolve_pops` (MULTI_POP_TRACKING_ORPHANS_PLAN
decision 1). A label appears in `label_to_source` iff it holds `track_id > 0`; its value is the
authoring `track_source` string (a pop UID or `WHOLE_SEG_TRACK_SOURCE`) or `nothing` for a legacy
row whose row has no marker (pre-P1 h5ad, or an h5ad written before the provenance ship).

This pop's ribbon fires when any of its labels was:
- authored by itself (`src == pop_uid`),
- authored by whole-seg tracking (`src == WHOLE_SEG_TRACK_SOURCE`) — the prime-everything mode, or
- an unmarked row (`src === nothing`) — treated as everyone's, so a project tracked before the
  provenance ship keeps its ribbons drawn. Re-tracking rewrites the marker and the guard tightens.

Pure — no I/O, no `img` handle. Extracted so the plan's tests can unit-test the rule without
building a fixture that carries a categorical `track_source` column (which requires the Python
writer).
"""
function _pop_has_authored_tracks(pop_uid::AbstractString,
                                  pop_labels::AbstractVector{<:Integer},
                                  label_to_source::AbstractDict{Int,Union{String,Nothing}})::Bool
    isempty(label_to_source) && return false
    @inbounds for l in pop_labels
        src = get(label_to_source, Int(l), missing)
        src === missing && continue
        if src === nothing || src == WHOLE_SEG_TRACK_SOURCE || src == pop_uid
            return true
        end
    end
    false
end

"""
    _resolve_track_family_pops(img, pt, value_name) -> Vector{NamedTuple}

Track-family (`track`/`trackclust`) branch of `resolve_pops`. Membership comes from
`pop_df(granularity=:cell)` — which internally uses `_pop_df_track_gating` against `track_props` —
so `clusters.{suffix}` and motility columns resolve correctly, and the returned `labels` are cell
labels (same semantics as flow/clust pops, so `filterPayloadByLabels` / label→tile stay reusable).
"""
function _resolve_track_family_pops(img::CciaImage, pt::AbstractString,
                                    value_name::AbstractString)::Vector{NamedTuple}
    m = try
        load_pop_tree(img; value_name = value_name, pop_type = pt)
    catch e
        @warn "resolve_pops (track-family): load_pop_tree failed" value_name pop_type=pt exception=e
        return NamedTuple[]
    end
    meta = Dict{String,NamedTuple}()
    paths = String[]
    for path in pop_paths(m)
        p = pop_at(m, path)
        p.transient && continue
        # typed-as-track comes from the pop_type (`is_track_pop`), not the stored `p.is_track` flag,
        # which no GUI/MCP writer sets — every viewer/overlay/movie consumer reads it from here
        meta[String(path)] = (colour = p.colour, name = p.name, uid = p.uid,
                              show = p.show, is_track = is_track_pop(pt, String(path)))
        push!(paths, String(path))
    end
    isempty(paths) && return NamedTuple[]
    # `expand_cluster_pops=false` — we're already scoping to ONE `value_name`, so the run-wide
    # expansion that fans a bare cluster-pop ref out across every co-clustered segmentation would
    # pull in the other vn's cells and taint this vn's overlay.
    df = try
        pop_df(img, pt, paths; value_name = value_name, granularity = :cell,
               expand_cluster_pops = false)
    catch e
        @warn "resolve_pops (track-family): pop_df failed" value_name pop_type=pt exception=e
        return NamedTuple[]
    end
    (size(df, 1) > 0 && "label" in names(df) && "pop" in names(df)) || return NamedTuple[]
    labels_per_pop = Dict{String,Vector{Int}}()
    @inbounds for i in 1:size(df, 1)
        lab = df[i, :label]
        (lab isa Real && isfinite(Float64(lab))) || continue
        popname = String(df[i, :pop])
        push!(get!(labels_per_pop, popname, Int[]), Int(round(Float64(lab))))
    end
    out = NamedTuple[]
    for path in paths
        m_ = meta[path]
        labs = get(labels_per_pop, path, Int[])
        isempty(labs) && continue
        # Members of a track-family pop are tracked cells by construction (`_pop_df_track_gating`
        # only touches rows with `track_id > 0`), so `has_tracks=true` is definitional here. Kept
        # in the payload so the frontend's ribbon-eligibility check (`isTrack || hasTracks`) reads
        # trackclust pops the same way it reads flow-pop-with-tracked-cells.
        push!(out, (path = path, name = m_.name, colour = m_.colour, uid = m_.uid,
                    show = m_.show, is_track = m_.is_track, has_tracks = true,
                    labels = labs))
    end
    out
end


"""
    label_track_sources(img, value_name) -> Dict{Int,Union{String,Nothing}}

Every TRACKED cell (`track_id > 0`) of a segmentation → the `track_source` that authored it: a pop
UID, `WHOLE_SEG_TRACK_SOURCE`, or `nothing` for a legacy row written before provenance existed (no
`track_source` column, or an empty/missing marker). Untracked cells are absent. Shared by the
attribution predicate (`_pop_has_authored_tracks`) and the per-run delete's track-set listing
(`analysis_runs.jl`); the viewer's track payload (`api/src/viewer_api.jl`) reads the column with the
same legacy rule.
"""
function label_track_sources(img::CciaImage, value_name::AbstractString)::Dict{Int,Union{String,Nothing}}
    label_to_source = Dict{Int,Union{String,Nothing}}()
    lp = label_props(img; value_name = value_name)
    obs_cols = col_names(lp; data_type = :obs)
    "track_id" in obs_cols || return label_to_source
    cols = "track_source" in obs_cols ? ["track_id", "track_source"] : ["track_id"]
    select_cols(lp, cols)
    tdf = as_df(lp)
    has_src = "track_source" in names(tdf)
    @inbounds for i in 1:size(tdf, 1)
        tid = tdf[i, :track_id]
        (tid isa Real && isfinite(Float64(tid)) && Float64(tid) > 0) || continue
        src::Union{String,Nothing} = nothing
        if has_src
            s = tdf[i, :track_source]
            # `s` may be missing (unmarked row, legacy), a string (categorical / object), or an
            # empty string. Everything except a non-empty string is "unmarked" → legacy branch.
            (s isa AbstractString && !isempty(s)) && (src = String(s))
        end
        label_to_source[Int(tdf[i, :label])] = src
    end
    label_to_source
end

"""
    orphan_track_sources(img, value_name) -> Vector{NamedTuple}

The tracks on a segmentation whose authoring population is gone: one `(source, path, cells)` per
`track_source` that is a pop UID no longer in the flow map (tracking only ever stamps a flow pop's
UID or `WHOLE_SEG_TRACK_SOURCE`). `path` is where that pop last was (`removed_pop_path`), `nothing`
when the gate history doesn't reach back; `cells` counts its tracked cells. No pop can draw these
(`_pop_has_authored_tracks` refuses them, MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 1) until the
next tracking run sweeps them, so the viewer says why its ribbons are missing rather than "turn
tracks on". Legacy unmarked rows are everyone's, never orphans. Sorted by `cells`, most first.
"""
function orphan_track_sources(img::CciaImage, value_name::AbstractString)::Vector{NamedTuple}
    counts = _pop_track_source_counts(img, value_name)
    isempty(counts) && return NamedTuple[]
    m = load_pop_tree(img; value_name = value_name, pop_type = "flow")
    out = NamedTuple[]
    for (src, n) in counts
        pop_path_by_uid(m, src) === nothing || continue
        path = try removed_pop_path(img._dir, value_name, src; pop_type = "flow") catch; nothing end
        push!(out, (source = src, path = path, cells = n))
    end
    sort!(out; by = o -> -o.cells)
end

# tracked cells per authoring POP uid — whole-seg runs and legacy unmarked rows belong to no one pop
function _pop_track_source_counts(img::CciaImage, value_name::AbstractString)::Dict{String,Int}
    counts = Dict{String,Int}()
    for src in values(_label_track_sources_cached(img, value_name))
        (src === nothing || src == WHOLE_SEG_TRACK_SOURCE) && continue
        counts[src] = get(counts, src, 0) + 1
    end
    counts
end

"""
    pop_track_sources(img, value_name) -> Vector{String}

The UIDs of the populations whose tracking run made this segmentation's tracks (`track_source`),
sorted. The population manager warns on deleting one of them: its tracks stay in the cell table but
no population draws them as ribbons until the next tracking run (`orphan_track_sources`). Empty when
the segmentation has no cell table or no pop-authored tracks.
"""
function pop_track_sources(img::CciaImage, value_name::AbstractString)::Vector{String}
    haskey(img.label_props, value_name) || return String[]
    sort!(collect(keys(_pop_track_source_counts(img, value_name))))
end

# MODULE-LEVEL, NOT ON THE IMAGE: `init_object` builds a fresh `CciaImage` per request, so the
# image-held cache this used to live in never survived one — every viewer overlay, landscape and
# mask-colour request re-ran the gate evaluation (the same trap `_TRACKED_PARENTS_CACHE` documents).
# ONE SLOT per (image dir, value_name, pop_type) holding (stamp, result): a gate edit replaces the
# slot rather than adding a key, so the cache holds at most one membership set per segmentation × pop
# type. Computed OUTSIDE the lock — a slow evaluation on one image must not stall another's request;
# two concurrent misses on the same slot both compute and the later write wins, same answer.
# BOUNDED (`BoundedCache`, utils.jl): a slot's labels are ~5 MB on a 600k-cell table, and a session
# that opens many images would otherwise keep every one.
const _RESOLVE_POPS_CACHE = BoundedCache(32)

# `label_track_sources` for `_resolve_pops_compute`, keyed on the cell table alone. The resolver's own
# stamp follows every gating map, so without this each gate write or colour change re-read the table's
# tracking columns: 0.17 s of a 608k-cell movie's resolve, for an answer only re-tracking changes.
# Read-only use here (`_pop_has_authored_tracks` only looks up) — other callers keep their own fresh copy.
const _LABEL_TRACK_SOURCES_CACHE = BoundedCache(8)
function _label_track_sources_cached(img::CciaImage, value_name::AbstractString)
    lpp = img_label_props_path(img, value_name)
    cached!(_LABEL_TRACK_SOURCES_CACHE, string(lpp); stamp = file_stamp(lpp)) do
        label_track_sources(img, value_name)
    end::Dict{Int,Union{String,Nothing}}
end

# `pop_df`'s frames, same reasoning (they used to live on the image too). A frame can be large — a
# 600k-cell read with every channel runs to ~100 MB — so this one is bounded by bytes as well as by
# count, and a single frame over the byte budget is returned without being kept.
# A frame's size for the byte budget — an ESTIMATE, cheap on purpose: `Base.summarysize` walks every
# object through an IdDict (1.3 s on a 608k-row frame with a String column). Inline element types
# are exact; otherwise a pointer per row plus each string's bytes (shared strings counted per row,
# so it errs high) or a flat 16 bytes for any other boxed value.
_col_nbytes(v::AbstractVector{T}) where {T} = Base.allocatedinline(T) ?
    Base.aligned_sizeof(T) * length(v) + (isbitstype(T) ? 0 : length(v)) :
    8 * length(v) + sum(x -> x isa AbstractString ? sizeof(x) : 16, v; init = 0)
_df_nbytes(df::DataFrame)::Int = sum(_col_nbytes, eachcol(df); init = 0)
const _POP_DF_CACHE = BoundedCache(64; max_bytes = 1 << 30, nbytes = _df_nbytes)

# Everything a segmentation's gate evaluation depends on, for a cache that outlives a request
# (`_RESOLVE_POPS_CACHE`, `_TRACKED_PARENTS_CACHE`). A gating-map mtime alone was enough for a cache
# that died with the request; across requests it misses three things, so the stamp is:
#   • the SHA-256 of every gating map of the image — a cluster pop type with no map of its own borrows
#     a sibling segmentation's (`_borrow_cluster_pop_tree`), and a second save inside one mtime tick
#     (1 s on HFS+/FAT/some network mounts) leaves the mtime unchanged. A real digest, not `hash`: Julia
#     samples only some elements of an array of 8192+ bytes, so a same-length edit to a large map (a
#     cluster id 17→18) could leave `hash` unchanged. The maps are small; `_file_sha256` is the helper.
#   • this segmentation's `labelProps/{vn}*.json` sidecars (the cluster-feature suffixes a borrow
#     checks), digested the same way, and the h5ad by mtime + size (too big to digest per request).
#   • the image's µm/px — `load_pop_tree` scales spatial gates by it, and a calibration edit touches
#     only ccid.json.
function _gating_inputs_stamp(img::CciaImage, vn::AbstractString)::String
    io = IOBuffer()
    print(io, img_is_calibrated(img) ? first(img_physical_sizes(img)) : "uncal")
    gd = gating_dir(img._dir)
    if isdir(gd)
        for f in sort(readdir(gd))
            endswith(f, ".json") && print(io, "|", f, "=", _file_sha256(joinpath(gd, f)))
        end
    end
    lpp = img_label_props_path(img, vn)
    ld = dirname(lpp)
    if isdir(ld)
        for f in sort(readdir(ld))
            (startswith(f, vn) && endswith(f, ".json")) && print(io, "|", f, "=", _file_sha256(joinpath(ld, f)))
        end
    end
    print(io, "|h5ad=", file_stamp(lpp))
    String(take!(io))
end

"""
    resolve_pops(img, pop_type; value_name) -> Vector{NamedTuple}

Resolve a segmentation's stored populations to display-ready, membership-resolved entries: one per
**non-transient, non-empty** population of `value_name` under `pop_type`, as
`(path, name, colour, show, is_track, labels)` where `labels` is the pop's member cell IDs (raw label
IDs for cell pop_types). Membership is recomputed from the on-disk gating map + cell table, then
**cached at module level** (`_RESOLVE_POPS_CACHE`) under `_gating_inputs_stamp` — every gating map's
digest, the segmentation's sidecars, the h5ad's mtime + size and the image's µm/px. So an unchanged
segmentation returns instantly, and a saved gate edit on ONE segmentation invalidates only its entry
(a caller iterating every segmentation then only recomputes the one that changed). Complements
`cells_in_pop` (one pop, live map): this is the whole set, cached. The returned vector and its
`labels` are SHARED with the cache — read them, never mutate them.

The transient napari-selection pop is intentionally excluded (it's the selection *source*; rendering
it back would steal the legacy viewer's active layer). Cell pop_types only (`flow`/`clust`) — track pops go
through the Tracks overlay (`show_tracks`), not this points path.
"""
function resolve_pops(img::CciaImage, pop_type::PopTypeArg;
                      value_name::AbstractString)::Vector{NamedTuple}
    pt = string(pop_type)
    # Track-family pops (`track`/`trackclust`) have their gates on `track_props` (label==track_id,
    # `clusters.{suffix}`, motility metrics) — a `label_props` fetch can't evaluate them and every
    # pop degrades to empty members ("clusters.movement absent from the fetched data" per
    # gating_engine._filter_mask). Route through the canonical `pop_df(granularity=:cell)` path
    # `overlay_author.jl:489` already uses for exactly this case, then hand back the SAME
    # cell-label semantics every existing caller of resolve_pops expects (viewer overlays'
    # `filterPayloadByLabels`, landscape_api's label→tile map). Deliberately BEFORE the outer
    # cache read: the cache key hashes `img_label_props_path`'s mtime, which is insensitive to
    # updates in the `{vn}__tracks.h5ad` file we actually resolve against; `pop_df`'s own
    # mtime-keyed cache sits underneath the helper and handles that correctly.
    if is_track_grained(pt)
        return _resolve_track_family_pops(img, pt, String(value_name))
    end
    slot  = string(img._dir, "|", value_name, "|", pt)
    stamp = _gating_inputs_stamp(img, value_name)
    cached!(_RESOLVE_POPS_CACHE, slot; stamp) do
        _resolve_pops_compute(img, pt, String(value_name))
    end::Vector{NamedTuple}
end

function _resolve_pops_compute(img::CciaImage, pt::String, value_name::String)::Vector{NamedTuple}
    m = load_pop_tree(img; value_name = value_name, pop_type = pt)
    # `labels` are CELL labels, so membership is evaluated over the cell table — the shared source's
    # cell branch, whatever `pt` is (a `branch` map's ids are not cells). Raw column names. No transient
    # selection injection here — it must not enter the cached (pure on-disk) result.
    fetch = pop_membership_fetch(img, value_name, "flow")
    # the membership cache the plots and stats share (`recompute_cached!`): an edit evaluates the map
    # once, not once for the viewer and again for the plots. Its table stamp follows `pt`'s table, so
    # only the cell-table pop types take it (a `branch` map read here is evaluated over cells).
    _membership_table(pt) == MEMBERSHIP_CELL ? recompute_cached!(m, img, value_name, pt, fetch) :
                                               recompute!(m, fetch)
    # `has_tracks`: does this pop hold any cell with `track_id > 0` AND authored by this pop (or by
    # whole-seg tracking, or from a legacy row with no `track_source` marker)? Provenance-aware — an
    # orphan row from a deleted pop whose label happens to fall inside a live pop's gate no longer
    # bleeds into that pop's ribbon, and two overlapping live pops don't cross-claim each other's
    # tracks. Data-based, so a hand-drawn flow gate on cells that were later tracked still qualifies
    # without any change to `is_track` (which stays "was this pop TYPED as a track pop"). See
    # docs/todo/MULTI_POP_TRACKING_PLAN.md Decision 2 (`has_tracks` flag) and
    # docs/todo/MULTI_POP_TRACKING_ORPHANS_PLAN.md Decision 1 (attribution guard). One label-props
    # read, cached with the rest of `resolve_pops`'s output (`_RESOLVE_POPS_CACHE`, `_gating_inputs_stamp`-keyed), so the whole
    # membership + tracks lookup pays its cost once per (segmentation × edit).
    #
    # Legacy row (no `track_source` column, or NaN in it) → treated as whole-seg-equivalent
    # (claimed by every pop that touches its label). Preserves the pre-orphan-guard behaviour for
    # projects tracked before the P1 provenance ship; re-tracking rewrites the marker and the guard
    # tightens.
    label_to_source = _label_track_sources_cached(img, value_name)
    out = NamedTuple[]
    for path in pop_paths(m)
        p = pop_at(m, path)
        p.transient && continue
        labs = Int.(cells_in_pop(m, path))
        isempty(labs) && continue
        has_tracks = _pop_has_authored_tracks(p.uid, labs, label_to_source)
        # `uid` carried through so the client can filter cells whose `track_source` matches ONLY
        # this pop's authoring — needed by the viewer's per-pop ribbon to stop cells authored by
        # a sibling pop from bleeding into this pop's tracks. See viewer_api.jl overlays payload +
        # frontend `filterPayloadByTrackSource`. Additive field; existing callers ignore it.
        push!(out, (path = p.path, name = p.name, colour = p.colour, uid = p.uid,
                    show = p.show, is_track = is_track_pop(pt, p.path), has_tracks = has_tracks,
                    labels = labs))
    end
    out
end

# Ensure a `fetch(vn, cols)` provider also returns `vn`'s centroid columns. Resolved PER value_name —
# which axes exist differs per segmentation (no `centroid_z` on a 2D one), so a single global column
# list would ask a 2D file for a column it hasn't got. Appended to the PUSHDOWN (not filtered after the
# read), so `centroids=` costs the same as naming the columns in `pop_cols`: measured identical to a
# bare `view_centroid_cols` read at 1.5M cells. An EMPTY selection is already the read-everything
# shape, which carries the centroids anyway — leave it alone rather than narrowing it.
_fetch_with_centroids(img::CciaImage, fetch::Function; labels_version = nothing) = function (vn, cols)
    isempty(cols) && return fetch(vn, cols)
    lp = label_props(img; value_name = vn, version = labels_version)
    fetch(vn, unique(vcat(String.(cols), centroid_columns(lp), temporal_columns(lp))))
end

# The ONE exit for every `pop_df` branch: the cache holds the frame AS READ (pixels), and the unit
# conversion is applied to the returned COPY. Two reasons it goes here rather than at each `return`:
# `pop_df` has five of them (track/trackclust, branch, labels, :track, cell) and a new one would
# silently skip the conversion; and scaling the copy keeps the cached frame in one coordinate system, so
# `:pixel` and `:physical` can share a cached read (`copy` is copycols=true, so this cannot write
# through to it).
#
# `img` is in scope for the whole method — a `pop_df` call is always scoped to ONE image — so no per-row
# image lookup is needed. The set-level method calls this one per image, which is what makes pooling
# across images with DIFFERENT pixel sizes correct: each contribution is scaled before the `vcat`, and a
# pooled frame never has one physical size applied to it afterwards.
function _pop_df_finish(df::DataFrame, img::CciaImage, centroids::Union{Bool,Symbol})::DataFrame
    centroids === false && return df
    if !any(c -> occursin(r"^centroid_[xyz]$", c), names(df))
        # A track-grained or branch frame has no cell coordinates to convert (track tables hold measures
        # + lineage; a branch table's coordinates are `image-coord-src/dst-*`, not centroids). Say so
        # instead of handing back a frame that quietly ignored the argument.
        nrow(df) == 0 || @warn "pop_df(centroids=$(repr(centroids))): no centroid_x/_y/_z in this \
            frame — track-grained and branch frames carry no cell coordinates." maxlog = 1
        return df
    end
    centroids === :pixel && return df
    if !img_is_calibrated(img)
        # `img_physical_sizes` defaults a missing axis to 1.0, so scaling would be a no-op that is
        # nonetheless LABELLED µm. Pixels are the honest answer; say which one the caller got.
        @warn "pop_df(centroids=:physical): image '$(img.uid)' has no physical pixel size — returning \
            PIXELS, not µm. Set it on the image (Image info → physical size) and re-run." maxlog = 1
        return df
    end
    scale_centroids!(df, img)
end

# a hooked read is not cached (see `pop_df`)
_pop_df_store!(img::CciaImage, ckey, df, map_hook) =
    (map_hook === nothing && cache_put!(_POP_DF_CACHE, ckey.key, ckey.stamp, df); df)

"""
    pop_df(img, pop_type, pops; value_name=nothing, pop_cols=nothing, include_x=false,
           include_obs=true, unique_labels=true, drop_na=false, flush_cache=false,
           raw_channel_names=false, centroids=false, labels_version=nothing,
           map_hook=nothing) -> DataFrame

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
  (numeric → `.mean/.median/.sum/.qUp/.qLow/.sd`, categorical → per-category frequency `{m}.{cat}`).
  Any aggregate a per-track read's `pop_cols` NAME is derived automatically (`track_aggregate_measures`
  — `live.cell.hmm.state.movement.1` → base `live.cell.hmm.state.movement`), for every track-grained
  read (`track`/`trackclust`, and `granularity=:track`); pass `cell_measures` only to get ALL of a
  base's aggregates without naming them (clustTracks; mirrors R `tracksInfo`'s `trackStatsNames`).
- `value_name=nothing` resolves to the image's **active** segmentation (parity with
  `label_props(img)`); pass a name to override the default value_name for unprefixed pops.
- `labels_version` pins the cell-table reads (membership and output, `granularity=:track`'s cell-level
  membership included) to a labels vN (`nothing` = `_latest`); the per-track table has no
  labels-version axis, so track-table reads ignore it.
- `map_hook(m)` edits each freshly-loaded map before membership is evaluated — e.g. the API's
  transient pick-selection pop. A hooked read bypasses the cache.
- `drop_na=true` drops cells that are NA/NaN in any requested `pop_cols` (mirrors R popDT
  `dropNA`).
- Results are cached per image keyed by the request signature **plus the on-disk mtimes of the
  involved gating maps + h5ads**, so a saved gate edit or a re-tracked h5ad **auto-invalidates**
  the cache. `flush_cache=true` is a manual override for in-memory edits never written to disk.

By default intensity columns are returned under their **channel names** (e.g.
`mean_intensity_0` → `"CD4"`), mirroring R `popDT` / Python `change_channel_names` — pass
`raw_channel_names=true` to keep the raw `{measure}_intensity_{i}` column names instead.

`pop_cols` may name an intensity column **by its channel name** (`"CD4"`, `"nuc_CD4"`) OR by its raw
`{measure}_intensity_{i}` name — the reader resolves a channel name to its raw column, so you can
request the name you see. (Get the channel names from `get_gating_channels`/`get_measure_summary`.)

`centroids` adds the cells' **coordinates** without you having to name the columns (which axes exist
differs per segmentation — no `centroid_z` on a 2D image):
- `false` (default) — unchanged: present in the frame only if you asked for them (via `pop_cols`) or
  requested no columns at all.
- `:pixel` — the present `centroid_x`/`_y`/`_z` (+ `centroid_t`), **as stored: pixels and frames**.
  Resolved per value_name, so pooling a 2D and a 3D segmentation in one call works.
- `:physical` — the same columns with x/y/z scaled to **µm** (`scale_centroids!`, each axis by its own
  resolution). Applied per image *before* pooling, so a set-level call across images with different
  pixel sizes is correct — a pooled frame has no single physical size to apply afterwards.
  `centroid_t` stays a FRAME index (see `scale_centroids!`).

Prefer this over reading centroids through `label_props` yourself: it is the same narrow read (the
columns are pushed into the reader, not filtered afterwards) and it resolves membership in the same
call. On an **uncalibrated** image `:physical` warns and returns pixels — `img_physical_sizes` defaults
a missing axis to 1.0, so there is otherwise nothing to tell "µm" and "pixels" apart.
"""
function pop_df(img::CciaImage, pop_type::PopTypeArg, pops;
                value_name::Union{AbstractString,Nothing}=nothing, pop_cols=nothing,
                include_x::Bool=false, include_obs::Bool=true, unique_labels::Bool=true,
                drop_na::Bool=false, flush_cache::Bool=false,
                raw_channel_names::Bool=false, granularity::Symbol=:cell,
                cell_measures=String[], categorical=String[],
                expand_cluster_pops::Bool=true,
                centroids::Union{Bool,Symbol}=false,
                labels_version::Union{AbstractString,Nothing}=nothing,
                map_hook::Union{Function,Nothing}=nothing)::DataFrame
    granularity in (:cell, :track) ||
        error("pop_df: granularity must be :cell or :track (got :$granularity)")
    (centroids === false || centroids === :pixel || centroids === :physical) ||
        error("pop_df: centroids must be false, :pixel or :physical (got $(repr(centroids)))")
    # value_name=nothing → active segmentation key (same resolution as label_props(img))
    resolved_vn = resolve_value_name(img, value_name)
    # cluster pops are GLOBAL to a run → a bare ref spans all co-clustered segmentations (R popDT
    # parity); value_name-prefixed refs are untouched. No-op for non-cluster pop_types.
    #
    # `expand_cluster_pops=false` turns that off for a caller that has ALREADY decided the segmentation
    # per pop. The plot series path is the case that needs it: the picker offers each (segmentation,
    # population) pair as its OWN row, so selecting three region pops under `B` must plot exactly those
    # three — not six, silently doubled by the run-wide expansion into `T`. It cannot be inferred from
    # `value_name` being explicit, because the per-POPULATION cluster heatmap deliberately passes an
    # explicit value_name AND wants the expansion (docs/todo/CLUSTER_POOLING_PLAN.md), so the two
    # intentions have to be stated rather than guessed.
    pops = expand_cluster_pops ?
        _expand_cluster_pops(img, pops, string(pop_type), resolved_vn) : pops

    ckey = _pop_df_cache_key(img, pop_type, resolved_vn, pops, pop_cols, include_x, include_obs,
                             unique_labels, drop_na, raw_channel_names, granularity,
                             cell_measures, categorical, centroids, labels_version)
    # A `map_hook` edits the map in memory, which no on-disk stamp can see, and a closure has no
    # stable key — so a hooked read neither reads nor fills the cache (`_pop_df_store!`).
    if map_hook === nothing && !flush_cache
        hit = cache_get(_POP_DF_CACHE, ckey.key, ckey.stamp)
        hit === nothing || return _pop_df_finish(copy(something(hit)::DataFrame), img, centroids)
    end

    # `track` / `trackclust` pop_types: membership is defined directly on per-track properties
    # (one point per track), evaluated over the `track_props` table — `track` via hand-drawn gates
    # in `gating/{vn}__tracks.json`, `trackclust` via a `clusters.{suffix}` filter in
    # `gating/{vn}__trackclust.json`. `granularity` selects the return shape (:track rows, or
    # :cell-expanded member cells). Distinct from the `live`+:track path below, which gates CELL
    # properties and then aggregates to tracks.
    if is_track_grained(pop_type)
        df = _pop_df_track_gating(img, pops, resolved_vn; pop_type=string(pop_type),
                                  cell_measures=cell_measures,
                                  categorical=categorical, pop_cols=pop_cols,
                                  unique_labels=unique_labels, drop_na=drop_na,
                                  granularity=granularity, centroids=centroids,
                                  map_hook=map_hook, labels_version=labels_version)
        _pop_df_store!(img, ckey, df, map_hook)
        return _pop_df_finish(copy(df), img, centroids)
    end

    # `branch` pop_type: gates on per-branch measurements (branch-type, length, tortuosity, …) in
    # the skeleton sidecar `{vn}__branch.h5ad`. `_pop_df` is generic over table location, so the
    # only branch-specific pieces are the pop tree (`gating/{vn}__branch.json`) and the fetch that
    # reads the branch table. Membership is one row per skeleton path; there is no cell/track
    # duality within branches, so `granularity` is ignored (always one row per branch). See
    # docs/todo/BRANCHING_PLAN.md Decisions 1–3.
    if string(pop_type) == "branch"
        branch_load = vn -> load_pop_tree(img; value_name=vn, pop_type="branch")
        branch_fetch = function (vn, cols)
            lp = label_props(img_branch_props_path(img, vn); value_name=vn)
            isempty(cols) || select_cols(lp, cols)
            as_df(lp; include_x=(isempty(cols) ? include_x : true), include_obs=include_obs)
        end
        # NOT decorated with `centroids`: a branch table has no cell centroids at all — its coordinates
        # are `image-coord-src/dst-*` (see `branch_segments` in anisotropy.jl), and resolving centroid
        # names here would read the CELL table's axes against the branch sidecar. `_pop_df_finish` warns
        # if a caller asked for coordinates on this path.
        df = _pop_df(branch_load, branch_fetch, "branch", pops;
                     default_vn=resolved_vn, pop_cols=pop_cols, unique_labels=unique_labels,
                     drop_na=drop_na,
                     membership_fetch=(vn, cols) -> pop_membership_fetch(img, vn, "branch")(cols))
        _pop_df_store!(img, ckey, df, map_hook)
        return _pop_df_finish(copy(df), img, centroids)
    end

    # `labels` pop_type: ALL cells of the segmentation's labelProps, UNGATED — no gating map, no
    # membership eval. Mirrors the old R popType "labels" (`labelsPopUtils`). This is the raw
    # segmentation-output data source (e.g. the segmentation-integrity QC canvas): every measured
    # object of `resolved_vn`, tagged with a single "/labels" pop path so the summary framework groups
    # it like any other population (path starts with "/", matching the picker + manager-form id).
    # There are no sub-populations, so a pop ref carries no PATH information — but it still names its
    # segmentation as a value_name PREFIX (`"Neutrophil/labels"`), the same grammar every other
    # pop_type gets from `_group_pops_by_value_name`. Honour it, so ONE call can pool several
    # segmentations and the QC canvas can plot Tcell against Neutrophil (this used to read
    # `resolved_vn` and ignore `pops` entirely, which silently returned the image's ACTIVE
    # segmentation whichever one the picker asked for — the wrong data under the right label).
    # A value_name absent on THIS image is skipped rather than an error: a set-level call spans images
    # that were not all segmented the same way (one image's cellpose run can legitimately yield zero
    # objects and write no labelProps), mirroring the gated path's skip for a pop defined on only
    # some images. With no `pops` (or a leading-slash ref) this resolves to the active segmentation,
    # exactly as before.
    if string(pop_type) == "labels"
        want = isempty(pops) ? [resolved_vn] :
               sort!(collect(keys(_group_pops_by_value_name(pops, resolved_vn))))
        # The skip applies to the EXTRA value_names a multi-segmentation request names, never to the
        # image's own resolved one: if that is somehow unreadable the caller should get `label_props`'
        # error as before, not a silently empty frame.
        parts = DataFrame[]
        for vn in filter(v -> v == resolved_vn || img_has_value_name(img, v), want)
            lp = label_props(img; value_name=vn, version=labels_version) |>
                 v -> rename_channels!(v, !raw_channel_names)
            # No columns requested — `nothing` OR `String[]` — means a bare cell COUNT: read neither X nor
            # obs, just label/centroids. Reading every obs measure for millions of cells only to count rows
            # needlessly loads the (single-process) API and would stall e.g. a queued napari open. (Empty and
            # `nothing` used to diverge — `String[]` slipped past this into a muddled "all X, no usable obs"
            # state.) With pop_cols set, `select_cols` narrows the read to exactly those columns. NOTE: this
            # path is for gated/measure reads — to summarise an OBS column (HMM state, clusters), read it
            # directly via `as_df`, not through here (obs isn't a first-class pushdown target).
            no_cols = pop_cols === nothing || isempty(pop_cols)
            # `centroids` widens a narrowed read to include this value_name's coordinate columns; the
            # no-columns read already returns label/centroids, so it needs nothing. Resolved per
            # value_name — two segmentations of one image need not share an axis set.
            cols = no_cols ? String[] :
                   (centroids === false ? String.(pop_cols) :
                    unique(vcat(String.(pop_cols), centroid_columns(lp), temporal_columns(lp))))
            no_cols || select_cols(lp, cols)
            d = as_df(lp; include_x=(no_cols ? include_x : true),
                          include_obs=(no_cols ? false : include_obs))
            d[!, "value_name"] .= vn
            d[!, "pop"]        .= "/labels"
            push!(parts, d)
        end
        # `label` is only unique WITHIN a segmentation, so a pooled frame repeats label ids across
        # value_names. That is fine here and must stay that way: nothing on this path dedups by label
        # (the gated path's `unique_labels` never applied to it), and the (value_name, label) pair is
        # the real key — dedup by label alone would silently drop one segmentation's cells.
        df = isempty(parts) ? DataFrame() :
             length(parts) == 1 ? parts[1] : reduce((a, b) -> vcat(a, b; cols=:union), parts)
        _pop_df_store!(img, ckey, df, map_hook)
        return _pop_df_finish(copy(df), img, centroids)
    end

    # Derived pop_types (e.g. `live`): gates are stored under `flow`; layer the derived pops
    # (e.g. _tracked) on top, transiently. Pop_types with no registered derived specs load normally.
    groups = _group_pops_by_value_name(pops, resolved_vn)
    load_map = vn -> _read_pop_tree(img, vn, pop_type; derived_paths=get(groups, vn, String[]),
                                   map_hook=map_hook)

    # :track → one row per track (features from `track_props`: motility ⊕ `cell_measures` aggregates),
    # membership still evaluated at cell level then mapped to tracks.
    if granularity === :track
        df = _pop_df_tracks(img, load_map, pops, resolved_vn;
                            pop_cols=pop_cols, unique_labels=unique_labels, drop_na=drop_na,
                            cell_measures=cell_measures, categorical=categorical,
                            labels_version=labels_version)
        _pop_df_store!(img, ckey, df, map_hook)
        return _pop_df_finish(copy(df), img, centroids)
    end
    # label_props chain idiom (docs/DATAMODEL.md). Output columns resolve channel names by
    # default (raw_channel_names=true keeps raw); the conditional select + as_df kwargs keep
    # this as fluent statements rather than a single pipe.
    fetch = function (vn, cols)
        lp = label_props(img; value_name=vn, version=labels_version) |>
             v -> rename_channels!(v, !raw_channel_names)
        isempty(cols) || select_cols(lp, cols)
        as_df(lp; include_x=(isempty(cols) ? include_x : true), include_obs=include_obs)
    end
    # membership/gate eval: the shared source, raw column names (gates store raw intensity names)
    membership_fetch = (vn, cols) -> pop_membership_fetch(img, vn, pop_type;
                                                          labels_version=labels_version)(cols)
    # only the OUTPUT fetch gains the centroid columns — gate eval (`membership_fetch`) reads exactly
    # the columns its gates name and must not be widened.
    out_fetch = centroids === false ? fetch :
                _fetch_with_centroids(img, fetch; labels_version = labels_version)
    evaluate = (m, vn) -> recompute_cached!(m, img, vn, pop_type,
                                            pop_membership_fetch(img, vn, pop_type; labels_version=labels_version);
                                            labels_version=labels_version)
    df = _pop_df(load_map, out_fetch, pop_type, pops;
                 default_vn=resolved_vn, pop_cols=pop_cols, unique_labels=unique_labels,
                 drop_na=drop_na, membership_fetch=membership_fetch, evaluate=evaluate)
    _pop_df_store!(img, ckey, df, map_hook)
    # a copy so callers never mutate the cached frame; `_pop_df_finish` converts units on that copy
    _pop_df_finish(copy(df), img, centroids)
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

Because the work happens per image, `centroids = :physical` is correct across images with **different
pixel sizes**: each image's coordinates are scaled with its OWN resolution before the `vcat`. Never
scale a pooled frame afterwards — it has no single physical size to apply.
"""
function pop_df(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector, pop_type::PopTypeArg,
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

