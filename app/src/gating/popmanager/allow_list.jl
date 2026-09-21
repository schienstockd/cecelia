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

const ACCEPT_TOKENS = ("live", "flow", "clust", "trackclust", "region", "track", "branch")

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
    bad = setdiff(acc, ("live", "clust", "trackclust", "region", "track", "branch"))
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

# scope → accepts tokens, for the back-compatible `popScope` shim.
function _scope_accepts(scope::AbstractString, include_clusters::Bool)::Vector{String}
    if String(scope) == "tracks"
        acc = ["track"]; include_clusters && push!(acc, "trackclust")
    elseif String(scope) == "cells"
        acc = ["live"]; include_clusters && append!(acc, ("clust", "region"))
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
function _inject_derived_pops!(m::PopulationMap, paths, pop_type::PopTypeArg)::PopulationMap
    for path in paths
        path = String(path)
        has_pop(m, path) && continue
        spec = get(_DERIVED_POPS, pop_name(path), nothing)
        (spec === nothing || spec.pop_type != string(pop_type)) && continue
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
                           granularity, cell_measures, categorical, centroids = false)::String
    is_track  = string(pop_type) == "track"
    is_branch = string(pop_type) == "branch"
    stamps = String[]
    for vn in sort(collect(keys(_group_pops_by_value_name(pops, value_name))))
        # track gating reads `{vn}__tracks.json`; branch gating reads `{vn}__branch.json`;
        # cell-level pop_types read the flow map. Always fold the cell h5ad (track_props derives
        # from it; branch_props does not, but folding it costs one stat and disambiguates keys).
        gt = is_track ? "track" : (is_branch ? "branch" : "flow")
        gmtime = _pop_df_mtime(gating_path(img._dir, vn; pop_type=gt))
        s = string(vn, "@", gmtime, "/", _pop_df_mtime(img_label_props_path(img, vn)))
        # :track granularity and track-gating both read the companion track table — fold its mtime
        # so a re-run (or a saved track gate) auto-invalidates.
        (granularity === :track || is_track) &&
            (s *= "/" * _pop_df_mtime(img_track_props_path(img, vn)))
        # branch pop_type reads the per-branch sidecar; fold its mtime for cache invalidation.
        is_branch && (s *= "/" * _pop_df_mtime(img_branch_props_path(img, vn)))
        push!(stamps, s)
    end
    parts = (pop_type, value_name, join(sort(String.(collect(pops))), "&"),
             pop_cols === nothing ? "" : join(sort(String.(collect(pop_cols))), "&"),
             include_x, include_obs, unique_labels, drop_na, raw_channel_names, granularity,
             join(sort(String.(collect(cell_measures))), "&"),
             join(sort(String.(collect(categorical))), "&"),
             # only WHETHER centroids were read belongs in the key, not the unit: the cache holds the
             # frame as read (pixels) and `:physical` scales the returned copy, so `:pixel` and
             # `:physical` legitimately share one cached read instead of doubling the entries.
             centroids !== false,
             join(stamps, "|"))
    string(hash(parts))
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
    resolve_pops(img, pop_type; value_name) -> Vector{NamedTuple}

Resolve a segmentation's stored populations to display-ready, membership-resolved entries: one per
**non-transient, non-empty** population of `value_name` under `pop_type`, as
`(path, name, colour, show, is_track, labels)` where `labels` is the pop's member cell IDs (raw label
IDs for cell pop_types). Membership is recomputed from the on-disk gating map + cell table, then
**cached on the image** (`_pop_df_cache`, `Dict{String,Any}`) under a key folding the gating-map and
h5ad mtimes — the SAME auto-invalidation trick `pop_df` uses (`_pop_df_mtime`). So an unchanged
segmentation returns instantly, and a saved gate edit on ONE segmentation invalidates only its entry
(a caller iterating every segmentation — e.g. a napari overlay refresh — then only recomputes the one
that changed). Complements `cells_in_pop` (one pop, live map): this is the whole set, cached.

The transient napari-selection pop is intentionally excluded (it's the selection *source*; rendering
it back would steal the legacy viewer's active layer). Cell pop_types only (`flow`/`clust`) — track pops go
through the Tracks overlay (`show_tracks`), not this points path.
"""
function resolve_pops(img::CciaImage, pop_type::PopTypeArg;
                      value_name::AbstractString)::Vector{NamedTuple}
    pt = string(pop_type)
    ckey = string("poplayers:", value_name, ":", pt, "@",
                  _pop_df_mtime(gating_path(img._dir, value_name; pop_type = pt)),
                  "/", _pop_df_mtime(img_label_props_path(img, value_name)))
    cached = get(img._pop_df_cache, ckey, nothing)
    cached === nothing || return cached::Vector{NamedTuple}
    m = load_pop_map(img; value_name = value_name, pop_type = pt)
    # membership eval uses RAW column names (gates store raw intensity column names). No transient
    # napari-selection injection here — it must not enter the cached (pure on-disk) result.
    fetch = cols -> (lp = label_props(img; value_name = value_name);
                     isempty(cols) || select_cols(lp, cols); as_df(lp))
    recompute!(m, fetch)
    # `has_tracks`: does this pop hold any cell with `track_id > 0` AND authored by this pop (or by
    # whole-seg tracking, or from a legacy row with no `track_source` marker)? Provenance-aware — an
    # orphan row from a deleted pop whose label happens to fall inside a live pop's gate no longer
    # bleeds into that pop's ribbon, and two overlapping live pops don't cross-claim each other's
    # tracks. Data-based, so a hand-drawn flow gate on cells that were later tracked still qualifies
    # without any change to `is_track` (which stays "was this pop TYPED as a track pop"). See
    # docs/todo/MULTI_POP_TRACKING_PLAN.md Decision 2 (`has_tracks` flag) and
    # docs/todo/MULTI_POP_TRACKING_ORPHANS_PLAN.md Decision 1 (attribution guard). One label-props
    # read, cached on the image with the rest of `resolve_pops`'s output (mtime-keyed), so the whole
    # membership + tracks lookup pays its cost once per (segmentation × edit).
    #
    # Legacy row (no `track_source` column, or NaN in it) → treated as whole-seg-equivalent
    # (claimed by every pop that touches its label). Preserves the pre-orphan-guard behaviour for
    # projects tracked before the P1 provenance ship; re-tracking rewrites the marker and the guard
    # tightens.
    label_to_source = Dict{Int,Union{String,Nothing}}()
    begin
        lp = label_props(img; value_name = value_name)
        obs_cols = col_names(lp; data_type = :obs)
        if "track_id" in obs_cols
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
                    if s isa AbstractString && !isempty(s)
                        src = String(s)
                    end
                end
                label_to_source[Int(tdf[i, :label])] = src
            end
        end
    end
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
                    show = p.show, is_track = p.is_track, has_tracks = has_tracks,
                    labels = labs))
    end
    img._pop_df_cache[ckey] = out
    out
end

# Ensure a `fetch(vn, cols)` provider also returns `vn`'s centroid columns. Resolved PER value_name —
# which axes exist differs per segmentation (no `centroid_z` on a 2D one), so a single global column
# list would ask a 2D file for a column it hasn't got. Appended to the PUSHDOWN (not filtered after the
# read), so `centroids=` costs the same as naming the columns in `pop_cols`: measured identical to a
# bare `view_centroid_cols` read at 1.5M cells. An EMPTY selection is already the read-everything
# shape, which carries the centroids anyway — leave it alone rather than narrowing it.
_fetch_with_centroids(img::CciaImage, fetch::Function) = function (vn, cols)
    isempty(cols) && return fetch(vn, cols)
    lp = label_props(img; value_name = vn)
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

"""
    pop_df(img, pop_type, pops; value_name=nothing, pop_cols=nothing, include_x=false,
           include_obs=true, unique_labels=true, drop_na=false, flush_cache=false,
           raw_channel_names=false, centroids=false) -> DataFrame

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
                centroids::Union{Bool,Symbol}=false)::DataFrame
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
                             cell_measures, categorical, centroids)
    flush_cache && delete!(img._pop_df_cache, ckey)
    haskey(img._pop_df_cache, ckey) &&
        return _pop_df_finish(copy(img._pop_df_cache[ckey]::DataFrame), img, centroids)

    # `track` / `trackclust` pop_types: membership is defined directly on per-track properties
    # (one point per track), evaluated over the `track_props` table — `track` via hand-drawn gates
    # in `gating/{vn}__tracks.json`, `trackclust` via a `clusters.{suffix}` filter in
    # `gating/{vn}__trackclust.json`. `granularity` selects the return shape (:track rows, or
    # :cell-expanded member cells). Distinct from the `live`+:track path below, which gates CELL
    # properties and then aggregates to tracks.
    if string(pop_type) in ("track", "trackclust")
        df = _pop_df_track_gating(img, pops, resolved_vn; pop_type=string(pop_type),
                                  cell_measures=cell_measures,
                                  categorical=categorical, pop_cols=pop_cols,
                                  unique_labels=unique_labels, drop_na=drop_na,
                                  granularity=granularity, centroids=centroids)
        img._pop_df_cache[ckey] = df
        return _pop_df_finish(copy(df), img, centroids)
    end

    # `branch` pop_type: gates on per-branch measurements (branch-type, length, tortuosity, …) in
    # the skeleton sidecar `{vn}__branch.h5ad`. `_pop_df` is generic over table location, so the
    # only branch-specific pieces are the pop map (`gating/{vn}__branch.json`) and the fetch that
    # reads the branch table. Membership is one row per skeleton path; there is no cell/track
    # duality within branches, so `granularity` is ignored (always one row per branch). See
    # docs/todo/BRANCHING_PLAN.md Decisions 1–3.
    if string(pop_type) == "branch"
        branch_load = vn -> load_pop_map(img; value_name=vn, pop_type="branch")
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
                     drop_na=drop_na, membership_fetch=branch_fetch)
        img._pop_df_cache[ckey] = df
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
            lp = label_props(img; value_name=vn) |> v -> rename_channels!(v, !raw_channel_names)
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
        img._pop_df_cache[ckey] = df
        return _pop_df_finish(copy(df), img, centroids)
    end

    # Derived pop_types (e.g. `live`): gates are stored under `flow`; layer the derived pops
    # (e.g. _tracked) on top, transiently. Pop_types with no registered derived specs load normally.
    groups = _group_pops_by_value_name(pops, resolved_vn)
    has_derived = any(s -> s.pop_type == string(pop_type), values(_DERIVED_POPS))
    load_map = function (vn)
        if has_derived
            m = load_pop_map(img; value_name=vn, pop_type="flow")
            _inject_derived_pops!(m, get(groups, vn, String[]), pop_type)
        else
            load_pop_map(img; value_name=vn, pop_type=pop_type)
        end
    end

    # :track → one row per track (features from `track_props`: motility ⊕ `cell_measures` aggregates),
    # membership still evaluated at cell level then mapped to tracks.
    if granularity === :track
        df = _pop_df_tracks(img, load_map, pops, resolved_vn;
                            pop_cols=pop_cols, unique_labels=unique_labels, drop_na=drop_na,
                            cell_measures=cell_measures, categorical=categorical)
        img._pop_df_cache[ckey] = df
        return _pop_df_finish(copy(df), img, centroids)
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
    # only the OUTPUT fetch gains the centroid columns — gate eval (`membership_fetch`) reads exactly
    # the columns its gates name and must not be widened.
    out_fetch = centroids === false ? fetch : _fetch_with_centroids(img, fetch)
    df = _pop_df(load_map, out_fetch, pop_type, pops;
                 default_vn=resolved_vn, pop_cols=pop_cols, unique_labels=unique_labels,
                 drop_na=drop_na, membership_fetch=membership_fetch)
    img._pop_df_cache[ckey] = df
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

