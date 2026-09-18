# pop_df — the unified population accessor: given (image, value_name, pop_path) return a
# DataFrame of that population's cells, pop_type-agnostic. This is the ONE read path for every
# consumer (plots, exports, gating engine, notebook helpers) — a caller building its own filter is
# a bug, since booleans/derived/regions each resolve differently and re-doing that logic inline is
# how three separately-wrong "just filter label_props" attempts landed in the same PR queue.
# Derived populations (from a transform/re-index of another population) live here too — same
# accessor, different resolver.

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
function _pop_df(load_map::Function, fetch::Function, pop_type::PopTypeArg, pops;
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
                        pop_cols=nothing, unique_labels::Bool=true, drop_na::Bool=false,
                        cell_measures=String[], categorical=String[])::DataFrame
    frames = DataFrame[]
    for (vn, vpops) in _group_pops_by_value_name(pops, default_vn)
        # per-track feature table (label == track_id): motility (from `{vn}__tracks.h5ad`) ⊕ on-read
        # aggregates of `cell_measures` (HMM-state / transition frequencies, intensity/morphology
        # means). SAME source (`track_props`) as the `track`/`trackclust` gating path — so a `live`
        # `_tracked` population clusters on the identical per-track features as a hand-drawn track
        # gate. This is why clustTracks works off `_tracked` pops. Empty → untracked seg, skip.
        ttab = track_props(img; value_name=vn, cell_measures=cell_measures, categorical=categorical)
        nrow(ttab) == 0 && begin
            @warn "pop_df(:track): no tracks for value_name=$vn — run tracking.track_measures first" vn
            continue
        end
        # narrow to requested measures (keeping the track-key bookkeeping); empty pop_cols = all cols
        if !(pop_cols === nothing || isempty(pop_cols))
            keep = intersect(unique(vcat("label", "track_id", "num_cells", String.(pop_cols))), names(ttab))
            select!(ttab, keep)
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
function _expand_tracks_to_cells(img::CciaImage, trackdf::DataFrame; cell_cols=String[],
                                centroids::Union{Bool,Symbol}=false)::DataFrame
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
        #
        # `centroids` widens that read to the member cells' COORDINATES, resolved per value_name by the
        # same rule as `_fetch_with_centroids` (never a hardcoded column list — a 2D segmentation has no
        # `centroid_z`). Without it `pop_df(pop_type="track", granularity=:cell, centroids=:physical)`
        # returned a frame with no coordinates at all and `_pop_df_finish` could only warn about it —
        # which is what the track PLOTS need (a gated/clustered track's path is drawn from these).
        lp = label_props(img; value_name=vn)
        want = centroids === false ? unique(vcat("track_id", ccols)) :
               unique(vcat("track_id", ccols, centroid_columns(lp), temporal_columns(lp)))
        co = select_cols(lp, want) |> as_df
        keep = [r isa Number && !isnan(r) && Int(r) > 0 for r in co.track_id]
        co = co[keep, :]; nrow(co) == 0 && continue
        co[!, :track_id] = Int.(co.track_id)
        tid_rows = Dict{Int,Vector{Int}}()
        for (i, t) in enumerate(co.track_id); push!(get!(tid_rows, t, Int[]), i); end
        # requested cols living on the CELL table — plus the coordinate columns when they were asked
        # for, which are per-cell by definition (the whole point of the :cell expansion)
        present = intersect(filter(!=("track_id"), want), names(co))
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
                              pop_type::PopTypeArg="track",
                              cell_measures=String[], categorical=String[], pop_cols=nothing,
                              unique_labels::Bool=true, drop_na::Bool=false,
                              granularity::Symbol=:track,
                              centroids::Union{Bool,Symbol}=false)::DataFrame
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
        _expand_tracks_to_cells(img, trackdf; cell_cols=(pop_cols === nothing ? String[] : pop_cols),
                                centroids=centroids)
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

# Reserved leaf name for the auto-created spatial-aggregate population (Decision 14). Unlike
# `_DERIVED_POPS` (query-time injected), this is a *persisted* filter pop written by
# detectAggregates / aggregatesMeshes (filter on `<popType>.cell.is.aggregate > 0`, resolved lazily
# at read by `pop_df`). The `_` prefix keeps it in the reserved namespace (protected from user
# rename/delete, no name collision) and lets `pop_category` tag it "aggregated" for the picker.
const AGGREGATED_POP_NAME = "_aggregated"

"""True if `name` is in the reserved derived-population namespace (leaf begins with `_`)."""
is_reserved_pop_name(name::AbstractString) = startswith(String(name), DERIVED_POP_PREFIX)

"""
    derived_pop_paths(pop_type) -> Vector{String}

Root-level paths of the derived populations registered for `pop_type` (e.g. `/_tracked` under
`live`). Derived pops are injected at query time rather than stored in the gating map, so callers
that enumerate selectable populations (e.g. the summary-canvas population picker) need this to
surface them. Generic over `_DERIVED_POPS`, so future reserved pops appear automatically.
"""
derived_pop_paths(pop_type::PopTypeArg)::Vector{String} =
    ["/" * name for (name, spec) in _DERIVED_POPS if spec.pop_type == string(pop_type)]

# Cached by the gating sidecar's + the h5ad's mtimes, the same auto-invalidation `pop_df` keys on
# (`_pop_df_mtime`): a saved gate edit or a re-tracked segmentation changes a stamp, anything else
# reuses the answer. Worth caching because the picker asks on EVERY load and the answer costs a full
# gate evaluation per tracked segmentation. Module-level, NOT on the image — `init_object` builds a
# fresh `CciaImage` per request, so an object-held cache (`img._pop_df_cache`) never survives one.
# Written from concurrent request handlers, hence the lock (mirrors `_MOTION_DIMS_CACHE`).
const _TRACKED_PARENTS_CACHE = Dict{String,Set{String}}()
const _TRACKED_PARENTS_LOCK  = ReentrantLock()

"""
    tracked_pop_parents(img; value_name, pop_type = "flow", flush = false) -> Set{String}

The populations whose derived `_tracked` child SAYS SOMETHING the tree does not already say —
`""` standing for the segmentation root (`/_tracked`). A population qualifies when it holds tracks
at all AND no sub-population of it holds exactly the same tracks: where a child's track set is
identical, the child is the population that was tracked and the ancestor's `_tracked` is the same
row a level up.

Generalises the old root-only rule (`has_ungated_tracks`) to every level, because the duplicate it
was written for is not special to the root: tracking gated to `/qc/B` makes `/_tracked` *and*
`/qc/_tracked` copies of `/qc/B/_tracked`. An untracked segmentation qualifies nowhere, so gating a
segmentation no longer offers a `_tracked` under every gate before tracking has run.

Membership is counted in TRACKS (a track belongs to a population if any of its cells do — `pop_df`'s
track rule), because that is the unit the `_tracked` row plots. Cheap exit first: `is_tracked` reads
only the obs column list, so an untracked segmentation costs no gate evaluation.

Cached on the two mtimes it reads (see above); `flush = true` recomputes, for an in-memory edit that
was never written to disk — the same override `pop_df`'s `flush_cache` is.
"""
function tracked_pop_parents(img::CciaImage; value_name::Union{AbstractString,Nothing}=nothing,
                             pop_type::PopTypeArg="flow", flush::Bool=false)::Set{String}
    vn = resolve_value_name(img, value_name)
    gp = gating_path(img._dir, vn; pop_type=pop_type)
    lp = img_label_props_path(img, vn)
    key = string(pop_type, "|", gp, "@", _pop_df_mtime(gp), "|", lp, "@", _pop_df_mtime(lp))
    lock(_TRACKED_PARENTS_LOCK) do
        (!flush && haskey(_TRACKED_PARENTS_CACHE, key)) && return _TRACKED_PARENTS_CACHE[key]
        _TRACKED_PARENTS_CACHE[key] = _tracked_pop_parents(img, vn, pop_type)
    end
end

function _tracked_pop_parents(img::CciaImage, vn::AbstractString,
                              pop_type::PopTypeArg)::Set{String}
    out = Set{String}()
    is_tracked(img; value_name=vn) || return out
    cell = label_props(img; value_name=vn) |> lp -> select_cols(lp, ["track_id"]) |> as_df
    "track_id" in names(cell) || return out
    tid = Dict{Int,Int}()                              # cell label → track_id, tracked cells only
    for i in eachindex(cell[!, "label"])
        t = cell[i, "track_id"]
        (t isa Real && isfinite(t) && t > 0) && (tid[Int(cell[i, "label"])] = Int(t))
    end
    isempty(tid) && return out
    # No stored gates (or an unreadable map) → the root is the only population there is.
    m = try; load_pop_map(img; value_name=vn, pop_type=pop_type); catch; nothing; end
    (m === nothing || isempty(pop_paths(m))) && (push!(out, ""); return out)
    try
        recompute!(m, cols -> (label_props(img; value_name=vn) |>
                               lp -> select_cols(lp, cols) |> as_df))
    catch
        push!(out, ""); return out                     # can't evaluate the gates → hide nothing else
    end
    tracks = Dict{String,Set{Int}}("" => Set(values(tid)))
    for p in pop_paths(m)
        tracks[p] = Set(tid[l] for l in cells_in_pop(m, p) if haskey(tid, l))
    end
    for (p, ts) in tracks
        isempty(ts) && continue
        # A child's set can only be a subset of its parent's, so comparing DIRECT children is enough:
        # an equal grandchild forces the child to be equal too.
        kids = p == "" ? [c for c in pop_paths(m) if m.pops[c].parent == ROOT] : direct_children(m, p)
        any(c -> get(tracks, c, Set{Int}()) == ts, kids) && continue
        push!(out, p)
    end
    out
end

