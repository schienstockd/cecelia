# ── Gating API ────────────────────────────────────────────────────────────────
#
# HTTP routes for the population manager + gating engine (docs/POPULATION.md, docs/API.md).
# Synchronous, in-process — interactive latency, no task pool. Mutations persist the
# per-segmentation sidecar gating/{value_name}.json and broadcast `gating:popmap`.
# Plot data is served as raw Float32 (already transformed); membership as label IDs.

# ── helpers ──────────────────────────────────────────────────────────────────

_gerr(status, msg) = (status, JSON3.write((; error = msg)))

# ── Napari cell-selection registry (linked brushing) ──────────────────────────
# A spatial selection drawn in napari is mirrored onto the flow plots as a transient
# population ("see those XY cells in channel space"). The selection is ephemeral server
# state keyed by (task_dir, value_name) — never persisted (docs/POPULATION.md). It is
# injected as a transient `Population` (explicit-label membership) into every map served
# or broadcast, so it reuses all existing plotdata/stats/highlight machinery unchanged.
const NAPARI_SEL_PATH   = "/" * "Napari selection"
const NAPARI_SEL_NAME   = "Napari selection"
const NAPARI_SEL_COLOUR = "#22d3ee"                     # cyan, distinct from gate pops
const _napari_sel       = Dict{Tuple{String,String},Vector{Int}}()
const _napari_sel_lock  = ReentrantLock()

_set_napari_selection!(task_dir::AbstractString, vn::AbstractString, labels::Vector{Int}) =
    lock(_napari_sel_lock) do
        isempty(labels) ? delete!(_napari_sel, (String(task_dir), String(vn))) :
                           (_napari_sel[(String(task_dir), String(vn))] = labels)
    end
_get_napari_selection(task_dir::AbstractString, vn::AbstractString) =
    lock(_napari_sel_lock) do; get(_napari_sel, (String(task_dir), String(vn)), nothing); end

# add the transient napari-selection pop to a freshly-loaded map (no-op when no selection)
function _inject_napari_pop!(m::PopulationMap, img::CciaImage)
    labs = _get_napari_selection(img._dir, m.value_name)
    (labs === nothing || isempty(labs)) && return m
    has_pop(m, NAPARI_SEL_PATH) && return m
    add_pop!(m, NAPARI_SEL_NAME; parent = ROOT, colour = NAPARI_SEL_COLOUR,
             explicit_labels = labs, transient = true)
    m
end

# resolve a CciaImage from project/image uids; returns (img, nothing) or (nothing, errtuple)
function _gating_image(project_uid::AbstractString, image_uid::AbstractString)
    isempty(project_uid) && return (nothing, _gerr(400, "projectUid required"))
    isempty(image_uid)   && return (nothing, _gerr(400, "imageUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return (nothing, _gerr(404, "Project not found: $project_uid"))
    isfile(state_file(joinpath(projects_dir(), project_uid), image_uid)) ||
        return (nothing, _gerr(404, "Image not found: $image_uid"))
    obj = init_object(project_uid, image_uid)
    obj isa CciaImage || return (nothing, _gerr(404, "Not an image: $image_uid"))
    (obj, nothing)
end

# active label_props value name. label_props is a PLAIN map {value_name => file} with no
# "_active" pointer (unlike filepath/labels), so versioned_active would wrongly yield the
# literal "default". Prefer an explicit _active if present, else the first real key.
function _active_vn(img::CciaImage)::String
    isempty(img.label_props) && return "default"
    act = get(img.label_props, VERSIONED_ACTIVE_KEY, nothing)
    (act !== nothing && haskey(img.label_props, act)) && return String(act)
    String(first(versioned_keys(img.label_props)))
end

# resolve a requested value_name to a real label_props key (else the active one) — the
# client may send a stale/blank value_name (e.g. "default") that this image doesn't have.
function _resolve_vn(img::CciaImage, requested::AbstractString)::String
    (!isempty(requested) && haskey(img.label_props, requested)) ? String(requested) : _active_vn(img)
end

# true when the image actually has a labelProps table (i.e. it has been segmented/measured). An
# image that hasn't been segmented yet has none — `versioned_keys` is empty — so gating/membership
# endpoints that read the cell table must degrade gracefully rather than let `label_props` throw a
# 500. (Once segmented — static images included — labelProps exist and the normal path applies.)
_has_label_props(img::CciaImage)::Bool = !isempty(versioned_keys(img.label_props))

# pick the channel-name version whose length matches the intensity-column count
# (handles AF-corrected images with extra channels); fall back to the active version.
function _matching_channel_version(versions::AbstractDict, n_channels::Int)::String
    for (v, names) in versions
        names !== nothing && length(names) == n_channels && return v
    end
    isempty(versions) ? "" : VERSIONED_DEFAULT_VAL
end

# fetch label + cols for a value_name (the recompute/pop_df column provider).
# Uses the label_props chain idiom (docs/DATAMODEL.md) — read left-to-right.
_fetch(img, vn) = cols -> (label_props(img; value_name = vn) |> lp -> select_cols(lp, cols) |> as_df)

# track-table motility (var) columns — gateable per-track axes that need no per-cell aggregation.
_track_motility_cols(img, vn) = (p = img_track_props_path(img, vn);
    isfile(p) ? col_names(label_props(p); data_type = :vars) : String[])

# every column the per-track table provides DIRECTLY (no cell→track aggregation): motility (vars)
# + track-table obs (lineage, and `clusters.{suffix}` written by clustTracks). Passed to
# `track_cell_measures` as the free set so a `trackclust` filter on `clusters.{suffix}` isn't
# mistaken for a cell measure to aggregate.
_track_free_cols(img, vn) = (p = img_track_props_path(img, vn);
    isfile(p) ? vcat(col_names(label_props(p); data_type = :vars),
                     col_names(label_props(p); data_type = :obs)) : String[])

# pop_types whose membership is evaluated over the per-track table (one point per track):
# `track` (hand-drawn per-track gates) and `trackclust` (a `clusters.{suffix}` filter).
_track_grained(pop_type) = String(pop_type) in ("track", "trackclust")

# track data source: ONE row per track (`track_props`, label == track_id). The requested columns
# (gate channels / plot axes / filter measure) drive which cell measures get aggregated
# (`track_cell_measures`); motility + track-table obs come free. The track analogue of `_fetch`.
_track_fetch(img, vn) = cols -> track_props(img; value_name = vn,
    cell_measures = track_cell_measures(cols, _track_free_cols(img, vn)))

# load + recompute a map (membership ready). For `flow`/`live` the data source is the cell table
# and the transient napari-selection pop is injected so it is queryable like any population; for
# `track` the source is the per-track table and the napari selection (cell labels, not track_ids)
# does not apply, so it is not injected.
function _live_map(img, vn, pop_type)
    m = load_pop_map(img; value_name = vn, pop_type = pop_type)
    is_track = _track_grained(pop_type)
    is_track || _inject_napari_pop!(m, img)
    recompute!(m, is_track ? _track_fetch(img, vn) : _fetch(img, vn))
    m
end

# build an AxisTransform from query params, prefix "x"/"y" (e.g. xt=logicle&xT=262144)
function _axis_transform(q::AbstractDict, p::AbstractString)
    kind = lowercase(get(q, p * "t", "linear"))
    num(key, d) = parse(Float64, get(q, p * key, string(d)))
    kind == "log"     ? LogTransform(floor = num("floor", 1.0)) :
    kind == "asinh"   ? AsinhTransform(cofactor = num("cof", 150.0)) :
    kind == "logicle" ? LogicleTransform(T = num("T", 262144.0), W = num("W", 0.5),
                                         M = num("M", 4.5), A = num("A", 0.0)) :
    LinearTransform()
end

_fmt_tick(v) = abs(v) >= 1000 || (v != 0 && abs(v) < 0.01) ?
               string(round(v; sigdigits = 3)) : string(round(v; digits = 2))

# evenly-spaced ticks in transformed space, labelled with the raw (inverse) value
function _axis_ticks(t::AxisTransform, rmin::Real, rmax::Real; n::Int = 6)
    tmin = apply_transform(t, float(rmin)); tmax = apply_transform(t, float(rmax))
    [begin
        pos = tmin + (tmax - tmin) * (i - 1) / (n - 1)
        Dict("pos" => pos, "label" => _fmt_tick(invert_transform(t, pos)))
     end for i in 1:n]
end

# µm/px to apply to ONE plot axis so the DISPLAYED values are in the same unit as the GATES.
# Returns 1.0 — no conversion — for a non-spatial axis, a legacy px-stamped map, or an uncalibrated
# image. Deliberately the SAME three conditions `recompute!` scales on (docs/todo/SPATIAL_GATE_UNITS_
# PLAN.md decision 3), so the dots, the ticks and the gate outlines cannot end up in different units.
# `centroid_t` is not spatial (`is_spatial_axis`) and stays a frame index.
function _axis_scale(img, vn, pop_type, col)::Float64
    is_spatial_axis(String(col)) || return 1.0
    m = load_pop_map(img; value_name = vn, pop_type = pop_type)
    (m.spatial_unit == SPATIAL_UNIT_UM && m.physical_sizes !== nothing) || return 1.0
    physical_size_for_axis(m.physical_sizes, axis_of(String(col)))
end

# The unit a plot axis is displayed in — served to the client so it can label the axis (and so the
# label can never claim µm while the numbers are pixels). "" for a non-spatial axis (an intensity or
# morphology column has no length unit to state).
_axis_unit(img, vn, pop_type, col)::String =
    !is_spatial_axis(String(col)) ? "" :
    (_axis_scale(img, vn, pop_type, col) == 1.0 ? SPATIAL_UNIT_PX : "µm")

# read the raw x/y vectors, then put SPATIAL axes into the gates' unit. One wrapper over the stored-value
# read so every consumer — the point cloud, the whole-dataset extents that drive the ticks, density —
# gets the same unit from one place; the plotdata handler reads both through this.
_plot_xy_raw(img, vn, pop_type, x, y, pop) = _xy_pair(_plot_cols_raw(img, vn, pop_type, [x, y], pop))

# x/y are all-or-nothing: ONE of them missing means there is nothing to plot, and returning a full
# vector beside an empty one would index past the end of the short one (a 500) in every consumer.
_xy_pair(v) = (isempty(v[1]) || isempty(v[2])) ? (Float64[], Float64[]) : (v[1], v[2])

# The same read for ANY number of plot columns — x/y, plus the optional "colour by" measure z (a third
# property painted onto the 2D plot, FlowJo's colour-by-parameter). ONE read for all of them, because
# `z[i]` has to be the SAME CELL as `(x[i], y[i])`: a second, separate read for z would align only by
# luck (any difference in the population filter or row order silently mis-colours every dot).
function _plot_cols_raw(img, vn, pop_type, cols, pop)
    vs = _plot_cols_stored(img, vn, pop_type, cols, pop)
    [(s = _axis_scale(img, vn, pop_type, c); s == 1.0 ? v : v .* s) for (c, v) in zip(cols, vs)]
end

# read x/y (optionally subset to a population) → transformed Float32 vectors.
# Chain idiom (docs/DATAMODEL.md): select the two channels, push the population's label
# filter into the reader (filter_rows), then materialise once.
# read the STORED columns for the scatter (one row per cell, or per track for pop_type="track"),
# optionally subset to a population, before transform. Values as they are on disk — centroids in
# pixels; `_plot_cols_raw` is the wrapper that converts spatial axes. Returns one vector per REQUESTED
# column (duplicates allowed — the same measure on two axes reads once); a column this table doesn't
# have comes back EMPTY, so the CALLER decides what missing means (see `_xy_pair` and `_plot_xyz`).
function _plot_cols_stored(img, vn, pop_type, cols, pop)
    cols = String.(cols)
    empty = [Float64[] for _ in cols]
    want = unique(cols)                                  # `select_cols` takes each column once
    if _track_grained(pop_type)
        # per-track scatter: one point per track from `track_props` (label == track_id)
        tp = track_props(img; value_name = vn,
                         cell_measures = track_cell_measures(want, _track_free_cols(img, vn)))
        any(c -> c in names(tp), want) || return empty
        if !is_root(pop)
            m = _live_map(img, vn, pop_type)
            has_pop(m, pop) || return empty
            keep = Set(cells_in_pop(m, pop))                 # gated track_ids
            tp = tp[[t in keep for t in tp.label], :]
        end
        return [c in names(tp) ? Float64.(tp[!, c]) : Float64[] for c in cols]
    end
    # cell scatter — chain idiom: select the columns, push the population's label filter into
    # the reader (filter_rows), then materialise once.
    lp = label_props(img; value_name = vn) |> select_cols(want)
    if !is_root(pop)
        m = _live_map(img, vn, pop_type)
        # the pop may have vanished since the client last rendered (e.g. a plot/highlight still
        # pointing at a napari selection that has since been cleared) — return empty data rather
        # than letting cells_in_pop throw a 500 ("pop_membership: not found: /Napari selection").
        has_pop(m, pop) || return empty
        filter_rows(lp, cells_in_pop(m, pop); by = :label)
    end
    df = as_df(lp)
    # a requested column may not exist on the cell table (e.g. a stale panel pointing at track columns
    # like `live.track.*` while popType=flow) — `select_cols` drops unknown columns, so guard here and
    # return it EMPTY rather than throwing a 500. Per column, not all-or-nothing: an unusable COLOUR-BY
    # measure must not blank the x/y cloud it was only supposed to tint.
    [c in names(df) ? Float64.(df[!, c]) : Float64[] for c in cols]
end

# transformed Float32 vectors for plotdata/density/plotmeta
function _plot_xy(img, vn, pop_type, x, y, pop, xt, yt)
    xv, yv = _plot_xy_raw(img, vn, pop_type, x, y, pop)
    Float32.(apply_transform(xt, xv)), Float32.(apply_transform(yt, yv))
end

# x/y PLUS the colour-by measure, from the one aligned read (see `_plot_cols_raw`)
function _plot_xyz(img, vn, pop_type, x, y, z, pop, xt, yt, zt)
    v = _plot_cols_raw(img, vn, pop_type, [x, y, z], pop)
    xv, yv = _xy_pair(v)
    # an unreadable colour measure (not a column of THIS table) is "no value for any cell" — NaN, which
    # the client paints in the dim ink — not a short vector, and never a reason to drop the dots.
    zv = length(v[3]) == length(xv) ? Float32.(apply_transform(zt, v[3])) : fill(NaN32, length(xv))
    (Float32.(apply_transform(xt, xv)), Float32.(apply_transform(yt, yv)), zv)
end

# broadcast the updated tree to all clients
function _broadcast_popmap(project_uid, image_uid, vn, pop_type, m::PopulationMap)
    broadcast_ws(Dict{String,Any}(
        "type" => "gating:popmap", "projectUid" => project_uid, "imageUid" => image_uid,
        "valueName" => vn, "popType" => pop_type, "tree" => to_tree(m),
        # ride along with the tree so the undo/redo buttons settle in the same frame the edit lands
        # in — for THIS document, so a copy's target images report their own history, not the
        # source's (see the undo/redo block below)
        "canUndo" => _can_undo(project_uid, image_uid, vn, pop_type),
        "canRedo" => _can_redo(project_uid, image_uid, vn, pop_type)))
end

# Serialise gating pop CRUD. Each handler does load_pop_map → mutate → save_pop_map!; under
# `-t auto` two concurrent edits to the same map (rapid clicks / two tabs) would otherwise both
# load the same tree and the second save would clobber the first (lost update). One process-wide
# ReentrantLock held across the whole load→save is enough — gating edits are user-paced and cheap,
# so contention across images/value_names is negligible. (save_pop_map! also writes atomically.)
const _POPMAP_LOCK = ReentrantLock()
_with_popmap_lock(f) = lock(f, _POPMAP_LOCK)

# ── Undo / redo for HAND-DRAWN gating ─────────────────────────────────────────
#
# The whole document is one serialisable value: every mutation here is load → mutate →
# `save_pop_map!` of the ENTIRE tree, so "the state before this edit" is just `to_tree` of what is
# still on disk when the handler is about to save. That is what makes an undo stack a ring of
# snapshots rather than a set of hand-written inverse operations — there is no diff protocol to
# invert, and nothing to keep in sync as new mutations are added.
#
# Scope is `flow` + `track` (`is_gating_pop_type`): the hand-drawn pop types, where a wrong drag
# destroys work you cannot get back by re-ticking a box. `clust`/`trackclust`/`region` are filter
# pops whose edit IS the tick, and they mirror set-wide through the client's `mirrorUids` — one
# user action, N image sidecars — so undoing them is a different problem and deliberately not this
# one.
#
# In-process and session-scoped on purpose: history dies with the backend rather than growing a
# user-facing sidecar with an edit log nobody asked to keep.
const _GATING_HISTORY_LIMIT = 50
# One entry = the state to restore, as (image uid → tree). A vector because an entry may span
# images; today every entry holds exactly one, and `api_gating_copy` is the case that would not.
const _GatingSnapshot = Vector{Tuple{String,Dict{String,Any}}}
const _GATING_HISTORY = Dict{String,NamedTuple{(:undo, :redo),Tuple{Vector{_GatingSnapshot},Vector{_GatingSnapshot}}}}()

_history_key(proj, image_uid, vn, pop_type) = join((proj, image_uid, vn, pop_type), '\u0000')
_history_for(key) = get!(() -> (undo = _GatingSnapshot[], redo = _GatingSnapshot[]), _GATING_HISTORY, key)

# The tree as it would be READ BACK from disk. `include_transient=false` keeps the napari selection
# out of history — it is ephemeral server state that is re-injected on every read, and restoring a
# stale one would resurrect a selection the user has since cleared.
_gating_snapshot(img::CciaImage, vn, pop_type)::Dict{String,Any} =
    to_tree(load_pop_map(img; value_name = vn, pop_type = pop_type); include_transient = false)

# Record the PRE-edit state. Called from `_persist_and_broadcast!` — the one choke point every
# mutating handler already goes through, so a new mutation cannot forget to be undoable. Runs under
# `_POPMAP_LOCK` (the handler holds it across load→save), so what is on disk here is exactly what
# this edit is about to replace. A new edit invalidates the redo branch, as everywhere else.
function _history_record!(proj, image_uid, vn, pop_type, img::CciaImage)
    is_gating_pop_type(pop_type) || return
    h = _history_for(_history_key(proj, image_uid, vn, pop_type))
    push!(h.undo, [(String(image_uid), _gating_snapshot(img, vn, pop_type))])
    length(h.undo) > _GATING_HISTORY_LIMIT && popfirst!(h.undo)
    empty!(h.redo)
    nothing
end

_can_undo(proj, image_uid, vn, pop_type) = is_gating_pop_type(pop_type) &&
    !isempty(_history_for(_history_key(proj, image_uid, vn, pop_type)).undo)
_can_redo(proj, image_uid, vn, pop_type) = is_gating_pop_type(pop_type) &&
    !isempty(_history_for(_history_key(proj, image_uid, vn, pop_type)).redo)

# persist a mutated map then broadcast — re-injecting the transient napari pop AFTER save
# (so it never hits disk) but BEFORE broadcast (so the client keeps showing it). Returns the whole
# HTTP response body: the tree (incl. the transient pop) plus the undo/redo flags, so the POST that
# made the edit is self-sufficient. The broadcast carries them too, but a client must not need its
# WS to be up to know whether it can undo what it just did.
function _persist_and_broadcast!(m::PopulationMap, img::CciaImage, body, vn, pop_type)
    proj, image_uid = String(body["projectUid"]), String(body["imageUid"])
    _history_record!(proj, image_uid, vn, pop_type, img)
    save_pop_map!(m, img)
    _inject_napari_pop!(m, img)
    _broadcast_popmap(proj, image_uid, vn, pop_type, m)
    (; tree = to_tree(m),
       canUndo = _can_undo(proj, image_uid, vn, pop_type),
       canRedo = _can_redo(proj, image_uid, vn, pop_type))
end

# POST /api/gating/undo · /api/gating/redo — pop one snapshot off the stack, put the CURRENT state
# on the other one, write it back through the same save+broadcast path a normal edit uses (so every
# open client converges the same way). Deliberately does NOT go through `_persist_and_broadcast!`:
# stepping through history must not record itself as a new edit.
api_gating_undo(body_bytes::Vector{UInt8}) = _history_step(body_bytes, :undo)
api_gating_redo(body_bytes::Vector{UInt8}) = _history_step(body_bytes, :redo)

function _history_step(body_bytes::Vector{UInt8}, dir::Symbol)
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    is_gating_pop_type(pt) || return _gerr(400, "Undo is for hand-drawn gating only (flow/track), not $pt")
    proj, image_uid = String(body["projectUid"]), String(body["imageUid"])
    _with_popmap_lock() do
        h = _history_for(_history_key(proj, image_uid, vn, pt))
        from, to = dir === :undo ? (h.undo, h.redo) : (h.redo, h.undo)
        isempty(from) && return _gerr(409, dir === :undo ? "Nothing to undo" : "Nothing to redo")
        entry = pop!(from)
        counter = _GatingSnapshot()
        for (uid, tree) in entry
            timg, _ = _gating_image(proj, uid)
            timg === nothing && continue                      # image gone — skip, don't fail the step
            push!(counter, (uid, _gating_snapshot(timg, vn, pt)))
            save_pop_map!(from_tree(tree), timg)
            m = load_pop_map(timg; value_name = vn, pop_type = pt)
            _inject_napari_pop!(m, timg)
            _broadcast_popmap(proj, uid, vn, pt, m)
        end
        push!(to, counter)
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        _inject_napari_pop!(m, img)
        200, JSON3.write((; tree = to_tree(m),
                            canUndo = !isempty(h.undo), canRedo = !isempty(h.redo)))
    end
end

# Run suffixes available in a table's obs, for the pop_type's own column FAMILY: `clusters.{suffix}`
# for clust/trackclust (clustPops/clustTracks), `regions.{suffix}` for region (clustRegions). The prefix
# is never spelled here — it comes from the single package-side decision
# (`Cecelia._cluster_measure_prefix`, docs/todo/SPATIAL_REGIONS_PLAN.md Decision 5), which is why the
# region page saw an EMPTY run list while `regions.immune` sat in obs: this helper used to hardcode
# "clusters.". Drives the cluster/region pages' page-level run dropdown.
_cluster_suffixes(obscols, pop_type = "clust") =
    let prefix = Cecelia._cluster_measure_prefix(pop_type)
        String[c[ncodeunits(prefix)+1:end] for c in obscols if startswith(c, prefix)]
    end

# per-suffix run manifest ({props}.clustfeatures.json; clustPops/clustTracks/clustRegions write it).
# `features` → the heatmap offers exactly those columns; `partOf` → the uIDs clustered together (mirrors
# old R `valuePartOf`), so cluster/region pops are only defined for images in the run; `labels` →
# optional column → display-label map (region composition columns are machine names). Reads go through
# the package's shared sidecar reader, which owns the family-qualified + legacy key layouts — the API
# never indexes that JSON itself.
_clust_features(props_path::AbstractString, suffixes, family) = Dict{String,Any}(
    s => Cecelia._clustfeatures_features(props_path, s; family = family) for s in suffixes)
# JSON3 hands back Symbol OR String keys depending on how the object was parsed, so every field read
# goes through this. NEVER wrap it in `something(..., nothing)`: `something()` with all-nothing
# arguments THROWS `ArgumentError("No value arguments present")`, which is exactly how
# /api/gating/channels?popType=region started 500ing on any sidecar entry written before the `labels`
# field existed — i.e. every image that hadn't been re-run.
_entry_field(e, key) = e === nothing ? nothing :
    let v = get(e, String(key), nothing)
        v === nothing ? get(e, Symbol(key), nothing) : v
    end

function _clust_members(props_path::AbstractString, suffixes, family)
    out = Dict{String,Any}()
    for s in suffixes
        e = Cecelia._clustfeatures_entry(props_path, s; family = family)
        po = _entry_field(e, "partOf")
        out[s] = po isa AbstractVector ? String[string(x) for x in po] : String[]
    end
    out
end
function _clust_feature_labels(props_path::AbstractString, suffixes, family)
    out = Dict{String,Any}()
    for s in suffixes
        l = _entry_field(Cecelia._clustfeatures_entry(props_path, s; family = family), "labels")
        l isa AbstractDict && !isempty(l) &&
            (out[s] = Dict{String,Any}(String(k) => string(v) for (k, v) in l))
    end
    out
end

# distinct cluster IDs per suffix — the universe the pop-manager offers as tickable clusters. Clustering
# is SET-scope, so a single image's `clusters.{suffix}` column carries only the subset of the shared
# 0..k-1 codes present in ITS objects; scanning one image under-reports (e.g. 2 of 4). Pool across the
# run's MEMBER images (`partOf`, from _clust_members) so the universe matches the pooled UMAP/heatmap.
# A suffix with no recorded members (pre-partOf run) falls back to the primary image's own column.
function _cluster_ids(project_uid::AbstractString, primary_path::AbstractString, vn::AbstractString,
                      suffixes::Vector{String}, members::AbstractDict, track::Bool,
                      pop_type::AbstractString = "clust")
    pathcache = Dict{String,Union{String,Nothing}}()   # init_object per uID isn't free; a member may back several suffixes
    member_path(u) = get!(pathcache, u) do
        mi = try init_object(project_uid, u) catch; nothing end
        mi isa CciaImage || return nothing
        p = track ? img_track_props_path(mi, vn) : img_label_props_path(mi, vn)
        isfile(p) ? p : nothing
    end
    out = Dict{String,Any}()
    prefix = Cecelia._cluster_measure_prefix(pop_type)   # "clusters." | "regions." — never hardcoded
    for s in suffixes
        col = "$(prefix)$(s)"
        paths = String[]
        for u in String.(get(members, s, String[]))
            p = member_path(u); p === nothing || push!(paths, p)
        end
        isempty(paths) && push!(paths, primary_path)
        ids = Set{Int}()
        for p in paths
            df = try label_props(p) |> select_cols([col]) |> as_df catch; continue end
            hasproperty(df, Symbol(col)) || continue
            for x in df[!, col]
                (isfinite(x) && x >= 0) && push!(ids, Int(round(x)))
            end
        end
        out[s] = sort!(collect(ids))
    end
    out
end

# ── GET /api/gating/channels — gateable columns + channel display names ───────
function api_gating_channels(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    # image not segmented yet: no labelProps → nothing is gateable. Return empty rather than 500ing
    # when the gating UI probes channels for it (label_props would throw "No labelProps …"). After
    # segmentation the image has labelProps and the normal path below runs.
    if !_has_label_props(img)
        return 200, JSON3.write((;
            columns = String[], channels = String[], channelNames = String[],
            channelNameVersions = Dict{String,Any}(), obsColumns = String[], temporalColumns = String[],
            cellMeasures = String[], trackAggregates = String[], clusterSuffixes = String[],
            clusterFeatures = Dict{String,Any}(), clusterMembers = Dict{String,Any}(),
            valueNames = String[], valueName = vn, popType = get(q, "popType", "flow")))
    end
    # track gating: the gateable axes are the per-track properties — motility (the track table's
    # var columns, directly gateable) plus on-read aggregates of any cell measure. We return the
    # motility columns + the aggregatable cell measures + the aggregate suffixes so the client can
    # offer e.g. "mean CD4 per track" → axis `mean_intensity_0.mean` (track_cell_measures inverts it).
    if get(q, "popType", "flow") in ("track", "trackclust")
        motility = _track_motility_cols(img, vn)
        lpc = label_props(img; value_name = vn)
        cellmeas = col_names(lpc; data_type = :vars)         # aggregatable cell vars
        cellobs  = col_names(lpc; data_type = :obs)          # aggregatable cell obs (HMM state/transitions)
        chans = channel_columns(lpc)
        versions = Dict{String,Any}(
            v => channel_names(img; value_name = v) for v in versioned_keys(img.im_channel_names))
        display = get(versions, _matching_channel_version(versions, length(chans)), String[])
        tpath = img_track_props_path(img, vn)
        tobs  = isfile(tpath) ? col_names(label_props(tpath); data_type = :obs) : String[]
        pt    = get(q, "popType", "track")
        tsfx  = _cluster_suffixes(tobs, pt)                      # trackclust runs in the track table
        tfam  = Cecelia._cluster_measure_family(pt)
        # WHICH of this image's segmentations actually have tracks. Every track surface needs this and
        # none of them could get it: a track view defaulting to "default" or to the ACTIVE segmentation
        # picks an UNTRACKED one on any image where tracking ran on a named label set — on the
        # reference image (zolIMa/1/fXgbTl) `default` and the active `three` are both untracked while
        # `memTom` holds 374 tracks, so the correction worklist reported "nothing to review" for an
        # image with 31 candidates. `is_tracked` reads only the obs column list, so this is cheap.
        tracked = String[v for v in versioned_keys(img.label_props) if is_tracked(img; value_name = v)]
        return 200, JSON3.write((;
            columns = motility,                                  # whole-track motility (directly gateable)
            trackedValueNames = tracked,                          # the ones a track view may default to
            cellMeasures = cellmeas,                             # cell vars → per-track numeric aggregates
            cellObsMeasures = cellobs,                           # cell obs → per-track aggregates (HMM, …)
            channelNames = display === nothing ? String[] : display,  # relabel intensity aggregates
            trackAggregates = ["mean", "median", "sum", "qUp", "qLow", "sd"],  # see track_props
            clusterSuffixes = tsfx,
            clusterFeatures = _clust_features(tpath, tsfx, tfam),
            clusterMembers  = _clust_members(tpath, tsfx, tfam),  # uIDs clustered together (partOf)
            clusterFeatureLabels = _clust_feature_labels(tpath, tsfx, tfam),
            clusterIds      = isfile(tpath) ? _cluster_ids(get(q, "projectUid", ""), tpath, vn,
                                tsfx, _clust_members(tpath, tsfx, tfam), true, pt) : Dict{String,Any}(),
            valueNames = versioned_keys(img.label_props),
            valueName = vn,
            popType = pt,
        ))
    end
    lp = label_props(img; value_name = vn)
    cols = col_names(lp; data_type = :vars)        # all gateable feature columns
    chans = channel_columns(lp)                     # intensity columns specifically
    # Channel display names are VERSIONED (e.g. AF correction adds extra channels), and the
    # label value_name doesn't map 1:1 to a channel-name version. Pick the version whose
    # length matches the number of intensity columns; expose all versions for the client.
    versions = Dict{String,Any}(
        v => channel_names(img; value_name = v) for v in versioned_keys(img.im_channel_names))
    display = get(versions, _matching_channel_version(versions, length(chans)), String[])
    # TRACK-level cluster columns (clusters.* in `{vn}__tracks.h5ad`, written by clustTracks). These
    # aren't in the cell obs, but the napari colour-by broadcasts them to cells via track_id so you can
    # colour tracks by their cluster/population (see docs/NAPARI.md). Offered alongside cell obs columns.
    tpath = img_track_props_path(img, vn)
    trackObs = isfile(tpath) ? col_names(label_props(tpath); data_type = :obs) : String[]
    trackColourColumns = String[c for c in trackObs if startswith(c, "clusters.")]
    # cell-table run family for THIS pop_type: `clusters.*` for clust, `regions.*` for region. Computed
    # once — the obs scan, the sidecar lookups and the ID universe must all agree on the family.
    cpt   = get(q, "popType", "flow")
    cobs  = col_names(lp; data_type = :obs)
    csfx  = _cluster_suffixes(cobs, cpt)
    cfam  = Cecelia._cluster_measure_family(cpt)
    cpath = img_label_props_path(img, vn)
    cmem  = _clust_members(cpath, csfx, cfam)
    200, JSON3.write((;
        columns = cols,
        channels = chans,
        channelNames = display === nothing ? String[] : display,
        channelNameVersions = versions,
        obsColumns = cobs,              # per-cell obs measures (live.cell.*, hmm.state, …) for labelPropsColsSelection
        trackColourColumns = trackColourColumns,         # track-level clusters.* — colour-by broadcasts to cells
        # spatial/temporal centroid axes (obsm) — offered as gating scatter axes you can visualise AND
        # gate on (like the old R flow frame). Read/gate-eval already materialise them via as_df.
        spatialColumns = centroid_columns(lp; order = [:x, :y, :z]),   # centroid_x/_y[/_z], present axes
        temporalColumns = temporal_columns(lp),          # ["centroid_t"] — groupable + a gateable time axis
        clusterSuffixes = csfx,         # clust runs (`clusters.*`) / region runs (`regions.*`) in the cell table
        clusterFeatures = _clust_features(cpath, csfx, cfam),
        clusterMembers  = cmem,         # uIDs clustered together (partOf)
        clusterFeatureLabels = _clust_feature_labels(cpath, csfx, cfam),
        clusterIds      = _cluster_ids(get(q, "projectUid", ""), cpath, vn, csfx, cmem, false, cpt),
        valueNames = versioned_keys(img.label_props),
        valueName = vn,                 # the server-resolved value_name these columns belong to
        popType = cpt,
    ))
end

# ── GET /api/gating/popmap ────────────────────────────────────────────────────
function api_gating_popmap(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    pt = get(q, "popType", "flow")
    m = load_pop_map(img; value_name = vn, pop_type = pt)
    _inject_napari_pop!(m, img)
    proj, image_uid = get(q, "projectUid", ""), get(q, "imageUid", "")
    200, JSON3.write((; tree = to_tree(m),
                        canUndo = _can_undo(proj, image_uid, vn, pt),
                        canRedo = _can_redo(proj, image_uid, vn, pt)))
end

# ── GET /api/gating/stats?...&pop=/cd4 ────────────────────────────────────────
function api_gating_stats(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", "")); pop = get(q, "pop", ROOT)
    m = _live_map(img, vn, get(q, "popType", "flow"))
    (is_root(pop) || has_pop(m, pop)) || return _gerr(404, "Population not found: $pop")
    s = pop_stats(m, pop)
    200, JSON3.write((; count = s.count, parentCount = s.parent_count, pctParent = s.pct_parent))
end

# ── GET /api/gating/membership?...&pops=/a,/b[&binary=1] ──────────────────────
function api_gating_membership(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    pops = split(get(q, "pops", ""), ","; keepempty = false)
    isempty(pops) && return _gerr(400, "pops required")
    m = _live_map(img, vn, get(q, "popType", "flow"))
    for p in pops
        (is_root(p) || has_pop(m, p)) || return _gerr(404, "Population not found: $p")
    end
    if get(q, "binary", "") == "1"
        length(pops) == 1 || return _gerr(400, "binary=1 requires exactly one pop")
        return 200, collect(reinterpret(UInt8, Int32.(cells_in_pop(m, pops[1]))))
    end
    200, JSON3.write((; membership = Dict(String(p) => cells_in_pop(m, p) for p in pops)))
end

# Extrema over FINITE values only. Object/morphology measurements (volume, convex_hull_area,
# surface_to_volume, …) carry NaN/Inf for degenerate or border objects; plain `extrema` would
# propagate a single NaN into the extent, and JSON3 refuses to serialise NaN (→ 500, so the plot
# never renders). Falls back when the column is empty or all-non-finite.
function _finite_extrema(v, fallback::Tuple{Float64,Float64} = (0.0, 1.0))::Tuple{Float64,Float64}
    lo = Inf; hi = -Inf
    for x in v
        xf = float(x)
        isfinite(xf) || continue
        xf < lo && (lo = xf)
        xf > hi && (hi = xf)
    end
    lo <= hi ? (lo, hi) : fallback
end

# Display-space bounding box (xlo, xhi, ylo, yhi) enclosing a plotmeta `gates` list (projected
# child-gate outlines). Rectangles carry x_min/x_max/y_min/y_max; polygons carry `vertices`
# ([[x,y],…]). All coords are already in the plot's display transform. Inf's when there are no
# gates (nothing to enclose).
function _gates_bbox(gates)
    xlo = Inf; xhi = -Inf; ylo = Inf; yhi = -Inf
    for pj in gates
        if get(pj, "kind", "") == "rectangle"
            xlo = min(xlo, pj["x_min"]); xhi = max(xhi, pj["x_max"])
            ylo = min(ylo, pj["y_min"]); yhi = max(yhi, pj["y_max"])
        else
            for v in get(pj, "vertices", ())
                xlo = min(xlo, v[1]); xhi = max(xhi, v[1])
                ylo = min(ylo, v[2]); yhi = max(yhi, v[2])
            end
        end
    end
    (xlo, xhi, ylo, yhi)
end

# The COLOUR ramp's range: a robust 2–98 percentile of the (transformed) values, not min…max.
#
# Measured on a real 3P spleen dataset (4471 cells, `Bcells-ubiTom` on logicle): p2–p98 covered **28%**
# of the full min…max range, so a full-range ramp spent ~70% of the colour scale on a handful of outlying
# cells and the whole cloud came out one flat orange. The same clip is what a viewer does for contrast
# limits, and it is not hidden: the colour bar is labelled with THESE numbers, and values outside clamp
# to the ends of the ramp (they stay visible, at the extreme colour).
#
# An axis is different and keeps min…max — a gate is drawn against it, so it may not lie about where a
# cell sits. Nothing is gated on a colour.
const RAMP_CLIP = 0.02
function _ramp_range(v::AbstractVector)
    f = Float32[x for x in v if isfinite(x)]
    isempty(f) && return nothing
    n = length(f)
    lo_i = clamp(floor(Int, RAMP_CLIP * (n - 1)) + 1, 1, n)
    hi_i = clamp(ceil(Int, (1 - RAMP_CLIP) * (n - 1)) + 1, 1, n)
    # partialsort! reorders `f` but never changes WHICH values it holds, so the second k-th-smallest
    # query is still correct — two O(n) selections instead of an O(n log n) sort of a million cells.
    lo = partialsort!(f, lo_i)
    hi = partialsort!(f, hi_i)
    hi > lo && return (Float64(lo), Float64(hi))
    e = _finite_extrema(f)                      # degenerate clip (near-constant measure) → full range
    e[2] > e[1] ? e : nothing
end

# Grow a display extent `(lo, hi)` to also enclose `[glo, ghi]`, plus a `margin` fraction of the
# resulting span so a gate edge lands inside the axes (grabbable), not flush on the border. No-op
# when the target range is not finite (no gate). Used to autoscale a plot to its child gates so a
# gate drawn beyond the data cloud stays on-screen and adjustable.
function _include_range((lo, hi)::Tuple, glo, ghi; margin::Float64 = 0.05)
    (isfinite(glo) && isfinite(ghi)) || return (float(lo), float(hi))
    span = hi > lo ? hi - lo : 1.0
    m = margin * span
    nlo = glo < lo ? glo - m : float(lo)     # only grow the side a gate actually exceeds
    nhi = ghi > hi ? ghi + m : float(hi)
    (nlo, nhi)
end

# ── GET /api/gating/plotmeta ──────────────────────────────────────────────────
# returns n + mode (scatter|density) + transformed extents + axis ticks
function api_gating_plotmeta(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop_type = get(q, "popType", "flow"); pop = get(q, "pop", ROOT)
    # A track-grained plot (popType track/trackclust) of an untracked segmentation has no data — tell
    # the client to track first (it shows a message) and skip the empty data reads. `tracked` rides on
    # every plotmeta response so the client can distinguish "not tracked" from "genuinely no points".
    if _track_grained(pop_type) && !is_tracked(img; value_name = vn)
        return 200, JSON3.write((; n = 0, mode = "scatter", tracked = false,
            xExtent = [0.0, 1.0], yExtent = [0.0, 1.0], xLabel = x, yLabel = y,
            xUnit = "", yUnit = "",              # same response shape as the real path below
            xTicks = Dict{String,Any}[], yTicks = Dict{String,Any}[],
            usedX = "linear", usedY = "linear", gates = Dict{String,Any}[],
            zExtent = nothing, zTicks = Dict{String,Any}[], zUnit = "", usedZ = "linear"))
    end
    xt0 = _axis_transform(q, "x"); yt0 = _axis_transform(q, "y")
    # "colour by" (optional): a THIRD measure painted onto the 2D plot as the dot colour. It has no
    # axis — only a range for the colour ramp — so all it needs from here is its transform + extent.
    z = get(q, "z", "")
    # raw extents over the WHOLE dataset (root): used both for ticks (labels read in data units, and
    # selecting a population doesn't rescale the axis) AND to decide auto-linearisation — so the
    # transform choice is stable across populations, not re-decided per subset. Track-aware via _raw.
    rv = _plot_cols_raw(img, vn, pop_type, isempty(z) ? [x, y] : [x, y, z], ROOT)
    rxv, ryv = _xy_pair(rv)
    rxext = _finite_extrema(rxv); ryext = _finite_extrema(ryv)
    # A non-linear transform that would collapse a bounded/small-range measure (morphology like
    # solidity ∈ [0,1]) into a sliver is auto-swapped to linear; usedX/usedY tell the client so it can
    # flag the axis. Both plotmeta and plotdata transform with these EFFECTIVE transforms, so the
    # extent and the point cloud always agree. (The client sends its PREFERRED transform; the server
    # decides what's usable — see docs/POPULATION.md.)
    # Opt-in via autoLinear=1 (sent by the single flow plot AND the channel-pairs / gating-strategy
    # montage; other callers keep the requested transform verbatim). usedX/usedY report what was used.
    auto = get(q, "autoLinear", "") == "1"
    xt = auto ? effective_transform(xt0, rxext[1], rxext[2]) : xt0
    yt = auto ? effective_transform(yt0, ryext[1], ryext[2]) : yt0
    # The colour ramp's range is taken over the WHOLE dataset (root), like the ticks: selecting a
    # child population must not re-map the colours, or the same dot changes colour as you walk the
    # tree and two plots of the same measure can't be compared. `nothing` when no colour-by measure
    # (or when it isn't a column on this table — an empty read).
    zt = LinearTransform(); zext = nothing; zticks = Dict{String,Any}[]
    if !isempty(z) && !isempty(rv[3])
        zt0 = _axis_transform(q, "z")
        rzext = _finite_extrema(rv[3])
        zt = auto ? effective_transform(zt0, rzext[1], rzext[2]) : zt0
        # robust range (see `_ramp_range`): the ramp is a contrast setting, not an axis
        e = _ramp_range(Float32.(apply_transform(zt, rv[3])))
        if e !== nothing
            zext = [e[1], e[2]]
            # the legend's labels are RAW values (inverted through the transform) — served from here for
            # the same reason the axis ticks are: the client has no transform math, so it cannot label a
            # logicle-scaled ramp itself. They describe the CLIPPED range, so the bar states what it
            # actually shows. Three is what a thin colour bar fits.
            zticks = _axis_ticks(zt, invert_transform(zt, e[1]), invert_transform(zt, e[2]); n = 3)
        end
    end
    xv, yv = _plot_xy(img, vn, pop_type, x, y, pop, xt, yt)
    n = length(xv)
    density_threshold = parse(Int, get(q, "densityThreshold", "200000"))
    mode = n > density_threshold ? "density" : "scatter"
    xext = _finite_extrema(xv)
    yext = _finite_extrema(yv)
    # x0/y0=1 → "whole dataset" axis: origin at raw 0, upper bound = the FULL dataset max (rxext
    # is computed from all cells, not the pop subset), so selecting a population doesn't rescale
    # the axis (FlowJo behaviour). Autoscale (x0=0) fits the displayed population's extent.
    if get(q, "x0", "") == "1"
        xext = (apply_transform(xt, 0.0), apply_transform(xt, float(rxext[2]))); rxext = (0.0, rxext[2])
    end
    if get(q, "y0", "") == "1"
        yext = (apply_transform(yt, 0.0), apply_transform(yt, float(ryext[2]))); ryext = (0.0, ryext[2])
    end
    # Child gates of the displayed population, re-projected into the EFFECTIVE display transform so
    # their outlines land on the dots even when a gate was drawn under a different transform (the client
    # has no transform math). Colour/path travel so the client renders directly. Membership is untouched.
    m = load_pop_map(img; value_name = vn, pop_type = pop_type)
    gates = Dict{String,Any}[]
    for cpath in direct_children(m, pop)
        p = m.pops[cpath]
        p.gate === nothing && continue
        pj = project_gate(p.gate, x, y, xt, yt)
        pj === nothing && continue
        pj["path"] = cpath; pj["colour"] = p.colour
        push!(gates, pj)
    end
    # Autoscale to the child gates: a gate drawn beyond the data cloud would otherwise fall outside
    # the axes and be un-grabbable. Grow the display extent to enclose the projected outlines, and
    # re-derive the raw tick range from the widened extent so ticks span the whole axis. Only the
    # side a gate actually exceeds moves (the common in-bounds case is untouched).
    gb = _gates_bbox(gates)
    xext2 = _include_range(xext, gb[1], gb[2]); yext2 = _include_range(yext, gb[3], gb[4])
    if xext2 != xext || yext2 != yext
        xext, yext = xext2, yext2
        rxext = (invert_transform(xt, xext[1]), invert_transform(xt, xext[2]))
        ryext = (invert_transform(yt, yext[1]), invert_transform(yt, yext[2]))
    end
    200, JSON3.write((;
        n = n, mode = mode, tracked = true,
        xExtent = [xext[1], xext[2]], yExtent = [yext[1], yext[2]],
        xLabel = x, yLabel = y,
        xUnit = _axis_unit(img, vn, pop_type, x), yUnit = _axis_unit(img, vn, pop_type, y),
        xTicks = _axis_ticks(xt, rxext[1], rxext[2]),
        yTicks = _axis_ticks(yt, ryext[1], ryext[2]),
        usedX = transform_kind(xt), usedY = transform_kind(yt),
        gates = gates,
        # colour-by: the ramp's [lo, hi] in TRANSFORMED space (plotdata sends transformed values), the
        # unit for the legend, and the transform actually used — same auto-linearisation as an axis.
        zExtent = zext, zTicks = zticks,
        zUnit = isempty(z) ? "" : _axis_unit(img, vn, pop_type, z),
        usedZ = transform_kind(zt),
    ))
end

# ── GET /api/gating/plotdata → binary Float32 [x0,y0,x1,y1,...] (transformed) ──
function api_gating_plotdata(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    xt = _axis_transform(q, "x"); yt = _axis_transform(q, "y")
    pop_type = get(q, "popType", "flow"); pop = get(q, "pop", ROOT)
    # `z` (optional) = the colour-by measure: the response becomes TRIPLES [x,y,z,…] instead of pairs,
    # read in the SAME pass as x/y so each z belongs to its own dot (`_plot_cols_raw`). The client
    # switches stride on whether it asked for z, so old callers are untouched.
    z = get(q, "z", "")
    if isempty(z)
        xv, yv = _plot_xy(img, vn, pop_type, x, y, pop, xt, yt)
        n = length(xv)
        buf = Vector{Float32}(undef, 2n)
        @inbounds for i in 1:n
            buf[2i-1] = xv[i]; buf[2i] = yv[i]
        end
        return 200, collect(reinterpret(UInt8, buf))
    end
    xv, yv, zv = _plot_xyz(img, vn, pop_type, x, y, z, pop, xt, yt, _axis_transform(q, "z"))
    n = length(xv)
    buf = Vector{Float32}(undef, 3n)
    @inbounds for i in 1:n
        buf[3i-2] = xv[i]; buf[3i-1] = yv[i]; buf[3i] = zv[i]
    end
    200, collect(reinterpret(UInt8, buf))
end

# ── GET /api/plots/umap → binary Float32 [x, y, clusterCode, popIdx] per point ─
# `popIdx` = index (0-based) into the requested `colourPops` list that this point belongs to (-1 = in
# none / not requested), so the frontend can colour/facet the embedding by the individually-tracked
# populations (docs/todo/UMAP_COLOUR_FACET_PLAN.md). `code` (cluster) is always present too.
# The UMAP scatter for a clustering pop_type: the `obsm['X_umap.{suffix}']` embedding + the
# `clusters.{suffix}` code per point (frontend colours by code). `clust` reads the cell table;
# `trackclust` reads the per-track table (one point per track). Optionally subset to a population's
# membership (`pop`). Cluster codes are small ints → exact in Float32; an unclustered row → -1.
function api_plots_umap(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    pop_type = get(q, "popType", "clust")
    suffix   = get(q, "suffix", "default")
    pop      = get(q, "pop", ROOT)
    # the code column is the pop_type's OWN family (`clusters.{suffix}` | `regions.{suffix}`) — see
    # `_cluster_suffixes`. Hardcoding "clusters." here left every region UMAP point uncoloured.
    umap_key = "X_umap.$suffix"; clust_col = "$(Cecelia._cluster_measure_prefix(pop_type))$(suffix)"
    track    = _track_grained(pop_type)
    # POOL across every segmentation that took part in this clustering run (co-clustered value_names),
    # so the UMAP shows all segments' points in the ONE shared embedding — not just the active one
    # (docs/todo/CLUSTER_POOLING_PLAN.md). An explicit `valueName` restricts to that single segmentation.
    req_vn = get(q, "valueName", "")
    vns = (!isempty(req_vn) && haskey(img.label_props, req_vn)) ? String[String(req_vn)] :
          co_clustered_value_names(img, suffix; granularity = track ? :track : :cell,
                                   family = Cecelia._cluster_measure_family(pop_type))

    # Optional per-point POPULATION membership for colour/facet-by-population: resolve the picked pops
    # through pop_df — the canonical accessor already handles GRAIN (granularity=:track rolls a `live`
    # cell pop up to its track_ids so it aligns with a trackclust embedding), value_name pooling, and
    # derived pops. Each token is "popType~valueNamePrefixedPath" (e.g. "live~B/qc/_tracked"); popIdx =
    # its 0-based index in the ordered list, so a pop keeps one colour across segments. A point → the
    # FIRST picked pop it belongs to, else -1 (input pops are normally disjoint). See
    # docs/todo/UMAP_COLOUR_FACET_PLAN.md.
    colour_toks = String[String(t) for t in split(get(q, "colourPops", ""), ',') if !isempty(t)]
    grain = track ? :track : :cell
    pop_of = Dict{Tuple{String,Int},Int}()              # (value_name, label) → popIdx (0-based)
    for (pi, tok) in enumerate(colour_toks)
        parts = split(tok, '~'; limit = 2)
        pt, path = length(parts) == 2 ? (String(parts[1]), String(parts[2])) : (pop_type, String(parts[1]))
        df = try
            pop_df(img, pt, [path]; granularity = grain, include_obs = false, include_x = false)
        catch
            continue                                    # a bad/foreign pop just contributes no colour
        end
        ("value_name" in names(df) && "label" in names(df)) || continue
        for r in eachrow(df)
            get!(pop_of, (String(r.value_name), Int(r.label)), pi - 1)
        end
    end

    out = Float32[]   # [x, y, code, popIdx] quads, concatenated across the co-clustered segmentations
    for vn in vns
        path = track ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
        isfile(path) || continue
        xy = obsm(label_props(path), umap_key)          # (n_obs, 2), obs order
        size(xy, 1) == 0 && continue
        # cluster codes aligned to obs order (label + the code column, same obs order as obsm)
        cdf = label_props(path) |> select_cols([clust_col]) |> as_df
        # `size(cdf, 1)`, not `nrow` — DataFrames is NOT imported in the api scripts, so the fallback
        # branch used to throw `UndefVarError: nrow`. It fires exactly when the run's code column is
        # absent (an embedding with no codes), which is how it stayed hidden until region runs hit it.
        codes = clust_col in names(cdf) ? cdf[!, clust_col] : fill(missing, size(cdf, 1))
        keep = trues(size(xy, 1))
        if !is_root(pop)
            m = _live_map(img, vn, pop_type)
            has_pop(m, pop) || continue                 # this segment lacks the pop → contributes nothing
            sel  = Set(cells_in_pop(m, pop))
            keep = BitVector(l in sel for l in cdf.label)
        end
        # per-point popIdx from the resolved (value_name, label) → popIdx map (colour/facet-by-population)
        popidx = isempty(pop_of) ? fill(-1, size(xy, 1)) :
                 Int[get(pop_of, (String(vn), Int(l)), -1) for l in cdf.label]
        # drop rows with no embedding — cells/tracks outside the clustered set have NaN UMAP coords
        # (and NaN code); they'd otherwise render as a stray "NaN" cluster.
        @inbounds for i in 1:size(xy, 1)
            (isfinite(xy[i, 1]) && isfinite(xy[i, 2])) || (keep[i] = false)
        end
        idx = findall(keep)
        base = length(out); resize!(out, base + 4 * length(idx))
        @inbounds for (j, i) in enumerate(idx)
            out[base + 4j - 3] = Float32(xy[i, 1]); out[base + 4j - 2] = Float32(xy[i, 2])
            c = codes[i]
            out[base + 4j - 1] = (c isa Number && !ismissing(c)) ? Float32(c) : -1f0
            out[base + 4j]     = Float32(popidx[i])
        end
    end
    isempty(out) && return _gerr(404, "No $umap_key — run clustering with UMAP enabled")
    200, collect(reinterpret(UInt8, out))
end

# ── GET /api/gating/density → binary Float32 grid (bins×bins, row-major) ──────
function api_gating_density(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    bins = parse(Int, get(q, "bins", "256"))
    xt = _axis_transform(q, "x"); yt = _axis_transform(q, "y")
    xv, yv = _plot_xy(img, vn, get(q, "popType", "flow"), x, y, get(q, "pop", ROOT), xt, yt)
    d = density_2d(xv, yv; bins = bins)
    200, collect(reinterpret(UInt8, Float32.(vec(d.counts))))
end

# ── POST mutations (add / set-gate / delete / rename) ─────────────────────────

# parse + resolve common fields from a POST body
function _gating_post(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return (nothing, nothing, nothing, nothing, _gerr(400, "Invalid JSON body"))
    end
    img, err = _gating_image(String(get(body, "projectUid", "")), String(get(body, "imageUid", "")))
    err === nothing || return (nothing, nothing, nothing, body, err)
    vn = _resolve_vn(img, String(get(body, "valueName", "")))
    pt = String(get(body, "popType", "flow"))
    (img, vn, pt, body, nothing)
end

function api_gating_pop_add(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "name") || return _gerr(400, "name required")
    # name-uniqueness guard: reject a name already used by a DIFFERENT pop type in this segmentation, so
    # the mixed-type module picker (which resolves a pop by path alone) is never ambiguous.
    _name = String(body["name"])
    _parent = String(get(body, "parent", ROOT))
    _intended = pop_path(is_root(_parent) ? ROOT : _parent, _name)
    _conflict = pop_name_conflict(img, vn, _intended; pop_type = pt)
    _conflict === nothing || return _gerr(400,
        "A $_conflict population named \"$_name\" already exists in this segmentation — names must be " *
        "unique across population types so a population can be selected unambiguously. Choose another name.")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        gate = haskey(body, "gate") && body["gate"] !== nothing ? gate_from_spec(body["gate"]) : nothing
        flt = get(body, "filter", nothing)
        bl = get(body, "boolean", nothing)      # Decision 16: a set operation over other pops
        try
            add_pop!(m, String(body["name"]); parent = String(get(body, "parent", ROOT)),
                     gate = gate, colour = String(get(body, "colour", "#ffffff")),
                     show = Bool(get(body, "show", true)),
                     filter_measure = flt === nothing ? nothing : get(flt, "measure", nothing),
                     filter_fun     = flt === nothing ? nothing : get(flt, "fun", nothing),
                     filter_values  = flt === nothing ? nothing : get(flt, "values", nothing),
                     filter_default_all = flt === nothing ? false : Bool(get(flt, "default_all", false)),
                     filter_conditions = flt === nothing ? nothing : get(flt, "conditions", nothing),
                     is_track = Bool(get(body, "is_track", false)),
                     boolean_op   = bl === nothing ? nothing : get(bl, "op", nothing),
                     boolean_pops = bl === nothing ? nothing : get(bl, "pops", nothing),
                     boolean_not  = bl === nothing ? nothing : get(bl, "not", nothing))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

function api_gating_pop_set_gate(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "gate")) || return _gerr(400, "path and gate required")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        try
            set_gate!(m, String(body["path"]), gate_from_spec(body["gate"]))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

# `childrenOnly: true` deletes the subtree UNDER `path` and keeps the population itself — the
# "prune what I gated off this" action in the manager's row menu. One request (so one undo step),
# not one delete per child.
function api_gating_pop_delete(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "path") || return _gerr(400, "path required")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        _path = String(body["path"])
        _children_only = Bool(get(body, "childrenOnly", false))
        # A boolean pop (Decision 16) references pops by PATH, so deleting one it combines would leave
        # it silently pointing at nothing (empty membership + a server warning nobody reads). Refuse
        # and name the dependants instead — rename/move rewrite references, only delete can orphan them.
        _going = _children_only ? descendants(m, _path) : [_path; descendants(m, _path)]
        _deps = boolean_dependents(m, _going)
        isempty(_deps) || return _gerr(400,
            "Can't delete: " * join(["\"$(pop_name(d))\" combines $(join([pop_name(r) for r in rs], ", "))"
                                    for (d, rs) in _deps], "; ") *
            ". Edit or delete the combined population$(length(_deps) > 1 ? "s" : "") first.")
        _children_only ? del_children!(m, _path) : del_pop!(m, _path)
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

function api_gating_pop_update(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "path") || return _gerr(400, "path required")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        p = pop_at(m, String(body["path"]))
        haskey(body, "colour") && (p.colour = String(body["colour"]))
        haskey(body, "show")   && (p.show = Bool(body["show"]))
        # filter update — the tick-cluster-into-population UX toggles which cluster IDs belong to a
        # `clust`/`trackclust` pop by rewriting its filter (typically filter_values). Mutates only the
        # keys present in the `filter` dict, so a `{values:[…]}` tick leaves measure/fun untouched.
        flt = get(body, "filter", nothing)
        if flt !== nothing
            haskey(flt, "measure")     && (p.filter_measure = get(flt, "measure", nothing))
            haskey(flt, "fun")         && (p.filter_fun = get(flt, "fun", nothing))
            haskey(flt, "values")      && (p.filter_values = get(flt, "values", nothing))
            haskey(flt, "default_all") && (p.filter_default_all = Bool(get(flt, "default_all", false)))
            # compound filter (Decision 15): replace the AND-ed conditions, mirroring conditions[1] onto
            # the single fields (same contract as add_pop!). `conditions: []`/null clears back to single.
            if haskey(flt, "conditions")
                conds = Cecelia._normalise_conditions(get(flt, "conditions", nothing))
                p.filter_conditions = conds
                conds === nothing || (p.filter_measure = conds[1].measure;
                                      p.filter_fun = conds[1].fun; p.filter_values = conds[1].values)
            end
        end
        # boolean update (Decision 16): the whole definition is replaced, not patched key-by-key — a
        # boolean pop IS its term list, and a partial patch has no meaning ("keep the old excludes but
        # take the new includes" is not something the form can express). `boolean: null` clears it.
        if haskey(body, "boolean")
            bl = body["boolean"]
            try
                set_boolean!(m, String(body["path"]);
                             op   = bl === nothing ? nothing : get(bl, "op", nothing),
                             pops = bl === nothing ? nothing : get(bl, "pops", nothing),
                             nots = bl === nothing ? nothing : get(bl, "not", nothing))
            catch e
                return _gerr(400, sprint(showerror, e))
            end
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

function api_gating_pop_rename(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "newName")) || return _gerr(400, "path and newName required")
    # name-uniqueness guard: the renamed path must not collide with another pop type's population
    _newName = String(body["newName"])
    _newpath = pop_path(pop_parent(String(body["path"])), _newName)
    _conflict = pop_name_conflict(img, vn, _newpath; pop_type = pt)
    _conflict === nothing || return _gerr(400,
        "A $_conflict population named \"$_newName\" already exists in this segmentation — names must be " *
        "unique across population types so a population can be selected unambiguously. Choose another name.")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        newpath = try
            rename_pop!(m, String(body["path"]), String(body["newName"]))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        r = _persist_and_broadcast!(m, img, body, vn, pt)
        200, JSON3.write((; r..., path = newpath))
    end
end

# POST /api/gating/pop/move  { …, path, parent }  → re-parent a population, subtree and all.
# The gate is unchanged; its MEMBERSHIP is not — a pop is its own gate ∩ its parent's, so this is how
# a population gated under a QC gate is lifted out to all cells (or tucked under another gate) without
# redrawing it and its children. `parent` is "root" for top level.
function api_gating_pop_move(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "parent")) || return _gerr(400, "path and parent required")
    _path   = String(body["path"])
    _parent = String(body["parent"])
    # the move changes the pop's PATH, so it needs the same cross-pop-type uniqueness guard a rename
    # does (the mixed-type module picker resolves a population by path alone).
    _newpath = pop_path(is_root(_parent) ? ROOT : _parent, pop_name(_path))
    _conflict = pop_name_conflict(img, vn, _newpath; pop_type = pt)
    _conflict === nothing || return _gerr(400,
        "A $_conflict population already exists at \"$_newpath\" in this segmentation — names must be " *
        "unique across population types so a population can be selected unambiguously. Rename it first.")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, _path) || return _gerr(404, "Population not found: $_path")
        newpath = try
            move_pop!(m, _path, _parent)
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        r = _persist_and_broadcast!(m, img, body, vn, pt)
        200, JSON3.write((; r..., path = newpath))
    end
end

# POST /api/gating/copy  { projectUid, imageUid (source), valueName, popType, toImageUids: [...] }
# Copy the gating strategy for ONE gating pop_type (flow = cell gates, or track = track gates) from
# the source image to each target image, by writing the source's gating sidecar to each target —
# REPLACING any existing gating there. Membership recomputes per image on read, so gates alone are
# enough (no per-image recompute step, unlike the old R flowGatingSet copy). Plot layout is copied
# client-side (the canvas store is in-memory + autosaves), not here.
function api_gating_copy(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    is_gating_pop_type(pt) || return _gerr(400, "Not a gating pop type: $pt (only flow/track)")
    tgt_raw = get(body, "toImageUids", nothing)
    (tgt_raw isa AbstractVector && !isempty(tgt_raw)) || return _gerr(400, "toImageUids required")
    proj    = String(body["projectUid"])
    src_uid = String(body["imageUid"])
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        isempty(m.order) && return _gerr(400, "Source image has no $pt gating to copy")
        copied = String[]; skipped = Dict{String,String}()
        for t in tgt_raw
            tuid = String(t)
            tuid == src_uid && continue
            timg, terr = _gating_image(proj, tuid)
            timg === nothing && (skipped[tuid] = "not found"; continue)
            img_has_value_name(timg, vn) ||
                (skipped[tuid] = "no segmentation '$vn'"; continue)   # gates would reference absent channels
            # A µm POSITION gate needs the target's own pixel size to be evaluated. Without one the
            # coordinates would silently be read as pixels — wrong by the pixel size, which is the
            # exact silent-drift this unit change removes. Refuse rather than warn-and-proceed
            # (docs/todo/SPATIAL_GATE_UNITS_PLAN.md decision 7). Intensity-only strategies copy fine.
            (m.spatial_unit == SPATIAL_UNIT_UM && has_spatial_gate(m) && !img_is_calibrated(timg)) &&
                (skipped[tuid] = "no physical pixel size — set it to copy a position gate"; continue)
            try
                save_pop_map!(m, timg)                        # writes target gating sidecar (replace)
                _broadcast_popmap(proj, tuid, vn, pt, m)      # refresh any client viewing the target
                push!(copied, tuid)
            catch e
                skipped[tuid] = sprint(showerror, e)
            end
        end
        200, JSON3.write((; copied = copied, skipped = skipped))
    end
end
