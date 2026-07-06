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
    isfile(joinpath(projects_dir(), project_uid, "1", image_uid, "ccid.json")) ||
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

# read x/y (optionally subset to a population) → transformed Float32 vectors.
# Chain idiom (docs/DATAMODEL.md): select the two channels, push the population's label
# filter into the reader (filter_rows), then materialise once.
# read the raw x/y vectors for the scatter (one row per cell, or per track for pop_type="track"),
# optionally subset to a population, before transform.
function _plot_xy_raw(img, vn, pop_type, x, y, pop)
    if _track_grained(pop_type)
        # per-track scatter: one point per track from `track_props` (label == track_id)
        tp = track_props(img; value_name = vn,
                         cell_measures = track_cell_measures([x, y], _track_free_cols(img, vn)))
        (x in names(tp) && y in names(tp)) || return (Float64[], Float64[])
        if !is_root(pop)
            m = _live_map(img, vn, pop_type)
            has_pop(m, pop) || return (Float64[], Float64[])
            keep = Set(cells_in_pop(m, pop))                 # gated track_ids
            tp = tp[[t in keep for t in tp.label], :]
        end
        return (Float64.(tp[!, x]), Float64.(tp[!, y]))
    end
    # cell scatter — chain idiom: select the two channels, push the population's label filter into
    # the reader (filter_rows), then materialise once.
    lp = label_props(img; value_name = vn) |> select_cols([x, y])
    if !is_root(pop)
        m = _live_map(img, vn, pop_type)
        # the pop may have vanished since the client last rendered (e.g. a plot/highlight still
        # pointing at a napari selection that has since been cleared) — return empty data rather
        # than letting cells_in_pop throw a 500 ("pop_membership: not found: /Napari selection").
        has_pop(m, pop) || return (Float64[], Float64[])
        filter_rows(lp, cells_in_pop(m, pop); by = :label)
    end
    df = as_df(lp)
    # the requested axes may not exist on the cell table (e.g. a stale panel pointing at track columns
    # like `live.track.*` while popType=flow) — `select_cols` drops unknown columns, so guard here and
    # return empty rather than throwing a 500 (mirrors the track branch's `x in names(...)` guard).
    (x in names(df) && y in names(df)) || return (Float64[], Float64[])
    (Float64.(df[!, x]), Float64.(df[!, y]))
end

# transformed Float32 vectors for plotdata/density/plotmeta
function _plot_xy(img, vn, pop_type, x, y, pop, xt, yt)
    xv, yv = _plot_xy_raw(img, vn, pop_type, x, y, pop)
    Float32.(apply_transform(xt, xv)), Float32.(apply_transform(yt, yv))
end

# broadcast the updated tree to all clients
function _broadcast_popmap(project_uid, image_uid, vn, pop_type, m::PopulationMap)
    broadcast_ws(Dict{String,Any}(
        "type" => "gating:popmap", "projectUid" => project_uid, "imageUid" => image_uid,
        "valueName" => vn, "popType" => pop_type, "tree" => to_tree(m)))
end

# Serialise gating pop CRUD. Each handler does load_pop_map → mutate → save_pop_map!; under
# `-t auto` two concurrent edits to the same map (rapid clicks / two tabs) would otherwise both
# load the same tree and the second save would clobber the first (lost update). One process-wide
# ReentrantLock held across the whole load→save is enough — gating edits are user-paced and cheap,
# so contention across images/value_names is negligible. (save_pop_map! also writes atomically.)
const _POPMAP_LOCK = ReentrantLock()
_with_popmap_lock(f) = lock(f, _POPMAP_LOCK)

# persist a mutated map then broadcast — re-injecting the transient napari pop AFTER save
# (so it never hits disk) but BEFORE broadcast (so the client keeps showing it). Returns the
# tree (incl. the transient pop) for the HTTP response.
function _persist_and_broadcast!(m::PopulationMap, img::CciaImage, body, vn, pop_type)
    save_pop_map!(m, img)
    _inject_napari_pop!(m, img)
    _broadcast_popmap(String(body["projectUid"]), String(body["imageUid"]), vn, pop_type, m)
    to_tree(m)
end

# cluster suffixes available in a table's obs (the `clusters.{suffix}` columns written by
# clustPops/clustTracks) → drives the cluster pages' page-level suffix dropdown.
_cluster_suffixes(obscols) = String[c[ncodeunits("clusters.")+1:end] for c in obscols if startswith(c, "clusters.")]

# per-suffix clustering manifest ({props}.clustfeatures.json; clustPops/clustTracks write it),
# keyed by suffix as {features, partOf}. `features` → the heatmap offers exactly those columns;
# `partOf` → the uIDs clustered together (mirrors old R `valuePartOf`), so cluster pops are only
# defined for images in the run. Back-compat: pre-partOf runs stored {suffix => [features]} (a bare
# array, no membership) — surface those as features with an empty member list.
function _clust_manifest(props_path::AbstractString)
    s = replace(props_path, r"\.h5ad$" => ".clustfeatures.json")
    isfile(s) ? JSON3.read(read(s, String), Dict{String,Any}) : Dict{String,Any}()
end
_clust_features(props_path::AbstractString) = Dict{String,Any}(
    String(k) => (v isa AbstractVector ? v : get(v, "features", String[]))
    for (k, v) in _clust_manifest(props_path))
_clust_members(props_path::AbstractString) = Dict{String,Any}(
    String(k) => (v isa AbstractVector ? String[] : get(v, "partOf", String[]))
    for (k, v) in _clust_manifest(props_path))

# distinct cluster IDs per suffix — the universe the pop-manager offers as tickable clusters. Reads
# the `clusters.{suffix}` obs column (integer codes) and drops unclustered (<0). One column read per
# suffix; suffixes are few per table (usually 1), so the cost is a single-column scan on image-select.
function _cluster_ids(path::AbstractString, suffixes::Vector{String})
    out = Dict{String,Any}()
    for s in suffixes
        col = "clusters.$s"
        df = label_props(path) |> select_cols([col]) |> as_df
        out[s] = sort!(collect(Set(Int(round(x)) for x in df[!, col] if isfinite(x) && x >= 0)))
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
        return 200, JSON3.write((;
            columns = motility,                                  # whole-track motility (directly gateable)
            cellMeasures = cellmeas,                             # cell vars → per-track numeric aggregates
            cellObsMeasures = cellobs,                           # cell obs → per-track aggregates (HMM, …)
            channelNames = display === nothing ? String[] : display,  # relabel intensity aggregates
            trackAggregates = ["mean", "median", "sum", "qUp", "qLow", "sd"],  # see track_props
            clusterSuffixes = _cluster_suffixes(tobs),           # trackclust runs in the track table
            clusterFeatures = _clust_features(tpath),
            clusterMembers  = _clust_members(tpath),             # uIDs clustered together (partOf)
            clusterIds      = isfile(tpath) ? _cluster_ids(tpath, _cluster_suffixes(tobs)) : Dict{String,Any}(),
            valueNames = versioned_keys(img.label_props),
            valueName = vn,
            popType = get(q, "popType", "track"),
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
    200, JSON3.write((;
        columns = cols,
        channels = chans,
        channelNames = display === nothing ? String[] : display,
        channelNameVersions = versions,
        obsColumns = col_names(lp; data_type = :obs),   # per-cell obs measures (live.cell.*, hmm.state, …) for labelPropsColsSelection
        temporalColumns = temporal_columns(lp),          # obsm temporal col(s) (e.g. "t") — groupable for the per-timepoint QC view
        clusterSuffixes = _cluster_suffixes(col_names(lp; data_type = :obs)),   # clust runs in the cell table
        clusterFeatures = _clust_features(img_label_props_path(img, vn)),
        clusterMembers  = _clust_members(img_label_props_path(img, vn)),        # uIDs clustered together (partOf)
        clusterIds      = _cluster_ids(img_label_props_path(img, vn), _cluster_suffixes(col_names(lp; data_type = :obs))),
        valueNames = versioned_keys(img.label_props),
        valueName = vn,                 # the server-resolved value_name these columns belong to
    ))
end

# ── GET /api/gating/popmap ────────────────────────────────────────────────────
function api_gating_popmap(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    m = load_pop_map(img; value_name = vn, pop_type = get(q, "popType", "flow"))
    _inject_napari_pop!(m, img)
    200, JSON3.write((; tree = to_tree(m)))
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

# ── GET /api/gating/plotmeta ──────────────────────────────────────────────────
# returns n + mode (scatter|density) + transformed extents + axis ticks
function api_gating_plotmeta(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    xt = _axis_transform(q, "x"); yt = _axis_transform(q, "y")
    xv, yv = _plot_xy(img, vn, get(q, "popType", "flow"), x, y, get(q, "pop", ROOT), xt, yt)
    n = length(xv)
    density_threshold = parse(Int, get(q, "densityThreshold", "200000"))
    mode = n > density_threshold ? "density" : "scatter"
    xext = isempty(xv) ? (0.0, 1.0) : extrema(xv)
    yext = isempty(yv) ? (0.0, 1.0) : extrema(yv)
    # ticks use the raw extent (over the WHOLE dataset, not the pop subset) so labels read in data
    # units and selecting a population doesn't rescale the axis — track-aware via _plot_xy_raw.
    rxv, ryv = _plot_xy_raw(img, vn, get(q, "popType", "flow"), x, y, ROOT)
    rxext = isempty(rxv) ? (0.0, 1.0) : extrema(rxv)
    ryext = isempty(ryv) ? (0.0, 1.0) : extrema(ryv)
    # x0/y0=1 → "whole dataset" axis: origin at raw 0, upper bound = the FULL dataset max (rxext
    # is computed from all cells, not the pop subset), so selecting a population doesn't rescale
    # the axis (FlowJo behaviour). Autoscale (x0=0) fits the displayed population's extent.
    if get(q, "x0", "") == "1"
        xext = (apply_transform(xt, 0.0), apply_transform(xt, float(rxext[2]))); rxext = (0.0, rxext[2])
    end
    if get(q, "y0", "") == "1"
        yext = (apply_transform(yt, 0.0), apply_transform(yt, float(ryext[2]))); ryext = (0.0, ryext[2])
    end
    200, JSON3.write((;
        n = n, mode = mode,
        xExtent = [xext[1], xext[2]], yExtent = [yext[1], yext[2]],
        xLabel = x, yLabel = y,
        xTicks = _axis_ticks(xt, rxext[1], rxext[2]),
        yTicks = _axis_ticks(yt, ryext[1], ryext[2]),
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
    xv, yv = _plot_xy(img, vn, get(q, "popType", "flow"), x, y, get(q, "pop", ROOT), xt, yt)
    n = length(xv)
    buf = Vector{Float32}(undef, 2n)
    @inbounds for i in 1:n
        buf[2i-1] = xv[i]; buf[2i] = yv[i]
    end
    200, collect(reinterpret(UInt8, buf))
end

# ── GET /api/plots/umap → binary Float32 [x,y,clusterCode, …] per point ───────
# The UMAP scatter for a clustering pop_type: the `obsm['X_umap.{suffix}']` embedding + the
# `clusters.{suffix}` code per point (frontend colours by code). `clust` reads the cell table;
# `trackclust` reads the per-track table (one point per track). Optionally subset to a population's
# membership (`pop`). Cluster codes are small ints → exact in Float32; an unclustered row → -1.
function api_plots_umap(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn       = _resolve_vn(img, get(q, "valueName", ""))
    pop_type = get(q, "popType", "clust")
    suffix   = get(q, "suffix", "default")
    pop      = get(q, "pop", ROOT)
    umap_key = "X_umap.$suffix"; clust_col = "clusters.$suffix"

    # data table: trackclust → per-track table; clust → cell table
    path = _track_grained(pop_type) ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
    isfile(path) || return _gerr(404, "No labelProps/track table for value_name=$vn")
    xy = obsm(label_props(path), umap_key)              # (n_obs, 2), obs order
    size(xy, 1) == 0 && return _gerr(404, "No $umap_key — run clustering with UMAP enabled")
    # cluster codes aligned to obs order (label + the code column, same obs order as obsm)
    cdf = label_props(path) |> select_cols([clust_col]) |> as_df
    codes = clust_col in names(cdf) ? cdf[!, clust_col] : fill(missing, nrow(cdf))

    keep = trues(size(xy, 1))
    if !is_root(pop)
        m = _live_map(img, vn, pop_type)
        has_pop(m, pop) || return _gerr(404, "Population not found: $pop")
        sel  = Set(cells_in_pop(m, pop))
        keep = BitVector(l in sel for l in cdf.label)
    end
    # drop rows with no embedding — cells/tracks outside the clustered set have NaN UMAP coords
    # (and NaN code); they'd otherwise render as a stray "NaN" cluster.
    @inbounds for i in 1:size(xy, 1)
        (isfinite(xy[i, 1]) && isfinite(xy[i, 2])) || (keep[i] = false)
    end

    idx = findall(keep)
    buf = Vector{Float32}(undef, 3 * length(idx))
    @inbounds for (j, i) in enumerate(idx)
        buf[3j-2] = Float32(xy[i, 1]); buf[3j-1] = Float32(xy[i, 2])
        c = codes[i]
        buf[3j]   = (c isa Number && !ismissing(c)) ? Float32(c) : -1f0
    end
    200, collect(reinterpret(UInt8, buf))
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
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        gate = haskey(body, "gate") && body["gate"] !== nothing ? gate_from_spec(body["gate"]) : nothing
        flt = get(body, "filter", nothing)
        try
            add_pop!(m, String(body["name"]); parent = String(get(body, "parent", ROOT)),
                     gate = gate, colour = String(get(body, "colour", "#ffffff")),
                     show = Bool(get(body, "show", true)),
                     filter_measure = flt === nothing ? nothing : get(flt, "measure", nothing),
                     filter_fun     = flt === nothing ? nothing : get(flt, "fun", nothing),
                     filter_values  = flt === nothing ? nothing : get(flt, "values", nothing),
                     filter_default_all = flt === nothing ? false : Bool(get(flt, "default_all", false)),
                     is_track = Bool(get(body, "is_track", false)))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        200, JSON3.write((; tree = _persist_and_broadcast!(m, img, body, vn, pt)))
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
        200, JSON3.write((; tree = _persist_and_broadcast!(m, img, body, vn, pt)))
    end
end

function api_gating_pop_delete(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "path") || return _gerr(400, "path required")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        del_pop!(m, String(body["path"]))
        200, JSON3.write((; tree = _persist_and_broadcast!(m, img, body, vn, pt)))
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
        end
        200, JSON3.write((; tree = _persist_and_broadcast!(m, img, body, vn, pt)))
    end
end

function api_gating_pop_rename(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "newName")) || return _gerr(400, "path and newName required")
    _with_popmap_lock() do
        m = load_pop_map(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _gerr(404, "Population not found: $(body["path"])")
        newpath = try
            rename_pop!(m, String(body["path"]), String(body["newName"]))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        tree = _persist_and_broadcast!(m, img, body, vn, pt)
        200, JSON3.write((; tree = tree, path = newpath))
    end
end
