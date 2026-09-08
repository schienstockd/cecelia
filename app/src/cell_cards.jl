# ── Cell cards: pooled feature reader, medoid picker, bbox, stats ────────────────
#
# The Julia half of docs/todo/CELL_CARDS_PLAN.md — no rendering, no HTTP. Given a clustering run
# `clusters.{suffix}` on a `trackclust` pop, these four helpers resolve the ONE medoid track (pool-wide),
# its pixel bbox over its own frame span, and per-pop median/IQR of the canonical `live.track.*`
# measures. `render_view_frame` + `overlay_author` (api/) then bake frames on top; the route wires it
# all together.
#
# Pool = `partOf` (clustfeatures.json) × `co_clustered_value_names(img, suffix; granularity=:track)`
# per pool member (`CELL_CARDS_PLAN` Decision 0). A single-image run is a pool of one — the code path
# does not fork.

using DataFrames
using Statistics: median, quantile
using LinearAlgebra: norm

# The ten canonical per-track measures the card's stats footer summarises. Fixed list, independent of
# which columns clustering happened to consume — so a run over motility + HMM still shows motility
# medians. Order chosen for readability (kinematics → shape → geometry).
const CELL_CARD_STATS_MEASURES = String[
    "live.track.speed",
    "live.track.duration",
    "live.track.trackLength",
    "live.track.displacement",
    "live.track.straightness",
    "live.track.displacementRatio",
    "live.track.outreachRatio",
    "live.track.meanTurningAngle",
    "live.track.overallAngle",
    "live.track.asphericity",
]

"""
    clustering_features_pooled(img, value_name, suffix; proj_uid, family="clusters")
        -> (df::DataFrame, pool::Vector{@NamedTuple{uid::String, value_name::String}})

Concatenate the run's feature columns across every `(uid, value_name)` in the pool. `img` is the
board's root image; `partOf` (from `_clustfeatures_entry`) names the other images in the run; each
image's own `co_clustered_value_names` names the segmentations. Each row tags its origin with `_uid`
and `_value_name` so `medoid_track` can return a triple.

`proj_uid` is required when `partOf` names more than the root — it's the only way to open sibling
images. When `partOf` is a single-image run the call works without it (skipping `init_object`).

Returns `(df, pool)` where `df` is stacked feature columns + `_uid` + `_value_name` + `_track_id` +
`clusters.{suffix}` (cluster code). The order of rows follows the pool's iteration order (image ×
value_name, first-appearance). Missing feature columns on a sibling degrade to `NaN` for those rows.
"""
function clustering_features_pooled(img::CciaImage, value_name::AbstractString,
                                    suffix::AbstractString;
                                    proj_uid::Union{Nothing,AbstractString}=nothing,
                                    family::AbstractString="clusters")::Tuple{DataFrame,Vector{@NamedTuple{uid::String, value_name::String}}}
    entry = _clustfeatures_entry(img_track_props_path(img, value_name), String(suffix); family=family)
    entry === nothing && error("clustering_features_pooled: no clustfeatures entry for suffix '$suffix' " *
                               "under value_name '$value_name'")
    feats = String[string(x) for x in get(entry, "features", get(entry, :features, String[]))]
    isempty(feats) && error("clustering_features_pooled: clustfeatures entry has no features (suffix=$suffix)")
    part_of = String[string(x) for x in get(entry, "partOf", get(entry, :partOf, String[]))]
    isempty(part_of) && (part_of = [img.uid])   # legacy sidecars without partOf → single-image run

    cluster_col = "$(family).$(suffix)"
    pool = @NamedTuple{uid::String, value_name::String}[]
    frames = DataFrame[]
    for uid in part_of
        sib_img = if uid == img.uid
            img
        else
            proj_uid === nothing &&
                error("clustering_features_pooled: partOf spans multiple images ($part_of) but " *
                      "proj_uid was not given — cannot open sibling '$uid'")
            init_object(String(proj_uid), uid)
        end
        for vn in co_clustered_value_names(sib_img, String(suffix); granularity=:track, family=family)
            push!(pool, (uid = uid, value_name = vn))
            path = img_track_props_path(sib_img, vn)
            isfile(path) || continue
            wanted = String[cluster_col; feats]
            lp = label_props(path) |> select_cols(wanted)
            frame = as_df(lp; include_x = true, include_obs = true)
            # `label` on a __tracks table IS the track_id (obs._index). Rename for the medoid picker.
            "label" in names(frame) || error("clustering_features_pooled: no label column at $path")
            rename!(frame, :label => :_track_id)
            # Tag origin, and fill missing features with NaN so vcat aligns.
            frame[!, :_uid] .= uid
            frame[!, :_value_name] .= vn
            for f in feats
                Symbol(f) in propertynames(frame) || (frame[!, Symbol(f)] .= NaN)
            end
            push!(frames, frame)
        end
    end
    isempty(frames) && error("clustering_features_pooled: pool is empty (suffix=$suffix)")
    # `cols = :union` fills anything missing on a per-image basis with `missing`; feature columns are
    # always present after the padding above, so this only touches tag columns.
    df = reduce((a, b) -> vcat(a, b; cols = :union), frames)
    (df, pool)
end

"""
    medoid_track(df, cluster_id; features, cluster_col) -> (uid, value_name, track_id)

The row (over the pooled `df`) whose feature vector has the smallest euclidean distance to the pool's
per-cluster mean. Ties are broken by preferring longer tracks (higher `live.track.duration`, when the
column is present in `df`) — otherwise by row order. Returns a `(uid, value_name, track_id)` triple.

Errors if the cluster has no rows in the pool, so a caller cannot silently render "the closest thing
to an empty cluster".
"""
function medoid_track(df::DataFrame, cluster_id::Real;
                      features::AbstractVector{<:AbstractString},
                      cluster_col::AbstractString)::@NamedTuple{uid::String, value_name::String, track_id::Int}
    Symbol(cluster_col) in propertynames(df) ||
        error("medoid_track: cluster column '$cluster_col' not in df")
    sub = subset(df, Symbol(cluster_col) => x -> .!ismissing.(x) .& (Float64.(x) .== Float64(cluster_id)))
    nrow(sub) == 0 && error("medoid_track: cluster $cluster_id has no rows in the pool")
    feat_syms = Symbol.(features)
    # Feature matrix: rows × features. Missing/NaN entries treated as the centroid value (contribute
    # zero to the distance) — a track missing a feature does not get penalised out of medoid contention
    # just for that gap, but a track that has the feature still competes on it.
    M = Matrix{Float64}(undef, nrow(sub), length(feat_syms))
    for (j, f) in enumerate(feat_syms)
        col = f in propertynames(sub) ? sub[!, f] : fill(NaN, nrow(sub))
        M[:, j] .= Float64.(coalesce.(col, NaN))
    end
    # Column means over the finite values; if a whole column is missing the mean is 0.0 (no signal).
    _col_mean(v) = (fv = filter(isfinite, v); isempty(fv) ? 0.0 : Float64(mean(fv)))
    centroid = Float64[_col_mean(M[:, j]) for j in axes(M, 2)]
    # Replace any remaining NaN with the centroid so it contributes zero to that dimension's distance.
    for j in axes(M, 2), i in axes(M, 1)
        isfinite(M[i, j]) || (M[i, j] = centroid[j])
    end
    dists = [norm(M[i, :] .- centroid) for i in axes(M, 1)]

    dur_sym = :"live.track.duration"
    if dur_sym in propertynames(sub)
        # Tie-break by duration (longer wins). Multiply distance by (1 - ε·rank(duration)) is too
        # subtle; simpler: pick the min-distance set (within eps), then argmax duration.
        min_d = minimum(dists); eps_d = max(1e-12, 1e-9 * (min_d == 0 ? 1.0 : abs(min_d)))
        cand = findall(d -> d <= min_d + eps_d, dists)
        durs = Float64.(coalesce.(sub[cand, dur_sym], -Inf))
        i = cand[argmax(durs)]
    else
        i = argmin(dists)
    end
    (uid = String(sub[i, :_uid]), value_name = String(sub[i, :_value_name]),
     track_id = Int(sub[i, :_track_id]))
end

"""
    track_bbox(img, value_name, track_id; pad_px=8) -> (; x, y, t0, t1)

The pixel-space bounding box of a single track over its own frame span, plus (t0, t1) inclusive. Reads
per-cell centroids via `pop_df` (the label-props view path), so it never hits the h5ad directly.

`pad_px` is applied to (xmin, xmax, ymin, ymax) before returning — enough room for a small crop
around the medoid cell without cropping into its own shape. Not clamped here (the renderer clamps
`crop` against the store's grid via `_clamp_range` in `render_view_frame`).
"""
function track_bbox(img::CciaImage, value_name::AbstractString, track_id::Integer;
                    pad_px::Int=8)::@NamedTuple{x::Tuple{Int,Int}, y::Tuple{Int,Int}, t0::Int, t1::Int}
    df = pop_df(img, "live", ["/_tracked"]; value_name = String(value_name), granularity = :cell,
                include_obs = true, centroids = :pixel)
    :track_id in propertynames(df) ||
        error("track_bbox: pop_df returned no track_id column for $(value_name)")
    sub = subset(df, :track_id => x -> .!ismissing.(x) .& (Int.(x) .== Int(track_id)))
    nrow(sub) == 0 && error("track_bbox: no cells with track_id=$track_id under $(value_name)")
    xs = Int.(round.(Float64.(sub[!, :centroid_x])))
    ys = Int.(round.(Float64.(sub[!, :centroid_y])))
    ts = :centroid_t in propertynames(sub) ? Int.(sub[!, :centroid_t]) : Int[0 for _ in eachrow(sub)]
    (x = (minimum(xs) - pad_px, maximum(xs) + pad_px),
     y = (minimum(ys) - pad_px, maximum(ys) + pad_px),
     t0 = minimum(ts), t1 = maximum(ts))
end

"""
    cell_cards_metadata(img, value_name, suffix, pops; proj_uid=nothing, family="clusters")
        -> (pool, cards)

The rendering-free half of the cell-cards pipeline — everything the frame renderer needs, plus the
stats footer, without touching a zarr. Returns `(pool, cards)` where each card is a NamedTuple with
`path, name, colour, n, medoid, frames_ts, stats` — `medoid` is a `(uid, value_name, track_id)`
triple, `frames_ts` is the three chosen filmstrip timepoints, `stats` is the `card_stats` vector.

`pops` is `Vector{@NamedTuple{path::String, cluster_ids::Vector{Int}}}` — one entry per pop, each
naming one or more cluster codes (usually one) whose union defines that pop.

Frame selection: first, mid, last of the medoid track's frame span — see Decision 4 in
`docs/todo/CELL_CARDS_PLAN.md`. "Max instantaneous speed" is deferred until per-frame speed is a
resolved column here (`live.cell.speed` is per-cell, not per-track-per-t).

Errors on a missing suffix or a cluster with no rows in the pool — callers should let those bubble
up as 4xx/500, not paper over them with an empty card.
"""
function cell_cards_metadata(img::CciaImage, value_name::AbstractString,
                             suffix::AbstractString,
                             pops::AbstractVector;
                             proj_uid::Union{Nothing,AbstractString}=nothing,
                             family::AbstractString="clusters")
    df, pool = clustering_features_pooled(img, value_name, suffix;
                                          proj_uid=proj_uid, family=family)
    entry = _clustfeatures_entry(img_track_props_path(img, value_name), String(suffix); family=family)
    feats = String[string(x) for x in get(entry, "features", get(entry, :features, String[]))]
    cluster_col = "$(family).$(suffix)"

    # Resolve the pop map ONCE — we need each pop's colour + name + count.
    m = load_pop_map(img._dir, String(value_name); pop_type="trackclust")

    cards = @NamedTuple{path::String, name::String, colour::String, n::Int,
                        medoid::@NamedTuple{uid::String, value_name::String, track_id::Int},
                        frames_ts::Vector{Int},
                        stats::Vector{@NamedTuple{name::String, median::Float64, q25::Float64, q75::Float64}}}[]

    for p in pops
        path = String(p.path); cids = collect(Int, p.cluster_ids)
        isempty(cids) && continue
        # For "one card = one pop" the medoid is picked over the UNION of the pop's cluster codes
        # (usually one); build a per-pop cluster-code virtual by picking the first non-empty code,
        # then a merged sub-frame. Same code path when cids is a single element.
        sub_rows = falses(nrow(df))
        col = df[!, Symbol(cluster_col)]
        @inbounds for i in eachindex(col)
            v = col[i]; ismissing(v) && continue
            sub_rows[i] = Int(round(Float64(v))) in cids
        end
        n = count(sub_rows)
        n == 0 && continue    # pop has no rows in the pool → skip (rather than fake a medoid)
        # medoid over the union: use the whole cluster set at once by passing the union code as a
        # synthetic cluster id — easier to just filter the df first and reuse the primitive.
        sub_df = df[sub_rows, :]
        # Treat the union as one virtual cluster (id 0 arbitrarily) — medoid_track only filters by
        # cluster_col, so overwrite it.
        sub_df = deepcopy(sub_df); sub_df[!, Symbol(cluster_col)] .= 0.0
        medoid = medoid_track(sub_df, 0.0; features=feats, cluster_col=cluster_col)

        # Resolve the medoid's image + bbox + frame span. The `img` we have IS the root; a medoid on
        # a sibling requires init_object under `proj_uid`.
        med_img = medoid.uid == img.uid ? img :
                  (proj_uid === nothing ?
                      error("cell_cards_metadata: medoid on sibling '$(medoid.uid)' needs proj_uid") :
                      init_object(String(proj_uid), medoid.uid))
        bbox = track_bbox(med_img, medoid.value_name, medoid.track_id; pad_px=8)
        # 3 frames: first / mid / last. Cast into a stable Vector{Int} — the caller then hands each t
        # to `render_view_frame`.
        frames_ts = bbox.t0 == bbox.t1 ?
                    Int[bbox.t0] :
                    Int[bbox.t0, (bbox.t0 + bbox.t1) ÷ 2, bbox.t1]

        # Stats footer: pop_df over the medoid's (uid, vn) — for the single-image pool this matches
        # the pool-wide pop, and for a multi-image pool the footer intentionally reflects "this pop
        # on this image" (the card's own frame). Whole-pool medians land in Phase 2's detail panel.
        frame = pop_df(med_img, "trackclust", [path]; value_name=medoid.value_name, granularity=:track)
        stats = card_stats(frame)

        # Pop colour + display name from the map — falls back to path segment + white if not found
        # (shouldn't happen: rail restricts to authored pops, but be resilient).
        pop_name = haskey(m.pops, path) ? m.pops[path].name : split(path, "/")[end]
        pop_colour = haskey(m.pops, path) ? m.pops[path].colour : "#ffffff"

        push!(cards, (path=path, name=pop_name, colour=pop_colour, n=n,
                      medoid=medoid, frames_ts=frames_ts, stats=stats))
    end
    (pool, cards)
end

"""
    card_stats(pop_df_frame; measures=CELL_CARD_STATS_MEASURES) -> Vector{@NamedTuple{name,median,q25,q75}}

Per-pop median + q25–q75 over `measures`, given a `pop_df(...; pop_type="trackclust", granularity=:track)`
frame. Skips a measure that isn't a column in the frame (returns nothing for that entry — a
run that clustered on a smaller feature set still gets a stats block; the missing measures just don't
appear rather than lying with a NaN median).
"""
function card_stats(frame::DataFrame; measures::AbstractVector{<:AbstractString}=CELL_CARD_STATS_MEASURES)::Vector{@NamedTuple{name::String, median::Float64, q25::Float64, q75::Float64}}
    out = @NamedTuple{name::String, median::Float64, q25::Float64, q75::Float64}[]
    for m in measures
        sym = Symbol(m)
        sym in propertynames(frame) || continue
        vals = Float64[Float64(v) for v in skipmissing(frame[!, sym]) if isfinite(Float64(v))]
        isempty(vals) && continue
        qs = quantile(vals, (0.25, 0.75))
        push!(out, (name = m, median = median(vals), q25 = qs[1], q75 = qs[2]))
    end
    out
end
