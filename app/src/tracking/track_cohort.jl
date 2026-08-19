# ── Cohort grouping for the two track PLOTS (paths + diagnostics) ─────────────────────────────────
#
# The track plots used to answer for ONE image what every summary plot answers for a cohort. A board can
# put treatments beside each other and populations beside each other; tracks and the diagnostics battery
# could do neither, so a figure comparing WT with MerTK had to be assembled by hand out of per-image
# screenshots. This file is the missing half: the same comparison vocabulary the summary aggregator
# takes, resolved down to the cell frames the two plots draw from.
#
# A **group** is one (images × population) cell of that comparison — what the plot facets or colours by:
#
#   - images     grouped by an image ATTRIBUTE (images sharing the combined value pool into one group,
#                labelled by it), or pooled into one, or one group each.
#   - population one group per selected population, or all of them pooled, or — with nothing selected —
#                the whole segmentation, which is what the Track page's own canvas asks for.
#
# There is deliberately NO separate single-image path: one image and no populations is one group, and the
# payload the routes send is the shape they always sent, one level down.
#
# Two rules are borrowed rather than restated, because a second copy is how two plots on one board start
# disagreeing about what "Treatment" means:
#   - `image_attr_groups` (`model/set.jl`) is THE attribute join (values joined with ".", a gap keeping
#     its own uID) — shared with `POST /api/plot_data`.
#   - `_split_pop_ref` is THE population-ref grammar ("B/qc" names segmentation B; "/qc" stays in the
#     given one) — shared with `pop_df`, which is also what reads the cells.
#
# The frames come from `pop_df` when a population is named and from `label_props` when none is
# (`docs/POPULATION.md`: pop_df is primary, label_props is the escape hatch for a read with no
# population). Both hand back µm through the one `scale_centroids!`, so distances here mean what
# `track_measures` reports.

"""
    TrackPlotSource

One (image, segmentation, population) the cells of a track-plot group came from, with its frame: cell
rows carrying `label`, `track_id`, coordinates in µm and `centroid_t` in frames.

Kept as a LIST inside a group rather than pre-pooled because the two plots pool differently — paths keep
them apart so a track can be labelled by where it came from; the diagnostics battery pools them through
[`pooled_track_frame`] so its pair scan can still stay within one movie.
"""
struct TrackPlotSource
    uid::String
    img::Any                          # the CciaImage (untyped: this file is included before nothing)
    value_name::String
    pop::String                       # "" = the whole segmentation
    df::DataFrame
    spatial::Vector{String}
end

"""
    TrackPlotGroup

One (images × population) cell of a track-plot comparison: its label, and the [`TrackPlotSource`]s
behind it with the coordinate columns they all share.
"""
struct TrackPlotGroup
    key::String
    label::String
    pop_type::String
    sources::Vector{TrackPlotSource}
    spatial::Vector{String}           # the coordinate columns EVERY source in the group has
    time_step::Float64                # minutes per frame of the first image (NaN when uncalibrated)
end

"""Distinct image uIDs behind a group, in source order."""
track_group_images(g::TrackPlotGroup) = unique(String[s.uid for s in g.sources])
"""The group's segmentation, or `""` when its sources disagree (a cross-segmentation selection)."""
track_group_value_name(g::TrackPlotGroup) = _one_or_empty(String[s.value_name for s in g.sources])
"""The group's population, or `""` when its sources disagree (a pooled selection)."""
track_group_pop(g::TrackPlotGroup) = _one_or_empty(String[s.pop for s in g.sources])
_one_or_empty(v) = (u = unique(v); length(u) == 1 ? first(u) : "")

"""
    track_plot_groups(imgs, uids; kwargs...) -> (groups, dropped, value_name)

Resolve a track-plot request into [`TrackPlotGroup`]s, in the order they should be drawn.

- `group_attrs` — image attributes to group by (`String[]` = don't). Wins over `pool_images`: grouping by
  attribute IS the pooling, and doing both would silently discard the attribute.
- `pool_images` — one group over every image (the "pooled" compare mode).
- `pops` — value-name-prefixed population refs; empty means the whole segmentation.
- `pool_pops` — the selected populations as ONE group instead of one each.
- `max_groups` — a cap, because the cost is linear in the groups and a 5-image cohort crossed with four
  populations would otherwise compute 20 diagnostics batteries on one click. `dropped` says how many were
  left off so the plot can SAY it instead of quietly showing a subset.

`value_name` is the segmentation resolved for the first image — what a picker should show as current. A
group whose images carry no tracks yields no sources and is dropped here, so "not tracked" is answered by
`isempty(groups)` rather than by an empty plot.
"""
function track_plot_groups(imgs::AbstractVector, uids::AbstractVector;
                           group_attrs::Vector{String} = String[],
                           pool_images::Bool = false,
                           pops::Vector{String} = String[],
                           pop_type::AbstractString = "live",
                           value_name::Union{AbstractString,Nothing} = nothing,
                           pool_pops::Bool = false,
                           max_groups::Int = 12)
    length(imgs) == length(uids) ||
        error("track_plot_groups: imgs and uids must be parallel ($(length(imgs)) vs $(length(uids)))")
    isempty(imgs) && return (TrackPlotGroup[], 0, "")
    pairs = collect(zip(imgs, uids))
    vn0 = resolve_value_name(first(imgs), value_name)

    igroups = _track_image_groups(pairs, group_attrs, pool_images)
    pgroups = _track_pop_groups(pops, vn0, pool_pops)

    out = TrackPlotGroup[]
    dropped = 0
    for ig in igroups, pg in pgroups
        if length(out) >= max_groups
            dropped += 1
            continue
        end
        sources = TrackPlotSource[]
        for (img, uid) in ig.items, (vn, pop) in pg.refs
            src = _track_plot_source(img, String(uid), vn, pop, pop_type)
            src === nothing || push!(sources, src)
        end
        isempty(sources) && continue
        spatial = reduce(intersect, [s.spatial for s in sources])
        isempty(spatial) && continue
        _, tstep = img_physical_sizes(first(sources).img)
        push!(out, TrackPlotGroup(string(ig.key, "|", pg.key), _track_group_label(ig.label, pg.label),
                                  String(pop_type), sources, spatial, Float64(tstep)))
    end
    # ONE group has nothing to name: a legend of one entry, or a facet title over the only cell, is noise.
    # Blanked here rather than in each caller, because only this loop knows the final count.
    length(out) == 1 && (out[1] = _relabel(out[1], ""))
    (_disambiguate_labels(out), dropped, vn0)
end

_relabel(g::TrackPlotGroup, label::AbstractString) =
    TrackPlotGroup(g.key, String(label), g.pop_type, g.sources, g.spatial, g.time_step)

# Two groups may honestly want the same NAME — two images called "Image 1" (names are not unique, only
# uIDs are), or a population called "tcells" under two segmentations. A legend with the same entry twice
# in two colours is not a legend, so a collision gains the first dimension that actually differs.
function _disambiguate_labels(out::Vector{TrackPlotGroup})
    counts = Dict{String,Int}()
    for g in out
        isempty(g.label) || (counts[g.label] = get(counts, g.label, 0) + 1)
    end
    any(>(1), values(counts)) || return out
    fixed = TrackPlotGroup[]
    for g in out
        if isempty(g.label) || counts[g.label] == 1
            push!(fixed, g)
        else
            peers = [p for p in out if p.label == g.label && p.key != g.key]
            push!(fixed, _relabel(g, string(g.label, " · ", _label_suffix(g, peers))))
        end
    end
    fixed
end

# The first dimension that tells this group apart from the ones sharing its label. Groups with the same
# images AND segmentation AND population cannot both exist (that is the key), so this always terminates.
function _label_suffix(g::TrackPlotGroup, peers)
    imgs = track_group_images(g)
    any(p -> track_group_images(p) != imgs, peers) && return join(imgs, "+")
    vn = track_group_value_name(g)
    any(p -> track_group_value_name(p) != vn, peers) && return vn
    _track_pop_label(track_group_pop(g))
end

# Images → labelled bundles. `key` is stable (uIDs); `label` is what a legend shows, which is the image's
# NAME — two images can share a name, so the two are not the same string.
function _track_image_groups(pairs, group_attrs::Vector{String}, pool_images::Bool)
    if !isempty(group_attrs)
        amap = image_attr_groups(first.(pairs), last.(pairs), group_attrs)
        order = String[]
        byval = Dict{String,Vector{Any}}()
        for (img, uid) in pairs
            # a gap keeps its own uID rather than joining every other gap under one empty label
            v = get(amap, String(uid), String(uid))
            haskey(byval, v) || (push!(order, v); byval[v] = Any[])
            push!(byval[v], (img, String(uid)))
        end
        return [(; key = v, label = v, items = byval[v]) for v in order]
    end
    pool_images && return [(; key = "pooled", label = "",
                             items = Any[(img, String(uid)) for (img, uid) in pairs])]
    [(; key = String(uid), label = _img_label(img, uid), items = Any[(img, String(uid))])
     for (img, uid) in pairs]
end

# The image as a person names it, falling back to the uID (a name is optional on disk).
_img_label(img, uid) = (n = try; String(img.name); catch; ""; end; isempty(n) ? String(uid) : n)

# Population refs → labelled bundles of (value_name, pop). A pooled bundle carries every ref.
function _track_pop_groups(pops::Vector{String}, vn0::AbstractString, pool_pops::Bool)
    isempty(pops) && return [(; key = String(vn0), label = "", refs = Tuple{String,String}[(String(vn0), "")])]
    refs = Tuple{String,String}[_split_pop_ref(p, vn0) for p in pops]
    pool_pops && return [(; key = "pooled", label = "populations", refs = refs)]
    [(; key = string(vn, pop), label = _track_pop_label(pop), refs = Tuple{String,String}[(vn, pop)])
     for (vn, pop) in refs]
end

# The leaf name is what a user calls a population ("/T cells/CD4" → "CD4"); the whole path is a file
# path, not a label.
function _track_pop_label(pop::AbstractString)::String
    segs = split(String(pop), '/'; keepempty = false)
    isempty(segs) ? "" : String(last(segs))
end

# "WT · CD4" — the two dimensions of the comparison, in the order the board's own controls read. Empty
# when there is only one group to name (one image, one population): a legend of one entry is noise.
_track_group_label(image_label, pop_label) =
    join(filter(!isempty, String[String(image_label), String(pop_label)]), " · ")

# One (image, segmentation, population)'s cells, or `nothing` when it has no tracks to draw.
function _track_plot_source(img, uid::String, vn::AbstractString, pop::AbstractString,
                            pop_type::AbstractString)::Union{Nothing,TrackPlotSource}
    props = img_label_props_path(img, vn)
    isfile(props) || return nothing
    lp = label_props(props)
    ("track_id" in col_names(lp; data_type = :obs)) || return nothing
    spatial  = centroid_columns(lp; order = [:x, :y, :z])
    temporal = temporal_columns(lp)
    (isempty(spatial) || isempty(temporal)) && return nothing
    pixel_res, _ = img_physical_sizes(img)
    df = if isempty(pop)
        # no population named — the escape hatch, and the whole segmentation is what it means
        select_cols(lp, vcat(spatial, temporal, ["track_id"]))
        d = as_df(lp; include_x = false, include_obs = true)
        scale_centroids!(d, pixel_res)          # µm, via the ONE shared conversion
        d
    else
        # `expand_cluster_pops = false`: the picker offers each (segmentation, population) pair as its own
        # row, so a bare cluster ref must plot exactly the one that was ticked — the same reason the
        # summary series path passes it.
        pop_df(img, pop_type, [pop]; value_name = vn, pop_cols = ["track_id"],
               granularity = :cell, centroids = :physical, expand_cluster_pops = false)
    end
    nrow(df) == 0 && return nothing
    t_col = first(temporal)
    if !("centroid_t" in names(df))
        t_col in names(df) || return nothing
        df[!, :centroid_t] = df[!, Symbol(t_col)]
    end
    # the coordinates this frame actually carries — a `pop_df` frame for a population with no cells of
    # its own comes back narrower, and a group's `spatial` is the intersection over its sources
    have = intersect(spatial, names(df))
    isempty(have) && return nothing
    TrackPlotSource(uid, img, String(vn), String(pop), df, have)
end

# What a track key is prefixed with inside a group. Nothing when the group has one source (the plain
# track id, exactly as before); otherwise whichever dimensions VARY within it — a pooled group holds two
# movies' track 17 and they are not the same cell.
function _track_source_labels(g::TrackPlotGroup)::Vector{String}
    n = length(g.sources)
    n == 1 && return [""]
    by_uid = length(unique(String[s.uid for s in g.sources])) > 1
    by_pop = length(unique(String[s.pop for s in g.sources])) > 1
    [join(filter(!isempty, String[by_uid ? s.uid : "", by_pop ? _track_pop_label(s.pop) : ""]), "/")
     for s in g.sources]
end

"""
    track_group_paths(g; limit, ids, color_by, occupancy) -> NamedTuple

One group's path geometry, JSON-ready: `(; paths, values, color_by, color_kind, total, shown, step_scale)`.

Track keys are the plain track id for a single-source group and `"\$source:\$tid"` when the group pools
several (see [`_track_source_labels`]). The colour `values` are keyed the same way, so a caller cannot
join them wrongly.

The cap is by track LENGTH, longest first (a hairball of one-point fragments is the least informative
thing an image has) and taken ROUND-ROBIN across the group's sources, so a pooled group shows each of its
images rather than whichever one won the length ties; `total`/`shown` both come back so the plot can say
what it left out. `ids` names
tracks explicitly and ignores the cap — matched against the plain track id in every source, since the id
is what a user can read off a viewer.

`occupancy = true` sends the TIMEPOINTS ONLY (`x`/`y`/`label` come back empty). It is for the track
timeline, which draws lanes over frames and reads nothing but `t` — and which, unlike a path plot, must
not be capped at a top-N: a hairball of 5000 polylines is unreadable, whereas a lane list capped is
simply a lie, since "pick track 2001" has no answer if 2001 was never sent.
"""
function track_group_paths(g::TrackPlotGroup; limit::Int = 500, ids = String[],
                           color_by::AbstractString = "", occupancy::Bool = false)
    labels = _track_source_labels(g)
    want_ids = Set{String}(String.(ids))
    all_paths = Dict{String,Any}()
    owner = Dict{String,Tuple{Int,String}}()          # plot key → (source index, plain track id)
    for (i, s) in enumerate(g.sources)
        for (tid, p) in track_path_dicts(s.df, g.spatial; occupancy = occupancy)
            key = isempty(labels[i]) ? String(tid) : string(labels[i], ":", tid)
            all_paths[key] = p
            owner[key] = (i, String(tid))
        end
    end
    # The cap is FAIR across the group's sources: each source's own longest-first list, taken
    # round-robin. Sorting the pooled list and cutting would break ties by source order, and ties are
    # the common case (in a short movie most tracks span every frame) — so a pooled group could show
    # one movie's tracks and none of its replicate's, which looks exactly like a complete plot.
    per_src = Dict{Int,Vector{String}}()
    for k in keys(all_paths); push!(get!(per_src, owner[k][1], String[]), k); end
    for (_, ks) in per_src
        sort!(ks; by = k -> (-length(all_paths[k]["t"]), parse(Int, owner[k][2])))
    end
    order = String[]
    src_order = sort!(collect(keys(per_src)))
    for i in 1:maximum(length(ks) for (_, ks) in per_src; init = 0), sidx in src_order
        ks = per_src[sidx]
        i <= length(ks) && push!(order, ks[i])
    end
    shown_keys = isempty(want_ids) ? first(order, max(limit, 0)) :
                 String[k for k in order if owner[k][2] in want_ids]
    paths = Dict{String,Any}(k => all_paths[k] for k in shown_keys)

    values, kind = isempty(color_by) ? (Dict{String,Any}(), "none") :
                   _track_group_values(g, shown_keys, owner, color_by)
    (; paths = paths, values = values, color_by = (kind == "none" ? "" : String(color_by)),
       color_kind = kind, total = length(order), shown = length(shown_keys),
       step_scale = track_step_scale(pooled_track_frame([s.df for s in g.sources]), g.spatial))
end

# The per-track colour values for the shown tracks, read from each source's own per-track table.
#
# Only columns the per-track table provides DIRECTLY (the motility measures, and the track table's own
# obs such as `clusters.{suffix}`): a cell measure would first have to be aggregated, which means
# choosing WHICH aggregate — a decision a plot has nowhere to ask about. An unknown column comes back as
# "none" so the caller drops the picker rather than painting a scale with no data behind it.
function _track_group_values(g::TrackPlotGroup, shown_keys, owner, color_by::AbstractString)
    values = Dict{String,Any}()
    kind = "none"
    by_src = Dict{Int,Vector{String}}()
    for k in shown_keys; push!(get!(by_src, owner[k][1], String[]), k); end
    for (i, ks) in by_src
        s = g.sources[i]
        tp = track_props(s.img; value_name = s.value_name)
        (color_by in names(tp)) || continue
        col = tp[!, color_by]
        # the ONE measure-type detector (`track_props`' own) — `eltype <: Real` is not it: a joined
        # column decodes as Union{Missing,Float64} and would read as categorical
        cat = _is_categorical_col(col, color_by)
        kind = cat ? "categorical" : "numeric"
        want = Dict{String,String}(owner[k][2] => k for k in ks)
        for (r, tid) in enumerate(tp[!, :track_id])
            key = get(want, string(Int(tid)), "")
            isempty(key) && continue
            v = col[r]
            values[key] = cat ? string(v) : (v isa Real && !isnan(Float64(v)) ? Float64(v) : nothing)
        end
    end
    (values, kind)
end

"""
    track_group_frame(g) -> Union{Nothing,NamedTuple{(:df,:spatial,:value_name)}}

One group's cells as a single frame — the accessor for a readout that must EDIT rather than summarise.

Beside [`track_group_paths`] and [`track_group_diagnostics`], which each shape their own answer. The
correction detector is different: it needs the raw frame (it reports track ids, then the same route sends
those tracks' geometry and their step scale), so what it needs from a group is the cells, not a summary.

It exists so the CANDIDATES and the LANES answer for the same cells. The timeline draws its lanes from
`track_group_paths`, which honours the picked population; the candidate list was reading `label_props` for
the whole segmentation, so ticking a population narrowed the PICTURE and not the RANKING — a candidate
naming a track outside the population is un-actionable, and the two counts the panel prints side by side
("23 candidates · 306 with gaps") were tallied over two different track sets.

**`nothing` for a POOLED group, and that is not a limitation to work around.** A `track_id` is unique only
within one (image, segmentation), so an op built from a pooled frame would carry an id naming two different
cells and would corrupt one of them. `pooled_track_frame` exists for the diagnostics battery because a
*statistic* can pool; an *edit* cannot. The caller then says the ranking is unavailable rather than showing
a wrong one — the timeline already treats a missing ranking as degraded-but-useful, since its own job (when
each track existed) needs no detector at all.
"""
function track_group_frame(g::TrackPlotGroup)
    length(g.sources) == 1 || return nothing
    src = only(g.sources)
    nrow(src.df) == 0 && return nothing
    (; df = src.df, spatial = g.spatial, value_name = src.value_name)
end

"""
    track_group_diagnostics(g; max_lag, step_spacing) -> Union{Nothing,NamedTuple}

One group's celltrackR battery — [`track_diagnostics`] over the group's sources POOLED through
[`pooled_track_frame`], so a condition is judged on all of its replicates at once and the O(n²) pair scan
still never pairs two tracks from different movies.
"""
function track_group_diagnostics(g::TrackPlotGroup; max_lag::Int = 10,
                                 step_spacing::Int = DRIFT_STEP_SPACING)
    df = pooled_track_frame([s.df for s in g.sources])
    nrow(df) == 0 && return nothing
    track_diagnostics(df, g.spatial; max_lag = max_lag, step_spacing = step_spacing,
                      group_col = :__pool_grp)
end
