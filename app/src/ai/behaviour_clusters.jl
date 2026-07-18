# Behaviour + cluster summaries for the read-only observer (Slice D of OBSERVER_DATA_ACCESS_PLAN.md).
# Two tools, one file (shared distribution helper):
#   behaviour_summary — HMM state distribution (fraction per state) + transition counts, from the
#     per-cell obs columns `live.cell.hmm.state.*` / `live.cell.hmm.transitions.*`.
#   cluster_summary   — per clustering run (`clusters.{suffix}`): n clusters, sizes, largest fraction,
#     the feature list — cell obs for clustPops, track obs for clustTracks.
# READ-ONLY, summary-level. Reads obs through the canonical `pop_df` (mtime-cached) — distributions,
# never raw rows. `category_dist_metrics` (qc.jl) only returns aggregates, so we compute the per-category
# breakdown here (same null/NaN filtering).

# A categorical obs column → its distribution: total n (non-null), distinct count, and the top-N
# categories by frequency (`value, n, fraction`). `cap` bounds the list (transitions can be many).
function _category_distribution(vals; cap::Int = 50)
    counts = Dict{Any,Int}(); n = 0
    for v in vals
        (v === nothing || ismissing(v)) && continue
        (v isa Real && isnan(v)) && continue
        counts[v] = get(counts, v, 0) + 1; n += 1
    end
    n == 0 && return (; n = 0, nDistinct = 0, top = NamedTuple[])
    ranked = sort(collect(counts); by = last, rev = true)
    top = [(; value = string(k), n = c, fraction = round(c / n; digits = 4))
           for (k, c) in ranked[1:min(end, cap)]]
    (; n = n, nDistinct = length(counts), top = top)
end

# An h5ad's obs table (label + all obs, no X) via the canonical reader — the HMM/cluster columns are obs,
# and pop_df's "labels" pop DROPS obs while its gated paths add cost/quirks we don't need for a whole-
# column distribution. obs is narrow, so a direct read is both correct and cheap. nothing if absent.
_read_obs(path::AbstractString) =
    isfile(path) ? (try as_df(label_props(path); include_x = false, include_obs = true) catch; nothing end) : nothing

# One image's HMM behaviour: the distribution of each state column + the transition counts. HMM runs on
# tracked cells, so untracked cells carry a missing state and drop out of the distribution (⇒ n = decoded).
function _behaviour_image(img::CciaImage)
    out = Any[]
    for vn in sort(img_value_names(img))
        df = _read_obs(img_label_props_path(img, vn)); df === nothing && continue
        for c in names(df)
            if startswith(c, "live.cell.hmm.state.")
                d = _category_distribution(df[!, c]); d.n == 0 && continue
                push!(out, (; valueName = vn, kind = "state", column = c,
                             n = d.n, nStates = d.nDistinct, distribution = d.top))
            elseif startswith(c, "live.cell.hmm.transitions.")
                d = _category_distribution(df[!, c]; cap = 15); d.n == 0 && continue
                push!(out, (; valueName = vn, kind = "transitions", column = c,
                             n = d.n, nDistinct = d.nDistinct, distribution = d.top))
            end
        end
    end
    (; _observer_image_header(img)..., behaviour = out)
end

# The feature list a clustering run was computed on, from the `.clustfeatures.json` sidecar.
function _clustfeatures_features(props_path::AbstractString, suffix::AbstractString)::Vector{String}
    p = _clustfeatures_path(props_path); isfile(p) || return String[]
    d = try JSON3.read(read(p, String)) catch; return String[] end
    e = get(d, Symbol(suffix), nothing); (e isa AbstractDict) || return String[]
    f = get(e, :features, nothing)
    f isa AbstractVector ? String[string(x) for x in f] : String[]
end

# Collect the `clusters.{suffix}` distributions from one table's obs (cell props = clustPops, track props
# = clustTracks) into `out`. The run's feature list is the SAME for a suffix across every image, so it's
# recorded ONCE into `feats` (returned as a top-level `featuresByRun` map) rather than repeated on every
# entry — that repetition was the bulk of a set-scoped payload.
function _collect_cluster_cols!(out, feats::AbstractDict, vn::AbstractString, props::AbstractString, gran::Symbol)
    df = _read_obs(props); df === nothing && return
    for c in names(df)
        startswith(c, "clusters.") || continue
        d = _category_distribution(df[!, c]); d.n == 0 && continue
        suffix = c[ncodeunits("clusters.")+1:end]
        haskey(feats, suffix) || (f = _clustfeatures_features(props, suffix); isempty(f) || (feats[suffix] = f))
        push!(out, (; valueName = vn, suffix = suffix, granularity = string(gran),
                     nClusters = d.nDistinct, n = d.n,
                     largestFrac = isempty(d.top) ? 0.0 : d.top[1].fraction, sizes = d.top))
    end
end

# One image's clustering runs: cell-level (clustPops) + track-level (clustTracks) cluster distributions.
function _cluster_image(img::CciaImage, feats::AbstractDict)
    out = Any[]
    for vn in sort(img_value_names(img))
        _collect_cluster_cols!(out, feats, vn, img_label_props_path(img, vn), :cell)
        _collect_cluster_cols!(out, feats, vn, img_track_props_path(img, vn), :track)
    end
    (; _observer_image_header(img)..., clusters = out)
end

"""
    behaviour_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image, the HMM behaviour distribution — fraction per state and transition counts — over the tracked
cells. Reads the `live.cell.hmm.*` obs columns via `pop_df`. Slice D of OBSERVER_DATA_ACCESS_PLAN.md.
"""
behaviour_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "") =
    observer_image_summary(proj, _behaviour_image; image_uid = image_uid, set_uid = set_uid)

"""
    cluster_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image, each clustering run (`clusters.{suffix}`): n clusters, cluster sizes, largest fraction (cell
obs for clustPops, track obs for clustTracks) — plus a top-level `featuresByRun` (`{suffix => features}`)
holding each run's feature list ONCE instead of on every per-image entry. Slice D of OBSERVER_DATA_ACCESS_PLAN.md.
"""
function cluster_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "")
    feats = Dict{String,Vector{String}}()
    imgs = [_cluster_image(img, feats) for img in _observer_scope_images(proj, image_uid, set_uid)]
    (; projectUid = proj.uid, images = imgs, featuresByRun = feats)
end
