# Measure summaries for the read-only observer (Slice C of docs/todo/OBSERVER_DATA_ACCESS_PLAN.md).
# Phenotype (per-cell channel intensities + morphology) and motility (per-track speed/displacement/…)
# summarised PER POPULATION — because the meaningful signal is the analysed cells, NOT the raw
# segmentation (in a gated project most labels are gated out). So:
#   - gated pops present (e.g. T/_qc) → summarise each: phenotype over its cells, motility over its tracks
#   - not gated but tracked         → the base `_tracked` population (all tracked cells)
#   - not gated, static             → all cells
# Reuses the canonical `pop_df` accessor (mtime-cached) with column PUSHDOWN to a small measure set —
# never materialises the full table, never returns raw rows. No per-column summary helper exists, so we
# compute median/quantiles/mean over the finite values, mirroring the boxplot builder (plot_data.jl).

using Statistics: mean, median, quantile

# Curated morphology measures (regionprops) — the ones worth a summary; intersected with what's present.
const _MEASURE_MORPHOLOGY = ["area", "eccentricity", "perimeter", "solidity", "aspect_ratio",
                             "major_axis_length", "minor_axis_length", "extent", "convex_area"]
# pop_df output columns that are identity/metadata, not measures.
const _MEASURE_META_COLS = Set(["label", "pop", "value_name", "uID", "track_id"])
const _MEASURE_TARGET_CAP = 40   # population×granularity summaries per image (hard cap); `truncated` flags a cut

_finite_reals(v) = Float64[Float64(x) for x in v if (x isa Real) && !(x isa Bool) && isfinite(x)]

# One measure column → its summary (nothing when no finite values). q25/q75 bracket the median. Values
# rounded to 4 significant digits and `mean` dropped (median + quantiles are the headline) to keep the
# per-image×pop×measure payload small — set-scoped calls returned ~80k tokens otherwise.
function _summarise_measure(name, vals)
    f = _finite_reals(vals); isempty(f) && return nothing
    r(x) = round(x; sigdigits = 4)
    (; name = string(name), n = length(f),
       median = r(median(f)), q25 = r(quantile(f, 0.25)), q75 = r(quantile(f, 0.75)))
end

# The pushdown columns for a (value_name, granularity), read from the var list (col_names is
# metadata-only). Intensity columns are requested BY CHANNEL NAME, so the central raw↔channel resolution
# in `label_props`/`pop_df` returns them under a consistent channel name across pop_types — no local
# rename map (that used to be hand-rolled here because pop_df's rename once varied by pop_type).
function _measure_cols(img::CciaImage, vn::AbstractString, gran::Symbol)::Vector{String}
    if gran === :track
        p = img_track_props_path(img, vn); isfile(p) || return String[]
        String[c for c in col_names(label_props(p); data_type = :vars) if startswith(c, "live.track.")]
    else
        lp = label_props(img; value_name = vn); isfile(lp.path) || return String[]
        vars  = Set(col_names(lp; data_type = :vars))
        chans = try channel_columns(lp; as_channel_names = true) catch; channel_columns(lp) end
        unique(vcat(chans, String[m for m in _MEASURE_MORPHOLOGY if m in vars]))
    end
end

# Populations grouped by their segmentation (value_name), from the persisted gating maps.
function _gated_by_value_name(img::CciaImage)
    d = Dict{String,Vector{Population}}()
    _observer_each_population(img) do p
        push!(get!(d, p.value_name, Population[]), p)
    end
    d
end

# The (label, pop_type, path, granularity, kind) summary targets for one segmentation: its gated pops
# when present (phenotype for cell pops, + motility when tracked; motility for track pops), else the
# base populations (all cells, and the tracked base when tracked).
function _vn_targets(img::CciaImage, vn::AbstractString, gated::AbstractVector)
    tracked = is_tracked(img; value_name = vn)
    T = Tuple{String,String,String,Symbol,String}[]
    if isempty(gated)
        push!(T, ("all cells", "labels", "/labels", :cell, "phenotype"))
        if tracked
            push!(T, ("tracked", "live", "/_tracked", :cell, "phenotype"))
            push!(T, ("tracked", "live", "/_tracked", :track, "motility"))
        end
    else
        for p in gated
            if p.is_track
                push!(T, (p.path, p.pop_type, p.path, :track, "motility"))
            else
                push!(T, (p.path, p.pop_type, p.path, :cell, "phenotype"))
                tracked && push!(T, (p.path, "live", p.path, :track, "motility"))
            end
        end
    end
    T
end

# Run one target through pop_df (pushdown) → per-measure summaries. Any read error (e.g. a base pop that
# doesn't resolve on this image) degrades to nothing, never fails the whole call.
function _target_summary(img::CciaImage, vn::AbstractString, tgt, cache)
    label, pop_type, path, gran, kind = tgt
    cols = get!(() -> _measure_cols(img, vn, gran), cache, (vn, gran))
    isempty(cols) && return nothing
    # pop_cols are channel names — the reader resolves them to raw and returns them under the channel
    # name, so output naming is consistent across pop_types with no local rename (the point of the
    # central raw↔channel resolution).
    df = try
        pop_df(img, pop_type, [path]; value_name = vn, granularity = gran, pop_cols = cols)
    catch
        return nothing
    end
    nrow(df) == 0 && return nothing
    ms = Any[]
    for c in names(df)
        c in _MEASURE_META_COLS && continue
        s = _summarise_measure(c, df[!, c]); s === nothing || push!(ms, s)
    end
    isempty(ms) && return nothing
    (; population = label, valueName = vn, kind = kind, n = nrow(df), measures = ms)
end

# Per-image builder: measure summaries across the image's meaningful populations, capped.
function _measures_image(img::CciaImage)
    gated = _gated_by_value_name(img)
    cache = Dict{Tuple{String,Symbol},Vector{String}}()
    out = Any[]; truncated = false
    for vn in sort(img_value_names(img))
        for tgt in _vn_targets(img, vn, get(gated, vn, Population[]))
            length(out) >= _MEASURE_TARGET_CAP && (truncated = true; break)
            s = _target_summary(img, vn, tgt, cache)
            s === nothing || push!(out, s)
        end
        truncated && break
    end
    (; _observer_image_header(img)..., summaries = out, truncated = truncated)
end

"""
    measure_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image, phenotype + motility summaries (median/quantiles/mean/n) over the meaningful populations —
gated pops when present, else the base tracked/all-cells population. Scoped to one `image_uid`/`set_uid`
or the whole project. Summary-level only; reuses `pop_df` with column pushdown. Heavier than the
lineage/populations reads (it touches cell data), so prefer scoping to an image/set. Slice C of
OBSERVER_DATA_ACCESS_PLAN.md.
"""
measure_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "") =
    observer_image_summary(proj, _measures_image; image_uid = image_uid, set_uid = set_uid)
