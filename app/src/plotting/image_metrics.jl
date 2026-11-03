# ── Per-image metrics from analysis sidecars → summary plots, grouped by image attribute ─────────
#
# docs/todo/SPATIAL_PATTERN_PLAN.md Decision 8. Some readouts are ONE number per image (per population
# or per population pair) that a task already wrote to a sidecar: a spatial-pattern run's NN ratio, a
# neighbourStats run's contact log-odds. `pop_df` yields per-cell/per-track rows, so no popType can carry
# them, and the summary plots could not compare them between groups of images. This turns a run's
# sidecars into a frame shaped like `pop_df`'s output — one row per (image, population), the metric as a
# column — and hands it to the SAME `_summary_agg` every summary plot uses. Grouping by image attribute
# (`attr_map`), the box/strip/bar charts, the between-group tests (`stats.jl`) and the raw CSV export are
# therefore the existing ones, with the image as the statistical unit by construction.
#
# A new source is one entry in `IMAGE_METRIC_SOURCES`: how to list its runs and how to read one run's
# rows.

using DataFrames: DataFrame, nrow

# one run's rows: [(pop, Dict(measure => value))]. Missing / null values become NaN, which the
# aggregation drops (`_finite`), so an untested population is a gap rather than a zero.
_metric_value(v) = (v isa Real && isfinite(v)) ? Float64(v) : NaN

function _pattern_metric_rows(img::CciaImage, suffix::AbstractString)
    runs = _collect_pattern_stats(img)
    s = findfirst(r -> r.suffix == suffix, runs)
    s === nothing && return Tuple{String,Dict{String,Float64}}[]
    run = runs[s]
    [(String(r[:pop]), Dict{String,Float64}(m => _metric_value(get(r, Symbol(m), nothing))
                                            for m in IMAGE_METRIC_SOURCES["spatialPattern"].measures))
     for r in run.records]
end

function _contact_metric_rows(img::CciaImage, suffix::AbstractString)
    stats = _collect_contact_stats(img)
    s = findfirst(r -> r.suffix == suffix, stats)
    s === nothing && return Tuple{String,Dict{String,Float64}}[]
    [("$(p.popA) × $(p.popB)", Dict{String,Float64}("logOdds" => _metric_value(p.logOdds),
                                                    "zScore" => _metric_value(p.zScore)))
     for p in stats[s].pairs]
end

"""
    ImageMetricSource(suffixes, rows, measures)

One per-image metric source: `suffixes(img)` lists the image's runs, `rows(img, suffix)` reads one run as
`(pop, metrics)` pairs, and `measures` is the allow-list a request may plot. Same shape as `RunKind`.
"""
struct ImageMetricSource
    suffixes::Function
    rows::Function
    measures::Vector{String}
end

"""
    IMAGE_METRIC_SOURCES

The per-image metric sources a summary plot can group by image attribute (`dataSource.imageMetric` in a
plot spec), one `ImageMetricSource` each.
"""
const IMAGE_METRIC_SOURCES = Dict{String,ImageMetricSource}(
    "spatialPattern" => ImageMetricSource(img_pattern_suffixes, _pattern_metric_rows,
                                          ["nnRatio", "ceR", "zScore", "nnObserved"]),
    "neighbourStats" => ImageMetricSource(img_stats_suffixes, _contact_metric_rows,
                                          ["logOdds", "zScore"]),
)

# The run to plot: the requested one when some image has it, else the first run present on EVERY image
# (a run missing on some images would silently drop them), else the first run on any — so a stale pick
# saved on a board falls back rather than drawing nothing. Returns (suffix, all suffixes).
function _image_metric_run(imgs, src, suffix::AbstractString)
    per = [Set(src.suffixes(img)) for img in imgs]
    every = isempty(per) ? String[] : sort!(collect(reduce(intersect, per)))
    any_ = sort!(collect(reduce(union, per; init = Set{String}())))
    chosen = String(suffix) in any_ ? String(suffix) :
             !isempty(every) ? first(every) : (isempty(any_) ? "" : first(any_))
    (chosen, any_)
end

"""
    image_metric_frame(imgs, uids, source; suffix="") -> (df, suffix, suffixes)

One row per (image, population) of run `suffix` of `source` (a key of `IMAGE_METRIC_SOURCES`): columns
`uID`, `value_name` (""), `pop` and one column per metric. An image without that run contributes no rows.
"""
function image_metric_frame(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector, source::AbstractString;
                            suffix::AbstractString = "")
    src = IMAGE_METRIC_SOURCES[String(source)]
    chosen, suffixes = _image_metric_run(imgs, src, suffix)
    cols = Dict{String,Vector{Float64}}(m => Float64[] for m in src.measures)
    uid_col, pop_col = String[], String[]
    for (img, uid) in zip(imgs, uids), (pop, vals) in src.rows(img, chosen)
        push!(uid_col, String(uid)); push!(pop_col, pop)
        for m in src.measures; push!(cols[m], get(vals, m, NaN)); end
    end
    df = DataFrame(uID = uid_col, value_name = fill("", length(uid_col)), pop = pop_col)
    for m in src.measures; df[!, m] = cols[m]; end
    (df, chosen, suffixes)
end

"""
    image_metric_summary(imgs, uids, source, chart_type; measure, suffix="", pop="", attr_map=nothing,
                         stats_enabled=false, stats_test=:auto, raw=false) -> Dict

The summary-plot response for a per-image metric: `chart_type` ∈ boxplot | points | bar, each dot one
image. With `attr_map` (uID → attribute value) the series are attribute groups per population and the
between-group test compares images; without it, each population pools its images into one series.
`pop` plots one of the run's populations (the response lists them as `pops`), so the test compares
attribute groups within that population rather than across populations.
"""
function image_metric_summary(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector,
                              source::AbstractString, chart_type::AbstractString;
                              measure::AbstractString, suffix::AbstractString = "",
                              pop::AbstractString = "",
                              attr_map::Union{Nothing,AbstractDict} = nothing,
                              stats_enabled::Bool = false, stats_test::Symbol = :auto,
                              raw::Bool = false)::Dict{String,Any}
    df, chosen, suffixes = image_metric_frame(imgs, uids, source; suffix = suffix)
    pops = unique(df.pop)
    # a population the run lacks (a stale pick) falls back to the first; "" plots them all
    chosen_pop = isempty(pop) ? "" : (String(pop) in pops ? String(pop) : (isempty(pops) ? "" : first(pops)))
    isempty(chosen_pop) || (df = df[df.pop .== chosen_pop, :])
    result = _summary_agg(df, chart_type; measure = String(measure), granularity = :cell, nbins = 30,
                          normalize = :none, by_image = attr_map !== nothing, attr_map = attr_map,
                          raw_points = true, raw = raw,
                          stats_enabled = stats_enabled, stats_test = stats_test)
    result["imageMetric"] = String(source)
    result["suffix"] = chosen
    result["suffixes"] = suffixes
    result["pops"] = pops
    result["pop"] = chosen_pop
    result["nImages"] = length(unique(df.uID))
    result
end
