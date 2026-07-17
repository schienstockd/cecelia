# Cohort QC — aggregate the per-image objective metrics banked by `write_qc` (qc.jl) across a
# CciaSet, so a run producing far fewer cells/tracks than its cohort surfaces as an outlier. This is
# the cohort-relative layer QC-PROCESS.md §3 calls for: it READS through `read_qc` (never recomputes
# a metric), aggregates over the set's INCLUDED images, and writes a per-set summary sidecar
#   {proj}/1/{set_uid}/qc/cohort/{funName}/{valueName}.json
# — the source for the morning summary and the MCP get_qc_metrics cohort view.
#
# Advisory only, like the rest of qc.jl: cohort stats never block a task. Cohort membership = the
# set's `included` images; fewer than `_COHORT_MIN_N` values ⇒ stats reported, nothing flagged (you
# can't call an outlier without a cohort to compare against). Recompute-from-current-data on demand,
# so there is no stale state to invalidate when images are added/excluded/re-processed.

using Statistics: mean, std, median

const _COHORT_MIN_N = 3
# Modified z-score cutoff (Iglewicz & Hoaglin 1993, "How to Detect and Handle Outliers"): |Mᵢ| > 3.5
# is their recommended default. Robust — a single outlier doesn't inflate the scale and hide itself
# the way mean/SD does, so a clear anomaly flags even at n=3 (mean/SD needs n≥6 for a lone point to
# reach 2σ). See docs/ai-assist/QC-PROCESS.md §3.
const _COHORT_MODZ_THRESHOLD = 3.5

# The objective metrics banked today (segment/measure/tracking tasks → qc.jl). Drives the default
# cohort pass so a caller need only name the (fun, value_name) to aggregate. A second producer just
# adds an entry here.
const COHORT_METRICS = Dict{String,Vector{String}}(
    "segment.cellpose"           => ["nCells"],
    "segment.measureLabels"      => ["nCells"],
    "tracking.bayesian_tracking" => ["nTracks", "meanTrackLength", "nTrackedCells"],
)

# Pure: robust outlier detection via the modified z-score (median/MAD). For each value,
#   Mᵢ = 0.6745·(xᵢ − median) / MAD,   MAD = median(|xᵢ − median|)
# flag `abs(Mᵢ) ≥ threshold`. Robust — one bad image doesn't inflate the scale and mask itself, so a
# clear outlier flags even at n=3. If MAD == 0 (≥ half the values identical) fall back to the
# mean-absolute-deviation scale; all-identical ⇒ nothing flagged. Returns
# (; n, median, mad, mean, sd, outliers), outliers = Dict(uid => Dict("value"=>, "z"=> modified-z)).
# `mean`/`sd` are reported for human context; flagging is the robust `z`. Unit-tested directly.
function _cohort_outliers(values_by_uid::AbstractDict, threshold::Real = _COHORT_MODZ_THRESHOLD)
    uids = collect(keys(values_by_uid))
    vals = Float64[values_by_uid[u] for u in uids]
    n = length(vals)
    n == 0 && return (; n = 0, median = 0.0, mad = 0.0, mean = 0.0, sd = 0.0, outliers = Dict{String,Any}())
    med = median(vals); μ = mean(vals)
    sd  = n < 2 ? 0.0 : std(vals)
    n < _COHORT_MIN_N &&
        return (; n = n, median = round(med; digits = 3), mad = 0.0,
                  mean = round(μ; digits = 3), sd = round(sd; digits = 3), outliers = Dict{String,Any}())
    mad = median(abs.(vals .- med))
    modz = if mad > 0
        [0.6745 * (v - med) / mad for v in vals]
    else
        meanad = mean(abs.(vals .- med))          # MAD==0 (≥half identical) → mean-abs-dev fallback
        meanad > 0 ? [(v - med) / (1.253314 * meanad) for v in vals] : zeros(n)
    end
    outliers = Dict{String,Any}()
    for (u, v, m) in zip(uids, vals, modz)
        abs(m) >= threshold &&
            (outliers[string(u)] = Dict{String,Any}("value" => v, "z" => round(m; digits = 2)))
    end
    (; n = n, median = round(med; digits = 3), mad = round(mad; digits = 3),
       mean = round(μ; digits = 3), sd = round(sd; digits = 3), outliers = outliers)
end

# Read one banked metric value from an image's (fun, vn) QC doc; `nothing` if absent/non-numeric.
function _read_metric(img::CciaImage, fun_name, value_name, metric_key)
    doc = read_qc(img, fun_name, value_name)
    isnothing(doc) && return nothing
    m = get(doc, :metrics, nothing)
    (m isa AbstractDict) || return nothing
    v = get(m, Symbol(metric_key), nothing)
    (v isa Real && !(v isa Bool)) ? Float64(v) : nothing
end

cohort_qc_dir(set::CciaSet) = joinpath(set._dir, QC_DIRNAME, "cohort")
cohort_qc_path(set::CciaSet, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    joinpath(cohort_qc_dir(set), string(fun_name), _qc_vn(value_name) * ".json")

"""
    cohort_qc!(set, fun_name, value_name, metric_keys; threshold=3.5) -> Dict

Aggregate the named banked metrics across the set's INCLUDED images and write the cohort summary
sidecar. Returns the summary doc `{funName, valueName, nIncluded, metrics}` where each metric maps to
`{n, median, mad, mean, sd, threshold, outliers}` (outliers by robust modified z-score; `threshold`
is its cutoff). Advisory; recomputes from current data so it's safe to call any time.
"""
function cohort_qc!(set::CciaSet, fun_name::AbstractString, value_name::AbstractString,
                    metric_keys::AbstractVector; threshold::Real = _COHORT_MODZ_THRESHOLD)
    imgs = filter(image_included, images(set))
    metrics = Dict{String,Any}()
    for mk in metric_keys
        vals = Dict{String,Float64}()
        for img in imgs
            v = _read_metric(img, fun_name, value_name, mk)
            isnothing(v) || (vals[img.uid] = v)
        end
        s = _cohort_outliers(vals, threshold)
        metrics[string(mk)] = Dict{String,Any}(
            "n" => s.n, "median" => s.median, "mad" => s.mad, "mean" => s.mean, "sd" => s.sd,
            "threshold" => Float64(threshold), "outliers" => s.outliers)
    end
    doc = Dict{String,Any}("funName" => string(fun_name), "valueName" => _qc_vn(value_name),
                           "nIncluded" => length(imgs), "metrics" => metrics)
    path = cohort_qc_path(set, fun_name, value_name); mkpath(dirname(path))
    open(path, "w") do io; JSON3.write(io, doc); end
    doc
end

"""
    cohort_qc_for!(set, fun_name, value_name="default"; threshold=3.5) -> Dict

Convenience: run `cohort_qc!` for the known metrics of `fun_name` (`COHORT_METRICS`). Errors if the
fun isn't a known metric producer.
"""
function cohort_qc_for!(set::CciaSet, fun_name::AbstractString,
                        value_name::AbstractString = VERSIONED_DEFAULT_VAL;
                        threshold::Real = _COHORT_MODZ_THRESHOLD)
    ks = get(COHORT_METRICS, string(fun_name), nothing)
    isnothing(ks) &&
        error("No known cohort metrics for fun '$fun_name' (known: $(join(sort(collect(keys(COHORT_METRICS))), ", ")))")
    cohort_qc!(set, fun_name, value_name, ks; threshold = threshold)
end

read_cohort_qc(set::CciaSet, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (p = cohort_qc_path(set, fun_name, value_name); isfile(p) ? JSON3.read(read(p, String)) : nothing)

# All cohort docs for a set, keyed "funName/valueName" → parsed doc (mirrors read_all_qc).
function read_all_cohort_qc(set::CciaSet)
    root = cohort_qc_dir(set); out = Dict{String,Any}()
    isdir(root) || return out
    for fun in readdir(root)
        fdir = joinpath(root, fun); isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") || continue
            out[string(fun, "/", f[1:end-5])] = JSON3.read(read(joinpath(fdir, f), String))
        end
    end
    out
end
