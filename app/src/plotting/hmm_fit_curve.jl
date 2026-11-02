# The HMM state-count comparison (docs/todo/HMM_FIT_QUALITY_PLAN.md, P3): every recorded HMM run across
# the selected images, grouped into FAMILIES of runs that differ only in their number of states, each
# family one curve of fit against K — the log-likelihood elbow. Read from the `{props}.hmmfit.json`
# records `behaviour.hmm_states` leaves (never refits), so it is a precomputed readout like the
# interaction matrix: no population selection, served through `plot_summary_data` (chartType "elbow").
#
# Only runs within a family are comparable: log-likelihood is of the PREPROCESSED observations, so a
# different population, measure set, noise filter or normalisation is a different curve, not a point
# on this one. Information criteria are reported but do not choose K (Pohle et al. 2017,
# doi:10.1007/s13253-017-0283-8) — the reader looks for where the gain per extra state flattens.

# what makes two runs comparable — everything about the fit except numStates
_hmm_family_key(e::AbstractDict) = JSON3.write((
    pops = sort!(String[string(p) for p in get(e, "pops", String[])]),
    measures = String[string(m) for m in get(e, "modelMeasurements", String[])],
    prep = get(e, "preprocessing", nothing)))

_hmm_short_measure(m::AbstractString) = replace(String(m), r"^live\.(?:cell|track)\." => "")

_hmm_family_label(e::AbstractDict) =
    join(_hmm_short_measure.(String[string(m) for m in get(e, "modelMeasurements", String[])]), ", ") *
    " · " * join(String[string(p) for p in get(e, "pops", String[])], ", ")

"""
    hmm_fit_runs(imgs) -> Vector{Dict{String,Any}}

Every HMM fit record on `imgs` (all segmentations), one per run name — a set-scope run writes the same
record into every image it decoded, so a run seen on several images is one run; on a name clash the
most recent (`at`) wins. Each carries its `run` (colName). A record whose state column is no longer on
its table is skipped: rebuilding the table (re-measure, re-track) drops the column but leaves the
sidecar, and that run's states no longer exist anywhere.
"""
function hmm_fit_runs(imgs::AbstractVector{<:CciaImage})
    byrun = Dict{String,Dict{String,Any}}()
    for img in imgs, vn in img_value_names(img)
        props = img_label_props_path(img, vn)
        recs = read_props_sidecar(hmm_fit_path(props))
        isempty(recs) && continue
        cols = isfile(props) ? Set(col_names(label_props(props); data_type = :obs)) : Set{String}()
        for (run, e) in recs
            (e isa AbstractDict && (_HMM_STATE_PREFIX * run) in cols) || continue
            d = Dict{String,Any}(String(k) => v for (k, v) in e); d["run"] = run
            old = get(byrun, run, nothing)
            (old === nothing || string(get(d, "at", "")) > string(get(old, "at", ""))) && (byrun[run] = d)
        end
    end
    sort!(collect(values(byrun)); by = d -> d["run"])
end

"""
    hmm_fit_curves(runs) -> Vector{Dict{String,Any}}

Group fit records into families (`_hmm_family_key`) and order each by numStates (PURE → unit-tested).
Per point: `k, run, logLik, aic, bic, converged, nObs`, and `gain` = the log-likelihood gained over the
family's previous state count (`nothing` for the first). When one family has two runs at the same K
both are kept as points; the gain is measured from the better of the previous K's.
"""
function hmm_fit_curves(runs::AbstractVector)
    fams = Dict{String,Vector{Dict{String,Any}}}(); order = String[]
    for e in runs
        get(e, "numStates", nothing) isa Real && get(e, "logLik", nothing) isa Real || continue
        k = _hmm_family_key(e)
        haskey(fams, k) || (fams[k] = Dict{String,Any}[]; push!(order, k))
        push!(fams[k], e)
    end
    curves = Dict{String,Any}[]
    for key in order
        es = sort(fams[key]; by = e -> (Int(e["numStates"]), string(e["run"])))
        best = Dict{Int,Float64}()
        for e in es
            k = Int(e["numStates"]); best[k] = max(get(best, k, -Inf), Float64(e["logLik"]))
        end
        ks = sort!(collect(keys(best)))
        prev = Dict(ks[i] => ks[i-1] for i in 2:length(ks))
        pts = [Dict{String,Any}(
                   "k" => Int(e["numStates"]), "run" => e["run"], "logLik" => Float64(e["logLik"]),
                   "aic" => get(e, "aic", nothing), "bic" => get(e, "bic", nothing),
                   "converged" => get(e, "converged", nothing), "nObs" => get(e, "nObs", nothing),
                   "gain" => haskey(prev, Int(e["numStates"])) ?
                       Float64(e["logLik"]) - best[prev[Int(e["numStates"])]] : nothing)
               for e in es]
        push!(curves, Dict{String,Any}("key" => key, "label" => _hmm_family_label(first(es)),
                                       "modelMeasurements" => get(first(es), "modelMeasurements", String[]),
                                       "pops" => get(first(es), "pops", String[]), "points" => pts))
    end
    curves
end

# The /api/plot_data response for chartType "elbow". `series` stays empty (not a pop_df aggregation);
# the curves carry the data.
hmm_fit_curve_data(imgs::AbstractVector{<:CciaImage}) =
    Dict{String,Any}("chartType" => "elbow", "measure" => "logLik", "granularity" => "cell",
                     "curves" => hmm_fit_curves(hmm_fit_runs(imgs)), "series" => Any[])
