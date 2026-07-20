# ── Summary-plot data aggregation ────────────────────────────────────────────────
#
# Server-side aggregation for the analysis-plot canvas (docs/UI.md plotting canvas). Vue's
# Vega-Lite panels render *summaries* — histogram bins, frequency counts — never raw cell arrays,
# so payloads stay tiny regardless of N (the big point clouds stay on regl-scatterplot). All data
# flows through the unified `pop_df` accessor, so a summary plot pools across populations and
# segmentations exactly like everything else (e.g. compare `live.track.speed` of A/B/C on one
# histogram), and `granularity=:track` gives the one-point-per-track view.
#
# Each requested population is one SERIES (its path encodes the value_name; the frontend colours it
# by the population's manager colour). Aggregation is pure over the `pop_df` frame and headless-
# testable — the API route (`/api/plot_data`) is a thin wrapper.

using DataFrames: DataFrame, nrow, groupby, propertynames
using Statistics: mean, median, std, quantile

# Equal-width bin edges over the finite values; `nbins+1` edges, or empty when there's no data.
function _hist_edges(vals, nbins::Int)::Vector{Float64}
    finite = Float64[Float64(v) for v in vals if v isa Real && isfinite(v)]
    isempty(finite) && return Float64[]
    lo, hi = minimum(finite), maximum(finite)
    hi <= lo && (hi = lo + 1.0)                      # degenerate (single value) → 1-wide bin
    [lo + (hi - lo) * i / nbins for i in 0:nbins]
end

# Count finite values into the given edges (last bin is closed on the right).
function _hist_counts(vals, edges::Vector{Float64})::Vector{Int}
    n = length(edges) - 1
    counts = zeros(Int, max(n, 0))
    n <= 0 && return counts
    lo = edges[1]; w = (edges[end] - lo) / n
    for v in vals
        (v isa Real && isfinite(v)) || continue
        b = clamp(Int(floor((Float64(v) - lo) / w)) + 1, 1, n)
        counts[b] += 1
    end
    counts
end

# category key for a categorical value: integers render as "1"/"2"/… (hmm.state, generation),
# everything else stringified. obs columns are Float64, so integer states arrive as 1.0/2.0/…
_catkey(v) = (v isa Real && isfinite(v) && v == round(v)) ? string(Int(round(v))) : string(v)

# sort category keys numerically when they're all numbers, else lexically (stable category axis)
function _sort_cats(keys::Vector{String})::Vector{String}
    nums = tryparse.(Float64, keys)
    all(!isnothing, nums) ? keys[sortperm(Float64.(nums))] : sort(keys)
end

# Split a pop_df frame into one series per distinct group, in first-appearance order. The group key
# is `(value_name, pop)` — plus `uID` when `by_image=true` and a `uID` column is present (set-level
# pooling), so each image becomes its own series for cross-image comparison; with `by_image=false`
# the images are pooled (uID ignored). `pop_df` stores the pop PATH (e.g. "/_tracked") and the
# value_name SEPARATELY, so pooled requests share the path and differ only by value_name — hence the
# pair key. The series id is rebuilt in the manager's prefixed form (value_name + path → "B/_tracked").
#
# `group_col` adds a GENERIC categorical sub-axis: each (uid, vn, pop) is split again by the distinct
# levels of that column, and the level lands in the `grp` field. This ports the old behaviour
# "hmmPlotParams" (a measure split by HMM state) but is column-agnostic — any categorical obs column
# (state, transitions, generation, a cluster id, …) works. Rows where `group_col` is missing/NaN are
# dropped (mirrors R `drop_na`). Returns NamedTuples `(uid, vn, sid, grp, sub)`; `uid`/`grp` are ""
# when not grouping by image / not group-splitting.
# `attr_map` (uID → attribute value, e.g. "Treatment") groups cross-image series by that ATTRIBUTE
# instead of by image: each image's uID is mapped to its attribute value, so images sharing a value
# pool into one series labelled by the value (the "compare by attribute" scope). Images with no value
# fall back to their uID. Pure relabelling of the image key — per-track aggregation (done upstream in
# `pop_df`) is unaffected. `nothing` → group by image as before.
function _series_groups(df::DataFrame; by_image::Bool=false,
                        group_col::Union{Nothing,AbstractString}=nothing,
                        collapse::Bool=false,
                        attr_map::Union{Nothing,AbstractDict}=nothing)
    out = NamedTuple[]
    (nrow(df) == 0 || !(:pop in propertynames(df))) && return out
    has_vn  = :value_name in propertynames(df)
    has_uid = by_image && (:uID in propertynames(df))
    # image key per row: the raw uID, or its attribute value when grouping by attribute.
    imgkeys = has_uid ?
        (attr_map === nothing ? String[String(u) for u in df.uID] :
         String[string(get(attr_map, String(u), String(u))) for u in df.uID]) : String[]
    gcol = (group_col !== nothing && String(group_col) in names(df)) ? String(group_col) : nothing
    # per-row group key ("" placeholder unused); `nothing` for missing/NaN rows (dropped from grouping)
    gvals = gcol === nothing ? nothing :
            Union{Nothing,String}[(ismissing(v) || (v isa Real && !isfinite(v))) ? nothing : _catkey(v)
                                  for v in df[!, gcol]]
    # COLLAPSE: pool across pop/segmentation/image — series form ONLY by the groupBy level (or a
    # single pooled series when there's no groupBy). The "no separation by population or image" view:
    # one series per groupBy level over every selected population and image (generic — any categorical).
    if collapse
        if gcol === nothing
            return [(uid="", vn="", sid="", grp="", sub=df)]
        end
        levels = String[]; seenl = Set{String}()
        for g in gvals; (g === nothing || g in seenl) || (push!(levels, g); push!(seenl, g)); end
        for lvl in _sort_cats(levels)
            push!(out, (uid="", vn="", sid="", grp=lvl, sub=df[gvals .=== lvl, :]))
        end
        return out
    end
    order = Tuple{String,String,String,String}[]; seen = Set{Tuple{String,String,String,String}}()
    for i in 1:nrow(df)
        grp = gcol === nothing ? "" : gvals[i]
        grp === nothing && continue                      # drop missing-group rows
        k = (has_uid ? imgkeys[i] : "",
             has_vn  ? String(df.value_name[i]) : "", String(df.pop[i]), grp)
        (k in seen) || (push!(order, k); push!(seen, k))
    end
    for (uid, vn, pop, grp) in order
        mask = trues(nrow(df))
        has_uid && (mask .&= (imgkeys .== uid))
        has_vn  && (mask .&= (df.value_name .== vn))
        mask .&= (df.pop .== pop)
        gcol === nothing || (mask .&= (gvals .=== grp))   # === so `nothing` rows never match a level
        sid = (!isempty(vn) && startswith(pop, "/")) ? vn * pop : pop
        push!(out, (uid=uid, vn=vn, sid=sid, grp=grp, sub=df[mask, :]))
    end
    out
end

# finite Float64 values of a measure column
_finite(vals) = Float64[Float64(v) for v in vals if v isa Real && isfinite(v)]

# Evenly-strided downsample to ≤ cap values (keeps the distribution shape; deterministic, so it is
# test-stable and resume-safe — no RNG). Used for raw-point overlays (boxplot/violin) + strip charts,
# so payloads stay bounded regardless of N (docs/PLOTS.md §6.2).
function _downsample(vals::Vector{Float64}, cap::Int)::Vector{Float64}
    n = length(vals)
    (n <= cap || cap <= 0) && return vals
    vals[round.(Int, range(1, n; length = cap))]
end

# Morphology/intensity `var` columns are quantitative by construction — an integer-valued one
# (`euler_number`, voxel-count `area`) is STILL numeric. The categorical heuristic (`_is_categorical_col`)
# is only for `obs` columns (hmm.state, clusters, generation — written as anndata categoricals). So the
# measure type of a var column is always numeric; only an obs measure defers to detection. This is the
# structural rule that replaces the old R `config.yml parameters.labelStats` per-column map.
function _var_measure_set(img::CciaImage, value_name)::Set{String}
    vn = something(value_name, get(img.label_props, "_active", "default"))
    try
        Set(String.(col_names(label_props(img; value_name=vn); data_type=:vars)))
    catch
        Set{String}()
    end
end
_var_measure_set(imgs::AbstractVector{<:CciaImage}, value_name)::Set{String} =
    isempty(imgs) ? Set{String}() : _var_measure_set(first(imgs), value_name)

# POPULATION SUMMARY backbone: collapse a cell/track pop_df to ONE ROW PER (value_name, pop, uID),
# whose value is the population's COUNT — or, when `normalize`, its FRACTION of that image's plotted
# total (across the plotted pops; pooled over the whole set when there's no uID). Feeding these
# per-image rows to the normal distribution builders (boxplot/violin/strip/bar) makes each IMAGE a
# data point grouped by pop → within-pop variability + between-pop comparison (R popsSummary port).
const _POP_METRIC_COL = "__pop_metric"
function _population_metric_frame(df::DataFrame; normalize::Symbol=:none,
                                  col::AbstractString=_POP_METRIC_COL)::DataFrame
    has_uid = :uID in propertynames(df); has_vn = :value_name in propertynames(df)
    vn  = has_vn  ? String.(df.value_name) : fill("", nrow(df))
    pop = String.(df.pop)
    uid = has_uid ? String.(df.uID) : fill("", nrow(df))
    # count per (value_name, pop, uID), tracking first-appearance order of pops/images per segmentation.
    cnt = Dict{Tuple{String,String,String},Int}()
    vn_order = String[]; seen_vn = Set{String}()
    pop_order = Dict{String,Vector{String}}(); uid_order = Dict{String,Vector{String}}()
    for i in 1:nrow(df)
        v = vn[i]; p = pop[i]; u = uid[i]
        cnt[(v, p, u)] = get(cnt, (v, p, u), 0) + 1
        if !(v in seen_vn); push!(vn_order, v); push!(seen_vn, v); pop_order[v] = String[]; uid_order[v] = String[]; end
        p in pop_order[v] || push!(pop_order[v], p)
        u in uid_order[v] || push!(uid_order[v], u)
    end
    # COMPLETE CASES (R tidyr::complete / expand.grid): within each segmentation (value_name), every
    # population seen in ANY image gets a row for EVERY image that has that segmentation — a missing
    # (pop, image) is a genuine 0, not absent data. Without this, the boxplot/strip of a cluster's
    # per-image count/proportion silently DROPS the images lacking that cluster, biasing the
    # distribution (n too small, median/mean shifted up). Universe is derived per value_name (pops seen
    # under vn × images seen under vn), so we never fabricate a 0 for an image not segmented for that vn.
    order = Tuple{String,String,String}[]
    for v in vn_order, p in pop_order[v], u in uid_order[v]
        k = (v, p, u); haskey(cnt, k) || (cnt[k] = 0); push!(order, k)
    end
    # proportion denominator = the tracked population's total per image → keyed by (uID, value_name),
    # so clusters within B and within T are each normalised to their OWN population (not pooled B+T).
    frac = normalize in (:fraction, :total)
    tot = Dict{Tuple{String,String},Int}()   # (uid, vn) → total
    frac && for (k, n) in cnt; tk = (k[3], k[1]); tot[tk] = get(tot, tk, 0) + n; end
    vns = String[]; pops = String[]; uids = String[]; vals = Float64[]
    for k in order
        n = cnt[k]; push!(vns, k[1]); push!(pops, k[2]); push!(uids, k[3])
        d = frac ? get(tot, (k[3], k[1]), 0) : 0
        push!(vals, frac ? (d == 0 ? 0.0 : n / d) : Float64(n))
    end
    DataFrame("value_name" => vns, "pop" => pops, "uID" => uids, String(col) => vals)
end

# IMAGE as the statistical unit: collapse each image to ONE datapoint per series — the MEAN (or MEDIAN,
# per `agg`) of `measure` within each (value_name, pop, uID[, group-level]) group. Feeding these
# per-image summaries to the distribution builders (boxplot/strip/bar) makes each IMAGE a data point
# (n = #images) instead of each cell/track — "each dot is an image", the pseudoreplication-safe view
# where the image is the unit (biologists' n = animals/images, not cells). Rows with a missing/non-finite
# measure (or group level) are dropped. `group_col` is preserved so the caller can still split the
# resulting points by that level. `uID` absent (single image) → one row per (value_name, pop[, group]).
# Mirrors `_population_metric_frame`.
function _image_agg_frame(df::DataFrame, measure::AbstractString;
                          group_col::Union{Nothing,AbstractString}=nothing, agg::Symbol=:mean)::DataFrame
    m = String(measure)
    (nrow(df) == 0 || !(m in names(df))) && return DataFrame()
    reduce_fn = agg == :median ? median : mean
    has_uid = :uID in propertynames(df); has_vn = :value_name in propertynames(df)
    gcol = (group_col !== nothing && String(group_col) in names(df)) ? String(group_col) : nothing
    acc = Dict{NTuple{4,String},Vector{Float64}}(); order = NTuple{4,String}[]
    for i in 1:nrow(df)
        v = df[i, m]
        (ismissing(v) || !(v isa Real) || !isfinite(v)) && continue
        g = ""
        if gcol !== nothing
            gv = df[i, gcol]
            (ismissing(gv) || (gv isa Real && !isfinite(gv))) && continue
            g = _catkey(gv)
        end
        k = (has_vn ? String(df.value_name[i]) : "", String(df.pop[i]),
             has_uid ? String(df.uID[i]) : "", g)
        haskey(acc, k) || (acc[k] = Float64[]; push!(order, k))
        push!(acc[k], Float64(v))
    end
    vns = String[]; pops = String[]; uids = String[]; grps = String[]; vals = Float64[]
    for k in order
        push!(vns, k[1]); push!(pops, k[2]); push!(uids, k[3]); push!(grps, k[4]); push!(vals, reduce_fn(acc[k]))
    end
    out = DataFrame("value_name" => vns, "pop" => pops, "uID" => uids, m => vals)
    gcol === nothing || (out[!, gcol] = grps)
    out
end

# Shared aggregation core over an already-built pop_df frame. `by_image` → one series per source
# image (cross-image comparison); else images (if any) are pooled. Used by both the single-image
# and the multi-image `plot_summary_data` methods so the chart logic lives in one place.
function _summary_agg(df::DataFrame, chart_type::AbstractString;
                      measure::Union{AbstractString,Nothing}, granularity::Symbol,
                      nbins::Int, normalize::Symbol, by_image::Bool,
                      var_cols::Set{String}=Set{String}(),
                      group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                      raw_points::Bool=false, max_points::Int=1500, raw::Bool=false,
                      stat_unit::Symbol=:individual, image_agg::Symbol=:mean,
                      matrix_mode::Union{AbstractString,Nothing}=nothing,
                      measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                      category::Union{AbstractString,Nothing}=nothing,
                      separator::AbstractString="_", zscore::Bool=false,
                      matrix_normalize::Symbol=:none,
                      attr_map::Union{Nothing,AbstractDict}=nothing)::Dict{String,Any}
    # matrix/heatmap pools the whole frame into one grid — handled before the per-series charts below.
    if chart_type == "matrix"
        matrix_mode === nothing && error("plot_summary_data: matrix needs a `matrix_mode` (profile | crosstab)")
        ms = measures !== nothing ? String[String(m) for m in measures] :
             (measure === nothing ? String[] : String[String(measure)])
        return _matrix_agg(df; mode = String(matrix_mode), measures = ms, category = category,
                           separator = separator, zscore = zscore, normalize = matrix_normalize,
                           granularity = granularity)
    end
    # POPULATION SUMMARY — a distribution/bar chart with NO cell `measure`: each IMAGE becomes one data
    # point whose value is the pop's count (or its fraction of the image's plotted total). Pre-aggregate
    # to per-(value_name, pop, uID) rows, then run the normal distribution builder over those per-image
    # values (series = pop, points = images) → boxplot/violin/strip/beeswarm/bar show within-pop
    # variability and compare pops. `count` keeps its per-(pop, image) bar view (handled below).
    if measure === nothing && chart_type in ("boxplot", "violin", "strip", "points", "bar")
        # friendly axis label (also the df column name so the builders read it) — count vs proportion
        metric_label = normalize in (:fraction, :total) ? "proportion" : "count"
        df = _population_metric_frame(df; normalize=normalize, col=metric_label)
        measure = metric_label
        by_image = false           # pool images into the pop series as individual points
        normalize = :none
    elseif stat_unit == :image && measure !== nothing && chart_type in ("boxplot", "points", "bar")
        # IMAGE is the statistical unit: collapse each image to its per-series MEAN/MEDIAN, then plot
        # those per-image summaries (each dot = one image). Pool images into ONE series (by_image=false)
        # unless grouping by an image attribute — then keep one series per attribute value, points = its
        # images.
        df = _image_agg_frame(df, String(measure);
                              group_col = (group_by === nothing ? nothing : String(group_by)), agg = image_agg)
        by_image = attr_map !== nothing
    end
    # `group` is the optional categorical sub-axis level (e.g. an HMM state) — "" when not grouping.
    base(g) = Dict{String,Any}("pop" => g.sid, "value_name" => g.vn, "uID" => g.uid, "group" => g.grp)
    gb = group_by === nothing ? nothing : String(group_by)
    sgroups(d) = _series_groups(d; by_image=by_image, group_col=gb, collapse=collapse_series, attr_map=attr_map)
    withgb(r) = (r["groupBy"] = (gb === nothing ? nothing : gb); r)
    # detected measure type (numeric vs categorical) so the panel can offer only the applicable
    # chart types (docs/PLOTS.md §2). Auto-detection is shared with track_props (`_is_categorical_col`).
    mtype = (measure !== nothing && String(measure) in names(df)) ?
            (String(measure) in var_cols ? "numeric" :                        # var = quantitative, always
             _is_categorical_col(df[!, String(measure)], String(measure)) ? "categorical" : "numeric") : "numeric"
    # RAW EXPORT: emit the tidy per-datapoint rows behind the plot — identity (uID, label/track_id,
    # value_name, pop) + the optional groupBy level + the measure value — so the board CSV export can
    # be re-plotted in external software (Prism etc). Not an on-screen path: fired only on export, so
    # payload size (all N points) is a deliberate one-off, not a per-render cost. A measure-less `count`
    # chart collapses to per-image counts (the boxplot/violin/… population summary already substituted a
    # metric above); non-finite/missing values are dropped to mirror the plotted distribution. Matrix is
    # handled earlier and never reaches here.
    if raw
        rdf = df; mcol = measure === nothing ? nothing : String(measure)
        if mcol === nothing
            mcol = normalize in (:fraction, :total) ? "proportion" : "count"
            rdf = _population_metric_frame(df; normalize=normalize, col=mcol)
        end
        nms = names(rdf)
        # only emit columns that carry real identity: `label` is the CELL id (meaningless for the track
        # table, where it duplicates `track_id`), so drop it for track granularity; `track_id` only when
        # present; `group` only when the groupBy column was actually applied (a cell-level groupBy on a
        # track measure is echoed but never applied — it would be an empty column). The frontend also
        # drops any column left all-empty (single-image uID, population-summary label) as a backstop.
        gb_applied = gb !== nothing && gb in nms
        emit_label = granularity == :cell && "label" in nms
        emit_track = "track_id" in nms
        rows = Vector{Dict{String,Any}}()
        for i in 1:nrow(rdf)
            v = rdf[i, mcol]
            (ismissing(v) || (v isa Real && !isfinite(v))) && continue
            rec = Dict{String,Any}(
                "uID"        => ("uID" in nms) ? String(rdf[i, "uID"]) : "",
                "value_name" => ("value_name" in nms) ? String(rdf[i, "value_name"]) : "",
                "pop"        => ("pop" in nms) ? String(rdf[i, "pop"]) : "",
                # keep the actual number for a numeric measure (so Prism etc. read it as numeric); a
                # genuinely non-numeric (string) category is emitted as its label.
                "value"      => (v isa Real ? Float64(v) : _catkey(v)))
            emit_label && (rec["label"] = _catkey(rdf[i, "label"]))
            emit_track && (rec["track_id"] = _catkey(rdf[i, "track_id"]))
            if gb_applied
                gv = rdf[i, gb]
                rec["group"] = (ismissing(gv) || (gv isa Real && !isfinite(gv))) ? "" : _catkey(gv)
            end
            push!(rows, rec)
        end
        return Dict{String,Any}("chartType" => "raw", "measure" => mcol, "measureType" => mtype,
                                "granularity" => String(granularity),
                                "groupBy" => (gb_applied ? gb : nothing), "rows" => rows)
    end
    if chart_type == "points"
        # raw (downsampled) values per series — the data source for strip/jitter and (client-side
        # density) violin charts. No server aggregation beyond the per-group downsample.
        measure === nothing && error("plot_summary_data: points needs a `measure`")
        m = String(measure); groups = sgroups(df)
        series = [merge(base(g),
                        Dict("points" => _downsample(_finite(g.sub[!, m]), max_points))) for g in groups]
        return withgb(Dict{String,Any}("chartType" => "points", "measure" => m, "measureType" => mtype,
                                "granularity" => String(granularity), "series" => series))

    elseif chart_type == "histogram"
        measure === nothing && error("plot_summary_data: histogram needs a `measure`")
        m = String(measure); groups = sgroups(df)
        allvals = Float64[]
        for g in groups; append!(allvals, _finite(g.sub[!, m])); end
        edges = _hist_edges(allvals, nbins)
        series = [merge(base(g),
                        Dict("counts" => _hist_counts(g.sub[!, m], edges))) for g in groups]
        return withgb(Dict{String,Any}("chartType" => "histogram", "measure" => m, "measureType" => mtype,
                                "granularity" => String(granularity), "binEdges" => edges, "series" => series))

    elseif chart_type == "frequency"
        measure === nothing && error("plot_summary_data: frequency needs a `measure`")
        m = String(measure); groups = sgroups(df)
        catorder = String[]; catset = Set{String}()
        percounts = Vector{Tuple{NamedTuple,Dict{String,Int}}}()
        for g in groups
            counts = Dict{String,Int}()
            for v in g.sub[!, m]
                ismissing(v) && continue
                (v isa Real && !isfinite(v)) && continue
                k = _catkey(v)
                counts[k] = get(counts, k, 0) + 1
                (k in catset) || (push!(catorder, k); push!(catset, k))
            end
            push!(percounts, (g, counts))
        end
        cats = _sort_cats(catorder)
        series = map(percounts) do (g, counts)
            cvec = [get(counts, c, 0) for c in cats]; tot = sum(cvec)
            merge(base(g),
                  Dict("counts" => cvec, "values" => normalize == :fraction ?
                       [tot == 0 ? 0.0 : c / tot for c in cvec] : Float64.(cvec)))
        end
        return withgb(Dict{String,Any}("chartType" => "frequency", "measure" => m, "measureType" => mtype,
                                "granularity" => String(granularity), "normalize" => String(normalize),
                                "categories" => cats, "series" => series))

    elseif chart_type == "bar"
        # one bar per series = the mean of a continuous measure, with all three error metrics so the
        # panel can switch between them: sd (spread), sem (= sd/√n), ci95 (half-width of the 95% CI of
        # the mean). ci95 uses the normal approximation (1.96·sem) — fine for the n we plot; a t-based
        # interval would need a stats dep (docs/PLOTS.md §6.3). Chart type ⊥ data source.
        measure === nothing && error("plot_summary_data: bar needs a `measure`")
        m = String(measure); groups = sgroups(df)
        series = map(groups) do g
            vals = _finite(g.sub[!, m]); n = length(vals)
            sd  = n < 2 ? NaN : std(vals)
            sem = n < 2 ? NaN : sd / sqrt(n)
            merge(base(g),
                  Dict("value" => isempty(vals) ? NaN : mean(vals),
                       "sd" => sd, "sem" => sem, "ci95" => n < 2 ? NaN : 1.96 * sem, "n" => n))
        end
        return withgb(Dict{String,Any}("chartType" => "bar", "measure" => m, "measureType" => mtype,
                                "granularity" => String(granularity), "series" => series))

    elseif chart_type == "count"
        # # objects per series (row count) — the segmentation-integrity headline. Needs NO measure.
        # With by_image + group_by="t" each series is one (image, timepoint) bucket, so this yields
        # cell count per timepoint per image — the temporal-consistency time series (drops/spikes are
        # visible). Series shape mirrors `bar` (`value` = count) so the frontend renders it as a bar
        # or a line over the ordered group (t). See docs/PLOTS.md → Segmentation QC plot.
        #
        # `normalize` (:fraction/:total) → each series' FRACTION of the plotted total WITHIN its own
        # tracked population, per image: the denominator is keyed by (uID, value_name), so a plot
        # spanning several segmentations/tracked pops (e.g. B and T) reports each cluster's share of
        # *that* population's cells — not pooled across B+T. Pooled scope (uID="") folds over images.
        groups = sgroups(df)
        frac = normalize == :fraction || normalize == :total
        totals = Dict{Tuple{String,String},Int}()
        frac && for g in groups; k = (g.uid, g.vn); totals[k] = get(totals, k, 0) + nrow(g.sub); end
        series = map(groups) do g
            n = nrow(g.sub); tot = get(totals, (g.uid, g.vn), 0)
            v = frac ? (tot == 0 ? 0.0 : n / tot) : Float64(n)
            merge(base(g), Dict("value" => v, "n" => n))
        end
        return withgb(Dict{String,Any}("chartType" => "count", "measureType" => "numeric",
                                "normalize" => String(normalize),
                                "granularity" => String(granularity), "series" => series))

    elseif chart_type == "boxplot"
        # per-series box statistics of a continuous measure (Tukey: box = q1..q3, whiskers to the
        # furthest point within 1.5·IQR, + median and mean). This is the cross-image "compare track
        # speed across images X/Y/Z" chart (one box per image in per_image scope). Outliers omitted
        # to keep the payload tiny; `n` carries the sample size.
        measure === nothing && error("plot_summary_data: boxplot needs a `measure`")
        m = String(measure); groups = sgroups(df)
        series = map(groups) do g
            vals = sort(_finite(g.sub[!, m]))
            # optional downsampled raw points for the jitter overlay (docs/PLOTS.md §4)
            pts = raw_points ? _downsample(vals, max_points) : Float64[]
            if isempty(vals)
                merge(base(g),
                      Dict("q1"=>NaN,"median"=>NaN,"q3"=>NaN,"lower"=>NaN,"upper"=>NaN,"mean"=>NaN,"n"=>0,"points"=>pts))
            else
                q1 = quantile(vals, 0.25); q2 = quantile(vals, 0.5); q3 = quantile(vals, 0.75)
                iqr = q3 - q1
                lo = q1 - 1.5*iqr; hi = q3 + 1.5*iqr
                lower = minimum(v for v in vals if v >= lo)   # furthest point within the fence
                upper = maximum(v for v in vals if v <= hi)
                merge(base(g),
                      Dict("q1"=>q1, "median"=>q2, "q3"=>q3, "lower"=>lower, "upper"=>upper,
                           "mean"=>mean(vals), "n"=>length(vals), "points"=>pts))
            end
        end
        return withgb(Dict{String,Any}("chartType" => "boxplot", "measure" => m, "measureType" => mtype,
                                "granularity" => String(granularity), "series" => series))
    else
        error("plot_summary_data: unknown chart_type '$chart_type' (expected points | histogram | frequency | bar | count | boxplot)")
    end
end

# ── Matrix / heatmap aggregation (Plot.cell renderer) ─────────────────────────────
# A matrix pools the whole `pop_df` frame (every selected population/segmentation/image) into ONE
# grid — it's a composition view, not a per-series overlay. Two modes (docs/PLOTS.md §9):
#
#   profile  — rows = `measures`, columns = the levels of a categorical `category` column; each cell
#              is the MEAN of that measure for cells in that level. `zscore` standardises each row
#              (per measure) across the levels so differently-scaled measures are comparable — the
#              "state signature" (what properties do cells in each HMM state have).
#   crosstab — a single categorical `category` column whose values encode a pair "from<sep>to" (e.g.
#              HMM transitions "1_2", or the cross-model hybrid "1.2_3.4" — the hybrid joins state
#              columns with '.', so the FIRST `sep` splits prev|cur). Cell = count of from→to, or a
#              normalised rate (`:row` = P(to|from), `:col` = P(from|to), `:total` = fraction).
#
# Returns a flat `cells` list `[{x, y, value, n|count}]` plus ordered `xLabels`/`yLabels` (so the
# renderer lays out a fixed grid even where a cell is absent). Pure over the frame, headless-testable.
function _matrix_agg(df::DataFrame; mode::AbstractString,
                     measures::AbstractVector{<:AbstractString}=String[],
                     category::Union{Nothing,AbstractString}=nothing,
                     separator::AbstractString="_", zscore::Bool=false,
                     normalize::Symbol=:none, granularity::Symbol=:cell)::Dict{String,Any}
    cat = category === nothing ? nothing : String(category)
    (cat === nothing || !(cat in names(df))) &&
        error("plot_summary_data: matrix needs a `category` column present in the data (got $(cat === nothing ? "none" : repr(cat)))")
    catvals = df[!, cat]
    # finite/non-missing category key per row (`nothing` for dropped rows)
    catkeys = Union{Nothing,String}[(ismissing(v) || (v isa Real && !isfinite(v))) ? nothing :
                                    (v isa AbstractString ? String(v) : _catkey(v)) for v in catvals]

    if mode == "profile"
        ms = String[String(m) for m in measures if String(m) in names(df)]
        isempty(ms) && error("plot_summary_data: matrix profile needs at least one measure column present in the data")
        levels = String[]; seen = Set{String}()
        for k in catkeys; (k === nothing || k in seen) || (push!(levels, k); push!(seen, k)); end
        levels = _sort_cats(levels)
        cells = Dict{String,Any}[]
        for m in ms
            col = df[!, m]
            means = Dict{String,Float64}(); ns = Dict{String,Int}()
            for lvl in levels
                vals = Float64[Float64(col[i]) for i in eachindex(col)
                               if catkeys[i] === lvl && col[i] isa Real && isfinite(col[i])]
                means[lvl] = isempty(vals) ? NaN : mean(vals); ns[lvl] = length(vals)
            end
            if zscore                                  # standardise the row across its levels
                present = Float64[means[l] for l in levels if !isnan(means[l])]
                if length(present) >= 2
                    μ = mean(present); σ = std(present)
                    for l in levels
                        means[l] = (isnan(means[l]) || σ == 0) ? NaN : (means[l] - μ) / σ
                    end
                end
            end
            for lvl in levels
                v = means[lvl]
                # NaN (empty level, zero-variance z-score row, or a non-numeric measure) → JSON null —
                # JSON3 rejects NaN, and the renderer skips null cells. Keeps the grid axes intact.
                push!(cells, Dict{String,Any}("x" => lvl, "y" => m,
                                              "value" => isnan(v) ? nothing : v, "n" => ns[lvl]))
            end
        end
        return Dict{String,Any}("chartType" => "matrix", "matrixMode" => "profile",
            "xLabels" => levels, "yLabels" => ms, "cells" => cells, "category" => cat,
            "zscore" => zscore, "valueLabel" => zscore ? "z-score" : "mean",
            "granularity" => String(granularity), "series" => Any[])

    elseif mode == "crosstab"
        sep = isempty(String(separator)) ? "_" : String(separator)
        counts = Dict{Tuple{String,String},Int}()
        froms = String[]; fseen = Set{String}(); tos = String[]; tseen = Set{String}()
        for k in catkeys
            k === nothing && continue
            parts = split(k, sep; limit = 2)               # hybrid uses '.', so the first sep splits prev|cur
            length(parts) == 2 || continue
            f = String(parts[1]); t = String(parts[2])
            (isempty(f) || isempty(t)) && continue
            (f in fseen) || (push!(froms, f); push!(fseen, f))
            (t in tseen) || (push!(tos, t); push!(tseen, t))
            counts[(f, t)] = get(counts, (f, t), 0) + 1
        end
        froms = _sort_cats(froms); tos = _sort_cats(tos)
        rowtot = Dict{String,Int}(); coltot = Dict{String,Int}(); total = 0
        for ((f, t), c) in counts
            rowtot[f] = get(rowtot, f, 0) + c; coltot[t] = get(coltot, t, 0) + c; total += c
        end
        cells = Dict{String,Any}[]
        for f in froms, t in tos
            c = get(counts, (f, t), 0)
            val = normalize == :row   ? (get(rowtot, f, 0) == 0 ? 0.0 : c / rowtot[f]) :
                  normalize == :col   ? (get(coltot, t, 0) == 0 ? 0.0 : c / coltot[t]) :
                  normalize == :total ? (total == 0 ? 0.0 : c / total) : Float64(c)
            push!(cells, Dict{String,Any}("x" => t, "y" => f, "value" => val, "count" => c))
        end
        vlabel = normalize == :row ? "P(to|from)" : normalize == :col ? "P(from|to)" :
                 normalize == :total ? "fraction" : "count"
        return Dict{String,Any}("chartType" => "matrix", "matrixMode" => "crosstab",
            "xLabels" => tos, "yLabels" => froms, "cells" => cells, "category" => cat,
            "normalize" => String(normalize), "valueLabel" => vlabel,
            "granularity" => String(granularity), "series" => Any[])
    else
        error("plot_summary_data: unknown matrix mode '$mode' (expected profile | crosstab)")
    end
end

"""
    plot_summary_data(img, pop_type, pops, chart_type; value_name=nothing, granularity=:cell,
                      measure=nothing, nbins=30, normalize=:none) -> Dict
    plot_summary_data(imgs::Vector{CciaImage}, uids, pop_type, pops, chart_type;
                      scope=:per_image, …) -> Dict

Aggregate a summary plot over `pops` (each a series). `chart_type`:
- `"histogram"` — distribution of a continuous `measure` (one shared set of bin edges across all
  series so they are comparable); each series returns per-bin `counts`.
- `"frequency"` — counts per category of a categorical `measure` (e.g. `live.cell.hmm.state.*`);
  shared category axis. `normalize=:fraction` → within-series proportions instead of raw counts.
- `"bar"` — one bar per series: the `mean` of a continuous `measure` (+ `sd`, `n`).
- `"boxplot"` — per-series Tukey box statistics (`q1`/`median`/`q3`, whiskers, `mean`, `n`) of a
  continuous `measure`.

Chart type is **independent of the data source** — any chart works with a single image or a pooled
set (`scope`). E.g. "compare track speed across images X/Y/Z" can be a boxplot, bar, or overlaid
histogram; the same chart works on one image too.

The **multi-image** method pools across images (`uids` parallel to `imgs`; the API passes a
`CciaSet`'s `_images`+`image_uids`); `scope=:per_image` makes each image its own series (compare
images), `scope=:summarised` pools all images into the per-population series (one curve over the set).

`granularity=:track` reads the per-track table (`measure` ∈ `live.track.*`); `:cell` reads the cell
table. Returns a `Dict` ready to JSON-serialise to the `/api/plot_data` response.
"""
# pop_cols to fetch: the measure AND the optional group_by column (so the categorical sub-axis is
# available to `_series_groups`). `nothing` (fetch all) when neither is set.
function _plot_cols(measure, group_by)
    c = String[]
    measure  === nothing || push!(c, String(measure))
    group_by === nothing || push!(c, String(group_by))
    isempty(c) ? nothing : unique(c)
end

# pop_cols for a matrix: every row measure (profile) or the single `measure`, plus the `category`
# column (profile levels / crosstab from_to). `nothing` (fetch all) when neither is set.
function _matrix_cols(measures, measure, category)
    c = String[]
    if measures !== nothing
        append!(c, String[String(m) for m in measures])
    elseif measure !== nothing
        push!(c, String(measure))
    end
    # `pop` is the population category (per-population heatmap) — it's added by pop_df itself, not a
    # real table column, so don't list it for the fetch (avoids a spurious "unknown column" warning).
    category === nothing || String(category) == "pop" || push!(c, String(category))
    isempty(c) ? nothing : unique(c)
end

# pop_cols selector shared by all plot_summary_data methods — matrix needs the measure-list + category.
_cols_for(chart_type, measure, group_by, measures, category) =
    chart_type == "matrix" ? _matrix_cols(measures, measure, category) : _plot_cols(measure, group_by)

# The per-CLUSTER heatmap is a matrix over a `clusters.{suffix}` (or `regions.{suffix}` for spatial
# regions) column (root pop) → its frame must span ALL co-clustered segmentations so the signature
# covers the whole run, not just the active value_name (docs/todo/CLUSTER_POOLING_PLAN.md). Returns the
# run suffix when this applies, else nothing (the normal single-value_name pop_df is used). Per-POPULATION
# heatmaps (category="pop", bare cluster-pop paths) already pool via the bare-pop expansion in pop_df, so
# only the root case needs it.
function _cluster_matrix_suffix(chart_type, category)::Union{String,Nothing}
    (chart_type == "matrix" && category !== nothing) || return nothing
    c = String(category)
    for prefix in ("clusters.", "regions.")
        startswith(c, prefix) && return c[ncodeunits(prefix)+1:end]
    end
    nothing
end

# vcat pop_df across the co-clustered segmentations (`fetch_vn(vn)` returns one segment's frame).
_pool_co_clustered(vns, fetch_vn)::DataFrame = begin
    frames = DataFrame[]
    for vn in vns; d = fetch_vn(vn); nrow(d) > 0 && push!(frames, d); end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols=:union), frames)
end

# Per-POPULATION cluster heatmap (category="pop", bare cluster-pop paths): the pops must be evaluated
# against a segmentation that actually took part in the clustering run — NOT the drifting *active*
# value_name, which may not be in the run at all (its track/label table then lacks `clusters.{suffix}`,
# which is exactly what produced the `column name :clusters.default not found` crash). Mirror the UMAP
# endpoint's resolution (api/src/gating_api.jl `api_plots_umap`): pick a co-clustered value_name for the
# run's suffix. Only kicks in for cluster pop_types with a known suffix and NO explicit value_name — the
# per-cluster path (category="clusters.{suffix}") still pools via `_pool_co_clustered`, and an explicit
# value_name is always honoured.
function _cluster_pop_vn(img, pop_type, value_name, cluster_suffix, granularity)
    (value_name === nothing && _is_cluster_pop_type(pop_type) &&
        cluster_suffix !== nothing && !isempty(String(cluster_suffix))) || return value_name
    cc = co_clustered_value_names(img, String(cluster_suffix); granularity=granularity)
    isempty(cc) ? value_name : first(cc)
end

"""
    plot_summary_data(img, pop_type, pops, chart_type; measure, granularity, …) -> NamedTuple
    plot_summary_data(imgs, uids, pop_type, pops, chart_type; …)                 -> NamedTuple

Compute the data behind an analysis-board **summary plot** (histogram/box/violin/bar/…) for `pops` and
a `measure`, from `pop_df`. Returns the plot-ready series (not a figure). The vector form pools the
same population across images (tagged by `uID`). Use it to reproduce a board plot's numbers in a
notebook; pick `measure` from the population's columns (`pop_df` / `get_measure_summary`).
"""
function plot_summary_data(img::CciaImage, pop_type::AbstractString, pops, chart_type::AbstractString;
                           value_name::Union{AbstractString,Nothing}=nothing,
                           granularity::Symbol=:cell, measure::Union{AbstractString,Nothing}=nothing,
                           nbins::Int=30, normalize::Symbol=:none,
                           group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                           raw_points::Bool=false, max_points::Int=1500, raw::Bool=false, stat_unit::Symbol=:individual, image_agg::Symbol=:mean,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none,
                           cluster_suffix::Union{AbstractString,Nothing}=nothing)::Dict{String,Any}
    pops = String.(collect(pops))
    cols = _cols_for(chart_type, measure, group_by, measures, category)
    sfx = _cluster_matrix_suffix(chart_type, category)
    eff_vn = _cluster_pop_vn(img, pop_type, value_name, cluster_suffix, granularity)
    df = (sfx !== nothing && _is_cluster_pop_type(pop_type)) ?
        _pool_co_clustered(co_clustered_value_names(img, sfx; granularity=granularity),
            vn -> pop_df(img, pop_type, pops; value_name=vn, granularity=granularity,
                         pop_cols=cols, raw_channel_names=true)) :
        pop_df(img, pop_type, pops; value_name=eff_vn, granularity=granularity,
               pop_cols=cols, raw_channel_names=(chart_type == "matrix"))
    _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                 nbins=nbins, normalize=normalize, by_image=false, group_by=group_by,
                 var_cols=_var_measure_set(img, eff_vn),
                 collapse_series=collapse_series, raw_points=raw_points, max_points=max_points, raw=raw, stat_unit=stat_unit, image_agg=image_agg,
                 matrix_mode=matrix_mode, measures=measures, category=category,
                 separator=separator, zscore=zscore, matrix_normalize=matrix_normalize)
end

function plot_summary_data(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector,
                           pop_type::AbstractString, pops, chart_type::AbstractString;
                           scope::Symbol=:per_image,
                           value_name::Union{AbstractString,Nothing}=nothing,
                           granularity::Symbol=:cell, measure::Union{AbstractString,Nothing}=nothing,
                           nbins::Int=30, normalize::Symbol=:none,
                           group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                           raw_points::Bool=false, max_points::Int=1500, raw::Bool=false, stat_unit::Symbol=:individual, image_agg::Symbol=:mean,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none,
                           attr_map::Union{Nothing,AbstractDict}=nothing,
                           cluster_suffix::Union{AbstractString,Nothing}=nothing)::Dict{String,Any}
    pops = String.(collect(pops))
    cols = _cols_for(chart_type, measure, group_by, measures, category)
    sfx = _cluster_matrix_suffix(chart_type, category)
    # per-population cluster heatmap: resolve value_name to the run's segmentation (see _cluster_pop_vn),
    # taking the run from the first image — clustering is set-scope so the value_name set is consistent.
    eff_vn = isempty(imgs) ? value_name :
             _cluster_pop_vn(first(imgs), pop_type, value_name, cluster_suffix, granularity)
    # per-cluster heatmap → pool across the run's co-clustered segmentations (clustering is set-scope,
    # so the value_name set is consistent; take it from the first image). Each vn still pools across
    # images via the multi-image pop_df.
    df = (sfx !== nothing && _is_cluster_pop_type(pop_type) && !isempty(imgs)) ?
        _pool_co_clustered(co_clustered_value_names(first(imgs), sfx; granularity=granularity),
            vn -> pop_df(imgs, uids, pop_type, pops; value_name=vn, granularity=granularity,
                         pop_cols=cols, raw_channel_names=true)) :
        pop_df(imgs, uids, pop_type, pops; value_name=eff_vn, granularity=granularity,
               pop_cols=cols, raw_channel_names=(chart_type == "matrix"))
    result = _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                          nbins=nbins, normalize=normalize, by_image=(scope == :per_image),
                          group_by=group_by, var_cols=_var_measure_set(imgs, eff_vn),
                          collapse_series=collapse_series,
                          raw_points=raw_points, max_points=max_points, raw=raw, stat_unit=stat_unit, image_agg=image_agg,
                          matrix_mode=matrix_mode, measures=measures, category=category,
                          separator=separator, zscore=zscore, matrix_normalize=matrix_normalize,
                          attr_map=attr_map)
    result["scope"] = String(scope)
    result
end

# ── Multiple SEGMENTATIONS on one plot ────────────────────────────────────────────
# A "target" is a `(value_name, pop)` pair — a population from a specific segmentation. Plotting
# several lets the user overlay populations drawn on DIFFERENT segmentations (e.g. `/T cells` from
# the `base` labels next to `/macrophages` from `nuc`). Targets are grouped by `value_name`, each
# group read through `pop_df` with that segmentation, then `vcat`-ed into one frame. `_series_groups`
# keys by `(uID, value_name, pop)`, so each (segmentation, pop) — and each image, when cross-image —
# becomes its own series. Order is preserved (first appearance) for a stable series order/colouring.

# group `(value_name, pop)` targets → ordered [(value_name, [pop…])]; vcat per-vn frames (cols union).
function _targets_frame(targets, fetch_vn)::DataFrame
    order = String[]; byvn = Dict{String,Vector{String}}()
    for (vn, pop) in targets
        v = String(vn)
        haskey(byvn, v) || (push!(order, v); byvn[v] = String[])
        push!(byvn[v], String(pop))
    end
    frames = DataFrame[]
    for vn in order
        d = fetch_vn(vn, byvn[vn])
        nrow(d) == 0 || push!(frames, d)
    end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols=:union), frames)
end

# single image, multiple segmentations (one series per (value_name, pop)).
function plot_summary_data(img::CciaImage, pop_type::AbstractString,
                           targets::AbstractVector{<:Tuple}, chart_type::AbstractString;
                           granularity::Symbol=:cell, measure::Union{AbstractString,Nothing}=nothing,
                           nbins::Int=30, normalize::Symbol=:none,
                           group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                           raw_points::Bool=false, max_points::Int=1500, raw::Bool=false, stat_unit::Symbol=:individual, image_agg::Symbol=:mean,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none)::Dict{String,Any}
    pcols = _cols_for(chart_type, measure, group_by, measures, category)
    df = _targets_frame(targets, (vn, pops) ->
        pop_df(img, pop_type, pops; value_name=vn, granularity=granularity, pop_cols=pcols,
               raw_channel_names=(chart_type == "matrix")))
    _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                 nbins=nbins, normalize=normalize, by_image=false, group_by=group_by,
                 var_cols=_var_measure_set(img, isempty(targets) ? nothing : first(targets)[1]),
                 collapse_series=collapse_series, raw_points=raw_points, max_points=max_points, raw=raw, stat_unit=stat_unit, image_agg=image_agg,
                 matrix_mode=matrix_mode, measures=measures, category=category,
                 separator=separator, zscore=zscore, matrix_normalize=matrix_normalize)
end

# multiple images AND multiple segmentations. `scope=:per_image` → one series per image per
# (value_name, pop); `scope=:summarised` → pool the images per (value_name, pop).
function plot_summary_data(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector,
                           pop_type::AbstractString, targets::AbstractVector{<:Tuple},
                           chart_type::AbstractString; scope::Symbol=:per_image,
                           granularity::Symbol=:cell, measure::Union{AbstractString,Nothing}=nothing,
                           nbins::Int=30, normalize::Symbol=:none,
                           group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                           raw_points::Bool=false, max_points::Int=1500, raw::Bool=false, stat_unit::Symbol=:individual, image_agg::Symbol=:mean,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none,
                           attr_map::Union{Nothing,AbstractDict}=nothing)::Dict{String,Any}
    pcols = _cols_for(chart_type, measure, group_by, measures, category)
    df = _targets_frame(targets, (vn, pops) ->
        pop_df(imgs, uids, pop_type, pops; value_name=vn, granularity=granularity, pop_cols=pcols,
               raw_channel_names=(chart_type == "matrix")))
    result = _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                          nbins=nbins, normalize=normalize, by_image=(scope == :per_image),
                          group_by=group_by,
                          var_cols=_var_measure_set(imgs, isempty(targets) ? nothing : first(targets)[1]),
                          collapse_series=collapse_series,
                          raw_points=raw_points, max_points=max_points, raw=raw, stat_unit=stat_unit, image_agg=image_agg,
                          matrix_mode=matrix_mode, measures=measures, category=category,
                          separator=separator, zscore=zscore, matrix_normalize=matrix_normalize,
                          attr_map=attr_map)
    result["scope"] = String(scope)
    result
end
