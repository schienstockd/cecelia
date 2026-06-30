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

# Shared aggregation core over an already-built pop_df frame. `by_image` → one series per source
# image (cross-image comparison); else images (if any) are pooled. Used by both the single-image
# and the multi-image `plot_summary_data` methods so the chart logic lives in one place.
function _summary_agg(df::DataFrame, chart_type::AbstractString;
                      measure::Union{AbstractString,Nothing}, granularity::Symbol,
                      nbins::Int, normalize::Symbol, by_image::Bool,
                      group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                      raw_points::Bool=false, max_points::Int=1500,
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
    # `group` is the optional categorical sub-axis level (e.g. an HMM state) — "" when not grouping.
    base(g) = Dict{String,Any}("pop" => g.sid, "value_name" => g.vn, "uID" => g.uid, "group" => g.grp)
    gb = group_by === nothing ? nothing : String(group_by)
    sgroups(d) = _series_groups(d; by_image=by_image, group_col=gb, collapse=collapse_series, attr_map=attr_map)
    withgb(r) = (r["groupBy"] = (gb === nothing ? nothing : gb); r)
    # detected measure type (numeric vs categorical) so the panel can offer only the applicable
    # chart types (docs/PLOTS.md §2). Auto-detection is shared with track_props (`_is_categorical_col`).
    mtype = (measure !== nothing && String(measure) in names(df)) ?
            (_is_categorical_col(df[!, String(measure)], String(measure)) ? "categorical" : "numeric") : "numeric"
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
        error("plot_summary_data: unknown chart_type '$chart_type' (expected points | histogram | frequency | bar | boxplot)")
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
    category === nothing || push!(c, String(category))
    isempty(c) ? nothing : unique(c)
end

# pop_cols selector shared by all plot_summary_data methods — matrix needs the measure-list + category.
_cols_for(chart_type, measure, group_by, measures, category) =
    chart_type == "matrix" ? _matrix_cols(measures, measure, category) : _plot_cols(measure, group_by)

function plot_summary_data(img::CciaImage, pop_type::AbstractString, pops, chart_type::AbstractString;
                           value_name::Union{AbstractString,Nothing}=nothing,
                           granularity::Symbol=:cell, measure::Union{AbstractString,Nothing}=nothing,
                           nbins::Int=30, normalize::Symbol=:none,
                           group_by::Union{AbstractString,Nothing}=nothing, collapse_series::Bool=false,
                           raw_points::Bool=false, max_points::Int=1500,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none)::Dict{String,Any}
    pops = String.(collect(pops))
    df = pop_df(img, pop_type, pops; value_name=value_name, granularity=granularity,
                pop_cols=_cols_for(chart_type, measure, group_by, measures, category))
    _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                 nbins=nbins, normalize=normalize, by_image=false, group_by=group_by,
                 collapse_series=collapse_series, raw_points=raw_points, max_points=max_points,
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
                           raw_points::Bool=false, max_points::Int=1500,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none,
                           attr_map::Union{Nothing,AbstractDict}=nothing)::Dict{String,Any}
    pops = String.(collect(pops))
    df = pop_df(imgs, uids, pop_type, pops; value_name=value_name, granularity=granularity,
                pop_cols=_cols_for(chart_type, measure, group_by, measures, category))
    result = _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                          nbins=nbins, normalize=normalize, by_image=(scope == :per_image),
                          group_by=group_by, collapse_series=collapse_series,
                          raw_points=raw_points, max_points=max_points,
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
                           raw_points::Bool=false, max_points::Int=1500,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none)::Dict{String,Any}
    pcols = _cols_for(chart_type, measure, group_by, measures, category)
    df = _targets_frame(targets, (vn, pops) ->
        pop_df(img, pop_type, pops; value_name=vn, granularity=granularity, pop_cols=pcols))
    _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                 nbins=nbins, normalize=normalize, by_image=false, group_by=group_by,
                 collapse_series=collapse_series, raw_points=raw_points, max_points=max_points,
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
                           raw_points::Bool=false, max_points::Int=1500,
                           matrix_mode::Union{AbstractString,Nothing}=nothing,
                           measures::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
                           category::Union{AbstractString,Nothing}=nothing,
                           separator::AbstractString="_", zscore::Bool=false,
                           matrix_normalize::Symbol=:none,
                           attr_map::Union{Nothing,AbstractDict}=nothing)::Dict{String,Any}
    pcols = _cols_for(chart_type, measure, group_by, measures, category)
    df = _targets_frame(targets, (vn, pops) ->
        pop_df(imgs, uids, pop_type, pops; value_name=vn, granularity=granularity, pop_cols=pcols))
    result = _summary_agg(df, chart_type; measure=measure, granularity=granularity,
                          nbins=nbins, normalize=normalize, by_image=(scope == :per_image),
                          group_by=group_by, collapse_series=collapse_series,
                          raw_points=raw_points, max_points=max_points,
                          matrix_mode=matrix_mode, measures=measures, category=category,
                          separator=separator, zscore=zscore, matrix_normalize=matrix_normalize,
                          attr_map=attr_map)
    result["scope"] = String(scope)
    result
end
