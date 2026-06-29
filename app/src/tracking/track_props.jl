# ── track_props — per-track property table (compute-on-read) ─────────────────────
#
# Ports R `cciaImage$tracksInfo` (docs/TRACKING.md, TODO #00029). A track's properties are its
# motility measures (precomputed in {vn}__tracks.h5ad) PLUS on-read aggregates of any per-CELL
# column (intensity, morphology, hmm.state, …) over the track's cells. Compute-on-read — nothing
# extra is stored, so track properties never go stale and need no re-run when new cell measures
# appear (same "derive, don't duplicate" choice as the track DataFrame itself).
#
# This is the data source for track-gating (pop_type="track") and per-track summary plots.

using Statistics: mean, median, std, quantile
using DataFrames: DataFrame, groupby, combine, leftjoin, rename!, nrow

# finite-only reductions (cells with NaN/missing in a measure are skipped, like R na.rm=TRUE)
_finite_vals(x) = Float64[Float64(v) for v in x if v isa Real && isfinite(v)]

# Up to this many distinct INTEGER levels still reads as a categorical code set (e.g. hmm.state ∈
# {1,2,3}) rather than a numeric count. A spread-out integer column (area, neighbour counts over a
# wide range) exceeds it and stays numeric.
const _MAX_CATEGORICAL_LEVELS = 20

# Detect categorical vs numeric automatically from the decoded column — no config map (the old R
# version used `config.yml` `parameters.labelStats`). Three cases, matching the real data:
#   • String / non-`Real`  → categorical   — anndata `string-array`/`categorical` obs decode to
#       `String` (`label_props.jl::_read_anndata_vector`).  e.g. hmm.transitions = "1.3","2.2"
#   • `Real` with a fractional value → numeric (continuous).  e.g. speed = 10.12
#   • `Real`, all-integer, few distinct levels → categorical (integer code set).  e.g. hmm.state = 1,2,3
# The integer rule is a heuristic: an integer measure with a *large* spread (area, wide counts) stays
# numeric, but a small-range integer COUNT could be misread — pass it in `numeric` to force it (or
# `categorical` to force the other way). The first rule is exact, so the cleanest path for a true
# categorical is to have the producing task write it as an anndata `categorical`.
function _is_categorical_col(col)::Bool
    nonmissingtype(eltype(col)) <: Real || return true            # String / categorical-encoded
    vals = _finite_vals(col)
    isempty(vals) && return false
    all(v -> v == floor(v), vals) || return false                 # any decimal ⇒ continuous ⇒ numeric
    length(unique(vals)) <= _MAX_CATEGORICAL_LEVELS               # few integer levels ⇒ code set
end
_f_mean(x)   = (f = _finite_vals(x); isempty(f) ? NaN : mean(f))
_f_median(x) = (f = _finite_vals(x); isempty(f) ? NaN : median(f))
_f_sum(x)    = (f = _finite_vals(x); isempty(f) ? 0.0 : sum(f))
_f_sd(x)     = (f = _finite_vals(x); length(f) < 2 ? NaN : std(f))
_f_q(x, q)   = (f = _finite_vals(x); isempty(f) ? NaN : quantile(f, q))

"""
    track_props(img; value_name=nothing, cell_measures=String[],
                categorical=String[], numeric=String[],
                include_motility=true, quantiles=(0.95, 0.05)) -> DataFrame

Per-track property table keyed by `track_id` (one row per track; `track_id > 0` only). Columns:
`track_id`, `label` (= `track_id`, so the gating engine's by-`label` membership works unchanged),
`num_cells`, the motility measures from `{vn}__tracks.h5ad` (when `include_motility`), and per-track
aggregates of each requested **cell** column joined by `track_id`:
- numeric → `{m}.mean / .median / .sum / .qUp / .qLow / .sd`
- categorical (e.g. `live.cell.hmm.state.*`) → per-category within-track frequency `{m}.{cat}`

The numeric/categorical split is detected **automatically** from each column's decoded type +
values (see `_is_categorical_col`): strings (e.g. `hmm.transitions = "1.3"`) and integer code sets
(e.g. `hmm.state ∈ {1,2,3}`) → categorical; continuous floats (e.g. `speed = 10.12`) → numeric. No
config map (replaces the old R `config.yml` `parameters.labelStats`). `categorical`/`numeric` are
escape-hatch overrides that force a column's classification when the heuristic misreads it.

Compute-on-read; nothing persisted. Ports R `tracksInfo`.
"""
function track_props(img::CciaImage; value_name::Union{AbstractString,Nothing}=nothing,
                     cell_measures=String[], categorical=String[], numeric=String[],
                     include_motility::Bool=true,
                     quantiles::Tuple{Float64,Float64}=(0.95, 0.05))::DataFrame
    vn = something(value_name, get(img.label_props, "_active", "default"))
    cell_measures = String.(collect(cell_measures))
    categorical   = String.(collect(categorical))
    numeric       = String.(collect(numeric))

    # read tracked cells: track_id + requested measures (label-keyed)
    cols = unique(vcat("track_id", cell_measures))
    cell = label_props(img; value_name=vn) |> lp -> select_cols(lp, cols) |> as_df
    keep = [r isa Number && !isnan(r) && Int(r) > 0 for r in cell[!, "track_id"]]
    cell = cell[keep, :]
    cell[!, :track_id] = Int.(cell[!, "track_id"])

    out = combine(groupby(cell, :track_id), nrow => :num_cells)

    for m in cell_measures
        m == "track_id" && continue
        g = groupby(cell, :track_id)
        # auto-detect type from the decoded column; `categorical`/`numeric` kwargs force the call
        is_cat = m in categorical ? true :
                 m in numeric     ? false :
                 _is_categorical_col(cell[!, m])
        if is_cat
            cats = sort(unique(_catkey(v) for v in cell[!, m]
                               if !(v isa Missing) && !(v isa Real && !isfinite(v))))
            freq = combine(g) do d
                n = nrow(d); vals = d[!, m]
                DataFrame(Dict(Symbol("$m.$c") =>
                    (n == 0 ? 0.0 : count(v -> !(v isa Missing) && _catkey(v) == c, vals) / n)
                    for c in cats))
            end
            out = leftjoin(out, freq, on=:track_id)
        else
            agg = combine(g,
                m => (x -> _f_mean(x))            => Symbol("$m.mean"),
                m => (x -> _f_median(x))          => Symbol("$m.median"),
                m => (x -> _f_sum(x))             => Symbol("$m.sum"),
                m => (x -> _f_q(x, quantiles[1])) => Symbol("$m.qUp"),
                m => (x -> _f_q(x, quantiles[2])) => Symbol("$m.qLow"),
                m => (x -> _f_sd(x))              => Symbol("$m.sd"))
            out = leftjoin(out, agg, on=:track_id)
        end
    end

    if include_motility
        tpath = img_track_props_path(img, vn)
        if isfile(tpath)
            mot = label_props(tpath) |> as_df  # label(=track_id) + motility vars (+ lineage obs)
            rename!(mot, :label => :track_id)
            out = leftjoin(out, mot, on=:track_id)
        end
    end

    out[!, :label] = out[!, :track_id]         # engine membership is by-`label`
    out
end

# numeric aggregate suffixes track_props appends to a base cell measure
const _TRACK_NUM_AGG_SUFFIXES = (".mean", ".median", ".sum", ".qUp", ".qLow", ".sd")

"""
    track_cell_measures(cols, motility_cols) -> Vector{String}

Inverse of `track_props`'s column naming: given desired **track-property** column names (a gating
axis or a gate's channels) and the set of motility columns (free from `{vn}__tracks.h5ad`), return
the base **cell** measures that must be aggregated to produce them — i.e. the `cell_measures` to hand
`track_props`/`pop_df(…, "track")`. A motility column (or `track_id`/`label`/`num_cells`
bookkeeping) needs no aggregation; `{base}.mean|median|sum|qUp|qLow|sd` → `base`; any other
`{base}.{cat}` (a categorical within-track frequency) → `base`. Lets the gating API request exactly
the per-track aggregates an axis needs without enumerating every measure×aggregate.
"""
function track_cell_measures(cols, motility_cols)::Vector{String}
    mot = Set(String.(collect(motility_cols)))
    skip = union(mot, Set(["track_id", "label", "num_cells"]))
    bases = String[]
    for c in String.(collect(cols))
        c in skip && continue
        hit = findfirst(s -> endswith(c, s), _TRACK_NUM_AGG_SUFFIXES)
        if hit !== nothing
            push!(bases, c[1:end-ncodeunits(_TRACK_NUM_AGG_SUFFIXES[hit])])  # strip `.agg`
        else
            idx = findlast('.', c)                                          # `{base}.{cat}` → base
            idx === nothing || push!(bases, c[1:prevind(c, idx)])
        end
    end
    unique(bases)
end
