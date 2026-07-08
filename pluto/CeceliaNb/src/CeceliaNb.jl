"""
    CeceliaNb

Small notebook-side helper set for the Cecelia Notebooks (Pluto) Playground. Two jobs:

1. **Aggregation** (`nb_count`, `nb_summary`) — tidy summaries of a `pop_df` DataFrame. These operate
   on the SAME table the `/analysis` board builds from (both start from `Cecelia.pop_df`), so the
   numbers match the board; only the rendering differs (AlgebraOfGraphics here vs Observable Plot there).
2. **Plot shortcuts** (`nb_hist`, `nb_box`, `nb_scatter`) — one-liners over AlgebraOfGraphics +
   CairoMakie for the plots notebooks reach for most, so users don't re-roll the grammar each time.

Deliberately minimal — grow from real notebook usage. For anything beyond these, use
AlgebraOfGraphics directly (`data(df) * mapping(...) * visual(...)` |> `draw`).
"""
module CeceliaNb

using AlgebraOfGraphics, CairoMakie, DataFrames, Statistics

export nb_count, nb_summary, nb_hist, nb_box, nb_scatter

_sym(x) = x isa Symbol ? x : Symbol(x)

# Grouping keys that `pop_df` adds (value_name / pop), when present — the natural summary axes.
_pop_keys(df::DataFrame) = intersect([:value_name, :pop], propertynames(df))

"""
    nb_count(df) -> DataFrame

Cell counts per `(value_name, pop)` when those columns are present (the usual `pop_df` shape), else
a single total. The "how many cells landed where" summary.
"""
function nb_count(df::DataFrame)::DataFrame
    isempty(df) && return DataFrame()
    keys = _pop_keys(df)
    isempty(keys) ? DataFrame(n = nrow(df)) : sort!(combine(groupby(df, keys), nrow => :n), keys)
end

"""
    nb_summary(df, measure; by = value_name/pop) -> DataFrame

Grouped `mean` / `median` / `sd` / `n` of a continuous `measure`. Defaults to grouping by whichever
of `value_name`/`pop` are present. Missing/NaN are skipped.
"""
function nb_summary(df::DataFrame, measure; by = _pop_keys(df))::DataFrame
    isempty(df) && return DataFrame()
    m  = _sym(measure)
    by = Symbol.(collect(by))
    g  = isempty(by) ? df : groupby(df, by)
    combine(g,
        m => (x -> mean(skipmissing(x)))   => :mean,
        m => (x -> median(skipmissing(x))) => :median,
        m => (x -> std(skipmissing(x)))    => :sd,
        m => (x -> count(!ismissing, x))   => :n)
end

# Optional grouping → an AoG `mapping` colour/dodge, or no extra visual channel.
_maybe_color(col) = col === nothing ? mapping() : mapping(color = _sym(col))

"""
    nb_hist(df, col; bins = 40, color = nothing, kwargs...) -> figure

Histogram of a continuous column, optionally split by `color` (overlaid). `kwargs` pass to `draw`.
"""
function nb_hist(df::DataFrame, col; bins::Int = 40, color = nothing, kwargs...)
    layer = data(df) * mapping(_sym(col)) * _maybe_color(color) * AlgebraOfGraphics.histogram(bins = bins)
    draw(layer; kwargs...)
end

"""
    nb_box(df, group, value; color = nothing, kwargs...) -> figure

Boxplot of `value` per category `group` (Tukey stats), e.g. compare a measure across `:value_name`.
"""
function nb_box(df::DataFrame, group, value; color = nothing, kwargs...)
    layer = data(df) * mapping(_sym(group), _sym(value)) * _maybe_color(color) * visual(BoxPlot)
    draw(layer; kwargs...)
end

"""
    nb_scatter(df, x, y; color = nothing, kwargs...) -> figure

Scatter of `y` vs `x`, optionally coloured by a third column.
"""
function nb_scatter(df::DataFrame, x, y; color = nothing, kwargs...)
    layer = data(df) * mapping(_sym(x), _sym(y)) * _maybe_color(color) * visual(Scatter)
    draw(layer; kwargs...)
end

end # module
