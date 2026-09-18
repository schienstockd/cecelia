### A Pluto.jl notebook ###
# v1.0.3

using Markdown
using InteractiveUtils

# ╔═╡ a1000000-0000-0000-0000-000000000000
# Activate the Notebooks env (path-sources dev Cecelia + CeceliaNb helpers). Keep this first.
begin
    import Pkg
    Pkg.activate(get(ENV, "CECELIA_PLUTO_ENV", joinpath(@__DIR__, "..", "pluto")))
end

# ╔═╡ a2000000-0000-0000-0000-000000000000
using Cecelia, DataFrames, CeceliaNb, AlgebraOfGraphics, CairoMakie

# ╔═╡ a0000000-0000-0000-0000-000000000000
md"""
# Motif ribbon — sub-track behavioural motifs (P1 POC)

Renders the consumer side of `behaviour.motif_discovery` (P1): one lane per track, coloured
by discovered motif class, x-axis = frame. If nothing shows, run the
**`behaviour.motif_discovery`** task first (any suffix); this notebook then reads the banked
per-cell obs columns via `pop_df` and paints them.

> **Prerequisite.** This notebook is READ-ONLY over `motif.class.{suffix}` +
> `motif.distance.{suffix}` (per-cell obs written by the task). This example is **UID-free**:
> set the project/image + motif suffix below (or use `CECELIA_EXAMPLE_PROJ` / `_UID` /
> `CECELIA_MOTIF_SUFFIX`). `SUFFIX` = the source segmentation's value_name that the task ran
> against (whatever you selected under Populations — e.g. `flowTom` if your tracked pop lives
> on the `flowTom` value_name).
"""

# ╔═╡ a3000000-0000-0000-0000-000000000000
Cecelia.init_cecelia!()

# ╔═╡ b0000000-0000-0000-0000-000000000000
md"## 1 · Load the image"

# ╔═╡ b1000000-0000-0000-0000-000000000000
proj_uid = get(ENV, "CECELIA_EXAMPLE_PROJ", "")   # ← your project UID

# ╔═╡ b2000000-0000-0000-0000-000000000000
uid = get(ENV, "CECELIA_EXAMPLE_UID", "")         # ← your image UID

# ╔═╡ b3000000-0000-0000-0000-000000000000
SUFFIX = get(ENV, "CECELIA_MOTIF_SUFFIX", "default")   # ← source segmentation's value_name (e.g. "flowTom")

# ╔═╡ b4000000-0000-0000-0000-000000000000
img = (isempty(proj_uid) || isempty(uid)) ? nothing : init_object(proj_uid, uid)

# ╔═╡ b5000000-0000-0000-0000-000000000000
img === nothing ? md"➡️ set `proj_uid` + `uid` above (or `CECELIA_EXAMPLE_PROJ`/`_UID`)." :
    md"""Loaded **$(img.name)**. Motif suffix: **`$(SUFFIX)`**."""

# ╔═╡ c0000000-0000-0000-0000-000000000000
md"""
## 2 · Pull the per-cell motif columns

The motif task writes its output back onto the source segmentation's `.h5ad` — so the
columns `motif.class.<SUFFIX>` live on `<SUFFIX>.h5ad`. We read directly through the
`label_props` chain (`select_cols → as_df`) rather than via `pop_df` because the
gating tree isn't the filter here — the motif columns themselves are.
"""

# ╔═╡ c1000000-0000-0000-0000-000000000000
class_col    = "motif.class.$(SUFFIX)"

# ╔═╡ c2000000-0000-0000-0000-000000000000
distance_col = "motif.distance.$(SUFFIX)"

# ╔═╡ c4000000-0000-0000-0000-000000000000
# temporal column name — pick whichever the SUFFIX value_name exposes (centroid_t is the common one)
tcol = img === nothing ? nothing : begin
    tc = temporal_columns(label_props(img; value_name = SUFFIX))
    isempty(tc) ? nothing : first(tc)
end

# ╔═╡ c5000000-0000-0000-0000-000000000000
# Read the SUFFIX value_name's cell obs directly. Returns an empty frame if any of the
# requested columns aren't there yet (task hasn't been run for this suffix).
cells = (img === nothing || tcol === nothing) ? DataFrame() : try
    label_props(img; value_name = SUFFIX) |>
        lp -> select_cols(lp, [class_col, distance_col, "track_id", tcol]) |>
        as_df
catch e
    DataFrame()
end

# ╔═╡ c6000000-0000-0000-0000-000000000000
have_motif = !isempty(cells) && all(c -> c in names(cells), [class_col, "track_id", tcol])

# ╔═╡ c7000000-0000-0000-0000-000000000000
have_motif ? md"Loaded **$(nrow(cells))** cells with `$(class_col)`." :
    md"""_No motif columns found on `$(SUFFIX).h5ad`. Run the
    `behaviour.motif_discovery` task first (Populations = tracked pops on the
    `$(SUFFIX)` segmentation; Name = `$(SUFFIX)`), then re-open this notebook._"""

# ╔═╡ d0000000-0000-0000-0000-000000000000
md"""
## 3 · Motif ribbon — one lane per track, coloured by class

Every cell of every track becomes a coloured tile at its frame `t`. Cells without a motif
assignment (unassigned windows / short tracks) are drawn as a grey background lane.
"""

# ╔═╡ d1000000-0000-0000-0000-000000000000
ribbon_data = have_motif ? begin
    d = copy(cells)
    d = d[d.track_id .> 0, :]
    d[!, :_class] = String[c === missing || c === nothing ? "unassigned" : string(c) for c in d[!, class_col]]
    d[!, :_t]     = Float64[Float64(x) for x in d[!, tcol]]
    d[!, :_track] = Int[Int(x) for x in d[!, "track_id"]]
    d
end : DataFrame()

# ╔═╡ d2000000-0000-0000-0000-000000000000
# `draw` a rectangle per (track, frame). AlgebraOfGraphics maps `_class` to colour; the
# categorical palette Makie picks stays consistent within one notebook session.
ribbon = isempty(ribbon_data) ? md"_(no ribbon to draw)_" : begin
    plt = data(ribbon_data) *
        mapping(:_t => "frame", :_track => "track", color = :_class => "motif class") *
        visual(Heatmap)
    draw(plt; figure = (; size = (900, 500)))
end

# ╔═╡ e0000000-0000-0000-0000-000000000000
md"""
## 4 · Class counts

Sanity check on how many cells landed in each class (matches what the Analysis board would show
once the P2 `motifClassFrequency` plot ships).
"""

# ╔═╡ e1000000-0000-0000-0000-000000000000
class_counts = isempty(ribbon_data) ? DataFrame() :
    sort(combine(groupby(ribbon_data, :_class), nrow => :n), :_class)

# ╔═╡ Cell order:
# ╟─a0000000-0000-0000-0000-000000000000
# ╠═a1000000-0000-0000-0000-000000000000
# ╠═a2000000-0000-0000-0000-000000000000
# ╠═a3000000-0000-0000-0000-000000000000
# ╟─b0000000-0000-0000-0000-000000000000
# ╠═b1000000-0000-0000-0000-000000000000
# ╠═b2000000-0000-0000-0000-000000000000
# ╠═b3000000-0000-0000-0000-000000000000
# ╠═b4000000-0000-0000-0000-000000000000
# ╟─b5000000-0000-0000-0000-000000000000
# ╟─c0000000-0000-0000-0000-000000000000
# ╠═c1000000-0000-0000-0000-000000000000
# ╠═c2000000-0000-0000-0000-000000000000
# ╠═c4000000-0000-0000-0000-000000000000
# ╠═c5000000-0000-0000-0000-000000000000
# ╠═c6000000-0000-0000-0000-000000000000
# ╟─c7000000-0000-0000-0000-000000000000
# ╟─d0000000-0000-0000-0000-000000000000
# ╠═d1000000-0000-0000-0000-000000000000
# ╠═d2000000-0000-0000-0000-000000000000
# ╟─e0000000-0000-0000-0000-000000000000
# ╠═e1000000-0000-0000-0000-000000000000
