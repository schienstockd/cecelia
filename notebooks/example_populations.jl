### A Pluto.jl notebook ###
# v1.0.3

using Markdown
using InteractiveUtils

# ╔═╡ a1000000-0000-0000-0000-000000000000
# Activate the shared Playground env (path-sources the dev Cecelia). Doing Pkg ops here makes Pluto
# disable its own registered-only package manager for this notebook — required for the local dev pkg.
begin
    import Pkg
    Pkg.activate(get(ENV, "CECELIA_PLUTO_ENV", joinpath(@__DIR__, "..", "pluto")))
end

# ╔═╡ a2000000-0000-0000-0000-000000000000
using Cecelia

# ╔═╡ a3000000-0000-0000-0000-000000000000
using DataFrames, AlgebraOfGraphics, CairoMakie, CSV

# ╔═╡ a0000000-0000-0000-0000-000000000000
md"""
# Cecelia Playground — population summary (example)

A **pure-Julia** downstream-analysis notebook. It loads a Cecelia image object, pulls its cell table
through the documented `pop_df` accessor, plots a measurement with AlgebraOfGraphics + CairoMakie,
and exports a CSV — the same shape of work the old R Markdown vignettes did, now structured and
per-project.

This example is **UID-free**: set the project/image below (or `CECELIA_EXAMPLE_PROJ` / `_UID` /
`_VN`). Everything else is reactive — edit a cell and downstream cells recompute.
"""

# ╔═╡ a4000000-0000-0000-0000-000000000000
Cecelia.init_cecelia!()

# ╔═╡ b0000000-0000-0000-0000-000000000000
md"## 1 · Choose your data"

# ╔═╡ b1000000-0000-0000-0000-000000000000
proj_uid = get(ENV, "CECELIA_EXAMPLE_PROJ", "")   # ← your project UID

# ╔═╡ b2000000-0000-0000-0000-000000000000
# Guide: available projects (shown when the field above is blank). The listing is built in plain
# code — the `md"..."` macro can't take nested double-quotes inside `$(…)`, so only a single var
# is interpolated.
let
    projs = isempty(proj_uid) ?
        (try filter(d -> isdir(joinpath(projects_dir(), d)), readdir(projects_dir())) catch; String[] end) :
        String[]
    listing = isempty(projs) ? "none found" : join(projs, ", ")
    isempty(proj_uid) ? md"➡️ Set `proj_uid`. Available projects: **$(listing)**" :
                        md"✅ project `$(proj_uid)`"
end

# ╔═╡ b3000000-0000-0000-0000-000000000000
uid = get(ENV, "CECELIA_EXAMPLE_UID", "")         # ← your image (or set) UID

# ╔═╡ b4000000-0000-0000-0000-000000000000
# Guide: available images/sets in the chosen project (dir names under {proj}/1/).
let
    show_objs = !isempty(proj_uid) && isempty(uid)
    objs = show_objs ?
        (try readdir(joinpath(projects_dir(), proj_uid, "1")) catch; String[] end) :
        String[]
    listing = isempty(objs) ? "none found" : join(objs, ", ")
    if show_objs
        md"➡️ Set `uid`. Objects in this project: **$(listing)**"
    elseif isempty(uid)
        md""
    else
        md"✅ object `$(uid)`"
    end
end

# ╔═╡ b5000000-0000-0000-0000-000000000000
value_name = get(ENV, "CECELIA_EXAMPLE_VN", "A")  # ← segmentation / value_name

# ╔═╡ c0000000-0000-0000-0000-000000000000
md"## 2 · Load the object and its cell table"

# ╔═╡ c1000000-0000-0000-0000-000000000000
# `nothing` until both UIDs are set — keeps the reactive graph clean instead of throwing.
img = (isempty(proj_uid) || isempty(uid)) ? nothing : init_object(proj_uid, uid)

# ╔═╡ c2000000-0000-0000-0000-000000000000
# `pop_df` with pop_type "labels" = ALL cells of this segmentation, ungated (no gate names needed —
# the dataset-agnostic entry point). Swap for ("root", […]) / ("live", […]) etc. for gated pops.
df = img === nothing ? nothing : pop_df(img, "labels", String[]; value_name = value_name)

# ╔═╡ c3000000-0000-0000-0000-000000000000
df === nothing ? md"_(waiting for a project + image above)_" :
    md"Loaded **$(nrow(df)) cells** × $(ncol(df)) columns from `$(value_name)`."

# ╔═╡ d0000000-0000-0000-0000-000000000000
md"## 3 · Plot a measurement"

# ╔═╡ d1000000-0000-0000-0000-000000000000
# Numeric columns available to plot (excludes the label id).
numeric_cols = df === nothing ? String[] :
    [n for n in names(df) if eltype(df[!, n]) <: Union{Missing,Real} && n != "label"]

# ╔═╡ d2000000-0000-0000-0000-000000000000
plot_col = isempty(numeric_cols) ? nothing : first(numeric_cols)   # ← pick any of `numeric_cols`

# ╔═╡ d3000000-0000-0000-0000-000000000000
if df !== nothing && plot_col !== nothing
    draw(data(df) * mapping(plot_col) * AlgebraOfGraphics.histogram(bins = 40);
         axis = (; title = "$(value_name): $(plot_col)"))
else
    md"_(no numeric column to plot yet)_"
end

# ╔═╡ e0000000-0000-0000-0000-000000000000
md"## 4 · Export a CSV"

# ╔═╡ e1000000-0000-0000-0000-000000000000
if df !== nothing
    outdir = joinpath(projects_dir(), proj_uid, "exports")
    mkpath(outdir)
    outpath = joinpath(outdir, "$(uid)_$(value_name)_cells.csv")
    CSV.write(outpath, df)
    md"💾 wrote `$(outpath)`"
else
    md"_(nothing to export yet)_"
end

# ╔═╡ Cell order:
# ╟─a0000000-0000-0000-0000-000000000000
# ╠═a1000000-0000-0000-0000-000000000000
# ╠═a2000000-0000-0000-0000-000000000000
# ╠═a3000000-0000-0000-0000-000000000000
# ╠═a4000000-0000-0000-0000-000000000000
# ╟─b0000000-0000-0000-0000-000000000000
# ╠═b1000000-0000-0000-0000-000000000000
# ╟─b2000000-0000-0000-0000-000000000000
# ╠═b3000000-0000-0000-0000-000000000000
# ╟─b4000000-0000-0000-0000-000000000000
# ╠═b5000000-0000-0000-0000-000000000000
# ╟─c0000000-0000-0000-0000-000000000000
# ╠═c1000000-0000-0000-0000-000000000000
# ╠═c2000000-0000-0000-0000-000000000000
# ╟─c3000000-0000-0000-0000-000000000000
# ╟─d0000000-0000-0000-0000-000000000000
# ╠═d1000000-0000-0000-0000-000000000000
# ╠═d2000000-0000-0000-0000-000000000000
# ╠═d3000000-0000-0000-0000-000000000000
# ╟─e0000000-0000-0000-0000-000000000000
# ╠═e1000000-0000-0000-0000-000000000000
