### A Pluto.jl notebook ###
# v1.0.3

using Markdown
using InteractiveUtils

# ╔═╡ 10000000-0000-0000-0000-000000000001
# Activate the Notebooks env (path-sources the dev Cecelia). Keep this as the first cell.
begin
    import Pkg
    Pkg.activate(get(ENV, "CECELIA_PLUTO_ENV", joinpath(@__DIR__, "..", "pluto")))
end

# ╔═╡ 10000000-0000-0000-0000-000000000002
using Cecelia

# ╔═╡ 10000000-0000-0000-0000-000000000003
using DataFrames, AlgebraOfGraphics, CairoMakie, CSV

# ╔═╡ 10000000-0000-0000-0000-000000000004
Cecelia.init_cecelia!()

# ╔═╡ 10000000-0000-0000-0000-000000000005
md"""
# New notebook

Set a project + image and go. Accessors: `init_object(proj_uid, uid)`, `pop_df(img, pop_type, pops;
value_name=…)`, `label_props(img; value_name=…) |> as_df`. Plot with AlgebraOfGraphics + CairoMakie;
export with `CSV.write`. See `example_populations.jl` for a worked example.
"""

# ╔═╡ 10000000-0000-0000-0000-000000000006
# proj_uid = ""
# uid = ""
# img = init_object(proj_uid, uid)

# ╔═╡ Cell order:
# ╠═10000000-0000-0000-0000-000000000001
# ╠═10000000-0000-0000-0000-000000000002
# ╠═10000000-0000-0000-0000-000000000003
# ╠═10000000-0000-0000-0000-000000000004
# ╟─10000000-0000-0000-0000-000000000005
# ╠═10000000-0000-0000-0000-000000000006
