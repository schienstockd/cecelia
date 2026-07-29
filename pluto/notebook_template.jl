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

# ╔═╡ 10000000-0000-0000-0000-000000000007
md"""
## Refreshing after a pipeline re-run

**Pluto reacts to CELL changes, not to files.** There is no filesystem watcher, so re-running a task
does not invalidate anything here — your plots keep showing the old data, silently. Give every cell
that READS from disk a dependency on a stamp cell, then re-running that one cell (Shift+Enter)
cascades through all of them. This is the standard pattern; see `docs/NOTEBOOKS.md`.
"""

# ╔═╡ 10000000-0000-0000-0000-000000000008
# ⟳ RE-RUN THIS CELL (Shift+Enter) after a pipeline task wrote new data.
# List every file this notebook reads. Then put a bare `DATA_STAMP` line at the top of each cell
# that reads from disk — that is what makes the refresh reach it.
# DATA_STAMP = let
#     _mt(f) = isfile(f) ? mtime(f) : 0.0
#     (labels = _mt(img_label_props_path(img, "A")), read_at = time())
# end

# ╔═╡ Cell order:
# ╠═10000000-0000-0000-0000-000000000001
# ╠═10000000-0000-0000-0000-000000000002
# ╠═10000000-0000-0000-0000-000000000003
# ╠═10000000-0000-0000-0000-000000000004
# ╟─10000000-0000-0000-0000-000000000005
# ╠═10000000-0000-0000-0000-000000000006
# ╟─10000000-0000-0000-0000-000000000007
# ╠═10000000-0000-0000-0000-000000000008
