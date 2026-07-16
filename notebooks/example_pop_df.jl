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
using Cecelia, DataFrames, CeceliaNb

# ╔═╡ a0000000-0000-0000-0000-000000000000
md"""
# Populations — the `pop_df` accessor

How to pull populations out of an image with the unified `pop_df` accessor: gated cells of a
segmentation (`flow`), several pooled into one table, and the **tracked** subset (`live`). `pop_df`
reads the H5AD via the `label_props` chain, evaluates gates in-process, and pools across
segmentations — see `docs/POPULATION.md`. Ported from the old `populations.ipynb`, now UID-free.
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
img = (isempty(proj_uid) || isempty(uid)) ? nothing : init_object(proj_uid, uid)

# ╔═╡ b4000000-0000-0000-0000-000000000000
# `label_props` keys are the segmentations (value_names); `_active` is the default one.
segs = img === nothing ? String[] : versioned_keys(img.label_props)

# ╔═╡ b5000000-0000-0000-0000-000000000000
img === nothing ? md"➡️ set `proj_uid` + `uid` above (or `CECELIA_EXAMPLE_PROJ`/`_UID`)." :
    md"""Loaded **$(img.name)** — segmentations: **$(join(segs, ", "))**"""

# ╔═╡ c0000000-0000-0000-0000-000000000000
md"""
## 2 · What's gated on each segmentation?

Each segmentation has its own gating sidecar `gating/{value_name}.json`. List the flow population
paths per segmentation. Set `flow_pop` below to the leaf name you want to pull.
"""

# ╔═╡ c1000000-0000-0000-0000-000000000000
gated = img === nothing ? DataFrame() :
    DataFrame(value_name = segs,
              pops = [join(pop_paths(load_pop_map(img; value_name = vn, pop_type = "flow")), ", ")
                      for vn in segs])

# ╔═╡ c2000000-0000-0000-0000-000000000000
# Leaf name of the population to pull (must exist in the gated list above). Auto-picks the first
# non-root population found if you leave it blank.
flow_pop = let
    override = get(ENV, "CECELIA_EXAMPLE_POP", "")
    if !isempty(override)
        override
    elseif img === nothing
        ""
    else
        leaves = String[]
        for vn in segs, p in pop_paths(load_pop_map(img; value_name = vn, pop_type = "flow"))
            p == "/" || push!(leaves, pop_name(p))
        end
        isempty(leaves) ? "" : first(unique(leaves))
    end
end

# ╔═╡ d0000000-0000-0000-0000-000000000000
md"""
## 3 · Flow populations — one segmentation at a time

A **leading-slash** path (`"/qc"`) is resolved *within* the `value_name` segmentation. Membership is
computed by evaluating the gate in-process; intensity columns come back under their channel names.
"""

# ╔═╡ d1000000-0000-0000-0000-000000000000
# Guarded pull — returns an empty frame (not an error) if the population isn't present.
safe_pop_df(args...; kw...) = try pop_df(args...; kw...) catch; DataFrame() end

# ╔═╡ d2000000-0000-0000-0000-000000000000
per_seg = (img === nothing || isempty(flow_pop)) ? DataFrame() :
    DataFrame(value_name = segs,
              n = [nrow(safe_pop_df(img, "flow", ["/$(flow_pop)"]; value_name = vn)) for vn in segs])

# ╔═╡ e0000000-0000-0000-0000-000000000000
md"""
## 4 · Pool all segmentations in one call

A **prefixed** path (`"A/qc"`) names its own value_name, so one call pulls the population from
several segmentations and `vcat`s them into a single table with a `value_name` column. `nb_count`
(from `CeceliaNb`) tallies cells per `(value_name, pop)` — the same numbers the Analysis board shows.
"""

# ╔═╡ e1000000-0000-0000-0000-000000000000
pooled = (img === nothing || isempty(flow_pop)) ? DataFrame() :
    safe_pop_df(img, "flow", ["$(vn)/$(flow_pop)" for vn in segs])

# ╔═╡ e2000000-0000-0000-0000-000000000000
nb_count(pooled)

# ╔═╡ f0000000-0000-0000-0000-000000000000
md"""
## 5 · Tracked (`live`) populations — the `_tracked` derived filter

`_tracked` is a **derived filtered population**: the parent gate intersected with `track_id > 0`.
For `pop_type="live"`, `pop_df` layers this filter on at read time (there is no `live` gating file).
Tracked cells are a subset of the gated cells — the same cells, minus those that never got a track.
"""

# ╔═╡ f1000000-0000-0000-0000-000000000000
tracked = (img === nothing || isempty(flow_pop)) ? DataFrame() :
    safe_pop_df(img, "live", ["$(vn)/$(flow_pop)/_tracked" for vn in segs]; pop_cols = ["track_id"])

# ╔═╡ f2000000-0000-0000-0000-000000000000
nb_count(tracked)

# ╔═╡ f3000000-0000-0000-0000-000000000000
# gated vs tracked, side by side
(isempty(pooled) || isempty(tracked)) ? md"_(no gated/tracked data yet)_" :
    leftjoin(rename(nb_count(pooled), :n => :gated),
             rename(select(nb_count(tracked), :value_name, :n), :n => :tracked),
             on = :value_name)

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
# ╟─d0000000-0000-0000-0000-000000000000
# ╠═d1000000-0000-0000-0000-000000000000
# ╠═d2000000-0000-0000-0000-000000000000
# ╟─e0000000-0000-0000-0000-000000000000
# ╠═e1000000-0000-0000-0000-000000000000
# ╠═e2000000-0000-0000-0000-000000000000
# ╟─f0000000-0000-0000-0000-000000000000
# ╠═f1000000-0000-0000-0000-000000000000
# ╠═f2000000-0000-0000-0000-000000000000
# ╠═f3000000-0000-0000-0000-000000000000
