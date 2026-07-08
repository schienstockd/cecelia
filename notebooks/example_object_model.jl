### A Pluto.jl notebook ###
# v1.0.3

using Markdown
using InteractiveUtils

# ╔═╡ a1000000-0000-0000-0000-000000000000
begin
    import Pkg
    Pkg.activate(get(ENV, "CECELIA_PLUTO_ENV", joinpath(@__DIR__, "..", "pluto")))
end

# ╔═╡ a2000000-0000-0000-0000-000000000000
using Cecelia, DataFrames

# ╔═╡ a0000000-0000-0000-0000-000000000000
md"""
# The object model — navigating a project (read-only)

A quick tour of `load_project` / `init_object`: how a project, its sets, and its images fit together
on disk, and how to reach the segmentations of an image. Read-only — it never writes. (Creating
projects, importing images and opening Napari are done through the app UI, not notebooks.)
Ported from the old `backend_model.ipynb`.
"""

# ╔═╡ a3000000-0000-0000-0000-000000000000
Cecelia.init_cecelia!()

# ╔═╡ b0000000-0000-0000-0000-000000000000
proj_uid = get(ENV, "CECELIA_EXAMPLE_PROJ", "")   # ← your project UID

# ╔═╡ b1000000-0000-0000-0000-000000000000
md"## 1 · The project"

# ╔═╡ b2000000-0000-0000-0000-000000000000
proj = isempty(proj_uid) ? nothing : load_project(proj_uid)

# ╔═╡ b3000000-0000-0000-0000-000000000000
proj === nothing ? md"➡️ set `proj_uid` above (or `CECELIA_EXAMPLE_PROJ`)." :
    md"**$(proj.name)** · kind `$(proj.kind)` · $(length(proj.set_uids)) set(s)"

# ╔═╡ c0000000-0000-0000-0000-000000000000
md"""
## 2 · Sets and their images

`init_object(projectUID, uid)` dispatches on the stored `class` field — a set UID returns a
`CciaSet`, an image UID a `CciaImage`. Here we list each set and its member images.
"""

# ╔═╡ c1000000-0000-0000-0000-000000000000
set_table = proj === nothing ? DataFrame() : let rows = NamedTuple[]
    for suid in proj.set_uids
        s = init_object(proj_uid, suid)
        push!(rows, (; set = s.name, set_uid = suid, images = length(s.image_uids),
                       image_uids = join(s.image_uids, ", ")))
    end
    DataFrame(rows)
end

# ╔═╡ d0000000-0000-0000-0000-000000000000
md"## 3 · An image and its segmentations"

# ╔═╡ d1000000-0000-0000-0000-000000000000
# First image of the first set (or set CECELIA_EXAMPLE_UID to pick one).
img_uid = let e = get(ENV, "CECELIA_EXAMPLE_UID", "")
    !isempty(e) ? e :
    (proj !== nothing && !isempty(proj.set_uids)) ?
        (first(init_object(proj_uid, first(proj.set_uids)).image_uids)) : ""
end

# ╔═╡ d2000000-0000-0000-0000-000000000000
img = (proj === nothing || isempty(img_uid)) ? nothing : init_object(proj_uid, img_uid)

# ╔═╡ d3000000-0000-0000-0000-000000000000
img === nothing ? md"_(no image resolved)_" :
    md"""
    **$(img.name)** (`$(img_uid)`)
    - segmentations: **$(join([v for v in value_names(img.label_props) if v != "_active"], ", "))**
    - active: **$(get(img.label_props, "_active", "—"))**
    """

# ╔═╡ d4000000-0000-0000-0000-000000000000
md"""
From here, `pop_df(img, pop_type, pops; value_name=…)` and `label_props(img; value_name=…) |> as_df`
give you the cell tables — see `example_pop_df.jl` and `example_populations.jl`.
"""

# ╔═╡ Cell order:
# ╟─a0000000-0000-0000-0000-000000000000
# ╠═a1000000-0000-0000-0000-000000000000
# ╠═a2000000-0000-0000-0000-000000000000
# ╠═a3000000-0000-0000-0000-000000000000
# ╠═b0000000-0000-0000-0000-000000000000
# ╟─b1000000-0000-0000-0000-000000000000
# ╠═b2000000-0000-0000-0000-000000000000
# ╟─b3000000-0000-0000-0000-000000000000
# ╟─c0000000-0000-0000-0000-000000000000
# ╠═c1000000-0000-0000-0000-000000000000
# ╟─d0000000-0000-0000-0000-000000000000
# ╠═d1000000-0000-0000-0000-000000000000
# ╠═d2000000-0000-0000-0000-000000000000
# ╟─d3000000-0000-0000-0000-000000000000
# ╟─d4000000-0000-0000-0000-000000000000
