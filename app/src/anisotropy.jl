# Structure anisotropy — the readouts of the `segment.branching` anisotropy pass, as tidy
# DataFrames for a NOTEBOOK. Three functions, one per thing a researcher plots:
#
#   quiver_df(img)         the fibre-orientation field  → arrows          (Figure 4 panel B)
#   branch_segments(img)   the skeleton as line segments → branch network (panel B, 4th facet)
#   anisotropy_df(imgs)    one anisotropy scalar per image → scatter      (panel D)
#
# Cell tracks are deliberately NOT here: `pop_df(img, "live", pops)` already returns
# `centroid_x`/`centroid_y`/`track_id`/`centroid_t`, which is exactly what a track overlay needs.
# A `quiver_tracks` wrapper would be a second way to do what `pop_df` does. See docs/NOTEBOOKS.md
# for the three-layer recipe.
#
# Everything here is a READ of what the branching task banked — nothing is recomputed, so a plot
# can never disagree with the stored analysis.
#
# THE ONE THING TO GET RIGHT: the fibre direction is the structure tensor's **MINOR** eigenvector.
# `uns/orientation_meta["fibre_direction"]` states it and `quiver_df` is the only place that reads it —
# a hand-rolled `eigvec[…, 2, :]` elsewhere draws every arrow 90° off and still looks like a
# plausible vector field. Same rule as `cecelia.utils.anisotropy_utils.fibre_orientation` on the
# Python side. See docs/SEGMENTATION.md and docs/todo/SPATIAL_ANISOTROPY_PLAN.md A1.

using DataFrames

"""
    quiver_df(img; value_name, t=nothing) -> DataFrame

The fibre-orientation field as one row per grid box:
`t`, `iy`, `ix` (box indices), `x`, `y` (box centre, IMAGE PIXELS), `u`, `v` (a UNIT direction),
`coherence` ∈ [0,1], `length` (skeleton pixels in the box).

`t=nothing` returns every stored frame; pass a timepoint to take one. A T-collapsed run
(`integrateTime`) stores a single frame with `t = -1`.

Arrow LENGTH is deliberately not baked in — plot the unit direction and let the renderer scale to
the grid (what `ggquiver` did). Eigenvalue magnitude makes the field unreadable and is not what
the eye should read; use `coherence` for opacity/colour if you want to show it.

The direction is an **axis, not a vector**: `(u, v)` and `(-u, -v)` describe the same fibre, so
the sign carries no meaning (the old vignette negated `u` purely to suit `ggquiver`'s axis
orientation). Image `y` grows downward, so plot with the y axis reversed.

Only a 2-D grid is returned. A 3-D run has a per-Z grid that has no single overlay — re-run
branching with `flattenBranching` for a quiver.
"""
function quiver_df(img::CciaImage; value_name::AbstractString,
                   t::Union{Integer,Nothing}=nothing)::DataFrame
    lp = _branch_props(img, value_name)
    coor   = uns_array(lp, "orientation_coords")
    eigvec = uns_array(lp, "orientation_eigvec")
    (coor === nothing || eigvec === nothing) && error(
        "'$value_name' has no anisotropy grid — re-run segment.branching with Anisotropy on")
    ndims(coor) == 4 || error(
        "'$value_name' has a $(ndims(coor) - 2)D anisotropy grid; a quiver needs a 2D one " *
        "(re-run segment.branching with Flatten Z on)")
    coh = uns_array(lp, "orientation_box_coherence")
    len = uns_array(lp, "orientation_box_length")
    meta = something(uns_dict(lp, "orientation_meta"), Dict{String,Any}())

    # `t_index` maps stack position → real timepoint (-1 when time was collapsed). Never assume
    # position == t: the pass used to skip empty frames, which silently shifted the whole axis.
    tix = Int[Int(v) for v in get(meta, "t_index", collect(0:size(coor, 1) - 1))]
    slots = t === nothing ? (1:length(tix)) : findall(==(Int(t)), tix)
    isempty(slots) && error("timepoint $t is not in the anisotropy grid (frames: $(tix))")
    # ascending eigenvalues ⇒ the minor eigenvector is index 1
    vec_i = String(get(meta, "fibre_direction", "minor")) == "minor" ? 1 : size(eigvec, 4)

    ny, nx = size(coor, 2), size(coor, 3)
    n = length(slots) * ny * nx
    df = DataFrame(t = Vector{Int}(undef, n), iy = Vector{Int}(undef, n), ix = Vector{Int}(undef, n),
                   x = Vector{Float64}(undef, n), y = Vector{Float64}(undef, n),
                   u = Vector{Float64}(undef, n), v = Vector{Float64}(undef, n),
                   coherence = Vector{Float64}(undef, n), length = Vector{Float64}(undef, n))
    r = 0
    for s in slots, iy in 1:ny, ix in 1:nx
        # producer order: coor[t, y, x, comp] with comp = (y, x); eigvec[t, y, x, vec, comp]
        dy, dx = Float64(eigvec[s, iy, ix, vec_i, 1]), Float64(eigvec[s, iy, ix, vec_i, 2])
        nrm = hypot(dx, dy)
        r += 1
        df.t[r] = tix[s]; df.iy[r] = iy - 1; df.ix[r] = ix - 1
        df.x[r] = Float64(coor[s, iy, ix, 2]); df.y[r] = Float64(coor[s, iy, ix, 1])
        df.u[r] = nrm > 0 ? dx / nrm : 0.0
        df.v[r] = nrm > 0 ? dy / nrm : 0.0
        df.coherence[r] = coh === nothing ? NaN : Float64(coh[s, iy, ix])
        df.length[r] = len === nothing ? NaN : Float64(len[s, iy, ix])
    end
    df
end

"""
    branch_segments(img; value_name, t=nothing) -> DataFrame

The skeleton as drawable line segments: `label`, `x1`, `y1`, `x2`, `y2`, `branch_type`, and
`centroid_t` on a timeseries. Endpoints come from the branch table's own
`image-coord-src-*` / `image-coord-dst-*` columns, in image pixels; a 3D table's z is dropped
since this is a 2D overlay.

`branch_type` is skan's integer code — 0 endpoint-to-endpoint, 1 endpoint-to-junction,
2 junction-to-junction, 3 isolated cycle.
"""
function branch_segments(img::CciaImage; value_name::AbstractString,
                         t::Union{Integer,Nothing}=nothing)::DataFrame
    path = img_branch_props_path(img, String(value_name))
    isfile(path) ||
        error("No branch table for value_name='$value_name' — run segment.branching first")
    cols = col_names(label_props(path))
    n_sp = count(c -> startswith(c, "image-coord-src-"), cols)
    n_sp >= 2 || error("'$value_name' branch table has no endpoint coordinates")
    # the trailing two spatial axes are always (y, x); a 3D table also has a leading z we ignore
    yi, xi = n_sp - 2, n_sp - 1
    want = ["image-coord-src-$yi", "image-coord-src-$xi",
            "image-coord-dst-$yi", "image-coord-dst-$xi", "branch-type", "centroid_t"]
    # unknown columns are dropped with a warning, so `centroid_t` is safe to ask for on a still
    df = label_props(path) |> lp -> select_cols(lp, want) |> as_df
    out = DataFrame(label = df.label,
                    x1 = Float64.(df[!, want[2]]), y1 = Float64.(df[!, want[1]]),
                    x2 = Float64.(df[!, want[4]]), y2 = Float64.(df[!, want[3]]),
                    branch_type = Int.(df[!, "branch-type"]))
    if "centroid_t" in names(df)
        out.centroid_t = Float64.(df.centroid_t)
        t === nothing || (out = out[out.centroid_t .== Float64(t), :])
    elseif t !== nothing
        error("'$value_name' branch table has no time axis — it was not run on a timeseries")
    end
    out
end

"""
    anisotropy_df(img_or_imgs; value_name=nothing) -> DataFrame

The per-image structure summary the branching task banks in `uns['orientation_summary']` — one row
per image per anisotropy frame:

`uID`, `value_name`, `t`, `anisotropy`, `occupancy`, `linear_density`, `skewness`, `cv`,
`MF_full_length`, `branching_act`.

`anisotropy` is the length-weighted mean coherence (ILEE's `by_length`): **0 = uniform,
1 = non-uniform**. Real fibrous tissue sits around 0.1–0.4, so a low number is not a defect.
This is the column the old vignettes merged onto `exp.info` as `SHG.anisotropy` and scattered
against behaviour-state composition (Figure 4 panel D).

`value_name=nothing` reads every branch table an image has — one image can carry several
(`SHG` collagen + `DCs` network, as in `behaviourUbiTom3P.Rmd`), so the result is long-format and
you pick with a filter. Images with no branch table, or none with anisotropy, contribute no rows.
"""
function anisotropy_df(imgs::AbstractVector{<:CciaImage};
                       value_name::Union{AbstractString,Nothing}=nothing)::DataFrame
    frames = DataFrame[]
    for img in imgs
        vns = value_name === nothing ? img_branch_value_names(img) : [String(value_name)]
        for vn in vns
            path = img_branch_props_path(img, vn)
            isfile(path) || continue
            s = uns_df(label_props(path), "orientation_summary")
            (s === nothing || nrow(s) == 0) && continue
            meta = something(uns_dict(label_props(path), "orientation_meta"), Dict{String,Any}())
            tix = Int[Int(v) for v in get(meta, "t_index", collect(0:nrow(s) - 1))]
            insertcols!(s, 1, :uID => fill(img.uid, nrow(s)), :value_name => fill(vn, nrow(s)),
                        :t => (length(tix) == nrow(s) ? tix : collect(0:nrow(s) - 1)))
            push!(frames, s)
        end
    end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols = :union), frames)
end

anisotropy_df(img::CciaImage; kwargs...) = anisotropy_df([img]; kwargs...)

function _branch_props(img::CciaImage, value_name::AbstractString)::LabelProps
    path = img_branch_props_path(img, String(value_name))
    isfile(path) ||
        error("No branch table for value_name='$value_name' — run segment.branching first")
    label_props(path)
end
