# A skeleton (`segment.branching`) as line segments — what the viewer and every recording draw.
#
# The branching task stores each branch's path as a simplified polyline in its branch table
# (`uns/paths`, `python/cecelia/utils/skeleton_paths.py`). One reader turns them into segments for
# both the viewer route (`/api/viewer/skeletons`) and the movie overlay closure, so the two cannot
# draw different lines (docs/ARCHITECTURE.md → *Recording parity*).

"""
    SkeletonSegments

One skeleton's segments in native voxel coordinates. `t[i]` is segment i's frame, `-1` for a skeleton
without time (drawn on every frame). `plane[i]` is the plane it draws on: a segment crossing planes is
split so each piece lies within one (`k ± 0.5`), so a plane view shows a branch on every plane it
passes through. `flat`: a Z-flattened skeleton on a stack — `plane` -1, drawn on every plane (its `z`
is 0). The plane is computed here, once, so the viewer and a movie cannot round it apart. `type[i]`
is its branch's skan `branch-type` (0–3, -1 when the table has none) — what the lines are coloured by
(`palettes.json` → `branchTypeColours`).
"""
struct SkeletonSegments
    x0::Vector{Float32}; y0::Vector{Float32}; z0::Vector{Float32}
    x1::Vector{Float32}; y1::Vector{Float32}; z1::Vector{Float32}
    t::Vector{Int}
    plane::Vector{Int}
    type::Vector{Int}
    flat::Bool
end

Base.length(s::SkeletonSegments) = length(s.x0)

"""
    skeleton_segments(img, value_name) -> SkeletonSegments | nothing

`value_name`'s skeleton as segments (one per consecutive pair of each branch's polyline points).
`nothing` when it has no branch table, or a table written before the paths were.
"""
function skeleton_segments(img::CciaImage, value_name::AbstractString)
    path = img_branch_props_path(img, value_name)
    isfile(path) || return nothing
    skeleton_segments(path; is_stack = :Z in img_axes(img))
end

function skeleton_segments(h5ad_path::AbstractString; is_stack::Bool = false)
    lp = label_props(h5ad_path)
    bp = branch_paths(lp)
    bp === nothing && return nothing
    offsets, coords, axes = bp
    n_rows = length(offsets) - 1
    tcols = temporal_columns(lp)
    tvals = isempty(tcols) ? fill(-1, n_rows) :
        round.(Int, coalesce.(as_df(select_cols(lp, tcols); include_x = false, include_obs = false)[!, tcols[1]], -1))
    bt = label_props(h5ad_path)
    btype = isempty(selectable_cols(bt, ["branch-type"])) ? fill(-1, n_rows) :
        round.(Int, coalesce.(as_df(select_cols(bt, ["branch-type"]))[!, "branch-type"], -1))
    col(a) = (i = findfirst(==(a), axes); i === nothing ? nothing : i)
    ix, iy, iz = col("x"), col("y"), col("z")
    (ix === nothing || iy === nothing) && error("skeleton_segments: path axes $(axes) lack x / y")
    flat = iz === nothing && is_stack
    out = SkeletonSegments((Float32[] for _ in 1:6)..., Int[], Int[], Int[], flat)
    zat(j) = iz === nothing ? 0f0 : coords[j, iz]
    for r in 1:n_rows, j in (offsets[r] + 1):(offsets[r + 1] - 1)
        a = (coords[j, ix], coords[j, iy], zat(j))
        b = (coords[j + 1, ix], coords[j + 1, iy], zat(j + 1))
        for (p0, p1, pl) in (flat ? [(a, b, -1)] : _plane_pieces(a, b))
            push!(out.x0, p0[1]); push!(out.y0, p0[2]); push!(out.z0, p0[3])
            push!(out.x1, p1[1]); push!(out.y1, p1[2]); push!(out.z1, p1[3])
            push!(out.t, tvals[r]); push!(out.plane, pl); push!(out.type, btype[r])
        end
    end
    out
end

# Segment a → b (x, y, z voxels) split where z crosses a plane boundary (k + 0.5), each piece with the
# plane it lies in: a branch running from plane 3 to 6 draws on 3, 4, 5 and 6, not only on 6.
function _plane_pieces(a::NTuple{3,Float32}, b::NTuple{3,Float32})
    cuts = Float32[0f0]
    if a[3] != b[3]
        lo, hi = minmax(a[3], b[3])
        for k in ceil(Int, lo - 0.5f0):floor(Int, hi - 0.5f0)
            f = (k + 0.5f0 - a[3]) / (b[3] - a[3])
            0f0 < f < 1f0 && push!(cuts, f)
        end
    end
    sort!(cuts); push!(cuts, 1f0)
    lerp(f) = a .+ f .* (b .- a)
    [(lerp(cuts[q]), lerp(cuts[q + 1]),
      round(Int, (a[3] + (cuts[q] + cuts[q + 1]) / 2 * (b[3] - a[3])), RoundNearestTiesUp))
     for q in 1:(length(cuts) - 1)]
end

"""
    skeleton_segments_at(segs, t) -> indices

The segments `segs` draws at frame `t`: its own frame's, or every one of a skeleton without time.
"""
skeleton_segments_at(segs::SkeletonSegments, t::Integer) =
    findall(x -> x == t || x == -1, segs.t)
