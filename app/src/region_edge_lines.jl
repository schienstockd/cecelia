# A region's boundary (`spatialAnalysis.regionEdge`) as line segments — for the viewer route
# (`/api/viewer/region-edges`) and the movie overlay closure, so the two cannot draw different lines
# (docs/ARCHITECTURE.md → *Recording parity*). The lines ride the skeleton path (`SkeletonSegments`):
# static, each on its frame and plane.

"""
    region_edge_segments(img, name) -> SkeletonSegments | nothing

`name`'s boundary lines (`regionEdges/<name>.json`, written by `region_edge_run.py`) as segments, one
per consecutive pair of each polyline's points, in native voxel coordinates. Each line carries its frame
(`t`, -1 on a still image) and plane: a 3D run's lines are cut per image plane already, so each segment
lies on its own plane; a 2D run drawn on a stack is `flat` (plane -1, every plane). The image frame was
never part of the boundary, so there is nothing to drop here. `nothing` when the run's file is absent.
"""
function region_edge_segments(img::CciaImage, name::AbstractString)
    path = img_region_edge_path(img, name)
    isfile(path) || return nothing
    region_edge_segments(path; is_stack = :Z in img_axes(img))
end

function region_edge_segments(path::AbstractString; is_stack::Bool = false)
    doc = JSON3.read(read(path, String))
    has_z = "z" in String.(get(doc, :axes, String[]))
    flat = is_stack && !has_z
    out = SkeletonSegments((Float32[] for _ in 1:6)..., Int[], Int[], Int[], flat)
    for line in get(doc, :lines, [])
        x = Float32.(line.x); y = Float32.(line.y)
        t = Int(get(line, :t, -1)); pl = Int(get(line, :plane, 0))
        z = has_z ? Float32(pl) : 0f0
        plane = flat ? -1 : has_z ? pl : 0
        for j in 1:(length(x) - 1)
            push!(out.x0, x[j]); push!(out.y0, y[j]); push!(out.z0, z)
            push!(out.x1, x[j + 1]); push!(out.y1, y[j + 1]); push!(out.z1, z)
            push!(out.t, t); push!(out.plane, plane); push!(out.type, -1)
        end
    end
    out
end
