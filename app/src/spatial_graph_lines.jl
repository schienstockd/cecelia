# A neighbour graph (`spatialAnalysis.cellNeighbours`) as line segments — one per edge, centroid to
# centroid — for the viewer route (`/api/viewer/spatial-graphs`) and the movie overlay closure, so the
# two cannot draw different lines (docs/ARCHITECTURE.md → *Recording parity*). The lines ride the
# skeleton path (`SkeletonSegments`, `skeleton_segments_at`): static, each on its frame and plane.

"""
    SpatialGraphSegments

A graph's drawable edges: `segs` (native voxel coordinates, `type` -1) and, per segment, the two cells
it joins as `(valueName, label)` (`a_vn`/`a_label`, `b_vn`/`b_label`) — what a filter to the shown
populations matches on. `n_cross_frame`: edges of a graph pooled over time whose two cells are on
different frames, not drawn (no frame to draw them on). `n_unplaced`: edges with a cell missing from
its cell table (re-segmented since the graph was built). `stale`: a segmentation's mask store was
written after the graph, so a label may now name a different cell — the edges are drawn, and the route
says so. The mask, not the cell table: tasks write columns back into the table (regions, contacts,
clusters) without changing a label, and one of them, clustRegions, reads this very graph.
"""
struct SpatialGraphSegments
    segs::SkeletonSegments
    a_vn::Vector{String}; a_label::Vector{Int}
    b_vn::Vector{String}; b_label::Vector{Int}
    n_cross_frame::Int
    n_unplaced::Int
    stale::Bool
end

Base.length(g::SpatialGraphSegments) = length(g.segs)

"""
    spatial_graph_segments(img, suffix) -> SpatialGraphSegments | nothing

`suffix`'s neighbour graph as segments. Node identity comes from the graph; positions from each
node's cell table by label, read by axis NAME (`view_centroid_cols`) — the graph's own `obsm/spatial`
names no axes. A node's frame is the graph's `_t` (built per timepoint), else its cell table's
`centroid_t`, else -1 (a still image: every frame). `nothing` when the graph file is absent.
"""
function spatial_graph_segments(img::CciaImage, suffix::AbstractString)
    path = img_spatial_graph_path(img, suffix)
    isfile(path) || return nothing
    grid = spatial_graph_window_grid(path)
    grid === nothing || return window_grid_segments(grid, first(img_physical_sizes(img));
                                                    is_stack = :Z in img_axes(img))
    g = spatial_graph_edges(path)
    n = length(g.label)
    x = fill(NaN32, n); y = fill(NaN32, n); z = zeros(Float32, n)
    tt = g.t === nothing ? fill(-1, n) : copy(g.t)
    stale = false
    for vn in unique(g.value_name)
        rows = findall(==(vn), g.value_name)
        path_vn = img_label_props_path(img, vn)
        isfile(path_vn) || continue
        # re-segmented since: its mask store is newer than the graph (`store_written_at` reads the
        # store's own metadata — 0 for a table with no mask, which is never stale)
        stale |= store_written_at(img_labels_path(img, vn)) > mtime(path)
        lp = label_props(path_vn; value_name = vn)
        view_centroid_cols(lp; order = [:x, :y, :z])
        filter_rows(lp, g.label[rows])
        df = as_df(lp; include_x = false, include_obs = false)
        has(c) = c in names(df)
        at = Dict{Int,Int}(Int(l) => k for (k, l) in enumerate(df[!, "label"]))
        for r in rows
            k = get(at, g.label[r], nothing)
            k === nothing && continue
            f(c) = (v = df[k, c]; ismissing(v) ? NaN32 : Float32(v))
            x[r] = f("centroid_x"); y[r] = f("centroid_y")
            has("centroid_z") && (z[r] = f("centroid_z"))
            if g.t === nothing && has("centroid_t")
                v = df[k, "centroid_t"]
                tt[r] = ismissing(v) || !isfinite(v) ? -1 : round(Int, v)
            end
        end
    end
    segs = SkeletonSegments((Float32[] for _ in 1:6)..., Int[], Int[], Int[], false)
    a_vn = String[]; a_label = Int[]; b_vn = String[]; b_label = Int[]
    cross = 0; unplaced = 0
    for (p, q) in zip(g.i, g.j)
        if !(isfinite(x[p]) && isfinite(y[p]) && isfinite(x[q]) && isfinite(y[q]))
            unplaced += 1; continue
        end
        tt[p] == tt[q] || (cross += 1; continue)
        for (p0, p1, pl) in _plane_pieces((x[p], y[p], z[p]), (x[q], y[q], z[q]))
            push!(segs.x0, p0[1]); push!(segs.y0, p0[2]); push!(segs.z0, p0[3])
            push!(segs.x1, p1[1]); push!(segs.y1, p1[2]); push!(segs.z1, p1[3])
            push!(segs.t, tt[p]); push!(segs.plane, pl); push!(segs.type, -1)
            push!(a_vn, g.value_name[p]); push!(a_label, g.label[p])
            push!(b_vn, g.value_name[q]); push!(b_label, g.label[q])
        end
    end
    SpatialGraphSegments(segs, a_vn, a_label, b_vn, b_label, cross, unplaced, stale)
end

"""
    window_grid_segments(grid, sizes_zyx; is_stack=false) -> SpatialGraphSegments

A fixed-window graph drawn as its TILE BORDERS: one line per tile edge across the grid, in native voxel
coordinates (the grid is µm, `sizes_zyx` µm per voxel), on every frame (`t` -1) and — through a stack —
every plane (`flat`, plane -1). The tiles are what a cell is placed on; each window spans the 2 × 2
tiles around its centre. A grid over Z (windows as cubes) is drawn as its XY grid. A line has no cells
at either end (value_name ""), which "only between shown populations" keeps (`spatial_graph_between`).
"""
function window_grid_segments(grid, sizes_zyx; is_stack::Bool = false)
    t = grid.tile
    ext(ax) = (i = findfirst(==(ax), grid.axes); i === nothing ? 0.0 : grid.shape[i] * t)
    n(ax)   = (i = findfirst(==(ax), grid.axes); i === nothing ? 0 : grid.shape[i])
    ux = physical_size_for_axis(sizes_zyx, :x); uy = physical_size_for_axis(sizes_zyx, :y)
    X, Y = ext("x") / ux, ext("y") / uy
    segs = SkeletonSegments((Float32[] for _ in 1:6)..., Int[], Int[], Int[], is_stack)
    line!(x0, y0, x1, y1) = (push!(segs.x0, x0); push!(segs.y0, y0); push!(segs.z0, 0f0);
                             push!(segs.x1, x1); push!(segs.y1, y1); push!(segs.z1, 0f0);
                             push!(segs.t, -1); push!(segs.plane, is_stack ? -1 : 0); push!(segs.type, -1))
    for k in 0:n("x"); line!(Float32(k * t / ux), 0f0, Float32(k * t / ux), Float32(Y)); end
    for k in 0:n("y"); line!(0f0, Float32(k * t / uy), Float32(X), Float32(k * t / uy)); end
    m = length(segs)
    SpatialGraphSegments(segs, fill("", m), fill(-1, m), fill("", m), fill(-1, m), 0, 0, false)
end

"""
    spatial_graph_between(g, cells) -> SkeletonSegments

`g`'s segments whose two cells are both in `cells` (a set of `(valueName, label)`) — the edges between
the populations shown, for "only between shown populations". The viewer filters the same way
(`graphEdgesBetween`), from the same `(valueName, label)` per segment end.
"""
function spatial_graph_between(g::SpatialGraphSegments, cells::AbstractSet{Tuple{String,Int}})
    s = g.segs
    # a window graph's tile borders join no cells (value_name "") — they are kept whatever is shown
    k = [i for i in 1:length(s) if isempty(g.a_vn[i]) ||
         ((g.a_vn[i], g.a_label[i]) in cells && (g.b_vn[i], g.b_label[i]) in cells)]
    SkeletonSegments(s.x0[k], s.y0[k], s.z0[k], s.x1[k], s.y1[k], s.z1[k], s.t[k], s.plane[k], s.type[k], s.flat)
end
