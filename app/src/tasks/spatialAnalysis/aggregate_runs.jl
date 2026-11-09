# ── Named aggregate runs — column names + the pure steps both aggregate routes share ──
#
# An aggregate run writes under a NAME (`aggregateName`, namespace `obsCols`), so several scenarios sit
# side by side on one segmentation and each run's params are banked and restored under its name, like a
# cluster suffix or an HMM `colName`. Columns are `<pt>.cell.<kind>#<name>` — the `#` run separator the
# contact columns already use (`<pt>.cell.contact#<target>`), so `aggregate.id#…` / `aggregate.event_id#…`
# stay id-like to the measure picker (`plots/obsMeasures.ts` → ID_LIKE). Columns written before runs had
# names carry no `#<name>`; they list and delete as the unnamed run (`analysis_runs.jl`).
#
# The steps here are pure (coords + frames in, vectors out) so the semantics are unit-tested without an
# image: exclusion, isolation, the size cap, linking aggregates and isolated cells across frames into
# EVENTS, and whether a reference population was there before an event began. See
# docs/todo/SPATIAL_REGIONS_PLAN.md → Decision 17.

using NearestNeighbors: KDTree, inrange   # `_isolated_mask`; the exclusion reuses `_nearest_contacts`

const AGGREGATE_DEFAULT_NAME = "default"

# Every column kind a named run may write. The first three always; the event columns only when the run
# links across frames, `preceded` only when it was given a population to look for.
const AGGREGATE_COL_KINDS = ("is.aggregate", "aggregate.id", "is.isolated",
                             "aggregate.event_id", "aggregate.frames", "aggregate.preceded")

"""`<pt>.cell.<kind>#<name>` — one column of a named aggregate run; `name == ""` is the unnamed legacy run."""
aggregate_col(pt::AbstractString, kind::AbstractString, name::AbstractString) =
    isempty(name) ? "$pt.cell.$kind" : "$pt.cell.$kind#$name"

"""All columns a run named `name` can own on namespace `pt` — what a delete or a re-run clears."""
aggregate_run_cols(pt::AbstractString, name::AbstractString) =
    [aggregate_col(pt, k, name) for k in AGGREGATE_COL_KINDS]

"""The auto-created filter pops of a run: `(_aggregated_<name>, _isolated_<name>)`."""
aggregate_pop_names(name::AbstractString) =
    ("$(AGGREGATED_POP_NAME)_$name", "$(ISOLATED_POP_PREFIX)_$name")

"""Per-run QC key, as HMM keys `<vn>.<colName>`: runs under different names keep their own QC."""
aggregate_qc_key(value_name::AbstractString, name::AbstractString) = "$value_name.$name"

aggregate_run_name(params::AbstractDict) =
    (n = strip(string(something(get(params, "aggregateName", nothing), AGGREGATE_DEFAULT_NAME)));
     isempty(n) ? AGGREGATE_DEFAULT_NAME : String(n))

# Row indices grouped by frame; one group of every row when `frames === nothing` (still image, or pooled).
_rows_by_frame(n::Integer, frames) = frames === nothing ? Dict(0.0 => collect(1:n)) :
    (d = Dict{Float64,Vector{Int}}(); for (i, t) in enumerate(frames); push!(get!(d, Float64(t), Int[]), i); end; d)

"""
    _within_mask(coords, frames, ref_coords, ref_frames, dist) -> BitVector

True for each row with a reference point within `dist` (µm) in the SAME frame — the contacts' nearest-
neighbour search (`_nearest_contacts`, cellContacts.jl), cut at `dist`. Rows are points, columns spatial
dims. Exclusion uses this: a cell beside an intact cell is not debris. `dist ≤ 0` = off.
"""
function _within_mask(coords::AbstractMatrix, frames, ref_coords::AbstractMatrix, ref_frames, dist::Real)::BitVector
    (dist <= 0 || size(ref_coords, 1) == 0 || size(coords, 1) == 0) && return falses(size(coords, 1))
    d, _ = _nearest_contacts(Float64.(coords), frames, Float64.(ref_coords),
                             zeros(Int, size(ref_coords, 1)), ref_frames)
    BitVector(.!isnan.(d) .& (d .<= dist))
end

"""
    legacy_aggregate_cols(present, pt, name) -> Vector{String}

The UNNAMED run's columns (from before runs had names) that a `default` run supersedes, among the
`present` obs columns — the default run is that run's successor, so it takes them away rather than
leave `_aggregated` quietly showing the old cells. Empty for any other name.
"""
legacy_aggregate_cols(present, pt::AbstractString, name::AbstractString) =
    name == AGGREGATE_DEFAULT_NAME ? intersect(aggregate_run_cols(pt, ""), present) : String[]

"""True for each row with NO other row within `eps` (µm) in its frame — a single, unaccompanied cell."""
function _isolated_mask(coords::AbstractMatrix, frames, eps::Real)::BitVector
    out = falses(size(coords, 1))
    for (_, rows) in _rows_by_frame(size(coords, 1), frames)
        length(rows) == 1 && (out[rows[1]] = true; continue)
        tree = KDTree(permutedims(Float64.(coords[rows, :])))
        hits = inrange(tree, permutedims(Float64.(coords[rows, :])), Float64(eps))
        out[rows] .= length.(hits) .== 1          # only itself
    end
    out
end

"""Aggregate ids with an aggregate larger than `max_cells` zeroed (`max_cells ≤ 0` = no cap)."""
function _cap_aggregates(ids::AbstractVector{<:Integer}, max_cells::Integer)::Vector{Int}
    max_cells <= 0 && return collect(Int, ids)
    n = Dict{Int,Int}()
    for i in ids; i > 0 && (n[i] = get(n, i, 0) + 1); end
    [i > 0 && n[i] > max_cells ? 0 : Int(i) for i in ids]
end

"""
    _link_events(frames, centroids, max_gap, max_dist) -> Vector{Int}

Link per-frame events (rows) into event tracks: frame by frame, each event joins the nearest open track
whose last position is within `max_dist` µm and whose last frame is ≤ `max_gap` frames back, else starts
one. One event per track per frame — two events of a frame never merge into one track. Greedy nearest
pairs; enough for the sparse events this links (aggregates, isolated cells), not a cell tracker.
"""
function _link_events(frames::AbstractVector, centroids::AbstractMatrix, max_gap::Integer, max_dist::Real)::Vector{Int}
    n = length(frames); ids = zeros(Int, n); n == 0 && return ids
    last_t = Float64[]; last_c = Vector{Vector{Float64}}()
    for t in sort(unique(Float64.(frames)))
        rows = findall(==(t), frames)
        open = [k for k in eachindex(last_t) if 0 < t - last_t[k] <= max_gap]
        pairs = [(sqrt(sum(abs2, centroids[r, :] .- last_c[k])), r, k) for r in rows for k in open]
        filter!(p -> p[1] <= max_dist, pairs); sort!(pairs; by = first)
        used = Set{Int}()
        for (_, r, k) in pairs
            (ids[r] != 0 || k in used) && continue
            ids[r] = k; push!(used, k)
        end
        for r in rows
            ids[r] == 0 && (push!(last_t, t); push!(last_c, Float64.(centroids[r, :])); ids[r] = length(last_t))
            last_t[ids[r]] = t; last_c[ids[r]] = Float64.(centroids[r, :])
        end
    end
    ids
end

"""
    _preceded_fraction(t0, centre, ref_by_frame, back, dist, first_frame) -> Float64

Share of the `back` frames before `t0` in which a reference point lay within `dist` µm of `centre` —
"was an intact cell here just before this debris appeared". `ref_by_frame` maps a frame to its reference
points (rows × dims; `_points_by_frame`). `NaN` when the event starts on the movie's first frame (nothing
before it to look at). Frames are integers on the movie's time axis.
"""
function _preceded_fraction(t0::Real, centre::AbstractVector, ref_by_frame::AbstractDict,
                            back::Integer, dist::Real, first_frame::Real)::Float64
    window = Int(max(first_frame, t0 - back)):Int(t0 - 1)
    isempty(window) && return NaN
    hit = count(window) do t
        pts = get(ref_by_frame, Float64(t), nothing)
        pts !== nothing && any(i -> sqrt(sum(abs2, view(pts, i, :) .- centre)) <= dist, axes(pts, 1))
    end
    hit / length(window)
end

"""Reference points grouped by frame, for `_preceded_fraction`."""
_points_by_frame(coords::AbstractMatrix, frames) =
    Dict(t => Float64.(coords[rows, :]) for (t, rows) in _rows_by_frame(size(coords, 1), frames))

"""
    _aggregate_events(ids, isolated, coords, frames, max_gap, max_dist) -> Vector{Int}

Per-cell EVENT id (0 = none): each aggregate (at its members' mean position) and each isolated cell is
an event in its frame, and `_link_events` joins them across frames. Aggregates and isolated cells link
separately — a clump that thins to one cell starts a new event rather than continuing the clump's.
"""
function _aggregate_events(ids::AbstractVector{<:Integer}, isolated::AbstractVector{Bool},
                           coords::AbstractMatrix, frames::AbstractVector, max_gap::Integer, max_dist::Real)::Vector{Int}
    ev = zeros(Int, length(ids))
    members = Dict{Int,Vector{Int}}()
    for (r, a) in enumerate(ids); a > 0 && push!(get!(members, a, Int[]), r); end
    aggs = sort(collect(keys(members)))
    if !isempty(aggs)
        cent = reduce(vcat, [sum(coords[members[a], :]; dims = 1) ./ length(members[a]) for a in aggs])
        aev  = _link_events([frames[first(members[a])] for a in aggs], cent, max_gap, max_dist)
        for (k, a) in enumerate(aggs); ev[members[a]] .= aev[k]; end
    end
    rows = findall(isolated)
    off  = maximum(ev; init = 0)
    isempty(rows) || (ev[rows] .= _link_events(frames[rows], coords[rows, :], max_gap, max_dist) .+ off)
    ev
end

# rows of each event id
_event_rows(ev) = (d = Dict{Int,Vector{Int}}(); for (r, e) in enumerate(ev); e > 0 && push!(get!(d, e, Int[]), r); end; d)

"""How many frames each cell's event lasts (distinct frames it is present in); `NaN` for no event."""
function _event_frames(ev::AbstractVector{<:Integer}, frames::AbstractVector)::Vector{Float64}
    out = fill(NaN, length(ev))
    for (_, rows) in _event_rows(ev); out[rows] .= length(unique(frames[rows])); end
    out
end

"""
Each cell's event's `_preceded_fraction`, judged at where the event began; `NaN` for no event.
`first_frame` is the MOVIE's first frame (0), not the population's: debris that first appears at frame
10 still has frames 0–9 before it to look in.
"""
function _event_preceded(ev::AbstractVector{<:Integer}, coords::AbstractMatrix, frames::AbstractVector,
                         ref_by_frame::AbstractDict, back::Integer, dist::Real;
                         first_frame::Real = 0)::Vector{Float64}
    out = fill(NaN, length(ev)); isempty(frames) && return out
    for (_, rows) in _event_rows(ev)
        t0 = minimum(frames[rows]); r0 = rows[frames[rows] .== t0]
        centre = vec(sum(coords[r0, :]; dims = 1) ./ length(r0))
        out[rows] .= _preceded_fraction(t0, centre, ref_by_frame, back, dist, first_frame)
    end
    out
end
