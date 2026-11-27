# track_metrics.jl — the `Track` struct and the per-track measures (celltrackR ports).
# Julia port of track measurement functions from the celltrackR R package
# Original authors: Inge Wortel, Johannes Textor (MotilityLab / Radboud University)
# Source: https://github.com/ingewortel/celltrackR
# Reference: Wortel et al. (2021) ImmunoInformatics 1–2:100003, doi:10.1016/j.immuno.2021.100003
# Port is not one-to-one — see inline notes where behaviour was corrected or extended.
#
# Pure geometry over one track, below the tasks tier: the `tracking.track_measures` task
# (tasks/tracking/track_measures.jl) and the tracklet windows (tracking/track_windows.jl) both read it.

using LinearAlgebra: dot, eigvals
using Statistics: mean, cov

# ── Track data structure ───────────────────────────────────────────────────────

struct Track
    id::Int
    t::Vector{Float64}       # physical time (frame_index × time_step)
    coords::Matrix{Float64}  # n_steps × n_dims; physical space (px × pixel_res)
end

n_steps(tr::Track) = size(tr.coords, 1)

# ── Low-level measure functions (celltrackR ports) ─────────────────────────────

"""Step distances between consecutive positions (shared building block)."""
function _step_dists(coords::Matrix{Float64})::Vector{Float64}
    [sqrt(sum((coords[i, :] .- coords[i-1, :]).^2)) for i in 2:size(coords, 1)]
end

"""Total path length (celltrackR `trackLength`)."""
function track_length(tr::Track)::Float64
    n = n_steps(tr)
    n == 0 && return NaN
    n == 1 && return 0.0
    sum(_step_dists(tr.coords))
end

"""Last timepoint minus first (celltrackR `duration`)."""
function track_duration(tr::Track)::Float64
    n_steps(tr) < 2 && return 0.0
    tr.t[end] - tr.t[1]
end

"""Mean speed: trackLength / duration (celltrackR `speed`). NaN for single-step tracks."""
function track_speed(tr::Track)::Float64
    d = track_duration(tr)
    d == 0.0 && return NaN
    track_length(tr) / d
end

"""Euclidean distance from first to last position (celltrackR `displacement`)."""
function track_displacement(tr::Track)::Float64
    n_steps(tr) < 2 && return 0.0
    sqrt(sum((tr.coords[end, :] .- tr.coords[1, :]).^2))
end

"""Maximum distance from start to any position (celltrackR `maxDisplacement`)."""
function max_displacement(tr::Track)::Float64
    n = n_steps(tr)
    n < 2 && return 0.0
    maximum(sqrt(sum((tr.coords[i, :] .- tr.coords[1, :]).^2)) for i in 1:n)
end

"""Displacement / trackLength (celltrackR `straightness`). Returns 1.0 when trackLength == 0."""
function track_straightness(tr::Track)::Float64
    l = track_length(tr)
    l == 0.0 && return 1.0
    track_displacement(tr) / l
end

"""Displacement / maxDisplacement (celltrackR `displacementRatio`). NaN when maxDisplacement == 0."""
function track_displacement_ratio(tr::Track)::Float64
    md = max_displacement(tr)
    md == 0.0 && return NaN
    track_displacement(tr) / md
end

"""maxDisplacement / trackLength (celltrackR `outreachRatio`). NaN when trackLength == 0."""
function track_outreach_ratio(tr::Track)::Float64
    l = track_length(tr)
    l == 0.0 && return NaN
    max_displacement(tr) / l
end

"""
Angle between vectors a and b, in radians.
Clamps the dot product to [-1, 1] to guard floating-point rounding (celltrackR `vecAngle`).
Returns NaN for zero vectors.
"""
function vec_angle(a::AbstractVector, b::AbstractVector)::Float64
    na = sqrt(sum(a .^ 2))
    nb = sqrt(sum(b .^ 2))
    (na == 0.0 || nb == 0.0) && return NaN
    acos(clamp(dot(a ./ na, b ./ nb), -1.0, 1.0))
end

"""
Overall direction change: angle between first and last step vectors, in degrees
(celltrackR `overallAngle` with from=1, to=n_steps, degrees=TRUE).
NaN for < 3 steps.
"""
function track_overall_angle(tr::Track)::Float64
    n_steps(tr) < 3 && return NaN
    diffs = diff(tr.coords; dims=1)
    rad2deg(vec_angle(diffs[1, :], diffs[end, :]))
end

"""
Mean turning angle across all consecutive step pairs, in degrees
(celltrackR `meanTurningAngle`). NaN for < 3 steps.
"""
function track_mean_turning_angle(tr::Track)::Float64
    n_steps(tr) < 3 && return NaN
    diffs  = diff(tr.coords; dims=1)
    angles = [vec_angle(diffs[i, :], diffs[i+1, :]) for i in 1:size(diffs, 1)-1]
    valid  = filter(!isnan, angles)
    isempty(valid) && return NaN
    rad2deg(mean(valid))
end

"""
Asphericity: eigenvalue spread of position covariance matrix (celltrackR `asphericity`).
Returns 1.0 for < 3 steps (celltrackR convention), NaN for 1-D tracks or zero variance.
"""
function track_asphericity(tr::Track)::Float64
    n, d = size(tr.coords)
    d == 1  && return NaN
    n < 3   && return 1.0
    ev  = eigvals(cov(tr.coords))
    rav = mean(ev)
    rav == 0.0 && return NaN
    sum((v - rav)^2 for v in ev) / (d * (d - 1) * rav^2)
end
