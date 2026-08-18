# Track DIAGNOSTICS — the celltrackR quality-control battery, ported.
#
# `tasks/tracking/track_measures.jl` ports the per-track MEASURES (speed, straightness, …): one number
# per track, for gating and plotting. This file ports the other half of celltrackR — the diagnostics
# that ask whether the tracking result can be trusted at all, and what kind of motion it contains.
# They are curves and verdicts over the WHOLE image, not per-track columns, which is why they are not
# in the measures task.
#
# Sources (read from the package source, not the vignette prose):
#   • celltrackR — Wortel et al. (2021), Cell Reports Methods 1(3):100048.
#     doi:10.1016/j.crmeth.2021.100048.  https://github.com/ingewortel/celltrackR
#     `R/track-measures.R` (squareDisplacement, overallNormDot), `R/angle-functions.R`
#     (angleToPlane, distanceToPlane), vignette `QC.Rmd` (hotellingsTest, autocovariance).
#   • Beltman, Marée & de Boer (2009), Analysing immune cell migration.
#     Nature Reviews Immunology 9:789–798. doi:10.1038/nri2638, PMID 19834485.
#     Source of the 32.7° expected step-angle-to-plane for unbiased 3D motion.
#
# WHY THESE ARE HERE AND NOT ONLY IN A PLOT. The pair diagnostics ported earlier
# (`analyze_cell_pairs`, `find_duplicate_tracks`, `track_pair_drift`) shipped exported and reachable
# from nothing at all — no task, no route, no view. A diagnostic nobody runs is a diagnostic nobody
# has. So everything here is reached two ways from ONE roll-up (`track_diagnostics`): the plot a person
# opens, and the QC findings a tracking run banks whether or not anyone opens anything.
#
# All of it consumes the SAME shape as the rest of this directory — a label-keyed frame with `label`,
# `track_id`, `centroid_t` and the spatial centroid columns ALREADY IN µm (`scale_centroids!`).

using HypothesisTests: OneSampleHotellingT2Test, pvalue

# ── Expected values, and what a deviation means ────────────────────────────────

# Log-log slope of MSD against time lag. Brownian/random walk = 1, ballistic/perfectly directed = 2.
# Sub-1 means confined (a cell rattling inside a niche, or a stationary object being tracked).
const MSD_SLOPE_RANDOM   = 1.0
const MSD_SLOPE_DIRECTED = 2.0
# How far from 1.0 counts as a real departure from a random walk. Deliberately wide: this is a
# description of the motion, not a fault, and the only thing worth a QC line is a slope so low that
# the "tracks" are unlikely to be moving cells at all.
const MSD_SLOPE_CONFINED = 0.5

# Velocity autocorrelation is 1 at lag 0 by construction. The persistence lag is where it decays to
# 1/e — the standard reading of a decorrelation time, and the one number that makes an
# autocorrelation curve comparable between two movies.
const ACOR_PERSIST_LEVEL = exp(-1)

# Mean angle between a step and a reference PLANE for unbiased 3D motion: 32.7° (Beltman 2009 — not a
# number to re-derive or round to 30). Systematically lower angles CLOSE to a boundary plane of the
# imaging volume are the signature of a tracking artefact there.
const PLANE_ANGLE_UNBIASED = 32.7
# A deficit this many degrees below 32.7, among the steps nearest the plane, is worth reporting.
const PLANE_ANGLE_DEFICIT = 6.0
# "Near the plane" = this quantile of the step-to-plane distances.
const PLANE_NEAR_QUANTILE = 0.25

# Hotelling's T² on step displacement vectors tests whether the MEAN step is non-zero — i.e. whether
# the whole field is moving together. `step_spacing` is the number of frames SKIPPED between the steps
# sampled from one track (celltrackR's `hotellingsTest(step.spacing=)`, which passes
# `overlap = -step.spacing` to `subtracks`, giving a stride of `step_spacing + 1`). It matters because
# consecutive steps of a persistent cell are correlated: on the golden fixture below, every step gives
# p = 5.2e-4 and spacing 3 gives p = 0.11 — the same dataset, "drifting" or not depending only on this
# one argument. celltrackR's QC vignette makes exactly that point; 0 means every step, as in R.
const DRIFT_STEP_SPACING = 10
const DRIFT_ALPHA        = 0.05

# The pair half of the battery is O(tracks²) in the min-distance-over-shared-timepoints scan, and it
# DOMINATES. Measured (synthetic, 30 frames/track):
#
#   374 tracks (70k pairs)     1.1 s      curves 0.45 s of it
#   1000 tracks (500k pairs)   7.1 s
#   2000 tracks (2M pairs)    27.9 s      of which the pair scan alone is 25.3 s
#
# `track_measures` banks these findings on every run, so an unguarded scan would quietly add half a
# minute to a big movie's QC. Above this many tracks the pair diagnostics are SKIPPED and the roll-up
# says so (`summary.pairsSkipped`), rather than either stalling the run or reporting "0 duplicates"
# from a scan that never happened. The curves cost 0.45 s at 2000 tracks and always run.
#
# The fix, when it is worth doing, is a spatial grid per timepoint (bucket side = TRACK_DUP_DIST_UM,
# test only the 3×3 neighbourhood) — near-linear for the duplicate half. Recorded in docs/TODO.md.
const PAIR_SCAN_MAX_TRACKS = 800

# ── Mean squared displacement ──────────────────────────────────────────────────

"""
    track_msd(df, spatial; max_lag = 10) -> (lag, msd, sem, n)

Mean squared displacement against time lag, in µm² against FRAMES (the caller multiplies the lag by
the frame interval — this file never guesses a time unit).

Ports celltrackR's `aggregate(tracks, squareDisplacement, subtrack.length = 1:max_lag)`: every
OVERLAPPING pair of positions `lag` frames apart in every track contributes, which is what makes the
estimate usable on the short tracks real movies produce.

**A time gap is not a lag.** celltrackR's subtracks are contiguous by construction; our tables are
not — btrack leaves a track's frames non-consecutive when a detection drops out, and indexing by
position would then average a 1-frame displacement together with a 4-frame one and quietly flatten the
curve. Only pairs whose FRAME difference equals the lag are counted.

Returns empty vectors when nothing is tracked. `sem` is `NaN` where a lag has fewer than two samples.
"""
function track_msd(df::DataFrame, spatial::Vector{String}; max_lag::Int = 10)
    max_lag = max(max_lag, 1)
    sums = zeros(Float64, max_lag)
    sqs  = zeros(Float64, max_lag)
    ns   = zeros(Int, max_lag)

    for (_, p) in _track_paths(df, spatial)
        np = length(p)
        for lag in 1:min(max_lag, np - 1), i in 1:(np - lag)
            isapprox(p[i + lag][1] - p[i][1], Float64(lag); atol = 1e-6) || continue
            d2 = sum((p[i + lag][2] .- p[i][2]) .^ 2)
            sums[lag] += d2
            sqs[lag]  += d2^2
            ns[lag]   += 1
        end
    end

    keep = findall(>(0), ns)
    lag  = Float64.(keep)
    msd  = [sums[k] / ns[k] for k in keep]
    sem  = [_sem_from_moments(sums[k], sqs[k], ns[k]) for k in keep]
    (lag = lag, msd = msd, sem = sem, n = ns[keep])
end

# standard error from running sums, so a big table is never held in memory twice
function _sem_from_moments(s::Float64, s2::Float64, n::Int)::Float64
    n < 2 && return NaN
    v = (s2 - s^2 / n) / (n - 1)
    sqrt(max(v, 0.0) / n)
end

"""
    msd_log_slope(lag, msd) -> Float64

Least-squares slope of `log(msd)` against `log(lag)` — the number the log-log MSD plot exists to
show. `≈1` random walk, `≈2` perfectly directed, `<1` confined (`MSD_SLOPE_*`).

`NaN` with fewer than two usable points. Non-positive values are dropped rather than clamped: a zero
MSD is a real answer (nothing moved) and `log(0)` is not.
"""
function msd_log_slope(lag::AbstractVector{<:Real}, msd::AbstractVector{<:Real})::Float64
    x = Float64[]; y = Float64[]
    for (l, m) in zip(lag, msd)
        (l > 0 && m > 0 && isfinite(l) && isfinite(m)) || continue
        push!(x, log(l)); push!(y, log(m))
    end
    length(x) < 2 && return NaN
    x̄, ȳ = mean(x), mean(y)
    den = sum((x .- x̄) .^ 2)
    den == 0 && return NaN
    sum((x .- x̄) .* (y .- ȳ)) / den
end

"""
    msd_motion_kind(slope) -> String

The log-log MSD slope as the one word it stands for: `"directed"`, `"random walk"`, `"confined"`, or
`"unknown"`. Description, never a fault — a confined slope is the expected answer for cells in a
niche, and only becomes a QC line at `MSD_SLOPE_CONFINED`.
"""
function msd_motion_kind(slope::Real)::String
    isfinite(slope) || return "unknown"
    slope < MSD_SLOPE_CONFINED && return "confined"
    slope >= 1.5 && return "directed"
    "random walk"
end

# ── Velocity autocorrelation ───────────────────────────────────────────────────

"""
    track_autocorrelation(df, spatial; max_lag = 10) -> (lag, acor, sem, n)

Directional persistence: the mean cosine between two steps of the same track `lag` steps apart, for
`lag = 0, 1, …`. Ports celltrackR's `aggregate(tracks, overallNormDot, …)`, which dots the FIRST and
LAST step of each subtrack after normalising both — so this is the NORMALISED autocorrelation, in
`[-1, 1]`, and lag 0 is exactly 1.

How to read it: the decay is the cell's directional memory. Slow decay = persistent migration;
immediate collapse to ~0 = a random walk; a dip to NEGATIVE values means steps systematically reverse,
which for a whole image usually means jitter or a segmentation flickering between two objects rather
than biology.

Only steps between CONSECUTIVE frames are used, and only pairs whose spans are contiguous — the same
gap rule as [`track_msd`], for the same reason.
"""
function track_autocorrelation(df::DataFrame, spatial::Vector{String}; max_lag::Int = 10)
    max_lag = max(max_lag, 0)
    sums = zeros(Float64, max_lag + 1)      # index l+1 holds lag l
    sqs  = zeros(Float64, max_lag + 1)
    ns   = zeros(Int, max_lag + 1)

    for (_, p) in _track_paths(df, spatial)
        # unit step vectors between consecutive FRAMES, with the frame each starts at
        starts = Float64[]; units = Vector{Float64}[]
        for i in 1:(length(p) - 1)
            isapprox(p[i + 1][1] - p[i][1], 1.0; atol = 1e-6) || continue
            v = p[i + 1][2] .- p[i][2]
            nrm = sqrt(sum(v .^ 2))
            nrm > 0 || continue            # a stationary step has no direction to correlate
            push!(starts, p[i][1]); push!(units, v ./ nrm)
        end
        for k in 1:length(units), l in 0:min(max_lag, length(units) - k)
            # contiguous span only: step k and step k+l must be l frames apart
            isapprox(starts[k + l] - starts[k], Float64(l); atol = 1e-6) || continue
            c = clamp(sum(units[k] .* units[k + l]), -1.0, 1.0)
            sums[l + 1] += c
            sqs[l + 1]  += c^2
            ns[l + 1]   += 1
        end
    end

    keep = findall(>(0), ns)
    (lag  = Float64.(keep .- 1),
     acor = [sums[k] / ns[k] for k in keep],
     sem  = [_sem_from_moments(sums[k], sqs[k], ns[k]) for k in keep],
     n    = ns[keep])
end

"""
    persistence_lag(lag, acor) -> Float64

The lag, in frames, at which the autocorrelation first falls to `1/e` — linearly interpolated between
the bracketing lags, so the answer is not quantised to whole frames.

`NaN` when the curve never gets there within `max_lag`, which is itself the finding: the persistence
time is longer than the window measured, so report the window rather than a number.
"""
function persistence_lag(lag::AbstractVector{<:Real}, acor::AbstractVector{<:Real})::Float64
    length(lag) == length(acor) || throw(ArgumentError("lag and acor must be the same length"))
    for i in 2:length(acor)
        a0, a1 = acor[i - 1], acor[i]
        (isfinite(a0) && isfinite(a1)) || continue
        if a0 >= ACOR_PERSIST_LEVEL && a1 < ACOR_PERSIST_LEVEL
            f = (a0 - ACOR_PERSIST_LEVEL) / (a0 - a1)      # a0 > a1 here, so this is in (0, 1]
            return lag[i - 1] + f * (lag[i] - lag[i - 1])
        end
    end
    NaN
end

# ── Step angle to a boundary plane (3D only) ───────────────────────────────────

"""
    plane_angle_profile(df, spatial; plane_z = nothing) -> DataFrame

Per-step angle to a horizontal reference plane against the step's distance from it: columns
`distance` (µm) and `angle` (degrees, 0–90). Ports celltrackR `angleToPlane` / `distanceToPlane`
(3D only — an empty frame for 2D data, not an error).

`plane_z` defaults to the LOWER z bound of the data, i.e. the bottom of the imaging volume, which is
where the artefact this looks for lives.

The plot to make is angle against distance, with a line at `PLANE_ANGLE_UNBIASED` (32.7°, Beltman
2009). Unbiased 3D motion sits on that line at every distance. Angles that sag below it only for the
steps NEAREST the plane mean the tracking is being pulled along the boundary — cells clipped by the
volume edge, or a reflection being tracked.
"""
function plane_angle_profile(df::DataFrame, spatial::Vector{String};
                             plane_z::Union{Nothing,Real} = nothing)::DataFrame
    length(spatial) >= 3 || return DataFrame(distance = Float64[], angle = Float64[])
    paths = _track_paths(df, spatial)
    isempty(paths) && return DataFrame(distance = Float64[], angle = Float64[])

    z0 = plane_z === nothing ?
         minimum(c[3] for (_, p) in paths for (_, c) in p) : Float64(plane_z)

    dist, ang = Float64[], Float64[]
    # sorted track order — these rows reach a CSV export, and "whatever the Dict yielded" is not a
    # row order anyone can diff between two runs
    for tid in sort!(collect(keys(paths))), i in 1:(length(paths[tid]) - 1)
        p = paths[tid]
        isapprox(p[i + 1][1] - p[i][1], 1.0; atol = 1e-6) || continue
        v = p[i + 1][2] .- p[i][2]
        nrm = sqrt(sum(v .^ 2))
        nrm > 0 || continue
        # angle to the plane = 90° − angle to its normal; the normal here is ẑ, so the numerator is
        # simply the step's z component
        a = asind(clamp(abs(v[3]) / nrm, 0.0, 1.0))
        push!(dist, abs(p[i][2][3] - z0))
        push!(ang, a)
    end
    DataFrame(distance = dist, angle = ang)
end

"""
    plane_artefact(profile; near_quantile, expected) -> NamedTuple

Verdict over [`plane_angle_profile`] output: `(mean_angle_near, mean_angle_far, n_near, suspect)`.

`suspect` is true only when the steps NEAREST the plane sit more than `PLANE_ANGLE_DEFICIT` below the
unbiased 32.7° **and** below the far steps. Both halves matter: a whole-image deviation is a
directional bias (chemotaxis, flow), while a deficit that appears only at the boundary is the
imaging artefact this is looking for.
"""
function plane_artefact(profile::DataFrame; near_quantile::Real = PLANE_NEAR_QUANTILE,
                        expected::Real = PLANE_ANGLE_UNBIASED,
                        deficit::Real = PLANE_ANGLE_DEFICIT)
    nrow(profile) == 0 && return (mean_angle_near = NaN, mean_angle_far = NaN,
                                  n_near = 0, suspect = false)
    cut  = quantile(profile.distance, clamp(Float64(near_quantile), 0.0, 1.0))
    near = profile.angle[profile.distance .<= cut]
    far  = profile.angle[profile.distance .>  cut]
    isempty(near) && return (mean_angle_near = NaN, mean_angle_far = NaN,
                             n_near = 0, suspect = false)
    mn = mean(near)
    mf = isempty(far) ? NaN : mean(far)
    (mean_angle_near = mn, mean_angle_far = mf, n_near = length(near),
     suspect = mn < expected - deficit && (isnan(mf) || mn < mf))
end

# ── Whole-field drift (Hotelling's T²) ────────────────────────────────────────

"""
    drift_test(df, spatial; step_spacing = DRIFT_STEP_SPACING) -> NamedTuple

Is the whole field moving together? Hotelling's one-sample T² on step displacement vectors against a
zero mean (celltrackR `hotellingsTest`), returning
`(p, statistic, n, mean_step, drifting, step_spacing)`.

`step_spacing` is the reason this test is trustworthy: consecutive steps of a persistent cell are
correlated, so using every step makes the test significant for essentially every real dataset. It
counts the frames SKIPPED between sampled steps, so the stride is `step_spacing + 1` — celltrackR's
own definition (`overlap = -step.spacing`), and `0` means every step there and here.

One deliberate difference: celltrackR strides by row INDEX, this strides by FRAME NUMBER. They agree
on contiguous tracks and disagree where a detection dropped out, and the frame is the honest reading —
an index stride over a gappy track samples steps that are further apart in time than asked.

`spatial` chooses the dimensions tested; the roll-up passes **x and y only**, matching celltrackR's
`dim = c("x","y")` default. Stage drift is an xy phenomenon; adding z would fold focus drift into the
same verdict.

A significant result is NOT automatically an artefact: real chemotaxis moves a whole field too. It
says the field has a net direction; deciding between stage drift and biology is the user's call, and
the repair for the former is `cleanupImages.driftCorrect`, not a track edit.

`p` is `NaN` when there are too few decorrelated samples for the test (it needs more samples than
dimensions), which is the common case on short movies and must read as "not assessed".
"""
function drift_test(df::DataFrame, spatial::Vector{String}; step_spacing::Int = DRIFT_STEP_SPACING)
    dims = length(spatial)
    steps = Vector{Float64}[]
    for (_, p) in _track_paths(df, spatial)
        last_t = -Inf
        for i in 1:(length(p) - 1)
            isapprox(p[i + 1][1] - p[i][1], 1.0; atol = 1e-6) || continue
            p[i][1] - last_t >= step_spacing + 1 || continue
            push!(steps, p[i + 1][2] .- p[i][2])
            last_t = p[i][1]
        end
    end
    n = length(steps)
    mean_step = n == 0 ? fill(NaN, dims) : [mean(s[d] for s in steps) for d in 1:dims]
    # T² needs more samples than dimensions to have a covariance to invert at all
    n > dims + 1 || return (p = NaN, statistic = NaN, n = n, mean_step = mean_step,
                            drifting = false, step_spacing = step_spacing)
    m = Matrix{Float64}(undef, n, dims)
    for (i, s) in enumerate(steps), d in 1:dims
        m[i, d] = s[d]
    end
    try
        t = OneSampleHotellingT2Test(m)          # HypothesisTests — not a hand-rolled T²
        pv = pvalue(t)
        (p = pv, statistic = t.T², n = n, mean_step = mean_step,
         drifting = pv < DRIFT_ALPHA, step_spacing = step_spacing)
    catch e
        # a singular covariance (every sampled step identical, or a dimension that never varies —
        # a 3-D table whose z never moves) is "cannot assess", not a failure
        @debug "Hotelling T² unavailable" exception = e
        (p = NaN, statistic = NaN, n = n, mean_step = mean_step,
         drifting = false, step_spacing = step_spacing)
    end
end

# ── The one roll-up both the plot and the QC read ──────────────────────────────

"""
    track_diagnostics(df, spatial; max_lag, step_spacing) -> NamedTuple

Every diagnostic in this file, computed once: `(msd, acor, plane, drift, pairs, duplicates, summary)`.

ONE entry point on purpose. The plot a person opens and the QC a tracking run banks read the same
object, so the panel and the finding can never disagree about whether an image drifts — and the
diagnostics cannot go back to being exported functions with no caller.

`summary` carries the readable scalars: `msdSlope`, `motionKind`, `persistenceLag`, `driftP`,
`planeAngleNear`, `nDuplicatePairs`, `nTracks`.
"""
function track_diagnostics(df::DataFrame, spatial::Vector{String};
                           max_lag::Int = 10, step_spacing::Int = DRIFT_STEP_SPACING,
                           max_pair_tracks::Int = PAIR_SCAN_MAX_TRACKS)
    msd  = track_msd(df, spatial; max_lag = max_lag)
    acor = track_autocorrelation(df, spatial; max_lag = max_lag)
    prof = plane_angle_profile(df, spatial)
    pl   = plane_artefact(prof)
    # xy only, as celltrackR's `hotellingsTest(dim = c("x","y"))` default — see `drift_test`
    dr   = drift_test(df, first(spatial, min(2, length(spatial))); step_spacing = step_spacing)

    # the pair half, guarded — see PAIR_SCAN_MAX_TRACKS for the measurements
    n_tracks = length(track_ids_present(df))
    skip_pairs = n_tracks > max_pair_tracks
    prs = skip_pairs ? DataFrame(track1 = Int[], track2 = Int[], angle = Float64[],
                                 distance = Float64[], n_shared = Int[]) :
                       analyze_cell_pairs(df, spatial)
    # `pairs = prs` so the scan runs ONCE for both the duplicate finder and the drift verdict —
    # at 374 tracks that is ~70k pairs, and doing it twice is a visible cost for nothing
    dup  = skip_pairs ? TrackIssue[] : find_duplicate_tracks(df, spatial; pairs = prs)
    pdr  = track_pair_drift(prs)

    slope = msd_log_slope(msd.lag, msd.msd)
    (msd = msd, acor = acor, plane = (profile = prof, verdict = pl),
     drift = (test = dr, pairs = pdr), pairs = prs, duplicates = dup,
     summary = (msdSlope = slope, motionKind = msd_motion_kind(slope),
                persistenceLag = persistence_lag(acor.lag, acor.acor),
                driftP = dr.p, driftMeanStep = dr.mean_step,
                pairAngleFar = pdr.mean_angle_far, pairsSkipped = skip_pairs,
                planeAngleNear = pl.mean_angle_near, planeAngleFar = pl.mean_angle_far,
                nDuplicatePairs = length(dup), nTracks = n_tracks))
end

"""
    track_diagnostic_findings(diag) -> Vector{Dict{String,Any}}

The diagnostics as QC findings (`write_qc` shape: `kind`, `short`, `long`). Advisory only — every one
of these can be the correct biology, so none is ever an `error`.

This is what makes the battery routine: a tracking run banks these whether or not anyone opens the
plot, which is the difference between a diagnostic that exists and one that gets used.
"""
function track_diagnostic_findings(diag)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    s = diag.summary

    # Text lives in `QC_TEXT` (app/src/qc.jl), not here — same rule as every other findings function,
    # so the wording can be reviewed as a set and re-rendered at read time.
    diag.drift.test.drifting &&
        push!(out, qc_finding("warn", "tracking.field_drift";
                              value = string(round(diag.drift.test.p; sigdigits = 2)),
                              detail = "mean step $(round.(diag.drift.test.mean_step; digits = 3)) µm " *
                                       "over $(diag.drift.test.n) steps " *
                                       "$(diag.drift.test.step_spacing) frames apart"))

    (isfinite(s.msdSlope) && s.msdSlope < MSD_SLOPE_CONFINED) &&
        push!(out, qc_finding("warn", "tracking.msd_confined";
                              value = string(round(s.msdSlope; digits = 2)),
                              detail = "motion reads as $(s.motionKind)"))

    diag.plane.verdict.suspect &&
        push!(out, qc_finding("warn", "tracking.plane_artefact";
                              value = string(round(s.planeAngleNear; digits = 1)),
                              detail = "$(diag.plane.verdict.n_near) steps nearest the plane; " *
                                       "farther steps average $(round(s.planeAngleFar; digits = 1))°"))

    # NB `nDuplicatePairs == 0` means either "none found" or "not looked for" (`pairsSkipped`) — the
    # absence of this finding is therefore not evidence of absence on a very large image
    s.nDuplicatePairs > 0 &&
        push!(out, qc_finding("warn", "tracking.duplicate_tracks"; count = string(s.nDuplicatePairs)))
    out
end

"""
    track_diagnostics_for(props_path, pixel_res; kwargs...) -> Union{Nothing,NamedTuple}

[`track_diagnostics`] straight from a `labelProps` file: reads the columns, scales the centroids to µm
through the ONE shared conversion, and returns `nothing` when the segmentation has no tracks or no
time axis.

The loader exists so the two callers cannot load differently — the tracking task (which banks the
findings as QC) and `GET /api/tracking/diagnostics` (which draws them). Mirrors
[`track_issues_for`].
"""
function track_diagnostics_for(props_path::AbstractString, pixel_res::AbstractVector{<:Real};
                               kwargs...)
    lp = label_props(props_path)
    ("track_id" in col_names(lp; data_type = :obs)) || return nothing
    spatial  = centroid_columns(lp; order = [:x, :y, :z])
    temporal = temporal_columns(lp)
    (isempty(spatial) || isempty(temporal)) && return nothing
    select_cols(lp, vcat(spatial, temporal, ["track_id"]))
    df = as_df(lp; include_x = false, include_obs = true)
    scale_centroids!(df, pixel_res)          # µm, via the ONE shared conversion
    t_col = first(temporal)
    t_col == "centroid_t" || (df[!, :centroid_t] = df[!, Symbol(t_col)])
    track_diagnostics(df, spatial; kwargs...)
end
