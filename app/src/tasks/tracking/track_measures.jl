# track_measures.jl
# Julia port of track measurement functions from the celltrackR R package
# Original authors: Inge Wortel, Johannes Textor (MotilityLab / Radboud University)
# Source: https://github.com/ingewortel/celltrackR
# Reference: Wortel et al. (2021) Cell Reports Methods, doi:10.1016/j.crmeth.2021.100006
# This port covers the measurement subset used by Cecelia. Simulation functions
# (random walk models, bootstrapping) are not included.
# Port is not one-to-one — see inline notes where behaviour was corrected or extended.

using LinearAlgebra: dot, eigvals, norm
using Statistics: mean, cov, cor, std
using DataFrames: DataFrame

struct TrackMeasures <: CciaTask end

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

# ── Per-cell (subtrack) measures ───────────────────────────────────────────────

"""
Per-step speed for every cell in the track (physical units: µm/s or px/frame).
Index i corresponds to cell_id=i (1-based):
  cell_id 1  → NaN (no prior position)
  cell_id i  → dist(pos[i-1], pos[i]) / Δt

Matches celltrackR subtracks(i=1) speed with increment.cell.id=TRUE, which assigns
each step speed to the endpoint cell.
"""
function step_speeds(tr::Track)::Vector{Float64}
    n   = n_steps(tr)
    out = fill(NaN, n)
    for i in 2:n
        dt = tr.t[i] - tr.t[i-1]
        dt == 0.0 && continue
        out[i] = sqrt(sum((tr.coords[i, :] .- tr.coords[i-1, :]).^2)) / dt
    end
    out
end

"""
Per-cell turning angle in degrees (physical coords).
Index i corresponds to cell_id=i (1-based):
  cell_id 1, 2 → NaN
  cell_id i≥3  → angle between step (i-2→i-1) and step (i-1→i)

Matches celltrackR subtracks(i=2) overallAngle with increment.cell.id=TRUE and degrees=TRUE.
The angle at position i-1 is assigned to cell_id i (to align with the step ending there).
"""
function step_turning_angles(tr::Track)::Vector{Float64}
    n    = n_steps(tr)
    out  = fill(NaN, n)
    diffs = diff(tr.coords; dims=1)
    for i in 3:n
        out[i] = rad2deg(vec_angle(diffs[i-2, :], diffs[i-1, :]))
    end
    out
end

"""
Load all tracked cells via the `LabelProps` reader (docs/DATAMODEL.md — the standard
data-access idiom; no raw HDF5 here). Returns `Vector{Tuple{Track, cell_labels::Vector{Int}}}`;
`cell_labels[i]` is the obs integer label for track step i (sorted by time).
Physical-unit conversion is applied: `coords *= pixel_res`, `t *= time_step`.
"""
function _load_tracks_with_labels(props_path::String,
                                   pixel_res::Vector{Float64},
                                   time_step::Float64
                                   )::Vector{Tuple{Track, Vector{Int}}}
    lp = label_props(props_path)

    ("track_id" in col_names(lp; data_type=:obs)) ||
        error("TrackMeasures: no track_id column — run btrack first")

    spatial_cols  = centroid_columns(lp; order=[:x, :y, :z])   # explicit axes, present only, x,y,z order
    temporal_cols = temporal_columns(lp)
    isempty(temporal_cols) &&
        error("TrackMeasures: no temporal column — tracking needs a timecourse")
    t_col = first(temporal_cols)

    select_cols(lp, vcat(spatial_cols, temporal_cols, ["track_id"]))
    df = as_df(lp; include_x=false, include_obs=true)

    # each centroid column scaled by ITS OWN axis resolution (by name, never by position) — 2D-safe
    res = [physical_size_for_axis(pixel_res, axis_of(c)) for c in spatial_cols]

    _to_int(x) = x isa Integer ? Int(x) : Int(round(Float64(x)))

    groups = Dict{Int, Vector{Tuple{Float64, Vector{Float64}, Int}}}()
    for row in eachrow(df)
        tid = row.track_id
        (tid isa Number && !isnan(tid)) || continue          # untracked cells: NaN/missing
        t_phys = Float64(row[t_col]) * time_step
        coords = [Float64(row[spatial_cols[k]]) * res[k] for k in eachindex(spatial_cols)]
        push!(get!(groups, Int(tid), []), (t_phys, coords, _to_int(row.label)))
    end

    map(collect(groups)) do (tid, rows)
        sorted    = sort(rows; by=x -> x[1])
        t_vec     = [r[1] for r in sorted]
        coord_mat = Matrix{Float64}(reduce(hcat, [r[2] for r in sorted])')
        lbls      = [r[3] for r in sorted]
        (Track(tid, t_vec, coord_mat), lbls)
    end
end

# ── Per-track aggregate column names → measure function ───────────────────────────
# These are per-TRACK measures (ONE value per track). They are written to the companion
# per-track h5ad ({value_name}__tracks.h5ad) as X/var, so they are gateable one-point-per-track
# (docs/TRACKING.md). Per-CELL measures (live.cell.*) stay in the cell labelProps obs. We do NOT
# broadcast track measures across the cells of a track — that was the old, redundant design.
const _TRACK_AGG_COLS = [
    "live.track.speed"             => track_speed,
    "live.track.duration"          => track_duration,
    "live.track.trackLength"       => track_length,
    "live.track.displacement"      => track_displacement,
    "live.track.straightness"      => track_straightness,
    "live.track.displacementRatio" => track_displacement_ratio,
    "live.track.outreachRatio"     => track_outreach_ratio,
    "live.track.meanTurningAngle"  => track_mean_turning_angle,
    "live.track.overallAngle"      => track_overall_angle,
    "live.track.asphericity"       => track_asphericity,
]

# obs lineage columns (per-track identity, constant within a track) copied into the track table obs.
const _TRACK_LINEAGE_COLS = ["track_parent", "track_root", "track_state", "track_generation"]

"""True when both measure outputs already exist: per-cell `live.cell.speed` in the cell obs AND
the companion per-track table file. Either missing ⇒ (re)compute."""
function _track_measures_cached(props_path::String, track_path::String)::Bool
    isfile(track_path) || return false
    "live.cell.speed" in col_names(label_props(props_path); data_type=:obs)
end

# JSON has no NaN literal — map NaN floats to `nothing` (→ JSON null → Python None → np.nan).
_jsonsafe(x::Real) = (x isa AbstractFloat && isnan(x)) ? nothing : x
_jsonsafe(x) = x

"""
Per-track lineage values aligned to `track_ids`, read from the cell obs (one value per track —
lineage is constant within a track, so the first cell of each track wins). Missing columns /
tracks ⇒ `nothing` (→ NaN in the track table).
"""
function _track_lineage(props_path::String, track_ids::Vector{Int})::Dict{String,Vector{Any}}
    have = intersect(_TRACK_LINEAGE_COLS, col_names(label_props(props_path); data_type=:obs))
    out  = Dict{String,Vector{Any}}()
    isempty(have) && return out
    df = label_props(props_path) |> select_cols(vcat("track_id", have)) |> as_df
    first_row = Dict{Int,Int}()
    for (i, t) in enumerate(df.track_id)
        (t isa Number && !isnan(t)) || continue
        ti = Int(t)
        haskey(first_row, ti) || (first_row[ti] = i)
    end
    for c in have
        out[c] = Any[(i = get(first_row, t, 0); i == 0 ? nothing : _jsonsafe(df[i, c]))
                     for t in track_ids]
    end
    out
end

"""
Create the companion per-track h5ad via Python anndata (new-file creation is Python's job,
docs/DATAMODEL.md). Julia computes the measures (the celltrackR port above) and hands Python a
per-track table: `trackIds` (obs index), `measureNames` (var), the `X` matrix, and obs `lineage`.
Returns true on a clean subprocess exit.
"""
function _write_track_props(track_path::String, track_ids::Vector{Int},
                            measure_names::Vector{String}, X::Vector{Vector{Any}},
                            lineage::Dict{String,Vector{Any}};
                            on_log::Function, on_process::Function)::Bool
    # track_path = {img._dir}/labelProps/{vn}__tracks.h5ad → up two dirs reaches img._dir
    run_py("tasks/tracking/track_props_run.py",
        (; outPath = track_path, trackIds = track_ids,
           measureNames = measure_names, X = X, lineage = lineage),
        task_run_dir(dirname(dirname(track_path)));
        on_log = on_log, on_process = on_process)
end

# ── Motion dimensionality detection (in-plane 2D vs full 3D) ────────────────────────
# Every per-cell and live.track.* measure derives from the SAME step vectors, so 2D-vs-3D is ONE
# decision. A coarse/anisotropic z (thin stacks, few slices) often carries only segmentation jitter
# rather than real migration — and feeding it corrupts turning angle + speed (a fast cell appears to
# reverse; see the diagnostics that motivated this). We detect whether z carries genuine directional
# MOTION: a migratory axis has a positive lag-1 velocity autocorrelation and preserves directional
# persistence (mean cos of the turning angle); a jitter axis is anti-persistent (autocorr ≤ 0) and
# flattens 3D persistence toward random (mean angle → 90°). The resolved choice feeds step_speeds,
# step_turning_angles AND every _TRACK_AGG_COLS aggregate.

struct MotionDims
    dims::Int            # 2 or 3
    z_used::Bool
    confidence::String   # "high" | "low"
    reason::String
    metrics::Dict{String,Float64}
end

# in-plane (drop the leading skimage z axis; coords are [z,y,x] when 3D, [y,x] when 2D)
_collapse_to_2d(tr::Track)::Track =
    size(tr.coords, 2) <= 2 ? tr : Track(tr.id, tr.t, tr.coords[:, end-1:end])

_lag1(prev::Vector{Float64}, next::Vector{Float64})::Float64 =
    length(prev) < 3 ? NaN : cor(prev, next)

# Pure detector over loaded tracks (physical coords). Thresholds chosen to require corroboration:
# z is kept (3D) only if it is clearly migratory; collapsed (2D) only if it is clearly jitter;
# ambiguous / thin data → keep 3D at low confidence (never silently drop a dimension).
function _detect_motion_dims(tracks)::MotionDims
    isempty(tracks) && return MotionDims(3, true, "low", "no tracks — defaulting to 3D", Dict{String,Float64}())
    d = size(first(tracks).coords, 2)
    d < 3 && return MotionDims(2, false, "high", "no z axis (2D segmentation)",
                               Dict("spatialDims" => Float64(d)))
    zp=Float64[]; zn=Float64[]; yp=Float64[]; yn=Float64[]; xp=Float64[]; xn=Float64[]
    c3=Float64[]; c2=Float64[]; zvals=Float64[]
    for tr in tracks
        P = tr.coords; n = size(P, 1); n < 4 && continue
        append!(zvals, P[:, 1])
        D = [P[k+1, :] .- P[k, :] for k in 1:n-1]
        for k in 1:length(D)-1
            a = D[k]; b = D[k+1]
            push!(zp, a[1]); push!(zn, b[1]); push!(yp, a[2]); push!(yn, b[2]); push!(xp, a[3]); push!(xn, b[3])
            na = norm(a); nb = norm(b); (na > 0 && nb > 0) && push!(c3, dot(a, b) / (na * nb))
            a2 = a[2:3]; b2 = b[2:3]; na2 = norm(a2); nb2 = norm(b2)
            (na2 > 0 && nb2 > 0) && push!(c2, dot(a2, b2) / (na2 * nb2))
        end
    end
    n_steps = length(zp)
    (n_steps < 1 || std(zvals) < 1e-9) &&
        return MotionDims(2, false, "high", "z is a single plane (no z variation)",
                          Dict("nSteps" => Float64(n_steps)))
    a_z = _lag1(zp, zn); a_y = _lag1(yp, yn); a_x = _lag1(xp, xn)
    a_xy = mean(filter(!isnan, [a_x, a_y]))
    p3 = isempty(c3) ? NaN : mean(c3); p2 = isempty(c2) ? NaN : mean(c2)
    metrics = Dict("nSteps"=>Float64(n_steps), "autocorrX"=>a_x, "autocorrY"=>a_y, "autocorrZ"=>a_z,
                   "persist3D"=>p3, "persist2D"=>p2)
    n_steps < 50 &&
        return MotionDims(3, true, "low", "too few steps ($n_steps) to assess z — kept 3D; review", metrics)
    if a_z > 0.1 && a_z >= 0.5 * a_xy && !isnan(p3) && !isnan(p2) && p3 >= 0.6 * p2
        return MotionDims(3, true, "high",
            "z shows directional persistence (Δz autocorr $(round(a_z,digits=2)) ≈ in-plane) — full 3D", metrics)
    end
    if a_z <= 0.1 && !isnan(p3) && !isnan(p2) && p3 <= 0.5 * p2
        return MotionDims(2, false, "high",
            "z is non-migratory jitter (Δz autocorr $(round(a_z,digits=2)) ≤ 0; in-plane persistence " *
            "$(round(p2,digits=2)) collapses to $(round(p3,digits=2)) in 3D) — recommend in-plane (2D)", metrics)
    end
    MotionDims(3, true, "low",
        "z signal ambiguous — kept 3D; review (Δz autocorr $(round(a_z,digits=2)))", metrics)
end

# Cached by the h5ad's mtime (re-tracking auto-invalidates) — so the run-form preflight and the task
# share one cheap computation per file version. Reads only centroid + track_id (one pass).
const _MOTION_DIMS_CACHE = Dict{String, MotionDims}()
# Written from both the GET /api/tracking/motion-dims preflight AND the track_measures task, so
# concurrent under `-t auto`. Serialise the check-and-fill to avoid an unlocked-Dict rehash race.
const _MOTION_DIMS_CACHE_LOCK = ReentrantLock()
function detect_motion_dims(props_path::AbstractString, pixel_res, time_step; flush::Bool=false)::MotionDims
    key = "$(props_path)|$(isfile(props_path) ? mtime(props_path) : 0.0)"
    lock(_MOTION_DIMS_CACHE_LOCK) do
        (!flush && haskey(_MOTION_DIMS_CACHE, key)) && return _MOTION_DIMS_CACHE[key]
        tracks = [tr for (tr, _) in _load_tracks_with_labels(props_path, pixel_res, time_step)]
        _MOTION_DIMS_CACHE[key] = _detect_motion_dims(tracks)
    end
end

# ── Main task entry point ──────────────────────────────────────────────────────

function _run_task(task::TrackMeasures, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    props_path = img_label_props_path(img, value_name)
    track_path = img_track_props_path(img, value_name)

    isfile(props_path) || begin
        on_log("[ERROR] No labelProps for valueName='$value_name': $props_path")
        return nothing
    end

    force = Bool(get(params, "forceRecompute", false))
    if !force && _track_measures_cached(props_path, track_path)
        on_log("[INFO] Track measures already cached — skipping (set forceRecompute=true to override)")
        return Dict{String,Any}("valueName" => value_name, "cached" => true)
    end

    on_log("[INFO] Reading physical pixel sizes...")
    pixel_res, time_step = img_physical_sizes(img)
    on_log("[INFO] pixel_res=$(round.(pixel_res; digits=4)) µm/px, time_step=$(time_step) min/frame")
    on_progress(1, 5)

    on_log("[INFO] Loading tracks from $props_path")
    tracks_with_labels = _load_tracks_with_labels(props_path, pixel_res, time_step)
    if isempty(tracks_with_labels)
        on_log("[ERROR] No tracks found — run btrack first")
        return nothing
    end
    n_tracks = length(tracks_with_labels)
    on_log("[INFO] Computing measures for $n_tracks tracks")
    on_progress(2, 5)

    # ── resolve motion dimensionality (2D in-plane vs 3D) — governs ALL measures ──
    # `dims` param: "auto" (detect), "2D", or "3D". The run-form preflight shows the auto
    # recommendation; the user may override (e.g. force 3D despite a 2D recommendation).
    dims_param = lowercase(strip(string(get(params, "dims", "auto"))))
    det = detect_motion_dims(props_path, pixel_res, time_step)   # cached by mtime
    resolved = dims_param == "2d" ? 2 : dims_param == "3d" ? 3 : det.dims
    if dims_param == "auto"
        tag = (resolved == 2 || det.confidence == "low") ? "[WARN]" : "[INFO]"
        on_log("$tag motion dims: auto → $(resolved)D — $(det.reason)")
    else
        on_log("[INFO] motion dims: $(resolved)D (user-set; auto recommends $(det.dims)D — $(det.reason))")
    end
    if resolved == 2 && size(first(tracks_with_labels)[1].coords, 2) > 2
        on_log("[INFO] Collapsing tracks to the in-plane (xy) axes for all measures")
        tracks_with_labels = [(_collapse_to_2d(tr), lbls) for (tr, lbls) in tracks_with_labels]
    end

    # per-cell (live.cell.*) → keyed by label, written to the CELL obs.
    # per-track (live.track.*) → ONE value per track, written to the track table X/var.
    agg_names  = String[first(p) for p in _TRACK_AGG_COLS]
    rows_label = Int[]
    rows_speed = Float64[]
    rows_angle = Float64[]
    track_ids  = Int[]
    X          = Vector{Vector{Any}}()        # n_tracks × n_measures (JSON-safe; NaN→nothing)

    for (tr, cell_labels) in tracks_with_labels
        speeds = step_speeds(tr)
        angles = step_turning_angles(tr)
        for (i, lbl) in enumerate(cell_labels)
            push!(rows_label, lbl)
            push!(rows_speed, speeds[i])
            push!(rows_angle, angles[i])
        end
        push!(track_ids, tr.id)
        push!(X, Any[_jsonsafe(fn(tr)) for (_, fn) in _TRACK_AGG_COLS])
    end
    on_progress(3, 5)

    # ── per-cell measures → cell obs; drop any stale broadcast live.track.* (old design) ──
    on_log("[INFO] Writing per-cell measures to cell obs...")
    cell_df = DataFrame("label"           => rows_label,
                        "live.cell.speed" => rows_speed,
                        "live.cell.angle" => rows_angle)
    stale = filter(c -> startswith(c, "live.track."),
                   col_names(label_props(props_path); data_type=:obs))
    isempty(stale) ||
        on_log("[INFO] Removing $(length(stale)) stale broadcast live.track.* columns from cell obs")
    label_props(props_path) |> drop_obs(stale) |> add_obs(cell_df) |> save!
    on_progress(4, 5)

    # ── per-track table (measures in X/var, lineage in obs) → {vn}__tracks.h5ad via Python ──
    on_log("[INFO] Writing per-track table → $track_path")
    lineage = _track_lineage(props_path, track_ids)
    ok = _write_track_props(track_path, track_ids, agg_names, X, lineage;
                            on_log = on_log, on_process = on_process)
    ok || begin
        on_log("[ERROR] Failed to write per-track table")
        return nothing
    end
    on_progress(5, 5)

    # QC (advisory): bank per-track counts/means + the motion-dims finding. Means over the per-track
    # X (skipping NaN→nothing); omitted when non-finite so a degenerate image is excluded from the
    # cohort rather than polluting it. Never fails the task.
    try
        speed_idx = findfirst(==("live.track.speed"), agg_names)
        disp_idx  = findfirst(==("live.track.displacement"), agg_names)
        _meas_mean(idx) = isnothing(idx) ? NaN : begin
            vs = Float64[Float64(row[idx]) for row in X
                         if row[idx] !== nothing && !(row[idx] isa Real && isnan(row[idx]))]
            isempty(vs) ? NaN : mean(vs)
        end
        ms = _meas_mean(speed_idx); md = _meas_mean(disp_idx)
        metrics = Dict{String,Any}("nTracks" => n_tracks, "motionDims" => resolved)
        isnan(ms) || (metrics["meanSpeed"] = round(ms; digits = 4))
        isnan(md) || (metrics["meanDisplacement"] = round(md; digits = 4))
        findings = track_measures_qc_findings(n_tracks, dims_param, resolved, det.dims,
                                              det.confidence, det.reason)
        write_qc(img, "tracking.track_measures", value_name, findings; metrics = metrics)
    catch e
        on_log("[QC] could not compute track-measures QC: $e")
    end

    on_log("[INFO] Track measures complete.")
    Dict{String,Any}("valueName" => value_name, "nTracks" => n_tracks, "trackProps" => track_path,
                     "dims" => resolved, "dimsAuto" => det.dims, "dimsReason" => det.reason)
end
