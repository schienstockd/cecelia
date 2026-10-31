# ── Windowed (tracklet) measures — docs/TRACKING.md → per-cell / per-track measures ────────────────
#
# Whole-track measures depend on track length (straightness falls with duration for any random walk,
# and the imaging window gives fast cells short tracks — Beltman, Marée & de Boer 2009, Nat. Rev.
# Immunol. 9:789, doi:10.1038/nri2638). Measuring over a FIXED duration removes that: every value
# describes the same span of time. The window is in MINUTES, not frames, so movies acquired at
# different frame rates give comparable numbers, and every cell gets one, so every overlapping
# tracklet is used.
#
# TRAILING, not centred: the window ENDS at its cell — celltrackR's `increment.cell.id` convention, the
# same one `step_speeds` uses. A centred window is symmetric only when it is an even number of frames
# (5 min on 1-min frames reaches ±2 frames = a 4-min span, on 0.5-min frames the full 5), so it would
# break exactly the cross-frame-rate comparison it exists for. A trailing window is exact whenever the
# window is a whole number of frames.
#
# Windows are in TIME, never a count of positions: btrack bridges missing detections, so a track's
# positions are not evenly spaced and an index window would silently span more time than asked.

using Statistics: median

"""
    window_measures(t, coords, window) -> (speed, displacement, straightness)

Per position `i` of one track (`t` in physical time, `coords` `n × dims` in µm), the measures over the
positions with `t_i − window ≤ t_j ≤ t_i` — the window ENDING at `i`:
- `speed` = path length ÷ the window's time span (µm per time unit of `t`);
- `displacement` = distance between the window's first and last position;
- `straightness` = displacement ÷ path length (1 when nothing moved);
each by `track_speed` / `track_displacement` / `track_straightness` on the window as a sub-track.

Every value spans EXACTLY `window`: the window needs a position at its start (`t_i − window`), so one
the track does not fully cover — too close to the track's start, or opening on a bridged gap — is `NaN`
rather than a shorter window. A truncated window is exactly the length dependence this exists to
remove. A gap strictly INSIDE the window is fine: the ends are still `window` apart. Pass a window that
is a whole number of frames (`window_span`) — any other value can never be covered.
"""
function window_measures(t::AbstractVector{<:Real}, coords::AbstractMatrix{<:Real}, window::Real)
    n = length(t)
    speed = fill(NaN, n); disp = fill(NaN, n); straight = fill(NaN, n)
    (n < 2 || window <= 0) && return (speed = speed, displacement = disp, straightness = straight)
    eps = 1e-9 * max(1.0, abs(window))
    for i in 1:n
        lo = i; while lo > 1 && t[i] - t[lo - 1] <= window + eps; lo -= 1; end
        hi = i
        (hi > lo && t[hi] - t[lo] >= window - eps) || continue
        # the window as a sub-track, through the celltrackR-ported definitions — one meaning of
        # "speed" / "straightness" whether it covers a whole track or a window of one
        sub = Track(0, Float64.(t[lo:hi]), Matrix{Float64}(coords[lo:hi, :]))
        speed[i]    = track_speed(sub)
        disp[i]     = track_displacement(sub)
        straight[i] = track_straightness(sub)
    end
    (speed = speed, displacement = disp, straightness = straight)
end

# THE windowed column names — per cell in the cell obs, per track (median over the track's cells) in
# the track table. One window per run (docs/TRACKING.md), so the names carry no window size.
const WINDOW_CELL_COLS  = ("live.cell.window.speed", "live.cell.window.displacement",
                           "live.cell.window.straightness")
const WINDOW_TRACK_COLS = ("live.track.window.speed", "live.track.window.straightness")

_finite_median(x) = (f = [v for v in x if isfinite(v)]; isempty(f) ? NaN : median(f))

"""
    window_span(window, frame_interval) -> Float64

The span a `window` actually measures on frames `frame_interval` apart: the largest whole number of
frames that fits (`0` when none does). Equal to `window` when it is a multiple of the frame interval;
otherwise shorter, and the task says so — "5 min" on 2-min frames measures 4.
"""
window_span(window::Real, frame_interval::Real)::Float64 =
    frame_interval > 0 ? floor(window / frame_interval + 1e-9) * frame_interval : 0.0

"""
    track_window_measures(tracks_with_labels, window, frame_interval) -> (cells, tracks)

[`window_measures`] over every track, at `window_span(window, frame_interval)` — the window snapped to
whole frames, so every value covers the same measured duration: `cells[col]` aligned to the tracks' cells in
`tracks_with_labels` order (the order `trackMeasures` writes its per-cell rows in), `tracks[col]` one
median per track over its finite windowed values (`NaN` for a track shorter than the window). Both empty
when `window ≤ 0` — windowed measures off.
"""
function track_window_measures(tracks_with_labels, window::Real, frame_interval::Real)
    cells  = Dict{String,Vector{Float64}}()
    tracks = Dict{String,Vector{Float64}}()
    window > 0 || return (cells = cells, tracks = tracks)
    window = window_span(window, frame_interval)
    sp, di, st = Float64[], Float64[], Float64[]
    tsp, tst = Float64[], Float64[]
    for (tr, _) in tracks_with_labels
        w = window_measures(tr.t, tr.coords, window)
        append!(sp, w.speed); append!(di, w.displacement); append!(st, w.straightness)
        push!(tsp, _finite_median(w.speed)); push!(tst, _finite_median(w.straightness))
    end
    cells[WINDOW_CELL_COLS[1]] = sp; cells[WINDOW_CELL_COLS[2]] = di; cells[WINDOW_CELL_COLS[3]] = st
    tracks[WINDOW_TRACK_COLS[1]] = tsp; tracks[WINDOW_TRACK_COLS[2]] = tst
    (cells = cells, tracks = tracks)
end
