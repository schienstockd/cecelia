# ── Approach toward a target population (docs/TRACKING.md → per-cell / per-track measures) ─────────
#
# Directed motion toward a STRUCTURE, from columns the contacts tasks already write: each cell's
# `<pt>.cell.min_distance#<target>` (µm to the nearest surface of the target population, per timepoint).
# The target is whatever population the contacts task was pointed at — cells, a segmented vessel, the
# TBM — so this is generic; the question lives in the population, not in a direction the user draws.
#
# The nearest target CAN change between frames (`contact_id#`), so this is approach to the population,
# not to one partner. Read it that way.

# The contacts column grammar `<pt>.cell.<kind>#<target>` for readers that need the pop type and
# target back (`_CONTACT_RE` in analysis_runs.jl is `contact_col_regex("contact")`). Name-class
# checks like `_SPATIAL_NUMERIC_MEASURE` (track_props.jl) only test membership and keep their own.
contact_col_regex(kind::AbstractString) = Regex("^([A-Za-z]+)\\.cell\\.$(kind)#(.+)\$")

# THE names derived from one contacts run `(pt, target)` — shared with `_contacts_delete!`
# (analysis_runs.jl), so deleting a contacts run takes both with it. The per-track index lives in the
# track table's OBS (not X/var) for that reason: obs columns can be dropped one at a time.
approach_cell_col(pt, t)  = "$pt.cell.approach#$t"
approach_track_col(pt, t) = "$pt.track.approachIndex#$t"

"""
    is_stale_track_measure(col) -> Bool

A cell-obs column computed along the tracks, so wrong once they change (re-track, track correction):
every `live.cell.*` / `live.track.*`, and any `<pt>.cell.approach#` whatever its pop-type prefix — it
differences distances along the old tracks. Mirrors `is_stale_track_measure` in
`python/cecelia/utils/tracking_utils.py` (the re-track path).
"""
is_stale_track_measure(col::AbstractString)::Bool =
    startswith(col, "live.cell.") || startswith(col, "live.track.") || occursin(".cell.approach#", col)

"""
    approach_columns(obs_cols) -> Vector{NamedTuple{(:src, :cell, :track)}}

Every `<pt>.cell.min_distance#<target>` obs column present, with the per-cell and per-track approach
columns it feeds (`approach_cell_col` / `approach_track_col`). Same prefix, same target suffix, so a
target renamed in the contacts task renames both.
"""
function approach_columns(obs_cols)
    out = NamedTuple{(:src, :cell, :track),NTuple{3,String}}[]
    re = contact_col_regex("min_distance")
    for c in obs_cols
        m = match(re, String(c))
        m === nothing && continue
        push!(out, (src = String(c), cell = approach_cell_col(m[1], m[2]),
                    track = approach_track_col(m[1], m[2])))
    end
    out
end

"""
    step_approach(t, d) -> Vector{Float64}

Per-step approach speed (µm/min) for one track: `−Δd / Δt`, positive = closing in. Assigned to the step's
ENDPOINT cell, like `step_speeds` (celltrackR `increment.cell.id`), so cell 1 is `NaN`. `t` is physical
time, so a bridged frame gap divides by the real interval. A non-finite distance on either side gives
`NaN` (no target meshed at that timepoint, or the cell has no distance).
"""
function step_approach(t::AbstractVector{<:Real}, d::AbstractVector{<:Real})::Vector{Float64}
    n = length(t)
    out = fill(NaN, n)
    for i in 2:n
        dt = t[i] - t[i - 1]
        (dt > 0 && isfinite(d[i]) && isfinite(d[i - 1])) || continue
        out[i] = -(d[i] - d[i - 1]) / dt
    end
    out
end

"""
    approach_index(d, path_length) -> Float64

`(d_first − d_last) / path_length` over the track's first and last FINITE distances — the McCutcheon
index (McCutcheon 1946, Physiol. Rev. 26:319–336, doi:10.1152/physrev.1946.26.3.319) with "toward the
nearest surface of the target" as the direction. In `[−1, 1]`: 1 = every µm travelled closed the gap,
0 = no net approach, negative = moved away. `NaN` with fewer than two finite distances or no path.

Bounded because a cell cannot reduce its distance to a fixed surface by more than it moves; a target
that itself moves toward the cell can push it past 1, which is clamped rather than reported as > 1.
"""
function approach_index(d::AbstractVector{<:Real}, path_length::Real)::Float64
    fin = [x for x in d if isfinite(x)]
    (length(fin) >= 2 && path_length > 0) || return NaN
    clamp((fin[1] - fin[end]) / path_length, -1.0, 1.0)
end

"""
    drop_stale_approach!(img, value_name, pop_type, targets; on_log) -> Int

After a contacts run REWRITES `min_distance#<target>`, the approach columns derived from the old
distances are wrong: drop `approach_cell_col` from the cell obs and `approach_track_col` from the track
table's obs, for each target written. Returns how many columns went. The next track-measures run
recomputes them from the new distances. Absent columns are ignored, so a first contacts run is a no-op.
"""
function drop_stale_approach!(img, value_name::AbstractString, pop_type::AbstractString, targets;
                              on_log::Function = _ -> nothing)::Int
    n = 0
    for (path, col) in ((img_label_props_path(img, value_name), approach_cell_col),
                        (img_track_props_path(img, value_name), approach_track_col))
        isfile(path) || continue
        have = col_names(label_props(path); data_type = :obs)
        stale = [col(pop_type, t) for t in targets if col(pop_type, t) in have]
        isempty(stale) && continue
        label_props(path) |> drop_obs(stale) |> save!
        n += length(stale)
    end
    n > 0 && on_log("[INFO] Dropped $n approach column(s) computed from the old distances — re-run track measures")
    n
end

"""
    approach_measures(props_path, tracks_with_labels) -> (cells, tracks)

Every approach column for one segmentation, for each `min_distance#` target present in its obs NOW
(run contacts first, then track measures). `cells[name]` is aligned to the tracks' cells in
`tracks_with_labels` order — the order `trackMeasures` writes its per-cell rows in; `tracks[name]` is
one value per track, same order, written to the track table's obs. Both empty when no contacts target has been measured.
"""
function approach_measures(props_path::AbstractString, tracks_with_labels)
    cells  = Dict{String,Vector{Float64}}()
    tracks = Dict{String,Vector{Float64}}()
    cols = approach_columns(col_names(label_props(props_path); data_type = :obs))
    isempty(cols) && return (cells = cells, tracks = tracks)
    dist = label_props(props_path) |> select_cols(vcat("label", [c.src for c in cols])) |> as_df
    row_of = Dict(Int(l) => i for (i, l) in enumerate(dist.label))
    for col in cols
        c = Float64[]; t = Float64[]
        for (tr, lbls) in tracks_with_labels
            d = Float64[(r = get(row_of, l, 0); v = r == 0 ? missing : dist[r, col.src];
                         v isa Real ? Float64(v) : NaN) for l in lbls]
            append!(c, step_approach(tr.t, d))
            push!(t, approach_index(d, track_length(tr)))
        end
        cells[col.cell]   = c
        tracks[col.track] = t
    end
    (cells = cells, tracks = tracks)
end
