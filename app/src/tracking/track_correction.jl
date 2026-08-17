# ── track_correction — manual track edits as pure ops over the cell table ────────
#
# The engine behind `tracking.correct` (docs/todo/CORRECTION_PLAN.md, P1). A track correction is an
# `obs` rewrite and nothing else: it moves cells between `track_id`s. It never touches `X`/`var`, so
# the cell table needs no re-measure — only `tracking.track_measures` has to run again, and that
# rebuilds every track from `obs.track_id` alone (`track_measures.jl:185`), which is what makes this
# half of correction cheap and independently shippable.
#
# Ports R `trackHelpers.R:436-484` (`tracks.points.rm/add`, `tracks.rm`, `tracks.join`) with three
# deliberate changes, all recorded in CORRECTION_PLAN.md:
#
#   1. `track.split` is new — the obvious complement to join, absent from old R.
#   2. A join CONSUMES the second track. Old R re-assigned only B's non-overlapping timepoints, which
#      left B alive as a remnant of whatever overlapped. Here an overlap is REJECTED instead (see
#      `_join_tracks`) — two tracks that coexist at one timepoint are not one cell, and silently
#      picking a winner is a guess the user should make.
#   3. Lineage and `cell_id` are maintained, not left stale. Old R only ever wrote `track_id`.
#
# EVERYTHING HERE IS PURE. Ops take a label-keyed DataFrame and mutate it in place; nothing reads or
# writes a file. That is what lets the whole op set be unit-tested without a fixture, and it is why
# the interactive (uncommitted) staging in the API can run the same ops as the task — one engine, no
# second implementation of "what a join means".

using DataFrames: DataFrame, nrow, eachrow
using Statistics: mean, median, quantile

# The obs columns a correction owns. `track_id` is the assignment; the rest are btrack's lineage
# output, which becomes wrong the moment cells move between tracks.
#
# Byte-for-byte convention, verified against a real tracked image
# (`zolIMa/1/fXgbTl/labelProps/memTom.h5ad`, 374 tracks): all six are **float64** (`add_obs` writes
# Float64 only — `label_props.jl:685`), and for a track with no parent
# `track_parent == track_root == track_id`, `track_generation == 0`. A root's parent is ITSELF, not
# `NaN` — do not "clear" a parent to make a root.
const TRACK_LINEAGE_OBS = ["track_parent", "track_root", "track_state", "track_generation", "cell_id"]
const TRACK_CORRECTION_OBS = vcat(["track_id"], TRACK_LINEAGE_OBS)

# `track_state` is btrack's per-object classification and is NOT derived from the track's identity —
# in real data it is constant within a track and carries no lineage meaning. A correction therefore
# leaves it alone. Writing NaN over it (an earlier draft of the plan) would invent a value rather
# than repair one.
const _TRACK_STATE_COL = "track_state"

# Cells carrying no track. `add_obs` writes NaN for any label absent from the staged frame
# (`label_props.jl:679`), and `track_measures` already skips NaN track ids (`:213`), so NaN is the
# established "untracked" value on both sides — a correction uses the same one rather than 0 or -1.
_is_untracked(v) = !(v isa Real) || isnan(v)

"""
    track_ids_present(df) -> Vector{Int}

Sorted distinct tracked ids in `df` (NaN/untracked excluded).
"""
function track_ids_present(df::DataFrame)::Vector{Int}
    "track_id" in names(df) || return Int[]
    ids = Set{Int}()
    for v in df.track_id
        _is_untracked(v) || push!(ids, Int(round(Float64(v))))
    end
    sort!(collect(ids))
end

"""
    next_track_id(df) -> Int

The id a newly created track takes: `max(track_id) + 1`, ignoring untracked cells (old R's
`max(popDT\$track_id, na.rm = TRUE) + 1`, `trackHelpers.R:455`). Recomputed per op, so several
allocating ops in one batch cannot collide. `1` when nothing is tracked yet.
"""
function next_track_id(df::DataFrame)::Int
    ids = track_ids_present(df)
    isempty(ids) ? 1 : maximum(ids) + 1
end

# Row indices of the cells belonging to `track_id`.
_rows_of_track(df::DataFrame, tid::Integer) =
    findall(v -> !_is_untracked(v) && Int(round(Float64(v))) == Int(tid), df.track_id)

# Row indices of the given labels. Labels absent from `df` are IGNORED rather than an error: a
# selection made in napari can name a label the current population scope no longer contains, and
# dropping it is the same forgiving behaviour as old R's `label %in% labelIDs`.
_rows_of_labels(df::DataFrame, labels) =
    findall(l -> Int(round(Float64(l))) in Set(Int(round(Float64(x))) for x in labels), df.label)

# ── The ops ──────────────────────────────────────────────────────────────────────
#
# Each returns a short human-readable summary of what it did, which becomes the journal entry's
# rendered line and the edit-history row. An op that cannot be applied THROWS with an actionable
# message — the task turns that into a failed run rather than a silently partial correction.

"""Untrack the given cells (`track_id := NaN`). R `tracks.points.rm`."""
function _remove_points!(df::DataFrame, labels)::String
    rows = _rows_of_labels(df, labels)
    isempty(rows) && return "no matching cells"
    _untrack_rows!(df, rows)
    "untracked $(length(rows)) cell(s)"
end

"""
Assign the given cells to a track. With no `track_id`, allocates a new one (R `tracks.points.add`).

Rejects a cell whose timepoint the target track already occupies — the same rule as `_join_tracks`,
for the same reason: a track cannot be in two places at one time, and picking a winner is a guess.
"""
function _add_points!(df::DataFrame, labels, tid::Union{Nothing,Integer})::String
    rows = _rows_of_labels(df, labels)
    isempty(rows) && return "no matching cells"
    target = isnothing(tid) ? next_track_id(df) : Int(tid)

    # times the target already holds, excluding the cells being moved INTO it
    moving = Set(rows)
    prior = [r for r in _rows_of_track(df, target) if !(r in moving)]
    held = Set{Float64}(Float64(df[r, :centroid_t]) for r in prior)
    clash = sort!(unique(Float64[df[r, :centroid_t] for r in rows if Float64(df[r, :centroid_t]) in held]))
    isempty(clash) || error("track $target already has a cell at timepoint(s) " *
                            join(Int.(clash), ", ") * " — untrack the duplicate first")

    for r in rows
        df[r, :track_id] = Float64(target)
    end
    # A track with no PRIOR cells is new (whether the id was allocated here or named by the caller),
    # so it becomes a root. One that already existed keeps the lineage it has and the added cells
    # adopt it — resetting the target to root here would silently orphan a track that has a real
    # parent just because the user attached one more cell to it.
    isempty(prior) ? _reset_lineage_to_root!(df, target) : _copy_lineage_from!(df, target, rows)
    "added $(length(rows)) cell(s) to track $target"
end

"""Untrack every cell of the given tracks. R `tracks.rm`."""
function _remove_tracks!(df::DataFrame, tids)::String
    n = 0
    for tid in tids
        rows = _rows_of_track(df, Int(round(Float64(tid))))
        _untrack_rows!(df, rows)
        n += length(rows)
    end
    "removed $(length(collect(tids))) track(s), untracking $n cell(s)"
end

"""
Join track `b` into track `a` — every cell of `b` becomes part of `a`, and `b` ceases to exist.

**Rejects a temporal overlap.** Old R re-assigned only B's non-overlapping timepoints
(`trackHelpers.R:475-485`), which left the overlapping cells on `track_id == B` — so "join" quietly
produced a third, shorter track. Consuming the overlap instead would give `a` two cells at one
timepoint, which makes every `dt` in `track_measures` zero and its speeds infinite. Neither is
acceptable, and choosing which duplicate detection is real is the user's call, not ours.

Lineage follows `a`: a joined track keeps A's parent/root/generation (CORRECTION_PLAN.md 4c). This
matters because `_track_lineage` (`track_measures.jl:267`) takes a track's lineage from its FIRST
cell by time, so without this the join would silently adopt whichever track happened to start first.
"""
function _join_tracks!(df::DataFrame, a::Integer, b::Integer)::String
    a, b = Int(a), Int(b)
    a == b && error("cannot join track $a to itself")
    rows_a, rows_b = _rows_of_track(df, a), _rows_of_track(df, b)
    isempty(rows_a) && error("track $a has no cells")
    isempty(rows_b) && error("track $b has no cells")

    t_a = Set{Float64}(Float64(df[r, :centroid_t]) for r in rows_a)
    overlap = sort!(unique(Float64[df[r, :centroid_t] for r in rows_b
                                   if Float64(df[r, :centroid_t]) in t_a]))
    isempty(overlap) || error("tracks $a and $b both have a cell at timepoint(s) " *
                              join(Int.(overlap), ", ") *
                              " — they cannot be one cell; untrack the duplicates first")

    for r in rows_b
        df[r, :track_id] = Float64(a)
    end
    _copy_lineage_from!(df, a, rows_b)          # B's cells adopt A's lineage
    "joined track $b into $a ($(length(rows_b)) cell(s))"
end

"""
Split a track at a timepoint: cells at `at_t` and later become a NEW track; earlier cells keep the
original id. No old-R equivalent — this is new (CORRECTION_PLAN.md Decision 6).

The new fragment becomes a ROOT: `track_parent = track_root = <new id>`, `track_generation = 0`,
which is btrack's own convention for a parentless track (verified on real data — a root's parent is
itself, not NaN).
"""
function _split_track!(df::DataFrame, tid::Integer, at_t::Real)::String
    tid = Int(tid)
    rows = _rows_of_track(df, tid)
    isempty(rows) && error("track $tid has no cells")

    times = Float64[df[r, :centroid_t] for r in rows]
    later = [r for (r, t) in zip(rows, times) if t >= Float64(at_t)]
    (isempty(later) || length(later) == length(rows)) &&
        error("splitting track $tid at t=$(Int(at_t)) would leave one side empty — " *
              "pick a timepoint inside the track (it spans $(Int(minimum(times)))–$(Int(maximum(times)))))")

    new_id = next_track_id(df)
    for r in later
        df[r, :track_id] = Float64(new_id)
    end
    _reset_lineage_to_root!(df, new_id)
    "split track $tid at t=$(Int(at_t)) → new track $new_id ($(length(later)) cell(s))"
end

# ── Lineage maintenance ──────────────────────────────────────────────────────────

# An untracked cell has no track identity left, so every lineage column goes with the track_id.
# `track_state` is per-cell and stays (see `_TRACK_STATE_COL`).
function _untrack_rows!(df::DataFrame, rows)
    for r in rows
        df[r, :track_id] = NaN
        for c in TRACK_LINEAGE_OBS
            c == _TRACK_STATE_COL && continue
            c in names(df) && (df[r, Symbol(c)] = NaN)
        end
    end
end

# Make `tid` a root: parent = root = itself, generation 0. Applied to every cell of the track, since
# lineage is constant within a track.
function _reset_lineage_to_root!(df::DataFrame, tid::Integer)
    for r in _rows_of_track(df, Int(tid))
        "track_parent"     in names(df) && (df[r, :track_parent]     = Float64(tid))
        "track_root"       in names(df) && (df[r, :track_root]       = Float64(tid))
        "track_generation" in names(df) && (df[r, :track_generation] = 0.0)
    end
end

# Copy the lineage of track `src` onto the given rows (used by join: B's cells adopt A's identity).
function _copy_lineage_from!(df::DataFrame, src::Integer, rows)
    src_rows = _rows_of_track(df, Int(src))
    isempty(src_rows) && return
    ref = first(setdiff(src_rows, rows), 1)
    ref = isempty(ref) ? first(src_rows) : only(ref)
    for c in TRACK_LINEAGE_OBS
        c == _TRACK_STATE_COL && continue
        c in names(df) || continue
        v = df[ref, Symbol(c)]
        for r in rows
            df[r, Symbol(c)] = v
        end
    end
end

"""
    renumber_cell_ids!(df) -> Int

Rewrite `cell_id` as the 1-based rank of each cell WITHIN its track, ordered by time — the exact
definition the tracking task uses (`tracking_utils.py:110`,
`groupby("track_id")["t"].rank(method="first")`). Any join, split or point move invalidates it, so
this runs once after a batch of ops rather than inside each one. Returns the number of cells
renumbered.

This is the column old R left stale on every correction.
"""
function renumber_cell_ids!(df::DataFrame)::Int
    "cell_id" in names(df) || return 0
    n = 0
    for tid in track_ids_present(df)
        rows = _rows_of_track(df, tid)
        order = sortperm(Float64[df[r, :centroid_t] for r in rows])
        for (i, k) in enumerate(order)
            df[rows[k], :cell_id] = Float64(i)
            n += 1
        end
    end
    n
end

# ── Op dispatch + replay ─────────────────────────────────────────────────────────

# One vocabulary for the ops, used by the task param, the journal and (later) the API. JSON-shaped
# so an op round-trips through all three unchanged — the journal is literally the ops that ran.
const TRACK_OP_KINDS = ("points.remove", "points.add", "track.remove", "track.join", "track.split")

_op_labels(op) = get(op, "labels", get(op, :labels, Int[]))
_op_tracks(op) = get(op, "trackIds", get(op, :trackIds, Int[]))
_op_get(op, k) = get(op, k, get(op, Symbol(k), nothing))

"""
    apply_track_op!(df, op) -> String

Apply ONE op to `df` in place, returning a summary line. Throws `ArgumentError` on an unknown kind
and a descriptive error when the op cannot be applied (overlap, empty side, missing track).
"""
function apply_track_op!(df::DataFrame, op)::String
    kind = string(something(_op_get(op, "op"), ""))
    kind in TRACK_OP_KINDS ||
        throw(ArgumentError("unknown track correction op \"$kind\" — expected one of " *
                            join(TRACK_OP_KINDS, ", ")))

    if kind == "points.remove"
        _remove_points!(df, _op_labels(op))
    elseif kind == "points.add"
        tid = _op_get(op, "trackId")
        _add_points!(df, _op_labels(op), isnothing(tid) ? nothing : Int(round(Float64(tid))))
    elseif kind == "track.remove"
        _remove_tracks!(df, _op_tracks(op))
    elseif kind == "track.join"
        tids = collect(_op_tracks(op))
        length(tids) == 2 || error("track.join needs exactly 2 trackIds, got $(length(tids))")
        _join_tracks!(df, Int(round(Float64(tids[1]))), Int(round(Float64(tids[2]))))
    else  # track.split
        tid = _op_get(op, "trackId")
        at  = _op_get(op, "atT")
        (isnothing(tid) || isnothing(at)) && error("track.split needs trackId and atT")
        _split_track!(df, Int(round(Float64(tid))), Float64(at))
    end
end

"""
    apply_track_ops!(df, ops) -> Vector{Dict{String,Any}}

Apply `ops` in order, then renumber `cell_id` once. Returns one journal entry per op —
`{op, …inputs…, summary}` — which is what gets appended to `corrections/{value_name}.json`.

Ops are applied SEQUENTIALLY and each sees the previous result, so the list is a replay script: the
same ops against the same producing-task output reproduce the same corrected state. That is the
whole basis of Decision 3's reproducibility claim, so do not reorder or parallelise them.
"""
function apply_track_ops!(df::DataFrame, ops)::Vector{Dict{String,Any}}
    entries = Dict{String,Any}[]
    for op in ops
        summary = apply_track_op!(df, op)
        entry = Dict{String,Any}(string(k) => v for (k, v) in pairs(op))
        entry["summary"] = summary
        push!(entries, entry)
    end
    renumber_cell_ids!(df)
    entries
end

# ── Journal sidecar: {task_dir}/corrections/{value_name}.json ────────────────────
#
# Shaped after `gating/{value_name}.json` (`gating_dir`/`gating_path`/`save_pop_map!`,
# population_manager.jl:340) — a per-segmentation sidecar next to the gates, written with
# `write_json_atomic` so a concurrent reader never sees a truncated file and an interrupted write
# keeps the previous journal. Append-only: this is the durable, per-image edit history that old R
# kept only in Shiny session state (CORRECTION_PLAN.md Decision 7).

corrections_dir(task_dir::AbstractString) = joinpath(task_dir, "corrections")
corrections_path(task_dir::AbstractString, value_name::AbstractString) =
    joinpath(corrections_dir(task_dir), string(value_name) * ".json")

"""
    load_corrections(task_dir, value_name) -> Dict

Read the journal (`{"valueName", "entries"}`), or an empty one when absent.
"""
function load_corrections(task_dir::AbstractString, value_name::AbstractString)::Dict{String,Any}
    path = corrections_path(task_dir, value_name)
    isfile(path) || return Dict{String,Any}("valueName" => String(value_name),
                                            "entries"   => Dict{String,Any}[])
    raw = JSON3.read(read(path, String), Dict{String,Any})
    get!(raw, "valueName", String(value_name))
    get!(raw, "entries", Dict{String,Any}[])
    raw
end

"""
    append_corrections!(task_dir, value_name, entries; run_id) -> String

Append `entries` to the journal and write it atomically. Each entry is stamped with the run that
produced it so the history can be read back grouped by correction run. Returns the path.
"""
function append_corrections!(task_dir::AbstractString, value_name::AbstractString,
                             entries::AbstractVector; run_id = nothing)::String
    doc = load_corrections(task_dir, value_name)
    existing = collect(Dict{String,Any}, doc["entries"])
    seq = length(existing)
    for e in entries
        rec = Dict{String,Any}(string(k) => v for (k, v) in pairs(e))
        rec["seq"] = (seq += 1)
        isnothing(run_id) || (rec["runId"] = string(run_id))
        push!(existing, rec)
    end
    doc["entries"] = existing
    dir = corrections_dir(task_dir)
    isdir(dir) || mkpath(dir)
    path = corrections_path(task_dir, String(value_name))
    write_json_atomic(path, doc)
    path
end

# ── QC (Decision 8) ──────────────────────────────────────────────────────────────

"""
    track_correction_metrics(before, after, n_ops) -> Dict

Objective counts for a correction run, from the `track_id` vectors before and after. Pure so it can
be unit-tested without a file:

  • `nOps`             — ops applied
  • `nCellsReassigned` — cells whose `track_id` changed (incl. to/from untracked)
  • `nTracksBefore/After`
  • `fracCellsEdited`  — reassigned / total cells, the share Decision 8's threshold reads
  • `nShortTracks`     — tracks THIS correction left below `MIN_USEFUL_TRACK_LENGTH` timepoints (4d)

`nShortTracks` counts only tracks the correction is responsible for — ones that are newly short, not
ones that were already short before it ran. Counting every short track would make a correction that
edits nothing report a warning about the *tracking* run's leftovers, blaming the wrong task.
"""
const MIN_USEFUL_TRACK_LENGTH = 5   # matches bayesian_tracking's own `minTimepoints` default

# id → number of cells carrying it (untracked excluded)
function _track_sizes(v::AbstractVector)::Dict{Int,Int}
    counts = Dict{Int,Int}()
    for x in v
        _is_untracked(x) && continue
        k = Int(round(Float64(x)))
        counts[k] = get(counts, k, 0) + 1
    end
    counts
end

function track_correction_metrics(before::AbstractVector, after::AbstractVector,
                                  n_ops::Integer)::Dict{String,Any}
    n = min(length(before), length(after))
    reassigned = 0
    for i in 1:n
        b, a = before[i], after[i]
        bu, au = _is_untracked(b), _is_untracked(a)
        changed = (bu != au) || (!bu && !au && Int(round(Float64(b))) != Int(round(Float64(a))))
        changed && (reassigned += 1)
    end
    was, now = _track_sizes(before), _track_sizes(after)
    # short BECAUSE of this correction: short now, AND either a new id or previously long enough
    newly_short = 0
    for (tid, sz) in now
        sz < MIN_USEFUL_TRACK_LENGTH || continue
        (!haskey(was, tid) || was[tid] >= MIN_USEFUL_TRACK_LENGTH) && (newly_short += 1)
    end
    Dict{String,Any}(
        "nOps"             => Int(n_ops),
        "nCellsReassigned" => reassigned,
        "nTracksBefore"    => length(was),
        "nTracksAfter"     => length(now),
        "fracCellsEdited"  => n == 0 ? 0.0 : round(reassigned / n, digits = 4),
        "nShortTracks"     => newly_short,
    )
end

# Share of cells above which hand-correction is the wrong tool. A mask/tracking result that needs a
# third of its cells re-assigned by hand is a tracking-parameter problem, not a correction job —
# the same tripwire logic as Decision 8's segmentation threshold.
const TRACK_CORRECTION_WARN_FRAC = 0.3

"""
    track_correction_qc_findings(metrics) -> Vector

Advisory findings for a correction run. Pure (no I/O) so it is unit-tested directly, per
`docs/MODULES.md`. Never `error` — QC is advisory and a correction the user asked for is not a
failure.
"""
function track_correction_qc_findings(metrics::AbstractDict)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    frac = Float64(get(metrics, "fracCellsEdited", 0.0))
    if frac >= TRACK_CORRECTION_WARN_FRAC
        push!(out, qc_finding("warn", "correction.large_share_edited";
                              pct = string(round(Int, frac * 100))))
    end
    n_short = Int(get(metrics, "nShortTracks", 0))
    if n_short > 0
        push!(out, qc_finding("warn", "correction.short_tracks"; count = string(n_short),
                              min = string(MIN_USEFUL_TRACK_LENGTH)))
    end
    out
end

# ── Finding what needs correcting ────────────────────────────────────────────────
#
# The part old R had NO answer to, and the actual work. Applying a join is one click; finding which
# of 374 tracks is wrong is an afternoon of scrubbing a timelapse. Tracking failures have signatures,
# so detect them and hand the user a worklist with the fix already chosen (CORRECTION_PLAN.md P4).
#
# Each candidate carries the OP that would fix it, in the same `TRACK_OP_KINDS` vocabulary the task
# takes — so a worklist row is literally a submittable op, and nothing has to translate "suggestion"
# into "edit". It also carries `atT` + `centroid` so a UI can fly the viewer to the problem
# (`napari_bridge.centre(pos, tp)`), and a `reason` string that is the instruction, not a diagnosis:
# what to look at, and what to do.
#
# These are CANDIDATES, never auto-applied. A gap that looks like one cell may be two cells passing;
# only the user can see that. The detector's job is to put the decision in front of them.

"""One suspicious thing found in a tracking result, with the op that would fix it."""
struct TrackIssue
    kind::String                    # "gap" | "jump" | "short"
    op::Dict{String,Any}            # a ready-to-submit op (TRACK_OP_KINDS)
    track_ids::Vector{Int}
    at_t::Float64                   # the timepoint to show
    centroid::Vector{Float64}       # µm, where to look
    severity::Float64               # for ranking — bigger = more suspicious
    reason::String                  # the instruction shown to the user
end

# Defaults, deliberately conservative: better to miss a candidate than to bury the real ones.
# A gap is only a join candidate if the two ends are CLOSE in space and NEAR in time — that is the
# signature of one cell whose detection dropped out, as opposed to two different cells.
#
# THE DISTANCE THRESHOLDS ARE RELATIVE TO THE DATA, NOT ABSOLUTE µm. Measured on a real timelapse
# (`zolIMa/1/fXgbTl`, 374 tracks, 15 s/frame): a fixed "15 µm" flagged 79 gaps — 21% of all tracks —
# while "5 µm" flagged 4. The number was doing all the work and it was picked out of the air. The
# median within-track step on that image is ~1 µm, so the meaningful question is not "how many µm"
# but "how far compared with how far these cells normally move in a frame". Expressed in step units
# the same setting travels across cell types, magnifications and frame intervals; in µm it does not.
const TRACK_GAP_MAX_FRAMES = 3       # frames between A's end and B's start
const TRACK_GAP_STEPS      = 3.0     # × the image's median step, between A's last and B's first
const TRACK_JUMP_FACTOR    = 4.0     # a step this many × the track's OWN median step is suspect
# …AND in the top (1 − this) of all steps in the image. A second, distribution-based floor is needed
# because step lengths are heavy-tailed: on the reference image the median step is 1.4 µm but p90 is
# 4.9 and p99 is 10.3, so "4 × median" lands around p90 and flagged 90 of 374 tracks — a quarter of
# the image, which is a worklist nobody reads. A quantile floor is self-calibrating: it flags a fixed
# SHARE of the steps that are extreme *for this image*, whatever the tail looks like.
const TRACK_JUMP_QUANTILE  = 0.99

# per-track (t, coords) sorted by time; coords already in µm
function _track_paths(df::DataFrame, spatial::Vector{String})
    paths = Dict{Int,Vector{Tuple{Float64,Vector{Float64}}}}()
    for r in 1:nrow(df)
        v = df[r, :track_id]
        _is_untracked(v) && continue
        tid = Int(round(Float64(v)))
        push!(get!(paths, tid, Tuple{Float64,Vector{Float64}}[]),
              (Float64(df[r, :centroid_t]), Float64[Float64(df[r, Symbol(c)]) for c in spatial]))
    end
    for (_, p) in paths; sort!(p; by = first); end
    paths
end

_dist(a::Vector{Float64}, b::Vector{Float64}) = sqrt(sum((a .- b) .^ 2))

# All within-track step distances (µm) in the image.
function _all_steps(paths)::Vector{Float64}
    out = Float64[]
    for (_, p) in paths, i in 2:length(p)
        push!(out, _dist(p[i][2], p[i-1][2]))
    end
    out
end

"""
    track_step_scale(paths_or_df, spatial) -> Float64

The image's median within-track step, in µm — the yardstick every distance threshold in
`find_track_issues` is expressed in.

Two distinct "no scale" answers, and the difference matters: **`NaN`** when there is no step to
measure at all (nothing tracked, or every track is a single timepoint), and **`0.0`** when there are
steps but nothing moves. The detector rejects both (`isfinite && > 0`) and then reports only the
count-based signature, rather than inventing a µm threshold — which is the mistake the whole
step-relative scheme exists to avoid.
"""
function track_step_scale(df::DataFrame, spatial::Vector{String})::Float64
    steps = _all_steps(_track_paths(df, spatial))
    isempty(steps) ? NaN : median(steps)
end

"""
    find_track_issues(df, spatial; gap_frames, gap_steps, jump_factor, jump_min_steps, min_len,
                      step_scale) -> Vector{TrackIssue}

Scan a corrected-or-not cell table for tracks that look wrong, most suspicious first. `df` needs
`label`, `track_id`, `centroid_t` and the `spatial` centroid columns **already in µm** (use the
shared `scale_centroids!` — do not multiply by hand).

Distances are in units of the image's own median step (`track_step_scale`), not µm — see the note on
`TRACK_GAP_STEPS`. Pass `step_scale` to override it (e.g. to reuse one scale across a set).

Three signatures, each with an unambiguous fix:

  • **gap** → `track.join`. Track A ends, track B starts within `gap_frames` frames and
    `gap_steps` × the median step. The most common btrack failure by far: one cell whose segmentation
    dropped out for a frame becomes two tracks. Ranked closest-first.
  • **jump** → `track.split`. A step that is an outlier BOTH for its own track (`jump_factor`× that
    track's median) AND for the image (above the `jump_quantile` of every step). Signature of two
    different cells linked into one track. **Runs of consecutive suspect steps collapse into one
    candidate** — a cell that jumps away and back is one mistake, and reporting it per step buries the
    rest of the worklist.
  • **short** → `track.remove`. A track below `min_len` timepoints. Normally EMPTY on fresh output,
    because `bayesian_tracking` already drops short tracks — this fires on what a *split* leaves
    behind, which is exactly when nobody is looking for it.

Pure: no file IO, so it is unit-tested directly and can run against a staged (uncommitted) frame just
as well as the file on disk.
"""
function find_track_issues(df::DataFrame, spatial::Vector{String};
                           gap_frames::Real     = TRACK_GAP_MAX_FRAMES,
                           gap_steps::Real      = TRACK_GAP_STEPS,
                           jump_factor::Real    = TRACK_JUMP_FACTOR,
                           jump_quantile::Real  = TRACK_JUMP_QUANTILE,
                           min_len::Integer     = MIN_USEFUL_TRACK_LENGTH,
                           step_scale::Union{Nothing,Real} = nothing)::Vector{TrackIssue}
    isempty(spatial) && return TrackIssue[]
    paths = _track_paths(df, spatial)
    out   = TrackIssue[]

    all_steps = _all_steps(paths)
    scale = isnothing(step_scale) ? (isempty(all_steps) ? NaN : median(all_steps)) :
            Float64(step_scale)
    # No measurable motion → no defensible distance threshold. Report the count-based signature only
    # rather than inventing a µm number, which is the mistake this whole scheme replaces.
    have_scale = isfinite(scale) && scale > 0
    gap_um  = have_scale ? gap_steps * scale : 0.0
    jump_um = isempty(all_steps) ? Inf : quantile(all_steps, clamp(Float64(jump_quantile), 0.0, 1.0))

    # ── gap → join ────────────────────────────────────────────────────────────
    # Index track STARTS by frame so this is O(tracks × gap_frames), not O(tracks²): a real image has
    # hundreds of tracks and this runs on every page load.
    if have_scale
        starts = Dict{Int,Vector{Int}}()
        for (tid, p) in paths
            push!(get!(starts, Int(round(first(p)[1])), Int[]), tid)
        end
        for (tid, p) in paths
            t_end, c_end = last(p)
            for dt in 1:Int(gap_frames), b in get(starts, Int(round(t_end)) + dt, Int[])
                b == tid && continue
                c_start = first(paths[b])[2]
                d = _dist(c_end, c_start)
                d <= gap_um || continue
                push!(out, TrackIssue("gap",
                    Dict{String,Any}("op" => "track.join", "trackIds" => [tid, b]),
                    [tid, b], t_end, c_end,
                    # closer + shorter gap = more likely one cell
                    (gap_um - d) / gap_um + (Float64(gap_frames) - dt + 1) / Float64(gap_frames),
                    "Track $tid ends at t=$(Int(t_end)); track $b starts $(dt) frame(s) later, " *
                    "$(round(d; digits = 1)) µm away — $(round(d / scale; digits = 1))× a normal " *
                    "step. Check they are the same cell, then join."))
            end
        end
    end

    # ── jump → split ──────────────────────────────────────────────────────────
    for (tid, p) in paths
        length(p) >= 3 || continue
        steps = [_dist(p[i][2], p[i-1][2]) for i in 2:length(p)]
        med = median(steps)
        med > 0 || continue
        suspect = [i for (i, s) in enumerate(steps) if s >= jump_factor * med && s >= jump_um]
        isempty(suspect) && continue
        # collapse consecutive suspect steps into ONE candidate, keeping the worst step in the run:
        # a cell that leaps out at t and back at t+1 is one bad link, not two.
        run_start = 1
        for k in 1:length(suspect)
            is_last = k == length(suspect) || suspect[k+1] != suspect[k] + 1
            is_last || continue
            run = suspect[run_start:k]
            run_start = k + 1
            i = run[argmax(steps[run])]      # the worst step in the run
            s, t_at = steps[i], p[i+1][1]
            push!(out, TrackIssue("jump",
                Dict{String,Any}("op" => "track.split", "trackId" => tid, "atT" => t_at),
                [tid], t_at, p[i+1][2], s / med,
                "Track $tid jumps $(round(s; digits = 1)) µm into t=$(Int(t_at)) — " *
                "$(round(s / med; digits = 1))× its usual step" *
                (length(run) > 1 ? " ($(length(run)) suspect steps in a row)" : "") *
                ". If that is a different cell, split it here."))
        end
    end

    # ── short → remove ────────────────────────────────────────────────────────
    for (tid, p) in paths
        length(p) < min_len || continue
        push!(out, TrackIssue("short",
            Dict{String,Any}("op" => "track.remove", "trackIds" => [tid]),
            [tid], first(p)[1], first(p)[2],
            Float64(min_len - length(p)) / min_len,
            "Track $tid is only $(length(p)) timepoint(s) — too short to measure. " *
            "Join it to a neighbour, or remove it."))
    end

    sort!(out; by = i -> -i.severity)
    out
end

"""
    track_issues_for(props_path, pixel_res; kwargs...) -> Vector{TrackIssue}

`find_track_issues` against a cell table on disk. Reads only the columns it needs and converts
centroids to µm through the shared `scale_centroids!`, so distances here mean the same thing as in
`track_measures` and the spatial tasks.
"""
function track_issues_for(props_path::AbstractString, pixel_res::AbstractVector{<:Real};
                          kwargs...)::Vector{TrackIssue}
    lp = label_props(props_path)
    ("track_id" in col_names(lp; data_type = :obs)) || return TrackIssue[]
    spatial  = centroid_columns(lp; order = [:x, :y, :z])
    temporal = temporal_columns(lp)
    isempty(temporal) && return TrackIssue[]
    select_cols(lp, vcat(spatial, temporal, ["track_id"]))
    df = as_df(lp; include_x = false, include_obs = true)
    scale_centroids!(df, pixel_res)
    t_col = first(temporal)
    t_col == "centroid_t" || (df[!, :centroid_t] = df[!, Symbol(t_col)])
    find_track_issues(df, spatial; kwargs...)
end

"""Plain-data form of a `TrackIssue`, for the API/UI (and so the op is submittable verbatim)."""
issue_to_dict(i::TrackIssue) = Dict{String,Any}(
    "kind" => i.kind, "op" => i.op, "trackIds" => i.track_ids,
    "atT" => i.at_t, "centroid" => i.centroid,
    "severity" => round(i.severity; digits = 3), "reason" => i.reason)

# ── Cell-pair angle/distance analysis (celltrackR) ───────────────────────────────
#
# Port of celltrackR's `analyzeCellPairs` (Wortel et al. 2021, Cell Reports Methods,
# doi:10.1016/j.crmeth.2021.100006; the analysis itself is Beltman, Marée & de Boer 2009,
# PMID 19834485). Verified against celltrackR 1.2.2's own QC vignette (`doc/QC.Rmd` §2.3, §3.1),
# which is where the two uses and their thresholds come from.
#
# For every PAIR of tracks: the angle between their displacement vectors, and the closest they ever
# come while both exist. One computation, two questions the same numbers answer:
#
#   • **Double tracking** (§3.1) — a pair with a small angle AND a small distance is one cell
#     segmented twice. This is a correction signature `find_track_issues` could not see any other
#     way: both tracks look individually healthy, and only their RELATIONSHIP is wrong.
#   • **Global drift** (§2.3) — across all pairs the mean angle should sit near 90° at every
#     distance. Consistently below it, even for cells far apart, means the whole field is moving
#     together: a stage/tissue drift artefact, not migration. That is a whole-image verdict rather
#     than a per-track fix, so it is reported, never "corrected" here — the fix is
#     `cleanupImages.driftCorrect` upstream.
#
# `distance` is NaN for a pair that never coexists (celltrackR returns NA), and such a pair can never
# be double tracking — you cannot be two places at once if you were never anywhere at the same time.

"""Angle in DEGREES between two vectors; `NaN` if either has no length (a track that never moved)."""
function _vec_angle_deg(u::Vector{Float64}, v::Vector{Float64})::Float64
    nu, nv = sqrt(sum(u .^ 2)), sqrt(sum(v .^ 2))
    (nu == 0 || nv == 0) && return NaN
    rad2deg(acos(clamp(sum(u .* v) / (nu * nv), -1.0, 1.0)))
end

"""
    analyze_cell_pairs(df, spatial) -> DataFrame

`(track1, track2, angle, distance, n_shared)` for every pair of tracks, per celltrackR
`analyzeCellPairs`:

  • `angle`    — degrees between the two tracks' **displacement vectors** (first → last position)
  • `distance` — the **minimum** separation at any timepoint BOTH tracks exist (µm); `NaN` when they
                 never overlap in time, exactly as celltrackR yields `NA`
  • `n_shared` — how many timepoints they share, so a `distance` from a single frame can be told
                 apart from one backed by a whole track

Centroids must already be in µm (`scale_centroids!`). O(tracks²) in the worst case, which is why
`find_track_issues` only runs it when asked.
"""
function analyze_cell_pairs(df::DataFrame, spatial::Vector{String})::DataFrame
    paths = _track_paths(df, spatial)
    ids   = sort!(collect(keys(paths)))
    t1, t2, ang, dst, nsh = Int[], Int[], Float64[], Float64[], Int[]

    # positions keyed by timepoint, per track — so the shared-time scan is a dict lookup
    at_t = Dict(tid => Dict(round(Int, t) => c for (t, c) in p) for (tid, p) in paths)

    for i in 1:length(ids), j in (i + 1):length(ids)
        a, b = ids[i], ids[j]
        pa, pb = paths[a], paths[b]
        disp_a = last(pa)[2] .- first(pa)[2]
        disp_b = last(pb)[2] .- first(pb)[2]

        shared = intersect(keys(at_t[a]), keys(at_t[b]))
        dmin = isempty(shared) ? NaN :
               minimum(_dist(at_t[a][t], at_t[b][t]) for t in shared)

        push!(t1, a); push!(t2, b)
        push!(ang, _vec_angle_deg(disp_a, disp_b))
        push!(dst, dmin); push!(nsh, length(shared))
    end
    DataFrame(track1 = t1, track2 = t2, angle = ang, distance = dst, n_shared = nsh)
end

# celltrackR QC.Rmd §3.1's own thresholds. The distance one is documented there as "this should be
# the expected cell radius" — so it is genuinely a property of the specimen, not a tuning knob, and
# it is the one threshold in this file that SHOULD be an absolute µm rather than step-relative.
const TRACK_DUP_ANGLE_DEG = 5.0
const TRACK_DUP_DIST_UM   = 10.0
# …and a pair has to actually coexist for long enough that "they are the same cell" means something.
const TRACK_DUP_MIN_SHARED = 3

"""
    find_duplicate_tracks(df, spatial; angle_deg, dist_um, min_shared) -> Vector{TrackIssue}

Pairs of tracks that look like ONE cell segmented twice — small angle between their directions and
never far apart (celltrackR QC.Rmd §3.1). The suggested op removes the SECOND (higher-id) track,
which is the same choice the vignette makes when it drops the duplicate it planted.

Separate from `find_track_issues` because it is O(tracks²) while the rest is linear, and because it
answers a different question — the other signatures look at a track on its own, this one only ever
sees a relationship. Call it when the user asks for a deeper scan.
"""
function find_duplicate_tracks(df::DataFrame, spatial::Vector{String};
                               angle_deg::Real  = TRACK_DUP_ANGLE_DEG,
                               dist_um::Real    = TRACK_DUP_DIST_UM,
                               min_shared::Integer = TRACK_DUP_MIN_SHARED)::Vector{TrackIssue}
    isempty(spatial) && return TrackIssue[]
    pairs = analyze_cell_pairs(df, spatial)
    paths = _track_paths(df, spatial)
    out = TrackIssue[]
    for r in eachrow(pairs)
        (isnan(r.angle) || isnan(r.distance)) && continue
        (r.angle <= angle_deg && r.distance <= dist_um && r.n_shared >= min_shared) || continue
        keep, drop = minmax(r.track1, r.track2)
        p = paths[drop]
        push!(out, TrackIssue("duplicate",
            Dict{String,Any}("op" => "track.remove", "trackIds" => [drop]),
            [keep, drop], first(p)[1], first(p)[2],
            # closer and more parallel = more certainly the same cell
            (dist_um - r.distance) / dist_um + (angle_deg - r.angle) / angle_deg,
            "Tracks $keep and $drop move together — $(round(r.angle; digits = 1))° apart in " *
            "direction and never more than $(round(r.distance; digits = 1)) µm apart over " *
            "$(r.n_shared) frames. Likely one cell tracked twice: check, then remove $drop."))
    end
    sort!(out; by = i -> -i.severity)
    out
end

"""
    track_pair_drift(pairs; far_quantile) -> NamedTuple

Whole-image drift verdict from `analyze_cell_pairs` output (celltrackR QC.Rmd §2.3). With no global
directionality the angle between two cells' paths averages ~90° however far apart they are; a mean
well below 90° for the FAR pairs means the whole field is moving together.

Returns `(mean_angle_far, n_far, drifting)`. Only the far pairs are judged: nearby cells can move
alike for real biological reasons (a swarm, a follicle), so they cannot separate drift from
coordination — which is precisely why the vignette reads the plot at large distances.

Advisory only. The fix is `cleanupImages.driftCorrect` on the image, not a track edit, so this never
produces a `TrackIssue`.
"""
function track_pair_drift(pairs::DataFrame; far_quantile::Real = 0.75,
                          angle_thresh::Real = 80.0)
    ok = [(a, d) for (a, d) in zip(pairs.angle, pairs.distance) if !isnan(a) && !isnan(d)]
    isempty(ok) && return (mean_angle_far = NaN, n_far = 0, drifting = false)
    dists = [d for (_, d) in ok]
    cut = quantile(dists, clamp(Float64(far_quantile), 0.0, 1.0))
    far = [a for (a, d) in ok if d >= cut]
    isempty(far) && return (mean_angle_far = NaN, n_far = 0, drifting = false)
    m = mean(far)
    (mean_angle_far = m, n_far = length(far), drifting = m < angle_thresh)
end
