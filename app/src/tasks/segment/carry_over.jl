# carry_over.jl — obs carry-over across `segment.measureLabels` in the correction composite
# (docs/todo/CORRECTION_PLAN.md, P2 Decision 4b).
#
# WHY THIS TASK EXISTS. `segment.measureLabels` builds a FRESH AnnData (measure_labels_run.py:104)
# and writes it via `write_h5ad_atomic` — a total obs replace. Every prior obs column drops:
# `track_id/parent/root/state/generation` (tracking), `live.cell.*`/`live.track.*` (per-cell
# streamed measures), HMM states, cluster ids, gating pops. Running the composite
# `segment.correct_measures` after a hand-correction would therefore erase the user's downstream
# work for the whole image, not just the touched rows — a bigger regression than the plan doc's
# "live.* drops" wording implied.
#
# HOW. Two tasks, both invoked by the composite executor (which threads the same params dict
# through unchanged, so per-step params aren't a lever — two typed structs is the shape that fits):
#
#   1. `segment.correct.carryOver.snapshot` runs BEFORE `segment.correct` + `segment.measureLabels`
#      and writes a JSON blob under the task's run dir with every non-measurement obs column, keyed
#      by label id.
#   2. `segment.correct.carryOver.restore` runs AFTER `segment.measureLabels` and adds those obs
#      columns back — filtered to columns NOT present in the fresh h5ad, so measureLabels' own
#      morphology always wins (no stale `mean_intensity_*` overrides a fresh one).
#
# WHAT MERGES INHERIT. Nothing special here — `LabelPropsView.add_obs` aligns by label, so a merged
# cell (the surviving `into` id) receives its OWN pre-op obs. Sacrificed ids are gone from the fresh
# h5ad and silently skipped. That matches the pixel semantics of `label.merge` (`src → into`, `into`
# survives) and the codebase's stance on not inventing collapsed numbers (see the drop-rather-than-
# average precedent in `tracking_utils.py:406`). Note the trade-off: a merged cell's carried
# `live.cell.speed` reflects its PRE-merge trajectory — stale, though preserved. Reporting that
# staleness is P3 invalidation-surface territory, not carry-over's.
#
# WHY THE SNAPSHOT ISN'T A FULL H5AD COPY. `write_json_atomic` is the mandated writer for durable
# structured state; a bytes-for-bytes h5ad copy would pull in `X` (the feature matrix) unnecessarily
# and require a direct `ad.read_h5ad` at restore time. JSON keeps obs-only, uses the sanctioned
# atomic helper, and stays inspectable when the composite goes sideways.

using JSON3

struct SegmentCorrectCarryOverSnapshot <: CciaTask end
struct SegmentCorrectCarryOverRestore  <: CciaTask end

# ── shared helpers ─────────────────────────────────────────────────────────────

# Path convention mirrors `label_props_utils.LabelPropsUtils.label_props_filepath` — one home for
# the h5ad location, but the Python side is what actually reads it. This is only used to short-
# circuit before spawning Python when the source h5ad is absent (a first-time image would trip that
# and the snapshot would still run — which is fine, it writes an empty payload; but skipping the
# subprocess spawn saves the spool and log-file overhead).
_labelprops_path(task_dir::AbstractString, value_name::AbstractString) =
    joinpath(task_dir, "labelProps", "$(value_name).h5ad")

# Both phases park their state under the run dir so a chain resume finds it in the expected place.
# One file per run; the composite runs both phases inside the same run, so the snapshot written by
# phase 1 is what phase 4 reads.
_carryover_snapshot_path(task_dir::AbstractString) =
    joinpath(task_run_dir(task_dir), "correct_carryover_snapshot.json")

_carryover_result_path(task_dir::AbstractString, phase::AbstractString) =
    joinpath(task_run_dir(task_dir), "correct_carryover_$(phase)_result.json")

# ── snapshot ───────────────────────────────────────────────────────────────────

function _run_task(task::SegmentCorrectCarryOverSnapshot, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    task_dir   = img._dir

    labelprops = _labelprops_path(task_dir, value_name)
    snap_path  = _carryover_snapshot_path(task_dir)
    result_file = _carryover_result_path(task_dir, "snapshot")
    isfile(snap_path)   && rm(snap_path;   force = true)
    isfile(result_file) && rm(result_file; force = true)

    # A first-time image has no labelProps yet — the composite must still work (someone might chain
    # `segment.correct_measures` against a freshly-segmented image before running measureLabels
    # standalone). Skip the Python spawn and leave the snapshot absent; restore then no-ops.
    if !isfile(labelprops)
        on_log("[INFO] No labelProps h5ad at $labelprops — nothing to snapshot.")
        return Dict{String,Any}("valueName" => value_name, "nRowsSnapshotted" => 0,
                                "snapshotPath" => nothing)
    end

    on_log("[INFO] Snapshotting obs for $value_name")
    on_progress(1, 2)

    ok = run_py("tasks/segment/carry_over_run.py",
        (; phase       = "snapshot",
           taskDir     = task_dir,
           valueName   = value_name,
           snapshotFile = snap_path,
           resultFile  = result_file),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing
    on_progress(2, 2)

    n_rows, n_cols, n_cat = 0, 0, 0
    try
        if isfile(result_file)
            r = JSON3.read(read(result_file, String), Dict{String,Any})
            n_rows = Int(get(r, "nRowsSnapshotted", 0))
            n_cols = Int(get(r, "nNumericCols", 0))
            n_cat  = Int(get(r, "nCategoricalCols", 0))
        end
    catch e
        on_log("[WARN] could not read snapshot result blob: $e")
    end

    on_log("[INFO] Snapshotted $(n_rows) row(s), $(n_cols + n_cat) column(s) " *
           "($(n_cols) numeric + $(n_cat) categorical).")
    Dict{String,Any}("valueName"        => value_name,
                     "nRowsSnapshotted" => n_rows,
                     "nColsSnapshotted" => n_cols + n_cat,
                     "snapshotPath"     => snap_path)
end

# ── restore ────────────────────────────────────────────────────────────────────

function _run_task(task::SegmentCorrectCarryOverRestore, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    task_dir   = img._dir

    snap_path  = _carryover_snapshot_path(task_dir)
    result_file = _carryover_result_path(task_dir, "restore")
    isfile(result_file) && rm(result_file; force = true)

    if !isfile(snap_path)
        on_log("[INFO] No snapshot at $snap_path — restore is a no-op (snapshot phase skipped or first run).")
        return Dict{String,Any}("valueName" => value_name, "nRowsCarried" => 0, "nColsCarried" => 0)
    end

    labelprops = _labelprops_path(task_dir, value_name)
    if !isfile(labelprops)
        on_log("[WARN] Snapshot exists but labelProps h5ad missing at $labelprops — nothing to restore into.")
        return Dict{String,Any}("valueName" => value_name, "nRowsCarried" => 0, "nColsCarried" => 0)
    end

    on_log("[INFO] Restoring carried obs onto $value_name")
    on_progress(1, 2)

    ok = run_py("tasks/segment/carry_over_run.py",
        (; phase       = "restore",
           taskDir     = task_dir,
           valueName   = value_name,
           snapshotFile = snap_path,
           resultFile  = result_file),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing
    on_progress(2, 2)

    n_rows, n_cols_carried, n_cols_skipped = 0, 0, 0
    try
        if isfile(result_file)
            r = JSON3.read(read(result_file, String), Dict{String,Any})
            n_rows         = Int(get(r, "nRowsCarried",   0))
            n_cols_carried = Int(get(r, "nColsCarried",   0))
            n_cols_skipped = Int(get(r, "nColsSkipped",   0))
        end
    catch e
        on_log("[WARN] could not read restore result blob: $e")
    end

    if n_cols_carried == 0
        on_log("[INFO] Nothing to carry (snapshot had no non-measurement obs, or every column " *
               "was regenerated by measureLabels).")
    else
        on_log("[INFO] Carried $(n_cols_carried) column(s) onto $(n_rows) row(s); " *
               "skipped $(n_cols_skipped) column(s) present in the fresh h5ad.")
    end

    # Snapshot file has done its job — clean up so a subsequent standalone run of restore isn't
    # tempted to re-apply stale obs onto a re-measured store.
    try
        rm(snap_path; force = true)
    catch e
        on_log("[WARN] could not remove snapshot file $snap_path: $e")
    end

    Dict{String,Any}("valueName"    => value_name,
                     "nRowsCarried" => n_rows,
                     "nColsCarried" => n_cols_carried)
end
