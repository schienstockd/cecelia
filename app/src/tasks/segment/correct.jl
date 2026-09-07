# correct.jl — manual label correction (docs/todo/CORRECTION_PLAN.md, P2)
#
# Rewrites the labels store `<vn>.zarr` per an op list — `label.merge` folds ids, `label.remove`
# zeros them, both frame-local (Decision 6b). The engine (`app/src/label_correction.jl`) is pure and
# unit-tested; this file is the task shell around it (params, IO, journal, QC) plus the delegation
# to Python for the actual array mutation and staged-store write.
#
# WHY A TASK AND NOT A BUTTON. Same as tracking.correct (Decision 1): a correction that lives only
# in a Vue click cannot be replayed, logged or audited. As a task it gets the scheduler's log file,
# resource pool and QC banking, and runs identically from the REPL:
#
#     run_task(SegmentCorrect(), img, Dict{String,Any}(
#         "valueName" => "memTom",
#         "labelOps"  => [Dict("op" => "label.merge", "t" => 0,
#                              "ids" => [3, 5], "into" => 3)]))
#
# WHY PYTHON DOES THE WRITE. The labels store is a zarr, its writer path is
# `zarr_utils.staged_store` + `store_compressor('labels')` + `create_multiscales`, and per-frame
# numpy assignment is the natural mutation. Julia stays with the ops, journal and QC; the runner
# receives the pre-folded rewrite table (`build_rewrite`) so its own per-t loop is one array pass
# per touched frame.
#
# WHAT THIS DOES NOT DO. It does NOT recompute measures. `segment.measureLabels` does, and the
# composite `segment.correct_measures` chains them + carries obs across the re-measure
# (Decision 4b). Running this task alone leaves the labelProps h5ad describing the PRE-correction
# row set — surfaced with a warn line so the user sees the staleness, since not everyone will run
# the composite from the REPL.

using JSON3

struct SegmentCorrect <: CciaTask end

"""
    parse_label_ops(value) -> Vector{Dict{String,Any}}

Normalise the `labelOps` param into a list of op dicts, from either a Vector (REPL/API/chain) or a
JSON string (the form). Throws `ParamValidationError` on any malformed entry — an unknown op kind,
missing field, wrong types — same reasoning as `parse_track_ops`: failing here costs nothing while
the alternative is a task that opens the labels store before discovering the ops are nonsense.

**Empty is legal and means "no correction".** The package suite requires every task's own spec
defaults to validate, so `""` / `nothing` / `[]` all parse to no ops and `_run_task` reports that
and writes nothing.
"""
function parse_label_ops(value)::Vector{Dict{String,Any}}
    raw = value
    isnothing(raw) && return Dict{String,Any}[]
    if raw isa AbstractString
        s = strip(String(raw))
        isempty(s) && return Dict{String,Any}[]
        raw = try
            JSON3.read(s, Vector{Dict{String,Any}})
        catch e
            throw(ParamValidationError("'labelOps' is not a JSON array of ops: $e"))
        end
    end
    raw isa AbstractVector ||
        throw(ParamValidationError("'labelOps' must be a list of ops, got: $(typeof(raw))"))

    ops = Dict{String,Any}[]
    for (i, o) in enumerate(raw)
        o isa AbstractDict ||
            throw(ParamValidationError("'labelOps'[$i] must be an object, got: $(typeof(o))"))
        d = Dict{String,Any}(string(k) => v for (k, v) in pairs(o))
        try
            validate_label_op(d)
        catch e
            msg = e isa ArgumentError ? e.msg : sprint(showerror, e)
            throw(ParamValidationError("'labelOps'[$i]: $msg"))
        end
        push!(ops, d)
    end
    ops
end

# Spec validation + the ops check — a malformed op is a ParamValidationError at submit time, not a
# stack trace mid-run. `kwargs...` accept-and-forward for the same reason as tracking.correct
# (chain/composite paths pass keywords; a keyword-less method silently falls through to the
# ::CciaTask fallback without running our own validation).
function validate_params(task::SegmentCorrect, params::Dict{String,Any}; kwargs...)
    invoke(validate_params, Tuple{CciaTask, Dict{String,Any}}, task, params; kwargs...)
    parse_label_ops(get(params, "labelOps", nothing))
    nothing
end

function _run_task(task::SegmentCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    task_dir   = img._dir
    ccid       = state_file(task_dir)
    raw        = read_ccid_raw(ccid)

    ops = parse_label_ops(get(params, "labelOps", nothing))
    if isempty(ops)
        on_log("[INFO] No correction ops — nothing to do.")
        return Dict{String,Any}("valueName" => value_name, "nOps" => 0)
    end
    on_log("[INFO] $(length(ops)) label correction op(s) on $value_name")
    on_progress(1, 5)

    # Resolve the labels zarr — same lookup measure_labels uses. Only the PRIMARY `<vn>.zarr` gets
    # rewritten; sibling stores (`<vn>_nuc.zarr`, `<vn>_cyto.zarr` from `matchAs` segmentations) are
    # left alone. That's deliberate: an op that says "merge id 3 into id 2" identifies cells by the
    # primary labels — a matched-secondary carries the SAME id space by construction, so the runner
    # can propagate the rewrite to siblings uniformly if needed. Not built here; when the first
    # matched-store use case appears we add a `matchStores` param, not a second engine.
    labels_dict_raw = get(raw, "labels", Dict{String,Any}())
    label_entry = get(labels_dict_raw, value_name,
                      get(labels_dict_raw, Symbol(value_name), nothing))
    if isnothing(label_entry)
        on_log("[ERROR] No labels registered for valueName='$value_name'")
        return nothing
    end
    label_files = label_entry isa AbstractVector ?
                  collect(String, label_entry) : [string(label_entry)]
    isempty(label_files) && begin
        on_log("[ERROR] labels entry for '$value_name' is empty")
        return nothing
    end
    labels_dir  = joinpath(task_dir, "labels")
    labels_path = joinpath(labels_dir, first(label_files))
    ispath(labels_path) ||
        (on_log("[ERROR] Labels store not found: $labels_path"); return nothing)

    # The labels store has no OME-XML of its own; the Python runner reads dims from the intensity
    # image (same rule measure_labels_run.py follows). Resolve the active image path here so the
    # runner is thin — it just uses what Julia hands it.
    im_filename = versioned_get_field(raw, "filepath", VERSIONED_DEFAULT_VAL)
    if isnothing(im_filename)
        on_log("[ERROR] No image filepath registered — cannot derive dims for labels correction")
        return nothing
    end
    proj_dir = dirname(dirname(task_dir))
    im_path  = joinpath(proj_dir, "0", img.uid, string(im_filename))
    ispath(im_path) ||
        (on_log("[ERROR] Image not found: $im_path"); return nothing)

    on_log("[INFO] Labels: $labels_path")
    on_log("[INFO] Image:  $im_path")
    on_progress(2, 5)

    on_progress(3, 5)

    # ── Run the writer ─────────────────────────────────────────────────────────
    #
    # Python opens the labels zarr, applies ops in order per touched frame (op-by-op, not folded, so
    # per-op pixel counts are the pixels THAT OP saw when it fired — a merge queued after another
    # merge that already touched those pixels reports the pixels it moved, not the historical ones),
    # stages the result via `zarr_utils.staged_store` + `store_compressor('labels')`, swaps
    # atomically at the end, and writes `<taskDir>/labels_correction_result.json` — a
    # `{perOpPixels: [ints], nLabelsBefore: int, nLabelsAfter: int}` blob we read back for QC.
    result_file = joinpath(task_run_dir(task_dir), "labels_correction_result.json")
    isfile(result_file) && rm(result_file; force = true)

    ok = run_py("tasks/segment/correct_run.py",
        (; taskDir     = task_dir,
           imPath      = im_path,
           labelsPath  = labels_path,
           valueName   = value_name,
           ops         = ops,
           resultFile  = result_file),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing
    on_log("[INFO] Corrected labels written.")
    on_progress(4, 5)

    # ── Journal (Decision 3/7) ────────────────────────────────────────────────
    #
    # Read the runner's result blob for the per-op pixel counts + label counts. Journal every op
    # that ran — even if the pixel count is 0 (the id wasn't in the frame), because a "no-op" for
    # an id the user believed was there is itself an interesting audit trail.
    per_op_pixels = Int[]
    n_before, n_after = 0, 0
    try
        if isfile(result_file)
            r = JSON3.read(read(result_file, String), Dict{String,Any})
            per_op_pixels = [Int(x) for x in get(r, "perOpPixels", Int[])]
            n_before = Int(get(r, "nLabelsBefore", 0))
            n_after  = Int(get(r, "nLabelsAfter",  0))
        end
    catch e
        on_log("[WARN] could not read the runner's result blob: $e")
    end

    entries = Dict{String,Any}[]
    for (i, op) in enumerate(ops)
        rec = Dict{String,Any}(k => v for (k, v) in op)
        rec["nPixels"] = i <= length(per_op_pixels) ? per_op_pixels[i] : 0
        push!(entries, rec)
    end
    journal = try
        append_label_corrections!(task_dir, value_name, entries)
    catch e
        on_log("[WARN] could not write the label correction journal: $e")
        nothing
    end
    isnothing(journal) || on_log("[INFO] Journalled $(length(entries)) op(s) → $journal")

    # ── QC (Decision 8) ───────────────────────────────────────────────────────
    metrics = label_correction_metrics(ops, per_op_pixels;
                                       n_labels_before = n_before, n_labels_after = n_after)
    try
        write_qc(img, "segment.correct", value_name,
                 label_correction_qc_findings(metrics); metrics = metrics)
        on_log("[QC] $(metrics["nLabelsRemoved"]) label(s) removed across " *
               "$(metrics["nFramesTouched"]) frame(s); $(metrics["nPixelsRewritten"]) pixel(s) rewritten.")
    catch e
        on_log("[QC] could not compute correction QC: $e")
    end

    # The h5ad still describes the PRE-correction row set — call this out plainly. The composite
    # (segment.correct_measures) chains a re-measure + obs carry-over. Running this task alone is a
    # deliberate REPL escape hatch, not the intended workflow; the warn line + the QC finding are
    # what makes the staleness visible if someone does it anyway.
    on_log("[WARN] labelProps h5ad still describes the pre-correction labels — run " *
           "segment.correct_measures (composite) to re-measure with obs carry-over, or " *
           "segment.measureLabels manually.")
    on_progress(5, 5)

    Dict{String,Any}("valueName" => value_name,
                     "nOps"      => length(ops),
                     "metrics"   => metrics)
end
