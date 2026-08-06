struct CellposeSegment <: CciaTask end

# Cellpose model options are enumerated at runtime — the four built-ins plus any file dropped
# into `<install>/models/cellposeModels/` (bundled, populated by install.sh / `pixi run
# models-fetch`) or `<config_dir>/models/cellposeModels/` (user drop-in slot, mirrors the
# custom-modules convention). A newly-added checkpoint appears in the picker AND passes
# `validate_params` without a server restart. See docs/SEGMENTATION.md → *Custom cellpose
# checkpoints*, and `list_cellpose_models` in `app/src/config.jl`.
_needs_dynamic_options(::CellposeSegment) = true

function _inject_dynamic_options!(spec::Dict{String,Any}, ::CellposeSegment)::Dict{String,Any}
    params = get(spec, "params", nothing)
    params isa AbstractVector || return spec
    for p in params
        p isa AbstractDict && string(get(p, "key", "")) == "models" || continue
        sub_params = get(p, "params", nothing)
        sub_params isa AbstractVector || continue
        for sub in sub_params
            sub isa AbstractDict || continue
            (string(get(sub, "key", "")) == "model" &&
             string(get(sub, "type", "")) == "select") || continue
            sub["options"] = [Dict{String,Any}("label" => m.label, "value" => m.name)
                              for m in list_cellpose_models()]
        end
    end
    spec
end

# Cellpose streams into label stores created at full shape up front (SegmentationUtils), so a run can
# be watched in napari before it finishes. One line, because nothing about that is cellpose-specific —
# see `segment_live_outputs` in segmentation.jl and `live_outputs` in task.jl.
live_outputs(::CellposeSegment, params::AbstractDict) = segment_live_outputs(params)

# The task preview runs this task's own compute over the visible region — the worker calls
# `CellposeUtils.predict_slice`, the same method the full run uses. See `task_previewable` in task.jl.
task_previewable(::CellposeSegment) = true

# Cellpose's built-in model names. Anything outside this set is treated as a *custom* checkpoint name
# and resolved via `cellpose_model_path` into a file path the Python runner loads with
# `CellposeModel(pretrained_model=<path>)` — see `cellpose_utils.py::_get_model` (its
# `os.path.isfile(model_type)` branch is the pickup point). See docs/SEGMENTATION.md → *Custom cellpose checkpoints*.
const BUILTIN_CELLPOSE_MODELS = ("cyto3", "cyto2", "cyto", "nuclei")

"""
    cellpose_models_for_python(params, raw; on_log) -> Dict

The `models` bag as the PYTHON side needs it: channel names resolved to 0-based indices, and a custom
model name resolved to its checkpoint path. Raises `ErrorException` with a user-facing message when a
custom checkpoint is missing — cellpose would otherwise fail deep inside the runner with a far less
useful one.

**Shared by the run and the task preview, and it has to be.** The frontend sends channel NAMES
(`"CH3"`) and a bare model name; Python expects indices and a path. This translation used to live inline
in `_run_task`, so the preview — which sends the frontend's params straight to the worker — hit
`ValueError: invalid literal for int() with base 10: 'CH3'`, and a custom model would have failed the
same way one step later. "The preview calls the same `predict_slice`" is only true of the compute; the
params reaching it are the task's to prepare, so preparing them is a task-level hook
(`preview_params`), not something the worker or the API can reasonably guess at.

`raw` is the image's ccid dict. Channel names come from the **default** version deliberately: a
corrected variant inherits them by versioned fallback and may carry no list of its own.
"""
function cellpose_models_for_python(params::AbstractDict, raw::AbstractDict;
                                    on_log::Function = _ -> nothing)::Dict{String,Any}
    ch_names = ccid_channel_names(raw)

    models_json = get(params, "models", nothing)
    out = Dict{String,Any}()
    isnothing(models_json) && return out

    for (k, v) in models_json
        m = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
        for field in ("cellChannels", "nucChannels")
            # one resolver (model/image.jl): already-resolved indices pass through, so a REPL/test
            # caller or a re-translated chain dict is idempotent; an unmatched name raises
            m[field] = channel_indices(get(m, field, []), ch_names; what = field)
        end
        model_name = String(get(m, "model", ""))
        if !isempty(model_name) && !(model_name in BUILTIN_CELLPOSE_MODELS) && !isfile(model_name)
            path = cellpose_model_path(model_name)
            isnothing(path) && error(
                "Custom cellpose model '$model_name' not found at " *
                "$(joinpath(cellpose_models_dir(), model_name)). Place the checkpoint there or " *
                "select a built-in model.")
            on_log("[INFO] Custom model: $model_name → $path")
            m["model"] = path
        end
        out[String(k)] = m
    end
    out
end

# The preview sends the FRONTEND's params, so they need the same preparation the run does before
# Python sees them (see `cellpose_models_for_python`). Without this the worker gets channel names where
# it expects indices.
function preview_params(::CellposeSegment, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    out = Dict{String,Any}(String(k) => v for (k, v) in params)
    out["models"] = cellpose_models_for_python(params, read_ccid_raw(state_file(img)))
    out
end

function _run_task(task::CellposeSegment, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name     = string(get(params, "valueName",     VERSIONED_DEFAULT_VAL))
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    # Resolve input image path
    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir = dirname(dirname(img._dir))
    im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
    task_dir = img._dir

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    models_converted = try
        cellpose_models_for_python(params, raw; on_log = on_log)
    catch e
        on_log("[ERROR] $(e isa ErrorException ? e.msg : sprint(showerror, e))")
        return nothing
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labels", out_value_name)).zarr")
    on_log("[INFO] Models: $(length(models_converted))")

    qc_out_path = joinpath(task_run_dir(task_dir), "segment_counts.json")

    ok = run_py("tasks/segment/cellpose_run.py",
        (; imPath              = im_path,
           taskDir             = task_dir,
           outputValueName     = out_value_name,
           qcOutPath           = qc_out_path,
           models              = models_converted,
           blockSize           = Int(get(params, "blockSize", 512)),
           overlap             = Int(get(params, "overlap", 64)),
           blockSizeZ          = Int(get(params, "blockSizeZ", 0)),
           overlapZ            = Int(get(params, "overlapZ", 0)),
           labelOverlap        = Float64(get(params, "labelOverlap", 0.0)),
           matchThreshold      = Float64(get(params, "matchThreshold", 0.3)),
           removeUnmatched     = Bool(get(params, "removeUnmatched", false)),
           minCellSize         = Int(get(params, "minCellSize", 0)),
           cellSizeMax         = Int(get(params, "cellSizeMax", 0)),
           labelSmoothing      = Float64(get(params, "labelSmoothing", 0.0)),
           labelExpansion      = Int(get(params, "labelExpansion", 0)),
           labelErosion        = Int(get(params, "labelErosion", 0)),
           clearTouchingBorder = Bool(get(params, "clearTouchingBorder", false)),
           clearDepth          = Bool(get(params, "clearDepth", false)),
           normaliseToWhole    = Bool(get(params, "normaliseToWhole", true))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Segmentation complete.")

    # The zarr filenames the Python code will have written — the same derivation the live-preview
    # declaration uses, so the two can't disagree about what this run produces — then register them
    # in ccid.json, which is what makes the set appear in every `labels` picker (segmentation.jl).
    # The atomic write this block used to do inline now lives in `register_label_files!`.
    label_files = segment_label_files(out_value_name, models_converted)
    register_label_files!(img, out_value_name, label_files)

    # QC (advisory): bank the objective per-type cell count the Python runner wrote (drift pattern).
    if isfile(qc_out_path)
        try
            qmeta  = JSON3.read(read(qc_out_path, String))
            counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
            findings, primary = segment_qc_findings(counts)
            write_qc(img, "segment.cellpose", out_value_name, findings;
                     metrics = Dict{String,Any}("nCells" => primary, "byType" => counts))
            on_log("[QC] segmented $primary cell(s)" *
                   (length(counts) > 1 ? " ($(join(["$k=$v" for (k, v) in counts], ", ")))" : "") * ".")
        catch e
            on_log("[QC] could not compute segment QC: $e")
        end
    end

    Dict{String,Any}("outputValueName"  => out_value_name,
                     "labelValueName"   => out_value_name,
                     "labelFiles"       => label_files)
end
