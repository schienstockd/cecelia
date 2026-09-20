struct CellposeSegment <: CciaTask end

# Typed shape of what `_run_task(::CellposeSegment, …)` reads from `params`. `models` stays a
# bag here (parsed separately by `cellpose_models_for_python`, which needs the image's channel
# names to resolve NAMES → 0-based indices for Python).
Base.@kwdef struct CellposeSegmentParams
    valueName::String            = VERSIONED_DEFAULT_VAL
    outputValueName::String      = VERSIONED_DEFAULT_VAL
    blockSize::Int               = 512
    overlap::Int                 = 64
    blockSizeZ::Int              = 0
    overlapZ::Int                = 0
    labelOverlap::Float64        = 0.0
    matchThreshold::Float64      = 0.3
    removeUnmatched::Bool        = false
    minCellSize::Float64         = 0.0
    cellSizeMax::Float64         = 0.0
    labelSmoothing::Float64      = 0.0
    labelExpansion::Float64      = 0.0
    labelErosion::Float64        = 0.0
    clearTouchingBorder::Bool    = false
    clearDepth::Bool             = false
    normaliseToWhole::Bool       = true
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_cellpose_segment_params(d::AbstractDict)::CellposeSegmentParams
    CellposeSegmentParams(;
        valueName           = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        outputValueName     = string(get(d, "outputValueName", VERSIONED_DEFAULT_VAL)),
        blockSize           = Int(get(d, "blockSize", 512)),
        overlap             = Int(get(d, "overlap", 64)),
        blockSizeZ          = Int(get(d, "blockSizeZ", 0)),
        overlapZ            = Int(get(d, "overlapZ", 0)),
        labelOverlap        = Float64(get(d, "labelOverlap", 0.0)),
        matchThreshold      = Float64(get(d, "matchThreshold", 0.3)),
        removeUnmatched     = Bool(get(d, "removeUnmatched", false)),
        minCellSize         = Float64(get(d, "minCellSize", 0.0)),
        cellSizeMax         = Float64(get(d, "cellSizeMax", 0.0)),
        labelSmoothing      = Float64(get(d, "labelSmoothing", 0.0)),
        labelExpansion      = Float64(get(d, "labelExpansion", 0.0)),
        labelErosion        = Float64(get(d, "labelErosion", 0.0)),
        clearTouchingBorder = Bool(get(d, "clearTouchingBorder", false)),
        clearDepth          = Bool(get(d, "clearDepth", false)),
        normaliseToWhole    = Bool(get(d, "normaliseToWhole", true)),
        version = parse_version_pin(d))
end

# Cellpose model options are enumerated at runtime — the four built-ins plus any file dropped
# into `<install>/models/cellposeModels/` (bundled, populated by install.sh / `pixi run
# models-fetch`) or `<config_dir>/models/cellposeModels/` (user drop-in slot, mirrors the
# custom-modules convention). A newly-added checkpoint appears in the picker AND passes
# `validate_params` without a server restart. See docs/SEGMENTATION.md → *Custom cellpose
# checkpoints*, and `list_cellpose_models` in `app/src/config.jl`.


# Cellpose streams into label stores created at full shape up front (SegmentationUtils), so a run can
# be watched in napari before it finishes. One line, because nothing about that is cellpose-specific —
# see `segment_live_outputs` in segmentation.jl and `live_outputs` in task.jl.
live_outputs(::CellposeSegment, params::AbstractDict) = segment_live_outputs(params)

# The task preview runs this task's own compute over the visible region — the worker calls
# `CellposeUtils.predict_slice`, the same method the full run uses. See `task_previewable` in task.jl.
task_previewable(::CellposeSegment) = true

# Cellpose's built-in model names live in `config.jl` (`BUILTIN_CELLPOSE_MODELS`, one copy —
# `list_cellpose_models` builds the picker from the same tuple). Anything outside that set is treated
# as a *custom* checkpoint name and resolved via `cellpose_model_path` into a file path the Python
# runner loads with `CellposeModel(pretrained_model=<path>)` — see `cellpose_utils.py::_get_model`
# (its `os.path.isfile` branch is the pickup point). See docs/SEGMENTATION.md → *Custom cellpose
# checkpoints*.
_builtin_cellpose_names() = (first(m) for m in BUILTIN_CELLPOSE_MODELS)

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
        # A Cellpose 3 model name is REJECTED, never passed through: cellpose 4 answers an unknown
        # `pretrained_model` with a log warning and `cpsam_v2`, so letting one through would silently
        # return a different segmentation. See RETIRED_CELLPOSE_MODELS in config.jl.
        if model_name in RETIRED_CELLPOSE_MODELS
            error("Cellpose model '$model_name' no longer exists: cellpose 4 replaced the " *
                  "cyto/nuclei zoo with one model. Select '$(first(first(BUILTIN_CELLPOSE_MODELS)))' " *
                  "and re-check the cell diameter. Results will not match the old run.")
        end
        if !isempty(model_name) && !(model_name in _builtin_cellpose_names()) && !isfile(model_name)
            path = cellpose_model_path(model_name)
            isnothing(path) && error(
                "Custom cellpose model '$model_name' not found at " *
                "$(joinpath(cellpose_models_dir(), model_name)). Place the checkpoint there or " *
                "select a built-in model. Note it must be a cellpose 4 checkpoint — a v3 file is " *
                "rejected on load.")
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

    p    = parse_cellpose_segment_params(params)
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    # Resolve input image path
    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = p.version)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
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

    # Pick the interpreter env from the model backend. Mixed v3+v4 in one task is refused up front
    # — one task = one env — because there is no sensible "compose two cellpose versions" story.
    # The v3 env is opt-in and Mac-only; on any other platform a v3 pick fails via run_py's env
    # guard with a message pointing at the install action. See docs/todo/CELLPOSE_V3_OPTIN_PLAN.md.
    backends = unique(cellpose_model_backend(get(m, "model", "")) for (_, m) in models_converted)
    if length(backends) > 1
        on_log("[ERROR] Cannot mix cellpose v3 and v4 models in one segmentation task: $(join(backends, ", ")).")
        return nothing
    end
    py_env = (first(backends) === :v3) ? :cellpose_v3 : nothing

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labels", p.outputValueName)).zarr")
    on_log("[INFO] Models: $(length(models_converted))" *
           (isnothing(py_env) ? "" : "  (cellpose v3 env)"))

    qc_out_path = joinpath(task_run_dir(task_dir), "segment_counts.json")

    ok = run_py("tasks/segment/cellpose_run.py",
        (; imPath              = im_path,
           taskDir             = task_dir,
           outputValueName     = p.outputValueName,
           qcOutPath           = qc_out_path,
           models              = models_converted,
           blockSize           = p.blockSize,
           overlap             = p.overlap,
           blockSizeZ          = p.blockSizeZ,
           overlapZ            = p.overlapZ,
           labelOverlap        = p.labelOverlap,
           matchThreshold      = p.matchThreshold,
           removeUnmatched     = p.removeUnmatched,
           minCellSize         = p.minCellSize,
           cellSizeMax         = p.cellSizeMax,
           labelSmoothing      = p.labelSmoothing,
           labelExpansion      = p.labelExpansion,
           labelErosion        = p.labelErosion,
           clearTouchingBorder = p.clearTouchingBorder,
           clearDepth          = p.clearDepth,
           normaliseToWhole    = p.normaliseToWhole),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process,
        env = py_env)
    ok || return nothing

    on_log("[INFO] Segmentation complete.")

    # The zarr filenames the Python code will have written — the same derivation the live-preview
    # declaration uses, so the two can't disagree about what this run produces — then register them
    # in ccid.json, which is what makes the set appear in every `labels` picker (segmentation.jl).
    # The atomic write this block used to do inline now lives in `register_label_files!`.
    label_files = segment_label_files(p.outputValueName, models_converted)
    register_label_files!(img, p.outputValueName, label_files)

    # QC (advisory): bank the objective per-type cell count the Python runner wrote (drift pattern).
    if isfile(qc_out_path)
        try
            qmeta  = JSON3.read(read(qc_out_path, String))
            counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
            findings, primary = segment_qc_findings(counts)
            write_qc(img, "segment.cellpose", p.outputValueName, findings;
                     metrics = Dict{String,Any}("nCells" => primary, "byType" => counts))
            on_log("[QC] segmented $primary cell(s)" *
                   (length(counts) > 1 ? " ($(join(["$k=$v" for (k, v) in counts], ", ")))" : "") * ".")
        catch e
            on_log("[QC] could not compute segment QC: $e")
        end
    end

    Dict{String,Any}("outputValueName"  => p.outputValueName,
                     "labelValueName"   => p.outputValueName,
                     "labelFiles"       => label_files)
end
