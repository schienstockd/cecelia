struct CoastalSegment <: CciaTask end

# Coastal ships no built-in models — the picker IS the vault. An empty vault therefore means an
# empty dropdown, deliberately: there is nothing sensible to segment with until the user trains a
# model on the Optical Flow page. Same runtime-enumeration hook as cellpose, so a model appears
# without a server restart. See `list_coastal_models` in `app/src/config.jl`.
_needs_dynamic_options(::CoastalSegment) = true

function _inject_dynamic_options!(spec::Dict{String,Any}, ::CoastalSegment)::Dict{String,Any}
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
            # "None" stays first and stays selectable. It is what the picker shows on a fresh
            # install — the vault is empty until the user trains something — and keeping it a real
            # option means the empty state is a legible choice rather than a select that rejects
            # everything including its own default. Running with it selected fails with an
            # actionable message from `coastal_models_for_python`, not a validator's "not a valid
            # option. Valid: ".
            sub["options"] = vcat(
                [Dict{String,Any}("label" => "None", "value" => "")],
                [Dict{String,Any}("label" => m.label, "value" => m.name)
                 for m in list_coastal_models()])
        end
    end
    spec
end

# Streams into label stores created at full shape up front, like every other segmenter.
live_outputs(::CoastalSegment, params::AbstractDict) = segment_live_outputs(params)

# The preview runs this task's own compute over the visible region — the worker calls
# `CoastalUtils.predict_slice`, the same method the full run uses.
task_previewable(::CoastalSegment) = true

"""
    coastal_models_for_python(params, raw; on_log) -> Dict

The `models` bag as Python needs it: channel names resolved to 0-based indices and the model NAME
resolved to its checkpoint path in the vault.

Same shape and same reason as [`cellpose_models_for_python`](@ref) — the frontend sends channel
names and a bare model name, Python expects indices and a path — so the preview and the run share
one translation instead of the preview hitting `int('CH3')` deep inside the runner.

The missing-model error is deliberately loud. A config-dir model does NOT travel with a `.ccbundle`
export, so a project shared from another machine will name a model the recipient does not have;
falling back to *some* model would produce a plausible wrong segmentation.
"""
function coastal_models_for_python(params::AbstractDict, raw::AbstractDict;
                                   on_log::Function = _ -> nothing)::Dict{String,Any}
    ch_names = ccid_channel_names(raw)

    models_json = get(params, "models", nothing)
    out = Dict{String,Any}()
    isnothing(models_json) && return out

    for (k, v) in models_json
        m = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
        # one resolver (model/image.jl): already-resolved indices pass through, so a REPL/test
        # caller or a re-translated chain dict is idempotent; an unmatched name raises
        m["cellChannels"] = channel_indices(get(m, "cellChannels", []), ch_names;
                                            what = "cellChannels")

        model_name = String(get(m, "model", ""))
        isempty(strip(model_name)) && error(
            "No optical-flow model selected. Train one on the Optical Flow page first.")
        if !isfile(model_name)
            path = coastal_model_path(model_name)
            isnothing(path) && error(
                "Optical-flow model '$model_name' not found in $(coastal_models_dir()). " *
                "Models are stored per machine and are not included in a project export — " *
                "train or copy it in before running this task.")
            manifest = coastal_model_manifest(model_name)
            if isempty(manifest)
                # Not fatal, but it means the metric set is a guess (coastal's training defaults),
                # and a wrong metric set fails SILENTLY — channels shift, nothing raises.
                on_log("[WARN] $model_name has no manifest; assuming coastal's default flow " *
                       "metrics. Re-train it to record what it was trained on.")
            else
                on_log("[INFO] Model: $model_name → $path " *
                       "(scales $(get(manifest, "temporalScales", "default")), " *
                       "channel $(get(manifest, "channelName", "?")))")
            end
            m["model"] = path
        end
        out[String(k)] = m
    end
    out
end

# Same reason as cellpose: the preview sends the FRONTEND's params, which need the run's preparation
# before Python sees them.
function preview_params(::CoastalSegment, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    out = Dict{String,Any}(String(k) => v for (k, v) in params)
    out["models"] = coastal_models_for_python(params, read_ccid_raw(state_file(img)))
    out
end

function _run_task(task::CoastalSegment, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name     = string(get(params, "valueName",     VERSIONED_DEFAULT_VAL))
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

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
        coastal_models_for_python(params, raw; on_log = on_log)
    catch e
        on_log("[ERROR] $(e isa ErrorException ? e.msg : sprint(showerror, e))")
        return nothing
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labels", out_value_name)).zarr")
    on_log("[INFO] Models: $(length(models_converted))")

    qc_out_path = joinpath(task_run_dir(task_dir), "segment_counts.json")

    ok = run_py("tasks/segment/coastal_run.py",
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
           minCellSize         = Float64(get(params, "minCellSize", 0.0)),
           cellSizeMax         = Float64(get(params, "cellSizeMax", 0.0)),
           labelSmoothing      = Float64(get(params, "labelSmoothing", 0.0)),
           labelExpansion      = Float64(get(params, "labelExpansion", 0.0)),
           labelErosion        = Float64(get(params, "labelErosion", 0.0)),
           clearTouchingBorder = Bool(get(params, "clearTouchingBorder", false)),
           clearDepth          = Bool(get(params, "clearDepth", false)),
           normaliseToWhole    = Bool(get(params, "normaliseToWhole", true))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Segmentation complete.")

    label_files = segment_label_files(out_value_name, models_converted)
    register_label_files!(img, out_value_name, label_files)

    # QC (advisory): the same objective per-type cell count every segmenter banks.
    if isfile(qc_out_path)
        try
            qmeta  = JSON3.read(read(qc_out_path, String))
            counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
            findings, primary = segment_qc_findings(counts)
            write_qc(img, "segment.coastal", out_value_name, findings;
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
