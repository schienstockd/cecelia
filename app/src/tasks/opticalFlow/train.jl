struct TrainFlowModel <: CciaTask end

"""
    parse_temporal_scales(s) -> Vector{Int}

`"1,2,4,8"` → `[1, 2, 4, 8]`, deduped and sorted. Raises on anything that is not a list of positive
integers.

Parsed here rather than in Python because it is the single most consequential parameter of the whole
pipeline and the failure is silent: the scales a model is trained on must be the scales inference
feeds it, and coastal does not check — a mismatched metric set shifts every later channel and
zero-fills the end, so the model reads misaligned inputs and returns a plausible wrong mask. Getting
a typo rejected at the form is the only cheap place to catch it.
"""
function parse_temporal_scales(v::AbstractVector)::Vector{Int}
    # A REPL/chain caller passes the list directly; re-parsing `string([1, 2])` would see "[1".
    isempty(v) && throw(ParamValidationError(
        "'temporalScales' is empty; give frame lags like 1,2,4,8"))
    parse_temporal_scales(join(v, ","))
end

function parse_temporal_scales(s)::Vector{Int}
    parts = filter(!isempty, strip.(split(string(s), r"[,\s]+")))
    isempty(parts) && throw(ParamValidationError(
        "'temporalScales' is empty; give frame lags like 1,2,4,8"))
    out = Int[]
    for p in parts
        v = tryparse(Int, p)
        (isnothing(v) || v < 1) && throw(ParamValidationError(
            "'temporalScales' must be positive whole numbers, got \"$p\""))
        push!(out, v)
    end
    sort!(unique!(out))
end

# Every fixed metric plane coastal computes. The per-scale `mag_{n}` planes are deliberately NOT
# here: they follow `temporalScales`, so offering them as separate ticks would let the two disagree.
const FIXED_FLOW_METRICS = ("acceleration", "cell_boundary_likelihood", "cumulative_mag",
                            "direction_stability", "divergence", "edge_strength",
                            "flow_structure_alignment", "normal_flow", "strain",
                            "tangential_flow", "vorticity")

# Flat, non-flow metric planes measured on intravital data — cell/background ratios 0.99, 1.00 and
# 1.65 (the last is salt-and-pepper across the whole field). They are also the three the original
# rank-AUC table scored at 0.51–0.53. Dropping them takes the model from 16 to 13 input channels.
# This is only the shipped DEFAULT (unticked in the picker), not a rule: the numbers are from ONE
# intravital dataset, and the Flow metrics plot exists so the user can judge their own.
const FLAT_FLOW_METRICS = ("divergence", "vorticity", "flow_structure_alignment")

"""
    flow_dropped_metrics(selected) -> Vector{String}

The fixed metric planes to EXCLUDE, given the ones the user ticked. Recorded in the manifest,
because inference must drop exactly the same ones — `predict_frame` stacks what it is given in
sorted-key order and zero-fills the rest, so a mismatch shifts every later channel silently.

`nothing` means "no picker in this call" (a chain or REPL caller written before it existed) and
keeps the shipped default rather than training on all 11.
"""
function flow_dropped_metrics(selected)::Vector{String}
    isnothing(selected) && return collect(FLAT_FLOW_METRICS)
    keep = Set(String.(selected))
    isempty(keep) && error("Select at least one flow metric to train on.")
    [m for m in FIXED_FLOW_METRICS if !(m in keep)]
end

"""
    flow_model_target(name; overwrite) -> String

Absolute `.pt` path in the vault for a new model, after checking the name is a plain filename and
that nothing is being clobbered. Creates the vault directory.
"""
function flow_model_target(name::AbstractString; overwrite::Bool = false,
                           dev_dir::Union{String,Nothing} = nothing)::String
    stem = strip(String(name))
    isempty(stem) && error("Give the model a name — it is how you will pick it in the segmenter.")
    # A name reaches the filesystem, so it must be a leaf. Not a security boundary (the user owns
    # this machine) — it stops a stray "/" writing outside the vault and vanishing from the picker.
    occursin(r"[/\\]", stem) && error("Model name cannot contain a path separator: '$stem'")
    stem in (".", "..") && error("Model name cannot be '$stem'")
    endswith(stem, ".pt") && (stem = first(splitext(stem)))

    dir = coastal_models_dir(dev_dir)
    mkpath(dir)
    target = joinpath(dir, "$(stem).pt")
    (!overwrite && isfile(target)) && error(
        "A model named '$stem' already exists. Choose another name, or tick Overwrite existing.")
    target
end

# SET scope, not image. A flow model is trained on the images of an experimental SET and then applied
# across it — one model per image would be N models where the point is one, and each would see a
# fraction of the data. Metrics are computed per movie (motion is only meaningful within a movie) and
# the frames are pooled for training; coastal's `prepare_data_for_unet_batch` +
# `train_test_split_per_movie` are exactly that shape.
function _run_task(task::TrainFlowModel, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    isempty(imgs) && (on_log("[ERROR] No images selected to train on."); return nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))

    # Channel names come from the FIRST image and every other image must agree. A set whose images
    # have different channel orders would silently train on a different reporter per movie, which is
    # the kind of thing that produces a model that works on some images and not others with no
    # visible cause.
    ch_names = ccid_channel_names(read_ccid_raw(state_file(imgs[1])))

    local channels, scales, model_path
    try
        channels = channel_indices(get(params, "trainChannels", []), ch_names;
                                   what = "trainChannels")
        isempty(channels) && error("Select at least one channel to train on.")
        scales = parse_temporal_scales(get(params, "temporalScales", "1,2,4,8"))
        model_path = flow_model_target(get(params, "modelName", "");
                                       overwrite = Bool(get(params, "overwrite", false)))
    catch e
        on_log("[ERROR] $(e isa ErrorException || e isa ParamValidationError ? e.msg : sprint(showerror, e))")
        return nothing
    end

    movies = Dict{String,Any}[]
    for img in imgs
        raw = read_ccid_raw(state_file(img))
        filename = versioned_get_field(raw, "filepath", value_name)
        if isnothing(filename)
            on_log("[WARN] $(img.uid): no filepath for valueName='$value_name' — skipped")
            continue
        end
        im_path = joinpath(dirname(dirname(img._dir)), "0", img.uid, string(filename))
        if !ispath(im_path)
            on_log("[WARN] $(img.uid): image not found, skipped — $im_path")
            continue
        end
        names_here = ccid_channel_names(raw)
        if names_here != ch_names
            # Loud, and skipped rather than trained on. Resolving the names per image would train on
            # whatever sits at that index, which is worse than leaving the movie out.
            on_log("[WARN] $(img.uid): channel names differ from $(imgs[1].uid) — skipped")
            continue
        end
        push!(movies, Dict{String,Any}("uID" => img.uid, "imPath" => im_path))
    end

    isempty(movies) && (on_log("[ERROR] No usable images — nothing to train on."); return nothing)

    dropped = flow_dropped_metrics(get(params, "flowMetrics", nothing))

    on_log("[INFO] Training on $(length(movies)) image(s) of $(length(imgs)) selected")
    on_log("[INFO] Model:  $model_path")
    on_log("[INFO] Scales: $(join(scales, ", ")) | cumulative window " *
           "$(Int(get(params, "cumulativeWindow", 5)))" *
           (isempty(dropped) ? "" : " | dropping $(join(dropped, ", "))"))

    # Set-scope run dir, consistent with every other set task (never a temp dir).
    task_dir = imgs[1]._dir
    qc_out_path = joinpath(task_run_dir(task_dir), "flow_training.json")

    # The manifest travels WITH the weights and is what `CoastalUtils` configures itself from, so
    # everything that changes the feature set is written by the runner alongside the .pt — never
    # re-entered by the user at inference time.
    ok = run_py("tasks/opticalFlow/train_run.py",
        (; movies           = movies,
           taskDir          = task_dir,
           modelPath        = model_path,
           qcOutPath        = qc_out_path,
           valueName        = value_name,
           trainChannels    = channels,
           channelName      = join([string(ch_names[c + 1]) for c in channels
                                    if 0 <= c < length(ch_names)], "+"),
           zPlanes          = Int(get(params, "zPlanes", 1)),
           temporalScales   = scales,
           cumulativeWindow = Int(get(params, "cumulativeWindow", 5)),
           droppedMetrics   = dropped,
           epochs           = Int(get(params, "epochs", 30)),
           foregroundWeight = Float64(get(params, "foregroundWeight", 1.0)),
           intensityWeight  = Float64(get(params, "intensityWeight", 1.0)),
           temporalWeight   = Float64(get(params, "temporalWeight", 2.0)),
           embeddingDim     = Int(get(params, "embeddingDim", 16)),
           # Not a form control. cuDNN is non-deterministic on this workload — the same config on
           # the same seed produced 84 and 79 instances across two runs (~6%) — so a seed box would
           # promise a reproducibility it cannot deliver. It is still recorded in the manifest, and
           # a REPL/chain caller can override it.
           seed             = Int(get(params, "seed", 42)),
           normalise        = Float64(get(params, "normalise", 99.99))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Model saved to the vault: $(basename(model_path))")

    # QC (advisory): a training run's one objective signal is whether the loss actually came down.
    # Banked against every image that CONTRIBUTED, the way set-scope clustering banks per image —
    # so the model's provenance is visible from any of its source images, not just the first.
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            metrics = Dict{String,Any}(
                "finalLoss" => Float64(get(qmeta, :finalLoss, NaN)),
                "lossDrop"  => Float64(get(qmeta, :lossDrop, NaN)),
                "epochs"    => Int(get(qmeta, :epochs, 0)),
                "nImages"   => length(movies))
            findings = flow_training_qc_findings(metrics)
            trained_uids = Set(String(m["uID"]) for m in movies)
            for img in imgs
                img.uid in trained_uids || continue
                write_qc(img, "opticalFlow.train", string(basename(model_path)), findings;
                         metrics = metrics)
            end
            on_log("[QC] final loss $(round(metrics["finalLoss"], digits = 4)) " *
                   "($(round(metrics["lossDrop"], digits = 2))x lower than the first epoch).")
        catch e
            on_log("[QC] could not compute training QC: $e")
        end
    end

    Dict{String,Any}("modelName" => basename(model_path),
                     "modelPath" => model_path,
                     "nImages"   => length(movies))
end

"""
    flow_training_qc_findings(metrics) -> Vector

The unambiguous bad case for a training run: the loss did not come down. A model whose loss ended at
or above where it started has learned nothing, and it will still segment — producing a confidently
wrong mask — so this is worth a warning rather than being left to the log.

Pure, so it is unit-tested without running a training job.
"""
function flow_training_qc_findings(metrics::AbstractDict)
    drop = get(metrics, "lossDrop", NaN)
    (drop isa Real && !isnan(drop) && drop <= 1.0) || return Dict{String,Any}[]
    [qc_finding("warn", "opticalFlow.loss_flat", "Loss did not decrease",
        "Check the channel has visible motion, then retrain";
        detail = Dict{String,Any}("finalLoss" => get(metrics, "finalLoss", nothing),
                                  "lossDrop"  => drop,
                                  "epochs"    => get(metrics, "epochs", 0)))]
end
