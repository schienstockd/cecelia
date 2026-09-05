struct TrainSupportDenoise <: CciaTask end

# SET scope, mirroring `TrainFlowModel` above. One denoise model per acquisition-class, reused across
# the set — that is why the vault is per-config, not per-image (DENOISE_INTEGRATION_PLAN.md D3).
#
# The fun-name namespace stays `opticalFlow.*` because it is baked into stored ccid.json chain state
# (a rename would break every persisted chain). The display category is "Model training" — Phase C
# renamed the page and added a kind selector to the vault so both training tasks live on one honest
# page together.

# UNet arch by size — the three points measured 2026-09-05 on 2h06xA. "large" is the v2 config that
# produced the "that's great" MP4; the smaller two exist for laptop VRAM budgets and quick iteration.
const _SUPPORT_UNET_SIZES = Dict{String,Any}(
    "small"  => Dict{String,Any}("midChannels" => [16, 32,  64, 128], "depth" => 4),
    "medium" => Dict{String,Any}("midChannels" => [32, 64, 128, 256], "depth" => 4),
    "large"  => Dict{String,Any}("midChannels" => [64,128, 256, 512], "depth" => 4),
)

# Two unambiguous bad cases, pure so a test can exercise them without a GPU. Both about the LOSS —
# training's one objective signal until inference runs on real data.
function _support_train_qc_findings(metrics::AbstractDict)
    out = Dict{String,Any}[]
    drop = get(metrics, "lossDrop", NaN)
    if drop isa Real && !isnan(drop) && drop <= 1.0
        push!(out, qc_finding("warn", "denoise.loss_flat", "Loss did not decrease",
            "Check the channel is photon-limited, then retrain — SUPPORT has nothing to remove on saturated data";
            detail = Dict{String,Any}("finalLoss" => get(metrics, "finalLoss", nothing),
                                      "lossDrop"  => drop,
                                      "epochs"    => get(metrics, "epochs", 0))))
    end
    out
end

function _run_task(task::TrainSupportDenoise, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    isempty(imgs) && (on_log("[ERROR] No images selected to train on."); return nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))

    # Channel names from the first image; every image must agree. Same rationale as
    # `TrainFlowModel` — a mixed set would silently train on a different reporter per movie.
    ch_names = ccid_channel_names(read_ccid_raw(state_file(imgs[1])))

    local channel_indices_selected, channel_names_selected, model_path, unet, unet_size
    try
        chan_sel = channel_indices(get(params, "trainChannels", []), ch_names;
                                   what = "trainChannels")
        isempty(chan_sel) && error("Select at least one channel to train on.")
        channel_indices_selected = chan_sel
        channel_names_selected   = String[String(ch_names[c + 1]) for c in chan_sel]

        unet_size = string(get(params, "unetSize", "medium"))
        haskey(_SUPPORT_UNET_SIZES, unet_size) ||
            throw(ParamValidationError("unetSize must be small/medium/large, got \"$unet_size\""))
        unet = _SUPPORT_UNET_SIZES[unet_size]

        model_path = denoise_model_target(get(params, "modelName", "");
                                          overwrite = Bool(get(params, "overwrite", false)))
    catch e
        on_log("[ERROR] $(e isa ErrorException || e isa ParamValidationError ? e.msg : sprint(showerror, e))")
        return nothing
    end

    # Collect usable images (per-image existence + channel-name agreement check).
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
            on_log("[WARN] $(img.uid): channel names differ from $(imgs[1].uid) — skipped")
            continue
        end
        push!(movies, Dict{String,Any}("uID" => img.uid, "imPath" => im_path))
    end
    isempty(movies) && (on_log("[ERROR] No usable images — nothing to train on."); return nothing)

    input_frames = Int(get(params, "inputFrames", 61))
    isodd(input_frames) || (on_log("[ERROR] inputFrames must be odd (centre is the target); got $input_frames"); return nothing)

    joined_names = join(channel_names_selected, "+")
    joined_idx   = join(channel_indices_selected, ",")
    on_log("[INFO] Training on $(length(movies)) image(s) of $(length(imgs)) selected")
    on_log("[INFO] Model:    $model_path")
    on_log("[INFO] Channels: $joined_names (indices $joined_idx)")
    on_log("[INFO] Arch:     UNet $(unet["midChannels"]) depth $(unet["depth"]) | " *
           "inputFrames $input_frames | patch $(Int(get(params, "patchXY", 128)))")

    task_dir = imgs[1]._dir
    qc_out_path = joinpath(task_run_dir(task_dir), "support_training.json")

    ok = run_py("tasks/opticalFlow/train_support_denoise_run.py",
        (; movies           = movies,
           taskDir          = task_dir,
           modelPath        = model_path,
           qcOutPath        = qc_out_path,
           valueName        = value_name,
           trainChannels    = channel_indices_selected,
           channelNames     = channel_names_selected,
           inputFrames      = input_frames,
           patchXY          = Int(get(params, "patchXY", 128)),
           epochs           = Int(get(params, "epochs", 20)),
           batchSize        = Int(get(params, "batchSize", 2)),
           learningRate     = Float64(get(params, "learningRate", 5e-4)),
           midChannels      = unet["midChannels"],
           depth            = unet["depth"],
           unetSize         = unet_size,
           blindConvChannels = Int(get(params, "blindConvChannels", 64)),
           midZOnly         = Bool(get(params, "midZOnly", true))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Model saved to the denoise vault: $(basename(model_path))")

    # QC banked against every source image, like opticalFlow.train.
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            metrics = Dict{String,Any}(
                "finalLoss" => Float64(get(qmeta, :finalLoss, NaN)),
                "lossDrop"  => Float64(get(qmeta, :lossDrop, NaN)),
                "epochs"    => Int(get(qmeta, :epochs, 0)),
                "nImages"   => length(movies))
            findings = _support_train_qc_findings(metrics)
            trained_uids = Set(String(m["uID"]) for m in movies)
            for img in imgs
                img.uid in trained_uids || continue
                write_qc(img, "opticalFlow.trainSupportDenoise", string(basename(model_path)),
                         findings; metrics = metrics)
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
