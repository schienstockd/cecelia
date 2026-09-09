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

# The "no image is long enough for the temporal window" refusal, spelled out with the actual T
# values and the largest inputFrames that would fit. Was: [WARN] skipped + [ERROR] no usable
# volumes deep in the Python runner log, and the fix was in neither. Pure so a test can pin the
# exact numbers without a GPU or a fixture image.
function _support_short_movie_refusal(short_ts::Vector{Tuple{String,Int}}, input_frames::Int)::Vector{String}
    isempty(short_ts) && return String[]
    min_t = minimum(t for (_, t) in short_ts)
    max_odd = isodd(min_t) ? min_t : max(min_t - 1, 1)
    ["[ERROR] No selected image has $input_frames+ timepoints — the shortest is $min_t.",
     "[ERROR] Set Temporal window to $max_odd (largest odd value ≤ $min_t) or pick longer movies."]
end

# Form-time advisory (served by /api/tasks/validate, rendered under the Temporal window slider).
# Same rule as the run-time refusal above, banked here rather than in `paramAdvisors.ts` so both
# sides read the same source of truth. Pure, tested — the "largest odd ≤ min T" suggestion mirrors
# `_support_short_movie_refusal` above by construction.
function _support_temporal_window_advisory(value, imgs::Vector{CciaImage}, _siblings::AbstractDict)
    v = value isa Integer ? Int(value) :
        value isa Real    ? (isfinite(value) ? Int(trunc(value)) : 0) :
        value isa AbstractString ? something(tryparse(Int, value), 0) : 0
    v > 0 || return nothing
    ts = Int[]
    for img in imgs
        t = something(tryparse_i(get(img.meta, "SizeT", nothing)), 0)
        t > 0 && push!(ts, t)
    end
    isempty(ts) && return nothing
    min_t, max_t = extrema(ts)
    n_over = count(t -> t < v, ts)
    n = length(ts)
    odd(x) = isodd(x) ? x : max(x - 1, 1)
    movies = n == 1 ? "1 movie" : "$n movies"
    if n_over == 0
        return (severity = "ok",
                message  = "$(v)f on $movies (shortest $(min_t)f)",
                tip      = "Every selected movie has at least $v timepoints, so all of them will train.")
    elseif n_over == n
        return (severity = "fail",
                message  = "over every movie (longest $(max_t)f) — nothing can train",
                tip      = "The temporal window has to fit inside every selected movie. Longest is " *
                           "$(max_t)f — set it to $(odd(max_t)) (largest odd value ≤ $max_t) or pick " *
                           "longer movies.")
    else
        return (severity = "warn",
                message  = "$n_over of $n too short (shortest $(min_t)f)",
                tip      = "Movies with fewer than $v timepoints are refused at Run. Set the window " *
                           "to $(odd(min_t)) (largest odd value ≤ $min_t) to train on all $movies.")
    end
end

register_param_validator!("opticalFlow.trainSupportDenoise", "inputFrames",
                          _support_temporal_window_advisory)

# QC findings for a SUPPORT training run. Two states, one warn:
#   - Stopped early (loss plateaued and early-stop caught it): expected on shot-noise-limited
#     fluorescence, NOT a bug. No finding.
#   - Ran to the full epoch budget AND the epoch-loss stayed within the plateau band all the way
#     through: something is off — either the channel is truly saturated (nothing to remove) or the
#     pooled prior collapsed a weak channel. Warn and point at the collapse QC in
#     `cleanupImages.denoise`, which is the reliable quality signal (per-run out/in ratio).
# Pure so a test can exercise it without a GPU.
function _support_train_qc_findings(metrics::AbstractDict)
    out = Dict{String,Any}[]
    stopped_early = get(metrics, "stoppedEarly", false) === true
    drop = get(metrics, "lossDrop", NaN)
    if drop isa Real && !isnan(drop) && drop <= 1.0 && !stopped_early
        push!(out, qc_finding("warn", "denoise.loss_flat", "Loss stayed flat for every epoch",
            "Compare out/in ratios in cleanupImages.denoise — if one channel collapses, retrain with trainMode=perChannel";
            detail = Dict{String,Any}("finalLoss"    => get(metrics, "finalLoss", nothing),
                                      "lossDrop"     => drop,
                                      "epochs"       => get(metrics, "epochs", 0),
                                      "stoppedEarly" => stopped_early)))
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

    local channel_indices_selected, channel_names_selected, model_path, bundle_dir, unet, unet_size
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

        # Reserve both targets and let the runner land the one matching trainMode. Cheap and it
        # keeps the vault helper's overwrite check symmetric across the two modes.
        # SUPPORT_PERCHANNEL_PLAN.md → D3.
        model_path, bundle_dir = denoise_model_target(get(params, "modelName", "");
                                          overwrite = Bool(get(params, "overwrite", false)),
                                          want_bundle = true)
    catch e
        on_log("[ERROR] $(e isa ErrorException || e isa ParamValidationError ? e.msg : sprint(showerror, e))")
        return nothing
    end

    input_frames = Int(get(params, "inputFrames", 61))
    isodd(input_frames) || (on_log("[ERROR] inputFrames must be odd (centre is the target); got $input_frames"); return nothing)

    # `auto` is parked — the v1 SNR precheck (Poisson head-room over nonzero-median) over-fires on
    # narrow-DR-but-clean channels like nuc-GFP. Post-run collapse QC in cleanupImages.denoise stays
    # as the reliable signal. See SUPPORT_PERCHANNEL_PLAN.md → Deferred (metric v2).
    train_mode = string(get(params, "trainMode", "pooled"))
    train_mode in ("pooled", "perChannel") || (
        on_log("[ERROR] trainMode must be pooled|perChannel, got \"$train_mode\""); return nothing)

    # Collect usable images (per-image existence + channel-name agreement + T-length check). The
    # T-length check runs HERE (not just in the Python runner) so a user picking a 61-frame window
    # on a 31-timepoint set sees the honest fix — including the largest inputFrames that would
    # actually fit — before any zarr load or GPU init. Was: [WARN] skipped + [ERROR] no usable
    # volumes deep in the runner log; the fix wasn't in either message.
    movies = Dict{String,Any}[]
    short_ts = Tuple{String,Int}[]   # (uid, T) for images too short for the current window
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
        size_t = something(tryparse_i(get(img.meta, "SizeT", nothing)), 0)
        if size_t < input_frames
            push!(short_ts, (String(img.uid), size_t))
            on_log("[WARN] $(img.uid): T=$size_t < inputFrames=$input_frames — skipped")
            continue
        end
        push!(movies, Dict{String,Any}("uID" => img.uid, "imPath" => im_path))
    end
    if isempty(movies)
        for line in _support_short_movie_refusal(short_ts, input_frames)
            on_log(line)
        end
        isempty(short_ts) && on_log("[ERROR] No usable images — nothing to train on.")
        return nothing
    end

    joined_names = join(channel_names_selected, "+")
    joined_idx   = join(channel_indices_selected, ",")
    on_log("[INFO] Training on $(length(movies)) image(s) of $(length(imgs)) selected")
    on_log("[INFO] Model:    $(train_mode == "perChannel" ? bundle_dir : model_path)")
    on_log("[INFO] Channels: $joined_names (indices $joined_idx)")
    on_log("[INFO] Mode:     $train_mode")
    on_log("[INFO] Arch:     UNet $(unet["midChannels"]) depth $(unet["depth"]) | " *
           "inputFrames $input_frames | patch $(Int(get(params, "patchXY", 128)))")

    task_dir = imgs[1]._dir
    qc_out_path = joinpath(task_run_dir(task_dir), "support_training.json")

    ok = run_py("tasks/opticalFlow/train_support_denoise_run.py",
        (; movies           = movies,
           taskDir          = task_dir,
           modelPath        = model_path,
           bundleDir        = bundle_dir,
           qcOutPath        = qc_out_path,
           valueName        = value_name,
           trainMode        = train_mode,
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
           # Early-stop budget — the runner passes patience=None to coastal when earlyStop is off,
           # matching the current "train to `epochs`" behaviour. Defaults track the coastal defaults
           # (patience 5, min_delta 0.005) which stop MERTK-large in ~6-10 epochs on the plateau.
           earlyStop        = Bool(get(params, "earlyStop", true)),
           patience         = Int(get(params, "patience", 5)),
           minLossDelta     = Float64(get(params, "minLossDelta", 5e-3)),
           midZOnly         = Bool(get(params, "midZOnly", true))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    saved_at = train_mode == "perChannel" ? bundle_dir : model_path
    on_log("[INFO] Model saved to the denoise vault: $(basename(saved_at)) ($train_mode)")

    # QC banked against every source image, like opticalFlow.train.
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            metrics = Dict{String,Any}(
                "finalLoss"    => Float64(get(qmeta, :finalLoss, NaN)),
                "lossDrop"     => Float64(get(qmeta, :lossDrop, NaN)),
                "epochs"       => Int(get(qmeta, :epochs, 0)),
                "stoppedEarly" => Bool(get(qmeta, :stoppedEarly, false)),
                "stopEpoch"    => Int(get(qmeta, :stopEpoch, 0)),
                "nImages"      => length(movies))
            findings = _support_train_qc_findings(metrics)
            trained_uids = Set(String(m["uID"]) for m in movies)
            for img in imgs
                img.uid in trained_uids || continue
                write_qc(img, "opticalFlow.trainSupportDenoise", string(basename(model_path)),
                         findings; metrics = metrics)
            end
            stop_note = metrics["stoppedEarly"] ? " (stopped early at epoch $(metrics["stopEpoch"]))" : ""
            on_log("[QC] final loss $(round(metrics["finalLoss"], digits = 4)) " *
                   "($(round(metrics["lossDrop"], digits = 2))x lower than the first epoch)$stop_note.")
        catch e
            on_log("[QC] could not compute training QC: $e")
        end
    end

    Dict{String,Any}("modelName" => basename(saved_at),
                     "modelPath" => saved_at,
                     "mode"      => train_mode,
                     "nImages"   => length(movies))
end
