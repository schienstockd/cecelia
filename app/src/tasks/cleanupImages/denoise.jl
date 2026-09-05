struct Denoise <: CciaTask end

task_output_effect(::Denoise) = "new-version"

# Findings recorded by the Python runner in `denoise_stats.json`. The runner writes at least:
#   channelsRun     - the channels the model actually ran on (post gate)
#   channelsSkipped - channels the saturation gate refused
#   inputFrames     - the mirror-pad width baked into the model (`SUPPORT.in_channels`)
#   perChannelMinMax - {chan: {inMin, inMax, outMin, outMax}} for a sanity check
# `skipped` is a warn: the user asked for denoise on a channel that will not benefit AND was
# refused, so the output store is intentionally NOT what they asked for.
function _denoise_qc_findings(meta)
    findings = Dict{String,Any}[]
    skipped = collect(Int, get(meta, "channelsSkipped", Int[]))
    if !isempty(skipped)
        push!(findings, qc_finding("warn", "denoise.channel_saturated";
            value = length(skipped),
            detail = Dict{String,Any}("channels" => skipped,
                                      "reason" => "at sensor ceiling — denoise on saturated data is a no-op")))
    end
    findings
end

# The saturation gate (DENOISE_INTEGRATION_PLAN.md D6). Reads `meta.saturation.channels` — the
# same block `saturation_qc_findings` reads. Returns the list of user-selected channels that are
# marked `saturated`, so the handler can log and the runner can refuse in one direction.
function _denoise_saturated_channels(meta::AbstractDict, wanted::Vector{Int})::Vector{Int}
    sat = get(meta, "saturation", nothing)
    sat isa AbstractDict || return Int[]
    chans = get(Dict{String,Any}(String(k) => v for (k, v) in sat), "channels", nothing)
    chans isa AbstractVector || return Int[]
    out = Int[]
    for ch in chans
        ch isa AbstractDict || continue
        get(ch, :saturated, false) === true || get(ch, "saturated", false) === true || continue
        idx = get(ch, :index, get(ch, "index", nothing))
        idx isa Integer || continue
        Int(idx) in wanted && push!(out, Int(idx))
    end
    sort!(out)
end

function _run_task(task::Denoise, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir       = dirname(dirname(img._dir))
    im_path        = joinpath(proj_dir, "0", img.uid, string(filename))
    im_output_path = joinpath(proj_dir, "0", img.uid, "ccidDenoised.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Model is a stem-or-filename; the manifest resolves the architecture. A missing model here is a
    # user-visible error — the runner can't infer the network shape without one.
    model_field = string(get(params, "model", ""))
    if isempty(strip(model_field))
        on_log("[ERROR] No denoise model selected. Train one on the Model Training page, then pick it here.")
        return nothing
    end
    model_path = denoise_model_path(model_field)
    if isnothing(model_path)
        on_log("[ERROR] Model '$(model_field)' not found in $(denoise_models_dir())")
        return nothing
    end
    manifest = denoise_model_manifest(model_field)
    if isempty(manifest)
        on_log("[ERROR] Model '$(model_field)' has no manifest sidecar. " *
               "SUPPORT does not encode its architecture in the checkpoint; without the manifest " *
               "the runner cannot rebuild the network. Retrain via the Model Training page.")
        return nothing
    end

    ch_names = ccid_channel_names(raw)
    channel_idx = channel_indices(get(params, "channels", nothing), ch_names; what = "channels")
    if isempty(channel_idx)
        on_log("[ERROR] Pick at least one channel to denoise — SUPPORT is per-channel.")
        return nothing
    end

    # Saturation gate (D6). Reads the `meta.saturation` block written at import time. If ALL
    # requested channels are saturated, refuse the run entirely (there is nothing left to denoise);
    # otherwise drop the saturated ones and continue with the rest, and let the QC finding say so.
    meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    saturated = _denoise_saturated_channels(meta, channel_idx)
    if !isempty(saturated)
        if length(saturated) == length(channel_idx)
            on_log("[ERROR] Every selected channel ($(channel_idx)) is at the sensor ceiling — " *
                   "SUPPORT/DeepCAD-RT-family denoisers assume shot noise and do nothing on saturated data. " *
                   "Uncheck those channels or re-acquire at lower gain.")
            return nothing
        else
            keep = [c for c in channel_idx if !(c in saturated)]
            on_log("[WARN] Dropping saturated channels $(saturated); denoising $(keep). " *
                   "(Saturated data is a no-op for shot-noise denoisers.)")
            channel_idx = keep
        end
    end

    batch_size = Int(get(params, "batchSize", 2))

    on_log("[INFO] Input:    $im_path")
    on_log("[INFO] Output:   $im_output_path")
    on_log("[INFO] Model:    $(model_path)")
    on_log("[INFO] Channels: $(channel_idx)")
    on_log("[INFO] Skipped (saturated): $(saturated)")

    qc_out_path = joinpath(task_run_dir(img._dir), "denoise_stats.json")

    ok = run_py("tasks/cleanupImages/denoise_run.py",
        (; imPath        = im_path,
           imOutputPath  = im_output_path,
           modelPath     = model_path,
           manifest      = manifest,
           channels      = channel_idx,
           channelsSkipped = saturated,
           batchSize     = batch_size,
           qcOutPath     = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Denoise complete.")

    out_value_name = _spec_output_value_name(task, "denoised")
    out_filename   = "ccidDenoised.ome.zarr"

    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            findings = _denoise_qc_findings(qmeta)
            write_qc(img, "cleanupImages.denoise", out_value_name, findings;
                     metrics = Dict{String,Any}(
                         "channelsRun"     => length(get(qmeta, "channelsRun", Int[])),
                         "channelsSkipped" => length(get(qmeta, "channelsSkipped", Int[])),
                         "inputFrames"     => get(qmeta, "inputFrames", 0)),
                     source = Dict{String,Any}("shape" => collect(Int, qmeta["shape"])),
                     output = Dict{String,Any}("shape" => collect(Int, qmeta["shape"])),
                     denoise = Dict{String,Any}(
                         "model"           => basename(model_path),
                         "channelsRun"     => get(qmeta, "channelsRun", Int[]),
                         "channelsSkipped" => get(qmeta, "channelsSkipped", Int[]),
                         "inputFrames"     => get(qmeta, "inputFrames", 0),
                         "perChannelMinMax" => get(qmeta, "perChannelMinMax", Dict())))
            isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
        catch e
            on_log("[QC] could not compute denoise QC: $e")
        end
    end

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
