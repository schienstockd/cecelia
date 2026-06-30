struct DriftCorrect <: CciaTask end

function _run_task(task::DriftCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    ccid       = joinpath(img._dir, "ccid.json")
    raw        = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir           = dirname(dirname(img._dir))
    im_path            = joinpath(proj_dir, "0", img.uid, string(filename))
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidDriftCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Convert driftChannel from channel name to 0-based index
    channel_names_raw = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]

    drift_channel_raw = get(params, "driftChannel", nothing)
    drift_channel_idx = 0
    if !isnothing(drift_channel_raw)
        # channelSelection stores as array even when multiple=false
        raw_ch = drift_channel_raw isa AbstractVector ? drift_channel_raw :
                 [drift_channel_raw]
        if !isempty(raw_ch) && !isempty(ch_names)
            ch_str = String(raw_ch[1])
            idx = findfirst(==(ch_str), ch_names)
            isnothing(idx) || (drift_channel_idx = idx - 1)
        end
    end

    on_log("[INFO] Input:       $im_path")
    on_log("[INFO] Output:      $im_correction_path")
    on_log("[INFO] Drift ch:    $drift_channel_idx")

    ok = run_py("tasks/cleanupImages/drift_correct_run.py",
        (; imPath             = im_path,
           imCorrectionPath   = im_correction_path,
           driftChannel       = drift_channel_idx,
           driftNormalisation = string(get(params, "driftNormalisation", "none"))),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Drift correction complete.")

    out_value_name = "driftCorrected"
    out_filename   = "ccidDriftCorrected.ome.zarr"

    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
