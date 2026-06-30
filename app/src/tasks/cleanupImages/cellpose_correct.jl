struct CellposeCorrect <: CciaTask end

function _run_task(task::CellposeCorrect, img::CciaImage, params::Dict{String,Any};
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
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidCpCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Channel names → 0-based indices
    channel_names_raw = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]

    models_json      = get(params, "models", nothing)
    models_converted = Dict{String,Any}()
    if !isnothing(models_json)
        for (k, v) in models_json
            m = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
            raw_channels = get(m, "modelChannels", [])
            idx_channels = Int[]
            for ch in raw_channels
                ch_str = String(ch)
                idx = findfirst(==(ch_str), ch_names)
                isnothing(idx) || push!(idx_channels, idx - 1)
            end
            m["modelChannels"] = idx_channels
            models_converted[String(k)] = m
        end
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    on_log("[INFO] Models: $(length(models_converted))")

    ok = run_py("tasks/cleanupImages/cellpose_correct_run.py",
        (; imPath           = im_path,
           imCorrectionPath = im_correction_path,
           models           = models_converted,
           useDask          = Bool(get(params, "useDask", false))),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Cellpose correction complete.")

    out_value_name = "cpCorrected"
    out_filename   = "ccidCpCorrected.ome.zarr"

    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
