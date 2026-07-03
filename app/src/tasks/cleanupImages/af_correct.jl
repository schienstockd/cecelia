struct AfCorrect <: CciaTask end

function _run_task(task::AfCorrect, img::CciaImage, params::Dict{String,Any};
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
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidAfCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Channel names → 0-based indices for divisionChannels in each combination
    channel_names_raw = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]

    af_combos_raw = get(params, "afCombinations", nothing)
    af_combos     = Dict{String,Any}()
    if !isnothing(af_combos_raw) && af_combos_raw isa AbstractDict
        for (k, v) in af_combos_raw
            entry = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)

            # divisionChannels: channel names → 0-based indices
            raw_channels = get(entry, "divisionChannels", [])
            idx_channels = Int[]
            for ch in raw_channels
                idx = findfirst(==(String(ch)), ch_names)
                isnothing(idx) || push!(idx_channels, idx - 1)
            end
            entry["divisionChannels"] = idx_channels

            # quotientChannel: resolve name → 0-based index → use as af_combos key
            raw_quot = get(entry, "quotientChannel", [])
            delete!(entry, "quotientChannel")
            if !isempty(raw_quot)
                ch_name = String(first(raw_quot))
                idx = findfirst(==(ch_name), ch_names)
                combo_key = isnothing(idx) ? String(k) : string(idx - 1)
            else
                combo_key = String(k)
            end
            af_combos[combo_key] = entry
        end
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    on_log("[INFO] Combinations: $(length(af_combos))")

    ok = run_py("tasks/cleanupImages/af_correct_run.py",
        (; imPath                = im_path,
           imCorrectionPath      = im_correction_path,
           afCombinations        = af_combos,
           applyGaussian         = Bool(get(params, "applyGaussian", true)),
           applyGaussianToOthers = Bool(get(params, "applyGaussianToOthers", true))),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] AF correction complete.")

    out_value_name = _spec_output_value_name(task, "afCorrected")
    out_filename   = "ccidAfCorrected.ome.zarr"

    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
