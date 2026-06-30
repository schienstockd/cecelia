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

    task_dirs   = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    task_subdir = string(get(task_dirs, "tasks", "tasks"))
    params_dir  = joinpath(img._dir, task_subdir)
    mkpath(params_dir)

    task_id     = get(params, "_task_id", string(rand(UInt32), base=16))
    params_file = joinpath(params_dir, "afCorrect.$task_id.params.json")
    open(params_file, "w") do io
        JSON3.write(io, (;
            imPath                = im_path,
            imCorrectionPath      = im_correction_path,
            afCombinations        = af_combos,
            applyGaussian         = Bool(get(params, "applyGaussian", true)),
            applyGaussianToOthers = Bool(get(params, "applyGaussianToOthers", true)),
        ))
    end

    py_script  = joinpath(dirname(dirname(dirname(@__DIR__))), "py", "tasks",
                          "cleanupImages", "af_correct_run.py")
    python_bin = python_bin_path()

    if !isfile(py_script)
        on_log("[ERROR] Python script not found: $py_script")
        return nothing
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    on_log("[INFO] Combinations: $(length(af_combos))")

    out_pipe = Pipe()
    proc = run(pipeline(`$python_bin $py_script --params $params_file`;
                        stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)

    for line in eachline(out_pipe)
        m = match(r"^\[PROGRESS\] (\d+)/(\d+)$", line)
        if !isnothing(m)
            on_progress(parse(Int, m[1]), parse(Int, m[2]))
        else
            on_log(line)
        end
    end
    wait(proc)
    ok = proc.exitcode == 0 && proc.termsignal == 0
    ok || return nothing

    on_log("[INFO] AF correction complete.")

    out_value_name = "afCorrected"
    out_filename   = "ccidAfCorrected.ome.zarr"

    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
