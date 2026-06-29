struct CellposeSegment <: CciaTask end

function _run_task(task::CellposeSegment, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name     = string(get(params, "valueName",     VERSIONED_DEFAULT_VAL))
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    ccid = joinpath(img._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    # Resolve input image path
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

    # Channel names → 0-based indices for cellChannels / nucChannels
    channel_names_raw = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]

    models_json      = get(params, "models", nothing)
    models_converted = Dict{String,Any}()
    if !isnothing(models_json)
        for (k, v) in models_json
            m = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
            for field in ("cellChannels", "nucChannels")
                raw_chs = get(m, field, [])
                idx_chs = Int[]
                for ch in raw_chs
                    ch_str = String(ch)
                    idx = findfirst(==(ch_str), ch_names)
                    isnothing(idx) || push!(idx_chs, idx - 1)
                end
                m[field] = idx_chs
            end
            models_converted[String(k)] = m
        end
    end

    task_dirs   = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    task_subdir = string(get(task_dirs, "tasks", "tasks"))
    params_dir  = joinpath(task_dir, task_subdir)
    mkpath(params_dir)

    task_id     = get(params, "_task_id", string(rand(UInt32), base=16))
    params_file = joinpath(params_dir, "cellposeSegment.$task_id.params.json")
    open(params_file, "w") do io
        JSON3.write(io, (;
            imPath              = im_path,
            taskDir             = task_dir,
            outputValueName     = out_value_name,
            models              = models_converted,
            blockSize           = Int(get(params, "blockSize", 512)),
            overlap             = Int(get(params, "overlap", 64)),
            blockSizeZ          = Int(get(params, "blockSizeZ", 0)),
            overlapZ            = Int(get(params, "overlapZ", 0)),
            labelOverlap        = Float64(get(params, "labelOverlap", 0.0)),
            matchThreshold      = Float64(get(params, "matchThreshold", 0.3)),
            removeUnmatched     = Bool(get(params, "removeUnmatched", false)),
            minCellSize         = Int(get(params, "minCellSize", 0)),
            cellSizeMax         = Int(get(params, "cellSizeMax", 0)),
            labelExpansion      = Int(get(params, "labelExpansion", 0)),
            labelErosion        = Int(get(params, "labelErosion", 0)),
            clearTouchingBorder = Bool(get(params, "clearTouchingBorder", false)),
            clearDepth          = Bool(get(params, "clearDepth", false)),
            normaliseToWhole    = Bool(get(params, "normaliseToWhole", true)),
            useDask             = Bool(get(params, "useDask", false)),
        ))
    end

    # @__DIR__ = app/src/tasks/segment/ → three dirname levels reach app/
    py_script  = joinpath(dirname(dirname(dirname(@__DIR__))), "py", "tasks",
                          "segment", "cellpose_run.py")
    python_bin = python_bin_path()

    if !isfile(py_script)
        on_log("[ERROR] Python script not found: $py_script")
        return nothing
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labels", out_value_name)).zarr")
    on_log("[INFO] Models: $(length(models_converted))")

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

    on_log("[INFO] Segmentation complete.")

    # Derive all zarr filenames that the Python code will have written.
    # 'base' → {outputValueName}.zarr; other types → {outputValueName}_{ma}.zarr
    match_as_list = unique([string(get(m, "matchAs", "base")) for (_, m) in models_converted])
    non_base      = filter(ma -> ma != "base", match_as_list)
    label_files   = vcat(["$(out_value_name).zarr"],
                         ["$(out_value_name)_$(ma).zarr" for ma in non_base])

    # Update ccid.json: record label outputs in the `labels` dict
    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    labels_dict = Dict{String, Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(raw2, "labels", Dict{String,Any}()))
    labels_dict[out_value_name] = label_files
    raw2["labels"] = labels_dict
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("outputValueName"  => out_value_name,
                     "labelValueName"   => out_value_name,
                     "labelFiles"       => label_files)
end
