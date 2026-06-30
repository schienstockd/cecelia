struct MeasureLabels <: CciaTask end

function _run_task(task::MeasureLabels, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    out_value_name       = string(get(params, "outputValueName",   VERSIONED_DEFAULT_VAL))
    intensity_value_name = string(get(params, "intensityValueName", VERSIONED_DEFAULT_VAL))
    task_dir             = img._dir
    ccid                 = joinpath(task_dir, "ccid.json")
    raw                  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    # Resolve the intensity image path
    im_filename = versioned_get_field(raw, "filepath", intensity_value_name)
    if isnothing(im_filename)
        on_log("[ERROR] No filepath for intensityValueName='$intensity_value_name'")
        return nothing
    end
    proj_dir = dirname(dirname(task_dir))
    im_path  = joinpath(proj_dir, "0", img.uid, string(im_filename))
    if !ispath(im_path)
        on_log("[ERROR] Intensity image not found: $im_path")
        return nothing
    end

    # Resolve label zarr paths from the labels dict
    labels_dict_raw = get(raw, "labels", Dict{String,Any}())
    label_entry = get(labels_dict_raw, out_value_name,
                      get(labels_dict_raw, Symbol(out_value_name), nothing))
    if isnothing(label_entry)
        on_log("[ERROR] No labels registered for outputValueName='$out_value_name'")
        return nothing
    end
    label_files = label_entry isa AbstractVector ?
                  collect(String, label_entry) : [string(label_entry)]
    label_dir   = joinpath(task_dir, "labels")

    # Write params JSON
    task_dirs   = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    task_subdir = string(get(task_dirs, "tasks", "tasks"))
    params_dir  = joinpath(task_dir, task_subdir)
    mkpath(params_dir)

    task_id     = get(params, "_task_id", string(rand(UInt32), base=16))
    params_file = joinpath(params_dir, "measureLabels.$task_id.params.json")
    open(params_file, "w") do io
        JSON3.write(io, (;
            imPath            = im_path,
            taskDir           = task_dir,
            outputValueName   = out_value_name,
            labelDir          = label_dir,
            labelFiles        = label_files,
            intensityMeasure  = string(get(params, "intensityMeasure",  "mean")),
            gaussianFilter    = Float64(get(params, "gaussianFilter",   0.0)),
            extendedMeasures  = Bool(get(params, "extendedMeasures",   false)),
            saveMeshes        = Bool(get(params, "saveMeshes",         false)),
            blockSize         = Int(get(params, "blockSize",           512)),
            overlap           = Int(get(params, "overlap",             64)),
            blockSizeZ        = Int(get(params, "blockSizeZ",          0)),
            overlapZ          = Int(get(params, "overlapZ",            0)),
        ))
    end

    py_script  = joinpath(dirname(dirname(dirname(@__DIR__))), "py", "tasks",
                          "segment", "measure_labels_run.py")
    python_bin = python_bin_path()

    if !isfile(py_script)
        on_log("[ERROR] Python script not found: $py_script")
        return nothing
    end

    on_log("[INFO] Labels: $(join(label_files, ", "))")
    on_log("[INFO] Image:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labelProps", out_value_name)).h5ad")

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

    on_log("[INFO] Measurement complete.")

    h5ad_filename = "$(out_value_name).h5ad"
    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    lp   = Dict{String,String}(String(k) => string(v)
                               for (k, v) in get(raw2, "label_props", Dict{String,Any}()))
    lp[out_value_name] = h5ad_filename
    # the segmentation just measured becomes the active label_props version, so gating (and any
    # value_name fallback) defaults to the most recently produced segmentation.
    lp[VERSIONED_ACTIVE_KEY] = out_value_name
    raw2["label_props"] = lp
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    Dict{String,Any}("outputValueName" => out_value_name,
                     "labelValueName"  => out_value_name,
                     "h5adFile"        => h5ad_filename)
end
