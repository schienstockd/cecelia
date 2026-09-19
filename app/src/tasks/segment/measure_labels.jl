struct MeasureLabels <: CciaTask end

Base.@kwdef struct MeasureLabelsParams
    outputValueName::String    = VERSIONED_DEFAULT_VAL
    intensityValueName::String = VERSIONED_DEFAULT_VAL
    intensityMeasure::String   = "mean"
    gaussianFilter::Float64    = 0.0
    extendedMeasures::Bool     = false
    blockSize::Int             = 512
    overlap::Int               = 64
    blockSizeZ::Int            = 0
    overlapZ::Int              = 0
end

function parse_measure_labels_params(d::AbstractDict)::MeasureLabelsParams
    MeasureLabelsParams(;
        outputValueName    = string(get(d, "outputValueName", VERSIONED_DEFAULT_VAL)),
        intensityValueName = string(get(d, "intensityValueName", VERSIONED_DEFAULT_VAL)),
        intensityMeasure   = string(get(d, "intensityMeasure", "mean")),
        gaussianFilter     = Float64(get(d, "gaussianFilter", 0.0)),
        extendedMeasures   = Bool(get(d, "extendedMeasures", false)),
        blockSize          = Int(get(d, "blockSize", 512)),
        overlap            = Int(get(d, "overlap", 64)),
        blockSizeZ         = Int(get(d, "blockSizeZ", 0)),
        overlapZ           = Int(get(d, "overlapZ", 0)))
end

function _run_task(task::MeasureLabels, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    p                    = parse_measure_labels_params(params)
    task_dir             = img._dir
    ccid                 = state_file(task_dir)
    raw                  = read_ccid_raw(ccid)

    # Resolve the intensity image path
    im_filename = versioned_get_field_at(raw, "filepath", p.intensityValueName; version = get(params, "version", nothing))  # ratchet-ok: chain-pinning read, orthogonal to typed params
    if isnothing(im_filename)
        on_log("[ERROR] No filepath for intensityValueName='$(p.intensityValueName)'")
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
    label_entry = get(labels_dict_raw, p.outputValueName,
                      get(labels_dict_raw, Symbol(p.outputValueName), nothing))
    if isnothing(label_entry)
        on_log("[ERROR] No labels registered for outputValueName='$(p.outputValueName)'")
        return nothing
    end
    label_files = label_entry isa AbstractVector ?
                  collect(String, label_entry) : [string(label_entry)]
    label_dir   = joinpath(task_dir, "labels")

    on_log("[INFO] Labels: $(join(label_files, ", "))")
    on_log("[INFO] Image:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labelProps", p.outputValueName)).h5ad")

    # Intra-task threading capped at the CPU pool's current headroom. `in_flight` already includes
    # this task, so `limit - in_flight + 1` is the total worker budget it can safely spend. Racy
    # (a peer may submit between here and thread spawn), but the cap moves down when the user
    # tightens the PoolThrottle. Also capped at 8: the per-timepoint work is disk-bound beyond that
    # on the typical zarr, and running more threads would just steal from peers for no wall-clock.
    pool_headroom = try
        cpu = filter(x -> x.name == "cpu", pool_status())
        isempty(cpu) ? 1 : max(1, cpu[1].limit - cpu[1].running + 1)
    catch; 1 end
    n_threads = min(pool_headroom, 8)

    ok = run_py("tasks/segment/measure_labels_run.py",
        (; imPath            = im_path,
           taskDir           = task_dir,
           outputValueName   = p.outputValueName,
           labelDir          = label_dir,
           labelFiles        = label_files,
           intensityMeasure  = p.intensityMeasure,
           gaussianFilter    = p.gaussianFilter,
           extendedMeasures  = p.extendedMeasures,
           blockSize         = p.blockSize,
           overlap           = p.overlap,
           blockSizeZ        = p.blockSizeZ,
           overlapZ          = p.overlapZ,
           nThreads          = n_threads),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing
    ok || return nothing

    on_log("[INFO] Measurement complete.")

    h5ad_filename = "$(p.outputValueName).h5ad"

    # QC (advisory): bank the objective cell count so cohort stats can later flag anomalies. A count
    # of 0 (measured nothing) is the one unambiguous problem → an advisory finding. Read the count
    # from the just-written .h5ad by path (img.label_props in-memory isn't refreshed here yet).
    try
        h5ad_path = joinpath(task_dir, "labelProps", h5ad_filename)
        n = n_obs(label_props(h5ad_path))
        findings = n == 0 ?
            [qc_finding("warn", "measure.no_cells", "No cells measured",
                "The segmentation produced no measurable objects — check the segmentation and re-run this step.")] :
            Dict{String,Any}[]
        write_qc(img, "segment.measureLabels", p.outputValueName, findings;
                 metrics = Dict{String,Any}("nCells" => n))
        on_log("[QC] measured $n cell(s).")
    catch e
        on_log("[QC] could not compute measure QC: $e")
    end

    commit_state!(img) do raw
        lp = Dict{String,String}(String(k) => string(v)
                                 for (k, v) in get(raw, "label_props", Dict{String,Any}()))
        lp[p.outputValueName] = h5ad_filename
        # the segmentation just measured becomes the active label_props version, so gating (and any
        # value_name fallback) defaults to the most recently produced segmentation.
        lp[VERSIONED_ACTIVE_KEY] = p.outputValueName
        raw["label_props"] = lp
    end

    Dict{String,Any}("outputValueName" => p.outputValueName,
                     "labelValueName"  => p.outputValueName,
                     "h5adFile"        => h5ad_filename)
end
