struct CellposeSegment <: CciaTask end

# Pure QC helper (drift pattern): objective segment counts → findings + the primary (base) count.
# Only the "0 cells" case is an advisory finding; the counts themselves are banked as metrics.
function _segment_qc_findings(counts::AbstractDict)
    primary = haskey(counts, "base") ? counts["base"] :
              (isempty(counts) ? 0 : first(values(counts)))
    findings = primary == 0 ?
        [qc_finding("warn", "segment.no_cells", "No cells segmented",
            "Segmentation produced no objects — check the channels/diameter and re-run this step.")] :
        Dict{String,Any}[]
    findings, primary
end

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

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $(joinpath(task_dir, "labels", out_value_name)).zarr")
    on_log("[INFO] Models: $(length(models_converted))")

    qc_out_path = joinpath(task_run_dir(task_dir), "segment_counts.json")

    ok = run_py("tasks/segment/cellpose_run.py",
        (; imPath              = im_path,
           taskDir             = task_dir,
           outputValueName     = out_value_name,
           qcOutPath           = qc_out_path,
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
           useDask             = Bool(get(params, "useDask", false))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
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

    # QC (advisory): bank the objective per-type cell count the Python runner wrote (drift pattern).
    if isfile(qc_out_path)
        try
            qmeta  = JSON3.read(read(qc_out_path, String))
            counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
            findings, primary = _segment_qc_findings(counts)
            write_qc(img, "segment.cellpose", out_value_name, findings;
                     metrics = Dict{String,Any}("nCells" => primary, "byType" => counts))
            on_log("[QC] segmented $primary cell(s)" *
                   (length(counts) > 1 ? " ($(join(["$k=$v" for (k, v) in counts], ", ")))" : "") * ".")
        catch e
            on_log("[QC] could not compute segment QC: $e")
        end
    end

    Dict{String,Any}("outputValueName"  => out_value_name,
                     "labelValueName"   => out_value_name,
                     "labelFiles"       => label_files)
end
