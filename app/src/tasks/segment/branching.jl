struct Branching <: CciaTask end

# Pure QC helper: objective branch count → advisory finding for the empty-skeleton case
# (unambiguous failure mode: over-eroded input or an empty ref population).
function _branching_qc_findings(n_branches::Integer)
    findings = n_branches == 0 ?
        [qc_finding("warn", "branching.no_branches", "No branches found",
            "Lower the pre-dilation or check the segmentation, then re-run.")] :
        Dict{String,Any}[]
    findings
end

function _run_task(task::Branching, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name     = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    ref_pops       = string(get(params, "refPops", "NONE"))

    ccid = joinpath(img._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    # Resolve input image path (for physical scale — not read; scale flows via ome_xml_utils)
    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end
    im_path = joinpath(dirname(dirname(img._dir)), "0", img.uid, string(filename))

    # Resolve input labels zarr — a segmentation the user chose from `img.labels`
    if !haskey(img.labels, value_name) || isempty(img.labels[value_name])
        on_log("[ERROR] No labels registered for valueName='$value_name'")
        return nothing
    end
    labels_path = joinpath(img._dir, "labels", first(img.labels[value_name]))
    if !ispath(labels_path)
        on_log("[ERROR] Input labels not found: $labels_path")
        return nothing
    end

    # Resolve refPops in Julia (Decision 7): Python receives a plain list of label IDs, never a
    # pop map. Multi-accept picker → resolve_pop_type discovers which map to load.
    label_ids = nothing
    if ref_pops != "NONE"
        vn, path = _split_pop_ref(ref_pops, value_name)
        pt = resolve_pop_type(img, vn, path)
        m = try; load_pop_map(img; value_name = vn, pop_type = pt); catch; nothing; end
        if isnothing(m) || !has_pop(m, path)
            on_log("[ERROR] Population not found for refPops='$ref_pops' (value_name=$vn, pop_type=$pt)")
            return nothing
        end
        recompute!(m, cols -> (label_props(img; value_name = vn) |>
                               lp -> select_cols(lp, cols) |> as_df))
        label_ids = collect(Int, cells_in_pop(m, path))
        on_log("[INFO] Restricting to $(length(label_ids)) label(s) from population '$ref_pops'")
        if isempty(label_ids)
            on_log("[ERROR] Population '$ref_pops' is empty — nothing to skeletonise")
            return nothing
        end
    end

    task_dir       = img._dir
    branch_zarr    = "$(out_value_name).zarr"
    branch_lbl_dir = img_branch_labels_dir(img)
    branch_lbl_out = joinpath(branch_lbl_dir, branch_zarr)
    branch_props   = img_branch_props_path(img, out_value_name)
    qc_out_path    = joinpath(task_run_dir(task_dir), "branching_counts.json")

    isdir(branch_lbl_dir) || mkpath(branch_lbl_dir)

    on_log("[INFO] Input labels:  $labels_path")
    on_log("[INFO] Branch labels: $branch_lbl_out")
    on_log("[INFO] Branch props:  $branch_props")

    ok = run_py("tasks/segment/branching_run.py",
        (; imPath              = im_path,
           labelsPath           = labels_path,
           branchLabelsOutPath  = branch_lbl_out,
           branchPropsOutPath   = branch_props,
           qcOutPath            = qc_out_path,
           labelIds             = something(label_ids, Int[]),
           preDilationSize      = Int(get(params, "preDilationSize", 2)),
           postDilationSize     = Int(get(params, "postDilationSize", 2)),
           useBorders           = Bool(get(params, "useBorders", false)),
           flattenBranching     = Bool(get(params, "flattenBranching", false))),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # Register the branch labels zarr in ccid.json under `branch_labels` — NOT `labels`. Decision 6:
    # branch labels get their own registry so the generic labels picker never lists them.
    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    bl_dict = Dict{String,Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(raw2, "branch_labels", Dict{String,Any}()))
    bl_dict[out_value_name] = [branch_zarr]
    raw2["branch_labels"] = bl_dict
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    # QC (advisory): objective branch count + zero-branches warning
    n_branches = 0
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            n_branches = Int(get(qmeta, :nBranches, 0))
            n_skeletons = Int(get(qmeta, :nSkeletons, 0))
            mean_branch_length = Float64(get(qmeta, :meanBranchLength, 0.0))
            findings = _branching_qc_findings(n_branches)
            write_qc(img, "segment.branching", out_value_name, findings;
                     metrics = Dict{String,Any}("nBranches"        => n_branches,
                                                 "nSkeletons"       => n_skeletons,
                                                 "meanBranchLength" => mean_branch_length))
            on_log("[QC] $n_branches branch(es) across $n_skeletons skeleton(s).")
        catch e
            on_log("[QC] could not compute branching QC: $e")
        end
    end

    Dict{String,Any}("outputValueName" => out_value_name,
                     "branchLabelFile" => branch_zarr,
                     "branchPropsFile" => "$(out_value_name)$(BRANCH_PROPS_SUFFIX).h5ad",
                     "nBranches"       => n_branches)
end
