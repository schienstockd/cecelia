struct ImportLabels <: CciaTask end

# Import a label image made outside Cecelia (OME-Zarr or TIFF) onto an existing image, as a label set
# no different from one a segmenter wrote: `labels/{vn}.zarr` (+ `{vn}_nuc.zarr`), uint32, shaped like
# the image minus C, ids unique across timepoints, the image's pyramid depth. The streaming work is
# `cecelia.utils.label_import_utils`; this handler resolves paths, registers the stores and banks QC —
# the same tail as every segmenter (segmentation.jl). Run `segment.importLabelsMeasure` to get the
# cell table as well. See docs/SEGMENTATION.md → *Importing labels*.

Base.@kwdef struct ImportLabelsParams
    valueName::String              = VERSIONED_DEFAULT_VAL
    outputValueName::String        = VERSIONED_DEFAULT_VAL
    labelsPath::String             = ""
    nucLabelsPath::String          = ""
    matchIds::Bool                 = false
    matchThreshold::Float64        = 0.3
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_import_labels_params(d::AbstractDict)::ImportLabelsParams
    ImportLabelsParams(;
        valueName       = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        outputValueName = string(get(d, "outputValueName", VERSIONED_DEFAULT_VAL)),
        labelsPath      = strip(string(something(get(d, "labelsPath", ""), ""))),
        nucLabelsPath   = strip(string(something(get(d, "nucLabelsPath", ""), ""))),
        matchIds        = Bool(get(d, "matchIds", false)),
        matchThreshold  = Float64(get(d, "matchThreshold", 0.3)),
        version = parse_version_pin(d))
end

"""
    import_label_sources(p) -> Dict{String,String}

The source path per `matchAs` role: `base` always, `nuc` when a nucleus image was given. The role keys
are what `segment_label_files` turns into store names, so the import writes exactly the files a
segmenter with the same roles would.
"""
function import_label_sources(p::ImportLabelsParams)::Dict{String,String}
    src = Dict{String,String}("base" => expand_user(p.labelsPath))
    isempty(p.nucLabelsPath) || (src["nuc"] = expand_user(p.nucLabelsPath))
    src
end

# Share of nucleus pixels inside a cell whose id is not that cell's, above which an import with
# matching OFF is flagged. Paired ids disagree only where a nucleus crosses a cell edge; independent
# numbering disagrees almost everywhere. A placeholder from that geometry, not calibrated on real data.
const IMPORT_ID_MISMATCH_FRAC = 0.25

"""
    import_labels_qc_findings(qmeta) -> Vector{Dict}

Pure QC helper: the runner's QC JSON → a `warn` when nucleus ids were kept (matching off) but do not
name the cells they sit in — every `nuc_*` column would then land on the wrong cell, silently.
"""
function import_labels_qc_findings(qmeta)::Vector{Dict{String,Any}}
    frac = get(qmeta, :idMismatchFrac, get(qmeta, "idMismatchFrac", nothing))
    (frac isa Real && frac > IMPORT_ID_MISMATCH_FRAC) || return Dict{String,Any}[]
    [qc_finding("warn", "segment.import_ids_unpaired";
                detail = Dict{String,Any}("idMismatchFrac" => round(Float64(frac); digits = 3)))]
end

function _run_task(task::ImportLabels, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    p    = parse_import_labels_params(params)
    raw  = read_ccid_raw(state_file(img))
    assert_unversioned_field(raw, "labels", p.outputValueName)   # before the runner overwrites it

    if isempty(p.labelsPath)
        on_log("[ERROR] No cell labels given — pick the label image to import")
        return nothing
    end

    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = p.version)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
        return nothing
    end
    im_path = joinpath(img_zero_dir(img), string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    sources = import_label_sources(p)
    roles   = Dict(r => Dict("matchAs" => r) for r in keys(sources))
    label_files = segment_label_files(p.outputValueName, roles)   # base first, then {vn}_{role}.zarr
    labels_dir  = img_labels_dir(img)
    # each role's own store, through the same derivation (a one-role bag names its file last)
    out_paths   = Dict{String,String}(
        r => joinpath(labels_dir, last(segment_label_files(p.outputValueName, Dict(r => roles[r]))))
        for r in keys(sources))
    qc_out_path = joinpath(task_run_dir(img._dir), "segment_counts.json")

    for (r, path) in sort(collect(sources))
        on_log("[INFO] $r labels: $path")
    end
    on_log("[INFO] Image:  $im_path")
    on_log("[INFO] Output: $(join(label_files, ", "))" *
           (p.matchIds ? "  (nucleus ids matched to cells, IoU ≥ $(p.matchThreshold))" : ""))

    ok = run_py("tasks/segment/import_labels_run.py",
        (; imPath         = im_path,
           sources        = sources,
           outPaths       = out_paths,
           matchIds       = p.matchIds,
           matchThreshold = p.matchThreshold,
           qcOutPath      = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    register_label_files!(img, p.outputValueName, label_files)
    bank_segment_qc!(img, "segment.importLabels", p.outputValueName, qc_out_path;
                     extra_findings = import_labels_qc_findings, on_log = on_log)

    Dict{String,Any}("outputValueName" => p.outputValueName,
                     "labelValueName"  => p.outputValueName,
                     "labelFiles"      => label_files)
end
