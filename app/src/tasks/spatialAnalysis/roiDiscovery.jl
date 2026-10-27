# ── spatialAnalysis.roiDiscovery — where do the objects of one population gather? ──────────────
#
# docs/todo/ROI_TRIAGE_PLAN.md. Generic: the QUESTION is the population — segmentation + gating,
# filter populations on any measure (e.g. fragments inside a macrophage, from contactsMeshes'
# containment columns) — so the task needs no cue of its own. It pools the population's objects over
# fixed 12-frame windows, finds groups in each (HDBSCAN: no radius) and ranks them by summed object
# weight (1 each, or the mean percentile of the measures picked to rank by): `rois/{suffix}.json`.
# No tracking, no pixels.
#
# Division of labour: Julia resolves the population to a label list (`pop_df_multi`, so membership
# composes with gating); Python (`roi_discovery_run.py` + `python/cecelia/utils/roi_utils.py`) does
# the maths. The ROI set carries its own review labels (`reviewLabels`), so review fits the question.

struct RoiDiscovery <: CciaTask end

const ROI_LABELS_DEFAULT = "of interest, artefact, unsure"
const ROI_LABELS_MAX = 12
const ROI_LABEL_MAX_CHARS = 40

Base.@kwdef struct RoiDiscoveryParams
    pops::Vector{String}   = String[]
    rankBy::Vector{String} = String[]
    minObjects::Int        = 20
    maxRois::Int           = 50
    reviewLabels::String   = ROI_LABELS_DEFAULT
    roisSuffix::String     = "default"
end

function parse_roi_discovery_params(d::AbstractDict)::RoiDiscoveryParams
    RoiDiscoveryParams(;
        pops         = _str_list(d, "pops"),
        rankBy       = _str_list(d, "rankBy"),
        minObjects   = Int(get(d, "minObjects", 20)),
        maxRois      = Int(get(d, "maxRois", 50)),
        reviewLabels = string(something(get(d, "reviewLabels", ROI_LABELS_DEFAULT), "")),
        roisSuffix   = string(get(d, "roisSuffix", "default")))
end

"""
    roi_review_labels(text) -> Vector{String}

The comma-separated review labels of a "Find ROIs" run, trimmed, de-duplicated, in order; at most
$(ROI_LABELS_MAX), each at most $(ROI_LABEL_MAX_CHARS) characters (longer ones are cut).
"""
function roi_review_labels(text::AbstractString)::Vector{String}
    out = String[]
    for raw in split(text, ",")
        l = first(strip(raw), ROI_LABEL_MAX_CHARS)
        (isempty(l) || l in out) && continue
        push!(out, String(l))
        length(out) == ROI_LABELS_MAX && break
    end
    out
end

# Pure QC (advisory, never gates). No ROIs is the unambiguous bad case.
function _roi_discovery_findings(n_objects::Integer, n_rois::Integer)
    n_objects == 0 && return [qc_finding("warn", "rois.no_objects", "No objects",
        "Check the population: it resolved no objects.")]
    n_rois == 0 ? [qc_finding("warn", "rois.none_found", "No ROIs found",
        "Lower the minimum objects, or widen the population.")] : Dict{String,Any}[]
end

function _run_task(::RoiDiscovery, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_roi_discovery_params(params)
    isempty(p.pops) && (on_log("[ERROR] roiDiscovery: select a population"); return nothing)
    isempty(p.roisSuffix) && (on_log("[ERROR] roiDiscovery: name the ROI set"); return nothing)
    p.minObjects >= 2 || (on_log("[ERROR] roiDiscovery: minimum objects must be at least 2"); return nothing)
    vn = pops_value_name(p.pops)
    haskey(img.labels, vn) || (on_log("[ERROR] roiDiscovery: no label set $(vn)"); return nothing)
    on_progress(0, 1)

    # one segmentation: label ids collide across segmentations, and the geometry is one label store
    labels = Int.(pop_df_multi(img, p.pops; value_name = vn, granularity = :cell, restrict_to = vn).label)
    isempty(labels) && (on_log("[ERROR] roiDiscovery: no objects in $(p.pops)"); return nothing)

    rois_path = img_rois_path(img, p.roisSuffix)
    mkpath(dirname(rois_path))
    qc_out_path = joinpath(task_run_dir(img._dir), "roi_discovery_qc.json")
    on_log("[INFO] roiDiscovery: $(length(labels)) objects in $(join(p.pops, ", ")), groups of ≥$(p.minObjects) per window" *
           (isempty(p.rankBy) ? "" : ", ranked by $(join(p.rankBy, ", "))") * " → rois/$(p.roisSuffix).json")

    task_params = Dict{String,Any}(
        "propsPath" => img_label_props_path(img, vn), "labelsPath" => img_labels_path(img, vn),
        "imPath" => something(img_filepath(img), ""), "roisPath" => rois_path, "qcOutPath" => qc_out_path,
        "suffix" => p.roisSuffix, "valueName" => vn, "labels" => labels, "rankBy" => p.rankBy,
        "minObjects" => p.minObjects, "maxRois" => p.maxRois,
        "reviewLabels" => roi_review_labels(p.reviewLabels),
        # the user's params, verbatim, so the ROI set records how it was made (reproducible)
        "taskParams" => Dict{String,Any}(String(k) => v for (k, v) in params))

    ok = run_py("tasks/spatialAnalysis/roi_discovery_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] roiDiscovery: Python runner failed"); return nothing)

    n_rois = 0
    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_obj = Int(get(qc, "nObjects", 0)); n_rois = Int(get(qc, "nRois", 0))
        metrics = Dict{String,Any}("nObjects" => n_obj, "nRois" => n_rois,
                                   "nGrouped" => Int(get(qc, "nGrouped", 0)), "nSites" => Int(get(qc, "nSites", 0)))
        write_qc(img, "spatialAnalysis.roiDiscovery", p.roisSuffix, _roi_discovery_findings(n_obj, n_rois);
                 metrics = metrics)
        on_log("[QC] $(n_rois) ROI(s) from $(n_obj) object(s).")
    catch e
        on_log("[QC] could not compute roiDiscovery QC: $e")
    end
    on_log("[INFO] roiDiscovery done → $(basename(rois_path))")
    Dict{String,Any}("roisPath" => rois_path, "nRois" => n_rois, "suffix" => p.roisSuffix)
end
