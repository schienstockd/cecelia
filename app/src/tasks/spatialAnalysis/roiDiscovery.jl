# ── spatialAnalysis.roiDiscovery — find regions where something happens to fragments ──────────
#
# docs/todo/ROI_TRIAGE_PLAN.md. Candidate objects (fragments) are scored per frame on five cues —
# containment in / proximity to a thresholded mask channel (CD169 macrophages), local density,
# density change against t±1, "naked" (in the nuclear population, not the membrane one) and proximity
# to a partner population — then clustered in (x, y, z, t) into ranked ROIs: `rois/{suffix}.json`.
# No tracking and no macrophage objects (Decisions 1–2): every cue is per frame, the mask is a mask.
#
# Division of labour: Julia resolves the populations to label lists (`pop_df_multi`, so membership
# composes with gating) and the mask channel NAME to an index; Python
# (`roi_discovery_run.py` + `python/cecelia/utils/roi_utils.py`) does the pixels and the maths.

using DataFrames: nrow

struct RoiDiscovery <: CciaTask end

Base.@kwdef struct RoiDiscoveryParams
    candidatePops::Vector{String} = String[]
    partnerPops::Vector{String}   = String[]
    nucPops::Vector{String}       = String[]
    memPops::Vector{String}       = String[]
    maskChannel::Any              = nothing
    maskThreshold::Float64        = 0.0
    maskSmoothUm::Float64         = 0.5
    maskMinVolumeUm3::Float64     = 50.0
    densityRadiusUm::Float64      = 10.0
    contactScaleUm::Float64       = 2.0
    partnerScaleUm::Float64       = 3.0
    wContact::Float64             = 2.0
    wDensity::Float64             = 1.0
    wTurnover::Float64            = 1.0
    wNaked::Float64               = 1.0
    wPartner::Float64             = 0.5
    candidateQuantile::Float64    = 0.95
    clusterRadiusUm::Float64      = 6.0
    timeScaleUm::Float64          = 2.0
    windowFrames::Int             = 12
    minObjects::Int               = 5
    maxRois::Int                  = 50
    paddingUm::Float64            = 5.0
    roisSuffix::String            = "default"
end

_roi_f(d, k, def) = Float64(get(d, k, def))

function parse_roi_discovery_params(d::AbstractDict)::RoiDiscoveryParams
    RoiDiscoveryParams(;
        candidatePops     = _str_list(d, "candidatePops"),
        partnerPops       = _str_list(d, "partnerPops"),
        nucPops           = _str_list(d, "nucPops"),
        memPops           = _str_list(d, "memPops"),
        maskChannel       = get(d, "maskChannel", nothing),
        maskThreshold     = _roi_f(d, "maskThreshold", 0.0),
        maskSmoothUm      = _roi_f(d, "maskSmoothUm", 0.5),
        maskMinVolumeUm3  = _roi_f(d, "maskMinVolumeUm3", 50.0),
        densityRadiusUm   = _roi_f(d, "densityRadiusUm", 10.0),
        contactScaleUm    = _roi_f(d, "contactScaleUm", 2.0),
        partnerScaleUm    = _roi_f(d, "partnerScaleUm", 3.0),
        wContact          = _roi_f(d, "wContact", 2.0),
        wDensity          = _roi_f(d, "wDensity", 1.0),
        wTurnover         = _roi_f(d, "wTurnover", 1.0),
        wNaked            = _roi_f(d, "wNaked", 1.0),
        wPartner          = _roi_f(d, "wPartner", 0.5),
        candidateQuantile = _roi_f(d, "candidateQuantile", 0.95),
        clusterRadiusUm   = _roi_f(d, "clusterRadiusUm", 6.0),
        timeScaleUm       = _roi_f(d, "timeScaleUm", 2.0),
        windowFrames      = Int(get(d, "windowFrames", 12)),
        minObjects        = Int(get(d, "minObjects", 5)),
        maxRois           = Int(get(d, "maxRois", 50)),
        paddingUm         = _roi_f(d, "paddingUm", 5.0),
        roisSuffix        = string(get(d, "roisSuffix", "default")))
end

# The cue weights the runner reads, keyed by `roi_utils.CUES`. A cue whose input is missing (no mask
# channel, no partner / nuc pops) gets weight 0, so it can't take a share of the score it can't earn.
function _roi_weights(p::RoiDiscoveryParams; has_mask::Bool, has_partner::Bool, has_naked::Bool)
    Dict{String,Float64}(
        "contact"  => has_mask ? p.wContact : 0.0,
        "density"  => p.wDensity,
        "turnover" => p.wTurnover,
        "naked"    => has_naked ? p.wNaked : 0.0,
        "partner"  => has_partner ? p.wPartner : 0.0)
end

# Pure QC (advisory, never gates). No ROIs is the unambiguous bad case; a mask covering a large share
# of the volume means the threshold is catching background or bleed-through (plan §4), which makes
# every object a "contact".
function _roi_discovery_findings(n_candidates::Integer, n_rois::Integer, mask_coverage)
    n_candidates == 0 && return [qc_finding("warn", "rois.no_candidates", "No candidate objects",
        "Check the candidate population — it resolved no objects.")]
    findings = Dict{String,Any}[]
    n_rois == 0 && push!(findings, qc_finding("warn", "rois.none_found", "No ROIs found",
        "Lower the candidate quantile or the minimum objects, or widen the cluster radius."))
    if mask_coverage !== nothing && mask_coverage > 0.2
        push!(findings, qc_finding("warn", "rois.mask_too_large", "Mask covers much of the image",
            "Raise the mask threshold: bleed-through is making most objects read as in contact.";
            detail = "$(round(Int, mask_coverage * 100))% of voxels in the mask"))
    end
    findings
end

function _run_task(::RoiDiscovery, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_roi_discovery_params(params)
    isempty(p.candidatePops) && (on_log("[ERROR] roiDiscovery: select the candidate population"); return nothing)
    isempty(p.roisSuffix) && (on_log("[ERROR] roiDiscovery: name the ROI set"); return nothing)
    (0.0 <= p.candidateQuantile < 1.0) ||
        (on_log("[ERROR] roiDiscovery: candidate quantile must be in [0, 1)"); return nothing)
    vn = pops_value_name(p.candidatePops)
    haskey(img.labels, vn) || (on_log("[ERROR] roiDiscovery: no label set $(vn)"); return nothing)
    on_progress(0, 1)

    # every population is resolved WITHIN the candidates' segmentation: label ids collide across
    # segmentations, and the runner reads one label store
    labels_of(pops) = isempty(pops) ? Int[] :
        Int.(pop_df_multi(img, pops; value_name = vn, granularity = :cell, restrict_to = vn).label)
    cand = labels_of(p.candidatePops)
    isempty(cand) && (on_log("[ERROR] roiDiscovery: no objects in $(p.candidatePops)"); return nothing)
    partners = labels_of(p.partnerPops)
    nuc = labels_of(p.nucPops)
    mem = labels_of(p.memPops)

    names = something(channel_names(img), String[])
    mask_idx = channel_indices(p.maskChannel, names; what = "mask channel")
    mask_ch = isempty(mask_idx) ? -1 : mask_idx[1]
    im_path = img_filepath(img)
    (mask_ch < 0 || (im_path !== nothing && ispath(im_path))) ||
        (on_log("[ERROR] roiDiscovery: image store not found for the mask channel"); return nothing)

    weights = _roi_weights(p; has_mask = mask_ch >= 0, has_partner = !isempty(partners),
                           has_naked = !isempty(nuc))
    sum(values(weights)) > 0 || (on_log("[ERROR] roiDiscovery: every usable cue has weight 0"); return nothing)

    rois_path = img_rois_path(img, p.roisSuffix)
    mkpath(dirname(rois_path))
    qc_out_path = joinpath(task_run_dir(img._dir), "roi_discovery_qc.json")
    on_log("[INFO] roiDiscovery: $(length(cand)) candidates in $(vn), $(length(partners)) partners, " *
           "mask channel $(mask_ch < 0 ? "none" : names[mask_ch + 1]) → rois/$(p.roisSuffix).json")

    task_params = Dict{String,Any}(
        "propsPath" => img_label_props_path(img, vn), "labelsPath" => img_labels_path(img, vn),
        "imPath" => something(im_path, ""), "roisPath" => rois_path, "qcOutPath" => qc_out_path,
        "suffix" => p.roisSuffix, "valueName" => vn,
        "candidateLabels" => cand, "partnerLabels" => partners, "nucLabels" => nuc, "memLabels" => mem,
        "maskChannel" => mask_ch, "weights" => weights,
        "maskThreshold" => p.maskThreshold, "maskSmoothUm" => p.maskSmoothUm,
        "maskMinVolumeUm3" => p.maskMinVolumeUm3, "densityRadiusUm" => p.densityRadiusUm,
        "contactScaleUm" => p.contactScaleUm, "partnerScaleUm" => p.partnerScaleUm,
        "candidateQuantile" => p.candidateQuantile, "clusterRadiusUm" => p.clusterRadiusUm,
        "timeScaleUm" => p.timeScaleUm, "windowFrames" => p.windowFrames,
        "minObjects" => p.minObjects, "maxRois" => p.maxRois, "paddingUm" => p.paddingUm,
        # the user's params, verbatim, so the ROI set records how it was made (reproducible)
        "taskParams" => Dict{String,Any}(String(k) => v for (k, v) in params))

    ok = run_py("tasks/spatialAnalysis/roi_discovery_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] roiDiscovery: Python runner failed"); return nothing)

    n_rois = 0
    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cand = Int(get(qc, "nCandidates", 0)); n_rois = Int(get(qc, "nRois", 0))
        cov = get(qc, "maskCoverage", nothing)
        findings = _roi_discovery_findings(n_cand, n_rois, cov === nothing ? nothing : Float64(cov))
        metrics = Dict{String,Any}("nCandidates" => n_cand, "nRois" => n_rois,
                                   "nHigh" => Int(get(qc, "nHigh", 0)))
        cov === nothing || (metrics["maskCoverage"] = Float64(cov))   # no mask channel → no metric
        write_qc(img, "spatialAnalysis.roiDiscovery", p.roisSuffix, findings; metrics = metrics)
        on_log("[QC] $(n_rois) ROI(s) from $(n_cand) candidate(s).")
    catch e
        on_log("[QC] could not compute roiDiscovery QC: $e")
    end
    on_log("[INFO] roiDiscovery done → $(basename(rois_path))")
    Dict{String,Any}("roisPath" => rois_path, "nRois" => n_rois, "suffix" => p.roisSuffix)
end
