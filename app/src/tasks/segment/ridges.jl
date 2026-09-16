struct Ridges <: CciaTask end

# Ridge segmentation of curvilinear structures (collagen fibres in SHG, other label-free structure
# in THG). Wraps `skimage.filters.{meijering,sato,frangi}` — three Hessian-family filters that
# measure ridge-ness per pixel — followed by an Otsu / manual threshold and connected-component
# labelling. Runs per T frame, either per-Z or on a Z-MIP, and streams into a pre-created labels
# store so the viewer can watch it.
#
# Why three filters, not one: they trade recall for background suppression differently, and which one
# wins depends on the data. Meijering was the ranking winner on Unimelb 3P SHG on Cohen's d and
# fibre/bg ratio; Sato tied it; Frangi was sparse-but-clean.
#
# Not used: coastal / cellpose / any learned model. See docs/archive/shg-optical-flow-viability-prompt.md
# (colleague probe) — coastal's flow-warp supervision is a noise-driven signal on this data.

Base.@kwdef struct RidgesParams
    valueName::String              = VERSIONED_DEFAULT_VAL
    outputValueName::String        = "ridges"
    channel::Any                   = nothing
    filter::String                 = "meijering"
    sigmaMinPx::Int                = 1
    sigmaMaxPx::Int                = 5
    threshold::Float64             = 0.0
    darkRidges::Bool               = false
    perZ::Bool                     = true
    minSizePx::Int                 = 5
end

function parse_ridges_params(d::AbstractDict)::RidgesParams
    RidgesParams(;
        valueName       = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        outputValueName = string(get(d, "outputValueName", "ridges")),
        channel         = get(d, "channel", nothing),
        filter          = string(get(d, "filter", "meijering")),
        sigmaMinPx      = Int(get(d, "sigmaMinPx", 1)),
        sigmaMaxPx      = Int(get(d, "sigmaMaxPx", 5)),
        threshold       = Float64(get(d, "threshold", 0.0)),
        darkRidges      = Bool(get(d, "darkRidges", false)),
        perZ            = Bool(get(d, "perZ", true)),
        minSizePx       = Int(get(d, "minSizePx", 5)),
    )
end

const _RIDGES_FILTERS = ("meijering", "sato", "frangi")

# NOT overloading `live_outputs`: the runner assembles the label stack in RAM and writes it once
# at the end (same shape as `segment.branching`), so there is no partial store for a viewer to
# watch mid-run. Overloading would point the viewer at a `.partial` path that stays empty until the
# whole task finishes.

# Task preview runs the same compute on the visible region. `preview_params` translates the
# frontend's channel NAME to a 0-based index, since the preview worker doesn't have the image handle
# to resolve names itself — same reason as cellpose's preview_params.
task_previewable(::Ridges) = true

function preview_params(::Ridges, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    out = Dict{String,Any}(String(k) => v for (k, v) in params)
    raw = read_ccid_raw(state_file(img))
    ch_names = ccid_channel_names(raw)
    idxs = channel_indices(get(params, "channel", []), ch_names; what = "channel")
    # `channelIndex` is what the worker expects — the JSON widget's list-shape stays behind.
    out["channelIndex"] = isempty(idxs) ? nothing : idxs[1]
    out
end

function _run_task(task::Ridges, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    p    = parse_ridges_params(params)
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    if !(p.filter in _RIDGES_FILTERS)
        on_log("[ERROR] Unknown filter '$(p.filter)'; expected one of $(join(_RIDGES_FILTERS, ", "))")
        return nothing
    end

    if p.sigmaMinPx <= 0 || p.sigmaMaxPx < p.sigmaMinPx
        on_log("[ERROR] Invalid sigma range: min=$(p.sigmaMinPx) max=$(p.sigmaMaxPx)")
        return nothing
    end

    # Channel names live on the DEFAULT version and fall back through versioned_get_field. Resolving
    # here (not in the runner) keeps the runner in array space — same rule branching.jl follows.
    # `channel` is a channelSelection (multiple=false) → list-shape, one or zero entries.
    ch_names = ccid_channel_names(raw)
    ch_idxs  = channel_indices(p.channel, ch_names; what = "channel")
    if isempty(ch_idxs)
        on_log("[ERROR] No channel selected — pick the fibre channel (typically SHG)")
        return nothing
    end
    ch_index = ch_idxs[1]

    filename = versioned_get_field(raw, "filepath", p.valueName)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
        return nothing
    end
    im_path  = joinpath(dirname(dirname(img._dir)), "0", img.uid, string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    task_dir     = img._dir
    label_files  = segment_label_files(p.outputValueName, nothing)  # ["{vn}.zarr"]
    labels_out   = joinpath(task_dir, "labels", first(label_files))
    qc_out_path  = joinpath(task_run_dir(task_dir), "ridges_qc.json")

    on_log("[INFO] Ridge filter: $(p.filter)   sigmas $(p.sigmaMinPx)..$(p.sigmaMaxPx) px")
    on_log("[INFO] Output labels: $labels_out")

    ok = run_py("tasks/segment/ridges_run.py",
        (; imPath          = im_path,
           labelsOutPath   = labels_out,
           qcOutPath       = qc_out_path,
           channelIndex    = ch_index,
           filter          = p.filter,
           sigmaMin        = p.sigmaMinPx,
           sigmaMax        = p.sigmaMaxPx,
           threshold       = p.threshold,
           darkRidges      = p.darkRidges,
           perZ            = p.perZ,
           minSizePx       = p.minSizePx,
           outputValueName = p.outputValueName),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    register_label_files!(img, p.outputValueName, label_files)

    n_labels = 0
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            n_labels = Int(get(qmeta, :nLabels, 0))
            findings, _ = segment_qc_findings(Dict{String,Any}("base" => n_labels))
            metrics = Dict{String,Any}(
                "nLabels" => n_labels,
                "byType"  => Dict{String,Any}("base" => n_labels))
            write_qc(img, "segment.ridges", p.outputValueName, findings; metrics = metrics)
            on_log("[QC] $n_labels ridge component(s).")
        catch e
            on_log("[QC] could not compute ridges QC: $e")
        end
    end

    Dict{String,Any}("outputValueName" => p.outputValueName,
                     "labelFile"       => first(label_files),
                     "nLabels"         => n_labels)
end
