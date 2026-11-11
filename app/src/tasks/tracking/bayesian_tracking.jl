struct BayesianTracking <: CciaTask end

# Typed shape of what `_run_task(::BayesianTracking, …)` reads from `params`. Names mirror the
# btrack Bayesian tracker's inputs (accuracy / probToAssign / noise* / lambda* / theta* / *Thresh);
# defaults line up with the spec JSON so an omitted key parses identically to what the handler
# used to fall back to.
Base.@kwdef struct BayesianTrackingParams
    valueName::String            = VERSIONED_DEFAULT_VAL
    popsToTrack::String          = "NONE"
    trackSourceForce::Bool       = false
    maxSearchRadius::Int         = 20
    maxLost::Int                 = 3
    trackBranching::Bool         = false
    minTimepoints::Int           = 5
    accuracy::Float64            = 0.8
    probToAssign::Float64        = 0.8
    noiseInital::Int             = 300
    noiseProcessing::Int         = 100
    noiseMeasurements::Int       = 100
    distThresh::Float64          = 10.0
    timeThresh::Int              = 5
    segmentationMissRate::Float64 = 0.1
    lambdaLink::Int              = 5
    lambdaBranch::Int            = 50
    lambdaTime::Int              = 5
    lambdaDist::Float64          = 5.0
    thetaTime::Int               = 5
    thetaDist::Float64           = 5.0
end

function parse_bayesian_tracking_params(d::AbstractDict)::BayesianTrackingParams
    BayesianTrackingParams(;
        valueName            = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        popsToTrack          = string(get(d, "popsToTrack", "NONE")),
        trackSourceForce     = Bool(get(d, "trackSourceForce", false)),
        maxSearchRadius      = Int(get(d, "maxSearchRadius", 20)),
        maxLost              = Int(get(d, "maxLost", 3)),
        trackBranching       = Bool(get(d, "trackBranching", false)),
        minTimepoints        = Int(get(d, "minTimepoints", 5)),
        accuracy             = Float64(get(d, "accuracy", 0.8)),
        probToAssign         = Float64(get(d, "probToAssign", 0.8)),
        noiseInital          = Int(get(d, "noiseInital", 300)),
        noiseProcessing      = Int(get(d, "noiseProcessing", 100)),
        noiseMeasurements    = Int(get(d, "noiseMeasurements", 100)),
        distThresh           = Float64(get(d, "distThresh", 10.0)),
        timeThresh           = Int(get(d, "timeThresh", 5)),
        segmentationMissRate = Float64(get(d, "segmentationMissRate", 0.1)),
        lambdaLink           = Int(get(d, "lambdaLink", 5)),
        lambdaBranch         = Int(get(d, "lambdaBranch", 50)),
        lambdaTime           = Int(get(d, "lambdaTime", 5)),
        lambdaDist           = Float64(get(d, "lambdaDist", 5.0)),
        thetaTime            = Int(get(d, "thetaTime", 5)),
        thetaDist            = Float64(get(d, "thetaDist", 5.0)))
end

# Bayesian (btrack) cell tracking. Ports old-R-shiny bayesianTracking.R.
#
# Tracks either the whole segmentation or a gated flow population. Membership for the
# gated case is computed IN-PROCESS — Julia is the sole gate evaluator (docs/POPULATION.md),
# so we hand the label-ID list directly to Python (no CSV / no HTTP callback). btrack runs
# in the Python subprocess and writes the lineage columns (track_id, track_parent,
# track_root, track_state, track_generation, cell_id) back into the segmentation's
# labelProps/{valueName}.h5ad obs. No track measures / filters are computed here — gating
# on track properties is a later phase (docs/POPULATION.md).
function _run_task(task::BayesianTracking, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    p        = parse_bayesian_tracking_params(params)
    task_dir = img._dir
    props_path = img_label_props_path(img, p.valueName)
    if !isfile(props_path)
        on_log("[ERROR] No labelProps for valueName='$(p.valueName)': $props_path")
        return nothing
    end

    # Resolve gated-population membership in-process (Julia is the gate evaluator).
    label_ids = nothing
    # `trackSource` is the STABLE key `_write_back` uses to delete this pop's rows on a re-run —
    # the pop's UID (unchanged by rename/move) for a gated run, `WHOLE_SEG_TRACK_SOURCE` when tracking
    # the whole segmentation. See MULTI_POP_TRACKING_PLAN.md Decision 1.
    track_source = WHOLE_SEG_TRACK_SOURCE
    # Load the flow pop map up-front (even for a whole-seg run) so the P3 orphan sweep in
    # `_write_back` can see the current live UIDs and NaN any row still stamped by a deleted pop's
    # `track_source` — see MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 3. No sidecar → no live pops,
    # every non-whole_seg row is an orphan; `nothing` there disables the sweep entirely on the
    # Python side, so a legacy tracking task that never emits the param still lands somewhere sane.
    m = try
        load_pop_map(img; value_name = p.valueName, pop_type = "flow")
    catch _
        nothing
    end
    # Shadow the params struct only inside the comprehension so `p` still refers to it below.
    live_track_sources = m === nothing ? nothing : [pop_uid(m, path) for path in pop_paths(m)]
    if p.popsToTrack != "NONE"
        m === nothing && (on_log("[ERROR] No gating sidecar for value_name='$(p.valueName)'"); return nothing)
        if !has_pop(m, p.popsToTrack)
            on_log("[ERROR] Population not found in gating/$(p.valueName).json: $(p.popsToTrack)")
            return nothing
        end
        recompute!(m, pop_membership_fetch(img, p.valueName, "flow"))
        label_ids = collect(Int, cells_in_pop(m, p.popsToTrack))
        track_source = pop_uid(m, p.popsToTrack)
        on_log("[INFO] Tracking $(length(label_ids)) cells from population '$(p.popsToTrack)' (uid=$track_source)")
        if isempty(label_ids)
            on_log("[ERROR] Population '$(p.popsToTrack)' is empty — nothing to track")
            return nothing
        end
    else
        on_log("[INFO] Tracking whole segmentation '$(p.valueName)'")
    end

    on_log("[INFO] Tracking labelProps: $props_path")

    # µm per pixel, skimage order [sz, sy, sx] — the same accessor every other spatial task uses
    # (cellNeighbours, the mesh tasks, track_measures). Tracking was the only one not calling it, so
    # the linking ran in pixels while the measures computed on its own output ran in µm.
    (pixel_res, _time_step) = img_physical_sizes(img)

    ok = run_py("tasks/tracking/bayesian_tracking_run.py",
        (; taskDir              = task_dir,
           physicalSizes        = pixel_res,
           valueName            = p.valueName,
           labelIds             = label_ids,                          # null = whole segmentation
           trackSource          = track_source,                       # pop UID or "whole_seg"
           # Override the P1 conflict detector: allow writing over labels currently owned by a
           # different pop's track_source. Only for the intentional pop→pop refinement idiom; the
           # whole-seg→pop case doesn't need it (whole_seg is treated as bypass in the detector).
           # Not exposed as a param widget yet — wired for future use.
           trackSourceForce     = p.trackSourceForce,
           # Live pop UIDs seen when this run launched. `nothing` (no sidecar loaded) disables the
           # P3 sweep — matches the legacy behaviour. See MULTI_POP_TRACKING_ORPHANS_PLAN decision 3.
           liveTrackSources     = live_track_sources,
           maxSearchRadius      = p.maxSearchRadius,
           maxLost              = p.maxLost,
           trackBranching       = p.trackBranching,
           minTimepoints        = p.minTimepoints,
           accuracy             = p.accuracy,
           probToAssign         = p.probToAssign,
           noiseInital          = p.noiseInital,
           noiseProcessing      = p.noiseProcessing,
           noiseMeasurements    = p.noiseMeasurements,
           distThresh           = p.distThresh,
           timeThresh           = p.timeThresh,
           segmentationMissRate = p.segmentationMissRate,
           lambdaLink           = p.lambdaLink,
           lambdaBranch         = p.lambdaBranch,
           lambdaTime           = p.lambdaTime,
           lambdaDist           = p.lambdaDist,
           thetaTime            = p.thetaTime,
           thetaDist            = p.thetaDist),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Tracking complete.")

    # The write-back renumbered track ids — this source's new tracks AND every surviving source's
    # (compacted to 1..N) — so a per-track table from before joins its measures and track clusters
    # onto the wrong tracks (`track_props` joins on track_id). Remove it with its sidecar, as a
    # track-set delete with no survivors does (`_tracks_delete!`); track measures rebuild it.
    tpath = img_track_props_path(img, p.valueName)
    if isfile(tpath)
        _rm_if(tpath; on_log); _rm_if(_clustfeatures_path(tpath); on_log)
        on_log("[INFO] Track ids changed — run track measures to rebuild the per-track table")
    end

    _tracking_qc!(img, p.valueName, props_path, track_source; on_log)
    Dict{String,Any}("valueName" => p.valueName)
end

# QC (advisory): bank the track count + mean track length of the tracks THIS run wrote (rows stamped
# with its `track_source`, read through `label_track_sources`) — another pop's tracks on the
# same segmentation are not this run's. 0 tracks (btrack linked nothing) is the one unambiguous
# problem → an advisory finding; the counts are always recorded as metrics for cohort stats.
function _tracking_qc!(img::CciaImage, vn::AbstractString, props_path::AbstractString,
                       track_source::AbstractString; on_log::Function = _ -> nothing)
    try
        # the write-back always stamps `track_source`, so this run's rows are exactly those carrying it
        src   = label_track_sources(img, vn)
        cells = label_props(props_path) |> select_cols(["track_id"]) |> as_df
        tids  = [cells.track_id[i] for i in eachindex(cells.track_id)
                 if get(src, Int(cells.label[i]), nothing) == track_source]
        n_tracks, mean_len, n_tracked = track_count_metrics(tids)
        findings = n_tracks == 0 ?
            [qc_finding("warn", "tracking.no_tracks", "No tracks formed",
                "btrack linked no cells into tracks — check segmentation continuity and the tracking parameters, then re-run.")] :
            Dict{String,Any}[]
        write_qc(img, "tracking.bayesian_tracking", vn, findings;
                 metrics = Dict{String,Any}("nTracks"         => n_tracks,
                                            "meanTrackLength" => round(mean_len, digits = 2),
                                            "nTrackedCells"   => n_tracked))
        on_log(n_tracks == 0 ? "[QC] no tracks formed — see the image's QC badge." :
               "[QC] $n_tracks track(s), mean length $(round(mean_len, digits = 1)) frames.")
    catch e
        on_log("[QC] could not compute tracking QC: $e")
    end
end
