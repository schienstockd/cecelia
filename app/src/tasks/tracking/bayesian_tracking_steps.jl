# Steps of `tracking.bayesian_tracking` (`bayesian_tracking.jl`) around the btrack runs: which pops
# a run tracks on its segmentation, and the QC of what it wrote.

# The pop paths a run tracks, on its segmentation `vn`: `String[]` = the whole segmentation, or an
# error message. Every ref must be on `vn` — the Segmentation dropdown names where the tracks are
# written, so a pop picked from another segmentation would track nothing there; say so instead.
function _tracking_targets(refs::Vector{String}, vn::AbstractString)::Union{Vector{String},String}
    paths = String[]
    for ref in refs
        rvn, path = _split_pop_ref(ref, vn)
        rvn == vn || return "'$ref' is on segmentation '$rvn', but Segmentation is '$vn' — pick populations of '$vn'"
        path in paths || push!(paths, path)
    end
    roots = filter(is_root, paths)
    isempty(roots) && return paths
    length(paths) == 1 && return String[]                 # the all-cells root alone = whole segmentation
    "'$vn/' (all cells) overlaps every other pick — track it alone, or pick only populations"
end

# One btrack run over `label_ids` (`nothing` = the whole segmentation), written back under
# `track_source`. `true` when the Python subprocess succeeded.
function _btrack_run(p, task_dir::AbstractString, pixel_res, label_ids, track_source::AbstractString,
                     live_track_sources; on_log::Function, on_process::Function, on_progress::Function)::Bool
    run_py("tasks/tracking/bayesian_tracking_run.py",
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
        on_log = on_log, on_process = on_process, on_progress = on_progress)
end

# After a write-back: it dropped every `live.cell.*` (HMM states too) and renumbered track ids — this
# source's new tracks AND every surviving source's (compacted to 1..N) — so a per-track table from
# before joins its measures and track clusters onto the wrong tracks (`track_props` joins on
# track_id). Remove it with its sidecar, as a track-set delete with no survivors does
# (`_tracks_delete!`); track measures rebuild it.
function _after_write_back!(img::CciaImage, vn::AbstractString, props_path::AbstractString;
                            on_log::Function = _ -> nothing)
    prune_stale_hmmfit!(props_path; on_log)
    tpath = img_track_props_path(img, vn)
    if isfile(tpath)
        _rm_if(tpath; on_log); _rm_if(_clustfeatures_path(tpath); on_log)
        on_log("[INFO] Track ids changed — run track measures to rebuild the per-track table")
    end
end
