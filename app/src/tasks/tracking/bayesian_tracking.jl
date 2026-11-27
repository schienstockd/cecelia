struct BayesianTracking <: CciaTask end
register_builtin_task!("tracking.bayesian_tracking", BayesianTracking(); spec = joinpath(@__DIR__, "bayesian_tracking.json"))

# Typed shape of what `_run_task(::BayesianTracking, …)` reads from `params`. Names mirror the
# btrack Bayesian tracker's inputs (accuracy / probToAssign / noise* / lambda* / theta* / *Thresh);
# defaults line up with the spec JSON so an omitted key parses identically to what the handler
# used to fall back to.
Base.@kwdef struct BayesianTrackingParams
    valueName::String            = VERSIONED_DEFAULT_VAL
    popsToTrack::Vector{String}  = String[]   # pop refs; empty = the whole segmentation
    trackSourceForce::Bool       = false
    maxSearchRadius::Float64     = 20.0  # µm: sub-µm on super-resolution data
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
        # the canonical picker's refs (`"T/qc"`, all-cells `"T/"`), or an older run's "NONE" / one
        # bare path; "NONE"/"" drop out, so an empty list IS the whole segmentation
        popsToTrack          = unique(_str_list(d, "popsToTrack")),
        trackSourceForce     = Bool(get(d, "trackSourceForce", false)),
        maxSearchRadius      = Float64(get(d, "maxSearchRadius", 20)),
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
# Tracks either the whole segmentation or one or more gated flow populations, each separately. Membership for the
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

    # Load the flow pop tree up-front (even for a whole-seg run) so the P3 orphan sweep in
    # `_write_back` can see the current live UIDs and NaN any row still stamped by a deleted pop's
    # `track_source` — see MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 3. No sidecar → no live pops,
    # every non-whole_seg row is an orphan; `nothing` there disables the sweep entirely on the
    # Python side, so a legacy tracking task that never emits the param still lands somewhere sane.
    m = try
        load_pop_tree(img; value_name = p.valueName, pop_type = "flow")
    catch _
        nothing
    end
    live_track_sources = m === nothing ? nothing : [pop_uid(m, path) for path in pop_paths(m)]

    # One btrack run per picked pop (or one over the whole segmentation), each under its own
    # `trackSource` — the STABLE key `_write_back` replaces on a re-run: the pop's UID, unchanged by
    # rename/move, or `WHOLE_SEG_TRACK_SOURCE` (MULTI_POP_TRACKING_PLAN.md Decision 1) — so one pop's
    # cells never link to another's. Membership is resolved in-process (Julia is the sole gate
    # evaluator, docs/POPULATION.md) and handed to Python as a label-ID list.
    targets = _tracking_targets(p.popsToTrack, p.valueName)
    targets isa String && (on_log("[ERROR] $targets"); return nothing)
    runs = Tuple{String,Union{Nothing,Vector{Int}},String,String}[]   # (label, label_ids, track_source, name)
    if isempty(targets)
        push!(runs, ("whole segmentation '$(p.valueName)'", nothing, WHOLE_SEG_TRACK_SOURCE, "$(p.valueName)/"))
    else
        m === nothing && (on_log("[ERROR] No gating sidecar for value_name='$(p.valueName)'"); return nothing)
        missing_pops = filter(path -> !has_pop(m, path), targets)
        if !isempty(missing_pops)
            on_log("[ERROR] Population not found in gating/$(p.valueName).json: $(join(missing_pops, ", "))")
            return nothing
        end
        recompute!(m, pop_membership_fetch(img, p.valueName, "flow"))
        members = Dict(path => collect(Int, cells_in_pop(m, path)) for path in targets)
        # A cell can carry one track only. Two selected pops sharing cells (a parent and its child,
        # or overlapping gates) would have the second run refused by the write-back's ownership check
        # after the first had already written — so refuse up front, naming the pair.
        for (i, a) in enumerate(targets), b in targets[i+1:end]
            shared = length(intersect(Set(members[a]), Set(members[b])))
            if shared > 0
                on_log("[ERROR] '$a' and '$b' share $shared cells; a cell can belong to one track. " *
                       "Pick populations that do not overlap.")
                return nothing
            end
        end
        # An empty pick fails the run (as with one pop per run): skipping it would leave its earlier
        # tracks in place — only a btrack write-back replaces them — under a successful run.
        empty = filter(path -> isempty(members[path]), targets)
        if !isempty(empty)
            on_log("[ERROR] Empty population(s), nothing to track: $(join(empty, ", ")) — " *
                   "fix the gate or leave it out")
            return nothing
        end
        for path in targets
            push!(runs, ("$(length(members[path])) cells from population '$path'", members[path], pop_uid(m, path), path))
        end
    end

    on_log("[INFO] Tracking labelProps: $props_path")

    # µm per pixel, skimage order [sz, sy, sx] — the same accessor every other spatial task uses
    # (cellNeighbours, the mesh tasks, track_measures). Tracking was the only one not calling it, so
    # the linking ran in pixels while the measures computed on its own output ran in µm.
    (pixel_res, _time_step) = img_physical_sizes(img)

    nruns = length(runs)
    for (i, (what, label_ids, track_source, _)) in enumerate(runs)
        on_log("[INFO] Tracking $what" * (nruns > 1 ? " ($i of $nruns)" : "") *
               (track_source == WHOLE_SEG_TRACK_SOURCE ? "" : " (uid=$track_source)"))
        ok = _btrack_run(p, task_dir, pixel_res, label_ids, track_source, live_track_sources;
            on_log, on_process,
            # one bar across all runs: run i fills the i-th slice
            on_progress = (n, t) -> on_progress((i - 1) * t + n, nruns * t))
        if !ok
            # the earlier runs' write-backs stand, so their renumbering needs the same clean-up
            i > 1 && (on_log("[ERROR] Stopped at run $i of $nruns; the earlier populations were tracked and kept.");
                      _after_write_back!(img, p.valueName, props_path; on_log))
            return nothing
        end
    end

    on_log("[INFO] Tracking complete.")
    _after_write_back!(img, p.valueName, props_path; on_log)
    _tracking_qc!(img, p.valueName, props_path, [r[3] for r in runs];
                  names = Dict(r[3] => r[4] for r in runs), on_log)
    Dict{String,Any}("valueName" => p.valueName)
end

# QC (advisory): bank the track count + mean track length of the tracks THIS run wrote (rows stamped
# with one of its `track_source`s — one per tracked pop — read through `label_track_sources`); another
# pop's tracks are not this run's. A pop with 0 tracks (btrack linked nothing) is the one unambiguous
# problem → an advisory finding naming it (`names`: source → pop); the counts are always metrics.
function _tracking_qc!(img::CciaImage, vn::AbstractString, props_path::AbstractString,
                       track_sources; names::AbstractDict = Dict{String,String}(), on_log::Function = _ -> nothing)
    mine = track_sources isa AbstractString ? [String(track_sources)] : unique(String.(track_sources))
    try
        # the write-back always stamps `track_source`, so this run's rows are exactly those carrying it
        src   = label_track_sources(img, vn)
        cells = label_props(props_path) |> select_cols(["track_id"]) |> as_df
        of(s) = [cells.track_id[i] for i in eachindex(cells.track_id) if get(src, Int(cells.label[i]), nothing) == s]
        tids  = reduce(vcat, (of(s) for s in mine); init = Any[])
        n_tracks, mean_len, n_tracked = track_count_metrics(tids)
        none  = [get(names, s, s) for s in mine if first(track_count_metrics(of(s))) == 0]
        some  = n_tracks > 0 && length(mine) > 1          # some pops formed tracks: name the others
        findings = isempty(none) ? Dict{String,Any}[] : some ?
            [qc_finding("warn", "tracking.no_tracks"; key = "tracking.no_tracks_pops", pops = join(none, ", "))] :
            [qc_finding("warn", "tracking.no_tracks")]
        write_qc(img, "tracking.bayesian_tracking", vn, findings;
                 metrics = Dict{String,Any}("nTracks"         => n_tracks,
                                            "meanTrackLength" => round(mean_len, digits = 2),
                                            "nTrackedCells"   => n_tracked))
        on_log(!isempty(none) ? "[QC] no tracks formed$(some ? " for " * join(none, ", ") : "") — see the image's QC badge." :
               "[QC] $n_tracks track(s), mean length $(round(mean_len, digits = 1)) frames.")
    catch e
        on_log("[QC] could not compute tracking QC: $e")
    end
end
