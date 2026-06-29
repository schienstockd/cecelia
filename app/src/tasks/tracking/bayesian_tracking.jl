struct BayesianTracking <: CciaTask end

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

    value_name    = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    pops_to_track = string(get(params, "popsToTrack", "NONE"))

    task_dir   = img._dir
    props_path = img_label_props_path(img, value_name)
    if !isfile(props_path)
        on_log("[ERROR] No labelProps for valueName='$value_name': $props_path")
        return nothing
    end

    # Resolve gated-population membership in-process (Julia is the gate evaluator).
    label_ids = nothing
    if pops_to_track != "NONE"
        m = load_pop_map(img; value_name = value_name, pop_type = "flow")
        if !has_pop(m, pops_to_track)
            on_log("[ERROR] Population not found in gating/$(value_name).json: $pops_to_track")
            return nothing
        end
        recompute!(m, cols -> (label_props(img; value_name = value_name) |>
                               lp -> select_cols(lp, cols) |> as_df))
        label_ids = collect(Int, cells_in_pop(m, pops_to_track))
        on_log("[INFO] Tracking $(length(label_ids)) cells from population '$pops_to_track'")
        if isempty(label_ids)
            on_log("[ERROR] Population '$pops_to_track' is empty — nothing to track")
            return nothing
        end
    else
        on_log("[INFO] Tracking whole segmentation '$value_name'")
    end

    # Write the params file the Python script reads.
    task_dirs   = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    task_subdir = string(get(task_dirs, "tasks", "tasks"))
    params_dir  = joinpath(task_dir, task_subdir)
    mkpath(params_dir)

    task_id     = get(params, "_task_id", string(rand(UInt32), base = 16))
    params_file = joinpath(params_dir, "bayesian_tracking.$task_id.params.json")
    open(params_file, "w") do io
        JSON3.write(io, (;
            taskDir              = task_dir,
            valueName            = value_name,
            labelIds             = label_ids,                          # null = whole segmentation
            maxSearchRadius      = Int(get(params, "maxSearchRadius", 20)),
            maxLost              = Int(get(params, "maxLost", 3)),
            trackBranching       = Bool(get(params, "trackBranching", false)),
            minTimepoints        = Int(get(params, "minTimepoints", 5)),
            accuracy             = Float64(get(params, "accuracy", 0.8)),
            probToAssign         = Float64(get(params, "probToAssign", 0.8)),
            noiseInital          = Int(get(params, "noiseInital", 300)),
            noiseProcessing      = Int(get(params, "noiseProcessing", 100)),
            noiseMeasurements    = Int(get(params, "noiseMeasurements", 100)),
            distThresh           = Float64(get(params, "distThresh", 10.0)),
            timeThresh           = Int(get(params, "timeThresh", 5)),
            segmentationMissRate = Float64(get(params, "segmentationMissRate", 0.1)),
            lambdaLink           = Int(get(params, "lambdaLink", 5)),
            lambdaBranch         = Int(get(params, "lambdaBranch", 50)),
            lambdaTime           = Int(get(params, "lambdaTime", 5)),
            lambdaDist           = Float64(get(params, "lambdaDist", 5.0)),
            thetaTime            = Int(get(params, "thetaTime", 5)),
            thetaDist            = Float64(get(params, "thetaDist", 5.0)),
        ))
    end

    # @__DIR__ = app/src/tasks/tracking/ → three dirname levels reach app/
    py_script  = joinpath(dirname(dirname(dirname(@__DIR__))), "py", "tasks",
                          "tracking", "bayesian_tracking_run.py")
    python_bin = python_bin_path()

    if !isfile(py_script)
        on_log("[ERROR] Python script not found: $py_script")
        return nothing
    end

    on_log("[INFO] Tracking labelProps: $props_path")

    out_pipe = Pipe()
    proc = run(pipeline(`$python_bin $py_script --params $params_file`;
                        stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)

    for line in eachline(out_pipe)
        m = match(r"^\[PROGRESS\] (\d+)/(\d+)$", line)
        if !isnothing(m)
            on_progress(parse(Int, m[1]), parse(Int, m[2]))
        else
            on_log(line)
        end
    end
    wait(proc)
    ok = proc.exitcode == 0 && proc.termsignal == 0
    ok || return nothing

    on_log("[INFO] Tracking complete.")
    Dict{String,Any}("valueName" => value_name)
end
