# ── behaviour.hmm_transitions — per-cell HMM transitions across the set ──────────
#
# Set-scope task (port of behaviourAnalysis `hmmTransitions`). Reads one or more HMM state columns
# (combining several into a cross-model hybrid), computes per-track lagged transitions, and writes
# `live.cell.hmm.transitions.<colName>` (categorical string, e.g. "1_2") back to each image. Runs
# standalone (to combine independently-fit models) or as the second step of the `behaviour.hmm`
# composite. Compute lives in app/src/behaviour/hmm.jl; this is the I/O wrapper.

using DataFrames: nrow

struct HmmTransitions <: CciaTask end

# Typed shape of what `_run_task(::HmmTransitions, …)` reads from `params`. `hmmStates` is the
# cross-model list threaded from the composite; `nothing` / empty falls back to the standalone
# `live.cell.hmm.state.<colName>` column derived from `colName`.
Base.@kwdef struct HmmTransitionsParams
    pops::Vector{String}          = String[]
    colName::String               = "default"
    hmmStates::Any                = nothing   # nothing → derive from colName; else Vector{String}
    includeStart::Bool            = false
    includeSelfTransitions::Bool  = true
end

function parse_hmm_transitions_params(d::AbstractDict)::HmmTransitionsParams
    HmmTransitionsParams(;
        pops                   = _hmm_pops(d),
        colName                = string(get(d, "colName", "default")),
        hmmStates              = get(d, "hmmStates", nothing),
        includeStart           = Bool(get(d, "includeStart", false)),
        includeSelfTransitions = Bool(get(d, "includeSelfTransitions", true)))
end

function _run_task(::HmmTransitions, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] HMM transitions: no images"); return nothing)

    p = parse_hmm_transitions_params(params)
    isempty(p.pops) && (on_log("[ERROR] HMM transitions: select at least one population/segmentation"); return nothing)
    # Standalone, the user selects one or more state columns explicitly (the cross-model hybrid).
    # In the `behaviour.hmm` composite the states step's produced column is injected as `hmmStates`
    # (see the set-scope CompositeTask in task.jl); falling back to colName covers a bare REPL call.
    state_cols = (p.hmmStates === nothing || isempty(p.hmmStates)) ?
                 String["live.cell.hmm.state.$(p.colName)"] : String[string(x) for x in p.hmmStates]
    isempty(state_cols) && (on_log("[ERROR] HMM transitions: no hmmStates columns selected"); return nothing)

    on_log("[INFO] HMM transitions: $(length(imgs)) image(s), pops=$(p.pops), " *
           "states=$(state_cols), includeStart=$(p.includeStart), includeSelf=$(p.includeSelfTransitions)")
    on_progress(1, 3)

    default_vn = get(imgs[1].label_props, VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL)
    _, tcol    = _hmm_temporal(imgs, p.pops, default_vn)
    isnothing(tcol) && (on_log("[ERROR] No temporal column in the selected segmentation(s) — HMM needs a timecourse"); return nothing)

    uids     = [img.uid for img in imgs]
    pop_cols = unique(vcat(state_cols, ["track_id", tcol]))
    # pop_type "live" (derived `/_tracked` injected); no value_name kwarg — each prefixed pop resolves
    # its own segmentation (cross-segmentation pool).
    df = pop_df(imgs, uids, "live", p.pops; pop_cols=pop_cols, granularity=:cell)
    nrow(df) == 0 && (on_log("[ERROR] No cells for pops=$(p.pops)"); return nothing)
    # all requested state columns must be present (run HMM states first)
    missing_cols = [c for c in state_cols if !(c in names(df))]
    isempty(missing_cols) || (on_log("[ERROR] missing state column(s): $(missing_cols) — run HMM states first"); return nothing)
    on_progress(2, 3)

    trans = hmm_transitions(df, state_cols; time_col=tcol,
                            group_cols=["uID", "value_name", "track_id"],
                            include_start=p.includeStart, include_self=p.includeSelfTransitions)
    n_tr = count(!ismissing, trans)
    on_log("[INFO] $(n_tr) transitions, $(length(unique(skipmissing(trans)))) distinct")

    trans_col = "live.cell.hmm.transitions.$(p.colName)"
    df[!, :_hmm_trans] = trans
    n_ok = 0
    for img in imgs
        sub = df[df.uID .== img.uid, :]
        nrow(sub) == 0 && continue
        for vn in unique(sub.value_name)
            vsub  = sub[sub.value_name .== vn, :]
            props = img_label_props_path(img, string(vn))
            vals  = Any[ismissing(t) ? nothing : string(t) for t in vsub._hmm_trans]
            ok = write_categorical_obs(props,
                     [(name=trans_col, labels=Int.(vsub.label), values=vals)];
                     drop=[trans_col], on_log=on_log, on_process=on_process)
            if ok
                n_ok += 1
                # QC (advisory): bank this image-segmentation's transition distribution + the
                # no-transitions finding. Never fails the write.
                m = category_dist_metrics(vals)
                write_qc(img, "behaviour.hmm_transitions", string(vn), hmm_transitions_qc_findings(m);
                         metrics = Dict{String,Any}("nTransitions" => m.n,
                             "nDistinctTransitions" => m.n_distinct))
            else
                on_log("[WARN] write failed: $(img.uid)/$vn")
            end
        end
    end
    on_progress(3, 3)
    on_log("[INFO] HMM transitions done → $trans_col ($n_ok image-segmentations written)")

    Dict{String,Any}("colName" => p.colName, "transitionColumn" => trans_col,
                     "images" => length(imgs), "transitions" => n_tr)
end
