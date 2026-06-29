# ── behaviour.hmm_states — fit a Gaussian HMM across the set ─────────────────────
#
# Set-scope task (port of behaviourAnalysis `hmmStates`): pools the selected images' tracked cells,
# fits ONE Gaussian HMM jointly (the original fitted across images too), Viterbi-decodes a state per
# cell, and writes `live.cell.hmm.state.<colName>` back to each image's labelProps as **numeric
# integer codes** (1.0/2.0/…): auto-detected categorical by the ≤20-levels rule (track_props.jl),
# fastest to gate/filter ("show cells with state 1 vs 2"), and matches the original R obs values.
# (Transitions, which are inherently strings, use the categorical writer instead.)
# The fit + decode + preprocessing live in the headless core (app/src/behaviour/hmm.jl); this task
# is the I/O wrapper. Cells without complete measurements (track starts) get NaN (no state).
#
# Populations are selected ACROSS SEGMENTATIONS (the R `popDT` semantics): each `pops` entry carries
# its segmentation as a value_name prefix ("A/_tracked", "B/_tracked", …). `pop_df` (pop_type "live")
# pools them — one row per cell tagged with its `value_name` — so a single run fits tracked A, B, C
# from every image together. There is no separate `valueName` param; the segmentations are exactly
# those named by the pops.

using DataFrames: nrow, DataFrame

struct HmmStates <: CciaTask end

# value_names referenced by the (value-name-prefixed) pops, e.g. "A/tracked" → "A".
_hmm_pop_value_names(pops, default_vn::AbstractString) =
    collect(keys(_group_pops_by_value_name(pops, default_vn)))

# Coerce the pops param (array from the multi-select; tolerate a bare string) and drop placeholders.
function _hmm_pops(params)::Vector{String}
    raw  = get(params, "pops", String[])
    pops = raw isa AbstractString ? String[raw] : String[string(x) for x in raw]
    filter(p -> !isempty(p) && p != "NONE", pops)
end

# First value_name among the pops whose labelProps exists and carries a temporal column.
function _hmm_temporal(imgs, pops, default_vn::AbstractString)
    for vn in _hmm_pop_value_names(pops, default_vn)
        p = img_label_props_path(imgs[1], vn)
        isfile(p) || continue
        tc = temporal_columns(label_props(p))
        isempty(tc) || return (vn, first(tc))
    end
    (nothing, nothing)
end

function _run_task(::HmmStates, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] HMM states: no images"); return nothing)

    pops       = _hmm_pops(params)
    isempty(pops) && (on_log("[ERROR] HMM states: select at least one population/segmentation"); return nothing)
    col_name   = string(get(params, "colName", "default"))
    # Comprehensions (not `String.(collect(...))`): an empty GUI selection collects to a
    # `Vector{Union{}}`, which fails the typed `_normalise_scale!`; `String[...]` is always Vector{String}.
    measures   = String[string(x) for x in get(params, "modelMeasurements", ["live.cell.speed", "live.cell.angle"])]
    isempty(measures) && (on_log("[ERROR] HMM states: no modelMeasurements selected"); return nothing)
    num_states = Int(get(params, "numStates", 2))

    noise_filter = Int(get(params, "noiseFilterMeasurements", 0))
    post_filter  = Int(get(params, "postFiltering", 0))
    post_iters   = Int(get(params, "postIterations", 1))
    norm_to      = string(get(params, "normaliseTo", "none"))
    norm_list    = String[string(x) for x in get(params, "normaliseMeasurements", String[])]
    scale_list   = String[string(x) for x in get(params, "scaleMeasurements", String[])]
    normalise    = (norm_to == "none" || isempty(norm_list)) ? Dict{String,String}() :
                   Dict(m => norm_to for m in norm_list)

    on_log("[INFO] HMM states: $(length(imgs)) image(s), pops=$(pops), " *
           "measures=$(measures), states=$num_states")
    on_progress(1, 4)

    default_vn = get(imgs[1].label_props, VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL)
    vn0, tcol  = _hmm_temporal(imgs, pops, default_vn)
    isnothing(tcol) && (on_log("[ERROR] No temporal column in the selected segmentation(s) — HMM needs a timecourse"); return nothing)

    uids     = [img.uid for img in imgs]
    pop_cols = unique(vcat(measures, ["track_id", tcol]))
    # pop_type "live" = cell-level pops with the derived `/_tracked` injected (track_id > 0); the same
    # flow gate file backs it (gating/{vn}.json). No value_name kwarg: each prefixed pop resolves its
    # own segmentation (cross-segmentation pool), one row per cell tagged with its `value_name`.
    df = pop_df(imgs, uids, "live", pops; pop_cols=pop_cols, granularity=:cell)
    nrow(df) == 0 && (on_log("[ERROR] No cells for pops=$(pops)"); return nothing)
    on_progress(2, 4)

    on_log("[INFO] Fitting HMM over $(nrow(df)) cells…")
    states = hmm_fit_states(df, measures; num_states=num_states, time_col=tcol,
                            group_cols=["uID", "value_name", "track_id"],
                            noise_filter=noise_filter, normalise=normalise,
                            scale_measures=scale_list, post_filter=post_filter,
                            post_iterations=post_iters)
    n_decoded = count(!ismissing, states)
    on_log("[INFO] decoded $n_decoded/$(length(states)) cells into " *
           "$(length(unique(skipmissing(states)))) states")
    on_progress(3, 4)

    state_col = "live.cell.hmm.state.$col_name"
    df[!, :_hmm_state] = states
    n_ok = 0
    for img in imgs
        sub = df[df.uID .== img.uid, :]
        nrow(sub) == 0 && continue
        for vn in unique(sub.value_name)
            vsub  = sub[sub.value_name .== vn, :]
            props = img_label_props_path(img, string(vn))
            # integer state codes as Float64 (1.0/2.0/…); track-start cells → NaN. Numeric obs via
            # the native writer — fast to filter/gate, auto-detected categorical (few integer levels).
            vals  = Float64[ismissing(s) ? NaN : Float64(s) for s in vsub._hmm_state]
            cell_df = DataFrame("label" => Int.(vsub.label), state_col => vals)
            try
                label_props(props) |> add_obs(cell_df) |> save!   # overwrites any existing column
                n_ok += 1
            catch e
                on_log("[WARN] write failed: $(img.uid)/$vn — $e")
            end
        end
    end
    on_progress(4, 4)
    on_log("[INFO] HMM states done → $state_col ($n_ok image-segmentations written)")

    # `stateColumn` is threaded into the transitions step by the `behaviour.hmm` composite.
    Dict{String,Any}("colName" => col_name, "numStates" => num_states,
                     "images" => length(imgs), "stateColumn" => state_col,
                     "decoded" => n_decoded)
end
