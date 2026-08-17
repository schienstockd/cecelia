# correct.jl — manual track correction (docs/todo/CORRECTION_PLAN.md, P1)
#
# Rewrites `obs.track_id` + the lineage columns from a list of edit ops, journals them, and leaves
# `X`/`var` untouched. The ops engine is `app/src/tracking/track_correction.jl` — pure, shared, and
# unit-tested; this file is the task shell around it (params, IO, QC, journal).
#
# WHY A TASK AND NOT A BUTTON (Decision 1): a correction that only exists as a click in a Vue page
# cannot be replayed, logged, cancelled or audited. As a task it gets the scheduler's log file,
# resource pool and QC banking for free, and runs identically from the REPL:
#
#     run_task(TrackCorrect(), img, Dict{String,Any}(
#         "valueName" => "memTom",
#         "trackOps"  => [Dict("op" => "track.join", "trackIds" => [78, 92])]))
#
# HOW OPS ARRIVE. `trackOps` accepts either a real Vector (REPL, API, chain) or a JSON string (the
# form). One parser for both — `parse_track_ops`. The spec declares it as `text` because there is no
# array widget yet; that is a P1 stopgap, NOT the authoring path. P4 builds the real surface (select
# in napari → op buttons → commit), and the interactive staging described in Decision 3b accumulates
# ops in the API and submits them here as ONE run. Do not ship the JSON text box as the way a user
# corrects a track.
#
# WHAT THIS DOES NOT DO. It does not recompute track measures — `tracking.track_measures` does, and
# the composite `tracking.correct_measures` chains the two (Decision 4). Running this task alone
# leaves `live.*` measures stale, so it DROPS them rather than leaving numbers that describe the
# pre-correction tracks (the same invalidation `bayesian_tracking` performs when it rewrites tracks,
# `tracking_utils.py:224-231`).

using DataFrames: DataFrame, select!, Not

struct TrackCorrect <: CciaTask end

"""
    parse_track_ops(value) -> Vector{Dict{String,Any}}

Normalise the `trackOps` param into a list of op dicts, from either a Vector (REPL/API/chain) or a
JSON string (the form). Throws `ParamValidationError` on anything MALFORMED — an unknown op kind, a
missing field, a non-object entry — because failing here costs nothing while the alternative is a
task that opens the cell table before discovering the ops are nonsense.

**Empty is legal and means "no correction".** Not a judgement call: the package suite asserts that
every task's own spec defaults validate (`app/test/suite.jl` → *the spec's own defaults must satisfy
the spec*), and a task whose default cannot be submitted does not fit the framework. So an empty
list, `""` and `nothing` all parse to no ops, and `_run_task` reports that and writes nothing —
visibly, not silently.
"""
function parse_track_ops(value)::Vector{Dict{String,Any}}
    raw = value
    isnothing(raw) && return Dict{String,Any}[]
    if raw isa AbstractString
        s = strip(String(raw))
        isempty(s) && return Dict{String,Any}[]
        raw = try
            JSON3.read(s, Vector{Dict{String,Any}})
        catch e
            throw(ParamValidationError("'trackOps' is not a JSON array of ops: $e"))
        end
    end
    raw isa AbstractVector ||
        throw(ParamValidationError("'trackOps' must be a list of ops, got: $(typeof(raw))"))

    ops = Dict{String,Any}[]
    for (i, o) in enumerate(raw)
        o isa AbstractDict ||
            throw(ParamValidationError("'trackOps'[$i] must be an object, got: $(typeof(o))"))
        d = Dict{String,Any}(string(k) => v for (k, v) in pairs(o))
        kind = string(get(d, "op", ""))
        kind in TRACK_OP_KINDS ||
            throw(ParamValidationError("'trackOps'[$i] has op \"$kind\" — expected one of " *
                                       join(TRACK_OP_KINDS, ", ")))
        # per-kind required fields — the same checks the engine would make, but before any IO
        if kind in ("points.remove", "points.add")
            ls = get(d, "labels", nothing)
            (ls isa AbstractVector && !isempty(ls)) ||
                throw(ParamValidationError("'trackOps'[$i] ($kind) needs a non-empty `labels` list"))
        elseif kind == "track.remove"
            ts = get(d, "trackIds", nothing)
            (ts isa AbstractVector && !isempty(ts)) ||
                throw(ParamValidationError("'trackOps'[$i] (track.remove) needs a non-empty `trackIds` list"))
        elseif kind == "track.join"
            ts = get(d, "trackIds", nothing)
            (ts isa AbstractVector && length(ts) == 2) ||
                throw(ParamValidationError("'trackOps'[$i] (track.join) needs exactly 2 `trackIds`"))
        else  # track.split
            (haskey(d, "trackId") && haskey(d, "atT")) ||
                throw(ParamValidationError("'trackOps'[$i] (track.split) needs `trackId` and `atT`"))
        end
        push!(ops, d)
    end
    ops
end

# Spec validation plus the ops check — so a malformed op is a ParamValidationError at submit time,
# not a stack trace halfway through a run.
function validate_params(task::TrackCorrect, params::Dict{String,Any})
    invoke(validate_params, Tuple{CciaTask, Dict{String,Any}}, task, params)
    parse_track_ops(get(params, "trackOps", nothing))
    nothing
end

function _run_task(task::TrackCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    props_path = img_label_props_path(img, value_name)
    isfile(props_path) || begin
        on_log("[ERROR] No labelProps for valueName='$value_name': $props_path")
        return nothing
    end

    ops = parse_track_ops(get(params, "trackOps", nothing))
    if isempty(ops)
        on_log("[INFO] No correction ops — nothing to do.")
        return Dict{String,Any}("valueName" => value_name, "nOps" => 0)
    end
    on_log("[INFO] $(length(ops)) correction op(s) on $value_name")
    on_progress(1, 5)

    # ── Read the FULL cell table, never a population subset ───────────────────────
    #
    # THE trap of this task (CORRECTION_PLAN.md, `add_obs` §): `add_obs` aligns by label and writes
    # NaN for every label ABSENT from the staged frame (`label_props.jl:679`). So staging a
    # population-scoped frame would untrack every cell outside that population. A population may
    # scope which cells the USER can select (Decision 6) — it must never scope the write.
    lp = label_props(props_path)
    have = col_names(lp; data_type = :obs)
    "track_id" in have || begin
        on_log("[ERROR] No track_id column in $props_path — run tracking first")
        return nothing
    end
    temporal = temporal_columns(lp)
    isempty(temporal) && begin
        on_log("[ERROR] No temporal column — track correction needs a timecourse segmentation")
        return nothing
    end
    lineage_present = [c for c in TRACK_LINEAGE_OBS if c in have]
    select_cols(lp, vcat(["track_id"], lineage_present, temporal))
    df = as_df(lp; include_x = false, include_obs = true)

    # the engine works on `centroid_t`; alias whatever the temporal column is called
    t_col = first(temporal)
    t_col == "centroid_t" || (df[!, :centroid_t] = df[!, Symbol(t_col)])

    before = copy(df.track_id)
    on_log("[INFO] $(nrow(df)) cells, $(length(track_ids_present(df))) track(s) before correction")
    on_progress(2, 5)

    # ── Apply ─────────────────────────────────────────────────────────────────────
    entries = try
        apply_track_ops!(df, ops)
    catch e
        # an op that cannot be applied fails the RUN — a partially applied correction would leave
        # the journal describing a state the file is not in, which breaks Decision 3's replay claim
        on_log("[ERROR] $(e isa ErrorException ? e.msg : sprint(showerror, e))")
        return nothing
    end
    for e in entries
        on_log("[INFO] $(e["op"]): $(e["summary"])")
    end
    on_progress(3, 5)

    # ── Write: track_id + lineage, full label set, one save ───────────────────────
    write_cols = vcat(["label", "track_id"], lineage_present)
    out = df[!, Symbol.(write_cols)]

    # Stale per-cell/per-track measures: they describe the PREVIOUS track assignment. The chained
    # `tracking.track_measures` recomputes them, but this task must not depend on the chain running
    # — a standalone correction that leaves `live.cell.speed` from the old tracks is exactly the
    # silent-staleness old R shipped.
    stale = [c for c in have if startswith(c, "live.cell.") || startswith(c, "live.track.")]
    isempty(stale) || on_log("[INFO] Dropping $(length(stale)) stale track-measure column(s)")

    label_props(props_path) |> v -> drop_obs(v, stale) |> v -> add_obs(v, out) |> save!
    on_log("[INFO] Wrote track_id + $(length(lineage_present)) lineage column(s) → $props_path")
    on_progress(4, 5)

    # ── Journal (Decision 3/7): durable, per-image, append-only ───────────────────
    journal = try
        append_corrections!(img._dir, value_name, entries)
    catch e
        on_log("[WARN] could not write the correction journal: $e")
        nothing
    end
    isnothing(journal) || on_log("[INFO] Journalled $(length(entries)) op(s) → $journal")

    # ── QC (Decision 8) ───────────────────────────────────────────────────────────
    metrics = track_correction_metrics(before, df.track_id, length(ops))
    try
        write_qc(img, "tracking.correct", value_name,
                 track_correction_qc_findings(metrics); metrics = metrics)
        on_log("[QC] $(metrics["nCellsReassigned"]) cell(s) reassigned, " *
               "$(metrics["nTracksBefore"]) → $(metrics["nTracksAfter"]) track(s).")
    catch e
        on_log("[QC] could not compute correction QC: $e")
    end
    on_progress(5, 5)

    Dict{String,Any}("valueName" => value_name, "nOps" => length(ops), "metrics" => metrics)
end
