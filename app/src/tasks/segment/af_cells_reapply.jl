# af_cells_reapply.jl — a re-measure puts the `af_` columns back (segment.afCells, af_cells.jl).
#
# `segment.measureLabels` rebuilds the cell table, which drops the corrected columns a gate or clustering
# may read. When the old table had them, the re-measure re-runs the correction on the new values with
# its last run's settings, and records that as its own afCells run — so the staleness walk
# (`stale_outputs_for`) sees columns made from the new table, not ones left over from the old.

"""
    af_cells_had_columns(h5ad_path) -> Bool

Does the cell table at `h5ad_path` carry corrected (`af_`) columns? `false` when there is no table.
Read by a re-measure BEFORE it rewrites the table, so it can put them back (`af_cells_reapply!`).
"""
af_cells_had_columns(h5ad_path::AbstractString) =
    isfile(h5ad_path) && (try any(startswith("af_"), col_names(label_props(h5ad_path))) catch; false end)

"""
    af_cells_reapply!(img, value_name; by, on_log, on_progress, on_process) -> Union{Dict,Nothing}

Re-run the correction on a freshly measured `value_name`, with the settings of the last run that wrote
it (the defaults when the record has none), resolved and validated as a re-run would be; `by` = the
profile that ran the re-measure. A re-measure rebuilds the cell table and drops the `af_`
columns, which gates and clusterings built on them read — so the re-measure puts them back rather than
leave the user to notice. Logged as its own `segment.afCells` run, with what it read, so the record
(and the staleness walk) sees corrected columns made from the new table, not left over from the old.
"""
function af_cells_reapply!(img::CciaImage, value_name::AbstractString; by::AbstractString = "",
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    params = something(run_log_params_for_output(img._dir, "segment.afCells", value_name),
                       Dict{String,Any}())
    params["valueName"] = String(value_name)
    # a record from an older spec: fill what it lacks and refuse what no longer validates, as a re-run would
    params = try
        p = resolve_run_params(AfCells(), params, img); validate_params(AfCells(), p); p
    catch e
        on_log("[WARN] afCells: the table was rebuilt, but its last correction's settings no longer apply " *
               "($(sprint(showerror, e))) — run Correct autofluorescence again")
        return nothing
    end
    on_log(">> afCells: the table was rebuilt — re-applying the autofluorescence correction " *
           "(settings of its last run)")
    pv  = run_provenance("segment.afCells", img, params)
    tid = "afCells-reapply-" * string(rand(UInt32); base = 16)
    open_run_log!(img, "segment.afCells", value_name, params; task_id = tid, by = by,
                  inputs = pv.inputs, replaces = pv.replaces)
    res = try
        _run_task(AfCells(), img, params; on_log, on_progress, on_process)
    catch e
        on_log("[ERROR] afCells: re-applying the correction failed: $e")
        nothing
    end
    close_run_log!(img, tid, res === nothing ? "failed" : "done")
    res
end
