# ── spatialAnalysis.aggregatesMeshes — aggregate detection (MESH route) ─────────────
#
# The MESH counterpart to spatialAnalysis.detectAggregates (points DBSCAN): a population's cells whose
# SURFACE-to-surface distance is within a threshold are linked into a proximity graph; connected
# components of ≥ minCells are aggregates (legacy cellClustersMeshes). For large/live cells where
# surfaces matter (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11). Meshes are built on the fly per
# timepoint in Python (`cell_aggregates_mesh_run.py`); Julia resolves membership + the label-zarr path
# + physical sizes. Writes the same `<popType>.cell.is.aggregate` / `.aggregate.id` slot as the points
# route (alternative methods for the one annotation). Reuses `_label_zarr_path`/`_aggregate_qc_findings`.

using DataFrames: nrow

struct AggregatesMeshes <: CciaTask end

function _run_task(::AggregatesMeshes, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", "default"))
    pops       = _str_list(params, "pops")
    isempty(pops) && (on_log("[ERROR] aggregatesMeshes: select a population"); return nothing)
    # pops may mix types — resolve across types + namespace output by cell kind (see detectAggregates)
    pop_type   = pop_namespace(img, pops; value_name = value_name)
    haskey(img.labels, value_name) ||
        (on_log("[ERROR] aggregatesMeshes: no label zarr for $(value_name)"); return nothing)
    max_dist   = Float64(get(params, "maxClusterDist", 5.0))
    min_cells  = Int(get(params, "minCells", 5))
    on_progress(1, 3)

    mem = pop_df_multi(img, pops; value_name = value_name, granularity = :cell, restrict_to = value_name)
    nrow(mem) == 0 && (on_log("[ERROR] aggregatesMeshes: no cells for $(pops)"); return nothing)
    (sizes, _) = img_physical_sizes(img)
    qc_out_path = joinpath(task_run_dir(img._dir), "aggregates_mesh_qc.json")
    on_log("[INFO] aggregatesMeshes: $(nrow(mem)) cells, ≤$(max_dist)µm, min $(min_cells) cells")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "imPath" => img_filepath(img), "labelPath" => _label_zarr_path(img, value_name),
        "labels" => Int.(mem.label), "physicalSizes" => sizes,
        "maxClusterDist" => max_dist, "minCells" => min_cells, "popType" => pop_type,
        "propsPath" => img_label_props_path(img, value_name), "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_aggregates_mesh_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] aggregatesMeshes: Python runner failed"); return nothing)

    # Auto-create the reusable "aggregated" population (Decision 14) — same as the points route
    # (detectAggregates): a filter pop under each input pop on `<popType>.cell.is.aggregate > 0`.
    agg_paths = ensure_filter_pop!(img, pop_type, value_name, pops, AGGREGATED_POP_NAME;
                                   filter_measure = "$(pop_type).cell.is.aggregate",
                                   filter_fun = "gt", filter_values = 0)
    isempty(agg_paths) || on_log("[INFO] aggregatesMeshes: aggregated population → $(join(agg_paths, ", "))")

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cells = Int(get(qc, "nCells", 0)); n_agg = Int(get(qc, "nAggregates", 0))
        frac = Float64(get(qc, "fracAggregated", 0.0))
        write_qc(img, "spatialAnalysis.aggregatesMeshes", value_name,
                 _aggregate_qc_findings(n_cells, frac);
                 metrics = Dict{String,Any}("nCells" => n_cells, "nAggregates" => n_agg, "fracAggregated" => frac))
        on_log("[QC] mesh aggregates: $(n_agg), $(round(Int, frac*100))% of cells.")
    catch e
        on_log("[QC] could not compute aggregatesMeshes QC: $e")
    end
    on_progress(3, 3)

    Dict{String,Any}("valueName" => value_name)
end
