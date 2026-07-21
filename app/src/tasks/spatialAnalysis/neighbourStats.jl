# ── spatialAnalysis.neighbourStats — pairwise cell-type contact statistics ──────────
#
# The canonical CODEX pairwise interaction readout (Goltsev et al., Cell 2018): for a set of basis
# populations, the log-odds ratio of observed vs expected contacts between every pair in the neighbour
# graph — positive = selective association, negative = avoidance (e.g. T/B-cell avoidance =
# follicle/PALS segregation). Complements squidpy's permutation z-score with the interpretable
# odds-ratio the CODEX/Gerner community expects. Output is a FLAT table (Decision 9), written to a
# per-image spatialStats sidecar for the analysis canvas + MCP to read.
#
# Division of labour: Julia resolves the basis (value_name, pop) codes (shared `_basis_segments`);
# Python pools the image's basis cells into one combined graph (`spatial_utils.build_pooled_image_graph`)
# and computes the log-odds (`spatial_utils.pairwise_contact_logodds`). squidpy/CODEX attribution in
# the runner + THIRD_PARTY.md.

using DataFrames: nrow

struct NeighbourStats <: CciaTask end

function _neighbour_stats_findings(n_cells::Integer, n_edges::Integer)
    n_cells == 0 && return [qc_finding("warn", "spatial.no_cells", "No cells",
        "No cells in the selected populations — check the population basis.")]
    n_edges == 0 && return [qc_finding("warn", "spatial.no_edges", "No neighbours found",
        "The neighbour graph has no edges — increase the radius (µm) or use a kNN/Delaunay graph.")]
    Dict{String,Any}[]
end

function _run_task(::NeighbourStats, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    pops = _str_list(params, "basisPops")
    isempty(pops) && (on_log("[ERROR] neighbourStats: select ≥2 populations"); return nothing)
    suffix   = string(get(params, "statsSuffix", "default"))
    on_progress(1, 3)

    # basis pops may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves each
    # under its own type; set-scope form carries uID (shared _basis_segments).
    df = pop_df_multi([img], [img.uid], pops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] neighbourStats: no cells for pops=$(pops)"); return nothing)
    basis, segments, phys = _basis_segments([img], df)
    length(basis) < 2 &&
        (on_log("[ERROR] neighbourStats: need ≥2 populations for pairwise stats (got $(basis))"); return nothing)

    stats_path = joinpath(img._dir, "spatialStats", "$(suffix).json")
    qc_out_path = joinpath(task_run_dir(img._dir), "neighbour_stats_qc.json")
    on_log("[INFO] neighbourStats: basis=$(basis), suffix=$suffix")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "segments" => segments, "basis" => basis, "physicalSizes" => phys,
        "statsPath" => stats_path,
        "neighbourMethod" => string(get(params, "neighbourMethod", "delaunay")),
        "neighbourRadius" => Float64(get(params, "neighbourRadius", 30.0)),
        "nNeighbours" => Int(get(params, "nNeighbours", 6)),
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_neighbour_stats_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] neighbourStats: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cells = Int(get(qc, "nCells", 0)); n_edges = Int(get(qc, "nEdges", 0))
        mean_degree = Float64(get(qc, "meanDegree", 0.0))
        write_qc(img, "spatialAnalysis.neighbourStats", suffix,
                 _neighbour_stats_findings(n_cells, n_edges);
                 metrics = Dict{String,Any}("nCells" => n_cells, "nEdges" => n_edges, "meanDegree" => mean_degree))
        on_log("[QC] pairwise stats over $(n_cells) cell(s), $(n_edges) edge(s).")
    catch e
        on_log("[QC] could not compute neighbourStats QC: $e")
    end
    on_progress(3, 3)

    on_log("[INFO] neighbourStats done → $(basename(stats_path))")
    Dict{String,Any}("statsPath" => stats_path, "basis" => length(basis))
end
