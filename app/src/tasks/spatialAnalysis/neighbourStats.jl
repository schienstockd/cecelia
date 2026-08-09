# ── spatialAnalysis.neighbourStats — pairwise cell-interaction statistics ──────────────
#
# "Who neighbours whom, and is it real?" — the canonical CODEX pairwise interaction readout (Goltsev et
# al., Cell 2018): for a set of populations, the log-odds ratio of observed vs expected contacts between
# every pair in the neighbour graph (positive = selective association, negative = avoidance, e.g. T/B
# avoidance = follicle/PALS segregation), PLUS a permutation test that says whether the pattern is
# distinguishable from a random arrangement of the same cell types over the same graph.
#
# This needs NO regions. Interactions are just a labelling of the shared neighbour graph
# (`spatialAnalysis.cellNeighbours` → `spatialGraph/{suffix}.h5ad`), which this task LOADS rather than
# rebuilding — so the neighbourhood means the same thing here as it does in region clustering and the
# graph parameters are not duplicated across task specs. Output is a FLAT table (Decision 9) in a
# per-image spatialStats sidecar, read by the interaction heatmap and MCP.
#
# Division of labour (docs/ARCHITECTURE.md): Julia resolves the populations → per-segment (label →
# basis code) via `pop_df` (so membership composes with gating across poptypes), and names the graph.
# Python (`cell_neighbour_stats_run.py`) loads the graph, joins the codes onto its nodes by
# (valueName, label), and computes the log-odds + permutation z/p (`spatial_utils`). squidpy/CODEX
# attribution in `spatial_utils.pairwise_contact_logodds` + THIRD_PARTY.md.

using DataFrames: nrow

struct NeighbourStats <: CciaTask end

# Pure QC helpers (unit-tested per docs/MODULES.md). Advisory only — never gates.
# `coverage` = the fraction of GRAPH nodes that fell into one of the selected populations. A low value
# means the graph was built over a much wider cell set than the analysis asks about, so most of each
# neighbourhood is unlabelled and the pairwise counts rest on a small slice of the graph.
function _neighbour_stats_findings(n_cells::Integer, n_edges::Integer,
                                   coverage::Real = 1.0, n_significant::Integer = -1)
    n_cells == 0 && return [qc_finding("warn", "spatial.no_cells", "No cells",
        "No cells in the selected populations — check the population basis.")]
    n_edges == 0 && return [qc_finding("warn", "spatial.no_edges", "No neighbours found",
        "No contacts between the selected populations in this graph — rebuild the neighbour graph with a larger radius, or select populations that are actually adjacent.")]
    findings = Dict{String,Any}[]
    if coverage < 0.1
        push!(findings, qc_finding("warn", "spatial.low_coverage",
            "Populations cover little of the graph",
            "Rebuild the neighbour graph over these populations, or read the result as covering that subset only.";
            detail = "$(round(Int, coverage * 100))% of graph cells are in the selected populations"))
    end
    n_significant == 0 && push!(findings, qc_finding("warn", "spatial.none_significant",
        "No interaction beat chance",
        "Treat the log-odds as descriptive, or rebuild the graph with a larger radius."))
    findings
end

function _run_task(::NeighbourStats, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    pops = _str_list(params, "basisPops")
    isempty(pops) && (on_log("[ERROR] neighbourStats: select ≥2 populations"); return nothing)
    suffix   = string(get(params, "statsSuffix", "default"))
    graph_sfx = string(get(params, "graphSuffix", "default"))
    on_progress(1, 3)

    # the graph is a REQUIRED input, not something this task builds — fail with the available names
    # rather than silently falling back to a graph of its own (that fallback is the divergence this
    # design removes; see cellNeighbours.jl).
    graph_path = img_spatial_graph_path(img, graph_sfx)
    if !isfile(graph_path)
        have = img_spatial_graph_suffixes(img)
        on_log("[ERROR] neighbourStats: no neighbour graph \"$(graph_sfx)\" for this image — " *
               (isempty(have) ? "run 'Neighbour graph' first" : "available: $(join(have, ", "))"))
        return nothing
    end

    # pops may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves each
    # under its own type; set-scope form carries uID (shared _basis_segments).
    df = pop_df_multi([img], [img.uid], pops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] neighbourStats: no cells for pops=$(pops)"); return nothing)
    basis, segments = _basis_segments([img], df)
    length(basis) < 2 &&
        (on_log("[ERROR] neighbourStats: need ≥2 populations for pairwise stats (got $(basis))"); return nothing)

    stats_path = joinpath(img._dir, "spatialStats", "$(suffix).json")
    qc_out_path = joinpath(task_run_dir(img._dir), "neighbour_stats_qc.json")
    n_perm = Int(get(params, "nPermutations", 1000))
    on_log("[INFO] neighbourStats: graph=$(graph_sfx) basis=$(basis) permutations=$(n_perm) → $(suffix)")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "graphPath" => graph_path, "graphSuffix" => graph_sfx,
        "segments" => segments, "basis" => basis,
        "statsPath" => stats_path,
        "nPermutations" => n_perm,
        "randomState" => 0,
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_neighbour_stats_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] neighbourStats: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cells = Int(get(qc, "nCells", 0)); n_edges = Int(get(qc, "nEdges", 0))
        mean_degree = Float64(get(qc, "meanDegree", 0.0))
        coverage    = Float64(get(qc, "coverage", 1.0))
        n_sig       = Int(get(qc, "nSignificant", -1))
        write_qc(img, "spatialAnalysis.neighbourStats", suffix,
                 _neighbour_stats_findings(n_cells, n_edges, coverage, n_sig);
                 metrics = Dict{String,Any}("nCells" => n_cells, "nEdges" => n_edges,
                                            "meanDegree" => mean_degree, "coverage" => coverage,
                                            "nSignificant" => n_sig))
        on_log("[QC] interactions over $(n_cells) cell(s), $(n_edges) contact(s)" *
               (n_sig >= 0 ? ", $(n_sig) pair(s) beat chance." : "."))
    catch e
        on_log("[QC] could not compute neighbourStats QC: $e")
    end
    on_progress(3, 3)

    on_log("[INFO] neighbourStats done → $(basename(stats_path))")
    Dict{String,Any}("statsPath" => stats_path, "basis" => length(basis), "graphSuffix" => graph_sfx)
end
