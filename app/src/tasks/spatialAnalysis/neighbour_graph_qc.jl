# ── spatialAnalysis.cellNeighbours — the neighbour graph's QC ─────────────────────────────────────
#
# Banked by the graph task and re-judged by each consumer (clustRegions, neighbourStats) on the graph it
# loaded: edge findings for a cell-to-cell graph, the grid for a fixed-window one.

# Pure QC helper (unit-tested per docs/MODULES.md): objective graph metrics → advisory findings. The
# unambiguous problems are an empty graph (no edges — usually a radius far below the cell spacing, or
# wrong units), a majority of isolated cells, and a graph too sparse to describe a neighbourhood:
# a mean degree under `SPARSE_GRAPH_MEAN_DEGREE` leaves each cell's composition vector a handful
# of 0/1 steps, and region clustering on it is noise. An unpruned 2D Delaunay graph averages ~6, so
# falling under 3 means the radius is cutting most real contacts. Counts bank as metrics, not findings.
const SPARSE_GRAPH_MEAN_DEGREE = 3.0

function _neighbours_qc_findings(n_cells::Integer, n_edges::Integer, isolated_frac::Real,
                                 mean_degree::Real = Inf)
    findings = Dict{String,Any}[]
    if n_cells == 0
        push!(findings, qc_finding("warn", "spatial.no_cells", "No cells",
            "The selection had no cells to build a neighbour graph from — check the population."))
    elseif n_edges == 0
        push!(findings, qc_finding("warn", "spatial.no_edges", "No neighbours found",
            "The neighbour graph has no edges — the radius (in µm) may be far below the cell spacing. Increase the radius or use a kNN / Delaunay graph."))
    elseif isolated_frac >= 0.5
        push!(findings, qc_finding("warn", "spatial.many_isolated", "Over half of cells have no neighbours",
            "$(round(Int, isolated_frac * 100))% of cells are isolated (no graph edge). Consider a larger radius or a kNN graph."))
    elseif mean_degree < SPARSE_GRAPH_MEAN_DEGREE
        md = round(mean_degree; digits = 1)
        push!(findings, qc_finding("warn", "spatial.sparse_graph"; value = md,
                                   detail = Dict{String,Any}("meanDegree" => md)))
    end
    findings
end

# The same check, re-run by a graph CONSUMER (clustRegions, neighbourStats) on the graph it loaded:
# `m` = the runner's `spatial_utils.graph_metrics` dict (nCells, nEdges = the GRAPH's edges, isolatedFrac,
# meanDegree). Each finding's detail names the graph, since the run's QC is not the graph's own.
function _loaded_graph_findings(m::AbstractDict, graph_suffix::AbstractString)
    get(m, "method", "") == "windows" && return _window_graph_qc_findings(Int(get(m, "nCells", 0)))
    fs = _neighbours_qc_findings(Int(get(m, "nCells", 0)), Int(get(m, "nEdges", 0)),
                                 Float64(get(m, "isolatedFrac", 0.0)), Float64(get(m, "meanDegree", 0.0)))
    for f in fs
        d = get(f, "detail", nothing)
        d isa AbstractDict || (d = Dict{String,Any}())
        d["graphSuffix"] = String(graph_suffix); f["detail"] = d
    end
    fs
end

# A FIXED-WINDOW graph has no edges by design, so the edge findings above don't apply: its QC is the
# grid and how full its tiles are. The one unambiguous problem is no cells at all. Pure (unit-tested).
_window_graph_qc_findings(n_cells::Integer) = n_cells == 0 ?
    [qc_finding("warn", "spatial.no_cells", "No cells",
        "The selection had no cells to lay windows over — check the population.")] : Dict{String,Any}[]

function _write_window_graph_qc!(img, graph_suffix, qc::AbstractDict, on_log)
    n_cells = Int(get(qc, "nCells", 0))
    shape = Int.(collect(get(qc, "gridShape", Int[])))
    write_qc(img, "spatialAnalysis.cellNeighbours", graph_suffix, _window_graph_qc_findings(n_cells);
             metrics = Dict{String,Any}("nCells" => n_cells, "nTiles" => Int(get(qc, "nTiles", 0)),
                                        "occupiedTiles" => Int(get(qc, "occupiedTiles", 0)),
                                        "cellsPerTile" => Float64(get(qc, "cellsPerTile", 0.0))))
    on_log("[QC] window graph: $(n_cells) cell(s) on a $(join(shape, " × ")) grid of " *
           "$(get(qc, "tile", 0)) µm tiles.")
end
