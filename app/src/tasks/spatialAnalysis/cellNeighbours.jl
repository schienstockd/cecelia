# ── spatialAnalysis.cellNeighbours — spatial neighbour graph (squidpy) ──────────────
#
# The shared substrate for all spatial analysis + region clustering (docs/todo/SPATIAL_REGIONS_PLAN.md,
# Phase 2): build a per-cell spatial neighbour graph over a segmentation (or a population subset) and
# persist it as a `{vn}.spatial.h5ad` sidecar (squidpy's native obsp connectivities/distances). Region
# composition (Phase 3) and the neighbourhood statistics (Phase 5) read this graph back.
#
# Division of labour (docs/ARCHITECTURE.md): Julia resolves the value_name, the optional population
# subset (member labels via `pop_df`), and the physical pixel sizes (centroids are stored in pixels; the
# radius is in µm). Python (`cell_neighbours_run.py`) reads centroids through the sanctioned
# LabelPropsView, scales them to physical units, runs `sq.gr.spatial_neighbors`, and writes the graph
# h5ad. This mirrors the legacy `cell_neighbours.py` (squidpy delaunay/grid/radius), re-homed onto the
# new boundary. squidpy attribution: it is a bundled dependency (THIRD_PARTY.md); the graph modes are
# squidpy's, not ported CytoMAP code.

using DataFrames: nrow

struct CellNeighbours <: CciaTask end

# Pure QC helper (unit-tested per docs/MODULES.md): objective graph metrics → advisory findings. The
# unambiguous problems are an empty graph (no edges — usually a radius far below the cell spacing, or
# wrong units) and a majority of isolated cells. Counts themselves bank as metrics, not findings.
function _neighbours_qc_findings(n_cells::Integer, n_edges::Integer, isolated_frac::Real)
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
    end
    findings
end

# a value_name-qualified root pick (e.g. "B/", the picker's per-segmentation "all" entry) means
# "every cell of that segmentation" — no membership filter. The accepts picker always offers this root
# when cell gates are accepted (population_accept_groups), so it needs no includeRoot flag in the spec.
_is_all_cells(pops) = !isempty(pops) && all(p -> is_root(_split_pop_ref(p, "default")[2]), pops)

function _run_task(::CellNeighbours, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    method     = string(get(params, "neighbourMethod", "delaunay"))
    pops       = _str_list(params, "pops")     # shared helper (clustPops/cluster.jl) — module-visible
    isempty(pops) && (on_log("[ERROR] cellNeighbours: select a population, or the segmentation's \
        'all cells' root, to build the graph over"); return nothing)
    # segmentation derived from the pick (value_name-prefixed; the "… · all" root carries it too) — no
    # separate dropdown (legacy used one only to name the output; the graph came from the pops).
    value_name = pops_value_name(pops)
    on_progress(1, 3)

    # ── optional population subset → member labels (else all cells) ──
    # pops may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves across types.
    labels = nothing
    if !_is_all_cells(pops)
        df = pop_df_multi(img, pops; value_name = value_name, granularity = :cell, pop_cols = String[], restrict_to = value_name)
        nrow(df) == 0 && (on_log("[ERROR] cellNeighbours: no cells for pops=$(pops)"); return nothing)
        labels = Int.(df.label)
    end

    props_path = img_label_props_path(img, value_name)
    graph_path = replace(props_path, r"\.h5ad$" => ".spatial.h5ad")
    (sizes, _) = img_physical_sizes(img)       # [sz, sy, sx] (skimage order, matches centroid cols)
    qc_out_path = joinpath(task_run_dir(img._dir), "spatial_qc.json")

    on_log("[INFO] cellNeighbours: $(value_name) method=$(method) " *
           (labels === nothing ? "(all cells)" : "($(length(labels)) cells)"))
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "propsPath" => props_path, "graphPath" => graph_path,
        "physicalSizes" => sizes,
        "labels" => labels,                     # nothing → all cells (JSON null)
        "neighbourMethod" => method,
        "neighbourRadius" => Float64(get(params, "neighbourRadius", 30.0)),
        "nNeighbours" => Int(get(params, "nNeighbours", 6)),
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_neighbours_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] cellNeighbours: Python runner failed"); return nothing)

    # QC (advisory): the runner banks objective graph metrics; findings + metrics recorded here.
    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cells = Int(get(qc, "nCells", 0)); n_edges = Int(get(qc, "nEdges", 0))
        isolated_frac = Float64(get(qc, "isolatedFrac", 0.0))
        mean_degree   = Float64(get(qc, "meanDegree", 0.0))
        write_qc(img, "spatialAnalysis.cellNeighbours", value_name,
                 _neighbours_qc_findings(n_cells, n_edges, isolated_frac);
                 metrics = Dict{String,Any}("nCells" => n_cells, "nEdges" => n_edges, "meanDegree" => mean_degree))
        on_log("[QC] neighbour graph: $(n_edges) edge(s) over $(n_cells) cell(s).")
    catch e
        on_log("[QC] could not compute neighbour QC: $e")
    end
    on_progress(3, 3)

    on_log("[INFO] cellNeighbours done → $(basename(graph_path))")
    Dict{String,Any}("valueName" => value_name, "graphPath" => graph_path)
end
