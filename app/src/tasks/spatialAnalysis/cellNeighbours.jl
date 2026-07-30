# ── spatialAnalysis.cellNeighbours — the spatial neighbour graph (squidpy) ──────────────
#
# THE substrate for every spatial readout (docs/todo/SPATIAL_REGIONS_PLAN.md, Phase 2). Builds a
# per-cell spatial neighbour graph, pooled across the selected segmentations, and persists it as
# `spatialGraph/{suffix}.h5ad` (squidpy's native obsp connectivities/distances + obsm coordinates in µm).
#
# The graph is POP-AGNOSTIC: it stores only each node's IDENTITY (`valueName`, `label`). Cell
# interactions (`spatialAnalysis.neighbourStats`) and neighbourhood-composition regions
# (`clustRegions.cluster`) both LOAD this graph and attach their own population labelling at analysis
# time. Nothing else builds a graph. Consequences that motivated the design:
#   • interactions need no regions — "who neighbours whom" is a labelling of this graph, nothing more;
#   • one neighbourhood definition across every readout, and the graph parameters live in ONE place;
#   • the expensive step is paid once per image, not once per analysis.
#
# This is the legacy architecture restored. Old-R `cellNeighbours` persisted a neighbour table which
# `clustRegions/kmeansClust.R:45` read back via `cciaObj$spatialDT(...)` and joined `popDT` onto — with
# `neighbour_value_name`/`neighbour_label` identity per edge, i.e. cross-segmentation and pop-agnostic,
# exactly as here. The interim new-Cecelia code had each task rebuild its own basis-restricted graph
# in-process, so `{vn}.spatial.h5ad` had no consumers at all; that divergence is what this removes.
#
# Division of labour (docs/ARCHITECTURE.md): Julia resolves which cells are nodes (the segmentations +
# labels named by the pops, via `pop_df`) and the physical pixel sizes (centroids are stored in pixels;
# the radius is in µm). Python (`cell_neighbours_run.py`) reads centroids through the sanctioned
# LabelPropsView, scales them, runs squidpy, and writes the graph. squidpy attribution: a bundled
# dependency (THIRD_PARTY.md); the graph modes are squidpy's, not ported CytoMAP code.

using DataFrames: nrow, groupby

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

# The graph's NODE SET, one entry per segmentation: {valueName, propsPath, labels}. `labels = nothing`
# means "every cell of this segmentation" (the runner then reads the whole centroid table). Pop-agnostic
# by construction — no popCodes here, unlike `_basis_segments` which resolves an analysis's labelling.
function _graph_segments(img::CciaImage, pops)
    if _is_all_cells(pops)
        vns = unique(String[_split_pop_ref(p, "default")[1] for p in pops])
        return Dict{String,Any}[Dict{String,Any}(
            "valueName" => vn, "propsPath" => img_label_props_path(img, vn),
            "labels" => nothing) for vn in vns if img_has_value_name(img, vn)]
    end
    df = pop_df_multi(img, pops; granularity = :cell, pop_cols = String[])
    nrow(df) == 0 && return Dict{String,Any}[]
    segs = Dict{String,Any}[]
    for g in groupby(df, :value_name)
        vn = string(first(g.value_name))
        push!(segs, Dict{String,Any}(
            "valueName" => vn, "propsPath" => img_label_props_path(img, vn),
            "labels" => unique(Int.(g.label))))
    end
    segs
end

function _run_task(::CellNeighbours, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    method = string(get(params, "neighbourMethod", "delaunay"))
    pops   = _str_list(params, "pops")     # shared helper (clustPops/cluster.jl) — module-visible
    isempty(pops) && (on_log("[ERROR] cellNeighbours: select the segmentation's 'all cells' root (or \
        populations) to build the graph over"); return nothing)
    suffix = string(get(params, "graphSuffix", "default"))
    per_t  = Bool(get(params, "perTimepoint", false))
    on_progress(1, 3)

    segments = _graph_segments(img, pops)
    isempty(segments) &&
        (on_log("[ERROR] cellNeighbours: no cells for pops=$(pops)"); return nothing)

    graph_path = img_spatial_graph_path(img, suffix)
    (sizes, _) = img_physical_sizes(img)       # [sz, sy, sx] (skimage order, matches centroid cols)
    qc_out_path = joinpath(task_run_dir(img._dir), "spatial_qc.json")

    vns = String[seg["valueName"] for seg in segments]
    on_log("[INFO] cellNeighbours: segmentations=$(join(vns, ", ")) method=$(method) " *
           (per_t ? "per-timepoint " : "") * "→ spatialGraph/$(suffix).h5ad")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "segments" => segments, "graphPath" => graph_path,
        "physicalSizes" => sizes,
        "neighbourMethod" => method,
        "neighbourRadius" => Float64(get(params, "neighbourRadius", 30.0)),
        "nNeighbours" => Int(get(params, "nNeighbours", 6)),
        "perTimepoint" => per_t,
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_neighbours_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] cellNeighbours: Python runner failed"); return nothing)

    # QC (advisory): the runner banks objective graph metrics; findings + metrics recorded here. Keyed on
    # the RUN suffix (the graph is cross-segmentation, so a value_name would be the wrong key).
    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_cells = Int(get(qc, "nCells", 0)); n_edges = Int(get(qc, "nEdges", 0))
        isolated_frac = Float64(get(qc, "isolatedFrac", 0.0))
        mean_degree   = Float64(get(qc, "meanDegree", 0.0))
        write_qc(img, "spatialAnalysis.cellNeighbours", suffix,
                 _neighbours_qc_findings(n_cells, n_edges, isolated_frac);
                 metrics = Dict{String,Any}("nCells" => n_cells, "nEdges" => n_edges, "meanDegree" => mean_degree))
        on_log("[QC] neighbour graph: $(n_edges) edge(s) over $(n_cells) cell(s).")
    catch e
        on_log("[QC] could not compute neighbour QC: $e")
    end
    on_progress(3, 3)

    on_log("[INFO] cellNeighbours done → spatialGraph/$(suffix).h5ad")
    Dict{String,Any}("graphSuffix" => suffix, "valueNames" => vns, "graphPath" => graph_path)
end
