# ── spatialAnalysis.regionEdge — every cell's distance to a region's boundary, and inside or not ──
#
# The region is a set of cells: the whole tissue (the segmentation's "all" root), a region cluster, a
# gate, aggregated cells. Its shape is drawn from those cells' centroids alone — never from pixels, so
# it works on any segmented image — closed with a radius (gaps narrower than ~2× the radius are inside),
# holes filled for the tissue (a tear is not boundary) but kept for a population (a region nested in its
# ring is not part of it), and the image frame never counts as boundary. The geometry is
# `python/cecelia/utils/region_edge_utils.py`; the runner is `region_edge_run.py`.
#
# Writes, for every cell of the region's segmentation: `<popType>.cell.edge_dist#<name>` (µm to the
# boundary, from either side; NaN where the shape has no boundary in view) and — unless the region is
# every cell — `<popType>.cell.inside#<name>` (0/1). The boundary itself goes to
# `regionEdges/<name>.json` (`img_region_edge_path`), which the viewer and every recording draw
# (`region_edge_lines.jl`).

using DataFrames: nrow

struct RegionEdge <: CciaTask end
register_builtin_task!("spatialAnalysis.regionEdge", RegionEdge(); spec = joinpath(@__DIR__, "regionEdge.json"))

Base.@kwdef struct RegionEdgeParams
    edgeName::String          = "tissue"
    regionPops::Vector{String} = String[]
    closeRadius::Float64      = 75.0
    fillHoles::String         = "auto"     # "auto" | "on" | "off"
end

function parse_region_edge_params(d::AbstractDict)::RegionEdgeParams
    RegionEdgeParams(;
        edgeName    = string(something(get(d, "edgeName", "tissue"), "tissue")),
        regionPops  = _str_list(d, "regionPops"),
        closeRadius = Float64(something(get(d, "closeRadius", 75.0), 75.0)),
        fillHoles   = _fill_holes_choice(get(d, "fillHoles", "auto")))
end

# a stored bool (an earlier spec) reads as on / off
_fill_holes_choice(v) = v isa Bool ? (v ? "on" : "off") : string(something(v, "auto"))

"""
    region_edge_fill_holes(choice, whole) -> Bool

Whether holes in the shape are filled. `auto`: for the tissue (`whole`, every cell) a hole is a tear and
is filled; for a population it is left open — another region nested inside this one (a crypt core in its
ring) is not part of it.
"""
region_edge_fill_holes(choice::AbstractString, whole::Bool) = choice == "auto" ? whole : choice == "on"

"""
    region_edge_col(pop_type, kind, name) -> String

A region-edge run's obs column: `kind` `"edge_dist"` (µm) or `"inside"` (0/1) — `<pt>.cell.<kind>#<name>`.
The one naming rule, used by the task, the run listing and the run deletion.
"""
region_edge_col(pop_type::AbstractString, kind::AbstractString, name::AbstractString) =
    "$(pop_type).cell.$(kind)#$(name)"

const REGION_EDGE_COL_RE = r"^(.+)\.cell\.(edge_dist|inside)#(.+)$"

#: More than this share of a region's cells in small pieces (`SMALL_PIECE_CELLS` in
#: region_edge_utils.py) reads as a scattered region: no compact edge to measure from.
const REGION_EDGE_SCATTERED_FRAC = 0.5

"""
    region_edge_qc_findings(qc) -> Vector

Pure: the run's findings from its runner QC (`fracNoBoundary`, `fracInSmallPieces`). No boundary in view
(the shape covers the whole frame) leaves every distance unmeasured; most of the region's cells in small
pieces means they are scattered, not a compact area. A solid region with stray specks is fine.
"""
function region_edge_qc_findings(qc::AbstractDict)
    out = Dict{String,Any}[]
    Float64(get(qc, "fracNoBoundary", 0.0)) >= 1.0 && push!(out, qc_finding("warn", "regionEdge.no_boundary"))
    Float64(get(qc, "fracInSmallPieces", 0.0)) > REGION_EDGE_SCATTERED_FRAC &&
        push!(out, qc_finding("warn", "regionEdge.scattered"))
    out
end

"""
    region_edge_inputs(img, p) -> NamedTuple | String

What the run and its preview both compute from: the segmentation, its pop-type namespace, whether the
region is every cell, the region's labels (`nothing` = every cell), the resolved hole setting and µm
sizes. A `String` is the error to report.
"""
function region_edge_inputs(img::CciaImage, p::RegionEdgeParams)
    isempty(p.regionPops) && return "select the region's cells (the \"all\" root for the tissue)"
    value_name = pops_value_name(p.regionPops)
    img_has_value_name(img, value_name) || return "no cell table for $(value_name)"
    whole = _is_all_cells(p.regionPops)          # the tissue: every cell, so `inside` would be all 1
    region_labels = nothing
    if !whole
        mem = pop_df_multi(img, p.regionPops; value_name = value_name, granularity = :cell,
                           restrict_to = value_name, pop_cols = String[])
        nrow(mem) == 0 && return "no cells for $(p.regionPops)"
        region_labels = unique(Int.(mem.label))
    end
    (; value_name, pop_type = pop_namespace(img, p.regionPops; value_name = value_name), whole,
       region_labels, fill = region_edge_fill_holes(p.fillHoles, whole), sizes = first(img_physical_sizes(img)))
end

# Previewable: the worker draws the shape the run would measure from (`_preview_region_edge`).
task_previewable(::RegionEdge) = true

function preview_params(::RegionEdge, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    p = parse_region_edge_params(Dict{String,Any}(String(k) => v for (k, v) in params))
    r = region_edge_inputs(img, p)
    r isa String && error(r)
    Dict{String,Any}("propsPath" => img_label_props_path(img, r.value_name), "regionLabels" => r.region_labels,
                     "physicalSizes" => r.sizes, "closeRadius" => p.closeRadius, "fillHoles" => r.fill)
end

function _run_task(::RegionEdge, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_region_edge_params(params)
    r = region_edge_inputs(img, p)
    r isa String && (on_log("[ERROR] regionEdge: $r"); return nothing)
    (; value_name, pop_type, whole, region_labels, fill, sizes) = r
    on_progress(1, 3)

    edge_path = img_region_edge_path(img, p.edgeName)
    mkpath(dirname(edge_path))
    qc_out_path = joinpath(task_run_dir(img._dir), "region_edge_qc.json")
    props_path = img_label_props_path(img, value_name)
    dist_col = region_edge_col(pop_type, "edge_dist", p.edgeName)
    inside_col = whole ? nothing : region_edge_col(pop_type, "inside", p.edgeName)
    on_log("[INFO] regionEdge: $(whole ? "every cell" : "$(length(region_labels)) cells") of $(value_name), " *
           "radius $(p.closeRadius) µm, holes $(fill ? "filled" : "kept")$(p.fillHoles == "auto" ? " (auto)" : "")")

    task_params = Dict{String,Any}(
        "imPath" => img_filepath(img), "propsPath" => props_path, "regionLabels" => region_labels,
        "physicalSizes" => sizes, "closeRadius" => p.closeRadius, "fillHoles" => fill,
        "distCol" => dist_col, "insideCol" => inside_col, "edgePath" => edge_path, "qcOutPath" => qc_out_path)
    # a re-run onto the tissue drops the old run's `inside` column, which would otherwise outlive it
    stale = String[c for c in col_names(label_props(props_path); data_type = :obs)
                   if c == region_edge_col(pop_type, "inside", p.edgeName)]
    isempty(stale) || (label_props(props_path) |> drop_obs(stale) |> save!)
    on_progress(2, 3)

    ok = run_py("tasks/spatialAnalysis/region_edge_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] regionEdge: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        metrics = Dict{String,Any}(k => qc[k] for k in ("nCells", "nRegionCells", "fracInside", "fracNoBoundary", "maxPieces", "fracInSmallPieces")
                                   if haskey(qc, k))
        write_qc(img, "spatialAnalysis.regionEdge", p.edgeName, region_edge_qc_findings(qc); metrics = metrics)
        on_log("[QC] $(get(qc, "nRegionCells", 0)) region cells, $(get(qc, "maxPieces", 0)) piece(s), " *
               "$(round(Int, 100 * Float64(get(qc, "fracNoBoundary", 0.0))))% with no edge in view.")
    catch e
        on_log("[QC] could not compute regionEdge QC: $e")
    end
    on_progress(3, 3)
    Dict{String,Any}("valueName" => value_name, "edgePath" => edge_path)
end
