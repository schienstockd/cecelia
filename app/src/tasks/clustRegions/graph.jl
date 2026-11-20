# ── clustRegions: the neighbour graph a region run reads ─────────────────────────────────────────
#
# What kind of graph was picked decides how its neighbourhoods may be pooled. ONE rule
# (`region_pooling_problem`) serves the form — a check on "Neighbourhood over" that names the picked
# graph's kind, re-run when the graph changes — and Run, which refuses the same mismatch before Python
# starts. The graph's own QC, re-judged on the graph a run loaded, lives here too.

# How a run pools neighbourhoods (`spatial_utils.POOLINGS`): "frame" on any graph, "track" on a
# cell-to-cell graph built per frame, "movie" on a fixed-window graph.
const REGION_POOLINGS = ("frame", "track", "movie")

# What one image's graph file is: `:window` (fixed windows), `:frames` (cell-to-cell, per frame),
# `:pooled` (cell-to-cell over all frames together, so no `_t`), or `nothing` when there is no file.
function region_graph_kind(path::AbstractString)::Union{Symbol,Nothing}
    isfile(path) || return nothing
    spatial_graph_window_grid(path) === nothing || return :window
    isempty(selectable_cols(label_props(path), ["_t"])) ? :pooled : :frames
end

const _GRAPH_KIND_NAME = Dict(:window => "a window graph", :frames => "a cell graph",
                              :pooled => "a cell graph over all frames")

"""
    region_pooling_problem(pooling, kinds) -> String | Nothing

Why `pooling` cannot run on graphs of these `kinds` (one per image, `region_graph_kind`), or `nothing`.
"""
function region_pooling_problem(pooling::AbstractString, kinds)::Union{String,Nothing}
    pooling in REGION_POOLINGS || return "pick one of $(join(REGION_POOLINGS, ", "))"
    ks = unique(k for k in kinds if k !== nothing)
    isempty(ks) && return nothing
    (:window in ks && length(ks) > 1) &&
        return "the graph is a window graph on some images and a cell graph on others"
    k = first(ks)
    pooling == "track" && k == :window && return "Whole track needs a cell graph; this is a window graph"
    pooling == "track" && :pooled in ks &&
        return "Whole track needs a graph built per frame; this one pools all frames"
    pooling == "movie" && k != :window && return "Whole movie needs a window graph; this is a cell graph"
    nothing
end

region_graph_kinds(imgs, graph_suffix::AbstractString) =
    [region_graph_kind(img_spatial_graph_path(img, graph_suffix)) for img in imgs]

# The form-time check: names the picked graph's kind, or says why this pooling can't use it. Pure on the
# kinds (one per image, `nothing` = no such graph), so the verdict is testable without images.
function _region_pooling_advisory(pooling, graph_suffix::AbstractString, kinds)
    ks = [k for k in kinds if k !== nothing]
    isempty(ks) && return nothing
    prob = region_pooling_problem(string(pooling), ks)
    prob === nothing || return (severity = "fail", message = prob,
        tip = "Window graphs pool per frame or over the movie; cell graphs per frame or per track.")
    k = first(ks)
    (severity = "ok", message = "\"$graph_suffix\" is $(_GRAPH_KIND_NAME[k])",
     tip = k == :window ? "Each frame or Whole movie" : k == :frames ? "Each frame or Whole track" : "Each frame")
end

function _region_pooling_advisory(pooling, imgs, siblings::AbstractDict)
    sfx = string(get(siblings, "graphSuffix", "default"))
    _region_pooling_advisory(pooling, sfx, region_graph_kinds(imgs, sfx))
end

register_param_validator!("clustRegions.cluster", "pooling", _region_pooling_advisory;
                          reload_on = ["graphSuffix"])

# The neighbour graph's own QC, re-judged per image on the graph the run actually LOADED. Composition
# vectors on a sparse graph are noise, so the region run carries the same finding the graph task banks
# (`_neighbours_qc_findings`, same `SPARSE_GRAPH_MEAN_DEGREE`) where the regions are inspected. Pure:
# the parsed region_qc.json → uID → findings. A run from before the runner banked `graphMetrics` → none.
function _region_graph_findings(qc::AbstractDict, graph_suffix::AbstractString)
    out = Dict{String,Vector{Dict{String,Any}}}()
    gm = get(qc, "graphMetrics", nothing)
    gm isa AbstractDict || return out
    for (uid, m) in gm
        m isa AbstractDict || continue
        m = Dict{String,Any}(String(k) => v for (k, v) in m)    # nested JSON3 objects key by Symbol
        fs = _loaded_graph_findings(m, graph_suffix)
        isempty(fs) || (out[String(uid)] = fs)
    end
    out
end

function _region_graph_findings(qc_path::AbstractString, graph_suffix::AbstractString)
    isfile(qc_path) || return Dict{String,Vector{Dict{String,Any}}}()
    try
        _region_graph_findings(JSON3.read(read(qc_path, String), Dict{String,Any}), graph_suffix)
    catch
        Dict{String,Vector{Dict{String,Any}}}()
    end
end
