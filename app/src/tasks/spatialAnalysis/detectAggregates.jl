# ── spatialAnalysis.detectAggregates — spatial aggregate detection (DBSCAN, Julia) ──
#
# Detects spatial AGGREGATES: groups of a population's cells that are physically proximal (a T-cell
# aggregate = a cluster of T cells in the tissue). Density-based, NOT feature-space clustering — hence
# "aggregate", kept distinct from the `clust` poptype (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 10).
#
# Julia-native: the legacy used the R `dbscan` package; here it's a from-scratch port onto Clustering.jl
# (`dbscan`). Membership AND µm centroids come from ONE `pop_df(…; centroids = :physical)` call, and
# DBSCAN on centroids is a simple density op — so this needs no Python round-trip (the celltrackR
# track-measures precedent). Writes a NAMED run (`aggregateName`) back via the sanctioned label-props
# writer — `<popType>.cell.is.aggregate#<name>`, `.aggregate.id#<name>`, `.is.isolated#<name>`, and with
# linking on the event columns (`aggregate_runs.jl`, where the naming and the pure steps live). Several
# named runs sit side by side, so one segmentation can hold different scenarios (single debris cells,
# loose fragment clumps) and each run's settings come back with its name.
#
# Per timepoint on a timecourse BY DEFAULT: pooled x/y/z would cluster a cell with where its neighbours
# were (or it itself was) in other frames, so a slow track alone reads as an "aggregate". Pooling all
# frames is an explicit opt-in (`perTimepoint = false`). A still image has no frames; both are one pass.

using DataFrames: nrow, DataFrame
using Clustering: dbscan

struct DetectAggregates <: CciaTask end

Base.@kwdef struct DetectAggregatesParams
    pops::Vector{String}          = String[]
    aggregateName::String         = AGGREGATE_DEFAULT_NAME
    clustDiameter::Float64        = 15.0
    minCells::Int                 = 5
    maxCells::Int                 = 0          # 0 = no cap
    excludePops::Vector{String}   = String[]
    excludeDistance::Float64      = 5.0
    perTimepoint::Bool            = true
    linkEvents::Bool              = false      # link aggregates + isolated cells across frames
    linkFrames::Int               = 3
    linkDistance::Float64         = 5.0
    precededPops::Vector{String}  = String[]
    precededFrames::Int           = 12
    precededDistance::Float64     = 8.0
end

function parse_detect_aggregates_params(d::AbstractDict)::DetectAggregatesParams
    DetectAggregatesParams(;
        pops             = _str_list(d, "pops"),
        aggregateName    = aggregate_run_name(d),
        clustDiameter    = Float64(get(d, "clustDiameter", 15.0)),
        minCells         = Int(get(d, "minCells", 5)),
        maxCells         = Int(get(d, "maxCells", 0)),
        excludePops      = _str_list(d, "excludePops"),
        excludeDistance  = Float64(get(d, "excludeDistance", 5.0)),
        perTimepoint     = Bool(get(d, "perTimepoint", true)),
        linkEvents       = Bool(get(d, "linkEvents", false)),
        linkFrames       = Int(get(d, "linkFrames", 3)),
        linkDistance     = Float64(get(d, "linkDistance", 5.0)),
        precededPops     = _str_list(d, "precededPops"),
        precededFrames   = Int(get(d, "precededFrames", 12)),
        precededDistance = Float64(get(d, "precededDistance", 8.0)))
end

# DBSCAN cluster ids for one coord block (rows = points, cols = spatial dims); 0 = noise, 1..k = cluster.
# Clustering.jl wants (dims × points). min_cluster_size = the legacy `minPts`; a core point needs
# `minPts-1` neighbours within `eps`.
function _aggregate_ids(coords::AbstractMatrix{<:Real}, eps::Real, min_cells::Integer)::Vector{Int}
    size(coords, 1) < min_cells && return zeros(Int, size(coords, 1))
    r = dbscan(permutedims(Float64.(coords)), Float64(eps);
               min_neighbors = max(1, min_cells - 1), min_cluster_size = min_cells)
    r.assignments
end

# DBSCAN per frame (offset ids so they stay unique across frames), or once over every row when
# `frames === nothing` (still image, or pooling opted into).
function _aggregate_ids_by_frame(coords::AbstractMatrix{<:Real}, frames, eps::Real, min_cells::Integer)::Vector{Int}
    frames === nothing && return _aggregate_ids(coords, eps, min_cells)
    ids = zeros(Int, size(coords, 1)); offset = 0
    for tv in sort(unique(frames))
        m = frames .== tv
        sub = _aggregate_ids(coords[m, :], eps, min_cells)
        ids[m] = [i == 0 ? 0 : i + offset for i in sub]
        offset += maximum(sub; init = 0)
    end
    ids
end

function _aggregate_qc_findings(n_cells::Integer, frac::Real)
    n_cells == 0 && return [qc_finding("warn", "aggregate.no_cells", "No cells",
        "No cells in the selected population — check the population selection.")]
    Dict{String,Any}[]
end

function _run_task(::DetectAggregates, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_detect_aggregates_params(params)
    isempty(p.pops) && (on_log("[ERROR] detectAggregates: select a population to detect aggregates of"); return nothing)
    # segmentation derived from the populations (value_name-prefixed picks) — no dropdown (legacy parity)
    value_name = pops_value_name(p.pops)
    # pops may mix types (flow gates, clusters, regions, tracked cells) — resolve across types and
    # namespace output by cell kind (tracked → live.cell.*, else flow.cell.*)
    pop_type   = pop_namespace(img, p.pops; value_name = value_name)
    on_progress(1, 3)

    # member cells WITH their µm coordinates in ONE read (`pop_df` is the population accessor,
    # docs/POPULATION.md); the exclude / preceded populations are read the same way
    cdf = pop_df_multi(img, p.pops; value_name = value_name, granularity = :cell,
                       restrict_to = value_name, centroids = :physical)
    nrow(cdf) == 0 && (on_log("[ERROR] detectAggregates: no cells for pops=$(p.pops)"); return nothing)
    # µm point cloud + frames — the contacts' assembly (`_centroid_matrix`); pooling drops the frames
    pts(df) = (m = _centroid_matrix(img, value_name, df); (m[1], p.perTimepoint ? m[3] : nothing))
    coords, frames = pts(cdf)
    ref(pops) = pts(pop_df_multi(img, pops; value_name = value_name, granularity = :cell,
                                 restrict_to = value_name, centroids = :physical))
    on_progress(2, 3)

    # a cell beside an excluded population (an intact cell) is left out of every group, and is never isolated
    excluded = isempty(p.excludePops) ? falses(nrow(cdf)) :
               _within_mask(coords, frames, ref(p.excludePops)..., p.excludeDistance)
    keep = findall(.!excluded)
    ids  = zeros(Int, nrow(cdf))
    ids[keep] = _aggregate_ids_by_frame(coords[keep, :], frames === nothing ? nothing : frames[keep],
                                        p.clustDiameter, p.minCells)
    ids = _cap_aggregates(ids, p.maxCells)
    # isolated = no other selected cell within the neighbourhood (excluded cells still count as company)
    isolated = _isolated_mask(coords, frames, p.clustDiameter) .& .!excluded

    name = p.aggregateName
    out = DataFrame("label" => cdf.label,
                    aggregate_col(pop_type, "is.aggregate", name) => Float64.(ids .> 0),
                    aggregate_col(pop_type, "aggregate.id", name) => Float64.(ids),
                    aggregate_col(pop_type, "is.isolated", name) => Float64.(isolated))
    linked = frames !== nothing && p.linkEvents
    if linked
        ev = _aggregate_events(ids, isolated, coords, frames, p.linkFrames, p.linkDistance)
        out[!, aggregate_col(pop_type, "aggregate.event_id", name)] = Float64.(ev)
        out[!, aggregate_col(pop_type, "aggregate.frames", name)]   = _event_frames(ev, frames)
        isempty(p.precededPops) || (out[!, aggregate_col(pop_type, "aggregate.preceded", name)] =
            _event_preceded(ev, coords, frames, _points_by_frame(ref(p.precededPops)...),
                            p.precededFrames, p.precededDistance; first_frame = 0))
    end
    # a re-run under this name replaces it whole: drop the columns this run does not write (a previous
    # run's event columns), never another name's — and a `default` run supersedes the unnamed one
    props_path = img_label_props_path(img, value_name)
    legacy = legacy_aggregate_cols(col_names(label_props(props_path); data_type = :obs), pop_type, name)
    stale  = vcat(setdiff(aggregate_run_cols(pop_type, name), names(out)), legacy)
    label_props(props_path) |> drop_obs(stale) |> add_obs(out) |> save!
    isempty(legacy) || on_log("[INFO] detectAggregates: replaced the unnamed aggregate run — its " *
                              "`$(AGGREGATED_POP_NAME)` populations are now `$(first(aggregate_pop_names(name)))`")

    # the reusable populations (Decision 14): aggregated and isolated cells under each input pop,
    # resolved lazily by pop_df — so either flows into any downstream popSelection, no hand-drawn gate
    agg_pop, iso_pop = aggregate_pop_names(name)
    agg_paths = ensure_filter_pop!(img, pop_type, value_name, p.pops, agg_pop;
                                   filter_measure = aggregate_col(pop_type, "is.aggregate", name),
                                   filter_fun = "gt", filter_values = 0)
    ensure_filter_pop!(img, pop_type, value_name, p.pops, iso_pop;
                       filter_measure = aggregate_col(pop_type, "is.isolated", name),
                       filter_fun = "gt", filter_values = 0)

    n_agg = length(unique(ids[ids .> 0]))
    frac  = count(>(0), ids) / nrow(cdf)
    write_qc(img, "spatialAnalysis.detectAggregates", aggregate_qc_key(value_name, name),
             _aggregate_qc_findings(nrow(cdf), frac);
             metrics = Dict{String,Any}("nCells" => nrow(cdf), "nAggregates" => n_agg,
                                        "fracAggregated" => frac, "nIsolated" => count(isolated),
                                        "nExcluded" => count(excluded)))
    on_log("[INFO] detectAggregates '$name': $(n_agg) aggregate(s), $(round(Int, frac*100))% of cells aggregated, " *
           "$(count(isolated)) isolated, $(count(excluded)) excluded.")
    isempty(agg_paths) || on_log("[INFO] detectAggregates: populations → $(join(agg_paths, ", ")) (+ $iso_pop)")
    on_progress(3, 3)

    Dict{String,Any}("valueName" => value_name, "aggregateName" => name,
                     "nAggregates" => n_agg, "fracAggregated" => frac)
end
