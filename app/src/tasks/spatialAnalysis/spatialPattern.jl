# ── spatialAnalysis.spatialPattern — is one population randomly placed among the tissue's cells? ──
#
# docs/todo/SPATIAL_PATTERN_PLAN.md. The REFERENCE is a neighbour graph's nodes — the cells it was
# built over (Decision 1), so "which cells are the tissue" is chosen once, on the graph. Each target
# population is scored against random relabelling of those nodes, within a frame for a per-timepoint
# graph (Decision 2): a nearest-neighbour ratio + p, a pair-ratio curve with an envelope, and a
# descriptive Clark–Evans R (Decision 3). Output: `spatialPattern/{suffix}.json`, one flat record per
# target (Decision 5).
#
# Division of labour: Julia resolves each target to (value_name, labels) via `pop_df_multi`, ONE call
# per target so a cell in two selected populations counts in both (Decision 6). Python
# (`spatial_pattern_run.py` + `python/cecelia/utils/spatial_pattern_utils.py`) loads the graph and
# does the statistics.

using DataFrames: nrow, groupby

struct SpatialPattern <: CciaTask end

Base.@kwdef struct SpatialPatternParams
    pops::Vector{String}  = String[]
    graphSuffix::String   = "default"
    patternSuffix::String = "default"
    rMax::Float64         = 100.0
    nPermutations::Int    = 1000
end

function parse_spatial_pattern_params(d::AbstractDict)::SpatialPatternParams
    SpatialPatternParams(;
        pops          = _str_list(d, "pops"),
        graphSuffix   = string(get(d, "graphSuffix", "default")),
        patternSuffix = string(get(d, "patternSuffix", "default")),
        rMax          = Float64(get(d, "rMax", 100.0)),
        nPermutations = Int(get(d, "nPermutations", 1000)))
end

const PATTERN_MIN_TARGET = 20        # fewer target cells than this: too few to judge a spacing
const PATTERN_MIN_COVERAGE = 0.9     # share of a target's cells that must be nodes of the graph
const PATTERN_MAX_TARGET_FRACTION = 0.95   # target ≈ the whole graph: nothing to compare against

# Pure QC (advisory, never gates), one pass over the runner's per-target records.
function _spatial_pattern_findings(records::AbstractVector)
    isempty(records) && return [qc_finding("warn", "pattern.no_targets", "No populations scored",
        "Check the populations: none resolved to cells.")]
    findings = Dict{String,Any}[]
    for r in records
        pop = String(r["pop"]); n = Int(r["nTarget"])
        if n == 0
            push!(findings, qc_finding("warn", "pattern.not_in_graph", "Cells not in the graph",
                "Rebuild the neighbour graph over this population's segmentation."; detail = pop))
            continue
        end
        Float64(something(r["coverage"], 0.0)) < PATTERN_MIN_COVERAGE &&
            push!(findings, qc_finding("warn", "pattern.low_coverage", "Some cells not in the graph",
                "Rebuild the neighbour graph over these segmentations.";
                detail = "$(pop): $(round(Int, 100 * Float64(r["coverage"])))% of cells in the graph"))
        n < PATTERN_MIN_TARGET &&
            push!(findings, qc_finding("warn", "pattern.too_few", "Too few cells to judge",
                "Read the ratio as descriptive."; detail = "$(pop): $(n) cells"))
        Float64(r["targetFraction"]) >= PATTERN_MAX_TARGET_FRACTION &&
            push!(findings, qc_finding("warn", "pattern.target_is_reference", "Population is the whole graph",
                "Build the neighbour graph over more cells than this population."; detail = pop))
    end
    findings
end

# one target population → the per-segmentation label lists the runner matches graph nodes against
function _pattern_target(img::CciaImage, pop::AbstractString)
    df = pop_df_multi([img], [img.uid], [String(pop)]; pop_cols = String[], granularity = :cell)
    segs = nrow(df) == 0 ? Dict{String,Any}[] :
        [Dict{String,Any}("valueName" => String(first(g.value_name)), "labels" => Int.(g.label))
         for g in groupby(df, :value_name)]
    Dict{String,Any}("pop" => String(pop), "segments" => segs)
end

function _run_task(::SpatialPattern, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_spatial_pattern_params(params)
    isempty(p.pops) && (on_log("[ERROR] spatialPattern: select at least one population"); return nothing)
    p.rMax > 0 || (on_log("[ERROR] spatialPattern: max distance must be > 0"); return nothing)
    on_progress(1, 3)

    # the graph is a REQUIRED input (Decision 1): fail with the available names, never build one here
    graph_path = img_spatial_graph_path(img, p.graphSuffix)
    if !isfile(graph_path)
        have = img_spatial_graph_suffixes(img)
        on_log("[ERROR] spatialPattern: no neighbour graph \"$(p.graphSuffix)\" for this image — " *
               (isempty(have) ? "run 'Neighbour graph' first" : "available: $(join(have, ", "))"))
        return nothing
    end

    targets = [_pattern_target(img, pop) for pop in p.pops]
    all(t -> isempty(t["segments"]), targets) &&
        (on_log("[ERROR] spatialPattern: no cells for pops=$(p.pops)"); return nothing)

    out_path = img_pattern_path(img, p.patternSuffix)
    qc_out_path = joinpath(task_run_dir(img._dir), "spatial_pattern_qc.json")
    on_log("[INFO] spatialPattern: graph=$(p.graphSuffix) pops=$(p.pops) rMax=$(p.rMax) µm " *
           "permutations=$(p.nPermutations) → $(p.patternSuffix)")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "graphPath" => graph_path, "graphSuffix" => p.graphSuffix, "targets" => targets,
        # each node's frame on a pooled-frame graph comes from its cell table (Decision 4)
        "propsPaths" => Dict{String,Any}(vn => img_label_props_path(img, vn) for vn in img_value_names(img)),
        "rMax" => p.rMax, "nRadii" => 25, "nPermutations" => p.nPermutations, "randomState" => 0,
        "outPath" => out_path, "qcOutPath" => qc_out_path)
    ok = run_py("tasks/spatialAnalysis/spatial_pattern_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] spatialPattern: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        recs = [Dict{String,Any}(String(k) => v for (k, v) in r) for r in get(qc, "records", Any[])]
        findings = _spatial_pattern_findings(recs)
        n_min = isempty(recs) ? 0 : minimum(Int(r["nTarget"]) for r in recs)
        frac_max = isempty(recs) ? 0.0 : maximum(Float64(r["targetFraction"]) for r in recs)
        n_sig = count(r -> r["significant"] === true, recs)
        write_qc(img, "spatialAnalysis.spatialPattern", p.patternSuffix, findings;
                 metrics = Dict{String,Any}("nTargetMin" => n_min, "targetFractionMax" => frac_max,
                                            "nSignificant" => n_sig))
        on_log("[QC] $(length(recs)) population(s) scored, $(n_sig) differ from random placement.")
    catch e
        on_log("[QC] could not compute spatialPattern QC: $e")
    end
    on_progress(3, 3)

    on_log("[INFO] spatialPattern done → $(basename(out_path))")
    Dict{String,Any}("patternPath" => out_path, "pops" => length(p.pops), "graphSuffix" => p.graphSuffix)
end
