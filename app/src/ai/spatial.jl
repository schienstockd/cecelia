# ── Observer/MCP spatial summary ────────────────────────────────────────────────────
#
# Per image, the two spatial readouts made MCP-accessible (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision
# 9 — flat + interpretable so Claude can answer "which cell types avoid each other?" / "how many regions
# are there?"): the region-clustering runs (`regions.{suffix}` obs distributions) and the pairwise
# cell-type contact statistics (`spatialAnalysis.neighbourStats` sidecars — observed/expected/log-odds
# per population pair, association vs avoidance). Mirrors cluster_summary (behaviour_clusters.jl).

# region runs on one segmentation's cell obs (regions.{suffix} columns) → distribution summary
function _collect_region_cols!(out, vn::AbstractString, props::AbstractString)
    df = _read_obs(props); df === nothing && return
    for c in names(df)
        startswith(c, "regions.") || continue
        d = _category_distribution(df[!, c]); d.n == 0 && continue
        push!(out, (; valueName = vn, suffix = c[ncodeunits("regions.")+1:end],
                     nRegions = d.nDistinct, n = d.n,
                     largestFrac = isempty(d.top) ? 0.0 : d.top[1].fraction, sizes = d.top))
    end
end

# pairwise interaction sidecars (spatialStats/{suffix}.json) → flat records. Carries the permutation
# test alongside the effect size, so a reader can tell "these avoid each other" from "these avoid each
# other more than a random arrangement would" (`zScore`/`pValue` are null when the test was skipped).
_nullable_float(r, k) = let v = get(r, k, nothing)
    (v === nothing || v isa Nothing) ? nothing : Float64(v)
end
function _collect_contact_stats(img::CciaImage)
    dir = img_stats_dir(img); out = Any[]
    isdir(dir) || return out
    for f in sort(readdir(dir))
        endswith(f, ".json") || continue
        d = try JSON3.read(read(joinpath(dir, f), String)) catch; continue end
        recs = get(d, :records, nothing); recs isa AbstractVector || continue
        pairs = [(; popA = string(r.popA), popB = string(r.popB),
                   observed = Float64(get(r, :observed, 0)), expected = Float64(get(r, :expected, 0)),
                   logOdds = Float64(get(r, :logOdds, 0)),
                   zScore = _nullable_float(r, :zScore), pValue = _nullable_float(r, :pValue),
                   significant = Bool(get(r, :significant, false)),
                   association = string(get(r, :association, "")))
                 for r in recs]
        push!(out, (; suffix = f[1:prevind(f, end, 5)],
                     basis = String[string(x) for x in get(d, :basis, [])],
                     nCells = Int(get(d, :nCells, 0)), nEdges = Int(get(d, :nEdges, 0)),
                     graphSuffix = string(get(d, :graphSuffix, "")),
                     nPermutations = Int(get(d, :nPermutations, 0)),
                     shufflePops = String[string(x) for x in get(d, :shufflePops, [])],
                     coverage = Float64(get(d, :coverage, 1.0)), pairs = pairs))
    end
    out
end

# spatial-pattern sidecars (spatialPattern/{suffix}.json, SPATIAL_PATTERN_PLAN Decision 5) → one entry
# per run with its flat per-population records, passed through as written (JSON3 objects serialise
# unchanged). A sidecar that fails to parse is skipped, like the contact reader above.
function _collect_pattern_stats(img::CciaImage)
    out = Any[]
    for s in img_pattern_suffixes(img)
        d = try JSON3.read(read(img_pattern_path(img, s), String)) catch; continue end
        recs = get(d, :records, nothing); recs isa AbstractVector || continue
        push!(out, (; suffix = s, graphSuffix = string(get(d, :graphSuffix, "")),
                     nPermutations = Int(get(d, :nPermutations, 0)),
                     rMax = Float64(something(get(d, :rMax, 0.0), 0.0)), records = recs))
    end
    out
end

# proximity-profile sidecars (proximity/{suffix}.json, spatialAnalysis.proximityProfile) → one entry per
# run with its flat per-(population, band) records, passed through as written. A sidecar that fails to
# parse is skipped, like the readers above.
function _collect_proximity_stats(img::CciaImage)
    out = Any[]
    for s in img_proximity_suffixes(img)
        d = try JSON3.read(read(img_proximity_path(img, s), String)) catch; continue end
        recs = get(d, :records, nothing); recs isa AbstractVector || continue
        push!(out, (; suffix = s,
                     structurePops = String[string(x) for x in get(d, :structurePops, [])],
                     pops = String[string(x) for x in get(d, :pops, [])],
                     referencePops = String[string(x) for x in get(d, :referencePops, [])],
                     bands = String[string(x) for x in get(d, :bands, [])],
                     bandWidth = Float64(something(get(d, :bandWidth, 0.0), 0.0)),
                     nStructureCells = Int(get(d, :nStructureCells, 0)),
                     nCounted = Int(get(d, :nCounted, 0)), nReference = Int(get(d, :nReference, 0)),
                     records = recs))
    end
    out
end

"""
    contact_matrix(img; suffix="") -> NamedTuple

The pairwise cell-type contact LOG-ODDS as a heatmap-ready matrix for one image + one neighbourStats
run (CODEX Goltsev 2018; Decision 16). Reuses `_collect_contact_stats` (the `spatialStats/{suffix}.json`
reader) — no second reader. Returns `(; suffixes, suffix, basis, cells, nCells, nEdges)` where `cells`
is `[(x=popA, y=popB, value=logOdds)]` symmetric-filled over `basis × basis`, ready for the shared
matrix renderer (PlotChart). `suffix=""` picks the first run; `suffixes` lists all runs so the UI can
switch. Empty everything when the image has no contact stats.
"""
function contact_matrix(img::CciaImage; suffix::AbstractString = "")
    stats = _collect_contact_stats(img)
    isempty(stats) && return (; suffixes = String[], suffix = "", basis = String[],
                                cells = NamedTuple[], nCells = 0, nEdges = 0)
    suffixes = String[s.suffix for s in stats]
    idx = suffix == "" ? 1 : something(findfirst(==(String(suffix)), suffixes), 1)
    e = stats[idx]
    lo = Dict{Tuple{String,String},Float64}()
    for p in e.pairs                                   # symmetric fill (records may be upper-triangle)
        lo[(p.popA, p.popB)] = p.logOdds
        lo[(p.popB, p.popA)] = p.logOdds
    end
    cells = NamedTuple[]
    for a in e.basis, b in e.basis
        v = get(lo, (a, b), nothing)
        v === nothing || push!(cells, (; x = a, y = b, value = v))
    end
    (; suffixes, suffix = e.suffix, basis = e.basis, cells, nCells = e.nCells, nEdges = e.nEdges)
end

# Per region run: how many cells of each NAMED cell-cluster population (the image's `clust` pops on
# the same segmentation) sit in each region — the counts behind the Region composition plot, read
# through the SAME `plot_summary_data` composition matrix (one computation, two readers). Regions read
# by their population names where the user named them. Compact grid: `pops` × `regions` → `counts`
# (row = pop). `nothing` when the segmentation has no cluster populations. A failure on one image is
# logged and leaves that run without a composition, never the whole summary.
function _region_composition(img::CciaImage, vn::AbstractString, suffix::AbstractString)
    pt = "clust"
    isfile(gating_path(img._dir, vn; pop_type = pt)) || return nothing
    pops = pop_paths(load_pop_map(img; value_name = vn, pop_type = pt, backfill_save = false))
    isempty(pops) && return nothing
    m = try
        plot_summary_data(img, pt, [(String(vn), p) for p in pops], "matrix";
                          matrix_mode = "composition", category = "regions." * suffix,
                          matrix_normalize = :none)
    catch e
        @warn "spatial summary: region composition failed" uid = img.uid vn suffix exception = e
        return nothing
    end
    rows = String[string(x) for x in m["yLabels"]]; cols = String[string(x) for x in m["xLabels"]]
    isempty(rows) && return nothing
    at = Dict((string(c["y"]), string(c["x"])) => Int(c["count"]) for c in m["cells"])
    (; popType = pt, pops = rows, regions = cols,
       counts = [Int[get(at, (r, c), 0) for c in cols] for r in rows])
end

function _spatial_image(img::CciaImage)
    regions = Any[]
    for vn in sort(img_value_names(img))
        _collect_region_cols!(regions, vn, img_label_props_path(img, vn))
    end
    regions = Any[merge(r, (; composition = _region_composition(img, r.valueName, r.suffix))) for r in regions]
    (; _observer_image_header(img)..., regionRuns = regions, contactStats = _collect_contact_stats(img),
       patternStats = _collect_pattern_stats(img), proximityStats = _collect_proximity_stats(img))
end

"""
    spatial_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image: region-clustering runs (`regions.{suffix}` — n regions, sizes), pairwise cell-type contact
statistics (observed/expected/log-odds per population pair, association vs avoidance), one-population
spatial-pattern runs (`patternStats`: nearest-neighbour ratio vs random relabelling) and proximity
profiles (`proximityStats`: each population's share in distance bands around a structure vs the image). Flat + MCP-friendly
(docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 9).
"""
spatial_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "") =
    observer_image_summary(proj, _spatial_image; image_uid = image_uid, set_uid = set_uid)
