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

# pairwise contact log-odds sidecars (spatialStats/{suffix}.json) → flat records
function _collect_contact_stats(img::CciaImage)
    dir = joinpath(img._dir, "spatialStats"); out = Any[]
    isdir(dir) || return out
    for f in sort(readdir(dir))
        endswith(f, ".json") || continue
        d = try JSON3.read(read(joinpath(dir, f), String)) catch; continue end
        recs = get(d, :records, nothing); recs isa AbstractVector || continue
        pairs = [(; popA = string(r.popA), popB = string(r.popB),
                   observed = Float64(get(r, :observed, 0)), expected = Float64(get(r, :expected, 0)),
                   logOdds = Float64(get(r, :logOdds, 0)), association = string(get(r, :association, "")))
                 for r in recs]
        push!(out, (; suffix = f[1:prevind(f, end, 5)],
                     basis = String[string(x) for x in get(d, :basis, [])],
                     nCells = Int(get(d, :nCells, 0)), nEdges = Int(get(d, :nEdges, 0)), pairs = pairs))
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

function _spatial_image(img::CciaImage)
    regions = Any[]
    for vn in sort(img_value_names(img))
        _collect_region_cols!(regions, vn, img_label_props_path(img, vn))
    end
    (; _observer_image_header(img)..., regionRuns = regions, contactStats = _collect_contact_stats(img))
end

"""
    spatial_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image: region-clustering runs (`regions.{suffix}` — n regions, sizes) and pairwise cell-type contact
statistics (observed/expected/log-odds per population pair, association vs avoidance). Flat + MCP-friendly
(docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 9).
"""
spatial_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "") =
    observer_image_summary(proj, _spatial_image; image_uid = image_uid, set_uid = set_uid)
