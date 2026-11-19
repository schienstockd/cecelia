# ── spatialAnalysis.proximityProfile — which populations sit near a structure, in distance bands ──
#
# The STRUCTURE is a population whose cells' positions define it: a region (germinal centre, follicle),
# aggregated cells from `detectAggregates`, or any gate. Each counted cell gets its distance to the
# nearest structure cell — the centroid search the contacts points route uses (`_target_cloud` +
# `_nearest_contacts`, cellContacts.jl), so it works in 2D and 3D and per frame on a timecourse. Cells of
# the structure itself are never counted around it. The band arithmetic and the enrichment against the
# image's own composition live in `proximity_bands.jl` (pure, unit-tested).
#
# Output: `proximity/{suffix}.json` (`img_proximity_path`), one record per (population, band). Read by the
# Spatial page's proximity plots (`image_metrics.jl`, `plot_data.jl` → `_proximity_matrix`) and by MCP
# `get_proximity_profiles` (`ai/spatial.jl`). All Julia, no Python round-trip.

using DataFrames: nrow, DataFrame, groupby

struct ProximityProfile <: CciaTask end

Base.@kwdef struct ProximityProfileParams
    structurePops::Vector{String} = String[]
    pops::Vector{String}          = String[]
    referencePops::Vector{String} = String[]   # empty = every cell of the counted pops' segmentations
    proximitySuffix::String       = "default"
    bandWidth::Float64            = 15.0
    nBands::Int                   = 3
end

function parse_proximity_profile_params(d::AbstractDict)::ProximityProfileParams
    ProximityProfileParams(;
        structurePops   = _str_list(d, "structurePops"),
        pops            = _str_list(d, "pops"),
        referencePops   = _str_list(d, "referencePops"),
        proximitySuffix = string(get(d, "proximitySuffix", "default")),
        bandWidth       = Float64(get(d, "bandWidth", 15.0)),
        nBands          = Int(get(d, "nBands", 3)))
end

# one counted population → `(value_name, label) => band index`, structure members left out. Each
# segmentation's cells are searched against the whole structure cloud (it may span segmentations).
function _proximity_members(img::CciaImage, pop::AbstractString, cloud, p::ProximityProfileParams)
    sCoords, sLabels, sT, sKeys = cloud
    df = pop_df_multi([img], [img.uid], [String(pop)]; granularity = :cell, centroids = :physical)
    out = Dict{Tuple{String,Int},Int}()
    nrow(df) == 0 && return out
    for g in groupby(df, :value_name)
        vn = string(first(g.value_name))
        coords, labels, frames = _centroid_matrix(img, vn, DataFrame(g))
        isempty(labels) && continue
        dist, _ = _nearest_contacts(coords, frames, sCoords, sLabels, sT)
        for (l, d) in zip(labels, dist)
            key = (vn, l)
            key in sKeys && continue
            out[key] = proximity_band_index(d, p.bandWidth, p.nBands)
        end
    end
    out
end

function _run_task(::ProximityProfile, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_proximity_profile_params(params)
    (isempty(p.structurePops) || isempty(p.pops)) &&
        (on_log("[ERROR] proximityProfile: select a structure and the populations to count"); return nothing)
    (p.bandWidth > 0 && p.nBands >= 1) ||
        (on_log("[ERROR] proximityProfile: band width must be > 0 and bands ≥ 1"); return nothing)
    on_progress(1, 3)

    cloud = _target_cloud(img, p.structurePops)
    n_structure = cloud === nothing ? 0 : length(cloud[2])
    bands = proximity_bands(p.bandWidth, p.nBands)
    members = Dict{String,Dict{Tuple{String,Int},Int}}()
    reference = Dict{Tuple{String,Int},Int}()
    ref_pops = p.referencePops
    if cloud !== nothing
        for pop in p.pops
            members[pop] = _proximity_members(img, pop, cloud, p)
        end
        # default reference: every cell of the segmentations the counted populations live in
        isempty(ref_pops) &&
            (ref_pops = sort!(unique(["$(k[1])/$(ROOT)" for m in values(members) for k in keys(m)])))
        for pop in ref_pops
            merge!(reference, _proximity_members(img, pop, cloud, p))
        end
        # a counted cell is always part of the reference, so no share can exceed 1
        for m in values(members); merge!(reference, m); end
    end
    records = proximity_records(p.pops, members, reference, bands)
    on_progress(2, 3)

    band_totals = Dict(String(r["band"]) => Int(r["nBand"]) for r in records)
    n_counted = length(union(Set{Tuple{String,Int}}(), (keys(m) for m in values(members))...))
    n_reference = sum(values(band_totals); init = 0)
    out_path = img_proximity_path(img, p.proximitySuffix)
    write_json_atomic(out_path, Dict{String,Any}(
        "structurePops" => p.structurePops, "pops" => p.pops, "referencePops" => ref_pops,
        "bandWidth" => p.bandWidth, "nBands" => p.nBands, "bands" => [b.label for b in bands],
        "nStructureCells" => n_structure, "nCounted" => n_counted, "nReference" => n_reference,
        "records" => records))

    write_qc(img, "spatialAnalysis.proximityProfile", p.proximitySuffix,
             proximity_findings(n_structure, records);
             metrics = Dict{String,Any}("nStructureCells" => n_structure, "nCounted" => n_counted,
                                        "nReference" => n_reference,
                                        "minBandCells" => isempty(band_totals) ? 0 : minimum(values(band_totals))))
    n_structure == 0 && on_log("[WARN] proximityProfile: no structure cells for $(p.structurePops)")
    on_log("[QC] $(n_structure) structure cells; $(n_counted) counted of $(n_reference) reference cells in $(length(bands)) bands.")
    on_progress(3, 3)

    on_log("[INFO] proximityProfile done → $(basename(out_path))")
    Dict{String,Any}("proximityPath" => out_path, "nStructureCells" => n_structure, "nCounted" => n_counted)
end
