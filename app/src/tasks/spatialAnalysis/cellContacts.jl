# ── spatialAnalysis.cellContacts — cell-to-cell contact (points route, Julia) ───────
#
# The POINTS route: for each cell of population A, its nearest population-B cell by CENTROID distance,
# and whether that is within a contact threshold. Fast, centroid-based — the quick counterpart to the
# mesh (surface-distance) route (spatialAnalysis.contactsMeshes, mostly for live images). Cross-poptype:
# A and B may be any poptypes / segmentations (e.g. flow CD8 vs a region, live T cells vs a clust
# aggregate). Legacy used the R `dbscan::kNN`; this is a from-scratch port onto NearestNeighbors.jl.
#
# Writes, for A's cells: `<popTypeA>.cell.contact#<target>` (0/1), `.min_distance#<target>` (µm),
# `.contact_id#<target>` (the nearest B cell's label) — the legacy naming schema. `<target>` identifies
# the B population set. All Julia (membership + centroids + physical sizes), no Python round-trip.

using DataFrames: nrow, DataFrame, groupby
using NearestNeighbors: KDTree, knn

struct CellContacts <: CciaTask end

# physical-scaled centroids (rows = points, cols = spatial dims) + labels for member labels of a seg
function _scaled_centroids(img::CciaImage, vn::AbstractString, labels::AbstractVector{<:Integer})
    props = img_label_props_path(img, vn)
    scols = centroid_columns(label_props(props))
    cdf = label_props(props) |> view_centroid_cols |> filter_rows(collect(Int, labels)) |> as_df
    nrow(cdf) == 0 && return (zeros(Float64, 0, length(scols)), Int[])
    (sizes, _) = img_physical_sizes(img)
    scale = sizes[end - length(scols) + 1:end]
    coords = hcat((Float64.(cdf[!, c]) .* scale[j] for (j, c) in enumerate(scols))...)
    (coords, Int.(cdf.label))
end

_contact_target(pop_type_b, popsB) =
    replace(string(pop_type_b, ".", join(sort(collect(String, popsB)), "+")), "/" => "_")

function _run_task(::CellContacts, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", "default"))
    popsA      = _str_list(params, "popsA")
    popsB      = _str_list(params, "popsB")
    max_dist   = Float64(get(params, "maxContactDist", 10.0))
    (isempty(popsA) || isempty(popsB)) &&
        (on_log("[ERROR] cellContacts: select both an A population and a B population"); return nothing)
    # A / B may each be a MIX of pop types (flow gates, clusters, regions, tracked cells) — resolve
    # membership across types (pop_df_multi), then namespace the output columns by the A/B cell kind
    # (tracked → live.cell.*, else flow.cell.*). Was hardcoded to pop_type="flow", so tracked cells
    # never resolved and always wrote flow.cell.contact.
    pop_type   = pop_namespace(img, popsA; value_name = value_name)
    pop_type_b = pop_namespace(img, popsB)
    on_progress(1, 3)

    # A cells (this segmentation) + their centroids. restrict_to = value_name: A is annotated in
    # THIS segmentation, so drop any A pop picked from another one (its labels index this seg's props).
    aMem = pop_df_multi(img, popsA; value_name = value_name, granularity = :cell, restrict_to = value_name)
    nrow(aMem) == 0 && (on_log("[ERROR] cellContacts: no A cells for $(popsA)"); return nothing)
    aCoords, aLabels = _scaled_centroids(img, value_name, Int.(aMem.label))
    isempty(aLabels) && (on_log("[ERROR] cellContacts: no A centroids"); return nothing)

    # B cells (may span segmentations) → one pooled point cloud + their labels
    bMem = pop_df_multi([img], [img.uid], popsB; granularity = :cell)
    nrow(bMem) == 0 && (on_log("[ERROR] cellContacts: no B cells for $(popsB)"); return nothing)
    bCoordsList = Matrix{Float64}[]; bLabels = Int[]
    for g in groupby(bMem, :value_name)
        c, l = _scaled_centroids(img, string(first(g.value_name)), Int.(g.label))
        isempty(l) && continue
        push!(bCoordsList, c); append!(bLabels, l)
    end
    isempty(bLabels) && (on_log("[ERROR] cellContacts: no B centroids"); return nothing)
    bCoords = vcat(bCoordsList...)
    on_progress(2, 3)

    # nearest B for each A (KDTree over B, query A) — distances in µm
    tree = KDTree(permutedims(bCoords))
    idxs, dists = knn(tree, permutedims(aCoords), 1)
    min_dist  = [d[1] for d in dists]
    contactid = [Float64(bLabels[i[1]]) for i in idxs]
    contact   = Float64.(min_dist .<= max_dist)

    target = _contact_target(pop_type_b, popsB)
    out = DataFrame("label" => aLabels,
                    "$(pop_type).cell.contact#$(target)"      => contact,
                    "$(pop_type).cell.min_distance#$(target)" => min_dist,
                    "$(pop_type).cell.contact_id#$(target)"   => contactid)
    label_props(img_label_props_path(img, value_name)) |> add_obs(out) |> save!

    n_contact = Int(sum(contact)); frac = n_contact / length(contact)
    write_qc(img, "spatialAnalysis.cellContacts", value_name,
             (length(aLabels) == 0 ? [qc_finding("warn", "contact.no_cells", "No cells", "No A cells.")] : Dict{String,Any}[]);
             metrics = Dict{String,Any}("nCellsA" => length(aLabels), "nContacts" => n_contact,
                                        "fracInContact" => frac))
    on_log("[INFO] cellContacts: $(n_contact)/$(length(aLabels)) A cells in contact with $(target) (≤$(max_dist)µm).")
    on_progress(3, 3)

    Dict{String,Any}("valueName" => value_name, "target" => target,
                     "nContacts" => n_contact, "fracInContact" => frac)
end
