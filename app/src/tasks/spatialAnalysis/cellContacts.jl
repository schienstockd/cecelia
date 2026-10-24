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
#
# One column set PER TARGET: several picked B populations give one distance each, not one distance to
# the nearest of any (#1734). Grouping + naming live in `contact_targets.jl`, shared with the mesh route.
#
# Per timepoint on a timecourse: an A cell's nearest B is searched only among B cells in the SAME frame
# (`centroid_t`), as the mesh route does — x/y/z alone would pair a cell with where another cell was
# (or will be) in a different frame. An A cell whose frame has no B cell gets NaN distance, id 0.

using DataFrames: nrow, DataFrame, groupby
using NearestNeighbors: KDTree, knn

struct CellContacts <: CciaTask end

Base.@kwdef struct CellContactsParams
    popsA::Vector{String}      = String[]
    popsB::Vector{String}      = String[]
    maxContactDist::Float64    = 10.0
    combineTargets::Bool       = false
end

function parse_cell_contacts_params(d::AbstractDict)::CellContactsParams
    CellContactsParams(;
        popsA          = _str_list(d, "popsA"),
        popsB          = _str_list(d, "popsB"),
        maxContactDist = Float64(get(d, "maxContactDist", 10.0)),
        combineTargets = Bool(get(d, "combineTargets", false)))
end

# A µm point cloud (rows = points, cols = spatial dims) + labels, from a frame `pop_df` already returned
# with `centroids = :physical`. Only the matrix assembly lives here — the read, the axis selection and
# the pixel→µm conversion are all the accessor's (`scale_centroids!`), not this task's.
# Also returns each row's frame (`centroid_t`), or `nothing` for a still image.
function _centroid_matrix(img::CciaImage, vn::AbstractString, cdf::DataFrame)
    lp    = label_props(img_label_props_path(img, vn))
    scols = centroid_columns(lp; order=[:x, :y, :z])
    tcol  = first(vcat(temporal_columns(lp), [""]))
    frames = (isempty(tcol) || !(tcol in names(cdf))) ? nothing : Float64.(cdf[!, tcol])
    nrow(cdf) == 0 && return (zeros(Float64, 0, length(scols)), Int[], frames)
    (hcat((Float64.(cdf[!, c]) for c in scols)...), Int.(cdf.label), frames)
end

# Nearest B for each A — `(min_dist, contact_id)`, one per A row. With frames for both sides the
# search is per frame (A at t only sees B at t); an A row with no B in its frame gets (NaN, 0).
# Without frames on either side (still image) it is one pooled search, as before.
function _nearest_contacts(aCoords::AbstractMatrix, aT, bCoords::AbstractMatrix, bLabels::AbstractVector{Int}, bT)
    nA = size(aCoords, 1)
    min_dist  = fill(NaN, nA)
    contactid = zeros(Float64, nA)
    query!(aRows, bRows) = begin
        (isempty(aRows) || isempty(bRows)) && return
        tree = KDTree(permutedims(bCoords[bRows, :]))
        idxs, dists = knn(tree, permutedims(aCoords[aRows, :]), 1)
        for (k, r) in enumerate(aRows)
            min_dist[r]  = dists[k][1]
            contactid[r] = Float64(bLabels[bRows[idxs[k][1]]])
        end
    end
    if aT === nothing || bT === nothing
        query!(collect(1:nA), collect(1:size(bCoords, 1)))
    else
        bByT = Dict{Float64,Vector{Int}}()
        for (j, t) in enumerate(bT); push!(get!(bByT, t, Int[]), j); end
        aByT = Dict{Float64,Vector{Int}}()
        for (i, t) in enumerate(aT); push!(get!(aByT, t, Int[]), i); end
        for (t, aRows) in aByT
            query!(aRows, get(bByT, t, Int[]))
        end
    end
    (min_dist, contactid)
end

# One target group's µm point cloud: `(coords, labels, frames)` over every segmentation the group spans,
# or `nothing` when it resolves no cells. Each segmentation's rows are scaled with ITS OWN image
# resolution by `pop_df` before they pool here; frames only if EVERY segmentation carries them.
function _target_cloud(img::CciaImage, refs::Vector{String})
    bMem = pop_df_multi([img], [img.uid], refs; granularity = :cell, centroids = :physical)
    nrow(bMem) == 0 && return nothing
    coordsList = Matrix{Float64}[]; labels = Int[]; tList = Union{Nothing,Vector{Float64}}[]
    for g in groupby(bMem, :value_name)
        c, l, ft = _centroid_matrix(img, string(first(g.value_name)), DataFrame(g))
        isempty(l) && continue
        push!(coordsList, c); append!(labels, l); push!(tList, ft)
    end
    isempty(labels) && return nothing
    (vcat(coordsList...), labels, any(isnothing, tList) ? nothing : vcat(tList...))
end

function _run_task(::CellContacts, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_cell_contacts_params(params)
    (isempty(p.popsA) || isempty(p.popsB)) &&
        (on_log("[ERROR] cellContacts: select both an A population and a B population"); return nothing)
    # segmentation = the A populations' own value_name (each picker value is value_name-prefixed); no
    # separate dropdown (legacy parity — cellContacts never had one). A is annotated in this seg.
    value_name = pops_value_name(p.popsA)
    # A / B may each be a MIX of pop types (flow gates, clusters, regions, tracked cells) — resolve
    # membership across types (pop_df_multi), then namespace the output columns by the A/B cell kind
    # (tracked → live.cell.*, else flow.cell.*). Was hardcoded to pop_type="flow", so tracked cells
    # never resolved and always wrote flow.cell.contact.
    pop_type   = pop_namespace(img, p.popsA; value_name = value_name)
    pop_type_b = pop_namespace(img, p.popsB)      # the COMBINED set's; each target group takes its own
    on_progress(1, 3)

    # A cells (this segmentation) + their µm centroids, in ONE read (`centroids = :physical`).
    # restrict_to = value_name: A is annotated in THIS segmentation, so drop any A pop picked from
    # another one (its labels index this seg's props).
    aMem = pop_df_multi(img, p.popsA; value_name = value_name, granularity = :cell,
                        restrict_to = value_name, centroids = :physical)
    nrow(aMem) == 0 && (on_log("[ERROR] cellContacts: no A cells for $(p.popsA)"); return nothing)
    aCoords, aLabels, aT = _centroid_matrix(img, value_name, aMem)
    isempty(aLabels) && (on_log("[ERROR] cellContacts: no A centroids"); return nothing)

    # One column set per target group (`_contact_target_groups`): a group may span segmentations and
    # pools within itself; `combineTargets` makes every B pop one group (the pooled form).
    groups = _contact_target_groups(p.popsB; combine = p.combineTargets)
    props_path = img_label_props_path(img, value_name)
    _log_stale_combined(on_log, props_path, pop_type, pop_type_b, p.popsB, groups)
    on_progress(2, 3)

    out = DataFrame("label" => aLabels)
    findings = Dict{String,Any}[]
    targets = String[]
    in_any = falses(length(aLabels))            # within the threshold of ANY target = the pooled answer
    for refs in groups
        target = _contact_target(pop_namespace(img, refs), refs)
        cloud = _target_cloud(img, refs)
        if cloud === nothing
            on_log("[WARN] cellContacts: no B cells for $(refs) — skipped")
            push!(findings, qc_finding("warn", "contact.no_target_cells";
                                       detail = Dict{String,Any}("target" => refs)))
            continue
        end
        bCoords, bLabels, bT = cloud
        # nearest B for each A (KDTree over B, query A), per frame on a timecourse — distances in µm
        (aT === nothing) == (bT === nothing) || on_log("[WARN] cellContacts: only one side has " *
            "timepoints for $(target) — searching across all frames")
        min_dist, contactid = _nearest_contacts(aCoords, aT, bCoords, bLabels, bT)
        contact = Float64.(min_dist .<= p.maxContactDist)
        out[!, "$(pop_type).cell.contact#$(target)"]      = contact
        out[!, "$(pop_type).cell.min_distance#$(target)"] = min_dist
        out[!, "$(pop_type).cell.contact_id#$(target)"]   = contactid
        in_any .|= contact .> 0
        push!(targets, target)
        on_log("[INFO] cellContacts: $(Int(sum(contact)))/$(length(aLabels)) A cells in contact with " *
               "$(target) (≤$(p.maxContactDist)µm).")
    end
    isempty(targets) && (on_log("[ERROR] cellContacts: no B cells for $(p.popsB)"); return nothing)
    label_props(props_path) |> add_obs(out) |> save!

    # cohort metrics stay comparable with earlier runs: "in contact" = with ANY target, which is exactly
    # what the single pooled column used to report; `nTargets` says how many column sets were written
    n_contact = count(in_any); frac = n_contact / length(aLabels)
    write_qc(img, "spatialAnalysis.cellContacts", value_name, findings;
             metrics = Dict{String,Any}("nCellsA" => length(aLabels), "nContacts" => n_contact,
                                        "fracInContact" => frac, "nTargets" => length(targets)))
    on_progress(3, 3)

    Dict{String,Any}("valueName" => value_name, "targets" => targets,
                     "nContacts" => n_contact, "fracInContact" => frac)
end
