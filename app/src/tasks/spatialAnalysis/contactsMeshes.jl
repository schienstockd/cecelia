# ── spatialAnalysis.contactsMeshes — cell-to-cell contact (MESH route) ──────────────
#
# The MESH counterpart to spatialAnalysis.cellContacts (points): surface-to-surface distance from
# meshes built on the fly (marching cubes on the label submasks), rather than centroid distance —
# mostly for live images where cell surfaces matter (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11).
# Per timepoint; candidate pairs pre-filtered by centroid distance (mesh_utils), so O(edges) not O(N²);
# no fcl (trimesh.proximity). A and B are each ONE segmentation (label ids collide across
# segmentations). Legacy `cellContactsMeshes`, re-homed: Julia resolves membership + label-zarr paths +
# physical sizes; Python (`cell_contacts_mesh_run.py`) does the mask I/O + meshing + surface distance.
# One column set per target population, as the points route (`_contact_target_groups`, #1734);
# `combineTargets` pools every B pop into one set. B is meshed once and split per target in Python.

using DataFrames: nrow

struct ContactsMeshes <: CciaTask end

Base.@kwdef struct ContactsMeshesParams
    popsA::Vector{String}    = String[]
    popsB::Vector{String}    = String[]
    maxContactDist::Float64  = 5.0
    combineTargets::Bool     = false
end

function parse_contacts_meshes_params(d::AbstractDict)::ContactsMeshesParams
    ContactsMeshesParams(;
        popsA          = _str_list(d, "popsA"),
        popsB          = _str_list(d, "popsB"),
        maxContactDist = Float64(get(d, "maxContactDist", 5.0)),
        combineTargets = Bool(get(d, "combineTargets", false)))
end

_label_zarr_path(img::CciaImage, vn::AbstractString) =
    joinpath(img._dir, "labels", first(unversion_value(img.labels[vn])))

function _run_task(::ContactsMeshes, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_contacts_meshes_params(params)
    (isempty(p.popsA) || isempty(p.popsB)) &&
        (on_log("[ERROR] contactsMeshes: select both an A and a B population"); return nothing)
    # A and B segmentations derived from their own populations (value_name-prefixed picks); each is ONE
    # segmentation (label ids collide across segmentations) — no separate dropdowns (legacy parity).
    vnA        = pops_value_name(p.popsA)
    vnB        = pops_value_name(p.popsB)
    (haskey(img.labels, vnA) && haskey(img.labels, vnB)) ||
        (on_log("[ERROR] contactsMeshes: missing a label zarr for $(vnA) / $(vnB)"); return nothing)
    # A / B may each mix pop types — resolve across types + namespace output by cell kind (see cellContacts)
    pop_type   = pop_namespace(img, p.popsA; value_name = vnA)
    pop_type_b = pop_namespace(img, p.popsB; value_name = vnB)
    on_progress(1, 3)

    # A/B are each ONE segmentation (label ids collide across segmentations) → restrict each to its vn
    aMem = pop_df_multi(img, p.popsA; value_name = vnA, granularity = :cell, restrict_to = vnA)
    nrow(aMem) == 0 && (on_log("[ERROR] contactsMeshes: no A cells for $(p.popsA)"); return nothing)
    # one column set per target group (see cellContacts `_contact_target_groups`)
    groups = _contact_target_groups(p.popsB; combine = p.combineTargets)
    targets = Dict{String,Any}[]
    findings = Dict{String,Any}[]
    for refs in groups
        bMem = pop_df_multi(img, refs; value_name = vnB, granularity = :cell, restrict_to = vnB)
        target = _contact_target(pop_namespace(img, refs; value_name = vnB), refs)
        if nrow(bMem) == 0
            on_log("[WARN] contactsMeshes: no B cells for $(refs) — skipped")
            push!(findings, qc_finding("warn", "contact.no_target_cells";
                                       detail = Dict{String,Any}("target" => refs)))
            continue
        end
        push!(targets, Dict{String,Any}("target" => target, "bLabels" => Int.(bMem.label)))
    end
    isempty(targets) && (on_log("[ERROR] contactsMeshes: no B cells for $(p.popsB)"); return nothing)
    _log_stale_combined(on_log, img_label_props_path(img, vnA), pop_type, pop_type_b, p.popsB, groups)

    (sizes, _) = img_physical_sizes(img)
    qc_out_path = joinpath(task_run_dir(img._dir), "contacts_mesh_qc.json")
    target_names = String[t["target"] for t in targets]
    on_log("[INFO] contactsMeshes: $(nrow(aMem)) A vs $(join(target_names, ", ")), ≤$(p.maxContactDist)µm")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "imPath" => img_filepath(img),
        "aLabelPath" => _label_zarr_path(img, vnA), "bLabelPath" => _label_zarr_path(img, vnB),
        "aLabels" => Int.(aMem.label), "targets" => targets,
        "physicalSizes" => sizes, "maxContactDist" => p.maxContactDist,
        "popType" => pop_type, "propsPath" => img_label_props_path(img, vnA),
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_contacts_mesh_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] contactsMeshes: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_a = Int(get(qc, "nCellsA", 0)); n_c = Int(get(qc, "nContacts", 0))
        frac = Float64(get(qc, "fracInContact", 0.0))
        n_a == 0 && push!(findings, qc_finding("warn", "contact.no_cells"))
        # targets whose cells never meshed (all below min_voxels) got no columns — same finding as
        # a target that resolved no cells here
        empty = String[string(t) for t in get(qc, "emptyTargets", String[])]
        for t in empty
            push!(findings, qc_finding("warn", "contact.no_target_cells"; detail = Dict{String,Any}("target" => t)))
        end
        write_qc(img, "spatialAnalysis.contactsMeshes", vnA, findings;
                 metrics = Dict{String,Any}("nCellsA" => n_a, "nContacts" => n_c, "fracInContact" => frac,
                                            "nTargets" => length(targets) - length(empty)))
        on_log("[QC] mesh contact: $(n_c)/$(n_a) A cells in contact.")
    catch e
        on_log("[QC] could not compute contactsMeshes QC: $e")
    end
    on_progress(3, 3)

    Dict{String,Any}("valueName" => vnA, "targets" => target_names)   # requested; QC names any unmeshed
end
