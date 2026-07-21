# ── spatialAnalysis.contactsMeshes — cell-to-cell contact (MESH route) ──────────────
#
# The MESH counterpart to spatialAnalysis.cellContacts (points): surface-to-surface distance from
# meshes built on the fly (marching cubes on the label submasks), rather than centroid distance —
# mostly for live images where cell surfaces matter (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11).
# Per timepoint; candidate pairs pre-filtered by centroid distance (mesh_utils), so O(edges) not O(N²);
# no fcl (trimesh.proximity). A and B are each ONE segmentation (label ids collide across
# segmentations). Legacy `cellContactsMeshes`, re-homed: Julia resolves membership + label-zarr paths +
# physical sizes; Python (`cell_contacts_mesh_run.py`) does the mask I/O + meshing + surface distance.

using DataFrames: nrow

struct ContactsMeshes <: CciaTask end

_label_zarr_path(img::CciaImage, vn::AbstractString) =
    joinpath(img._dir, "labels", first(img.labels[vn]))

function _run_task(::ContactsMeshes, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    popsA      = _str_list(params, "popsA")
    popsB      = _str_list(params, "popsB")
    max_dist   = Float64(get(params, "maxContactDist", 10.0))
    (isempty(popsA) || isempty(popsB)) &&
        (on_log("[ERROR] contactsMeshes: select both an A and a B population"); return nothing)
    # A and B segmentations derived from their own populations (value_name-prefixed picks); each is ONE
    # segmentation (label ids collide across segmentations) — no separate dropdowns (legacy parity).
    vnA        = pops_value_name(popsA)
    vnB        = pops_value_name(popsB)
    (haskey(img.labels, vnA) && haskey(img.labels, vnB)) ||
        (on_log("[ERROR] contactsMeshes: missing a label zarr for $(vnA) / $(vnB)"); return nothing)
    # A / B may each mix pop types — resolve across types + namespace output by cell kind (see cellContacts)
    pop_type   = pop_namespace(img, popsA; value_name = vnA)
    pop_type_b = pop_namespace(img, popsB; value_name = vnB)
    on_progress(1, 3)

    # A/B are each ONE segmentation (label ids collide across segmentations) → restrict each to its vn
    aMem = pop_df_multi(img, popsA; value_name = vnA, granularity = :cell, restrict_to = vnA)
    bMem = pop_df_multi(img, popsB; value_name = vnB, granularity = :cell, restrict_to = vnB)
    (nrow(aMem) == 0 || nrow(bMem) == 0) &&
        (on_log("[ERROR] contactsMeshes: no A ($(nrow(aMem))) or B ($(nrow(bMem))) cells"); return nothing)

    (sizes, _) = img_physical_sizes(img)
    qc_out_path = joinpath(task_run_dir(img._dir), "contacts_mesh_qc.json")
    target = _contact_target(pop_type_b, popsB)
    on_log("[INFO] contactsMeshes: $(nrow(aMem)) A × $(nrow(bMem)) B, target=$(target), ≤$(max_dist)µm")
    on_progress(2, 3)

    task_params = Dict{String,Any}(
        "imPath" => img_filepath(img),
        "aLabelPath" => _label_zarr_path(img, vnA), "bLabelPath" => _label_zarr_path(img, vnB),
        "aLabels" => Int.(aMem.label), "bLabels" => Int.(bMem.label),
        "physicalSizes" => sizes, "maxContactDist" => max_dist, "target" => target,
        "popType" => pop_type, "propsPath" => img_label_props_path(img, vnA),
        "qcOutPath" => qc_out_path)

    ok = run_py("tasks/spatialAnalysis/cell_contacts_mesh_run.py", task_params, task_run_dir(img._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] contactsMeshes: Python runner failed"); return nothing)

    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_a = Int(get(qc, "nCellsA", 0)); n_c = Int(get(qc, "nContacts", 0))
        frac = Float64(get(qc, "fracInContact", 0.0))
        findings = n_a == 0 ? [qc_finding("warn", "contact.no_cells", "No cells", "No A cells.")] : Dict{String,Any}[]
        write_qc(img, "spatialAnalysis.contactsMeshes", vnA, findings;
                 metrics = Dict{String,Any}("nCellsA" => n_a, "nContacts" => n_c, "fracInContact" => frac))
        on_log("[QC] mesh contact: $(n_c)/$(n_a) A cells in contact.")
    catch e
        on_log("[QC] could not compute contactsMeshes QC: $e")
    end
    on_progress(3, 3)

    Dict{String,Any}("valueName" => vnA, "target" => target)
end
