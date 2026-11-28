# Data patches — one-off, project-scoped maintenance scripts, run from the Settings page.
#
# Unlike a CciaTask (image-scoped, scheduler-run), a data patch operates on a whole PROJECT — e.g.
# rewriting every labelProps h5ad to a new on-disk convention. It runs its Python via `run_py` and
# streams over the same task WS rail (ws_log/ws_progress/ws_status keyed by a taskId), so the user sees
# live output + progress + a working Cancel, like an HPC task spin-off. Confined to ONE project (the
# open one) by design — the handler passes that project's root. See docs/DEV.md → "Data patches".

struct MaintenancePatch
    id::String            # stable slug — the frontend + any saved state key off it
    title::String
    description::String
    script::String        # python module under python/cecelia/, invoked via run_py
end

# The shipped patches. Add a new one-off data migration here as an entry.
const MAINTENANCE_PATCHES = MaintenancePatch[
    MaintenancePatch(
        "store-debris",
        "Remove leftover stores",
        # What it sweeps and WHY detection is structural rather than name-based belongs in the code
        # that does it (`store_sweep.py` header, docs/SEGMENTATION.md), not in a Settings string the
        # user reads every time. Dry-run/Apply is not restated: both are buttons, and the section
        # already says "Dry-run first to see what would change".
        "Delete store directories left by cancelled or crashed runs. Run with no tasks in flight.",
        "utils/store_sweep.py",
    ),
    MaintenancePatch(
        "ims-softlink",
        "Repair Imaris sources",
        # The user's entry point to this is the import error, so the description quotes it. WHY
        # Bio-Formats can't read the file (netcdf-java doesn't follow HDF5 soft links) is in the
        # script header, not in a Settings string. "Pixel data is untouched" earns the write: this is
        # the only patch that edits a file OUTSIDE the project.
        "Fix .ims sources that fail to import with \"Unknown pixel type: null\". Pixel data is untouched.",
        "utils/ims_relink.py",
    ),
]

"""
    store_debris_summary(proj) -> Dict

Counts + bytes the `store-debris` patch would free, without deleting anything. Runs the SAME detector
the patch runs (`store_sweep.summarise` via `run_py`), rather than re-counting `*.partial` names in
Julia — that second implementation would under-report exactly the cases structural detection exists
for (a cancelled import writes a half-finished store at its FINAL name).

Spawning Python makes this cost a subprocess, which is why it belongs to the on-demand storage Scan and
not to anything that runs on page open. Returns an empty summary on any failure: the storage box is
advisory, and a broken sweep must not fail the scan the user actually asked for.
"""
function store_debris_summary(proj::CciaProject)::Dict{String,Any}
    empty = Dict{String,Any}("count" => 0, "bytes" => 0, "activeSkipped" => 0, "byWhy" => Dict())
    run_dir = task_run_dir(proj.root)
    result  = joinpath(run_dir, "store_debris.$(string(rand(UInt32); base = 16)).result.json")
    try
        ok = run_py("utils/store_sweep.py", (; root = proj.root, resultPath = result), run_dir)
        (ok && isfile(result)) || return empty
        Dict{String,Any}(String(k) => v for (k, v) in
                         pairs(JSON3.read(read(result, String), Dict{String,Any})))
    catch e
        @warn "Could not summarise store debris" exception = e
        empty
    finally
        rm(result; force = true)
    end
end

maintenance_patches()::Vector{MaintenancePatch} = MAINTENANCE_PATCHES
function maintenance_patch(id::AbstractString)::Union{MaintenancePatch,Nothing}
    i = findfirst(p -> p.id == id, MAINTENANCE_PATCHES)
    isnothing(i) ? nothing : MAINTENANCE_PATCHES[i]
end

"""
    run_maintenance_patch(patch, proj; apply, task_id, on_log, on_progress) -> Bool

Run a data patch over ONE project — the project root is passed as `root`, `apply` toggles dry-run vs
write. Just a `run_py` with its subprocess registered in the shared job registry (jobs.jl) so
`cancel_job!` can kill it — the same track/cancel that project export/import use. Returns clean exit.
"""
function run_maintenance_patch(patch::MaintenancePatch, proj::CciaProject; apply::Bool,
                               task_id::AbstractString = "",
                               on_log::Function = println,
                               on_progress::Function = (n, t) -> nothing)::Bool
    start_job!(task_id)
    try
        run_py(patch.script, (; root = proj.root, apply = apply), task_run_dir(proj.root);
               on_log = on_log, on_progress = on_progress,
               on_process = p -> track_job!(task_id, p))
    finally
        finish_job!(task_id)
    end
end

# The `maintenance:cancel` WS message still targets patches by name; route it to the shared canceller.
cancel_maintenance!(task_id::AbstractString) = cancel_job!(task_id)

# ── Project-wide summary (the storage box) ────────────────────────────────────

"""
    project_storage_summary(proj_uid) -> Dict

Disk total/available (via `diskstat`, a cheap statvfs) plus a walked breakdown of the project's image
stores and the reclaimable (non-active) versions. The `reclaimable` list carries one entry per image
that has at least one freeable version — the total bytes freed, the version kept active, and the list
of versions that would be removed. Shape mirrors what the frontend renders + posts back to reclaim.
"""
function project_storage_summary(proj_uid::String)::Dict{String,Any}
    proj = load_project(proj_uid)
    reclaimable    = Dict{String,Any}[]
    total_reclaim  = 0
    image_bytes    = 0   # image OME-ZARR versions only (the `0/` dir)
    analysis_bytes = 0   # everything DERIVED (the `1/` dir minus ANALYSIS_KEEP)

    for s in proj._sets, img in s._images
        st = image_storage(img)
        image_bytes    += sum(v.bytes for v in st.versions; init = 0)
        analysis_bytes += analysis_bytes_of(img)
        if !isempty(st.reclaimable) && st.reclaimableBytes > 0
            push!(reclaimable, Dict{String,Any}(
                "imageUid"      => img.uid,
                "name"          => img.name,
                "setUid"        => s.uid,
                "bytes"         => st.reclaimableBytes,
                "activeVersion" => st.active,
                "versions"      => [Dict{String,Any}("valueName" => v.valueName, "bytes" => v.bytes)
                                    for v in st.versions if v.valueName in st.reclaimable],
            ))
            total_reclaim += st.reclaimableBytes
        end
    end
    sort!(reclaimable; by = e -> e["bytes"], rev = true)   # biggest first

    ds = Base.Filesystem.diskstat(projects_dir())
    Dict{String,Any}(
        "diskTotal"        => ds.total,
        "diskAvailable"    => ds.available,
        "imageBytes"       => image_bytes,   # image stores only (the `0/` dir)
        # Everything DERIVED, so the one screen that reports disk usage isn't silent about the half of
        # the project it can't otherwise see. Reported, never auto-freed: dropping analysis is a
        # deliberate per-image act in the Import page's Delete modal, not something a project-wide
        # "free up space" button should do (docs/todo/IMAGE_DELETE_PLAN.md Decision 5).
        "analysisBytes"    => analysis_bytes,
        "reclaimableBytes" => total_reclaim,
        "reclaimable"      => reclaimable,
        # Leftovers a cancelled/crashed run abandoned: staging dirs, import scratch, unregistered and
        # truncated stores. Distinct from `reclaimable` above — that is real data the user could choose
        # to drop, this is bytes nothing can reach. Reported here so the cleanup ANNOUNCES itself
        # instead of waiting to be discovered in Data patches; freed by the `store-debris` patch, which
        # uses this same detector.
        "debris"           => store_debris_summary(proj),
    )
end
