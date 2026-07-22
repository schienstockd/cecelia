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
        "centroid-axes",
        "Convert centroid axis names",
        "Rewrite this project's label-props files to explicit centroid_x/_y/_z + centroid_t axis " *
        "names (from the legacy positional centroid-N / t). Needed to open projects created before " *
        "the centroid-axis change. Dry-run lists what would change; Apply writes.",
        "tasks/importImages/convert_centroid_names_run.py",
    ),
]

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
