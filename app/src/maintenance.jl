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

# task_id → running subprocess, so a maintenance run is cancellable. Mirrors the scheduler's process
# registry but decoupled (a patch is not a scheduler task, so `cancel_task!`/`_TASKS` don't reach it).
const _MAINT_PROCS = Dict{String,Base.Process}()
const _MAINT_LOCK  = ReentrantLock()

"""
    run_maintenance_patch(patch, proj; apply, task_id, on_log, on_progress) -> Bool

Run a data patch over ONE project via `run_py` — the project root is passed as `root`, `apply` toggles
dry-run vs write. Registers the subprocess under `task_id` for `cancel_maintenance!` (cleared on exit).
Returns clean exit. Sink-agnostic: callers wire `on_log`/`on_progress` (the WS handler sends them over
the task rail).
"""
function run_maintenance_patch(patch::MaintenancePatch, proj::CciaProject; apply::Bool,
                               task_id::AbstractString = "",
                               on_log::Function = println,
                               on_progress::Function = (n, t) -> nothing)::Bool
    try
        run_py(patch.script, (; root = proj.root, apply = apply), task_run_dir(proj.root);
               on_log = on_log, on_progress = on_progress,
               on_process = proc -> lock(_MAINT_LOCK) do
                   _MAINT_PROCS[String(task_id)] = proc
               end)
    finally
        lock(_MAINT_LOCK) do; delete!(_MAINT_PROCS, String(task_id)); end
    end
end

function cancel_maintenance!(task_id::AbstractString)
    proc = lock(_MAINT_LOCK) do; get(_MAINT_PROCS, String(task_id), nothing); end
    isnothing(proc) || try
        _kill_proc_tree(proc)
    catch e
        @warn "Error cancelling maintenance run $task_id" exception = e
    end
end
