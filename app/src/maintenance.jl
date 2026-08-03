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
