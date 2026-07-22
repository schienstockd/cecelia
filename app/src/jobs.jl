# ── jobs.jl — OS process control + background-job lifecycle ───────────────────────
#
# Two layers, both process-oriented and language-agnostic (nothing here knows about Python):
#
#   1. Kill primitives — end a process and its child tree, or whatever listens on a port. Used
#      everywhere a spawned process must be stopped: the scheduler (`cancel_task!`), napari, the AI
#      agent runner, app shutdown, and the job registry below. (Moved here from scheduler.jl — they
#      were always general process control, not scheduler-specific.)
#
#   2. Job registry — a `task_id → Base.Process` map + `cancel_job!`, for background jobs that stream
#      over the WS task rail but are NOT scheduler tasks (Settings data patches, Project Manager
#      export/import). A job registers its subprocess with `track_job!` (via `run_py`'s `on_process`
#      hook — the ONLY Python touchpoint, and it lives in the caller, not here) and is killed by
#      `cancel_job!`. Decoupled from the scheduler's `_TASKS`, which those jobs aren't in.
#
# ── Job vs scheduler task — which do I write? ─────────────────────────────────────
# Both surface in the UI over the same WS task rail (task:log / task:progress / task:status keyed by a
# taskId), so they LOOK alike to the frontend — but the backend machinery and the fit are different:
#
#   Scheduler task (CciaTask + `run_task`/`_run_task`, tasks/scheduler.jl) — IMAGE/SET-scoped analysis
#     work. Gets: resource-pool concurrency limits, a queue + worker threads, per-image logfiles,
#     chainability + resume, mandatory QC. Registered in task_registry.jl with a fun_name + JSON spec.
#     Write one when the work operates on image(s) and should participate in pools/chains/QC.
#
#   Background job (this file: `track_job!`/`cancel_job!`, spawned with `Threads.@spawn` in a WS
#     handler) — a one-off PROJECT-/bundle-scoped operation with NO image target and no place in a
#     pool or chain. No fun_name, no JSON spec, no QC. Write one for maintenance/data-patch and
#     project export/import — especially import, which has no project at all until it finishes.
#
# Rule of thumb: operates on images + wants pooling/chaining/QC/resume → scheduler task. One-off
# project/bundle op, or no image target → job. See maintenance.jl, project_io.jl, docs/SCHEDULER.md,
# docs/todo/PROJECT_IO_PLAN.md.

# ── 1. Kill primitives ────────────────────────────────────────────────────────

function _kill_tree(pid::Int)
    if Sys.iswindows()
        try; run(ignorestatus(`taskkill /F /T /PID $pid`)); catch; end
    else
        try
            for line in split(readchomp(`pgrep -P $pid`), '\n'; keepempty=false)
                child = tryparse(Int, strip(line))
                isnothing(child) || _kill_tree(child)
            end
        catch; end
        try; run(ignorestatus(`kill -9 $pid`)); catch; end
    end
end

# Kill a live process AND its child tree, given the Julia Process handle. `Base.Process` has no
# `.pid` field, so get the OS pid via libuv, kill the tree (subprocesses — napari/Qt, bioformats —
# spawn children), then SIGKILL the handle itself. Best-effort; callers wrap in try/catch where a
# dead/reused handle is possible. One place for the `uv_process_get_pid` + `_kill_tree` + `kill` dance.
function _kill_proc_tree(proc::Base.Process)
    pid = Int(ccall(:uv_process_get_pid, Cint, (Ptr{Cvoid},), proc.handle))
    _kill_tree(pid)
    kill(proc, Base.SIGKILL)
end

# Kill whatever process is LISTENING on a TCP port (+ its tree). Cross-platform, best-effort. Used to
# guarantee a clean app shutdown even for a child we only ADOPTED or that outlived a crash (no process
# handle to `kill`) — napari :7655, notebooks :7660 — mirroring `pixi run stop`. There is no libuv API
# for port→pid, so we shell out per-OS; this is the one sanctioned place, alongside `_kill_tree`.
# Extract listener PIDs from `ss -tlnpH` output (the `users:(("name",pid=NNN,fd=..))` field), deduped
# (a listener shows once for IPv4 and once for IPv6). Pure/unit-tested — the rest of
# _kill_listeners_on_port shells out and kills, which isn't.
_listener_pids_from_ss(raw::AbstractString) =
    unique(parse(Int, m.captures[1]) for m in eachmatch(r"pid=(\d+)", raw))

function _kill_listeners_on_port(port::Integer)
    pids = Int[]
    try
        # All three keep to the LISTENING process (docstring promise): plain `lsof -ti tcp:$port` (or
        # ss without -l) also returns clients CONNECTED to the port, so we'd kill them too (e.g. the
        # browser on :5173). Each shell-out exits non-zero / throws when nothing listens → caught.
        if Sys.iswindows()
            out = readchomp(`powershell -NoProfile -Command "(Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue).OwningProcess"`)
            for line in split(out, '\n'; keepempty=false)
                p = tryparse(Int, strip(line))
                isnothing(p) || push!(pids, p)
            end
        elseif Sys.islinux()
            # `ss` reads /proc/net/tcp* directly; unlike lsof it does NOT walk each process's
            # /proc/<pid>/fd table, so a process wedged in D (uninterruptible) sleep can't hang it
            # (lsof does — it stalled `pixi run stop`; see pixi.toml's linux-64 stop tasks).
            append!(pids, _listener_pids_from_ss(readchomp(`ss -tlnpH $("sport = :$port")`)))
        else   # macOS: no ss; lsof is fine here (the D-state /proc walk hang is Linux-specific).
            out = readchomp(`lsof -ti tcp:$port -sTCP:LISTEN`)
            for line in split(out, '\n'; keepempty=false)
                p = tryparse(Int, strip(line))
                isnothing(p) || push!(pids, p)
            end
        end
    catch; end
    for p in unique(pids)
        p == getpid() && continue   # never kill ourselves
        _kill_tree(p)
    end
    nothing
end

# ── 2. Background-job registry ──────────────────────────────────────────────────

# One background job's cancel state: a flag + the set of live subprocesses it spawned. A job may run
# ONE subprocess (a data patch's `run_py`) or MANY at once (project export tars N stores in parallel),
# so `procs` is a vector. `cancelled` lets an in-Julia loop bail between units even before/without a
# subprocess to kill. Decoupled from the scheduler's `_TASKS` — a job is not a scheduler task.
mutable struct _Job
    cancelled::Bool
    procs::Vector{Base.Process}
end
const _JOBS      = Dict{String,_Job}()
const _JOBS_LOCK = ReentrantLock()

"""
    start_job!(task_id)

Register a background job (idempotent), so `job_cancelled` is meaningful before any subprocess is
spawned. Pair with `finish_job!` in a `finally`."""
start_job!(task_id::AbstractString) =
    (lock(_JOBS_LOCK) do; get!(_JOBS, String(task_id), _Job(false, Base.Process[])); end; nothing)

"""
    track_job!(task_id, proc) -> proc

Register `proc` as one of job `task_id`'s live subprocesses, so `cancel_job!` kills it. Wire into
`run_py`'s `on_process` hook: `on_process = p -> track_job!(task_id, p)`. Race guard: if a cancel
already landed, kill `proc` immediately (it was spawned after `cancel_job!` snapshotted the list)."""
function track_job!(task_id::AbstractString, proc::Base.Process)
    was_cancelled = lock(_JOBS_LOCK) do
        j = get!(_JOBS, String(task_id), _Job(false, Base.Process[]))
        push!(j.procs, proc)
        j.cancelled
    end
    was_cancelled && try; _kill_proc_tree(proc); catch; end
    proc
end

"""    job_cancelled(task_id) -> Bool — has this job been cancelled? (checked in-loop by parallel packers)."""
job_cancelled(task_id::AbstractString)::Bool =
    lock(_JOBS_LOCK) do; j = get(_JOBS, String(task_id), nothing); isnothing(j) ? false : j.cancelled; end

"""    finish_job!(task_id) — drop the job's registry entry once it has fully exited."""
finish_job!(task_id::AbstractString) =
    lock(_JOBS_LOCK) do; delete!(_JOBS, String(task_id)); end

"""
    cancel_job!(task_id)

Cancel the background job `task_id`: set its cancel flag AND kill every live subprocess it spawned (a
no-op if unknown). Shared by the `task:cancel` and `maintenance:cancel` WS handlers, so it reaches data
patches (one proc) AND a parallel project export/import (many) alike."""
function cancel_job!(task_id::AbstractString)
    procs = lock(_JOBS_LOCK) do
        j = get(_JOBS, String(task_id), nothing)
        isnothing(j) && return Base.Process[]
        j.cancelled = true
        copy(j.procs)
    end
    for p in procs
        try; _kill_proc_tree(p); catch e; @warn "Error cancelling job $task_id" exception = e; end
    end
end
