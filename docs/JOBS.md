# Background jobs (vs scheduler tasks)

Cecelia runs long operations two different ways. They **look the same in the UI** — both stream over
the WebSocket *task rail* (`task:log` / `task:progress` / `task:status`, keyed by a client `taskId`,
with a Cancel button) — but the backend machinery and the kind of work they suit are different. This
doc is the map: what each is, why both exist, and where new code goes.

## The two mechanisms

### Scheduler task — `CciaTask` (`app/src/tasks/scheduler.jl`, `docs/SCHEDULER.md`)
**Image/set-scoped analysis work.** Segmentation, tracking, clustering, measurement, corrections — the
pipeline. A task is a `CciaTask` struct + `_run_task` + a `.json` param spec, registered by `fun_name`
in `task_registry.jl`. The scheduler gives it: **resource-pool concurrency limits**, a queue + worker
threads, **per-image threading**, **chainability + resume**, per-image logfiles, and **mandatory QC**.
Launched from the frontend as `task:run` (needs an `imageUid`/`imageUids`).

Write one when the work **operates on image(s)** and should participate in pools / chains / QC / resume.

### Background job — `jobs.jl` (`track_job!` / `cancel_job!`)
**One-off, project- or bundle-scoped operations with no image target and no place in a pool or chain.**
Two exist today:
- **Settings → data patches** (`app/src/maintenance.jl`) — one-off migrations over a whole project
  (e.g. rewrite every labelProps h5ad to a new convention). One `run_py` subprocess.
- **Project Manager → Export / Import** (`app/src/project_io.jl`) — bundle a whole project / restore
  one. Many `tar` subprocesses at once (stores packed in parallel).

A job has no `fun_name`, no JSON spec, no QC. It is spawned with `Threads.@spawn` directly in a WS
handler (`api/src/sockets.jl`), reports through the same injected `on_log`/`on_progress` callbacks the
handler wires to `ws_log`/`ws_progress`, and is cancelled via `cancel_job!`.

Write one for a **one-off project/bundle operation, or anything with no image target** — especially
project **import**, which has no project at all until it finishes (so it can't be image- or even
project-scoped, and must not require an *open* project).

### Decision rule
> Operates on image(s) and wants pooling / chaining / QC / resume → **scheduler task**.
> One-off project-/bundle-scoped op, or no image target → **background job**.

## The `jobs.jl` API

A job is identified by the client's `taskId`. The registry tracks a **cancel flag** plus the job's
**live subprocesses** (a vector — a job may run one, like a data patch, or many at once, like a
parallel export).

| Function | Purpose |
|---|---|
| `start_job!(task_id)` | register the job (idempotent), so `job_cancelled` is meaningful before any subprocess spawns |
| `track_job!(task_id, proc)` | register a live subprocess (wire into `run_py`'s `on_process`, or call around a `run(...)`); kills it immediately if the job was already cancelled (race guard) |
| `job_cancelled(task_id)` | `Bool` — checked *in-loop* by a parallel packer to stop between units |
| `cancel_job!(task_id)` | set the flag **and** kill every tracked subprocess |
| `finish_job!(task_id)` | drop the registry entry once the job has fully exited (call in a `finally`) |

`jobs.jl` also owns the **OS process-kill primitives** (`_kill_tree`, `_kill_proc_tree`,
`_kill_listeners_on_port`) — general process control used by the scheduler, napari, the AI agent
runner, app shutdown, and `cancel_job!`. They live here (not `scheduler.jl`) because they were always
general, not scheduler-specific.

## How a project-wide operation runs, end to end

Export is the canonical example (import and data patches follow the same rail):

```
frontend (ProjectPanel)                 api/src/sockets.jl                  app/src/project_io.jl
──────────────────────                  ──────────────────                  ────────────────────
taskStore.add() -> taskId
ws.send({type:'project:export',   ──►   handle_project_export:
         taskId, projectUid,             Threads.@spawn begin
         outDir})                          ws_status(taskId,"running")
                                           export_project(uid;
                                             task_id, out_dir,
                                             on_log      = l -> ws_log(taskId,l),      ──►  start_job!(taskId)
                                             on_progress = (n,t)->ws_progress(...))         mirror tree (copy files)
                                                                                            @sync Threads.@spawn per store:
   task:log / task:progress  ◄───────────────────────────────────────────────────────       tar store  (track_job!)
   update the task rail live                                                                write ccbundle.json
                                           ws_status(taskId, ok ? "done":"failed")           finish_job!(taskId)
ws.send({type:'task:cancel',taskId}) ─►  task:cancel  ->  cancel_job!(taskId)  ───────────►  flag + kill all tars
```

Key points:
- **No open project needed.** Export reads a project dir off disk by uid; import creates a new one.
  So both live in the **Project Manager** (`ProjectPanel.vue`), not Settings (data patches, by
  contrast, act on the *open* project).
- **Parallelism is Julia threads.** `export_project`/`import_project` pack/unpack stores with a
  bounded `Threads.@spawn` pool — the per-store work is I/O-bound (millions of tiny chunk files), so
  concurrent `tar`s overlap the per-file stat/open latency. Big win on SSD/NVMe and network storage;
  can be neutral on a single spinning disk. See `docs/todo/PROJECT_IO_PLAN.md` for the bundle format.
- **Cancel reaches everything.** The `task:cancel` and `maintenance:cancel` WS handlers both call
  `cancel_job!`, which kills every tracked subprocess (the one data-patch `run_py`, or all in-flight
  export `tar`s) and sets the flag so the pack loop stops spawning more.

## Adding a new background job

1. Write the Julia entry point (project_io.jl-style): take `task_id`, `on_log`, `on_progress`; wrap
   the body in `start_job!` / `finish_job!`; register each subprocess with `track_job!`; check
   `job_cancelled` in any loop. Return a plain result (a path, a uid, `""` on failure) — jobs are
   sink-agnostic like scheduler tasks (no `ws_*` calls inside).
2. Add a WS message type + `handle_*` in `api/src/sockets.jl` that `Threads.@spawn`s it and wires
   `on_log`/`on_progress` → `ws_log`/`ws_progress`, `ws_status` for running/done/failed.
3. Route `task:cancel` (or a dedicated cancel message) to `cancel_job!`.
4. Trigger it from the frontend by `taskStore.add()` + `ws.send({type, taskId, …})`.
