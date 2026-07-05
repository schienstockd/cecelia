# Audit prompt — Cecelia Pineapple outstanding issues

You are auditing four specific correctness issues in Cecelia Pineapple, a bioimaging tool with a
Julia backend, Vue 3 frontend, and Python/Napari image viewer. Read the relevant files listed
under each issue, verify that the described problem is real, propose a concrete fix for each, and
flag any additional correctness or design issues you notice while reading.

## System context (brief)

- `app/` — Julia package `Cecelia.jl` (tasks, chain executor, scheduler)
- `api/` — Julia API server (`sockets.jl`, `routes.jl`, `server.jl`) — NOT a package; `include`d at startup
- `frontend/` — Vue 3 + Pinia (`stores/tasks.ts`, `tasks/TaskRunner.vue`, `modules/ChainModule.vue`)
- Chain executor (`app/src/tasks/chain.jl`): spawns one OS thread per image; each image walks chain
  nodes in topo order; set-scope nodes use a barrier; cancel uses a flag set in `scheduler.jl`
- Scheduler (`api/src/scheduler.jl`): global `ResourcePool` worker pools keyed by name; `run_task`
  acquires a pool slot before running a Python subprocess; `cancel_task!` kills the subprocess via
  `_kill_tree`; `ChainRun._pools` are separate per-run `Base.Semaphore` instances

---

## Issue A — Pool initialization: GPU tasks run in parallel instead of serializing

### Description

There are two pool layers:

1. `ChainRun._pools` — `Base.Semaphore(N)` per pool name, per chain run. Created from `pool_limits`
   kwarg to `run_chain`. Limits how many image threads can simultaneously enter a given chain node.

2. `_POOLS` in `scheduler.jl` — global `ResourcePool` with OS worker threads. All `run_task` calls
   (from chains AND from module-page TaskRunner) go through these. Created from the cecelia config
   `pools` section. The default pool limit is `tasks_concurrent_limit()`, typically 20+.

**The bug**: if `"gpu"` is absent from the user's config `pools` section, `_pool("gpu")` returns
`_POOLS["default"]` silently. The default pool may have limit 20+, so all GPU tasks run in parallel
on the GPU. The old workaround (a slider that called `task:setLimit` to resize the default pool to 1)
has been removed.

### Files to read

- `api/src/scheduler.jl` — `_pool()`, `_POOLS`, `ResourcePool`, `list_pools()`, `run_task`
- `app/src/tasks/chain.jl` — `_execute_image_chain!`, how `pool_name` is passed to `run_task`
- `api/src/routes.jl` — `api_pools_list` handler
- `app/src/config.jl` — `tasks_concurrent_limit()`, `pools` config reading

### Questions for auditor

1. Is the silent fallback in `_pool()` confirmed? Show the exact code path.
2. Was `task:setLimit` / `handle_task_set_limit` the only mitigation, and is it fully removed?
3. Propose the cleanest fix. Options to evaluate: (a) auto-initialize well-known pools (`"gpu": 1`,
   `"io": 4`) when absent from config; (b) raise an error / warning when a `run_task` call requests
   a pool name that was not initialized; (c) change the frontend to always send `poolName` and rely
   on the user having configured the pool. Are there other options?
4. Does `list_pools()` returning only initialized pools cause any frontend UX problem if `"gpu"` is
   not in config (e.g. the pool dropdown is empty or missing the expected choice)?

---

## Issue B — Chain cancel doesn't kill running subprocesses

### Description

`cancel_chain_run!(run_id)` adds `run_id` to `_CANCELLED_CHAINS`. The `_is_cancelled()` closure
in `_execute_image_chain!` checks this set BETWEEN nodes. If a node is currently executing (a Python
subprocess is running), the check doesn't run until that node finishes. The Python process is never
sent `SIGKILL`.

**Proposed fix approach**: maintain a `_chain_run_tasks::Dict{String, Vector{String}}` in
`scheduler.jl` mapping `run_id → [task_id, ...]`. When `cancel_chain_run!(run_id)` is called, also
call `cancel_task!(tid)` for each registered task ID. The chain executor must register the `tid =
gen_uid()` it creates inside `_execute_image_chain!` before `run_task`, and deregister after.

### Files to read

- `api/src/scheduler.jl` — `cancel_chain_run!`, `_CANCELLED_CHAINS`, `cancel_task!`, `_kill_tree`,
  `run_task` (look for where `task_id` / `tid` is created and registered in `_TASKS`)
- `app/src/tasks/chain.jl` — `_execute_image_chain!`, where `tid` / task IDs are generated,
  the `_is_cancelled()` closure, cancel check sites
- `api/src/sockets.jl` — `chain:cancel` WS handler

### Questions for auditor

1. Confirm the cancel check placement: are there any check sites inside a node execution (not just
   between nodes)?
2. Is the proposed `_chain_run_tasks` map approach correct? Identify any locking issues: the map
   needs a lock if multiple image threads register concurrently. What lock should guard it?
3. Is there a simpler approach — e.g. storing `chain_run_id` on the task record in `_TASKS` and
   iterating `_TASKS` in `cancel_chain_run!`? Evaluate trade-offs.
4. Are there any tasks that correctly handle being killed mid-execution (cleanup, temp files)?
   Does `_kill_tree` leave any resources in an inconsistent state?

---

## Issue C — SIGKILL timing race in cancel_task!

### Description

In `cancel_task!` (`scheduler.jl`): the flow inside a task is roughly:

1. `_set_status!(rec, :running)`  ← rec.status = :running
2. `run(pipeline(...); wait=false)` ← process starts
3. `on_process(proc)` callback fires ← sets `rec.proc = proc`

If `cancel_task!` arrives between steps 1 and 3, it finds `rec.proc = nothing` and returns without
killing. The process starts at step 2 and runs unimpeded. The window is tiny but real.

**Proposed fix**: after `on_process` sets `rec.proc`, immediately check `rec.status === :cancelled`
and call `_kill_tree` / `kill(proc)` if so.

### Files to read

- `api/src/scheduler.jl` — `cancel_task!`, `_set_status!`, task record struct (find `proc` field),
  `run_task` execution path
- `app/src/tasks/cleanupImages/cellpose_correct.jl` — where `on_process(proc)` is called, the
  subprocess launch sequence
- `app/src/tasks/importImages/omezarr.jl` — same check for the bf2raw subprocess path

### Questions for auditor

1. Confirm the race window: what is the exact sequence between `_set_status!(rec, :running)` and
   `on_process` firing? Are there await points (yield opportunities) in between?
2. Is the proposed fix (check `rec.status` inside `on_process`) the right place, or should it
   be inside `cancel_task!` (retry after a brief yield, or use a condition variable)?
3. Is there a lock-free approach? Could `rec.proc` be set atomically before `_set_status!(:running)`?
4. Does this race also affect module-page tasks (not just chain tasks)?

---

## Issue D — /api/pools not in ARCHITECTURE.md WS/REST reference

### Description

The `/api/pools` GET endpoint was added to `api/src/routes.jl` and wired in `api/src/server.jl`,
but was not documented in `docs/ARCHITECTURE.md`. This has now been added to the REST endpoint
reference table in ARCHITECTURE.md (as of 2026-06-22). Verify that the documentation accurately
describes the endpoint's behavior, especially the note about pools only appearing if initialized
from config.

### Files to read

- `api/src/routes.jl` — `api_pools_list` handler implementation
- `api/src/server.jl` — the GET `/api/pools` route registration
- `api/src/scheduler.jl` — `list_pools()` implementation
- `docs/ARCHITECTURE.md` — REST endpoint reference table (newly added section)

### Questions for auditor

1. Does `list_pools()` return all pools or only initialized ones? Does it include the default pool?
2. Is the response format (`[{name, limit}]`) correct per the handler code?
3. Is there any auth or project-scoping on this endpoint, or is it global?
4. Does the frontend pool dropdown handle the case where `/api/pools` returns an empty array or
   doesn't include the pool named in a task's `resource_pool`?

---

## Additional audit scope

While reading the above files, please also flag:

- Any other places where `run.run_id` is referenced instead of `run.id` on a `ChainRun` struct
  (the bug that caused `FieldError` on every chain run — fixed as TODO #00019)
- Any WS message handlers in `sockets.jl` that still reference the removed `task:setLimit` event
  type or `handle_task_set_limit` function
- Any task JSON files that still have `tasksLimit` instead of `resource_pool`
- Any TypeScript code in the frontend that still references `tasksLimit` on `TaskDef`

Report format: for each issue, state (1) whether the described problem is confirmed or not, (2) your
proposed fix with specific file + line guidance, and (3) any additional issues found in passing.

---

## Resolution (2026-06-22)

All four issues + the additional-scope sweep were audited and fixed. See `docs/TODO.md` #00015–#00020.

**Issue A — confirmed, but root cause was different.** Pools *are* config-defined (`config.toml
[pools]`: `gpu=1`, `gpu-light=4`, `io=8`, `default=20`) and the global worker pool *did* serialise
GPU execution (completions staggered 2/4/6 min prove it). The real bug was **queue visibility**:
`_execute_image_chain!` marked a node `:running` before `run_task` acquired a pool slot, so all
three cellpose nodes showed `:running` at `startedAt≈t0` (elapsed 2/4/6) though only one ran at a
time. Fix: mark `:queued` before `run_task`; flip to `:running` from the pool worker via
`on_status_change`; new `chain:node:queued` event + live `:queued` state. The dead per-run
`ChainRun._pools` semaphore layer (built from `pool_limits` but never wired from the API) was
**removed** — pools are now single-layer, config-only. `_pool` now `@warn`s once on fallback to
`default` instead of silently swallowing a missing pool name. `task:setLimit`/`handle_task_set_limit`
fully removed (Q2 confirmed).

**Issue B — confirmed, fixed.** Chose the "store `chain_run_id` on the `TaskRecord` and iterate
`_TASKS`" approach (audit Q3) over a separate map — one lock (`_TASKS_LOCK`), no extra registry to
keep in sync. `run_task(...; chain_run_id=run.id)` threads it; `cancel_chain_run!` collects matching
ids under the lock, then `cancel_task!`s (SIGKILL) outside it. Killed task → node marked `:cancelled`
(not `:failed`) via a post-`run_task` `_is_cancelled()` re-check; frontend maps `cancelled` correctly
and `setStatus` keeps user cancels sticky.

**Issue C — confirmed, fixed.** `_execute_job!`'s `on_process` wrapper re-checks `is_cancelled(job.id)`
right after storing `rec.proc` and kills immediately. Affects both chain and module-page tasks (Q4).

**Issue D — verified.** `/api/pools` is documented in ARCHITECTURE.md; `list_pools()` returns all
initialised pools incl. `default`; response shape `[{name, limit}]` confirmed; endpoint is global
(no project scope).

**Additional scope:** no stray `run.run_id` (only the event-payload field name, mapped from `run.id`);
no `task:setLimit`/`handle_task_set_limit`; no `tasksLimit` in any task JSON or in `TaskDef`.

**Found in passing:** (1) `:queued` now persists to `run.json`, so `_reset_stale_nodes!` was extended
to treat a crashed `:queued` like `:running` (→ retry). (2) Set-scope/incremental nodes call the
multi-image `_run_task` directly and aren't registered in `_TASKS`, so their subprocesses aren't
killed on cancel — logged as TODO #00020 (nil impact today: no real set-scope subprocess task exists).
