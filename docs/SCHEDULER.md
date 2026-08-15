# Chain Scheduler

Design reference for `app/src/tasks/scheduler.jl` (pools, task records, `run_task`),
`app/src/tasks/chain.jl` (the chain executor) and `app/src/events.jl` (the event bus).

The scheduler exists to solve two problems the old R version had: (1) images were processed in
lockstep batches — image 1 sat idle after finishing import instead of starting denoise immediately,
and (2) there was no per-stage concurrency awareness — GPU-bound denoising and CPU-parallel
segmentation were treated identically. This file documents every non-obvious decision made to
fix those problems.

> **Where this runs.** Everything below describes the scheduler wherever it lives — and in **dev** it can
> live in a second process. With the runner enabled (Settings → System, dev only) the API server hands
> tasks *and chains* to the detached **task runner** (`app/src/runner/`, port 7657), which owns the pools
> and the task registry there instead, so restarting the backend no longer kills work in flight. Same
> `run_task`/`run_chain`, same pools, same frames; the API server relays them. Off by default and
> unavailable in a production install, in which case everything here runs in the API server exactly as
> documented. Background jobs and the task preview are never on it.
> How it works and why dev-only: [`docs/RUNNER.md`](RUNNER.md).

> The scheduler is for **image/set-scoped analysis tasks** (`CciaTask`). One-off **project-/bundle-scoped**
> operations with no image target — Settings data patches, Project Manager export/import — are NOT
> scheduler tasks; they're **background jobs** (`app/src/jobs.jl`), which also now owns the process-kill
> primitives (`_kill_tree`/`_kill_proc_tree`/`_kill_listeners_on_port`) this doc's `cancel_task!` uses.
> When to use which, and how project-wide ops run end-to-end: [`docs/JOBS.md`](JOBS.md).

---

## Core rule

> Each image progresses through the chain independently. A stage's concurrency limit is a
> property of the stage (resource pool), not a property of the batch.

Image A starting segmentation has nothing to do with whether image B has finished denoising yet.
The only gates are: (a) did this image's previous node finish, and (b) is there a free slot in
this node's resource pool.

---

## Execution model

```
run_chain(proj, image_uids; chain="my-chain")
```

Concurrency is **not** a `run_chain` argument — it is a property of the named resource pools
defined globally in `config.toml` `[pools]` (see Resource pools below).

Internally spawns three kinds of concurrent tasks (`Threads.@spawn`):

```
image_tasks  — one task per image, walks all "image"-scope nodes in topo order
set_tasks    — one task per "set"-scope node, waits for all images then runs once
incr_tasks   — one task per "incremental"-scope node, event-driven debounced watcher
```

All three kinds run concurrently. `run_chain` `fetch`es all of them before returning —
it blocks the caller until the entire chain finishes.

### Why `Threads.@spawn` (not `@async`)?

A **Task** is a coroutine the Julia runtime schedules; a **thread** is an OS thread it runs *on*. Both
macros create Tasks — the difference is placement. `@async` pins its Task to the *current* thread
(concurrency, never parallelism); `Threads.@spawn` lets the scheduler place it on any thread in the
pool. Tasks are multiplexed onto the `-t` count (`prod` and the backend `dev.jl` spawns both pass
`-t auto`), so 50 images means 50 Tasks over ~N threads — **not** 50 threads.

A Task only occupies a thread while it is *running*; every yield point hands the thread back. Waiting
on a subprocess is a yield point — `run_py` reads `eachline(out_pipe)` then `wait(proc)`, both
libuv-backed — so a node blocked on cellpose costs no thread at all, and its parallelism lives in the
OS processes rather than in Julia. What *does* hold a thread hostage is work that never yields:
pure-Julia compute loops and `ccall`s into C (notably HDF5, hence the `_with_h5` serialisation). Under
`@async` those run on the one shared thread and stall every other image *and* the WS accept loop —
that, not subprocess waiting, is the reason for `Threads.@spawn`.

Hence two independent ceilings: a pool's slot `limit` (the one you tune) and the thread count, which
only bites for Julia-side non-yielding work.

---

## Resource pools

There is **one** concurrency mechanism: the global pools in `scheduler.jl`. (An earlier design added a
second, per-run `Base.Semaphore` layer on `ChainRun`; it was removed — it double-gated, and was never
wired from the API, which silently disabled it. Pools are config-only.)

Pools are **global and persistent**, shared across every chain run and every module-page task —
all execution goes through `run_task` → `_pool(name)` → the pool's queue. Defined in `config.toml`:

```toml
[pools]
cpu     = 20   # general CPU compute — most tasks
gpu     = 1    # the GPU — cellpose family; raise for batch segmentation
io      = 8    # local disk IO — bioformats2raw import/convert, crop
network = 1    # remote/SMB reads — reserved for HPC/remote tasks (no tasks assigned yet)
```

One pool per real bottleneck resource; the name says *what* it rations, not *how much*. Every limit is
only a starting default — each is a live throttle (below), so you can drop `io` to 1 while importing
over a slow SMB share without editing config or restarting. `network` is defined but unused today; it
exists so remote/HPC task runners have a lane to land in later.

`_pools_init!` reads `[pools]` at first use; `cpu` always exists (falls back to
`tasks_concurrent_limit()`). A task names its pool in the `resource_pool` field of its JSON spec, a
chain node in `ChainNode.resource_pool`. **A missing pool warns once** and falls back to `cpu` — a GPU
task silently landing in the wide cpu pool was the original "all GPU tasks run at once" bug.

### Slot-acquire model — a resizable slot budget, not a worker count

Each pool (`ResourcePool` in `_POOLS`) owns **one** persistent `Channel` of `TaskJob`s, **one**
dispatcher task draining it, a mutable `limit` (= max concurrent jobs), a live `in_flight` count, and a
`Threads.Condition` guarding both counters. The dispatcher pulls a job, calls `_acquire_slot!` (blocks
while `in_flight >= limit`), then runs it on its own spawned task, which releases the slot in a
`finally`. A pool with `limit = 1` runs its jobs strictly one at a time — that is how `gpu` work is
serialised.

Because a slot is claimed **at execution time** and checked against the *current* `limit`, a pool never
runs more than `limit` jobs at once — including the instant after a throttle-down. So `resize_pool!`
only sets `limit` and `notify`s, keeping the same queue and dispatcher (no queued job is ever orphaned):

- **grow** → the `notify` wakes a dispatcher waiting for a slot; the backlog fans out immediately.
- **shrink** → running jobs are never interrupted; the dispatcher stays blocked in `_acquire_slot!`
  until enough drain, so occupancy settles down to the new limit without ever oversubscribing.

This replaced a design that swapped the queue on resize, which orphaned already-queued jobs onto the
old workers at the old concurrency. `resize_pool!` also *creates* a pool if absent — the REPL/test path
(tests register `slow_pool`/`par_pool` this way).

### Live pool limits (Task Manager throttle)

Each pool's limit is a live throttle, not a fixed config value — the day-to-day control (the old R
"concurrent tasks" slider, but one per resource). `set_pool_limit!(name, limit)` = `resize_pool!` to
apply immediately **plus** a merged write to the user's `custom.toml` `[pools]` (like
`set_projects_dir!`) so it survives a restart. Clamped to `[1, POOL_LIMIT_MAX]`. Exposed as
`POST /api/pools/set` `{name, limit}`, which rejects unknown pool names so typo pools can't accumulate
in `custom.toml`. The UI is `PoolThrottle.vue` — a compact 2×2 slider grid (`cpu`/`gpu`, then
`io`/`network`) in a `TeleportPopover` off the Task Manager toolbar (the sliders icon), not Settings.

### Reporting occupancy

`list_pools()` → `[(; name, limit)]` for every initialised pool (this feeds the pool dropdowns).
`pool_status()` → the same plus **live occupancy**: `running` = `in_flight` (slots executing now),
`queued` = tasks in `_TASKS` at `:queued` for that pool. The two snapshots are taken under their **own**
locks (`_POOLS_LOCK`, `_TASKS_LOCK`) and merged outside both — never nest them. `GET /api/pools` serves
`pool_status()`; the `PoolThrottle` popover polls it (~1.5 s) for the "running / limit" readout + bar
under each slider. There is no `pool:*` WS event — occupancy is poll-only.

### Thread budgets — a pool slot is not a core budget

A pool limit caps how many tasks run at once. It says nothing about how many **threads** each one
takes, and the default answer is "all of them": any numpy/scipy call reaching BLAS grabs every core,
so `cpu` at its default limit of 20 means twenty tasks each asking for 32 threads on a 32-core box.

`run_py` therefore sets **`OPENBLAS_NUM_THREADS`** (`BLAS_THREADS_PER_TASK`, currently 4) on every
Python task it launches. That is the only layer that can: the variable is read when the child imports
numpy, so nothing inside Python can change it afterwards — and a `threadpoolctl` context manager only
bounds the pools already *loaded* when it is entered, which is a genuine hole (clustering loads a
second BLAS after the first is capped).

**Bounded is the default because unbounded is not neutral.** Measured on a 32-core box, nothing gets
faster with all cores and one thing gets much slower:

| workload | 32 threads | 4 threads |
|---|---|---|
| drift estimation (`kSUFux/mkh3Tu`) | 56.3 s (309.7 s ×4 concurrent) | **31.8 s (70.7 s ×4)** |
| scanpy neighbors + leiden + umap | 44.6 s | 44.0 s — flat, not BLAS-bound |
| spatial KDTree neighbour graph | 0.14 s | 0.14 s — not BLAS at all |
| dense SVD 20000×400 | 2.30 s | **1.28 s** |
| dense GEMM 4000² | 1.04 s | 0.91 s — memory-bound at this size |

Four concurrent drift tasks *uncapped* (309.7 s) are slower than running them one after another
(4 × 56.3 = 225 s) — past a point the threads fight for cache rather than work.

**`OPENBLAS_NUM_THREADS` only — deliberately not `OMP_NUM_THREADS`.** That one also throttles torch's
intra-op parallelism, and torch on CPU is the one workload measured that genuinely wants the cores: a
cellpose-shaped conv stack goes 0.19 s → 0.34 s at 4 threads. Capping OpenBLAS alone leaves torch
untouched while drift keeps the full win. An MKL-backed numpy is unaffected by this variable and
simply keeps the old behaviour — no regression, no benefit.

A task that has **measured** a need for more raises it locally with
`cecelia.utils.cpu_utils.limit_blas_threads`. Do not raise it on a hunch; the table above is what
"obviously wants all cores" actually looks like when measured.

### Queue visibility — :queued vs :running

A node that is waiting for a pool slot and a node actively executing are **different states**, and
the live view must distinguish them (a saturated GPU pool must not look like a hang).

`_execute_image_chain!` marks the node `:queued` *before* calling `run_task`, then flips it to
`:running` only when the job actually starts — driven by `run_task`'s `on_status_change` callback
(`_execute_job!` calls `_set_status!(rec, :running)` once it holds a slot).
So `startedAt` (and the live elapsed timer) counts from the real slot acquisition, not from when
the image thread reached the node. With `gpu = 1` and three images, the live grid shows one
`:running` and two `:queued`, and each task's elapsed reflects its own ~2 min, not 2/4/6 min.

> **Pitfall (tests):** `node:running` fires from the job's own task while `node:done`
> fires from the image thread, so a size-1 pool has a benign running/done handoff overlap (both
> contend on `run._lock`). Assert serialisation by **wall-clock** (sum of durations), not by
> counting concurrent `node:running`/`node:done` events. A size-N>1 pool is race-free because all
> N `running` events precede any `done`.

---

## Chain cancel

### Data structures

`_CANCELLED_CHAINS::Set{String}` — set of run IDs that have been cancelled.
`_CANCELLED_CHAINS_LOCK::ReentrantLock` — guards all reads and writes to the set.

Both live in `app/src/tasks/scheduler.jl` — in the **package**, not the API layer, so cancel works
from the REPL and tests with no server running.

### Public API (exported from `Cecelia.jl`)

```julia
cancel_chain_run!(run_id::String)   # flag the run AND kill its running tasks
is_chain_cancelled(run_id::String)  # return true if run_id is in the set
```

Cancel works on **two fronts**, because flag-checking alone leaves an in-flight subprocess running:

1. **Flag** — `cancel_chain_run!` adds `run_id` to `_CANCELLED_CHAINS`. The `_is_cancelled()`
   closure is checked BETWEEN nodes, so not-yet-started nodes are marked `:cancelled` and skipped.
2. **Kill** — `cancel_chain_run!` then looks up every `TaskRecord` whose `chain_run_id == run_id`
   and calls `cancel_task!(tid)` on it, which SIGKILLs the running subprocess (`_kill_tree`).
   The chain threads this id through: `run_task(...; chain_run_id = run.id)` stores it on the
   `TaskRecord` (collect ids under `_TASKS_LOCK`, then cancel outside it — `cancel_task!` re-locks).

When the killed task returns `nothing`, the chain marks the node `:cancelled` (not `:failed`) by
re-checking `_is_cancelled()` after `run_task` returns — so the GUI shows cancelled, not a green
"done" or a red "failed".

### How the check reaches the executor

`run_chain` (in `chain.jl`) receives an `on_cancel_check` kwarg (a `Function`). The WS handler
in `sockets.jl` passes `on_cancel_check = is_chain_cancelled`. Inside `run_chain`, the closure:

```julia
_is_cancelled() = on_cancel_check(run.id)   # run.id — NOT run.run_id (that field doesn't exist)
```

is injected into `_execute_image_chain!`, `_run_set_scope_node!`, and `_run_incremental_node!`.

In `_run_set_scope_node!` the cancel check also runs before the barrier wait; if cancelled, the
set-scope runner marks all pending images as `:cancelled` and signals the done channel to unblock
waiting image threads (avoiding deadlock).

**Race guard (start window):** if cancel arrives after the job sets `:running` but before the
task's `on_process` callback records the subprocess handle, `cancel_task!` would find `rec.proc ==
nothing` and skip the kill. `_execute_job!`'s `on_process` wrapper closes this: right after storing
`rec.proc`, it re-checks `is_cancelled(job.id)` and kills immediately if so.

### Limitation — set-scope / incremental nodes *inside a chain*

The per-image path is fully covered. It's specifically the **chain's** set-scope and incremental
runners (`_run_set_scope_node!`, `_run_incremental_node!`) that call the multi-image `_run_task`
**directly** — with `on_process = _ -> nothing` and no `_TASKS` entry — so `cancel_chain_run!` can't
reach a subprocess they spawned mid-run (the between-node flag still stops not-yet-started ones). A
set-scope task launched from a **module page** is unaffected: `handle_task_run` goes through the
registered `run_task(task, imgs, …)` overload, so it has a `TaskRecord` and cancels normally.

Impact is currently nil — no real set-scope subprocess task exists (only mock/plot tasks). When the
first one lands (e.g. HMM training), give the chain's multi-image path a `TaskRecord` + `chain_run_id`
like the per-image path: `docs/TODO.md` → *Set-scope / incremental node subprocesses not killed on
chain cancel*.

---

## pool_name override in run_task

Both `run_task` overloads take `pool_name::String = ""`; when non-empty it overrides the task spec's
`resource_pool`, directing that one call to a specific pool. Two callers use it:

- **WS handler** — `handle_task_run` (`sockets.jl`) forwards `poolName` from the `task:run` message,
  which the frontend populates from the pool dropdown in `TaskRunner.vue`.
- **Chain nodes** — `_execute_image_chain!` passes `pool_name = node.resource_pool`, so a node routes
  to its configured pool even when the task spec's default differs.

---

## Node scopes

| `scope` | Who runs it | When |
|---------|------------|------|
| `"image"` (default) | Image thread | Once per image, after all predecessor nodes for that image are done |
| `"set"` | Dedicated set-scope thread | Once for the whole set, after ALL images arrive at the barrier |
| `"incremental"` | Dedicated watcher thread | After each upstream node:done event, debounced |

**Scope is inherited from the task spec, not restated per node.** The task JSON's `"scope"` field
is the single source of truth (`task_scope`, `task.jl`). A `ChainNode` / `chain_node` built with no
explicit `scope` resolves it from the spec via `_task_default_scope(fn)` (`chain.jl`) — so
`chain_node("behaviour.hmm")` and dragging HMM/clustering onto the whiteboard both produce a
picnic node without the author naming the scope. An explicit non-empty `scope` still overrides
(force a set task per-image if ever needed); a frozen template's stored scope is honoured verbatim.
The whiteboard drop handler mirrors this — `def.scope ?? 'image'` picks the node's visual type.

### Set-scope (picnic) nodes

A picnic node is a synchronisation point: every image must arrive before anything runs.
The HMM training example: one model trained on all images' tracking output simultaneously.

**Barrier protocol** (two `Channel{Nothing}` per set-scope node):

```
_barriers[node_id]      capacity=N   (arrive channel)
_barriers_done[node_id] capacity=N   (done broadcast channel)
```

Image thread arrival:
1. `_barrier_arrive!(run, node_id)` — `put!(arrive_ch, nothing)`, then blocks on done channel
2. `_barrier_wait_done!(run, node_id)` — `take!(done_ch)`, then continues downstream

Set-scope runner:
1. `_barrier_wait_all!(run, node_id)` — `take!(arrive_ch)` N times (one per image)
2. Runs the task once over the participating image set
3. `_barrier_signal_done!(run, node_id)` — `put!(done_ch, nothing)` N times

**Critical**: the skip check (resume) in `_run_set_scope_node!` is placed AFTER
`_barrier_wait_all!`, not before. If placed before, image threads would block on
`_barrier_arrive!` forever because the set-scope runner already returned.

**Barrier policies** (`ChainNode.barrier_policy`):

| Policy | Behaviour |
|--------|-----------|
| `"all"` (default) | Run with all images regardless of upstream failures |
| `"require_all"` | Abort (all images `:failed`) if any image failed upstream |
| `"successful_only"` | Run with only upstream-successful images; failed ones get `:skipped` |

After a set-scope node, downstream per-image nodes resume the `"image"` scope. The picnic is
a single sync point, not a mode switch for the rest of the chain.

### Incremental nodes

An incremental node subscribes to `"node:done"` events from its upstream node. It does not
block image threads — image threads skip it entirely (continue past it in the node loop).
Instead, a dedicated watcher thread:

1. Pre-populates already-done images (resume scenario)
2. Subscribes to `"node:done"` events
3. Runs a `timedwait` drain loop: collects events, fires the plot task after each debounce window
4. Exits when all images are accounted for

**Debounce**: `timedwait(() -> isready(event_ch), debounce_s; pollint=0.005)` — waits up to
`debounce_s` for the next event. If more images arrive within that window, they're all included
in the next plot batch. Prevents firing once per image when ten images clear a stage simultaneously.

**Fault isolation**: incremental nodes are excluded from the fault-isolation check in the image
thread loop. A failed plot never kills the pipeline. The `incremental_ids` set in
`_execute_image_chain!` is the mechanism.

---

## Value-name propagation between linked nodes

A processing task consumes an input image version (a `valueName`) and produces a new one — e.g.
`cleanupImages.cellposeCorrect` reads `default` and writes `cpCorrected`. Because the output only
exists on disk **after** the chain runs, a downstream node's `valueNameSelection` widget can't
offer it from the image (the image still only has `default` at authoring time). Two pieces close
this gap so a chain like `import → cellposeCorrect → afDriftCorrect` can be wired before any image
is processed:

1. **Declared output (introspectable).** Every producer declares its output value_name in the JSON
   spec — a top-level `"outputValueName"` for a fixed output (`cpCorrected`, `driftCorrected`,
   `afCorrected`), or an `outputValueName` **param** when the user names it (`segment.cellpose`).
   The task's `_run_task` reads the fixed form via `_spec_output_value_name(task, default)` instead
   of hardcoding the string, so exactly one place states it. `GET /api/tasks/definitions` serves
   the field to the whiteboard.

2. **Edge-driven prefill (whiteboard).** On connecting A→B, `ChainModule.vue` reads A's declared
   output (`nodeOutputValueName`) and prefills every field-compatible `valueNameSelection` param on
   B with it (`propagateValueName`) — matched by field (`filepath` vs `labels`). The value is
   **auto-populated but editable**: it's offered through `paramContext.extraValueNames` (upstream
   outputs merged into the dropdown even though they don't exist on the image yet), and
   `ParamRenderer`'s auto-select watch keeps an already-valid edge value instead of resetting it to
   the image's active version.

The chain executor already threads real outputs at run time — the composite step-wiring
(`params["valueName"] = result["valueName"]`, `task.jl`) and each node re-reading `ccid.json` — so
propagation is an **authoring-time convenience**, not a second execution path.

## Template vs run record

Two distinct artifacts. This matters for reproducibility.

**Template** (`<project>/settings/chains/<name>.json`) — reusable, no images baked in.
Editing a template after a run has started does not retroactively change what that run did.

> **Path must match the API.** Chains live under `settings/chains/`, computed identically by the
> package (`_chains_dir` in `chain.jl`) and the API (`_chains_dir_for_project` in `api/src/routes.jl`).
> The whiteboard **saves** a template through the API, then `run_chain` **loads** it through the
> package — if the two dirs disagree, every chain run fails with "template not found". They once
> diverged (API moved to `settings/chains/`, the package stayed at `<project>/chains/`); both now
> also migrate a legacy `<project>/chains/` on access. A round-trip test asserts the settings/ path.

**Run record** (`<project>/settings/chains/runs/<run_id>/run.json`) — created when a template is
applied to a set of images. It stores a **frozen** `template_hash` (SHA-256 of the content), not the
template inline and not a pointer to the template *file*: the content is cached once under
`settings/chains/.cache/<hash>.json`, and `load_chain_run` resolves the hash back to it. So editing the
template afterwards can't rewrite history, and `run.json` stays compact.

That freeze is load-bearing beyond reproducibility: it is *why* a template can be edited, renamed or
deleted at all without a snapshot mechanism. Renaming one is a plain file move
(`POST /api/chains/rename`) and past runs keep the name they ran under — a historical fact, not stale
data. The only thing a rename degrades is a run *in flight*, because the Live view fetches the current
template by name for its column layout (it already falls back to a task-derived layout on a miss),
which is why the whiteboard disables the control while the chain has a live run.

### Who may author a template, and who may run one

Templates have three authors: the whiteboard (`POST /api/chains/save` — a verbatim overwrite of the
user's own canvas, including its `positions`), the REPL (`chain_node`/`make_chain`), and Claude over
the MCP (`POST /api/chains/create` — create-only, 409 on an existing name).

Only the whiteboard can *not* produce an invalid template: it offers only real task defs and can't
draw an edge to a node that isn't there. The other two can, and until author-time validation existed a
typo surfaced as a mid-run `_task_from_fun_name` throw or a `KeyError` in `_topo_sort` — after the
user pressed Run. **`validate_chain_template` (`chain.jl`) is the gate**: unknown `fn`, dangling edge
(either endpoint), self-edge, cycle, unknown scope/`barrier_policy`/`resource_pool`, bad
`startTargets`, out-of-spec params. It is what `/api/chains/create` runs before writing. It cannot
check intent or anything per-image (`requires`/axis gating is evaluated against a real image at run
time; selection params name project state) — a valid template is well-formed, not sensible.

**Running is a separate right.** There is exactly one launch path, the WS message `chain:run`
(`api/src/sockets.jl` → `handle_chain_run`), with no HTTP equivalent — which is what keeps chain runs
a user action even though Claude can author the chain. `handle_chain_run` also owns the guards
`run_chain` itself does not have (project exists, `_drop_excluded`), so a second launch entry point
would have to share them rather than re-implement them.

### Live QC row

A task whose spec declares `"qcPlot": "<plotDefId>"` (`segment.cellposeMeasure` /
`segment.measureLabels` → `segmentation_qc`) gets an automatic QC thumbnail in a band above the image
grid, aligned to the producing column. QC is **not** a chain node — it's an overlay tied to the
producing node, toggled from the Live toolbar. A segment run can produce several segmentations, so each
column stacks **one thumbnail per `value_name`** (B, T, …), discovered from the canonical population
picker (`/api/plots/populations?popType=labels`). Each `ChainQcNode` shows the aggregate cell count + a
per-image sparkline (`POST /api/plot_data`, `popType=labels` + `chartType=count`; one debounced request
per image, re-run as images clear the stage — incremental fill), and expands on click to the full
`SummaryCanvas module="segment"`. All of it reuses the canonical plot framework — no bespoke QC route or
panel. Distinct from user-dragged plot nodes (a separate, later mechanism).

### Loading past runs into the Live view

The whiteboard Live tab renders runs from **two sources**, normalised to one task-like shape:
1. **Live** — the in-memory task store (WS `chain:node:*` events), for a run happening/just-happened
   this session.
2. **Persisted** — loaded from disk so a past run survives a reload. `GET /api/chains/runs?projectUid`
   lists run records (id, chain, `createdAt`, image count — read straight from each `run.json`
   header, newest first); `GET /api/chains/run?projectUid&runId` returns a run's frozen template
   (nodes/edges for the layered layout) + per-image per-node **status** (`load_chain_run`). The Live
   view synthesises task entries from `image_states` (funName from the frozen template node, label
   from the task def); persisted runs have **status but no timing** (`ImageNodeState` stores no
   started/finished — the elapsed timer is live-only). The run dropdown merges both, tags live runs
   `· live`, and labels each with its timestamp.

---

## Resume and restart semantics

### params_hash

Every `ImageNodeState` stores a `params_hash`: SHA-256 of the canonical (sorted-key) JSON of the
effective params at the time the node ran. "Effective params" = node params merged with any
per-node overrides passed to `run_chain`.

On resume, `_execute_image_chain!` skips a node if:
```julia
st.status == :done && st.params_hash == _params_hash(effective_params)
```

Both conditions must hold. A node that completed but whose params have since changed will be reset
to `:pending` by `_reset_stale_nodes!` and re-run.

### _reset_stale_nodes!

Pre-pass before resuming — called once on the loaded run, before spawning threads.

Iterates nodes in topological order. For each node and each image:

| Condition | Action |
|-----------|--------|
| `status == :running` | → `:failed` (crash recovery — a crash left it mid-flight) |
| `status ∈ (:failed, :skipped, :cancelled)` | → `:pending` (retry) |
| `status == :done` and `params_hash` changed | → `:pending` (params stale) |
| Any predecessor of this node was reset to `:pending` in this pass | → `:pending` (downstream dirty) |

Staleness propagates downstream via a topo-ordered predecessor map — if n3 re-runs because its
params changed, n4 and n5 are also reset even if their own params are unchanged.

### Explicit start node (`start_node` — "resume from here")

`_reset_stale_nodes!` only re-runs nodes that are *stale* (failed / crashed / params-changed) and
their descendants — a node that is `:done` with unchanged params is always kept and skipped. To
**force** a re-run of a completed section (e.g. re-do measurements with the same params), `run_chain`
takes an optional `start_node`:

```julia
run_chain(proj, String[]; run_id, start_node = "n3")   # re-run n3 + everything downstream
```

`_force_restart_from!` resets `start_node` **and all its descendants** (`_descendants`, transitive
successors over the edge set) back to `:pending` across every image — regardless of status or
`params_hash`. It runs *after* `_reset_stale_nodes!`, so it only ever adds to the reset set. Upstream
(ancestor) nodes are untouched: they stay `:done` and are skipped. This is the whiteboard Live-tab
"resume from here" — pick a node, everything from it down re-runs, everything above it is reused.

The WS `chain:run` message carries `runId` (→ resume) and optional `startNode`; the frontend Live
tab sends them when you hit **Resume** (with or without a picked node). See `docs/UI.md` → Chain
whiteboard.

### Fresh-run start dot (`start_targets` — UML initial node)

For a *fresh* run there's an authored entry point: a **UML start dot** on the edit whiteboard, linked
to the task(s) a run begins from. It's persisted as `ChainTemplate.start_targets` (the node ids the
dot links to; the dot isn't a task). At run start, `run_chain` calls `_prune_to_start`:

- `start_targets` empty ⇒ template unchanged — run the whole chain from its natural roots (backward-
  compatible; existing chains are untouched).
- non-empty ⇒ execute **only the reachable subgraph** — the inclusive descendants of the targets.
  Edges into a target from now-excluded nodes are dropped, so a target becomes an effective root.

So dropping the dot mid-chain runs from there onward (upstream tasks kept in the editor as drafts,
skipped for the run), and linking it to one branch runs that branch while a disconnected branch stays
a draft. Pruning happens once, up front, so every downstream stage (topo sort, per-image state,
resume, the frozen `template_snapshot`) sees one clean effective template — no node knows it was
pruned. Distinct from `start_node` above: `start_targets` is a persisted *authoring* choice for fresh
runs; `start_node` is a transient *resume* choice made on the Live tab.

### In-place restart (no new task record)

Resume re-uses the existing `run.id`. There is no "new task created" on resume — the run record
is mutated in place. This also fixes the module-page rerun-button bug: the frontend sends
`"task:restart"` (same `task_id`) instead of `"task:run"` (new ID), and the task store resets
the existing entry rather than appending a duplicate.

---

## Event bus

`app/src/events.jl` — package-level pub/sub, no API dependency.

```julia
subscribe_chain_events!("node:done",    handler)   # handler(payload::NamedTuple)
subscribe_chain_events!("node:running", handler)
unsubscribe_chain_events!("node:done",  handler)
```

**Events fired** (from `_update_node_state!`, outside the `run._lock`):

| Event | Payload fields | Fired when |
|-------|---------------|-----------|
| `"node:queued"` | *base* = `run_id, chain_name, project_uid, image_uid, node_id, fn, params, task_id` | Node submitted to its pool, waiting for a free slot |
| `"node:running"` | *base* | The job acquired a slot and started (real start) |
| `"node:done"` | *base* `+ result` | Node transitions to `:done` |
| `"node:failed"` | *base* `− params + status` | Node transitions to `:failed`, `:skipped` or `:cancelled` (`status` carries which) |

**`task_id` — the correlation handle.** The scheduler task the node ran as, captured under `run._lock`
alongside the result. It matters because **a chain run emits no `task:status` frames at all**:
`handle_chain_run` passes no `on_status_change`, by design — the GUI renders chain nodes from
`chain:node:*` keyed by `runId::nodeId::imageUid`, and emitting parallel `task:status` frames would give
every chain node a second row in the Task Manager. But chain nodes *are* registered in `_TASKS`, so they
appear in `GET /api/tasks` and hence in the task console — which without `task_id` could only ever
report them as "finished, outcome unseen". The record also carries **`chain_node_id`** (passed down from
the executor's `run_task` call), reported on the snapshot for the mirror-image reason: the GUI keys a chain
row `runId::nodeId::imageUid`, so a snapshot row that names only the *run* cannot be matched to one, and a
reloaded tab adopting it would list the same node twice. A set-scope node bypasses `run_task` entirely, so
it has no record and no node id — clients must treat that as "not adoptable", never guess. It is `""`, never `nothing`, when there is no task to
correlate (skipped/cancelled before submission; set-scope and incremental nodes bypass `run_task`
entirely) — consumers must treat that as "no correlation available". The bridge reads it through
`_ev_task_id`, so a hand-fired REPL event that omits the field can't take chain telemetry down.

**Why fired outside the lock**: handlers may need to read `run.image_states` or trigger further
work. Re-entering `run._lock` from inside the lock would deadlock. The result is captured inside
the lock into a `Ref` before the lock releases, so the event payload is consistent.

**Handler safety**: `_fire_chain_event!` copies the handler list before iterating (so
subscribe/unsubscribe during dispatch affects the next dispatch, not the current one) and wraps
each call in `try/catch` (a misbehaving handler must not disrupt chain execution).

**REPL usage**: subscribe directly from the REPL for live feedback during a chain run:
```julia
subscribe_chain_events!("node:done", p -> println("$(p.image_uid)/$(p.node_id) done"))
run_chain(proj, uids; chain="my-chain")
```

**Triggering a run from the UI**: `ws.ts` sends `{ type: "chain:run", projectUid, chain, imageUids }`. The handler in `sockets.jl` (`handle_chain_run`) calls `load_project(project_uid)` then `run_chain` in a `Threads.@spawn` so the WS thread is never blocked. On success it broadcasts `chain:run:done`; on error `chain:run:failed` (also surfaced in the log console).

**API WebSocket bridge**: `api/src/server.jl` subscribes to **all four** events at startup and
broadcasts each to every connected client, `chain:`-prefixed with camelCase keys:
- `chain:node:queued`  — `{type, runId, chainName, projectUid, imageUid, nodeId, fn, params, taskId}`
- `chain:node:running` — same shape
- `chain:node:done`    — `{…, params, result}`
- `chain:node:failed`  — `{…, status}` (which of failed/skipped/cancelled)

`ws.ts` routes these into `taskStore.addFromChainEvent`, which upserts a `TaskEntry` keyed by `runId::nodeId::imageUid`. The Live tab in `ChainModule.vue` renders these tasks as a VueFlow grid.

---

## State machine per image per node

```
:pending → :queued → :running → :done
                              → :failed
                              → :cancelled (cancel_chain_run! killed the subprocess mid-run)
         → :queued → :cancelled (cancel flag seen before the job got a slot)
         → :skipped  (fault isolation: a DIRECT predecessor is :failed/:cancelled/:skipped)
```

**Fault isolation is per-predecessor, not global.** A node is skipped only when one of its *own*
direct predecessors failed/was cancelled/was skipped — not when *any* node in the chain failed.
This keeps independent branches of a fan-out independent: with `afDriftCorrect → {segA, segB}`, a
failure in `segA` does not skip `segB` (they share only the upstream ancestor). `:skipped` is in
the trigger set so a failure propagates transitively down a branch (pred failed → node skipped →
its successor sees a skipped pred → also skipped). Topo order guarantees every predecessor's status
is set before a node is evaluated. Incremental (plot) predecessors never gate. The predecessor map
is built from `template_snapshot.edges` in `_execute_image_chain!`.

`:queued` is the slot-wait state (submitted to its pool, no free slot yet); `:running` is set when the
job acquires a slot and starts — see *Queue visibility* above.

Transitions are written under `run._lock` and persisted to `run.json` after every change
(`_save_run!` inside the lock). Events are fired outside the lock.

---

## Concurrency invariants

These are easy to break accidentally:

1. **`run._lock` guards `image_states` and `_save_run!`** — never read or write `image_states`
   from outside the lock without understanding the consequence. Tests that mutate states directly
   must call `_save_run!` after.

2. **Events fire outside the lock** — `_fire_chain_event!` is called after the `lock(run._lock) do`
   block returns. Any code path that fires an event must not hold `run._lock`.

3. **Incremental nodes are excluded from fault isolation** — `incremental_ids` set in
   `_execute_image_chain!`. If you add a new scope type, decide whether it gates image progression
   or not, and update this exclusion accordingly.

4. **Set-scope skip check is after `_barrier_wait_all!`** — see the picnic node section above.
   Moving it before causes a deadlock that is non-obvious to debug.

5. **Concurrency lives in the global pool, not the chain** — there is no per-run semaphore. A node
   blocks inside `run_task` on its pool's queue. Don't reintroduce a chain-level gate; size
   the pool in `config.toml` instead. The slot is released in the dispatcher's `finally`, so it comes
   back even if the job dies.

6. **Mark `:queued` before `run_task`, `:running` from the job** — the node must not flip to
   `:running` until the job actually starts (via `on_status_change`), or `startedAt`/elapsed and the
   "waiting vs running" distinction break (the 2/4/6-min bug).

7. **`Threads.@spawn` not `@async`** — tasks call blocking Python subprocesses. `@async` would
   starve other images on the same thread. If you change the execution model, verify that blocking
   I/O in `_run_task` still works.

8. **Every job posts to `job.done` exactly once — in a `finally`.** `run_task` is parked in
   `take!(job.done)`, nothing else will ever wake it, and an exception in the dispatcher's
   fire-and-forget `Threads.@spawn` is **silent**. So a throw escaping `_execute_job!` costs a submitter
   blocked forever and a `TaskRecord` stranded at `:running` (`_deregister_task!` never runs) — while
   looking fine, because the slot was already released: pools read idle as `list_tasks()`, the GUI and
   the task console keep listing a finished task. Hence `post!` (never a bare `put!` — the channel holds
   1) in a `finally`, and an outer `catch` that marks the record `:failed`. Both `Threads.@spawn`s in
   `_start_pool!` log their exceptions for the same reason — and a dispatcher that dies wedges its whole
   pool at `:queued`. Pinned by the *"Job posts its result even when the error path throws"* testset.

   > The console side of this pairs with it: WS task frames are lossy by design, so the task console
   > must reconcile `GET /api/tasks` in **both** directions — see `docs/API.md`. A row that is only ever
   > added produces the same false "still running" readout from the opposite direction.

### Terminal outcomes are banked for replay (`recent_tasks`)

A unit of work on the WS task rail announces how it ENDED exactly once, in one frame — and that frame is
droppable by design (per-client drop-on-full queue in `server.jl`). Nothing survived it: a task is
deregistered the instant it finishes, so `list_tasks()` can only say what is in flight, and a background
job was never in that registry at all. A client that missed the frame could never learn the outcome — the
task console counted every such run as "finished, outcome unseen" (`0 done · 17 ended` for nine images
that all succeeded) and the browser left the task pinned at `running`.

So terminal frames are kept, in `app/src/tasks/task_outcomes.jl`: a bounded log (`_OUTCOME_CAP` = 500)
written by `record_task_outcome!`, read by `recent_tasks(; since)` → `GET /api/tasks/recent`. A consumer
that missed the live frame **reconstructs it** from there — same shape, same handlers, no second code
path (`api/task_console.jl`, `frontend/src/utils/taskReconcile.ts`).

- **Written at the rail's status SINKS, not by the producers.** A terminal outcome reaches a client
  through exactly two carriers, and both bank it: `ws_status` (`api/src/sockets.jl`) for `task:status`,
  and the `node:done`/`node:failed` chain-event subscribers (`api/src/server.jl`) for `chain:node:*`.
  Sinks, not producers, is what makes coverage automatic — scheduler tasks, background jobs
  (`pool="job"`) and batch movies (`pool="viewer"`) all reach `ws_status`, so a new producer needs no
  extra thought. Banking in the scheduler's `_deregister_task!` instead (where this started) covers only
  *that* producer and leaves a dropped project-export frame stranding its row forever. **Both carriers
  are needed:** a chain run passes no `on_status_change`, so a chain node never reaches `ws_status` at
  all — banking only there left every chain node unrecoverable. **Two carriers, two banks, no more.**
  `record_task_outcome!` no-ops on a non-terminal status (and on `"skipped"`, which never ran), so a sink
  hands over every status unfiltered and the terminal test exists in exactly one place.
- **A consequence, and the right one:** a REPL `run_task` banks nothing — no client to recover, no server
  to serve the route. This is a reporting aid for the rail, not task state.
- **One row per task id**, re-appended on repeat. Both repeats are real: a cancel is announced twice
  (immediately from `on_status_change`, then as the final status), and `task:restart` reuses the id for a
  new run whose outcome must supersede the old one.
- **`image_uids` is carried** — a set-scope task's full member list exists *only* on that frame, and a
  replay without it invalidates the representative image's plots alone.
- **`since` is inclusive**, so a poller always re-reads its own newest entry; two units finishing in the
  same millisecond would otherwise let a poll landing between them drop the second forever. Consumers
  de-duplicate by task id, which they must anyway.
- **Not run history.** Fixed size, in memory, gone on restart. Durable per-image history is the run
  log → `GET /api/tasks/history`.

### The durable half: a run is logged twice, not once

`{1/uid}/runlog.json` is the record that outlives every process. It is written at **both** ends of a
run — `open_run_log!` at `:running`, `close_run_log!` at the terminal status — and that shape is
load-bearing, not bookkeeping taste:

| status | means |
|---|---|
| `running` | started; outcome not yet known |
| `done` / `failed` | finished; `failed` is recorded so repeated failures are visible, not just successes |
| `cancelled` | stopped by someone |
| `interrupted` | its **process died mid-run** — reaped at the next project open |

Two bugs are why. First, `:cancelled` used to be skipped on the reasoning that "the user aborted — not
an outcome worth logging". A cancel is also how a task ends when its process is killed, so a
segmentation twenty minutes in could end leaving *nothing*: no entry, no outcome, and a task log that
just stopped mid-stream, indistinguishable from a crash. Second, and the reason opening early is not
optional: **an append-on-finish log cannot record a run that never reaches its finish.** The detached
runner holds its queue in memory with no spool (`docs/RUNNER.md`), so a Ctrl-C or a crash takes every
in-flight task with it and no Julia code ever runs again. Only something already on disk can say the
run happened.

`reap_run_log_for_project!` (`api/src/runner_api.jl`) closes those out at project open, converting
stale `running` entries to `interrupted`. It reaps only what it can *prove* is dead — a task the
detached runner is still executing must survive a backend restart untouched, since that is the runner's
entire purpose — so it skips the reap when the runner is alive but won't list its tasks. See
`_live_task_ids`.

**Readers must treat `status` as an open set.** `running` on disk is a real state, not a corrupt entry,
and `cancelled`/`interrupted`/`running` all mean *no output was produced* — never attribute a result to
one.

### …and so is the START (`note_task_started!`)

The same problem from the other end: a client asking "how long has this been running?" had nothing to
ask, so it timed the task from when it first happened to *see* it — the console printed `≥0s` for a task
that had been running for twenty minutes, and the GUI's elapsed restarted from zero on a page reload.

`TaskRecord` now carries `queued_at` (set at registration) and `started_at` (set in `_set_status!` the
moment a pool slot is acquired), both UTC, both published by `list_tasks()` → `GET /api/tasks` as
ISO-8601 strings. `started_at − queued_at` is therefore the real queue wait, which is what makes a task
blocked on a busy GPU read as *waiting* rather than as a run of zero seconds.

It also carries **`project_uid`** — which project the row's `image_uid` belongs to, resolved from the
image's own path at submit time (`img_project_uid`). One server serves every project under
`projects_dir()`, so a client watching the whole rail rather than one project (the task console) cannot
tell two images apart by uid; it is read off the path rather than stored because a project's identity is
its directory name (`docs/OBJECTMODEL.md`).

The record also carries the **`params` the run was submitted with** (post-`_flatten_sections`, so the
shape `run_task` consumed — and flattening is idempotent, so it can be handed straight back). Same
motivation one step further on: a client that didn't launch the task knew its `fun_name` but nothing
about its configuration, so it had to withhold Re-run rather than relaunch on the JSON defaults. They are
published all-or-nothing (`_publishable_params` → `null` if any value isn't a JSON-native shape), because
`GET /api/tasks` writes the whole snapshot in one `JSON3.write`: one unserialisable value would otherwise
throw and take the endpoint down for every row. It whitelists shapes rather than attempting the write,
because attempting it doesn't fail where it must — JSON3 throws on a `Function` but serialises a plain
struct into an object, and a client would then Re-run on that object as if it were the param.

But the record dies with the task, and the duration is mostly wanted afterwards — the chain bridge fires
`node:done` only once `run_task` has returned *and deregistered*, and a dropped terminal frame is
recovered from `recent_tasks` minutes later. So the start is also banked on the rail, beside the outcomes
and by the same rules:

- **Same sink rule.** The scheduler stamps it when there is a record (exact, at the transition);
  `ws_status` stamps it on the first `running` frame otherwise, which is what covers the producers with no
  record at all — background jobs and batch movies. `note_task_started!` is **first-write-wins**, so the
  two writers can never fight and a re-announced `running` doesn't restart the clock.
- **One home at a time.** `record_task_outcome!` copies the start into the banked row (`started_at`) and
  then forgets the in-flight note, so nothing can report two different starts for one task. The map is
  bounded (`_STARTED_CAP`, oldest evicted) as a backstop for a producer that never announces an outcome.
- **`record_task_outcome!` returns the row it banked**, and every sink publishes *those* values on the
  live frame — so the live frame and the replayed one cannot disagree about when the task ran.
- **`""` means unknown, never epoch zero** (`iso_utc(nothing)`). A task cancelled from the queue never
  ran; a consumer must fall back to its own clock rather than render a duration of decades.

Wire fields, consumers and the fallback rule: `docs/API.md` → *Elapsed time is served, not guessed*.

---

## REPL API

```julia
using Cecelia
init_cecelia!()

# Build a chain with the thin helpers (auto-generates node IDs)
make_chain(proj, "my-pipeline", [
    chain_node("importImages.omezarr"),
    chain_node("cleanupImages.cellposeCorrect"; resource_pool="gpu",
               params=Dict("model" => "cyto2")),
    chain_node("testTasks.set_task"),                # picnic node — "set" comes from its spec
])

# Or build manually (identical result, more control over node IDs)
n1 = ChainNode(; id="import",  fn="importImages.omezarr")
n2 = ChainNode(; id="denoise", fn="cleanupImages.cellposeCorrect", resource_pool="gpu")
save_chain_template!(proj, ChainTemplate("my-pipeline", [n1, n2], [ChainEdge(n1.id, n2.id)]))

# Fresh run (pool concurrency comes from config.toml [pools], not from run_chain)
run = run_chain(proj, image_uids;
    chain       = "my-chain",
    overrides   = Dict("n2" => Dict("threshold" => 0.5)),
    on_log      = line -> println(line))

# Resume (pass run_id, ignore image_uids)
run = run_chain(proj, String[];
    run_id      = run.id,
    overrides   = Dict("n3" => Dict("threshold" => 0.7)))

# Inspect state
run.image_states[uid]["n2"].status      # :done / :failed / :skipped / …
run.image_states[uid]["n2"].result      # Dict returned by the task
run.image_states[uid]["n2"].params_hash # SHA-256 of effective params at run time

# Load a completed run from disk
run = load_chain_run(proj, run_id)
```

---

## File map

| File | Role |
|------|------|
| `app/src/tasks/scheduler.jl` | Resource pools + dispatchers, `TaskRecord`/`_TASKS`, `run_task`, cancel registry |
| `app/src/tasks/chain.jl` | Chain data model, executor, barriers, resume logic, `chain_node`/`make_chain` REPL helpers |
| `app/src/events.jl` | Package-level pub/sub event bus |
| `app/src/tasks/testTasks/image_task.jl` | Mock image-scope task (supports `waitMs` for timing tests) |
| `app/src/tasks/testTasks/set_task.jl` | Mock set-scope task |
| `app/src/tasks/testTasks/incremental_plot_task.jl` | Mock incremental plot task |
| `app/test/runtests.jl` | Pool, chain-run, resume, barrier-policy and job-completion testsets |
| `api/src/routes.jl` | `api_chains_list`, `api_chains_get`, `api_chains_save` — chain CRUD over HTTP |
| `frontend/src/modules/ChainModule.vue` | Whiteboard page: Edit tab (palette, VueFlow canvas, node config) + Live tab (real-time task grid) |
| `frontend/src/components/ChainTaskNode.vue` | Custom VueFlow node for image/incremental-scope tasks |
| `frontend/src/components/ChainPicnicNode.vue` | Custom VueFlow node for set-scope tasks (visually distinct) |
| `frontend/src/components/ChainLiveNode.vue` | Custom VueFlow node for live run display (status-colored header) |
| `frontend/src/components/ChainQcNode.vue` | QC thumbnail in the Live view's QC band (see *Live QC row*) |
