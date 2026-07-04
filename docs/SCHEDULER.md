# Chain Scheduler

Design reference for `app/src/tasks/chain.jl` and `app/src/events.jl`.

The scheduler exists to solve two problems the old R version had: (1) images were processed in
lockstep batches — image 1 sat idle after finishing import instead of starting denoise immediately,
and (2) there was no per-stage concurrency awareness — GPU-bound denoising and CPU-parallel
segmentation were treated identically. This file documents every non-obvious decision made to
fix those problems.

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
image_tasks  — one OS thread per image, walks all "image"-scope nodes in topo order
set_tasks    — one OS thread per "set"-scope node, waits for all images then runs once
incr_tasks   — one OS thread per "incremental"-scope node, event-driven debounced watcher
```

All three kinds run concurrently. `run_chain` `fetch`es all of them before returning —
it blocks the caller until the entire chain finishes.

### Why OS threads (not async tasks)?

Chain nodes call `run_task`, which may block on a Python subprocess. Julia's async (`@async`)
doesn't help for blocking I/O — it only helps for I/O that yields back to the scheduler.
`Threads.@spawn` puts each image on a real OS thread so blocking one image never stalls others.

---

## Resource pools

There is **one** concurrency mechanism: the global scheduler worker pools. (An earlier design
had a second, per-run `Base.Semaphore` layer on `ChainRun`; it was removed — it double-gated and
was never wired from the API, which silently disabled it. Pools are now config-only.)

### Scheduler worker pools (global, config-defined)

`ResourcePool` structs in `_POOLS::Dict{String, ResourcePool}` in `scheduler.jl`. Each pool owns
exactly `limit` long-lived OS worker threads draining a shared `Channel` of `TaskJob`s. A pool
with `limit = 1` runs its jobs strictly one at a time — that is how `gpu` work is serialised.

Pools are **global and persistent**, shared across every chain run and every module-page task —
all execution goes through `run_task` → `_pool(name)` → the pool's queue. Defined in `config.toml`:

```toml
[pools]
default   = 20   # general-purpose CPU work
gpu       = 1    # GPU-bound (cellpose): serialised
gpu-light = 4    # GPU work tolerant of light concurrency
io        = 8    # IO-bound (bioformats2raw)
```

`_pools_init!` reads `[pools]` at first use. `default` is guaranteed to exist (falls back to
`tasks_concurrent_limit()` if absent). A node/task names its pool via the `resource_pool` field
of its JSON spec (or, for a chain node, `ChainNode.resource_pool`).

**Missing-pool fallback warns.** If a task requests a pool not in `[pools]`, `_pool` falls back to
`default` and emits a one-time `@warn` (a GPU task silently landing in the 20-wide default pool was
the original "all GPU tasks run at once" bug). Add the pool to config to silence it.

`resize_pool!(name, limit)` creates or resizes a pool at runtime (REPL/test path; e.g. tests
register `slow_pool`/`par_pool` this way). `list_pools()` returns `[(; name, limit)]` for every
initialized pool, exposed via `GET /api/pools` and shown in the pool dropdowns.

### Queue visibility — :queued vs :running

A node that is waiting for a pool slot and a node actively executing are **different states**, and
the live view must distinguish them (a saturated GPU pool must not look like a hang).

`_execute_image_chain!` marks the node `:queued` *before* calling `run_task`, then flips it to
`:running` only when a pool worker actually picks the job up — driven by `run_task`'s
`on_status_change` callback (the worker calls `_set_status!(rec, :running)` in `_execute_job!`).
So `startedAt` (and the live elapsed timer) counts from the real slot acquisition, not from when
the image thread reached the node. With `gpu = 1` and three images, the live grid shows one
`:running` and two `:queued`, and each task's elapsed reflects its own ~2 min, not 2/4/6 min.

> **Pitfall (tests):** `node:running` now fires from the pool worker thread while `node:done`
> fires from the image thread, so a size-1 pool has a benign running/done handoff overlap (both
> contend on `run._lock`). Assert serialisation by **wall-clock** (sum of durations), not by
> counting concurrent `node:running`/`node:done` events. A size-N>1 pool is race-free because all
> N `running` events precede any `done`.

---

## Chain cancel

### Data structures

`_CANCELLED_CHAINS::Set{String}` — set of run IDs that have been cancelled.
`_CANCELLED_CHAINS_LOCK::ReentrantLock` — guards all reads and writes to the set.

Both live in `api/src/scheduler.jl`.

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

**Race guard (start window):** if cancel arrives after a worker sets `:running` but before the
task's `on_process` callback records the subprocess handle, `cancel_task!` would find `rec.proc ==
nothing` and skip the kill. `_execute_job!`'s `on_process` wrapper closes this: right after storing
`rec.proc`, it re-checks `is_cancelled(job.id)` and kills immediately if so.

### Limitation — set-scope / incremental subprocesses

The per-image path is fully covered. Set-scope and incremental nodes call the multi-image
`_run_task` directly with `on_process = _ -> nothing` and are **not** registered in `_TASKS`, so a
chain cancel won't kill a subprocess they spawned mid-run (the between-node flag still stops
not-yet-started ones). No real set-scope subprocess task exists yet (only mock/plot tasks); see
TODO #00016 if one is added.

---

## pool_name override in run_task

`run_task` (both overloads in `scheduler.jl`) accepts an optional kwarg:

```julia
run_task(task_type, img, params; pool_name::String = "", ...)
```

When `pool_name` is non-empty, it overrides the pool name from the task spec's `resource_pool`
field. This allows the caller to direct a task to a specific scheduler worker pool at call time.

**From the WS handler**: `handle_task_run` in `sockets.jl` reads `poolName` from the WS message
payload and passes it as `pool_name` to `run_task`. The frontend sends `poolName` in the
`task:run` message, populated from the pool dropdown in `TaskRunner.vue`.

**From chain nodes**: `_execute_image_chain!` passes `pool_name = node.resource_pool` so each
node routes to the correct global pool even when the task spec's default differs.

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
applied to a set of images. Stores a **frozen copy** of the template via a SHA-256 content hash, not
a pointer to the template file. The template content is cached once under
`settings/chains/.cache/<hash>.json`.

Run records store `template_hash` (not the template inline) to keep `run.json` compact.
`load_chain_run` resolves the hash to the cached template.

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
| `"node:queued"` | `run_id, project_uid, image_uid, node_id, fn, params` | Node submitted to its pool, waiting for a worker slot |
| `"node:running"` | `run_id, project_uid, image_uid, node_id, fn, params` | A pool worker picked the job up (real start) |
| `"node:done"` | `run_id, project_uid, image_uid, node_id, fn, params, result` | Node transitions to `:done` |
| `"node:failed"` | `run_id, project_uid, image_uid, node_id, fn, status` | Node transitions to `:failed`, `:skipped`, or `:cancelled` (`status` carries which) |

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

**API WebSocket bridge**: `api/src/server.jl` subscribes to all three events at startup and broadcasts them to all connected clients as:
- `chain:node:running` — `{type, runId, projectUid, imageUid, nodeId, fn, params}`
- `chain:node:done`    — `{type, runId, projectUid, imageUid, nodeId, fn, params, result}`
- `chain:node:failed`  — `{type, runId, projectUid, imageUid, nodeId, fn, status}`

`ws.ts` routes these into `taskStore.addFromChainEvent`, which upserts a `TaskEntry` keyed by `runId::nodeId::imageUid`. The Live tab in `ChainModule.vue` renders these tasks as a VueFlow grid.

---

## State machine per image per node

```
:pending → :queued → :running → :done
                              → :failed
                              → :cancelled (cancel_chain_run! killed the subprocess mid-run)
         → :queued → :cancelled (cancel flag seen before the pool worker picked it up)
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

`:queued` is the slot-wait state (job submitted to its pool, no worker free yet); `:running` is
set when a pool worker actually starts the job — see Queue visibility above.

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
   blocks inside `run_task` on its pool's worker queue. Don't reintroduce a chain-level gate; size
   the pool in `config.toml` instead. Slots are freed when `_execute_job!` returns (no `finally`
   release to forget).

6. **Mark `:queued` before `run_task`, `:running` from the worker** — the node must not flip to
   `:running` until a pool worker starts it (via `on_status_change`), or `startedAt`/elapsed and the
   "waiting vs running" distinction break (the 2/4/6-min bug).

7. **`Threads.@spawn` not `@async`** — tasks call blocking Python subprocesses. `@async` would
   starve other images on the same thread. If you change the execution model, verify that blocking
   I/O in `_run_task` still works.

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
    chain_node("testTasks.setTask"; scope="set"),   # picnic node
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
| `app/src/tasks/chain.jl` | Chain data model, scheduler, executor, resume logic, `chain_node`/`make_chain` REPL helpers |
| `app/src/events.jl` | Package-level pub/sub event bus |
| `app/src/tasks/testTasks/imageTask.jl` | Mock image-scope task (supports `waitMs` for timing tests) |
| `app/src/tasks/testTasks/setTask.jl` | Mock set-scope task |
| `app/src/tasks/testTasks/incrementalPlotTask.jl` | Mock incremental plot task |
| `app/test/runtests.jl` | Verification tests (Steps 4–7) |
| `api/src/routes.jl` | `api_chains_list`, `api_chains_get`, `api_chains_save` — chain CRUD over HTTP |
| `frontend/src/modules/ChainModule.vue` | Whiteboard page: Edit tab (palette, VueFlow canvas, node config) + Live tab (real-time task grid) |
| `frontend/src/components/ChainTaskNode.vue` | Custom VueFlow node for image/incremental-scope tasks |
| `frontend/src/components/ChainPicnicNode.vue` | Custom VueFlow node for set-scope tasks (visually distinct) |
| `frontend/src/components/ChainLiveNode.vue` | Custom VueFlow node for live run display (status-colored header) |
| `frontend/src/components/plots/IntensityHistogram.vue` | Plotly before/after intensity histogram; lazy-loads `plotly.js-dist-min` |
| `frontend/src/plotly-dist-min.d.ts` | TS module shim re-exporting `plotly.js` types for `plotly.js-dist-min` |
