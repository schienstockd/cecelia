# Resource pools — redesign + live throttles

Status: **Slice 1 + Slice 2 done** (recategorise + rename, and live per-pool sliders). Once merged
+ verified live, the durable parts here are covered by `docs/SCHEDULER.md` → *Live pool limits*.

## Why

The old pools (`default`/`gpu`/`gpu-light`/`io`) fused *which resource* with *how much
concurrency* into one name. To throttle SMB imports the only limit-1 bucket was `gpu`, so imports
and cellpose collided. The R version's single "concurrent tasks" slider was easy but resource-blind
(turning it down serialised the GPU along with everything else).

## Locked decisions

- **One pool per real bottleneck resource; the name says *what* it rations, not how much.**
  - `cpu` (default 20) — general compute; the fallback pool (`_task_pool_name` / `_pool` default).
  - `gpu` (1) — cellpose family. `gpu-light` folded in (a shared GPU cares about *total* GPU jobs).
  - `io` (8) — local disk IO: bioformats2raw import/convert, crop.
  - `network` (1) — **reserved for future HPC/remote task runners; no tasks assigned yet.** Kept in
    on purpose so remote runners have a lane later.
- **Nothing is pinned** (GPU included — batch segmentation at 4 is fine). Config limits are just
  starting defaults.
- **Imports throttle via the `io` slider**, not a network pool — the SMB pull is `io` work today.

## Slice 1 — recategorise + rename (DONE)

`config.toml [pools]`; all task JSON `resource_pool` (23 `default`→`cpu`, 2 `gpu-light`→`gpu`; the 3
`io` tasks unchanged); `scheduler.jl` fallback/guaranteed pool `default`→`cpu`; `TaskRunner.vue`
default; docs (SCHEDULER/MODULES/CUSTOM_MODULES/CLAUDE); test `Resource pool mapping` (pins the map +
asserts no task resolves outside {cpu,gpu,io,network}).

## Slice 2 — live per-pool sliders (DONE)

Adjust each pool's limit at runtime, no config edit / restart — the R-slider spirit, per pool.

- **Backend:** `set_pool_limit!(name, limit)` (`scheduler.jl`) = `resize_pool!` (apply now) +
  persist to `custom.toml [pools]` (merged, `set_projects_dir!` pattern via `custom_toml_path()`).
  Clamped to `[1, POOL_LIMIT_MAX=64]`. Route `POST /api/pools/set` `{name, limit}` (`api_pool_set`,
  wired in `server.jl`) — only already-configured pool names accepted (else `400`).
- **Resize = slot-acquire model** (rewritten twice — the original swapped the queue and orphaned
  queued jobs; the interim worker-count model had a narrow idle-burst oversubscription window). Final:
  `ResourcePool` has one fixed `queue`, a mutable `limit` (slot budget), a live `in_flight`, and a
  `Threads.Condition`. One dispatcher per pool pulls a job, `_acquire_slot!` (blocks while
  `in_flight >= limit`), runs it on its own task, `_release_slot!` on finish. Grow `notify`s → backlog
  fans out; shrink → dispatcher blocks until in-flight drain, then admits next. Slot claimed at
  execution time, so concurrency NEVER exceeds `limit` even right after a throttle-down. See
  docs/SCHEDULER.md → *Resource pools*. Tests: throttle-up parallelises backlog; throttle-down settles
  (wall-clock).
- **UI:** `SettingsModule.vue` "Task scheduler" section — compact 2×2 grid, **row 1 `cpu`/`gpu`,
  row 2 `io`/`network`**, one `<input type=range>` per pool, `@change` → `POST /api/pools/set`.
- **Tests:** `set_pool_limit!` resize+persist+clamp (app/test, temp `CECELIA_DEV_DIR`); unknown-pool
  `400` guard (api/test).

## Notes / gotchas

- Don't reintroduce a second global gating layer — an earlier per-run `Base.Semaphore` on
  `ChainRun` was removed for double-gating (see docs/SCHEDULER.md). Pools are the one mechanism.
- Separate pools are **independent queues — they don't sum to a global budget.** A per-pool slider
  is the right model *because* of this (throttle `io` alone, leave `cpu`/`gpu` full).
