# Where things live

Task-first index of the codebase. Organised by what a person is trying to *do*, not by module name
— for when you know you want to change how something works but not where it lives.

Standard for what a change gets checked against: [`MAINTAINABILITY.md`](MAINTAINABILITY.md).
The area-per-area reference: [root doc index](../CLAUDE.md).

> This map is a **skeleton** — the initial pass (2026-09-15) covers entries derived from three
> anchor files (`scheduler.jl`, `af_correct.jl`, `hmm.jl`) plus a frontend sanity-check. Entries
> get added as the comment/contract audit sweeps other files.

---

## Tasks & the scheduler

| I want to… | Go to |
|---|---|
| Add a new task (module page, JSON spec, Julia handler, Python runner, tests) | [`docs/MODULES.md`](MODULES.md) — the procedural guide |
| Change how a task reports QC findings | `app/src/tasks/<category>/<name>.jl` → the `*_qc_findings` function (convention across all tasks); shared helpers in `app/src/qc.jl` |
| Add or change a cohort-comparable metric | `app/src/qc_cohort.jl` (`COHORT_METRICS`) |
| Change what a task streams into as it runs (preview stores) | The task's `live_outputs(::CciaTask, params)` overload in `app/src/tasks/<name>.jl` and `app/src/tasks/task.jl` |
| Change resource-pool behaviour (add a pool, change a default limit) | `app/src/tasks/scheduler.jl` §*Resource pools* + `app/config.toml [pools]`; live-resize via `resize_pool!` / persisted via `set_pool_limit!` |
| Cancel a chain run / cancel a task | `app/src/tasks/scheduler.jl:cancel_chain_run!` → `cancel_task!` |
| Understand how cancel reaches an in-flight Python subprocess | `app/src/tasks/scheduler.jl:_execute_job!` → the race guard inside the `on_process` closure |
| Change how a task is dispatched (params flatten, defaults, gating) | `app/src/tasks/scheduler.jl:run_task` (four overloads) + `app/src/tasks/task.jl` for the helpers each one calls |
| Change what the task snapshot shows (live rows) | `app/src/tasks/scheduler.jl:list_tasks` — LIVE only; a task disappears on completion |
| Look up how a **finished** task ended (outcome, duration) | `app/src/tasks/task_outcomes.jl:recent_tasks` — the terminal-outcome record. Do NOT infer an outcome from a task's absence from `list_tasks`. |
| Spawn a Python subprocess | Always `run_py` in `app/src/py_runner.jl`. Never build your own. [Root `CLAUDE.md`](../CLAUDE.md) → *Spawning Python* |

## Data model & storage

| I want to… | Go to |
|---|---|
| Read or write cell data (`.h5ad`) | Julia: `app/src/label_props.jl` · Python: `python/cecelia/utils/label_props_utils.py`. Never touch HDF5 internals directly. [Root `CLAUDE.md`](../CLAUDE.md) → *H5AD / cell-data access* |
| Open or write an OME-ZARR image / label store | `python/cecelia/utils/zarr_utils.py` (+ `ome_xml_utils.py`). Always use `staged_store` for writes, always pass `store_compressor(kind)`. [Root `CLAUDE.md`](../CLAUDE.md) → *Image / OME-ZARR access* |
| Change the ccid.json shape or versioned-field convention | `app/src/model/image.jl` (versioned helpers) + `docs/OBJECTMODEL.md` |
| Resolve channel names → indices | `app/src/model/image.jl:channel_indices` and `channel_names`. Never hand-roll a `findfirst`. [`app/CLAUDE.md`](../app/CLAUDE.md) → *Channel names → indices* |

## Analysis families

| I want to… | Go to |
|---|---|
| Change AF correction — QC scoring | `app/src/tasks/cleanupImages/af_correct.jl:af_qc_findings` |
| Change AF correction — frontend→Python param translation | `app/src/tasks/cleanupImages/af_correct.jl:af_combinations_for_python` |
| Change AF correction — run orchestration | `app/src/tasks/cleanupImages/af_correct.jl:_run_task` |
| Change AF weight math (bleedthrough alphas, weight stats) | Python side: `python/cecelia/correction_utils.py` (`af_bleedthrough_alphas`, `af_correct_frame`, `af_weight_stats`) |
| Add or change an HMM fit over track measurements | `app/src/behaviour/hmm.jl:hmm_fit_states` |
| Change how HMM transitions are labelled (self-transitions, start rows, hybrid states) | `app/src/behaviour/hmm.jl:hmm_transitions` |
| Change track-measure algorithms (celltrackR port) | `app/src/tasks/tracking/track_measures.jl` |

## Frontend

| I want to… | Go to |
|---|---|
| Change what appears in the viewer's side panel (label rows, live previews, populations) | `frontend/src/components/ViewerPanel.vue` |
| Add or change the smoothing-method schematic | `frontend/src/tasks/smoothVis.ts` (design-doc-in-code — read the header) |
| Render **any** UI primitive (button, toggle, popover, chip, modal, spinner, badge, empty state) | [`docs/ui/PRIMITIVES.md`](ui/PRIMITIVES.md) — canonical catalog. A new variant is a bug. |
| Write user-facing text | [`docs/ui/COPY.md`](ui/COPY.md) — before typing any label, tooltip or empty state |
| Persist a user-settable option | Always via `useViewState`, never a bare `ref()`. [`frontend/CLAUDE.md`](../frontend/CLAUDE.md) → *Persist every user-settable option* |
| Coalesce a continuous control (slider, wheel, drag) | One of three canonical schedulers (`debouncedLatest`, `rafCoalesce`, `debouncedSave`). Never hand-roll a fourth. [`frontend/CLAUDE.md`](../frontend/CLAUDE.md) → *A continuous control's effect is coalesced* |

## Concurrency-critical files

Files where comments document lock ordering, cancellation races, or silent-failure contracts.
**Do not casually trim comments here** — see [`MAINTAINABILITY.md`](MAINTAINABILITY.md) →
*Correctness-critical comments*.

- `app/src/tasks/scheduler.jl` — pool concurrency, task registry, cancellation, `_execute_job!`

## Structure register — files where new functionality should NOT be appended by default

The next feature in this area belongs in its own file, not on the end of the existing one.
See [`MAINTAINABILITY.md`](MAINTAINABILITY.md) → *File responsibility*. Full register with
proposed seams: [`docs/archive/comment-audit-findings.md`](archive/comment-audit-findings.md) →
*Structure register*.

**Highest ROI (Tier 1):**
- `app/src/tasks/task.jl` (1657 L, 14 sections, 45 commits/6mo). ~14 mixed responsibilities.
  Split by `task/spec.jl`, `task/validate.jl`, `task/composite.jl`, `task/dispatch.jl`, etc.
- `app/src/gating/population_manager.jl` (2412 L, 16 sections, 36 commits/6mo). Splitting
  **requires preserving the `uid_index` sync invariant** — tests must pin uid_index/pops parity.
- `app/src/tasks/chain.jl` (1415 L, 19 sections). Splitting **requires preserving the
  `ChainRun._lock` + `_barriers` invariants** — same class as `scheduler.jl`, milder.

**Worthwhile (Tier 2):**
- `app/src/tasks/importImages/omezarr.jl` (1074 L) — separable metadata reader.
- `app/src/qc.jl` (1015 L) — `QC_TEXT` catalog is separable.
- `api/src/routes.jl` (3053 L) — split by route family.

**Anchor (Tier 3):**
- `app/src/tasks/cleanupImages/af_correct.jl` — run + QC + param translation. Cleaner
  seams: `af/run.jl`, `af/qc.jl`, `af/translate.jl`.
- `app/src/tasks/scheduler.jl` — 733 lines, five responsibilities. Splitting requires explicit
  lock-ordering-invariant proof; not a routine cleanup.

---

*This is not an exhaustive file listing. For that, see [`docs/inventory/`](inventory/).*
