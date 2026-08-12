# Cecelia Feijoa — Architecture Reference

---

## Layer boundary (Package / API / GUI)

The hard rule: **Cecelia.jl must be usable headless from the Julia REPL with zero knowledge of HTTP, WebSockets, or Vue.**

```
cecelia-feijoa/
  app/          Julia package — Cecelia.jl + each task's co-located Python runner
                (app/src/tasks/<cat>/<name>_run.py, run by path via run_py)
  api/          Julia API server — depends on Cecelia.jl as Pkg dependency
  frontend/     Vue 3 — unchanged
  python/       Installable Python package `cecelia` — IO LIBRARY only: analysis/IO helpers
                (cecelia.utils) + writers. NO task runners. Top-level sibling of app/.
  napari/       Python napari bridge process (imports `cecelia`; not part of the package)
```

**`python/` is the installable IO library; task runners live with their task in `app/`.** A task's
`_run.py` is run by file path (never imported), so it doesn't make `app/` an importable Python
package. The Python helpers (`cecelia.utils` + writers) are a
*separate*, pip-installable **IO library** at `python/cecelia/` (moved there from the old `app/py/`
in 2026-07 so external consumers can `pip install cecelia`), holding **no task runners**. `run_py`
(`app/src/py_runner.jl`) resolves `"tasks/…"` under `app/src/` (co-located runners) and other
scripts (e.g. `"writers/…"`) under `python/cecelia/`, and sets `PYTHONPATH=python/` so every runner
can `import cecelia.*`. Dependency split: light IO deps in `python/pyproject.toml`; heavy/conda/
per-platform deps in `pixi.toml`. See [`docs/todo/PY_PACKAGING_PLAN.md`](todo/PY_PACKAGING_PLAN.md).

### Every language boundary carries a version

Julia and Python meet at three places, and **each one can end up with the two halves running different
code**. Two of them hold a long-lived Python process that is *adopted* rather than relaunched (it
outlives a backend crash or Ctrl-C on purpose); the third spawns Python fresh every run, so it is the
**Julia** half that goes stale — `app/src` is Revise-tracked and a branch switch or a merge under a
live server does not always reload it.

| Boundary | Julia | Python | Mismatch → |
|---|---|---|---|
| napari bridge (:7655) | `NAPARI_PROTOCOL` (`app/src/napari.jl`) | `PROTOCOL` (`napari/napari_bridge.py`) | refuse to adopt, kill the port, relaunch |
| preview worker (:7656) | `PREVIEW_PROTOCOL` (`app/src/preview.jl`) | `PROTOCOL` (`preview/preview_worker.py`) | refuse to adopt, relaunch |
| task params (`run_py`) | `PY_CONTRACT_VERSION` (`app/src/py_runner.jl`) | `CONTRACT_VERSION` (`cecelia.utils.script_utils`) | the runner exits, naming the restart |

**Bump both sides together**, and add a row rather than a bespoke check. The pairs are asserted equal by
the `language boundaries agree on their protocol` testset — before it existed the preview pair had been
bumped by hand three times and the fourth was nearly missed.

**When to bump is BEHAVIOURAL, not structural: whenever an adopted older peer would answer
differently.** A changed message shape is the obvious case; a bug fixed *inside* an adopted process is
the one that gets missed, and it is the worse of the two. `PREVIEW_PROTOCOL` 5 is exactly that — the
reply shape is byte-identical to 4, and the only change is that AF preview no longer dies on an
`AttributeError`. Left unbumped, a backend carrying the fix adopts the broken worker and serves the bug,
which is how the fix appeared not to work at all. The version is the only thing that can refuse a
process we did not start, so anything we would not want served from the old code has to move it.

Why versions at all, rather than trusting the processes to match: a mismatch is **never a clean
failure**. A stale peer answers the handshake perfectly and then misreads the actual work. The three
real occurrences read as `unexpected keyword argument 'mask'`, a bare `Preview failed`, and
`invalid literal for int() with base 10: 'CH3'` — none of which named the cause. The params guard lives
in `script_params` (the one function every runner already calls) so a new runner is covered without
doing anything, and an ABSENT `CECELIA_PY_CONTRACT` is allowed through so replaying a saved params file
by hand still works.

### Layer ownership

| Concern | Layer | Notes |
|---------|-------|-------|
| Data model (project/image/set structs, ccid.json) | Package | `model/image.jl`, `model/set.jl`, `model/project.jl` |
| Config loading, path helpers | Package | `config.jl` |
| Versioned-variable helpers | Package | `helpers.jl` |
| Lockfile / `with_transaction` | Package | `model/project.jl` |
| Task abstract type + validation | Package | `tasks/task.jl` |
| Concrete task implementations | Package | `tasks/<category>/<name>.jl` |
| Task param specs (JSON) | Package | `tasks/<category>/<name>.json` — single source of truth |
| NapariViewer (control protocol) | Package | `napari.jl` — REPL-callable, no server needed |
| REST route handlers | API | `api/src/routes.jl` |
| WS task queue + status push | API | `api/src/sockets.jl` |
| Napari bridge lifecycle (multi-client) | API | `api/src/napari_api.jl` |
| HTTP/WS server entry point | API | `api/src/server.jl` |
| Serving task param specs to Vue | API | `GET /api/tasks/definitions?category=X` |
| UI rendering, param forms | GUI | Vue, Pinia — no param definitions duplicated here |

### REPL-runnable contract

```julia
using Cecelia
init_cecelia!()

# Load by UIDs
img = init_object("proj-uid", "img-uid")

# Type dispatch
run_task(ImportOmezarr(), img, Dict("pyramidScale" => 2))

# String dispatch (ergonomic for notebooks/scripts)
run_task("proj-uid", "img-uid"; fun_name="importImages.omezarr", params=Dict("pyramidScale" => 2))

# Batch
run_tasks("proj-uid", ["uid-1", "uid-2"]; fun_name="cleanupImages.cellposeCorrect", params=params)
```

Both forms validate params against the co-located `.json` spec before running. Invalid params throw `ParamValidationError` with the constraint that was violated.

**Side channels are dependency-injected, not hard-wired.** `_run_task` takes `on_log`,
`on_progress`, and `on_process` callbacks (defaults: `println`, no-op, no-op). The task is
sink-agnostic — it never references the WebSocket. The API binds these to `ws_log`/`ws_progress`
and the process registry (`api/src/sockets.jl`); a REPL/test caller uses the defaults or injects
its own. This is the mechanism that keeps the package runnable headless — a task that called
`ws_progress` directly would break the contract and fail with no WS running.

### Audit table (post-cleanup)

| File | Classification | Reason |
|------|---------------|--------|
| `app/src/config.jl` | PACKAGE | Config loading, path helpers — no HTTP |
| `app/src/utils.jl` | PACKAGE | `gen_uid`, `_dir_bytes` — pure utilities |
| `app/src/helpers.jl` | PACKAGE | Versioned-dict helpers — pure |
| `app/src/model/image.jl` | PACKAGE | CciaImage; round-trips every ccid.json field (status, attr, imChannelNames, filepath) |
| `app/src/model/set.jl` | PACKAGE | CciaSet, `init_object`, `delete_image!` |
| `app/src/model/project.jl` | PACKAGE | CciaProject, `delete_set!`, naive `with_transaction` lockfile |
| `app/src/napari.jl` | PACKAGE | NapariViewer opens its own WS to bridge |
| `app/src/tasks/task.jl` | PACKAGE | Abstract type, validation, REPL contract |
| `app/src/tasks/importImages/omezarr.jl` | PACKAGE | bf2raw via callbacks — no WS |
| `app/src/tasks/importImages/remove.jl` | PACKAGE | File deletion + ccid.json update — no WS |
| `app/src/tasks/cleanupImages/cellpose_correct.jl` | PACKAGE | Python subprocess via callbacks — no WS |
| `app/src/tasks/*/*.json` | PACKAGE | Param specs — served via API, not bundled in Vue |
| `app/src/gating/population_manager.jl` | PACKAGE | Gating engine + `pop_df` unified accessor (cell/track) — pure, no HTTP |
| `app/src/plotting/plot_data.jl` | PACKAGE | Summary-plot aggregation (`pop_df` → histogram bins / frequency counts) — pure `Dict`, no HTTP |
| `app/src/plotDefinitions/*.json` | PACKAGE | Plot-type specs (one plot per file) — served via API like task specs, not bundled in Vue |
| `api/src/server.jl` | API | HTTP+WS entry point, router |
| `api/src/routes.jl` | API | REST handlers — delegate to package; shape responses only |
| `api/src/gating_api.jl` | API | Gating/plot HTTP routes — delegate to `pop_df`/`plot_summary_data`; serve plot specs (`GET /api/plots/definitions`), aggregate (`POST /api/plot_data`) |
| `api/src/sockets.jl` | API | WS task queue, process registry, `_kill_tree`, status push |
| `api/src/napari_api.jl` | API | Napari bridge lifecycle + broadcast for multi-client |

The plotting canvas obeys the same boundary as everything else: **aggregation is a PACKAGE function**
(`plot_summary_data`, headless-testable, returns a plain data structure), the **route is a thin API
wrapper** (parse request → call package → serialise), **plot specs are PACKAGE JSON served over a GET
route** (Vue keeps no copy), and the **Vega-Lite rendering is frontend-only** (the package never emits
chart config, only aggregated numbers). A summary panel that needed bespoke Julia HTTP code, or a Vue
component that computed bins from raw cells, would be a boundary violation.

> **Cleanup (this audit):** the pre-separation monolith orphaned in `app/src/` —
> `server.jl`, `tasks.jl`, `projects.jl`, `metadata_handler.jl`, `napari_handler.jl` — was deleted
> (none were loaded by `Cecelia.jl` or the live server; one even `include`d an already-deleted file).
> Task implementations were consolidated from flat `tasks/*.jl` into co-located
> `tasks/<category>/<name>.{jl,json}`, matching the module pattern.

---

## Data model ↔ ccid.json contract

See [`docs/OBJECTMODEL.md`](OBJECTMODEL.md) for the full schema, versioned field conventions, and disk layout.

**Invariants to enforce here:**

- `save!` must write back **every** persisted field; a field not in the struct is silently dropped on next save. When you add a field: update the struct, `save!`, `_load_image`/`_load_set`, and add a round-trip test in `app/test/runtests.jl`.
- `api/src/routes.jl` does **not** parse or mutate `ccid.json` directly. All mutations go through package functions (`delete_image!`, `init_object` → mutate → `save!`). The only exception is project discovery (`_scan_projects_raw`), which reads `project.json` directly as a lightweight listing.

---

## QC (quality-control) sidecars

A task can exit 0 yet produce output that's quietly wrong (e.g. drift correction on a reference channel
that didn't track → the canvas balloons). The **QC layer** (`app/src/qc.jl`, image-owned) lets any task
emit **advisory** findings about the output it produced.

- **Convention:** one JSON per (task, output) at `1/{uid}/qc/{funName}/{valueName}.json`
  (no-value_name tasks → `"default"`). Written via `write_qc(img, fun_name, value_name, findings; …)`,
  read via `read_qc` / `read_all_qc`.
- **Contract:** each file has a generic `findings` list — `{level ("info"|"warn"), code, short, long,
  detail?}`. `"error"` is reserved: **QC never fails or gates a task.**
- **Boundary:** the **backend computes** findings (thresholds live in Julia — one source of truth); the
  **GUI only renders** them (badge + tooltip). `api_images_meta`'s `_image_payload` exposes
  `qc = read_all_qc(img)`; `frontend/src/lib/qc.ts` aggregates into a badge (ImageTable; MetadataPanel +
  whiteboard are later phases).
- **First producer:** `cleanupImages.driftCorrect` — persists the applied per-frame drift and flags a
  registration whose own measurements contradict each other (`drift.unreliable`), frames it could not
  register at all (`drift.unregistered_frames`), a large inter-frame jump (`drift.jump`) or abnormal
  XY canvas growth (`drift.canvas_expansion`).
- **A finding is worth more when it can't be fooled by the thing it is checking.** The first three
  drift findings above read the *trajectory*, so they can only compare it against an expectation of
  how much drift is normal — which is why a movie that genuinely moves a lot looks like a broken one.
  `drift.unreliable` instead compares the registration against **itself** (`shift(a→b) + shift(b→c)`
  must equal `shift(a→c)`), needs no ground truth, and separated every movie on this machine that
  registered (0.13–0.39 px RMS) from the one that did not (24 px) by ~60×. Prefer a self-consistency
  check over a threshold on the answer whenever the task can produce one.

(Where the data *is*, as opposed to how good it is, is not a QC concern — see
*Valid box* below.)

Full design + phased plan: [`docs/todo/QC_PLAN.md`](todo/QC_PLAN.md).

---

## Valid box — which part of a store is data

A task may write a canvas larger than its data. Drift correction expands the canvas to hold the
whole trajectory and drops each frame into a **zeroed** canvas at that frame's own offset, so the
rest is padding — 3–56% of the z planes on real movies here, one going from 8 to 18. NGFF has no way to
say where the data is, so without this a consumer either treats padding as background (it will skew
any background estimate, and it borders real signal) or pays to process it.

**One question, one answer, on the store.** `zarr_utils.write_valid_box` / `read_valid_box`, under a
namespaced `cecelia` attr next to the pixels:

```python
box = zarr_utils.read_valid_box(path)                  # None  → the whole store is valid
box = zarr_utils.read_valid_box(path, timepoint=t)     # {'Z': (5, 13), 'Y': …, 'X': …}
box = zarr_utils.read_valid_box(path, level=1)         # rescaled to that pyramid level
```

Design points, each of which was a candidate mistake:

- **On the store, not in the producer's QC sidecar.** QC is advisory and task-scoped; a consumer
  would have to know *which* task padded and where its JSON lives. This is a property of the data,
  so it travels with a copy or an export.
- **`None` means "all valid",** which is every store that never padded. That is what lets a
  consumer have one code path instead of special-casing drift output.
- **The producer passes the numbers it placed the pixels with** — for drift,
  `correction_utils.drift_frame_slices`, the call the writer itself uses. Not a second derivation
  that can disagree. (Same discipline as *Calibration — three copies, one stamp* in `CLAUDE.md`.)
- **Level-0 coordinates,** rescaled on read by the same `DOWNSAMPLED_AXES` rule the NGFF scale uses;
  start floors and stop ceils so a level never crops real data.
- **One producer, but every derived store must carry it.** Drift correction is the only task that
  *computes* a box. It is not the only one that has to think about one — see the propagation rule
  below.

**Two traps.** The box is per timepoint, and each frame sits at its own offset *because* the
correction aligned them in the shared canvas — cropping each frame to its own box puts them back out
of register. Crop to a common region or not at all. And the intersection across all timepoints can
be **empty**, which is not hypothetical: it is true for 4 of the 9 movies in `kSUFux`, where the
z-drift exceeded the 8-plane stack depth.

### Propagating it — the rule for anyone writing a derived store

**A box that does not survive the pipeline buys nothing.** Drift correction writes one; what people
then segment is a *smoothed* or *AF-corrected* version of that store. If the box stops there, every
consumer sees "all valid" and the padding is processed anyway — which is exactly what happened:
`af_correct` and `cellpose_correct` dropped it silently, and `smooth` carried it via
`read_valid_box(path)`, which on a per-frame box returns the **union over frames** — nearly the whole
canvas once the window drifts. The box survived in name while losing the only thing that made it
useful.

So there is **one call**, and it is unconditional:

```python
zarr_utils.carry_valid_box(src, staging)      # after the store exists; returns whether it carried
```

Never `read_valid_box` + `write_valid_box` — that is the union bug. `carry_valid_box` preserves
per-frame boxes as per-frame, and **self-refuses when the geometry moved**: it compares only the
axes the box speaks about, so a label store that legitimately dropped C still carries, while a crop,
a resize or a Z-MIP does not. A per-frame box additionally needs T at the same length, or the frame
index it is keyed by means nothing. That is why the caller never branches on its own mode —
`segment.branching` makes the same call whether it keeps Z in 3D or flattens it, and gets the right
answer both times.

**Absent is always safe.** A consumer reads `None` as "all valid" and merely does more work, never
the wrong work. The failure mode this guards against is not danger, it is *silence* — so silence is
what is forbidden: a runner that opens a `staged_store` must call `carry_valid_box`/`write_valid_box`
or carry a `VALID-BOX-EXEMPT: <why>` comment. Enforced by
`python/cecelia/tests/test_valid_box_propagation.py`, which also fails the read-without-timepoint
union pattern outright.

### Consuming it

Narrowing to the box belongs at the boundary the work goes through, once, not in each algorithm
behind it — segmentation does it in `SegmentationUtils.predict_from_zarr`, so every backend gets it
by implementing nothing (`docs/SEGMENTATION.md` → *Skipping the padding a drift correction added*).
The rule that boundary has to hold: **if it hands out several arrays for the same unit of work, all
of them narrow together.** Segmentation hands a temporal backend both a tile and a window through
time, read from different places, and narrowing one of them was enough to break it.

---

## Napari

See [`docs/NAPARI.md`](NAPARI.md) for the full bridge design, command protocol, OME-ZARR loading, contrast limits, and layer props.

**Architectural invariant:** `napariImageUid` in `project.ts` is set by the `napari:opened` WS broadcast, not the HTTP 200 response — this keeps it in sync on both the immediate (200) and deferred startup (202) paths.

---

## Napari → Julia event flow (gating linked brushing)

```
Draw a region on the Cell selection layer in Napari
  → napari_bridge.py: point-in-polygon over cell centroids → inside label IDs
  → POST http://localhost:8080/api/napari/event {type:"cellSelection", labels:[…], …}
  → Julia /api/napari/event: store as transient "Napari selection" pop; broadcast_ws gating:popmap
  → frontend ws.ts → gating store: tree gains the transient pop → flow plots highlight those cells
```

See `docs/NAPARI.md` (commands) and `docs/POPULATION.md` (transient pops). Julia stays the sole
gate evaluator; napari only draws regions and displays membership.

Components: `ws.on(...)` in `onMounted`, `ws.off(...)` in `onUnmounted`.
This replaces R/Shiny's pattern of writing files + `reactiveFileReader`. No polling.

---

## WS broadcast

`broadcast_ws(msg)` in `server.jl` sends JSON to all connected browser tabs.
Clients tracked in `_ws_clients::Set` guarded by `_ws_clients_lock`.
`handle_ws` registers on connect, deregisters on disconnect.

---

## WS message type reference

| Direction | Type | Payload | Handler |
|-----------|------|---------|---------|
| S→C | `task:status` | `taskId`, `status`, `imageUid` | `ws.ts` → task store |
| S→C | `task:log` | `taskId`, `log` (string) | `ws.ts` → task store |
| S→C | `task:progress` | `taskId`, `progress` (0–1 float) | `ws.ts` → `setProgress` |
| S→C | `task:result` | `taskId`, `imageUid`, `meta` (Dict) | `ws.ts` → image/task store |
| C→S | `task:run` | `taskId`, `projectUid`, `imageUid`, `module`, `task`, `params`, `poolName` | `sockets.jl` → `handle_task_run`; `poolName` (string) overrides the task-spec `resource_pool` when non-empty — set by the pool dropdown in `TaskRunner.vue` |
| C→S | `task:cancel` | `taskId` | `sockets.jl` → `kill_task` — kills the individual task subprocess |
| C→S | `chain:run` | `projectUid`, `chain`, `imageUids` | `sockets.jl` → `handle_chain_run` — starts a chain run in a `Threads.@spawn`; does not block the WS thread |
| C→S | `chain:cancel` | `runId` | `sockets.jl` → `cancel_chain_run!(runId)` — sets the cancelled flag; nodes check it between steps (does **not** kill currently-executing subprocesses — see `docs/TODO.md` → *Set-scope / incremental node subprocesses not killed on chain cancel*) |
| S→C | `chain:run:started` | `runId`, `projectUid` | broadcast when a chain run begins |
| S→C | `chain:run:done` | `runId`, `projectUid` | broadcast when `run_chain` returns successfully |
| S→C | `chain:run:failed` | `runId`, `projectUid`, `error` | broadcast on unhandled error in `run_chain` |
| S→C | `chain:node:queued` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params` | `server.jl` event-bus subscriber → all clients; `ws.ts` → `taskStore.addFromChainEvent`. Node submitted to its pool, waiting for a worker slot (no `startedAt` yet) |
| S→C | `chain:node:running` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params` | Same path. Fired when a pool worker actually starts the job — this is when `startedAt`/elapsed begins |
| S→C | `chain:node:done` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params`, `result` | same path as above |
| S→C | `chain:node:failed` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `status` | same path; `status` is `failed`/`skipped`/`cancelled` (the frontend maps `cancelled` to a cancelled entry, not failed) |

### `task:result` — mandatory fields for filepath-producing tasks

Tasks that write an image version **must** include `valueName` and `filename` in `meta`:
```json
{ "valueName": "default", "filename": "ccidImage.ome.zarr" }
```
`ws.ts` uses these to update `img.filepaths[valueName]` in the Pinia project store **in-memory**. Without this, `valueNameSelection` widgets (e.g. in cellpose correction) show nothing — the store is only refreshed from disk when the project is reloaded. The on-disk `ccid.json` is always written by the Julia task regardless; the `task:result` is the in-session live update.

Tasks that also write OME dimensions can merge both into one return:
```julia
merge(zarr_meta, Dict{String,Any}("valueName" => value_name, "filename" => basename(zarr_out)))
```
| S→C | `napari:event` | `name`, `data` | `ws.ts` → named handlers |
| S→C | `napari:opened` | `imageUid` | `ws.ts` → `project.napariImageUid` |

---

## REST endpoint reference

| Method | Path | Handler | Purpose |
|--------|------|---------|---------|
| `GET` | `/api/chains?projectUid=X` | `api_chains_list` | List chain template names for a project |
| `GET` | `/api/chains/get?projectUid=X&name=Y` | `api_chains_get` | Fetch a chain template JSON |
| `POST` | `/api/chains/save` | `api_chains_save` | Write a chain template JSON (`{projectUid, template}`) |
| `GET` | `/api/pools` | `api_pools_list` | Return initialized scheduler worker pools as `[{name, limit}]`. Used by `TaskRunner.vue` and `ChainModule.vue` to populate pool dropdowns. Returns only pools that have been explicitly initialized from the cecelia config `pools` section. |

For the full chain of existing project/image/task REST routes see `api/src/routes.jl`.

---

## Language boundaries

```
Vue 3 + Pinia   →  UI only; no analysis logic
Julia            →  task dispatch, gating, statistics, REST/WS API, HPC
Python           →  image I/O, Napari, PyTorch, Cellpose
```

**Never add Rust or a fourth language.** The rationale:
Python owns all image I/O and ML regardless of what orchestrates it. The remaining work (gating, HMM,
spatial stats, clustering) maps 1:1 from R to Julia's scientific ecosystem (StatsBase, Distributions,
Distances). There is no systems performance argument for Rust when the computation is iterative research
workflows over scientific data structures.

(The distribution shell does not change this: Cecelia ships via a conda `constructor` installer +
browser, with the Julia server serving the built frontend — no Rust, no Electron, no bundled browser.
See `docs/SHIPPING.md`.)

---

## H5AD / LabelProps layout

```
{proj}/1/{uid}/labelProps/{valueName}.h5ad
```

| HDF5 group | Content |
|------------|---------|
| `/obs` | Per-cell DataFrame (`label`, population assignments) |
| `/var` | Channel metadata (var names = intensity column names in `X`) |
| `/X` | float32 n_cells × n_channels intensity matrix |
| `/obsm` | Named arrays: `spatial` (centroids), `X_umap`, `temporal` |
| `/obsp` | Sparse: `spatial_connectivities`, `spatial_distances` |
| `/uns` | Metadata: `intensity_measure`, `spatial_cols` |

Julia reads via HDF5.jl. Writes go through a **Python subprocess** (`python_bin_path()` running an
`anndata.write_h5ad` script) — anndata's encoding spec (categoricals, sparse CSR, encoding attrs) is
complex to reimplement and changes between versions, so we don't write H5AD from Julia directly.
(No in-process PythonCall — the package has no Python dependency; all Python is out-of-process.)

### LabelProps Julia reader (`app/src/label_props.jl`)

Fluent/pipeable lazy reader (`label_props(...) |> verb |> as_df`), mirrors Python `LabelPropsView`;
HDF5 opens inside `as_df` (in a `do` block) — no leaked handles. **This `LabelProps` chain is the
standard idiom for all cell-level data access** — see `docs/DATAMODEL.md` for the verbs and rules.

Intensity columns in `X`: `{measure}_intensity_{i}` where `i` is channel index and `measure` comes
from `uns.intensity_measure` (default `"mean"`).

---

## Gating engine (planned — replaces flowWorkspace/cytolib)

Lives in Julia, operates on H5AD files written by Python tasks.

- **Gate types**: rectangle, polygon, ellipse, quadrant, boolean combination
- **Population hierarchy**: tree; each node is a named population with a boolean mask over its parent
- **Gate coordinates**: always in transformed space (logicle/biex first, gates drawn on top — same convention as FlowJo)
- **Storage**: per-segmentation sidecar `gating/{value_name}.json` (NOT `ccid.json`); population tree = `{name, gate, children: [...]}`
- **Napari integration**: *linked brushing*, not gate-drawing — napari draws an image region → bridge POSTs the inside cells' label IDs to `/api/napari/event` → Julia mirrors them as a transient population and broadcasts `gating:popmap` (see flow above + `docs/POPULATION.md`)
- **logicle transform**: port Logicle.cpp (Parks et al. 2006); Python implementations available as reference

---

## OME-ZARR

See [`docs/NAPARI.md`](NAPARI.md) for the dual-layout detection, `create_multiscales` rules, byte-order issue, scale/unit reading, and dask vs zarr loading.
