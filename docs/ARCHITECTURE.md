# Cecelia Pineapple — Architecture Reference

---

## Layer boundary (Package / API / GUI)

The hard rule: **Cecelia.jl must be usable headless from the Julia REPL with zero knowledge of HTTP, WebSockets, or Vue.**

```
cecelia-pineapple/
  app/          Julia package — Cecelia.jl
  api/          Julia API server — depends on Cecelia.jl as Pkg dependency
  frontend/     Vue 3 — unchanged
```

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
| C→S | `chain:cancel` | `runId` | `sockets.jl` → `cancel_chain_run!(runId)` — sets the cancelled flag; nodes check it between steps (does **not** kill currently-executing subprocesses — see TODO #00016) |
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

Julia reads via HDF5.jl. Writes go through PythonCall.jl calling `anndata.write_h5ad` — anndata's
encoding spec (categoricals, sparse CSR, encoding attrs) is complex to reimplement and changes between
versions, so we don't write H5AD from Julia directly.

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
