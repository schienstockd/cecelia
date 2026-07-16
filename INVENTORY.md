# Inventory

Living index of the key existing components — the things a new task should **find and reuse**, not
rebuild. When you add a significant new component (a shared util, a canonical helper, a reusable
Vue component, a new API handler file), add a line here. When a task touches something in this list,
open it and use it.

This is the *what exists and where* companion to `CLAUDE.md` (*how to behave*) and
`docs/ARCHITECTURE.md` (*how the layers fit together*). It is intentionally not exhaustive — leaf
task files and one-off components are omitted; shared/cross-cutting things are not.

Last audited: 2026-07-16 (full six-area ground-truth read; against `main` @ c1ce165).

---

## Cross-cutting flows (one canonical path each — do not add a second)

- **App shutdown / quit** — UI: sidebar footer + Settings only → `appControl.quit()` → `POST /api/app/shutdown` → `api_app_shutdown` → `_stop_children_for_exit()`. Two UI entry points, one store action, one endpoint. Do NOT add a third quit button. (Per-service stops — napari `/api/napari/close`, notebooks `/api/notebooks/shutdown` — are separate and legitimate.)
- **App update** — `appControl.checkUpdate()`/`applyUpdate()` → `/api/update/check` + `/api/update/apply`. Header badge and Settings both read the one store; one handler each.
- **Napari WebSocket (7655)** — one connector: `app/src/napari.jl` `send(v, msg)`; one bridge server: `napari/napari_bridge.py` `ws_server()`. Port constant `NAPARI_PORT` defined once in `napari.jl`.
- **Julia ↔ Vue WebSocket (8080 `/ws`)** — one server handler: `api/src/server.jl` `handle_ws` → `sockets.jl` `handle_message`; one Vue client: `frontend/src/stores/ws.ts` (the only `new WebSocket` in the frontend). All outbound goes through `broadcast_ws`.
- **Task status/progress/log push** — task callbacks → `ws_log`/`ws_progress`/`ws_status`/`ws_result` (`api/src/sockets.jl`) → `broadcast_ws` → `frontend/src/stores/ws.ts`. One emitter family, reused by both the scheduler and the napari batch runner.
- **Image selection in the UI** — one component: `frontend/src/components/ImageTable.vue`, mounted only via `ModuleLayout.vue`. Every module/analysis page gets its picker by wrapping `ModuleLayout`.

---

## Data access (the canonical readers/writers — never hand-roll)

- **Image / OME-ZARR (Python)**: `python/cecelia/utils/zarr_utils.py` — `open_as_zarr`/`open_zarr` (image AND labels), `series_base` (structural layout detection), `read_axes`/`read_scale`/`read_multiscales_meta`, writers `create_multiscales`/`create_zarr_from_ndarray`, and `multiscales_metadata` (the shared NGFF `.zattrs` builder used by both the image writer and the label writer). All store access routes through here.
- **OME-XML (Python)**: `python/cecelia/utils/ome_xml_utils.py` — `load_ome_xml` (cached), `read_pixel_unit`/`read_scale_from_ome_xml`/`read_time_increment`, `read_imagej_metadata`; writers `save_meta_in_zarr`/`write_ome_xml`. The one TIFF/ImageJ metadata reader.
- **Cell data `.h5ad` (Python)**: `python/cecelia/utils/label_props_utils.py` — `LabelPropsView` chain (read `view_cols`/`view_centroid_cols`/`filter_by_label`/`as_df`/`obsm`; write `add_obs`/`add_categorical_obs`/`add_obsm`/`save`). The one Python existing-file h5ad open path. Only NEW-file *creation* uses `anndata` directly (measure/tracking output).
- **Cell data `.h5ad` (Julia)**: `app/src/label_props.jl` — the sole Julia HDF5 chokepoint: `label_props(...) |> select_cols/filter_rows |> as_df` read chain, `add_obs |> save!` write path, `_with_h5` lock. Categorical/string obs writes delegate to Python via `run_py` (`write_categorical_obs`).
- **Categorical/string obs (Python)**: `python/cecelia/utils/obs_utils.py` — `write_categorical_obs`, a thin driver over `LabelPropsView.add_categorical_obs` (the categorical-encoding half of the h5ad write contract; Julia writes only numeric obs).
- **GPU device (Python)**: `python/cecelia/utils/gpu_utils.py` — `torch_device()` → `(use_gpu, device)`, the one CUDA→MPS→CPU detector for torch-backed tasks (cellpose segment + denoise).
- **Population membership (Python)**: `python/cecelia/cecelia_client.py` — `CeceliaClient` (stdlib HTTP to the Julia gating API); `python/cecelia/utils/pop_utils.py` `PopUtils.pop_df` is the Python population accessor.

## Python utilities (`python/cecelia/utils/`)

- **measure_utils**: `measure_from_zarr` — per-cell morphology + intensity; writes a NEW `labelProps/{vn}.h5ad`.
- **segmentation_utils**: `SegmentationUtils` base — tiled segmentation loop, seam stitching, post-processing, nuc↔cyto matching, `_write_labels_zarr`.
- **cellpose_utils**: `CellposeUtils(SegmentationUtils)` — cellpose predict, channel prep, µm→px diameter, auto-GPU.
- **tracking_utils**: `BayesianTrackingUtils.track_objects` (btrack) + `write_track_props` (new `{vn}__tracks.h5ad`).
- **clustering_utils**: shared Leiden engine (`find_populations` + `split_back_and_write`); used by both cluster runners.
- **correction_utils**: AF + drift correction (drift shifts, AF channel correct, denoise/rolling-ball/top-hat).
- **dim_utils**: `DimUtils` — dimension-order/index/physical-size/slice reasoning off an ome-types OME object. Core dependency of most tasks.
- **slice_utils**: numpy slice-tuple generators for tiled 2D/3D(+time) processing + downsample strides.
- **napari_utils**: project-agnostic napari layer builders (`add_image`/`add_labels`/`add_tracks`), hex↔rgba, clip planes, view snapshot, movie recording, colour-by helpers. Imported by the bridge (and coastal).
- **script_utils**: subprocess plumbing — `StdoutLogger` (`[PROGRESS] n/total`), `script_params`, `get_param`.
- **math_helpers**: `round_up`/`round_down` to a multiple.
- **rechunk_zarr**: standalone CLI — rewrites flat stores to per-plane chunks (low-level surgery; reuses `plane_chunks`).

## Python tasks & writers

- **Task runners**: `python/cecelia/tasks/<category>/<name>_run.py` — thin subprocess entry points (segment, cleanupImages, tracking, editImages, importImages, clustPops, clustTracks). Reusable logic lives in `utils/`, not the runners.
- **Data-layer writers**: `python/cecelia/writers/*_run.py` — e.g. `write_categorical_obs_run` (delegates to `obs_utils`).

## Julia app (`app/src/`)

- **Object model**: `model/image.jl` (`CciaImage` + path accessors `img_filepath`/`img_label_props_path`/`img_physical_sizes`), `model/set.jl` (`CciaSet`, `init_object`), `model/project.jl` (`CciaProject`, `with_transaction`).
- **Versioned fields**: `helpers.jl` — `versioned_get`/`versioned_set!`/`versioned_keys`/`versioned_active` (the `{value_name => …, "_active" => …}` convention; the one family for both `Dict{String,String}` path dicts and `Any`/JSON3 raw dicts), plus `read_ccid_raw(path)` (the one ccid.json read+Symbol-key normalize).
- **H5AD chain**: `label_props.jl` (see Data access) + `tracking/track_props.jl` (compute-on-read per-track table, auto categorical/numeric detection).
- **Python launcher**: `py_runner.jl` — `run_py`, the single Python spawn point (`PYTHONPATH=python/`, `[PROGRESS]` streaming, exit+signal check).
- **Scheduler**: `tasks/scheduler.jl` — `ResourcePool`, `run_task`/`run_tasks`, `TaskRecord`/`cancel_task!`, per-image logfile, and the kill helpers `_kill_tree`(pid) / `_kill_proc_tree`(proc) / `_kill_listeners_on_port`(port) — the single home for process/port killing.
- **Task base + registry**: `tasks/task.jl` (`CciaTask`, JSON spec load, `validate_params`, `CompositeTask`), `tasks/task_registry.jl` (`_spec_path` + `_fun_name_map` + composites). Every task = co-located `.jl` + `.json`, same base name.
- **Chain executor + events**: `tasks/chain.jl` (`ChainTemplate`/`ChainRun`, per-image threads, barriers, resume), `events.jl` (chain event pub/sub; the API WS layer subscribes).
- **Gating engine**: `gating/population_manager.jl` (`PopulationMap`, `pop_df` unified accessor, sidecar `gating/{vn}.json`), `gating/gating_engine.jl` (`recompute!`, membership, `cells_in_pop`), `gating/gates.jl`, `gating/transforms.jl` (logicle cited), `gating/density.jl`.
- **Behaviour / plotting**: `behaviour/hmm.jl` (Gaussian HMM), `plotting/plot_data.jl` (`plot_summary_data` server-side aggregation).
- **Napari client**: `napari.jl` — `NapariViewer` package-level bridge client (WS :7655, `launch!`/`close!`/`restart!`).
- **Utils / config / logs**: `utils.jl` (`gen_uid`, `_dir_bytes` — the one directory-size helper), `config.jl` (`init_cecelia!`, `.env` reader, `bioformats2raw_bin()`, `python_bin_path()`), `qc.jl`, `run_log.jl`, `lab_log.jl`/`lab_log_context.jl`.

## Julia API (`api/src/`)

- **server.jl**: single `HTTP.listen` entry; `handle_stream` (WS upgrade + static serving), `handle_http` route table, `broadcast_ws` fan-out, `BroadcastLogger`, chain-event→WS bridge, `handle_ws` loop.
- **sockets.jl**: `handle_message` (the one inbound-WS switch), task telemetry `ws_log`/`ws_status`/`ws_result`/`ws_progress`, `handle_task_run`/`handle_chain_run`/`handle_movie_batch`.
- **routes.jl**: project/set/image + chain-template CRUD, task-definition serving, fs browser, `api_images_tasklog` (reads the task log file), lab-log routes.
- **napari_api.jl**: the global `NapariViewer` ref + `_with_viewer` single-flight, all `/api/napari/*` handlers, `run_batch_movies`.
- **gating_api.jl**: population manager + gating engine routes; **owns the shared helpers** (`_gating_image`/`_resolve_vn`/`_gerr`/`_live_map`) reused by tracking/plotting/napari (include-order coupled — `gating_api.jl` first).
- **plotting_api.jl**: summary-canvas routes (thin wrappers over `plot_summary_data`).
- **tracking_api.jl**: `/api/tracking/motion-dims` preflight.
- **notebooks_api.jl**: Pluto engine lifecycle (:7660) + registry/snapshot CRUD + sysimage build.
- **app_api.jl**: `_stop_children_for_exit`, `/api/app/shutdown|restart`, dev worktree-switch.
- **repl_api.jl / update_api.jl / setup_api.jl**: diagnostics + gated REPL; self-update; first-launch wizard.
- **task_console.jl / dev.jl**: standalone `pixi run console` WS *client*; the `pixi run dev` supervisor. (Both outside the server.)

## Frontend (`frontend/src/`)

- **appControl store** (`stores/appControl.ts`): all app-lifecycle actions — `quit`, `checkUpdate`/`applyUpdate`, dev `restartBackend`/`switchWorktree`, setup state.
- **ws store** (`stores/ws.ts`): THE WebSocket client (connect/reconnect/`send`/`on`/`off`); dispatches all `task:*`/`chain:*`/`napari:*`/`server:log` events.
- **Domain stores** (`stores/`): `gating` (pop/gating, pop_type-agnostic), `project`/`projectMeta`, `settings`, `tasks`, `taskDefs`.
- **ImageTable** (`components/ImageTable.vue`): THE image-selection/listing table. Mounted only via **ModuleLayout** (`components/ModuleLayout.vue`, the standard module-page left column).
- **Dialog/panel shells**: `BaseModal.vue` (centred dialog), `TeleportPopover.vue` (body-teleported popover, 7 call sites). Two floating-box abstractions that are **deliberately separate** (different coordinate system + event model + features, not a duplicate): `FloatingPanel.vue` = top-level viewport window (fixed, pointer-drag, resize/collapse/persist); `useFloatingPanel.ts` = zoomable-canvas panel behaviour (absolute, mouse-drag+zoom, tile/cascade arrange).
- **Service control**: `utils/serviceApi.ts` — the one home for napari-bridge + notebooks service endpoints (`napariApi.restart/close`, `notebooksApi.launch/restart/shutdown`); used by both SettingsModule and NotebooksModule. (App lifecycle — quit/update — stays in `appControl`.)
- **Confirm buttons**: `ConfirmButton.vue` (arm→confirm, replaces `window.confirm`), `ConfirmDeleteButton.vue` (destructive-delete icon).
- **Canvas / plots**: `canvas/CanvasPanel.vue`, `canvas/SummaryCanvas.vue`, `canvas/SummaryPanel.vue`/`InteractivePanel.vue`, `canvas/TabbedCanvas.vue`, `canvas/PopulationPanelShell.vue`; composables `useCanvasPanels`/`useCanvasWorkspace`/`useCanvasZoom`; the plot layer `plots/{plot,export,pdf,overlays,types}.ts`.
- **Napari open**: `composables/useNapariOpen.ts` (the one open/reload-in-napari path); overlay utils `utils/{napariOverlays,overlayLayers,napariColormap}.ts`.
- **View state**: `composables/useViewState.ts` — persisted per-page/per-panel options (mandated by the "persist every user-settable option" rule).
- **Shared lib**: `lib/{imageMetadataWarnings,qc,valueNames}.ts`; pure helpers in `utils/*.ts` (`imageTable.ts`, `boardAssets.ts`, …).

## Napari bridge (`napari/`)

- **napari_bridge.py**: the whole bridge process — `NapariState` (viewer + per-image state + all command handlers), `execute_command` dispatcher, WS server (`ws_server`, :7655, `_port_available` guard), Qt/asyncio glue. Imports `cecelia.utils.napari_utils`/`zarr_utils` (+ lazy `ome_xml_utils`, `label_props_utils`). No private store readers (consolidated in PR #170).

## MCP observer (`mcp/`)

- **cecelia_mcp/server.py**: FastMCP stdio server `cecelia-observer` — 8 read tools + 1 append tool, each delegating to the client.
- **cecelia_mcp/client.py**: `CeceliaClient` (stdlib urllib to the Julia API) with `ALLOWED_ROUTES` allow-list — the read-only guarantee. Never opens images/h5ad itself; talks HTTP to :8080 only.
