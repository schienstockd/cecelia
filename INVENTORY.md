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
- **Task base + registry**: `tasks/task.jl` (`CciaTask`, JSON spec load, `validate_params`, `CompositeTask`, plus the runtime `register_task!` custom registry consulted by `_task_from_fun_name`/`_spec_path`), `tasks/task_registry.jl` (built-in `_spec_path` + `_fun_name_map` + composites). Every task = co-located `.jl` + `.json`, same base name.
- **Custom (user drop-in) modules**: `tasks/custom_modules.jl` — `load_custom_modules!` scans `<config_dir>/modules/sources/**/*.jl` and `include`s each (self-registers via `register_task!`); specs merged in `api_task_definitions`; `run_py` runs their `modules/python/` scripts. Loaded on server start; `GET`/`POST /api/tasks/custom-modules[/reload]`. Guide: `docs/CUSTOM_MODULES.md`; example: `docs/examples/custom-modules/`.
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
- **Domain stores** (`stores/`): `gating` (pop/gating, pop_type-agnostic), `project`/`projectMeta`, `settings`, `tasks`, `taskDefs`, `customModules` (user drop-in modules: status/reload + category list).
- **Custom modules (frontend)**: `modules/CustomModule.vue` — the generic page for a new custom category (route `/custom/:category`), ModuleLayout + TaskRunner; AppSidebar renders a dynamic "Custom" group and SettingsModule a "Custom modules" panel (dir + loaded/error list + Reload), both off `stores/customModules`. See `docs/CUSTOM_MODULES.md`.
- **ImageTable** (`components/ImageTable.vue`): THE image-selection/listing table. Mounted only via **ModuleLayout** (`components/ModuleLayout.vue`, the standard module-page left column).
- **Dialog/panel shells**: `BaseModal.vue` (centred dialog), `TeleportPopover.vue` (body-teleported popover, 7 call sites). Two floating-box abstractions that are **deliberately separate** (different coordinate system + event model + features, not a duplicate): `FloatingPanel.vue` = top-level viewport window (fixed, pointer-drag, resize/collapse/persist); `useFloatingPanel.ts` = zoomable-canvas panel behaviour (absolute, mouse-drag+zoom, tile/cascade arrange).
- **Service control**: `utils/serviceApi.ts` — the one home for napari-bridge + notebooks service endpoints (`napariApi.restart/close`, `notebooksApi.launch/restart/shutdown`); used by both SettingsModule and NotebooksModule. (App lifecycle — quit/update — stays in `appControl`.)
- **Confirm buttons**: `ConfirmButton.vue` (arm→confirm, replaces `window.confirm`), `ConfirmDeleteButton.vue` (destructive-delete icon).
- **Canvas / plots**: `canvas/CanvasPanel.vue`, `canvas/SummaryCanvas.vue`, `canvas/SummaryPanel.vue`/`InteractivePanel.vue`, `canvas/TabbedCanvas.vue`, `canvas/PopulationPanelShell.vue`; composables `useCanvasPanels`/`useCanvasWorkspace`/`useCanvasZoom`; the plot layer `plots/{plot,export,pdf,overlays,types}.ts`.
- **Plot export (one module)**: `plots/export.ts` — the single export helper. Download plumbing (`downloadDataUrl`/`downloadBlob`/`downloadText`), raster capture (`svgToImageURL`/`plotHostToImageURL`/`rasterPlotToImageURL`), tidy CSV (`rowsToCsv`), the **true-vector SVG builders** (`svgDoc`/`svgCircles`/`svgPolygon`/`svgPath`/`svgRect`/`svgLine`/`svgText`/`svgImage`/`svgEsc`), and **`nestSvg`** (stitch one plot SVG into a board slot). Dot-plot SVG emitters that USE them: `UmapView.exportSvg`, `PlotLayers.exportSvgContent`+`GateOverlay.exportSvgContent`→`GateScatterCell.exportSvg`, stitched by `GateMontage.exportSvg`. Tests: `plots/export.test.ts`. Don't hand-roll a second SVG/CSV serialiser — extend this.
- **Board figure export**: `plots/pdf.ts` `layoutPages(pages)` = the shared A4 page geometry (ONE source), consumed by BOTH `exportTabsToPdf` (raster PDF, `pdf-lib`) and `plots/boardSvg.ts` `buildBoardSvgs` (vector board SVG). Panels expose `exportSvg()` (full light `<svg>`) — `SummaryPanel`/`ClusterHeatmapPanel` (decode `PlotChart.toImageURL('svg')`), `InteractivePanel`→view (`UmapView`/`GatingStrategyView`); `LayoutCanvas.capturePage(vector)` collects them, `TabbedCanvas` drives the PDF/SVG dropdown. Tests: `plots/boardSvg.test.ts`.
- **Napari open**: `composables/useNapariOpen.ts` (the one open/reload-in-napari path); overlay utils `utils/{napariOverlays,overlayLayers,napariColormap}.ts`.
- **View state**: `composables/useViewState.ts` — persisted per-page/per-panel options (mandated by the "persist every user-settable option" rule).
- **Background task-completion watch**: `composables/useTaskCompletionWatch.ts` — THE shared backbone for "run a debounced callback when a task/chain node finishes, for the app lifetime" (subscribe-once + filter terminal frames + debounce + `enabled()` gate; policy stays with the caller). Install from an always-mounted place (App.vue), never a `v-if`'d component. First consumer: the observer "Watch" (`stores/observer.ts`). Don't hand-roll a second `ws.on('task:status', …)` background listener — reuse this.
- **Observer (in-app AI)**: `stores/observer.ts` owns observer state (`available`/`models`/`session`/`busy`) + `runPass()`/`clear`/`refresh`. Claude is **on-demand only** — the "Ask Claude" button runs one pass (its verdict lands in the "Claude activity" log, tagged "Ask"); there is NO auto-firing Watch (removed — deterministic reporting is Cecelia's job, not Claude's). **Chat to Claude** button hands off to a full external MCP session by copying a starter prompt (`lib/chatHandoff.ts`, tested) to the clipboard. `LabLogPanel.vue` drives it + renders the per-pass activity log; App.vue refreshes on project change. Setup-hint logic (`isAuthError`/`observerSetupReason`) in `utils/observerSetup.ts` (tested). Backed by `/api/observer/*`. No MCP registration needed in-app (config generated per run); see `docs/ai-assist/OBSERVER-SETUP.md`.
- **Task-completion watch (shared backbone)**: `composables/useTaskCompletionWatch.ts` + the `isObserverTrigger`/`OBSERVER_AUTO_FRAME_TYPES` predicate in `utils/observerAuto.ts` (tested) — "run a debounced callback when a task/chain node finishes." Consumed by `stores/labCapture.ts` (Cecelia auto-summaries).
- **Cecelia auto-summaries**: `stores/labCapture.ts` owns `capture()` (POST `/api/lablog/capture` → a `[Cecelia]` digest) + `installAutoCapture()` (fires it on task/chain completion, app-lifetime install from App.vue) + `captureTick` (open panel reloads). Actionable digests (⚠️/❌) light the sidebar badge (`pi-bell`, coloured by `settings.labLogUnseenLevel`) when the panel is closed; parsed by `lib/labDigest.ts` (`worstDigestLevel`/`firstActionableLine`, tested). The badge model (`labLogUnseen`/`Kind`/`Level`) is shared with Claude notes (`pi-sparkles`).
- **Shared lib**: `lib/{imageMetadataWarnings,qc,severity,valueNames}.ts`; pure helpers in `utils/*.ts` (`imageTable.ts`, `boardAssets.ts`, …). `imageMetadataWarnings.ts` (`metadataWarning`/`flaggedFields`) now READS calibration findings from `img.qc` (`metadata.*` codes) — the source is `qc.jl metadata_qc_findings`, NOT frontend re-derivation; `qc.ts qcSummary` excludes `metadata.*` (via `isMetadataCode`) so there's ONE indicator per concern.
- **Severity / traffic-light (colour-blind-safe)**: `lib/severity.ts` — THE canonical ok/warn/fail model (`SEVERITY` icon+colour+emoji+label, `worstSeverity`, `severityFor`). Hues are `--cc-sev-*` (validated status palette); colour is NEVER the sole cue — always icon+label. Julia lab-log counterpart: `qc.jl` `severity_symbol`/`SEVERITY_SYMBOLS` (✅/⚠️/❌). Don't hand-pick green/amber/red or a coloured dot — import this. See `docs/todo/QC_OBSERVER_PLAN.md`.
- **Toast (transient feedback)**: PrimeVue `<Toast />` mounted once in `App.vue` (`ToastService` in `main.ts`); `useToast()` anywhere. ONE notification system — don't add a second. Foreground-op feedback only (not background progress, not per-entry badges). Severity = the traffic-light scale. Convention + the four surfaces (toast/badge/lab-log/traffic-light) in `docs/UI.md` → *Toast notifications*.
- **Cohort consistency check**: `components/CohortCheckButton.vue` (in the ModuleLayout action bar; self-hides unless the module banks cohort metrics) → `lib/cohortStages.ts` (module→fun_names, mirrors `COHORT_METRICS`) + `lib/cohortCheck.ts` (`runCohortCheck` POSTs `/api/qc/cohort/check` per fun; pure `summariseCohortResult` for the toast). Persists per-image findings + a `[Cecelia]` lab-log summary server-side.

## Napari bridge (`napari/`)

- **napari_bridge.py**: the whole bridge process — `NapariState` (viewer + per-image state + all command handlers), `execute_command` dispatcher, WS server (`ws_server`, :7655, `_port_available` guard), Qt/asyncio glue. Imports `cecelia.utils.napari_utils`/`zarr_utils` (+ lazy `ome_xml_utils`, `label_props_utils`). No private store readers (consolidated in PR #170).

## MCP observer (`mcp/`)

- **cecelia_mcp/server.py**: FastMCP stdio server `cecelia-observer` — 13 read tools (incl. `get_recent_logs` = backend console, `get_cohort_qc` = per-set outliers via `/api/qc/cohort`, `get_repl_api` = the notebook/REPL data-access surface, `get_module_params` = task param specs, and `get_session_briefing` = chat startup context) + `poll_observations`/`set_observer_active`/`get_observer_stats` + 1 append tool, each delegating to the client/monitor.
- **REPL / notebook data-access surface**: `app/src/ai/repl_api.jl` — the ONE curated `NOTEBOOK_API` allow-list of notebook-safe read accessors + live docstring introspection (`repl_api_reference`). Generates the `docs/REPL.md` API section (`write_repl_doc`), golden-tested so it can't drift from the code. Served via `GET /api/repl/api` → the `get_repl_api` MCP tool. When adding a notebook-facing accessor, add it here (and give it a docstring — the test enforces it).
- **Session briefing / image QC severity**: `app/src/ai/briefing.jl` (`session_briefing`) — the compact chat-startup context (name/count + flagged images + recent lab log), served via `GET /api/observer/briefing` → `get_session_briefing`. Its flagged-images source is `all_qc_docs(img)` in `qc.jl` — the ONE canonical merge of persisted QC sidecars + the computed calibration fallback, shared with the API image payload (`_image_qc_payload`) so the table indicator and the briefing never diverge. Don't re-merge those two elsewhere.
- **cecelia_mcp/client.py**: `CeceliaClient` (stdlib urllib to the Julia API) with `ALLOWED_ROUTES` allow-list — the read-only guarantee. Never opens images/h5ad itself; talks HTTP to :8080 only.
- **cecelia_mcp/monitor.py**: `SessionMonitor` + `normalize_frame` — pure, thread-safe session-state for the 10-attempts pattern (counts terminal task outcomes per `(image_uid, fn)` off the WS stream) + note/lab-log events, plus the per-session throttle (`surfaceCap` → silent lab-log flush via `drain_for_log`), token-cost estimate (`stats`), and off switch (`set_enabled`). No I/O; unit-tested. `poll` drains observations.
- **cecelia_mcp/wsclient.py**: thin WS listener (`observe`/`start_listener`) feeding the monitor from `ws://…/ws`; best-effort, reconnects, receive-only. Backend events it consumes: `chain:node:*`, `task:status` (now carries `fun`), `image_note_added`, `lab_log_entry_added`.
