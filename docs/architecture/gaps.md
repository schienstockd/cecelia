# Coherence gaps

> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.

Where the code breaks the rules in [`README.md`](README.md#rules-and-how-they-are-enforced). Each count may only go down: `test_architecture_gaps.py` fails a change that adds one. Static text matching, so an item can be a false alarm (a name built at runtime) — close it by making the name a literal. After closing gaps: `pixi run arch-diagrams && pixi run arch-gaps-baseline`.

| Section | Kind | Count |
|---|---|---|
| Broken ends | [Julia broadcasts a WebSocket type nothing in the browser handles](#ws-unhandled) | 5 |
| Broken ends | [An `/api` route no Vue, MCP or Python code calls](#route-uncalled) | 6 |
| Loose contracts | [A data file two languages read, named by no test](#shared-file-unguarded) | 1 |
| Layers | [A Julia function called from an area below its own in the declared order](#julia-upward) | 76 |
| Layers | [The viewer engine (`lib/webgpu`, `utils/viewer`) imports an app store](#viewer-store) | 4 |
| Tangles | [Python modules that import each other](#py-cycle) | 1 |
| Tangles | [Pinia stores that import each other](#store-cycle) | 1 |

## Broken ends

<a id="ws-unhandled"></a>
### Julia broadcasts a WebSocket type nothing in the browser handles

`ws-unhandled` · to close: handle it in the frontend, or stop sending it.

- **`chain:run:done`** · `api/src/sockets.jl`, `app/src/runner/server.jl`
- **`chain:run:started`** · `api/src/sockets.jl`
- **`runner:log`** · `app/src/runner/server.jl`
- **`update:revert-staged`** · `api/src/update_api.jl`
- **`update:staged`** · `api/src/update_api.jl`

<a id="route-uncalled"></a>
### An `/api` route no Vue, MCP or Python code calls

`route-uncalled` · to close: delete the route, or name the caller (a route built from pieces is invisible here).

- **`/api/gating/density`** · `api/src/gating_api.jl`
- **`/api/kiwi/refs/cells`** · `api/src/kiwi_refs.jl`
- **`/api/projects/list`** · `api/src/routes/project.jl`
- **`/api/system/weights/fetch`** · `api/src/system_api.jl`
- **`/api/system/weights`** · `api/src/system_api.jl`
- **`/api/viewer/record-test`** · `api/src/viewer_api.jl`

## Loose contracts

<a id="shared-file-unguarded"></a>
### A data file two languages read, named by no test

`shared-file-unguarded` · to close: add a parity or schema test that names the file.

- **`figureStyle.json`** — read by Julia, Python, TS/Vue · `frontend/src/lib/figureStyle.json`

## Layers

<a id="julia-upward"></a>
### A Julia function called from an area below its own in the declared order

`julia-upward` · to close: move it down to the lowest tier that calls it (or a shared lower area), or — if the order is wrong — change `[layers]` in `scripts/architecture/julia_areas.toml`. Most callers first: one move closes every listed call.

- **56 `CciaTask` subtypes** — defined in tasks (tier 6), named from task framework (tier 5) — a list of every subtype; deriving it (`subtypes`, self-registration) closes it
- **`_history_leaves`** — defined in scheduler+chains (tier 7), called from cell data (tier 3), model (tier 2), qc+checks (tier 4), run records (tier 2)
- **`Track`** — defined in tasks (tier 6), called from gating (tier 4), qc+checks (tier 4), tracking+spatial (tier 4)
- **`active_profile_name`** — defined in ai+lab log (tier 8), called from config (tier 1), jobs (export, import, maintenance) (tier 7), scheduler+chains (tier 7)
- **`Denoise`** — defined in tasks (tier 6), called from qc+checks (tier 4), task framework (tier 5)
- **`_composite_step_names`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`_history_output`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4), run records (tier 2)
- **`_task_from_fun_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`gating_path`** — defined in gating (tier 4), called from cell data (tier 3), model (tier 2)
- **`load_pop_tree`** — defined in gating (tier 4), called from model (tier 2), run records (tier 2)
- **`roi_suffixes_on`** — defined in tracking+spatial (tier 4), called from cell data (tier 3), model (tier 2)
- **`task_params_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`TaskRequest`** — defined in runner (tier 8), called from scheduler+chains (tier 7)
- **`_catkey`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_clustfeatures_entry`** — defined in gating (tier 4), called from model (tier 2)
- **`_clustfeatures_part_of`** — defined in gating (tier 4), called from model (tier 2)
- **`_clustfeatures_suffixes`** — defined in gating (tier 4), called from model (tier 2)
- **`_comp_col`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`_finite`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_fun_name_from_task`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`_keyed_strings`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_label_name_taken`** — defined in cell data (tier 3), called from model (tier 2)
- **`_obs_suffixes`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`_ordered_cats`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_parse_tkey`** — defined in ai+lab log (tier 8), called from scheduler+chains (tier 7)
- **`_preflight_fields`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_preflight_has`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_rm_if`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`_run_py_hpc`** — defined in hpc (tier 7), called from python bridge (tier 3)
- **`_run_tar`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_stored_key`** — defined in task framework (tier 5), called from qc+checks (tier 4)
- **`_tar_available`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_tar_unpack_cmd`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_task_spec`** — defined in task framework (tier 5), called from cell data (tier 3)
- **`append_lab_log!`** — defined in ai+lab log (tier 8), called from qc+checks (tier 4)
- **`corrections_dir`** — defined in tracking+spatial (tier 4), called from cell data (tier 3)
- **`default_export_dir`** — defined in jobs (export, import, maintenance) (tier 7), called from tasks (tier 6)
- **`execute_task`** — defined in runner (tier 8), called from scheduler+chains (tier 7)
- **`free_label_name`** — defined in cell data (tier 3), called from model (tier 2)
- **`gated_pop_refs`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`gating_history_path`** — defined in gating (tier 4), called from model (tier 2)
- **`hpc_task_cpus`** — defined in hpc (tier 7), called from tasks (tier 6)
- **`img_project_dir`** — defined in model (tier 2), called from config (tier 1)
- **`img_project_uid`** — defined in model (tier 2), called from config (tier 1)
- **`import_source_fingerprint`** — defined in tasks (tier 6), called from qc+checks (tier 4)
- **`is_bookkeeping_param`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4)
- **`is_gating_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`is_track_grained`** — defined in gating (tier 4), called from cell data (tier 3)
- **`json_native`** — defined in model (tier 2), called from config (tier 1)
- **`list_tasks`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`load_roi_set`** — defined in tracking+spatial (tier 4), called from model (tier 2)
- **`ngff_multiscales`** — defined in tasks (tier 6), called from qc+checks (tier 4)
- **`plugin_roots`** — defined in task framework (tier 5), called from python bridge (tier 3)
- **`pool_status`** — defined in scheduler+chains (tier 7), called from tasks (tier 6)
- **`pop_gate_hashes`** — defined in gating (tier 4), called from run records (tier 2)
- **`prune_stale_hmmfit!`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`qc_finding`** — defined in qc+checks (tier 4), called from cell data (tier 3)
- **`read_state_json`** — defined in model (tier 2), called from config (tier 1)
- **`rename_label_set_in_params`** — defined in scheduler+chains (tier 7), called from model (tier 2)
- **`resolve_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`roi_review_labels`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`save_pop_tree!`** — defined in gating (tier 4), called from model (tier 2)
- **`series_base`** — defined in tasks (tier 6), called from qc+checks (tier 4)
- **`store_axis_sizes`** — defined in tasks (tier 6), called from task framework (tier 5)
- **`store_debris_summary`** — defined in jobs (export, import, maintenance) (tier 7), called from model (tier 2)
- **`store_rev`** — defined in tasks (tier 6), called from task framework (tier 5)
- **`store_written_at`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`task_source_stamp`** — defined in task framework (tier 5), called from run records (tier 2)
- **`task_writes_into_key`** — defined in task framework (tier 5), called from run records (tier 2)
- **`track_displacement`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`track_length`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`track_speed`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`track_straightness`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
- **`with_pop_tree_lock`** — defined in gating (tier 4), called from model (tier 2)
- **`write_qc`** — defined in qc+checks (tier 4), called from cell data (tier 3)
- **`zarr_array_meta`** — defined in tasks (tier 6), called from qc+checks (tier 4)

<a id="viewer-store"></a>
### The viewer engine (`lib/webgpu`, `utils/viewer`) imports an app store

`viewer-store` · to close: pass what it needs in as an input instead (`lib/webgpu` already holds this rule). The viewer's own `viewer` store and `log` are allowed.

- **`linkedSelection`** — imported by 1 viewer file · `frontend/src/utils/viewer/showTracksInViewer.ts`
- **`projectMeta`** — imported by 1 viewer file · `frontend/src/utils/viewer/showTracksInViewer.ts`
- **`project`** — imported by 1 viewer file · `frontend/src/utils/viewer/showTracksInViewer.ts`
- **`settings`** — imported by 3 viewer files · `frontend/src/utils/viewer/drawSpec.ts`, `frontend/src/utils/viewer/showTracksInViewer.ts`, `frontend/src/utils/viewer/trackSelectionFromViewer.ts`

## Tangles

<a id="py-cycle"></a>
### Python modules that import each other

`py-cycle` · to close: break the loop.

- **movie_io ↔ title_card** · `python/cecelia/utils/movie_io.py`, `python/cecelia/utils/title_card.py`

<a id="store-cycle"></a>
### Pinia stores that import each other

`store-cycle` · to close: break the loop.

- **animation ↔ projectMeta** · `frontend/src/stores/animation.ts`, `frontend/src/stores/projectMeta.ts`
