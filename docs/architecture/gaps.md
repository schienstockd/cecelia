# Coherence gaps

> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.

Where the code breaks the rules in [`README.md`](README.md#rules-and-how-they-are-enforced). Each count may only go down: `test_architecture_gaps.py` fails a change that adds one. Static text matching, so an item can be a false alarm (a name built at runtime) — close it by making the name a literal. After closing gaps: `pixi run arch-diagrams && pixi run arch-gaps-baseline`.

| Section | Kind | Count |
|---|---|---|
| Layers | [A Julia function called from an area below its own in the declared order](#julia-upward) | 74 |
| Layers | [The viewer engine (`lib/webgpu`, `utils/viewer`) imports an app store](#viewer-store) | 4 |
| Layers | [A frontend file calls the backend with a bare `fetch(`, not through `utils/api.ts`](#fetch-direct) | 15 |
| Tangles | [Python modules that import each other](#py-cycle) | 1 |
| Tangles | [Pinia stores that import each other](#store-cycle) | 1 |

## Layers

<a id="julia-upward"></a>
### A Julia function called from an area below its own in the declared order

`julia-upward` · to close: move it down to the lowest tier that calls it (or a shared lower area), or — if the order is wrong — change `[layers]` in `scripts/architecture/julia_areas.toml`. Most callers first: one move closes every listed call.

- **`_history_leaves`** — defined in scheduler+chains (tier 7), called from cell data (tier 3), model (tier 2), qc+checks (tier 4), run records (tier 2)
- **`active_profile_name`** — defined in ai+lab log (tier 8), called from config (tier 1), jobs (export, import, maintenance) (tier 7), scheduler+chains (tier 7), task framework (tier 5)
- **`_composite_step_names`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`_history_output`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4), run records (tier 2)
- **`_task_from_fun_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`gating_path`** — defined in gating (tier 4), called from cell data (tier 3), model (tier 2)
- **`load_pop_tree`** — defined in gating (tier 4), called from model (tier 2), run records (tier 2)
- **`roi_suffixes_on`** — defined in tracking+spatial (tier 4), called from cell data (tier 3), model (tier 2)
- **`task_params_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`TaskRequest`** — defined in runner (tier 8), called from scheduler+chains (tier 7)
- **`Track`** — defined in tasks (tier 6), called from tracking+spatial (tier 4)
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

<a id="fetch-direct"></a>
### A frontend file calls the backend with a bare `fetch(`, not through `utils/api.ts`

`fetch-direct` · to close: use `getJson` / `postJson` / `getBuffer` / `tryApi` from `utils/api.ts` — keep the call site's behaviour, pass `quiet` for statuses it handles. A non-backend URL (`blob:`, external) says so with `// ARCH-EXEMPT: <reason>` on its line.

- **`stores/analysisLayout.ts`** — 2 bare `fetch(` calls · `frontend/src/stores/analysisLayout.ts:122`, `frontend/src/stores/analysisLayout.ts:145`
- **`stores/animation.ts`** — 1 bare `fetch(` call · `frontend/src/stores/animation.ts:42`
- **`stores/appControl.ts`** — 6 bare `fetch(` calls · `frontend/src/stores/appControl.ts:80`, `frontend/src/stores/appControl.ts:89`, `frontend/src/stores/appControl.ts:97`, `frontend/src/stores/appControl.ts:218` +2
- **`stores/canvasPanels.ts`** — 1 bare `fetch(` call · `frontend/src/stores/canvasPanels.ts:101`
- **`stores/customModules.ts`** — 1 bare `fetch(` call · `frontend/src/stores/customModules.ts:98`
- **`stores/gating.ts`** — 6 bare `fetch(` calls · `frontend/src/stores/gating.ts:253`, `frontend/src/stores/gating.ts:272`, `frontend/src/stores/gating.ts:315`, `frontend/src/stores/gating.ts:346` +2
- **`stores/labCapture.ts`** — 1 bare `fetch(` call · `frontend/src/stores/labCapture.ts:33`
- **`stores/log.ts`** — 1 bare `fetch(` call · `frontend/src/stores/log.ts:127`
- **`stores/plotRegistry.ts`** — 1 bare `fetch(` call · `frontend/src/stores/plotRegistry.ts:77`
- **`stores/project.ts`** — 2 bare `fetch(` calls · `frontend/src/stores/project.ts:318`, `frontend/src/stores/project.ts:331`
- **`stores/projectMeta.ts`** — 7 bare `fetch(` calls · `frontend/src/stores/projectMeta.ts:51`, `frontend/src/stores/projectMeta.ts:65`, `frontend/src/stores/projectMeta.ts:124`, `frontend/src/stores/projectMeta.ts:161` +3
- **`stores/staleOutputs.ts`** — 1 bare `fetch(` call · `frontend/src/stores/staleOutputs.ts:21`
- **`stores/taskDefs.ts`** — 1 bare `fetch(` call · `frontend/src/stores/taskDefs.ts:32`
- **`stores/viewProfiles.ts`** — 3 bare `fetch(` calls · `frontend/src/stores/viewProfiles.ts:48`, `frontend/src/stores/viewProfiles.ts:77`, `frontend/src/stores/viewProfiles.ts:94`
- **`stores/ws.ts`** — 3 bare `fetch(` calls · `frontend/src/stores/ws.ts:71`, `frontend/src/stores/ws.ts:704`, `frontend/src/stores/ws.ts:759`

## Tangles

<a id="py-cycle"></a>
### Python modules that import each other

`py-cycle` · to close: break the loop.

- **movie_io ↔ title_card** · `python/cecelia/utils/movie_io.py`, `python/cecelia/utils/title_card.py`

<a id="store-cycle"></a>
### Pinia stores that import each other

`store-cycle` · to close: break the loop.

- **animation ↔ projectMeta** · `frontend/src/stores/animation.ts`, `frontend/src/stores/projectMeta.ts`
