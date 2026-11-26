# Coherence gaps

> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.

Places where the code does not line up with itself. Static text matching, so an item can be a false alarm (a name built at runtime). Close one by fixing the code — or, for a false alarm, by making the name visible (a literal) — then regenerate.

| Section | Kind | Count |
|---|---|---|
| Broken ends | [Julia broadcasts a WebSocket type nothing in the browser handles](#ws-unhandled) | 5 |
| Broken ends | [An `/api` route no Vue, MCP or Python code calls](#route-uncalled) | 6 |
| Loose contracts | [A data file two languages read, named by no test](#shared-file-unguarded) | 1 |
| Tangles | [Two Julia areas that each call into the other](#julia-two-way) | 36 |
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

## Tangles

<a id="julia-two-way"></a>
### Two Julia areas that each call into the other

`julia-two-way` · to close: move the few names on the weaker side to the area that calls them, or to a lower area both can use. Easiest first. One module, so legal — but each pair is two areas that cannot be read apart.

- **config ↔ ai+lab log** — the weaker side is config → ai+lab log, 1 name: `active_profile_name` (14 the other way)
- **hpc ↔ ai+lab log** — the weaker side is hpc → ai+lab log, 1 name: `_last_line` (1 the other way)
- **jobs (export, import, maintenance) ↔ ai+lab log** — the weaker side is jobs (export, import, maintenance) → ai+lab log, 1 name: `active_profile_name` (2 the other way)
- **model ↔ jobs (export, import, maintenance)** — the weaker side is model → jobs (export, import, maintenance), 1 name: `store_debris_summary` (5 the other way)
- **python bridge ↔ hpc** — the weaker side is python bridge → hpc, 1 name: `_run_py_hpc` (3 the other way)
- **qc+checks ↔ ai+lab log** — the weaker side is qc+checks → ai+lab log, 1 name: `append_lab_log!` (8 the other way)
- **task framework ↔ cell data** — the weaker side is cell data → task framework, 1 name: `_task_spec` (6 the other way)
- **task framework ↔ qc+checks** — the weaker side is task framework → qc+checks, 1 name: `channel_signal_flags` (4 the other way)
- **tasks ↔ cell data** — the weaker side is cell data → tasks, 1 name: `_run_task` (29 the other way)
- **tasks ↔ gating** — the weaker side is gating → tasks, 1 name: `Track` (26 the other way)
- **tasks ↔ hpc** — the weaker side is tasks → hpc, 1 name: `hpc_task_cpus` (1 the other way)
- **cell data ↔ gating** — the weaker side is cell data → gating, 2 names: `gating_path`, `is_track_grained` (16 the other way)
- **cell data ↔ qc+checks** — the weaker side is cell data → qc+checks, 2 names: `qc_finding`, `write_qc` (2 the other way)
- **cell data ↔ tracking+spatial** — the weaker side is cell data → tracking+spatial, 2 names: `corrections_dir`, `roi_suffixes_on` (19 the other way)
- **model ↔ cell data** — the weaker side is model → cell data, 2 names: `_label_name_taken`, `free_label_name` (20 the other way)
- **model ↔ tracking+spatial** — the weaker side is model → tracking+spatial, 2 names: `load_roi_set`, `roi_suffixes_on` (16 the other way)
- **scheduler+chains ↔ ai+lab log** — the weaker side is scheduler+chains → ai+lab log, 2 names: `_parse_tkey`, `active_profile_name` (27 the other way)
- **scheduler+chains ↔ cell data** — the weaker side is scheduler+chains → cell data, 2 names: `_stale_reads_names`, `stale_outputs_for` (11 the other way)
- **scheduler+chains ↔ python bridge** — the weaker side is python bridge → scheduler+chains, 2 names: `read_local_py_usage`, `record_run_usage!` (3 the other way)
- **scheduler+chains ↔ qc+checks** — the weaker side is scheduler+chains → qc+checks, 2 names: `chain_cohort_qc!`, `write_defaulted_scale_qc!` (7 the other way)
- **scheduler+chains ↔ runner** — the weaker side is scheduler+chains → runner, 2 names: `TaskRequest`, `execute_task` (18 the other way)
- **scheduler+chains ↔ tasks** — the weaker side is scheduler+chains → tasks, 2 names: `_str_list`, `validate_params` (9 the other way)
- **scheduler+chains ↔ hpc** — the weaker side is scheduler+chains → hpc, 3 names: `_with_run_target`, `hpc_resources`, `task_runs_on_hpc` (5 the other way)
- **scheduler+chains ↔ task framework** — the weaker side is task framework → scheduler+chains, 3 names: `_fun_name_from_task`, `list_tasks`, `run_env_of` (27 the other way)
- **tasks ↔ plots+boards** — the weaker side is tasks → plots+boards, 3 names: `_obs_suffixes`, `_rm_if`, `prune_stale_hmmfit!` (10 the other way)
- **tracking+spatial ↔ plots+boards** — the weaker side is tracking+spatial → plots+boards, 3 names: `_catkey`, `_finite`, `_ordered_cats` (6 the other way)
- **model ↔ config** — the weaker side is config → model, 4 names: `img_project_dir`, `img_project_uid`, `json_native`, `read_state_json` (7 the other way)
- **plots+boards ↔ ai+lab log** — the weaker side is ai+lab log → plots+boards, 4 names: `boards_doc_path`, `plot_spec_index`, `plot_summary_data`, `read_boards_doc` (5 the other way)
- **scheduler+chains ↔ gating** — the weaker side is gating → scheduler+chains, 4 names: `drop_props_sidecar_keys!`, `props_sidecar_path`, `put_props_sidecar_entry!`, `read_props_sidecar` (11 the other way)
- **gating ↔ tracking+spatial** — the weaker side is gating → tracking+spatial, 5 names: `is_tracked`, `track_aggregate_base`, `track_aggregate_measures`, `track_name_sets`, `track_props` (6 the other way)
- **gating ↔ qc+checks** — the weaker side is gating → qc+checks, 6 names: `_cohort_outliers`, `cohort_value_names`, `qc_finding`, `qc_path`, `severity_symbol`, … (13 the other way)
- **tasks ↔ qc+checks** — the weaker side is qc+checks → tasks, 6 names: `Denoise`, `Track`, `import_source_fingerprint`, `ngff_multiscales`, `series_base`, … (18 the other way)
- **model ↔ gating** — the weaker side is model → gating, 8 names: `_clustfeatures_entry`, `_clustfeatures_part_of`, `_clustfeatures_suffixes`, `gating_history_path`, `gating_path`, … (18 the other way)
- **model ↔ scheduler+chains** — the weaker side is model → scheduler+chains, 8 names: `_history_leaves`, `_rl_str`, `_update_run_log!`, `read_run_log`, `read_run_log_archive`, … (18 the other way)
- **task framework ↔ tasks** — the weaker side is tasks → task framework, 8 names: `ParamValidationError`, `_picked_channels`, `_spec_output_value_name`, `live_outputs`, `manual_check`, … (64 the other way)
- **tasks ↔ tracking+spatial** — the weaker side is tracking+spatial → tasks, 8 names: `Track`, `_comp_col`, `roi_review_labels`, `store_written_at`, `track_displacement`, … (27 the other way)

<a id="py-cycle"></a>
### Python modules that import each other

`py-cycle` · to close: break the loop.

- **movie_io ↔ title_card** · `python/cecelia/utils/movie_io.py`, `python/cecelia/utils/title_card.py`

<a id="store-cycle"></a>
### Pinia stores that import each other

`store-cycle` · to close: break the loop.

- **animation ↔ projectMeta** · `frontend/src/stores/animation.ts`, `frontend/src/stores/projectMeta.ts`
