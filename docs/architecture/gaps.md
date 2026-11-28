# Coherence gaps

> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.

Where the code breaks the rules in [`README.md`](README.md#rules-and-how-they-are-enforced). Each count may only go down: `test_architecture_gaps.py` fails a change that adds one. Static text matching, so an item can be a false alarm (a name built at runtime) — close it by making the name a literal. After closing gaps: `pixi run arch-diagrams && pixi run arch-gaps-baseline`.

| Section | Kind | Count |
|---|---|---|
| Layers | [A Julia function called from an area below its own in the declared order](#julia-upward) | 55 |

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
- **`_clustfeatures_entry`** — defined in gating (tier 4), called from model (tier 2)
- **`_clustfeatures_part_of`** — defined in gating (tier 4), called from model (tier 2)
- **`_clustfeatures_suffixes`** — defined in gating (tier 4), called from model (tier 2)
- **`_finite`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_fun_name_from_task`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`_keyed_strings`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_label_name_taken`** — defined in cell data (tier 3), called from model (tier 2)
- **`_parse_tkey`** — defined in ai+lab log (tier 8), called from scheduler+chains (tier 7)
- **`_preflight_fields`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_preflight_has`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
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
- **`is_bookkeeping_param`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4)
- **`is_gating_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`is_track_grained`** — defined in gating (tier 4), called from cell data (tier 3)
- **`json_native`** — defined in model (tier 2), called from config (tier 1)
- **`list_tasks`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`load_roi_set`** — defined in tracking+spatial (tier 4), called from model (tier 2)
- **`plugin_roots`** — defined in task framework (tier 5), called from python bridge (tier 3)
- **`pool_status`** — defined in scheduler+chains (tier 7), called from tasks (tier 6)
- **`pop_gate_hashes`** — defined in gating (tier 4), called from run records (tier 2)
- **`qc_finding`** — defined in qc+checks (tier 4), called from cell data (tier 3)
- **`read_state_json`** — defined in model (tier 2), called from config (tier 1)
- **`rename_label_set_in_params`** — defined in scheduler+chains (tier 7), called from model (tier 2)
- **`resolve_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`save_pop_tree!`** — defined in gating (tier 4), called from model (tier 2)
- **`store_debris_summary`** — defined in jobs (export, import, maintenance) (tier 7), called from model (tier 2)
- **`task_source_stamp`** — defined in task framework (tier 5), called from run records (tier 2)
- **`task_writes_into_key`** — defined in task framework (tier 5), called from run records (tier 2)
- **`with_pop_tree_lock`** — defined in gating (tier 4), called from model (tier 2)
- **`write_qc`** — defined in qc+checks (tier 4), called from cell data (tier 3)
