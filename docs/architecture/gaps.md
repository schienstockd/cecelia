# Coherence gaps

> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.

Where the code breaks the rules in [`README.md`](README.md#rules-and-how-they-are-enforced). Each count may only go down: `test_architecture_gaps.py` fails a change that adds one. Static text matching, so an item can be a false alarm (a name built at runtime) — close it by making the name a literal. After closing gaps: `pixi run arch-diagrams && pixi run arch-gaps-baseline`.

| Section | Kind | Count |
|---|---|---|
| Layers | [A Julia function called from an area below its own in the declared order](#julia-upward) | 44 |

## Layers

<a id="julia-upward"></a>
### A Julia function called from an area below its own in the declared order

`julia-upward` · to close: move it down to the lowest tier that calls it (or a shared lower area), or — if the order is wrong — change `[layers]` in `scripts/architecture/julia_areas.toml`. Most callers first: one move closes every listed call.

- **`active_profile_name`** — defined in ai+lab log (tier 8), called from config (tier 1), jobs (export, import, maintenance) (tier 7), model (tier 2), scheduler+chains (tier 7), task framework (tier 5)
- **`_history_leaves`** — defined in scheduler+chains (tier 7), called from cell data (tier 3), qc+checks (tier 4), run records (tier 2)
- **`_composite_step_names`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`_history_output`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4), run records (tier 2)
- **`_task_from_fun_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`task_params_name`** — defined in task framework (tier 5), called from qc+checks (tier 4), run records (tier 2)
- **`TaskRequest`** — defined in runner (tier 8), called from scheduler+chains (tier 7)
- **`_catkey`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_finite`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_fun_name_from_task`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`_keyed_strings`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_obs_suffixes`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`_ordered_cats`** — defined in plots+boards (tier 8), called from tracking+spatial (tier 4)
- **`_preflight_fields`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_preflight_has`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`_rm_if`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`_run_py_hpc`** — defined in hpc (tier 7), called from python bridge (tier 3)
- **`_run_tar`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_stored_key`** — defined in task framework (tier 5), called from qc+checks (tier 4)
- **`_tar_available`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_tar_unpack_cmd`** — defined in jobs (export, import, maintenance) (tier 7), called from task framework (tier 5)
- **`_task_spec`** — defined in task framework (tier 5), called from cell data (tier 3)
- **`corrections_dir`** — defined in tracking+spatial (tier 4), called from cell data (tier 3)
- **`default_export_dir`** — defined in jobs (export, import, maintenance) (tier 7), called from tasks (tier 6)
- **`execute_task`** — defined in runner (tier 8), called from scheduler+chains (tier 7)
- **`gated_pop_refs`** — defined in scheduler+chains (tier 7), called from run records (tier 2)
- **`gating_path`** — defined in gating (tier 4), called from cell data (tier 3)
- **`hpc_task_cpus`** — defined in hpc (tier 7), called from tasks (tier 6)
- **`is_bookkeeping_param`** — defined in scheduler+chains (tier 7), called from qc+checks (tier 4)
- **`is_gating_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`is_track_grained`** — defined in gating (tier 4), called from cell data (tier 3)
- **`list_tasks`** — defined in scheduler+chains (tier 7), called from task framework (tier 5)
- **`load_pop_tree`** — defined in gating (tier 4), called from run records (tier 2)
- **`plugin_roots`** — defined in task framework (tier 5), called from python bridge (tier 3)
- **`pool_status`** — defined in scheduler+chains (tier 7), called from tasks (tier 6)
- **`pop_gate_hashes`** — defined in gating (tier 4), called from run records (tier 2)
- **`prune_stale_hmmfit!`** — defined in plots+boards (tier 8), called from tasks (tier 6)
- **`qc_finding`** — defined in qc+checks (tier 4), called from cell data (tier 3)
- **`resolve_pop_type`** — defined in gating (tier 4), called from run records (tier 2)
- **`roi_suffixes_on`** — defined in tracking+spatial (tier 4), called from cell data (tier 3)
- **`store_debris_summary`** — defined in jobs (export, import, maintenance) (tier 7), called from model (tier 2)
- **`task_source_stamp`** — defined in task framework (tier 5), called from run records (tier 2)
- **`task_writes_into_key`** — defined in task framework (tier 5), called from run records (tier 2)
- **`write_qc`** — defined in qc+checks (tier 4), called from cell data (tier 3)
