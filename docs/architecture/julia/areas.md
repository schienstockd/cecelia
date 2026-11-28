# Julia — areas and who calls whom

> **Generated** by `scripts/architecture/julia_extract.jl` — do not edit; run `pixi run arch-diagrams`.

The backend is **one module** (`Cecelia`, every file `include`d), so there is no module graph. These
areas are a curated grouping of files (`scripts/architecture/julia_areas.toml`); the arrows are from
the code: `A → B (n)` means files in A call `n` distinct functions (or name `n` types) defined in B.
Static name matching — approximate, see the script header.

**Read it as a size map.** Big areas call more, so the thickest arrows mostly show where the code
is, not where it is wrong. What to act on is the calls up the declared layer order (below, and
listed by name in [`gaps.md`](../gaps.md#julia-upward)). Diagram rule: the strongest 25 arrows per diagram
(ties at the cut-off included); every count is in the full matrix at the bottom.

## Inside the package (`app/src`)

```mermaid
flowchart LR
  model["model<br/><small>10 files · 3.7k lines</small>"]
  jobs__export__import__maintenance_["jobs (export, import, maintenance)<br/><small>5 files · 1.2k lines</small>"]
  config["config<br/><small>16 files · 3.8k lines</small>"]
  run_records["run records<br/><small>3 files · 0.8k lines</small>"]
  run_history["run history<br/><small>3 files · 1.7k lines</small>"]
  scheduler_chains["scheduler+chains<br/><small>21 files · 5.1k lines</small>"]
  task_framework["task framework<br/><small>13 files · 3.8k lines</small>"]
  tasks["tasks<br/><small>81 files · 12.6k lines</small>"]
  runner["runner<br/><small>5 files · 1.4k lines</small>"]
  cell_data["cell data<br/><small>3 files · 1.9k lines</small>"]
  gating["gating<br/><small>23 files · 6.3k lines</small>"]
  tracking_spatial["tracking+spatial<br/><small>16 files · 4.8k lines</small>"]
  qc_checks["qc+checks<br/><small>7 files · 4.0k lines</small>"]
  plots_boards["plots+boards<br/><small>12 files · 4.9k lines</small>"]
  ai_lab_log["ai+lab log<br/><small>20 files · 4.9k lines</small>"]
  package_root["package root<br/><small>1 files · 0.6k lines</small>"]
  tasks -->|70| model
  package_root -->|46| tasks
  plots_boards -->|39| model
  tasks -->|36| config
  tasks -->|35| tracking_spatial
  scheduler_chains -->|33| task_framework
  tasks -->|30| cell_data
  plots_boards -->|28| gating
  ai_lab_log -->|25| gating
  ai_lab_log -->|25| model
  tasks -->|22| gating
  tracking_spatial -->|21| cell_data
  tasks -->|19| qc_checks
  runner -->|18| scheduler_chains
  gating -->|17| model
  package_root -->|17| gating
  qc_checks -->|17| model
  ai_lab_log -->|16| scheduler_chains
  gating -->|16| cell_data
  task_framework -->|16| model
  tracking_spatial -->|16| model
  ai_lab_log -->|15| config
  cell_data -->|14| model
  jobs__export__import__maintenance_ -->|14| model
  run_history -->|14| model
  scheduler_chains -->|14| config
  scheduler_chains -->|14| model
  scheduler_chains -->|14| run_records
  tracking_spatial -->|14| gating
```

## API server → package (`api/src` → `app/src`)

The API server's own files are grouped on the left; arrows into the package only.

```mermaid
flowchart LR
  subgraph API["api/src"]
    api__server_ws["api: server+ws<br/><small>6 files · 3.1k lines</small>"]
    api__rendering["api: rendering<br/><small>16 files · 9.5k lines</small>"]
    api__claude_kiwi["api: claude+kiwi<br/><small>10 files · 4.1k lines</small>"]
    api__routes["api: routes<br/><small>45 files · 17.5k lines</small>"]
  end
  subgraph PKG["app/src — Cecelia.jl"]
    model["model<br/><small>10 files · 3.7k lines</small>"]
    jobs__export__import__maintenance_["jobs (export, import, maintenance)<br/><small>5 files · 1.2k lines</small>"]
    config["config<br/><small>16 files · 3.8k lines</small>"]
    scheduler_chains["scheduler+chains<br/><small>21 files · 5.1k lines</small>"]
    task_framework["task framework<br/><small>13 files · 3.8k lines</small>"]
    tasks["tasks<br/><small>81 files · 12.6k lines</small>"]
    runner["runner<br/><small>5 files · 1.4k lines</small>"]
    cell_data["cell data<br/><small>3 files · 1.9k lines</small>"]
    gating["gating<br/><small>23 files · 6.3k lines</small>"]
    tracking_spatial["tracking+spatial<br/><small>16 files · 4.8k lines</small>"]
    qc_checks["qc+checks<br/><small>7 files · 4.0k lines</small>"]
    plots_boards["plots+boards<br/><small>12 files · 4.9k lines</small>"]
    hpc["hpc<br/><small>9 files · 2.5k lines</small>"]
    ai_lab_log["ai+lab log<br/><small>20 files · 4.9k lines</small>"]
  end
  api__routes -->|123| gating
  api__routes -->|86| model
  api__routes -->|58| config
  api__claude_kiwi -->|47| ai_lab_log
  api__rendering -->|34| model
  api__routes -->|31| plots_boards
  api__routes -->|31| tracking_spatial
  api__routes -->|29| scheduler_chains
  api__routes -->|28| qc_checks
  api__routes -->|27| task_framework
  api__server_ws -->|25| config
  api__routes -->|22| ai_lab_log
  api__server_ws -->|20| runner
  api__rendering -->|19| gating
  api__claude_kiwi -->|17| config
  api__rendering -->|15| config
  api__routes -->|15| hpc
  api__routes -->|13| cell_data
  api__routes -->|13| jobs__export__import__maintenance_
  api__rendering -->|12| tracking_spatial
  api__server_ws -->|12| scheduler_chains
  api__rendering -->|10| cell_data
  api__routes -->|10| tasks
  api__server_ws -->|8| model
  api__claude_kiwi -->|7| model
  api__server_ws -->|7| jobs__export__import__maintenance_
```

## Two-way dependencies between package areas

Both directions present (any n). In one module these are legal, but each pair is two areas that cannot be understood apart.

| Areas | → | ← |
|---|---|---|
| model ↔ jobs (export, import, maintenance) | 1: `store_debris_summary` | 14: `_rm_tree`, `commit_state!`, `discard_later`, `img_branch_labels_dir`, … |
| model ↔ ai+lab log | 1: `active_profile_name` | 25: `CciaImage`, `DataSource`, `_bare_doi`, `attr_value_counts`, … |
| jobs (export, import, maintenance) ↔ scheduler+chains | 1: `rename_label_set_in_params` | 1: `_kill_proc_tree` |
| jobs (export, import, maintenance) ↔ task framework | 1: `_history_leaves` | 3: `_run_tar`, `_tar_available`, `_tar_unpack_cmd` |
| jobs (export, import, maintenance) ↔ ai+lab log | 1: `active_profile_name` | 2: `_kill_proc_tree`, `write_stored_zip` |
| config ↔ ai+lab log | 1: `active_profile_name` | 15: `_native`, `_pid_alive`, `_pretty_json`, `api_token_path`, … |
| run records ↔ task framework | 2: `_task_from_fun_name`, `task_params_name` | 3: `close_run_log!`, `open_run_log!`, `run_env_of` |
| run history ↔ task framework | 12: `_composite_step_names`, `_history_leaves`, `_history_output`, `_keyed_strings`, … | 1: `run_provenance` |
| scheduler+chains ↔ task framework | 33: `CompositeTask`, `ParamValidationError`, `TaskApplicabilityError`, `_clamp_to_spec`, … | 2: `_fun_name_from_task`, `list_tasks` |
| scheduler+chains ↔ tasks | 2: `_str_list`, `validate_params` | 1: `pool_status` |
| scheduler+chains ↔ runner | 2: `TaskRequest`, `execute_task` | 18: `_is_probe_code_bug`, `cancel_chain_run!`, `cancel_task!`, `chain_template_from_raw`, … |
| scheduler+chains ↔ hpc | 3: `_with_run_target`, `hpc_resources`, `task_runs_on_hpc` | 4: `_fun_name_from_task`, `_task_pool_name`, `is_cancelled`, `iso_utc` |
| scheduler+chains ↔ ai+lab log | 1: `active_profile_name` | 16: `_cache_dir`, `_chains_dir`, `_history_param_defaults`, `_hp_cluster_runs`, … |
| task framework ↔ cell data | 4: `label_props`, `n_obs`, `select_cols`, `view_centroid_cols` | 1: `_task_spec` |
| task framework ↔ qc+checks | 1: `channel_signal_flags` | 2: `_composite_step_names`, `_task_from_fun_name` |
| task framework ↔ ai+lab log | 1: `active_profile_name` | 9: `CompositeTask`, `_composite_step_names`, `_composite_steps`, `_history_leaves`, … |
| tasks ↔ hpc | 1: `hpc_task_cpus` | 1: `zarr_group_meta_files` |
| python bridge ↔ hpc | 1: `_run_py_hpc` | 3: `_app_dir`, `_py_task_env`, `pixi_env_name` |
| gating ↔ tracking+spatial | 8: `_track_class_mask`, `add_pop_fraction_cols!`, `is_tracked`, `track_aggregate_base`, … | 14: `_catkey`, `_clustfeatures_features`, `_coerce_filter_fun`, `_missing_col_mask`, … |
| gating ↔ qc+checks | 6: `_cohort_outliers`, `cohort_value_names`, `qc_finding`, `qc_path`, … | 7: `_gate_canon`, `gating_qc!`, `gating_qc_fun`, `gating_qc_pop_type`, … |
| tracking+spatial ↔ plots+boards | 1: `_finite` | 8: `_is_categorical_col`, `_is_var_measure`, `add_pop_fraction_cols!`, `approach_cell_col`, … |
| plots+boards ↔ ai+lab log | 4: `_collect_contact_stats`, `_collect_pattern_stats`, `_collect_proximity_stats`, `contact_matrix` | 4: `boards_doc_path`, `plot_spec_index`, `plot_summary_data`, `read_boards_doc` |

## Layers — calls up the declared order

The declared order, bottom first (`[layers]` in `julia_areas.toml`). An area may call its own tier and the ones below; a call up the order is a gap, listed by name in [`gaps.md`](../gaps.md#julia-upward).

| Tier | Areas |
|---|---|
| 1 | config, logging |
| 2 | model, run records |
| 3 | python bridge, cell data |
| 4 | gating, tracking+spatial, qc+checks |
| 5 | task framework, run history |
| 6 | tasks |
| 7 | scheduler+chains, hpc, jobs (export, import, maintenance) |
| 8 | runner, plots+boards, ai+lab log |
| 9 | package root |

<details><summary>Full matrix (rows call columns)</summary>

| | model | jobs (export, import, maintenance) | config | run records | run history | scheduler+chains | task framework | tasks | runner | python bridge | cell data | gating | tracking+spatial | qc+checks | plots+boards | hpc | ai+lab log | logging | package root |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| **model** | · | 1 | 10 | 2 |  |  |  |  |  |  |  |  |  |  |  |  | 1 |  |  |
| **jobs (export, import, maintenance)** | 14 | · | 6 | 8 |  | 1 | 1 |  |  | 2 | 2 | 5 | 2 |  |  |  | 1 |  |  |
| **config** |  |  | · |  |  |  |  |  |  |  |  |  |  |  |  |  | 1 |  |  |
| **run records** |  |  | 5 | · |  |  | 2 |  |  |  |  |  |  |  |  |  |  |  |  |
| **run history** | 14 |  | 2 | 9 | · |  | 12 |  |  |  | 2 | 10 | 1 | 11 |  |  |  |  |  |
| **scheduler+chains** | 14 | 1 | 14 | 14 | 8 | · | 33 | 2 | 2 | 3 |  | 7 |  | 2 |  | 3 | 1 | 1 |  |
| **task framework** | 16 | 3 | 9 | 3 | 1 | 2 | · |  |  |  | 4 | 2 |  | 1 |  |  | 1 |  |  |
| **tasks** | 70 | 1 | 36 | 7 | 2 | 1 | 7 | · |  | 2 | 30 | 22 | 35 | 19 |  | 1 |  |  |  |
| **runner** | 4 | 1 | 13 |  |  | 18 | 2 |  | · |  |  |  |  |  |  |  | 1 | 6 |  |
| **python bridge** |  |  | 7 | 2 |  |  | 1 |  |  | · |  |  |  |  |  | 1 |  |  |  |
| **cell data** | 14 |  | 7 | 1 |  |  | 1 |  |  | 2 | · |  |  | 2 |  |  |  |  |  |
| **gating** | 17 |  | 13 | 9 |  |  |  |  |  |  | 16 | · | 8 | 6 |  |  |  |  |  |
| **tracking+spatial** | 16 |  | 2 | 2 |  |  |  |  |  |  | 21 | 14 | · | 1 | 1 |  |  |  |  |
| **qc+checks** | 17 |  | 6 | 1 |  |  | 2 |  |  |  |  | 7 |  | · |  |  |  |  |  |
| **plots+boards** | 39 |  | 7 | 10 | 1 | 3 | 5 | 9 |  | 2 | 11 | 28 | 8 | 6 | · |  | 4 |  |  |
| **hpc** | 1 | 1 | 13 | 1 |  | 4 | 1 | 1 |  | 3 |  |  |  |  |  | · |  |  |  |
| **ai+lab log** | 25 | 2 | 15 | 11 | 2 | 16 | 9 | 1 |  |  | 5 | 25 | 1 | 8 | 4 | 1 | · |  |  |
| **logging** |  |  | 1 |  |  |  |  |  |  |  |  |  |  |  |  |  |  | · |  |
| **package root** | 4 | 1 | 4 | 1 | 3 | 10 | 3 | 46 | 3 |  | 1 | 17 | 6 | 1 | 3 | 10 | 2 | 1 | · |
| **api: server+ws** | 8 | 7 | 25 | 2 |  | 12 | 2 |  | 20 |  |  |  |  |  |  |  | 3 | 6 |  |
| **api: rendering** | 34 | 4 | 15 |  |  | 3 |  |  |  | 1 | 10 | 19 | 12 |  |  |  |  |  |  |
| **api: claude+kiwi** | 7 | 5 | 17 | 5 |  |  | 3 |  |  |  | 4 | 5 | 1 |  | 5 |  | 47 |  |  |
| **api: routes** | 86 | 13 | 58 | 6 | 3 | 29 | 27 | 10 | 5 | 3 | 13 | 123 | 31 | 28 | 31 | 15 | 22 | 1 |  |

</details>

