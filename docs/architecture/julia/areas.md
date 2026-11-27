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
  model["model<br/><small>8 files · 3.6k lines</small>"]
  config["config<br/><small>16 files · 3.7k lines</small>"]
  run_records["run records<br/><small>3 files · 0.9k lines</small>"]
  scheduler_chains["scheduler+chains<br/><small>21 files · 5.3k lines</small>"]
  task_framework["task framework<br/><small>12 files · 3.7k lines</small>"]
  tasks["tasks<br/><small>82 files · 12.9k lines</small>"]
  runner["runner<br/><small>5 files · 1.4k lines</small>"]
  cell_data["cell data<br/><small>4 files · 2.4k lines</small>"]
  gating["gating<br/><small>23 files · 6.3k lines</small>"]
  tracking_spatial["tracking+spatial<br/><small>15 files · 4.6k lines</small>"]
  qc_checks["qc+checks<br/><small>8 files · 4.9k lines</small>"]
  plots_boards["plots+boards<br/><small>12 files · 4.9k lines</small>"]
  ai_lab_log["ai+lab log<br/><small>21 files · 5.2k lines</small>"]
  package_root["package root<br/><small>1 files · 0.6k lines</small>"]
  tasks -->|63| model
  package_root -->|46| tasks
  plots_boards -->|39| model
  tasks -->|36| config
  plots_boards -->|31| gating
  tasks -->|31| cell_data
  tasks -->|29| tracking_spatial
  ai_lab_log -->|28| gating
  ai_lab_log -->|24| model
  scheduler_chains -->|24| task_framework
  tasks -->|24| gating
  cell_data -->|20| model
  scheduler_chains -->|20| run_records
  tracking_spatial -->|20| cell_data
  ai_lab_log -->|18| scheduler_chains
  gating -->|18| model
  runner -->|18| scheduler_chains
  tasks -->|18| qc_checks
  package_root -->|17| gating
  qc_checks -->|17| model
  gating -->|16| cell_data
  tracking_spatial -->|16| model
  scheduler_chains -->|15| model
  ai_lab_log -->|14| config
  scheduler_chains -->|14| config
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
    api__routes["api: routes<br/><small>45 files · 17.4k lines</small>"]
  end
  subgraph PKG["app/src — Cecelia.jl"]
    model["model<br/><small>8 files · 3.6k lines</small>"]
    jobs__export__import__maintenance_["jobs (export, import, maintenance)<br/><small>4 files · 0.9k lines</small>"]
    config["config<br/><small>16 files · 3.7k lines</small>"]
    scheduler_chains["scheduler+chains<br/><small>21 files · 5.3k lines</small>"]
    task_framework["task framework<br/><small>12 files · 3.7k lines</small>"]
    tasks["tasks<br/><small>82 files · 12.9k lines</small>"]
    runner["runner<br/><small>5 files · 1.4k lines</small>"]
    cell_data["cell data<br/><small>4 files · 2.4k lines</small>"]
    gating["gating<br/><small>23 files · 6.3k lines</small>"]
    tracking_spatial["tracking+spatial<br/><small>15 files · 4.6k lines</small>"]
    qc_checks["qc+checks<br/><small>8 files · 4.9k lines</small>"]
    plots_boards["plots+boards<br/><small>12 files · 4.9k lines</small>"]
    hpc["hpc<br/><small>9 files · 2.5k lines</small>"]
    ai_lab_log["ai+lab log<br/><small>21 files · 5.2k lines</small>"]
  end
  api__routes -->|126| gating
  api__routes -->|79| model
  api__routes -->|57| config
  api__claude_kiwi -->|47| ai_lab_log
  api__routes -->|31| ai_lab_log
  api__routes -->|31| plots_boards
  api__routes -->|30| qc_checks
  api__rendering -->|29| model
  api__routes -->|29| scheduler_chains
  api__routes -->|29| tracking_spatial
  api__routes -->|27| task_framework
  api__server_ws -->|25| config
  api__server_ws -->|20| runner
  api__rendering -->|19| gating
  api__claude_kiwi -->|17| config
  api__rendering -->|15| config
  api__routes -->|15| hpc
  api__routes -->|13| cell_data
  api__rendering -->|12| tracking_spatial
  api__routes -->|12| jobs__export__import__maintenance_
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
| model ↔ jobs (export, import, maintenance) | 1: `store_debris_summary` | 5: `_rm_tree`, `discard_later`, `load_project`, `move_aside`, … |
| model ↔ config | 6: `_path_bytes`, `gen_uid`, `keep_previous_version`, `projects_dir`, … | 4: `img_project_dir`, `img_project_uid`, `json_native`, `read_state_json` |
| model ↔ run records | 6: `_rl_str`, `_update_run_log!`, `read_run_log`, `read_run_log_archive`, … | 5: `_load_image`, `img_has_value_name`, `resolve_value_name`, `resolve_version`, … |
| model ↔ scheduler+chains | 2: `_history_leaves`, `rename_label_set_in_params` | 15: `CciaImage`, `CciaSet`, `_fire_chain_event!`, `_load_image`, … |
| model ↔ cell data | 2: `_label_name_taken`, `free_label_name` | 20: `channel_names`, `commit_state!`, `img_branch_labels_dir`, `img_branch_props_path`, … |
| model ↔ gating | 8: `_clustfeatures_entry`, `_clustfeatures_part_of`, `_clustfeatures_suffixes`, `gating_history_path`, … | 18: `CciaImage`, `_load_image`, `channel_names`, `images`, … |
| model ↔ tracking+spatial | 2: `load_roi_set`, `roi_suffixes_on` | 16: `channel_names`, `image_attr_groups`, `img_axes`, `img_branch_props_path`, … |
| jobs (export, import, maintenance) ↔ ai+lab log | 1: `active_profile_name` | 2: `_kill_proc_tree`, `write_stored_zip` |
| config ↔ ai+lab log | 1: `active_profile_name` | 14: `_pid_alive`, `_pretty_json`, `api_token_path`, `cecelia_conf`, … |
| run records ↔ scheduler+chains | 6: `_history_leaves`, `_history_output`, `_keyed_strings`, `_preflight_fields`, … | 20: `RunUsage`, `_rl_get`, `_rl_str`, `_run_has_output`, … |
| run records ↔ task framework | 5: `_composite_step_names`, `_task_from_fun_name`, `task_params_name`, `task_source_stamp`, … | 4: `close_run_log!`, `open_run_log!`, `run_env_of`, `run_provenance` |
| run records ↔ gating | 4: `is_gating_pop_type`, `load_pop_tree`, `pop_gate_hashes`, `resolve_pop_type` | 4: `drop_props_sidecar_keys!`, `props_sidecar_path`, `put_props_sidecar_entry!`, `read_props_sidecar` |
| scheduler+chains ↔ task framework | 24: `CompositeTask`, `ParamValidationError`, `TaskApplicabilityError`, `_clamp_to_spec`, … | 2: `_fun_name_from_task`, `list_tasks` |
| scheduler+chains ↔ tasks | 2: `_str_list`, `validate_params` | 1: `pool_status` |
| scheduler+chains ↔ runner | 2: `TaskRequest`, `execute_task` | 18: `_is_probe_code_bug`, `cancel_chain_run!`, `cancel_task!`, `chain_template_from_raw`, … |
| scheduler+chains ↔ cell data | 2: `_stale_reads_names`, `stale_outputs_for` | 1: `_history_leaves` |
| scheduler+chains ↔ qc+checks | 2: `chain_cohort_qc!`, `write_defaulted_scale_qc!` | 3: `_history_leaves`, `_history_output`, `is_bookkeeping_param` |
| scheduler+chains ↔ hpc | 3: `_with_run_target`, `hpc_resources`, `task_runs_on_hpc` | 4: `_fun_name_from_task`, `_task_pool_name`, `is_cancelled`, `iso_utc` |
| scheduler+chains ↔ ai+lab log | 2: `_parse_tkey`, `active_profile_name` | 18: `_cache_dir`, `_chains_dir`, `_history_leaves`, `_history_param_defaults`, … |
| task framework ↔ tasks | 2: `store_axis_sizes`, `store_rev` | 7: `ParamValidationError`, `_picked_channels`, `_spec_output_value_name`, `register_builtin_task!`, … |
| task framework ↔ cell data | 4: `label_props`, `n_obs`, `select_cols`, `view_centroid_cols` | 1: `_task_spec` |
| task framework ↔ qc+checks | 1: `channel_signal_flags` | 4: `_composite_step_names`, `_stored_key`, `_task_from_fun_name`, `task_params_name` |
| task framework ↔ ai+lab log | 1: `active_profile_name` | 6: `CompositeTask`, `_composite_step_names`, `_composite_steps`, `_show_if_satisfied`, … |
| tasks ↔ tracking+spatial | 29: `_basis_name`, `_basis_segments`, `add_track_pop_fractions!`, `append_corrections!`, … | 8: `Track`, `_comp_col`, `roi_review_labels`, `store_written_at`, … |
| tasks ↔ qc+checks | 18: `_cal_num`, `_cluster_run_env`, `_saturation_channels`, `acquisition_technology`, … | 4: `import_source_fingerprint`, `ngff_multiscales`, `series_base`, `zarr_array_meta` |
| tasks ↔ plots+boards | 3: `_obs_suffixes`, `_rm_if`, `prune_stale_hmmfit!` | 8: `TrackMeasures`, `_motiffeatures_path`, `_region_owned_cols`, `aggregate_qc_key`, … |
| tasks ↔ hpc | 1: `hpc_task_cpus` | 1: `zarr_group_meta_files` |
| python bridge ↔ hpc | 1: `_run_py_hpc` | 3: `_app_dir`, `_py_task_env`, `pixi_env_name` |
| cell data ↔ gating | 2: `gating_path`, `is_track_grained` | 16: `_split_channel_column`, `as_df`, `axis_of`, `canonical_channel_columns`, … |
| cell data ↔ tracking+spatial | 2: `corrections_dir`, `roi_suffixes_on` | 20: `_agg_suffix`, `_channel_label`, `_var_measure_set`, `as_df`, … |
| cell data ↔ qc+checks | 2: `qc_finding`, `write_qc` | 2: `centroid_columns`, `label_props` |
| gating ↔ tracking+spatial | 9: `_track_class_mask`, `is_pop_fraction`, `is_tracked`, `track_aggregate_base`, … | 14: `_clustfeatures_entry`, `_clustfeatures_features`, `_clustfeatures_part_of`, `_coerce_filter_fun`, … |
| gating ↔ qc+checks | 6: `_cohort_outliers`, `cohort_value_names`, `qc_finding`, `qc_path`, … | 12: `_gate_canon`, `_split_pop_ref`, `gate_shape_spec`, `gating_qc!`, … |
| tracking+spatial ↔ plots+boards | 3: `_catkey`, `_finite`, `_ordered_cats` | 9: `_is_categorical_col`, `_is_var_measure`, `approach_cell_col`, `approach_track_col`, … |
| qc+checks ↔ ai+lab log | 1: `append_lab_log!` | 8: `_read_qc_file`, `all_qc_docs`, `cohort_qc_summary_lines`, `qc_fun_dir`, … |
| plots+boards ↔ ai+lab log | 5: `_collect_contact_stats`, `_collect_pattern_stats`, `_collect_proximity_stats`, `_parse_tkey`, … | 4: `boards_doc_path`, `plot_spec_index`, `plot_summary_data`, `read_boards_doc` |

## Layers — calls up the declared order

The declared order, bottom first (`[layers]` in `julia_areas.toml`). An area may call its own tier and the ones below; a call up the order is a gap, listed by name in [`gaps.md`](../gaps.md#julia-upward).

| Tier | Areas |
|---|---|
| 1 | config, logging |
| 2 | model, run records |
| 3 | python bridge, cell data |
| 4 | gating, tracking+spatial, qc+checks |
| 5 | task framework |
| 6 | tasks |
| 7 | scheduler+chains, hpc, jobs (export, import, maintenance) |
| 8 | runner, plots+boards, ai+lab log |
| 9 | package root |

<details><summary>Full matrix (rows call columns)</summary>

| | model | jobs (export, import, maintenance) | config | run records | scheduler+chains | task framework | tasks | runner | python bridge | cell data | gating | tracking+spatial | qc+checks | plots+boards | hpc | ai+lab log | logging | package root |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| **model** | · | 1 | 6 | 6 | 2 |  |  |  |  | 2 | 8 | 2 |  |  |  |  |  |  |
| **jobs (export, import, maintenance)** | 5 | · | 5 |  |  |  |  |  | 2 |  |  |  |  |  |  | 1 |  |  |
| **config** | 4 |  | · |  |  |  |  |  |  |  |  |  |  |  |  | 1 |  |  |
| **run records** | 5 |  | 6 | · | 6 | 5 |  |  |  |  | 4 |  |  |  |  |  |  |  |
| **scheduler+chains** | 15 | 1 | 14 | 20 | · | 24 | 2 | 2 | 3 | 2 | 8 |  | 2 |  | 3 | 2 | 1 |  |
| **task framework** | 13 | 3 | 9 | 4 | 2 | · | 2 |  |  | 4 |  |  | 1 |  |  | 1 |  |  |
| **tasks** | 63 | 1 | 36 | 5 | 1 | 7 | · |  | 2 | 31 | 24 | 29 | 18 | 3 | 1 |  |  |  |
| **runner** | 4 | 1 | 13 |  | 18 | 2 |  | · |  |  |  |  |  |  |  | 1 | 6 |  |
| **python bridge** |  |  | 7 | 2 |  | 1 |  |  | · |  |  |  |  |  | 1 |  |  |  |
| **cell data** | 20 |  | 7 | 10 | 1 | 1 |  |  | 2 | · | 2 | 2 | 2 |  |  |  |  |  |
| **gating** | 18 |  | 12 | 4 |  |  |  |  |  | 16 | · | 9 | 6 |  |  |  |  |  |
| **tracking+spatial** | 16 |  | 1 |  |  |  | 8 |  |  | 20 | 14 | · | 1 | 3 |  |  |  |  |
| **qc+checks** | 17 |  | 5 | 4 | 3 | 4 | 4 |  |  | 2 | 12 |  | · |  |  | 1 |  |  |
| **plots+boards** | 39 |  | 7 | 4 | 2 | 5 | 8 |  | 2 | 10 | 31 | 9 | 7 | · |  | 5 |  |  |
| **hpc** | 2 | 1 | 12 | 1 | 4 | 1 | 1 |  | 3 |  |  |  |  |  | · |  |  |  |
| **ai+lab log** | 24 | 2 | 14 | 9 | 18 | 6 | 1 |  |  | 6 | 28 | 1 | 8 | 4 | 1 | · |  |  |
| **logging** |  |  | 1 |  |  |  |  |  |  |  |  |  |  |  |  |  | · |  |
| **package root** | 4 | 1 | 4 | 1 | 10 | 3 | 46 | 3 |  | 3 | 17 | 6 | 2 | 3 | 10 | 2 | 1 | · |
| **api: server+ws** | 8 | 7 | 25 | 2 | 12 | 2 |  | 20 |  |  |  |  |  |  |  | 3 | 6 |  |
| **api: rendering** | 29 | 4 | 15 |  | 3 |  | 5 |  | 1 | 10 | 19 | 12 |  |  |  |  |  |  |
| **api: claude+kiwi** | 7 | 5 | 17 | 5 |  | 3 |  |  |  | 4 | 5 | 1 |  | 5 |  | 47 |  |  |
| **api: routes** | 79 | 12 | 57 | 4 | 29 | 27 | 10 | 5 | 3 | 13 | 126 | 29 | 30 | 31 | 15 | 31 | 1 |  |

</details>

