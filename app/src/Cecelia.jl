module Cecelia

# ── Config ────────────────────────────────────────────────────────────────────
export init_cecelia!, cecelia_conf, config_dir, ensure_config_dir, custom_toml_path, expand_user
export cellpose_models_dir, cellpose_model_path, list_cellpose_models
export coastal_models_dir, coastal_model_path, coastal_model_manifest, list_coastal_models
export projects_dir, setup_required, set_projects_dir!
export bioformats2raw_bin, python_bin_path, tasks_concurrent_limit, napari_discrete_gpu
export task_worker_threads
export runner_enabled, set_runner_enabled!, is_dev_session
export image_compressor, set_image_compressor!, bf2raw_compression_flags, bf2raw_chunk_flags, bf2raw_format_flags, IMAGE_COMPRESSOR_CHOICES
export IMAGE_COMPRESSOR_MEASURED_ON
export ngff_version, chunk_separator, set_store_layout!
export NGFF_VERSION_DEFAULT, CHUNK_SEPARATOR_DEFAULT
export store_layout, STORE_LAYOUT_CHOICES, STORE_LAYOUT_DEFAULT, STORE_LAYOUT_MEASURED_ON

# ── Utils ─────────────────────────────────────────────────────────────────────
export gen_uid, UID_LENGTH
export write_atomic, write_json_atomic
export write_http_body!
export safe_name_part

# ── Log rail (console records, the server tee, child-process capture) ─────────
# `log_record`/`TeeLogger`/`install_log_tee!` are what a SERVER installs to forward the package's own
# `@info`/`@warn`/`@error` to a transport; `spawn_logged` is how a long-lived child gets onto the same
# rail. Deliberately generic names are avoided (no bare `Tee`/`record`) — see the export rule in
# CLAUDE.md → Julia conventions.
export log_record, TeeLogger, install_log_tee!, log_level_name, log_timestamp, spawn_logged
export LogRing, log_ring_push!, log_ring_since, log_ring_seq, log_ring_id
export LOG_SOURCES, CHILD_LOG_SOURCES, LOG_DETAIL_CAP
export LOG_SOURCE_BACKEND, LOG_SOURCE_NAPARI, LOG_SOURCE_PREVIEW, LOG_SOURCE_RUNNER,
       LOG_SOURCE_NOTEBOOKS

# ── Versioned-variable helpers ────────────────────────────────────────────────
export VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL
export versioned_active, versioned_get, versioned_set!
export versioned_get_field, versioned_set_field!, versioned_keys, read_ccid_raw, read_state_json
export json_native
export project_storage_summary, reclaim_inactive!, remove_image_version!, reclaimable_versions, image_storage
export reset_image_analysis!, analysis_bytes_of, ANALYSIS_KEEP

# ── Data model ────────────────────────────────────────────────────────────────
export CciaImage, CciaSet, CciaProject
export save!
export load_project, init_object
export create_project!, add_image!, add_set!, images, image_by_uid, sets
export delete_image!, delete_set!, rename_set!, set_name_taken, move_image!
export img_filepath, img_zero_dir, img_project_dir, img_project_uid, img_physical_sizes, physical_size_for_axis, img_is_calibrated,
       image_included
export img_axes, img_has_time
export img_label_props_dir, img_label_props_path, img_track_props_path, img_track_value_names, img_value_names, img_has_value_name, resolve_value_name
export img_labels_dir, img_labels_path
export img_spatial_graph_dir, img_spatial_graph_path, img_spatial_graph_suffixes
export img_stats_dir, img_stats_path, img_stats_suffixes, img_cluster_suffixes
export img_branch_props_path, img_branch_value_names, img_branch_labels_dir, img_branch_labels_path
export read_module_fun_params, read_module_fun_params_by_name, write_module_fun_params!
export state_file, obj_meta_dir, STATE_FILENAME
export TRACK_PROPS_SUFFIX, BRANCH_PROPS_SUFFIX, is_reserved_value_name
export migrate_qc_findings
export write_qc, read_qc, read_all_qc, all_qc_docs, qc_finding, qc_canvas_expansion, qc_path, track_count_metrics
export cohort_qc, cohort_qc!, cohort_qc_for, cohort_qc_for!, read_cohort_qc, read_all_cohort_qc, cohort_qc_path, COHORT_METRICS, register_cohort_metrics!
export cohort_value_names, cohort_runs, cohort_qc_for_all, cohort_qc_for_all!
export cohort_qc_summary_lines, cohort_has_outliers
export read_run_log, append_run_log!, run_log_path, run_log_params_for_output
export open_run_log!, close_run_log!, reap_run_log!, RUN_LOG_RUNNING, RUN_LOG_INTERRUPTED
export read_lab_log, append_lab_log!, upsert_daily_context_block!, parse_lab_log, lab_log_path, LAB_LOG_FILENAME
export read_dismissed, set_dismissed!
export la_doc_path, read_la_doc, write_la_doc!, la_gaps, la_briefing, LA_DOC_FILE
export attr_value_counts, image_attr_groups
export capture_context!, record_stats_event!, CONTEXT_AUTHOR
export set_channel_names!, channel_names
export channel_index, channel_indices, ccid_channel_names

# ── Lockfile / transaction ────────────────────────────────────────────────────
export with_transaction, commit_state!

# ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────────
export LabelProps, label_props, as_df, as_matrix, add_obs, drop_obs, write_categorical_obs, n_obs
export select_cols, view_cols, view_channel_cols, view_centroid_cols, view_label_col
export filter_rows, sort_by, rename_channels!
export col_names, channel_columns, centroid_columns, temporal_columns, axis_of, scale_centroids!
export obsm, obsm_keys, uns_keys, uns_array, uns_dict, uns_df

# ── Gating engine: transforms, gates, density ─────────────────────────────────
export AxisTransform, LinearTransform, LogTransform, AsinhTransform, LogicleTransform
export apply_transform, invert_transform, transform_spec, transform_from_spec
export transform_kind, transform_collapses, effective_transform
export Gate, RectangleGate, PolygonGate
export inside, point_in_polygon, gate_channels, gate_spec, gate_from_spec, project_gate
export Density2D, density_2d

# ── Population manager ─────────────────────────────────────────────────────────
export Population, PopulationMap, ROOT
export pop_parent, pop_name, pop_path, is_root
export add_pop!, set_gate!, rename_pop!, del_pop!
export is_reserved_pop_name, DERIVED_POP_PREFIX, derived_pop_paths, has_ungated_tracks
export GATING_POP_TYPES, is_gating_pop_type
export SPATIAL_UNIT_PX, SPATIAL_UNIT_UM, is_spatial_axis, has_spatial_gate
export flatten_pop_tree, plot_pop_types, plot_population_groups
export is_track_pop, pop_category, scope_pop_types, population_scope_groups, population_accept_groups
export AGGREGATED_POP_NAME, ensure_filter_pop!
export pop_at, has_pop, pop_paths, direct_children, descendants, topo_order
export to_tree, from_tree, save_pop_map!, load_pop_map, gating_dir, gating_path
export co_clustered_value_names
export colour_by_palette, pop_colour_overrides, pop_label_overrides, OKABE_ITO
export recompute!, cells_in_pop, pop_membership, pop_stats, pop_df, resolve_pops
export pop_df_multi, resolve_pop_type, pop_namespace, pop_name_conflict, pops_value_name
export region_membership, region_enrichment
export plot_summary_data
export quiver_df, branch_segments, anisotropy_df
export track_props, track_cell_measures, is_tracked
# manual track correction (docs/todo/CORRECTION_PLAN.md) — the ops engine, its journal and its QC.
# `apply_track_ops!`/`renumber_cell_ids!` are the names anything correcting tracks must go through;
# the per-op `_remove_points!`-style methods stay internal so there is one entry point, not six.
export apply_track_ops!, apply_track_op!, renumber_cell_ids!, next_track_id, track_ids_present
export TRACK_OP_KINDS, TRACK_LINEAGE_OBS, TRACK_CORRECTION_OBS, MIN_USEFUL_TRACK_LENGTH
export corrections_dir, corrections_path, load_corrections, append_corrections!
export track_correction_metrics, track_correction_qc_findings, TRACK_CORRECTION_WARN_FRAC
# finding what needs correcting — the triage worklist (old R had no equivalent)
export TrackIssue, find_track_issues, track_issues_for, issue_to_dict
export TRACK_GAP_MAX_FRAMES, TRACK_GAP_STEPS, TRACK_JUMP_FACTOR, TRACK_JUMP_QUANTILE
export track_step_scale, analyze_cell_pairs, find_duplicate_tracks, track_pair_drift
export track_path_dicts
# ── Track diagnostics (celltrackR QC battery) ─────────────────────────────────
export track_msd, msd_log_slope, msd_motion_kind, track_autocorrelation, persistence_lag
export plane_angle_profile, plane_artefact, drift_test
export track_diagnostics, track_diagnostics_for, track_diagnostic_findings, pooled_track_frame
export TrackPlotGroup, TrackPlotSource, track_plot_groups, track_group_paths, track_group_diagnostics
export track_group_frame
export track_group_images, track_group_value_name, track_group_pop
export MSD_SLOPE_RANDOM, MSD_SLOPE_DIRECTED, MSD_SLOPE_CONFINED, ACOR_PERSIST_LEVEL
export PLANE_ANGLE_UNBIASED, DRIFT_STEP_SPACING, DRIFT_ALPHA, PAIR_SCAN_MAX_TRACKS
export TRACK_DUP_ANGLE_DEG, TRACK_DUP_DIST_UM, TRACK_DUP_MIN_SHARED
export hmm_fit_states, hmm_transitions, DiagGaussEmission

# ── Task system ───────────────────────────────────────────────────────────────
export CciaTask
export validate_params, ParamValidationError
export _task_from_fun_name, task_scope
export task_requires_axes, task_applies, task_applicability_reason, TaskApplicabilityError
export register_task!, load_custom_modules!, custom_modules_dir, custom_modules_report
export custom_task_clashes
export plugins_dir, plugin_roots, plugin_name_of, read_plugin_manifest, plugin_version_warning
export user_task_specs, user_plot_specs, plugins_report, tier_name, plugin_contributions, plugin_views, bundled_plugins, plugin_install_local!
export plugin_unpack!, plugin_remove!, plugin_tarball_url, plugin_name_from_url
export read_install_record, install_record_path, plugin_registry, plugin_registry_status
export PLUGINS_SUBDIR, PLUGIN_MANIFEST, PLUGIN_INSTALL_RECORD, LEGACY_LAYOUT_DIRS, PLOT_DEFS_SUBDIR
export TIER_PLUGIN, TIER_USER, TIER_BUILTIN

# ── View profiles (curated sidebar) ───────────────────────────────────────────
export view_profiles_dir, read_view_profiles, parse_view_profile,
       view_profile_id, write_view_profile, delete_view_profile!
export TestImageTask, TestSetTask, IncrementalPlotTask
export ImportOmezarr, read_ome_metadata, update_ome_scale!, update_ome_xml_pixels!, ome_xml_unit_name
# The ONE Julia resolver for zarr v2-vs-v3 NGFF metadata (see omezarr.jl). Exported because
# `api/src/image_geometry.jl` reads axes through it — without the export it resolved to nothing,
# the reader's catch-all swallowed the UndefVarError, and EVERY store silently reported no axes.
export ngff_group_attrs, ngff_multiscales, zarr_array_meta, ngff_version
export resync_ome_meta!
export RemoveImage
export CellposeSegment
export CoastalSegment, coastal_models_for_python
export TrainFlowModel, parse_temporal_scales, flow_model_target, flow_training_qc_findings
export flow_model_filename
export MeasureLabels
export Branching
export BayesianTracking, TrackMeasures, TrackCorrect, parse_track_ops
export ClustPops, ClustTracks
export CellNeighbours, ClustRegions, NeighbourStats, DetectAggregates, CellContacts, ContactsMeshes
export AggregatesMeshes
export detect_motion_dims, MotionDims
export AfCorrect, DriftCorrect, Smooth, CompositeTask
export CropImage
export CopyImage
export ExportOmeTiff

# ── Scheduler ─────────────────────────────────────────────────────────────────
export ResourcePool, TaskRecord
export run_task, run_tasks
export cancel_task!, is_cancelled, cancel_chain_run!, is_chain_cancelled, list_pools, list_tasks,
       recent_tasks, record_task_outcome!, pool_status
export note_task_started!, task_started_at, forget_task_start!, iso_utc, parse_iso_utc, TASK_TS_FORMAT
export MaintenancePatch, maintenance_patches, maintenance_patch, run_maintenance_patch, cancel_maintenance!
export start_job!, track_job!, job_cancelled, finish_job!, cancel_job!
export export_project, import_project, default_export_dir, list_bundles, bundle_info, reidentify_project!
export resize_pool!, set_pool_limit!
# Sink-agnostic execution (runner/execute.jl) — one implementation, driven by the API server today and
# by the detached runner next. See docs/todo/TASK_RUNNER_PLAN.md.
export TaskRequest, execute_task, task_request, task_request_dict
export ChainRequest, execute_chain, chain_request, chain_request_dict
export subscribe_chain_frames!, chain_event_task_id
# The detached task runner (runner/server.jl + runner/client.jl)
export RUNNER_PORT, RUNNER_PROTOCOL, runner_serve, runner_identity, runner_emit
export RunnerHandle, runner_launch!, runner_stop!, runner_ping, runner_alive, runner_subscribe!
export runner_submit, runner_cancel, runner_tasks, runner_recent, runner_logs, runner_pools, runner_set_pool_limit
export runner_submit_chain, runner_cancel_chain, runner_chain_runs

# ── Chain event bus ───────────────────────────────────────────────────────────
export subscribe_chain_events!, unsubscribe_chain_events!

# ── Chain executor ─────────────────────────────────────────────────────────────
export ChainNode, ChainEdge, ChainTemplate, ChainRun, ImageNodeState
export load_chain_template, save_chain_template!, load_template_from_cache
export validate_chain_template, ChainTemplateError, chain_template_from_raw, chain_root_ids
export load_chain_run
export run_chain
export chain_node, make_chain

# ── Napari viewer ─────────────────────────────────────────────────────────────
export NapariViewer
export launch!, close!, restart!, send
export open_image!, show_labels!, show_branch_labels!, refresh_labels!, set_z_view!, set_3d_level!
export show_layer!, hide_layer!, remove_layer!, clear!
export centre!, save_layer_props!, load_layer_props!, save_screenshot!, record_timelapse!, record_keyframes!, stitch_movies!
export capture_view_state, apply_view_state!, preview_region

# ── Task preview (resident worker) ────────────────────────────────────────────
# `launch!`/`close!`/`send` above are shared generics — the preview worker adds methods, not names.
export PreviewWorker, PREVIEW_PORT, PREVIEW_PROTOCOL, preview_alive, preview_request
export preview_show_command, show_task_preview!, hide_task_preview!
export task_previewable, preview_params, preview_params_for_run,
       preview_steps_not_previewed

# ── Includes ──────────────────────────────────────────────────────────────────
include("config.jl")
include("utils.jl")
# The log rail: the canonical console-record shape, the tee a server installs, and `spawn_logged`
# (the only sanctioned way to start a long-lived child, because `run(cmd; wait=false)` swallows its
# stdio). Early — napari, the preview worker and the notebook server all launch through it.
include("log_stream.jl")
include("py_runner.jl")
# OS process control (kill primitives) + the background-job registry (track/cancel by task_id) shared
# by data patches and project export/import. Foundational; before the scheduler + jobs that use it.
include("jobs.jl")
include("helpers.jl")
include("events.jl")
include("model/image.jl")
include("qc.jl")
include("run_log.jl")
include("label_props.jl")
include("gating/transforms.jl")
include("gating/gates.jl")
include("gating/density.jl")
include("gating/population_manager.jl")
include("gating/gating_engine.jl")
include("spatial.jl")   # cross-poptype region queries (needs pop_df)
include("anisotropy.jl")   # branching anisotropy readouts as tidy frames (notebooks)
include("plotting/plot_data.jl")
include("plotting/stats.jl")
include("tracking/track_props.jl")
include("tracking/track_correction.jl")   # manual track edit ops + journal (pure; used by the task)
include("tracking/track_diagnostics.jl")  # celltrackR QC battery (pure; the plot AND the task QC)
include("tracking/track_cohort.jl")       # (images × population) grouping for the two track plots
include("behaviour/hmm.jl")
include("model/set.jl")
include("model/project.jl")
include("qc_cohort.jl")   # after CciaSet (set.jl) — cohort QC dispatches on it
include("lab_log.jl")
include("lab_log_context.jl")
# the one reader/writer of settings/analysisBoards.json — after CciaProject (model/project.jl), and
# before ai/lineage.jl, whose board_summaries reads through it
include("analysis_boards.jl")
# the semantic board spec + its expander/validator (MCP board authoring, Phase 2). After
# analysis_boards.jl for BoardsDoc; `_observer_each_population` (ai/) is resolved at call time.
include("analysis_board_spec.jl")
include("tasks/task.jl")
# algorithm-agnostic segmentation label-store conventions shared by every segmentation task —
# after task.jl (uses the `LiveOutput` trait type), before the segment/ tasks that call it
include("segmentation.jl")
include("tasks/testTasks/image_task.jl")
include("tasks/testTasks/set_task.jl")
include("tasks/testTasks/incremental_plot_task.jl")
include("tasks/importImages/omezarr.jl")
include("tasks/importImages/remove.jl")
include("tasks/importImages/migrateLegacy.jl")
include("tasks/cleanupImages/af_correct.jl")
include("tasks/cleanupImages/drift_correct.jl")
include("tasks/cleanupImages/smooth.jl")
include("tasks/editImages/cropImage.jl")
include("tasks/editImages/copyImage.jl")
include("tasks/exportImages/ome_tiff.jl")
include("tasks/segment/cellpose.jl")
include("tasks/opticalFlow/train.jl")
include("tasks/segment/coastal.jl")
include("tasks/segment/measure_labels.jl")
include("tasks/segment/branching.jl")
include("tasks/tracking/bayesian_tracking.jl")
include("tasks/tracking/track_measures.jl")
include("tasks/tracking/correct.jl")
include("tasks/behaviour/hmm_states.jl")
include("tasks/behaviour/hmm_transitions.jl")
include("tasks/clustPops/cluster.jl")
include("tasks/clustTracks/cluster.jl")
include("tasks/spatialAnalysis/cellNeighbours.jl")
include("tasks/spatialAnalysis/neighbourStats.jl")
include("tasks/spatialAnalysis/detectAggregates.jl")
include("tasks/spatialAnalysis/cellContacts.jl")
include("tasks/spatialAnalysis/contactsMeshes.jl")
include("tasks/spatialAnalysis/aggregatesMeshes.jl")
include("tasks/clustRegions/cluster.jl")
include("storage.jl")
include("tasks/task_registry.jl")
include("tasks/custom_modules.jl")
include("tasks/plugins.jl")
# User drop-in sidebar profiles — the other <config_dir> data surface beside modules/. Read + written
# here; the ROUTE TABLE it filters lives in the frontend, so this validates shape only.
# See docs/todo/VIEW_PROFILES_PLAN.md.
include("view_profiles.jl")
include("tasks/scheduler.jl")
include("tasks/task_outcomes.jl")
include("tasks/chain.jl")
# Sink-agnostic task execution — the body `handle_task_run` used to inline, so the API server and the
# detached runner drive the SAME execution. See docs/todo/TASK_RUNNER_PLAN.md.
include("runner/execute.jl")
# Chain events -> wire frames, shared by the API server and the runner (one builder, one bank).
include("runner/chain_frames.jl")
include("napari.jl")
# Task preview — the resident preview worker's lifecycle + request shape. After napari.jl (shares the
# `send` generic and the resident-WS-process pattern) and jobs.jl (_kill_proc_tree).
include("preview.jl")
# The detached task runner: `server.jl` is the process that owns the pools and executes tasks,
# `client.jl` is the API server's side of it. After napari.jl/preview.jl — same resident-child
# lifecycle — and after jobs.jl, whose `_kill_listeners_on_port` is how a runner we only ADOPTED gets
# stopped. See docs/todo/TASK_RUNNER_PLAN.md.
include("runner/server.jl")
include("runner/client.jl")
# Data patches (project-scoped maintenance scripts, run from Settings). After jobs.jl (track/cancel)
# + py_runner.jl (run_py/task_run_dir).
include("maintenance.jl")
# Project Manager export/import — background jobs (jobs.jl) that tar each store in parallel. See docs/JOBS.md.
include("project_io.jl")

# AI observer (in-app assistant) — spawns a headless agent that reads state + appends to the lab log
# through the cecelia-observer MCP. After scheduler.jl (uses _kill_proc_tree). See
# docs/todo/OBSERVER_INTEGRATION_PLAN.md.
include("ai/observer_prompt.jl")
include("ai/agent_runner.jl")
include("ai/observer_session.jl")
include("ai/observer_summary.jl")
include("ai/lineage.jl")
include("ai/populations.jl")
include("ai/measures.jl")
include("ai/behaviour_clusters.jl")
include("ai/spatial.jl")
include("ai/chains.jl")
include("ai/repl_api.jl")
include("ai/labarchives.jl")   # before briefing.jl — session_briefing calls la_briefing
include("ai/briefing.jl")
export analysis_lineage, board_summaries, populations_summary, measure_summary, behaviour_summary, cluster_summary
export BoardsDoc, boards_doc_path, normalise_boards, read_boards_doc, write_boards_doc, boards_doc_payload
export BoardSpecError, expand_board, append_board, plot_specs, plot_spec_index, board_template_grid,
       board_slot_areas, board_display_name
export chains_summary, session_briefing
export NOTEBOOK_API, repl_api_reference, repl_api_section, write_repl_doc
export spatial_summary, contact_matrix
export ClaudeAgent, agent_available, agent_bin_path, run_observer_turn, observer_mcp_config, observer_mcp_spec,
       OBSERVER_MCP_NAME, register_observer_mcp, observer_registration_state,
       claude_config_path, read_registered_observer_spec,
       read_local_observer_specs, observer_shadow_dirs, shadowing_observer_dirs, mcp_connections,
       remove_shadowing_observer_mcps,
       observer_feedback_prompt, observer_prompt_display, observer_agent_bin,
       OBSERVER_MODELS, observer_default_model, observer_valid_model,
       read_observer_session, record_observer_turn!, log_observer_pass!, clear_observer_session!

end
