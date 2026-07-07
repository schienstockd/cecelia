module Cecelia

# ── Config ────────────────────────────────────────────────────────────────────
export init_cecelia!, cecelia_conf
export projects_dir, bioformats2raw_bin, python_bin_path, tasks_concurrent_limit

# ── Utils ─────────────────────────────────────────────────────────────────────
export gen_uid, UID_LENGTH

# ── Versioned-variable helpers ────────────────────────────────────────────────
export VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL
export versioned_active, versioned_get, versioned_set!
export versioned_get_field, versioned_set_field!, versioned_keys

# ── Data model ────────────────────────────────────────────────────────────────
export CciaImage, CciaSet, CciaProject
export save!
export load_project, init_object
export create_project!, add_image!, add_set!, images, sets
export delete_image!, delete_set!
export active, set_active!, value_names, img_filepath, img_zero_dir, img_physical_sizes, image_included
export img_label_props_dir, img_label_props_path, img_track_props_path
export read_module_fun_params, write_module_fun_params!
export TRACK_PROPS_SUFFIX, is_reserved_value_name
export write_qc, read_qc, read_all_qc, qc_finding, qc_canvas_expansion, qc_path
export set_channel_names!, channel_names

# ── Lockfile / transaction ────────────────────────────────────────────────────
export with_transaction

# ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────────
export LabelProps, label_props, as_df, as_matrix, add_obs, drop_obs, write_categorical_obs
export select_cols, view_cols, view_channel_cols, view_centroid_cols, view_label_col
export filter_rows, sort_by, rename_channels!
export col_names, channel_columns, centroid_columns, temporal_columns
export obsm, obsm_keys

# ── Gating engine: transforms, gates, density ─────────────────────────────────
export AxisTransform, LinearTransform, LogTransform, AsinhTransform, LogicleTransform
export apply_transform, invert_transform, transform_spec, transform_from_spec
export Gate, RectangleGate, PolygonGate
export inside, point_in_polygon, gate_channels, gate_spec, gate_from_spec
export Density2D, density_2d

# ── Population manager ─────────────────────────────────────────────────────────
export Population, PopulationMap, ROOT
export pop_parent, pop_name, pop_path, is_root
export add_pop!, set_gate!, rename_pop!, del_pop!
export is_reserved_pop_name, DERIVED_POP_PREFIX, derived_pop_paths
export flatten_pop_tree, plot_pop_types, plot_population_groups
export pop_at, has_pop, pop_paths, direct_children, descendants, topo_order
export to_tree, from_tree, save_pop_map!, load_pop_map, gating_dir, gating_path
export recompute!, cells_in_pop, pop_membership, pop_stats, pop_df
export plot_summary_data
export track_props, track_cell_measures
export hmm_fit_states, hmm_transitions, DiagGaussEmission

# ── Task system ───────────────────────────────────────────────────────────────
export CciaTask
export validate_params, ParamValidationError
export _task_from_fun_name, task_scope
export TestImageTask, TestSetTask, IncrementalPlotTask
export ImportOmezarr, read_ome_metadata, update_ome_scale!, update_ome_xml_pixels!, ome_xml_unit_name
export resync_ome_meta!
export RemoveImage
export CellposeCorrect
export CellposeSegment
export MeasureLabels
export BayesianTracking, TrackMeasures
export ClustPops, ClustTracks
export detect_motion_dims, MotionDims
export AfCorrect, DriftCorrect, CompositeTask

# ── Scheduler ─────────────────────────────────────────────────────────────────
export ResourcePool, TaskRecord
export run_task, run_tasks
export cancel_task!, is_cancelled, cancel_chain_run!, is_chain_cancelled, list_pools, list_tasks
export resize_pool!

# ── Chain event bus ───────────────────────────────────────────────────────────
export subscribe_chain_events!, unsubscribe_chain_events!

# ── Chain executor ─────────────────────────────────────────────────────────────
export ChainNode, ChainEdge, ChainTemplate, ChainRun, ImageNodeState
export load_chain_template, save_chain_template!, load_template_from_cache
export load_chain_run
export run_chain
export chain_node, make_chain

# ── Napari viewer ─────────────────────────────────────────────────────────────
export NapariViewer
export launch!, close!, restart!, send
export open_image!, show_labels!
export show_layer!, hide_layer!, remove_layer!, clear!
export centre!, save_layer_props!, load_layer_props!, save_screenshot!

# ── Includes ──────────────────────────────────────────────────────────────────
include("config.jl")
include("utils.jl")
include("py_runner.jl")
include("helpers.jl")
include("events.jl")
include("model/image.jl")
include("qc.jl")
include("label_props.jl")
include("gating/transforms.jl")
include("gating/gates.jl")
include("gating/density.jl")
include("gating/population_manager.jl")
include("gating/gating_engine.jl")
include("plotting/plot_data.jl")
include("tracking/track_props.jl")
include("behaviour/hmm.jl")
include("model/set.jl")
include("model/project.jl")
include("tasks/task.jl")
include("tasks/testTasks/imageTask.jl")
include("tasks/testTasks/setTask.jl")
include("tasks/testTasks/incrementalPlotTask.jl")
include("tasks/importImages/omezarr.jl")
include("tasks/importImages/remove.jl")
include("tasks/cleanupImages/cellpose_correct.jl")
include("tasks/cleanupImages/af_correct.jl")
include("tasks/cleanupImages/drift_correct.jl")
include("tasks/segment/cellpose.jl")
include("tasks/segment/measure_labels.jl")
include("tasks/tracking/bayesian_tracking.jl")
include("tasks/tracking/track_measures.jl")
include("tasks/behaviour/hmm_states.jl")
include("tasks/behaviour/hmm_transitions.jl")
include("tasks/clustPops/cluster.jl")
include("tasks/clustTracks/cluster.jl")
include("tasks/task_registry.jl")
include("tasks/scheduler.jl")
include("tasks/chain.jl")
include("napari.jl")

end
