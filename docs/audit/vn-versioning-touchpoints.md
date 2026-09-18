# VN versioning — touchpoint audit

Grounds `docs/todo/VN_VERSIONING_PLAN.md`. Every callsite that would change under a
`default/<vn>/vN/...` layout. Grep-verified 2026-09-18 against origin/main at 20a4fa73.

Legend: `(vn, kind)` — kind is one of {image, labels, branchLabels, cellH5AD, trackH5AD,
branchH5AD, gating, corrections, qc, clustfeatures, sidecar}.

## Summary counts

| Category | Helpers | Prod callsites |
|---|---|---|
| Julia writers (staged_store / create_multiscales / open_multiscales_for_writing) | 3 | 15 |
| Julia writers (register_label_files! / commit_state! for filepath) | 2 | 20 (+ raw versioned_set_field! for filepath: 15) |
| Julia writers (write_atomic gating/tracking/label corrections) | 4 | 6 |
| Julia writers (write_json_atomic per-vn QC/sidecar) | 1 | ~35 (of which per-vn: ~20) |
| Python writers (staged_store) | 1 | 15 |
| Python writers (write_h5ad_atomic on {vn}.h5ad / {vn}__tracks.h5ad / {vn}__branch.h5ad) | 1 | 5 |
| Python writers (write_json_atomic per-vn) | 1 | ~30 (task runners) |
| Julia readers keyed by value_name (label_props, img_*_path, load_pop_map) | 8 | ~65 |
| Python readers keyed by value_name (LabelPropsView, open_as_zarr, read_axes/scale, series_base) | 5 | ~40 |
| API-side resolvers (resolve_image_version, resolve_value_name in payload builders) | 2 | ~15 |
| ccid.json versioned-field helpers | 8 | schema definition + ~40 callers |
| Frontend vn pickers (Vue components with valueName) | many | 43 `.vue` files |
| Chain planner refs (composite outputValueName snapshotting) | 3 | ~10 |
| MCP tools referencing value_name | 2 | 3 callers |
| Legacy `default/<vn>/` layout assumptions | — | 15 path-joining helpers |

## 1. Julia writers — the `staged_store` / `create_multiscales` family (Python-invoked, listed under app/src/tasks/*_run.py)

These are Python task runners that Julia dispatches through `run_py`. Each writes into a store whose
final path is a `default/<vn>/...` sibling and will target `default/<vn>/vN/...` after P2.

| file:line | Helper | (vn, kind) written |
|---|---|---|
| `app/src/tasks/editImages/bin_run.py:110` | `zarr_utils.staged_store` + `open_multiscales_for_writing` | (outputValueName, image) |
| `app/src/tasks/editImages/zProject_run.py:85` | staged_store + open_multiscales_for_writing | (VERSIONED_DEFAULT_VAL, image) |
| `app/src/tasks/editImages/dtype_run.py:136` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/editImages/flip_run.py:61` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/editImages/cropImage_run.py:87` | staged_store + open_multiscales_for_writing | (VERSIONED_DEFAULT_VAL, image) |
| `app/src/tasks/editImages/register_run.py:175` | staged_store + open_multiscales_for_writing | (VERSIONED_DEFAULT_VAL, image) |
| `app/src/tasks/editImages/resampleZ_run.py:96` | staged_store + open_multiscales_for_writing | (VERSIONED_DEFAULT_VAL, image) |
| `app/src/tasks/editImages/tProject_run.py:110` | staged_store + open_multiscales_for_writing | (VERSIONED_DEFAULT_VAL, image) |
| `app/src/tasks/cleanupImages/stack_align_run.py:77` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/cleanupImages/flow_register_run.py:157` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/cleanupImages/drift_correct_run.py:146` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/cleanupImages/denoise_run.py:163` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/cleanupImages/af_correct_run.py:74` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/cleanupImages/smooth_run.py:301` | staged_store + open_multiscales_for_writing | (outputValueName, image) |
| `app/src/tasks/segment/ridges_run.py:193` | staged_store + open_multiscales_for_writing | (outputValueName, labels) |
| `app/src/tasks/segment/branching_run.py:548` | staged_store + open_multiscales_for_writing | (outputValueName, branchLabels) |
| `app/src/tasks/segment/correct_run.py:297` | staged_store + open_multiscales_for_writing | (valueName, labels) |
| `python/cecelia/utils/segmentation_utils.py:362` (`_store_path` in `save_labels`, called from `cellpose_run.py`, `coastal_run.py`) | staged_store per mask-type (`base`/`nuc`) | (outputValueName, labels) |

## 2. Julia writers — ccid.json versioned-field mutations (path segment producers)

Every one of these sets `raw["filepath"][<vn>]`, which is the on-disk pointer P1 must migrate.

| file:line | Helper | (vn, kind) |
|---|---|---|
| `app/src/model/project.jl:376` | `versioned_set_field!(raw, "filepath", ...)` (`copyImage` fallback) | (out_value_name, filepath) |
| `app/src/tasks/importImages/omezarr/ccid_sync.jl:48,91` | versioned_set_field! filepath / imChannelNames | (value_name, filepath+channels) |
| `app/src/tasks/editImages/zProject.jl:98-99` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/copyImage.jl:152-153` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/cropImage.jl:154-155` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/dtype.jl:68` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/editImages/bin.jl:114-115` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/flip.jl:63` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/editImages/resampleZ.jl:110-111` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/tProject.jl:92-93` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/editImages/register.jl:188-189` | ver.set filepath + imChannelNames | (VERSIONED_DEFAULT_VAL, filepath) |
| `app/src/tasks/cleanupImages/af_correct/run.jl:84` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/cleanupImages/denoise.jl:241` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/cleanupImages/smooth.jl:212` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/cleanupImages/stack_align.jl:187` | ver.set filepath | (out_value_name, filepath) |
| `app/src/tasks/cleanupImages/flow_register.jl:174` | ver.set filepath | (out_value_name, filepath) |
| `app/src/storage.jl:250-258` | ver.set filepath (delete) + imChannelNames | (value_name, filepath) |

Labels registry (writes `raw["labels"][<vn>]` = filenames):

| `app/src/segmentation.jl:84 register_label_files!` | commit_state! + labels write | (out_value_name, labels) |
| `app/src/tasks/segment/coastal.jl:206`, `cellpose.jl:222`, `ridges.jl:133` | call `register_label_files!` | (outputValueName, labels) |
| `app/src/tasks/segment/branching.jl:240` | commit_state! + `raw["branch_labels"][…]` | (outputValueName, branchLabels) |
| `app/src/tasks/segment/measure_labels.jl:118` | commit_state! + `raw["label_props"][…]` | (valueName, cellH5AD) |

## 3. Julia writers — `write_atomic` / `write_json_atomic` per-vn sidecars

| file:line | Helper | (vn, kind) |
|---|---|---|
| `app/src/gating/popmanager/persistence.jl:144` (`save_pop_map!`) | write_json_atomic to `gating/{vn}[__suffix].json` | (m.value_name, gating) |
| `app/src/tracking/track_correction.jl:383` | write_json_atomic to `corrections/{vn}.json` | (value_name, corrections) |
| `app/src/label_correction.jl:236` | write_json_atomic to `corrections/labels_{vn}.json` | (value_name, corrections) |
| `app/src/correction_staleness.jl:199` | write_json_atomic to `corrections/{vn}.staleness.json` | (value_name, corrections) |
| `app/src/qc.jl:133` (`write_qc`) | write_json_atomic to `qc/{fun}/{vn}.json` | (value_name, qc) — versioned per (run × vn) already, will need vN-scoping |
| `app/src/tasks/clustPops/cluster.jl:96` | write_json_atomic to `{props}.clustfeatures.json` | (props' vn, clustfeatures) |
| `app/src/tasks/behaviour/motif_discovery.jl:90` | write_json_atomic per-vn motif sidecar | (vn, sidecar) |
| `app/src/tasks/importImages/omezarr/calibration.jl:94,139` | write_json_atomic `.zattrs` + xml under `default/<vn>/…` | (value_name, image-attrs) |
| `app/src/model/image.jl:522, 645` | write_json_atomic `ccid.json` itself | **NOT vn-scoped by design (D7)** — but the writer must gain schema fields for the version list + latest pointer |

## 4. Python writers — atomic file writes on per-vn targets

`write_h5ad_atomic` — 5 production callers write to a `default/<vn>/...` target:

| file:line | Target |
|---|---|
| `python/cecelia/utils/label_props_utils.py:370` (`LabelPropsView.save`) | overwrites the existing `{vn}.h5ad` (or `{vn}__tracks.h5ad`, `{vn}__branch.h5ad`) |
| `python/cecelia/utils/spatial_utils.py:183` | writes `spatialGraph/{suffix}.h5ad` — pop-agnostic, not vn-scoped (see notes) |
| `python/cecelia/utils/tracking_utils.py:467` (`save_tracks_to_labels`) | writes `labelProps/{vn}__tracks.h5ad` (`self.props_path`) |
| `python/cecelia/utils/measure_utils.py:471` | writes `labelProps/{output_value_name}.h5ad` |
| `python/cecelia/utils/legacy_migrate.py:139` | one-shot legacy → h5ad, only reached from `importImages.migrate_legacy` |
| `app/src/tasks/segment/branching_run.py:419` | writes `labelProps/{vn}__branch.h5ad` |

`write_json_atomic` — per-(vn, run) QC/manifest sidecars (partial list, all in `app/src/tasks/**/*_run.py`):
`cleanupImages/{af_correct,denoise,drift_correct,flow_register,smooth,stack_align}_run.py`,
`editImages/register_run.py`, `segment/{cellpose,coastal,correct,branching,ridges,carry_over}_run.py`,
`clustPops/cluster_run.py`, `clustTracks/cluster_run.py`, `clustRegions/cluster_run.py`,
`spatialAnalysis/{cell_neighbours,cell_neighbour_stats,cell_contacts_mesh,cell_aggregates_mesh}_run.py`,
`opticalFlow/{train,train_support_denoise}_run.py`, `behaviour/motif_discovery_run.py`,
`exportImages/ome_tiff_run.py`, `importImages/{scan_legacy,migrate_legacy,peek_pyramid,probe_series,read_ims_time_interval,read_imagej_physical_size,saturation}_run.py`. Ones whose target ties to a vn (via `params['qcOutPath']` or `params['taskDir']` + `outputValueName`) will need the vN qualifier; the ones whose target is `qc/{fun}/{vn}.json` are already funneled through `qc.jl write_qc` on the Julia side (§3).

## 5. Julia readers — every path built from a `value_name`

| Helper | Source | Prod callsites (representative) |
|---|---|---|
| `label_props(img; value_name=…)` | `app/src/label_props.jl:63` | `anisotropy.jl:105,113,171`; `cell_cards.jl:78`; `tasks/behaviour/motif_discovery.jl:68,198`; `tasks/tracking/correct.jl:146`; `tasks/segment/measure_labels.jl:106`; `tasks/clustTracks/cluster.jl:96`; `tasks/spatialAnalysis/cellContacts.jl:35,96`; `tasks/spatialAnalysis/detectAggregates.jl:69` |
| `img_filepath` | `app/src/model/image.jl:89` | `qc.jl:581`; `preview.jl:189`; `tasks/importImages/omezarr/ccid_sync.jl:135`; `tasks/spatialAnalysis/{aggregatesMeshes,contactsMeshes}.jl`; `api/src/routes/metadata.jl:201`; `api/src/cell_cards_api.jl:98` |
| `img_label_props_path` | `app/src/model/image.jl:110` | `spatial.jl:41`; `correction_staleness.jl:70`; `tasks/tracking/{track_measures,correct,bayesian_tracking}.jl`; `tasks/spatialAnalysis/{cellNeighbours,cellContacts,contactsMeshes,aggregatesMeshes}.jl`; `tasks/behaviour/{motif_discovery,hmm_transitions}.jl`; `tasks/clustPops/cluster.jl:130`; `gating/popmanager/{allow_list,clustering_colour,pop_df}.jl`; `ai/{behaviour_clusters,spatial,lineage}.jl`; `api/src/gating_api.jl:447,553,608,621,971`, `api/src/tracking_api.jl:14,53,346,408` |
| `img_track_props_path` | `image.jl:261` | `cell_cards.jl:53,75,201`; `correction_staleness.jl:53`; `tasks/tracking/track_measures.jl:414`; `tasks/behaviour/{motif_discovery,hmm_transitions}.jl`; `tasks/clustTracks/cluster.jl:95,148`; `gating/popmanager/{allow_list,clustering_colour}.jl`; `ai/{behaviour_clusters,lineage}.jl`; `api/src/gating_api.jl:115,122,447,524,553,608,971`; `api/src/cell_cards_api.jl:35,280` |
| `img_branch_props_path` | `image.jl:299` | `anisotropy.jl:102,151,168`; `tasks/segment/branching.jl:201`; `gating/popmanager/allow_list.jl:178,480` |
| `img_labels_path` / `img_labels_dir` | `image.jl:117,132` | `api/src/viewer_api.jl:282,286,562,583`; `api/src/overlay_author.jl:868`; `tasks/spatialAnalysis/{aggregatesMeshes,contactsMeshes}.jl:_label_zarr_path` |
| `img_branch_labels_path` / `img_branch_labels_dir` | `image.jl:329-340` | `tasks/segment/branching.jl:199` |
| `gating_path(task_dir, vn; pop_type=…)` | `app/src/gating/popmanager/persistence.jl:131` | `correction_staleness.jl:81`; `gating/popmanager/{allow_list,pop_df}.jl`; `ai/{observer_summary,lineage}.jl`; every read path around `save_pop_map!/load_pop_map` (§3) |
| `load_pop_map(img; value_name=…, pop_type=…)` | `gating/popmanager/clustering_colour.jl:245` | `analysis_board_spec.jl:108`; `cell_cards.jl:206`; `gating/popmanager/{picker,mixed_resolution,allow_list,pop_df,clustering_colour}.jl`; `ai/{observer_summary,lineage}.jl`; `tasks/{tracking/bayesian_tracking, segment/branching}.jl`; `api/src/gating_api.jl:141,178,317,345,375,380,656,848,1068`; `api/src/viewer_api.jl:844,921,977,999` |
| `resolve_value_name(img[, value_name])` | `image.jl:246` | called through every `img_*_path` caller that passes `value_name=nothing`; `img_cluster_suffixes:196` |
| `img_value_names` / `img_has_value_name` | `image.jl:209-212` | `analysis_board_spec.jl:107,115`; task validators; image payload builders |
| `versioned_get_field(raw, "filepath"|"imChannelNames", vn)` | `helpers.jl:58` | 20+ callers reading channel names / filepath; every image payload build |

## 6. Python readers — every path built from a `value_name`

| Helper | file:line | Prod callsites |
|---|---|---|
| `LabelPropsView(path)` constructor | `python/cecelia/utils/label_props_utils.py:22` | `python/cecelia/utils/{obs_utils,spatial_utils,clustering_utils,tracking_utils}.py`; `app/src/tasks/{clustPops/cluster_run.py, spatialAnalysis/cell_contacts_mesh_run.py, cell_aggregates_mesh_run.py}`; every test under `python/cecelia/tests/test_label_props_utils.py` |
| `LabelPropsView.label_props_filepath(value_name)` | `label_props_utils.py:392` | joins `labelProps/{vn}.h5ad` unconditionally — a legacy assumption (§10) |
| `zarr_utils.open_as_zarr` / `open_zarr` | `zarr_utils.py:317, 341` | 22 callers, all under `app/src/tasks/**_run.py` — reads `params['imPath']` / `params['labelPath']` that Julia built via `img_filepath` / `img_labels_path` |
| `zarr_utils.series_base` | `zarr_utils.py:541` | called internally by every open, plus by `ome_xml_utils.py:load_ome_xml`, `zarr_utils.read_axes/read_scale`, and NGFF-attr writes at lines 912/940/955/1032/1080/1220 |
| `zarr_utils.read_axes` / `read_scale` | `zarr_utils.py:573,695` | `measure_utils.py`; `correction_utils.py`; `segmentation_utils.py`; every task runner that reads a per-vn store's axes |
| `ome_xml_utils.load_ome_xml` / `read_pixel_unit` / `read_scale_from_ome_xml` / `read_time_increment` | `ome_xml_utils.py` | reached through `zarr_utils.read_*` for a bf2raw store; each hits `series_base` |
| `tracking_utils.Utils.props_path` | `tracking_utils.py:106` | joins `labelProps/{value_name}.h5ad` unconditionally — legacy assumption (§10) |
| `measure_utils.Measure` `out_path` | `measure_utils.py:470` | joins `{output_value_name}.h5ad` unconditionally (§10) |
| `segmentation_utils.SegmentationUtils._store_path` | `segmentation_utils.py:295` | joins `labels/{outputValueName}[_ma].zarr` unconditionally (§10) |

## 7. API-side readers — `resolve_image_version`

`api/src/image_geometry.jl:157` is the ONE api-layer VN resolver returning `(zarr_path, meta_dir, error)`. Callers:
`api/src/crop_api.jl:15,37`; `api/src/movie_rail.jl:23`; `api/src/optical_flow_api.jl:131`;
`api/src/viewer_api.jl:221,421,752,773,1281,1402`; `api/src/image_geometry.jl:456`.
Every HTTP surface that lets a client name a value_name goes through here.

## 8. ccid.json schema — producers, validators, versioned-field helpers

- **Struct** — `app/src/model/image.jl:22-40` (`CciaImage`: `filepath::Dict{String,String}`,
  `labels::Dict{String,Vector{String}}`, `label_props::Dict{String,String}`,
  `branch_labels::Dict{String,Vector{String}}`, `im_channel_names::Dict{String,Any}`).
- **Serialiser** — `app/src/model/image.jl:511-517` (payload dict), `save!` at :522, `read/normalise` in `helpers.jl:97` (`read_ccid_raw`).
- **Versioned-field helpers** — `app/src/helpers.jl:14-90` (`VERSIONED_ACTIVE_KEY`,
  `VERSIONED_DEFAULT_VAL`, `versioned_active`, `versioned_get`, `versioned_set!`,
  `versioned_get_field`, `versioned_set_field!`, `versioned_keys`).
- **Consumers** — every `img_*_path`, every `commit_state!`, `storage.jl:20-49,220-285` (reclaim),
  `analysis_board_spec.jl:107-115`, `gating/popmanager/*` (via `versioned_keys` on `label_props`),
  `api/src/routes/metadata.jl:201`.
- **Legacy migrations** — `python/cecelia/utils/legacy_migrate.py:332,483` builds the versioned-field shape.
- **P1 will add**: per-(vn, kind) version list + `latest` pointer. Schema definer to touch: `image.jl:511-517` + `helpers.jl` (new `versioned_versions_get`/`versioned_versions_set!` helpers).

## 9. Frontend vn pickers

**Widget-level (declarative):** `frontend/src/tasks/ParamRenderer.vue:140,170,393,768` renders
every `valueNameSelection` spec via `<select>`; `frontend/src/tasks/paramValues.ts:428-542`
resolves the field and preferred name (`isImageVersionField`, `preferredValueName`,
`VALUE_NAME_FIELDS`); `frontend/src/tasks/paramAdvisors` supplies `imageVersionAdvisory`.
The **advisor is registered under the TYPE**, so every version picker already inherits the
"not on the active version" warning.

**Ad-hoc `<select v-model="valueName">` — component-local:**

| file:line | Surface |
|---|---|
| `frontend/src/components/plots/GatingStrategyView.vue:232` | plot |
| `frontend/src/components/plots/TrackDiagnosticsView.vue:82-93,108-116` | plot |
| `frontend/src/components/plots/TrackPathsView.vue:98-101,135,161-166` | plot |
| `frontend/src/components/plots/TrackSchemeView.vue:120-124,262-269` | plot |
| `frontend/src/components/plots/FlowMetricsView.vue:204` | plot |
| `frontend/src/components/plots/FlowProbabilityView.vue:203` | plot |
| `frontend/src/components/plots/UmapView.vue:296` | plot |
| `frontend/src/components/plots/CellCardsView.vue`, `CellCardDetailPanel.vue` | plot |
| `frontend/src/components/plots/GateOverlay.vue`, `GateMontage.vue`, `GatePlotPanel.vue` (module) | gating |
| `frontend/src/components/canvas/SummaryPanel.vue:223` | canvas panel |
| `frontend/src/components/canvas/SummaryCanvas.vue`, `LayoutCanvas.vue`, `SeriesPicker.vue` | canvas |
| `frontend/src/components/canvas/PopulationManager.vue` | PopManager |
| `frontend/src/components/ViewerPanel.vue:97,131,510,806,820` | LabelView / viewer |
| `frontend/src/modules/ViewerWindow.vue` | popout viewer |
| `frontend/src/modules/batchmovies/BatchMoviesPanel.vue:650` | batch movies |
| `frontend/src/modules/ChainModule.vue:938` | ChainDesigner — reads sibling `valueNameSelection` field |
| `frontend/src/modules/SegmentModule.vue`, `SettingsModule.vue` | module pages |
| `frontend/src/modules/gate/{GatingCopyDialog,GatingPlots,GatePairsPanel,GatePlotPanel}.vue` | gating pages |
| `frontend/src/modules/cluster/{ClusterHeatmapPanel,ClusterHmmStatesPanel,ClusterHmmTransitionsPanel}.vue` | cluster views |
| `frontend/src/modules/animation/AnimationPanel.vue` | animation |
| `frontend/src/components/correction/CorrectionCockpit.vue` | correction cockpit |
| `frontend/src/components/CopyDialog.vue:33,95-98`, `CopyFunParamsModal.vue`, `DeleteImagesDialog.vue`, `ImagePickerModal.vue`, `ImageTable.vue`, `ImageFileActions.vue`, `ImageMetadataDialog.vue`, `ChainQcNode.vue`, `ChainLiveNode.vue`, `TaskPreviewControls.vue`, `MovieCompareControls.vue`, `CohortCheckButton.vue` | misc image/version selectors |

**Movie compare** already models the "versions + segmentations as chip sets" pattern
(`frontend/src/utils/movieCompare.ts` + `MovieCompareControls.vue`); P6's version chip could
extend this rather than introduce a second control.

## 10. Legacy `default/<vn>/` (single-directory) assumptions — the P4 migration surface

Every one of these joins a vn straight into the leaf, with no version segment. All will need to
resolve through `resolve_value_name(vn) -> (vn, version)` after P1.

| file:line | Pattern |
|---|---|
| `app/src/model/image.jl:110-113` (`img_label_props_path`) | `labelProps/{filename}` where filename is `img.label_props[vn]` |
| `app/src/model/image.jl:132-135` (`img_labels_path`) | `labels/{filename}` where filename is `first(img.labels[vn])` |
| `app/src/model/image.jl:261` (`img_track_props_path`) | `labelProps/{vn}__tracks.h5ad` |
| `app/src/model/image.jl:299` (`img_branch_props_path`) | `labelProps/{vn}__branch.h5ad` |
| `app/src/model/image.jl:336-340` (`img_branch_labels_path`) | `branchLabels/{filename}` |
| `app/src/model/image.jl:89-92` (`img_filepath`) | `0/{filename}` where filename is `img.filepath[vn]` |
| `app/src/gating/popmanager/persistence.jl:131-132` (`gating_path`) | `gating/{vn}[__suffix].json` |
| `app/src/gating/popmanager/clustering_colour.jl:13` (`_clustfeatures_path`) | replaces `.h5ad` → `.clustfeatures.json` — inherits the labelProps vn assumption |
| `app/src/tracking/track_correction.jl:343-345` (`corrections_path`) | `corrections/{vn}.json` |
| `app/src/label_correction.jl:191` (`_label_corrections_path`) | `corrections/labels_{vn}.json` |
| `app/src/correction_staleness.jl:158` | `corrections/{vn}.staleness.json` |
| `app/src/qc.jl:24` (`qc_path`) | `qc/{fun}/{vn}.json` |
| `app/src/qc_cohort.jl:201` (`cohort_qc_path`) | `qc/{fun}/{vn}.json` at set scope |
| `python/cecelia/utils/label_props_utils.py:392-394` (`label_props_filepath`) | `labelProps/{vn}.h5ad` |
| `python/cecelia/utils/tracking_utils.py:106` | `labelProps/{value_name}.h5ad` |
| `python/cecelia/utils/measure_utils.py:470` | `{output_value_name}.h5ad` |
| `python/cecelia/utils/segmentation_utils.py:295-299` (`_store_path`) | `labels/{outputValueName}[_ma].zarr` |
| `python/cecelia/utils/store_sweep.py:110-113` | scans ccid.json versioned dicts (needs to know the new (vn, version) shape) |

## 11. MCP tools referencing `value_name`

| file:line | Tool / method |
|---|---|
| `mcp/cecelia_mcp/server.py:158-196` | `get_cohort_qc(project_uid, set_uid, fun_name, value_name)` — the only tool schema with `value_name` today |
| `mcp/cecelia_mcp/client.py:255-261` | HTTP client `get_cohort_qc` (adds `valueName` query param) |
| `mcp/cecelia_mcp/guidance.py:100,117,127` | prose guidance on when to pass `value_name` — needs a note about version resolution after P6 |
| `mcp/cecelia_mcp/server.py:259` | `valueNameSelection` row in the param-widget catalogue — reference by tools that call `run_task` |

## 12. Chain planner refs (`docs/todo/VN_VERSIONING_PLAN.md` D3 pin site)

- **Frontend edge propagation** — `frontend/src/modules/ChainModule.vue:938` reads a sibling
  `valueNameSelection` (`normaliseField(p.field) === out.field`); `frontend/src/utils/taskOutput.ts`
  is the resolver. This is where P3 must inject the `@version` qualifier.
- **Julia composite snapshot** — `app/src/tasks/task/composite.jl:204-365` already does the closest
  thing today: it snapshots the ccid.json filepath keys BEFORE a step, then patches the
  intermediate `filepath` entries once `outputValueName` is set (mostly to prevent the intermediate
  from replacing an earlier version). P3 lands here.
- **Spec side** — `app/src/tasks/task/spec.jl:406-414` (`task_output_name`) resolves an
  `outputValueName` from a task's JSON spec; `app/src/tasks/task/validate.jl:263-293` names the
  eleven keys across six spellings that would need a version-aware validator variant.
- **Runlog / lineage viewer** — `frontend/src/utils/versionLineage.ts` already parses
  `{valueName, outputValueName}` pairs; would benefit from carrying `@version` per entry.

## Notes on synonyms and spelling

- Julia writes almost always call the axis `value_name`; Julia readers `value_name` in kwargs.
- Task JSON specs and typed Julia param structs use `valueName` and `outputValueName` (camelCase);
  a spec's producing key can also be `valueNameSuffix`, `graphSuffix`, `statsSuffix`, `colName`,
  `modelName` — **eleven param keys across six spellings** (`taskOutput.ts`). None are synonyms.
- Frontend uses `valueName` (camelCase) throughout. Storage helpers on the frontend spell it
  `valueName`. `VALUE_NAME_FIELDS` is the enum of `field` values a `valueNameSelection` may declare
  (`'filepaths' | 'labels' | 'spatialGraphs'`, per `paramValues.ts:443`).
- Python uses `value_name` (snake_case) consistently, plus `output_value_name` on writer objects.
- Chain-level references never use `input_value_name` as a literal — the input pin is just the
  `valueName` key on the downstream node's params, threaded through `ChainModule.nodeOutputValueName`
  on the frontend and `composite.jl` on Julia.

## Notes on scoping (not-versioned, per D7)

- `spatialGraph/{suffix}.h5ad` is pop-agnostic and keyed by run suffix, NOT vn — see
  `app/src/model/image.jl:143-156`. Skip in P2.
- `spatialStats/{suffix}.json` — same shape, skip.
- `plan.json` (correction plan) lives at `1/{image_uid}/plan.json` — per image, not per vn. Skip.
- `runlog.json`, `project.json`, `ccid.json` itself — project/image scope, not vn scope. Skip.
- Denoise `{name}.json` model manifest lives under `<config_dir>/models/denoiseModels/` — project scope, skip.
