# Cross-language boundaries — reference

> **Generated** by `scripts/architecture/boundaries_extract.py` — do not edit; run `pixi run arch-diagrams`.


Every touchpoint, row by row. The map and what does not line up: [`boundaries.md`](boundaries.md). Look things up here; do not read it through.

## 1. Julia → Python: `run_py`

*Keys read* is what the script takes from its params (`params["k"]`, `.get("k")`, `get_param(…, "k")`) — the Python side of the interface.

| Script | Launched from | Keys read |
|---|---|---|
| `tasks/behaviour/motif_discovery_run.py` | `app/src/tasks/behaviour/motif_discovery.jl:198` | `angle`, `hmmState`, `labels`, `numClasses`, `randomState`, `resolution`, `resultsOutPath`, `speed`, `suffix`, `topK`, `trackIds`, `ts` +3 |
| `tasks/cleanupImages/af_correct_run.py` | `app/src/tasks/cleanupImages/af_correct/run.jl:83` | `afCombinations`, `backgroundMethod`, `imCorrectionPath`, `imPath`, `qcOutPath` |
| `tasks/cleanupImages/denoise_run.py` | `app/src/tasks/cleanupImages/denoise.jl:209` | `batchSize`, `channels`, `channelsSkipped`, `imOutputPath`, `imPath`, `inputFrames`, `manifest`, `mode`, `modelPath`, `perChannelModels`, `qcOutPath` |
| `tasks/cleanupImages/drift_correct_run.py` | `app/src/tasks/cleanupImages/drift_correct.jl:245` | `driftChannel`, `driftEstimator`, `driftMaxAngle`, `driftMaxLag`, `driftNormalisation`, `driftPerPlane`, `driftSmoothSigma`, `driftZSmoothness`, `imCorrectionPath`, `imPath`, `partialTimepoint`, `qcOutPath` |
| `tasks/cleanupImages/flow_register_run.py` | `app/src/tasks/cleanupImages/flow_register.jl:139` | `imOutPath`, `imPath`, `maxShiftPx`, `pyrLevels`, `qcOutPath`, `referenceMode`, `registerChannel`, `structuralChannels`, `winsize` |
| `tasks/cleanupImages/smooth_run.py` | `app/src/tasks/cleanupImages/smooth.jl:191` | `bilateralColor`, `bilateralPolish`, `bilateralReach`, `channels`, `farnebackMaxShiftPx`, `imOutputPath`, `imPath`, `qcOutPath`, `restoreGain`, `spatialMethod`, `spatialSigma`, `temporalFrames` +1 |
| `tasks/cleanupImages/stack_align_run.py` | `app/src/tasks/cleanupImages/stack_align.jl:154` | `alignChannel`, `imAlignedPath`, `imPath`, `maxShiftPx`, `minConfidence`, `qcOutPath`, `referenceMode` |
| `tasks/clustPops/cluster_run.py` | `app/src/tasks/clustPops/cluster.jl:161` | `createUmap`, `featureCols`, `logBase`, `maxFraction`, `normaliseAxis`, `normalisePercentile`, `normalisePercentileBottom`, `normaliseToMedian`, `pagaThreshold`, `qcOutPath`, `randomState`, `resolution` +4 |
| `tasks/clustRegions/cluster_run.py` | `app/src/tasks/clustRegions/cluster.jl:178` | `basis`, `clusterMethod`, `compCols`, `createUmap`, `densityCol`, `diversityCol`, `graphPaths`, `graphSuffix`, `includeOther`, `numClusters`, `otherCol`, `pooling` +5 |
| `tasks/clustTracks/cluster_run.py` | `app/src/tasks/clustTracks/cluster.jl:191` | `X`, `axesOutPath`, `createUmap`, `featureCols`, `labels`, `logBase`, `maxFraction`, `normaliseAxis`, `normalisePercentile`, `normalisePercentileBottom`, `normaliseToMedian`, `pagaThreshold` +10 |
| `tasks/editImages/bin_run.py` | `app/src/tasks/editImages/bin.jl:115` | `factorX`, `factorY`, `imOutPath`, `imPath`, `op` |
| `tasks/editImages/cropImage_run.py` | `app/src/tasks/editImages/cropImage.jl:116` | `imOutPath`, `imPath`, `t0`, `t1`, `x0`, `x1`, `y0`, `y1`, `z0`, `z1` |
| `tasks/editImages/dtype_run.py` | `app/src/tasks/editImages/dtype.jl:65` | `dtype`, `imOutPath`, `imPath`, `rescale` |
| `tasks/editImages/flip_run.py` | `app/src/tasks/editImages/flip.jl:66` | `axis`, `imOutPath`, `imPath` |
| `tasks/editImages/register_run.py` | `app/src/tasks/editImages/register.jl:163` | `autoMask`, `doAffine2d`, `doAffine3d`, `doFftInitialization`, `expand`, `ignoreSpacing`, `imOutPath`, `imPaths`, `keepRegChannels`, `pixelSizeUm`, `qcOutPath`, `regChannels` +3 |
| `tasks/editImages/resampleZ_run.py` | `app/src/tasks/editImages/resampleZ.jl:110` | `imOutPath`, `imPath`, `order` |
| `tasks/editImages/splitTissues_run.py` | `app/src/tasks/editImages/splitTissues.jl:133`, `app/src/tasks/editImages/splitTissues.jl:97` | `channelIndices`, `imPath`, `mode`, `outputs`, `planPath` |
| `tasks/editImages/tProject_run.py` | `app/src/tasks/editImages/tProject.jl:89` | `imOutPath`, `imPath`, `op` |
| `tasks/editImages/zProject_run.py` | `app/src/tasks/editImages/zProject.jl:94` | `imOutPath`, `imPath`, `op` |
| `tasks/exportImages/ome_tiff_run.py` | `app/src/tasks/exportImages/ome_tiff.jl:174` | `calibration`, `channelNames`, `channels`, `imPath`, `outPath`, `qcOutPath`, `timepoint`, `zMip` |
| `tasks/importImages/fetch_immunemap_run.py` | `app/src/tasks/importImages/immunemapFetch.jl:54` | `destPath`, `md5`, `resultPath`, `sizeBytes`, `spacingXYZ`, `videoUid` |
| `tasks/importImages/ims_rebuild_attrs_run.py` | `app/src/tasks/importImages/omezarr/bf2raw_run.jl:47` | `imPath` |
| `tasks/importImages/migrate_legacy_run.py` | `app/src/tasks/importImages/migrateLegacy.jl:121` | `metaDir`, `mode`, `resultPath`, `rscript`, `sourceProjectDir`, `sourceUid`, `zeroDir` |
| `tasks/importImages/peek_pyramid_run.py` | `api/src/routes/image.jl:152` | `chunk`, `paths`, `resultPath`, `showinfBin`, `stillTarget` |
| `tasks/importImages/probe_series_run.py` | `api/src/routes/image.jl:102` | `imPath`, `maxPx`, `resultPath` |
| `tasks/importImages/read_acquisition_settings_run.py` | `app/src/tasks/importImages/omezarr/acquisition.jl:47` | `imPath`, `resultPath`, `series` |
| `tasks/importImages/read_imagej_physical_size_run.py` | `app/src/tasks/importImages/omezarr/task.jl:222` | `imPath`, `resultPath` |
| `tasks/importImages/read_ims_time_interval_run.py` | `app/src/tasks/importImages/omezarr/source_timing.jl:89` | `imPath`, `resultPath` |
| `tasks/importImages/saturation_run.py` | `app/src/tasks/importImages/omezarr/saturation.jl:23` | `imPath`, `notPhotonCounting`, `photonsUntestable`, `resultPath` |
| `tasks/importImages/scan_immunemap_run.py` | `api/src/routes/immunemap.jl:21` | `experiment`, `resultPath` |
| `tasks/importImages/scan_legacy_run.py` | `api/src/routes/image.jl:186` | `imageUids`, `resultPath`, `rscript`, `sourceProjectDir` |
| `tasks/importImages/trim_timepoints_run.py` | `app/src/tasks/importImages/omezarr/partial_timepoint.jl:36` | `imPath`, `resultPath`, `sizeT` |
| `tasks/opticalFlow/train_run.py` | `app/src/tasks/opticalFlow/train.jl:293` | `channelName`, `cropSize`, `cumulativeWindow`, `droppedMetrics`, `embeddingDim`, `epochs`, `foregroundBlurSigma`, `foregroundBoundaryWeight`, `foregroundWeight`, `intensityWeight`, `maxFrames`, `modelPath` +13 |
| `tasks/opticalFlow/train_support_denoise_run.py` | `app/src/tasks/opticalFlow/train_support_denoise.jl:237` | `batchSize`, `blindConvChannels`, `bundleDir`, `channelNames`, `depth`, `earlyStop`, `epochs`, `inputFrames`, `learningRate`, `midChannels`, `midZOnly`, `minLossDelta` +10 |
| `tasks/segment/af_cells_run.py` | `app/src/tasks/segment/af_cells.jl:141` | `exclude`, `groups`, `propsPath`, `qcOutPath` |
| `tasks/segment/branching_run.py` | `app/src/tasks/segment/branching.jl:229` | `anisotropyBoxSize`, `anisotropyBoxUm`, `anisotropySource`, `branchLabelsOutPath`, `branchPropsOutPath`, `calcAnisotropy`, `calcFlattened`, `fibreChannels`, `flattenBranching`, `imPath`, `integrateTime`, `integrateTimeMode` +10 |
| `tasks/segment/carry_over_run.py` | `app/src/tasks/segment/carry_over.jl:158`, `app/src/tasks/segment/carry_over.jl:98` | `phase`, `resultFile`, `snapshotFile`, `taskDir`, `valueName` |
| `tasks/segment/cellpose_run.py` | `app/src/tasks/segment/cellpose.jl:221` | `imPath`, `models`, `qcOutPath` |
| `tasks/segment/coastal_run.py` | `app/src/tasks/segment/coastal.jl:183` | `imPath`, `models`, `qcOutPath` |
| `tasks/segment/correct_run.py` | `app/src/tasks/segment/correct.jl:181` | `imPath`, `labelsPath`, `ops`, `resultFile` |
| `tasks/segment/import_labels_run.py` | `app/src/tasks/segment/import_labels.jl:109` | `imPath`, `matchIds`, `matchThreshold`, `outPaths`, `qcOutPath`, `sources` |
| `tasks/segment/measure_labels_run.py` | `app/src/tasks/segment/measure_labels.jl:81` | `imPath`, `labelDir`, `labelFiles`, `nThreads`, `outputValueName` |
| `tasks/segment/ridges_run.py` | `app/src/tasks/segment/ridges.jl:123` | `channelIndex`, `darkRidges`, `filter`, `imPath`, `labelsOutPath`, `minSizePx`, `perZ`, `qcOutPath`, `sigmaMax`, `sigmaMin`, `threshold` |
| `tasks/spatialAnalysis/cell_aggregates_mesh_run.py` | `app/src/tasks/spatialAnalysis/aggregatesMeshes.jl:61` | `aggregateIdCol`, `imPath`, `isAggregateCol`, `labelPath`, `labels`, `maxClusterDist`, `minCells`, `physicalSizes`, `popType`, `propsPath`, `qcOutPath` |
| `tasks/spatialAnalysis/cell_contacts_mesh_run.py` | `app/src/tasks/spatialAnalysis/contactsMeshes.jl:95` | `aLabelPath`, `aLabels`, `bLabelPath`, `checkContained`, `imPath`, `maxContactDist`, `physicalSizes`, `popType`, `propsPath`, `qcOutPath`, `targets` |
| `tasks/spatialAnalysis/cell_neighbour_stats_run.py` | `app/src/tasks/spatialAnalysis/neighbourStats.jl:147` | `basis`, `graphPath`, `graphSuffix`, `nPermutations`, `qcOutPath`, `randomState`, `segments`, `shufflePops`, `statsPath` |
| `tasks/spatialAnalysis/cell_neighbours_run.py` | `app/src/tasks/spatialAnalysis/cellNeighbours.jl:119` | `graphPath`, `nNeighbours`, `neighbourMethod`, `neighbourRadius`, `perTimepoint`, `physicalSizes`, `qcOutPath`, `segments`, `windowSize` |
| `tasks/spatialAnalysis/roi_discovery_run.py` | `app/src/tasks/spatialAnalysis/roiDiscovery.jl:114` | `groupBy`, `imPath`, `labels`, `labelsPath`, `maxRois`, `mergeGap`, `minObjects`, `propsPath`, `qcOutPath`, `rankBy`, `readCols`, `reviewLabels` +4 |
| `tasks/spatialAnalysis/spatial_pattern_run.py` | `app/src/tasks/spatialAnalysis/spatialPattern.jl:110` | `graphPath`, `graphSuffix`, `nPermutations`, `nRadii`, `outPath`, `propsPaths`, `qcOutPath`, `rMax`, `randomState`, `targets` |
| `tasks/tracking/bayesian_tracking_run.py` | `app/src/tasks/tracking/bayesian_tracking_steps.jl:24` | — |
| `tasks/tracking/delete_track_source_run.py` | `app/src/analysis_runs.jl:232` | `propsPath`, `trackSource` |
| `tasks/tracking/track_props_run.py` | `app/src/tasks/tracking/track_measures.jl:315` | — |
| `utils/model_weights.py` | `api/src/system_api.jl:186` | `models` |
| `utils/store_sweep.py` | `app/src/maintenance.jl:57` | `apply`, `resultPath`, `root` |
| `writers/render_animation_run.py` | `api/src/movie_render.jl:213`, `api/src/movie_render.jl:280` +1 | `canvasH`, `canvasW`, `fps`, `labelColouring`, `labelColours`, `labelContourPx`, `labelFiles`, `labelOpacity`, `labelsPath`, `outPath`, `outPaths`, `overlaySizeUnit` +10 |
| `writers/render_figure.py` | `app/src/figures.jl:103` | `states` |
| `writers/stitch_movies_run.py` | `api/src/movie_rail.jl:835` | `fps`, `labels`, `layout`, `outPath`, `sources`, `titleCard` |
| `writers/write_categorical_obs_run.py` | `app/src/label_props.jl:1175` | — |

Call sites with a computed script name (not resolved): `app/src/maintenance.jl:88`, `app/src/model/project.jl:412`, `app/src/py_runner.jl:188`

## 2. HTTP: `/api` routes by area

Caller files per side; matching rules from `scripts/judge/coherence.py`, comments excluded.

| Area | Routes | Julia side | Vue | MCP | Python | preview | No caller |
|---|---|---|---|---|---|---|---|
| `/api/viewer` | 42 | `api/src/captures_api.jl`, `api/src/landscape_api.jl` +6 | 14 | 1 |  |  |  |
| `/api/gating` | 29 | `api/src/gating_api.jl`, `api/src/gating_views_api.jl` | 19 | 2 | 1 |  |  |
| `/api/images` | 24 | `api/src/image_geometry.jl`, `api/src/routes/image.jl` +4 | 18 | 2 |  |  |  |
| `/api/kiwi` | 20 | `api/src/kiwi_api.jl`, `api/src/kiwi_profile_api.jl` +1 | 3 |  |  |  |  |
| `/api/notebooks` | 17 | `api/src/notebooks_api.jl` | 4 | 1 |  |  |  |
| `/api/projects` | 13 | `api/src/routes/project.jl`, `api/src/server.jl` | 5 | 2 |  |  |  |
| `/api/blackboard` | 12 | `api/src/blackboard_api.jl`, `api/src/blackboard_run_review.jl` | 1 | 1 |  |  |  |
| `/api/chains` | 12 | `api/src/routes/chain.jl` | 3 | 2 |  |  |  |
| `/api/analysis` | 11 | `api/src/routes/chain.jl`, `api/src/routes/image.jl` | 4 | 1 |  |  |  |
| `/api/tasks` | 11 | `api/src/routes.jl`, `api/src/routes/image.jl` +1 | 11 | 2 |  |  |  |
| `/api/storage` | 8 | `api/src/routes.jl`, `api/src/storage_api.jl` | 1 |  |  |  |  |
| `/api/hpc` | 7 | `api/src/hpc_api.jl` | 1 |  |  |  |  |
| `/api/sets` | 7 | `api/src/routes.jl`, `api/src/routes/experiment_design.jl` +1 | 3 | 2 |  |  |  |
| `/api/config` | 6 | `api/src/routes.jl` | 2 |  |  |  |  |
| `/api/import` | 6 | `api/src/routes/image.jl`, `api/src/routes/immunemap.jl` | 4 |  |  |  |  |
| `/api/observer` | 6 | `api/src/observer_api.jl`, `api/src/routes/image.jl` | 2 | 1 |  |  |  |
| `/api/tracking` | 6 | `api/src/tracking_api.jl` | 6 |  |  |  |  |
| `/api/claim` | 5 | `api/src/claim_review.jl`, `api/src/routes/claim.jl` | 1 | 1 |  |  |  |
| `/api/plots` | 5 | `api/src/gating_api.jl`, `api/src/plotting_api.jl` | 7 | 1 |  |  |  |
| `/api/app` | 4 | `api/src/app_api.jl` | 1 |  |  |  |  |
| `/api/board-assets` | 4 | `api/src/routes/project.jl`, `api/src/server.jl` | 6 |  |  |  |  |
| `/api/lablog` | 4 | `api/src/routes/lab_log.jl` | 2 | 1 |  |  |  |
| `/api/movies` | 4 | `api/src/movies_api.jl`, `api/src/routes.jl` +1 | 7 | 1 |  |  |  |
| `/api/optical-flow` | 4 | `api/src/optical_flow_api.jl` | 4 |  |  |  |  |
| `/api/preview` | 4 | `api/src/preview_api.jl` | 1 |  |  |  |  |
| `/api/qc` | 4 | `api/src/routes/image.jl` | 1 | 1 |  |  |  |
| `/api/system` | 4 | `api/src/system_api.jl` | 2 |  |  |  |  |
| `/api/acqcheck` | 3 | `api/src/acq_review.jl`, `api/src/routes/acq_check.jl` | 1 |  |  |  |  |
| `/api/denoise` | 3 | `api/src/denoise_api.jl` | 1 |  |  |  |  |
| `/api/figures` | 3 | `api/src/figures_api.jl` | 4 | 1 |  |  |  |
| `/api/plugins` | 3 | `api/src/plugins_api.jl` | 1 |  |  |  |  |
| `/api/profiles` | 3 | `api/src/routes.jl` | 1 |  |  |  |  |
| `/api/push` | 3 | `api/src/push_api.jl` | 1 | 1 |  |  |  |
| `/api/repl` | 3 | `api/src/repl_api.jl`, `api/src/routes/image.jl` | 1 | 1 |  |  |  |
| `/api/runner` | 3 | `api/src/runner_api.jl` | 3 |  |  |  |  |
| `/api/setup` | 3 | `api/src/setup_api.jl` | 1 |  |  |  |  |
| `/api/update` | 3 | `api/src/update_api.jl` | 1 |  |  |  |  |
| `/api/versions` | 3 | `api/src/routes.jl`, `api/src/storage_api.jl` | 2 |  |  |  |  |
| `/api/diagnostics` | 2 | `api/src/repl_api.jl` | 4 |  |  |  |  |
| `/api/labels` | 2 | `api/src/labels_api.jl`, `api/src/labels_by_category_api.jl` | 1 | 1 |  |  |  |
| `/api/pools` | 2 | `api/src/routes.jl` | 3 |  |  |  |  |
| `/api/profile` | 2 | `api/src/profile_settings_api.jl` | 1 |  |  |  |  |
| `/api/rois` | 2 | `api/src/rois_api.jl` | 1 |  |  |  |  |
| `/api/auth` | 1 | `api/src/server.jl` |  |  |  |  |  |
| `/api/boards` | 1 | `api/src/routes/project.jl` |  | 1 |  |  |  |
| `/api/cell_cards` | 1 | `api/src/cell_cards_api.jl` | 1 |  |  |  |  |
| `/api/data-source` | 1 | `api/src/routes/data_source.jl` | 1 |  |  |  |  |
| `/api/fs` | 1 | `api/src/routes.jl` | 1 |  |  |  |  |
| `/api/health` | 1 | `api/src/server.jl` | 2 |  |  |  |  |
| `/api/hmm_state_cards` | 1 | `api/src/hmm_state_cards_api.jl` | 1 |  |  |  |  |
| `/api/logs` | 1 | `api/src/server.jl` | 1 | 1 |  |  |  |
| `/api/maintenance` | 1 | `api/src/maintenance_api.jl` | 1 |  |  |  |  |
| `/api/mcp` | 1 | `api/src/routes/image.jl` | 1 |  |  |  |  |
| `/api/motif_cards` | 1 | `api/src/motif_cards_api.jl` | 1 |  |  |  |  |
| `/api/objects` | 1 | `api/src/routes/image.jl` |  | 1 |  |  |  |
| `/api/plot_data` | 1 | `api/src/plotting_api.jl` | 5 |  |  |  |  |
| `/api/version` | 1 | `api/src/update_api.jl` |  | 1 |  |  |  |

Routes with no caller on purpose (`ARCH-EXEMPT:` on the route's line):

- `/api/kiwi/refs/cells` — kept for MCP / overlay consumers (docs/API.md)
- `/api/system/weights` — manual status of the model-weights job (curl / script); installer, boot and preview start it
- `/api/system/weights/fetch` — manual re-fetch of missing weights, e.g. after an offline first start
- `/api/viewer/record-test` — smoke test of the offline renderer, called from api/test

## 3. WebSocket `/ws`

### Browser → Julia

| Type | Sent from | Handled in |
|---|---|---|
| `chain:cancel` | `frontend/src/modules/TasksModule.vue`, `frontend/src/tasks/TaskList.vue` | `api/src/sockets.jl` |
| `chain:continue` | — | `api/src/sockets.jl` |
| `chain:run` | `frontend/src/modules/ChainModule.vue`, `frontend/src/tasks/TaskRunner.vue` | `api/src/sockets.jl` |
| `maintenance:cancel` | `frontend/src/components/SettingsModal.vue` | `api/src/sockets.jl` |
| `maintenance:run` | `frontend/src/components/SettingsModal.vue` | `api/src/sockets.jl` |
| `movie:batch` | `frontend/src/modules/batchmovies/BatchMoviesPanel.vue` | `api/src/sockets.jl` |
| `movie:record` | `frontend/src/components/QuickMovie.vue`, `frontend/src/modules/animation/AnimationPanel.vue` | `api/src/sockets.jl` |
| `ping` | — | `api/src/sockets.jl` |
| `project:export` | `frontend/src/components/ProjectPanel.vue` | `api/src/sockets.jl` |
| `project:import` | `frontend/src/components/ProjectPanel.vue` | `api/src/sockets.jl` |
| `task:cancel` | `frontend/src/components/ProjectPanel.vue`, `frontend/src/modules/TasksModule.vue`, `frontend/src/tasks/TaskList.vue` | `api/src/sockets.jl` |
| `task:restart` | `frontend/src/modules/TasksModule.vue`, `frontend/src/tasks/TaskList.vue` | `api/src/sockets.jl` |
| `task:run` | `frontend/src/components/CopyDialog.vue`, `frontend/src/composables/useVwLensQuery.ts`, `frontend/src/lib/trackOpsRun.ts` +3 | `api/src/sockets.jl` |
| `viewer:hello` | — | `api/src/sockets.jl` |

### Julia → browser

| Type | Emitted in | Handled in |
|---|---|---|
| `blackboard:changed` | `api/src/blackboard_api.jl`, `api/src/blackboard_run_review.jl` | `frontend/src/stores/ws.ts` |
| `boards:changed` | `api/src/routes/project.jl` | `frontend/src/stores/analysisLayout.ts` |
| `captures:changed` | `api/src/captures_api.jl` | `frontend/src/stores/ws.ts` |
| `chain:continue:result` | `api/src/sockets.jl` | `frontend/src/stores/ws.ts` |
| `chain:log` | `api/src/sockets.jl`, `app/src/runner/server.jl` | `frontend/src/stores/ws.ts` |
| `chain:node:done` | `api/src/runner_api.jl`, `app/src/runner/chain_frames.jl` | `frontend/src/composables/useTaskDefs.ts`, `frontend/src/stores/ws.ts` |
| `chain:node:failed` | `api/src/runner_api.jl`, `app/src/runner/chain_frames.jl` | `frontend/src/stores/ws.ts` |
| `chain:node:queued` | `app/src/runner/chain_frames.jl` | `frontend/src/stores/ws.ts` |
| `chain:node:running` | `app/src/runner/chain_frames.jl` | `frontend/src/stores/ws.ts` |
| `chain:node:waiting` | `api/src/runner_api.jl`, `app/src/runner/chain_frames.jl` | `frontend/src/stores/ws.ts` |
| `chain:run:done` | `api/src/sockets.jl`, `app/src/runner/server.jl` | `frontend/src/stores/ws.ts` |
| `chain:run:failed` | `api/src/sockets.jl`, `app/src/runner/server.jl` | `frontend/src/stores/ws.ts` |
| `chain:run:started` | `api/src/sockets.jl` | `frontend/src/stores/ws.ts` |
| `chains_updated` | `api/src/routes.jl` | `frontend/src/modules/ChainModule.vue` |
| `error` | `api/src/acq_review.jl`, `api/src/claim_review.jl` +19 | `frontend/src/lib/webgpu/compileShader.ts`, `frontend/src/stores/ws.ts`, `frontend/src/utils/webgpuProbe.ts` |
| `gating:pop-tree` | `api/src/gating_api.jl` | `frontend/src/composables/useSummaryData.ts`, `frontend/src/stores/gating.ts` |
| `kiwi:done` | `api/src/kiwi_api.jl` | `frontend/src/stores/kiwi.ts` |
| `kiwi:step` | `api/src/kiwi_api.jl` | `frontend/src/components/AcqCheckDialog.vue`, `frontend/src/stores/kiwi.ts` |
| `lab_log_updated` | `api/src/routes/image.jl`, `api/src/routes/lab_log.jl` | `frontend/src/stores/ws.ts` |
| `movies:changed` | `api/src/figures_api.jl` | `frontend/src/stores/ws.ts` |
| `notebooks_changed` | `api/src/notebooks_api.jl` | `frontend/src/components/NotebookTable.vue` |
| `plot:request` | `api/src/viewer_request.jl` | `frontend/src/stores/canvasPanelExports.ts` |
| `pong` | `api/src/sockets.jl` | `frontend/src/stores/ws.ts` |
| `push:sent` | `api/src/push_writer.jl` | `frontend/src/stores/ws.ts` |
| `push_target:changed` | `api/src/push_api.jl`, `api/src/push_writer.jl` | `frontend/src/stores/ws.ts` |
| `rois:changed` | `api/src/rois_api.jl` | `frontend/src/components/QuickMovie.vue`, `frontend/src/components/RoiSection.vue` |
| `runner:log` | `app/src/runner/server.jl` | `api/src/runner_api.jl` |
| `server:log` | `api/src/server.jl` | `frontend/src/stores/ws.ts` |
| `task:cancel:ack` | `api/src/sockets.jl` | `frontend/src/stores/ws.ts` |
| `task:hpc` | `api/src/runner_api.jl`, `api/src/sockets.jl` +1 | `frontend/src/stores/ws.ts` |
| `task:log` | `api/src/runner_api.jl`, `api/src/sockets.jl` +1 | `frontend/src/stores/ws.ts` |
| `task:progress` | `api/src/runner_api.jl`, `api/src/sockets.jl` +2 | `frontend/src/composables/useLivePreview.ts`, `frontend/src/stores/ws.ts` |
| `task:result` | `api/src/runner_api.jl`, `api/src/sockets.jl` +1 | `frontend/src/composables/useViewerAutoUpdate.ts`, `frontend/src/stores/ws.ts` |
| `task:status` | `api/src/runner_api.jl`, `api/src/sockets.jl` +1 | `frontend/src/composables/useTaskDefs.ts`, `frontend/src/composables/useViewerAutoUpdate.ts`, `frontend/src/composables/useVwLensQuery.ts` +2 |
| `update:progress` | `api/src/update_api.jl` | `frontend/src/stores/ws.ts` |
| `update:revert-staged` | `api/src/update_api.jl` | `frontend/src/stores/ws.ts` |
| `update:staged` | `api/src/update_api.jl` | `frontend/src/stores/ws.ts` |
| `viewer:landscape-request` | `api/src/landscape_api.jl` | `frontend/src/modules/ViewerWindow.vue` |
| `viewer:mark` | `api/src/marks_api.jl` | `frontend/src/stores/ws.ts` |
| `viewer:navigate` | `api/src/viewer_nav_api.jl` | `frontend/src/stores/ws.ts` |
| `viewer:request` | `api/src/viewer_request.jl` | `frontend/src/modules/ViewerWindow.vue` |
| `viewer:seek` | `api/src/viewer_nav_api.jl` | `frontend/src/stores/ws.ts` |

## 4. Shared data files — where each is named

| File | Path | Named from |
|---|---|---|
| `constants.json` | `frontend/src/lib/webgpu/shaders/constants.json` | Julia: `api/src/shader_constants.jl`; Python: `python/cecelia/utils/wgpu_host.py`, `python/cecelia/utils/wgsl_utils.py`; TS/Vue: `frontend/src/lib/webgpu/mipShader.ts`, `frontend/src/lib/webgpu/shaderSource.ts` +6 |
| `figureStyle.json` | `frontend/src/lib/figureStyle.json` | Julia: `app/src/figures.jl`; Python: `python/cecelia/writers/render_figure.py`; TS/Vue: `frontend/src/components/FigureStyleEditor.vue`, `frontend/src/utils/figureStyle.ts` |
| `golden.json` | `frontend/src/lib/webgpu/shaders/golden.json` | Python: `python/cecelia/utils/wgpu_host.py`, `python/cecelia/utils/wgsl_utils.py`; TS/Vue: `frontend/src/lib/webgpu/wgslExpand.ts` |
| `guides.json` | `mcp/cecelia_mcp/guides.json` | Python: `mcp/cecelia_mcp/server.py`, `scripts/agent_eval/guide_run.py` +1; TS/Vue: `frontend/src/lib/claudeOverview.ts`, `frontend/src/lib/guides/guideText.ts` +1 |
| `kiwiRef.schema.json` | `frontend/src/lib/kiwiRef.schema.json` | Julia: `api/src/kiwi_refs.jl`, `api/src/kiwi_turn.jl` +1; TS/Vue: `frontend/src/utils/kiwiRef.ts` |
| `label_palette.json` | `frontend/src/lib/webgpu/shaders/label_palette.json` | Julia: `api/src/overlay_author.jl`; Python: `python/cecelia/utils/wgpu_host.py` |
| `lens.wgsl` | `frontend/src/lib/webgpu/shaders/lens.wgsl` | Julia: `api/src/movie_render.jl`; Python: `python/cecelia/writers/render_animation_run.py`; TS/Vue: `frontend/src/modules/ViewerWindow.vue`, `frontend/src/utils/batchMovie.ts` +2 |
| `maskPopTable.parity.json` | `frontend/src/utils/maskPopTable.parity.json` | Julia: `api/src/overlay_author.jl`; TS/Vue: `frontend/src/utils/maskPopTable.ts` |
| `mip.wgsl` | `frontend/src/lib/webgpu/shaders/mip.wgsl` | Python: `python/cecelia/utils/wgpu_host.py`, `python/cecelia/writers/render_animation_run.py`; TS/Vue: `frontend/src/lib/webgpu/mipShader.ts`, `frontend/src/utils/viewerLabels.ts` |
| `mip_common.wgsl` | `frontend/src/lib/webgpu/shaders/mip_common.wgsl` | Julia: `api/src/movie_render.jl`; TS/Vue: `frontend/src/components/AxesGizmo.vue`, `frontend/src/lib/webgpu/brickVolumeRenderer.ts` +3 |
| `mip_points.wgsl` | `frontend/src/lib/webgpu/shaders/mip_points.wgsl` | Python: `python/cecelia/utils/wgpu_host.py`; TS/Vue: `frontend/src/lib/webgpu/mipShader.ts` |
| `mip_segments.wgsl` | `frontend/src/lib/webgpu/shaders/mip_segments.wgsl` | Python: `python/cecelia/utils/wgpu_host.py`; TS/Vue: `frontend/src/lib/webgpu/mipShader.ts` |
| `overlay_quads.wgsl` | `frontend/src/lib/webgpu/shaders/overlay_quads.wgsl` | Julia: `api/src/overlay_author.jl`, `api/src/shader_constants.jl`; Python: `python/cecelia/writers/render_animation_run.py`; TS/Vue: `frontend/src/lib/webgpu/brickShader.ts`, `frontend/src/lib/webgpu/overlayPasses.ts` +3 |
| `palettes.json` | `frontend/src/plots/palettes.json` | Julia: `api/src/gate_plot_render.jl`, `api/src/movie_render.jl` +3; TS/Vue: `frontend/src/components/AcquisitionLightPath.vue`, `frontend/src/components/LightPathSourceGlyph.vue` +6 |
| `popTypeRegistry.json` | `frontend/src/plots/popTypeRegistry.json` | Julia: `app/src/gating/popmanager/registry.jl`; TS/Vue: `frontend/src/plots/popTypeRegistry.ts` |
| `tools.json` | `mcp/cecelia_mcp/tools.json` | Python: `mcp/cecelia_mcp/autonomous_server.py`, `mcp/cecelia_mcp/server.py` +2; TS/Vue: `frontend/src/lib/claudeTools.ts`, `frontend/src/lib/guides/mapData.ts` |
| `track_measures.json` | `app/src/plotDefinitions/track_measures.json` | Julia: `app/src/tasks/tracking/track_measures.jl`; TS/Vue: `frontend/src/lib/guides/taskGuides.ts` |
| `uniforms.json` | `frontend/src/lib/webgpu/shaders/uniforms.json` | Python: `python/cecelia/utils/wgpu_host.py`, `python/cecelia/utils/wgsl_utils.py`; TS/Vue: `frontend/src/lib/webgpu/brickShader.ts`, `frontend/src/lib/webgpu/mipShader.ts` +5 |

## 5. On-disk formats — who names them

Files per language that name each kind of project file (a reference, not necessarily a read or a write). Readers and writers: `docs/OBJECTMODEL.md`, `docs/DATAMODEL.md`.

| File kind | Julia | Python | TS/Vue |
|---|---|---|---|
| project.json | 12 |  |  |
| ccid.json (image / set record) | 42 | 16 | 17 |
| OME-ZARR (.zarr) | 36 | 32 | 10 |
| .h5ad (cell data) | 32 | 22 | 6 |
| gating JSON | 28 |  | 4 |
| run log | 40 | 5 | 23 |
