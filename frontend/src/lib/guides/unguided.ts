// Features with no guide — listed, not invisible (docs/todo/FEATURE_MATURITY_PLAN.md, Decision 4).
// Each carries the same maturity record a guide does; `maturity.covers` is the feature's key: the
// sidebar routes and task fun_names it stands for (empty for a feature that is neither, e.g. the
// viewer panel). This list is the gap list: either a guide gets written, or the feature isn't ready.
//
// `utils/maturity.test.ts` fails when a sidebar page or a registered task is in neither a guide nor
// here. Coming soon is NOT written here — it is the task JSON `comingSoon` / nav `soon` flag, and a
// record covering a flagged feature fails the test (one source per fact).
//
// Seeded from docs/todo/FEATURE_MATURITY_TIERS.md; rows proposed there for Tested are Beta + REVIEW.

import type { Maturity } from './types'
import { EXEMPT, IV2P, REVIEW, todo } from './maturityRecords'

export interface UnguidedFeature {
  id: string
  title: string
  group: string                 // one of GROUP_ORDER (lib/guides/index.ts)
  maturity: Maturity
}

const covering = (covers: string[], m: Maturity): Maturity => ({ ...m, covers })

export const UNGUIDED: UnguidedFeature[] = [
  // ── Start: the app itself ──
  { id: 'viewer', title: 'Viewer window and controls', group: 'Start', maturity: {
    tier: 'beta', testedOn: IV2P,
    handCheck: 'docs/PROVENANCE.md → WebGPU viewer / shader rendering',
    evidence: [
      { path: 'api/test/suite/viewer_slab_and_meta.jl',
        test: 'API: viewer slab — the response is written from the pool, byte-exact under concurrency',
        failsIf: 'the voxels the viewer receives differ from the store' },
      { path: 'api/test/suite/version_and_render.jl', test: 'API: sRGB encode matches the browser viewer\'s canvas gamma',
        failsIf: 'a movie frame’s brightness differs from the viewer’s' },
    ],
    next: [
      todo('A picking test derived from the shader’s pan sign, not by hand'),
      todo('Viewer region selection filters by time'),
    ],
  } },
  { id: 'project-bundle', title: 'Project export and import', group: 'Start', maturity: {
    tier: 'experimental', testedOn: '—',
    next: [todo('An import round-trip asserts identical data')],
  } },
  { id: 'system-install', title: 'Shared / system install', group: 'Start', maturity: {
    tier: 'experimental', testedOn: 'CI on Linux, macOS and Windows',
    next: [todo('Verify the setup wizard’s system-scope path')],
  } },
  { id: 'plugins', title: 'Custom modules and plugins', group: 'Start', maturity: {
    tier: 'experimental', testedOn: '—',
  } },

  // ── Data ──
  { id: 'other-preprocessing', title: 'Project, bin, resample, flip, bit depth', group: 'Data', maturity: covering(
    ['editImages.zProject', 'editImages.tProject', 'editImages.bin', 'editImages.resampleZ', 'editImages.flip', 'editImages.dtype'], {
      tier: 'experimental', testedOn: 'Synthetic',
      evidence: [
        { path: 'python/cecelia/tests/test_flip_runner.py', test: 'test_flip_mirrors_content_so_source_labels_no_longer_line_up',
          failsIf: 'a flip doesn’t mirror the pixels (flip only)' },
      ],
      next: [todo('A pixel test per op (max projection, bin mean)'), todo('A bit-depth test')],
    }) },
  { id: 'stack-align', title: 'Within-stack alignment', group: 'Data', maturity: covering(['cleanupImages.stackAlign'], {
    tier: 'experimental', testedOn: 'Synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_stack_alignment.py', test: 'test_recovers_a_known_planar_shift',
        failsIf: 'a known planar shift is not recovered' },
    ],
    next: [todo('Record a hand-check on a real stack')],
  }) },
  { id: 'flow-register', title: 'Flow-based registration', group: 'Data', maturity: covering(['cleanupImages.flowRegister'], {
    tier: 'experimental', testedOn: '—',
    next: [todo('A known-warp recovery test')],
  }) },
  { id: 'smooth', title: 'Smoothing', group: 'Data', maturity: covering(['cleanupImages.smooth'], {
    tier: 'experimental', testedOn: 'Synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_smooth_runner.py', test: 'test_gated_keeps_the_moving_feature_the_median_flattens',
        failsIf: 'gated smoothing erases motion' },
    ],
    next: [todo('Record a hand-check on real data')],
  }) },
  { id: 'denoise', title: 'Denoise with a trained model', group: 'Data', maturity: covering(['cleanupImages.denoise'], {
    tier: 'experimental', testedOn: '—',
    next: [todo('An output-quality check on a known-noise input')],
    needs: 'Paired low- and high-SNR acquisitions',
    datasetAsk: 'paired-snr',
  }) },
  { id: 'import-labels', title: 'Import external labels', group: 'Data', maturity: covering(
    ['segment.importLabels', 'segment.importLabelsMeasure'], {
      tier: 'experimental', testedOn: 'Synthetic',
      evidence: [
        { path: 'python/cecelia/tests/test_label_import.py', test: 'test_frame_offsets_only_where_ids_collide',
          failsIf: 'imported ids collide across frames' },
      ],
      next: [todo('Round trip: measuring imported labels equals measuring the source')],
    }) },
  { id: 'branching', title: 'Branching (skeleton) analysis', group: 'Data', maturity: covering(['segment.branching'], {
    tier: 'experimental', testedOn: 'Synthetic anisotropic fixture',
    evidence: [
      { path: 'python/cecelia/tests/test_branching_anisotropy.py', test: 'test_match_rank_z_mips_a_3d_channel_onto_a_2d_skeleton',
        failsIf: 'a 3D channel is sampled onto a 2D skeleton wrongly' },
    ],
    next: [todo('Record a hand-check on second-harmonic (SHG) data in the docs')],
  }) },
  { id: 'ridges', title: 'Ridge segmentation', group: 'Data', maturity: covering(['segment.ridges'], {
    tier: 'experimental', testedOn: 'Synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_ridges_runner.py', test: 'test_meijering_finds_two_ridges_per_frame',
        failsIf: 'two planted ridges aren’t found per frame' },
    ],
    next: [todo('A known-line synthetic test across all three filters')],
  }) },
  { id: 'migrate-legacy', title: 'Migrate legacy R projects', group: 'Data', maturity: covering(['importImages.migrateLegacy'], {
    tier: 'experimental', testedOn: 'Legacy projects from the R version (not recorded)',
    evidence: [
      { path: 'python/cecelia/tests/test_legacy_migrate.py', test: 'test_legacy_obsm_names_relabelled',
        failsIf: 'legacy track and centroid columns keep their old names' },
    ],
    next: [todo('A test that migrating fills the frame interval')],
  }) },
  { id: 'import-immunemap', title: 'Import from Immunemap', group: 'Data', maturity: covering(['importImages.immunemap', 'importImages.immunemapFetch'], {
    tier: 'experimental', testedOn: 'Immunemap experiment 63 (15 videos, one lab)',
    evidence: [
      { path: 'python/cecelia/tests/test_immunemap.py', test: 'test_resumes_after_a_dropped_connection',
        failsIf: 'a dropped download restarts instead of resuming' },
      { path: 'api/test/suite/import_immunemap.jl', test: 'API: /api/import/register-immunemap',
        failsIf: 'a registered video lacks its condition, channel labels or frame interval' },
    ],
    next: [todo('Import a second Immunemap experiment with a different microscope')],
  }) },
  { id: 'export-ome-tiff', title: 'Export as OME-TIFF', group: 'Data', maturity: covering(['exportImages.ome_tiff'], {
    tier: 'experimental', testedOn: 'Synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_ome_tiff_export.py', test: 'test_full_export_round_trips_every_voxel',
        failsIf: 'an exported voxel differs from the source' },
      { path: 'python/cecelia/tests/test_ome_tiff_export.py', test: 'test_full_export_carries_the_calibration_it_was_handed',
        failsIf: 'the export’s pixel size differs from the image’s' },
    ],
    next: [todo('Record opening an export in Fiji')],
  }) },

  // ── Populations ──
  { id: 'track-measures', title: 'Track measures', group: 'Populations', maturity: covering(['tracking.track_measures'], {
    tier: 'beta', testedOn: 'Hand-built and analytic tracks; intravital movies',
    handCheck: 'docs/PROVENANCE.md → celltrackR track measures',
    evidence: [
      { path: 'app/test/suite/labelprops.jl', test: 'Track measures (celltrackR golden)',
        failsIf: 'speed, straightness etc. deviate from celltrackR 1.2.2' },
      { path: 'app/test/suite/tracking.jl', test: 'diagnostics golden — MSD matches celltrackR squareDisplacement',
        failsIf: 'mean squared displacement drifts from celltrackR' },
    ],
    next: [
      REVIEW,
      todo('Run the celltrackR golden script on a schedule'),
      todo('A fixture with a frame interval not in seconds (speed units end to end)'),
    ],
  }) },
  { id: 'copy-gating', title: 'Copy gating to other images', group: 'Populations', maturity: covering(['gate.manual_gate'], {
    tier: 'beta', testedOn: 'A real measured fixture',
    next: [todo('Compare channel names when copying gates; refuse or warn on a mismatch')],
    needs: 'Two images of one set with the channel order swapped',
    datasetAsk: 'swapped-channel-set',
  }) },
  { id: 'proposed-gate', title: 'Claude proposes a gate', group: 'Populations', maturity: {
    tier: 'experimental', testedOn: IV2P,
    evidence: [
      { path: 'api/test/suite/gating_views.jl', test: 'API: data-derived transform parameters (logicle top, asinh cofactor)',
        failsIf: 'a transform’s parameters don’t follow the data' },
    ],
    next: [todo('A proposed gate on a fixture compared with a hand gate')],
  } },

  // ── Explore ──
  { id: 'phenotype', title: 'Phenotype page', group: 'Explore', maturity: covering(['/phenotype'], {
    tier: 'experimental', testedOn: 'A real measured fixture; synthetic',
    evidence: [
      { path: 'app/test/suite/plots.jl', test: 'plot count (raw + proportion normalize) — population summary',
        failsIf: 'a population count or proportion is wrong' },
    ],
    next: [todo('Record a hand-check: counts against a manual tally on one image')],
  }) },
  { id: 'cell-contacts', title: 'Cell contacts', group: 'Explore', maturity: covering(
    ['spatialAnalysis.cellContacts', 'spatialAnalysis.contactsMeshes'], {
      tier: 'beta', testedOn: 'A real measured fixture; a real movie',
      evidence: [
        { path: 'app/test/suite/spatial_neighbours.jl', test: 'cellContacts nearest B is per frame',
          failsIf: 'a contact pairs cells from different frames' },
        { path: 'python/cecelia/tests/test_mesh_containment.py', test: 'test_fragment_in_an_open_vacuole_is_mostly_enclosed',
          failsIf: 'an enclosed fragment reads as outside' },
      ],
      next: [REVIEW, todo('Mark containment columns written under the old 0/1 meaning stale')],
    }) },
  { id: 'aggregates', title: 'Aggregates', group: 'Explore', maturity: covering(
    ['spatialAnalysis.detectAggregates', 'spatialAnalysis.aggregatesMeshes'], {
      tier: 'experimental', testedOn: 'A real measured fixture; synthetic',
      evidence: [
        { path: 'app/test/suite/spatial_neighbours.jl', test: 'aggregate DBSCAN is per frame',
          failsIf: 'a slow track reads as an aggregate' },
        { path: 'app/test/suite/spatial_neighbours.jl', test: 'detectAggregates named runs coexist (KDIeEm)',
          failsIf: 'a second named run overwrites the first' },
      ],
      next: [todo('A result test for aggregates from meshes')],
    }) },
  { id: 'find-rois', title: 'Find ROIs', group: 'Explore', maturity: covering(['spatialAnalysis.roiDiscovery'], {
    tier: 'experimental', testedOn: 'Synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_roi_utils.py', test: 'test_groups_at_any_density_need_no_radius',
        failsIf: 'planted groups at different densities aren’t found' },
    ],
    next: [todo('Look at the viewer panel on real data')],
  }) },
  { id: 'spatial-pattern', title: 'Spatial pattern', group: 'Explore', maturity: covering(['spatialAnalysis.spatialPattern'], {
    tier: 'experimental', testedOn: 'A real measured fixture; a real movie; synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_spatial_pattern_utils.py', test: 'test_clustered_target_reads_as_clustered',
        failsIf: 'a clustered population reads as random' },
      { path: 'python/cecelia/tests/test_spatial_pattern_utils.py', test: 'test_shuffle_never_moves_a_label_across_frames',
        failsIf: 'the null moves a cell into another frame' },
    ],
    next: [todo('Look at the Spatial pattern by group panel on real data'), todo('Whole-curve group test')],
  }) },
  { id: 'motif-discovery', title: 'Motif discovery', group: 'Explore', maturity: covering(['behaviour.motif_discovery'], {
    tier: 'experimental', testedOn: 'A real measured + tracked fixture',
    next: [
      todo('A regression test that motif labels don’t land on unrelated tracks'),
      todo('A run-time refusal test for several segmentations'),
    ],
    needs: 'Tracks with known behaviour switches',
    datasetAsk: 'behaviour-switch-tracks',
  }) },
  { id: 'behaviour-cards', title: 'Behaviour and cell cards', group: 'Explore', maturity: {
    tier: 'experimental', testedOn: 'A real measured + tracked fixture',
    next: [todo('Footer stats on two different segmentations')],
  } },

  // ── Analysis ──
  { id: 'movies-page', title: 'Movies and figures page', group: 'Analysis', maturity: covering(['/movies'], EXEMPT) },
  { id: 'still-figures', title: 'Still figures', group: 'Analysis', maturity: {
    tier: 'experimental', testedOn: '—',
    evidence: [
      { path: 'python/cecelia/tests/test_plane_render.py', test: 'test_viridis_matches_matplotlib_at_both_ends_and_the_middle',
        failsIf: 'a colormap differs from matplotlib’s' },
    ],
  } },
  { id: 'headless-chat', title: 'Chat without the app (pixi run chat)', group: 'Analysis', maturity: {
    tier: 'experimental', testedOn: '—',
  } },

  // ── Pipeline ──
  { id: 'tasks-page', title: 'Tasks page', group: 'Pipeline', maturity: covering(['/tasks'], EXEMPT) },
  { id: 'chain-from-history', title: 'Chain from history', group: 'Pipeline', maturity: {
    tier: 'experimental', testedOn: 'Real run logs, one lab',
    evidence: [
      { path: 'app/test/suite/chain_from_history.jl', test: 'Chain from history — zolIMa UMq4v3: segment → gate → tracking ×2',
        failsIf: 'a real run history rebuilds into the wrong chain' },
    ],
  } },
  { id: 'cohort-qc', title: 'Cohort outlier check', group: 'Pipeline', maturity: {
    tier: 'experimental', testedOn: '—',
    next: [todo('A known-outlier fixture')],
  } },
  { id: 'hpc', title: 'Run segmentation on a cluster', group: 'Pipeline', maturity: {
    tier: 'experimental', testedOn: 'One lab’s SLURM cluster',
    next: [todo('A second site'), todo('An HPC run’s output equals the local run’s')],
    needs: 'Access to a second HPC site',
    noAsk: 'A cluster account to run on, not a dataset to share',
  } },
]
