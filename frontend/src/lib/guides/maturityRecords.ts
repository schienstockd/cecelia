// The maturity record of every guide's feature, keyed by guide id — one file, so a promotion PR edits
// one place (docs/todo/FEATURE_MATURITY_PLAN.md, Decision 6: only the maintainer promotes). Each guide
// reads its own entry (`maturity: GUIDE_MATURITY['…']`); `utils/maturity.test.ts` pins that every
// guide does, and that every evidence path and test name exists.
//
// Seeded from docs/todo/FEATURE_MATURITY_TIERS.md and provisional until the hardening matrix re-scores
// them. Rows proposed there for Tested are seeded as Beta with REVIEW as their first `next` item — the
// tier moves when the maintainer accepts the fails-if lines, not before.
//
// Copy budget (rendered in the map's expanded row): testedOn ≤ 70, failsIf / next ≤ 110, needs ≤ 90.

import type { Maturity, NextItem } from './types'

export const REVIEW: NextItem = { text: 'Maintainer review of the fails-if lines → Tested', done: false }
/** An open checklist item. */
export const todo = (text: string): NextItem => ({ text, done: false })

export const IV2P = 'Intravital 2-photon movies, one lab'
export const EXEMPT: Maturity = { tier: 'exempt', testedOn: '' }

export const GUIDE_MATURITY = {
  // ── Start ──
  'find-your-way-around': EXEMPT,
  'use-a-user-profile': EXEMPT,
  'use-a-view-profile': EXEMPT,

  // ── Data ──
  'import-images': {
    tier: 'beta',
    testedOn: 'Intravital LIF/ND2/IMS/TIF, one lab; a real 64×64 crop',
    evidence: [
      { path: 'app/test/suite/ome_qc.jl', test: '_delta_t_fallback',
        failsIf: 'the frame interval is not the median DeltaT in seconds' },
      { path: 'app/test/suite/omezarr_bf2raw.jl', test: 'OME-ZARR metadata reads v2 and v3 alike',
        failsIf: 'a real pixel size, z step or frame interval falls back to 1' },
    ],
    next: [
      REVIEW,
      todo('Convert non-second / non-µm units on import, with a QC finding for units it can’t'),
      todo('Tiny vendor files with known calibration, incl. ms/min time and nm pixels'),
    ],
    needs: 'Vendor files with known pixel size and frame interval, in other units',
    datasetAsk: 'vendor-calibration',
    covers: ['importImages.remove', 'editImages.copyImage'],
  },
  'fix-metadata': {
    tier: 'beta',
    testedOn: `${IV2P}; synthetic stores`,
    evidence: [
      { path: 'app/test/suite/ome_qc.jl', test: 'sync_zarr_calibration!',
        failsIf: 'a corrected z step or frame interval doesn’t reach what the viewer reads' },
      { path: 'app/test/suite/ome_qc.jl', test: 'resync_ome_meta! fill-only',
        failsIf: 'a resync overwrites a human correction' },
    ],
    next: [
      REVIEW,
      todo('A calibration edit marks downstream µm and speed outputs stale'),
    ],
    needs: 'Vendor files with known pixel size and frame interval, in other units',
    datasetAsk: 'vendor-calibration',
  },
  'preprocess-images': {
    tier: 'experimental',
    testedOn: 'Synthetic; intravital movies (not recorded)',
    evidence: [
      { path: 'python/cecelia/tests/test_crop_image.py', test: 'test_slices_xyz_keeps_channel_full',
        failsIf: 'crop bounds slice the wrong pixels' },
      { path: 'python/cecelia/tests/test_crop_image.py', test: 'test_time_trim',
        failsIf: 'a time trim keeps the wrong frames' },
    ],
    next: [
      todo('A backend test that crop cuts the active version, not the default'),
      todo('Record a hand-check of cropped pixels on a drift-corrected store'),
    ],
  },
  'drift-correct': {
    tier: 'beta',
    testedOn: `${IV2P}; synthetic known shifts`,
    handCheck: 'docs/PROVENANCE.md → Ported subsystems (believed equal to R, not re-checked)',
    evidence: [
      { path: 'python/cecelia/tests/test_drift_estimate_rigid.py', test: 'test_recovers_a_known_rotation_and_translation',
        failsIf: 'the estimated rotation or translation is off' },
      { path: 'python/cecelia/tests/test_drift_estimate.py', test: 'test_recovers_a_known_linear_drift',
        failsIf: 'a known linear drift is not recovered' },
    ],
    next: [
      REVIEW,
      todo('Re-check parity against the R version on one real movie'),
      todo('A real-pixel fixture with a known applied drift'),
    ],
    needs: 'A movie whose tissue tilts through Z',
    datasetAsk: 'sitk-rigid-3d-full',
  },
  'af-correct': {
    tier: 'beta',
    testedOn: `${IV2P}; synthetic injected leak`,
    handCheck: 'docs/PROVENANCE.md → Autofluorescence correction',
    evidence: [
      { path: 'python/cecelia/tests/test_correction_utils.py',
        test: 'test_exclusive_recovers_the_WHOLE_leak_when_nothing_is_co_labelled',
        failsIf: 'the estimator under-recovers the leak' },
      { path: 'python/cecelia/tests/test_correction_utils.py', test: 'test_the_leaked_amount_is_subtracted_not_scaled',
        failsIf: 'a co-positive voxel is zeroed instead of having the leak subtracted' },
    ],
    next: [
      REVIEW,
      todo('A 2-channel fixture with a measured single-stain control'),
      todo('Land the downstream-impact audit PROVENANCE.md calls in flight'),
    ],
    needs: 'Single-stained controls from another microscope',
    datasetAsk: 'single-stain-controls',
  },
  'segment-an-image': {
    tier: 'beta',
    testedOn: `${IV2P} (outlines by eye); synthetic tiles`,
    handCheck: 'docs/SEGMENTATION.md → Which knob first',
    evidence: [
      { path: 'python/cecelia/tests/test_tile_seam_stitch.py', test: 'test_every_cell_has_one_id_and_no_two_cells_share_one',
        failsIf: 'a cell crossing a tile seam keeps two ids' },
      { path: 'python/cecelia/tests/test_cellpose_stitch_ids.py', test: 'test_stitch3d_collision_is_repaired',
        failsIf: 'an empty plane merges two cells' },
      { path: 'python/cecelia/tests/test_segmentation_streaming.py', test: 'test_2d_timeseries_matches_golden',
        failsIf: 'streamed labels drift from the golden (stub model, not real weights)' },
    ],
    next: [
      todo('A tiny annotated 3D stack run with real weights in CI: count and IoU golden'),
      todo('An ellipticity test for measure'),
    ],
    needs: 'A fixed or cultured 3D stack with hand-annotated masks',
    datasetAsk: 'annotated-3d-stack',
  },
  'train-denoise-model': {
    tier: 'experimental',
    testedOn: 'Intravital movies (not recorded)',
    next: [
      todo('An output-quality check on a known-noise input'),
      todo('Record a hand-check of denoised output in the docs'),
    ],
    needs: 'Paired low- and high-SNR acquisitions',
    datasetAsk: 'paired-snr',
  },
  'train-flow-model': {
    tier: 'experimental',
    testedOn: IV2P,
    evidence: [
      { path: 'python/cecelia/tests/test_flow_train_normalisation.py',
        test: 'test_cutting_early_gives_the_reference_numbers_exactly',
        failsIf: 'training normalisation drifts from the reference numbers (not model quality)' },
    ],
    next: [
      todo('A test that the flow metric sheet seeds the model’s channel'),
      todo('A tiny checkpoint fixture, shared with segment by motion'),
    ],
  },
  'segment-by-motion': {
    tier: 'beta',
    testedOn: `${IV2P} only`,
    handCheck: 'docs/PROVENANCE.md → Segmentation on real intravital data',
    evidence: [
      { path: 'python/cecelia/tests/test_coastal_utils.py', test: 'test_renaming_restores_the_training_channel_order',
        failsIf: 'motion planes are string-sorted, so a plane reads the wrong channel' },
    ],
    next: [
      todo('A tiny trained checkpoint + 16-frame synthetic movie, frame interval ≠ the model’s'),
      todo('Fix and test the empty-plane collision in 3D mask matching'),
    ],
  },

  // ── Populations ──
  'gate-populations': {
    tier: 'beta',
    testedOn: `${IV2P}; a real measured fixture`,
    handCheck: 'docs/PROVENANCE.md → Gating — population plausibility',
    evidence: [
      { path: 'app/test/suite/gating.jl', test: 'Transforms',
        failsIf: 'logicle deviates from the FlowUtils golden values' },
      { path: 'app/test/suite/gating.jl', test: 'Gates',
        failsIf: 'gate membership is wrong, incl. a logicle gate selecting by raw value' },
      { path: 'app/test/suite/pop_df.jl', test: 'pop_df integration (KDIeEm)',
        failsIf: 'a gate on the real fixture counts differently from an independent count' },
    ],
    next: [
      todo('Compare channel names when copying gates or pooling; refuse or warn on a mismatch'),
      todo('A test copying a gate onto swapped channels selects no wrong marker'),
      todo('A nested child is a subset of its parent, on real data'),
    ],
    needs: 'Two images of one set with the channel order swapped',
    datasetAsk: 'swapped-channel-set',
  },
  'track-cells': {
    tier: 'beta',
    testedOn: IV2P,
    handCheck: 'docs/TRACKING.md (btrack two-phase) + docs/PROVENANCE.md → btrack pipeline',
    evidence: [
      { path: 'python/cecelia/tests/test_tracking_units.py', test: 'test_anisotropic_z_is_no_longer_under_counted',
        failsIf: 'btrack links in pixels instead of µm (units only — no link scoring)' },
    ],
    next: [
      todo('Simulated tracks with known links (gap, division, anisotropic voxels), scored'),
      todo('A fixture with a frame interval not in seconds'),
    ],
    needs: 'Ground-truth tracks, e.g. a Cell Tracking Challenge-style movie',
    datasetAsk: 'ground-truth-tracks',
  },
  'correct-a-mask': {
    tier: 'experimental',
    testedOn: 'Intravital movies, checked by eye',
    evidence: [
      { path: 'app/test/suite/correction.jl', test: 'track correction — join and split',
        failsIf: 'a join or split gives the wrong track ids' },
    ],
    next: [
      todo('A test that correct + re-measure forwards the corrected output name'),
      todo('The op loop on ZTYX labels, not just the detector'),
    ],
    covers: ['segment.correct', 'segment.correct_measures', 'tracking.correct', 'tracking.correct_measures'],
  },
  'cluster-cells': {
    tier: 'beta',
    testedOn: 'Intravital movies, few markers; a real measured fixture',
    next: [
      todo('One PROVENANCE.md line: what was checked by hand, on which data'),
      todo('A synthetic table with 3 planted clusters and two segmentations'),
      todo('Compare channel names when pooling a set'),
    ],
    needs: 'A multiplex image whose clustering gives 10+ clusters',
    datasetAsk: 'cluster-grouping-highplex',
  },
  'cluster-tracks': {
    tier: 'beta',
    testedOn: IV2P,
    checkedBy: { who: 'Dominik', when: '2026-11', data: 'intravital movies; matches the old R version' },
    evidence: [
      { path: 'app/test/suite/plots.jl', test: 'track-grained plots return the aggregates they name',
        failsIf: 'a track heatmap row shows a different aggregate than it names' },
    ],
    next: [
      todo('A test that re-measuring tracks prunes stale cluster features'),
      todo('A guard for which track-cluster version is preferred'),
    ],
    needs: 'Tracks with known behaviour switches',
    datasetAsk: 'behaviour-switch-tracks',
  },
  'cluster-regions': {
    tier: 'experimental',
    testedOn: 'Intravital movies; synthetic',
    evidence: [
      { path: 'python/cecelia/tests/test_spatial_utils.py', test: 'test_two_populations_fractions',
        failsIf: 'the neighbourhood composition formula is wrong (helper only)' },
    ],
    next: [
      todo('A synthetic fixture with planted regions'),
      todo('Extend the contact-mesh fixture to 2+ frames'),
    ],
    needs: 'A multiplex tissue section with annotated niches',
    datasetAsk: 'annotated-niches',
  },

  // ── Explore ──
  'behaviour-states': {
    tier: 'experimental',
    testedOn: 'A real measured + tracked fixture',
    next: [
      todo('Simulated tracks switching between two speed states on a known schedule'),
      todo('An interleaved-track-order fixture'),
    ],
    needs: 'Tracks with known behaviour switches',
    datasetAsk: 'behaviour-switch-tracks',
  },
  'spatial-analysis': {
    tier: 'experimental',
    testedOn: 'A real measured fixture; synthetic',
    next: [
      todo('Known-answer log-odds on a planted arrangement'),
      todo('Extend the contact-mesh fixture to 2+ frames'),
    ],
    needs: 'A multiplex tissue section with annotated niches',
    datasetAsk: 'annotated-niches',
    covers: ['spatialAnalysis.cellNeighbours'],
  },

  // ── Analysis ──
  'build-plots': {
    tier: 'beta',
    testedOn: `${IV2P}; a real measured fixture`,
    evidence: [
      { path: 'app/test/suite/plots.jl', test: 'plot percent (% positive of a 0/1 measure)',
        failsIf: 'the Wilson interval or the percent aggregation is wrong' },
      { path: 'app/test/suite/plots.jl', test: 'plot statUnit=image (per-image mean = each dot an image)',
        failsIf: 'a per-image mean is computed over the wrong cells' },
    ],
    next: [
      REVIEW,
      todo('A test for pooled point ids'),
      todo('The CSV attributes toggle covers the gating, UMAP, track and HMM CSVs'),
    ],
  },
  'record-a-movie': {
    tier: 'beta',
    testedOn: IV2P,
    handCheck: 'docs/PROVENANCE.md → Offline renderer + WebGPU viewer',
    evidence: [
      { path: 'api/test/suite/overlay_3d_and_rail_3d.jl',
        test: 'API: co-clustered segmentations stay apart — overlays, heatmap pooling',
        failsIf: 'one segmentation’s overlay draws another’s cells' },
      { path: 'api/test/suite/movie_frames_and_overlays.jl',
        test: 'API: label_geometry_mismatch — a mask from another image version is not drawn',
        failsIf: 'a mask from another image version is drawn' },
    ],
    next: [todo('Check the Z-window overlay by eye on real data')],
  },
  'build-an-animation': {
    tier: 'beta',
    testedOn: IV2P,
    handCheck: 'docs/PROVENANCE.md → Offline renderer (shared, not animation-specific)',
    evidence: [
      { path: 'api/test/suite/movie_rail_translators.jl',
        test: 'API: interpolate_keyframes reaches the incoming keyframe exactly',
        failsIf: 'a fly-through misses the keyframe it moves to' },
    ],
    next: [todo('Record one hand-checked fly-through')],
  },
  'create-a-notebook': {
    tier: 'experimental',
    testedOn: '—',
    next: [todo('A notebook pop_df returns the same table as the app')],
  },
  'use-the-blackboard': EXEMPT,
  'assist-with-kiwi': {
    tier: 'experimental',
    testedOn: 'Test fixture, fake engine',
    evidence: [
      { path: 'api/test/suite/kiwi_refs.jl', test: 'Kiwi refs — resolver against the testpr fixture',
        failsIf: 'a cited reference resolves to the wrong object' },
    ],
    next: [todo('Run the Kiwi eval in CI, or record its results')],
  },

  // ── Pipeline ──
  'run-a-chain': {
    tier: 'beta',
    testedOn: 'The chain shapes one lab runs day to day',
    handCheck: 'docs/PROVENANCE.md → Chain executor',
    evidence: [
      { path: 'app/test/suite/task_machinery.jl', test: 'Section params flatten (chain nesting)',
        failsIf: 'nested section params don’t reach the run' },
    ],
    next: [
      todo('A 3-task chain on the cellpose fixture equals the same tasks run by hand'),
      todo('Parallel branches and resume-then-retry'),
    ],
  },
} satisfies Record<string, Maturity>

export type GuideId = keyof typeof GUIDE_MATURITY
