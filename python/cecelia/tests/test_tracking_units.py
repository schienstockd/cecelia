"""Tracking coordinates reach btrack in µm — `cecelia.utils.tracking_utils`.

Nothing covered this, and the gap was the bug. btrack works in whatever space it is handed, so
`maxSearchRadius` meant PIXELS while `track_measures` — computing speed on the very same tracks —
reported µm/min from `img_physical_sizes`. One pipeline, two coordinate systems, and the form said
neither: "radius 8" and "an 8 µm jump" could both be true at once.

The Z half is the part a per-param conversion could never fix. At 0.33 µm XY and 2 µm Z, pixel-space
tracking scored a one-plane hop as 0.33 µm of motion when it is 2 µm — a 6x under-count, in exactly
the direction that links cells at different depths.
"""
import os
import tempfile
import unittest

import anndata as ad
import numpy as np
import pandas as pd

from cecelia.utils.tracking_utils import BayesianTrackingUtils
from cecelia.utils.label_props_utils import LabelPropsView


class _View:
    """The two methods `_centroids_from_view` uses, over a fixed frame."""

    def __init__(self, df, temporal=('centroid_t',)):
        self._df, self._t = df, list(temporal)

    def temporal_columns(self):
        return self._t

    def view_centroid_cols(self):
        return self

    def as_df(self):
        return self._df


class _Log:
    def log(self, *_a, **_k):
        pass


def _utils(physical_sizes=None):
    params = {'taskDir': '/tmp', 'btrackConfig': '/tmp/cfg.json', 'maxSearchRadius': 20,
              'maxLost': 1, 'trackBranching': False, 'minTimepoints': 5, 'accuracy': 0.8,
              'probToAssign': 0.8, 'noiseInital': 300, 'noiseProcessing': 100,
              'noiseMeasurements': 100, 'distThresh': 10, 'timeThresh': 5,
              'segmentationMissRate': 0.1, 'lambdaLink': 5, 'lambdaBranch': 50, 'lambdaTime': 5,
              'lambdaDist': 5, 'thetaTime': 5, 'thetaDist': 5}
    if physical_sizes is not None:
        params['physicalSizes'] = physical_sizes
    return BayesianTrackingUtils(params, _Log())


def _frame(z=True):
    d = {'centroid_t': [0.0, 1.0], 'centroid_y': [10.0, 20.0], 'centroid_x': [30.0, 40.0],
         'label': [1, 2]}
    if z:
        d['centroid_z'] = [2.0, 5.0]
    return pd.DataFrame(d)


class TrackingUnitsTest(unittest.TestCase):
    def test_centroids_are_scaled_to_microns(self):
        # [sz, sy, sx] — skimage order, as `img_physical_sizes` returns it
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_allclose(got['x'], [30 * 0.25, 40 * 0.25])
        np.testing.assert_allclose(got['y'], [10 * 0.5, 20 * 0.5])
        np.testing.assert_allclose(got['z'], [2 * 2.0, 5 * 2.0])

    def test_the_axis_order_is_z_y_x_not_x_y_z(self):
        """The one way to get this silently wrong. With distinct sizes per axis, swapping the order
        still produces plausible coordinates — just the wrong distances, on the wrong axis."""
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        # x used the LAST size (0.25); had the order been read x,y,z it would be 30*2.0 = 60
        self.assertAlmostEqual(got['x'].iloc[0], 7.5)
        self.assertNotAlmostEqual(got['x'].iloc[0], 60.0)

    def test_anisotropic_z_is_no_longer_under_counted(self):
        """The 6x. One Z plane at 2 µm must not score as 0.33 µm of motion."""
        px, pz = 0.33, 2.0
        got = _utils([pz, px, px])._centroids_from_view(_View(_frame()))
        step_z = abs(got['z'].iloc[1] - got['z'].iloc[0])
        self.assertAlmostEqual(step_z, 3 * pz)                 # 3 planes → 6 µm
        self.assertGreater(step_z / (3 * px), 5.9)             # ~6x what pixel space scored it

    def test_time_is_left_in_frames(self):
        """btrack's `t` is a frame index and the config's time params are in frames; scaling it here
        would silently redefine `maxLost`/`timeThresh`. Minutes are `track_measures`' business."""
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_allclose(got['t'], [0.0, 1.0])

    def test_no_sizes_means_unscaled_not_a_fake_micron(self):
        """An uncalibrated image keeps the old pixel behaviour rather than pretending 1 px = 1 µm."""
        for sizes in (None, []):
            got = _utils(sizes)._centroids_from_view(_View(_frame()))
            np.testing.assert_allclose(got['x'], [30.0, 40.0])
            np.testing.assert_allclose(got['z'], [2.0, 5.0])

    def test_2d_has_no_z_column_and_still_scales_xy(self):
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame(z=False)))
        np.testing.assert_allclose(got['z'], [0.0, 0.0])
        np.testing.assert_allclose(got['x'], [7.5, 10.0])

    def test_labels_survive_unscaled(self):
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_array_equal(got['label_id'], [1, 2])

    def test_a_missing_temporal_axis_still_stops_the_run(self):
        with self.assertRaises(SystemExit):
            _utils([1.0, 1.0, 1.0])._centroids_from_view(_View(_frame(), temporal=()))


# ── Provenance-aware _write_back (MULTI_POP_TRACKING_PLAN.md P1) ───────────────────────────────
# `_write_back` used to NaN out `track_id` for labels missing from the current run — sequential
# tracking on two pops overwrote each other. New contract: delete rows by `track_source == this run`,
# compact surviving `track_id`s to 1..N (remapping parent/root), write new ids from N+1 and stamp
# `track_source` on them. Every assertion below points at that contract.


def _make_labelprops_h5ad(path, labels):
    """A minimal label-props h5ad with `n` cells and an integer-string obs index (the on-disk
    shape `LabelPropsView` reads)."""
    n = len(labels)
    adata = ad.AnnData(
        X=np.zeros((n, 2), dtype=np.float64),
        obs=pd.DataFrame(index=[str(l) for l in labels]),
    )
    adata.var_names = ["f0", "f1"]
    adata.write_h5ad(path)


def _run_utils(props_path, source, force=False, live=None):
    """A `BayesianTrackingUtils` wired to a fixture h5ad and a specific `trackSource`. Only
    `_write_back` is exercised; params for btrack itself are irrelevant here. `force=True` sets
    `trackSourceForce` — the intentional-overlap override the P1 conflict detector honours.
    `live=None` disables the P3 orphan sweep (matches a legacy Julia handler that never emitted the
    param); `live=[...]` (or `[]`) engages it against that live-UID set."""
    params = {'taskDir': os.path.dirname(props_path), 'btrackConfig': '/tmp/cfg.json',
              'maxSearchRadius': 20, 'maxLost': 1, 'trackBranching': False, 'minTimepoints': 1,
              'accuracy': 0.8, 'probToAssign': 0.8, 'noiseInital': 300, 'noiseProcessing': 100,
              'noiseMeasurements': 100, 'distThresh': 10, 'timeThresh': 5,
              'segmentationMissRate': 0.1, 'lambdaLink': 5, 'lambdaBranch': 50, 'lambdaTime': 5,
              'lambdaDist': 5, 'thetaTime': 5, 'thetaDist': 5,
              'valueName': 'test', 'trackSource': source, 'trackSourceForce': force,
              'liveTrackSources': live}
    u = BayesianTrackingUtils(params, _Log())
    # override the labelProps location `__init__` computed so it points at the fixture we built
    u.props_path = props_path
    return u


def _lineage_df(pairs):
    """`(label, track_id)` pairs → a btrack-shaped `track_df` for `_write_back`. `parent`/`root`
    default to `track_id` (a root track with no division, matching btrack's convention).
    `cell_id` is a 1-based index within each track — assigned per-track by insertion order."""
    rows = []
    seen = {}
    for label, tid in pairs:
        seen.setdefault(tid, 0)
        seen[tid] += 1
        rows.append({'label_id': label, 'track_id': tid, 'parent': tid, 'root': tid,
                     'state': 5.0, 'generation': 0.0, 'cell_id': seen[tid], 't': 0.0})
    return pd.DataFrame(rows)


def _obs(path):
    return LabelPropsView(path).adata.obs


class WriteBackProvenanceTest(unittest.TestCase):
    """The three cases MULTI_POP_TRACKING_PLAN.md P1 pins."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, "cells.h5ad")
        # 10 cells so two disjoint five-cell pops give a clear before/after
        _make_labelprops_h5ad(self.path, list(range(1, 11)))

    def _track_ids_by_label(self):
        obs = _obs(self.path)
        return dict(zip(obs.index.astype(int).tolist(),
                        obs["track_id"].tolist()))

    def test_two_disjoint_pops_both_survive(self):
        """Track pop A (labels 1..5) then pop B (labels 6..10). Both sets keep track_ids; the
        sources match; ids don't collide."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        _run_utils(self.path, "B")._write_back(_lineage_df([(l, 200 + l) for l in range(6, 11)]))
        obs = _obs(self.path)
        # A: labels 1..5 are tracked, source == A
        for l in range(1, 6):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        # B: labels 6..10 are tracked, source == B
        for l in range(6, 11):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "B")
        # id space is compact: exactly 10 unique tracks, no NaN
        ids = obs["track_id"].dropna().astype(int).tolist()
        self.assertEqual(len(set(ids)), 10)
        # and starts at 1 (the compact step guarantees dense numbering)
        self.assertEqual(min(ids), 1)
        self.assertEqual(max(ids), 10)

    def test_retrack_pop_replaces_its_own_rows_without_growing_id_space(self):
        """Track A, track B, re-track A. The old A rows are gone (delete step), B is renumbered
        (compact step) and the new A run gets fresh dense ids from the top of the compacted range —
        no unbounded growth across re-runs."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        _run_utils(self.path, "B")._write_back(_lineage_df([(l, 200 + l) for l in range(6, 11)]))
        # re-track A with a smaller pop this time (labels 1..3) so we can see the delete step at work
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 999 + l) for l in range(1, 4)]))
        obs = _obs(self.path)
        # labels 4 and 5 were in the OLD A run but not the new one → their prior track_ids were
        # deleted (source==A matched); they now hold NaN
        self.assertTrue(np.isnan(obs.loc["4", "track_id"]))
        self.assertTrue(np.isnan(obs.loc["5", "track_id"]))
        self.assertTrue(pd.isna(obs.loc["4", "track_source"]))
        # B still has all 5 rows and its source
        for l in range(6, 11):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "B")
        # new A run has 3 rows and its source
        for l in range(1, 4):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        # id space: 5 B + 3 A = 8 tracks, dense 1..8 (compact + N+1 write step)
        ids = obs["track_id"].dropna().astype(int).tolist()
        self.assertEqual(len(set(ids)), 8)
        self.assertEqual(max(ids), 8)

    def test_whole_seg_then_per_pop_is_non_symmetric(self):
        """`whole_seg` primes every cell; a per-pop run afterwards only touches its own cells — the
        `whole_seg` rows outside the pop are LEFT ALONE because their source doesn't match. This is
        the intended 'prime then refine' mode (plan Decision 1, non-symmetric note)."""
        _run_utils(self.path, "whole_seg")._write_back(
            _lineage_df([(l, 100 + l) for l in range(1, 11)]))
        # 10 rows sourced whole_seg
        obs = _obs(self.path)
        self.assertTrue(all(obs.loc[str(l), "track_source"] == "whole_seg"
                            for l in range(1, 11)))
        # then track pop A on labels 1..5
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 500 + l) for l in range(1, 6)]))
        obs = _obs(self.path)
        # A cells: source flipped, ids refreshed
        for l in range(1, 6):
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        # non-A cells: still whole_seg-sourced, still tracked (the plan's non-symmetric behaviour)
        for l in range(6, 11):
            self.assertEqual(obs.loc[str(l), "track_source"], "whole_seg")
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))


class WriteBackConflictDetectorTest(unittest.TestCase):
    """MULTI_POP_TRACKING_ORPHANS_PLAN.md P1 — the conflict detector in `_write_back`.

    Guards the general pop→pop label-collision case that the shipped plan left undocumented: if
    pop B's run includes a label currently owned by pop A's `track_source`, the WRITE step used to
    silently transfer ownership. Now it fails by default and names the labels; the caller passes
    `trackSourceForce=true` to override (the intentional pop→pop refinement idiom).
    """

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, "cells.h5ad")
        _make_labelprops_h5ad(self.path, list(range(1, 11)))

    def test_disjoint_pops_do_not_trigger_the_detector(self):
        """Baseline: pop A on 1..5, pop B on 6..10 — no shared labels, no conflict, no raise."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        # If the detector fired incorrectly this would raise. Assert both runs land.
        _run_utils(self.path, "B")._write_back(_lineage_df([(l, 200 + l) for l in range(6, 11)]))
        obs = _obs(self.path)
        self.assertEqual(obs.loc["1", "track_source"], "A")
        self.assertEqual(obs.loc["6", "track_source"], "B")

    def test_overlapping_pops_second_run_raises(self):
        """Pop A tracks 1..5. Pop B then tracks 3..7 — labels 3, 4, 5 are still stamped `A`. The
        detector raises with an actionable message; nothing is written."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        # Snapshot the id-space BEFORE B's attempt so we can prove the raise didn't half-write.
        obs_before = _obs(self.path).copy()
        with self.assertRaises(ValueError) as cm:
            _run_utils(self.path, "B")._write_back(
                _lineage_df([(l, 200 + l) for l in range(3, 8)]))
        msg = str(cm.exception)
        # The message names the count, the sample labels/sources, and the override switch.
        self.assertIn("3 labels already owned", msg)
        self.assertIn("source='A'", msg)
        self.assertIn("trackSourceForce", msg)
        # Nothing was written — A's rows are untouched, B has no rows.
        obs_after = _obs(self.path)
        for l in range(1, 6):
            self.assertEqual(obs_after.loc[str(l), "track_source"], "A")
            self.assertEqual(obs_before.loc[str(l), "track_id"],
                             obs_after.loc[str(l), "track_id"])
        # Labels 6..10 stay untracked.
        for l in range(6, 11):
            self.assertTrue(pd.isna(obs_after.loc[str(l), "track_source"]))
            self.assertTrue(np.isnan(obs_after.loc[str(l), "track_id"]))

    def test_force_overrides_and_transfers_ownership(self):
        """Same collision as above, but with `trackSourceForce=true`. B wins the overlap; A's rows
        outside the overlap survive."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        _run_utils(self.path, "B", force=True)._write_back(
            _lineage_df([(l, 200 + l) for l in range(3, 8)]))
        obs = _obs(self.path)
        # A's non-overlapping cells (1, 2) still A-owned
        for l in (1, 2):
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        # Overlap (3, 4, 5) transferred to B
        for l in (3, 4, 5):
            self.assertEqual(obs.loc[str(l), "track_source"], "B")
        # B's new-only cells (6, 7) B-owned
        for l in (6, 7):
            self.assertEqual(obs.loc[str(l), "track_source"], "B")

    def test_whole_seg_is_bypass_and_does_not_raise(self):
        """`whole_seg` primes every cell. Per-pop A afterwards touches labels also stamped
        whole_seg — but whole_seg is the sanctioned bypass, no raise. This is the shipped plan's
        non-symmetric refine mode; the P1 detector must not regress it."""
        _run_utils(self.path, "whole_seg")._write_back(
            _lineage_df([(l, 100 + l) for l in range(1, 11)]))
        # Should not raise — every overlap is against whole_seg.
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 500 + l) for l in range(1, 6)]))
        obs = _obs(self.path)
        for l in range(1, 6):
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        for l in range(6, 11):
            self.assertEqual(obs.loc[str(l), "track_source"], "whole_seg")

    def test_own_source_retrack_is_not_a_conflict(self):
        """Re-tracking pop A on labels A already owns must not raise. The DELETE step clears A's
        rows before the detector runs, so `src_obj` at those rows is `None` when checked."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        # Second A run overlaps its OWN labels; DELETE clears them first, detector sees None.
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 999 + l) for l in range(1, 4)]))
        obs = _obs(self.path)
        for l in (1, 2, 3):
            self.assertEqual(obs.loc[str(l), "track_source"], "A")
        # Labels 4, 5 dropped from the re-run — DELETE cleared them, no re-write follows.
        self.assertTrue(pd.isna(obs.loc["4", "track_source"]))


class WriteBackOrphanSweepTest(unittest.TestCase):
    """MULTI_POP_TRACKING_ORPHANS_PLAN.md P3 — the tracking-time orphan sweep.

    A `track_source` value in the h5ad outlives the pop that stamped it. Between DELETE and COMPACT
    the sweep NaNs any row whose `track_source` is neither the whole_seg sentinel nor a member of
    the live-UID set the Julia handler passed in. `liveTrackSources=None` (legacy handler) disables
    the sweep entirely; `[]` runs it with an empty live set.
    """

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, "cells.h5ad")
        _make_labelprops_h5ad(self.path, list(range(1, 11)))

    def test_orphaned_rows_are_swept_when_a_live_pop_re_tracks(self):
        """Track pop A, delete A (simulated by removing A from `liveTrackSources`), then track pop
        B. B's run carries `liveTrackSources=['B_uid']` — A's rows are orphans and get NaN'd."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        # B tracks non-overlapping labels (6..10) — no conflict, plain sweep territory.
        _run_utils(self.path, "B", live=["B"])._write_back(
            _lineage_df([(l, 200 + l) for l in range(6, 11)]))
        obs = _obs(self.path)
        # A's rows (1..5) were orphaned by the sweep — track_id NaN, source cleared.
        for l in range(1, 6):
            self.assertTrue(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertTrue(pd.isna(obs.loc[str(l), "track_source"]))
        # B's rows survive.
        for l in range(6, 11):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "B")

    def test_none_disables_the_sweep_backwards_compat(self):
        """A legacy Julia handler that never emits `liveTrackSources` sends `None`. Under the
        `None` path the sweep is skipped — pre-P3 behaviour unchanged, orphans persist."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        _run_utils(self.path, "B", live=None)._write_back(
            _lineage_df([(l, 200 + l) for l in range(6, 11)]))
        obs = _obs(self.path)
        # A's rows PRESERVED because the sweep didn't run.
        for l in range(1, 6):
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
            self.assertEqual(obs.loc[str(l), "track_source"], "A")

    def test_whole_seg_rows_survive_the_sweep_always(self):
        """`whole_seg` is the sentinel — a whole-seg row is never an orphan even if the live-UID
        set is empty. Otherwise a per-pop refine after a whole-seg run would wipe every non-refined
        cell."""
        _run_utils(self.path, "whole_seg")._write_back(
            _lineage_df([(l, 100 + l) for l in range(1, 11)]))
        # Run a per-pop A on labels 1..5, live-set carries only A. whole_seg rows 6..10 must live.
        _run_utils(self.path, "A", live=["A"])._write_back(
            _lineage_df([(l, 500 + l) for l in range(1, 6)]))
        obs = _obs(self.path)
        for l in range(6, 11):
            self.assertEqual(obs.loc[str(l), "track_source"], "whole_seg")
            self.assertFalse(np.isnan(obs.loc[str(l), "track_id"]))
        # And A's rows are A.
        for l in range(1, 6):
            self.assertEqual(obs.loc[str(l), "track_source"], "A")

    def test_sweep_prevents_the_conflict_detector_from_seeing_orphans(self):
        """An orphan whose label overlaps THIS run's target set would otherwise trip the P1
        detector — it wouldn't be `self.track_source`, wouldn't be `whole_seg`, wouldn't be None.
        The sweep runs FIRST so the detector sees only live-pop conflicts, not deleted-pop
        residue."""
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 100 + l) for l in range(1, 6)]))
        # Track B on labels 3..7. A owns 3..5. With the sweep engaged and A absent from live, A's
        # rows become orphans → cleared → B proceeds without raising.
        _run_utils(self.path, "B", live=["B"])._write_back(
            _lineage_df([(l, 200 + l) for l in range(3, 8)]))
        obs = _obs(self.path)
        for l in range(3, 8):
            self.assertEqual(obs.loc[str(l), "track_source"], "B")
        # Labels 1, 2 were owned by A → also swept (orphan) → NaN.
        for l in (1, 2):
            self.assertTrue(pd.isna(obs.loc[str(l), "track_source"]))

    def test_empty_live_set_still_preserves_whole_seg(self):
        """Edge case: no live pops at all (every pop was deleted). Only whole_seg survives the sweep."""
        _run_utils(self.path, "whole_seg")._write_back(
            _lineage_df([(l, 100 + l) for l in range(1, 6)]))
        _run_utils(self.path, "A")._write_back(_lineage_df([(l, 500 + l) for l in range(6, 11)]))
        # A now writes; then a whole-seg re-run with empty live sweeps A (deleted) but keeps ws.
        # But whole_seg's re-run also DELETEs its own rows first, then re-writes them. So verify
        # the interaction is clean.
        _run_utils(self.path, "whole_seg", live=[])._write_back(
            _lineage_df([(l, 900 + l) for l in range(1, 6)]))
        obs = _obs(self.path)
        # Labels 1..5: newly whole-seg-authored, source == whole_seg
        for l in range(1, 6):
            self.assertEqual(obs.loc[str(l), "track_source"], "whole_seg")
        # Labels 6..10: A's rows were swept as orphans (A not in live=[])
        for l in range(6, 11):
            self.assertTrue(pd.isna(obs.loc[str(l), "track_source"]))
            self.assertTrue(np.isnan(obs.loc[str(l), "track_id"]))


if __name__ == '__main__':
    unittest.main()
