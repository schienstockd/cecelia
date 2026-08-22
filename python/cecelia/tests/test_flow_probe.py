"""The flow-engine fingerprint: does it detect a recipe change, and stay quiet otherwise?

The guard that matters is the last class — a real `cv2.DISOpticalFlow` swap, which is the concrete
thing this exists to catch (it only accepts 8-bit input, so it reintroduces the cast coastal removed
in PR #19, and its magnitude field correlates 0.00 with Farneback's on zolIMa/fXgbTl).
"""

import copy
import importlib.util
import unittest

import numpy as np

from cecelia.utils import flow_probe


class ProbeWindowTest(unittest.TestCase):

    def test_shape_and_dtype(self):
        w = flow_probe.probe_window()
        self.assertEqual(w.shape, flow_probe.SHAPE)
        self.assertEqual(w.dtype, np.float32)

    def test_deterministic(self):
        np.testing.assert_array_equal(flow_probe.probe_window(),
                                      flow_probe.probe_window())

    def test_scaled_like_a_projected_window(self):
        w = flow_probe.probe_window()
        self.assertGreater(w.max(), 100.0)
        self.assertLessEqual(w.max(), 300.0)
        self.assertGreaterEqual(w.min(), 0.0)

    def test_the_blobs_actually_move(self):
        """A static field would leave the deformation metrics constant, and a constant plane cannot
        detect a change to how it is computed."""
        w = flow_probe.probe_window()
        self.assertGreater(np.abs(w[-1] - w[0]).max(), 10.0)


class SummariseTest(unittest.TestCase):

    def test_positive_for_a_varying_plane(self):
        std, p99 = flow_probe._summarise(np.linspace(-3, 3, 400).reshape(20, 20))
        self.assertGreater(std, 0)
        self.assertGreater(p99, 0)

    def test_constant_plane_is_zero_spread(self):
        std, p99 = flow_probe._summarise(np.full((8, 8), 2.5))
        self.assertAlmostEqual(std, 0.0)
        self.assertAlmostEqual(p99, 2.5)

    def test_empty_plane_does_not_raise(self):
        self.assertEqual(flow_probe._summarise(np.zeros((0, 4))), [0.0, 0.0])

    def test_mean_is_not_used(self):
        """A signed near-symmetric metric has a mean at ~0, where a relative comparison carries no
        information — so both statistics must be scale-like, not location-like."""
        a = flow_probe._summarise(np.array([[-1.0, 1.0], [-1.0, 1.0]]))
        b = flow_probe._summarise(np.array([[-2.0, 2.0], [-2.0, 2.0]]))
        self.assertNotAlmostEqual(a[0], b[0])


def _fp(**metrics):
    return {'version': flow_probe.VERSION,
            'metrics': {k: list(v) for k, v in metrics.items()}}


class CompareTest(unittest.TestCase):

    def test_identical_agrees(self):
        f = _fp(mag_1=[1.0, 4.0], strain=[0.25, 0.9])
        self.assertIsNone(flow_probe.compare(f, copy.deepcopy(f)))

    def test_drift_below_tolerance_agrees(self):
        a = _fp(mag_1=[1.0, 4.0])
        b = _fp(mag_1=[1.0 * (1 + flow_probe.RTOL / 2), 4.0])
        self.assertIsNone(flow_probe.compare(a, b))

    def test_change_above_tolerance_is_reported(self):
        a = _fp(mag_1=[1.0, 4.0])
        b = _fp(mag_1=[1.5, 4.0])
        note = flow_probe.compare(a, b)
        self.assertIsNotNone(note)
        self.assertIn('mag_1', note)

    def test_the_worst_offender_is_named(self):
        a = _fp(mag_1=[1.0, 4.0], strain=[1.0, 1.0])
        b = _fp(mag_1=[1.02, 4.0], strain=[3.0, 1.0])
        self.assertIn('strain', flow_probe.compare(a, b))

    def test_a_dropped_metric_is_reported(self):
        note = flow_probe.compare(_fp(mag_1=[1.0, 1.0], strain=[1.0, 1.0]),
                                  _fp(mag_1=[1.0, 1.0]))
        self.assertIn('no longer produces strain', note)

    def test_a_new_metric_is_reported(self):
        note = flow_probe.compare(_fp(mag_1=[1.0, 1.0]),
                                  _fp(mag_1=[1.0, 1.0], newthing=[1.0, 1.0]))
        self.assertIn('now also produces newthing', note)

    def test_a_long_metric_list_is_capped(self):
        """A whole-stack difference names 13 metrics; the count carries it, not the list."""
        a = _fp(mag_1=[1.0, 1.0])
        b = _fp(**{f'm{i}': [1.0, 1.0] for i in range(9)}, mag_1=[1.0, 1.0])
        note = flow_probe.compare(a, b)
        self.assertIn('6 more', note)
        self.assertLess(len(note), 120)

    def test_missing_either_side_is_not_a_mismatch(self):
        """"Could not be checked" is a different answer from "these disagree", and the caller says
        so differently."""
        f = _fp(mag_1=[1.0, 1.0])
        self.assertIsNone(flow_probe.compare({}, f))
        self.assertIsNone(flow_probe.compare(f, {}))
        self.assertIsNone(flow_probe.compare(None, f))

    def test_a_different_probe_version_is_not_compared(self):
        a = _fp(mag_1=[1.0, 1.0])
        b = _fp(mag_1=[9.0, 9.0])
        b['version'] = flow_probe.VERSION + 1
        self.assertIsNone(flow_probe.compare(a, b))

    def test_empty_metric_dicts_are_not_compared(self):
        self.assertIsNone(flow_probe.compare(_fp(), _fp(mag_1=[1.0, 1.0])))

    def test_a_zero_summary_becoming_nonzero_is_caught(self):
        """The case relative tolerance alone cannot see: a metric that was constant on the probe and
        is not any more."""
        note = flow_probe.compare(_fp(vorticity=[0.0, 0.0]), _fp(vorticity=[0.0, 0.5]))
        self.assertIn('vorticity', note)


_HAS_COASTAL = importlib.util.find_spec('coastal') is not None


@unittest.skipUnless(_HAS_COASTAL, 'coastal is not installed')
class AgainstTheRealEngineTest(unittest.TestCase):
    """The probe against coastal itself. Skipped where coastal is absent, like every other
    coastal-dependent test in this suite."""

    def test_fingerprint_has_the_metric_stack(self):
        f = flow_probe.fingerprint()
        self.assertEqual(f['version'], flow_probe.VERSION)
        self.assertGreater(len(f['metrics']), 5)
        for name, stats in f['metrics'].items():
            self.assertEqual(len(stats), 2, name)

    def test_reproducible_in_process(self):
        self.assertIsNone(flow_probe.compare(flow_probe.fingerprint(),
                                             flow_probe.fingerprint()))

    def test_it_catches_a_real_estimator_swap(self):
        """The guard. Replace Farneback with DIS — the swap this whole field exists to detect — and
        the fingerprint must disagree.

        DIS only accepts 8-bit input (`cv2.error: I0.depth() == CV_8U`), so the cast is part of the
        swap rather than an unfair handicap: any real adoption of DIS would have to reintroduce it.
        """
        import cv2
        import coastal.flow as cflow

        recorded = flow_probe.fingerprint()

        dis = cv2.DISOpticalFlow_create(cv2.DISOPTICAL_FLOW_PRESET_FAST)

        def dis_flow(f1, f2):
            def to8(a):
                a = np.asarray(a, dtype=np.float32)
                lo, hi = float(a.min()), float(a.max())
                return np.clip((a - lo) / max(hi - lo, 1e-9) * 255.0, 0, 255).astype(np.uint8)
            fl = dis.calc(to8(f1), to8(f2), None)
            return fl[..., 0], fl[..., 1]

        original = cflow.calc_flow_farneback_between_frames
        cflow.calc_flow_farneback_between_frames = dis_flow
        try:
            swapped = flow_probe.fingerprint()
        finally:
            cflow.calc_flow_farneback_between_frames = original

        self.assertTrue(swapped, 'the swapped engine produced no fingerprint at all')
        self.assertIsNotNone(flow_probe.compare(recorded, swapped),
                             'a DIS swap went undetected')
        # and the original is restored, so the mismatch was the swap and not the patching
        self.assertIsNone(flow_probe.compare(recorded, flow_probe.fingerprint()))


if __name__ == '__main__':
    unittest.main()
