"""Colocalisation primitives — validated by INJECTION, which is the only honest way to test an
estimator: build a signal whose answer is known, and check the number that comes back.

The reference behaviours are Costes et al. 2004 (doi:10.1529/biophysj.103.038422); the injection
protocol is the one `docs/todo/AF_CORRECTION_AUDIT.md` used to establish that bleedthrough and
broadband autofluorescence are separable — add a known `alpha * source` to the target and see whether
the estimator finds it back while a real co-present structure is present.
"""
import unittest

import numpy as np

from cecelia.utils import coloc_utils as cl


def _scene(rng, n=200_000, alpha=0.0, co_positive=True):
    """A source channel with structure, a target with its own structure, optional co-presence, and
    `alpha` of the source leaked into the target. Backgrounds are already subtracted, as they are by
    the time `af_correct_frame` calls any of this."""
    src = np.clip(rng.exponential(30.0, n), 0, None)
    bright = rng.random(n) < 0.02                      # ONE mask — two draws would not line up
    src = src + bright * rng.uniform(200, 2000, n)     # bright structures in the source
    tgt = np.clip(rng.exponential(8.0, n), 0, None)
    if co_positive:
        # a population bright in BOTH for a real reason — must NOT move the envelope
        both = rng.random(n) < 0.01
        tgt = tgt + both * rng.uniform(100, 400, n)
        src = src + both * rng.uniform(100, 400, n)
    return src, tgt + alpha * src


class EnvelopeSlopeTest(unittest.TestCase):
    """`envelope_slope` is the estimator whose number gets SUBTRACTED, so its bias matters in one
    direction: it must not over-estimate, or real target signal is removed."""

    def test_it_recovers_an_injected_alpha(self):
        """Pins the measured accuracy, not an aspiration: recovery runs high, by +10% at alpha=0.2 up
        to +55% at alpha=0.01. Erring high is the direction a caller can defend against (clamp the
        subtraction at zero); the upper bound is here so a change that made it worse cannot pass."""
        for alpha in (0.01, 0.02, 0.05, 0.10, 0.20):
            src, tgt = _scene(np.random.default_rng(7), alpha=alpha, co_positive=False)
            got, r2, nbins = cl.envelope_slope(src, tgt)
            self.assertGreater(nbins, 20)
            self.assertGreater(got, 0.9 * alpha, f'alpha={alpha} under-recovered as {got}')
            self.assertLess(got, 1.7 * alpha, f'alpha={alpha} OVER-recovered as {got}')
            self.assertGreater(r2, 0.9, f'a clean injected leak must fit well, got R2={r2}')

    def test_it_reports_no_leak_when_there_is_none(self):
        """The result that matters most, because it is the one that licenses subtracting nothing. A
        co-present population is present throughout and must not be mistaken for a proportional leak.

        This is what the free intercept bought: forced through the origin the same scene reported
        0.0064, which is a 0.6% leak invented out of the target's own noise floor."""
        for co in (False, True):
            src, tgt = _scene(np.random.default_rng(7), alpha=0.0, co_positive=co)
            got, r2, _n = cl.envelope_slope(src, tgt)
            self.assertLess(abs(got), 0.005, f'invented a leak of {got} (co_positive={co})')
            # R^2 is deliberately NOT a leak test, and the numbers here are why: these two leak-free
            # scenes score 0.47 and 0.08, straddling the 0.43 that the REAL leak on `WIaUjL/p6t4mC`
            # scores. No threshold on it separates leak from no-leak in either direction. It is
            # reported because it says how well determined the slope is; the slope is the evidence.
            self.assertTrue(np.isfinite(r2))

    def test_a_co_present_structure_barely_moves_the_envelope(self):
        """Bleedthrough is the FLOOR of the joint distribution; co-presence sits above it. The
        separation is not perfect — a population bright in both does lift the floor a little, because
        it raises the source in the same voxels — but it must stay small next to the leak itself,
        or the two are not separable and none of this holds."""
        for alpha in (0.05, 0.20):
            a, _ra, _na = cl.envelope_slope(*_scene(np.random.default_rng(7), alpha=alpha, co_positive=False))
            b, _rb, _nb = cl.envelope_slope(*_scene(np.random.default_rng(7), alpha=alpha, co_positive=True))
            # measured: 0.0608 -> 0.0768 (+26%) at alpha=0.05, 0.2203 -> 0.2697 (+22%) at 0.20, with a
            # deliberately strong co-positive population (1% of voxels, +100..400 counts in BOTH)
            self.assertLess(abs(b - a), 0.4 * alpha,
                            f'co-presence moved alpha={alpha} too far: {a:.4f} -> {b:.4f}')

    def test_too_little_data_reports_nan_rather_than_a_number(self):
        got, r2, nbins = cl.envelope_slope(np.arange(10.0), np.arange(10.0))
        self.assertTrue(np.isnan(got) and np.isnan(r2))
        self.assertEqual(nbins, 0)


class TlsSlopeTest(unittest.TestCase):

    def test_it_is_symmetric_in_the_two_channels(self):
        """The property ordinary least squares does not have, and the reason Costes specifies it: fit
        y on x, fit x on y, and the same line must come back. An OLS fit gives two different answers,
        so a threshold derived from one would depend on which channel was named first."""
        rng = np.random.default_rng(8)
        x = rng.normal(100, 30, 20_000)
        y = 0.4 * x + rng.normal(0, 8, 20_000)
        a_fwd, _ = cl.tls_slope(x, y)
        a_rev, _ = cl.tls_slope(y, x)
        self.assertAlmostEqual(a_fwd, 1.0 / a_rev, places=4)

    def test_an_unrelated_pair_has_no_principal_axis(self):
        rng = np.random.default_rng(9)
        a, b = cl.tls_slope(rng.normal(size=5000), np.ones(5000))
        self.assertTrue(np.isnan(a) and np.isnan(b))


class CostesThresholdTest(unittest.TestCase):

    def test_it_converges_on_a_pair_with_a_distinct_coloc_population(self):
        """Discrete colocalisation: dim voxels are unrelated, bright ones share a population. r must
        reach zero somewhere, and that crossing is the threshold."""
        rng = np.random.default_rng(10)
        n = 100_000
        x = np.clip(rng.exponential(20, n), 0, None)
        y = np.clip(rng.exponential(20, n), 0, None)
        both = rng.random(n) < 0.02
        x[both] += 500; y[both] += 500
        out = cl.costes_threshold(x, y)
        self.assertTrue(out['converged'], f'never crossed zero: minR={out["minR"]}')
        self.assertLessEqual(out['r'], 0.0)
        self.assertGreater(out['threshold'], 0.0)

    def test_it_does_NOT_converge_on_a_purely_proportional_pair(self):
        """The diagnostic half. A global proportional relationship — spillover — keeps the two channels
        correlated all the way down, so there is no level below which they stop explaining one another.
        `converged=False` is the answer, not a failure, and it is what tells a caller that subtracting a
        proportional term is the right move rather than thresholding.
        """
        rng = np.random.default_rng(11)
        x = np.clip(rng.exponential(40, 100_000), 0, None)
        y = 0.2 * x + rng.normal(0, 0.5, 100_000)      # leak only, no independent target signal
        out = cl.costes_threshold(x, y)
        self.assertFalse(out['converged'])
        self.assertIsNotNone(out['minR'])
        self.assertGreater(out['minR'], 0.1, 'a proportional pair should stay correlated throughout')

    def test_the_slope_it_reports_is_the_injected_one(self):
        rng = np.random.default_rng(12)
        x = np.clip(rng.exponential(40, 100_000), 0, None)
        y = 0.2 * x + rng.normal(0, 1.0, 100_000)
        out = cl.costes_threshold(x, y)
        self.assertAlmostEqual(out['slope'], 0.2, delta=0.02)

    def test_it_never_raises_on_degenerate_input(self):
        for x, y in ((np.zeros(10), np.zeros(10)),
                     (np.array([]), np.array([])),
                     (np.ones(500), np.arange(500.0))):
            out = cl.costes_threshold(x, y)
            self.assertIn('converged', out)
            self.assertFalse(out['converged'])


if __name__ == '__main__':
    unittest.main()
