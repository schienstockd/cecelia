"""`cpu_utils.limit_blas_threads` — the BLAS thread budget for small-matmul work.

A numpy/scipy call that lands in BLAS takes every core by default, and every Python task here runs
inside one `cpu` pool slot, so `n` concurrent tasks ask for `n × cores`. On drift estimation that is
not merely wasteful, it is SLOWER than capping — the sub-pixel refinement is many small matmuls, and
past a handful of threads OpenBLAS spends more on fan-out than on arithmetic. Measured numbers are
on the module docstring.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np                                   # noqa: F401 — loads BLAS so it is introspectable

import cecelia.utils.cpu_utils as cpu_utils


def _blas_limits():
    try:
        import threadpoolctl
    except ImportError:                              # pragma: no cover
        return None
    return [d['num_threads'] for d in threadpoolctl.threadpool_info()
            if d.get('user_api') == 'blas']


class LimitBlasThreadsTest(unittest.TestCase):
    def setUp(self):
        if not _blas_limits():
            self.skipTest('no introspectable BLAS pool in this environment')

    def test_caps_inside_and_restores_after(self):
        """Restoring matters as much as capping: this wraps one phase of a long-lived runner, and
        leaking a 4-thread cap onto whatever it does next would be a silent slowdown elsewhere."""
        before = _blas_limits()
        with cpu_utils.limit_blas_threads(2):
            self.assertEqual(_blas_limits(), [2] * len(before))
        self.assertEqual(_blas_limits(), before)

    def test_restores_even_when_the_block_raises(self):
        before = _blas_limits()
        with self.assertRaises(RuntimeError):
            with cpu_utils.limit_blas_threads(1):
                raise RuntimeError('boom')
        self.assertEqual(_blas_limits(), before)

    def test_default_is_the_measured_small_matmul_budget(self):
        with cpu_utils.limit_blas_threads():
            self.assertEqual(_blas_limits(),
                             [cpu_utils.BLAS_THREADS_SMALL_MATMUL] * len(_blas_limits()))

    def test_nests(self):
        with cpu_utils.limit_blas_threads(8):
            outer = _blas_limits()
            with cpu_utils.limit_blas_threads(2):
                self.assertEqual(_blas_limits(), [2] * len(outer))
            self.assertEqual(_blas_limits(), outer)

    def test_the_budget_is_a_small_positive_number(self):
        """A guard on the constant itself: 1 was measured SLOWER than 4 (the work is genuinely
        parallel, just not 32-ways parallel), and anything large defeats the point."""
        self.assertGreaterEqual(cpu_utils.BLAS_THREADS_SMALL_MATMUL, 2)
        self.assertLessEqual(cpu_utils.BLAS_THREADS_SMALL_MATMUL, 8)


class DriftUsesTheBudgetTest(unittest.TestCase):
    """The call site, not just the helper — the drift pair loop is the measured beneficiary, and a
    refactor that drops the wrapper would silently give back a 1.8x/4.4x win."""

    def test_pair_measurement_loop_limits_blas(self):
        import inspect
        import cecelia.utils.correction_utils as cu
        src = inspect.getsource(cu._drift_pair_measurements)
        self.assertIn('limit_blas_threads', src,
                      'drift pair measurement must bound the BLAS pool — see cpu_utils')


if __name__ == '__main__':
    unittest.main()
