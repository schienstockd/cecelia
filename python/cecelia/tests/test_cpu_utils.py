"""`cpu_utils` — the two thread budgets: BLAS pools underneath a task, and the task's own.

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

    def assertCappedAt(self, budget):
        """Every pool at OR BELOW the budget. That is the whole contract — nothing stronger holds.

        Two CI failures taught this, both from asserting more than is true:

        * exact equality (`[4, 4, 2] != [4, 4, 4]`, windows-latest) — some backends clamp the limit
          to their own maximum, so a pool can end up BELOW the budget;
        * "it never raises a pool" (`4 not less than or equal to 3`, macos-latest) — others simply
          SET the limit, so a pool that was below the budget can come up to it.

        Neither direction is guaranteed across backends. "No pool exceeds the budget" is, and it is
        the only part the caller depends on. Both failures passed on a 32-core Linux dev box, which
        is exactly why the assertion has to be the property rather than the local observation.
        """
        got = _blas_limits()
        self.assertTrue(got, 'no BLAS pool to check')
        for n in got:
            self.assertLessEqual(n, budget, f'pool above the budget: {got}')

    def test_caps_inside_and_restores_after(self):
        """Restoring matters as much as capping: this wraps one phase of a long-lived runner, and
        leaking a 4-thread cap onto whatever it does next would be a silent slowdown elsewhere."""
        before = _blas_limits()
        with cpu_utils.limit_blas_threads(2):
            self.assertCappedAt(2)
        self.assertEqual(_blas_limits(), before)

    def test_restores_even_when_the_block_raises(self):
        before = _blas_limits()
        with self.assertRaises(RuntimeError):
            with cpu_utils.limit_blas_threads(1):
                raise RuntimeError('boom')
        self.assertEqual(_blas_limits(), before)

    def test_default_is_the_measured_small_matmul_budget(self):
        with cpu_utils.limit_blas_threads():
            self.assertCappedAt(cpu_utils.BLAS_THREADS_SMALL_MATMUL)

    def test_nests(self):
        with cpu_utils.limit_blas_threads(8):
            outer = _blas_limits()
            with cpu_utils.limit_blas_threads(2):
                self.assertCappedAt(2)
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


class DefaultTaskWorkersTest(unittest.TestCase):

    def test_scales_with_the_machine(self):
        small = cpu_utils.default_task_workers(4)
        big = cpu_utils.default_task_workers(64)
        self.assertLess(small, big, 'a bigger box must get a bigger budget')

    def test_never_wider_than_the_box(self):
        for n in (1, 2, 3, 4, 8):
            self.assertLessEqual(cpu_utils.default_task_workers(n), n)

    def test_a_small_machine_is_not_left_serial(self):
        """cores // 4 alone gives 1 thread on a 4-core laptop — which is the un-parallelised path."""
        self.assertGreaterEqual(cpu_utils.default_task_workers(4), 2)

    def test_a_huge_machine_is_capped(self):
        self.assertEqual(cpu_utils.default_task_workers(1024),
                         cpu_utils.default_task_workers(128))


class TaskWorkersTest(unittest.TestCase):

    def test_the_environment_wins(self):
        self.assertEqual(cpu_utils.task_workers(env={cpu_utils.TASK_WORKERS_ENV: '12'}), 12)

    def test_an_algorithmic_cap_is_a_ceiling_not_a_preference(self):
        """Coastal's region growing peaks at 4 threads and is slower at 8 — no machine changes that."""
        env = {cpu_utils.TASK_WORKERS_ENV: '32'}
        self.assertEqual(cpu_utils.task_workers(cap=4, env=env), 4)
        # …but it must not RAISE a smaller budget up to the cap
        self.assertEqual(cpu_utils.task_workers(cap=4, env={cpu_utils.TASK_WORKERS_ENV: '2'}), 2)

    def test_an_unusable_setting_falls_back_rather_than_raising(self):
        """Every value here is a performance choice; a typo should not stop a run."""
        for bad in ('', 'eight', '0', '-4', None):
            with self.subTest(bad=bad):
                env = {} if bad is None else {cpu_utils.TASK_WORKERS_ENV: bad}
                self.assertEqual(cpu_utils.task_workers(env=env),
                                 cpu_utils.default_task_workers())


class CoastalUsesTheBudgetTest(unittest.TestCase):

    def test_the_two_stages_take_their_widths_from_it(self):
        from cecelia.utils import coastal_utils
        self.assertEqual(coastal_utils.FLOW_WORKERS, cpu_utils.task_workers())
        self.assertEqual(coastal_utils.PREDICT_WORKERS,
                         cpu_utils.task_workers(cap=coastal_utils.PREDICT_WORKER_CAP))
        self.assertLessEqual(coastal_utils.PREDICT_WORKERS, coastal_utils.FLOW_WORKERS)


if __name__ == '__main__':
    unittest.main()


class MemoryBoundedConcurrencyTest(unittest.TestCase):
    """`concurrency_for_memory` — how wide a RAM-bounded stage may run.

    Written for the flow-metrics loop in `opticalFlow.train`, which ran strictly one sequence at a
    time. That was correct when a sequence was a whole 1046x1104 movie (~1.55 GB of live float32
    planes) and pure waste once sequences became 256x256 crops (~0.24 GB) — the same constant, two
    orders of decision apart. So the width is derived from a MEASURED per-unit cost, and this is the
    arithmetic that turns the measurement into a number.
    """

    def test_divides_the_reserved_budget_by_the_measured_cost(self):
        # 8 GB available, half reserved, 1 GB per unit -> 4
        self.assertEqual(4, cpu_utils.concurrency_for_memory(2**30, 8 * 2**30, cap=16))

    def test_the_cpu_cap_still_wins_when_memory_is_plentiful(self):
        # this is CPU work too; RAM saying "32 fit" does not mean the throttle allows 32
        self.assertEqual(6, cpu_utils.concurrency_for_memory(2**20, 64 * 2**30, cap=6))

    def test_never_below_one_even_when_nothing_fits(self):
        # the work has to happen; refusing to run is not an option the caller has
        self.assertEqual(1, cpu_utils.concurrency_for_memory(100 * 2**30, 2**30, cap=8))

    def test_half_is_held_back_because_one_observation_is_not_the_worst_case(self):
        # 4 GB available, 1 GB per unit: 4 would fit exactly, so 2 is returned
        self.assertEqual(2, cpu_utils.concurrency_for_memory(2**30, 4 * 2**30, cap=16))
        # …and the reserve is adjustable for a caller that knows its peak is stable
        self.assertEqual(4, cpu_utils.concurrency_for_memory(2**30, 4 * 2**30, cap=16, reserve=1.0))

    def test_unmeasurable_memory_is_a_conservative_step_up_not_an_extreme(self):
        # Windows has no stdlib answer. Staying at 1 makes the platform gratuitously slower; taking
        # the cap is guessing with someone else's RAM.
        self.assertEqual(2, cpu_utils.concurrency_for_memory(2**30, None, cap=16))
        self.assertEqual(1, cpu_utils.concurrency_for_memory(2**30, None, cap=1))   # cap still wins

    def test_an_unmeasurable_per_unit_cost_falls_back_to_the_cpu_cap(self):
        # we know the memory but not the unit: the CPU budget is then the only real bound
        self.assertEqual(8, cpu_utils.concurrency_for_memory(None, 8 * 2**30, cap=8))
        self.assertEqual(8, cpu_utils.concurrency_for_memory(0, 8 * 2**30, cap=8))

    def test_the_live_readings_are_sane_on_this_machine(self):
        avail = cpu_utils.available_memory_bytes()
        if avail is not None:
            self.assertGreater(avail, 0)
        rss = cpu_utils.rss_bytes()
        if rss is not None:
            self.assertGreater(rss, 2**20)          # this interpreter is bigger than a megabyte
        peak = cpu_utils.peak_rss_bytes()
        if peak is not None and rss is not None:
            self.assertGreaterEqual(peak, rss * 0.5)   # the high-water mark is not below current use


class LinearStageWideningTest(unittest.TestCase):
    """`scales_linearly=True` — the escape hatch for a stage measured to keep scaling.

    The budget's divisor assumes four tasks are computing at once, which is ~2x pessimistic for a
    lone run. Coastal's flow metrics are the case that earns it (14x at 32 threads against 6.8x at 8
    — docs/SCHEDULER.md). Every guard below exists because the opposite behaviour would be wrong in a
    way nobody would notice: a slider silently exceeded, a cap silently ignored, or a stage widened
    on a machine where the process cannot touch the cores it asked for.
    """

    ENV = cpu_utils.TASK_WORKERS_ENV
    WIDEN = cpu_utils.TASK_WORKERS_WIDEN_ENV
    CPUS = cpu_utils.USABLE_CPUS_ENV

    def test_widening_is_off_unless_asked_for(self):
        """Default off, and not because it is unproven: several tasks CAN run at once, so widening
        every one of them oversubscribes the box. That is the user's call."""
        env = {self.ENV: '8', self.CPUS: '32'}
        self.assertEqual(cpu_utils.task_workers(scales_linearly=True, env=env), 8)

    def test_a_widened_linear_stage_takes_the_usable_cpus(self):
        env = {self.ENV: '8', self.WIDEN: '1', self.CPUS: '32'}
        self.assertEqual(cpu_utils.task_workers(scales_linearly=True, env=env), 32)

    def test_a_stage_that_did_not_claim_linear_scaling_is_untouched(self):
        """The flag is per CALL SITE, and only a measurement earns it. Coastal's region growing
        passes `cap=` instead, because its curve turns down."""
        env = {self.ENV: '8', self.WIDEN: '1', self.CPUS: '32'}
        self.assertEqual(cpu_utils.task_workers(env=env), 8)

    def test_an_algorithmic_cap_still_wins(self):
        """A cap is a ceiling, not a budget — the two arguments are not contradictory, and the
        degrades-past-4 measurement must survive the widening flag."""
        env = {self.ENV: '8', self.WIDEN: '1', self.CPUS: '32'}
        self.assertEqual(cpu_utils.task_workers(cap=4, scales_linearly=True, env=env), 4)

    def test_widening_never_lowers_the_budget(self):
        """On a machine where the usable count is BELOW a configured budget, this must not become a
        back-door way to shrink it — `max`, not assignment."""
        env = {self.ENV: '16', self.WIDEN: '1', self.CPUS: '4'}
        self.assertEqual(cpu_utils.task_workers(scales_linearly=True, env=env), 16)

    def test_the_usable_count_is_taken_as_given_not_re_derived(self):
        """Julia passes `usable_cpus()` — the affinity mask and cgroup quota applied. Re-deriving it
        from `os.cpu_count()` would hand out threads for CPUs this process cannot touch, which is
        the whole reason that helper exists."""
        env = {self.ENV: '2', self.WIDEN: '1', self.CPUS: '6'}
        self.assertEqual(cpu_utils.task_workers(scales_linearly=True, env=env), 6)

    def test_a_missing_usable_count_falls_back_without_raising(self):
        """Reached only outside `run_py` — a REPL session, a test, an external consumer."""
        env = {self.ENV: '2', self.WIDEN: '1'}
        got = cpu_utils.task_workers(scales_linearly=True, env=env)
        self.assertGreaterEqual(got, 2)

    def test_only_an_affirmative_flag_counts(self):
        for raw in ('0', '', 'no', 'off', 'false', 'maybe'):
            env = {self.ENV: '8', self.WIDEN: raw, self.CPUS: '32'}
            self.assertEqual(cpu_utils.task_workers(scales_linearly=True, env=env), 8, raw)
