"""The runner's exit-time usage report — `script_utils.register_usage_report` + `cpu_utils.peak_rss_bytes`.

`run_py` names a file in `CECELIA_USAGE_OUT`; the runner writes its peak resident memory there at exit,
and Julia stamps it on the run-log entry as `usage.peakRssBytes` (app/src/run_usage.jl). The Julia
testset `Run usage: a scheduled run's entry carries…` runs the whole loop; these pin the Python half on
every OS, including the Windows branch of `peak_rss_bytes` when CI runs there.
"""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from cecelia.utils import cpu_utils, script_utils

_PYTHON_DIR = str(Path(__file__).resolve().parents[2])      # python/, so the child imports cecelia.*

_ALLOC = """
from cecelia.utils import script_utils
script_utils.register_usage_report()
buf = b'\\x01' * ({mb} * 2**20)
"""


def _run_child(code, env_extra):
    env = dict(os.environ, PYTHONPATH=_PYTHON_DIR, **env_extra)
    return subprocess.run([sys.executable, '-c', code], env=env, capture_output=True, text=True,
                          encoding='utf-8', timeout=120)


class PeakRssTest(unittest.TestCase):
    def test_this_process_has_a_peak_on_every_platform(self):
        peak = cpu_utils.peak_rss_bytes()
        self.assertIsInstance(peak, int)
        self.assertGreater(peak, 2**20)                    # an interpreter is bigger than a megabyte

    def test_report_is_the_peak_in_bytes(self):
        rep = script_utils.usage_report()
        self.assertEqual(set(rep), {'peakRssBytes'})
        self.assertGreaterEqual(rep['peakRssBytes'], cpu_utils.peak_rss_bytes())


@unittest.skipUnless(os.path.exists('/proc/self/status'), 'Linux /proc only')
class LinuxHighWaterTest(unittest.TestCase):
    def test_peak_is_vmhwm_in_bytes(self):
        # proc(5): `VmHWM` = peak resident set size, in kB
        with open('/proc/self/status', encoding='utf-8') as fh:
            kb = next(int(l.split()[1]) for l in fh if l.startswith('VmHWM:'))
        self.assertAlmostEqual(cpu_utils._linux_vm_hwm(), kb * 1024, delta=64 * 2**20)
        self.assertAlmostEqual(cpu_utils.peak_rss_bytes(), kb * 1024, delta=64 * 2**20)


class UsageReportTest(unittest.TestCase):
    def test_no_env_no_report(self):
        self.assertIsNone(script_utils.register_usage_report(env={}))

    def test_registers_once_for_this_process(self):
        with mock.patch.object(script_utils, '_usage_report_registered', False), \
                mock.patch('atexit.register') as reg:
            self.assertEqual(script_utils.register_usage_report(env={'CECELIA_USAGE_OUT': '/x/u.json'}), '/x/u.json')
            reg.assert_called_once_with(script_utils.write_usage_report, '/x/u.json', os.getpid())
            self.assertIsNone(script_utils.register_usage_report(env={'CECELIA_USAGE_OUT': '/y/u.json'}))
            reg.assert_called_once()

    def test_reads_the_process_environment_by_default(self):
        with mock.patch.object(script_utils, '_usage_report_registered', False), \
                mock.patch('atexit.register') as reg, \
                mock.patch.dict(os.environ, {'CECELIA_USAGE_OUT': '/z/u.json'}):
            self.assertEqual(script_utils.register_usage_report(), '/z/u.json')
            reg.assert_called_once()

    def test_a_child_writes_its_own_peak_at_exit(self):
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, 'u.usage.json')
            r = _run_child(_ALLOC.format(mb=80), {script_utils.USAGE_OUT_ENV: out})
            self.assertEqual(r.returncode, 0, r.stderr)
            with open(out, encoding='utf-8') as fh:
                peak = json.load(fh)['peakRssBytes']
            # it held 80 MiB; and not the parent's memory (a forked child's ru_maxrss would carry ours)
            self.assertGreaterEqual(peak, 80 * 2**20)
            self.assertLess(peak, 80 * 2**20 + 200 * 2**20)

    def test_only_the_registering_process_writes(self):
        # a forked worker inherits the atexit hook; its exit must not overwrite the runner's report
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, 'u.usage.json')
            script_utils.write_usage_report(out, pid=os.getpid() + 1)
            self.assertFalse(os.path.exists(out))
            script_utils.write_usage_report(out, pid=os.getpid())
            self.assertTrue(os.path.exists(out))

    def test_a_crashing_runner_still_reports(self):
        # an uncaught exception still runs atexit — a failed run is when the peak matters most
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, 'u.usage.json')
            r = _run_child(_ALLOC.format(mb=1) + "\nraise RuntimeError('boom')\n",
                           {script_utils.USAGE_OUT_ENV: out})
            self.assertNotEqual(r.returncode, 0)
            self.assertTrue(os.path.exists(out))


@unittest.skipIf(os.name == 'nt', 'no RUSAGE_CHILDREN on Windows')
class ChildrenPeakTest(unittest.TestCase):
    def test_a_child_reaped_here_is_counted(self):
        subprocess.run([sys.executable, '-c', "b = b'\\x01' * (150 * 2**20)"], check=True, timeout=120)
        self.assertGreaterEqual(cpu_utils.children_peak_rss_bytes(), 150 * 2**20)

    def test_no_child_reads_none_not_zero(self):
        import resource
        with mock.patch.object(resource, 'getrusage', return_value=mock.Mock(ru_maxrss=0)):
            self.assertIsNone(cpu_utils.children_peak_rss_bytes())

    def test_windows_has_no_children_figure(self):
        # even with a child figure on offer, Windows answers None: RUSAGE_CHILDREN does not exist there
        import resource
        with mock.patch.object(cpu_utils.os, 'name', 'nt'), \
                mock.patch.object(resource, 'getrusage', return_value=mock.Mock(ru_maxrss=10**6)):
            self.assertIsNone(cpu_utils.children_peak_rss_bytes())

    def test_largest_waited_child_counts(self):
        r = _run_child(
            "import subprocess, sys\n"
            "subprocess.run([sys.executable, '-c', \"b = b'\\\\x01' * (150 * 2**20)\"], check=True)\n"
            "from cecelia.utils import cpu_utils\n"
            "print(cpu_utils.children_peak_rss_bytes())\n", {})
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertGreaterEqual(int(r.stdout.strip()), 150 * 2**20)


if __name__ == '__main__':
    unittest.main()
