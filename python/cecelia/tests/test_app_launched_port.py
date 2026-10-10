"""The launcher learns the server's port from its lock (``app.py`` → ``_launched_port``).

Several users can run Cecelia on one machine; the SERVER picks its port slot (``app/src/ports.jl``),
so the launcher cannot assume 8080. It reads ``api_port`` back from ``<config_dir>/cecelia.lock``,
which ``acquire_single_instance!`` writes before binding — and must not believe a lock left over
from an earlier run, which names a port nobody is on.
"""
import importlib.util
import json
import os
import pathlib
import tempfile
import time
import unittest
from unittest import mock

APP_PY = pathlib.Path(__file__).resolve().parents[3] / "app.py"
_DEV_DIR_VAR = "CECELIA_DEV_DIR"   # DEV-DIR-OK: always pointed at a fresh temp dir below


def _load_app():
    spec = importlib.util.spec_from_file_location("cecelia_app_launcher_port", APP_PY)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _Proc:
    """A stand-in `Popen`: running until `code` is set."""
    def __init__(self, code=None):
        self.code = code

    def poll(self):
        return self.code


class LaunchedPortTest(unittest.TestCase):
    def setUp(self):
        self.app = _load_app()
        self.cfg = tempfile.mkdtemp()
        self.lock = os.path.join(self.cfg, "cecelia.lock")

    def _env(self):
        return mock.patch.dict(os.environ, {_DEV_DIR_VAR: self.cfg}, clear=True)

    def _write_lock(self, port, mtime=None):
        with open(self.lock, "w", encoding="utf-8") as f:
            json.dump({"pid": 1, "startedAt": "", "host": "127.0.0.1", "api_port": port}, f)
        if mtime is not None:
            os.utime(self.lock, (mtime, mtime))

    def test_reads_the_port_the_server_chose(self):
        self._write_lock(8090)
        with self._env():
            self.assertEqual(self.app._launched_port(_Proc(), time.time() - 5, timeout=2), "8090")

    def test_ignores_a_lock_from_an_earlier_run(self):
        self._write_lock(8080, mtime=time.time() - 3600)
        with self._env():
            self.assertIsNone(self.app._launched_port(_Proc(), time.time(), timeout=1))

    def test_gives_up_when_the_server_exits(self):
        # e.g. it refused because Cecelia is already running for this user — no lock of its own
        with self._env():
            t0 = time.time()
            self.assertIsNone(self.app._launched_port(_Proc(code=1), t0, timeout=30))
            self.assertLess(time.time() - t0, 5)


if __name__ == "__main__":
    unittest.main()
