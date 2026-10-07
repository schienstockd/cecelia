"""The launcher's thread flag (``app.py`` → ``_thread_args``).

The installed app starts the API through ``app.py``, so this is where its thread count is decided.
Settings → System → "Use all CPU cores" writes ``[server] multithreaded`` to ``custom.toml`` (Julia:
``set_api_multithreaded!``); the launcher reads it here, default ON.

The TOML below is the literal shape Julia's ``TOML.print`` writes — the Julia testset pins the
writer to it, so the two sides of the key cannot drift apart silently.
"""
import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock

APP_PY = pathlib.Path(__file__).resolve().parents[3] / "app.py"
_DEV_DIR_VAR = "CECELIA_DEV_DIR"   # DEV-DIR-OK: always pointed at a fresh temp dir below


def _load_app():
    spec = importlib.util.spec_from_file_location("cecelia_app_launcher_threads", APP_PY)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class ThreadArgsTest(unittest.TestCase):
    def setUp(self):
        self.app = _load_app()
        self.root = tempfile.mkdtemp()          # install root: no .env
        self.cfg = tempfile.mkdtemp()           # config dir: holds custom.toml
        self.app.ROOT = self.root

    def _env(self, **extra):
        return mock.patch.dict(os.environ, {_DEV_DIR_VAR: self.cfg, **extra}, clear=True)

    def _write(self, text):
        with open(os.path.join(self.cfg, "custom.toml"), "w", encoding="utf-8") as f:
            f.write(text)

    def test_default_is_all_cores(self):
        with self._env():
            self.assertEqual(self.app._thread_args(), (["-t", "auto"], "auto"))

    def test_setting_off_is_one_thread(self):
        self._write('[dirs]\nprojects = "/x"\n\n[server]\nmultithreaded = false\n')
        with self._env():
            self.assertEqual(self.app._thread_args(), (["-t", "1"], "1"))

    def test_setting_on(self):
        self._write("[server]\nmultithreaded = true\n")
        with self._env():
            self.assertEqual(self.app._thread_args(), (["-t", "auto"], "auto"))

    def test_julia_num_threads_wins(self):
        self._write("[server]\nmultithreaded = false\n")
        with self._env(JULIA_NUM_THREADS="4"):
            self.assertEqual(self.app._thread_args(), ([], "env"))

    def test_bad_toml_or_bad_value_reads_as_default(self):
        self._write("[server\nmultithreaded = nope")
        with self._env():
            self.assertEqual(self.app._thread_args()[1], "auto")
        self._write('[server]\nmultithreaded = "no"\n')
        with self._env():
            self.assertEqual(self.app._thread_args()[1], "auto")

    def test_config_dir_resolution(self):
        # env → .env in the install root → ~/.cecelia, the same order as Julia's `config_dir`.
        with self._env():
            self.assertEqual(self.app._config_dir(), self.cfg)
        dot = tempfile.mkdtemp()
        with open(os.path.join(self.root, ".env"), "w", encoding="utf-8") as f:
            f.write(f"# comment\n{_DEV_DIR_VAR}={dot}\n")
        with mock.patch.dict(os.environ, {}, clear=True):
            self.assertEqual(self.app._config_dir(), dot)
        os.remove(os.path.join(self.root, ".env"))
        with mock.patch.dict(os.environ, {"HOME": self.root, "USERPROFILE": self.root}, clear=True):
            self.assertEqual(self.app._config_dir(), os.path.join(self.root, ".cecelia"))


class LaunchCommandTest(unittest.TestCase):
    """The regression itself: `main()` must put the flag on the julia command line and tell the
    server what it applied. Popen / health probe / browser are stubbed; nothing is started."""

    def setUp(self):
        self.app = _load_app()
        self.root = tempfile.mkdtemp()
        self.cfg = tempfile.mkdtemp()
        self.app.ROOT = self.root

    def _launch(self, **extra):
        proc = mock.Mock()
        proc.wait.return_value = 0          # the in-app Quit: main returns, no relaunch
        proc.poll.return_value = 0
        with mock.patch.dict(os.environ, {_DEV_DIR_VAR: self.cfg, **extra}, clear=True), \
             mock.patch.object(self.app, "_find_julia", return_value="julia"), \
             mock.patch.object(self.app, "_server_ready", return_value=True), \
             mock.patch.object(self.app.webbrowser, "open"), \
             mock.patch.object(self.app.subprocess, "Popen", return_value=proc) as popen:
            self.assertEqual(self.app.main(), 0)
        (cmd,), kw = popen.call_args
        return cmd, kw["env"]

    def test_default_launch_is_multithreaded(self):
        cmd, env = self._launch()
        self.assertEqual(cmd, ["julia", "--project", "-t", "auto", "src/server.jl"])
        self.assertEqual(env["CECELIA_LAUNCH_THREADS"], "auto")
        self.assertEqual(env["CECELIA_SUPERVISED"], "1")

    def test_setting_off_launches_one_thread(self):
        with open(os.path.join(self.cfg, "custom.toml"), "w", encoding="utf-8") as f:
            f.write("[server]\nmultithreaded = false\n")
        cmd, env = self._launch()
        self.assertEqual(cmd, ["julia", "--project", "-t", "1", "src/server.jl"])
        self.assertEqual(env["CECELIA_LAUNCH_THREADS"], "1")


if __name__ == "__main__":
    unittest.main()
