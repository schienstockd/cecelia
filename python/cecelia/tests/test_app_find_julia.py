"""The launcher's Julia resolution (``app.py`` → ``_find_julia``).

``install.sh`` puts a Cecelia-owned juliaup in ``<install>/juliaup`` for system scope, and on Apple
Silicon when the Julia on PATH is an Intel build. The launcher must run THAT Julia, with juliaup
pointed at its own state, or the server comes up under Rosetta again.
"""
import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock

APP_PY = pathlib.Path(__file__).resolve().parents[3] / "app.py"


def _load_app():
    spec = importlib.util.spec_from_file_location("cecelia_app_launcher", APP_PY)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class FindJuliaTest(unittest.TestCase):
    def setUp(self):
        self.app = _load_app()
        self.root = tempfile.mkdtemp()
        self.app.ROOT = self.root

    def test_install_owned_juliaup_wins_over_path(self):
        bin_dir = os.path.join(self.root, "juliaup", "bin")
        julia = os.path.join(bin_dir, self.app._exe("julia"))   # julia.exe on the Windows runner
        os.makedirs(bin_dir)
        open(julia, "w", encoding="utf-8").close()
        env = {"PATH": "/usr/local/bin:/usr/bin", "JULIAUP_DEPOT_PATH": "/Users/x/.julia/juliaup"}
        with mock.patch.dict(os.environ, env, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), julia)
            self.assertEqual(os.environ["JULIAUP_DEPOT_PATH"], os.path.join(self.root, "juliaup"))
            self.app._find_julia()          # called again on every reprovision: PATH must not grow
            self.assertEqual(os.environ["PATH"], bin_dir + os.pathsep + "/usr/local/bin:/usr/bin")

    def test_system_scope_stacks_a_writable_depot_over_the_shared_one(self):
        # <root>/juliaup/depot exists only for a system install, which other accounts see read-only.
        bin_dir = os.path.join(self.root, "juliaup", "bin")
        shared = os.path.join(self.root, "juliaup", "depot")
        os.makedirs(bin_dir)
        os.makedirs(shared)
        open(os.path.join(bin_dir, self.app._exe("julia")), "w", encoding="utf-8").close()
        home = tempfile.mkdtemp()
        with mock.patch.dict(os.environ, {"PATH": "/usr/bin", "HOME": home, "USERPROFILE": home}, clear=True):
            self.app._find_julia()
            self.assertEqual(os.environ["JULIA_DEPOT_PATH"].split(os.pathsep),
                             [os.path.join(home, ".cecelia", "julia-depot"), shared, ""])

    def test_user_scope_private_juliaup_leaves_depot_alone(self):
        # The Apple-Silicon user-scope juliaup has no shared depot: the user's own ~/.julia stays.
        bin_dir = os.path.join(self.root, "juliaup", "bin")
        os.makedirs(bin_dir)
        open(os.path.join(bin_dir, self.app._exe("julia")), "w", encoding="utf-8").close()
        with mock.patch.dict(os.environ, {"PATH": "/usr/bin"}, clear=True):
            self.app._find_julia()
            self.assertNotIn("JULIA_DEPOT_PATH", os.environ)

    def test_without_it_path_julia_is_used_and_env_untouched(self):
        env = {"PATH": "/usr/local/bin:/usr/bin"}
        with mock.patch.dict(os.environ, env, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), "/usr/local/bin/julia")
            self.assertNotIn("JULIAUP_DEPOT_PATH", os.environ)
            self.assertEqual(os.environ["PATH"], "/usr/local/bin:/usr/bin")


class FindBinaryWindowsTest(unittest.TestCase):
    """On Windows the hand-built fallback paths must carry `.exe`: install.ps1 puts
    `~\\.juliaup\\bin\\julia.exe`, and a GUI launch whose PATH lacks juliaup reaches these fallbacks.
    Simulated by patching `sys.platform`, which is what `app._exe` branches on."""

    def setUp(self):
        self.app = _load_app()
        self.root = tempfile.mkdtemp()
        self.home = tempfile.mkdtemp()
        self.app.ROOT = self.root

    def _touch(self, *parts):
        path = os.path.join(*parts)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        open(path, "w", encoding="utf-8").close()
        return path

    def _windows(self, env):
        env = {"HOME": self.home, "USERPROFILE": self.home, **env}
        return (mock.patch.dict(os.environ, env, clear=True),
                mock.patch("sys.platform", "win32"),
                mock.patch("shutil.which", return_value=None))

    def test_user_juliaup_exe_is_found_when_not_on_path(self):
        exe = self._touch(self.home, ".juliaup", "bin", "julia.exe")
        a, b, c = self._windows({"PATH": "/usr/bin"})
        with a, b, c:
            self.assertEqual(self.app._find_julia(), exe)

    def test_install_owned_juliaup_exe_wins(self):
        exe = self._touch(self.root, "juliaup", "bin", "julia.exe")
        a, b, c = self._windows({"PATH": "/usr/bin"})
        with a, b, c:
            self.assertEqual(self.app._find_julia(), exe)
            self.assertEqual(os.environ["JULIAUP_DEPOT_PATH"], os.path.join(self.root, "juliaup"))

    def test_pixi_fallback_is_exe(self):
        a, b, c = self._windows({"PATH": "/usr/bin"})
        with a, b, c:
            self.assertEqual(self.app._find_pixi(), os.path.join(self.home, ".pixi", "bin", "pixi.exe"))


if __name__ == "__main__":
    unittest.main()
