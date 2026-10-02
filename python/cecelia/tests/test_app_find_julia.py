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
        os.makedirs(bin_dir)
        open(os.path.join(bin_dir, "julia"), "w", encoding="utf-8").close()
        env = {"PATH": "/usr/local/bin:/usr/bin", "JULIAUP_DEPOT_PATH": "/Users/x/.julia/juliaup"}
        with mock.patch.dict(os.environ, env, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), os.path.join(bin_dir, "julia"))
            self.assertEqual(os.environ["JULIAUP_DEPOT_PATH"], os.path.join(self.root, "juliaup"))
            self.app._find_julia()          # called again on every reprovision: PATH must not grow
            self.assertEqual(os.environ["PATH"], bin_dir + os.pathsep + "/usr/local/bin:/usr/bin")

    def test_without_it_path_julia_is_used_and_env_untouched(self):
        env = {"PATH": "/usr/local/bin:/usr/bin"}
        with mock.patch.dict(os.environ, env, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), "/usr/local/bin/julia")
            self.assertNotIn("JULIAUP_DEPOT_PATH", os.environ)
            self.assertEqual(os.environ["PATH"], "/usr/local/bin:/usr/bin")


if __name__ == "__main__":
    unittest.main()
