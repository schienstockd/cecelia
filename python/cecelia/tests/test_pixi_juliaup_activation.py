"""Every `pixi run` task prefers the install's own juliaup (``scripts/activate_juliaup.sh``).

``app.py``'s ``_find_julia`` already does this for ``pixi run app`` (see test_app_find_julia.py). The
other tasks run a bare ``julia`` from PATH: ``stop*``, which the app's own errors tell installed users
to run, plus ``prod``, ``doctor``, ``julia-instantiate``, ``update-julia``. On an Apple Silicon user
install over an Intel Julia they started that Julia under Rosetta. pixi sources the activation script
before every task, so the rule lives there once, for all of them.
"""
import os
import pathlib
import shutil
import subprocess
import tempfile
import tomllib
import unittest

REPO = pathlib.Path(__file__).resolve().parents[3]
SCRIPT = REPO / "scripts" / "activate_juliaup.sh"


def _activated(root, path):
    """Source the script the way pixi does, in POSIX sh; return the resulting (PATH, JULIAUP_DEPOT_PATH)."""
    out = subprocess.run(
        ["sh", "-c", '. "$1"; . "$1"; printf "%s\\n%s" "$PATH" "${JULIAUP_DEPOT_PATH-<unset>}"',
         "sh", str(SCRIPT)],
        env={"PIXI_PROJECT_ROOT": root, "PATH": path},
        capture_output=True, text=True, check=True)
    return out.stdout.split("\n")


class JuliaupActivationTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root)

    def test_pixi_sources_it_on_unix(self):
        manifest = tomllib.loads((REPO / "pixi.toml").read_text(encoding="utf-8"))
        scripts = manifest["target"]["unix"]["activation"]["scripts"]
        self.assertIn("scripts/activate_juliaup.sh", scripts)
        self.assertTrue(SCRIPT.is_file())

    @unittest.skipIf(os.name == "nt", "unix-only script: pixi.toml wires it under [target.unix.activation]")
    def test_install_owned_juliaup_wins_over_path(self):
        bin_dir = os.path.join(self.root, "juliaup", "bin")
        os.makedirs(bin_dir)
        julia = os.path.join(bin_dir, "julia")
        open(julia, "w", encoding="utf-8").close()
        os.chmod(julia, 0o755)
        path, depot = _activated(self.root, "/usr/local/bin:/usr/bin:/bin")
        # sourced twice above (nested `pixi shell`): PATH must not grow
        self.assertEqual(path, bin_dir + ":/usr/local/bin:/usr/bin:/bin")
        self.assertEqual(depot, os.path.join(self.root, "juliaup"))

    @unittest.skipIf(os.name == "nt", "unix-only script: pixi.toml wires it under [target.unix.activation]")
    def test_dev_checkout_is_untouched(self):
        path, depot = _activated(self.root, "/usr/local/bin:/usr/bin:/bin")
        self.assertEqual(path, "/usr/local/bin:/usr/bin:/bin")
        self.assertEqual(depot, "<unset>")


if __name__ == "__main__":
    unittest.main()
