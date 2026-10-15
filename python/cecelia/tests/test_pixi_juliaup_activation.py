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
import sys
import tempfile
import tomllib
import unittest

REPO = pathlib.Path(__file__).resolve().parents[3]
SCRIPT = REPO / "scripts" / "activate_juliaup.sh"
BAT = REPO / "scripts" / "activate_juliaup.bat"


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

    def test_pixi_runs_the_bat_on_windows(self):
        manifest = tomllib.loads((REPO / "pixi.toml").read_text(encoding="utf-8"))
        self.assertIn("scripts/activate_juliaup.bat", manifest["target"]["win"]["activation"]["scripts"])
        self.assertIn(b"\r\n", BAT.read_bytes())  # cmd misreads goto/labels in an LF batch file
        bundled = (REPO / "scripts" / "bundle_required_paths.txt").read_text(encoding="utf-8").split()
        self.assertIn("scripts/activate_juliaup.bat", bundled)

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
    def test_system_scope_stacks_a_writable_depot_over_the_shared_one(self):
        # Shared depot is read-only to every account but the owner: a per-user depot must come first,
        # and the trailing empty entry keeps Julia's bundled stdlib depot (else EROFS precompiling).
        bin_dir = os.path.join(self.root, "juliaup", "bin")
        os.makedirs(bin_dir)
        os.makedirs(os.path.join(self.root, "juliaup", "depot"))
        julia = os.path.join(bin_dir, "julia")
        open(julia, "w", encoding="utf-8").close()
        os.chmod(julia, 0o755)
        out = subprocess.run(
            ["sh", "-c", '. "$1"; printf "%s" "${JULIA_DEPOT_PATH-<unset>}"', "sh", str(SCRIPT)],
            env={"PIXI_PROJECT_ROOT": self.root, "PATH": "/usr/bin:/bin", "HOME": "/home/u"},
            capture_output=True, text=True, check=True)
        shared = os.path.join(self.root, "juliaup", "depot")
        self.assertEqual(out.stdout, f"/home/u/.cecelia/julia-depot:{shared}:")

    @unittest.skipIf(os.name == "nt", "unix-only script: pixi.toml wires it under [target.unix.activation]")
    def test_dev_checkout_is_untouched(self):
        path, depot = _activated(self.root, "/usr/local/bin:/usr/bin:/bin")
        self.assertEqual(path, "/usr/local/bin:/usr/bin:/bin")
        self.assertEqual(depot, "<unset>")


@unittest.skipUnless(os.name == "nt", "cmd.exe only: pixi.toml wires the .bat under [target.win.activation]")
class WindowsActivationTest(unittest.TestCase):
    """``scripts/activate_juliaup.bat`` applies ``_find_julia``'s order on Windows: the install's juliaup,
    then the recorded Julia ahead of PATH (through a ``julia.cmd`` shim), then PATH."""

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.profile = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root, True)
        self.addCleanup(shutil.rmtree, self.profile, True)
        self.system32 = os.path.join(os.environ["SystemRoot"], "System32")
        self.other = os.path.join(self.root, "other")
        os.makedirs(self.other)
        with open(os.path.join(self.other, "julia.cmd"), "w", encoding="utf-8", newline="") as f:
            f.write("@echo other\r\n")

    def _record(self, julia):
        with open(os.path.join(self.root, ".cecelia-julia"), "w", encoding="utf-8") as f:
            f.write(julia + "\n")

    def _run(self, path, twice=False, julia_args=None):
        """Activate the way pixi does, then report PATH, the depot vars and, optionally, run `julia`."""
        driver = os.path.join(self.root, "driver.cmd")
        lines = ["@echo off", f'call "{BAT}"'] + ([f'call "{BAT}"'] if twice else []) + [
            "echo PATH=%PATH%", "echo JULIAUP=%JULIAUP_DEPOT_PATH%", "echo DEPOT=%JULIA_DEPOT_PATH%"]
        if julia_args is not None:
            lines.append("julia " + julia_args)
        with open(driver, "w", encoding="utf-8", newline="\r\n") as f:
            f.write("\n".join(lines) + "\n")
        env = {"PATH": path, "PIXI_PROJECT_ROOT": self.root, "USERPROFILE": self.profile,
               "SystemRoot": os.environ["SystemRoot"], "PATHEXT": ".COM;.EXE;.BAT;.CMD"}
        out = subprocess.run(["cmd", "/d", "/c", driver], env=env, capture_output=True, text=True,
                             encoding="utf-8")
        got = dict(line.split("=", 1) for line in out.stdout.splitlines()[:3])
        return got, out.stdout.splitlines()[3:], out.returncode

    def test_recorded_julia_beats_the_one_on_path(self):
        # Any real exe stands in for julia.exe: it proves the shim runs the recorded binary with the
        # task's args, and hands back its exit code.
        self._record(sys.executable)
        got, rest, code = self._run(f"{self.other};{self.system32}",
                                    julia_args='-c "import sys; print(\'recorded\'); sys.exit(3)"')
        shim = os.path.join(self.profile, ".cecelia", "julia-shim")
        self.assertEqual(got["PATH"], f"{shim};{self.other};{self.system32}")
        self.assertEqual(os.listdir(shim), ["julia.cmd"])  # nothing else from the recorded dir comes first
        self.assertEqual(rest, ["recorded"])
        self.assertEqual(code, 3)

    def test_sourced_twice_does_not_grow_path(self):
        self._record(sys.executable)
        once, _, _ = self._run(f"{self.other};{self.system32}")
        twice, _, _ = self._run(f"{self.other};{self.system32}", twice=True)
        self.assertEqual(once["PATH"], twice["PATH"])

    def test_recorded_julia_already_first_leaves_path_alone(self):
        # install.ps1 records the `julia` PATH offered, so this is the usual install: no shim, no cmd prompt.
        self._record(os.path.join(self.other, "julia.cmd"))
        got, _, _ = self._run(f"{self.other};{self.system32}")
        self.assertEqual(got["PATH"], f"{self.other};{self.system32}")
        self.assertFalse(os.path.exists(os.path.join(self.profile, ".cecelia")))

    def test_unwritable_shim_dir_falls_back_to_appending(self):
        self._record(sys.executable)
        open(os.path.join(self.profile, ".cecelia"), "w", encoding="utf-8").close()  # a file: mkdir fails
        got, _, _ = self._run(self.system32)
        self.assertEqual(got["PATH"], f"{self.system32};{os.path.dirname(sys.executable)}\\")

    def test_install_owned_juliaup_wins_with_its_depot(self):
        self._record(sys.executable)
        juliaup = os.path.join(self.root, "juliaup")
        os.makedirs(os.path.join(juliaup, "bin"))
        os.makedirs(os.path.join(juliaup, "depot"))
        open(os.path.join(juliaup, "bin", "julia.exe"), "w", encoding="utf-8").close()
        got, _, _ = self._run(f"{self.other};{self.system32}", twice=True)
        self.assertEqual(got["PATH"], f"{juliaup}\\bin;{self.other};{self.system32}")
        self.assertEqual(got["JULIAUP"], juliaup)
        self.assertEqual(got["DEPOT"], f"{self.profile}\\.cecelia\\julia-depot;{juliaup}\\depot;")

    def test_no_record_leaves_path_alone(self):
        got, _, _ = self._run(f"{self.other};{self.system32}")
        self.assertEqual(got["PATH"], f"{self.other};{self.system32}")


if __name__ == "__main__":
    unittest.main()
