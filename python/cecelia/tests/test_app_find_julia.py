"""The launcher's Julia resolution (``app.py`` → ``_find_julia``).

``install.sh`` puts a Cecelia-owned juliaup in ``<install>/juliaup`` for system scope, and on Apple
Silicon when the Julia on PATH is an Intel build. The launcher must run THAT Julia, with juliaup
pointed at its own state, or the server comes up under Rosetta again.
"""
import importlib.util
import os
import pathlib
import shutil
import subprocess
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

    def test_installer_recorded_julia_is_found_off_path(self):
        # A Homebrew juliaup's `julia` sits in /opt/homebrew/bin, which launchd's PATH leaves out.
        julia = os.path.join(tempfile.mkdtemp(), "bin", self.app._exe("julia"))
        os.makedirs(os.path.dirname(julia))
        open(julia, "w", encoding="utf-8").close()
        with open(os.path.join(self.root, ".cecelia-julia"), "w", encoding="utf-8") as f:
            f.write(julia + "\n")
        home = tempfile.mkdtemp()
        with mock.patch.dict(os.environ, {"PATH": "/usr/bin:/bin", "HOME": home, "USERPROFILE": home}, clear=True), \
             mock.patch("shutil.which", return_value=None):
            self.assertEqual(self.app._find_julia(), julia)

    def test_recorded_julia_that_is_gone_falls_through_to_path(self):
        with open(os.path.join(self.root, ".cecelia-julia"), "w", encoding="utf-8") as f:
            f.write(os.path.join(self.root, "removed", "julia") + "\n")
        with mock.patch.dict(os.environ, {"PATH": "/usr/local/bin:/usr/bin"}, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), "/usr/local/bin/julia")

    def test_both_installers_record_the_julia(self):
        # The file `_recorded_julia` reads only exists if the installers write it.
        repo = APP_PY.parent
        self.assertIn('"$INSTALL_DIR/.cecelia-julia"', (repo / "install.sh").read_text(encoding="utf-8"))
        self.assertIn("'.cecelia-julia')   -Value $Julia", (repo / "install.ps1").read_text(encoding="utf-8"))

    def test_without_it_path_julia_is_used_and_env_untouched(self):
        env = {"PATH": "/usr/local/bin:/usr/bin"}
        with mock.patch.dict(os.environ, env, clear=True), \
             mock.patch("shutil.which", return_value="/usr/local/bin/julia"):
            self.assertEqual(self.app._find_julia(), "/usr/local/bin/julia")
            self.assertNotIn("JULIAUP_DEPOT_PATH", os.environ)
            self.assertEqual(os.environ["PATH"], "/usr/local/bin:/usr/bin")


@unittest.skipIf(os.name == "nt", "pixi sources activate_juliaup.sh on unix only")
class ActivateRecordedJuliaTest(unittest.TestCase):
    """``scripts/activate_juliaup.sh`` falls back to the recorded Julia like ``_recorded_julia``,
    appending its dir so it never shadows the pixi env's own binaries."""

    def _activate(self, root):
        script = APP_PY.parent / "scripts" / "activate_juliaup.sh"
        out = subprocess.run(["/bin/sh", "-c", '. "$1"; echo "$PATH"', "sh", str(script)],
                             env={"PATH": "/usr/bin:/bin", "PIXI_PROJECT_ROOT": root},
                             capture_output=True, text=True, encoding="utf-8", check=True)
        return out.stdout.strip()

    def test_recorded_julia_dir_is_appended_when_no_julia_on_path(self):
        root = tempfile.mkdtemp()
        bin_dir = os.path.join(root, "hb")
        os.makedirs(bin_dir)
        julia = os.path.join(bin_dir, "julia")
        with open(julia, "w", encoding="utf-8") as f:
            f.write("#!/bin/sh\n")
        os.chmod(julia, 0o755)
        with open(os.path.join(root, ".cecelia-julia"), "w", encoding="utf-8") as f:
            f.write(julia + "\n")
        if shutil.which("julia", path="/usr/bin:/bin"):
            self.skipTest("a julia in /usr/bin or /bin already satisfies PATH")
        self.assertEqual(self._activate(root), "/usr/bin:/bin:" + bin_dir)

    def test_no_record_leaves_path_alone(self):
        self.assertEqual(self._activate(tempfile.mkdtemp()), "/usr/bin:/bin")


class FindPixiTest(unittest.TestCase):
    """Same order as `_find_pixi` in api/src/pixi_bin.jl. The macOS `.app` starts the launcher with
    launchd's minimal PATH; `pixi run app` exports PIXI_EXE, which must win."""

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

    def _env(self, **extra):
        env = {"PATH": "", "HOME": self.home, "USERPROFILE": self.home, **extra}
        return (mock.patch.dict(os.environ, env, clear=True),
                mock.patch("shutil.which", return_value=None))

    def test_pixi_exe_wins(self):
        launcher = self._touch(self.home, "launcher", "pixi")
        self._touch(self.root, "pixi", "bin", self.app._exe("pixi"))
        a, b = self._env(PIXI_EXE=launcher)
        with a, b:
            self.assertEqual(self.app._find_pixi(), launcher)

    def test_system_scope_then_user_scope(self):
        a, b = self._env(PIXI_EXE=os.path.join(self.home, "gone"))
        with a, b:
            user = self._touch(self.home, ".pixi", "bin", self.app._exe("pixi"))
            self.assertEqual(self.app._find_pixi(), user)
            system = self._touch(self.root, "pixi", "bin", self.app._exe("pixi"))
            self.assertEqual(self.app._find_pixi(), system)


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
