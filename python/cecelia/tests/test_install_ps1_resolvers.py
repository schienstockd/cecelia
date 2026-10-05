"""``install.ps1``'s Julia and Pixi resolution, run under PowerShell against stub binaries.

Only the ``# ── Julia`` / ``# ── Pixi`` sections are executed (sliced out of the script), so nothing
is installed. A juliaup with no ``julia`` launcher must be asked for a channel — winget no-ops on
it — and a Julia that still can't be found must fail before the multi-GB ``pixi install``, not at
its first use; likewise a Pixi installer that leaves no ``pixi``. Runs where ``pwsh`` is on PATH,
on Linux/macOS only: the stubs are shell scripts, which Windows' PATHEXT lookup wouldn't resolve.
"""
import os
import pathlib
import shutil
import subprocess
import tempfile
import unittest

INSTALL_PS1 = pathlib.Path(__file__).resolve().parents[3] / "install.ps1"
PWSH = shutil.which("pwsh")

# `winget` is shadowed by a function: it logs, and (when asked) drops a `julia` alias on PATH the way
# the Store package does.
PRELUDE = r"""
$ErrorActionPreference = 'Stop'
function Say($m) { Write-Host "[cecelia] $m" }
function winget {
  Add-Content -Path $env:STUB_LOG -Value "winget $args"
  if ($env:STUB_WINGET_ALIAS) { Set-Content -Path $env:STUB_WINGET_ALIAS -Value "#!/bin/sh"; chmod +x $env:STUB_WINGET_ALIAS }
}
"""


def _section(header):
    text = INSTALL_PS1.read_text(encoding="utf-8")
    start = text.index("# ── " + header)
    end = text.index("\n# ──", start + 1)
    return text[start:end]


def _stub(path, body="#!/bin/sh\n"):
    pathlib.Path(path).write_text(body, encoding="utf-8")
    os.chmod(path, 0o755)


@unittest.skipUnless(PWSH and os.name != "nt", "needs pwsh and POSIX shell stubs")
class InstallPs1JuliaTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.bin = os.path.join(self.root, "bin")
        self.home = os.path.join(self.root, "home")
        os.makedirs(self.bin)
        os.makedirs(self.home)
        self.log = os.path.join(self.root, "log")
        pathlib.Path(self.log).touch()
        self.script = os.path.join(self.root, "julia_section.ps1")
        pathlib.Path(self.script).write_text(
            PRELUDE + _section("Julia") + '\nWrite-Output "JULIA=$Julia"\n', encoding="utf-8")

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def _run(self, **extra_env):
        env = {"PATH": self.bin + os.pathsep + "/usr/bin:/bin", "USERPROFILE": self.home,
               "HOME": self.home, "STUB_LOG": self.log, **extra_env}
        out = subprocess.run([PWSH, "-NoProfile", "-NonInteractive", "-File", self.script],
                             env=env, capture_output=True, text=True, encoding="utf-8", timeout=120)
        calls = pathlib.Path(self.log).read_text(encoding="utf-8").splitlines()
        return out, calls

    def test_juliaup_without_launcher_adds_a_channel(self):
        # `juliaup add` puts the launcher beside juliaup, as a real juliaup does.
        _stub(os.path.join(self.bin, "juliaup"),
              '#!/bin/sh\necho "juliaup $*" >> "$STUB_LOG"\n'
              '[ "$1" = add ] && printf "#!/bin/sh\\n" > "$(dirname "$0")/julia.exe" '
              '&& chmod +x "$(dirname "$0")/julia.exe"\nexit 0\n')
        out, calls = self._run()
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertEqual(calls, ["juliaup add release", "juliaup default release"])
        self.assertIn("JULIA=" + os.path.join(self.bin, "julia.exe"), out.stdout)

    def test_juliaup_whose_launcher_never_appears_fails_early(self):
        _stub(os.path.join(self.bin, "juliaup"), '#!/bin/sh\necho "juliaup $*" >> "$STUB_LOG"\n')
        out, calls = self._run()
        self.assertNotEqual(out.returncode, 0)
        self.assertIn("Julia not found", out.stderr + out.stdout)
        self.assertNotIn("JULIA=", out.stdout)
        self.assertFalse(any(c.startswith("winget") for c in calls))

    def test_fresh_winget_install_resolves_the_alias(self):
        alias = os.path.join(self.bin, "julia")
        out, calls = self._run(STUB_WINGET_ALIAS=alias)
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertEqual(len(calls), 1)
        self.assertTrue(calls[0].startswith("winget install"))
        self.assertIn("JULIA=" + alias, out.stdout)

    def test_julia_on_path_is_used_untouched(self):
        _stub(os.path.join(self.bin, "julia"))
        _stub(os.path.join(self.bin, "juliaup"), '#!/bin/sh\necho "juliaup $*" >> "$STUB_LOG"\n')
        out, calls = self._run()
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertEqual(calls, [])
        self.assertIn("JULIA=" + os.path.join(self.bin, "julia"), out.stdout)


# The Pixi installer is a child `powershell`; shadowed by a function that installs nothing.
PIXI_PRELUDE = r"""
$ErrorActionPreference = 'Stop'
function Say($m) { Write-Host "[cecelia] $m" }
function powershell { Add-Content -Path $env:STUB_LOG -Value "pixi-installer" }
$Scope = 'user'
$PixiHome = $env:STUB_PIXI_HOME
"""


@unittest.skipUnless(PWSH and os.name != "nt", "needs pwsh and POSIX shell stubs")
class InstallPs1PixiTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.bin = os.path.join(self.root, "bin")
        os.makedirs(self.bin)
        self.log = os.path.join(self.root, "log")
        pathlib.Path(self.log).touch()
        self.script = os.path.join(self.root, "pixi_section.ps1")
        pathlib.Path(self.script).write_text(
            PIXI_PRELUDE + _section("Pixi") + '\nWrite-Output "PIXI=$Pixi"\n', encoding="utf-8")

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def _run(self):
        env = {"PATH": self.bin + os.pathsep + "/usr/bin:/bin", "HOME": self.root,
               "STUB_LOG": self.log, "STUB_PIXI_HOME": os.path.join(self.root, "pixi")}
        return subprocess.run([PWSH, "-NoProfile", "-NonInteractive", "-File", self.script],
                              env=env, capture_output=True, text=True, encoding="utf-8", timeout=120)

    def test_installer_that_installs_nothing_fails_early(self):
        out = self._run()
        self.assertNotEqual(out.returncode, 0)
        self.assertIn("Pixi not found", out.stderr + out.stdout)
        self.assertEqual(pathlib.Path(self.log).read_text(encoding="utf-8").split(), ["pixi-installer"])

    def test_pixi_on_path_is_used(self):
        _stub(os.path.join(self.bin, "pixi"))
        out = self._run()
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertIn("PIXI=" + os.path.join(self.bin, "pixi"), out.stdout)


if __name__ == "__main__":
    unittest.main()
