"""``install.ps1``'s Julia and Pixi resolution, run under PowerShell against stub binaries.

Only the ``# ── Julia`` / ``# ── Pixi`` sections are executed (sliced out of the script), so nothing
is installed. A juliaup with no ``julia`` launcher must be asked for a channel — winget no-ops on
it — and a Julia that still can't be found must fail before the multi-GB ``pixi install``, not at
its first use; likewise a Pixi installer that leaves no ``pixi``. System scope must ignore the admin's
own Julia and put a Cecelia-owned juliaup in the shared depot, which the all-users launcher puts on PATH. Runs where ``pwsh`` is on PATH,
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


def _isolated_path(root, bin_dir):
    """PATH of the stub dir + only the tools the stubs use. No system bin dirs: a `julia` already on
    the machine (CI's ubuntu runner has one) would otherwise shadow the stubs."""
    tools = os.path.join(root, "tools")
    os.makedirs(tools, exist_ok=True)
    for name in ("dirname", "chmod", "printf"):
        if not os.path.lexists(os.path.join(tools, name)):
            os.symlink(shutil.which(name), os.path.join(tools, name))
    return bin_dir + os.pathsep + tools


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
        env = {"PATH": _isolated_path(self.root, self.bin), "USERPROFILE": self.home,
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


# System scope: `Invoke-WebRequest` is shadowed to hand over a portable juliaup archive built from
# stubs (juliaup.exe + julia.exe, flat, like the real release asset). Its juliaup.exe logs its calls.
SYSTEM_PRELUDE = PRELUDE + r"""
$Scope = 'system'
$JuliaupDepot = $env:STUB_DEPOT
$env:JULIAUP_DEPOT_PATH = $JuliaupDepot
function Invoke-WebRequest($Uri, $OutFile) {
  Add-Content -Path $env:STUB_LOG -Value "download $Uri"
  Copy-Item $env:STUB_PORTABLE $OutFile
}
"""


@unittest.skipUnless(PWSH and os.name != "nt", "needs pwsh and POSIX shell stubs")
class InstallPs1SystemJuliaTest(unittest.TestCase):
    """System scope must provision a Cecelia-owned juliaup in the shared depot. The admin's own Julia
    (or a Store juliaup) is per-user, so other accounts' launcher couldn't run it."""

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.bin = os.path.join(self.root, "bin")
        self.home = os.path.join(self.root, "home")
        self.depot = os.path.join(self.root, "install", "juliaup")
        os.makedirs(self.bin)
        os.makedirs(self.home)
        self.log = os.path.join(self.root, "log")
        pathlib.Path(self.log).touch()
        pkg = os.path.join(self.root, "pkg")
        os.makedirs(pkg)
        _stub(os.path.join(pkg, "juliaup.exe"), '#!/bin/sh\necho "shared-juliaup $*" >> "$STUB_LOG"\n')
        _stub(os.path.join(pkg, "julia.exe"))
        self.portable = os.path.join(self.root, "portable.tar.gz")
        subprocess.run(["tar", "-czf", self.portable, "-C", pkg, "."], check=True)
        self.script = os.path.join(self.root, "julia_section.ps1")
        pathlib.Path(self.script).write_text(
            SYSTEM_PRELUDE + _section("Julia") + '\nWrite-Output "JULIA=$Julia"\n', encoding="utf-8")

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def _run(self):
        env = {"PATH": self.bin + os.pathsep + "/usr/bin:/bin", "USERPROFILE": self.home,
               "HOME": self.home, "STUB_LOG": self.log, "STUB_DEPOT": self.depot,
               "STUB_PORTABLE": self.portable}
        out = subprocess.run([PWSH, "-NoProfile", "-NonInteractive", "-File", self.script],
                             env=env, capture_output=True, text=True, encoding="utf-8", timeout=120)
        return out, pathlib.Path(self.log).read_text(encoding="utf-8").splitlines()

    def test_admins_own_julia_is_not_used(self):
        # The elevated admin has a Julia and a juliaup of their own on PATH.
        _stub(os.path.join(self.bin, "julia"))
        _stub(os.path.join(self.bin, "juliaup"), '#!/bin/sh\necho "own-juliaup $*" >> "$STUB_LOG"\n')
        out, calls = self._run()
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertIn("JULIA=" + os.path.join(self.depot, "bin", "julia.exe"), out.stdout)
        self.assertEqual(len(calls), 3, calls)
        self.assertRegex(calls[0], r"^download https://github\.com/JuliaLang/juliaup/releases/download/"
                                   r"v[0-9.]+/juliaup-[0-9.]+-x86_64-pc-windows-gnu-portable\.tar\.gz$")
        self.assertEqual(calls[1:], ["shared-juliaup add release", "shared-juliaup default release"])

    def test_existing_shared_juliaup_is_reused(self):
        os.makedirs(os.path.join(self.depot, "bin"))
        _stub(os.path.join(self.depot, "bin", "julia.exe"))
        out, calls = self._run()
        self.assertEqual(out.returncode, 0, out.stderr)
        self.assertEqual(calls, [])
        self.assertIn("JULIA=" + os.path.join(self.depot, "bin", "julia.exe"), out.stdout)

    def test_all_users_launcher_puts_the_shared_julia_on_path(self):
        text = INSTALL_PS1.read_text(encoding="utf-8")
        start = text.index("@\"\n@echo off")
        launcher = text[start:text.index('\n"@', start) + 3]
        script = os.path.join(self.root, "launcher.ps1")
        pathlib.Path(script).write_text(
            "$PixiHome='P'; $JuliaupDepot='J'; $InstallDir='I'; $Pixi='X'\n"
            "Write-Output " + launcher + "\n", encoding="utf-8")
        out = subprocess.run([PWSH, "-NoProfile", "-NonInteractive", "-File", script],
                             capture_output=True, text=True, encoding="utf-8", timeout=120)
        self.assertEqual(out.returncode, 0, out.stderr)
        path_lines = [ln for ln in out.stdout.splitlines() if ln.startswith('set "PATH=')]
        self.assertEqual(path_lines, ['set "PATH=P\\bin;J\\bin;%PATH%"'])


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
        env = {"PATH": _isolated_path(self.root, self.bin), "HOME": self.root,
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
