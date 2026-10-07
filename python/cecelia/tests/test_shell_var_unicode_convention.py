"""Non-ASCII characters break the shipped install/uninstall scripts in two platform-specific ways.

1. A `$VAR` written directly before a non-ASCII character breaks the shell scripts on macOS.

macOS's `/bin/sh` is bash 3.2. In a non-UTF-8 locale it reads the first byte of `…`/`—` as part of
the variable name, so `"$INSTALL_DIR…"` aborts under `set -u` with `INSTALL_DIR�: unbound
variable`. Write `${VAR}…` instead.

2. Any non-ASCII character in PowerShell CODE breaks the .ps1 scripts on Windows PowerShell 5.1,
which reads a BOM-less file as Windows-1252: `—` becomes `â€”`, whose last byte is a curly quote
PowerShell treats as a string delimiter, so the script fails to parse. Comments are harmless.
"""
import pathlib
import re
import unittest

REPO = pathlib.Path(__file__).resolve().parents[3]
SCRIPTS = ["install.sh", "uninstall.sh", *(str(p.relative_to(REPO)) for p in (REPO / "scripts").rglob("*.sh"))]
BARE_VAR_THEN_NON_ASCII = re.compile(rb"\$[A-Za-z_][A-Za-z0-9_]*[\x80-\xff]")


class ShellVarBeforeUnicodeTest(unittest.TestCase):
    def test_no_bare_var_before_non_ascii(self):
        hits = []
        for rel in SCRIPTS:
            for n, line in enumerate((REPO / rel).read_bytes().split(b"\n"), 1):
                if BARE_VAR_THEN_NON_ASCII.search(line):
                    hits.append(f"{rel}:{n}: {line.decode('utf-8', 'replace').strip()}")
        self.assertEqual(hits, [], "use ${VAR} before a non-ASCII character:\n" + "\n".join(hits))


    def test_powershell_code_is_ascii(self):
        hits = []
        for rel in ("install.ps1", "uninstall.ps1"):
            for n, line in enumerate((REPO / rel).read_text(encoding="utf-8").split("\n"), 1):
                if line.lstrip().startswith("#"):
                    continue
                if re.search(r"[^\x00-\x7f]", line):
                    hits.append(f"{rel}:{n}: {line.strip()}")
        self.assertEqual(hits, [], "ASCII only in PowerShell code (comments may use any):\n" + "\n".join(hits))


if __name__ == "__main__":
    unittest.main()
