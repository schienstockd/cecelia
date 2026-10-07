"""A `$VAR` written directly before a non-ASCII character breaks the shipped shell scripts on macOS.

macOS's `/bin/sh` is bash 3.2. In a non-UTF-8 locale it reads the first byte of `…`/`—` as part of
the variable name, so `"$INSTALL_DIR…"` aborts under `set -u` with `INSTALL_DIR�: unbound
variable`. Write `${VAR}…` instead.
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


if __name__ == "__main__":
    unittest.main()
