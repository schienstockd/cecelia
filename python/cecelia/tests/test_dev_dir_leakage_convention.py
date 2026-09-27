"""Convention test: Python tests don't tap the user's real dev projects dir.

The rule is in the root `CLAUDE.md` → *Testing* — "Tests must not depend on the dev projects dir;
use `fixture_path(...)` + `have_fixture(...)`". A test that reads `CECELIA_DEV_DIR` or reaches
`cecelia_conf()["dirs"]["projects"]` outside a scoped setup/teardown risks (a) passing locally
on a machine where the dev dir happens to contain the right project, then failing on CI where it
doesn't; (b) corrupting a real user's projects during a test run.

Python-side today: **zero hits.** This ratchet is pure floor — locks the current-zero state so a
future test that reaches for the dev dir has to justify it explicitly with the `# DEV-DIR-OK:`
marker, or the discipline gets a review.

Julia-side is out of scope here: `app/test/**` uses `withenv(:CECELIA_DEV_DIR => …) do …; end` as
a legitimate scoped pattern (per feedback memory `feedback_never_call_set_projects_dir`), which a
Python-side grep ratchet has no reason to police.

Run with `pixi run test-py`.
"""
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_TESTS_ROOT = os.path.join(_REPO, 'python', 'cecelia', 'tests')

# Grep patterns for the failure modes. Each is a substring on the raw source line.
_BANNED_PATTERNS = (
    ('CECELIA_DEV_DIR',
     'reads the user\'s dev-projects env var — use fixture_path()/have_fixture() instead'),
    ('cecelia_conf()["dirs"]["projects"]',
     'reads the dev projects dir at runtime — use test-data/ fixtures'),
    ("cecelia_conf()['dirs']['projects']",
     'reads the dev projects dir at runtime — use test-data/ fixtures'),
)
# Rough shape of a hardcoded dev-projects path — `cecelia*/dev/projects/*`. Kept narrow to avoid
# flagging comments that just discuss the convention.
_HARDCODED_PATH_RE = re.compile(r'~/cecelia[a-z_-]*/dev/projects/')

_DEV_DIR_OK_RE = re.compile(r'#\s*DEV-DIR-OK:\s*.+')

_BASELINE_MAX = 0
_BASELINE = frozenset()


def _test_files():
    for dirpath, dirnames, filenames in os.walk(_TESTS_ROOT):
        dirnames[:] = [d for d in dirnames if not d.startswith('.') and d != '__pycache__']
        for fn in filenames:
            if not fn.endswith('.py'):
                continue
            full = os.path.join(dirpath, fn)
            yield os.path.relpath(full, _REPO), full


class DevDirLeakageConventionTest(unittest.TestCase):
    def test_no_dev_dir_reads_in_python_tests(self):
        offenders = []
        for rel, full in _test_files():
            if rel in _BASELINE:
                continue
            with open(full, encoding='utf-8') as fh:
                lines = fh.readlines()
            for i, line in enumerate(lines, start=1):
                if _DEV_DIR_OK_RE.search(line):
                    continue
                # Skip this file itself (the patterns literally appear in the source).
                if rel == os.path.join('python', 'cecelia', 'tests',
                                       'test_dev_dir_leakage_convention.py'):
                    continue
                for needle, why in _BANNED_PATTERNS:
                    if needle in line:
                        offenders.append(f'{rel}:{i}: `{needle}` — {why}')
                if _HARDCODED_PATH_RE.search(line):
                    offenders.append(f'{rel}:{i}: hardcoded `~/cecelia*/dev/projects/` path — use test-data/')
        self.assertEqual(
            offenders, [],
            "Python tests must not tap the user's dev projects dir. Use `fixture_path(...)` + "
            "`have_fixture(...)` for on-disk fixtures under `test-data/`. For a rare intentional "
            "reference (e.g. a doc-string example that mentions the env var), add "
            "`# DEV-DIR-OK: <reason>` on the same line:\n  "
            + '\n  '.join(offenders))

    def test_baseline_has_not_grown(self):
        self.assertLessEqual(
            len(_BASELINE), _BASELINE_MAX,
            f'_BASELINE grew to {len(_BASELINE)} (cap {_BASELINE_MAX}). Either fix the new '
            f'violation, or bump _BASELINE_MAX and justify in the PR body.')


if __name__ == '__main__':
    unittest.main()
