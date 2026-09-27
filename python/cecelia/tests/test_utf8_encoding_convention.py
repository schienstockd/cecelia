"""Convention test: every text-mode `open()` on a real file passes `encoding="utf-8"`.

Python's `open()` picks a *system-locale* encoding when `encoding=` is omitted. On Linux and macOS
that is almost always UTF-8; on Windows it is `cp1252`. A `.json`, `.jsonl` or `.csv` write that
omits `encoding=` silently produces cp1252-encoded bytes on a Windows install, which then round-trip
badly through any UTF-8 reader — no error, no test failure, wrong bytes on disk.

The rule is in the root `CLAUDE.md` → *Windows compatibility* (`encoding="utf-8"` on Python text
I/O). This is the ratchet the prose has always needed: an AST floor that catches every text-mode
`open(...)` without an `encoding=` kwarg. Baseline is empty; the ratchet is pure floor.

Only the bare builtin `open(...)` is scanned — matching every `.open()` call would flag
`zarr.open(...)`, `sqlite3.open(...)` and friends that have nothing to do with text I/O. The
codebase's pathlib usage is thin enough that a floor on bare `open()` catches the risk without
that noise; if pathlib usage grows, extend the check with a targeted `Path.open` detector rather
than a broad `.open` match.

Binary modes (`'rb'`, `'wb'`, `'ab'`, `'r+b'`, …) don't take an `encoding` argument and are ignored.
If the mode is not a compile-time literal, the caller must add an explicit `# UTF8-OK: <reason>`
marker on the call line — an unmatched dynamic mode is not a free pass.

Run with `pixi run test-py`.
"""
import ast
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

_UTF8_OK_RE = re.compile(r'#\s*UTF8-OK:\s*.+')

_SEARCH_DIRS = (
    os.path.join('python', 'cecelia'),
    os.path.join('app', 'src'),
    'scripts',
)

# Sanctioned bypasses. Empty today — kept as a shape for future exceptions (e.g. reading a file
# whose encoding is genuinely unknown and must be probed). Every entry needs a reason. Meta-
# ratchet lives in `test_baseline_has_not_grown` (same shape as h5ad + zarr).
_BASELINE_MAX = 0
_BASELINE = frozenset()


def _is_text_mode(mode_value: str) -> bool:
    """True if `mode_value` opens the file in TEXT mode (i.e. would use `encoding=`)."""
    if not isinstance(mode_value, str):
        return False
    return 'b' not in mode_value


def _mode_from_call(node: ast.Call) -> tuple[bool, str | None]:
    """Return (mode_determined, mode_str_or_None).

    Determined = True + a mode string when mode is a compile-time literal (kwarg or positional).
    Determined = True + `'r'` when the call has NO explicit mode at all (the language default).
    Determined = False when mode is present but not a literal — skip: the caller may manage
    encoding dynamically (as `atomic_io.write_atomic` does), and false-positive-ing on that
    forces reviewers to look at every dynamic-mode wrapper for no gain.
    """
    for kw in node.keywords:
        if kw.arg == 'mode':
            if isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, str):
                return True, kw.value.value
            return False, None
    # arg 1 is the positional mode for builtin `open(file, mode, ...)`. Non-literal at that slot
    # is the dynamic-mode case — skip.
    if len(node.args) >= 2:
        arg = node.args[1]
        if isinstance(arg, ast.Constant) and isinstance(arg.value, str):
            return True, arg.value
        return False, None
    # No mode arg at all → language default 'r' (text).
    return True, 'r'


def _has_encoding(node: ast.Call) -> bool:
    for kw in node.keywords:
        if kw.arg == 'encoding':
            return True
        # `**kwargs` unpacking — encoding might be in the unpacked dict; can't rule out.
        if kw.arg is None:
            return True
    return False


def _is_open_call(node: ast.Call) -> bool:
    # Bare `open(...)` — module-level builtin. Watches for the identifier only; a reassignment
    # like `open = something_else` would fool it, but grep would too, and that pattern isn't
    # used in the codebase. `.open()` on some other object (zarr.open, sqlite3.open, …) is NOT
    # the same function — deliberately excluded to keep noise out.
    f = node.func
    return isinstance(f, ast.Name) and f.id == 'open'


def _py_files():
    for root in _SEARCH_DIRS:
        full_root = os.path.join(_REPO, root)
        if not os.path.isdir(full_root):
            continue
        for dirpath, dirnames, filenames in os.walk(full_root):
            dirnames[:] = [d for d in dirnames if d not in ('tests', '__pycache__') and not d.startswith('.')]
            for fn in filenames:
                if not fn.endswith('.py'):
                    continue
                full = os.path.join(dirpath, fn)
                yield os.path.relpath(full, _REPO), full


def _offenders(tree: ast.AST, src_lines: list[str]) -> list[tuple[int, str]]:
    hits: list[tuple[int, str]] = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call) or not _is_open_call(node):
            continue
        line = src_lines[node.lineno - 1] if 1 <= node.lineno <= len(src_lines) else ''
        if _UTF8_OK_RE.search(line):
            continue
        determined, mode = _mode_from_call(node)
        if not determined:
            continue  # dynamic mode — caller may manage encoding themselves (e.g. atomic_io)
        if not _is_text_mode(mode):
            continue  # binary mode — encoding= is not applicable
        if not _has_encoding(node):
            hits.append((node.lineno, f"open(..., mode='{mode}') without encoding='utf-8'"))
    return hits


class Utf8EncodingConventionTest(unittest.TestCase):
    def test_no_text_mode_open_without_encoding(self):
        offenders = []
        for rel, full in _py_files():
            if rel in _BASELINE:
                continue
            with open(full, encoding='utf-8') as f:
                src = f.read()
            try:
                tree = ast.parse(src, filename=rel)
            except SyntaxError:
                continue
            src_lines = src.split('\n')
            for lineno, reason in _offenders(tree, src_lines):
                offenders.append(f'{rel}:{lineno}: {reason}')
        self.assertEqual(
            offenders, [],
            "pass `encoding='utf-8'` to every text-mode open (the default is cp1252 on Windows "
            "and silently corrupts JSON/CSV writes). See CLAUDE.md → *Windows compatibility*. "
            "For a genuinely dynamic mode where UTF-8 isn't right, add `# UTF8-OK: <reason>` on "
            "the call line:\n  "
            + '\n  '.join(offenders))

    def test_baseline_has_not_grown(self):
        self.assertLessEqual(
            len(_BASELINE), _BASELINE_MAX,
            f'_BASELINE grew to {len(_BASELINE)} (cap {_BASELINE_MAX}). Either fix the new '
            f'violation, or bump _BASELINE_MAX and justify in the PR body.')


class Utf8ScanCoverageTest(unittest.TestCase):
    """Guard against the scan silently covering nothing (a wrong root, no matches)."""
    def test_scan_sees_real_open_calls(self):
        # The scan must at least see a nontrivial number of `open(...)` sites in the tree — if
        # zero, the scan root is wrong and the pass is meaningless.
        seen = 0
        for _, full in _py_files():
            with open(full, encoding='utf-8') as f:
                src = f.read()
            try:
                tree = ast.parse(src)
            except SyntaxError:
                continue
            for node in ast.walk(tree):
                if isinstance(node, ast.Call) and _is_open_call(node):
                    seen += 1
        # 5 is a floor well below the actual count; the point is "not zero," not a tight number.
        # Cecelia's Python is mostly numpy/zarr, so bare-`open()` sites are few by design.
        self.assertGreater(seen, 5, f'scan saw only {seen} open() calls — scan root is likely wrong')


if __name__ == '__main__':
    unittest.main()
