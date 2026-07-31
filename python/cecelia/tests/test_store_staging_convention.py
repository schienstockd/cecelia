"""Convention test: a store write must go through `zarr_utils.staged_store`.

See docs/SEGMENTATION.md → *Stores are written staged, never in place*.

The bug this guards is silent. A writer that opens its FINAL path in write mode destroys the previous
store up front and then fills it over minutes, so cancelling a re-run of an already-registered
value_name leaves `ccid.json` advertising a store missing most of its frames. On a multi-level store
the next read raises `KeyError: '1'`; on a single-level one (drift/AF/cellpose-corrected output — the
common case) there is no error at all and the missing frames read as zeros.

`staged_store` fixes it, but only for the call sites that use it — and new tasks get added by people
who have never heard of it. So this scans the source, exactly like the `no hand-rolled state writes`
testset does for `ccid.json`. Matching is done over the AST, not the raw text, so a comment or a
docstring that merely names a writer is not an offender.

Run with `pixi run test-py`.
"""
import ast
import os
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

# Writers that create a store at a path the object model can point at.
_STORE_WRITERS = {'create_multiscales', 'open_multiscales_for_writing'}
_STAGING = 'staged_store'

# Files allowed to write a store path directly, and why.
_EXEMPT = {
    # Owns the idiom: defines staged_store/promote_store and the low-level writers themselves.
    os.path.join('python', 'cecelia', 'utils', 'zarr_utils.py'),
    # One-off maintenance CLI, not a task. Writes to a NON-registered output name
    # (`*.rechunked.ome.zarr`) or replaces in place while deliberately RETAINING the original as
    # `*.bak.ome.zarr` — a user-facing backup, where staged_store deletes the superseded copy. No
    # ccid.json can point at either temp, so the truncation bug class doesn't apply. See the note in
    # that file.
    os.path.join('python', 'cecelia', 'utils', 'rechunk_zarr.py'),
}


def _py_files():
    """Task runners + the cecelia library, excluding the test suite itself."""
    for root in (os.path.join('app', 'src'), os.path.join('python', 'cecelia')):
        for dirpath, dirnames, filenames in os.walk(os.path.join(_REPO, root)):
            dirnames[:] = [d for d in dirnames if d not in ('tests', '__pycache__')]
            for fn in filenames:
                if not fn.endswith('.py'):
                    continue
                full = os.path.join(dirpath, fn)
                rel = os.path.relpath(full, _REPO)
                if rel not in _EXEMPT:
                    yield rel, full


def _calls(tree):
    """Called names, by their last attribute segment (`zarr_utils.create_multiscales` → the name)."""
    out = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            f = node.func
            out.add(f.attr if isinstance(f, ast.Attribute) else
                    f.id if isinstance(f, ast.Name) else '')
    return out


def _write_mode_opens(tree):
    """`zarr.open_group(..., mode='w')` / `open_array(..., mode='w')` calls."""
    found = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        name = (node.func.attr if isinstance(node.func, ast.Attribute) else
                node.func.id if isinstance(node.func, ast.Name) else '')
        if name not in ('open_group', 'open_array'):
            continue
        for kw in node.keywords:
            if kw.arg == 'mode' and isinstance(kw.value, ast.Constant) and kw.value.value == 'w':
                found.append(node.lineno)
    return found


class StoreStagingConventionTest(unittest.TestCase):
    def _scan(self, predicate):
        offenders = []
        for rel, full in _py_files():
            with open(full, encoding='utf-8') as f:
                tree = ast.parse(f.read(), filename=rel)
            calls = _calls(tree)
            if _STAGING in calls:
                continue                      # writes through the staging helper
            hit = predicate(tree, calls)
            if hit:
                offenders.append(f'{rel}: {hit}')
        return offenders

    def test_multiscale_writers_go_through_staged_store(self):
        offenders = self._scan(
            lambda tree, calls: ', '.join(sorted(calls & _STORE_WRITERS)) or None)
        self.assertEqual(
            offenders, [],
            'these call a multiscales writer without zarr_utils.staged_store, so a cancelled run '
            'can leave a registered store truncated — see docs/SEGMENTATION.md:\n  '
            + '\n  '.join(offenders))

    def test_no_write_mode_store_opens_outside_the_staging_idiom(self):
        offenders = self._scan(
            lambda tree, calls: (lambda lines: f'write-mode open at line(s) '
                                 f'{", ".join(map(str, lines))}' if lines else None)(
                                     _write_mode_opens(tree)))
        self.assertEqual(
            offenders, [],
            'these open a store in write mode without zarr_utils.staged_store — write into a '
            'staging path and rename it into place instead — see docs/SEGMENTATION.md:\n  '
            + '\n  '.join(offenders))

    def test_the_scan_actually_reaches_the_writers(self):
        """Guard against the scan silently covering nothing (a wrong root, a bad exclude)."""
        staged = []
        for rel, full in _py_files():
            with open(full, encoding='utf-8') as f:
                if _STAGING in _calls(ast.parse(f.read(), filename=rel)):
                    staged.append(rel)
        self.assertGreaterEqual(
            len(staged), 6,
            f'expected the known staged writers to be in scope, found {staged}')


if __name__ == '__main__':
    unittest.main()
