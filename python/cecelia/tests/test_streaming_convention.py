"""Convention tests: per-timepoint reads and writes go through the streaming helpers.

Two rules, enforced by AST scan of the task runners under ``app/src/tasks/**/*_run.py``:

- **Read side.** Iterating a zarr level frame by frame goes through
  ``zarr_utils.read_timepoint`` — the ONE reusable primitive
  (docs/todo/ZARR_STREAMING_PLAN.md Decision 1). Hand-rolled ``level[tuple(sel)]``
  with a per-``t`` slice is exactly what the streaming plan replaced (segmentation, then
  bin/dtype/flip/tProject/zProject/cropImage/resampleZ). A new runner that hand-rolls a
  per-``t`` slice off a zarr level fails this test.

- **Write side.** A per-timepoint write goes through ``zarr_utils.open_multiscales_for_writing``
  and fills level 0 one frame at a time — never accumulate the whole T-stack in RAM and hand it
  to ``create_multiscales(stacked, …)``. The docstring of ``open_multiscales_for_writing`` is
  the point of the helper. A runner that computes ``np.stack(frames, …)`` and passes the result
  into ``create_multiscales`` fails this test.

Rules exist in prose (`docs/todo/ZARR_STREAMING_PLAN.md`, `open_multiscales_for_writing`'s
docstring, `read_timepoint`'s docstring) but were previously docs-only, which is how
`ridges_run.py` shipped both anti-patterns. Every established runner already follows the rule,
so the test succeeds on the current tree — this pins the discipline.

Exemption: add an inline ``STREAMING-READ-EXEMPT: <why>`` or ``STREAMING-WRITE-EXEMPT: <why>``
comment in the file for a legitimate reason (e.g. a whole-level analytical read that doesn't
map to per-frame semantics). Kept scarce on purpose.

Run with ``pixi run test-py``.
"""
import ast
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_RUNNERS_ROOT = os.path.join(_REPO, 'app', 'src', 'tasks')

_READ_EXEMPT_RE = re.compile(r'STREAMING-READ-EXEMPT:\s*.+')
_WRITE_EXEMPT_RE = re.compile(r'STREAMING-WRITE-EXEMPT:\s*.+')


def _runner_files():
    for dirpath, _, filenames in os.walk(_RUNNERS_ROOT):
        for fn in filenames:
            if fn.endswith('_run.py'):
                yield os.path.join(dirpath, fn)


def _iter_calls(tree):
    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            yield node


def _call_name(call):
    """Last attribute segment (``zarr_utils.read_timepoint`` → ``read_timepoint``)."""
    f = call.func
    if isinstance(f, ast.Attribute):
        return f.attr
    if isinstance(f, ast.Name):
        return f.id
    return ''


def _names_from_open_as_zarr(tree):
    """Names in the module that carry an opened zarr *level list* or a specific level.

    Tracks assignments of the shapes we actually use:
        levels, _ = zarr_utils.open_as_zarr(...)          # or open_zarr(...)
        level0    = zarr_utils.open_as_zarr(...)[0][0]
        level0    = levels[0]
        arr       = zarr_utils.open_as_zarr(...)[0][0]
    Anything referenced through one of those names is treated as a zarr level for the read rule.
    """
    zarr_openers = {'open_as_zarr', 'open_zarr'}
    tainted = set()

    def _taints(node):
        if isinstance(node, ast.Call) and _call_name(node) in zarr_openers:
            return True
        if isinstance(node, ast.Subscript):
            return _taints(node.value)
        if isinstance(node, ast.Name) and node.id in tainted:
            return True
        return False

    # multiple passes so a name assigned from another tainted name is caught
    changed = True
    while changed:
        changed = False
        for node in ast.walk(tree):
            if not isinstance(node, ast.Assign):
                continue
            if not _taints(node.value):
                continue
            for tgt in node.targets:
                # `a = ...`
                if isinstance(tgt, ast.Name) and tgt.id not in tainted:
                    tainted.add(tgt.id); changed = True
                # `a, _ = ...` (tuple unpack — first element)
                elif isinstance(tgt, (ast.Tuple, ast.List)):
                    for i, elt in enumerate(tgt.elts):
                        if i == 0 and isinstance(elt, ast.Name) and elt.id not in tainted:
                            tainted.add(elt.id); changed = True
    return tainted


def _subscript_target_name(subscript):
    """Root Name of a Subscript chain (``a[i][j][k]`` → ``a``)."""
    v = subscript.value
    while isinstance(v, ast.Subscript):
        v = v.value
    return v.id if isinstance(v, ast.Name) else None


def _for_range_loops(tree):
    for node in ast.walk(tree):
        if isinstance(node, ast.For) and isinstance(node.iter, ast.Call):
            if _call_name(node.iter) == 'range':
                yield node


def _find_per_t_reads(tree, tainted):
    """Subscript accesses on a tainted name inside a ``for … in range(...)`` loop body."""
    hits = []
    for loop in _for_range_loops(tree):
        for node in ast.walk(loop):
            if isinstance(node, ast.Subscript):
                root = _subscript_target_name(node)
                if root in tainted:
                    hits.append((node.lineno, root))
    return hits


def _find_stacked_writes(tree):
    """`create_multiscales(<name>, …)` where <name> was assigned from `np.stack(...)`."""
    stack_names = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Assign) and isinstance(node.value, ast.Call):
            if _call_name(node.value) == 'stack':
                for tgt in node.targets:
                    if isinstance(tgt, ast.Name):
                        stack_names.add(tgt.id)
    hits = []
    for call in _iter_calls(tree):
        if _call_name(call) != 'create_multiscales' or not call.args:
            continue
        first = call.args[0]
        if isinstance(first, ast.Name) and first.id in stack_names:
            hits.append(call.lineno)
    return hits


class StreamingReadConventionTest(unittest.TestCase):
    def test_per_timepoint_reads_go_through_read_timepoint(self):
        offenders = []
        for path in _runner_files():
            with open(path, encoding='utf-8') as f:
                src = f.read()
            if _READ_EXEMPT_RE.search(src):
                continue
            tree = ast.parse(src, filename=path)
            calls = {_call_name(c) for c in _iter_calls(tree)}
            if 'read_timepoint' in calls:
                continue
            tainted = _names_from_open_as_zarr(tree)
            if not tainted:
                continue
            hits = _find_per_t_reads(tree, tainted)
            if hits:
                rel = os.path.relpath(path, _REPO)
                for lineno, root in hits:
                    offenders.append(f'{rel}:{lineno}: `{root}[…]` inside `for … in range(…)`')
        self.assertEqual(
            offenders, [],
            "these runners hand-roll a per-timepoint zarr read; use "
            "`zarr_utils.read_timepoint(level, dim_utils, t)` — one reusable primitive per "
            "docs/todo/ZARR_STREAMING_PLAN.md Decision 1. Add "
            "`# STREAMING-READ-EXEMPT: <why>` for a legitimate whole-level read:\n  "
            + '\n  '.join(offenders))


class StreamingWriteConventionTest(unittest.TestCase):
    def test_no_accumulate_then_create_multiscales(self):
        offenders = []
        for path in _runner_files():
            with open(path, encoding='utf-8') as f:
                src = f.read()
            if _WRITE_EXEMPT_RE.search(src):
                continue
            tree = ast.parse(src, filename=path)
            hits = _find_stacked_writes(tree)
            if hits:
                rel = os.path.relpath(path, _REPO)
                for lineno in hits:
                    offenders.append(f'{rel}:{lineno}: `create_multiscales(np.stack(...), …)`')
        self.assertEqual(
            offenders, [],
            "these runners accumulate the whole T-stack in RAM before writing; stream through "
            "`zarr_utils.open_multiscales_for_writing` and fill level 0 one frame at a time (its "
            "docstring is the point of the helper). Add `# STREAMING-WRITE-EXEMPT: <why>` for a "
            "legitimate one-shot write:\n  "
            + '\n  '.join(offenders))


class StreamingConventionScanCoverageTest(unittest.TestCase):
    """Guard against the scan silently covering nothing (a wrong root, no matches)."""
    def test_the_scan_reaches_known_streaming_runners(self):
        known = {'bin_run.py', 'dtype_run.py', 'zProject_run.py', 'tProject_run.py',
                 'flip_run.py', 'ridges_run.py'}
        seen = set()
        for path in _runner_files():
            fn = os.path.basename(path)
            if fn in known:
                with open(path, encoding='utf-8') as f:
                    if 'read_timepoint' in f.read():
                        seen.add(fn)
        self.assertEqual(known, seen,
                         f'expected the scan to cover {sorted(known)}, missing {sorted(known - seen)}')


if __name__ == '__main__':
    unittest.main()
