"""Convention tests: image/label store access goes through `zarr_utils`, OME-XML through
`ome_xml_utils`, and `dask.array` is opt-in inside task runners.

The rule is in the root `CLAUDE.md` → *Image / OME-ZARR access — always go through `zarr_utils`*.
Existing convention tests are writer-side only (`test_store_compressor_convention.py`,
`test_store_staging_convention.py`, `test_streaming_convention.py`); none of them fail on a NEW bare
`zarr.open`, `da.from_zarr`, `tifffile.imread`, raw `.zarray`/`.zattrs` read, or hand-rolled
`xml.etree`/`lxml` OME-XML parse. This pins the discipline the same way the typed-params ratchet in
`app/test/suite.jl` does — an exact per-file baseline that MAY SHRINK, MUST NEVER GROW.

Three assertions:

- **No new zarr/tifffile/OME-XML bypass.** `import zarr`, `import tifffile`, `import xml.etree*`,
  `from lxml*`, and `dask.array.from_zarr(...)` are only allowed in the sanctioned readers
  themselves or in a listed baseline file. A new file that pulls one in fails.
- **No new dask in a task runner.** `import dask*` (or `from dask*`) inside `app/src/tasks/**/*.py`
  is banned without a `# DASK-OK: <reason>` marker. The sanctioned entry is
  `zarr_utils.open_as_zarr(..., as_dask=True)`, which returns dask-backed levels without pulling
  `dask.array` into the runner. Reach for `dask.array` directly only for a whole-level analytical
  pass; the streaming primitive `zarr_utils.read_timepoint` covers the per-frame case.
- **Scan coverage floor.** Guard against a wrong scan root by pinning a known-good file count.

Exemption discipline mirrors the H5AD/zarr readers (CLAUDE.md → deviations need an inline comment
on that exact line). File-level exemptions live in `_BASELINE`; the marker for a fresh runner
adopting dask deliberately is `# DASK-OK: <reason>` on the import line.

Run with `pixi run test-py`.
"""
import ast
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

# Files allowed to talk to zarr / tifffile / OME-XML / lxml directly. Every entry has a reason.
_BASELINE = {
    # Owns the idiom: zarr_utils IS the canonical zarr wrapper; ome_xml_utils IS the OME-XML parser.
    os.path.join('python', 'cecelia', 'utils', 'zarr_utils.py'),
    os.path.join('python', 'cecelia', 'utils', 'ome_xml_utils.py'),
    # One-off maintenance CLI, not a task. Rewrites a store to a NON-registered output name
    # (`*.rechunked.ome.zarr`) or in place with a `.bak` retained; explicitly exempt in
    # test_store_staging_convention.py for the same reason.
    os.path.join('python', 'cecelia', 'utils', 'rechunk_zarr.py'),
    # Legacy-migration import path: parses old-R `METADATA.ome.xml` (a parallel reader for a legacy
    # asset — the module header documents itself as such) and peeks a raw `0/.zarray` to get shape
    # when the source hasn't been rewrapped yet. Bounded to the import task; new callers should use
    # `zarr_utils.read_axes` / `ome_xml_utils.load_ome_xml`.
    os.path.join('python', 'cecelia', 'utils', 'legacy_migrate.py'),
    # The task IS to write a TIFF (`exportImages.ome_tiff`), so `tifffile` is unavoidable here. No
    # other task should import tifffile.
    os.path.join('app', 'src', 'tasks', 'exportImages', 'ome_tiff_run.py'),
}

# Banned top-level modules (via `import X` or `from X import ...`). The last-segment name matches.
_BANNED_MODULES = {
    'zarr',       # go through zarr_utils.open_as_zarr / open_zarr / staged_store / …
    'tifffile',   # exportImages/ome_tiff_run.py is the only sanctioned writer
}
# Banned dotted paths (for xml.etree.ElementTree and lxml.*).
_BANNED_MODULE_PREFIXES = ('xml.etree', 'lxml')

_DASK_OK_RE = re.compile(r'#\s*DASK-OK:\s*.+')
_RUNNER_ROOT = os.path.join('app', 'src', 'tasks')


def _py_files():
    """Task runners + the cecelia library, excluding the test suite itself."""
    for root in (os.path.join('app', 'src'), os.path.join('python', 'cecelia')):
        for dirpath, dirnames, filenames in os.walk(os.path.join(_REPO, root)):
            dirnames[:] = [d for d in dirnames if d not in ('tests', '__pycache__')]
            for fn in filenames:
                if not fn.endswith('.py'):
                    continue
                full = os.path.join(dirpath, fn)
                yield os.path.relpath(full, _REPO), full


def _imports(tree):
    """Yield (module_dotted, lineno) for every top-level module reference in Import / ImportFrom."""
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                yield alias.name, node.lineno
        elif isinstance(node, ast.ImportFrom):
            if node.module:
                yield node.module, node.lineno


def _module_is_banned(dotted):
    root = dotted.split('.', 1)[0]
    if root in _BANNED_MODULES:
        return True
    for prefix in _BANNED_MODULE_PREFIXES:
        if dotted == prefix or dotted.startswith(prefix + '.'):
            return True
    return False


def _dask_imports(tree, src_lines):
    """`import dask*` / `from dask* import ...` calls WITHOUT a `# DASK-OK:` marker on that line."""
    hits = []
    for dotted, lineno in _imports(tree):
        root = dotted.split('.', 1)[0]
        if root != 'dask':
            continue
        line = src_lines[lineno - 1] if 1 <= lineno <= len(src_lines) else ''
        if _DASK_OK_RE.search(line):
            continue
        hits.append((lineno, dotted))
    return hits


def _from_zarr_calls(tree):
    """`da.from_zarr(...)` / `dask.array.from_zarr(...)` — banned everywhere. Sanctioned entry is
    `zarr_utils.open_as_zarr(..., as_dask=True)`."""
    hits = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        f = node.func
        if isinstance(f, ast.Attribute) and f.attr == 'from_zarr':
            hits.append(node.lineno)
    return hits


class ZarrAccessConventionTest(unittest.TestCase):
    def test_no_bare_zarr_tifffile_or_ome_xml_imports(self):
        offenders = []
        for rel, full in _py_files():
            if rel in _BASELINE:
                continue
            with open(full, encoding='utf-8') as f:
                tree = ast.parse(f.read(), filename=rel)
            for dotted, lineno in _imports(tree):
                if _module_is_banned(dotted):
                    offenders.append(f'{rel}:{lineno}: `import {dotted}`')
        self.assertEqual(
            offenders, [],
            'these bypass the canonical readers. Use `zarr_utils.open_as_zarr` / `read_axes` / '
            '`staged_store`, and `ome_xml_utils.load_ome_xml`. See CLAUDE.md → '
            '*Image / OME-ZARR access*:\n  '
            + '\n  '.join(offenders))

    def test_no_from_zarr_calls(self):
        offenders = []
        for rel, full in _py_files():
            if rel in _BASELINE:
                continue
            with open(full, encoding='utf-8') as f:
                tree = ast.parse(f.read(), filename=rel)
            for lineno in _from_zarr_calls(tree):
                offenders.append(f'{rel}:{lineno}: `*.from_zarr(...)` — use '
                                 f'`zarr_utils.open_as_zarr(..., as_dask=True)`')
        self.assertEqual(offenders, [], '\n  ' + '\n  '.join(offenders))

    def test_baseline_still_needed(self):
        """A file in `_BASELINE` that became clean must leave the list in the same PR."""
        stale = []
        for rel in sorted(_BASELINE):
            full = os.path.join(_REPO, rel)
            if not os.path.exists(full):
                stale.append(f'{rel}: file no longer exists')
                continue
            with open(full, encoding='utf-8') as f:
                tree = ast.parse(f.read(), filename=rel)
            dotted_names = [d for d, _ in _imports(tree)]
            has_from_zarr = bool(_from_zarr_calls(tree))
            still_needs = any(_module_is_banned(d) for d in dotted_names) or has_from_zarr
            if not still_needs:
                stale.append(f'{rel}: no banned imports or `.from_zarr` remain — remove from '
                             f'`_BASELINE`')
        self.assertEqual(stale, [], '\n  ' + '\n  '.join(stale))


class DaskInRunnersConventionTest(unittest.TestCase):
    def test_no_dask_imports_in_task_runners_without_marker(self):
        offenders = []
        for rel, full in _py_files():
            if not rel.startswith(_RUNNER_ROOT + os.sep):
                continue
            with open(full, encoding='utf-8') as f:
                src = f.read()
            src_lines = src.split('\n')
            tree = ast.parse(src, filename=rel)
            for lineno, dotted in _dask_imports(tree, src_lines):
                offenders.append(f'{rel}:{lineno}: `import {dotted}`')
        self.assertEqual(
            offenders, [],
            'these task runners import `dask.array` directly. The sanctioned entry is '
            '`zarr_utils.open_as_zarr(..., as_dask=True)`, which returns dask-backed levels without '
            'pulling `dask.array` into the runner. Per-frame reads go through '
            '`zarr_utils.read_timepoint` — see CLAUDE.md → *Image / OME-ZARR access*. For a '
            'legitimate whole-level analytical pass that genuinely needs dask, add '
            '`# DASK-OK: <reason>` on the import line:\n  '
            + '\n  '.join(offenders))


class ZarrAccessScanCoverageTest(unittest.TestCase):
    """Guard against the scan silently covering nothing (a wrong root, no matches)."""
    def test_scan_reaches_known_zarr_util_call_sites(self):
        known = {'open_as_zarr', 'staged_store', 'read_timepoint'}
        seen = set()
        for _, full in _py_files():
            with open(full, encoding='utf-8') as f:
                src = f.read()
            for name in known:
                if f'zarr_utils.{name}' in src:
                    seen.add(name)
        self.assertEqual(known, seen,
                         f'expected the scan to see {sorted(known)}, missing {sorted(known - seen)}')


if __name__ == '__main__':
    unittest.main()
