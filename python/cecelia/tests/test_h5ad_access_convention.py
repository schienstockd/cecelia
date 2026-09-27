"""Convention test: `.h5ad` access goes through `LabelPropsView` + `write_h5ad_atomic`.

The rule is in the root `CLAUDE.md` → *H5AD / cell-data access — always go through the readers/
writers*. It is the exact structural mirror of the zarr rule enforced by
`test_zarr_access_convention.py`, and the same failure mode: a fresh task that reaches for `h5py`
or `anndata` directly bypasses the truncated-HDF5 guard, the atomic write, and the labelled-view
selection push-down. The rule has been prose-only since it was written; this file makes it a floor
ratchet.

Two assertions:

- **No new `import h5py` or `import anndata`.** Sanctioned readers/writers listed in `_BASELINE`
  keep working; a new file that imports either fails.
- **`_BASELINE` may shrink, must never grow.** `_BASELINE_MAX` locks the current count so an agent
  adding a file to skip the ratchet has to bump the cap in the same PR — visible in review.

Exemption discipline mirrors zarr's: a deliberate one-liner peek needs an inline `# H5AD-OK:`
marker on the exact line, and a whole-file exemption goes in `_BASELINE`.

Run with `pixi run test-py`.
"""
import ast
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

# Files allowed to `import h5py` / `import anndata` directly. Every entry has a reason.
# _BASELINE_MAX is a meta-ratchet: adding a file requires bumping the cap in the same PR, so a
# reviewer sees "weaken the check" attempts. Same shape as test_zarr_access_convention.py.
_BASELINE_MAX = 13
_BASELINE = {
    # Owns the read/write idiom — LabelPropsView IS the canonical h5ad wrapper.
    os.path.join('python', 'cecelia', 'utils', 'label_props_utils.py'),
    # Creators (per CLAUDE.md's "one sanctioned exception — file creation"). Each of these BUILDS
    # a new `.h5ad` from a producing task; the view wraps an existing file only.
    os.path.join('python', 'cecelia', 'utils', 'spatial_utils.py'),
    os.path.join('python', 'cecelia', 'utils', 'tracking_utils.py'),
    os.path.join('python', 'cecelia', 'utils', 'measure_utils.py'),
    # Legacy-migration import path (matches the same file's zarr exemption). Bounded to bridging
    # old-R projects into the h5ad shape; new writers should go through write_h5ad_atomic.
    os.path.join('python', 'cecelia', 'utils', 'legacy_migrate.py'),
    # `.ims` sidecar helpers — they open HDF5 files that AREN'T `.h5ad` (Imaris storage). The
    # baseline is coarse (file-level), so an `import h5py` here counts, though the actual
    # access target is a different HDF5 container. Left in baseline for now; if the `.ims`
    # helpers move into their own module they can leave.
    os.path.join('python', 'cecelia', 'utils', 'ims_meta.py'),
    os.path.join('python', 'cecelia', 'utils', 'ims_relink.py'),
    # Task runners that CREATE a new `.h5ad` (per CLAUDE.md's file-creation exception). Each of
    # these builds the initial file for a downstream LabelPropsView reader — the wrap-around-
    # existing-file rule doesn't apply. Any NEW creator has to enter the baseline in the same PR
    # (bumping _BASELINE_MAX) so the "is this really a creator" question is visible in review.
    os.path.join('app', 'src', 'tasks', 'segment', 'branching_run.py'),
    os.path.join('app', 'src', 'tasks', 'clustRegions', 'cluster_run.py'),
    os.path.join('app', 'src', 'tasks', 'clustTracks', 'cluster_run.py'),
    os.path.join('app', 'src', 'tasks', 'clustPops', 'cluster_run.py'),
    os.path.join('app', 'src', 'tasks', 'behaviour', 'motif_discovery_run.py'),
    # peek_pyramid_run reads `.ims` (Imaris HDF5) to probe series metadata; not `.h5ad`. Same
    # coarse-baseline caveat as ims_meta/ims_relink above.
    os.path.join('app', 'src', 'tasks', 'importImages', 'peek_pyramid_run.py'),
}

# Banned top-level modules (via `import X` or `from X import ...`). The last-segment name matches.
_BANNED_MODULES = {
    'h5py',      # go through LabelPropsView; deliberate peek needs a `# H5AD-OK:` marker
    'anndata',   # write_h5ad_atomic wraps `adata.write_h5ad` — never call it directly
}

_H5AD_OK_RE = re.compile(r'#\s*H5AD-OK:\s*.+')


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


def _banned_imports(tree, src_lines):
    """`import h5py` / `from anndata import ...` calls WITHOUT a `# H5AD-OK:` marker on that line."""
    hits = []
    for node in ast.walk(tree):
        dotted_and_line = []
        if isinstance(node, ast.Import):
            for alias in node.names:
                dotted_and_line.append((alias.name, node.lineno))
        elif isinstance(node, ast.ImportFrom):
            if node.module:
                dotted_and_line.append((node.module, node.lineno))
        for dotted, lineno in dotted_and_line:
            root = dotted.split('.', 1)[0]
            if root not in _BANNED_MODULES:
                continue
            line = src_lines[lineno - 1] if 1 <= lineno <= len(src_lines) else ''
            if _H5AD_OK_RE.search(line):
                continue
            hits.append((lineno, dotted))
    return hits


class H5adAccessConventionTest(unittest.TestCase):
    def test_no_bare_h5py_or_anndata_imports(self):
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
            for lineno, dotted in _banned_imports(tree, src_lines):
                offenders.append(f'{rel}:{lineno}: `import {dotted}`')
        self.assertEqual(
            offenders, [],
            'these bypass the canonical h5ad wrappers. Read via '
            '`cecelia.utils.label_props_utils.LabelPropsView`, write via '
            '`cecelia.utils.atomic_io.write_h5ad_atomic`. See CLAUDE.md → '
            '*H5AD / cell-data access*. For a deliberate one-line peek, add '
            '`# H5AD-OK: <reason>` on the import line:\n  '
            + '\n  '.join(offenders))

    def test_baseline_has_not_grown(self):
        """Meta-ratchet: growing _BASELINE means an agent added a file to skip a violation
        instead of fixing it. Bumping _BASELINE_MAX in the same PR makes that visible."""
        self.assertLessEqual(
            len(_BASELINE), _BASELINE_MAX,
            f'_BASELINE grew to {len(_BASELINE)} (cap {_BASELINE_MAX}). Either fix the new '
            f'violation, or bump _BASELINE_MAX and justify in the PR body. See '
            f'docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.')

    def test_baseline_still_needed(self):
        """A file in `_BASELINE` that became clean must leave the list in the same PR."""
        stale = []
        for rel in sorted(_BASELINE):
            full = os.path.join(_REPO, rel)
            if not os.path.exists(full):
                stale.append(f'{rel}: file no longer exists')
                continue
            with open(full, encoding='utf-8') as f:
                src = f.read()
            try:
                tree = ast.parse(src, filename=rel)
            except SyntaxError:
                continue
            src_lines = src.split('\n')
            still_needs = bool(_banned_imports(tree, src_lines))
            if not still_needs:
                stale.append(f'{rel}: no banned imports remain — remove from `_BASELINE`')
        self.assertEqual(stale, [], '\n  ' + '\n  '.join(stale))


class H5adAccessScanCoverageTest(unittest.TestCase):
    """Guard against the scan silently covering nothing (a wrong root, no matches)."""
    def test_scan_reaches_known_h5ad_util_call_sites(self):
        # Sanity-check: the scan must at least see the sanctioned readers in the file tree.
        # If the scan root drifts, this catches it before the ratchet passes on an empty
        # set for the wrong reason.
        known = {'LabelPropsView', 'write_h5ad_atomic'}
        seen = set()
        for _, full in _py_files():
            with open(full, encoding='utf-8') as f:
                src = f.read()
            for name in known:
                if name in src:
                    seen.add(name)
        self.assertEqual(known, seen,
                         f'expected the scan to see {sorted(known)}, missing {sorted(known - seen)}')


if __name__ == '__main__':
    unittest.main()
