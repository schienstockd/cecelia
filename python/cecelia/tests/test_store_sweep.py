"""Tests for the leftover-staging-store sweep (`store_sweep`, the `store-debris` data patch).

Debris is invisible by design — nothing in `ccid.json` names a `*.partial` dir — so the sweep is the
only thing that can find it, and a sweep that quietly deletes the wrong directory is worse than the
disk it reclaims. These pin what it does and does not touch.

Run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import time
import unittest

import cecelia.utils.store_sweep as sweep_mod
from cecelia.utils.zarr_utils import STAGING_SUFFIX, SUPERSEDED_SUFFIX


def _store(root, name, files=('0/0.0.0', '.zattrs'), size=16):
    path = os.path.join(root, name)
    for rel in files:
        full = os.path.join(path, *rel.split('/'))
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, 'wb') as f:
            f.write(b'x' * size)
    return path


def _age(path, seconds):
    """Backdate a store so it no longer looks like an in-flight write."""
    old = time.time() - seconds
    for p in (os.path.join(path, '0'), path):
        os.utime(p, (old, old))


class FindStoreDebrisTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.labels = os.path.join(self.d, '1', 'imgA', 'labels')
        os.makedirs(self.labels, exist_ok=True)

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_finds_both_debris_kinds_and_leaves_real_stores_alone(self):
        _store(self.labels, 'A.zarr')
        partial = _store(self.labels, 'B.zarr' + STAGING_SUFFIX)
        superseded = _store(self.labels, 'C.zarr' + SUPERSEDED_SUFFIX)
        for p in (partial, superseded):
            _age(p, 3600)

        found = sweep_mod.find_store_debris(self.d)
        self.assertEqual(sorted(x['path'] for x in found), sorted([partial, superseded]))
        self.assertTrue(all(x['bytes'] > 0 for x in found))
        self.assertTrue(all(not x['active'] for x in found))

    def test_a_recently_written_store_is_reported_active(self):
        # a run in flight keeps touching its level-0 dir as chunks appear
        _store(self.labels, 'B.zarr' + STAGING_SUFFIX)
        found = sweep_mod.find_store_debris(self.d)
        self.assertEqual(len(found), 1)
        self.assertTrue(found[0]['active'], 'a just-written staging store must not look like debris')

    def test_does_not_descend_into_a_real_store(self):
        # a chunk directory inside a store must never be mistaken for debris, however it is named
        real = _store(self.labels, 'A.zarr')
        os.makedirs(os.path.join(real, 'weird' + STAGING_SUFFIX), exist_ok=True)
        self.assertEqual(sweep_mod.find_store_debris(self.d), [])


class SweepTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.labels = os.path.join(self.d, '1', 'imgA', 'labels')
        os.makedirs(self.labels, exist_ok=True)
        self.keep = _store(self.labels, 'A.zarr')
        self.junk = _store(self.labels, 'B.zarr' + STAGING_SUFFIX)
        _age(self.junk, 3600)

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_dry_run_deletes_nothing(self):
        lines = []
        removed, skipped, freed = sweep_mod.sweep(self.d, apply=False, log=lines.append)
        self.assertEqual((removed, skipped), (1, 0))
        self.assertGreater(freed, 0)
        self.assertTrue(os.path.isdir(self.junk), 'dry-run deleted a store')
        self.assertTrue(any('DRY-RUN' in ln for ln in lines))
        self.assertTrue(any('would remove' in ln for ln in lines))

    def test_apply_removes_debris_and_keeps_the_registered_store(self):
        lines = []
        removed, skipped, freed = sweep_mod.sweep(self.d, apply=True, log=lines.append)
        self.assertEqual((removed, skipped), (1, 0))
        self.assertFalse(os.path.exists(self.junk))
        self.assertTrue(os.path.isdir(self.keep), 'swept a registered store')
        # every mutation is reported, not just counted
        self.assertTrue(any(self.junk in ln and 'removed' in ln for ln in lines))

    def test_apply_skips_a_store_that_still_looks_active(self):
        live = _store(self.labels, 'C.zarr' + STAGING_SUFFIX)     # fresh mtime
        removed, skipped, freed = sweep_mod.sweep(self.d, apply=True, log=lambda _: None)
        self.assertEqual((removed, skipped), (1, 1))
        self.assertTrue(os.path.isdir(live), 'deleted a store a task may still be writing')
        self.assertFalse(os.path.exists(self.junk))

    def test_progress_lines_are_emitted_for_the_task_rail(self):
        lines = []
        sweep_mod.sweep(self.d, apply=False, log=lines.append)
        progress = [ln for ln in lines if ln.startswith('[PROGRESS]')]
        self.assertEqual(progress[0], '[PROGRESS] 0/1')
        self.assertEqual(progress[-1], '[PROGRESS] 1/1')

    def test_nothing_to_do_is_not_an_error(self):
        shutil.rmtree(self.junk)
        removed, skipped, freed = sweep_mod.sweep(self.d, apply=True, log=lambda _: None)
        self.assertEqual((removed, skipped, freed), (0, 0, 0))


if __name__ == '__main__':
    unittest.main()
