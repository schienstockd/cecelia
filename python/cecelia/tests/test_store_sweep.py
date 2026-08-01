"""Tests for the leftover-staging-store sweep (`store_sweep`, the `store-debris` data patch).

Debris is invisible by design — nothing in `ccid.json` names a `*.partial` dir — so the sweep is the
only thing that can find it, and a sweep that quietly deletes the wrong directory is worse than the
disk it reclaims. These pin what it does and does not touch.

Run with `pixi run test-py`.
"""
import json
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


def _ccid(meta_dir, **fields):
    """Write a ccid.json registering the given fields (filepath / labels / branch_labels)."""
    os.makedirs(meta_dir, exist_ok=True)
    with open(os.path.join(meta_dir, 'ccid.json'), 'w', encoding='utf-8') as f:
        json.dump(fields, f)


def _multiscale_store(root, name, declared, present):
    """A store whose .zattrs declares `declared` levels but only has `present` of them on disk —
    what a streaming writer leaves when it is killed before `_finalize_label_pyramid`."""
    path = os.path.join(root, name)
    os.makedirs(path, exist_ok=True)
    with open(os.path.join(path, '.zattrs'), 'w', encoding='utf-8') as f:
        json.dump({'multiscales': [{'axes': [{'name': a} for a in ('t', 'z', 'y', 'x')],
                                    'datasets': [{'path': str(i)} for i in range(declared)]}]}, f)
    with open(os.path.join(path, '.zgroup'), 'w', encoding='utf-8') as f:
        json.dump({'zarr_format': 2}, f)
    for i in range(present):
        os.makedirs(os.path.join(path, str(i)), exist_ok=True)
        with open(os.path.join(path, str(i), '0.0.0'), 'wb') as f:
            f.write(b'x' * 16)
    return path


class StructuralDetectionTest(unittest.TestCase):
    """The half that name-matching cannot do.

    `*.partial` only catches writers that opted into `staged_store`. IMPORT does not: on the 16-bit path
    bioformats2raw writes straight to the FINAL name, so a cancelled import leaves a half-written store
    called `ccidImage.ome.zarr` — which the sweep actively SKIPPED as "a real store" — plus
    `ccidImage.16bit.tmp.ome.zarr` and `_stage_src` (often the biggest of the three).
    """

    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.proj = os.path.join(self.d, 'projX')
        self.zero = os.path.join(self.proj, '0', 'imgA')
        self.meta = os.path.join(self.proj, '1', 'imgA')
        os.makedirs(self.zero, exist_ok=True)
        _ccid(self.meta, filepath={'default': 'ccidImage.ome.zarr', '_active': 'default'})

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def _find(self, **kw):
        return {x['path']: x for x in sweep_mod.find_store_debris(self.proj, **kw)}

    def test_a_registered_store_is_left_alone(self):
        keep = _store(self.zero, 'ccidImage.ome.zarr')
        _age(keep, 3600)
        self.assertNotIn(keep, self._find())

    def test_an_unregistered_store_is_found(self):
        # a cancelled import is unregistered BY CONSTRUCTION — registration is the last thing a
        # successful run does
        orphan = _store(self.zero, 'ccidDriftCorrected.ome.zarr')
        _age(orphan, 3600)
        found = self._find()
        self.assertIn(orphan, found)
        self.assertEqual(found[orphan]['why'], 'unregistered')

    def test_the_import_leftovers_name_matching_missed(self):
        # the exact three a cancelled 16-bit import leaves
        half = _store(self.zero, 'ccidImage.16bit.tmp.ome.zarr')
        stage = os.path.join(self.zero, '_stage_src')
        os.makedirs(stage, exist_ok=True)
        with open(os.path.join(stage, 'src.tif'), 'wb') as f:
            f.write(b'x' * 4096)
        for p in (half, stage):
            _age(p, 3600) if os.path.isdir(os.path.join(p, '0')) else os.utime(p, (0, 0))

        found = self._find()
        self.assertIn(half, found)
        self.assertEqual(found[half]['why'], 'unregistered')
        self.assertIn(stage, found)
        self.assertEqual(found[stage]['why'], 'scratch')
        self.assertGreater(found[stage]['bytes'], 0)   # the biggest item is the one worth reporting

        # ...and the point of the whole change: name-matching alone sees NONE of it
        self.assertEqual(sweep_mod.find_store_debris(self.proj, structural=False), [])

    def test_an_incomplete_pyramid_is_found_even_at_a_registered_path(self):
        # .zattrs promises 3 levels, disk has 1 — the `KeyError: '1'` a consumer hits
        truncated = _multiscale_store(self.zero, 'ccidImage.ome.zarr', declared=3, present=1)
        _age(truncated, 3600)
        found = self._find()
        self.assertIn(truncated, found)
        self.assertEqual(found[truncated]['why'], 'incomplete')
        self.assertIn('3', found[truncated]['detail'])

    def test_a_complete_pyramid_at_a_registered_path_is_left_alone(self):
        whole = _multiscale_store(self.zero, 'ccidImage.ome.zarr', declared=3, present=3)
        _age(whole, 3600)
        self.assertNotIn(whole, self._find())

    def test_a_single_level_store_promises_nothing(self):
        # most label stores are single-level; "declared == present == 1" must not read as incomplete
        one = _multiscale_store(self.zero, 'ccidImage.ome.zarr', declared=1, present=1)
        _age(one, 3600)
        self.assertNotIn(one, self._find())

    def test_labels_are_checked_against_their_own_ccid_field(self):
        _ccid(self.meta, filepath={'default': 'ccidImage.ome.zarr'},
              labels={'A': ['A.zarr', 'A_nuc.zarr']})
        labels_dir = os.path.join(self.meta, 'labels')
        keep = _store(labels_dir, 'A.zarr')
        keep_nuc = _store(labels_dir, 'A_nuc.zarr')
        orphan = _store(labels_dir, 'B.zarr')
        for p in (keep, keep_nuc, orphan):
            _age(p, 3600)
        found = self._find()
        self.assertNotIn(keep, found)
        self.assertNotIn(keep_nuc, found)          # a multi-type run registers BOTH files
        self.assertIn(orphan, found)

    def test_analysis_dirs_are_never_treated_as_orphans(self):
        # data/, qc/, gating/, labelProps/ are legitimately unregistered — deleting them would take the
        # user's analysis with them, which is why the orphan check is scoped to store locations
        for sub in ('data', 'qc', 'gating', 'labelProps'):
            p = os.path.join(self.meta, sub)
            os.makedirs(p, exist_ok=True)
            with open(os.path.join(p, 'x.json'), 'w', encoding='utf-8') as f:
                f.write('{}')
        found = self._find()
        self.assertEqual([p for p in found if os.sep + 'labelProps' in p], [])
        self.assertEqual(found, {})

    def test_an_unreadable_ccid_reports_nothing_rather_than_everything(self):
        # a missing/corrupt ccid.json must not turn every store into an orphan
        with open(os.path.join(self.meta, 'ccid.json'), 'w', encoding='utf-8') as f:
            f.write('{not json')
        s = _store(self.zero, 'ccidImage.ome.zarr')
        _age(s, 3600)
        self.assertNotIn(s, self._find())

    def test_a_store_being_written_right_now_is_reported_active(self):
        # the in-use guard applies to the structural finds too, not just the named ones
        orphan = _store(self.zero, 'ccidSomethingNew.ome.zarr')     # freshly written, not backdated
        found = self._find()
        self.assertIn(orphan, found)
        self.assertTrue(found[orphan]['active'])
        removed, skipped, freed = sweep_mod.sweep(self.proj, apply=True, log=lambda _m: None)
        self.assertEqual((removed, skipped), (0, 1))
        self.assertTrue(os.path.isdir(orphan), 'an active store must survive an apply')


class SummariseTest(unittest.TestCase):
    """`summarise` feeds the Settings storage box, so it must agree with what a sweep would delete —
    it is the same detector, deliberately, not a cheaper name-based approximation beside it."""

    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.proj = os.path.join(self.d, 'projY')
        self.zero = os.path.join(self.proj, '0', 'imgA')
        os.makedirs(self.zero, exist_ok=True)
        _ccid(os.path.join(self.proj, '1', 'imgA'),
              filepath={'default': 'ccidImage.ome.zarr', '_active': 'default'})

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_counts_and_bytes_match_a_dry_run(self):
        _age(_store(self.zero, 'ccidImage.ome.zarr'), 3600)          # registered → not debris
        for n in ('orphan.ome.zarr', 'other.ome.zarr' + STAGING_SUFFIX):
            _age(_store(self.zero, n), 3600)
        s = sweep_mod.summarise(self.proj)
        removed, skipped, freed = sweep_mod.sweep(self.proj, apply=False, log=lambda _m: None)
        self.assertEqual(s['count'], removed)
        self.assertEqual(s['bytes'], freed)
        self.assertEqual(s['activeSkipped'], 0)
        self.assertEqual(sorted(s['byWhy']), ['staging', 'unregistered'])

    def test_excludes_what_a_sweep_would_skip(self):
        # an in-flight store must not be advertised as reclaimable — the box would be promising bytes
        # the patch then refuses to free
        _store(self.zero, 'inflight.ome.zarr')                        # fresh, looks active
        s = sweep_mod.summarise(self.proj)
        self.assertEqual(s['count'], 0)
        self.assertEqual(s['bytes'], 0)
        self.assertEqual(s['activeSkipped'], 1)

    def test_a_clean_project_reports_zero(self):
        _age(_store(self.zero, 'ccidImage.ome.zarr'), 3600)
        self.assertEqual(sweep_mod.summarise(self.proj),
                         {'count': 0, 'bytes': 0, 'activeSkipped': 0, 'byWhy': {}})
