"""Tests for the Imaris soft-link repair (`ims_relink`, the `ims-softlink` data patch).

This is the only patch that writes to a file OUTSIDE the project — the user's raw acquisition data —
so what it declines to touch matters as much as what it fixes. These pin both: a dangling link, a
non-Imaris HDF5, an unwritable file and a missing source must all be reported and left alone, and a
repair must leave the pixels byte-identical.

Run with `pixi run test-py`.
"""
import json
import os
import shutil
import stat
import tempfile
import unittest

import cecelia.utils.ims_relink as ims_relink

try:
    import h5py
    import numpy as np
    HAVE_H5PY = True
except ImportError:                       # h5py is a pixi-tier dep, not part of the IO tier
    HAVE_H5PY = False


def _ims(path, soft=True, dangling=False, imaris=True):
    """A miniature Imaris file: data under /Workflows/InitialImages, linked from the root."""
    rng = np.random.default_rng(0)
    with h5py.File(path, 'w') as f:
        wf = f.create_group('Workflows/InitialImages')
        wf.create_group('DataSetInfo/Image')
        d = wf.create_group('DataSet/ResolutionLevel 0/TimePoint 0/Channel 0')
        d.create_dataset('Data', data=rng.integers(0, 4095, (2, 4, 4), dtype=np.uint16))
        if not imaris:
            return
        for name in ('DataSet', 'DataSetInfo'):
            target = f'/Workflows/InitialImages/{name}'
            if dangling:
                f[name] = h5py.SoftLink('/Workflows/NotHere/' + name)
            elif soft:
                f[name] = h5py.SoftLink(target)
            else:
                f[name] = f[target]


def _project(root, sources):
    """A project skeleton whose images record `sources` as their `meta.ori_path`."""
    for i, src in enumerate(sources):
        meta_dir = os.path.join(root, '1', f'img{i}')
        os.makedirs(meta_dir)
        with open(os.path.join(meta_dir, 'ccid.json'), 'w', encoding='utf-8') as f:
            json.dump({'uid': f'img{i}', 'meta': {'ori_path': src}}, f)


@unittest.skipUnless(HAVE_H5PY, 'h5py not installed')
class InspectTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def _p(self, name):
        return os.path.join(self.d, name)

    def test_soft_linked_file_is_repairable(self):
        _ims(self._p('a.ims'))
        info = ims_relink.inspect(self._p('a.ims'))
        self.assertEqual(info['state'], 'repairable')
        self.assertEqual(set(info['links']), {'DataSet', 'DataSetInfo'})

    def test_already_direct_file_needs_no_change(self):
        _ims(self._p('a.ims'), soft=False)
        self.assertEqual(ims_relink.inspect(self._p('a.ims'))['state'], 'ok')

    def test_dangling_link_is_not_reported_repairable(self):
        # Repairing means hard-linking the TARGET; if the target isn't there we'd delete the only
        # record of what the link pointed at and fix nothing.
        _ims(self._p('a.ims'), dangling=True)
        self.assertEqual(ims_relink.inspect(self._p('a.ims'))['state'], 'unreadable')

    def test_hdf5_without_imaris_root_entries_is_left_alone(self):
        _ims(self._p('a.ims'), imaris=False)
        self.assertEqual(ims_relink.inspect(self._p('a.ims'))['state'], 'not-imaris')

    def test_missing_source_is_reported_not_raised(self):
        # The everyday case: the source lived on a share that isn't mounted right now.
        self.assertEqual(ims_relink.inspect(self._p('gone.ims'))['state'], 'missing')

    def test_non_hdf5_file_is_unreadable(self):
        with open(self._p('a.ims'), 'wb') as f:
            f.write(b'not hdf5')
        self.assertEqual(ims_relink.inspect(self._p('a.ims'))['state'], 'unreadable')


@unittest.skipUnless(HAVE_H5PY, 'h5py not installed')
class PatchTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.root = os.path.join(self.d, 'proj')
        self.src = os.path.join(self.d, 'a.ims')
        _ims(self.src)
        _project(self.root, [self.src])
        with h5py.File(self.src, 'r') as f:
            self.pixels = f['DataSet/ResolutionLevel 0/TimePoint 0/Channel 0/Data'][...]

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def _links(self):
        with h5py.File(self.src, 'r') as f:
            return {k: type(f.get(k, getlink=True)).__name__ for k in f}

    def test_sources_come_from_ccid_ori_path(self):
        self.assertEqual(ims_relink.source_paths(self.root), [self.src])

    def test_duplicate_sources_are_visited_once(self):
        root = os.path.join(self.d, 'proj2')
        _project(root, [self.src, self.src])
        self.assertEqual(ims_relink.source_paths(root), [self.src])

    def test_dry_run_writes_nothing(self):
        before = os.path.getsize(self.src)
        lines = []
        repaired, failed, skipped = ims_relink.patch(self.root, apply=False, log=lines.append)
        self.assertEqual((repaired, failed, skipped), (1, 0, 0))
        self.assertEqual(self._links()['DataSet'], 'SoftLink')
        self.assertEqual(os.path.getsize(self.src), before)
        self.assertTrue(any('would repair' in ln for ln in lines))

    def test_apply_hard_links_the_root_and_keeps_pixels(self):
        repaired, failed, skipped = ims_relink.patch(self.root, apply=True, log=lambda _: None)
        self.assertEqual((repaired, failed, skipped), (1, 0, 0))
        self.assertEqual(self._links(), {'DataSet': 'HardLink', 'DataSetInfo': 'HardLink',
                                         'Workflows': 'HardLink'})
        with h5py.File(self.src, 'r') as f:
            np.testing.assert_array_equal(
                f['DataSet/ResolutionLevel 0/TimePoint 0/Channel 0/Data'][...], self.pixels)
            # the workflow copy is still there — a hard link adds a name, it doesn't move the object
            self.assertIn('DataSet', f['Workflows/InitialImages'])

    def test_apply_is_idempotent(self):
        ims_relink.patch(self.root, apply=True, log=lambda _: None)
        repaired, failed, skipped = ims_relink.patch(self.root, apply=True, log=lambda _: None)
        self.assertEqual((repaired, failed, skipped), (0, 0, 1))

    def test_repair_rewrites_links_not_data(self):
        # The invariant is that nothing DATA-sized is written: a few link entries, bounded by one
        # HDF5 page. (Not zero — HDF5 only reuses the freed entries when there's a free block to
        # reuse, which a freshly-created fixture has none of. The real 5-6 GB files measured zero.)
        before = os.path.getsize(self.src)
        ims_relink.patch(self.root, apply=True, log=lambda _: None)
        self.assertLess(os.path.getsize(self.src) - before, 4096)

    @unittest.skipIf(os.getuid() == 0, 'root ignores the write bit')
    def test_readonly_source_fails_loudly_and_is_left_intact(self):
        os.chmod(self.src, stat.S_IRUSR)
        try:
            lines = []
            repaired, failed, _ = ims_relink.patch(self.root, apply=True, log=lines.append)
            self.assertEqual((repaired, failed), (0, 1))
            self.assertTrue(any('no write permission' in ln for ln in lines))
            self.assertEqual(self._links()['DataSet'], 'SoftLink')
        finally:
            os.chmod(self.src, stat.S_IRUSR | stat.S_IWUSR)

    def test_project_with_no_sources_reports_and_exits(self):
        empty = os.path.join(self.d, 'empty')
        os.makedirs(os.path.join(empty, '1'))
        lines = []
        self.assertEqual(ims_relink.patch(empty, log=lines.append), (0, 0, 0))
        self.assertTrue(any('No source images' in ln for ln in lines))

    def test_repair_is_proven_by_reading_through_the_new_link(self):
        lines = []
        ims_relink.patch(self.root, apply=True, log=lines.append)
        self.assertTrue(any('now reads (2, 4, 4) uint16' in ln for ln in lines))

    def test_file_without_the_standard_data_block_still_counts_as_repaired(self):
        # The verify read is best-effort: an Imaris file that resolves every link but doesn't have
        # `DataSet/ResolutionLevel 0/...` is repaired, not broken. Reporting the link edit as a
        # failure there would be a lie about a write that succeeded.
        odd = os.path.join(self.d, 'odd.ims')
        with h5py.File(odd, 'w') as f:
            f.create_group('Workflows/InitialImages/DataSet/Something')
            f.create_group('Workflows/InitialImages/DataSetInfo')
            for n in ('DataSet', 'DataSetInfo'):
                f[n] = h5py.SoftLink(f'/Workflows/InitialImages/{n}')
        root = os.path.join(self.d, 'proj3')
        _project(root, [odd])
        lines = []
        repaired, failed, _ = ims_relink.patch(root, apply=True, log=lines.append)
        self.assertEqual((repaired, failed), (1, 0))
        self.assertTrue(any('2 link(s) resolve' in ln for ln in lines))

    def test_progress_is_emitted_for_the_task_rail(self):
        # The patch streams over the task WS rail; without these the Settings progress bar never moves.
        lines = []
        ims_relink.patch(self.root, apply=False, log=lines.append)
        self.assertIn('[PROGRESS] 0/1', lines)
        self.assertIn('[PROGRESS] 1/1', lines)


if __name__ == '__main__':
    unittest.main()
