"""A last timepoint the acquisition never finished: detection (`intensity_utils.partial_last_timepoint`),
the import probe that reports it (`saturation_run.py`) and the trim that drops it
(`zarr_utils.trim_timepoints` via `trim_timepoints_run.py`).

Bio-Formats keeps such a timepoint and fills the z-planes the microscope never wrote with zeros. The
end-to-end tests reproduce that on COPIES of the committed bioformats2raw stores (test-data/README.md),
by zeroing the deepest planes of the last timepoint — the same pattern bioformats2raw 0.12.1 produced
from a real stopped-early OIR (z14-z25 of 26 empty in every channel of the last of 20 timepoints).
"""
import importlib.util
import json
import os
import shutil
import tempfile
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils

_IMPORT = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'app', 'src', 'tasks', 'importImages')
_ROOT = os.environ.get('CECELIA_TEST_DATA') or os.path.join(
    os.path.dirname(__file__), '..', '..', '..', 'test-data', 'projects')
_V2 = os.path.normpath(os.path.join(_ROOT, 'ZARRFMT', '0', 'ZV2img', 'ccidImage.ome.zarr'))
_V3 = os.path.normpath(os.path.join(_ROOT, 'ZARRFMT', '0', 'ZV3img', 'ccidImage.ome.zarr'))


def _runner(name):
    spec = importlib.util.spec_from_file_location(name, os.path.join(_IMPORT, f'{name}.py'))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _occ(rows):
    """`[T, Z]` occupancy from strings, one per timepoint: '.' = plane has data, '0' = empty."""
    return np.array([[c == '.' for c in r] for r in rows], dtype=bool)


class DetectorTest(unittest.TestCase):

    def test_a_stopped_volume_is_reported(self):
        r = iu.partial_last_timepoint(_occ(['......', '......', '...000']))
        self.assertEqual({'t': 2, 'emptyZ': [3, 4, 5], 'sizeZ': 6}, r)

    def test_a_complete_series_is_not(self):
        self.assertIsNone(iu.partial_last_timepoint(_occ(['....', '....'])))

    def test_planes_blank_in_every_timepoint_are_source_padding(self):
        # blank top and bottom planes throughout (a crop or registration done before import): not a stop
        self.assertIsNone(iu.partial_last_timepoint(_occ(['00...00000', '00...00000', '00...00000'])))
        self.assertIsNone(iu.partial_last_timepoint(_occ(['000.....', '000.....'])))

    def test_padding_does_not_hide_a_real_stop(self):
        # bottom 2 planes padded throughout; the last timepoint also lost the 2 real planes above them
        r = iu.partial_last_timepoint(_occ(['....00', '....00', '..0000']))
        self.assertEqual([2, 3], r['emptyZ'])

    def test_a_gap_that_is_not_a_tail_is_not_a_stop(self):
        self.assertIsNone(iu.partial_last_timepoint(_occ(['.....', '.....', '..0..'])))
        self.assertIsNone(iu.partial_last_timepoint(_occ(['.....', '.....', '00...'])))

    def test_a_blank_last_frame_of_a_2d_timelapse(self):
        self.assertEqual({'t': 2, 'emptyZ': [0], 'sizeZ': 1},
                         iu.partial_last_timepoint(_occ(['.', '.', '0'])))

    def test_one_timepoint_is_never_partial(self):
        self.assertIsNone(iu.partial_last_timepoint(_occ(['..00'])))


class EndToEndTest(unittest.TestCase):
    """Probe → trim on copies of the real bioformats2raw stores (T=3, C=4, Z=3)."""

    def setUp(self):
        for p in (_V2, _V3):
            if not os.path.isdir(p):
                self.skipTest(f'fixture missing: {p} (restore with `git checkout -- test-data`)')
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)

    def _store(self, src, empty_last_z=()):
        dst = os.path.join(self.tmp, os.path.basename(os.path.dirname(src)) + '.ome.zarr')
        shutil.copytree(src, dst)
        if empty_last_z:
            lvl = zarr_utils.open_as_zarr(dst, mode='r+')[0][0]
            for z in empty_last_z:
                lvl[-1, :, z] = 0
        return dst

    def _probe(self, store):
        res = os.path.join(self.tmp, 'sat.json')
        _runner('saturation_run').run({'imPath': store, 'resultPath': res})
        with open(res, encoding='utf-8') as f:
            return json.load(f)

    def test_the_probe_reports_a_stopped_volume_and_the_trim_drops_it(self):
        for src in (_V2, _V3):
            with self.subTest(store=os.path.basename(os.path.dirname(src))):
                store = self._store(src, empty_last_z=(1, 2))
                result = self._probe(store)
                self.assertEqual({'t': 2, 'emptyZ': [1, 2], 'sizeZ': 3}, result['partialTimepoint'])
                self.assertEqual(4, len(result['channels']))     # the saturation output is unchanged
                for ch in result['channels']:                      # the dropped frame is not profiled
                    self.assertEqual(2, len(ch['profile']['byT']['p50']))

                before = np.asarray(zarr_utils.open_as_zarr(store)[0][0][:2])
                out = os.path.join(self.tmp, 'trim.json')
                _runner('trim_timepoints_run').run({'imPath': store, 'sizeT': 2, 'resultPath': out})
                with open(out, encoding='utf-8') as f:
                    self.assertIn('levels', json.load(f))
                levels = zarr_utils.open_as_zarr(store)[0]
                for lvl in levels:
                    self.assertEqual(2, lvl.shape[0])
                np.testing.assert_array_equal(before, np.asarray(levels[0][:]))   # kept frames untouched

    def test_the_trim_leaves_no_chunk_directories_behind(self):
        store = self._store(_V2, empty_last_z=(2,))
        zarr_utils.trim_timepoints(store, 2)
        base = zarr_utils.series_base(store)
        # bioformats2raw nests chunk keys by T first: `<level>/<t>/...`
        self.assertFalse(os.path.exists(os.path.join(base, '0', '2')))

    def test_a_complete_store_reports_nothing(self):
        result = self._probe(self._store(_V2))
        self.assertNotIn('partialTimepoint', result)
        self.assertEqual(3, len(result['channels'][0]['profile']['byT']['p50']))

    def test_the_trim_refuses_to_grow_or_empty_a_store(self):
        store = self._store(_V2)
        for bad in (0, 4):
            with self.assertRaises(ValueError):
                zarr_utils.trim_timepoints(store, bad)
        self.assertEqual(3, zarr_utils.open_as_zarr(store)[0][0].shape[0])

    def test_the_ome_xml_still_describes_the_source_until_julia_updates_it(self):
        # the trim is pixels only; `update_ome_xml_pixels!` owns SizeT (pinned in app/test)
        store = self._store(_V2, empty_last_z=(2,))
        zarr_utils.trim_timepoints(store, 2)
        self.assertEqual(3, ome_xml_utils.load_ome_xml(store).images[0].pixels.size_t)


if __name__ == '__main__':
    unittest.main()
