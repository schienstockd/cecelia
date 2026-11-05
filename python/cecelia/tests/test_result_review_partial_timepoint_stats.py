"""RESULT-REVIEW: result-24d1d36e

The import's clipping/sparsity stats (`meta.saturation`) were taken before the incomplete last
timepoint was dropped, so they counted its black planes as zero voxels: `zeroFrac`/`signalFrac` (and
the photon-limited QC and cohort metrics built on them) described voxels no longer in the store.
The probe now leaves that timepoint out, so its stats match a fresh pass over the trimmed store.
"""
import importlib.util
import json
import os
import shutil
import tempfile
import unittest

import cecelia.utils.intensity_utils as iu
import cecelia.utils.zarr_utils as zarr_utils

_RUNNERS = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'app', 'src', 'tasks', 'importImages')
_ROOT = os.environ.get('CECELIA_TEST_DATA') or os.path.join(
    os.path.dirname(__file__), '..', '..', '..', 'test-data', 'projects')
_V2 = os.path.normpath(os.path.join(_ROOT, 'ZARRFMT', '0', 'ZV2img', 'ccidImage.ome.zarr'))


def _runner(name):
    spec = importlib.util.spec_from_file_location(name, os.path.join(_RUNNERS, f'{name}.py'))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class StatsDescribeTheTrimmedStoreTest(unittest.TestCase):

    def setUp(self):
        if not os.path.isdir(_V2):
            self.skipTest(f'fixture missing: {_V2}')
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)

    def test_reported_stats_match_the_trimmed_store(self):
        store = os.path.join(self.tmp, 'img.ome.zarr')
        shutil.copytree(_V2, store)
        lvl = zarr_utils.open_as_zarr(store, mode='r+')[0][0]
        lvl[-1, :, 1] = 0
        lvl[-1, :, 2] = 0

        res = os.path.join(self.tmp, 'sat.json')
        _runner('saturation_run').run({'imPath': store, 'resultPath': res})
        with open(res, encoding='utf-8') as f:
            reported = json.load(f)
        self.assertIn('partialTimepoint', reported)

        zarr_utils.trim_timepoints(store, 2)
        trimmed = zarr_utils.open_as_zarr(store)[0][0]
        fresh = [iu.saturation_stats(h) for h in iu.channel_histograms(trimmed, 1)]
        for got, want in zip(reported['channels'], fresh):
            for key in ('zeroFrac', 'signalFrac', 'clippedSignalFrac', 'topValue', 'topCount'):
                self.assertAlmostEqual(want[key], got[key], places=9, msg=f'ch{got["index"]} {key}')


if __name__ == '__main__':
    unittest.main()
