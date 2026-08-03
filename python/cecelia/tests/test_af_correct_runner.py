"""End-to-end test of the AF task RUNNER — `app/src/tasks/cleanupImages/af_correct_run.py`.

**Why this file exists.** `correction_utils` was well covered and every one of its 400+ tests passed,
while the runner that calls it threw `KeyError: 'clippedFrac'` on a real image — from a *log line*, and
only AFTER the corrected store had been fully written. The work was done and the task still failed.

The runner is executed by path (`run_py`), never imported, so nothing in the package suite touched it.
That is the same seam a napari-bridge dispatcher bug slipped through earlier: the unit under test was
right and the thing that calls it was not. So this loads it the way the launcher does and runs it
against a real (tiny) OME-ZARR, asserting the store, the QC sidecar and the log all survive.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path

import numpy as np
import ome_types
import zarr

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'cleanupImages' / 'af_correct_run.py')


def _load_runner():
    """Load the runner from its path, exactly as `run_py` does (it is not an importable module)."""
    spec = importlib.util.spec_from_file_location('af_correct_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    channels = ''.join(
        f'<Channel ID="Channel:0:{i}" Name="CH{i + 1}" SamplesPerPixel="1"/>' for i in range(size_c))
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeC="{size_c}" SizeZ="{size_z}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      {channels}
    </Pixels>
  </Image>
</OME>"""


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class AfCorrectRunnerTest(unittest.TestCase):
    SHAPE = dict(size_t=2, size_z=2, size_c=3, size_y=16, size_x=14)

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        omexml = ome_types.from_xml(_ome_xml(**self.SHAPE))
        du = DimUtils(omexml, use_channel_axis=True)

        # a plausible image: background pedestal + a bright blob per channel in a different place, so
        # the channels genuinely compete somewhere and a background threshold has something to find
        shape = [self.SHAPE['size_t'], self.SHAPE['size_c'], self.SHAPE['size_z'],
                 self.SHAPE['size_y'], self.SHAPE['size_x']]
        du.calc_image_dimensions(shape)
        rng = np.random.default_rng(4)
        data = np.full(shape, 30, dtype=np.uint16)
        data += rng.integers(0, 6, size=shape, dtype=np.uint16)
        c = du.dim_idx('C')
        for ch, (y0, x0) in enumerate([(2, 2), (4, 4), (6, 6)]):
            sl = [slice(None)] * len(shape)
            sl[c] = slice(ch, ch + 1)
            sl[du.dim_idx('Y')] = slice(y0, y0 + 6)
            sl[du.dim_idx('X')] = slice(x0, x0 + 6)
            data[tuple(sl)] += np.uint16(700 + 100 * ch)
        # a couple of saturated voxels in ch0, so `saturatedFrac` is exercised non-zero
        sat = [0] * len(shape)
        sat[c] = 0
        data[tuple(sat)] = np.iinfo(np.uint16).max

        self.in_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.in_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(self.in_path, omexml=omexml)
        self.du, self.data = du, data

    def _run(self, af_combinations, background_method='triangle'):
        out_path = os.path.join(self.dir, 'out.ome.zarr')
        qc_path = os.path.join(self.dir, 'af_output_stats.json')
        self.runner.run({
            'imPath': self.in_path,
            'imCorrectionPath': out_path,
            'afCombinations': af_combinations,
            'backgroundMethod': background_method,
            'qcOutPath': qc_path,
        })
        with open(qc_path, encoding='utf-8') as f:
            return out_path, json.load(f)

    def test_the_runner_completes_and_writes_a_store_plus_qc(self):
        """The regression this file was created for: it used to write the store and THEN throw."""
        out_path, qc = self._run({'0': {'competingChannels': [1, 2]},
                                  '1': {'competingChannels': [0, 2]}})

        # the store landed at its final path (staged_store only renames on success)
        self.assertTrue(os.path.isdir(out_path))
        out = zarr.open_group(out_path, mode='r')['0'][:]
        self.assertEqual(out.shape, self.data.shape)
        self.assertEqual(out.dtype, self.data.dtype)

        # QC carries exactly the keys af_correct.jl's `af_qc_findings` reads, for the corrected channels
        self.assertEqual(sorted(qc), ['0', '1'])
        for ch in ('0', '1'):
            for k in ('saturatedFrac', 'levelsUsed', 'levelsAvailable', 'background',
                      'competingBackgrounds', 'exponent'):
                self.assertIn(k, qc[ch], f'ch{ch} missing {k} — af_qc_findings reads it')
        # ...and NOT the ratio-era keys the log line used to subscript
        self.assertNotIn('clippedFrac', qc['0'])
        self.assertNotIn('ceiling', qc['0'])

    def test_a_channel_no_combination_covers_is_carried_through_untouched(self):
        out_path, qc = self._run({'0': {'competingChannels': [1]}})
        out = zarr.open_group(out_path, mode='r')['0'][:]
        c = self.du.dim_idx('C')
        for ch in (1, 2):                     # only channel 0 was corrected
            sl = [slice(None)] * out.ndim
            sl[c] = slice(ch, ch + 1)
            self.assertTrue(np.array_equal(out[tuple(sl)], self.data[tuple(sl)]),
                            f'ch{ch} was modified without a combination asking for it')
        self.assertEqual(sorted(qc), ['0'])

    def test_the_corrected_output_stays_in_input_counts(self):
        """The property the whole change rests on — no rescale, so nothing may brighten."""
        out_path, _ = self._run({'0': {'competingChannels': [1, 2]}})
        out = zarr.open_group(out_path, mode='r')['0'][:]
        c = self.du.dim_idx('C')
        sl = [slice(None)] * out.ndim
        sl[c] = slice(0, 1)
        self.assertTrue(np.all(out[tuple(sl)].astype(np.int64)
                               <= self.data[tuple(sl)].astype(np.int64)),
                        'a corrected voxel came out brighter than its input')

    def test_an_empty_combination_bag_still_completes(self):
        # nothing to correct: every channel is carried through and the QC sidecar is empty, not missing
        out_path, qc = self._run({})
        out = zarr.open_group(out_path, mode='r')['0'][:]
        self.assertTrue(np.array_equal(out, self.data))
        self.assertEqual(qc, {})


if __name__ == '__main__':
    unittest.main()
