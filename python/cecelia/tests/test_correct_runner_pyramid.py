"""End-to-end test of the correction runner's WRITE path — `app/src/tasks/segment/correct_run.py`.

The corrected mask must come out with as many levels as the IMAGE, every level re-derived from the
edited level 0: a shallower mask has nothing to draw when the viewer zooms out, and a stale lower
level would still show a cell the user merged or removed.

Skipped when `app/` is absent (external `pip install cecelia` consumers).
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

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'segment' / 'correct_run.py')

_T, _Y, _X = 2, 16, 20


def _load_runner():
    spec = importlib.util.spec_from_file_location('correct_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml():
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{_T}" SizeC="1" SizeZ="1" SizeY="{_Y}" SizeX="{_X}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" Name="CH1" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class CorrectRunnerPyramidTest(unittest.TestCase):
    IMAGE_NSCALES = 3

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        omexml = ome_types.from_xml(_ome_xml())
        du = DimUtils(omexml, use_channel_axis=True)
        shape = (_T, 1, 1, _Y, _X)
        du.calc_image_dimensions(list(shape))
        self.im_path = os.path.join(self.dir, 'im.ome.zarr')
        g, lv0, pchunks = zarr_utils.open_multiscales_for_writing(
            self.im_path, shape, np.uint16, du, nscales=self.IMAGE_NSCALES)
        lv0[:] = 1
        zarr_utils.write_multiscale_pyramid(g, lv0, du, self.IMAGE_NSCALES, list(pchunks))
        ome_xml_utils.save_meta_in_zarr(self.im_path, omexml=omexml)
        self.du = du

        # Three cells, left / middle / right thirds of every frame.
        self.labels = np.zeros((_T, _Y, _X), dtype=np.uint32)
        self.labels[:, :, 0:6], self.labels[:, :, 7:13], self.labels[:, :, 14:20] = 1, 2, 3

    def _write_labels(self, nscales):
        path = os.path.join(self.dir, f'labels_{nscales}.zarr')
        g, lv0, chunks = zarr_utils.open_multiscales_for_writing(
            path, self.labels.shape, np.uint32, self.du, axes=['T', 'Y', 'X'],
            nscales=nscales, kind='labels')
        lv0[:] = self.labels
        zarr_utils.write_label_pyramid(g, lv0, ['T', 'Y', 'X'], nscales, chunks)
        return path

    def _merge_2_into_1_at_t0(self, labels_path):
        result = os.path.join(self.dir, 'result.json')
        self.runner.run(dict(labelsPath=labels_path, imPath=self.im_path, resultFile=result,
                             ops=[{'op': 'label.merge', 't': 0, 'ids': [2], 'into': 1}]))
        with open(result, encoding='utf-8') as fh:
            return json.load(fh)

    def _assert_pyramid_of_edit(self, labels_path):
        levels, _ = zarr_utils.open_as_zarr(labels_path, as_dask=False)
        self.assertEqual(len(levels), self.IMAGE_NSCALES)
        expect = self.labels.copy()
        expect[0][expect[0] == 2] = 1
        np.testing.assert_array_equal(np.asarray(levels[0][:]), expect)
        for lv in range(1, self.IMAGE_NSCALES):
            s = 2 ** lv
            np.testing.assert_array_equal(np.asarray(levels[lv][:]), expect[:, ::s, ::s],
                                          err_msg=f'level {lv} is not the edited level 0')

    def test_a_multi_level_mask_is_corrected_on_every_level(self):
        path = self._write_labels(self.IMAGE_NSCALES)
        res = self._merge_2_into_1_at_t0(path)
        self.assertEqual(res['perOpPixels'], [6 * _Y])
        self._assert_pyramid_of_edit(path)

    def test_a_single_level_mask_comes_out_with_the_image_s_levels(self):
        path = self._write_labels(1)
        self._merge_2_into_1_at_t0(path)
        self._assert_pyramid_of_edit(path)


if __name__ == '__main__':
    unittest.main()
