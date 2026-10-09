"""End-to-end test of the flip task RUNNER — `app/src/tasks/editImages/flip_run.py`.

A flip keeps the dims but MIRRORS the content, and the task touches no labels — so a segmentation
made on the source version does NOT line up on the flipped one. Pinned here because the handler's
comment and the param tip once claimed the opposite.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
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
           / 'app' / 'src' / 'tasks' / 'editImages' / 'flip_run.py')

T, C, Z, Y, X = 2, 1, 1, 6, 8


def _load_runner():
    spec = importlib.util.spec_from_file_location('flip_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml():
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{T}" SizeC="{C}" SizeZ="{Z}" SizeY="{Y}" SizeX="{X}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" Name="CH1" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class FlipRunnerTest(unittest.TestCase):

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        omexml = ome_types.from_xml(_ome_xml())
        du = DimUtils(omexml, use_channel_axis=True)
        shape = (T, C, Z, Y, X)
        du.calc_image_dimensions(list(shape))
        self.data = np.zeros(shape, dtype=np.uint16)
        self.data[:, 0, 0, 1:3, 1:3] = 1000          # one "cell" near the top-left corner
        self.in_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.in_path, shape, np.uint16, du, nscales=1)
        level0[:] = self.data
        ome_xml_utils.save_meta_in_zarr(self.in_path, omexml=omexml)

    def _flip(self, axis):
        out = os.path.join(self.dir, f'out_{axis}.ome.zarr')
        _load_runner().run(dict(imPath=self.in_path, imOutPath=out, axis=axis))
        return np.asarray(zarr_utils.open_as_zarr(out, as_dask=False)[0][0][:])

    def test_flip_mirrors_content_so_source_labels_no_longer_line_up(self):
        for axis, np_axis in (('X', 4), ('Y', 3)):
            out = self._flip(axis)
            self.assertEqual(out.shape, self.data.shape)                     # dims preserved
            np.testing.assert_array_equal(out, np.flip(self.data, axis=np_axis))
            # a label mask drawn on the SOURCE cell misses the cell on the flipped version
            source_label = self.data > 0
            self.assertEqual(int(out[source_label].sum()), 0, f'{axis}: source labels still line up')


if __name__ == '__main__':
    unittest.main()
