"""Real-cellpose smoke test: `predict_from_zarr` end to end on a small synthetic image.

Opt-in (CECELIA_CELLPOSE_SMOKE=1) because it loads real model weights. Its job is the cellpose-3
call path, which the unit tests only run against a stub: `.github/workflows/verify-cellpose-v3.yml`
runs it on macOS arm64 under the `cellpose-v3` env with CECELIA_EXPECT_CELLPOSE_MAJOR=3, so a
detection regression (v3 env taking the v4 path) or a v3 `eval()` kwarg cellpose 3 rejects fails
there. Locally it runs under the default env against Cellpose-SAM.

    CECELIA_CELLPOSE_SMOKE=1 pixi run -e cellpose-v3 python -m unittest cecelia.tests.test_cellpose_smoke
"""
import os
import shutil
import tempfile
import unittest
from unittest import mock

import numpy as np
import ome_types
import zarr

from cecelia.utils import cellpose_utils as cpu
from cecelia.utils.dim_utils import DimUtils

_ENABLED = os.environ.get('CECELIA_CELLPOSE_SMOKE') == '1'


def _ome_xml(size_z, size_y, size_x):
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="1" SizeZ="{size_z}" SizeC="1" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""


def _blobs(size_y, size_x):
    """Bright disks of radius 6 px (diameter 6 µm at 0.5 µm/px) on a noisy background."""
    rng = np.random.default_rng(0)
    im = rng.normal(200, 20, (size_y, size_x))
    yy, xx = np.mgrid[:size_y, :size_x]
    for cy in range(12, size_y - 8, 20):
        for cx in range(12, size_x - 8, 20):
            im[(yy - cy) ** 2 + (xx - cx) ** 2 <= 36] = 3000
    return np.clip(im, 0, 65535).astype(np.uint16)


@unittest.skipUnless(_ENABLED, 'set CECELIA_CELLPOSE_SMOKE=1 to run against real cellpose')
class CellposeSmokeTest(unittest.TestCase):

    def setUp(self):
        self.major = cpu._cellpose_major_version()
        self.model = 'cyto3' if self.major == 3 else 'cpsam_v2'
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_detected_major_matches_the_env(self):
        expected = os.environ.get('CECELIA_EXPECT_CELLPOSE_MAJOR')
        if expected is None:
            self.skipTest('CECELIA_EXPECT_CELLPOSE_MAJOR not set')
        self.assertEqual(self.major, int(expected))
        self.assertEqual(cpu._CELLPOSE_MAJOR, int(expected))

    def _run(self, size_z, block_size, stitch):
        # The full [T, C, Z, Y, X] shape a converted image has, singletons kept. Not square, so
        # DimUtils can't confuse Y with X.
        im = np.broadcast_to(_blobs(96, 112), (1, 1, size_z, 96, 112)).copy()
        du = DimUtils(ome_types.from_xml(_ome_xml(size_z, 96, 112)), use_channel_axis=True)
        du.calc_image_dimensions(im.shape)
        params = {
            'taskDir': self.tmp, 'outputValueName': 'smoke',
            'blockSize': block_size, 'overlap': 8, 'labelOverlap': 0.1,
            'matchThreshold': 0.1, 'removeUnmatched': False,
            'minCellSize': 0, 'cellSizeMax': 0, 'labelExpansion': 0, 'labelErosion': 0,
            'clearTouchingBorder': False, 'clearDepth': False, 'normaliseToWhole': False,
            'models': {'0': {'matchAs': 'base', 'model': self.model, 'cellChannels': [0],
                             'cellDiameter': 6, 'stitchThreshold': stitch}},
        }
        seg = cpu.CellposeUtils(params, du)

        from cellpose import models
        real_eval = models.CellposeModel.eval
        with mock.patch.object(models.CellposeModel, 'eval', autospec=True,
                               side_effect=real_eval) as spy:
            counts = seg.predict_from_zarr([im])

        # The branch actually taken: only the v3 call sends `channels=`.
        self.assertTrue(spy.called)
        for call in spy.call_args_list:
            self.assertEqual('channels' in call.kwargs, self.major == 3, call.kwargs.keys())

        labels = zarr.open_group(os.path.join(self.tmp, 'labels', 'smoke.zarr'), mode='r')['0'][:]
        self.assertGreater(counts['base'], 0)
        self.assertGreater(int(labels.max()), 0)
        return labels

    def test_2d_single_tile(self):
        # One tile: a single-plane image over more than one tile crashes in the crop (#1551). Make
        # this multi-tile when that lands.
        self._run(size_z=1, block_size=128, stitch=0.0)

    def test_3d_tiled_stitched(self):
        # Z stack, 2×2 tiles, Z stitching: the v3 `z_axis=0, do_3D=False, stitch_threshold` call.
        labels = self._run(size_z=3, block_size=64, stitch=0.2)
        self.assertEqual(labels.shape[-3:], (3, 96, 112))


if __name__ == '__main__':
    unittest.main()
