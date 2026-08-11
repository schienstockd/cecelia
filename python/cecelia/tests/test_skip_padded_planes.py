"""Segmentation must not spend GPU time on the z planes a drift correction padded in.

The whole z-stack goes to cellpose in ONE call (it stitches across z internally), so padding is not
free: 3-64% of the planes across the movies on this machine, worst 8 valid in a 22-plane canvas.

This tests the WIRING, not the arithmetic — `test_valid_z_span` covers the span rule. What can go
wrong here is an axis mix-up: narrowing the wrong array, sizing the frame buffer wrong, or writing
the labels back at the wrong z offset. So it asserts what the model actually SAW and where the labels
LANDED, with a stub in place of cellpose.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import dask.array as da
import numpy as np
import ome_types
import zarr

import cecelia.utils.zarr_utils as zu
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils

T, C, Z, Y, X = 2, 1, 10, 16, 16
Z0, Z1 = 3, 7          # the only planes holding data

_OME = f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{T}" SizeC="{C}" SizeZ="{Z}" SizeY="{Y}" SizeX="{X}"
            PhysicalSizeX="1" PhysicalSizeY="1" PhysicalSizeZ="1"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""


class _StubSeg(SegmentationUtils):
    """Labels every voxel it is given, and records the z-depth of each tile it saw."""

    def predict_slice(self, tile, model_params, norm_params=None):
        zyx = tile.shape[-3:]
        self.seen_depths = getattr(self, 'seen_depths', [])
        self.seen_depths.append(zyx[0])
        return np.ones(zyx, dtype=np.uint32)


class SkipPaddedPlanesTest(unittest.TestCase):

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.du = DimUtils(ome_types.from_xml(_OME), use_channel_axis=True)
        self.du.calc_image_dimensions((T, C, Z, Y, X))
        self.arr = np.zeros((T, C, Z, Y, X), dtype=np.uint16)
        self.arr[:, :, Z0:Z1] = 1000                      # data only inside the box

        self.im_path = os.path.join(self.dir, 'img.ome.zarr')
        zu.create_multiscales(da.from_array(self.arr, chunks=(1, 1, 1, Y, X)),
                              self.im_path, dim_utils=self.du, nscales=1)

    def _run(self, with_box, **over):
        if with_box:
            zu.write_valid_box(self.im_path, ['Z'], {t: {'Z': (Z0, Z1)} for t in range(T)})
        params = {
            'taskDir': self.dir, 'outputValueName': 'stub', 'imPath': self.im_path,
            'blockSize': 64, 'overlap': 0, 'labelOverlap': 0,
            'matchThreshold': 0.1, 'removeUnmatched': False,
            'minCellSize': 0, 'cellSizeMax': 0, 'labelExpansion': 0, 'labelErosion': 0,
            'clearTouchingBorder': False, 'clearDepth': False, 'normaliseToWhole': False,
            'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}},
        }
        params.update(over)
        seg = _StubSeg(params, self.du)
        seg.predict_from_zarr([self.arr])
        out = zarr.open_group(os.path.join(self.dir, 'labels', 'stub.zarr'), mode='r')['0'][:]
        return seg, out

    def test_the_model_only_sees_the_valid_planes(self):
        seg, out = self._run(with_box=True)
        self.assertTrue(seg.seen_depths, 'stub was never called')
        self.assertEqual(set(seg.seen_depths), {Z1 - Z0},
                         f'cellpose was handed {set(seg.seen_depths)} z planes, expected {Z1 - Z0}')

    def test_labels_land_at_the_right_z_and_the_padding_stays_empty(self):
        _, out = self._run(with_box=True)
        self.assertEqual(out.shape, (T, Z, Y, X), 'the store must keep its FULL shape, not be cropped')
        z_axis = 1
        for z in range(Z):
            plane = out[:, z]
            if Z0 <= z < Z1:
                self.assertTrue((plane > 0).all(), f'z={z} is inside the box but was not segmented')
            else:
                self.assertFalse(plane.any(), f'z={z} is padding but carries labels')

    def test_the_label_store_records_the_span_it_segmented(self):
        """Outside the span the labels are zero because nothing ran, not because nothing was found."""
        _, _ = self._run(with_box=True)
        box = zu.read_valid_box(os.path.join(self.dir, 'labels', 'stub.zarr'), timepoint=0)
        self.assertIsNotNone(box)
        self.assertEqual(box['Z'], (Z0, Z1))

    def test_without_a_box_nothing_changes(self):
        """Most stores never padded — that path must be byte-identical to before."""
        seg, out = self._run(with_box=False)
        self.assertEqual(set(seg.seen_depths), {Z})
        self.assertTrue((out > 0).all())
        self.assertIsNone(zu.read_valid_box(os.path.join(self.dir, 'labels', 'stub.zarr')))


class ClearDepthMeetsTheSkipTest(SkipPaddedPlanesTest):
    """`clearDepth` clears labels touching the FIRST and LAST z slice of the array it is given.

    Before the skip, that array was the whole padded canvas, so those slices were padding — all zero,
    no labels, nothing cleared. `clearDepth` was therefore a silent NO-OP on every drift-corrected
    image. With the skip, the array is the valid span, so the faces it clears are the real top and
    bottom of the ACQUIRED stack — which is what the option means.

    So this is a deliberate behaviour change, and it changes results for anyone running `clearDepth`
    on drift-corrected data: cells the padding used to shield are now cleared. Pinned here so it is
    visible rather than discovered.
    """

    def test_clear_depth_acts_on_the_real_stack_edges_not_the_padding(self):
        _, out = self._run(with_box=True, clearDepth=True)
        # The stub labels every voxel it is handed, so every label touches both span faces and all
        # of them are cleared — the point is that clearing HAPPENS, at z0/z1-1 rather than 0/n_z-1.
        self.assertFalse(out.any(),
                         'clearDepth had no effect — it is still clearing the padding faces')

    def test_without_the_skip_clear_depth_still_reaches_the_canvas_edges(self):
        """The unboxed path is unchanged: the array edge is the canvas edge, as before."""
        _, out = self._run(with_box=False, clearDepth=True)
        self.assertFalse(out.any())
