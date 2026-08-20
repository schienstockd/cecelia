"""Segmentation must not spend GPU time on the z planes a drift correction padded in.

The whole z-stack goes to cellpose in ONE call (it stitches across z internally), so padding is not
free: 3-56% of the planes across the movies on this machine, worst 8 valid in an 18-plane canvas.

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
#: XY box, deliberately ASYMMETRIC and different from each other, so a y/x swap or a shared offset
#: cannot pass. A drift correction pads XY exactly as it pads Z — worth 30% of a cellpose pass on a
#: heavily drifted movie (WIaUjL/p6t4mC: a 512x512 frame inside a 605x617 canvas).
Y0, Y1 = 4, 12
X0, X1 = 5, 14

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
        self.seen_shapes = getattr(self, 'seen_shapes', [])
        self.seen_shapes.append(tuple(zyx))
        return np.ones(zyx, dtype=np.uint32)


class _TemporalStubSeg(SegmentationUtils):
    """A TEMPORAL subclass, shaped like `CoastalUtils`: the mask comes from the temporal WINDOW.

    That is the contract that matters here — coastal's `predict_slice` documents the tile as
    "present for the base's contract; the pixels used come from `context[context_index]`" — so a
    window the base forgot to narrow is not a cosmetic mismatch, it decides the output's depth.
    """
    TEMPORAL_RADIUS = 1

    def predict_slice(self, tile, model_params, norm_params=None,
                      context=None, context_index=None, context_id=None,
                      context_channels=None):
        self.seen = getattr(self, 'seen', [])
        self.seen.append((tile.shape[-3], context[context_index].shape[-3], context.shape[0],
                          np.array_equal(context[context_index], tile)))
        return np.ones(context[context_index].shape[-3:], dtype=np.uint32)


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

    #: What `with_box=True` writes. Overridden by the XY subclass so one fixture serves both.
    BOX = {'Z': (Z0, Z1)}

    def _run(self, with_box, cls=_StubSeg, **over):
        if with_box:
            zu.write_valid_box(self.im_path, list(self.BOX),
                               {t: dict(self.BOX) for t in range(T)})
        params = {
            'taskDir': self.dir, 'outputValueName': 'stub', 'imPath': self.im_path,
            'blockSize': 64, 'overlap': 0, 'labelOverlap': 0,
            'matchThreshold': 0.1, 'removeUnmatched': False,
            'minCellSize': 0, 'cellSizeMax': 0, 'labelExpansion': 0, 'labelErosion': 0,
            'clearTouchingBorder': False, 'clearDepth': False, 'normaliseToWhole': False,
            'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}},
        }
        params.update(over)
        seg = cls(params, self.du)
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
        # the in-plane region this frame actually segmented — the whole plane unless the subclass
        # also narrows XY, so one assertion serves both the z-only and the three-axis case
        (y0, y1), (x0, x1) = self.BOX.get('Y', (0, Y)), self.BOX.get('X', (0, X))
        for z in range(Z):
            if Z0 <= z < Z1:
                self.assertTrue((out[:, z, y0:y1, x0:x1] > 0).all(),
                                f'z={z} is inside the box but was not segmented')
            else:
                self.assertFalse(out[:, z].any(), f'z={z} is padding but carries labels')

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


class TemporalWindowMatchesTheTileTest(SkipPaddedPlanesTest):
    """The skip belongs to the BASE, so it must narrow everything it hands one `predict_slice` call.

    The tile comes from the timepoint already in RAM; the temporal window is read separately, from
    the full store, because it needs OTHER timepoints. So the narrowing has to be applied twice, and
    it was applied once — the tile arrived 4 planes deep and the window 10, which a subclass that
    predicts from the window turns into a mask of the wrong depth. Not silent: the write into the
    frame buffer then raises `operands could not be broadcast together`.

    `test_segmentation_streaming.TemporalContextTest` already asserts `context[context_index]` IS
    the tile — the same invariant, on an image with no valid box, which is why the skip could break
    it unnoticed. This is that assertion with a box in play.
    """

    def test_the_window_is_narrowed_with_the_tile(self):
        seg, out = self._run(with_box=True, cls=_TemporalStubSeg)
        self.assertTrue(seg.seen, 'stub was never called')
        for tile_z, ctx_z, w, same in seg.seen:
            self.assertEqual(tile_z, Z1 - Z0, 'the tile was not narrowed')
            self.assertEqual(ctx_z, tile_z,
                             f'the temporal window is {ctx_z} planes deep, the tile {tile_z}')
            self.assertTrue(same, 'context[context_index] is no longer the tile')
            self.assertGreater(w, 1, 'no temporal window was built at all')
        # and the labels still land where the non-temporal path puts them
        for z in range(Z):
            self.assertEqual(bool(out[:, z].any()), Z0 <= z < Z1, f'z={z}')

    def test_without_a_box_the_window_is_the_whole_stack(self):
        seg, _ = self._run(with_box=False, cls=_TemporalStubSeg)
        self.assertTrue(seg.seen)
        for tile_z, ctx_z, _, same in seg.seen:
            self.assertEqual((tile_z, ctx_z), (Z, Z))
            self.assertTrue(same)


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


class SkipPaddedXYTest(SkipPaddedPlanesTest):
    """The same skip, on Y and X — a drift correction pads those too.

    Whether it is worth anything is per-image and was measured, not assumed: on a 5.8 px-drift movie
    the XY padding is 0.4% of the frame, while on a 139.9 px-drift one the canvas is 605x617 around a
    512x512 frame and ~30% of every cellpose pass is padding (zolIMa/Dml3RG vs WIaUjL/p6t4mC).

    Inherits the whole z suite, re-run with an XY box in play — so the z behaviour has to survive
    narrowing on three axes at once, which is the case that actually ships.

    Unlike the z skip, this one is NOT output-preserving: cellpose sees a differently-sized image, so
    its internal tiling lands differently and masks near the data edge can shift. That is a real
    change to results, recorded here and in `predict_from_zarr`.
    """

    BOX = {'Z': (Z0, Z1), 'Y': (Y0, Y1), 'X': (X0, X1)}

    def test_the_model_only_sees_the_valid_region(self):
        seg, _ = self._run(with_box=True)
        self.assertTrue(seg.seen_shapes, 'stub was never called')
        self.assertEqual(set(seg.seen_shapes), {(Z1 - Z0, Y1 - Y0, X1 - X0)},
                         f'cellpose was handed {set(seg.seen_shapes)}, expected one '
                         f'{(Z1 - Z0, Y1 - Y0, X1 - X0)} region')

    def test_labels_land_at_the_right_yx_and_the_padding_stays_empty(self):
        _, out = self._run(with_box=True)
        self.assertEqual(out.shape, (T, Z, Y, X), 'the store must keep its FULL shape')
        for y in range(Y):
            for x in range(X):
                inside = (Y0 <= y < Y1) and (X0 <= x < X1)
                col = out[:, Z0:Z1, y, x]
                self.assertEqual(bool(col.any()), inside,
                                 f'(y={y}, x={x}) inside={inside} but labels={bool(col.any())}')

    def test_the_label_store_records_every_axis_it_narrowed(self):
        _, _ = self._run(with_box=True)
        box = zu.read_valid_box(os.path.join(self.dir, 'labels', 'stub.zarr'), timepoint=0)
        self.assertIsNotNone(box)
        self.assertEqual((box['Z'], box['Y'], box['X']), ((Z0, Z1), (Y0, Y1), (X0, X1)))


class TemporalWindowMatchesTheTileInXYTest(TemporalWindowMatchesTheTileTest):
    """The window is read from the FULL store, so an XY narrowing has to be added back to its index.

    Exactly the bug the z skip already had once, one axis over: `read_yx` addresses the narrowed
    frame, so passing it straight to the full store reads the wrong part of the image — silently, and
    only when XY is narrowed. `context[context_index] IS the tile` is what catches it, which is why
    this class exists rather than a new assertion.
    """

    BOX = {'Z': (Z0, Z1), 'Y': (Y0, Y1), 'X': (X0, X1)}


class ClearTouchingBorderMeetsTheXYSkipTest(SkipPaddedPlanesTest):
    """`clearTouchingBorder` changes meaning under the XY skip, exactly as `clearDepth` did under z.

    Before, the array's Y/X edges were drift padding — all zero, so no label ever touched them and
    the option was a silent NO-OP on drift-corrected data. Now the edges are the real acquired frame
    boundary, which is what the option means.

    A deliberate behaviour change, and it changes results for anyone running `clearTouchingBorder`
    on drift-corrected data. Pinned so it is visible rather than discovered.
    """

    BOX = {'Z': (Z0, Z1), 'Y': (Y0, Y1), 'X': (X0, X1)}

    def test_clear_touching_border_acts_on_the_real_frame_edge_not_the_padding(self):
        _, out = self._run(with_box=True, clearTouchingBorder=True)
        # the stub labels every voxel it is handed, so every label touches the span edge and all are
        # cleared — the point is that clearing HAPPENS, at the data boundary rather than the canvas.
        self.assertFalse(out.any(),
                         'clearTouchingBorder had no effect — still clearing the padding edges')

    def test_without_a_box_it_still_clears_at_the_canvas_edge(self):
        _, out = self._run(with_box=False, clearTouchingBorder=True)
        self.assertFalse(out.any())
