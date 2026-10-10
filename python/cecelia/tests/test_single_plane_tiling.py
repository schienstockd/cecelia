"""A single-plane image (Z axis present, size 1) larger than one tile segments — issue #1551.

The store keeps a length-1 Z axis (`TCZYX`, Z=1) whenever the source had one, so `is_3D()` is False
while the in-RAM frame still carries the Z axis. The base used to hand each engine that `[C,1,Y,X]`
tile, every engine guessed 3D from `tile.ndim == 4`, and the 3D masks then met the base's 2D crop:
`(1,Y,X)[pad:-pad, pad:-pad]` crops the Z axis to nothing. One tile has no padding, so the crop was a
no-op and every test fixture here was either one tile or had its size-1 axes dropped before
`calc_image_dimensions` — which is why nothing caught it.

The contract now: the base decides dimensionality ONCE, from `is_3D()`, and drops the singleton Z at
read time, so an engine only ever sees `[C,Y,X]` for a single-plane image (and the label frame is 2D
all the way to the store write). Pinned here through the real `CellposeUtils.predict_slice` and the
real `CoastalUtils.predict_slice`, with only the model calls faked, at both stitch settings.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import os
import tempfile
import unittest

import numpy as np
import ome_types
import zarr

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.utils.cellpose_utils import CellposeUtils


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeZ="{size_z}" SizeC="{size_c}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      {''.join(f'<Channel ID="Channel:0:{c}" SamplesPerPixel="1"/>' for c in range(size_c))}
    </Pixels>
  </Image>
</OME>"""


def _dim_utils(t, z, c, y, x):
    """`TCZYX` with every axis KEPT, size-1 ones included — the layout the user's store had.

    Y != X so `calc_image_dimensions` cannot confuse the two when it matches sizes to axes."""
    du = DimUtils(ome_types.from_xml(_ome_xml(t, z, c, y, x)), use_channel_axis=True)
    du.calc_image_dimensions((t, c, z, y, x))
    assert du.im_dim_order == list('TCZYX'), du.im_dim_order
    return du


def _params(tmp, **model):
    mp = {'matchAs': 'base', 'cellChannels': [0], 'cellDiameter': 4}
    mp.update(model)
    return {
        'taskDir': tmp, 'outputValueName': 'seg',
        'blockSize': 12, 'overlap': 4,          # 36x40 → 3x4 tiles, so every tile but none is padded
        'labelOverlap': 0.1,                    # seam stitching on: it reads the padded masks too
        'minCellSize': 0, 'cellSizeMax': 0, 'labelExpansion': 0, 'labelErosion': 0,
        'clearTouchingBorder': False, 'clearDepth': False, 'normaliseToWhole': False,
        'models': {'0': mp},
    }


def _labels(tmp):
    return zarr.open_group(os.path.join(tmp, 'labels', 'seg.zarr'), mode='r')['0'][:]


class _FakeCellpose:
    """`CellposeModel.eval`, faked: one label over the whole input, and the call recorded.

    Mirrors the one behaviour of the real model the bug depended on: with `z_axis=0` and stitching,
    a single plane comes back SQUEEZED to `(Y, X)` — measured with cellpose 4.2.1.1 on the reported
    image, where it then failed in `split_z_gaps`.
    """

    def __init__(self):
        self.calls = []

    def eval(self, x, **kw):
        self.calls.append(kw)
        if isinstance(x, list):
            return [np.ones(np.asarray(p).shape[:2], np.uint32) for p in x], None, None
        arr = np.asarray(x)
        shape = arr.shape[:-1] if kw.get('channel_axis') in (-1, arr.ndim - 1) else arr.shape
        if kw.get('z_axis') is not None and shape[0] == 1:
            shape = shape[1:]
        return np.ones(shape, np.uint32), None, None


class CellposeSinglePlaneTest(unittest.TestCase):

    def _run(self, t, stitch):
        du = _dim_utils(t, 1, 2, 36, 40)
        im = np.random.default_rng(0).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        with tempfile.TemporaryDirectory() as tmp:
            seg = CellposeUtils(_params(tmp, model='cpsam_v2', stitchThreshold=stitch), du)
            model = _FakeCellpose()
            seg._model_cache['cpsam_v2'] = model
            counts = seg.predict_from_zarr([im])
            labels = _labels(tmp)
        return counts, labels, model

    def _check(self, t, stitch):
        counts, labels, model = self._run(t, stitch)
        self.assertEqual(labels.shape, (t, 1, 36, 40), 'the store keeps its length-1 Z axis')
        self.assertTrue((labels > 0).all(), 'every tile must have been written')
        # 3x4 tiles of one label each, joined into ONE per frame by the seam stitch — which reads
        # the padded masks, so it is a second consumer of the shape the crop used to get wrong
        self.assertEqual(counts['base'], t)
        for kw in model.calls:
            self.assertIsNone(kw.get('z_axis'), 'a single plane must take the 2D path')

    def test_stitch_off(self):
        self._check(1, 0.0)

    def test_stitch_on_the_task_default(self):
        self._check(1, 0.2)

    def test_single_plane_timeseries(self):
        self._check(3, 0.2)


class _Recorder(SegmentationUtils):
    """What the base hands an engine: the tile's shape, per call."""

    def predict_slice(self, tile, model_params, norm_params=None):
        self.seen = getattr(self, 'seen', []) + [tile.shape]
        return np.ones(tile.shape[-2:] if tile.ndim == 3 else tile.shape[1:], np.uint32)


class TileRankContractTest(unittest.TestCase):

    def _seen(self, z):
        du = _dim_utils(1, z, 2, 36, 40)
        im = np.zeros(tuple(du.im_dim), np.uint16)
        with tempfile.TemporaryDirectory() as tmp:
            seg = _Recorder(_params(tmp), du)
            seg.predict_from_zarr([im])
            labels = _labels(tmp)
        return {s for s in seg.seen}, labels

    def test_single_plane_is_cyx(self):
        shapes, labels = self._seen(1)
        self.assertEqual({len(s) for s in shapes}, {3}, f'expected [C,Y,X] tiles, got {shapes}')
        self.assertTrue((labels > 0).all())

    def test_a_stack_is_still_czyx(self):
        shapes, labels = self._seen(3)
        self.assertEqual({len(s) for s in shapes}, {4}, f'expected [C,Z,Y,X] tiles, got {shapes}')
        self.assertTrue((labels > 0).all())

    def test_a_rank_mismatch_fails_at_the_source(self):
        """An engine handing back the wrong rank fails with a message naming the contract, not as a
        broadcast error three calls later."""

        class _Wrong(SegmentationUtils):
            def predict_slice(self, tile, model_params, norm_params=None):
                return np.ones((1,) + tile.shape[-2:], np.uint32)     # 3D masks for a 2D image

        du = _dim_utils(1, 1, 2, 36, 40)
        with tempfile.TemporaryDirectory() as tmp:
            seg = _Wrong(_params(tmp), du)
            with self.assertRaisesRegex(ValueError, 'single-plane'):
                seg.predict_from_zarr([np.zeros(tuple(du.im_dim), np.uint16)])


class CoastalSinglePlaneTest(unittest.TestCase):
    """Coastal guessed 3D from `tile.ndim` the same way; its temporal window must be 2D too."""

    def _run(self, stitch):
        from cecelia.utils.coastal_utils import CoastalUtils

        t = 10                                   # >= the largest default temporal scale
        du = _dim_utils(t, 1, 1, 36, 40)
        im = np.random.default_rng(0).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        with tempfile.TemporaryDirectory() as tmp:
            params = _params(tmp, model='/nonexistent/model.pt', stitchThreshold=stitch)
            seg = CoastalUtils(params, du)
            windows = []

            class _Inference:
                def predict_frame(self, frame, metrics):
                    return None, np.ones(np.asarray(frame).shape, np.uint32), None

            def _metrics(window, center, scales, cumulative, **_):
                windows.append(np.asarray(window).shape)
                return np.asarray(window)[center], {'mag_1': np.zeros_like(window[center])}

            seg._get_inference = lambda _mp: _Inference()
            seg._flow_metrics = _metrics
            seg.predict_from_zarr([im])
            labels = _labels(tmp)
        return labels, windows

    def _check(self, stitch):
        labels, windows = self._run(stitch)
        self.assertEqual(labels.shape, (10, 1, 36, 40))
        self.assertTrue((labels > 0).all(), 'every tile of every frame must have been written')
        self.assertEqual({len(s) for s in windows}, {3}, f'expected [W,Y,X] windows, got {windows}')

    def test_stitch_off(self):
        self._check(0.0)

    def test_stitch_on(self):
        self._check(0.2)


if __name__ == '__main__':
    unittest.main()
