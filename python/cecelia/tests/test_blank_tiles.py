"""A tile with nothing above the intensity window's low end is skipped, losslessly.

The engines map the window's low end to 0 and clip, so such a tile reaches the model as all zeros;
cellpose finds nothing in zeros (checked on `cpsam_v2`, 2D and stitched 3D). On a tile scan those are
the tiles outside the tissue. The skip must never fire on a tile with any signal, on an engine that
has not opted in, or without a known window. Run with `pixi run test-py`.
"""
import tempfile
import unittest

import numpy as np
import ome_types

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml

SIZES = (1, 1, 2, 48, 64)          # T,Z,C,Y,X — two channels so the channel axis is kept
SHAPE = (2, 48, 64)


def _image(du):
    """Left half empty, right half a ramp of real signal (tiles are 16 px: x 0–31 blank)."""
    im = np.zeros(tuple(du.im_dim), np.uint16)
    im[0, :, 32:] = np.arange(1, 48 * 32 + 1, dtype=np.uint16).reshape(48, 32) + 100
    return im


class _Engine(SegmentationUtils):
    """Labels every pixel of every tile it is ASKED to segment, so a skipped tile shows as zeros."""

    BLANK_IS_EMPTY = True

    def _model_channels(self, model_params):
        return list(model_params['cellChannels'])

    def _blank_is_empty(self, model_params):
        return self.BLANK_IS_EMPTY

    def predict_slice(self, tile, model_params, norm_params=None):
        self.calls = getattr(self, 'calls', 0) + 1
        return np.ones(tile.shape[1:], np.uint32)


class _NotOptedIn(_Engine):
    BLANK_IS_EMPTY = False


def _run(cls, image=None):
    du = DimUtils(ome_types.from_xml(_ome_xml(*SIZES)), use_channel_axis=True)
    du.calc_image_dimensions(SHAPE)
    with tempfile.TemporaryDirectory() as tmp:
        seg = cls({'taskDir': tmp, 'outputValueName': 'probe', 'blockSize': 16, 'overlap': 0,
                   'labelOverlap': 0, 'normaliseToWhole': False,
                   'models': {'0': {'matchAs': 'base', 'cellChannels': [0], 'normalise': 99.0}}}, du)
        seg.predict_from_zarr([_image(du) if image is None else image(du)])
    return seg


class BlankTileTest(unittest.TestCase):

    def test_blank_tiles_are_not_segmented(self):
        seg = _run(_Engine)
        # 3 x 4 tiles of 16 px; the 3 x 2 in the empty left half are skipped
        self.assertEqual(seg.calls, 6)

    def test_an_engine_that_has_not_opted_in_segments_everything(self):
        self.assertEqual(_run(_NotOptedIn).calls, 12)

    def test_one_pixel_of_signal_keeps_the_tile(self):
        def image(du):
            im = _image(du)
            im[0, 5, 5] = 60000                       # inside the first (otherwise blank) tile
            return im
        self.assertEqual(_run(_Engine, image).calls, 7)

    def test_no_window_no_skip(self):
        seg = _Engine({'taskDir': '.', 'models': {}}, None)
        tile = np.zeros((1, 8, 8), np.uint16)
        mp = {'cellChannels': [0]}
        self.assertFalse(seg._tile_is_blank(tile, mp, None))
        self.assertFalse(seg._tile_is_blank(tile, mp, {1: (5.0, 9.0)}))   # not this channel's
        self.assertTrue(seg._tile_is_blank(tile, mp, {0: (5.0, 9.0)}))

    def test_cellpose_does_not_opt_in_when_inverting(self):
        from cecelia.utils.cellpose_utils import CellposeUtils
        self.assertTrue(CellposeUtils._blank_is_empty(None, {}))
        self.assertFalse(CellposeUtils._blank_is_empty(None, {'invert': True}))


if __name__ == '__main__':
    unittest.main()
