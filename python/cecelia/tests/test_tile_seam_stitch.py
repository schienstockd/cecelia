"""A cell cut by a tile seam ends up with ONE id (`_collect_seam_strips` + `_stitch_tile_seams`).

Each tile writes only its own region, so a cell crossing a seam is written as two ids. The stitch
matches the two tiles' predictions of the overlap band both of them read. The old stitch compared
the WRITTEN labels instead, which never share a pixel across tiles, so it never joined anything.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import tempfile
import unittest

import numpy as np
import ome_types
import zarr
from scipy import ndimage

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml

# Ground-truth cells as intensity blobs on a 40x40 frame, tiled at 16 px: seams at 16 and 32 on
# both axes. Disjoint, so connected components of a tile ARE its cells.
_CELLS = {
    'inside': (slice(3, 8), slice(3, 8)),       # touches no seam
    'y_seam': (slice(13, 20), slice(5, 10)),    # crosses y=16
    'x_seam': (slice(22, 27), slice(13, 19)),   # crosses x=16
    'corner': (slice(29, 35), slice(29, 35)),   # crosses y=32 AND x=32: four tiles
    'long':   (slice(2, 6), slice(12, 37)),     # crosses x=16 and x=32: three tiles
}


class _ComponentSeg(SegmentationUtils):
    """Labels each tile's connected components with fresh per-tile ids, like a real segmenter."""

    def predict_slice(self, tile, model_params, norm_params=None):
        return ndimage.label(tile[0] > 0)[0].astype(np.uint32)


def _segment(tmp, label_overlap, overlap=4):
    sizes, arr_shape = (2, 1, 2, 40, 40), (2, 2, 40, 40)     # T,Z,C,Y,X; both frames the same
    du = DimUtils(ome_types.from_xml(_ome_xml(*sizes)), use_channel_axis=True)
    du.calc_image_dimensions(arr_shape)
    im = np.zeros(tuple(du.im_dim), dtype=np.uint16)
    for sl in _CELLS.values():
        im[:, 0][(Ellipsis,) + sl] = 1000
    seg = _ComponentSeg({
        'taskDir': tmp, 'outputValueName': 'seam',
        'blockSize': 16, 'overlap': overlap, 'labelOverlap': label_overlap,
        'normaliseToWhole': False,
        'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}},
    }, du)
    seg.predict_from_zarr([im])
    return np.squeeze(zarr.open_group(f'{tmp}/labels/seam.zarr', mode='r')['0'][:])[0]


def _ids_per_cell(labels):
    return {name: set(np.unique(labels[sl])) - {0} for name, sl in _CELLS.items()}


class TestSeamStitchThroughTheRun(unittest.TestCase):

    def test_every_cell_has_one_id_and_no_two_cells_share_one(self):
        with tempfile.TemporaryDirectory() as d:
            ids = _ids_per_cell(_segment(d, label_overlap=0.5))
        for name, got in ids.items():
            self.assertEqual(len(got), 1, f'{name} has ids {got}')
        self.assertEqual(len(set.union(*ids.values())), len(_CELLS))

    def test_off_leaves_seam_cells_split(self):
        """`labelOverlap=0` is off: the cut cells keep one id per tile, as before."""
        with tempfile.TemporaryDirectory() as d:
            ids = _ids_per_cell(_segment(d, label_overlap=0))
        self.assertEqual(len(ids['inside']), 1)
        self.assertEqual(len(ids['y_seam']), 2)
        self.assertEqual(len(ids['corner']), 4)
        self.assertEqual(len(ids['long']), 3)

    def test_no_overlap_means_nothing_to_match(self):
        """Without overlap padding no band is read by both tiles, so nothing can be joined."""
        with tempfile.TemporaryDirectory() as d:
            ids = _ids_per_cell(_segment(d, label_overlap=0.5, overlap=0))
        self.assertEqual(len(ids['y_seam']), 2)


class TestStitchMatching(unittest.TestCase):

    def setUp(self):
        self.seg = SegmentationUtils({'taskDir': '/tmp', 'labelOverlap': 0.3}, None)

    def test_one_to_one(self):
        """Two cells on one side both overlapping one cell on the other: only the better one joins."""
        lo = np.zeros((4, 10), np.uint32)
        lo[:, 0:10] = 1
        hi = np.zeros((4, 10), np.uint32)
        hi[:, 0:6] = 2
        hi[:, 6:10] = 3
        vol = np.array([[1, 2, 3]], np.uint32)
        out = self.seg._stitch_tile_seams(vol, {('X', 8, 0): {'lo': lo, 'hi': hi}})
        self.assertEqual(out.tolist(), [[1, 1, 3]])

    def test_below_threshold_stays_apart(self):
        lo = np.zeros((4, 10), np.uint32)
        lo[:, 0:2] = 1
        hi = np.zeros((4, 10), np.uint32)
        hi[:, 0:10] = 2
        vol = np.array([[1, 2]], np.uint32)
        out = self.seg._stitch_tile_seams(vol, {('X', 8, 0): {'lo': lo, 'hi': hi}})
        self.assertEqual(out.tolist(), [[1, 2]])

    def test_chain_takes_the_smallest_id(self):
        """A cell across two seams joins through union-find; no id is invented."""
        a = np.full((2, 2), 5, np.uint32)
        b = np.full((2, 2), 9, np.uint32)
        c = np.full((2, 2), 7, np.uint32)
        strips = {('X', 8, 0): {'lo': a, 'hi': b}, ('X', 16, 0): {'lo': c, 'hi': b}}
        out = self.seg._stitch_tile_seams(np.array([5, 7, 9], np.uint32), strips)
        self.assertEqual(out.tolist(), [5, 5, 5])


if __name__ == '__main__':
    unittest.main()
