"""`normaliseToWhole` off means ONE intensity window per FRAME — never one per tile.

On, the run windows every tile with one whole-image statistic. Off used to hand the engines no
window at all, so each engine percentile-normalised its own TILE: on a tiled image every tile got its
own brightness, the patchiness the whole-image window exists to remove. Off is for a movie that
bleaches or brightens; the window it needs is the whole frame's. Run with `pixi run test-py`.
"""
import tempfile
import unittest

import numpy as np
import ome_types

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml

SIZES = (2, 1, 2, 48, 40)          # T,Z,C,Y,X
SHAPE = (2, 2, 48, 40)


def _image(du):
    """Frame 1 is ten times brighter than frame 0, and each frame is a ramp — so a per-tile window
    would differ between tiles of one frame, and a per-frame one between frames."""
    im = np.zeros(tuple(du.im_dim), np.uint16)
    ramp = np.arange(1, 48 * 40 + 1, dtype=np.uint16).reshape(48, 40)
    im[0, 0] = ramp
    im[1, 0] = np.minimum(ramp.astype(np.uint32) * 10, 65535).astype(np.uint16)
    im[:, 1] = 7
    return im


class _Recorder(SegmentationUtils):
    def predict_slice(self, tile, model_params, norm_params=None):
        self.seen = getattr(self, 'seen', []) + [None if norm_params is None else dict(norm_params)]
        return np.zeros(tile.shape[-2:], np.uint32)


def _run(normalise_to_whole, cls=_Recorder, cell_channels=(0,)):
    du = DimUtils(ome_types.from_xml(_ome_xml(*SIZES)), use_channel_axis=True)
    du.calc_image_dimensions(SHAPE)
    im = _image(du)
    with tempfile.TemporaryDirectory() as tmp:
        seg = cls({'taskDir': tmp, 'outputValueName': 'probe', 'blockSize': 16, 'overlap': 4,
                   'labelOverlap': 0, 'normaliseToWhole': normalise_to_whole,
                   'models': {'0': {'matchAs': 'base', 'cellChannels': list(cell_channels),
                                    'normalise': 99.0}}}, du)
        seg.predict_from_zarr([im])
    return seg, im


class PerFrameWindowTest(unittest.TestCase):

    def test_off_gives_every_tile_of_a_frame_the_frames_window(self):
        seg, im = _run(False)
        per_frame = len(seg.seen) // 2               # 3 x 3 tiles per frame, two frames
        f0, f1 = seg.seen[:per_frame], seg.seen[per_frame:]
        self.assertTrue(all(w is not None for w in seg.seen), 'off must still hand a window')
        self.assertEqual(len({w[0] for w in f0}), 1, 'tiles of one frame got different windows')
        self.assertEqual(len({w[0] for w in f1}), 1)
        self.assertNotEqual(f0[0][0], f1[0][0], 'two frames of different brightness, one window')
        # and it IS the whole frame's statistic
        whole0 = im[0, 0].ravel()
        self.assertAlmostEqual(f0[0][0][1], float(np.percentile(whole0[whole0 > 0], 99.0)))

    def test_on_gives_every_frame_the_same_window(self):
        seg, _ = _run(True)
        self.assertEqual(len({w[0] for w in seg.seen}), 1)

    def test_a_narrowed_read_still_windows_the_right_channel(self):
        class Declaring(_Recorder):
            def _model_channels(self, model_params):
                return list(model_params['cellChannels'])
        seg, _ = _run(False, cls=Declaring, cell_channels=(1,))
        # channel 1 is a constant 7: lo == hi == 7, keyed by the IMAGE channel
        self.assertEqual({tuple(w.items()) for w in seg.seen}, {((1, (7.0, 7.0)),)})

    def test_too_few_pixels_get_no_window(self):
        self.assertIsNone(SegmentationUtils._percentile_range(np.ones(100), 99.0))
        self.assertEqual(SegmentationUtils._percentile_range(np.full(101, 3), 99.0), (3.0, 3.0))


if __name__ == '__main__':
    unittest.main()
