"""The segmenter reads only the channels its model groups use — into the frame, not just the window.

`predict_from_zarr` reads one whole frame and tiles it in RAM. It used to read EVERY channel: on a
25-channel tile scan segmented on its nuclear stain that is 17 GB held for the run, against 0.7 GB
used, whatever the tile size. An engine now declares its channels (`_model_channels`); the base
reads their union and the tile's channel axis carries only those, in sorted image order.

Narrowing renumbers the channel axis, so the thing to pin is the MAPPING: a subclass that asks for
image channel 2 must get channel 2's pixels (`tile_channel`), never whatever now sits at position 2.

Run with `pixi run test-py`.
"""
import tempfile
import unittest

import numpy as np
import ome_types
import zarr

import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml

SIZES = (2, 1, 4, 24, 20)       # T,Z,C,Y,X — four channels, two timepoints
SHAPE = (2, 4, 24, 20)


def _image(du):
    """Every pixel of channel c holds c + 1, so a pixel value names the channel it came from."""
    im = np.zeros(tuple(du.im_dim), dtype=np.uint16)
    c_axis = du.im_dim_order.index('C')
    for c in range(du.dim_val('C')):
        idx = [slice(None)] * im.ndim
        idx[c_axis] = c
        im[tuple(idx)] = c + 1
    return im


def _dim_utils():
    du = DimUtils(ome_types.from_xml(_ome_xml(*SIZES)), use_channel_axis=True)
    du.calc_image_dimensions(SHAPE)
    return du


class _Recorder(SegmentationUtils):
    """Records, per call, the tile's channel count and which channel's pixels `tile_channel` gave."""

    def predict_slice(self, tile, model_params, norm_params=None):
        ch = model_params['cellChannels'][0]
        got = self.tile_channel(tile, ch)
        self.seen = getattr(self, 'seen', [])
        self.seen.append((tile.shape[0], ch, set(np.unique(got).tolist())))
        return np.zeros(tile.shape[-2:], dtype=np.uint32)


class _Declaring(_Recorder):
    def _model_channels(self, model_params):
        return list(model_params['cellChannels'])


def _run(cls, models, tmp):
    du = _dim_utils()
    seg = cls({'taskDir': tmp, 'outputValueName': 'probe', 'blockSize': 12, 'overlap': 4,
               'labelOverlap': 0, 'normaliseToWhole': False, 'models': models}, du)
    seg.predict_from_zarr([_image(du)])
    return seg


class FrameChannelReadTest(unittest.TestCase):

    def test_tile_carries_only_the_declared_channel_and_it_is_the_right_one(self):
        with tempfile.TemporaryDirectory() as tmp:
            seg = _run(_Declaring, {'0': {'matchAs': 'base', 'cellChannels': [2]}}, tmp)
        self.assertTrue(seg.seen)
        for n_c, ch, values in seg.seen:
            self.assertEqual(n_c, 1, 'the frame was read with every channel')
            # channel 2 holds 3 everywhere; position 2 of a full read would too, but position 0 of
            # the narrowed one is what an unmapped `tile[2]` would miss or misread
            self.assertEqual(values, {3})

    def test_two_groups_read_the_union_and_each_maps_to_its_own(self):
        models = {'0': {'matchAs': 'base', 'cellChannels': [3]},
                  '1': {'matchAs': 'base', 'cellChannels': [1]}}
        with tempfile.TemporaryDirectory() as tmp:
            seg = _run(_Declaring, models, tmp)
        self.assertEqual({n for n, _, _ in seg.seen}, {2})
        self.assertEqual({ch: tuple(v) for _, ch, v in seg.seen}, {3: (4,), 1: (2,)})

    def test_an_engine_that_declares_nothing_still_reads_every_channel(self):
        """The default must not narrow: an engine that cannot say which channels it needs would
        otherwise silently lose the rest."""
        with tempfile.TemporaryDirectory() as tmp:
            seg = _run(_Recorder, {'0': {'matchAs': 'base', 'cellChannels': [2]}}, tmp)
        self.assertEqual({n for n, _, _ in seg.seen}, {4})
        self.assertEqual({tuple(v) for _, _, v in seg.seen}, {(3,)})

    def test_tile_channels_is_cleared_after_the_run(self):
        """A caller handing `predict_slice` its own full-channel tile (the preview) must not inherit
        a previous run's narrowing."""
        with tempfile.TemporaryDirectory() as tmp:
            seg = _run(_Declaring, {'0': {'matchAs': 'base', 'cellChannels': [2]}}, tmp)
        self.assertIsNone(seg.tile_channels)
        full = np.stack([np.full((4, 4), c + 1, np.uint16) for c in range(4)])
        self.assertEqual(set(np.unique(seg.tile_channel(full, 2)).tolist()), {3})

    def test_asking_for_an_undeclared_channel_raises(self):
        seg = _Declaring({'taskDir': '.', 'outputValueName': 'x', 'models': {}}, _dim_utils())
        seg.tile_channels = (0, 2)
        tile = np.zeros((2, 4, 4), np.uint16)
        with self.assertRaisesRegex(ValueError, 'channel 1 was asked for'):
            seg.tile_channel(tile, 1)


class CellposeChannelsTest(unittest.TestCase):

    def test_cellpose_declares_cell_and_nucleus_with_its_own_default(self):
        from cecelia.utils.cellpose_utils import CellposeUtils
        declare = CellposeUtils._model_channels
        self.assertEqual(declare(None, {'cellChannels': [4], 'nucChannels': [0]}), [4, 0])
        # `predict_slice` falls back to channel 0 when no cell channel is set; the read must too
        self.assertEqual(declare(None, {'cellChannels': [], 'nucChannels': []}), [0])


class ReadTimepointChannelsTest(unittest.TestCase):

    def test_channels_keeps_the_axis_in_the_order_asked(self):
        du = _dim_utils()
        im = _image(du)
        with tempfile.TemporaryDirectory() as tmp:
            arr = zarr.open_array(f'{tmp}/a.zarr', mode='w', shape=im.shape, dtype=im.dtype,
                                  chunks=(1, 1, 12, 10))
            arr[:] = im
            for level in (im, zarr.open_array(f'{tmp}/a.zarr', mode='r')):
                frame = zarr_utils.read_timepoint(level, du, 1, channels=[3, 0])
                self.assertEqual(frame.shape, (2, 24, 20))
                self.assertEqual(frame[0].max(), 4)
                self.assertEqual(frame[1].max(), 1)
                self.assertEqual(zarr_utils.read_timepoint(level, du, 1).shape, (4, 24, 20))


if __name__ == '__main__':
    unittest.main()
