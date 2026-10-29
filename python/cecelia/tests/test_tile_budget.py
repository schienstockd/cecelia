"""Auto tile size and overlap — `tile_budget` and how `SegmentationUtils` uses it.

`blockSize = auto` picks the largest tile that fits what is free when the run starts, up to the
whole frame; `overlap = auto` is 1.5 cell sizes. A run that still runs out of memory stops with the
size to set. Run with `pixi run test-py`.
"""
import tempfile
import unittest
from unittest import mock

import numpy as np
import ome_types

import cecelia.utils.cpu_utils as cpu_utils
import cecelia.utils.tile_budget as tb
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml

GB = 1e9


class ParseTest(unittest.TestCase):

    def test_auto_and_numbers(self):
        self.assertIsNone(tb.parse_tiling_param('auto'))
        self.assertIsNone(tb.parse_tiling_param(None))
        self.assertEqual(tb.parse_tiling_param('2048'), 2048)
        self.assertEqual(tb.parse_tiling_param(4096), 4096)       # saved params hold ints
        self.assertEqual(tb.parse_tiling_param('0', allow_zero=True), 0)
        with self.assertRaises(ValueError):
            tb.parse_tiling_param('0')                            # a 0 px tile is not a tile


class AutoBlockSizeTest(unittest.TestCase):
    COST = tb.TileCost(ram_per_px=100.0, vram_per_px=200.0, vram_fixed=2 * GB)

    def test_whole_frame_when_it_fits(self):
        block, why = tb.auto_block_size(self.COST, 1055, 1036, 30, free_ram=20 * GB, free_vram=8 * GB)
        self.assertEqual(block, 1055)
        self.assertIn('whole frame', why)

    def test_the_tighter_memory_wins_and_rounds_down(self):
        # VRAM: (8*0.8 - 2) GB / 200 B = 22e6 px → 4690 px side, minus 2*64 overlap → 4562 → 4352
        block, why = tb.auto_block_size(self.COST, 16898, 20329, 64, free_ram=100 * GB,
                                        free_vram=8 * GB)
        self.assertEqual(block, 4352)
        self.assertIn('gpu memory', why)
        # RAM tighter: (4*0.7) GB / 100 B = 28e6 px → 5291 - 128 → 5163 → 5120; VRAM gives 4352
        block, _ = tb.auto_block_size(self.COST, 16898, 20329, 64, free_ram=4 * GB, free_vram=8 * GB)
        self.assertEqual(block, 4352)
        block, why = tb.auto_block_size(self.COST, 16898, 20329, 64, free_ram=2 * GB, free_vram=8 * GB)
        self.assertLess(block, 4352)
        self.assertIn('ram', why)

    def test_what_the_run_already_holds_is_taken_off(self):
        free = tb.auto_block_size(self.COST, 20000, 20000, 0, free_ram=4 * GB)[0]
        held = tb.auto_block_size(self.COST, 20000, 20000, 0, held_bytes=2 * GB, free_ram=4 * GB)[0]
        self.assertLess(held, free)

    def test_never_below_the_floor(self):
        block, _ = tb.auto_block_size(self.COST, 20000, 20000, 0, free_ram=0.01 * GB)
        self.assertEqual(block, tb.MIN_AUTO_BLOCK)

    def test_unknowns_are_skipped_not_guessed(self):
        self.assertEqual(tb.auto_block_size(None, 900, 800, 0)[0], 900)
        block, why = tb.auto_block_size(self.COST, 20000, 20000, 0)
        self.assertEqual(block, 20000)
        self.assertIn('unknown', why)


class OverlapAndOomTest(unittest.TestCase):

    def test_overlap_is_one_and_a_half_cells(self):
        self.assertEqual(tb.auto_overlap(20), 30)
        self.assertEqual(tb.auto_overlap(None), 64)

    def test_oom_message_names_the_size_to_set(self):
        class OutOfMemoryError(RuntimeError):      # torch's class name; no torch import needed
            pass
        msg = tb.out_of_memory_message(OutOfMemoryError('CUDA out of memory'), 4096)
        self.assertIn('4096 px', msg)
        self.assertIn('2048 px', msg)
        self.assertIn('GPU memory', msg)
        self.assertIsNotNone(tb.out_of_memory_message(MemoryError(), 1024))
        self.assertIsNone(tb.out_of_memory_message(ValueError('bad channel'), 1024))


SIZES = (1, 1, 1, 64, 48)          # T,Z,C,Y,X
SHAPE = (64, 48)


def _du():
    du = DimUtils(ome_types.from_xml(_ome_xml(*SIZES)), use_channel_axis=True)
    du.calc_image_dimensions(SHAPE)
    return du


class _Sized(SegmentationUtils):
    """A stub engine with a declared cost and cell size, recording the tiles it is handed."""

    COST = tb.TileCost(ram_per_px=1000.0)

    def _object_size_um(self, model_params):
        return 10.0

    def _tile_cost(self, n_z):
        return self.COST

    def predict_slice(self, tile, model_params, norm_params=None):
        self.tiles = getattr(self, 'tiles', []) + [tile.shape[-2:]]
        return np.zeros(tile.shape[-2:], np.uint32)


class _Oom(_Sized):
    def predict_slice(self, tile, model_params, norm_params=None):
        class OutOfMemoryError(RuntimeError):
            pass
        raise OutOfMemoryError('CUDA out of memory. Tried to allocate 12.00 MiB')


def _params(tmp, **over):
    p = {'taskDir': tmp, 'outputValueName': 'probe', 'labelOverlap': 0, 'normaliseToWhole': False,
         'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}}}
    p.update(over)
    return p


class SegmentationAutoTest(unittest.TestCase):

    def test_auto_overlap_follows_the_cell_size_in_microns(self):
        # the fixture is 0.5 µm/px: a 10 µm cell is 20 px, so overlap 30 px
        seg = _Sized(_params('.'), _du())
        self.assertEqual(seg.overlap, 30)
        self.assertEqual(_Sized(_params('.', overlap='16'), _du()).overlap, 16)

    def test_auto_takes_the_whole_frame_when_it_fits(self):
        with tempfile.TemporaryDirectory() as tmp, \
                mock.patch.object(cpu_utils, 'available_memory_bytes', return_value=64 * GB):
            seg = _Sized(_params(tmp, overlap='4'), _du())
            seg.predict_from_zarr([np.zeros((64, 48), np.uint16)])
        self.assertEqual(seg.block_size, 64)
        self.assertEqual(len(seg.tiles), 1)
        self.assertIn('auto: whole frame', seg.tiling_note)

    def test_auto_tiles_when_memory_is_short(self):
        # (4 MB*0.7 - ~18 KB held) / 1000 B per px ≈ 2800 px → 52 px side, minus 2*4 overlap → 32
        with tempfile.TemporaryDirectory() as tmp, \
                mock.patch.object(cpu_utils, 'available_memory_bytes', return_value=4e6), \
                mock.patch.object(tb, 'MIN_AUTO_BLOCK', 16), mock.patch.object(tb, 'AUTO_BLOCK_STEP', 16):
            seg = _Sized(_params(tmp, overlap='4'), _du())
            seg.predict_from_zarr([np.zeros((64, 48), np.uint16)])
        self.assertLess(seg.block_size, 64)
        self.assertGreater(len(seg.tiles), 1)

    def test_a_set_size_is_kept(self):
        with tempfile.TemporaryDirectory() as tmp:
            seg = _Sized(_params(tmp, blockSize='16', overlap='4'), _du())
            seg.predict_from_zarr([np.zeros((64, 48), np.uint16)])
        self.assertEqual(seg.block_size, 16)
        self.assertEqual(seg.tiling_note, '16 px (set)')

    def test_out_of_memory_stops_with_the_size_to_set(self):
        with tempfile.TemporaryDirectory() as tmp:
            seg = _Oom(_params(tmp, blockSize='1024', overlap='4'), _du())
            with mock.patch.object(seg.logger, 'log') as log, self.assertRaises(SystemExit):
                seg.predict_or_stop([np.zeros((64, 48), np.uint16)])
        lines = ' '.join(str(c.args[0]) for c in log.call_args_list)
        self.assertIn('[ERROR] Ran out of GPU memory at 1024 px tiles', lines)
        self.assertIn('Set Tile size to 512 px', lines)


class CellposeCostTest(unittest.TestCase):

    def test_a_smaller_diameter_costs_more_gpu_memory(self):
        from cecelia.utils.cellpose_utils import CellposeUtils
        big = CellposeUtils({'taskDir': '.', 'models': {'0': {'cellDiameter': 15}}}, None)
        small = CellposeUtils({'taskDir': '.', 'models': {'0': {'cellDiameter': 15},
                                                          '1': {'cellDiameter': 5}}}, None)
        # 1 µm/px without dim_utils: 15 px → rescale (30/15)^2 = 4; 5 px → 36
        self.assertAlmostEqual(big._tile_cost(1).vram_per_px, 580 * 4)
        self.assertAlmostEqual(small._tile_cost(1).vram_per_px, 580 * 36)
        self.assertEqual(big._tile_cost(26).ram_per_px, 35 * 26)
        self.assertEqual(big._object_size_um({'cellDiameter': 8}), 8.0)


if __name__ == '__main__':
    unittest.main()
