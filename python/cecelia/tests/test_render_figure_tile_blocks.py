"""RESULT-REVIEW: result-5670aa1b — in ``pixels`` mode a crop under ``MAX_TILE_PX`` keeps whole native-pixel
blocks: a wanted tile whose nearest multiple overshoots the limit takes the largest multiple that fits,
never a non-multiple (2100 px at 4096 wanted gave 1.95 output px per native pixel)."""
import unittest

from cecelia.writers import render_figure as rf


class TestPixelBlocksUnderTheLimit(unittest.TestCase):
    def test_tile_is_a_whole_multiple_of_the_crop(self):
        for size, want in ((2100, 4096), (1500, 4096), (4096, 4096), (1000, 945), (90, 945)):
            with self.subTest(size=size, want=want):
                s = rf.validate(dict(zarrPath='x.zarr', outDir='out', center=[10, 10], sizePx=size, z=0,
                                     frames=[0], tilePx=want, rows=[{'channels': [{'c': 0, 'colour': '#ffffff'}]}]))
                tile = rf.tile_px(s, rf.STYLE, 1)
                self.assertEqual(tile % size, 0)                    # every native pixel the same block
                self.assertLessEqual(tile, rf.MAX_TILE_PX)


if __name__ == '__main__':
    unittest.main()
