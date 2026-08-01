"""Does a RUN's tile seam cross the previewed region?

The preview hands the whole visible region to `predict_slice` as ONE tile. A run tiles at `blockSize`
and re-stitches labels split across each seam, so where a seam crosses the region the run's mask is two
inferences plus an IoU re-join and the preview's is one — counts and boundaries near it differ. The
preview says so; these pin WHEN it says so.

The subtlety worth testing: the answer is POSITIONAL. "Is the region bigger than blockSize" is the
wrong question, because the run's tile grid is anchored at the image ORIGIN
(`_create_xy_tiles`: y = 0, block_size, 2*block_size, …) and only the *write* bounds land on it —
reads are padded by `overlap`.

Run with `pixi run test-py`.
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'preview'))
from preview_worker import _run_tile_seams          # noqa: E402

FULL = {'Y': 2048, 'X': 2048}


def seams(y=(0, 100), x=(0, 100), block=512, full=FULL):
    return _run_tile_seams({'Y': list(y), 'X': list(x)}, full, block)


class RunTileSeamsTest(unittest.TestCase):
    def test_a_region_inside_one_tile_has_no_seam(self):
        self.assertEqual(seams(y=(0, 300), x=(0, 300)), {})
        self.assertEqual(seams(y=(520, 1000), x=(520, 1000)), {})   # wholly inside tile (1,1)

    def test_a_big_region_that_still_fits_one_tile_has_no_seam(self):
        # THE case a size-only test gets wrong: 600 px > blockSize, but with block 1024 it is one tile
        self.assertEqual(seams(y=(0, 600), x=(0, 600), block=1024), {})

    def test_a_small_region_straddling_a_boundary_has_a_seam(self):
        # ...and the mirror image: 300 px < blockSize, but it crosses y=512
        self.assertEqual(seams(y=(400, 700), x=(0, 100)), {'Y': 1})

    def test_counts_a_seam_per_axis(self):
        self.assertEqual(seams(y=(400, 700), x=(400, 700)), {'Y': 1, 'X': 1})

    def test_counts_several_seams_on_one_axis(self):
        # 0..1600 crosses 512, 1024 and 1536
        self.assertEqual(seams(y=(0, 1600), x=(0, 10))['Y'], 3)

    def test_a_boundary_exactly_at_the_region_edge_is_not_a_seam(self):
        # the region ENDS where the tile ends: the run writes it as one whole tile too, so no re-join
        # inside what the user is looking at. Strict inequality, deliberately.
        self.assertEqual(seams(y=(0, 512), x=(0, 512)), {})
        self.assertEqual(seams(y=(512, 1024), x=(512, 1024)), {})

    def test_the_live_case_that_prompted_this(self):
        # r0hufV, 541x576 at the image origin, default blockSize 512 — the run tiles 2x2 here, so the
        # 12 cells the preview reported are close to but not exactly what a run produces
        self.assertEqual(_run_tile_seams({'Y': [0, 541], 'X': [0, 576]},
                                         {'Y': 541, 'X': 576}, 512), {'Y': 1, 'X': 1})

    def test_an_image_smaller_than_one_tile_never_seams(self):
        self.assertEqual(_run_tile_seams({'Y': [0, 300], 'X': [0, 300]},
                                         {'Y': 300, 'X': 300}, 512), {})

    def test_a_missing_or_absurd_block_size_reports_nothing(self):
        # never invent a warning from a bad param — an absent blockSize must not read as "tiled"
        self.assertEqual(seams(y=(0, 2000), x=(0, 2000), block=0), {})
        self.assertEqual(seams(y=(0, 2000), x=(0, 2000), block=-1), {})

    def test_axes_absent_from_the_region_are_skipped(self):
        self.assertEqual(_run_tile_seams({'Y': [400, 700]}, FULL, 512), {'Y': 1})
        self.assertEqual(_run_tile_seams({}, FULL, 512), {})


if __name__ == '__main__':
    unittest.main()
