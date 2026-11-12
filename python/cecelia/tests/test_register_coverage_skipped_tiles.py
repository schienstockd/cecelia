"""RESULT-REVIEW: result-78edacd5

A cycle whose scan skipped the tiles over part of the reference's tissue (the scanner writes exact
zeros there, inside its own rectangle) has not LOST that tissue — it never imaged it. The per-cycle
checks must call it not covered, so QC says "rescan", not "lifted or torn".
"""
import unittest
from unittest import mock

import numpy as np

from cecelia.utils import register_utils, tissue_utils
from cecelia.tests.test_tissue_and_slide_register import _pyramid, _sections, _shift


class SkippedTilesAreNotLostTissue(unittest.TestCase):
    def test_skipped_tiles_read_as_not_covered(self):
        try:
            import SimpleITK  # noqa: F401
        except Exception as e:                # pragma: no cover
            raise unittest.SkipTest(f'SimpleITK unavailable: {e}')
        ref, _ = _sections((1024, 768), [(250, 250), (700, 480)], 150, np.random.default_rng(3))
        mov = _shift(ref, -137.0, 61.0, 0.4)
        mov[450:] = 0                                              # tiles the cycle's scan skipped
        with mock.patch.multiple(tissue_utils, TISSUE_LEVEL_MAX_PX=200 * 200), \
                mock.patch.object(register_utils, 'REGISTER_FINE_MAX_PX', 600 * 600):
            tx, _ = register_utils.estimate_affine(_pyramid(ref), _pyramid(_shift(ref, -137.0, 61.0, 0.4)),
                                                   0, 0, (0, None, 0), 1)
            with mock.patch.multiple(register_utils, CHECK_PATCH_PX=96, CHECK_PATCHES=6):
                out = register_utils.cycle_checks(_pyramid(ref), _pyramid(mov), 0, 0, (0, None, 0), 1, tx)
        self.assertLess(out['tissueCovered'], 0.7)
        self.assertGreater(out['tissueRetained'], 0.85)


if __name__ == '__main__':
    unittest.main()
