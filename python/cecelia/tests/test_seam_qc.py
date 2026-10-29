"""Seam QC counts: label-to-label contacts across tile seams vs control lines (`seam_contacts`).

A joined seam looks like any line through the tissue; an unjoined one leaves two ids touching along
every cell it cuts. The run banks the raw counts as `objectStats.seam` (tiled runs only) and Julia
decides (`seg_seam_qc_findings`). Run with `pixi run test-py`.
"""
import tempfile
import unittest

import numpy as np
import ome_types

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import seam_contacts
from cecelia.tests.test_segmentation_streaming import _StubSeg, _ome_xml


class SeamContactsTest(unittest.TestCase):

    def test_a_cell_cut_at_the_seam_counts_there_and_not_on_the_controls(self):
        lab = np.zeros((6, 16), np.uint32)
        lab[:, 2:8] = 1
        lab[:, 8:14] = 2                 # the same cell, two ids either side of the seam at x = 8
        x = seam_contacts(lab, 8, -1)
        self.assertEqual(x['seamContacts'], 6)
        self.assertEqual(x['controlContacts'], 0)      # controls at x = 4 and 12: inside one id
        self.assertEqual((x['seamPixels'], x['controlPixels']), (6, 12))
        joined = lab.copy()
        joined[joined == 2] = 1
        self.assertEqual(seam_contacts(joined, 8, -1)['seamContacts'], 0)

    def test_background_is_not_a_contact(self):
        lab = np.zeros((4, 16), np.uint32)
        lab[:, 2:8] = 1                  # ends at the seam, nothing on the other side
        self.assertEqual(seam_contacts(lab, 8, -1)['seamContacts'], 0)


def _run(tmp, label_overlap, block=12):
    sizes, shape = (1, 1, 2, 24, 20), (2, 24, 20)
    du = DimUtils(ome_types.from_xml(_ome_xml(*sizes)), use_channel_axis=True)
    du.calc_image_dimensions(shape)
    seg = _StubSeg({'taskDir': tmp, 'outputValueName': 'stub', 'blockSize': block, 'overlap': 4,
                    'labelOverlap': label_overlap, 'normaliseToWhole': False,
                    'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}}}, du)
    seg.predict_from_zarr([np.ones(tuple(du.im_dim), np.uint16)])
    return seg.object_stats.get('seam')


class RunBanksSeamStatsTest(unittest.TestCase):
    """`_StubSeg` labels each tile top half 1 / bottom half 2, so its bands cross the seams."""

    def test_unjoined_seams_show_up_and_joined_ones_do_not(self):
        with tempfile.TemporaryDirectory() as a, tempfile.TemporaryDirectory() as b:
            off, on = _run(a, 0.0), _run(b, 0.1)
        self.assertGreater(off['seamContacts'], on['seamContacts'])
        self.assertEqual(on['seamContacts'], 0)

    def test_an_untiled_run_banks_nothing(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertIsNone(_run(tmp, 0.1, block=64))


if __name__ == '__main__':
    unittest.main()
