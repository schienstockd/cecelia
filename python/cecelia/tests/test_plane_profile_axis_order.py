"""RESULT-REVIEW: result-94235916

The probe took T/Z (and C) positions from the OME-XML `DimensionOrder`, which need not describe the
bioformats2raw store (always TCZYX). With `XYTZC` the frames were filed as depths and the whole stack
landed in one `byT` entry. The store's NGFF axes are authoritative.
"""
import unittest

import numpy as np

from cecelia.utils import intensity_utils


def _stack():
    """T=4, C=2, Z=5 as TCZYX: signal falls with depth (1000 → 600), flat in time."""
    arr = np.zeros((4, 2, 5, 16, 16), dtype=np.uint16)
    for z in range(5):
        arr[:, :, z] = 1000 - 100 * z
    return arr


class AxisOrderTest(unittest.TestCase):
    def test_ngff_axes_win_over_a_misleading_ome_order(self):
        arr = _stack()
        # What DimUtils derives from DimensionOrder="XYTZC" for this store: C right, T and Z wrong.
        from_ome = {'C': 1, 'T': 3, 'Z': 0}
        axes = intensity_utils.stack_axis_indices(['t', 'c', 'z', 'y', 'x'], arr.ndim, from_ome)
        self.assertEqual(axes, {'C': 1, 'T': 0, 'Z': 2})
        lead = [i for i in range(arr.ndim - 2) if i != axes['C']]
        prof = intensity_utils.PlaneProfile(lead, t_axis=axes['T'], z_axis=axes['Z'])
        intensity_utils.channel_histograms(arr, axes['C'], on_plane=prof)
        s = prof.summary(0, None)
        self.assertEqual(s['byZ']['p50'], [1000.0, 900.0, 800.0, 700.0, 600.0])
        self.assertEqual(s['byT']['p50'], [800.0] * 4)

    def test_fallback_only_without_store_axes(self):
        fb = {'C': 1, 'T': 0, 'Z': 2}
        self.assertEqual(intensity_utils.stack_axis_indices(None, 5, fb), fb)
        self.assertEqual(intensity_utils.stack_axis_indices(['y', 'x'], 5, fb), fb)   # incomplete


if __name__ == '__main__':
    unittest.main()
