"""Import leaves drift out of the probe (~19 min of a ~22 min probe on a 47 GB movie, read only by the
acquisition check); `measure_signal(drift=True)` — the check's Measure — adds it."""
import unittest

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path
from cecelia.tests.test_signal_pass_partial_drift import _OME


class DriftIsOptInTest(unittest.TestCase):
    """Import leaves drift out (~19 min on a 47 GB movie); the acquisition check's Measure asks for it."""

    def _movie(self):
        rng = np.random.default_rng(3)
        base = ndi.gaussian_filter(rng.random((2, 48, 48)) * 3000, (0, 2, 2)) + 100
        return np.stack([base] * 4).reshape(4, 1, 2, 48, 48).astype(np.uint16)

    def test_the_signal_pass_leaves_drift_out_unless_asked(self):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        arr = self._movie()
        du = DimUtils(ome_types.from_xml(_OME.format(t=4, z=2, y=48, x=48)), use_channel_axis=True)
        du.calc_image_dimensions(arr.shape)
        chans = [{'saturated': False, 'topValue': int(arr.max()), 'noise': {'bgMedian': 100.0}}]
        axes = {'C': 1, 'T': 0, 'Z': 2}
        assert 'drift' not in data_path.measure_signal(arr, axes, du, [dict(c) for c in chans])
        out = data_path.measure_signal(arr, axes, du, [dict(c) for c in chans], drift=True)
        assert max(out['drift']['excursionPx']) < 0.1, out['drift']

    def test_a_still_image_answers_none_when_asked(self):
        arr = self._movie()[:1]
        chans = [{'saturated': False, 'topValue': int(arr.max()), 'noise': {'bgMedian': 100.0}}]
        out = data_path.measure_signal(arr, {'C': 1, 'T': 0, 'Z': 2}, None, chans, drift=True)
        assert 'drift' in out and out['drift'] is None


if __name__ == '__main__':
    unittest.main()
