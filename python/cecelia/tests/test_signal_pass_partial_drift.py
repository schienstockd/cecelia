"""RESULT-REVIEW: result-bd7163e6

A stopped acquisition's half-empty last timepoint stays out of the import probe's drift estimate
(`data_path._drift_check`), as it stays out of every other stat: its black planes register as
movement. The repro the result review ran.
"""
import unittest

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path


_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{t}" SizeC="1" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


class DriftLeavesOutAPartialTimepointTest(unittest.TestCase):
    def test_a_still_movie_with_a_half_empty_last_frame_reports_no_drift(self):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        rng = np.random.default_rng(2)
        n_t, n_z, size = 6, 4, 64
        base = ndi.gaussian_filter(rng.random((n_z, size, size)) * 3000, (0, 2, 2)) + 100   # planes differ in z
        arr = np.stack([base] * n_t).reshape(n_t, 1, n_z, size, size).astype(np.uint16)
        arr[-1, :, 1:] = 0                         # acquisition stopped after the last frame's first plane
        du = DimUtils(ome_types.from_xml(_OME.format(t=n_t, z=n_z, y=size, x=size)), use_channel_axis=True)
        du.calc_image_dimensions(arr.shape)
        chans = [{'saturated': False, 'topValue': int(arr.max()), 'noise': {'bgMedian': 100.0},
                  'photons': {'objects': {'p50Photons': 1.0}}}]
        drift = data_path._drift_check(arr, du, chans, 0, 1, lambda _m: None, skip_t=n_t - 1)
        assert max(drift['excursionPx']) < 0.1, drift            # sub-pixel estimator noise only
        # the half-empty frame left in reads as movement — what the skip prevents
        with_it = data_path._drift_check(arr, du, chans, 0, 1, lambda _m: None)
        assert max(with_it['excursionPx']) > 2 * max(drift['excursionPx']) + 0.1, with_it


if __name__ == '__main__':
    unittest.main()
