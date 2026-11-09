"""RESULT-REVIEW: result-d224a0e7

A two-frame movie whose last frame the acquisition stopped mid-volume leaves ONE complete frame to
estimate on (`estimate_drift(measured_frames=1)`). That frame is the reference — nothing was predicted
for it — so no frame may be reported as interpolated, else the drift QC banks `framesInterpolated = 1`
and a `drift.unregistered_frames` warning for a movie with no registration to fail.
"""
import unittest

import numpy as np
import ome_types
from scipy import ndimage as ndi

import cecelia.utils.correction_utils as cu
from cecelia.utils.dim_utils import DimUtils

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="2" SizeC="1" SizeZ="4"
    SizeY="64" SizeX="64"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


class OneCompleteFrameTest(unittest.TestCase):
    def test_the_reference_frame_is_not_reported_as_interpolated(self):
        rng = np.random.default_rng(2)
        base = ndi.gaussian_filter(rng.random((4, 64, 64)) * 3000, (0, 2, 2)) + 100
        arr = np.stack([base, np.roll(base, 5, axis=1)]).reshape(2, 1, 4, 64, 64).astype(np.uint16)
        arr[-1, :, 1:] = 0
        du = DimUtils(ome_types.from_xml(_OME), use_channel_axis=True)
        du.calc_image_dimensions(arr.shape)
        for name, kw in [('multiLag', {}), ('chain', {'estimator': 'chain'}),
                         ('per-plane', {'per_plane': True})]:
            with self.subTest(name):
                est = cu.estimate_drift(arr, 0, du, measured_frames=1, **kw)
                self.assertEqual(len(est.interpolated), 0, est.interpolated)
                self.assertEqual(len(est.positions), 2)
                np.testing.assert_array_equal(est.positions[1], est.positions[0])


if __name__ == '__main__':
    unittest.main()
