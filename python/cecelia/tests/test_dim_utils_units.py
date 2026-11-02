"""`DimUtils.im_physical_size_um` converts the OME pixel size to µm by its unit, so a parameter given
in microns (cell diameter, AF mask scale) means the same thing on an image calibrated in nm."""
import unittest

from ome_types.model import OME, Image, Pixels

from cecelia.utils.dim_utils import DimUtils


def _dims(x, unit, z=None, z_unit=None):
    px = dict(dimension_order="XYZCT", size_x=4, size_y=4, size_z=1, size_c=1, size_t=1, type="uint16",
              physical_size_x=x, physical_size_x_unit=unit, physical_size_y=x, physical_size_y_unit=unit)
    if z is not None:
        px.update(physical_size_z=z, physical_size_z_unit=z_unit or unit)
    return DimUtils(OME(images=[Image(pixels=Pixels(**px))]))


class PhysicalSizeUmTest(unittest.TestCase):
    def test_micrometre_is_unchanged(self):
        self.assertAlmostEqual(_dims(0.5, "µm").im_physical_size_um("x"), 0.5)

    def test_nanometre_and_millimetre_convert(self):
        self.assertAlmostEqual(_dims(500.0, "nm").im_physical_size_um("x"), 0.5)
        self.assertAlmostEqual(_dims(0.0005, "mm").im_physical_size_um("y"), 0.5)
        # the raw accessor stays raw: callers that pair it with the unit are unaffected
        self.assertAlmostEqual(_dims(500.0, "nm").im_physical_size("x"), 500.0)

    def test_per_axis_unit(self):
        self.assertAlmostEqual(_dims(500.0, "nm", z=2.0, z_unit="µm").im_physical_size_um("z"), 2.0)

    def test_absent_size_returns_default(self):
        self.assertEqual(_dims(0.5, "µm").im_physical_size_um("z", default=1.0), 1.0)


if __name__ == "__main__":
    unittest.main()
