"""Colour mapping and contrast stretch for the computed-plane PNGs."""

import unittest

import numpy as np

from cecelia.utils.plane_render import (COLORMAPS, DEFAULT_COLORMAP, colormap_lut, plane_png,
                                        stretch_to_uint8)


class ColormapLutTest(unittest.TestCase):
    """The LUTs are baked in, so nothing at runtime can verify them against matplotlib.

    These anchors are matplotlib's own values. If a regenerated table ever disagrees, it is the table
    that is wrong — the whole reason for baking one in is that it cannot drift.
    """

    def test_viridis_matches_matplotlib_at_both_ends_and_the_middle(self):
        lut = colormap_lut('viridis')
        self.assertEqual(lut.shape, (256, 3))
        self.assertEqual(lut[0].tolist(), [68, 1, 84])        # #440154
        self.assertEqual(lut[128].tolist(), [33, 145, 140])
        self.assertEqual(lut[255].tolist(), [253, 231, 37])   # #fde725

    def test_magma_matches_matplotlib(self):
        lut = colormap_lut('magma')
        self.assertEqual(lut[0].tolist(), [0, 0, 4])
        self.assertEqual(lut[128].tolist(), [183, 55, 121])
        self.assertEqual(lut[255].tolist(), [252, 253, 191])

    def test_grey_has_no_lut_and_an_unknown_name_falls_back_to_it(self):
        # Not an error: an old client sending a map this build doesn't have should still get a plane.
        self.assertIsNone(colormap_lut('grey'))
        self.assertIsNone(colormap_lut('nonesuch'))

    def test_every_offered_colormap_resolves(self):
        for name in COLORMAPS:
            self.assertTrue(name == 'grey' or colormap_lut(name) is not None, name)
        self.assertIn(DEFAULT_COLORMAP, COLORMAPS)

    def test_the_ramp_is_monotonic_in_luminance(self):
        # What "perceptually uniform" buys, and the property that makes the sheet readable: a
        # brighter pixel is always a higher value, whichever map is on.
        for name in ('viridis', 'magma'):
            lum = colormap_lut(name).astype(float) @ [0.2126, 0.7152, 0.0722]
            self.assertTrue(np.all(np.diff(lum) > -1.0), f'{name} luminance is not monotonic')


class StretchTest(unittest.TestCase):

    def test_a_constant_plane_is_mid_grey_not_black(self):
        # "This metric is flat here" is information; black or white reads as a bug.
        out = stretch_to_uint8(np.full((4, 4), 7.0))
        self.assertTrue(np.all(out == 128))

    def test_non_finite_values_do_not_poison_the_range(self):
        arr = np.array([[0.0, 1.0], [np.nan, np.inf]])
        out = stretch_to_uint8(arr, percentiles=(0.0, 100.0))
        self.assertEqual(out.shape, (2, 2))
        self.assertEqual(out[0, 0], 0)
        self.assertEqual(out[0, 1], 255)

    def test_all_non_finite_is_black_rather_than_an_exception(self):
        self.assertTrue(np.all(stretch_to_uint8(np.full((3, 3), np.nan)) == 0))


class PlanePngTest(unittest.TestCase):

    def _decode(self, png):
        import imageio.v3 as iio
        import io
        return iio.imread(io.BytesIO(png))

    def test_a_colormapped_plane_comes_back_as_rgb(self):
        img = self._decode(plane_png(np.linspace(0, 1, 64).reshape(8, 8), colormap='viridis'))
        self.assertEqual(img.shape, (8, 8, 3))
        # lowest value → viridis dark purple, highest → yellow
        self.assertEqual(img[0, 0].tolist(), [68, 1, 84])
        self.assertEqual(img[7, 7].tolist(), [253, 231, 37])

    def test_grey_stays_single_channel(self):
        img = self._decode(plane_png(np.linspace(0, 1, 64).reshape(8, 8), colormap='grey'))
        self.assertEqual(img.shape, (8, 8))

    def test_a_3d_block_is_refused_rather_than_silently_projected(self):
        with self.assertRaises(ValueError):
            plane_png(np.zeros((2, 4, 4)))


if __name__ == '__main__':
    unittest.main()
