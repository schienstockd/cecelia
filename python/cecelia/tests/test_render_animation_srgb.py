"""The torch 3D renderer's sRGB transfer — golden values from IEC 61966-2-1, the same curve as
`api/src/image_render.jl` `_linear_to_srgb` (viewer parity: the canvas encodes through an sRGB view)."""
import unittest

from cecelia.writers.render_animation_run import _SRGB_LUT


class SrgbLutTest(unittest.TestCase):
    def test_golden_values(self):
        # 0 and 255 fixed; linear mid-grey 128 → sRGB 188; the linear toe below 0.0031308 (byte 0).
        self.assertEqual(int(_SRGB_LUT[0]), 0)
        self.assertEqual(int(_SRGB_LUT[255]), 255)
        self.assertEqual(int(_SRGB_LUT[128]), 188)
        self.assertEqual(int(_SRGB_LUT[1]), 13)

    def test_monotone(self):
        self.assertTrue(all(int(a) <= int(b) for a, b in zip(_SRGB_LUT, _SRGB_LUT[1:])))


if __name__ == '__main__':
    unittest.main()
