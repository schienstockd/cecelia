"""RESULT-REVIEW: result-081d832c

A 3D halo is grown in z too (`expand_labels`), so above and below its cell it is a DOME: each plane's
footprint is a different size. A raycast that outlined the first halo voxel it met drew a rim per cap
plane, and over the cell those rims stacked into near-solid rings — a contoured cell read as filled.
Derived-file outlines are a per-plane annotation (docs/todo/MASK_FILE_LAYERS_PLAN.md D3): a raycast
through several planes draws the cells exactly as it does without the halo.
"""
import unittest

import numpy as np

from cecelia.utils import wgsl_utils
from cecelia.tests.test_wgsl_utils import MipRenderTest, _shared_host


class HaloDomeIn3dTest(unittest.TestCase):
    N, NZ = 24, 9

    @classmethod
    def setUpClass(cls):
        cls.host = _shared_host()

    def _render(self, labels, contour):
        consts = wgsl_utils.shader_constants()
        n, nz = self.N, self.NZ
        stops, maxc = int(consts["LUT_STOPS"]), int(consts["MAX_CHANNELS"])
        lut = np.zeros((maxc, stops, 4), np.uint8)
        lut[:, :, 3] = 255                                  # a black image: only the mask shows
        self.host.set_volume(np.zeros((1, nz, n, n), np.uint16))
        self.host.set_lut(lut)
        self.host.set_palette(np.array([[0, 0, 0, 255], [255, 0, 0, 255], [0, 255, 0, 255]], np.uint8))
        self.host.set_labels(labels)
        half = float(consts["VIEW_HALF_ANGLE"])
        try:
            img = self.host.render(n, n, {
                "cam.yaw": 0.0, "cam.pitch": 0.0, "cam.dist": (n / 2) / half, "cam.steps": 8 * nz,
                "vp.nch": 1, "vp.ortho": 1, "ext.x": n, "ext.y": n, "ext.z": nz,
                "dims.nx": n, "dims.ny": n, "dims.nz": nz, "dims.zPerChannel": nz,
                "ov.planeLo": -1, "pan.ribbonLo": -1, "ch[0].lo": 0, "ch[0].hi": 1, "ch[0].visible": 1,
                "lab.opacity": 1, "lab.contourPx": contour, "lab.paletteRows": 3})
        finally:
            self.host.set_labels(None)
        return img[..., :3].astype(np.int32)

    def test_a_dome_cap_draws_the_cell_as_without_its_halo(self):
        out = int(wgsl_utils.shader_constants()["LABEL_FILE_OUT_BIT"])
        n, nz, c = self.N, self.NZ, self.N // 2
        yy, xx = np.mgrid[0:n, 0:n]
        r2 = (yy - c) ** 2 + (xx - c) ** 2
        base = np.zeros((nz, n, n), np.uint32)
        mid = nz // 2
        base[mid][r2 <= 4 ** 2] = 2                         # cell 2, one plane thick
        halo = base.copy()
        for z in range(nz):                                 # grown 4 voxels in 3D: a dome above / below
            d = abs(z - mid)
            if d <= 4:
                halo[z][r2 <= (4 + np.sqrt(16 - d * d)) ** 2] = 2
        packed = base.copy()
        packed[(halo != 0) & (base == 0)] = 2 | out         # compose_label_files!, halo: OUT only
        for contour in (0, 1):
            with self.subTest(contour=contour):
                without = self._render(base, contour)
                self.assertGreater(int(without.max()), 0, "nothing drawn; the check is vacuous")
                self.assertLessEqual(int(np.abs(self._render(packed, contour) - without).max()), 2)


if __name__ == "__main__":
    unittest.main()
