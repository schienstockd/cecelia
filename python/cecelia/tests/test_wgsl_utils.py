"""The movie host's side of the shared shaders: same expansion, same uniform layout, same pixels.

- The golden cases in ``frontend/src/lib/webgpu/shaders/golden.json`` are the ones
  ``shaderSource.test.ts`` runs for the browser; passing both is what makes the two hosts' shader
  text identical.
- ``MipRenderTest`` renders one frame of the viewer's MIP shader headlessly and checks it against a
  NumPy evaluation of the same maths. It is also the install check for ``wgpu`` on each CI OS
  (``docs/todo/SHARED_RENDERER_PLAN.md`` → Phase 1).
"""
import json
import os
import unittest

import numpy as np

from cecelia.utils import wgsl_utils

with open(wgsl_utils.SHADER_DIR / "golden.json", encoding="utf-8") as _f:
    GOLDEN = json.load(_f)


class GoldenExpandTest(unittest.TestCase):
    def test_cases(self):
        for case in GOLDEN["expand"]:
            with self.subTest(case["name"]):
                read = case["files"].__getitem__
                if case.get("error"):
                    with self.assertRaises((KeyError, ValueError)):
                        wgsl_utils.expand_source("main.wgsl", case["vars"], read)
                else:
                    self.assertEqual(wgsl_utils.expand_source("main.wgsl", case["vars"], read), case["out"])


class GoldenLayoutTest(unittest.TestCase):
    def test_sizes_and_slots(self):
        for name, g in GOLDEN["layout"].items():
            with self.subTest(name):
                L = wgsl_utils.UniformLayout(name)
                self.assertEqual(L.floats, g["floats"])
                self.assertEqual(L.bytes, g["floats"] * 4)
                u = L.pack({lane: i + 1 for i, lane in enumerate(g["slots"])})
                for i, slot in enumerate(g["slots"].values()):
                    self.assertEqual(u[slot], i + 1)
                self.assertEqual(int(np.count_nonzero(u)), len(g["slots"]))

    def test_bad_lanes(self):
        for name, lanes in GOLDEN["badLanes"].items():
            L = wgsl_utils.UniformLayout(name)
            for lane in lanes:
                with self.subTest(lane), self.assertRaises((KeyError, IndexError)):
                    L.pack({lane: 1})


class RealShadersTest(unittest.TestCase):
    def test_entry_shaders_expand_fully(self):
        brick_multi = dict(ATLAS_DECLS="", ATLAS_SWITCH="", LAB_ATLAS_DECLS="", LAB_ATLAS_SWITCH="",
                           PT_BINDING=1, PREV_PT_BINDING=4, LUT_BINDING=5, PAL_BINDING=8, PICK_BINDING=9)
        cases = {"mip.wgsl": {}, "mip_points.wgsl": {}, "mip_segments.wgsl": {}, "tile.wgsl": {},
                 "brick.wgsl": {}, "brick_points.wgsl": {}, "brick_segments.wgsl": {},
                 "brick_multi.wgsl": brick_multi}
        for name, extra in cases.items():
            with self.subTest(name):
                code = wgsl_utils.expand_wgsl(name, **extra)
                self.assertNotIn("${", code)
                self.assertNotRegex(code, r"(?m)^\s*#include")

    def test_constants_print_like_javascript(self):
        for k, v in wgsl_utils.shader_constants().items():
            with self.subTest(k):
                self.assertRegex(wgsl_utils.format_var(v), r"^\d+(\.\d+)?$")


class MacArmProcessorTest(unittest.TestCase):
    """wgpu's Metal backend imports rubicon-objc, which reads the CPU from ``platform.processor()``.
    A Rosetta parent makes that say ``i386`` in a native arm64 Python, and the import crashes."""

    def _run(self, plat, machine, processor):
        import platform
        from unittest import mock
        from cecelia.utils import wgpu_host
        saved = platform.processor
        try:
            with mock.patch("sys.platform", plat), \
                 mock.patch("os.uname", return_value=mock.Mock(machine=machine), create=True), \
                 mock.patch("platform.processor", return_value=processor):
                wgpu_host._native_arm_processor()
                return platform.processor()
        finally:
            platform.processor = saved

    def test_rosetta_parent_on_apple_silicon_reads_arm(self):
        self.assertEqual(self._run("darwin", "arm64", "i386"), "arm")

    def test_everything_else_is_left_alone(self):
        self.assertEqual(self._run("darwin", "arm64", "arm"), "arm")
        self.assertEqual(self._run("darwin", "x86_64", "i386"), "i386")      # a real Intel Mac
        self.assertEqual(self._run("linux", "aarch64", ""), "")


class GoldenHostInputsTest(unittest.TestCase):
    """The CPU-side inputs the viewer computes before upload — same golden as the TS test."""

    def test_view_camera(self):
        from cecelia.utils import wgpu_host
        for c in GOLDEN["viewCamera"]:
            with self.subTest(c["name"]):
                got = wgpu_host.view_camera(c["camera"], c["snapH"], c["nX"], c["nY"], c["voxelUm"],
                                            c["canvasH"], c["halfAngle"])
                for k, v in c["want"].items():
                    self.assertAlmostEqual(got[k], v, places=9, msg=k)

    def test_view_camera_without_centre_is_centred(self):
        from cecelia.utils import wgpu_host
        got = wgpu_host.view_camera({"zoom": 1, "angles": [10, 20, 0]}, None, 100, 80, [1, 1, 1], 400)
        self.assertEqual((got["panX"], got["panY"]), (0, 0))

    def test_lut_rows(self):
        from cecelia.utils import wgpu_host
        rows = wgpu_host.lut_rows(GOLDEN["lut"]["luts"])
        for c, stops in GOLDEN["lut"]["rows"].items():
            for i, want in stops.items():
                with self.subTest(channel=c, stop=i):
                    self.assertEqual(rows[int(c), int(i)].tolist(), want)

    def test_label_palette(self):
        from cecelia.utils import wgpu_host
        pal = wgpu_host.label_palette()
        self.assertEqual(pal.shape, (64, 4))
        self.assertTrue((pal[:, 3] == 255).all())


def _srgb_encode(linear: np.ndarray) -> np.ndarray:
    """The ``rgba8unorm-srgb`` store: IEC 61966-2-1 transfer, then 8-bit."""
    lin = np.clip(linear, 0.0, 1.0)
    s = np.where(lin <= 0.0031308, 12.92 * lin, 1.055 * np.power(lin, 1 / 2.4) - 0.055)
    return np.round(s * 255).astype(np.int32)


def _ramp(lut_row: np.ndarray, n: np.ndarray, stops: int) -> np.ndarray:
    """``ramp()`` in ``mip.wgsl``: lerp between the two stops ``n`` falls between."""
    p = np.clip(n, 0, 1) * (stops - 1)
    i = np.floor(p).astype(int)
    j = np.minimum(i + 1, stops - 1)
    f = (p - i)[..., None]
    rgb = lut_row[:, :3].astype(np.float64) / 255
    return rgb[i] * (1 - f) + rgb[j] * f


_HOST = None


def _shared_host():
    """One ``MipHost`` for every render test, or a skip. CI sets CECELIA_REQUIRE_WGPU so a missing
    adapter FAILS there: the job exists to prove the shared renderer runs on each OS. A dev machine
    without one just skips."""
    global _HOST
    if _HOST is not None:
        return _HOST
    required = os.environ.get("CECELIA_REQUIRE_WGPU") == "1"
    try:
        from cecelia.utils import wgpu_host
        adapter = wgpu_host.request_adapter()
    except Exception as e:  # import or driver failure — reported, not hidden
        if required:
            raise
        raise unittest.SkipTest(f"wgpu unavailable: {e!r}")
    if adapter is None:
        if required:
            raise RuntimeError("wgpu: no adapter, and CECELIA_REQUIRE_WGPU=1")
        raise unittest.SkipTest("wgpu: no adapter on this machine")
    _HOST = wgpu_host.MipHost(adapter)
    info = _HOST.adapter_info
    print(f"\n[wgpu] {info.get('device')} ({info.get('backend_type')}, {info.get('adapter_type')})")
    return _HOST


class MipRenderTest(unittest.TestCase):
    """A 16x12x6 two-channel volume seen face-on, orthographic, framed so one pixel is one voxel
    column: pixel (x, y) is then the max over z of voxel column (x, y), image row 0 at the top."""

    NX, NY, NZ = 16, 12, 6

    @classmethod
    def setUpClass(cls):
        cls.host = _shared_host()

        consts = wgsl_utils.shader_constants()
        cls.stops, maxc = int(consts["LUT_STOPS"]), int(consts["MAX_CHANNELS"])
        rng = np.random.default_rng(7)
        cls.vol = rng.integers(0, 2500, size=(2, cls.NZ, cls.NY, cls.NX), dtype=np.uint16)
        cls.lut = np.zeros((maxc, cls.stops, 4), np.uint8)
        ramp = np.round(255 * np.arange(cls.stops) / (cls.stops - 1)).astype(np.uint8)
        cls.lut[0, :, 0] = ramp                       # channel 0: red
        cls.lut[1, :, 1] = ramp                       # channel 1: green
        cls.lut[:, :, 3] = 255
        cls.palette = np.array([[0, 0, 0, 255], [255, 0, 0, 255], [0, 255, 0, 255], [10, 200, 30, 255]],
                               np.uint8)
        cls.host.set_volume(cls.vol)
        cls.host.set_lut(cls.lut)
        cls.host.set_palette(cls.palette)
        cls.windows = [(100.0, 1100.0), (0.0, 2000.0)]

    def setUp(self):
        # The host is shared across test classes; put this class's inputs back before each test.
        self.host.set_volume(self.vol)
        self.host.set_lut(self.lut)
        self.host.set_palette(self.palette)
        self.host.set_labels(None)

    def _uniforms(self, **extra):
        half_angle = float(wgsl_utils.shader_constants()["VIEW_HALF_ANGLE"])
        u = {"cam.yaw": 0.0, "cam.pitch": 0.0, "cam.dist": (self.NY / 2) / half_angle, "cam.steps": 8 * self.NZ,
             "vp.nch": 2, "vp.ortho": 1,
             "ext.x": self.NX, "ext.y": self.NY, "ext.z": self.NZ, "ext.zOriginUm": 0,
             "dims.nx": self.NX, "dims.ny": self.NY, "dims.nz": self.NZ, "dims.zPerChannel": self.NZ,
             "ov.planeLo": -1, "pan.ribbonLo": -1}
        for c, (lo, hi) in enumerate(self.windows):
            u[f"ch[{c}].lo"], u[f"ch[{c}].hi"], u[f"ch[{c}].visible"] = lo, hi, 1
        u.update(extra)
        return u

    def _expected(self):
        acc = np.zeros((self.NY, self.NX, 3))
        for c, (lo, hi) in enumerate(self.windows):
            mx = self.vol[c].max(axis=0).astype(np.float64)
            acc += _ramp(self.lut[c], (mx - lo) / max(hi - lo, 1), self.stops)
        return acc

    def test_mip_matches_numpy(self):
        img = self.host.render(self.NX, self.NY, self._uniforms())
        got = img[..., :3].astype(np.int32)
        want = _srgb_encode(self._expected())
        self.assertLessEqual(int(np.abs(got - want).max()), 2, "shader MIP != NumPy MIP")
        self.assertTrue((img[..., 3] == 255).all())

    def test_filled_label_draws_its_palette_row(self):
        labels = np.zeros((self.NZ, self.NY, self.NX), np.uint32)
        labels[2, 0:4, 0:5] = 3                        # top-left block: rows 0-3 are the TOP of the frame
        self.host.set_labels(labels)
        try:
            img = self.host.render(self.NX, self.NY, self._uniforms(
                **{"lab.opacity": 1, "lab.contourPx": 0, "lab.paletteRows": len(self.palette)}))
        finally:
            self.host.set_labels(None)
        got = img[..., :3].astype(np.int32)
        want = _srgb_encode(self._expected())
        want[0:4, 0:5] = _srgb_encode(self.palette[3, :3] / 255)
        self.assertLessEqual(int(np.abs(got - want).max()), 2)


if __name__ == "__main__":
    unittest.main()


class OverlayRenderTest(unittest.TestCase):
    """A point marks the voxel it was measured from at any camera — the overlay pass shares the
    raycast's uniform block, so this checks the host's instance layout and draw, not the maths."""

    N, W = 32, 96

    @classmethod
    def setUpClass(cls):
        cls.host = _shared_host()

    def _render(self, vol, points, yaw, pitch):
        from cecelia.utils import wgpu_host
        n = self.N
        self.host.set_volume(vol)
        lut = np.zeros((32, 64, 4), np.uint8)
        lut[0, :, 0] = np.round(255 * np.arange(64) / 63)
        lut[:, :, 3] = 255
        self.host.set_lut(lut)
        self.host.set_palette(wgpu_host.label_palette())
        self.host.set_points(points)
        half = float(wgsl_utils.shader_constants()["VIEW_HALF_ANGLE"])
        u = {"cam.yaw": yaw, "cam.pitch": pitch, "cam.dist": n / half, "cam.steps": 256,
             "vp.nch": 1, "vp.ortho": 1, "ext.x": n, "ext.y": n, "ext.z": n,
             "dims.nx": n, "dims.ny": n, "dims.nz": n, "dims.zPerChannel": n,
             "ov.pointPx": 3, "ov.planeLo": -1, "ov.planeHi": -1, "pan.ribbonLo": -1,
             "ch[0].lo": 0, "ch[0].hi": 1000, "ch[0].visible": 1}
        try:
            return self.host.render(self.W, self.W, u)[..., :3].astype(int)
        finally:
            self.host.set_points(None)

    def test_point_sits_on_its_voxel_when_rotated(self):
        n = self.N
        vox = (21, 9, 6)                                       # x, y, z voxel index
        vol = np.zeros((1, n, n, n), np.uint16)
        vol[0, vox[2], vox[1], vox[0]] = 1000
        centre_um = [v + 0.5 for v in vox]                     # 1 µm voxels: the voxel's centre
        point = np.array([[*centre_um, 0.0, 1.0, 0.0, float(vox[2])]], np.float32)
        empty = np.zeros_like(vol)
        for yaw, pitch in [(0.0, 0.0), (0.6, 0.0), (0.6, 0.4), (-1.1, -0.5)]:
            with self.subTest(yaw=yaw, pitch=pitch):
                img = self._render(vol, None, yaw, pitch)
                vy, vx = np.unravel_index(np.argmax(img[..., 0]), img.shape[:2])
                self.assertGreater(img[vy, vx, 0], 200, "the bright voxel is not in frame")
                dot = self._render(empty, point, yaw, pitch)[..., 1]
                ys, xs = np.nonzero(dot > 128)
                self.assertTrue(len(xs), "the point drew nothing")
                self.assertLessEqual(abs(xs.mean() - vx), 1.5)
                self.assertLessEqual(abs(ys.mean() - vy), 1.5)
