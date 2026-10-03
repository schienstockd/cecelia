"""The 3D movie runner end to end: a tiny store through ``render_animation_run.run`` to an mp4.

What it pins is the runner's own work — reading the store at the right t, the camera and LUT from
the view state, the mask, the overlays, the encode. The drawing itself is the viewer's shader and
is checked against NumPy in ``test_wgsl_utils.py``. h264 is lossy, so pixel checks are coarse
(which half of the frame is lit, which colour dominates), never exact values.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
import zarr

from cecelia.tests.test_wgsl_utils import _shared_host, _srgb_encode

N = 32          # x = y = z voxels
T = 2


def _store(path, data, axes):
    """A flat single-level OME-ZARR, the shape `create_multiscales` writes."""
    g = zarr.open_group(path, mode="w", zarr_format=2)
    g.attrs["multiscales"] = [{"axes": [{"name": a} for a in axes],
                               "datasets": [{"path": "0", "coordinateTransformations": [
                                   {"type": "scale", "scale": [1.0] * len(axes)}]}]}]
    g.create_array("0", data=data, chunks=data.shape)
    return path


def _frames(path):
    """Every frame of an mp4 as int RGB — the same reader `movie_io.stitch_movies` uses."""
    import imageio
    reader = imageio.get_reader(path)
    try:
        return np.stack([np.asarray(f)[..., :3] for f in reader]).astype(int)
    finally:
        reader.close()


class RenderAnimationRunTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        _shared_host()                      # skip, or fail under CECELIA_REQUIRE_WGPU, like the rest
        cls.d = tempfile.mkdtemp()
        img = np.zeros((T, 1, N, N, N), np.uint16)
        img[0, 0, :, 4:12, 4:12] = 1000     # t = 0: a bright block in the TOP-LEFT of the xy plane
        img[1, 0, :, 20:28, 20:28] = 1000   # t = 1: bottom-right
        cls.zarr = _store(os.path.join(cls.d, "img.zarr"), img, "tczyx")
        lab = np.zeros((T, N, N, N), np.uint32)
        lab[:, :, 4:12, 4:12] = 2           # label 2 over the t = 0 block, at every t
        cls.labels = _store(os.path.join(cls.d, "lab.zarr"), lab, "tzyx")

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.d, ignore_errors=True)

    def _run(self, **extra):
        from cecelia.writers import render_animation_run
        out = os.path.join(self.d, f"out{len(os.listdir(self.d))}.mp4")
        state = lambda t: {"t": t, "camera": {"angles": [0, 0, 0], "zoom": 1.0}, "snapH": N,
                           "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 0, 0]], "visible": True}]}
        params = {"zarrPath": self.zarr, "outPath": out, "states": [state(0), state(1)],
                  "canvasH": 64, "canvasW": 64, "zAniso": 1.0, "fps": 2, **extra}
        render_animation_run.run(params)
        return _frames(out)

    def test_frames_follow_t_and_the_camera(self):
        frames = self._run()
        self.assertEqual(frames.shape[:3], (2, 64, 64))
        red = frames[..., 0]
        # zoom 1 against a 32-row snapshot canvas → the whole 32-voxel height fills the 64-px frame,
        # so voxel block 4..12 maps to rows/cols ~8..24 and 20..28 to ~40..56.
        self.assertGreater(red[0, 8:24, 8:24].mean(), 150)
        self.assertLess(red[0, 40:56, 40:56].mean(), 40)
        self.assertGreater(red[1, 40:56, 40:56].mean(), 150)
        self.assertLess(red[1, 8:24, 8:24].mean(), 40)

    def test_mask_and_points_are_drawn(self):
        from cecelia.utils import wgpu_host
        # label 2's row, as the viewer shows it: the palette texture is linear and the target encodes
        pal = _srgb_encode(wgpu_host.label_palette()[2, :3] / 255)
        frames = self._run(labelsPath=self.labels, labelOpacity=1.0, labelContourPx=0,
                           pointSizePx=4)
        # At t = 1 the image block has moved away but the label is still top-left: that region is
        # the palette colour, not red.
        patch = frames[1, 10:22, 10:22].reshape(-1, 3).mean(0)
        self.assertLess(np.abs(patch - pal).max(), 40, f"mask colour {patch} != palette row {pal}")

        ov = {"points": {"x": [16.0], "y": [16.0], "z": [16.0], "colour": [[0.0, 1.0, 0.0]]}}
        from cecelia.writers import render_animation_run
        out = os.path.join(self.d, "pts.mp4")
        render_animation_run.run({
            "zarrPath": self.zarr, "outPath": out, "canvasH": 64, "canvasW": 64, "fps": 2,
            "pointSizePx": 4,
            "states": [{"t": 1, "camera": {"angles": [0, 0, 0], "zoom": 1.0}, "snapH": N,
                        "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 0, 0]], "visible": True}],
                        "overlays3d": ov}] * 2})
        f = _frames(out)[0]
        self.assertGreater(f[30:34, 30:34, 1].mean(), 150, "the point at the volume centre is not drawn")

    def test_title_card_is_prepended_only_when_asked(self):
        # The card has to reach the movie (it is prepended after the encode, so a card in params that
        # the runner drops ships a movie without it), and a record with none must not grow one.
        plain = self._run()
        carded = self._run(titleCard={"title": "Runner test", "durationSec": 1.0})
        self.assertEqual(len(plain), 2)
        self.assertGreaterEqual(len(carded), 2 + 2)           # 2 fps × 1 s of card, then the movie
        self.assertLess(int(carded[0].mean()), 120)           # the dark card comes first

    def _point_frame(self, **extra):
        """t = 1 (the block bottom-right, red) with one green point inside the block."""
        from cecelia.writers import render_animation_run
        out = os.path.join(self.d, f"pt{len(os.listdir(self.d))}.mp4")
        ov = {"points": {"x": [24.0], "y": [24.0], "z": [16.0], "colour": [[0.0, 1.0, 0.0]]}}
        render_animation_run.run({
            "zarrPath": self.zarr, "outPath": out, "canvasH": 64, "canvasW": 64, "fps": 2,
            "pointSizePx": 3, **extra,
            "states": [{"t": 1, "camera": {"angles": [0, 0, 0], "zoom": 1.0}, "snapH": N,
                        "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 0, 0]], "visible": True}],
                        "overlays3d": ov}] * 2})
        return _frames(out)[0]

    def test_point_border_is_a_black_ring(self):
        # The point sits at pixel ~(47.5, 47.5) over the red block; a 6-px border puts black where
        # the borderless frame shows the block. The quad's outer quarter is antialiased, so the solid
        # ring is 3..6.75 px from the centre.
        plain, ringed = self._point_frame(), self._point_frame(pointBorderPx=6)
        ring = (slice(46, 50), slice(51, 54))
        self.assertGreater(plain[ring][..., 0].mean(), 150)
        self.assertLess(ringed[ring][..., 0].mean(), 60, "no black ring around the point")
        self.assertGreater(ringed[47:50, 47:50, 1].mean(), 150, "the fill is gone")

    def test_perspective_camera_sets_the_projection(self):
        from cecelia.writers import render_animation_run
        st = lambda p: {"camera": {"angles": [0, 0, 0], "zoom": 1.0, "perspective": p}, "snapH": N}
        lanes = lambda p: render_animation_run.frame_uniforms(
            st(p), (1, N, N, N), (N, N, N), (1.0, 1.0, 1.0), 64, 64,
            None, {"pointPx": 1, "tailPx": 1, "borderPx": 0})
        self.assertEqual(lanes(0)['vp.ortho'], 1)
        self.assertEqual(lanes(1)['vp.ortho'], 0)



class Render2DRunTest(unittest.TestCase):
    """2D movies through the same pass: a slab of planes, head-on, as the viewer's 2D view draws."""

    @classmethod
    def setUpClass(cls):
        _shared_host()
        cls.d = tempfile.mkdtemp()
        img = np.zeros((1, 1, N, N, N), np.uint16)
        img[0, 0, 3, 4:12, 4:12] = 1000       # plane 3: top-left
        img[0, 0, 20, 20:28, 20:28] = 1000    # plane 20: bottom-right
        cls.zarr = _store(os.path.join(cls.d, "img.zarr"), img, "tczyx")
        lab = np.zeros((1, N, N, N), np.uint32)
        lab[:, :, 4:12, 4:12] = 2
        lab[:, :, 20:28, 20:28] = 5
        cls.labels = _store(os.path.join(cls.d, "lab.zarr"), lab, "tzyx")

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.d, ignore_errors=True)

    def _frame(self, z_range, overlays=None, plane_filter=None, **extra):
        from cecelia.writers import render_animation_run
        out = os.path.join(self.d, f"f{len(os.listdir(self.d))}.mp4")
        st = {"t": 0, "ndisplay": 2, "zRange": z_range, "snapH": N,
              "camera": {"center": [z_range[0], N / 2, N / 2], "zoom": 1.0, "angles": [0, 0, 0]},
              "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 0, 0]], "visible": True}]}
        if overlays:
            st["overlays3d"] = overlays
        if plane_filter:
            st["planeFilter"] = plane_filter
        render_animation_run.run({"zarrPath": self.zarr, "outPath": out, "canvasH": 64, "canvasW": 64,
                                  "fps": 2, "states": [st, st], **extra})
        return _frames(out)[0]

    def test_one_plane_is_that_plane(self):
        f = self._frame([3, 3])[..., 0]
        self.assertGreater(f[10:22, 10:22].mean(), 150)
        self.assertLess(f[42:54, 42:54].mean(), 40)       # plane 20 is not in the slab

    def test_a_slab_is_its_max(self):
        f = self._frame([0, N - 1])[..., 0]
        self.assertGreater(f[10:22, 10:22].mean(), 150)
        self.assertGreater(f[42:54, 42:54].mean(), 150)

    def test_colour_table_draws_only_its_labels_in_their_colours(self):
        f = self._frame([3, 3], labelsPath=self.labels, labelOpacity=1.0, labelContourPx=0,
                        labelColouring="table", labelColours={"ids": [2], "colours": [[0.0, 0.0, 1.0]]})
        tl = f[10:22, 10:22].reshape(-1, 3).mean(0)
        self.assertGreater(tl[2], 150)                      # label 2 in its table colour
        self.assertLess(tl[0], 60)
        self.assertLess(f[42:54, 42:54, 2].mean(), 40)      # label 5 is not in the table

    def test_an_empty_colour_table_draws_no_mask(self):
        # A population with no cells on this image — not the palette's every label.
        f = self._frame([0, N - 1], labelsPath=self.labels, labelOpacity=1.0, labelContourPx=0,
                        labelColouring="table", labelColours={"ids": [], "colours": []})
        np.testing.assert_array_equal(f, self._frame([0, N - 1]))

    def test_points_off_the_plane_are_hidden(self):
        ov = {"points": {"x": [16.0], "y": [16.0], "z": [20.0], "colour": [[0.0, 1.0, 0.0]]}}
        on = self._frame([20, 20], overlays=ov, plane_filter={"points": [18, 22]}, pointSizePx=4)
        off = self._frame([3, 3], overlays=ov, plane_filter={"points": [1, 5]}, pointSizePx=4)
        self.assertGreater(on[30:34, 30:34, 1].mean(), 150)
        self.assertLess(off[30:34, 30:34, 1].mean(), 40)


if __name__ == "__main__":
    unittest.main()


class Render2DRegionTest(unittest.TestCase):
    """A 2D frame uploads only the region it shows (``_xy_region``) — the same picture as the whole
    plane, and the reason a crop of a plane wider than the device's texture limit renders at all."""

    W = 96          # x
    H = 64          # y

    @classmethod
    def setUpClass(cls):
        _shared_host()
        cls.d = tempfile.mkdtemp()
        rng = np.random.default_rng(3)
        img = rng.integers(0, 1000, size=(1, 1, 4, cls.H, cls.W)).astype(np.uint16)
        cls.zarr = _store(os.path.join(cls.d, "img.zarr"), img, "tczyx")
        lab = np.zeros((1, 4, cls.H, cls.W), np.uint32)
        lab[:, :, 30:50, 60:80] = 3
        cls.labels = _store(os.path.join(cls.d, "lab.zarr"), lab, "tzyx")

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.d, ignore_errors=True)

    def _params(self):
        # zoom 2 on a 40-px canvas = 20 L0 rows, centred off-centre at (y 40, x 70)
        st = {"t": 0, "ndisplay": 2, "zRange": [1, 1], "snapH": 40,
              "camera": {"center": [1, 40, 70], "zoom": 2.0, "angles": [0, 0, 0]},
              "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 1, 1]], "visible": True}],
              "overlays3d": {"points": {"x": [72.0], "y": [38.0], "z": [1.0], "colour": [[1.0, 0.0, 0.0]]},
                             "segments": {"x0": [64.0], "y0": [34.0], "z0": [1.0], "x1": [76.0],
                                          "y1": [44.0], "z1": [1.0], "colour": [[0.0, 1.0, 0.0]]}}}
        return {"zarrPath": self.zarr, "states": [st], "canvasH": 40, "canvasW": 60,
                "labelsPath": self.labels, "labelContourPx": 1, "labelOpacity": 1.0, "pointSizePx": 3}

    def test_the_region_is_the_whole_planes_picture(self):
        from unittest import mock
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run as r
        host = wgpu_host.MipHost()
        region = next(r.render_frames(self._params(), host, _Log()))
        H, W = self.H, self.W
        whole = lambda st, l0, lyx, ch, cw: ((slice(0, lyx[0]), slice(0, lyx[1])), (0.0, 0.0, float(H), float(W)))
        with mock.patch.object(r, "_xy_region", whole):
            plane = next(r.render_frames(self._params(), host, _Log()))
        self.assertEqual(region.shape, plane.shape)
        # the same texels under the same pixels; a texel boundary may round the other way
        self.assertGreater((np.abs(region.astype(int) - plane.astype(int)).max(-1) <= 2).mean(), 0.995)
        self.assertGreater(region[..., 0].max(), 200)        # the point is drawn
        self.assertGreater(region[..., 1].max(), 200)        # and the tail

    def test_a_mask_on_another_grid_is_skipped(self):
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run as r
        odd = _store(os.path.join(self.d, "odd.zarr"), np.full((1, 4, self.H // 2, self.W // 2), 3, np.uint32), "tzyx")
        host = wgpu_host.MipHost()
        skipped = next(r.render_frames(dict(self._params(), labelsPath=odd), host, _Log()))
        bare = {k: v for k, v in self._params().items() if not k.startswith("label")}
        np.testing.assert_array_equal(skipped, next(r.render_frames(bare, host, _Log())))

    def test_no_camera_centre_is_the_images(self):
        from cecelia.writers import render_animation_run as r
        st = dict(self._params()["states"][0])
        st["camera"] = {"zoom": 2.0, "angles": [0, 0, 0]}
        (_, _), origin = r._xy_region(st, (4, self.H, self.W), (self.H, self.W), 40, 60)
        moved = r._in_region(st, origin, (4, self.H, self.W))["camera"]["center"]
        self.assertEqual((moved[1] + origin[0], moved[2] + origin[1]), (self.H / 2, self.W / 2))

    def test_the_region_is_what_the_camera_shows(self):
        from cecelia.writers import render_animation_run as r
        (ys, xs), (oy, ox, h, w) = r._xy_region(self._params()["states"][0], (4, self.H, self.W),
                                                (self.H, self.W), 40, 60)
        # 20 rows × 30 cols about (40, 70), a pixel of margin, clamped to the 96-wide image
        self.assertEqual((ys.start, ys.stop, xs.start, xs.stop), (29, 51, 54, 86))
        self.assertEqual((oy, ox, h, w), (29.0, 54.0, 22.0, 32.0))

    def test_a_crop_of_a_plane_over_the_limit_stays_at_level_0(self):
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run as r
        host = wgpu_host.MipHost()
        host.max_texture_3d = 48             # the plane (96 wide) is over it; the 32-wide region is not
        logged = []

        class Log:
            def log(self, m):
                logged.append(m)
        frame = next(r.render_frames(self._params(), host, Log()))
        self.assertEqual(frame.shape, (40, 60, 3))
        self.assertFalse(any("does not fit" in m for m in logged), logged)


class _Log:
    def log(self, _msg):
        pass


class RenderStillsTest(unittest.TestCase):
    """Stills are the movie's frames as PNGs, and one host serves request after request — what the
    preview worker relies on (docs/todo/STILLS_WORKER_PLAN.md Decisions 3 and 6)."""

    @classmethod
    def setUpClass(cls):
        _shared_host()
        cls.d = tempfile.mkdtemp()
        img = np.zeros((T, 1, N, N, N), np.uint16)
        img[0, 0, :, 4:12, 4:12] = 1000
        img[1, 0, :, 20:28, 20:28] = 1000
        cls.zarr = _store(os.path.join(cls.d, "img.zarr"), img, "tczyx")
        lab = np.zeros((T, N, N, N), np.uint32)
        lab[:, :, 4:12, 4:12] = 2
        cls.labels = _store(os.path.join(cls.d, "lab.zarr"), lab, "tzyx")

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.d, ignore_errors=True)

    def _params(self, name, **extra):
        state = lambda t: {"t": t, "camera": {"angles": [0, 30, 0], "zoom": 1.0}, "snapH": N,
                           "specs": [{"lo": 0, "hi": 1000, "lut": [[0, 0, 0], [1, 0, 0]], "visible": True}]}
        return {"zarrPath": self.zarr, "states": [state(0), state(1)], "canvasH": 48, "canvasW": 64,
                "outPaths": [os.path.join(self.d, f"{name}{i}.png") for i in range(2)], **extra}

    def _pngs(self, paths):
        from PIL import Image
        return [np.asarray(Image.open(p).convert("RGB")) for p in paths]

    def test_a_still_is_the_movie_frame(self):
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run as r
        p = self._params("still", labelsPath=self.labels, labelContourPx=1)
        r.run(p)                                         # through the one-off entry, as `run_py` would
        want = list(r.render_frames(p, wgpu_host.MipHost(), _Log()))
        got = self._pngs(p["outPaths"])
        self.assertEqual(got[0].shape, (48, 64, 3))
        for g, w in zip(got, want):
            np.testing.assert_array_equal(g, w)
        self.assertFalse(np.array_equal(got[0], got[1]))   # each state its own frame

    def test_a_reused_host_carries_nothing_over(self):
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run as r
        host = wgpu_host.MipHost()
        # a population mask first, then every label in the palette on the same host — the table must
        # not stand in for the palette
        r.render_stills(self._params("a", labelsPath=self.labels, labelColouring="table", labelOpacity=1.0,
                                     labelColours={"ids": [2], "colours": [[0.0, 0.0, 1.0]]}), host, _Log())
        plain = self._params("b", labelsPath=self.labels, labelOpacity=1.0)
        r.render_stills(plain, host, _Log())
        fresh = list(r.render_frames(plain, wgpu_host.MipHost(), _Log()))
        for g, w in zip(self._pngs(plain["outPaths"]), fresh):
            np.testing.assert_array_equal(g, w)

    def test_one_path_per_state(self):
        from cecelia.writers import render_animation_run as r
        p = self._params("n")
        p["outPaths"] = p["outPaths"][:1]
        with self.assertRaises(ValueError):
            r.run(p)
