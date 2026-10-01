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


if __name__ == "__main__":
    unittest.main()
