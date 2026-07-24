"""Tests for the movie title-card renderer (docs/todo/ANIMATION_PLAN.md → Phase H).

Pure rendering + the mp4 prepend, exercised without napari (imageio-ffmpeg encodes the tiny fixture
movie). Run via `pixi run test-py`.
"""
import os
import tempfile
import unittest

import numpy as np

from cecelia.utils import title_card as tc


class TitleFrameCountTests(unittest.TestCase):
    def test_fps_times_duration(self):
        self.assertEqual(tc.title_frame_count(15, 3), 45)
        self.assertEqual(tc.title_frame_count(24, 2.5), 60)

    def test_never_below_one(self):
        self.assertEqual(tc.title_frame_count(15, 0), 1)   # zero duration → still 1 frame
        self.assertEqual(tc.title_frame_count(0, 3), 1)
        self.assertEqual(tc.title_frame_count(None, None), 1)


class HexRgbTests(unittest.TestCase):
    def test_forms(self):
        self.assertEqual(tc._hex_rgb("#00ff00"), (0, 255, 0))
        self.assertEqual(tc._hex_rgb("00ff00"), (0, 255, 0))
        self.assertEqual(tc._hex_rgb("f00"), (255, 0, 0))       # shorthand

    def test_invalid_is_none(self):
        for bad in ("", None, "nope", "#12", "#1234567"):
            self.assertIsNone(tc._hex_rgb(bad))


class RenderCardFrameTests(unittest.TestCase):
    CONTENT = {
        "title": "MERTK — mouse 1 — location B",
        "sections": [
            {"heading": "Channels", "items": [{"label": "gBT", "colour": "#00ff00"},
                                              {"label": "SHG", "colour": "#808080"}]},
            {"heading": "Tracks",   "items": [{"label": "T cells", "colour": "#00bfff"}]},
        ],
        "note": "15s intravital, day 3",
    }

    def test_shape_and_dtype(self):
        arr = tc.render_card_frame(self.CONTENT, 400, 300)
        self.assertEqual(arr.shape, (300, 400, 3))
        self.assertEqual(arr.dtype, np.uint8)

    def test_draws_content_and_swatch(self):
        arr = tc.render_card_frame(self.CONTENT, 400, 300)
        # not just background — text/swatches were drawn
        self.assertTrue((arr > 40).any())
        # the pure-green channel swatch shows up (high green, low red/blue somewhere)
        green = (arr[:, :, 1] > 180) & (arr[:, :, 0] < 80) & (arr[:, :, 2] < 80)
        self.assertTrue(green.any())

    def test_empty_content_is_blank_card(self):
        arr = tc.render_card_frame({}, 200, 120)
        self.assertEqual(arr.shape, (120, 200, 3))          # no crash, just the background

    def test_sections_without_items_skipped(self):
        arr = tc.render_card_frame({"title": "T", "sections": [{"heading": "Empty", "items": []}]}, 200, 120)
        self.assertEqual(arr.shape, (120, 200, 3))


class PrependTests(unittest.TestCase):
    def test_prepends_card_frames(self):
        import imageio.v2 as imageio
        d = tempfile.mkdtemp()
        path = os.path.join(d, "movie.mp4")
        # a tiny 6-frame movie with even dimensions (yuv420p-safe)
        with imageio.get_writer(path, fps=10, macro_block_size=1) as w:
            for i in range(6):
                w.append_data(np.full((120, 160, 3), min(255, i * 40), dtype=np.uint8))

        n = tc.prepend_title_to_movie(path, {"title": "Test"}, duration_sec=1.0)
        self.assertEqual(n, 10)                              # 10 fps × 1s

        with imageio.get_reader(path) as r:
            total = sum(1 for _ in r)
        # card frames + the 6 originals (allow a small codec fudge on re-encode)
        self.assertGreaterEqual(total, n + 6 - 2)
        # first frame should now be the card (near-uniform dark bg), distinct from the movie's frame 0
        with imageio.get_reader(path) as r:
            first = r.get_data(0)
        self.assertLess(int(first.mean()), 120)


if __name__ == "__main__":
    unittest.main()
