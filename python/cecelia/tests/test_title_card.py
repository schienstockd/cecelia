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


class FontScaleTests(unittest.TestCase):
    """The card's type must not change size with the frame's ASPECT RATIO.

    Every font here scales off one reference while the margin, the wrap width and the ellipsis point
    are all driven by ``width``. When that reference was the raw height, a 500x500 batch recording drew
    its title at 5.0% of the frame width and a 1200x800 viewer recording at 3.33% — the same card, half
    again as large, purely because of the shape of the movie.
    """

    def _title_px_as_frac_of_width(self, w, h):
        return (tc._font_scale(w, h) * 0.05) / w

    def test_landscape_is_untouched(self):
        # the cap only binds below 3:2, so every wide recording renders exactly as it did
        for w, h in ((1600, 900), (1200, 800), (1024, 600)):
            self.assertEqual(tc._font_scale(w, h), h)

    def test_square_and_portrait_are_pulled_back(self):
        self.assertLess(tc._font_scale(500, 500), 500)
        self.assertLess(tc._font_scale(400, 800), 800)

    def test_same_relative_size_across_shapes(self):
        ref = self._title_px_as_frac_of_width(1200, 800)
        for w, h in ((500, 500), (600, 900), (800, 800)):
            self.assertAlmostEqual(self._title_px_as_frac_of_width(w, h), ref, places=6)

    def test_degenerate_sizes_terminate(self):
        # NOT just "doesn't raise": a frame narrower than twice the 16px margin makes `max_w`
        # NEGATIVE, and `_wrap_lines` used to append an empty string forever — the render hung and
        # took the recording with it. The bottom of the movie size policy is 2px, so this is
        # reachable from a size someone can actually type.
        self.assertGreater(tc._font_scale(0, 0), 0)
        for w, h in ((1, 1), (2, 2), (10, 400), (31, 31)):
            arr = tc.render_card_frame({"title": "a-long_name-that-cannot-fit"}, w, h)
            self.assertEqual(arr.shape[2], 3)

    def test_wrap_lines_terminates_on_a_negative_width(self):
        from PIL import Image, ImageDraw
        d = ImageDraw.Draw(Image.new("RGB", (4, 4)))
        f = tc._font(10)
        self.assertTrue(tc._wrap_lines(d, "abc", f, -50))       # returns at all = the fix


class FontCoverageTests(unittest.TestCase):
    """The font must render the characters real title cards actually contain.

    Titles are built as `name — attr — attr` with an EM DASH (U+2014) in api/src/napari_api.jl, and
    notes/attributes are free user text (µm, °, accents). Pillow's built-in Aileron covers little more
    than ASCII, so every title card shipped `.notdef` boxes where the separators were.

    A missing glyph is NOT an exception and NOT a zero-size mask — it is the font's `.notdef` box,
    which has a perfectly ordinary width. So coverage is asserted by comparing against a character
    the font certainly lacks: if `—` draws the same bitmap as a CJK ideograph, both are the box.
    """

    def _mask(self, font, ch):
        import numpy as np
        m = font.getmask(ch)
        if not m.size[0]:
            return None
        return np.array(m, dtype=np.uint8).reshape(m.size[1], m.size[0])

    def _is_notdef(self, font, ch):
        got, missing = self._mask(font, ch), self._mask(font, "中")   # 中 — not in any Latin font
        if got is None or missing is None:
            return got is None
        return got.shape == missing.shape and (got == missing).all()

    def test_renders_em_dash_and_not_a_box(self):
        font = tc._font(28)
        self.assertFalse(self._is_notdef(font, "—"), "em dash renders as .notdef — title cards show boxes")

    def test_renders_the_other_characters_user_text_carries(self):
        font = tc._font(28)
        for ch in ("µ", "°", "é", "–", "…"):     # µ ° é en-dash ellipsis
            self.assertFalse(self._is_notdef(font, ch), f"{ch!r} renders as .notdef")

    def test_ascii_still_renders(self):
        font = tc._font(28)
        self.assertFalse(self._is_notdef(font, "A"))

    def test_detector_catches_the_font_we_regressed_from(self):
        # Guards the assertions above: prove the check FAILS on Pillow's built-in, so a future reorder
        # of _font back to load_default-first is caught rather than silently passing.
        from PIL import ImageFont
        try:
            builtin = ImageFont.load_default(size=28)
        except TypeError:
            self.skipTest("Pillow < 10 has no scalable built-in")
        self.assertTrue(self._is_notdef(builtin, "—"))


class WrapTests(unittest.TestCase):
    def _draw(self):
        from PIL import Image, ImageDraw
        return ImageDraw.Draw(Image.new("RGB", (10, 10)))

    def test_short_title_stays_one_line(self):
        d, font = self._draw(), tc._font(16)
        self.assertEqual(tc._wrap_lines(d, "MERTK", font, 10_000), ["MERTK"])

    def test_long_name_wraps_and_every_line_fits(self):
        d, font = self._draw(), tc._font(16)
        name = "M1a-MERTK_KAT-SWHL-GFP-Tom-res_0001 — mouse 1 — location B"
        max_w = 120
        lines = tc._wrap_lines(d, name, font, max_w)
        self.assertGreater(len(lines), 1)                       # actually wrapped
        for ln in lines:
            self.assertLessEqual(d.textlength(ln, font=font), max_w + 0.5)
        # nothing dropped: joining the pieces back (ignoring the wrap spaces) preserves every character
        self.assertEqual("".join(lines).replace(" ", ""), name.replace(" ", ""))

    def test_hard_break_prefers_separator(self):
        d, font = self._draw(), tc._font(16)
        # a long no-space token → first line should end at a '-' or '_' where possible
        lines = tc._wrap_lines(d, "aaa-bbb_ccc-ddd-eee", font, 40)
        self.assertTrue(lines[0].endswith("-") or lines[0].endswith("_"))


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
