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


class FrameOverlayStyleTests(unittest.TestCase):
    """The per-frame timestamp + scale bar should MATCH the browser volume viewer's on-image
    overlay (`frontend/src/components/StillOverlay.vue`): white text with a dark stroke outline, a
    white bar with a hairline dark outline, no solid backing rectangles. Regressed once already —
    the movie had black rects while the viewer had none (Dominik, 2026-08-29 screenshot).
    """

    def test_bold_font_is_heavier_than_the_title_card_font(self):
        # `_bold_font` picks DejaVuSans-Bold; `_font` picks the regular. If matplotlib is present,
        # they should be structurally different at the same size — mask coverage of a solid letter
        # is greater for the bold face.
        import numpy as np
        regular = tc._font(48)
        bold    = tc._bold_font(48)
        r_mask  = np.array(regular.getmask("H"), dtype=np.uint8)
        b_mask  = np.array(bold.getmask("H"),    dtype=np.uint8)
        # allow either the same fallback face (no bold available → still works) or a heavier weight.
        self.assertTrue(b_mask.sum() >= r_mask.sum())

    def test_overlays_leave_no_solid_black_box_around_the_timestamp(self):
        # A translucent-black backing rect used to sit under the timestamp. It made the movie read
        # differently from the viewer, whose overlay is pure text with a dark STROKE. Guard: after
        # drawing on a bright frame, the pixels IMMEDIATELY under the glyphs must still contain
        # some bright background — a solid rect would zero them out.
        import numpy as np
        base = np.full((80, 200, 3), 220, dtype=np.uint8)     # bright brightfield-like frame
        out  = tc.draw_frame_overlays(base.copy(), timestamp="0:07:30")
        # Sample a strip inside the timestamp's rendered footprint but AWAY from the glyphs (a solid
        # rect would fill this too; a stroke does not). Row 0-25, columns 60-90 fall between glyphs
        # at typical bold-sans metrics.
        patch = out[0:25, 60:90]
        self.assertTrue(int(patch.max()) > 180,
                        "the timestamp overlay left a solid backing rect (viewer uses text-stroke)")

    def test_scale_bar_reads_as_a_white_bar_on_the_image(self):
        # The bar rectangle itself is drawn white with a dark hairline outline — same as the
        # viewer's `.ovl-fill`. Confirm: the bar body reads as bright white on a dark frame, and
        # the image above the bar (well away from the label) is untouched. Use a large frame so
        # the bar is chunky enough that the 1 px outline doesn't dominate the sample.
        import numpy as np
        H, W = 800, 800
        base = np.full((H, W, 3), 40, dtype=np.uint8)          # dark fluorescence-like frame
        out  = tc.draw_frame_overlays(
            base.copy(), scale_bar={"lengthPx": 200, "label": "25 µm"})
        bar_h = max(3, int(min(H, W) * 0.006))                 # matches title_card's own maths
        m     = 8
        # A row inside the bar, well away from the outline hairline; slice the middle of its width.
        bar_row     = out[H - m - bar_h // 2, W - 100:W - 20]  # centre row × middle columns
        image_above = out[H - 200:H - 100, W - 200:W - 20]     # image region above the label
        self.assertGreater(int(bar_row.mean()),   200)
        self.assertLess   (int(image_above.mean()), 100)


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


class EncodeRunnerTitleCardTests(unittest.TestCase):
    """The encoder runner (`writers/encode_movie_run.py`) is what the offline renderer invokes via `run_py`.
    A title card in params has to reach the movie, or the browser recorder ships without a card
    the legacy viewer path already writes. Exercised through the runner rather than the helper because the
    integration is where the params handoff can go wrong."""

    def test_runner_prepends_when_title_card_present(self):
        import imageio.v2 as imageio
        from cecelia.writers import encode_movie_run

        d = tempfile.mkdtemp()
        raw = os.path.join(d, "frames.rgb24")
        out = os.path.join(d, "clip.mp4")
        # 4 raw frames, brightening so the ORDER can be checked past the card.
        with open(raw, "wb") as fh:
            for i in range(4):
                fh.write(np.full((34, 66, 3), 20 + i * 40, dtype=np.uint8).tobytes())

        params = {
            "rawPath": raw, "outPath": out,
            "width": 66, "height": 34, "frames": 4, "fps": 10,
            "titleCard": {"title": "Runner test", "durationSec": 1.0},
        }
        encode_movie_run.run(params)

        with imageio.get_reader(out) as r:
            frames = [f for f in r]
        # 10 card frames (10 fps × 1 s) + 4 originals, with the usual small codec fudge.
        self.assertGreaterEqual(len(frames), 10 + 4 - 2)
        # The first frame is the DARK card, not the (already-dark) first movie frame — the runner has
        # to run the prepend AFTER the encode, or the card lands nowhere the reader can see it.
        self.assertLess(int(frames[0].mean()), 120)

    def test_runner_skips_when_title_card_absent(self):
        import imageio.v2 as imageio
        from cecelia.writers import encode_movie_run

        d = tempfile.mkdtemp()
        raw = os.path.join(d, "frames.rgb24")
        out = os.path.join(d, "clip.mp4")
        with open(raw, "wb") as fh:
            for i in range(4):
                fh.write(np.full((34, 66, 3), 80 + i * 40, dtype=np.uint8).tobytes())

        # No `titleCard` — the runner is called this way by every `record_view_movie` that was invoked
        # without one, so a stray prepend here would re-encode every movie the app records.
        encode_movie_run.run({
            "rawPath": raw, "outPath": out,
            "width": 66, "height": 34, "frames": 4, "fps": 10,
        })

        with imageio.get_reader(out) as r:
            frames = [f for f in r]
        # 4 frames in, 4 (± codec fudge) out. No card means no leading dark frames.
        self.assertLessEqual(len(frames), 4 + 1)
        self.assertGreater(int(frames[0].mean()), 40)         # brighter than a card would be


if __name__ == "__main__":
    unittest.main()
