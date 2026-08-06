"""The one movie size policy + the one writer (cecelia.utils.movie_io).

`coerce_movie_size` is where a requested output size is made safe, so its rules are pinned here rather
than left to the three surfaces that ask for one: clamp per axis, force even (h.264 with yuv420p rejects
odd dimensions outright, and we write with macro_block_size=1 so imageio will NOT quietly rescale for
us), and treat blank/zero as "the canvas size".

The last test is the title-card contract (docs/NAPARI.md → *Movie output size*): the card is rendered
from the recorded movie's own frame size and concatenated onto it, so if the two ever disagreed ffmpeg
would rescale one half of the file. That one encodes for real (imageio-ffmpeg is in the env).

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import tempfile
import unittest

import numpy as np

from cecelia.utils import movie_io
from cecelia.utils.movie_io import MAX_MOVIE_AXIS, coerce_movie_size, crop_to_even


class TestCoerceMovieSize(unittest.TestCase):
    def test_none_and_blank_mean_canvas_size(self):
        for value in (None, (0, 0), (None, None), (1920, 0)):
            hw, notes = coerce_movie_size(value)
            self.assertIsNone(hw, f'{value!r} should mean canvas size')
            self.assertEqual(notes, [], f'{value!r} is a default, not a problem to report')

    def test_even_size_passes_through(self):
        self.assertEqual(coerce_movie_size((1080, 1920)), ((1080, 1920), []))

    def test_odd_axes_are_evened_and_reported(self):
        hw, notes = coerce_movie_size((1081, 1921))
        self.assertEqual(hw, (1080, 1920))
        self.assertEqual(len(notes), 2)                      # the user must see both, not neither
        self.assertTrue(all('odd' in n for n in notes))

    def test_clamped_per_axis_and_reported(self):
        hw, notes = coerce_movie_size((9000, 1920))
        self.assertEqual(hw, (MAX_MOVIE_AXIS, 1920))         # only the offending axis moves
        self.assertEqual(len(notes), 1)
        self.assertIn(str(MAX_MOVIE_AXIS), notes[0])

    def test_clamp_result_is_still_even(self):
        # a clamp must not hand an odd number to ffmpeg, whatever MAX_MOVIE_AXIS is set to
        hw, _ = coerce_movie_size((99999, 99999))
        self.assertEqual([v % 2 for v in hw], [0, 0])

    def test_tiny_sizes_stay_encodable(self):
        self.assertEqual(coerce_movie_size((1, 1))[0], (2, 2))

    def test_garbage_falls_back_to_canvas_size_with_a_note(self):
        hw, notes = coerce_movie_size(('wide', 'tall'))
        self.assertIsNone(hw)
        self.assertEqual(len(notes), 1)                      # silent fallback would look like a no-op


class TestCropToEven(unittest.TestCase):
    def test_crops_only_what_is_odd(self):
        for shape, want in (((101, 65), (100, 64)), ((100, 64), (100, 64)),
                            ((101, 64), (100, 64)), ((100, 65), (100, 64))):
            frame = np.zeros((*shape, 4), dtype=np.uint8)
            self.assertEqual(crop_to_even(frame).shape[:2], want, f'{shape} → {want}')

    def test_keeps_the_channel_axis(self):
        self.assertEqual(crop_to_even(np.zeros((7, 7, 4), dtype=np.uint8)).shape, (6, 6, 4))


class TestWriterRoundTrip(unittest.TestCase):
    """A real encode: exact dimensions out, and a prepended card that matches them."""

    def _write(self, path, shape, n=4, fps=10):
        with movie_io.movie_writer(path, fps) as out:
            for i in range(n):
                frame = np.full((*shape, 3), i * 40, dtype=np.uint8)
                out.append_data(frame)

    def test_frames_keep_their_exact_size(self):
        # 66x34 is divisible by 2 but NOT by 16 — imageio's default macro_block_size would have
        # rescaled it to 80x48 with only a warning, which is why movie_writer sets it to 1.
        import imageio.v2 as imageio
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'clip.mp4')
            self._write(path, (34, 66))
            with imageio.get_reader(path) as r:
                self.assertEqual(r.get_data(0).shape[:2], (34, 66))

    def test_prepended_title_card_matches_the_movie(self):
        import imageio.v2 as imageio
        from cecelia.utils import title_card as tc
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'clip.mp4')
            self._write(path, (34, 66), n=4, fps=10)
            n_card = tc.prepend_title_to_movie(
                path, {'title': 'an image', 'note': '', 'sections': []}, duration_sec=0.3)
            self.assertGreater(n_card, 0)
            with imageio.get_reader(path) as r:
                shapes = {frame.shape[:2] for frame in r}
                count = sum(1 for _ in imageio.get_reader(path))
            # one size for the whole file: a card sized to a different canvas would be silently
            # rescaled by ffmpeg on concat, which is exactly what this pins
            self.assertEqual(shapes, {(34, 66)})
            self.assertEqual(count, n_card + 4)


if __name__ == '__main__':
    unittest.main()
