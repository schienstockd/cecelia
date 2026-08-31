"""The one movie size policy + the one writer (cecelia.utils.movie_io).

`coerce_movie_size` is where a requested output size is made safe, so its rules are pinned here rather
than left to the three surfaces that ask for one: clamp per axis, force even (h.264 with yuv420p rejects
odd dimensions outright, and we write with macro_block_size=1 so imageio will NOT quietly rescale for
us), and treat blank/zero as "the canvas size".

The last test is the title-card contract: the card is rendered from the recorded movie's own frame
size and concatenated onto it, so if the two ever disagreed ffmpeg would rescale one half of the
file. That one encodes for real (imageio-ffmpeg is in the env).

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

    def test_the_writer_closes_even_when_the_block_fails(self):
        # imageio's own __exit__ closes ONLY on a clean exit (`if value is None: self.close()`), so a
        # cancelled render left the writer open: the caller's cleanup looked for the staged file
        # before ffmpeg had finalised it, found nothing, and the temp appeared moments later — every
        # cancel leaking a `.tmp.mp4`. The recorder's own cancel tests missed it because they stub the
        # writer; this one encodes for real, which is the only way the behaviour shows up.
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'clip.mp4')
            with self.assertRaises(RuntimeError):
                with movie_io.movie_writer(path, 10) as out:
                    out.append_data(np.zeros((34, 66, 3), dtype=np.uint8))
                    raise RuntimeError('cancelled mid-render')
            self.assertTrue(os.path.exists(path), 'the file must exist by the time cleanup runs')
            os.remove(path)                       # must not raise: cleanup can actually delete it

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


class TestEncodeRawFrames(unittest.TestCase):
    """The offline renderer's hand-off: a file of raw RGB24 frames in, an mp4 out.

    Julia composites the frames and this encodes them, because ``movie_writer`` is the one imageio
    writer in the repo. The interesting failure is not "it did not encode" — it is a TRUNCATED render
    encoding to a movie that looks complete and is short.
    """

    def _raw(self, d, w, h, n, first=0):
        path = os.path.join(d, 'frames.rgb24')
        with open(path, 'wb') as fh:
            for i in range(n):
                fh.write(np.full((h, w, 3), first + i * 40, dtype=np.uint8).tobytes())
        return path

    def test_round_trip_keeps_the_frames_and_their_order(self):
        import imageio.v2 as imageio
        with tempfile.TemporaryDirectory() as d:
            raw = self._raw(d, 66, 34, 4, first=10)
            out = os.path.join(d, 'clip.mp4')
            self.assertEqual(
                movie_io.encode_raw_frames(raw, out, width=66, height=34, frames=4, fps=10), 4)
            with imageio.get_reader(out) as r:
                frames = [f for f in r]
            self.assertEqual(len(frames), 4)
            self.assertEqual(frames[0].shape[:2], (34, 66))
            # h264 is lossy, so the assertion is on ORDER (each frame brighter than the last) rather
            # than on exact values — a row/column swap or a reversed sweep breaks it, rounding does not.
            means = [float(f.mean()) for f in frames]
            self.assertEqual(means, sorted(means))
            self.assertGreater(means[-1] - means[0], 50)

    def test_a_truncated_render_is_refused_before_a_partial_movie_exists(self):
        with tempfile.TemporaryDirectory() as d:
            raw = self._raw(d, 66, 34, 3)          # three frames written...
            out = os.path.join(d, 'clip.mp4')
            with self.assertRaises(ValueError) as cm:
                movie_io.encode_raw_frames(raw, out, width=66, height=34, frames=4, fps=10)
            self.assertIn('truncated', str(cm.exception))
            self.assertFalse(os.path.exists(out), 'no half-written mp4 may be left behind')

    def test_a_wrong_frame_size_is_caught_rather_than_reshaping_the_movie(self):
        # Same byte count, wrong geometry: 66x34 and 34x66 are both 6732 px. Nothing downstream can
        # tell these apart, so the size has to come from the renderer and be trusted exactly once.
        with tempfile.TemporaryDirectory() as d:
            raw = self._raw(d, 66, 34, 2)
            out = os.path.join(d, 'clip.mp4')
            movie_io.encode_raw_frames(raw, out, width=34, height=66, frames=2, fps=10)
            import imageio.v2 as imageio
            with imageio.get_reader(out) as r:
                self.assertEqual(r.get_data(0).shape[:2], (66, 34))   # it believes the caller


class TestStitchMovies(unittest.TestCase):
    """Side-by-side version comparison (docs/todo/MOVIE_COMPARE_PLAN.md). These encode for real —
    the geometry rules only mean anything against frames that survived a round trip through h.264."""

    def _write(self, path, shape, n=4, fps=10, value=None):
        """An n-frame clip of solid frames; `value` fixes the grey level (else it ramps per frame)."""
        with movie_io.movie_writer(path, fps) as out:
            for i in range(n):
                out.append_data(np.full((*shape, 3), i * 40 if value is None else value, dtype=np.uint8))

    def _read(self, path):
        import imageio.v2 as imageio
        with imageio.get_reader(path) as r:
            return [np.asarray(f) for f in r]

    def test_row_layout_puts_the_tiles_side_by_side(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=4)
            self._write(b, (34, 66), n=4)
            n = movie_io.stitch_movies([a, b], out, fps=10)
            frames = self._read(out)
            self.assertEqual(n, 4)
            self.assertEqual(len(frames), 4)
            self.assertEqual(frames[0].shape[:2], (34, 134))   # same height, 2 tiles + a 2px divider

    def test_column_layout_stacks_them(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=3)
            self._write(b, (34, 66), n=3)
            movie_io.stitch_movies([a, b], out, fps=10, layout='column')
            self.assertEqual(self._read(out)[0].shape[:2], (70, 66))   # 2 tiles + a 2px divider

    def test_tiles_keep_their_own_content(self):
        # the whole point: column 1 is column 1 and column 2 is column 2, not an average of the two
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=2, value=20)
            self._write(b, (34, 66), n=2, value=200)
            movie_io.stitch_movies([a, b], out, fps=10)
            frame = self._read(out)[0]
            left, right = frame[:, :66], frame[:, 68:]     # skip the 2px divider between them
            self.assertLess(int(left.mean()), 60)
            self.assertGreater(int(right.mean()), 150)

    def test_the_shorter_input_holds_its_last_frame(self):
        # truncating to the shortest would silently drop the end of the longer timecourse
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=6)
            self._write(b, (34, 66), n=2)
            self.assertEqual(movie_io.stitch_movies([a, b], out, fps=10), 6)
            self.assertEqual(len(self._read(out)), 6)

    def test_a_smaller_input_is_padded_not_rejected(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=2)
            self._write(b, (20, 40), n=2)
            movie_io.stitch_movies([a, b], out, fps=10)
            self.assertEqual(self._read(out)[0].shape[:2], (34, 134))   # both tiles at the largest

    def test_captions_add_one_strip_per_tile(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, plain, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'p.mp4', 'o.mp4'))
            self._write(a, (34, 66), n=2)
            self._write(b, (34, 66), n=2)
            movie_io.stitch_movies([a, b], plain, fps=10)
            movie_io.stitch_movies([a, b], out, fps=10, labels=['default', 'af_corrected'])
            tall, short = self._read(out)[0].shape[0], self._read(plain)[0].shape[0]
            self.assertGreater(tall, short)                    # the strip is under the tiles
            self.assertEqual(self._read(out)[0].shape[1], 134)  # and does not change the width

    def test_odd_composed_dimensions_are_cropped_even(self):
        # 33 px tiles stacked → 66 (fine), but the h.264 rule has to hold on the COMPOSED frame too
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=2)
            self._write(b, (34, 66), n=2)
            movie_io.stitch_movies([a, b], out, fps=10, layout='column', labels=['x', 'y'])
            h, w = self._read(out)[0].shape[:2]
            self.assertEqual((h % 2, w % 2), (0, 0))

    def test_progress_and_cancel_follow_the_recorder_contract(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=5)
            self._write(b, (34, 66), n=5)
            seen = []
            movie_io.stitch_movies([a, b], out, fps=10, on_progress=lambda i, t: seen.append((i, t)))
            self.assertEqual([i for i, _ in seen], [1, 2, 3, 4, 5])

            cancel_at = {'n': 0}

            def should_cancel():
                cancel_at['n'] += 1
                return cancel_at['n'] > 2

            out2 = os.path.join(d, 'out2.mp4')
            with self.assertRaises(movie_io.RecordCancelled):
                movie_io.stitch_movies([a, b], out2, fps=10, should_cancel=should_cancel)
            # a cancelled stitch leaves NOTHING behind — not the output, not the staged temp
            self.assertFalse(os.path.exists(out2))
            self.assertFalse(os.path.exists(out2 + '.tmp.mp4'))

    def test_a_cancel_never_destroys_the_previous_movie(self):
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._write(a, (34, 66), n=4)
            self._write(b, (34, 66), n=4)
            self._write(out, (34, 66), n=9, value=90)          # a good movie already at the target
            with self.assertRaises(movie_io.RecordCancelled):
                movie_io.stitch_movies([a, b], out, fps=10, should_cancel=lambda: True)
            self.assertEqual(len(self._read(out)), 9)          # untouched

    def test_bad_arguments_are_rejected_up_front(self):
        with tempfile.TemporaryDirectory() as d:
            a, out = os.path.join(d, 'a.mp4'), os.path.join(d, 'out.mp4')
            self._write(a, (34, 66), n=2)
            with self.assertRaises(ValueError):
                movie_io.stitch_movies([], out, fps=10)
            with self.assertRaises(ValueError):
                movie_io.stitch_movies([a], out, fps=10, layout='diagonal')
            with self.assertRaises(ValueError):
                movie_io.stitch_movies([a], out, fps=10, labels=['one', 'too many'])

if __name__ == '__main__':
    unittest.main()
