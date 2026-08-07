"""What an optical-flow training run SAMPLES — `app/src/tasks/opticalFlow/train_run.py`.

Which Z planes, and which frames of each movie.

The runner is executed by path (`run_py`), never imported, so nothing else in the suite touches it.
This covers the one piece of arithmetic in it that is easy to get subtly wrong and impossible to
notice afterwards: the model trains fine either way, and the manifest records a number that looks
right, while the frames it learned from came from the wrong depth.

Two properties do the work:

  - `n = 1` must be the OLD single-plane rule (`n_z // 2`) exactly. The parameter replaced `zSlice`,
    and if the reduction is off by one then introducing it silently retrains every existing config
    on different data.
  - No `n` may reach plane 0 or `n_z - 1` while there is interior left. The top and bottom of an
    intravital stack are usually outside the tissue; `linspace(0, n_z-1, n)` puts two of five planes
    there, and the run would report five planes trained while two were noise.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import unittest
from pathlib import Path

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'opticalFlow' / 'train_run.py')


def _load_runner():
    """Load the runner from its path, exactly as `run_py` does (it is not an importable module)."""
    spec = importlib.util.spec_from_file_location('flow_train_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class ZPlanesTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # staticmethod: a plain function on a class attribute binds as a method,
        # and every call would pass `self` as `n_z`.
        cls.z_planes = staticmethod(_load_runner().z_planes)

    def test_one_plane_is_the_middle_exactly_as_before(self):
        """The compatibility hinge. `zPlanes=1` has to be `zSlice=-1`, or the default changed."""
        for n_z in (1, 2, 3, 9, 30, 31, 512):
            self.assertEqual(self.z_planes(n_z, 1), [n_z // 2], f'n_z={n_z}')

    def test_planes_are_evenly_spaced_and_inside_the_stack(self):
        got = self.z_planes(31, 3)
        self.assertEqual(got, [5, 15, 25])
        gaps = [b - a for a, b in zip(got, got[1:])]
        self.assertEqual(len(set(gaps)), 1, 'the spacing should be uniform')

    def test_it_avoids_the_top_and_bottom_of_the_stack(self):
        """Where `linspace(0, n_z-1, n)` would spend 2 of 5 planes outside the tissue."""
        for n in (2, 3, 4, 5, 8):
            got = self.z_planes(31, n)
            self.assertNotIn(0, got, f'n={n} reached the bottom plane')
            self.assertNotIn(30, got, f'n={n} reached the top plane')

    def test_asking_for_more_planes_than_exist_yields_each_once(self):
        """Not duplicates — a repeated index would weight those frames twice in the pooled set,
        silently, and the caller only asked for more planes than the stack has."""
        self.assertEqual(self.z_planes(4, 10), [0, 1, 2, 3])
        self.assertEqual(self.z_planes(1, 5), [0])

    def test_every_result_is_a_usable_index(self):
        for n_z in (1, 2, 5, 9, 31, 64):
            for n in (1, 2, 3, 5, 9, 20):
                got = self.z_planes(n_z, n)
                self.assertTrue(got, f'n_z={n_z} n={n} produced no planes')
                self.assertEqual(len(got), len(set(got)), f'n_z={n_z} n={n} repeated a plane')
                self.assertEqual(got, sorted(got), f'n_z={n_z} n={n} is unsorted')
                self.assertLessEqual(len(got), min(n, n_z), f'n_z={n_z} n={n} over-delivered')
                for z in got:
                    self.assertTrue(0 <= z < n_z, f'n_z={n_z} n={n} produced z={z}')

    def test_a_degenerate_count_still_returns_one_plane(self):
        """A chain or REPL caller can pass 0 or a negative; the run must not end up with no data."""
        self.assertEqual(self.z_planes(31, 0), [15])
        self.assertEqual(self.z_planes(31, -3), [15])


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class FrameWindowTest(unittest.TestCase):
    """The per-movie frame cap.

    It exists for a bias nothing in the run reported: with no cap, pooling is weighted by how long
    each recording happened to last, so a 200-frame movie contributes ~7x what a 30-frame one does
    and the model is mostly fitted to whichever image the microscope was left on longest. The pooled
    frame count is one number and says nothing about the split.
    """

    @classmethod
    def setUpClass(cls):
        cls.frame_window = staticmethod(_load_runner().frame_window)

    def test_no_cap_is_the_whole_movie(self):
        for max_frames in (0, -1, None):
            self.assertEqual(self.frame_window(200, max_frames, 42, 0), (0, 200),
                             f'max_frames={max_frames}')

    def test_a_cap_at_or_above_the_length_does_not_cut(self):
        self.assertEqual(self.frame_window(30, 30, 42, 0), (0, 30))
        self.assertEqual(self.frame_window(30, 500, 42, 0), (0, 30))

    def test_the_window_is_contiguous_and_the_requested_length(self):
        """Contiguous because the metrics are temporal: `mag_8` is the flow between t and t+8, so a
        random subset is not a shorter movie, it is a movie with the motion taken out."""
        for movie in range(6):
            start, stop = self.frame_window(200, 50, 42, movie)
            self.assertEqual(stop - start, 50)
            self.assertGreaterEqual(start, 0)
            self.assertLessEqual(stop, 200)

    def test_it_is_reproducible_from_the_seed(self):
        """The seed is in the manifest, so the window has to be recoverable from it."""
        a = [self.frame_window(200, 50, 42, i) for i in range(5)]
        b = [self.frame_window(200, 50, 42, i) for i in range(5)]
        self.assertEqual(a, b)

    def test_a_different_seed_gives_a_different_view(self):
        """Always starting at 0 samples one part of every experiment — as often as not before the
        interesting event, and at whatever bleaching level the start happens to have."""
        seeds = {self.frame_window(200, 50, s, 0)[0] for s in range(12)}
        self.assertGreater(len(seeds), 1, 'every seed produced the same start')

    def test_each_movie_is_seeded_independently(self):
        """Adding or reordering images must not reshuffle the other movies' windows — otherwise a
        re-run with one extra image is not comparable with the previous one."""
        before = [self.frame_window(200, 50, 42, i) for i in (0, 1, 2)]
        # movie 3 appended; 0-2 keep their windows
        after = [self.frame_window(200, 50, 42, i) for i in (0, 1, 2, 3)]
        self.assertEqual(before, after[:3])


if __name__ == '__main__':
    unittest.main()
