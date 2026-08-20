"""What an optical-flow training run SAMPLES — `app/src/tasks/opticalFlow/train_run.py`.

Which Z planes, which frames of each movie, and how the per-movie sequences pool into one set.

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

import numpy as np

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


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class PoolFramesTest(unittest.TestCase):
    """Pooling the per-movie sequences into one training set.

    Movies from one experiment are rarely the same size — six from zolIMa spanned 1033x1037 to
    1095x1106 — and `np.concatenate` across them raises. The whole-set run therefore has to survive
    a pool that is a flat LIST of frames rather than an array, which is what coastal's own splitter
    falls back to; a run that assumed an array crashed after ~20 minutes of flow metrics, at the
    first line that read `.shape[0]`.
    """

    @classmethod
    def setUpClass(cls):
        cls.pool = staticmethod(_load_runner().pool_frames)

    def _seq(self, n, h, w):
        return np.arange(n * h * w, dtype=np.float32).reshape(n, h, w)

    def test_same_size_movies_pool_into_one_array(self):
        """The common case must stay an array — that is what coastal documents as its input."""
        pooled = self.pool([self._seq(3, 8, 9), self._seq(4, 8, 9)])
        self.assertIsInstance(pooled, np.ndarray)
        self.assertEqual(pooled.shape, (7, 8, 9))

    def test_mixed_size_movies_pool_into_a_flat_list_of_frames(self):
        pooled = self.pool([self._seq(3, 8, 9), self._seq(4, 7, 9), self._seq(2, 8, 10)])
        self.assertEqual(len(pooled), 9)
        for frame in pooled:
            self.assertEqual(frame.ndim, 2, 'a pooled item has to be one frame, not a movie')

    def test_every_frame_survives_and_keeps_its_own_size(self):
        """No cropping to a common size: the frames reach the model as acquired."""
        a, b = self._seq(2, 8, 9), self._seq(2, 7, 9)
        pooled = self.pool([a, b])
        self.assertEqual([f.shape for f in pooled], [(8, 9), (8, 9), (7, 9), (7, 9)])
        np.testing.assert_array_equal(pooled[0], a[0])
        np.testing.assert_array_equal(pooled[3], b[1])

    def test_the_pool_counts_the_same_either_way(self):
        """`len` is the frame count in both shapes — what the log and the manifest report."""
        for sizes in (((3, 8, 9), (4, 8, 9)), ((3, 8, 9), (4, 7, 9))):
            pooled = self.pool([self._seq(*s) for s in sizes])
            self.assertEqual(len(pooled), sum(s[0] for s in sizes), f'{sizes}')
            self.assertEqual(sum(int(f.size) for f in pooled),
                             sum(s[0] * s[1] * s[2] for s in sizes), f'{sizes}')

    def test_a_single_movie_is_still_an_array(self):
        pooled = self.pool([self._seq(5, 8, 9)])
        self.assertIsInstance(pooled, np.ndarray)
        self.assertEqual(pooled.shape, (5, 8, 9))


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class ZSpacingTest(unittest.TestCase):
    """`zSpacing` — how far apart the `zPlanes` planes are. The two COMBINE.

    The first cut had spacing OVERRIDE the count, which on a real run (zolIMa, `zPlanes = 10`,
    `zSpacing = 2`, 38-deep) gave every 2nd plane of the whole stack — 19 sequences where 10 were
    asked for, i.e. twice the memory and twice the metric time, reported only as a list of indices in
    the log. The count is how many sequences a movie contributes; the spacing is how much depth they
    span. Neither is derivable from the other, so naming both has to mean both.
    """

    @classmethod
    def setUpClass(cls):
        mod = _load_runner()
        cls.z_planes = staticmethod(mod.z_planes)
        cls.cap = mod.MAX_Z_PLANES

    def test_the_count_is_honoured_at_the_requested_spacing(self):
        """The regression: 10 planes 2 apart is 10 planes, not the 19 that fit."""
        self.assertEqual(self.z_planes(38, 10, spacing=2), list(range(10, 29, 2)))
        self.assertEqual(len(self.z_planes(38, 10, spacing=2)), 10)

    def test_the_planes_are_exactly_that_far_apart(self):
        for n_z, k, n in ((38, 10, 3), (45, 5, 6), (35, 12, 2), (9, 2, 3), (38, 2, 10)):
            got = self.z_planes(n_z, n, spacing=k)
            gaps = {b - a for a, b in zip(got, got[1:])}
            self.assertIn(gaps, ({k}, set()), f'n_z={n_z} k={k} n={n} gave {got}')

    def test_the_sample_is_centred_on_the_stack(self):
        """"Somewhere in the middle" — the unsampled depth is split between top and bottom."""
        for n_z, k, n in ((38, 2, 10), (45, 7, 3), (31, 4, 5), (38, 10, 3)):
            got = self.z_planes(n_z, n, spacing=k)
            below, above = got[0], (n_z - 1) - got[-1]
            self.assertLessEqual(abs(below - above), 1, f'n_z={n_z} k={k} n={n} gave {got}')

    def test_every_plane_is_inside_the_stack(self):
        for n_z in (1, 2, 9, 31, 45):
            for k in (1, 2, 5, 10, 25):
                for n in (1, 3, 10, 40):
                    got = self.z_planes(n_z, n, spacing=k)
                    self.assertTrue(got, f'n_z={n_z} k={k} n={n} produced no planes')
                    self.assertEqual(len(got), len(set(got)), f'n_z={n_z} k={k} n={n} repeated')
                    for z in got:
                        self.assertTrue(0 <= z < n_z, f'n_z={n_z} k={k} n={n} produced z={z}')

    def test_a_count_that_fits_keeps_off_the_top_and_bottom_plane(self):
        """Same property the count rule has: the ends of a stack are usually outside the tissue."""
        for n_z, k, n in ((38, 10, 3), (9, 2, 3), (45, 5, 8), (31, 4, 6), (38, 2, 10)):
            got = self.z_planes(n_z, n, spacing=k)
            self.assertNotIn(0, got, f'n_z={n_z} k={k} n={n} reached the bottom plane')
            self.assertNotIn(n_z - 1, got, f'n_z={n_z} k={k} n={n} reached the top plane')

    def test_more_planes_than_fit_are_clamped_not_wrapped(self):
        """10 planes 2 apart do not fit in a 9-deep stack — take fewer, never read past the end."""
        got = self.z_planes(9, 10, spacing=2)
        self.assertEqual(got, [1, 3, 5, 7])       # the 4 that fit, still clear of both ends
        self.assertLess(len(got), 10)

    def test_a_runaway_count_is_capped(self):
        """A REPL caller is not bound by the form's max; 45 sequences is 45x a single-plane run."""
        self.assertEqual(len(self.z_planes(45, 100, spacing=1)), self.cap)

    def test_zero_or_negative_spacing_is_the_count_rule_untouched(self):
        for spacing in (0, -1, None):
            self.assertEqual(self.z_planes(38, 3, spacing=spacing), self.z_planes(38, 3),
                             f'spacing={spacing}')

    def test_a_spacing_deeper_than_the_stack_still_gives_the_middle_plane(self):
        self.assertEqual(self.z_planes(9, 10, spacing=40), [4])


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class CropWindowTest(unittest.TestCase):
    """`cropSize` — the random XY window each sequence is trained on.

    The only knob that DIVIDES the whole cost (a 512 window of a 1046×1104 field is 22% of the
    pixels, so 22% of the metric memory and of the Farneback time). Two properties carry it: the
    position is random per sequence — a fixed window would make the model's whole experience of every
    movie the same patch — and it stays off the border, where an intravital frame is routinely
    outside the specimen and Farneback has nothing beyond the edge to match.
    """

    @classmethod
    def setUpClass(cls):
        mod = _load_runner()
        cls.crop_window = staticmethod(mod.crop_window)
        cls.border = mod.CROP_BORDER_FRAC

    def _rng(self, seed=0):
        return np.random.default_rng(seed)

    def test_zero_means_the_whole_frame(self):
        for size in (0, -1, None):
            self.assertIsNone(self.crop_window((1046, 1104), size, self._rng()), f'size={size}')

    def test_a_window_at_least_as_big_as_the_frame_is_the_whole_frame(self):
        self.assertIsNone(self.crop_window((512, 480), 512, self._rng()))
        self.assertIsNone(self.crop_window((300, 400), 768, self._rng()))

    def test_the_window_is_the_requested_size_and_inside_the_frame(self):
        for seed in range(20):
            y0, x0, h, w = self.crop_window((1046, 1104), 512, self._rng(seed))
            self.assertEqual((h, w), (512, 512))
            self.assertGreaterEqual(y0, 0)
            self.assertGreaterEqual(x0, 0)
            self.assertLessEqual(y0 + h, 1046)
            self.assertLessEqual(x0 + w, 1104)

    def test_it_keeps_clear_of_the_border(self):
        for seed in range(20):
            y0, x0, h, w = self.crop_window((1046, 1104), 512, self._rng(seed))
            self.assertGreaterEqual(y0, int(1046 * self.border))
            self.assertGreaterEqual(x0, int(1104 * self.border))
            self.assertLessEqual(y0 + h, 1046 - int(1046 * self.border))
            self.assertLessEqual(x0 + w, 1104 - int(1104 * self.border))

    def test_the_position_actually_varies(self):
        """A fixed window would train every movie on the same patch of field."""
        seen = {self.crop_window((1046, 1104), 512, self._rng(s))[:2] for s in range(12)}
        self.assertGreater(len(seen), 1, 'every seed produced the same window')

    def test_it_is_reproducible_from_the_seed(self):
        self.assertEqual(self.crop_window((1046, 1104), 512, self._rng(7)),
                         self.crop_window((1046, 1104), 512, self._rng(7)))

    def test_a_window_that_leaves_no_room_shrinks_the_margin_rather_than_failing(self):
        """768 of a 780 px axis cannot honour a 10% margin; it must still return a usable window."""
        y0, x0, h, w = self.crop_window((780, 1104), 768, self._rng())
        self.assertEqual(h, 768)
        self.assertGreaterEqual(y0, 0)
        self.assertLessEqual(y0 + h, 780)

    def test_it_crops_only_the_axis_that_is_bigger_than_the_window(self):
        y0, x0, h, w = self.crop_window((700, 1039), 768, self._rng())
        self.assertEqual((h, w), (700, 768))
        self.assertEqual(y0, 0)


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class ReduceMetricsTest(unittest.TestCase):
    """What the run HOLDS per frame — the largest allocation in the task.

    Six zolIMa movies at 60 frames measured 23 GB of metric planes held for the whole run, on a 31 GB
    box: an OOM kill, and nothing about the failure would have named the metrics. Two reductions get
    it to 9.3 GB, and both are only worth anything if they happen at the point of production — a
    filtered dict SHARES its arrays, so dropping late frees nothing.
    """

    @classmethod
    def setUpClass(cls):
        mod = _load_runner()
        cls.reduce = staticmethod(mod.reduce_metrics)
        cls.dtype = mod.METRIC_DTYPE

    def _frames(self, n=3, keys=('mag_1', 'vorticity', 'divergence')):
        rng = np.random.default_rng(0)
        return [{k: (rng.random((6, 7)) * 30).astype(np.float32) for k in keys} for _ in range(n)]

    def test_dropped_metrics_are_gone_from_every_frame(self):
        out = self.reduce(self._frames(), ('vorticity', 'divergence'))
        self.assertEqual([sorted(mm) for mm in out], [['mag_1']] * 3)

    def test_nothing_is_dropped_when_nothing_was_asked_for(self):
        out = self.reduce(self._frames(), ())
        self.assertEqual(sorted(out[0]), ['divergence', 'mag_1', 'vorticity'])

    def test_what_survives_is_held_as_the_storage_dtype(self):
        out = self.reduce(self._frames(), ('vorticity',))
        for mm in out:
            for name, arr in mm.items():
                self.assertEqual(arr.dtype, np.dtype(self.dtype), name)

    def test_it_halves_what_is_held(self):
        """The measurable claim: same frames, half the bytes, minus the dropped planes."""
        frames = self._frames()
        before = sum(a.nbytes for mm in frames for a in mm.values())
        after = sum(a.nbytes for mm in self.reduce(frames, ('vorticity',)) for a in mm.values())
        self.assertEqual(after, before * 2 // 3 // 2)

    def test_the_values_survive_the_cast(self):
        """Storage, not arithmetic — float16 carries ~3 decimal digits and these are flow magnitudes."""
        frames = self._frames(n=1, keys=('cumulative_mag',))
        out = self.reduce(frames, ())
        np.testing.assert_allclose(out[0]['cumulative_mag'], frames[0]['cumulative_mag'],
                                   rtol=1e-3)

    def test_an_already_reduced_dict_is_not_copied_again(self):
        """`copy=False` — a re-reduce (or a coastal that ever returns float16) must not double it."""
        once = self.reduce(self._frames(n=1), ())
        twice = self.reduce(once, ())
        for name, arr in twice[0].items():
            self.assertIs(arr, once[0][name], name)

    def test_an_empty_sequence_is_not_an_error(self):
        self.assertEqual(self.reduce([], ('vorticity',)), [])


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class SplitFloorsTest(unittest.TestCase):
    """`lossFloors` is a SEPARATE manifest key, not entries inside `lossCurves`.

    coastal returns one flat history — `foreground`, `val_foreground`, `floor_foreground`,
    `val_floor_foreground` — because that is what its accumulation loop produces. The frontend draws
    one line per entry of `lossCurves`, so leaving the floors in it draws three extra "terms" that
    are not terms; and the frontend joins a floor to its curve by KEY, so the `val_` has to come back
    to the front.
    """

    def setUp(self):
        self.split = _load_runner()._split_floors

    def test_it_lifts_the_floors_out_and_keys_them_like_their_curves(self):
        curves, floors = self.split({
            'foreground': [0.30, 0.27], 'val_foreground': [0.31, 0.28],
            'floor_foreground': [0.26, 0.26], 'val_floor_foreground': [0.25, 0.25],
        })
        self.assertEqual(sorted(curves), ['foreground', 'val_foreground'])
        self.assertEqual(sorted(floors), ['foreground', 'val_foreground'])
        self.assertEqual(floors['val_foreground'], [0.25, 0.25])

    def test_the_terms_are_left_alone(self):
        curves, floors = self.split({'total': [1.0], 'temporal': [0.5], 'val_total': [1.1]})
        self.assertEqual(sorted(curves), ['temporal', 'total', 'val_total'])
        self.assertEqual(floors, {})

    def test_a_term_that_merely_starts_with_val_is_not_mistaken_for_a_split(self):
        """`variance` begins with no prefix of interest; the guard is on `val_`/`floor_` exactly."""
        curves, floors = self.split({'variance': [0.9], 'validation_thing': [0.1]})
        self.assertEqual(sorted(curves), ['validation_thing', 'variance'])
        self.assertEqual(floors, {})

    def test_an_empty_history_gives_two_empty_dicts(self):
        self.assertEqual(self.split({}), ({}, {}))


if __name__ == '__main__':
    unittest.main()
