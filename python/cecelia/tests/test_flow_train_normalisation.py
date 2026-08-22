"""What a flow-training sequence is NORMALISED by — `_training_sequence` in `opticalFlow/train_run.py`.

The invariant, and the only reason this function is shaped the way it is: the percentiles are taken
over the WHOLE plane sequence — every timepoint and every pixel — while the array returned is only
the frame window and the XY crop. That is the statistic inference reproduces from the image, so
normalising the crop by its own range would fit the model to a photometric scale nothing ever gives
it back, and the failure is silent: the same structure at a different brightness, training fine.

Cutting before the scale-and-clip is a pure optimisation of that ordering — the two are elementwise
and commute with slicing exactly — so the tests assert EQUALITY against a reference that normalises
everything and cuts last, not approximate agreement. Same for the cache: a hit must produce the
identical array a cold run does, or the first run of a campaign and every rerun of it disagree.

One thing genuinely did change, and it is a CORRECTION rather than a rounding difference. The
percentiles now come from the raw integers instead of a float32 copy, and `np.percentile` does its
interpolation arithmetic in the input's dtype. At a virtual index of `(n-1) * q` — ~5.7e6 for one
plane sequence of real data — float32's spacing is 0.5, so the old path quantised the interpolation
weight to halves and could land half an intensity unit off. Measured over ten planes x two channels
of `zolIMa/fXgbTl`: nineteen pairs agree exactly (equal neighbours, so the weight cannot matter), one
does not (`hi` 150.818 against 150.5), and that plane's output moves on 3.8% of pixels by at most
0.53 of 255. Tests pin both the agreement and the mechanism.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import unittest
from pathlib import Path

import numpy as np

from cecelia.utils import coastal_utils, norm_cache

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'opticalFlow' / 'train_run.py')


def _load_runner():
    """Load the runner from its path, exactly as `run_py` does (it is not an importable module)."""
    spec = importlib.util.spec_from_file_location('flow_train_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _Dims:
    """The two things `_training_sequence` asks of `DimUtils`. A stub rather than a real one so the
    fixture can be tiny and its axis order explicit — the real class needs an OME document."""

    def __init__(self, order, shape):
        self.im_dim_order = list(order)
        self._vals = dict(zip(self.im_dim_order, shape))

    def dim_val(self, ax):
        return self._vals[ax]


def _fixture(order='TCZYX', shape=(7, 3, 4, 9, 6), seed=3):
    """Distinct per-channel ranges, so a channel mix-up changes the numbers rather than hiding."""
    rng = np.random.default_rng(seed)
    arr = np.zeros(shape, dtype=np.uint16)
    c = order.index('C')
    for ch in range(shape[c]):
        idx = [slice(None)] * len(shape)
        idx[c] = ch
        arr[tuple(idx)] = rng.integers(0, 200 * (ch + 1), size=arr[tuple(idx)].shape)
    return arr, _Dims(order, shape)


def _reference(level, dims, channels, pct, z, window, crop, as_float32=False):
    """Percentile over EVERYTHING, then cut — the ordering the docstring promises, spelled out.

    `as_float32` reproduces the ordering as it was BEFORE the crop moved earlier: the percentiles
    taken from a float32 copy of the plane rather than from the raw integers. It is the only source
    of difference between the two versions, and it gets its own test.
    """
    ia = {ax: i for i, ax in enumerate(dims.im_dim_order)}
    projected = None
    for ch in channels:
        idx = [slice(None)] * level.ndim
        idx[ia['C']] = ch
        if z is not None:
            idx[ia['Z']] = z
        remaining = [ax for ax in dims.im_dim_order
                     if ax != 'C' and not (z is not None and ax == 'Z')]
        raw = np.moveaxis(np.asarray(level[tuple(idx)]), remaining.index('T'), 0)
        src = raw.astype(np.float32) if as_float32 else raw
        lo = float(np.percentile(src, 100 - pct))
        hi = float(np.percentile(src, pct))
        arr = np.clip((raw.astype(np.float32) - lo) / (hi - lo + 1e-8), 0.0, 1.0)
        projected = arr if projected is None else np.maximum(projected, arr)
    if window is not None:
        projected = projected[window[0]:window[1]]
    if crop is not None:
        y0, x0, hh, ww = crop
        projected = projected[:, y0:y0 + hh, x0:x0 + ww]
    return (projected * coastal_utils.PROJECTION_MAX).astype(np.float32)


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (IO-library-only install)')
class TrainingSequenceTest(unittest.TestCase):
    PARAMS = {'trainChannels': [2, 0], 'normalise': 99.0}
    WINDOW = (2, 6)
    CROP = (1, 2, 5, 3)

    @classmethod
    def setUpClass(cls):
        cls.seq = staticmethod(_load_runner()._training_sequence)

    def _call(self, level, dims, z=1, window=WINDOW, crop=CROP, stats=None):
        return self.seq([level], dims, self.PARAMS, z, window, crop, stats)

    def test_cutting_early_gives_the_reference_numbers_exactly(self):
        """The whole optimisation in one assertion: scale and clip are elementwise, so slicing first
        cannot change a value. `assert_array_equal`, not `allclose` — a tolerance here would hide
        exactly the reordering mistake it is meant to catch."""
        level, dims = _fixture()
        want = _reference(level, dims, self.PARAMS['trainChannels'],
                          self.PARAMS['normalise'], 1, self.WINDOW, self.CROP)
        np.testing.assert_array_equal(self._call(level, dims), want)

    def test_a_cache_hit_returns_the_identical_array(self):
        """Otherwise the first run of a training campaign and every rerun of it are fitted to
        different data, which is worse than no cache at all."""
        level, dims = _fixture()
        stats = {}
        cold = self._call(level, dims, stats=stats)
        self.assertTrue(stats, 'the cold run must have written its ranges through')
        warm = self._call(level, dims, stats=stats)
        np.testing.assert_array_equal(cold, warm)

    def test_the_cached_range_is_actually_used(self):
        """The guard against a silently ineffective cache: a hit that recomputed anyway would pass
        every other test here. Plant a wrong range and the output must change."""
        level, dims = _fixture()
        stats = {}
        cold = self._call(level, dims, stats=stats)
        planted = {k: (v[0], v[1] * 4 + 10) for k, v in stats.items()}
        warm = self._call(level, dims, stats=planted)
        self.assertFalse(np.array_equal(cold, warm),
                         'the cached (lo, hi) was ignored — the cache is a no-op')

    def test_the_range_does_not_depend_on_the_window_or_the_crop(self):
        """THE invariant. A range that moved with the window would make the model's photometric scale
        a function of the frame cap and of where a random crop landed."""
        level, dims = _fixture()
        whole, windowed, cropped = {}, {}, {}
        self._call(level, dims, window=None, crop=None, stats=whole)
        self._call(level, dims, window=(0, 3), crop=None, stats=windowed)
        self._call(level, dims, window=self.WINDOW, crop=(0, 0, 2, 2), stats=cropped)
        self.assertEqual(whole, windowed)
        self.assertEqual(whole, cropped)

    def test_the_keys_name_the_channel_and_the_plane(self):
        level, dims = _fixture()
        stats = {}
        self._call(level, dims, z=1, stats=stats)
        self.assertEqual(set(stats), {norm_cache.key(2, 1, 99.0), norm_cache.key(0, 1, 99.0)})

    def test_each_plane_gets_its_own_range(self):
        """Per-plane is deliberate (see the docstring), so two planes of one stack must not share a
        key — and on real data they differ by up to 2x with depth."""
        level, dims = _fixture()
        stats = {}
        self._call(level, dims, z=0, stats=stats)
        self._call(level, dims, z=2, stats=stats)
        self.assertEqual(len(stats), 4, 'two planes x two channels')

    def test_a_movie_with_no_z_axis_works(self):
        level, dims = _fixture(order='TCYX', shape=(7, 3, 9, 6))
        want = _reference(level, dims, self.PARAMS['trainChannels'],
                          self.PARAMS['normalise'], None, self.WINDOW, self.CROP)
        stats = {}
        np.testing.assert_array_equal(
            self._call(level, dims, z=None, stats=stats), want)
        self.assertIn(norm_cache.key(2, None, 99.0), stats)

    def test_no_cache_at_all_still_gives_the_reference(self):
        """`stats=None` is a supported mode, not a code path nobody takes — a store that cannot be
        fingerprinted uses it."""
        level, dims = _fixture()
        want = _reference(level, dims, self.PARAMS['trainChannels'],
                          self.PARAMS['normalise'], 1, self.WINDOW, self.CROP)
        np.testing.assert_array_equal(self._call(level, dims, stats=None), want)

    def test_the_whole_plane_is_returned_when_nothing_is_cut(self):
        level, dims = _fixture()
        out = self._call(level, dims, window=None, crop=None)
        self.assertEqual(out.shape, (7, 9, 6))

    def test_a_crop_that_the_window_only_partly_covers_is_sliced_not_wrapped(self):
        """A crop reaching the far edge must clip to the axis, never wrap around to the near one —
        `crop_window` pads off the border, but the slicing here must not depend on that."""
        level, dims = _fixture()
        out = self._call(level, dims, crop=(4, 3, 5, 3))
        self.assertEqual(out.shape, (4, 5, 3))


    def test_the_percentile_is_taken_at_full_precision_not_float32(self):
        """The ONE numeric difference from the pre-crop-early version, pinned deliberately.

        The old code percentiled a float32 copy; this one percentiles the raw integers. The gap is
        bounded but NOT negligible — see `Float32PercentileIndexTest` below for why — so this asserts
        a bound on the whole array rather than waving it away as floating point.
        """
        level, dims = _fixture()
        args = (level, dims, self.PARAMS['trainChannels'], self.PARAMS['normalise'],
                1, self.WINDOW, self.CROP)
        old = _reference(*args, as_float32=True)
        np.testing.assert_allclose(self._call(level, dims), old, rtol=1e-2, atol=1.0)

    def test_it_is_bit_identical_where_the_percentile_lands_on_a_repeated_value(self):
        """Which is the case for real 16-bit microscopy, and the case that matters: 200 M samples over
        a few hundred distinct values put both percentile neighbours on the same integer, so there is
        nothing to interpolate and the precision of the interpolation cannot matter. Measured on
        `zolIMa/VJy1Nx` — lo 0.0, hi 351.0 from both, and the two arrays compared equal.
        """
        # 8 distinct values over 378 samples per channel: every percentile neighbour is a repeat.
        rng = np.random.default_rng(11)
        level = rng.integers(0, 8, size=(7, 3, 4, 9, 6)).astype(np.uint16)
        dims = _Dims('TCZYX', level.shape)
        args = (level, dims, self.PARAMS['trainChannels'], self.PARAMS['normalise'],
                1, self.WINDOW, self.CROP)
        np.testing.assert_array_equal(self._call(level, dims), _reference(*args, as_float32=True))


class Float32PercentileIndexTest(unittest.TestCase):
    """WHY percentiling a float32 copy was worth changing — the mechanism, not a tolerance.

    `np.percentile` interpolates between two order statistics at a virtual index of `(n - 1) * q`,
    and does that arithmetic in the INPUT array's dtype. Past ~2^23 elements a float32 cannot hold
    that index to better than half a step, so the interpolation WEIGHT is quantised and the answer
    can miss the true percentile by up to half the gap between the two neighbouring values.

    Real data hits this: over ten planes x two channels of `zolIMa/fXgbTl` at 99.99, one pair came
    out `hi` 150.818 from the integers and 150.5 from a float32 copy. Nineteen agreed exactly,
    because their neighbours were the same integer and then no weight can matter — which is why this
    was invisible on the first movie measured and needs a test rather than a spot check.
    """

    def test_a_float32_copy_quantises_the_interpolation_weight(self):
        n = 4_000_000                      # (n-1)*q ~ 4e6, where float32 spacing is 0.5
        pct = 99.99
        rng = np.random.default_rng(5)
        raw = rng.integers(0, 40, size=n).astype(np.uint16)
        # Force the two neighbours at the cut to DIFFER, which is the only case that shows it.
        k = int(np.floor((n - 1) * pct / 100.0))
        raw.sort()
        raw[k], raw[k + 1:] = 150, 151
        from_int = float(np.percentile(raw, pct))
        from_f32 = float(np.percentile(raw.astype(np.float32), pct))
        self.assertNotEqual(from_int, from_f32,
                            'if these agree the mechanism is gone and the change is pointless')
        self.assertLess(abs(from_int - from_f32), 1.0,
                        'and it must stay bounded by the gap between the two neighbours')

    def test_equal_neighbours_make_the_two_agree_exactly(self):
        """The nineteen-of-twenty case, and the reason the first movie measured showed no difference
        at all: interpolating between two equal values gives that value for any weight."""
        n = 4_000_000
        raw = np.full(n, 7, dtype=np.uint16)
        raw[: n // 100] = 3
        self.assertEqual(float(np.percentile(raw, 99.99)),
                         float(np.percentile(raw.astype(np.float32), 99.99)))


if __name__ == '__main__':
    unittest.main()
