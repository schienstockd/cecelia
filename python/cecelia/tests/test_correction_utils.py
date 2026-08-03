"""Equivalence tests for the streaming (out=) correction paths.

The drift / AF / cellpose correction tasks used to allocate the ENTIRE corrected image as one
in-RAM numpy array (drift), or build every channel + a concatenate copy (AF), which OOMs on large
time-lapses. They now stream one timepoint / one channel at a time into an on-disk zarr. These tests
pin that the streaming output is BYTE-IDENTICAL to the legacy in-RAM output — the in-RAM path is kept
as the reference oracle (out=None) and asserted equal to the streamed path (out=<zarr>).

Also covers the shared `write_multiscale_pyramid` refactor (create_multiscales now builds the pyramid
one timepoint at a time). Part of the Python analysis-env suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
import dask.array as da
import zarr
import ome_types

import cecelia.utils.zarr_utils as zu
import cecelia.utils.correction_utils as cu
import cecelia.utils.intensity_utils as iu
import cecelia.utils.intensity_utils as intensity_utils
from cecelia.utils.dim_utils import DimUtils


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeZ="{size_z}" SizeC="{size_c}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm"
            PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      {''.join(f'<Channel ID="Channel:0:{c}" SamplesPerPixel="1"/>' for c in range(size_c))}
    </Pixels>
  </Image>
</OME>"""


def _dim_utils(size_t, size_z, size_c, size_y, size_x):
    """DimUtils for the given sizes (size-1 axes are dropped from im_dim_order). Sizes are chosen
    distinct by the callers so the axis order resolves unambiguously."""
    du = DimUtils(ome_types.from_xml(_ome_xml(size_t, size_z, size_c, size_y, size_x)),
                  use_channel_axis=True)
    shape = [s for s in (size_t, size_z, size_c, size_y, size_x) if s != 1]
    du.calc_image_dimensions(tuple(shape))
    return du


class DriftStreamingEquivalenceTest(unittest.TestCase):
    def _run(self, du, shifts):
        rng = np.random.default_rng(0)
        arr = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)

        # reference: legacy in-RAM path
        legacy = cu.drift_correct_im(arr, du, 0, shifts=shifts, out=None)

        # streamed path into an on-disk zarr level 0
        d = tempfile.mkdtemp()
        try:
            out_shape, _ = cu.drift_correct_shape(arr, du, shifts)
            self.assertEqual(tuple(out_shape), tuple(legacy.shape))
            path = os.path.join(d, "drift.ome.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(
                path, out_shape, arr.dtype, du, nscales=1)   # writer forces native byte order
            cu.drift_correct_im(arr, du, 0, shifts=shifts, out=level0)
            streamed = zarr.open_group(path, mode="r")["0"][:]
        finally:
            shutil.rmtree(d, ignore_errors=True)

        self.assertTrue(np.array_equal(legacy, streamed),
                        "streamed drift output differs from the in-RAM reference")

    def test_2d_timeseries(self):
        du = _dim_utils(size_t=5, size_z=1, size_c=2, size_y=17, size_x=13)
        shifts = np.array([[1, 2], [-1, 0], [2, -2], [0, 1]], dtype=float)   # [T-1, (Y,X)]
        self._run(du, shifts)

    def test_3d_timeseries(self):
        du = _dim_utils(size_t=4, size_z=3, size_c=2, size_y=15, size_x=11)
        shifts = np.array([[1, 1, 2], [0, -1, 1], [1, 2, -1]], dtype=float)  # [T-1, (Z,Y,X)]
        self._run(du, shifts)


class AfStreamingEquivalenceTest(unittest.TestCase):
    """Streamed (on-disk `out=`) AF output must equal the in-RAM reference byte for byte.

    The four tests that used to live here exercised parameters that no longer exist — an inverse
    channel, a rolling ball, a top hat, two denoisers, a gaussian — and one pinned the output sum
    against a golden captured from the pre-streaming implementation. They were **deleted rather than
    repaired**: a channel combination is now just channels, everything else is derived
    (`af_division_stats`), and the golden described a method that was deliberately replaced. Pinning
    the old numbers would have pinned the behaviour we set out to remove.

    What survives is the property that still means something: streaming to disk must not change the
    result. The method itself is pinned by `AfPreviewSeamTest` (the run and the preview agree) and by
    `AfDerivedValuesTest` (the derivations are what they claim).
    """

    def _run(self, du, af_combinations, background_method='triangle'):
        rng = np.random.default_rng(1)
        base = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)

        class _Log:
            def log(self, *_a, **_k):
                pass

        legacy = np.asarray(cu.af_correct_image(
            base.copy(), af_combinations, dim_utils=du, logfile_utils=_Log(),
            background_method=background_method, out=None))

        d = tempfile.mkdtemp()
        try:
            out_shape = cu.af_correction_output_shape(base, du)
            self.assertEqual(tuple(out_shape), tuple(legacy.shape))
            path = os.path.join(d, "af.ome.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(
                path, out_shape, base.dtype, du, nscales=1)   # writer forces native byte order
            cu.af_correct_image(
                base.copy(), af_combinations, dim_utils=du, logfile_utils=_Log(),
                background_method=background_method, out=level0)
            streamed = zarr.open_group(path, mode="r")["0"][:]
        finally:
            shutil.rmtree(d, ignore_errors=True)

        self.assertTrue(np.array_equal(legacy, streamed),
                        "streamed AF output differs from the in-RAM reference")
        return legacy

    def test_divide_and_passthrough(self):
        # ch0 corrected against ch1; ch1 covered by no combination, so carried through UNCHANGED
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=19, size_x=14)
        out = self._run(du, {"0": {"divisionChannels": [1]}})
        rng = np.random.default_rng(1)
        base = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        c = du.dim_idx('C')
        sl = tuple(slice(1, 2) if i == c else slice(None) for i in range(base.ndim))
        self.assertTrue(np.array_equal(out[sl], base[sl]),
                        "an uncorrected channel must be carried through untouched")

    def test_output_has_the_same_channels_as_the_input(self):
        # `generateInverse` used to widen the channel axis; nothing does now
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=19, size_x=14)
        base = np.zeros(tuple(du.im_dim), dtype=np.uint16)
        self.assertEqual(cu.af_correction_output_shape(base, du), tuple(base.shape))

    def test_every_background_method_runs(self):
        du = _dim_utils(size_t=2, size_z=1, size_c=2, size_y=16, size_x=12)
        for m in intensity_utils.BACKGROUND_METHODS:
            self._run(du, {"0": {"divisionChannels": [1]}}, background_method=m)

    def test_an_unknown_background_method_is_refused(self):
        du = _dim_utils(size_t=2, size_z=1, size_c=2, size_y=16, size_x=12)
        with self.assertRaises(ValueError):
            self._run(du, {"0": {"divisionChannels": [1]}}, background_method='nope')


class SourceHandleAgnosticTest(unittest.TestCase):
    """The streaming correction utils must not care whether they are handed a zarr or a dask array.

    The four streaming runners (af / drift / cellpose_correct / measure_labels) used to open their
    input with `as_dask=True` and then read every frame through `fortify(arr[slice])` anyway, so the
    dask handle only added graph overhead. They now pass `as_dask=False`. Measured on a real store
    (zolIMa/ldYr8J): `af_correct_image` **278.7 s → 30.1 s** (9.3×, because AF re-reads each slab
    across three passes) and a per-timepoint `copy_stream` 4.07 s → 1.62 s (2.5×).

    The runners have no tests of their own, so the guard lives here: identical bytes from either
    handle. If that ever stops holding, the flip is not safe.
    """

    def _sources(self, arr):
        """The same data as a zarr array and as a dask array — both independent copies, so an
        in-place op in one path cannot influence the other."""
        d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, d, ignore_errors=True)
        z = zarr.create_array(store=os.path.join(d, 'src.zarr'), shape=arr.shape,
                              dtype=arr.dtype, chunks=arr.shape)
        z[:] = arr
        return z, da.from_array(arr.copy(), chunks=arr.shape)

    def test_af_correct_image_is_identical_from_either_handle(self):
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=19, size_x=14)
        rng = np.random.default_rng(3)
        arr = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        combos = {"0": {"divisionChannels": [1]}}

        class _Log:
            def log(self, *_a, **_k): pass

        z, dk = self._sources(arr)
        out = [np.asarray(cu.af_correct_image(src, combos, dim_utils=du, logfile_utils=_Log(),
                                              out=None))
               for src in (z, dk)]
        self.assertTrue(np.array_equal(*out), 'AF output depends on the source handle type')

    def test_drift_correct_im_is_identical_from_either_handle(self):
        du = _dim_utils(size_t=4, size_z=1, size_c=2, size_y=21, size_x=17)
        rng = np.random.default_rng(4)
        arr = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        z, dk = self._sources(arr)
        # shifts are themselves read through the array, so derive them per handle too
        out = [cu.drift_correct_im(src, du, 0,
                                   shifts=cu.drift_correction_shifts(src, 0, du), out=None)
               for src in (z, dk)]
        self.assertTrue(np.array_equal(np.asarray(out[0]), np.asarray(out[1])),
                        'drift output depends on the source handle type')

    def test_copy_stream_is_identical_from_either_handle(self):
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=19, size_x=14)
        rng = np.random.default_rng(5)
        arr = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        z, dk = self._sources(arr)
        d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, d, ignore_errors=True)
        res = []
        for i, src in enumerate((z, dk)):
            dest = zarr.create_array(store=os.path.join(d, f'out{i}.zarr'), shape=arr.shape,
                                     dtype=arr.dtype, chunks=zu.plane_chunks(arr.shape, du))
            zu.copy_stream(dest, src, du)
            res.append(dest[:])
        self.assertTrue(np.array_equal(*res), 'copy_stream depends on the source handle type')
        self.assertTrue(np.array_equal(res[0], arr))       # and it is a faithful copy


class AfPreviewSeamTest(unittest.TestCase):
    """`af_division_stats` + `af_correct_frame` composed must BE the run.

    The task preview corrects only the region on screen, which is possible because the global values
    (both background levels and the output ceiling) are separable from the per-voxel work. That split
    is the whole feature, and it is only worth anything if the two halves recombine into exactly what a
    run produces — otherwise the preview shows a result the run won't reproduce, which is the one thing
    it exists to avoid.
    """

    SHAPE = dict(size_t=3, size_z=5, size_c=4, size_y=32, size_x=30)

    def _data(self, du, seed=11):
        return np.random.default_rng(seed).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)

    def test_the_preview_sequence_reproduces_the_run(self):
        du = _dim_utils(**self.SHAPE)
        data = self._data(du)
        c_axis, t_axis = du.dim_idx('C'), du.dim_idx('T')
        for method in intensity_utils.BACKGROUND_METHODS:
            run_out = np.zeros(data.shape, data.dtype)
            cu._stream_division_channel(data, run_out, du, channel_idx=1, out_ch=1,
                                        correction_channel_idx=[3], background_method=method)

            # ── what the preview does: stats once, then the per-voxel part per region ──
            stats = cu.af_division_stats(data, du, 1, [3], background_method=method)
            for t in range(du.dim_val('T')):
                corrected = cu.af_correct_frame(
                    cu._af_slab(data, du, 1, t),
                    cu._af_correction_slab(data, du, [3], t, 'maximum', 75),
                    stats, data.dtype)
                sl = [slice(None)] * data.ndim
                sl[t_axis] = slice(t, t + 1)
                sl[c_axis] = slice(1, 2)
                self.assertTrue(np.array_equal(run_out[tuple(sl)], corrected),
                                f'preview differs from the run: method={method} t={t}')

    def test_a_region_is_corrected_exactly_as_its_slice_of_the_whole_frame(self):
        """THE property the preview rests on: correcting a crop == cropping the correction.

        True only because every global value is passed in rather than derived from what is visible.
        Deriving them from the crop is the mistake this test exists to catch.
        """
        du = _dim_utils(**self.SHAPE)
        data = self._data(du, seed=12)
        stats = cu.af_division_stats(data, du, 1, [3])
        y, x = du.dim_idx('Y'), du.dim_idx('X')
        full = cu.af_correct_frame(cu._af_slab(data, du, 1, 0),
                                   cu._af_correction_slab(data, du, [3], 0, 'maximum', 75),
                                   stats, data.dtype)
        box = [slice(None)] * data.ndim
        box[y] = slice(8, 20)
        box[x] = slice(5, 25)
        crop = cu.af_correct_frame(cu._af_slab(data, du, 1, 0)[tuple(box)],
                                   cu._af_correction_slab(data, du, [3], 0, 'maximum', 75)[tuple(box)],
                                   stats, data.dtype)
        self.assertTrue(np.array_equal(full[tuple(box)], crop),
                        'a previewed region must equal that region of the full correction')

    def test_reusing_stats_is_what_makes_them_reusable(self):
        # the preview caches stats across parameter changes; passing them in must be equivalent to
        # letting the writer compute its own
        du = _dim_utils(**self.SHAPE)
        data = self._data(du, seed=13)
        a = np.zeros(data.shape, data.dtype)
        b = np.zeros(data.shape, data.dtype)
        cu._stream_division_channel(data, a, du, channel_idx=1, out_ch=1, correction_channel_idx=[3])
        stats = cu.af_division_stats(data, du, 1, [3])
        cu._stream_division_channel(data, b, du, channel_idx=1, out_ch=1, correction_channel_idx=[3],
                                    stats=stats)
        self.assertTrue(np.array_equal(a, b))


class AfDerivedValuesTest(unittest.TestCase):
    """The three values that used to be dialled in are now derived — pin what they are.

    Replaces `channelPercentile`, `correctionPercentile`, `correctionMin` and `correctionMax`.
    """

    SHAPE = dict(size_t=3, size_z=5, size_c=4, size_y=32, size_x=30)

    def test_none_means_no_background_subtraction(self):
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(1).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        s = cu.af_division_stats(data, du, 1, [3], background_method='none')
        self.assertEqual(s.val1, 0.0)
        self.assertEqual(s.val2, 0.0)

    def test_the_ceiling_never_collapses_into_the_background(self):
        # the trap on `robust_hist_max`: the background bin dominates, so an over-large count would
        # return a ceiling inside it and make the rescale degenerate
        du = _dim_utils(**self.SHAPE)
        data = np.zeros(tuple(du.im_dim), dtype=np.uint16)   # every voxel identical
        s = cu.af_division_stats(data, du, 1, [3], ceiling_min_count=10 ** 9)
        self.assertGreater(s.c_max, 0.0)
        self.assertGreaterEqual(s.c_max, s.nbins * cu.AF_CEILING_FLOOR_FRAC)

    def test_a_lone_hot_voxel_does_not_set_the_ceiling(self):
        """The measured failure: on a real 181-frame movie the top six occupied ratio bins held ONE
        voxel each, so one voxel in 5.88 billion set the output scale."""
        du = _dim_utils(**self.SHAPE)
        data = np.zeros(tuple(du.im_dim), dtype=np.uint16)
        c = du.dim_idx('C')
        # a broad dim signal in the target channel, no AF anywhere -> ratio ~= signal
        sl = [slice(None)] * data.ndim
        sl[c] = slice(1, 2)
        data[tuple(sl)] = 40
        clean = cu.af_division_stats(data, du, 1, [3], background_method='none',
                                     ceiling_min_count=100)
        # now plant ONE hot voxel, orders of magnitude brighter
        hot = [0] * data.ndim
        hot[c] = 1
        data[tuple(hot)] = 65535
        with_hot = cu.af_division_stats(data, du, 1, [3], background_method='none',
                                        ceiling_min_count=100)
        self.assertEqual(clean.c_max, with_hot.c_max,
                         'one hot voxel moved the derived ceiling')
        # ...whereas the true max would have been dragged all the way up
        self.assertGreater(cu.af_division_stats(data, du, 1, [3], background_method='none',
                                                ceiling_min_count=1).c_max,
                           with_hot.c_max)

    def test_a_spatial_stride_gives_the_same_ceiling(self):
        # what makes the cheap pass honest: measured identical under z::2 / xy::4 / both on real data
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(2).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        full = cu.af_division_stats(data, du, 1, [3])
        strided = cu.af_division_stats(data, du, 1, [3], spatial_stride=(1, 2))
        self.assertAlmostEqual(full.c_max, strided.c_max, delta=full.nbins * 0.1)

    def test_output_stats_report_both_failure_directions(self):
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(3).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        stats = cu.af_division_stats(data, du, 1, [3])
        out = np.zeros(data.shape, data.dtype)
        s = cu._stream_division_channel(data, out, du, channel_idx=1, out_ch=1,
                                        correction_channel_idx=[3], stats=stats)
        for k in ('clippedFrac', 'levelsUsed', 'levelsAvailable', 'trueMax', 'p999'):
            self.assertIn(k, s)
        self.assertGreaterEqual(s['clippedFrac'], 0.0)
        self.assertLessEqual(s['levelsUsed'], s['levelsAvailable'])

    def test_no_signal_and_no_autofluorescence_comes_out_as_ZERO(self):
        """The neutral ratio (1.0) is what AF correction removes, so it must map to 0, not a pedestal.

        Measured before this was anchored: on a real 8-bit image with a derived ceiling of 15.06, every
        background voxel came out at 17 of 255 — 6.6% of the range spent on nothing, and a background
        region's mean intensity reading 17 instead of 0 for everything downstream.
        """
        stats = cu.AfDivisionStats(val1=10, val2=10, c_max=15.06, nbins=256, rescale=255.0)
        # both at background -> img 0, corr 0 -> ratio 1.0 -> neutral
        flat = np.full((2, 4, 4), 10, dtype=np.uint8)
        out = cu.af_correct_frame(flat, flat, stats, np.uint8)
        self.assertTrue(np.all(out == 0), f'background did not come out as 0: {np.unique(out)}')

    def test_a_voxel_dimmer_than_its_reference_is_also_zero(self):
        stats = cu.AfDivisionStats(val1=0, val2=0, c_max=10.0, nbins=256, rescale=255.0)
        img = np.full((1, 2, 2), 1, dtype=np.uint8)
        corr = np.full((1, 2, 2), 50, dtype=np.uint8)     # reference far brighter -> ratio < 1
        self.assertTrue(np.all(cu.af_correct_frame(img, corr, stats, np.uint8) == 0))

    def test_the_ceiling_still_maps_to_full_scale(self):
        """Anchoring the bottom must not move the top — the ceiling is what `af_division_stats` derived."""
        stats = cu.AfDivisionStats(val1=0, val2=0, c_max=11.0, nbins=256, rescale=255.0)
        img = np.full((1, 2, 2), 10, dtype=np.uint8)      # corr 0 -> ratio (10+1)/1 == c_max
        out = cu.af_correct_frame(img, np.zeros((1, 2, 2), np.uint8), stats, np.uint8)
        self.assertTrue(np.all(out == 255), f'ceiling did not reach full scale: {np.unique(out)}')

    def test_a_degenerate_ceiling_does_not_divide_by_zero(self):
        for c in (1.0, 0.5, 0.0):
            stats = cu.AfDivisionStats(val1=0, val2=0, c_max=c, nbins=256, rescale=255.0)
            out = cu.af_correct_frame(np.ones((1, 2, 2), np.uint8),
                                      np.zeros((1, 2, 2), np.uint8), stats, np.uint8)
            self.assertTrue(np.all(np.isfinite(out.astype(float))), f'c_max={c} produced non-finite')

    def test_output_stats_carry_the_derived_values_themselves(self):
        """The fractions cannot stand in for the ceiling, so the ceiling has to be reported too."""
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(3).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        stats = cu.af_division_stats(data, du, 1, [3])
        out = np.zeros(data.shape, data.dtype)
        s = cu._stream_division_channel(data, out, du, channel_idx=1, out_ch=1,
                                        correction_channel_idx=[3], stats=stats)
        self.assertAlmostEqual(s['ceiling'], stats.c_max)
        self.assertAlmostEqual(s['background'], stats.val1)
        self.assertAlmostEqual(s['afBackground'], stats.val2)

    def test_a_proportional_gain_difference_is_invisible_to_both_fractions(self):
        """Why the ceiling is banked at all — and the reason it is a COHORT metric, not a warning.

        Two images whose ratios differ by a constant factor derive proportionally different ceilings
        and produce identical corrected output, so `clippedFrac` and `levelsUsedFrac` cannot tell them
        apart while their intensity scales differ by that factor. Measured across the nine kSUFux
        movies (one experiment, one channel pair, identical settings): a 1.71x spread in the ceiling.
        """
        rng = np.random.default_rng(7)
        ratios = rng.gamma(shape=2.0, scale=3.0, size=200_000)
        rescale = 255.0

        def stats_for(scale):
            c_max = float(iu.robust_hist_max(np.bincount((ratios * scale).astype(int))))
            corrected = np.clip(ratios * scale / c_max * rescale, 0, rescale).astype(np.uint8)
            hist = np.bincount(corrected, minlength=256)[:256]
            return c_max, cu.af_output_stats(
                hist, cu.AfDivisionStats(val1=0, val2=0, c_max=c_max, nbins=256, rescale=rescale))

        c1, s1 = stats_for(1.0)
        c2, s2 = stats_for(2.0)

        self.assertGreater(c2 / c1, 1.5)                       # the scales really do differ...
        self.assertEqual(s1['clippedFrac'], s2['clippedFrac'])  # ...and neither fraction moves at all
        self.assertEqual(s1['levelsUsed'], s2['levelsUsed'])
        self.assertNotAlmostEqual(s1['ceiling'], s2['ceiling'])  # only the banked ceiling shows it



class PyramidRefactorTest(unittest.TestCase):
    """create_multiscales now builds the pyramid via write_multiscale_pyramid (one timepoint at a
    time). Level 1 must equal level 0 strided by 2 in X and Y — the original downsampling semantics."""

    def test_timeseries_pyramid_matches_strided_level0(self):
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=20, size_x=16)
        rng = np.random.default_rng(2)
        base = rng.integers(0, 65535, size=tuple(du.im_dim), dtype=np.uint16)
        src = da.from_array(base, chunks=(1, 2, 20, 16))

        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "ms.ome.zarr")
            zu.create_multiscales(src, path, dim_utils=du, nscales=2)
            g = zarr.open_group(path, mode="r")
            self.assertTrue(np.array_equal(g["0"][:], base))
            y_idx, x_idx = du.dim_idx('Y'), du.dim_idx('X')
            sel = [slice(None)] * base.ndim
            sel[y_idx] = slice(0, None, 2)
            sel[x_idx] = slice(0, None, 2)
            self.assertTrue(np.array_equal(g["1"][:], base[tuple(sel)]),
                            "pyramid level 1 is not the strided-by-2 level 0")
        finally:
            shutil.rmtree(d, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
