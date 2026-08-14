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
    (`af_weight_stats`), and the golden described a method that was deliberately replaced. Pinning
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
        out = self._run(du, {"0": {"competingChannels": [1]}})
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
            self._run(du, {"0": {"competingChannels": [1]}}, background_method=m)

    def test_an_unknown_background_method_is_refused(self):
        du = _dim_utils(size_t=2, size_z=1, size_c=2, size_y=16, size_x=12)
        with self.assertRaises(ValueError):
            self._run(du, {"0": {"competingChannels": [1]}}, background_method='nope')


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
        combos = {"0": {"competingChannels": [1]}}

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
    """`af_weight_stats` + `af_correct_frame` composed must BE the run.

    The task preview corrects only the region on screen, which is possible because the global values
    (one background level per participating channel) are separable from the per-voxel work. That split
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
            cu._stream_corrected_channel(data, run_out, du, channel_idx=1, out_ch=1,
                                         competing_channel_idx=[3], background_method=method)

            # ── what the preview does: stats once, then the per-voxel part per region ──
            stats = cu.af_weight_stats(data, du, [1, 3], background_method=method)
            for t in range(du.dim_val('T')):
                corrected = cu.af_correct_frame(
                    cu._af_slabs(data, du, [1, 3], t), 1, stats, data.dtype)
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
        stats = cu.af_weight_stats(data, du, [1, 3])
        y, x = du.dim_idx('Y'), du.dim_idx('X')
        slabs = cu._af_slabs(data, du, [1, 3], 0)
        full = cu.af_correct_frame(slabs, 1, stats, data.dtype)
        box = [slice(None)] * data.ndim
        box[y] = slice(8, 20)
        box[x] = slice(5, 25)
        crop = cu.af_correct_frame({ch: s[tuple(box)] for ch, s in slabs.items()},
                                   1, stats, data.dtype)
        self.assertTrue(np.array_equal(full[tuple(box)], crop),
                        'a previewed region must equal that region of the full correction')

    def test_reusing_stats_is_what_makes_them_reusable(self):
        # the preview caches stats across parameter changes; passing them in must be equivalent to
        # letting the writer compute its own
        du = _dim_utils(**self.SHAPE)
        data = self._data(du, seed=13)
        a = np.zeros(data.shape, data.dtype)
        b = np.zeros(data.shape, data.dtype)
        cu._stream_corrected_channel(data, a, du, channel_idx=1, out_ch=1,
                                     competing_channel_idx=[3])
        stats = cu.af_weight_stats(data, du, [1, 3])
        cu._stream_corrected_channel(data, b, du, channel_idx=1, out_ch=1,
                                     competing_channel_idx=[3], stats=stats)
        self.assertTrue(np.array_equal(a, b))


class AfProgressTest(unittest.TestCase):
    """The progress scale, pinned end to end.

    The task reported `0/3, 1/3, 2/3, 3/3` for a run whose two long spans were each one of those steps
    — minutes of a still bar. The fix is one unit (a timepoint of one pass) spanning every span, and
    the thing that has to be true for it to work is that the PREDICTED total equals the number of ticks
    actually emitted. Two copies of that formula would give a bar that stalls or finishes early, which
    is why `af_progress_total` exists rather than the runner computing its own.
    """

    SHAPE = dict(size_t=4, size_z=3, size_c=4, size_y=16, size_x=14)

    def _image(self):
        du = _dim_utils(**self.SHAPE)
        rng = np.random.default_rng(77)
        return rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16), du

    def test_af_progress_total_matches_the_ticks(self):
        data, du = self._image()
        combos = {"1": {"competingChannels": [2]}, "2": {"competingChannels": [1]}}
        ticks = []
        cu.af_correct_image(data, combos, dim_utils=du, logfile_utils=None,
                            on_progress=lambda n, t: ticks.append((n, t)))
        expected = cu.af_progress_total(du, combos, nscales=1)
        self.assertEqual(len(ticks), expected, f'{len(ticks)} ticks against a predicted {expected}')
        self.assertEqual(ticks[-1][0], expected, 'the last tick must land ON the total')
        self.assertTrue(all(t == expected for _, t in ticks), 'the total must not move mid-run')

    def test_the_ticks_only_ever_go_forwards(self):
        """Each span reports from 1 within itself, so the offsets have to be right or the bar jumps
        back to the start of every channel — which is what makes a per-span scale unreadable."""
        data, du = self._image()
        combos = {"1": {"competingChannels": [2]}}
        ticks = []
        cu.af_correct_image(data, combos, dim_utils=du, logfile_utils=None,
                            on_progress=lambda n, _t: ticks.append(n))
        self.assertEqual(ticks, sorted(ticks), f'progress went backwards: {ticks}')
        self.assertEqual(len(set(ticks)), len(ticks), f'a unit was reported twice: {ticks}')

    def test_a_carried_through_channel_still_reports(self):
        """A channel no combination covers is copied, which costs a full pass over the movie. Leaving
        it out of the scale stalled the bar for exactly as long as the copy takes."""
        data, du = self._image()
        one = {"1": {"competingChannels": [2]}}
        ticks = []
        cu.af_correct_image(data, one, dim_utils=du, logfile_utils=None,
                            on_progress=lambda n, _t: ticks.append(n))
        n_t = du.dim_val('T')
        # 1 derivation pass + one pass per channel, INCLUDING the three not corrected
        self.assertEqual(len(ticks), n_t * (1 + du.dim_val('C')))

    def test_no_combination_means_no_derivation_pass(self):
        data, du = self._image()
        empty = {}
        self.assertEqual(cu.af_participating_channels(empty), [])
        ticks = []
        cu.af_correct_image(data, empty, dim_utils=du, logfile_utils=None,
                            on_progress=lambda n, _t: ticks.append(n))
        self.assertEqual(len(ticks), cu.af_progress_total(du, empty, nscales=1))
        self.assertEqual(len(ticks), du.dim_val('T') * du.dim_val('C'))

    def test_the_globals_are_derived_ONCE_for_every_combination(self):
        """Two combinations over one channel pair used to make two full passes over the movie for
        identical numbers — a background belongs to a channel and an alpha to a channel PAIR, so
        neither depends on which combination asked. Counted through the derivation's own progress
        ticks, which is the only place a second pass would show up."""
        data, du = self._image()
        combos = {"1": {"competingChannels": [2]}, "2": {"competingChannels": [1]}}
        self.assertEqual(cu.af_participating_channels(combos), [1, 2])
        n_t = du.dim_val('T')
        # derivation + 4 channel passes; a second derivation would add another n_t
        self.assertEqual(cu.af_progress_total(du, combos, nscales=1), n_t * (1 + 4))
        ticks = []
        cu.af_correct_image(data, combos, dim_utils=du, logfile_utils=None,
                            on_progress=lambda n, _t: ticks.append(n))
        self.assertEqual(len(ticks), n_t * (1 + 4))

    def test_deriving_once_gives_the_same_pixels_as_deriving_per_channel(self):
        """The equivalence the single derivation rests on. Correcting with stats derived over the
        UNION of participating channels must be byte-identical to stats derived per combination."""
        data, du = self._image()
        combos = {"1": {"competingChannels": [2]}, "2": {"competingChannels": [1]}}
        shared = cu.af_correct_image(data.copy(), combos, dim_utils=du, logfile_utils=None)
        # the old shape: each channel deriving its own over [target] + competing
        per_channel = np.zeros_like(shared)
        for target, competing in ((1, [2]), (2, [1])):
            cu._stream_corrected_channel(
                data, per_channel, du, channel_idx=target, out_ch=target,
                competing_channel_idx=competing, background_method='triangle')
        for target in (1, 2):
            sl = cu._af_slab(shared, du, target, 0)
            self.assertTrue(np.array_equal(sl, cu._af_slab(per_channel, du, target, 0)),
                            f'channel {target} differs between one derivation and per-channel')

    def test_the_pyramid_is_counted_too(self):
        """The other silent span: `2/3 -> 3/3` was the whole pyramid build."""
        du = _dim_utils(**self.SHAPE)
        combos = {"1": {"competingChannels": [2]}}
        n_t = du.dim_val('T')
        one_level = cu.af_progress_total(du, combos, nscales=1)
        self.assertEqual(cu.af_progress_total(du, combos, nscales=3) - one_level, 2 * n_t)


class AfBleedthroughTest(unittest.TestCase):
    """Job (a) — subtract the amount the filter set leaked, before the dominance weight scales anything.

    The failure this exists to prevent is measured, not hypothetical: on `WIaUjL/p6t4mC` a 2.3% leak
    from a channel 7x brighter made the weight erase the target it leaked into, so a corrected channel
    came out 98-99% zero and segmenting it found the SOURCE. See `af_bleedthrough_alphas`.
    """

    SHAPE = dict(size_t=3, size_z=5, size_c=4, size_y=32, size_x=30)

    def _stats(self, backgrounds, alphas, nbins=65536):
        return cu.AfWeightStats(backgrounds=dict(backgrounds), alphas=dict(alphas),
                                saturated={ch: 0.0 for ch in backgrounds},
                                exponent=cu.AF_WEIGHT_EXPONENT, nbins=nbins)

    def test_the_leaked_amount_is_subtracted_not_scaled(self):
        """The whole point, as arithmetic. A target voxel at 500 counts carrying 0.1x of a 1000-count
        source keeps 500 - 100 = 400 of its own — it does not get multiplied by anything on account of
        the leak."""
        stats = self._stats({0: 0.0, 1: 0.0}, {(1, 0): 0.1})
        target = np.array([[500]], dtype=np.uint16)
        source = np.array([[1000]], dtype=np.uint16)
        out = cu.af_correct_frame({0: target, 1: source}, 0, stats, np.uint16)
        # 500 - 0.1*1000 = 400, and that is the ANSWER: channel 1 was identified as a leak source, so
        # it is not also a co-presence competitor and the weight has nothing left to scale by. Leaving
        # it in the denominator would give 400 * 400^2/(400^2 + 1000^2) = 55 — the same overlap removed
        # twice, which is the composition `af_correct_frame` documents as the thing that fails.
        self.assertEqual(int(out[0, 0]), 400)

    def test_it_keeps_a_co_positive_voxel_the_weight_alone_would_erase(self):
        """The regression that started this. A dim target overlapping a much brighter source: with the
        leak removed first, the target keeps most of itself; with the weight alone it keeps almost
        nothing. Both numbers asserted, so a change that silently reverts the order fails here."""
        target = np.array([[300]], dtype=np.uint16)
        source = np.array([[2100]], dtype=np.uint16)          # ~7x brighter, as measured on p6t4mC
        weight_only = cu.af_correct_frame(
            {0: target, 1: source}, 0, self._stats({0: 0.0, 1: 0.0}, {}), np.uint16)
        unmixed = cu.af_correct_frame(
            {0: target, 1: source}, 0, self._stats({0: 0.0, 1: 0.0}, {(1, 0): 0.023}), np.uint16)
        self.assertLess(int(weight_only[0, 0]), 10, 'the weight alone should erase this voxel')
        # 300 - 0.023*2100 = 252, and channel 1 is NOT also weighed against — see the partition in
        # `af_correct_frame`. Unmixing AND weighing against the same competitor gave 4 here, i.e.
        # WORSE than the 6 the weight alone gives, which is the whole reason the partition exists.
        self.assertEqual(int(unmixed[0, 0]), 252)

    def test_every_channel_is_unmixed_not_only_the_target(self):
        """A competitor still carrying the target's leak would claim the target's own voxels in the
        weight. So the subtraction is applied to all participating channels, and the target's output
        therefore depends on a coefficient that does not name it at all."""
        slabs = {0: np.array([[400]], dtype=np.uint16), 1: np.array([[400]], dtype=np.uint16)}
        clean = cu.af_correct_frame(slabs, 0, self._stats({0: 0.0, 1: 0.0}, {}), np.uint16)
        # 0 -> 1 is a leak INTO the competitor; it must still change what the competitor can claim
        leaky = cu.af_correct_frame(
            slabs, 0, self._stats({0: 0.0, 1: 0.0}, {(0, 1): 0.5}), np.uint16)
        self.assertGreater(int(leaky[0, 0]), int(clean[0, 0]),
                           'cleaning the competitor must give the target back some of its voxel')

    def test_the_subtraction_is_clamped_at_zero(self):
        """`envelope_slope` errs high by construction, so an over-subtracted voxel is expected and its
        honest value is zero rather than negative fluorescence."""
        stats = self._stats({0: 0.0, 1: 0.0}, {(1, 0): 0.9})
        out = cu.af_correct_frame({0: np.array([[100]], dtype=np.uint16),
                                   1: np.array([[1000]], dtype=np.uint16)}, 0, stats, np.uint16)
        self.assertEqual(int(out[0, 0]), 0)

    def test_no_detected_leak_changes_nothing_at_all(self):
        """Most channel pairs do not leak, and for those this step must be a structural no-op — not
        'approximately unchanged'. `af_bleedthrough_alphas` omits a pair rather than storing a small
        number precisely so this holds."""
        rng = np.random.default_rng(31)
        slabs = {ch: rng.integers(0, 4000, size=(20, 20)).astype(np.uint16) for ch in range(3)}
        bg = {ch: 5.0 for ch in range(3)}
        np.testing.assert_array_equal(
            cu.af_correct_frame(slabs, 0, self._stats(bg, {}), np.uint16),
            cu.af_correct_frame(slabs, 0, cu.AfWeightStats(
                backgrounds=bg, alphas={}, saturated={ch: 0.0 for ch in bg},
                exponent=cu.AF_WEIGHT_EXPONENT, nbins=65536), np.uint16))

    def test_a_derived_alpha_finds_an_injected_leak_and_not_a_clean_pair(self):
        """End to end through `af_weight_stats`: build an image where channel 3 leaks into channel 1
        and nothing else does, and check both halves of the answer."""
        du = _dim_utils(**self.SHAPE)
        rng = np.random.default_rng(32)
        data = np.zeros(tuple(du.im_dim), dtype=np.uint16)
        shape = cu._af_slab(data, du, 1, 0).shape
        # EXPONENTIAL, not uniform. A fluorescence channel is mostly background with a bright tail, and
        # the estimator is calibrated on that shape; uniform noise is the degenerate input this module
        # already warns about for the triangle threshold, and it produces spurious ~0.01 fits.
        for t in range(du.dim_val('T')):
            src = rng.exponential(300, size=shape)
            own = rng.exponential(40, size=shape)
            cu._af_write_slab(data, du, 3, t, np.clip(src, 0, 65535).astype(np.uint16))
            cu._af_write_slab(data, du, 1, t, np.clip(own + 0.10 * src, 0, 65535).astype(np.uint16))
            cu._af_write_slab(data, du, 2, t,
                              np.clip(rng.exponential(40, size=shape), 0, 65535).astype(np.uint16))
        stats = cu.af_weight_stats(data, du, [1, 2, 3], background_method='none')
        self.assertIn((3, 1), stats.alphas, f'missed the injected leak: {stats.alphas}')
        self.assertAlmostEqual(stats.alphas[(3, 1)], 0.10, delta=0.05)
        self.assertNotIn((2, 1), stats.alphas, 'invented a leak between independent channels')
        self.assertNotIn((2, 3), stats.alphas, 'invented a leak between independent channels')

    def _leaky_image(self, du, alpha=0.12, co_positive=False, seed=41):
        """Channel 3 leaks `alpha` into channel 1. With `co_positive`, a population is bright in BOTH
        for a real reason — the thing the two estimators disagree about."""
        rng = np.random.default_rng(seed)
        data = np.zeros(tuple(du.im_dim), dtype=np.uint16)
        shape = cu._af_slab(data, du, 1, 0).shape
        for t in range(du.dim_val('T')):
            src = rng.exponential(300, size=shape)
            own = rng.exponential(40, size=shape)
            if co_positive:
                both = rng.random(shape) < 0.05
                src = src + both * rng.uniform(200, 600, shape)
                own = own + both * rng.uniform(200, 600, shape)
            cu._af_write_slab(data, du, 3, t, np.clip(src, 0, 65535).astype(np.uint16))
            cu._af_write_slab(data, du, 1, t,
                              np.clip(own + alpha * src, 0, 65535).astype(np.uint16))
        return data

    def test_exclusive_recovers_the_WHOLE_leak_when_nothing_is_co_labelled(self):
        """The case the flag is for, and the one that made the shipped correction visibly under-remove
        on `WIaUjL/p6t4mC` — two reporters, two cell types, no overlap.

        With nothing legitimately co-located the entire proportional relationship is leak, so the total
        regression IS the coefficient. The envelope fits only the floor of it: on that image it came
        back 0.024 against a real ~0.11 and left the overspill at 2.5x the target's level elsewhere.
        """
        du = _dim_utils(**self.SHAPE)
        data = self._leaky_image(du, alpha=0.12, co_positive=False)
        excl = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                  exclusive={1: True, 3: True})
        co   = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                  exclusive={1: False, 3: False})
        self.assertAlmostEqual(excl.alphas[(3, 1)], 0.12, delta=0.03)
        # …and with nothing co-labelled the two estimators AGREE (0.123 vs 0.127 here): the floor of the
        # joint distribution IS the whole relationship when nothing sits above it. So the flag only
        # changes the answer where it has something to protect, which is the property that makes a
        # wrong setting survivable on synthetic-clean data.
        #
        # It does NOT hold on `WIaUjL/p6t4mC`, where they read 0.025 and 0.113 on a pair Dominik
        # confirms has no overlap. Something real — scattering, out-of-focus competitor, a floor the
        # free intercept absorbs — depresses the envelope there, and it is not reproduced by this
        # scene. Recorded rather than explained: the flag is what makes the answer right either way.
        self.assertAlmostEqual(co.alphas.get((3, 1), 0.0), excl.alphas[(3, 1)], delta=0.03)

    def test_a_co_labelled_population_inflates_the_total_slope(self):
        """**The cost of the exclusive default, stated rather than hidden.** Add cells that are bright
        in BOTH for a real reason and the total regression climbs above the true leak — measured here,
        an injected 0.12 read back as ~0.22 — because it fits that population too. Turning the flag off
        is what protects them: the envelope ignores anything above the floor by construction.

        So the default is safe only where the premise holds. It is the right default because distinct
        cell types are the common case and the opposite default leaves them visibly uncorrected, but a
        co-labelled experiment MUST say so or it loses real signal.
        """
        du = _dim_utils(**self.SHAPE)
        data = self._leaky_image(du, alpha=0.12, co_positive=True)
        excl = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                  exclusive={1: True, 3: True})
        co   = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                  exclusive={1: False, 3: False})
        self.assertGreater(excl.alphas[(3, 1)], 0.15, 'co-labelling should inflate the total slope')
        self.assertLess(co.alphas.get((3, 1), 1.0), excl.alphas[(3, 1)])

    def test_exclusive_is_the_default_when_a_combination_does_not_say(self):
        """Different cell types is the common case, and the other default is what leaves a
        mutually-exclusive pair visibly uncorrected."""
        du = _dim_utils(**self.SHAPE)
        data = self._leaky_image(du, alpha=0.12, co_positive=True)
        silent = cu.af_weight_stats(data, du, [1, 3], background_method='none')
        stated = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                    exclusive={1: True, 3: True})
        self.assertEqual(silent.alphas, stated.alphas)

    def test_only_the_physically_possible_direction_survives(self):
        """`tls_slope` is symmetric — fit the pair the other way round and it returns 1/a — so both
        directions always 'fit'. `AF_ALPHA_MAX` is what leaves the one running from the brighter
        channel into the dimmer: a leak cannot exceed 100% of its source."""
        du = _dim_utils(**self.SHAPE)
        data = self._leaky_image(du, alpha=0.12)
        stats = cu.af_weight_stats(data, du, [1, 3], background_method='none',
                                   exclusive={1: True, 3: True})
        self.assertIn((3, 1), stats.alphas)
        self.assertNotIn((1, 3), stats.alphas, f'kept both directions: {stats.alphas}')
        self.assertLess(stats.alphas[(3, 1)], cu.AF_ALPHA_MAX)

    def test_the_flag_is_read_off_each_combination(self):
        """Per combination, not per run: one target can be a distinct cell type while another is
        co-labelled, and they are corrected in the same pass."""
        du = _dim_utils(**self.SHAPE)
        data = self._leaky_image(du, alpha=0.12, co_positive=True)
        combos = {"1": {"competingChannels": [3], "exclusive": True}}
        loose  = {"1": {"competingChannels": [3], "exclusive": False}}
        a = cu.af_correct_image(data.copy(), combos, dim_utils=du, logfile_utils=None)
        b = cu.af_correct_image(data.copy(), loose,  dim_utils=du, logfile_utils=None)
        self.assertFalse(np.array_equal(a, b), 'the flag must reach the pixels')
        # ...and it reaches them through the coefficient, which is what the flag actually selects
        excl = cu.af_weight_stats(data, du, [1, 3], background_method='none', exclusive={1: True})
        loosely = cu.af_weight_stats(data, du, [1, 3], background_method='none', exclusive={1: False})
        self.assertGreater(excl.alphas[(3, 1)], loosely.alphas.get((3, 1), 0.0))

    def test_a_coefficient_under_the_floor_is_not_applied(self):
        """`AF_ALPHA_MIN` exists because a fit always returns something. Below it the pair is reported
        as clean, so nothing is subtracted from anything."""
        du = _dim_utils(**self.SHAPE)
        rng = np.random.default_rng(33)
        data = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        stats = cu.af_weight_stats(data, du, [1, 3], background_method='none')
        for pair, a in stats.alphas.items():
            self.assertGreaterEqual(a, cu.AF_ALPHA_MIN, f'{pair} kept a sub-floor coefficient {a}')

    def test_the_readout_reports_what_leaked_into_this_channel(self):
        stats = self._stats({0: 0.0, 1: 0.0, 2: 0.0}, {(1, 0): 0.05, (2, 0): 0.01, (0, 2): 0.2})
        out = cu.af_derived_values(stats, 0)
        self.assertEqual(out['bleedthrough'], {'1': 0.05, '2': 0.01})   # into 0 only, not out of it


class AfWeightMechanismTest(unittest.TestCase):
    """What the power weight actually does, pinned as arithmetic rather than as a golden image.

    Replaces a class that pinned the derived ceiling — `AF_CEILING_MIN_COUNT`, the floor fraction, the
    lone-hot-voxel guard, the proportional-gain blindness. All of it described a rescale that no longer
    exists: the output is in input counts now, so there is no ceiling to derive, subsample, defend
    against a hot voxel, or compare across a set.
    """

    SHAPE = dict(size_t=3, size_z=5, size_c=4, size_y=32, size_x=30)

    def _stats(self, backgrounds, exponent=cu.AF_WEIGHT_EXPONENT, nbins=256, alphas=None):
        """A hand-built stats object, so the per-voxel arithmetic can be checked without an image.

        ``alphas`` defaults to EMPTY — no bleedthrough — which is what keeps every assertion below a
        statement about the dominance weight alone. The unmixing step is exercised on its own in
        `AfBleedthroughTest`; mixing the two here would make each of these tests depend on both.
        """
        return cu.AfWeightStats(backgrounds=dict(backgrounds), alphas=dict(alphas or {}),
                                saturated={ch: 0.0 for ch in backgrounds},
                                exponent=exponent, nbins=nbins)

    # ── the per-voxel form ────────────────────────────────────────────────────

    def test_a_channel_with_no_competition_passes_through_untouched(self):
        """The property that makes the output "input counts": alone, the weight is exactly 1.

        Under the ratio this same voxel came out rescaled by `rescale / ceiling` — one input count became
        ~17 output counts on a real 8-bit image, so the corrected channel's brightness depended on a
        derived number rather than on the data.
        """
        stats = self._stats({0: 10.0, 1: 10.0})
        target = np.array([[10, 30, 60, 255]], dtype=np.uint8)
        other = np.full_like(target, 10)                      # competitor at its own background
        out = cu.af_correct_frame({0: target, 1: other}, 0, stats, np.uint8)
        np.testing.assert_array_equal(out, np.array([[0, 20, 50, 245]], dtype=np.uint8))

    def test_two_equally_bright_channels_split_the_voxel_evenly(self):
        """No channel wins territory for being brighter overall — the symmetry the mutual ratio lacked.

        This is also the case the ratio destroyed: equal channels put the ratio at 1, which mapped to
        zero, so a cell carrying both reporters was hollowed out from the centre.
        """
        stats = self._stats({0: 0.0, 1: 0.0})
        a = np.array([[40, 100]], dtype=np.uint8)
        out0 = cu.af_correct_frame({0: a, 1: a.copy()}, 0, stats, np.uint8)
        out1 = cu.af_correct_frame({0: a, 1: a.copy()}, 1, stats, np.uint8)
        np.testing.assert_array_equal(out0, np.array([[20, 50]], dtype=np.uint8))   # b/2, not 0
        np.testing.assert_array_equal(out0, out1)

    def test_n_equally_bright_channels_each_keep_one_nth(self):
        # generalises the pair case, and pins that competitors are NOT collapsed into one reference
        stats = self._stats({0: 0.0, 1: 0.0, 2: 0.0, 3: 0.0})
        a = np.array([[120]], dtype=np.uint8)
        for n in (2, 3, 4):
            slabs = {ch: a.copy() for ch in range(n)}
            out = cu.af_correct_frame(slabs, 0, stats, np.uint8)
            self.assertEqual(int(out[0, 0]), 120 // n, f'{n} equal channels must split {n} ways')

    def test_the_brighter_channel_keeps_more_by_a_fixed_power(self):
        """`out_a / out_b == (b_a / b_b) ** (p + 1)` — the whole behaviour in one identity."""
        p = 3
        stats = self._stats({0: 0.0, 1: 0.0}, exponent=p, nbins=65536)
        a = np.array([[2000]], dtype=np.uint16)
        b = np.array([[1000]], dtype=np.uint16)
        out_a = cu.af_correct_frame({0: a, 1: b}, 0, stats, np.uint16)
        out_b = cu.af_correct_frame({0: a, 1: b}, 1, stats, np.uint16)
        self.assertAlmostEqual(float(out_a[0, 0]) / float(out_b[0, 0]), 2.0 ** (p + 1), delta=0.05)

    def test_the_output_never_exceeds_the_input(self):
        """Why there is no `clippedFrac` any more: `weight <= 1`, so clipping is structurally impossible.

        Under the ratio this was a real failure mode — a ceiling derived too low flattened bright
        structure against the top of the range, which is exactly what that metric watched for.
        """
        rng = np.random.default_rng(21)
        stats = self._stats({0: 5.0, 1: 5.0, 2: 5.0})
        slabs = {ch: rng.integers(0, 256, size=(40, 40)).astype(np.uint8) for ch in range(3)}
        out = cu.af_correct_frame(slabs, 0, stats, np.uint8)
        self.assertTrue(np.all(out.astype(int) <= np.clip(slabs[0].astype(int) - 5, 0, None)),
                        'the correction must never brighten a voxel')

    def test_background_voxels_come_out_zero_without_dividing_by_zero(self):
        # den == 0 everywhere nothing is above background: the `where=` branch, not a division by zero
        stats = self._stats({0: 20.0, 1: 20.0})
        slabs = {0: np.full((8, 8), 15, np.uint8), 1: np.full((8, 8), 20, np.uint8)}
        with np.errstate(divide='raise', invalid='raise'):
            out = cu.af_correct_frame(slabs, 0, stats, np.uint8)
        self.assertEqual(int(out.max()), 0)

    def test_a_voxel_dimmer_than_a_competitor_is_SUPPRESSED_not_zeroed(self):
        """The deliberate behaviour change, kept as a test so it cannot be reverted by accident.

        The ratio had `test_a_voxel_dimmer_than_its_reference_is_also_zero` (#448) asserting the exact
        opposite, and that assertion WAS correct for the ratio: everything with `ratio <= 1` mapped to 0.
        That is the hollowing — a co-positive cell's centre is precisely where the target is not the
        brighter channel. Here the loser keeps a share, small but non-zero, so the cell stays solid.
        """
        stats = self._stats({0: 0.0, 1: 0.0})
        img = np.full((1, 2, 2), 20, dtype=np.uint8)
        rival = np.full((1, 2, 2), 60, dtype=np.uint8)     # competitor 3x brighter
        out = cu.af_correct_frame({0: img, 1: rival}, 0, stats, np.uint8)
        # weight = 400 / (400 + 3600) = 0.1 -> 20 * 0.1 = 2
        self.assertTrue(np.all(out == 2), f'expected a suppressed 2, got {np.unique(out)}')
        self.assertGreater(int(out.max()), 0, 'the losing channel must not be zeroed outright')

    def test_the_integer_output_is_rounded_not_truncated(self):
        """The output is in input counts, so values are often single digits and truncation biases every
        one of them down by ~half a count. Measured on one plane of kSUFux/Or1L8a: truncating shifts the
        mean by -0.072 counts and forces 4.9% of real output to zero, against +0.002 and 4.0% rounding.
        """
        stats = self._stats({0: 0.0, 1: 0.0})
        # b0=3, b1=2 -> weight 9/13, out = 3 * 9/13 = 2.077 -> 2 either way
        # b0=5, b1=3 -> weight 25/34, out = 5 * 25/34 = 3.676 -> 4 rounded, 3 truncated
        a = np.array([[3, 5]], dtype=np.uint8)
        c = np.array([[2, 3]], dtype=np.uint8)
        out = cu.af_correct_frame({0: a, 1: c}, 0, stats, np.uint8)
        np.testing.assert_array_equal(out, np.array([[2, 4]], dtype=np.uint8))

    def test_a_float_output_is_left_unrounded(self):
        stats = self._stats({0: 0.0, 1: 0.0})
        a = np.array([[5]], dtype=np.uint8)
        c = np.array([[3]], dtype=np.uint8)
        out = cu.af_correct_frame({0: a, 1: c}, 0, stats, np.float32)
        self.assertAlmostEqual(float(out[0, 0]), 5.0 * 25.0 / 34.0, places=5)

    def test_a_target_absent_from_the_slabs_is_refused(self):
        stats = self._stats({0: 0.0, 1: 0.0})
        with self.assertRaises(ValueError):
            cu.af_correct_frame({1: np.zeros((4, 4), np.uint8)}, 0, stats, np.uint8)

    def test_a_channel_with_no_derived_background_is_refused(self):
        """Refused rather than defaulted. A missing background would silently skip subtraction, so that
        channel's pedestal would enter the denominator and over-suppress the target — a wrong answer
        that looks like a working one. It means the stats were derived for a different channel set."""
        stats = self._stats({0: 5.0})                      # nothing for channel 1
        slabs = {0: np.full((4, 4), 40, np.uint8), 1: np.full((4, 4), 40, np.uint8)}
        with self.assertRaises(ValueError) as ctx:
            cu.af_correct_frame(slabs, 0, stats, np.uint8)
        self.assertIn('1', str(ctx.exception))

    def test_naming_the_target_among_its_own_competitors_changes_nothing(self):
        """A slip the Julia side filters out (`af_combinations_for_python`); nothing downstream may
        depend on that filtering having happened. Squaring the target's own term into the denominator a
        second time would quietly halve its output.

        Two layers stop it: `af_weight_stats` dedupes the channel list, and `af_correct_frame` takes the
        slabs as a **dict keyed by channel**, so a channel physically cannot appear twice.
        """
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(5).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        plain = cu.af_weight_stats(data, du, [1, 3])
        dupes = cu.af_weight_stats(data, du, [1, 3, 1, 3])
        self.assertEqual(plain.backgrounds, dupes.backgrounds)
        self.assertEqual(plain.saturated, dupes.saturated)
        slabs = cu._af_slabs(data, du, [1, 3, 1], 0)
        self.assertEqual(sorted(slabs), [1, 3])
        np.testing.assert_array_equal(cu.af_correct_frame(slabs, 1, plain, data.dtype),
                                      cu.af_correct_frame(slabs, 1, dupes, data.dtype))

    # ── the derived globals ───────────────────────────────────────────────────

    def test_a_background_is_derived_per_channel(self):
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(1).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        s = cu.af_weight_stats(data, du, [1, 3])
        self.assertEqual(sorted(s.backgrounds), [1, 3])
        self.assertEqual(s.exponent, cu.AF_WEIGHT_EXPONENT)

    def _pedestal_image(self, du, pedestal=30, blob=900):
        """A flat background with one bright blob in each channel — enough of a histogram for a
        threshold to be derived from, unlike a single spike."""
        data = np.full(tuple(du.im_dim), pedestal, dtype=np.uint16)
        y, x = du.dim_idx('Y'), du.dim_idx('X')
        sl = [slice(None)] * data.ndim
        sl[y] = slice(2, 8)
        sl[x] = slice(2, 8)
        data[tuple(sl)] = blob
        return data, tuple(sl)

    def test_without_background_subtraction_the_background_survives(self):
        """Why the task JSON does not offer `'none'`, as a test rather than as a comment.

        The weight is a ratio of intensities, so an unsubtracted pedestal makes background voxels split
        evenly between the channels and come out non-zero. Measured on kSUFux/Or1L8a: 92.1% of
        background voxels survive and cell-to-background contrast collapses to 6.8x.
        """
        du = _dim_utils(size_t=1, size_z=1, size_c=2, size_y=24, size_x=24)
        data, blob = self._pedestal_image(du)
        slabs = cu._af_slabs(data, du, [0, 1], 0)

        none_stats = cu.af_weight_stats(data, du, [0, 1], background_method='none')
        self.assertEqual(none_stats.backgrounds, {0: 0.0, 1: 0.0})
        survived = cu.af_correct_frame(slabs, 0, none_stats, data.dtype)

        derived = cu.af_weight_stats(data, du, [0, 1], background_method='triangle')
        self.assertGreater(derived.backgrounds[0], 0.0)
        cleaned = cu.af_correct_frame(slabs, 0, derived, data.dtype)

        # outside the blob is background in both channels; it must not survive a derived background
        bg = np.ones(survived.shape, bool)
        bg[blob] = False
        self.assertGreater(int(survived[bg].max()), 0, 'background survives without subtraction')
        self.assertEqual(int(cleaned[bg].max()), 0, 'a derived background must remove the pedestal')

    def test_saturated_reports_the_input_clipped_at_the_sensor(self):
        """The QC signal that replaced `clippedFrac`. A clipped voxel's true value is gone before this
        task sees it — measured across the nine kSUFux movies, CH3 saturation spanned 0.001% to 0.018%
        at identical acquisition settings."""
        du = _dim_utils(size_t=1, size_z=1, size_c=2, size_y=10, size_x=10)
        data = np.zeros(tuple(du.im_dim), dtype=np.uint8)
        c, y = du.dim_idx('C'), du.dim_idx('Y')
        sl = [slice(None)] * data.ndim
        sl[c] = slice(0, 1)
        sl[y] = slice(0, 1)            # one row of 10 in a 10x10 channel
        data[tuple(sl)] = 255
        s = cu.af_weight_stats(data, du, [0, 1])
        self.assertAlmostEqual(s.saturated[0], 0.10, places=6)
        self.assertAlmostEqual(s.saturated[1], 0.0, places=6)

    def test_a_spatial_stride_gives_the_same_backgrounds(self):
        """What makes the preview's cheap pass honest, and it is EXACT — not merely close.

        The preview derives its globals from a strided read (`AF_PREVIEW_STRIDE`) while the run reads
        every voxel. If those disagree the preview subtracts a different background than the run, which
        is the one thing the feature exists to rule out. Measured here at `(1,1)`, `(1,2)` and `(2,4)`:
        the same background to the count.

        **It holds because the image HAS a background population**, which is what a threshold needs to
        find. On structureless uniform noise there is none, and the triangle threshold then swings
        wildly under subsampling (measured on `rng.integers(0, 4000)`: 3178 → 544 → 196). That is a
        degenerate input rather than a realistic one — a fluorescence channel is mostly background — but
        it is the assumption the strided pass rests on, so it is written down here and on
        `af_weight_stats` rather than left implicit. The previous implementation had the same exposure
        and no test covered it.
        """
        du = _dim_utils(**self.SHAPE)
        rng = np.random.default_rng(2)
        # a real channel: a background pedestal with sensor noise, plus signal over part of the frame
        data = np.full(tuple(du.im_dim), 30, dtype=np.uint16)
        data += rng.integers(0, 8, size=data.shape, dtype=np.uint16)
        y, x = du.dim_idx('Y'), du.dim_idx('X')
        sl = [slice(None)] * data.ndim
        sl[y] = slice(4, 20)
        sl[x] = slice(4, 20)
        data[tuple(sl)] += 900

        full = cu.af_weight_stats(data, du, [1, 3])
        for stride in ((1, 2), (2, 4)):
            strided = cu.af_weight_stats(data, du, [1, 3], spatial_stride=stride)
            self.assertEqual(full.backgrounds, strided.backgrounds,
                             f'stride {stride} moved a derived background')

    # ── what the run banks and the preview reads out ──────────────────────────

    def test_output_stats_carry_what_qc_acts_on(self):
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(3).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        stats = cu.af_weight_stats(data, du, [1, 3])
        out = np.zeros(data.shape, data.dtype)
        s = cu._stream_corrected_channel(data, out, du, channel_idx=1, out_ch=1,
                                         competing_channel_idx=[3], stats=stats)
        for k in ('levelsUsed', 'levelsAvailable', 'trueMax', 'p999',
                  'saturatedFrac', 'background', 'competingBackgrounds', 'exponent'):
            self.assertIn(k, s)
        self.assertNotIn('clippedFrac', s)     # structurally ~0 now — see af_output_stats
        self.assertNotIn('ceiling', s)         # there is no rescale to have a ceiling for
        self.assertLessEqual(s['levelsUsed'], s['levelsAvailable'])

    def test_the_readout_and_the_banked_metric_come_from_one_helper(self):
        """`af_derived_values` is shared by the run's QC and the preview's readout precisely so the two
        cannot drift on a key name or a value."""
        du = _dim_utils(**self.SHAPE)
        data = np.random.default_rng(3).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        stats = cu.af_weight_stats(data, du, [1, 3])
        out = np.zeros(data.shape, data.dtype)
        s = cu._stream_corrected_channel(data, out, du, channel_idx=1, out_ch=1,
                                         competing_channel_idx=[3], stats=stats)
        self.assertAlmostEqual(s['background'], stats.backgrounds[1])
        self.assertEqual(s['competingBackgrounds'], {'3': stats.backgrounds[3]})
        for k, v in cu.af_derived_values(stats, 1).items():
            self.assertEqual(s[k], v, f'{k} differs between the readout and the banked metric')


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
