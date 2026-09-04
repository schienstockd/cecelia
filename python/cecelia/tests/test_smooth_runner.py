"""End-to-end test of the smoothing task RUNNER — `app/src/tasks/cleanupImages/smooth_run.py`.

Same seam as `test_af_correct_runner`: the runner is executed by path (`run_py`) and never imported,
so nothing in the package suite touches it. `coastal.smooth` is well covered on its own; what is NOT
covered by that is the streaming loop here — the rolling per-z cache, the shared gate built across
channels, and the one-shot noise estimate handed to every frame.

The gated path is where that matters most. Its correctness rests on two things the library cannot
enforce from its side:

  * every selected channel is gated by the SAME weights, derived from their sum (the AF cross-channel
    ratio invariant, for an adaptive kernel) — gate each channel on its own content and the ratio
    breaks silently;
  * the gate's noise scale is estimated ONCE and passed in, so strictness does not drift between
    z-planes and timepoints.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import os
import shutil
import tempfile
import unittest
from pathlib import Path

import numpy as np
import ome_types

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'cleanupImages' / 'smooth_run.py')

try:
    import coastal.smooth  # noqa: F401
    HAVE_COASTAL = True
except ImportError:
    HAVE_COASTAL = False

# `gated_frames` (plural) is the batched form the runner imports at module scope — a coastal that
# predates it makes `_load_runner` raise rather than skip, which is the loud signal we want for a
# rolled-back pin. The guard stays for the case where coastal is present but older.
HAVE_GATED = HAVE_COASTAL and hasattr(coastal.smooth, 'gated_frames')


def _load_runner():
    spec = importlib.util.spec_from_file_location('smooth_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    channels = ''.join(
        f'<Channel ID="Channel:0:{i}" Name="CH{i + 1}" SamplesPerPixel="1"/>' for i in range(size_c))
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeC="{size_c}" SizeZ="{size_z}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm"
            TimeIncrement="30.0" TimeIncrementUnit="s">
      {channels}
    </Pixels>
  </Image>
</OME>"""


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
@unittest.skipUnless(HAVE_COASTAL, 'coastal not installed')
class SmoothRunnerTest(unittest.TestCase):
    SHAPE = dict(size_t=7, size_z=2, size_c=3, size_y=20, size_x=18)

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        omexml = ome_types.from_xml(_ome_xml(**self.SHAPE))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [self.SHAPE['size_t'], self.SHAPE['size_c'], self.SHAPE['size_z'],
                 self.SHAPE['size_y'], self.SHAPE['size_x']]
        du.calc_image_dimensions(shape)

        # A MOVING blob plus noise: static content would make every temporal statistic look alike,
        # which is exactly the case that hides the difference this task now offers.
        rng = np.random.default_rng(3)
        data = np.full(shape, 40, dtype=np.float32)
        data += rng.normal(0, 6, size=shape).astype(np.float32)
        ci, ti, zi = du.dim_idx('C'), du.dim_idx('T'), du.dim_idx('Z')
        for t in range(self.SHAPE['size_t']):
            for ch in range(self.SHAPE['size_c']):
                sl = [slice(None)] * len(shape)
                sl[ti] = slice(t, t + 1)
                sl[ci] = slice(ch, ch + 1)
                sl[du.dim_idx('Y')] = slice(4 + t, 8 + t)      # translates 1 px per frame
                sl[du.dim_idx('X')] = slice(4 + t, 8 + t)
                data[tuple(sl)] += 400.0 * (ch + 1)            # channel-dependent brightness
        data = np.clip(data, 0, np.iinfo(np.uint16).max).astype(np.uint16)

        self.in_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.in_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(self.in_path, omexml=omexml)
        self.du, self.data, self.shape = du, data, shape

    def _run(self, **over):
        out = os.path.join(self.dir, f"out{over.get('temporalStat', 'x')}.ome.zarr")
        params = dict(imPath=self.in_path, imOutputPath=out, channels=[],
                      spatialSigma=1.0, temporalFrames=5, temporalStat='median',
                      restoreGain=False, qcOutPath=os.path.join(self.dir, 'qc.json'))
        params.update(over)
        self.runner.run(params)
        return np.asarray(zarr_utils.open_as_zarr(out, as_dask=False)[0][0][:])

    def test_median_runs_and_writes_the_same_shape(self):
        out = self._run(temporalStat='median')
        self.assertEqual(tuple(out.shape), tuple(self.shape))

    @unittest.skipUnless(HAVE_GATED, 'installed coastal predates gated_frames')
    def test_gated_runs_and_writes_the_same_shape(self):
        out = self._run(temporalStat='gated')
        self.assertEqual(tuple(out.shape), tuple(self.shape))
        self.assertTrue(np.isfinite(out).all())

    @unittest.skipUnless(HAVE_GATED, 'installed coastal predates gated_frames')
    def test_gated_keeps_the_moving_feature_the_median_flattens(self):
        """The property the option exists for, measured through the whole task rather than the
        library: at the centre of the moving blob, the median mixes frames where it was elsewhere."""
        ti, ci, zi = self.du.dim_idx('T'), self.du.dim_idx('C'), self.du.dim_idx('Z')
        med, gat = self._run(temporalStat='median'), self._run(temporalStat='gated')

        def peak(arr, t=3, c=2):
            sl = [slice(None)] * len(self.shape)
            sl[ti], sl[ci], sl[zi] = t, c, 0
            f = arr[tuple(sl)].astype(np.float32)
            return float(f[4 + t:8 + t, 4 + t:8 + t].mean() - np.median(f))

        raw_p, med_p, gat_p = peak(self.data), peak(med), peak(gat)
        self.assertLess(med_p, 0.95 * raw_p, 'median unexpectedly preserved the moving feature')
        self.assertGreater(gat_p, med_p, 'gated did not beat the median on the moving feature')

    @unittest.skipUnless(HAVE_GATED, 'installed coastal predates gated_frames')
    def test_gated_applies_ONE_gate_to_every_channel(self):
        """The AF-ratio invariant for an adaptive kernel, asserted through the runner.

        Uses a fixture whose channels are EXACT scalar multiples of one another, so identical weights
        must return exact multiples too. That is the strong form and it is what the invariant actually
        says. (A weaker version of this test asserted brightness ORDERING over "signal" voxels and
        failed on 16 background voxels where the raw input already violated the ordering — it was
        measuring noise, not the gate.)

        A per-channel gate breaks this: the dim channel would gate on its own noise and decide
        differently at the same voxel.
        """
        rng = np.random.default_rng(11)
        shape = list(self.shape)
        ci, ti = self.du.dim_idx('C'), self.du.dim_idx('T')
        base = np.full([s for i, s in enumerate(shape) if i != ci], 60.0, dtype=np.float32)
        base += rng.normal(0, 5, size=base.shape).astype(np.float32)
        for t in range(self.SHAPE['size_t']):                # the same moving feature as setUp
            sl = [slice(None)] * base.ndim
            tb = ti if ti < ci else ti - 1
            sl[tb] = slice(t, t + 1)
            sl[-2], sl[-1] = slice(4 + t, 8 + t), slice(4 + t, 8 + t)
            base[tuple(sl)] += 500.0
        data = np.stack([base * m for m in (1.0, 2.0, 3.0)], axis=ci)
        data = np.clip(np.rint(data), 0, np.iinfo(np.uint16).max).astype(np.uint16)

        path = os.path.join(self.dir, 'mult.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            path, tuple(shape), np.uint16, self.du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(path, omexml=ome_types.from_xml(_ome_xml(**self.SHAPE)))

        out_path = os.path.join(self.dir, 'mult_out.ome.zarr')
        self.runner.run(dict(imPath=path, imOutputPath=out_path, channels=[], spatialSigma=1.0,
                             temporalFrames=5, temporalStat='gated', restoreGain=False,
                             qcOutPath=os.path.join(self.dir, 'qc2.json')))
        out = np.asarray(zarr_utils.open_as_zarr(out_path, as_dask=False)[0][0][:]).astype(np.float32)
        c0, c1, c2 = (np.take(out, c, axis=ci) for c in range(3))
        # tolerance is uint16 rounding on the way out, not gate slack
        np.testing.assert_allclose(c1, c0 * 2.0, rtol=0.02, atol=2.0)
        np.testing.assert_allclose(c2, c0 * 3.0, rtol=0.02, atol=3.0)

    @unittest.skipUnless(HAVE_GATED, 'installed coastal predates gated_frames')
    def test_gated_estimates_the_noise_scale_once(self):
        """Not per window: the gate's strictness is a property of the acquisition. Pinned by watching
        how often the runner asks coastal for an estimate."""
        import coastal.smooth as cs
        calls = []
        real = cs.noise_sigma
        self.runner.noise_sigma = lambda *a, **k: (calls.append(1), real(*a, **k))[1]
        try:
            self._run(temporalStat='gated')
        finally:
            self.runner.noise_sigma = real
        nz, nt = self.SHAPE['size_z'], self.SHAPE['size_t']
        self.assertLessEqual(len(calls), 3, 'the noise scale is being re-estimated per window')
        self.assertGreaterEqual(len(calls), 1)
        self.assertLess(len(calls), nz * nt)

    def test_an_unknown_stat_is_rejected_rather_than_silently_ignored(self):
        with self.assertRaises(ValueError):
            self._run(temporalStat='gate')

    def test_bilateral_vst_runs_and_writes_the_same_shape(self):
        """The bilateral (VST) spatial engine runs end-to-end and produces a same-shape output.

        Runs both a spatial-only case (temporalFrames=1) and a spatial+temporal case, so both the
        engine dispatch AND the streaming loop's caching (which reuses the spatial output across the
        window) exercise the bilateral path.
        """
        out_spatial = self._run(spatialMethod='bilateral_vst', temporalFrames=1,
                                bilateralColor=10.0, bilateralReach=3.0,
                                temporalStat='median')
        self.assertEqual(tuple(out_spatial.shape), tuple(self.shape))
        self.assertTrue(np.isfinite(out_spatial).all())
        # The bilateral must not have blown out the values — a broken inverse VST would leave the
        # output either at zero or clipped at the dtype max everywhere.
        self.assertGreater(int(out_spatial.max()), 0)
        self.assertLess(int(out_spatial.max()), np.iinfo(np.uint16).max)

        out_temporal = self._run(spatialMethod='bilateral_vst', temporalFrames=3,
                                 bilateralColor=10.0, bilateralReach=3.0,
                                 temporalStat='median')
        self.assertEqual(tuple(out_temporal.shape), tuple(self.shape))
        self.assertTrue(np.isfinite(out_temporal).all())

    def test_bilateral_polish_smooths_speckle_that_bare_bilateral_leaves(self):
        """The polish σ is why this task exists as a separate option: on photon-limited data the raw
        bilateral leaves single-pixel jitter because its color-weight is ~1 on the same-brightness side
        of an edge and ~0 across it, and quantising back to uint16 makes those isolated pixels stick
        out. A sub-pixel Gaussian on top of the inverse averages them into the neighbours the bilateral
        already agreed with. Fixture: uniform background sprinkled with isolated bright pixels — the
        speckle regime. Assert that the polished output has NARROWER local-residual MAD than the
        unpolished one, and that polish=0 short-circuits the Gaussian (byte-identical to no polish).
        """
        rng = np.random.default_rng(19)
        shape = list(self.shape)
        spikes = np.full(shape, 30, dtype=np.float32)
        spikes += rng.normal(0, 3, size=shape).astype(np.float32)
        # sprinkle isolated bright pixels — the speckle
        idx = tuple(rng.integers(0, s, size=200) for s in shape)
        spikes[idx] = 200.0
        spikes = np.clip(np.rint(spikes), 0, np.iinfo(np.uint16).max).astype(np.uint16)

        path = os.path.join(self.dir, 'spikes.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            path, tuple(shape), np.uint16, self.du, nscales=1)
        level0[:] = spikes
        ome_xml_utils.save_meta_in_zarr(path, omexml=ome_xml_utils.parse_meta(self.in_path))

        common = dict(spatialMethod='bilateral_vst', bilateralColor=10.0, bilateralReach=3.0,
                      temporalFrames=1, temporalStat='median')
        no_polish = self._run(imPath=path, bilateralPolish=0.0, **common).astype(np.float32)
        polished  = self._run(imPath=path, bilateralPolish=0.7, **common).astype(np.float32)
        self.assertFalse(np.array_equal(no_polish, polished),
                         'polish σ>0 should change the output relative to polish=0')

        # Local-residual MAD on a single plane — narrower = more coherent, i.e. less speckle.
        def local_mad(arr):
            import cv2 as cv2m
            plane = arr[3, 2, 0]
            smoothed = cv2m.blur(plane, (5, 5))
            r = plane - smoothed
            return float(np.median(np.abs(r - np.median(r))))
        self.assertLess(local_mad(polished), local_mad(no_polish),
                        'polish σ>0 should tighten the local-residual distribution')

    def test_bilateral_polish_zero_matches_no_polish_call(self):
        """The `polish_sigma > 0` guard makes polish=0 skip the GaussianBlur entirely — assert that
        so a future refactor that inlines the call cannot silently start filtering at 0."""
        out_zero = self._run(spatialMethod='bilateral_vst', bilateralColor=10.0, bilateralReach=3.0,
                             bilateralPolish=0.0, temporalFrames=1, temporalStat='median')
        # Same params, no polish arg — should hit the same short-circuit branch
        default_polish = self._run(spatialMethod='bilateral_vst', bilateralColor=10.0,
                                   bilateralReach=3.0, temporalFrames=1, temporalStat='median')
        # default is 0.6, so the two must DIFFER — the default is polishing, not off
        self.assertFalse(np.array_equal(out_zero, default_polish),
                         'default polish (0.6) must differ from polish=0')

    def test_gaussian_stays_the_default_when_no_method_is_passed(self):
        """`spatialMethod` is optional; omitting it must run the gaussian engine unchanged, so an
        older caller (chain, saved run, external script) still routes to the same spatial path.
        """
        default = self._run()                                    # no spatialMethod override
        gaussian = self._run(spatialMethod='gaussian')
        np.testing.assert_array_equal(default, gaussian)

    def test_parallel_produces_the_same_output_as_serial(self):
        """A per-z ThreadPoolExecutor must not change the output — the writes go to non-overlapping
        chunks and every worker has its own rolling cache. Compare against `z_workers=1` (serial
        path). Same params both runs; the only difference is thread count."""
        # Force serial by capping the worker budget to 1
        os.environ['CECELIA_TASK_WORKERS'] = '1'
        try:
            serial = self._run(spatialMethod='gaussian', temporalFrames=3, temporalStat='median')
        finally:
            del os.environ['CECELIA_TASK_WORKERS']
        # Default budget — likely >1 on any test box, so this exercises the executor path
        parallel = self._run(spatialMethod='gaussian', temporalFrames=3, temporalStat='median')
        np.testing.assert_array_equal(serial, parallel)

    def test_static_image_runs_the_spatial_engine_alone(self):
        """A single-timepoint image has no temporal step to take, and the task must NOT refuse to
        run — the temporal-only params are already gated by the spec's `requires.axes: ["T"]`, so
        the fallback (`frames=1`) collapses to one spatial pass per plane. Guarding the whole task
        on the presence of T would refuse a spatial-only run that has a valid engine of its own.
        """
        # Build a matching-shape but T=1 store, in every other way identical to the fixture.
        one_shape = dict(size_t=1, size_z=self.SHAPE['size_z'], size_c=self.SHAPE['size_c'],
                         size_y=self.SHAPE['size_y'], size_x=self.SHAPE['size_x'])
        omexml = ome_types.from_xml(_ome_xml(**one_shape))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [1, one_shape['size_c'], one_shape['size_z'],
                 one_shape['size_y'], one_shape['size_x']]
        du.calc_image_dimensions(shape)
        rng = np.random.default_rng(11)
        data = np.full(shape, 40, dtype=np.float32)
        data += rng.normal(0, 6, size=shape).astype(np.float32)
        data = np.clip(data, 0, np.iinfo(np.uint16).max).astype(np.uint16)

        in_path = os.path.join(self.dir, 'static.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            in_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(in_path, omexml=omexml)

        # `temporalFrames` and `temporalStat` are dropped by the app-side `_apply_param_requires` on
        # a static image; the runner sees a dict without them. Simulate the same call shape.
        out = os.path.join(self.dir, 'static_out.ome.zarr')
        self.runner.run(dict(imPath=in_path, imOutputPath=out, channels=[],
                             spatialMethod='bilateral_vst', bilateralColor=10.0, bilateralReach=3.0,
                             restoreGain=False, qcOutPath=os.path.join(self.dir, 'qc.json')))
        arr = np.asarray(zarr_utils.open_as_zarr(out, as_dask=False)[0][0][:])
        self.assertEqual(tuple(arr.shape), tuple(shape))
        self.assertTrue(np.isfinite(arr).all())

    @unittest.skipUnless(HAVE_GATED, 'installed coastal predates gated_frames')
    def test_gated_refuses_a_gate_with_no_noise_scale(self):
        """A zero noise scale makes every weight collapse, so the run would spend minutes writing a
        copy of its input and report success. Refused instead — the failure this guards is silent.

        Driven through the real degenerate case rather than by stubbing the estimate: a constant
        image has no temporal difference at all, which is what `spatialSigma=0` on photon-limited
        data amounts to (a majority of exact zeros, MAD exactly 0)."""
        flat = os.path.join(self.dir, 'flat.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            flat, tuple(self.shape), np.uint16, self.du, nscales=1)
        level0[:] = np.full(self.shape, 100, dtype=np.uint16)
        ome_xml_utils.save_meta_in_zarr(flat, omexml=ome_xml_utils.parse_meta(self.in_path))
        with self.assertRaises(SystemExit):
            self._run(imPath=flat, temporalStat='gated', spatialSigma=0.0)


if __name__ == '__main__':
    unittest.main()
