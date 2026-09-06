"""Drift ESTIMATION: recovering a known shift, and surviving frames that cannot be registered.

`test_drift_geometry.py` covers the other half — where the estimated shifts put the pixels. This
covers where the shifts come from.

The scenario that motivates all of it is real. On `4kS67f/fHqhyb` the reference channel loses lock
on 13 of 94 frames; the pairwise chain adds each of those errors to a running total and never
recovers, reporting 242 px of XY excursion and blowing the output store to 9.26x the input. Solving
the whole trajectory from overlapping measurements instead brings that to 37 px / 3.55x, because a
bad frame is outvoted by the pairs that skip over it rather than believed once and carried forever.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.correction_utils as cu

# See test_drift_estimate_rigid.py's top-of-file note — the conda-forge SimpleITK Windows build
# can fail to load its DLL, and the dispatch test below exercises the `sitkRigid` estimator which
# needs a working SimpleITK. The translation-only estimator tests do not, so this guard sits on
# ONE test rather than the whole class.
try:
    import SimpleITK as _sitk_probe          # noqa: F401
    _SITK_LOADS = True
except ImportError:
    _SITK_LOADS = False

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint8" SizeT="{t}" SizeC="1" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


def _movie(rolls, shape_zyx=(4, 64, 64), noise_frames=(), seed=0):
    """A [T,C,Z,Y,X] movie of one blurred random texture, with frame ``t`` rolled by ``rolls[t]``.

    Returns ``(array, dim_utils, expected_positions)``. Whole-pixel rolls with wraparound, so phase
    correlation — which is inherently circular — can recover them exactly rather than to a
    tolerance that would hide a real regression.

    ``expected_positions`` is ``-rolls``, and the minus sign is the actual convention rather than a
    fudge: the estimator answers "where must this frame be MOVED TO to sit on top of frame 0", so
    content that drifted by +d has to be placed at −d. That is the same number the writer offsets
    the frame by, which is why the two can be compared directly.

    Frames listed in ``noise_frames`` are replaced by unrelated noise — the failure this file
    exists to characterise, a frame carrying no usable registration signal at all.
    """
    import ome_types
    import scipy.ndimage
    from cecelia.utils.dim_utils import DimUtils

    rng = np.random.default_rng(seed)
    z, y, x = shape_zyx
    rolls = np.asarray(rolls, dtype=int)
    n_t = len(rolls)
    # Blurred noise: broadband enough to give a sharp correlation peak, smooth enough that the
    # peak is not sitting on top of pixel-level aliasing.
    base = scipy.ndimage.gaussian_filter(rng.random((z, y, x)) * 200,
                                         sigma=(0 if z == 1 else 1, 2, 2))

    frames = []
    for t, roll in enumerate(rolls):
        if t in noise_frames:
            frames.append(rng.random((z, y, x)) * 200)
        else:
            frames.append(np.roll(base, tuple(int(v) for v in roll), axis=(0, 1, 2)))
    arr = np.stack(frames).reshape(n_t, 1, z, y, x).astype(np.uint8)

    du = DimUtils(ome_types.from_xml(_OME.format(t=n_t, z=z, y=y, x=x)), use_channel_axis=True)
    du.calc_image_dimensions(arr.shape)

    expected = -rolls.astype(float)
    if z == 1:                       # no Z axis in the estimate — dim_utils reports a 2D image
        expected = expected[:, 1:]
    return arr, du, expected


class DriftEstimateTest(unittest.TestCase):
    def test_recovers_a_known_linear_drift(self):
        """The base case both estimators must agree on: a clean, steadily drifting movie."""
        arr, du, expected = _movie([[0, 2 * t, t] for t in range(8)])
        for estimator in ('multiLag', 'chain'):
            with self.subTest(estimator=estimator):
                est = cu.estimate_drift(arr, 0, du, estimator=estimator)
                np.testing.assert_allclose(est.positions, expected, atol=0.5)
                self.assertEqual(est.axes, ['Z', 'Y', 'X'])
                self.assertEqual(len(est.shifts), len(expected) - 1)
        # a movie that registers cleanly leaves its measurements agreeing with each other — only
        # askable of the redundant estimator (see test_chain_reports_no_residual_… below)
        self.assertLess(cu.estimate_drift(arr, 0, du).residual_rms, 0.5)

    def test_a_single_dead_frame_cancels_out_but_is_still_reported(self):
        """Pins a result that is easy to get backwards, and that this test caught being asserted
        the wrong way round.

        An isolated unregisterable frame does NOT wreck a chain. Phase correlation still assigns
        that frame some best-fit position `p`, and the chain uses it twice with opposite signs —
        `(p − pos3)` then `(pos5 − p)` — so `p` cancels and later frames are unharmed. Both
        estimators therefore land on the truth here.

        What DOES accumulate is a measurement set for which no consistent per-frame position exists
        at all, which is a different failure and the one `residual_rms` detects: the residual is
        zero for any self-consistent set however wrong, so it cannot be fooled by a bad frame that
        cancels, and cannot miss measurements that contradict each other. Both properties are
        asserted below."""
        arr, du, expected = _movie([[0, 2 * t, 0] for t in range(10)], noise_frames={4})

        multi = cu.estimate_drift(arr, 0, du, estimator='multiLag')
        chain = cu.estimate_drift(arr, 0, du, estimator='chain')

        tail = slice(5, None)
        for name, est in (('multiLag', multi), ('chain', chain)):
            self.assertLess(np.abs(est.positions - expected)[tail].max(), 1.5,
                            f'{name}: a lone dead frame should cancel, not offset the tail')

        # …but it is not silently absorbed: the pairs that reach across frame 4 contradict the ones
        # that stop at it, and that is what the user is told about. (Note the fit does not
        # necessarily REJECT them — six of the twenty-four pairs touch frame 4, which is too large
        # a share for the robust scale to treat as outliers. Reporting is the guarantee here;
        # rejection is asserted on a clean single outlier in DriftSolverTest.)
        self.assertGreater(multi.residual_rms, 0.5,
                           'a frame with no signal must show up in the consistency number')
        clean_arr, clean_du, _ = _movie([[0, 2 * t, 0] for t in range(10)])
        self.assertLess(cu.estimate_drift(clean_arr, 0, clean_du).residual_rms, 0.5,
                        'and the same movie without it must not')

    def test_inconsistent_measurements_are_what_actually_accumulate(self):
        """The failure mode the multi-lag fit exists for, at the level it happens: measurements
        that no single set of positions can satisfy.

        Constructed directly on the measurements rather than through images, because that is the
        honest scope — see the solver tests below for the same thing with an outlier. A chain
        believes each measurement in turn and integrates the error; the fit weighs them against
        the ones that skip past."""
        truth = np.array([[0.0, 1.0 * t, 0.0] for t in range(12)])
        pairs = []
        for j in range(12):
            for k in (1, 2, 3):
                i = j - k
                if i < 0:
                    continue
                s = truth[j] - truth[i]
                # every lag-1 measurement biased the same way — no position assignment satisfies
                # these together with the lag-2/3 ones
                if k == 1:
                    s = s + np.array([0.0, 3.0, 0.0])
                pairs.append((i, j, s))

        chain = np.vstack([np.zeros(3),
                           np.cumsum(np.vstack([s for i, j, s in pairs if j == i + 1]), axis=0)])
        fitted, _ = cu._solve_drift_trajectory(pairs, 12, 3)
        self.assertGreater(np.abs(chain - truth).max(), 20.0)      # 11 frames x 3 px, integrated
        self.assertLess(np.abs(fitted - truth).max(), 12.0)
        self.assertGreater(cu.drift_residuals(pairs, fitted).max(), 1.0)

    def test_residual_separates_a_registerable_movie_from_an_unregisterable_one(self):
        """The QC number. Every frame is unrelated noise, so there is nothing to register — the
        residual has to be large, because that is the signal the task banks for the user."""
        good, du_g, _ = _movie([[0, t, t] for t in range(8)])
        bad, du_b, _ = _movie([[0, t, t] for t in range(8)], noise_frames=set(range(8)))

        r_good = cu.estimate_drift(good, 0, du_g).residual_rms
        r_bad = cu.estimate_drift(bad, 0, du_b).residual_rms
        self.assertLess(r_good, 0.5)
        self.assertGreater(r_bad, 2.0, 'an unregisterable movie must exceed the QC warn threshold')

    def test_shifts_are_the_difference_of_the_positions(self):
        """The writer consumes `shifts` and the QC sidecar records both — they must not be able to
        describe two different trajectories."""
        arr, du, _ = _movie([[0, t, -t] for t in range(6)])
        est = cu.estimate_drift(arr, 0, du)
        np.testing.assert_allclose(np.cumsum(est.shifts, axis=0),
                                   est.positions[1:] - est.positions[0], atol=1e-9)

    def test_wrapper_returns_only_the_shifts(self):
        """`drift_correction_shifts` is the historic entry point; it must stay a plain array."""
        arr, du, _ = _movie([[0, t, 0] for t in range(5)])
        shifts = cu.drift_correction_shifts(arr, 0, du)
        self.assertEqual(shifts.shape, (4, 3))
        np.testing.assert_allclose(shifts, cu.estimate_drift(arr, 0, du).shifts)

    def test_2d_movie_reports_two_axes(self):
        """A single-plane movie has no Z to estimate, and the axis list has to say so — the QC
        sidecar labels the shift columns from it."""
        arr, du, expected = _movie([[0, t, 2 * t] for t in range(6)], shape_zyx=(1, 48, 48))
        est = cu.estimate_drift(arr, 0, du)
        self.assertEqual(est.axes, ['Y', 'X'])
        self.assertEqual(est.positions.shape[1], 2)
        np.testing.assert_allclose(est.positions, expected, atol=0.5)

    def test_chain_reports_no_residual_rather_than_a_perfect_one(self):
        """Caught in an end-to-end run, and the reason the field is nullable.

        With neighbour pairs only there is nothing for a measurement to contradict, so the cycle
        residual is identically zero however badly the movie registered. Banking that 0 would put
        `residualRms: 0.00` — a flawless score — on the one estimator that cannot check itself, and
        would drag the cohort median with it."""
        arr, du, _ = _movie([[0, 2 * t, 0] for t in range(8)], noise_frames=set(range(8)))
        chain = cu.estimate_drift(arr, 0, du, estimator='chain')
        self.assertIsNone(chain.residual_rms)
        self.assertIsNone(chain.residual_p90)
        # …while the redundant estimator on the same unregisterable movie does report one
        self.assertIsNotNone(cu.estimate_drift(arr, 0, du, estimator='multiLag').residual_rms)

    def test_rejects_an_unknown_estimator(self):
        arr, du, _ = _movie([[0, 0, 0] for _ in range(3)])
        with self.assertRaises(ValueError):
            cu.estimate_drift(arr, 0, du, estimator='bundleAdjust')

    def test_phase_normalisation_wire(self):
        """`normalisation="phase"` reaches skimage without an error and recovers the same clean
        drift as the default. Same rolls as `test_recovers_a_known_linear_drift`, but with the
        phase-whitened cross-power spectrum — a wiring test, not a numeric-equivalence claim
        (phase normalisation is expected to differ from the default on hard movies, which is why
        it exists; on this clean synthetic they agree within the same 0.5 px tolerance)."""
        arr, du, expected = _movie([[0, 2 * t, t] for t in range(8)])
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag', normalisation='phase')
        np.testing.assert_allclose(est.positions, expected, atol=0.5)
        self.assertTrue(np.all(np.isfinite(est.positions)))
        # `chain` also flows through `_drift_pair_measurements` — one call each, so a future
        # regression that skips the kwarg on one branch fails here.
        est_chain = cu.estimate_drift(arr, 0, du, estimator='chain', normalisation='phase')
        self.assertTrue(np.all(np.isfinite(est_chain.positions)))

    @unittest.skipUnless(_SITK_LOADS, "SimpleITK's shared library failed to load")
    def test_sitk_rigid_dispatches_and_reports_angles(self):
        """Dispatch smoke test: `estimate_drift(estimator='sitkRigid')` returns a `DriftEstimate`
        with the `angles` field populated and `residual_rms` None (direct-to-reference has no
        redundancy, so a residual is not measured — same discipline as `chain`).

        Runs on BOTH a 2D and a 3D input so the dispatch is exercised on both branches: `positions`
        picks up its Z column when the input has Z > 1, `axes` mirrors it, and `angles` stays
        scalar per frame regardless (in-plane rotation only — DRIFT_RIGID_PLAN.md Decision 3).

        Whether the numbers are correct is `test_drift_estimate_rigid.py`'s job. This pins that
        the dispatch reaches the right estimator and packs the result into the right named-tuple
        shape.
        """
        # 2D branch
        arr, du, _ = _movie([[0, 0, 0] for _ in range(4)], shape_zyx=(1, 48, 48))
        est = cu.estimate_drift(arr, 0, du, estimator='sitkRigid')
        self.assertEqual(est.estimator, 'sitkRigid')
        self.assertEqual(est.axes, ['Y', 'X'])
        self.assertEqual(est.positions.shape, (4, 2))
        self.assertIsNotNone(est.angles)
        self.assertEqual(est.angles.shape, (4,))
        self.assertIsNone(est.residual_rms)
        self.assertIsNone(est.residual_p90)

        # 3D branch — Z=4 is the SimpleITK gradient-filter minimum (see `sitk_estimate_rigid`).
        arr3, du3, _ = _movie([[0, 0, 0] for _ in range(4)], shape_zyx=(4, 32, 32))
        est3 = cu.estimate_drift(arr3, 0, du3, estimator='sitkRigid')
        self.assertEqual(est3.axes, ['Z', 'Y', 'X'])
        self.assertEqual(est3.positions.shape, (4, 3))
        self.assertEqual(est3.angles.shape, (4,))                    # still scalar-per-frame

        # translation-only estimators must still leave `angles` as None so a consumer branching
        # on it does not read a stale array from the previous run
        self.assertIsNone(cu.estimate_drift(arr, 0, du, estimator='chain').angles)
        self.assertIsNone(cu.estimate_drift(arr, 0, du, estimator='multiLag').angles)


def _shear_movie(n_t=8, n_z=8, ny=64, nx=64, breath_ampl=4.0, seed=0):
    """Synthetic breathing-shear movie: each Z-plane oscillates in Y with a DEPTH-DEPENDENT amplitude.

    Shallow planes translate +breath_ampl, deep planes translate −breath_ampl, sign flips per frame
    — the pattern measured on `zolIMa/x4E5HU` between t=40 and t=41 (see `DRIFT_PERPLANE_PLAN.md`).
    A whole-volume rigid estimator averages the two directions to ≈0 and leaves the shear in place;
    per-plane must recover a monotonic ramp in the per-Z amplitude across depth.
    """
    import ome_types, scipy.ndimage
    from cecelia.utils.dim_utils import DimUtils
    rng = np.random.default_rng(seed)
    base = scipy.ndimage.gaussian_filter(rng.random((n_z, ny, nx)) * 200, sigma=(0, 2, 2))
    amp_by_z = np.linspace(+breath_ampl, -breath_ampl, n_z)                    # ramp
    frames = []
    for t in range(n_t):
        phase = (-1) ** t
        vol = np.zeros_like(base)
        for z in range(n_z):
            vol[z] = np.roll(base[z], int(round(amp_by_z[z] * phase)), axis=0)
        frames.append(vol)
    arr = np.stack(frames).reshape(n_t, 1, n_z, ny, nx).astype(np.uint8)
    du = DimUtils(ome_types.from_xml(_OME.format(t=n_t, z=n_z, y=ny, x=nx)), use_channel_axis=True)
    du.calc_image_dimensions(arr.shape)
    return arr, du, amp_by_z


class DriftPerPlaneTest(unittest.TestCase):
    """The regime a whole-volume rigid fit cannot reach: motion that DIFFERS by Z depth. Documented
    in `docs/todo/DRIFT_PERPLANE_PLAN.md`; the pilot dataset is `zolIMa/x4E5HU`."""

    def test_whole_volume_cannot_fit_a_z_dependent_shear(self):
        """Baseline for the failure mode this feature exists to solve. A whole-volume rigid fit on
        a movie whose shallow and deep planes translate in OPPOSITE directions returns ONE
        translation per t — necessarily wrong for at least one plane. The point of the test isn't
        WHERE the fit lands (it depends on which plane's structure dominates the 3D correlation
        peak), but that per-plane's answer is meaningfully DIFFERENT."""
        arr, du, amp_by_z = _shear_movie()
        est_v = cu.estimate_drift(arr, 0, du, estimator='multiLag',
                                  smoothness=0.0, trajectory_smooth_sigma=0.0)
        est_p = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                  smoothness=0.0, trajectory_smooth_sigma=0.0)
        self.assertFalse(est_v.per_plane)
        self.assertTrue(est_p.per_plane)
        self.assertEqual(est_v.shifts.shape, (7, 3))                           # (T-1, ZYX)
        self.assertEqual(est_p.shifts.shape, (7, 8, 2))                        # (T-1, Z, YX)
        # The per-plane Y answer must SPREAD across depth (that's the shear signal); the whole-
        # volume answer must be a SCALAR that locks somewhere inside that range and therefore
        # cannot describe both ends of it.
        per_plane_spread_at_t0 = est_p.shifts[0, :, 0].max() - est_p.shifts[0, :, 0].min()
        self.assertGreater(per_plane_spread_at_t0, 10.0)                       # >>0, real shear
        # Whole-volume answer must not equal both extremes — it's one number per t. Concretely:
        # per-plane extremes are ±2·breath_ampl (±8); the whole-volume answer is bounded by the
        # per-plane range and therefore mis-describes at least one end by half the spread.
        self.assertLess(abs(est_v.shifts[0, 1]), per_plane_spread_at_t0 / 2 + 0.5)

    def test_per_plane_recovers_the_depth_ramp(self):
        """The whole point of the flag: per-plane multiLag recovers the injected per-Z amplitude.

        Sign convention (phase_cross_correlation): the returned shift is what you must add to
        `moving` to align it with `fixed`. My shallow plane (z=0) was rolled by +amp at t=0 and
        −amp at t=1 — so the alignment shift is +2·amp. Deep plane (z=n_z−1) was rolled by −amp at
        t=0 and +amp at t=1 — shift is −2·amp. The estimator reports one delta per (t, z); this
        pins the FIRST delta for three planes across the depth ramp.
        """
        arr, du, amp_by_z = _shear_movie(n_t=8, n_z=8, breath_ampl=4.0)
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                smoothness=0.0, trajectory_smooth_sigma=0.0)
        self.assertTrue(est.per_plane)
        self.assertEqual(est.axes, ['Y', 'X'])
        self.assertEqual(est.shifts.shape, (7, 8, 2))                          # (T-1, Z, YX)
        first_y = est.shifts[0, :, 0]
        # Two extreme planes only — with breath_ampl=4, amp_by_z at z=0 and z=7 rounds cleanly to
        # ±4 (integer roll), so shift=±8 is exact. Middle planes have small fractional amplitudes
        # whose integer rounding drifts the expected value by ±1 and would need per-z fixture.
        for z in (0, 7):
            self.assertAlmostEqual(first_y[z], +2 * amp_by_z[z], delta=0.6)

    def test_writer_puts_the_shear_back(self):
        """End-to-end: applying the per-plane trajectory canvas-aligns the SAME feature across
        frames. Measures residual on a MIDDLE canvas window (avoiding zero-padding rings that
        surround each frame's placed data box — otherwise phase correlation locks onto the
        pattern-of-padding rather than the content, which is not what the writer is being asked
        to fix)."""
        from skimage.registration import phase_cross_correlation
        arr, du, _ = _shear_movie(n_t=6, n_z=8, breath_ampl=4.0)
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                smoothness=0.0, trajectory_smooth_sigma=0.0)
        corrected = cu.drift_correct_im_per_plane(arr, du, est.positions)
        # A 20-px middle window is inside the union-of-placements box for every frame at the
        # tested amplitudes (excursion ≤ 4 px each direction, canvas expanded by ~8), so both
        # frames' content is present with no padding at the sample window.
        my = corrected.shape[3] // 2; mx = corrected.shape[4] // 2
        w = 15
        for z in (0, 3, 7):
            a = corrected[0, 0, z, my - w:my + w, mx - w:mx + w].astype(np.float32)
            b = corrected[1, 0, z, my - w:my + w, mx - w:mx + w].astype(np.float32)
            s, _, _ = phase_cross_correlation(a, b, upsample_factor=20, normalization=None)
            self.assertLess(abs(s[0]), 1.0, msg=f'Z={z} residual dy={s[0]}')
            self.assertLess(abs(s[1]), 1.0, msg=f'Z={z} residual dx={s[1]}')

    def test_z_smoothness_rescues_a_signal_poor_plane(self):
        """The last-mile fix: a plane with no reference signal (its pair measurements are noise)
        would land at a random fit under independent per-plane. With Z-smoothness > 0, the joint
        (T, Z) solver pulls it toward its neighbours' fits — the shear signal is preserved (second-
        difference prior is transparent to a linear ramp), only the noisy plane is disciplined.
        """
        # Same shear scene, but blank out the reference channel at Z=4 (dim plane, PC returns
        # near-random shifts).
        import ome_types, scipy.ndimage
        from cecelia.utils.dim_utils import DimUtils
        rng = np.random.default_rng(1)
        n_t, n_z, ny, nx = 8, 8, 64, 64
        base = scipy.ndimage.gaussian_filter(rng.random((n_z, ny, nx)) * 200, sigma=(0, 2, 2))
        amp_by_z = np.linspace(+4, -4, n_z)
        frames = []
        for t in range(n_t):
            phase = (-1) ** t
            vol = np.zeros_like(base)
            for z in range(n_z):
                vol[z] = np.roll(base[z], int(round(amp_by_z[z] * phase)), axis=0)
            # Blank out Z=4 — a plane the reference channel doesn't reach (like CD169 on deep
            # tissue). Random noise so PC fits are noise-limited, not zero-signal (which would
            # error).
            vol[4] = rng.random((ny, nx)) * 40
            frames.append(vol)
        arr = np.stack(frames).reshape(n_t, 1, n_z, ny, nx).astype(np.uint8)
        du = DimUtils(ome_types.from_xml(_OME.format(t=n_t, z=n_z, y=ny, x=nx)),
                      use_channel_axis=True)
        du.calc_image_dimensions(arr.shape)

        indep = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                  smoothness=0.5, trajectory_smooth_sigma=0.0)
        joint = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                  smoothness=0.5, z_smoothness=0.3,
                                  trajectory_smooth_sigma=0.0)
        # Bright neighbours (Z=3 and Z=5) have amp ≈ ±0.57 — expected first-shift ~±2. Their
        # average is the reference for what Z=4 SHOULD be under coupling.
        neighbours_first = 0.5 * (joint.shifts[0, 3, 0] + joint.shifts[0, 5, 0])
        # Joint solve must pull Z=4 closer to its neighbours than the independent solve did. Both
        # amounts are noise-driven for the blanked plane, so compare distance-from-neighbour.
        self.assertLess(abs(joint.shifts[0, 4, 0] - neighbours_first),
                        abs(indep.shifts[0, 4, 0] - neighbours_first) + 0.5)
        # And the shear at Z=0 / Z=7 must survive — Z-smoothness dampens the extremes a bit
        # (second-difference at the ends only sees one neighbour) but the DIRECTION and most of
        # the magnitude must be preserved. Assert same sign as the ideal and at least 60% of the
        # amplitude, not exact equality.
        for z in (0, 7):
            ideal = +2 * amp_by_z[z]
            got   = joint.shifts[0, z, 0]
            self.assertGreater(got * ideal, 0, msg=f'Z={z} shift sign flipped under coupling')
            self.assertGreater(abs(got), 0.6 * abs(ideal),
                               msg=f'Z={z} shift dampened too far: got {got}, ideal {ideal}')

    def test_per_plane_requires_3d(self):
        """A 2D image has no Z to fit per — the branch has to reject rather than silently return a
        degenerate whole-volume fit that reads as per-plane."""
        arr, du, _ = _movie([[0, 0, 0] for _ in range(3)], shape_zyx=(1, 48, 48))
        with self.assertRaises(ValueError):
            cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True)


class DriftSolverTest(unittest.TestCase):
    """The solver alone, on synthetic measurements — no images, so the behaviour is unambiguous."""

    @staticmethod
    def _pairs(positions, max_lag, corrupt=None):
        out = []
        for j in range(len(positions)):
            for k in range(1, max_lag + 1):
                i = j - k
                if i < 0:
                    continue
                s = positions[j] - positions[i]
                if corrupt and (i in corrupt or j in corrupt):
                    s = s + np.array([0.0, 50.0, -50.0])
                out.append((i, j, s))
        return out

    def test_exact_measurements_are_recovered_exactly(self):
        truth = np.array([[0.0, 1.5 * t, -0.3 * t] for t in range(12)])
        pos, w = cu._solve_drift_trajectory(self._pairs(truth, 3), 12, 3,
                                            smoothness=0.0, robust=False)
        np.testing.assert_allclose(pos, truth, atol=1e-8)
        np.testing.assert_allclose(w, 1.0)

    def test_robust_fit_outvotes_a_corrupted_frame(self):
        truth = np.array([[0.0, 1.0 * t, 0.0] for t in range(14)])
        pairs = self._pairs(truth, 3, corrupt={7})
        plain, _ = cu._solve_drift_trajectory(pairs, 14, 3, smoothness=0.0, robust=False)
        robust, w = cu._solve_drift_trajectory(pairs, 14, 3)
        self.assertLess(np.abs(robust - truth).max(), np.abs(plain - truth).max())
        self.assertGreater((w < 0.5).sum(), 0)

    def test_smoothness_keeps_the_system_solvable_with_a_gap(self):
        """A frame no measurement reaches. Without the second-difference prior the system is
        singular there; with it, the position is predicted from the neighbours — which is the best
        answer available for a frame that cannot be registered."""
        truth = np.array([[0.0, 2.0 * t, 0.0] for t in range(9)])
        pairs = [(i, j, s) for i, j, s in self._pairs(truth, 3) if 4 not in (i, j)]
        pos, _ = cu._solve_drift_trajectory(pairs, 9, 3, smoothness=0.5, robust=False)
        self.assertTrue(np.all(np.isfinite(pos)))
        np.testing.assert_allclose(pos[4], truth[4], atol=1.0)

    def test_positions_are_pinned_to_the_origin(self):
        truth = np.array([[0.0, 3.0 * t, 0.0] for t in range(6)])
        pos, _ = cu._solve_drift_trajectory(self._pairs(truth, 2), 6, 3)
        np.testing.assert_allclose(pos[0], 0.0, atol=1e-9)


class DriftResidualTest(unittest.TestCase):
    def test_consistent_measurements_have_no_residual(self):
        pos = np.array([[0.0, t * 1.0, 0.0] for t in range(6)])
        pairs = [(i, j, pos[j] - pos[i]) for i in range(6) for j in range(i + 1, min(i + 3, 6))]
        np.testing.assert_allclose(cu.drift_residuals(pairs, pos), 0.0, atol=1e-9)

    def test_an_inconsistent_triangle_shows_up(self):
        """a→b plus b→c disagreeing with a→c is the whole measurement: three readings of one
        geometry that cannot all be right."""
        pos = np.zeros((3, 2))
        pairs = [(0, 1, np.array([1.0, 0.0])),
                 (1, 2, np.array([1.0, 0.0])),
                 (0, 2, np.array([9.0, 0.0]))]      # should be 2.0
        self.assertGreater(cu.drift_residuals(pairs, pos).max(), 1.0)

    def test_no_pairs_is_not_an_error(self):
        self.assertEqual(len(cu.drift_residuals([], np.zeros((1, 3)))), 0)


if __name__ == '__main__':
    unittest.main()
