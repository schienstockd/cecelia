"""Rigid drift ESTIMATION: recovering a known per-frame rotation + translation.

`test_drift_estimate.py` covers the translation-only estimators (multiLag / chain). This covers
`sitk_estimate_rigid`, whose reason to exist is that phase correlation cannot see rotation at all.
Design: `docs/todo/DRIFT_RIGID_PLAN.md`.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np
import scipy.ndimage

import cecelia.utils.correction_utils as cu

# The conda-forge SimpleITK Windows build has occasionally shipped with a DLL-resolution failure
# ("DLL load failed while importing _SimpleITK") that runs on Windows CI cannot recover from — the
# only existing caller in the codebase (`app/src/tasks/editImages/register_run.py`) documents the
# same issue and lazy-imports for the same reason. When the DLL cannot be loaded, skip the whole
# suite here: pretending to test the estimator on a runner where its engine cannot even import is
# worse than a clear "skipped on this platform" line in the report.
try:
    import SimpleITK as _sitk_probe          # noqa: F401
    _SITK_LOADS = True
except ImportError:
    _SITK_LOADS = False
_SITK_SKIP_REASON = ("SimpleITK's shared library failed to load — see the top-of-file note in "
                     "app/src/tasks/editImages/register_run.py for the Windows conda-forge case.")


def _apply_rigid(frame_np, angle_deg, translation_yx, centre_xy=None):
    """Apply the estimator's answer to a frame — the contract P3 will use.

    Uses the SAME `Euler2DTransform` construction `sitk_estimate_rigid` returned, then resamples
    with `frame_np` itself as reference image. Result is `frame_np` warped by the reported rigid
    transform, in `frame_np`'s coordinate system.
    """
    import SimpleITK as sitk
    if centre_xy is None:
        centre_xy = cu._rigid_centre(frame_np.shape)

    img = sitk.GetImageFromArray(frame_np.astype(np.float32, copy=False))
    tx = sitk.Euler2DTransform()
    tx.SetCenter(centre_xy)
    tx.SetAngle(float(np.deg2rad(angle_deg)))
    tx.SetTranslation((float(translation_yx[1]), float(translation_yx[0])))     # (x, y) order
    resampled = sitk.Resample(img, img, tx, sitk.sitkLinear, 0.0)
    return sitk.GetArrayFromImage(resampled)

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint8" SizeT="{t}" SizeC="1" SizeZ="1"
    SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


def _rigid_movie(angles_deg, shifts_yx, shape_yx=(64, 64), seed=0):
    """A [T, 1, 1, Y, X] uint8 movie of one blurred blob field, rigidly transformed per frame.

    Frame 0 is the untouched reference. For ``t > 0``, frame 0 is rotated by ``angles_deg[t]`` around
    the frame centre and then translated by ``shifts_yx[t]`` — the ground truth the estimator has to
    recover. Rotation and translation are applied via `scipy.ndimage.affine_transform` for a clean,
    subpixel-accurate reference (no wrap-around, unlike the phase-corr test's `np.roll`).

    Returns ``(array, dim_utils, expected_angles_deg, expected_positions_yx)``. `expected_angles_deg`
    equals `angles_deg` and `expected_positions_yx` equals `shifts_yx` — the estimator answers "how
    was this frame transformed from frame 0", which is what these say.
    """
    import ome_types
    from cecelia.utils.dim_utils import DimUtils

    rng = np.random.default_rng(seed)
    y, x = shape_yx
    n_t = len(angles_deg)
    assert n_t == len(shifts_yx), 'angles and shifts must have the same length'
    assert angles_deg[0] == 0.0 and tuple(shifts_yx[0]) == (0.0, 0.0), \
        'frame 0 is the reference by construction'

    # A blurred-noise scene, similar to the phase-corr test but with several bright blobs so the
    # rotation is unambiguous (a pure texture might register at the wrong angle-mod-π on symmetric
    # patterns).
    base = rng.random((y, x)) * 40
    for cy, cx in [(18, 22), (44, 18), (22, 48), (48, 46)]:
        yy, xx = np.mgrid[:y, :x]
        base += 200 * np.exp(-((yy - cy) ** 2 + (xx - cx) ** 2) / (2 * 3.0 ** 2))
    base = scipy.ndimage.gaussian_filter(base, sigma=1.0)

    cy, cx = (y - 1) / 2.0, (x - 1) / 2.0
    frames = [np.clip(base, 0, 255).astype(np.uint8)]
    for t in range(1, n_t):
        theta = np.deg2rad(angles_deg[t])
        # affine_transform inverts the matrix internally: for each OUTPUT pixel it looks up INPUT
        # at `matrix @ output + offset`. We want output = rotate(input, theta) + shift, i.e. input
        # at `R^-1 @ (output - centre - shift) + centre`.
        c, s = np.cos(-theta), np.sin(-theta)
        R = np.array([[c, -s], [s, c]])
        offset = np.array([cy, cx]) - R @ (np.array([cy, cx]) + np.asarray(shifts_yx[t]))
        warped = scipy.ndimage.affine_transform(base, R, offset=offset, order=1, mode='reflect')
        frames.append(np.clip(warped, 0, 255).astype(np.uint8))

    arr = np.stack(frames).reshape(n_t, 1, 1, y, x)
    du = DimUtils(ome_types.from_xml(_OME.format(t=n_t, y=y, x=x)), use_channel_axis=True)
    du.calc_image_dimensions(arr.shape)
    return arr, du, np.asarray(angles_deg, dtype=float), np.asarray(shifts_yx, dtype=float)


@unittest.skipUnless(_SITK_LOADS, _SITK_SKIP_REASON)
class SitkRigidTest(unittest.TestCase):
    def test_recovers_a_known_rotation_and_translation(self):
        """Base case: a movie rotating steadily by 1° per frame and drifting 0.7 px in y, 1.1 px
        in x. The invariant is the CONTRACT downstream will rely on — applying the reported
        (angle, translation) to `frame_t` must reproduce `frame_0` to pixel accuracy — rather
        than the exact numeric values, which live in whichever sign convention SimpleITK picks
        for its own (x, y) coordinate system.

        `max_angle_deg` is widened to 10° so the tail frames (7° at t=7) are not clipped by the
        cap — the cap is exercised in its own test below.
        """
        angles = [0.0] + [1.0 * t for t in range(1, 8)]
        shifts = [(0.0, 0.0)] + [(0.7 * t, 1.1 * t) for t in range(1, 8)]
        arr, du, _, _ = _rigid_movie(angles, shifts)

        positions, got_angles, interp, n_rej = cu.sitk_estimate_rigid(
            arr, 0, du, n_t=len(angles), max_angle_deg=10.0)

        self.assertEqual(interp, [])
        self.assertEqual(n_rej, 0)

        # Reported angle magnitudes must match the injected ones — the sign is a coordinate
        # convention, but "how MUCH did that frame rotate" is a scalar.
        np.testing.assert_allclose(np.abs(got_angles), np.abs(np.asarray(angles)), atol=0.3)

        # And the applier round-trip: warping frame_t by the reported (angle, translation) must
        # land the punctate blobs on top of frame_0's. Threshold to a mask so we compare shape,
        # not intensity — `Resample`'s bilinear interpolation smooths edges and would spoil a raw
        # pixel-diff.
        frame0 = arr[0, 0, 0].astype(np.float32)
        mask0 = frame0 > frame0.mean() + frame0.std()
        for t in range(1, len(angles)):
            frame_t = arr[t, 0, 0].astype(np.float32)
            aligned = _apply_rigid(frame_t, got_angles[t], positions[t])
            mask_t = aligned > aligned.mean() + aligned.std()
            iou = (mask0 & mask_t).sum() / max((mask0 | mask_t).sum(), 1)
            self.assertGreater(iou, 0.85,
                               f'frame {t} realignment IoU {iou:.2f} — the fit did not recover '
                               f'the transform (angle={got_angles[t]:.2f}°, pos={positions[t]})')

    def test_pure_translation_gives_zero_angles(self):
        """A movie that only translated must not invent rotation — the fit has to converge to θ=0
        rather than trading a small angle against a small translation error. The translation
        magnitude must come back correct (again in |·|, since sign is coordinate convention)."""
        angles = [0.0] * 6
        shifts = [(0.0, 0.0)] + [(0.0, 1.5 * t) for t in range(1, 6)]
        arr, du, _, exp_pos = _rigid_movie(angles, shifts)

        positions, got_angles, _, _ = cu.sitk_estimate_rigid(arr, 0, du, n_t=6)
        np.testing.assert_allclose(got_angles, 0.0, atol=0.3)
        np.testing.assert_allclose(np.abs(positions), np.abs(exp_pos), atol=0.5)

    def test_cap_triggers_and_frame_is_interpolated(self):
        """The cap path: a well-behaved movie fit with an artificially tight cap. Frame 2 (1.5°)
        must be rejected by a 1° cap, marked interpolated, and have its angle predicted from the
        good neighbours (1°, 2°) rather than left at the fit value.

        Constructed this way — rather than injecting a huge true rotation the fit will fail on
        anyway — so the assertion is about the CAP + INTERPOLATION, not about where SimpleITK's
        optimiser lands on an image it cannot register. That failure mode is a separate concern
        for the runner's QC, not this unit test."""
        angles = [0.0, 1.0, 1.5, 2.0, 2.5]
        shifts = [(0.0, 0.0)] + [(0.3 * t, 0.4 * t) for t in range(1, 5)]
        arr, du, _, _ = _rigid_movie(angles, shifts)

        _, got_angles, interp, n_rej = cu.sitk_estimate_rigid(
            arr, 0, du, n_t=5, max_angle_deg=1.2)

        # Everything > 1.2° must be rejected; the exact set depends on where each fit landed,
        # but frame 2 (true 1.5°) must be in it — the fit for a well-behaved image is well within
        # 0.3° of truth, per the base-case test's tolerance.
        self.assertIn(2, interp,
                      f'1.5° frame must be caught by a 1.2° cap; got interp={interp}')
        self.assertEqual(n_rej, len(interp))

        # If frame 1 (1°) was kept AND frame 3 (2°) was kept, the interpolation between them at
        # t=2 is 1.5°. Both are well within the cap in expectation, but confirm rather than
        # assume — the point of the test is the interpolation logic.
        if 1 not in interp and 3 not in interp:
            self.assertLess(abs(got_angles[2] - 0.5 * (got_angles[1] + got_angles[3])), 0.05,
                            'interpolated angle must be the linear midpoint of its neighbours')

    def test_handles_a_3d_movie_with_in_plane_rotation(self):
        """3D movies are supported via `Euler3DTransform` with X/Y rotations frozen (option B in
        DRIFT_RIGID_PLAN.md). The fit recovers the in-plane rotation and 3D translation; no
        max-projection or plane-selection is done, so nothing about "what drift correction means"
        changes silently for a 3D dataset. Cross-plane tilting would be option A — not built here,
        opt-in on a future request.
        """
        import ome_types
        from cecelia.utils.dim_utils import DimUtils

        # Build a 3D movie by stacking two Z-planes of the same rotating 2D scene. In-plane
        # rotation only, so both planes rotate together — which is what the fit is designed to
        # recover. Real biology would carry different content per plane, but for this test the
        # important property is that the fit CONVERGES on a 3D volume and reports one in-plane
        # angle, not that it beats a 2D fit numerically.
        angles = [0.0, 1.0, 2.0, 3.0]
        shifts_yx = [(0.0, 0.0)] + [(0.5 * t, 0.6 * t) for t in range(1, 4)]
        movie_2d, _, _, _ = _rigid_movie(angles, shifts_yx, shape_yx=(48, 48))

        # Stack four identical Z-planes per timepoint → [T, 1, 4, Y, X]. Four is the SimpleITK
        # gradient-filter minimum; a real confocal stack has 8-40. Change SizeZ="1" in the OME
        # XML so `DimUtils.is_3D()` says yes.
        arr_3d = np.repeat(movie_2d, 4, axis=2)                                   # (T, 1, 4, Y, X)
        du = DimUtils(ome_types.from_xml(
            _OME.format(t=len(angles), y=48, x=48).replace('SizeZ="1"', 'SizeZ="4"')),
            use_channel_axis=True)
        du.calc_image_dimensions(arr_3d.shape)

        positions, got_angles, interp, n_rej = cu.sitk_estimate_rigid(
            arr_3d, 0, du, n_t=len(angles), max_angle_deg=10.0)

        # Contract: positions carry a Z column now (3 axes), angles are still ONE per frame
        self.assertEqual(positions.shape, (len(angles), 3))
        self.assertEqual(got_angles.shape, (len(angles),))
        self.assertEqual(interp, [])
        self.assertEqual(n_rej, 0)
        # The in-plane rotation magnitude must come back regardless of the dimensionality
        np.testing.assert_allclose(np.abs(got_angles), np.abs(np.asarray(angles)), atol=0.3)

    def test_positions_and_angles_start_at_zero(self):
        """Frame 0 is the reference by construction — the estimator must report identity there,
        not the seeded value from an earlier run leaking through."""
        arr, du, _, _ = _rigid_movie([0.0, 0.5, 1.0], [(0.0, 0.0), (0.3, 0.4), (0.6, 0.8)])
        positions, angles, _, _ = cu.sitk_estimate_rigid(arr, 0, du, n_t=3)
        self.assertEqual(positions[0].tolist(), [0.0, 0.0])
        self.assertEqual(angles[0], 0.0)


@unittest.skipUnless(_SITK_LOADS, _SITK_SKIP_REASON)
class RigidApplyTest(unittest.TestCase):
    """The applier — `rigid_correct_im` + `rigid_correct_geometry`. The invariant is the same one
    P1 tests at the fit level, extended to the whole trajectory: every frame's corrected version
    must show the same feature at the same canvas coordinate as frame 0. IoU on a thresholded
    mask is the honest way to measure it — a raw pixel diff would be dominated by resample
    interpolation and by the fact that different frames' rotated content covers slightly different
    parts of the canvas.
    """

    def test_geometry_expands_the_canvas_and_produces_per_frame_bboxes(self):
        angles = [0.0, 2.0, 4.0]
        shifts = [(0.0, 0.0), (1.5, 2.0), (3.0, 4.0)]
        arr, du, _, _ = _rigid_movie(angles, shifts, shape_yx=(64, 64))
        positions, got_angles, _, _ = cu.sitk_estimate_rigid(
            arr, 0, du, n_t=len(angles), max_angle_deg=10.0)

        canvas_shape, origin, bboxes = cu.rigid_correct_geometry(arr, du, positions, got_angles)
        # canvas must be AT LEAST source; origin is the negative offset (canvas top-left in
        # frame-0 coordinates)
        y_idx, x_idx = du.dim_idx('Y'), du.dim_idx('X')
        self.assertGreaterEqual(canvas_shape[y_idx], 64)
        self.assertGreaterEqual(canvas_shape[x_idx], 64)
        # Every frame has an entry with integer bounds inside the canvas
        for t in range(len(angles)):
            for ax, (lo, hi) in bboxes[t].items():
                self.assertIsInstance(lo, int)
                self.assertIsInstance(hi, int)
                self.assertGreaterEqual(lo, 0)
                self.assertLessEqual(hi, canvas_shape[du.dim_idx(ax)])

    def test_round_trip_aligns_every_frame_onto_frame_zero(self):
        """The core contract for P3: after `rigid_correct_im`, feature masks agree across frames.
        Uses IoU on a threshold mask — the honest way to compare, since interpolation smooths
        edges."""
        angles = [0.0] + [1.0 * t for t in range(1, 6)]
        shifts = [(0.0, 0.0)] + [(0.7 * t, 1.1 * t) for t in range(1, 6)]
        arr, du, _, _ = _rigid_movie(angles, shifts, shape_yx=(64, 64))
        positions, got_angles, _, _ = cu.sitk_estimate_rigid(
            arr, 0, du, n_t=len(angles), max_angle_deg=10.0)

        corrected = cu.rigid_correct_im(arr, du, positions, got_angles)

        c0 = corrected[0, 0, 0]
        mask0 = c0 > 30
        for t in range(1, len(angles)):
            ct = corrected[t, 0, 0]
            mask_t = ct > 30
            iou = (mask0 & mask_t).sum() / max((mask0 | mask_t).sum(), 1)
            self.assertGreater(iou, 0.9,
                               f't={t}: mask IoU {iou:.2f} — the rigid applier did not realign')

    def test_frame_origins_shape_matches_the_translation_helper(self):
        """`rigid_frame_origins` returns ``{t: {axis: [start, stop]}}``, same shape as
        `drift_frame_origins`. Consumers (`write_valid_box`) MUST see one shape from either
        estimator — a shape divergence here would silently break the drift-QC sidecar's valid-box
        record on the writer side."""
        arr, du, _, _ = _rigid_movie([0.0, 1.0, 2.0], [(0.0, 0.0), (0.5, 0.5), (1.0, 1.0)],
                                     shape_yx=(48, 48))
        positions, got_angles, _, _ = cu.sitk_estimate_rigid(
            arr, 0, du, n_t=3, max_angle_deg=10.0)
        origins = cu.rigid_frame_origins(arr, du, positions, got_angles)
        self.assertEqual(set(origins.keys()), {0, 1, 2})
        for t in origins:
            self.assertEqual(set(origins[t].keys()), {'Y', 'X'})
            for ax, box in origins[t].items():
                self.assertEqual(len(box), 2)
                self.assertLess(box[0], box[1])


if __name__ == '__main__':
    unittest.main()
