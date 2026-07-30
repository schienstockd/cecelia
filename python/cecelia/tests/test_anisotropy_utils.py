"""Tests for `cecelia.utils.anisotropy_utils`.

**Correctness is established against synthetic fields with a KNOWN orientation**, not against the
old R implementation. The legacy tangent tensor is reproduced here too, but as a *comparison
baseline* — it was eyeballed rather than validated, so "we agree with legacy" is an explanation of
differences, never a proof of correctness. Both estimators are held to the same synthetic oracle;
if one of them failed it, that would be the finding.

The load-bearing assertion in this file is `OrientationConventionTest` — that the two estimators
read the fibre direction off OPPOSITE ends of the spectrum. A regression there is silent in
production (arrows still look like a vector field, they just point 90° wrong).
"""
import unittest

import numpy as np
import skimage.morphology

from cecelia.utils import anisotropy_utils as aniso


def _striped_field(angle_deg: float, size: int = 180, period: int = 12,
                   half_width: float = 1.0) -> np.ndarray:
    """Parallel stripes running at `angle_deg`, as a float image. The fibre direction is
    `(sin θ, cos θ)` in (row, col) order — i.e. θ measured from the +column (x) axis."""
    th = np.radians(angle_deg)
    direction = np.array([np.sin(th), np.cos(th)])
    perp = np.array([-direction[1], direction[0]])
    rr, cc = np.mgrid[0:size, 0:size]
    s = rr * perp[0] + cc * perp[1]
    img = (np.abs(((s + period / 4) % period) - period / 2) < half_width).astype(np.float32)
    return img, direction


def _thin(img: np.ndarray) -> np.ndarray:
    """Skeletonise before handing anything to `tangent_tensor_field`.

    Not optional: `skan.Skeleton` walks pixel adjacency, so a multi-pixel-wide stripe reads as a
    thicket of spurious junctions and its edge directions scatter — the same trap that corrupted
    the branching topology in PR #396. Passing the raw mask here made the tangent tensor miss a
    30° field by 59°.
    """
    return skimage.morphology.skeletonize(img > 0.5)


ANGLES = (0.0, 30.0, 45.0, 60.0, 90.0, 135.0)


class PoolByBoxTest(unittest.TestCase):
    def test_2d_mean_pool(self):
        x = np.arange(16, dtype=np.float32).reshape(4, 4)
        np.testing.assert_allclose(aniso.pool_by_box(x, 2),
                                   np.array([[2.5, 4.5], [10.5, 12.5]], dtype=np.float32))

    def test_trims_trailing_pixels(self):
        x = np.ones((5, 5), dtype=np.float32)     # 5 not divisible by 2 → last row/col dropped
        out = aniso.pool_by_box(x, 2)
        self.assertEqual(out.shape, (2, 2))
        np.testing.assert_allclose(out, 1.0)

    def test_3d(self):
        x = np.ones((4, 6, 8), dtype=np.float32)
        self.assertEqual(aniso.pool_by_box(x, 2).shape, (2, 3, 4))


class BoxCentresTest(unittest.TestCase):
    def test_centre_of_each_box(self):
        c = aniso.box_centres((4, 6), 2)          # 2x3 boxes of side 2
        self.assertEqual(c.shape, (2, 3, 2))
        np.testing.assert_allclose(c[0, 0], [1.0, 1.0])
        np.testing.assert_allclose(c[1, 2], [3.0, 5.0])

    def test_matches_pool_by_box_grid(self):
        """The centres grid and the pooled grid must agree on shape, or every consumer that zips
        them together silently misaligns coordinates with values."""
        x = np.zeros((97, 53), dtype=np.float32)  # deliberately not divisible
        self.assertEqual(aniso.box_centres(x.shape, 10).shape[:2],
                         aniso.pool_by_box(x, 10).shape)


class StructureTensorOrientationTest(unittest.TestCase):
    """The oracle: stripes at a known angle, recovered via `fibre_orientation`."""

    def test_recovers_known_angles(self):
        for deg in ANGLES:
            with self.subTest(angle=deg):
                img, truth = _striped_field(deg)
                _, ev, evec, _ = aniso.structure_tensor_field(img, sigma=2.0, box=60)
                direction, _ = aniso.fibre_orientation(ev, evec)
                err = float(np.mean(aniso.acute_angle(direction, truth)))
                self.assertLess(err, 5.0, f"{deg}°: minor eigenvector off by {err:.1f}°")

    def test_major_eigenvector_is_across_the_fibre(self):
        """Guards the 90° trap directly: whatever else changes, the MAJOR eigenvector must stay
        perpendicular. If this starts passing at ~0°, the layout flipped and every quiver is wrong."""
        img, truth = _striped_field(30.0)
        _, ev, evec, _ = aniso.structure_tensor_field(img, sigma=2.0, box=60)
        major = evec[..., -1, :]
        err = float(np.mean(aniso.acute_angle(major, truth)))
        self.assertGreater(err, 85.0)

    def test_coherence_high_on_stripes_low_on_noise(self):
        img, _ = _striped_field(0.0)
        _, ev, _, coh = aniso.structure_tensor_field(img, sigma=2.0, box=60)
        self.assertGreater(float(coh.mean()), 0.8)
        rng = np.random.default_rng(0)
        _, _, _, coh_n = aniso.structure_tensor_field(
            rng.random((180, 180)).astype(np.float32), sigma=2.0, box=60)
        self.assertLess(float(coh_n.mean()), 0.3)

    def test_3d_recovers_a_fibre_axis(self):
        """A line-like structure in 3D: the structure tensor has ONE small eigenvalue, along the
        fibre — so the minor eigenvector is still the answer in 3D."""
        vol = np.zeros((40, 40, 40), dtype=np.float32)
        vol[22:26, 22:26, :] = 1.0                      # a rod along the last (x) axis
        _, ev, evec, _ = aniso.structure_tensor_field(vol, sigma=2.0, box=20)
        direction, _ = aniso.fibre_orientation(ev, evec)
        # grid is (2, 2, 2); the rod sits in z-box 1, y-box 1 and runs through both x-boxes
        centre = direction[1, 1, 1, :]
        err = float(aniso.acute_angle(centre, np.array([0.0, 0.0, 1.0])))
        self.assertLess(err, 10.0)


class TangentTensorTest(unittest.TestCase):
    """The legacy estimator, held to the SAME oracle. Comparison baseline, not ground truth."""

    def test_recovers_known_angles_via_major_eigenvector(self):
        for deg in ANGLES:
            with self.subTest(angle=deg):
                img, truth = _striped_field(deg)
                _, ev, evec, _ = aniso.tangent_tensor_field(_thin(img), box=60, radius=120)
                direction, _ = aniso.tangent_orientation(ev, evec)
                err = float(np.mean(aniso.acute_angle(direction, truth)))
                self.assertLess(err, 5.0, f"{deg}°: major eigenvector off by {err:.1f}°")


class OrientationConventionTest(unittest.TestCase):
    """THE regression guard. The two estimators must disagree by ~90° on which eigenvector is the
    fibre — that asymmetry is the whole reason `fibre_orientation` and `tangent_orientation` are
    separate functions. Collapsing them into one would silently rotate every quiver."""

    def test_conventions_are_orthogonal(self):
        img, truth = _striped_field(30.0)
        _, s_val, s_vec, _ = aniso.structure_tensor_field(img, sigma=2.0, box=60)
        _, t_val, t_vec, _ = aniso.tangent_tensor_field(_thin(img), box=60, radius=120)

        s_right, _ = aniso.fibre_orientation(s_val, s_vec)      # minor  → correct
        t_right, _ = aniso.tangent_orientation(t_val, t_vec)    # major  → correct
        self.assertLess(float(np.mean(aniso.acute_angle(s_right, truth))), 5.0)
        self.assertLess(float(np.mean(aniso.acute_angle(t_right, truth))), 5.0)

        # …and using the OTHER one's convention is the 90° failure we're guarding against.
        s_wrong, _ = aniso.tangent_orientation(s_val, s_vec)
        t_wrong, _ = aniso.fibre_orientation(t_val, t_vec)
        self.assertGreater(float(np.mean(aniso.acute_angle(s_wrong, truth))), 85.0)
        self.assertGreater(float(np.mean(aniso.acute_angle(t_wrong, truth))), 85.0)


class WeightedAnisotropyTest(unittest.TestCase):
    def test_weighting_ignores_empty_boxes(self):
        """The point of length-weighting: background boxes must not drag the image scalar around.
        Unweighted, adding empty field changes the number; weighted, it doesn't."""
        coh = np.array([[0.8, 0.8], [0.0, 0.0]], dtype=np.float32)
        blen = np.array([[100.0, 100.0], [0.0, 0.0]], dtype=np.float32)
        self.assertAlmostEqual(aniso.weighted_anisotropy(coh, blen), 0.8, places=5)
        self.assertAlmostEqual(float(coh.mean()), 0.4, places=5)      # the unweighted answer

    def test_empty_field_is_zero_not_nan(self):
        z = np.zeros((3, 3), dtype=np.float32)
        self.assertEqual(aniso.weighted_anisotropy(z, z), 0.0)

    def test_box_lengths_counts_skeleton_pixels(self):
        sk = np.zeros((4, 4), dtype=bool)
        sk[0, :2] = True                                  # 2 px, both in the top-left 2x2 box
        np.testing.assert_allclose(aniso.box_lengths(sk, 2), [[2.0, 0.0], [0.0, 0.0]])


class AcuteAngleTest(unittest.TestCase):
    def test_folds_to_0_90(self):
        a = np.array([1.0, 0.0])
        self.assertAlmostEqual(float(aniso.acute_angle(a, np.array([1.0, 0.0]))), 0.0, places=4)
        # antiparallel is the SAME orientation, not 180° away — a fibre has no head or tail
        self.assertAlmostEqual(float(aniso.acute_angle(a, np.array([-1.0, 0.0]))), 0.0, places=4)
        self.assertAlmostEqual(float(aniso.acute_angle(a, np.array([0.0, 1.0]))), 90.0, places=4)
        self.assertAlmostEqual(float(aniso.acute_angle(a, np.array([-1.0, 1.0]))), 45.0, places=4)

    def test_zero_vector_does_not_nan(self):
        self.assertEqual(float(aniso.acute_angle(np.array([0.0, 0.0]), np.array([1.0, 0.0]))), 90.0)


class NeighbourConsistencyTest(unittest.TestCase):
    def test_uniform_field_is_zero_random_is_about_45(self):
        uniform = np.zeros((8, 8, 2)); uniform[..., 1] = 1.0
        self.assertAlmostEqual(aniso.neighbour_consistency(uniform), 0.0, places=4)
        rng = np.random.default_rng(1)
        th = rng.uniform(0, np.pi, size=(40, 40))
        rand = np.stack([np.sin(th), np.cos(th)], axis=-1)
        self.assertGreater(aniso.neighbour_consistency(rand), 35.0)

    def test_mask_restricts_to_valid_boxes(self):
        field = np.zeros((4, 4, 2)); field[..., 1] = 1.0
        field[2:, :, :] = np.array([1.0, 0.0])            # bottom half rotated 90°
        valid = np.zeros((4, 4), dtype=bool); valid[:2, :] = True
        self.assertAlmostEqual(aniso.neighbour_consistency(field, valid), 0.0, places=4)


class DirectionContrastTest(unittest.TestCase):
    """`direction_contrast` must reward real spatial structure and refuse to be gamed by blurring —
    the failure mode that made `neighbour_consistency` alone useless as a tuning objective."""

    def _field(self, angles):
        return np.stack([np.sin(angles), np.cos(angles)], axis=-1)

    def test_uniform_field_has_no_contrast(self):
        """Perfectly smooth: neighbours agree AND distant boxes agree → contrast 0. This is the
        oversmoothed case; `neighbour_consistency` alone would score it PERFECT."""
        out = aniso.direction_contrast(self._field(np.zeros((20, 20))))
        self.assertAlmostEqual(out["near_deg"], 0.0, places=3)
        self.assertAlmostEqual(out["contrast_deg"], 0.0, places=3)

    def test_noise_field_has_no_contrast(self):
        rng = np.random.default_rng(2)
        out = aniso.direction_contrast(self._field(rng.uniform(0, np.pi, size=(40, 40))))
        self.assertGreater(out["near_deg"], 35.0)
        self.assertLess(abs(out["contrast_deg"]), 8.0)

    def test_structured_field_beats_both_degenerate_cases(self):
        """A smoothly rotating orientation is the field we WANT: neighbours agree, distant boxes
        don't. The property under test is the ordering — structured must out-score both the
        oversmoothed and the noise field, which is exactly what `neighbour_consistency` alone
        cannot do (uniform beats everything on that metric)."""
        rng = np.random.default_rng(3)
        rr, _ = np.mgrid[0:40, 0:40]
        structured = aniso.direction_contrast(self._field(rr * (np.pi / 40)), far_lag=8)
        uniform = aniso.direction_contrast(self._field(np.zeros((40, 40))), far_lag=8)
        noise = aniso.direction_contrast(self._field(rng.uniform(0, np.pi, (40, 40))), far_lag=8)
        self.assertLess(structured["near_deg"], 10.0)
        self.assertGreater(structured["contrast_deg"], uniform["contrast_deg"] + 10.0)
        self.assertGreater(structured["contrast_deg"], noise["contrast_deg"] + 10.0)

    def test_contrast_grows_with_separation(self):
        """Sanity on the lag knob: the further apart the compared boxes, the less they should
        agree on a field with a finite correlation length."""
        rr, _ = np.mgrid[0:40, 0:40]
        f = self._field(rr * (np.pi / 40))
        self.assertGreater(aniso.direction_contrast(f, far_lag=16)["contrast_deg"],
                           aniso.direction_contrast(f, far_lag=4)["contrast_deg"])


class CompareFieldsTest(unittest.TestCase):
    def test_reports_both_estimators_and_their_disagreement(self):
        img, _ = _striped_field(30.0)
        out = aniso.compare_fields(img, _thin(img), sigma=2.0, box=60)
        self.assertIn("structure", out)
        self.assertIn("tangent_reference", out)
        # On a clean synthetic field the two agree closely — the real-image disagreement this
        # function exists to quantify is a property of the DATA, not of the estimators.
        self.assertLess(out["mean_angle_between_deg"], 10.0)
        self.assertGreater(out["n_boxes_with_skeleton"], 0)


if __name__ == "__main__":
    unittest.main()
