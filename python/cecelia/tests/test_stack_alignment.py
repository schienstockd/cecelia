"""
Within-stack XY alignment (`estimate_stack_alignment` + `apply_stack_alignment`).

The estimator's job is to recover per-plane XY shifts that put every plane
back onto the reference plane's position. The applier's job is to warp the
input so those shifts are realised. The gate's job is to REFUSE to force a
shift when either (a) the PC peak isn't confident enough or (b) the
estimated shift is unreasonably large — because on a real Z stack with 5 µm
between planes those cases are more likely structural difference than
motion, and shifting them injects an artefact instead of removing one.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.correction_utils as cu

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{t}" SizeC="1" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


def _blurred_texture(y=96, x=96, seed=0):
    rng = np.random.default_rng(seed)
    im = rng.normal(0.5, 0.15, size=(y, x)).astype(np.float32)
    # A few strong blobs so PC has content to lock on to.
    for cy, cx in [(24, 30), (60, 45), (30, 72), (70, 78)]:
        yy, xx = np.mgrid[0:y, 0:x]
        im += 0.8 * np.exp(-((yy - cy) ** 2 + (xx - cx) ** 2) / (2 * 4.0 ** 2))
    return im


def _rolled(base, dy, dx):
    return np.roll(np.roll(base, dy, axis=0), dx, axis=1)


def _dim_utils_for(t, c, z, y, x):
    from cecelia.utils.dim_utils import DimUtils
    import ome_types
    ome = ome_types.from_xml(_OME.format(t=t, z=z, y=y, x=x))
    du = DimUtils(ome, use_channel_axis=True)
    du.calc_image_dimensions((t, c, z, y, x))
    return du


class PickRefTest(unittest.TestCase):
    def test_middle(self):
        stack = np.zeros((7, 32, 32), dtype=np.float32)
        self.assertEqual(cu._pick_ref(stack, 'middle'), 3)

    def test_explicit_int(self):
        stack = np.zeros((5, 32, 32), dtype=np.float32)
        self.assertEqual(cu._pick_ref(stack, 2), 2)

    def test_out_of_range_raises(self):
        stack = np.zeros((4, 32, 32), dtype=np.float32)
        with self.assertRaises(ValueError):
            cu._pick_ref(stack, 9)
        with self.assertRaises(ValueError):
            cu._pick_ref(stack, -1)

    def test_unknown_mode_raises(self):
        stack = np.zeros((3, 32, 32), dtype=np.float32)
        with self.assertRaises(ValueError):
            cu._pick_ref(stack, 'centroid')

    def test_sharpest_picks_the_sharp_plane(self):
        # A pyramid of blur widths — sharpest at z=1, plane 0 and 2 blurred.
        base = _blurred_texture()
        stack = np.stack([
            # Heavy gaussian blur — mimicked by averaging a shifted neighbourhood.
            0.25 * (np.roll(base, 4, 0) + np.roll(base, -4, 0)
                    + np.roll(base, 4, 1) + np.roll(base, -4, 1)),
            base,
            0.25 * (np.roll(base, 3, 0) + np.roll(base, -3, 0)
                    + np.roll(base, 3, 1) + np.roll(base, -3, 1)),
        ])
        self.assertEqual(cu._pick_ref(stack, 'sharpest'), 1)


class EstimateApplyTest(unittest.TestCase):
    def _make_movie(self, plane_shifts_per_t, y=96, x=96, seed=0):
        """Build a (T=1, C=1, Z, Y, X) movie where each plane is the SAME
        base texture rolled by `plane_shifts_per_t[t][z]`. The estimator
        should recover the shift that undoes each roll."""
        base = _blurred_texture(y=y, x=x, seed=seed)
        n_t = len(plane_shifts_per_t)
        n_z = len(plane_shifts_per_t[0])
        vol = np.zeros((n_t, 1, n_z, y, x), dtype=np.uint16)
        for t in range(n_t):
            for z, (dy, dx) in enumerate(plane_shifts_per_t[t]):
                plane = _rolled(base, dy, dx)
                # scale to uint16 range so PC has real dynamic range
                lo, hi = np.percentile(plane, (1, 99))
                plane = np.clip(65535 * (plane - lo) / (hi - lo + 1e-8), 0, 65535)
                vol[t, 0, z] = plane.astype(np.uint16)
        return vol, base

    def test_recovers_a_known_planar_shift(self):
        # z=0 shifted by (2, -3), z=1 is the ref, z=2 shifted by (-1, 2).
        shifts = [[(2, -3), (0, 0), (-1, 2)]]
        vol, _ = self._make_movie(shifts, seed=1)
        du = _dim_utils_for(1, 1, 3, 96, 96)
        est = cu.estimate_stack_alignment(vol, align_channel=0, dim_utils=du,
                                          reference='middle')
        # Shape and gate contract
        self.assertEqual(est.shifts.shape, (1, 3, 2))
        self.assertEqual(est.confidence.shape, (1, 3))
        self.assertEqual(est.applied.shape, (1, 3))
        self.assertTrue(est.applied[0, 1])                                # ref always applied
        self.assertEqual(est.ref_idx[0], 1)
        # Fits are the NEGATIVE of the injected roll — the shift the applier
        # will use to bring each plane BACK to the reference position.
        for z, (dy, dx) in enumerate(shifts[0]):
            if z == 1:
                continue
            got_dy, got_dx = est.shifts[0, z]
            self.assertLess(abs(got_dy - (-dy)), 1.0, msg=f"z={z} dy off")
            self.assertLess(abs(got_dx - (-dx)), 1.0, msg=f"z={z} dx off")
            self.assertTrue(est.applied[0, z], msg=f"z={z} rejected unexpectedly")

    def test_applier_restores_a_rolled_stack(self):
        shifts = [[(3, -4), (0, 0), (-2, 5)]]
        vol, base = self._make_movie(shifts, seed=2)
        du = _dim_utils_for(1, 1, 3, 96, 96)
        est = cu.estimate_stack_alignment(vol, align_channel=0, dim_utils=du,
                                          reference='middle')
        aligned = cu.apply_stack_alignment(vol, est, du)
        # After alignment every plane should look ~= plane 1 (the ref).
        # Compare central 60x60 window (avoid the zero-fill edges).
        ref = aligned[0, 0, 1, 18:78, 18:78].astype(np.float32)
        for z in range(3):
            got = aligned[0, 0, z, 18:78, 18:78].astype(np.float32)
            # Correlate — a well-aligned pair sits above 0.9 correlation.
            c = float(np.corrcoef(ref.ravel(), got.ravel())[0, 1])
            self.assertGreater(c, 0.9, msg=f"z={z} corr {c:.2f}")

    def test_gate_rejects_a_wildly_shifted_plane(self):
        # A plane 30 px off is over the default max_shift_px=8 clamp.
        shifts = [[(30, -30), (0, 0), (1, -1)]]
        vol, _ = self._make_movie(shifts, seed=3, y=128, x=128)
        du = _dim_utils_for(1, 1, 3, 128, 128)
        est = cu.estimate_stack_alignment(vol, align_channel=0, dim_utils=du,
                                          reference='middle')
        # z=0 is too far off — gate refused to apply.
        self.assertFalse(est.applied[0, 0])
        self.assertEqual(tuple(est.shifts[0, 0]), (0.0, 0.0))
        # z=2 is small — gate applied.
        self.assertTrue(est.applied[0, 2])

    def test_gate_rejects_low_confidence(self):
        # A plane of pure noise correlates poorly with the reference.
        du = _dim_utils_for(1, 1, 3, 96, 96)
        vol = np.zeros((1, 1, 3, 96, 96), dtype=np.uint16)
        base = _blurred_texture(seed=4)
        for z in (0, 1):
            lo, hi = np.percentile(base, (1, 99))
            vol[0, 0, z] = np.clip(65535 * (base - lo) / (hi - lo + 1e-8), 0, 65535).astype(np.uint16)
        # z=2 is unrelated noise.
        rng = np.random.default_rng(4)
        vol[0, 0, 2] = rng.integers(0, 65535, size=(96, 96), dtype=np.uint16)
        est = cu.estimate_stack_alignment(vol, align_channel=0, dim_utils=du,
                                          reference='middle',
                                          min_conf=0.5)
        self.assertFalse(est.applied[0, 2], "pure-noise plane should have failed the confidence gate")
        self.assertEqual(tuple(est.shifts[0, 2]), (0.0, 0.0))

    def test_reference_index_records_the_pick(self):
        # A sharpest ref explicit test would need a per-timepoint sharpness
        # asymmetry, which _make_movie above doesn't produce. Assert the
        # simpler property that `reference=1` reports back as ref_idx=1.
        shifts = [[(1, 0), (0, 0), (0, 1)]]
        vol, _ = self._make_movie(shifts, seed=5)
        du = _dim_utils_for(1, 1, 3, 96, 96)
        est = cu.estimate_stack_alignment(vol, align_channel=0, dim_utils=du,
                                          reference=1)
        self.assertEqual(est.ref_idx[0], 1)
        self.assertTrue(est.applied[0, 1])


if __name__ == '__main__':
    unittest.main()
