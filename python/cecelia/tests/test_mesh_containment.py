"""Unit tests for the mesh route's containment readout (cecelia.utils.mesh_utils).

Pins the case the measure exists for: an engulfed fragment sits in a HOLE of its host's segmentation
(dark in the host's channel), and must still read as contained — plus the count each host holds, the
one-host majority rule, and per-target separation."""
import unittest

import numpy as np

from cecelia.utils import mesh_utils


def _host_with_hole():
    """(z, y, x) volumes: B = one ring-shaped host (label 7) whose hole holds A fragment 1; A fragment
    2 sits outside; B host 8 elsewhere holds nothing."""
    b = np.zeros((3, 12, 20), dtype=np.uint32)
    b[:, 1:8, 1:8] = 7
    b[:, 3:6, 3:6] = 0                  # the hole the fragment leaves in the host's signal
    b[:, 1:6, 12:18] = 8
    a = np.zeros_like(b)
    a[1, 4, 4] = 1                      # inside the hole
    a[1, 10, 10] = 2                    # outside everything
    return a, b


class TestContainment(unittest.TestCase):
    def test_fill_label_holes_fills_into_background_only(self):
        _, b = _host_with_hole()
        f = mesh_utils.fill_label_holes(b, [7, 8])
        self.assertTrue((f[:, 3:6, 3:6] == 7).all())
        self.assertEqual(int((f == 8).sum()), int((b == 8).sum()))
        self.assertTrue((b[:, 3:6, 3:6] == 0).all())          # input untouched

    def test_fragment_in_a_hole_is_contained_and_counted(self):
        a, b = _host_with_hole()
        out = mesh_utils.containment_per_target(a, {1, 2}, b, [("tbm", {7, 8})])
        contained, n = out["tbm"]
        self.assertEqual(contained, {1: 1, 2: 0})
        self.assertEqual(n, {1: 0, 2: 0})
        # the other way round: hosts as A, fragments as B — the per-host count
        out = mesh_utils.containment_per_target(b, {7, 8}, a, [("frags", {1, 2})])
        self.assertEqual(out["frags"][1], {7: 1, 8: 0})

    def test_targets_are_separate(self):
        a, b = _host_with_hole()
        out = mesh_utils.containment_per_target(a, {1, 2}, b, [("t7", {7}), ("t8", {8})])
        self.assertEqual(out["t7"][0][1], 1)
        self.assertEqual(out["t8"][0][1], 0)

    def test_straddling_two_hosts_is_not_contained(self):
        b = np.zeros((1, 4, 8), dtype=np.uint32)
        b[0, :, :4] = 1
        b[0, :, 4:] = 2
        a = np.zeros_like(b)
        a[0, 1, 2:6] = 5                # half on host 1, half on host 2
        out = mesh_utils.containment_per_target(a, {5}, b, [("t", {1, 2})], min_frac=0.6)
        self.assertEqual(out["t"][0], {5: 0})


if __name__ == "__main__":
    unittest.main()
