"""Unit tests for the mesh route's enclosure readout (cecelia.utils.mesh_utils.enclosure_per_target).

Pins the case the measure exists for: an engulfed fragment sits in a vacuole of its host's
segmentation — open on one side in real data — and must read as mostly enclosed; plus the count each
host wraps, A and B gated from ONE label image, and per-target separation."""
import unittest

import numpy as np

from cecelia.utils import mesh_utils


def _host_with_open_vacuole():
    """(z, y, x) label volume: host 7 wraps fragment 1 in a vacuole OPEN to the top (y) through a channel;
    fragment 2 sits in empty space; host 8 elsewhere wraps nothing."""
    v = np.zeros((7, 16, 24), dtype=np.uint32)
    v[:, 1:12, 1:12] = 7
    v[2:5, 4:7, 5:7] = 1                # the fragment, in a vacuole that hugs it…
    v[2:5, 0:4, 5:7] = 0                # …open to the outside through one channel
    v[3, 14, 18] = 2                    # a fragment in the open
    v[:, 1:6, 16:22] = 8
    return v


class TestEnclosure(unittest.TestCase):
    def test_fragment_in_an_open_vacuole_is_mostly_enclosed(self):
        v = _host_with_open_vacuole()
        enc, held = mesh_utils.enclosure_per_target(v, {1, 2}, v, [("tbm", {7, 8})])["tbm"]
        self.assertGreater(enc[1], 0.6)                         # most of its shell is host…
        self.assertLess(enc[1], 1.0)                            # …but not all: the vacuole is open
        self.assertEqual(enc[2], 0.0)
        self.assertEqual(held, {1: 0, 2: 0})

    def test_the_host_counts_the_fragment_it_wraps(self):
        v = _host_with_open_vacuole()
        _, held = mesh_utils.enclosure_per_target(v, {7, 8}, v, [("frags", {1, 2})])["frags"]
        self.assertEqual(held, {7: 1, 8: 0})

    def test_fragment_and_host_in_separate_label_images(self):
        v = _host_with_open_vacuole()
        a = np.where(np.isin(v, [1, 2]), v, 0)
        b = np.where(np.isin(v, [7, 8]), v, 0)
        enc, _ = mesh_utils.enclosure_per_target(a, {1, 2}, b, [("tbm", {7, 8})])["tbm"]
        self.assertGreater(enc[1], 0.6)
        self.assertEqual(enc[2], 0.0)

    def test_targets_are_separate(self):
        v = _host_with_open_vacuole()
        out = mesh_utils.enclosure_per_target(v, {1}, v, [("t7", {7}), ("t8", {8})])
        self.assertGreater(out["t7"][0][1], 0.6)
        self.assertEqual(out["t8"][0][1], 0.0)

    def test_a_gap_round_the_fragment_lowers_the_share(self):
        # the shell is 2 voxels, so a 1-voxel gap between fragment and host counts against it — the
        # measure is "how much of its surroundings is host", and a threshold is the gate's job
        v = np.zeros((7, 16, 16), dtype=np.uint32)
        v[:, 1:14, 1:14] = 7
        v[1:6, 4:10, 4:10] = 0
        v[2:5, 5:9, 5:9] = 1
        enc, _ = mesh_utils.enclosure_per_target(v, {1}, v, [("t", {7})])["t"]
        self.assertGreater(enc[1], 0.1)
        self.assertLess(enc[1], mesh_utils.ENCLOSED_FRAC)

    def test_touching_side_by_side_is_partial_not_enclosed(self):
        # two cubes face to face: one face of the shell is the neighbour, the rest is background
        v = np.zeros((8, 8, 16), dtype=np.uint32)
        v[2:6, 2:6, 2:6] = 1
        v[2:6, 2:6, 6:10] = 5
        enc, _ = mesh_utils.enclosure_per_target(v, {1}, v, [("t", {5})])["t"]
        self.assertGreater(enc[1], 0.0)
        self.assertLess(enc[1], mesh_utils.ENCLOSED_FRAC)


if __name__ == "__main__":
    unittest.main()
