"""The valid box: which part of a store is data, and which is padding a task added.

Lives on the STORE (namespaced `cecelia` attr), not in the producing task's QC sidecar, so a
consumer asks one question regardless of which task made the store — and gets None, meaning "all of
it", for the stores that never padded. That None case is most of them, and it is what lets a
consumer have one code path.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
import zarr

import cecelia.utils.zarr_utils as zu


def _store(path, shape=(2, 1, 8, 16, 12), nscales=2):
    g = zarr.open_group(path, mode="w", zarr_format=2)
    g.attrs["multiscales"] = zu.multiscales_metadata(
        ["T", "C", "Z", "Y", "X"], nscales, scale_for_axis={"Z": 2.0, "Y": 0.5, "X": 0.5})
    for lvl in range(nscales):
        s = list(shape)
        s[3] //= 2 ** lvl
        s[4] //= 2 ** lvl
        g.create_array(str(lvl), data=np.zeros(s, dtype=np.uint8), chunks=tuple(s))
    return path


class ValidBoxTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.d, True)

    def test_a_store_with_no_padding_reports_none(self):
        """The default, and the reason a consumer needs no knowledge of the producer."""
        p = _store(os.path.join(self.d, "plain.ome.zarr"))
        self.assertIsNone(zu.read_valid_box(p))
        self.assertIsNone(zu.read_valid_box(os.path.join(self.d, "does-not-exist")))

    def test_static_box_roundtrips(self):
        p = _store(os.path.join(self.d, "static.ome.zarr"))
        zu.write_valid_box(p, ["Z", "Y", "X"], {"Z": (1, 5), "Y": (2, 14), "X": (0, 12)})
        self.assertEqual(zu.read_valid_box(p), {"Z": (1, 5), "Y": (2, 14), "X": (0, 12)})

    def test_per_timepoint_box_and_its_union(self):
        p = _store(os.path.join(self.d, "moving.ome.zarr"))
        boxes = {0: {"Z": (0, 4), "Y": (0, 10), "X": (0, 12)},
                 1: {"Z": (3, 7), "Y": (2, 12), "X": (0, 12)}}
        zu.write_valid_box(p, ["Z", "Y", "X"], boxes)
        self.assertEqual(zu.read_valid_box(p, timepoint=0)["Z"], (0, 4))
        self.assertEqual(zu.read_valid_box(p, timepoint=1)["Z"], (3, 7))
        # no timepoint → the union, i.e. the smallest region containing every frame's data
        self.assertEqual(zu.read_valid_box(p)["Z"], (0, 7))
        self.assertEqual(zu.read_valid_box(p)["Y"], (0, 12))

    def test_level_rescales_xy_only_and_never_crops(self):
        """Level-n coordinates must not lose a pixel of real data, so start floors and stop ceils.
        XY halve per level; Z does not — the same rule the NGFF scale uses (DOWNSAMPLED_AXES),
        because a box and a scale expressed in level-0 pixels have to agree about what a level is."""
        p = _store(os.path.join(self.d, "lvl.ome.zarr"))
        zu.write_valid_box(p, ["Z", "Y", "X"], {"Z": (1, 5), "Y": (3, 11), "X": (1, 12)})
        self.assertEqual(zu.read_valid_box(p, level=1),
                         {"Z": (1, 5),            # Z is not downsampled
                          "Y": (1, 6),            # 3//2=1, ceil(11/2)=6  → contains [3,11)
                          "X": (0, 6)})           # 1//2=0, ceil(12/2)=6
        self.assertIn("Y", zu.read_valid_box(p, level=0))

    def test_writing_does_not_disturb_the_ngff_metadata(self):
        """It is a sibling attr, not an edit to `multiscales` — a store stays readable by anything
        that has never heard of it."""
        p = _store(os.path.join(self.d, "keep.ome.zarr"))
        before = zarr.open_group(p, mode="r").attrs["multiscales"]
        zu.write_valid_box(p, ["Z"], {"Z": (2, 6)})
        g = zarr.open_group(p, mode="r")
        self.assertEqual(g.attrs["multiscales"], before)
        self.assertEqual(zu.read_axes(p), ["t", "c", "z", "y", "x"])
        self.assertIsNotNone(zu.read_scale(p))

    def test_nested_series_layout(self):
        """Both on-disk layouts, via series_base — a bioformats2raw store must work too."""
        p = os.path.join(self.d, "nested.ome.zarr")
        root = zarr.open_group(p, mode="w", zarr_format=2)
        s = root.create_group("0")
        s.attrs["multiscales"] = zu.multiscales_metadata(["T", "C", "Z", "Y", "X"], 1)
        s.create_array("0", data=np.zeros((2, 1, 8, 16, 12), dtype=np.uint8), chunks=(2, 1, 8, 16, 12))
        zu.write_valid_box(p, ["Z"], {"Z": (1, 7)})
        self.assertEqual(zu.read_valid_box(p), {"Z": (1, 7)})


if __name__ == "__main__":
    unittest.main()
