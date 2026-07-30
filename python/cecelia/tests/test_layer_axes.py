"""Layer-axis alignment: a derived label store may have fewer axes than the image it came from, and
napari aligns a layer's dimensions against the viewer's **from the right**.

The bug this pins: `segment.branching` with `flattenBranching` (or a z==1 image, via the squeeze in
`_iterate_timepoints`) writes a (t,y,x) skeleton for a (t,z,y,x) image. napari read that layer's TIME
axis as Z — every timepoint stacked into one volume, "a tower on top of the actual image". Fixing up
`scale` cannot help; the dimensions themselves are misassigned. Three pieces have to agree:

  1. the writer declares the axes of the ARRAY  (`branching_run.output_axes` → `create_multiscales`)
  2. the reader aligns BY NAME                  (`napari_utils.expand_to_axes`)
  3. a store whose metadata disagrees with its array is REJECTED, not guessed at

No napari and no zarr needed for 1 + 2.
"""

import importlib.util
import os
import unittest

import numpy as np

from cecelia.utils.napari_utils import expand_to_axes, layer_ndim

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
RUNNER_PATH = os.path.join(REPO_ROOT, "app", "src", "tasks", "segment", "branching_run.py")


def _load_runner():
    spec = importlib.util.spec_from_file_location("branching_run_axes", RUNNER_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class TestExpandToAxes(unittest.TestCase):
    def test_flattened_timelapse_gets_a_singleton_z(self):
        # THE bug: (t,y,x) skeleton, (t,z,y,x) viewer → right-alignment made t render as z
        arr = np.zeros((201, 544, 548), dtype=np.uint32)
        out, ok = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (201, 1, 544, 548))   # z inserted where z belongs

    def test_static_volume_of_a_timelapse_gets_a_singleton_t(self):
        # the OTHER direction, which trailing-trim happened to get right: a (z,y,x) store
        arr = np.zeros((20, 64, 64))
        out, ok = expand_to_axes(arr, ["z", "y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (1, 20, 64, 64))

    def test_2d_store_of_a_3d_timelapse_inserts_both(self):
        arr = np.zeros((64, 64))
        out, ok = expand_to_axes(arr, ["y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (1, 1, 64, 64))

    def test_already_aligned_is_a_no_op_that_still_reports_ok(self):
        arr = np.zeros((7, 20, 283, 230))
        out, ok = expand_to_axes(arr, ["t", "z", "y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertIs(out, arr)

    def test_multiscale_list_expands_every_level(self):
        levels = [np.zeros((4, 16, 16)), np.zeros((4, 8, 8))]
        out, ok = expand_to_axes(levels, ["t", "y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual([a.shape for a in out], [(4, 1, 16, 16), (4, 1, 8, 8)])
        self.assertEqual(layer_ndim(out), 4)

    def test_insertion_is_lazy_for_a_dask_array(self):
        # the labels store is opened as dask; expanding must not materialise it
        try:
            import dask.array as da
        except ImportError:                                     # pragma: no cover
            self.skipTest("dask not installed")
        arr = da.zeros((5, 32, 32), chunks=(1, 32, 32))
        out, ok = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (5, 1, 32, 32))
        self.assertTrue(hasattr(out, "compute"))               # still lazy

    # ── the refusals: guessing here is what produced silently-wrong dimensions ──
    def test_rejects_metadata_that_does_not_describe_the_array(self):
        # the real Y7oL9h store: 4-D array, .zattrs claiming (t,c,z,y,x). Unreadable — must not act.
        arr = np.zeros((7, 20, 283, 230))
        out, ok = expand_to_axes(arr, ["t", "c", "z", "y", "x"], ["t", "z", "y", "x"])
        self.assertFalse(ok)
        self.assertIs(out, arr)

    def test_rejects_an_axis_the_viewer_does_not_have(self):
        arr = np.zeros((3, 64, 64))
        _, ok = expand_to_axes(arr, ["c", "y", "x"], ["t", "z", "y", "x"])
        self.assertFalse(ok)

    def test_rejects_a_transposed_store(self):
        # (y,x,t) can't be fixed by inserting axes — it needs a transpose, which we don't guess at
        arr = np.zeros((64, 64, 7))
        _, ok = expand_to_axes(arr, ["y", "x", "t"], ["t", "z", "y", "x"])
        self.assertFalse(ok)

    def test_rejects_more_axes_than_the_viewer(self):
        arr = np.zeros((2, 3, 4, 5, 6))
        _, ok = expand_to_axes(arr, ["t", "c", "z", "y", "x"], ["z", "y", "x"])
        self.assertFalse(ok)

    def test_rejects_missing_or_duplicated_names(self):
        arr = np.zeros((4, 8, 8))
        self.assertFalse(expand_to_axes(arr, None, ["t", "z", "y", "x"])[1])
        self.assertFalse(expand_to_axes(arr, ["t", "y", "x"], None)[1])
        self.assertFalse(expand_to_axes(arr, ["y", "y", "x"], ["t", "y", "x"])[1])

    def test_is_case_insensitive(self):
        # dim_utils speaks upper case ('T','Z','Y','X'); NGFF .zattrs store lower case
        arr = np.zeros((4, 8, 8))
        out, ok = expand_to_axes(arr, ["T", "Y", "X"], ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (4, 1, 8, 8))


class TestBranchingOutputAxes(unittest.TestCase):
    """What the writer must declare. The store's rank is not the image's: frames are squeezed (a z==1
    image loses Z) and `flattenBranching` Z-projects."""

    def setUp(self):
        self.runner = _load_runner()

    def test_flattened_timelapse(self):
        # (t,y,x) — the store that produced the tower
        self.assertEqual(self.runner.output_axes(3, True, 0), ["T", "Y", "X"])

    def test_3d_timelapse(self):
        self.assertEqual(self.runner.output_axes(4, True, 0), ["T", "Z", "Y", "X"])

    def test_static_2d_and_3d(self):
        self.assertEqual(self.runner.output_axes(2, False), ["Y", "X"])
        self.assertEqual(self.runner.output_axes(3, False), ["Z", "Y", "X"])

    def test_time_lands_at_its_stacked_position(self):
        # `np.stack(frames, axis=t_idx)` puts T wherever t_idx says, so the names must follow it
        self.assertEqual(self.runner.output_axes(4, True, 1), ["Z", "T", "Y", "X"])

    def test_names_always_match_the_rank(self):
        for ndim, has_t in ((2, False), (3, False), (3, True), (4, True)):
            self.assertEqual(len(self.runner.output_axes(ndim, has_t)), ndim)

    def test_the_writer_and_the_reader_agree(self):
        # end-to-end on the failing shape: what output_axes declares is what expand_to_axes can use
        arr = np.zeros((201, 544, 548), dtype=np.uint32)
        axes = self.runner.output_axes(arr.ndim, True, 0)
        out, ok = expand_to_axes(arr, axes, ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (201, 1, 544, 548))


if __name__ == "__main__":
    unittest.main()
