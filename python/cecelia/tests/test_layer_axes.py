"""Layer-axis alignment: a derived label store may have fewer axes than the image it came from, and
napari aligns a layer's dimensions against the viewer's **from the right**.

The bug this pins: `segment.branching` with `flattenBranching` (or a z==1 image, via the squeeze in
`_iterate_timepoints`) writes a (t,y,x) skeleton for a (t,z,y,x) image. napari read that layer's TIME
axis as Z — every timepoint stacked into one volume, "a tower on top of the actual image". Fixing up
`scale` cannot help; the dimensions themselves are misassigned. Three pieces have to agree:

  1. the writer declares the axes of the ARRAY  (`branching_run._store_axes` → `create_multiscales`)
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


class TestProjectionCurtain(unittest.TestCase):
    """A projected store belongs to the WHOLE volume, so it renders on every plane it collapsed.

    Inserting a singleton Z put a Z-MIP skeleton on plane 0 only — correct about the data, wrong about the
    meaning, and it read as "a separate layer floating beside the image". The old R version got this right
    by writing the MIP onto every Z plane *before* skeletonising (`create_branching.py`: "this will
    propagate the 2D image into 3D"), i.e. it duplicated the bytes. Here it is a lazy broadcast, so the
    store stays honest about having no Z and nothing is stored twice.
    """
    def test_z_projection_becomes_a_curtain(self):
        arr = np.zeros((201, 544, 548), dtype=np.uint32)
        out, ok = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"],
                                 viewer_shape=[201, 20, 544, 548])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (201, 20, 544, 548))      # every z plane, not just the first

    def test_it_stays_lazy_and_stores_nothing(self):
        try:
            import dask.array as da
        except ImportError:                                     # pragma: no cover
            self.skipTest("dask not installed")
        arr = da.zeros((201, 544, 548), chunks=(1, 544, 548), dtype=np.uint32)
        out, ok = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"],
                                 viewer_shape=[201, 20, 544, 548])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (201, 20, 544, 548))
        self.assertTrue(hasattr(out, "compute"))               # 4.8 GB if materialised — it never is
        # every z plane reads the SAME source plane, which is what makes it free
        self.assertTrue(np.array_equal(np.asarray(out[7, 0]), np.asarray(out[7, 19])))

    def test_only_the_INSERTED_axes_stretch(self):
        # a pyramid level's own Y/X must survive — stretching them would resample the level
        levels = [np.zeros((4, 16, 16)), np.zeros((4, 8, 8))]
        out, ok = expand_to_axes(levels, ["t", "y", "x"], ["t", "z", "y", "x"],
                                 viewer_shape=[4, 5, 16, 16])
        self.assertTrue(ok)
        self.assertEqual([a.shape for a in out], [(4, 5, 16, 16), (4, 5, 8, 8)])

    def test_omitting_the_extent_keeps_the_single_plane(self):
        arr = np.zeros((201, 544, 548), dtype=np.uint32)
        out, _ = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"])
        self.assertEqual(out.shape, (201, 1, 544, 548))

    def test_a_full_rank_store_is_untouched(self):
        arr = np.zeros((7, 20, 283, 230))
        out, ok = expand_to_axes(arr, ["t", "z", "y", "x"], ["t", "z", "y", "x"],
                                 viewer_shape=[7, 20, 283, 230])
        self.assertTrue(ok)
        self.assertIs(out, arr)                                # nothing inserted, nothing stretched

    def test_a_degenerate_extent_is_harmless(self):
        arr = np.zeros((4, 8, 8))
        for shape in ([4, 0, 8, 8], [4, 1, 8, 8], [4]):
            out, ok = expand_to_axes(arr, ["t", "y", "x"], ["t", "z", "y", "x"], viewer_shape=shape)
            self.assertTrue(ok)
            self.assertEqual(out.shape[0], 4)
            self.assertEqual(out.shape[-2:], (8, 8))


class TestWriterAndReaderAgree(unittest.TestCase):
    """The writer's declared axes must be usable by the reader's name alignment.

    `branching_run._store_axes` is the writer half (it shipped with the anisotropy work, finding A8:
    the store is not the image's shape — labels drop C, `integrateTime` drops T, `flattenBranching`
    drops Z). This asserts the two halves MEET: what the writer declares is exactly what
    `expand_to_axes` needs to put a short store's axes back where they belong. Nothing else pins that
    contract, and it is the one that decides whether a Z-projected timelapse renders as a tower.
    """

    def setUp(self):
        self.runner = _load_runner()

    def test_z_projected_timelapse_round_trips(self):
        # the store that produced the tower: (t,y,x) over a 3D+t image
        arr = np.zeros((201, 544, 548), dtype=np.uint32)
        axes = self.runner._store_axes(["T", "C", "Z", "Y", "X"], has_time=True, is_3d=False)
        self.assertEqual(axes, ["T", "Y", "X"])           # C dropped (labels), Z dropped (projected)
        out, ok = expand_to_axes(arr, axes, ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (201, 1, 544, 548))   # Z reinserted where Z belongs

    def test_time_collapsed_store_round_trips(self):
        # `integrateTime` drops T instead
        arr = np.zeros((20, 544, 548), dtype=np.uint32)
        axes = self.runner._store_axes(["T", "C", "Z", "Y", "X"], has_time=False, is_3d=True)
        self.assertEqual(axes, ["Z", "Y", "X"])
        out, ok = expand_to_axes(arr, axes, ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertEqual(out.shape, (1, 20, 544, 548))

    def test_full_store_needs_no_expansion(self):
        arr = np.zeros((7, 20, 283, 230), dtype=np.uint32)
        axes = self.runner._store_axes(["T", "C", "Z", "Y", "X"], has_time=True, is_3d=True)
        self.assertEqual(axes, ["T", "Z", "Y", "X"])
        out, ok = expand_to_axes(arr, axes, ["t", "z", "y", "x"])
        self.assertTrue(ok)
        self.assertIs(out, arr)

    def test_declared_axes_always_match_the_stored_rank(self):
        for has_t, is_3d, rank in ((True, True, 4), (True, False, 3), (False, True, 3), (False, False, 2)):
            axes = self.runner._store_axes(["T", "C", "Z", "Y", "X"], has_time=has_t, is_3d=is_3d)
            self.assertEqual(len(axes), rank)
            # …and a rank mismatch is exactly what expand_to_axes must refuse
            self.assertFalse(expand_to_axes(np.zeros((9,) * (rank + 1)), axes, ["t", "z", "y", "x"])[1])


if __name__ == "__main__":
    unittest.main()
