"""Unit tests for `branching_run.py` — the RUNNER's own logic.

Pure-numpy, no zarr / h5ad / Julia. The anisotropy MATHS moved to
`cecelia.utils.anisotropy_utils` and is tested in `test_anisotropy_utils.py` against synthetic
known-angle fields; what stays here is the runner's plumbing around it — choosing the input array,
reconciling ranks, and keeping labels aligned across timepoints. Pins:

- `_anisotropy_input` returns the array `anisotropySource` names.
- `_match_rank` Z-MIPs a 3D fibre channel down to a Z-MIPed skeleton's rank (finding A3).
- `_globalise_labels` keeps the labels zarr and the h5ad `label` column in lockstep.
- `_skeletonise` never dilates before skan reads the topology (the PR #396 regression).

The runner file lives at `app/src/tasks/segment/branching_run.py` (not part of the `cecelia` IO
package), so this test loads it by absolute path via importlib — mirroring how `run_py` calls it.
"""
import importlib.util
import os
import tempfile
import unittest

import numpy as np
import pandas as pd


HERE = os.path.dirname(os.path.abspath(__file__))
# python/cecelia/tests/ → ../.. → repo root
REPO_ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
RUNNER_PATH = os.path.join(REPO_ROOT, "app", "src", "tasks", "segment", "branching_run.py")


def _load_runner():
    spec = importlib.util.spec_from_file_location("branching_run", RUNNER_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod





class _FakeDimUtils:
    """Duck-typed DimUtils covering the bits `_extract_fibre_image` reaches for. Keeps this
    test off the real DimUtils constructor's need for an OME-XML."""
    def __init__(self, axes: str, shape):
        self._axes = axes
        self._shape = shape
    def dim_idx(self, name, ignore_channel=False, ignore_time=False):
        return self._axes.index(name)
    def is_timeseries(self):
        return "T" in self._axes


class ExtractFibreImageTest(unittest.TestCase):
    """`_extract_fibre_image` must max-merge the selected fibre channels for the current
    timepoint (or the whole static image), regardless of where C/T sit in the axis order.
    Getting this wrong silently produces the wrong anisotropy input on non-canonical layouts."""

    def test_4d_TCYX_selects_timepoint_and_merges_channels(self):
        mod = _load_runner()
        # (T=3, C=4, Y=6, X=6). Channel 0: constant 5. Channel 1: constant 10. Others zero.
        im = np.zeros((3, 4, 6, 6), dtype=np.float32)
        im[:, 0] = 5.0
        im[:, 1] = 10.0
        du = _FakeDimUtils("TCYX", im.shape)
        out = mod._extract_fibre_image(im, du, fibre_channels=[0, 1], t_index=2)
        # max-merge of channels 0 (=5) and 1 (=10) at timepoint 2 → all-10 (Y, X)
        self.assertEqual(out.shape, (6, 6))
        np.testing.assert_allclose(out, 10.0)

    def test_3d_static_CYX(self):
        mod = _load_runner()
        # No time axis: (C=2, Y=5, X=5). channel 0 = 3, channel 1 = 7.
        im = np.zeros((2, 5, 5), dtype=np.float32)
        im[0] = 3.0
        im[1] = 7.0
        du = _FakeDimUtils("CYX", im.shape)
        out = mod._extract_fibre_image(im, du, fibre_channels=[1], t_index=None)
        self.assertEqual(out.shape, (5, 5))
        np.testing.assert_allclose(out, 7.0)


class AnisotropyInputTest(unittest.TestCase):
    """`anisotropySource` picks the array; `_match_rank` reconciles it with the skeleton's rank."""

    def test_source_selection(self):
        mod = _load_runner()
        labels = np.array([[0, 3], [3, 0]], dtype=np.uint32)
        skel = np.array([[False, True], [False, False]])
        fibre = np.array([[7.0, 8.0], [9.0, 10.0]], dtype=np.float32)
        np.testing.assert_allclose(
            mod._anisotropy_input("skeleton", fibre, labels, skel), [[0, 1], [0, 0]])
        np.testing.assert_allclose(
            mod._anisotropy_input("mask", fibre, labels, skel), [[0, 1], [1, 0]])
        np.testing.assert_allclose(
            mod._anisotropy_input("channel", fibre, labels, skel), fibre)

    def test_channel_source_without_channels_is_an_error_not_a_silent_zero(self):
        mod = _load_runner()
        with self.assertRaises(ValueError):
            mod._anisotropy_input("channel", None, np.zeros((2, 2), np.uint32),
                                  np.zeros((2, 2), bool))

    def test_match_rank_z_mips_a_3d_channel_onto_a_2d_skeleton(self):
        """Finding A3: with `flattenBranching` the labels are Z-MIPed (2D skeleton) while the
        fibre channel is still 3D. Unreconciled, the 3D branch wrote a `box_total_length` grid of
        a different rank to its four siblings — silently, because nothing raised."""
        mod = _load_runner()
        vol = np.zeros((3, 4, 4), dtype=np.float32)
        vol[2] = 5.0
        out = mod._match_rank(vol, 2)
        self.assertEqual(out.shape, (4, 4))
        np.testing.assert_allclose(out, 5.0)          # MIP, not a slice

    def test_match_rank_is_a_noop_at_equal_rank(self):
        mod = _load_runner()
        x = np.ones((4, 4), dtype=np.float32)
        self.assertIs(mod._match_rank(x, 2), x)


class IntegrateTimeTest(unittest.TestCase):
    """`integrateTime` collapses the stack to ONE network for the whole movie."""

    def test_collapse_modes(self):
        mod = _load_runner()
        a = np.array([[[1.0, 9.0]], [[5.0, 1.0]]])      # (T=2, 1, 2)
        np.testing.assert_allclose(mod._collapse_time(a, 0, "max"), [[5.0, 9.0]])
        np.testing.assert_allclose(mod._collapse_time(a, 0, "avg"), [[3.0, 5.0]])

    def test_unknown_mode_falls_back_to_max(self):
        mod = _load_runner()
        a = np.array([[1.0], [5.0]])
        np.testing.assert_allclose(mod._collapse_time(a, 0, "nonsense"), [5.0])

    def test_iterate_timepoints_honours_the_has_time_override(self):
        """After a T-collapse `dim_utils` still describes the SOURCE image, so the iterator has to
        be told the stack is no longer a timeseries — otherwise it slices a collapsed array."""
        mod = _load_runner()
        du = _FakeDimUtils("TYX", (3, 4, 4))
        arr = np.zeros((4, 4), dtype=np.uint32)
        out = list(mod._iterate_timepoints(arr, du, has_time=False))
        self.assertEqual(len(out), 1)
        self.assertIsNone(out[0][0])


class PerBranchAnisotropyTest(unittest.TestCase):
    """Each branch takes the coherence of the grid box it sits in, as an obs column.

    This is what makes anisotropy a BRANCH measurement — readable through
    `pop_df(img, "branch", …)` and therefore comparable across branch populations, images and
    treatments by the ordinary plot machinery. A whole-image scalar can't do any of that."""

    def _df(self, ys, xs, ts=None):
        n = len(ys)
        d = {"label": np.arange(1, n + 1),
             "image-coord-src-0": np.asarray(ys, float), "image-coord-src-1": np.asarray(xs, float),
             "image-coord-dst-0": np.asarray(ys, float), "image-coord-dst-1": np.asarray(xs, float)}
        if ts is not None:
            d["centroid_t"] = np.asarray(ts, float)
        return pd.DataFrame(d)

    def test_box_lookup_is_integer_division_on_the_centroid(self):
        mod = _load_runner()
        grid = np.array([[0.10, 0.20], [0.30, 0.40]], dtype=np.float32)   # 2x2 boxes of side 10
        df = self._df(ys=[1, 1, 15, 15], xs=[1, 15, 1, 15])
        out = mod._per_branch_anisotropy(df, [grid], [0], box=10, n_spatial=2)
        np.testing.assert_allclose(out, [0.10, 0.20, 0.30, 0.40], rtol=1e-6)

    def test_outside_the_grid_is_nan_not_a_clamped_neighbour(self):
        """`pool_by_box` trims trailing pixels, so branches past the last whole box have no value.
        Clamping them to the edge box would invent a measurement."""
        mod = _load_runner()
        grid = np.array([[0.5]], dtype=np.float32)
        out = mod._per_branch_anisotropy(self._df(ys=[1, 99], xs=[1, 99]), [grid], [0],
                                         box=10, n_spatial=2)
        self.assertAlmostEqual(float(out[0]), 0.5, places=6)
        self.assertTrue(np.isnan(out[1]))

    def test_uses_each_branch_own_timepoint(self):
        mod = _load_runner()
        f0 = np.array([[0.1]], dtype=np.float32)
        f1 = np.array([[0.9]], dtype=np.float32)
        out = mod._per_branch_anisotropy(self._df(ys=[1, 1], xs=[1, 1], ts=[0, 1]),
                                         [f0, f1], [0, 1], box=10, n_spatial=2)
        np.testing.assert_allclose(out, [0.1, 0.9], rtol=1e-6)

    def test_t_collapsed_run_uses_its_single_frame(self):
        """integrateTime stores one frame under t_index [-1]; every branch reads it."""
        mod = _load_runner()
        grid = np.array([[0.42]], dtype=np.float32)
        out = mod._per_branch_anisotropy(self._df(ys=[1, 1], xs=[1, 1]), [grid], [-1],
                                         box=10, n_spatial=2)
        np.testing.assert_allclose(out, [0.42, 0.42], rtol=1e-6)

    def test_2d_grid_over_a_3d_branch_table_indexes_by_yx(self):
        """calcFlattened computes anisotropy on a Z-MIP while the branches stay 3D — the grid's
        axes are then the LAST two spatial axes of the centroid, not the first two."""
        mod = _load_runner()
        grid = np.array([[0.1, 0.2], [0.3, 0.4]], dtype=np.float32)
        n = 2
        df = pd.DataFrame({
            "label": [1, 2],
            "image-coord-src-0": [5.0, 5.0],      # z — must be ignored
            "image-coord-src-1": [1.0, 15.0],     # y
            "image-coord-src-2": [1.0, 15.0],     # x
            "image-coord-dst-0": [5.0, 5.0],
            "image-coord-dst-1": [1.0, 15.0],
            "image-coord-dst-2": [1.0, 15.0],
        })
        out = mod._per_branch_anisotropy(df, [grid], [0], box=10, n_spatial=3)
        np.testing.assert_allclose(out, [0.1, 0.4], rtol=1e-6)

    def test_no_grid_gives_all_nan(self):
        mod = _load_runner()
        out = mod._per_branch_anisotropy(self._df(ys=[1], xs=[1]), [], [], box=10, n_spatial=2)
        self.assertTrue(np.isnan(out).all())


class StoreAxesTest(unittest.TestCase):
    """Finding A8: the label store's axes are not the image's.

    Getting this wrong tagged a 3-axis (T, Y, X) array as `t,c,z,y,x` with scale
    [1, 1, 3.0, 0.596, 0.596], so a positional reader gave Y the 3 µm Z step."""

    def test_drops_channel_always(self):
        mod = _load_runner()
        self.assertEqual(mod._store_axes(["T", "C", "Z", "Y", "X"], True, True), ["T", "Z", "Y", "X"])

    def test_drops_z_when_flattened(self):
        mod = _load_runner()
        self.assertEqual(mod._store_axes(["T", "C", "Z", "Y", "X"], True, False), ["T", "Y", "X"])

    def test_drops_t_when_integrated(self):
        mod = _load_runner()
        self.assertEqual(mod._store_axes(["T", "C", "Z", "Y", "X"], False, True), ["Z", "Y", "X"])

    def test_drops_both(self):
        mod = _load_runner()
        self.assertEqual(mod._store_axes(["T", "C", "Z", "Y", "X"], False, False), ["Y", "X"])


class BranchH5adTemporalTest(unittest.TestCase):
    """Finding A7: a Z-flattened TIMESERIES must keep its time axis.

    `flattenBranching` is a Z operation; the runner still skeletonises every timepoint. Writing
    the h5ad with `has_time and not flatten_branching` dropped obsm['temporal'] from exactly the
    case Panel B needs — 66k branches over 201 frames with no frame attribution."""

    def _paths_df(self, n=4):
        return pd.DataFrame({
            "label": np.arange(1, n + 1),
            "centroid_t": np.arange(n, dtype=float),
            "branch-type": np.ones(n, dtype=int),
            "image-coord-src-0": np.zeros(n), "image-coord-src-1": np.zeros(n),
            "image-coord-dst-0": np.ones(n),  "image-coord-dst-1": np.ones(n),
        })

    def test_timeseries_keeps_temporal_obsm(self):
        mod = _load_runner()
        import anndata as ad
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, "x__branch.h5ad")
            mod._write_branch_h5ad(self._paths_df(), is_3d=False, has_time=True, out_path=out)
            a = ad.read_h5ad(out)
            self.assertIn("temporal", a.obsm)
            self.assertEqual(list(a.uns["temporal_cols"]), ["centroid_t"])
            np.testing.assert_allclose(a.obsm["temporal"].ravel(), [0, 1, 2, 3])

    def test_static_image_has_no_temporal(self):
        mod = _load_runner()
        import anndata as ad
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, "x__branch.h5ad")
            df = self._paths_df().drop(columns=["centroid_t"])
            mod._write_branch_h5ad(df, is_3d=False, has_time=False, out_path=out)
            self.assertNotIn("temporal", ad.read_h5ad(out).obsm)


class GlobaliseLabelsTest(unittest.TestCase):
    """Pin the invariant behind the labels-zarr / h5ad alignment: after `_globalise_labels`,
    the nonzero pixel values in the shifted skeleton array match the h5ad `label` column
    across many timepoints (a bug here silently misaligns the two, which only a real run
    would catch). Also covers empty-frame passthrough."""

    def _synthetic_frame(self, n_paths: int, start: int = 1):
        # A per-frame skeleton array whose nonzero pixels are labelled 1..n_paths — the shape
        # skan.Skeleton produces via np.asarray(sk).
        arr = np.zeros((10, 10), dtype=np.uint32)
        # place each label at a distinct pixel
        for i in range(n_paths):
            arr[i, 0] = start + i
        return arr

    def test_multi_timepoint_labels_align_across_frames(self):
        mod = _load_runner()
        offset = 0
        assigned = []
        for t, n in enumerate([5, 3, 4, 0, 2]):        # varied path counts + an empty frame
            df = pd.DataFrame({"path-id": np.arange(n), "skeleton-id": np.zeros(n, dtype=np.int64)})
            skeleton_arr = self._synthetic_frame(n)
            df, arr, offset = mod._globalise_labels(df, skeleton_arr, offset)
            # arr's nonzero values must equal df.label exactly — no gaps, no overlap with prior frames
            if n == 0:
                self.assertTrue((arr == 0).all())
                continue
            self.assertEqual(set(arr[arr > 0].tolist()), set(df["label"].tolist()))
            assigned.extend(df["label"].tolist())
        # Every assigned label is unique across timepoints (this is the whole point of the offset).
        self.assertEqual(len(assigned), len(set(assigned)))
        # And they're strictly increasing starting at 1 (5 + 3 + 4 + 0 + 2 = 14 total).
        self.assertEqual(assigned, list(range(1, 15)))

    def test_empty_frame_is_noop(self):
        mod = _load_runner()
        empty_df = pd.DataFrame({"path-id": np.array([], dtype=np.int64),
                                 "skeleton-id": np.array([], dtype=np.int64)})
        arr_in = np.zeros((4, 4), dtype=np.uint32)
        df_out, arr_out, offset_out = mod._globalise_labels(empty_df, arr_in, offset=17)
        self.assertEqual(offset_out, 17)
        self.assertEqual(len(df_out), 0)
        self.assertTrue((arr_out == 0).all())


class SkeletoniseDilationOrderTest(unittest.TestCase):
    """Pins the fix for a real regression: `postDilationSize` must be applied to the already
    PATH-LABELLED array (after `skan.Skeleton`/`summarize` has read the topology), never to the
    boolean mask before skan sees it. Dilating the mask first turns a thin 1px-wide skeleton into
    a multi-pixel-wide blob; skan then reads spurious junctions along the width of every line,
    exploding a handful of real branches into hundreds of bogus ones with a meaningless
    branch-type mix. Caught via real project data: a run with `postDilationSize>0` produced
    170,215 paths (only types 1/2, zero type-0 — an implausible distribution for real fibre
    data) where the true topology has a small handful of edges."""

    def _t_junction(self):
        # One real junction, three real termini: a horizontal bar crossed by a vertical stub.
        # Correct analysis: exactly 3 paths, all "endpoint-to-junction" (branch-type 1).
        im = np.zeros((21, 41), dtype=bool)
        im[10, 2:39] = True
        im[2:11, 20] = True
        return im

    def test_skeletonise_does_not_dilate_before_skan_sees_it(self):
        mod = _load_runner()
        thin = mod._skeletonise(self._t_junction(), pre=0, is_3d=False)
        df, arr = mod._summarise_paths(thin, t_index=None)
        # The real topology: 3 paths, all endpoint-to-junction. If `_skeletonise` dilated the
        # mask before this point (the bug), skan would read dozens of spurious paths instead.
        self.assertEqual(len(df), 3)
        self.assertEqual(set(df["branch-type"].tolist()), {1})
        self.assertEqual(len(np.unique(arr[arr > 0])), 3)

    def test_post_dilation_grows_footprint_without_changing_topology(self):
        mod = _load_runner()
        thin = mod._skeletonise(self._t_junction(), pre=0, is_3d=False)
        df, arr = mod._summarise_paths(thin, t_index=None)
        dilated = mod._dilate_label_image(arr, post=2, is_3d=False)
        # More pixels lit up (visibility), but the SET of distinct path labels is unchanged —
        # dilation must never re-derive or corrupt the topology skan already read.
        self.assertGreater(int((dilated > 0).sum()), int((arr > 0).sum()))
        self.assertEqual(set(np.unique(dilated[dilated > 0]).tolist()),
                         set(np.unique(arr[arr > 0]).tolist()))

    def test_post_dilation_zero_is_a_noop(self):
        mod = _load_runner()
        arr = np.array([[0, 1], [2, 0]], dtype=np.uint32)
        out = mod._dilate_label_image(arr, post=0, is_3d=False)
        np.testing.assert_array_equal(out, arr)


if __name__ == "__main__":
    unittest.main()
