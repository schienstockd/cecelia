"""Phase 3 unit tests for the anisotropy path in branching_run (docs/todo/BRANCHING_PLAN.md
Decision 4).

Pure-numpy tests over the small helpers — no zarr, no h5ad, no Julia. Pins:

- `_pool_by_box` averages correctly and trims trailing pixels rather than crashing.
- `_box_centres` returns the geometric centre of each box.
- Structure-tensor + eigendecomp identifies a synthetic parallel-line field as anisotropic
  (λ₂ ≫ λ₁, principal eigenvector along the lines) and an isotropic noise field as anisotropy ≈ 0.

The runner file lives at `app/src/tasks/segment/branching_run.py` (not part of the `cecelia` IO
package), so this test loads it by absolute path via importlib — mirroring how `run_py` calls it.
"""
import importlib.util
import os
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


class PoolByBoxTest(unittest.TestCase):
    def test_2d_mean_pool(self):
        mod = _load_runner()
        x = np.arange(16, dtype=np.float32).reshape(4, 4)
        # 2x2 pool of 4x4 → 2x2 grid; each cell is the mean of a 2x2 block
        out = mod._pool_by_box(x, 2)
        expected = np.array([[2.5, 4.5], [10.5, 12.5]], dtype=np.float32)
        np.testing.assert_allclose(out, expected)

    def test_trims_trailing_pixels(self):
        mod = _load_runner()
        x = np.ones((5, 5), dtype=np.float32)   # 5 not divisible by 2 → last row/col dropped
        out = mod._pool_by_box(x, 2)
        self.assertEqual(out.shape, (2, 2))
        np.testing.assert_allclose(out, 1.0)


class BoxCentresTest(unittest.TestCase):
    def test_centre_of_each_box(self):
        mod = _load_runner()
        # 4x6 → 2x3 boxes of side 2; centres at pixel positions (1, 3, 5) × (1, 3)
        c = mod._box_centres((4, 6), 2)
        self.assertEqual(c.shape, (2, 3, 2))
        np.testing.assert_allclose(c[0, 0], [1.0, 1.0])
        np.testing.assert_allclose(c[1, 2], [3.0, 5.0])


class Anisotropy2DTest(unittest.TestCase):
    def test_parallel_lines_flag_as_anisotropic(self):
        """Horizontal stripes: the gradient of the image runs vertically (row axis), so the
        structure tensor's LARGER eigenvector points across the stripes (row direction). The
        SMALLER eigenvector then points ALONG the fibres (column direction). Anisotropy ≫ 0."""
        mod = _load_runner()
        H = W = 60
        img = np.zeros((H, W), dtype=np.float32)
        img[::4, :] = 1.0            # horizontal lines every 4 rows
        sk = img > 0
        coor, eigval, eigvec, box_len, box_aniso = mod._anisotropy_2d(
            img, sk, sigma=1.5, box=15,
        )
        self.assertEqual(coor.shape, (H // 15, W // 15, 2))
        self.assertEqual(eigval.shape, (H // 15, W // 15, 2))
        self.assertEqual(eigvec.shape, (H // 15, W // 15, 2, 2))
        # eigenvalues are sorted ascending; the trailing one dominates on structured content.
        self.assertGreater(float(box_aniso.mean()), 0.5)
        # LARGER eigenvector (index 1 in row dim after swapaxes) — across the stripes → row-axis dominant.
        larger = eigvec[..., 1, :]
        self.assertGreater(float(np.mean(np.abs(larger[..., 0]))),   # row component (y)
                           float(np.mean(np.abs(larger[..., 1]))))   # column component (x)
        # SMALLER eigenvector (index 0) — along the stripes → column-axis dominant.
        smaller = eigvec[..., 0, :]
        self.assertGreater(float(np.mean(np.abs(smaller[..., 1]))),  # column (x)
                           float(np.mean(np.abs(smaller[..., 0]))))  # row (y)

    def test_isotropic_noise_is_low_anisotropy(self):
        mod = _load_runner()
        rng = np.random.default_rng(0)
        img = rng.random((60, 60)).astype(np.float32)
        sk = np.zeros_like(img, dtype=bool)
        _, _, _, _, box_aniso = mod._anisotropy_2d(img, sk, sigma=1.5, box=15)
        self.assertLess(float(box_aniso.mean()), 0.4)


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
