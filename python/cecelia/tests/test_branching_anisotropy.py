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


if __name__ == "__main__":
    unittest.main()
