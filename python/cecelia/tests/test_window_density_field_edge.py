"""RESULT-REVIEW: result-45f86208

A window centred on a tile at the edge of the field reaches half a tile past it, so on tissue of even
density its raw count was ~25% lower (more in a corner), and the density beside the composition sorted
a band along the image border into a region of its own. Counts are now per full window: divided by the
fraction of the window inside the field, [0, gridShape·tile) per axis."""

import unittest

import anndata as ad
import numpy as np
import pandas as pd

from cecelia.utils.spatial_utils import image_region_rows, window_coverage, window_grid


class FieldEdgeTest(unittest.TestCase):
    def test_coverage_is_the_windows_overlap_with_the_field(self):
        g = {"gridShape": [4, 5], "tile": 10.0}
        cov = window_coverage(g).reshape(4, 5)
        # window centred on tile k spans [(k-0.5)t, (k+1.5)t); the field is [0, n·t)
        def inside(k, n):
            return (min(k + 1.5, n) - max(k - 0.5, 0.0)) / 2.0
        expect = np.array([[inside(i, 4) * inside(j, 5) for j in range(5)] for i in range(4)])
        np.testing.assert_allclose(cov, expect)
        self.assertEqual(cov[0, 0], 0.75 * 0.75)
        self.assertEqual(cov[1, 1], 1.0)

    def test_even_tissue_reads_as_even_at_the_border(self):
        rng = np.random.default_rng(3)
        coords = rng.uniform(0, 400, size=(40000, 2))
        a = ad.AnnData(np.zeros((len(coords), 0), dtype=np.float32))
        a.obsm["spatial"] = coords
        grid = window_grid(coords, ["y", "x"], 50.0)
        a.uns.update(grid)
        obs = pd.DataFrame({"valueName": "A", "label": np.arange(len(coords))})
        counts, *_ = image_region_rows(a, obs, np.zeros(len(coords), dtype=int), 1, "movie", False, {})
        dens = counts.sum(axis=1).reshape(grid["gridShape"])
        interior, edge = dens[1:-1, 1:-1].mean(), dens[0, 1:-1].mean()
        self.assertLess(abs(edge / interior - 1.0), 0.05)


if __name__ == "__main__":
    unittest.main()
