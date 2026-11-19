"""RESULT-REVIEW: result-b7cd5050

A window graph pooled over the movie: the cells-per-window measure that is clustered beside the
composition must not grow with the movie's length. Summing cell-frames gave identical tissue 4 cells
per window in a 2-frame movie and 20 in a 10-frame one. It is now the mean per frame."""

import unittest

import anndata as ad
import numpy as np
import pandas as pd

from cecelia.utils.spatial_utils import image_region_rows, window_grid


def _movie(n_frames, seed=0):
    rng = np.random.default_rng(seed)
    one = rng.uniform(0, 200, size=(80, 2))                  # the same layout on every frame
    coords = np.vstack([one] * n_frames)
    a = ad.AnnData(np.zeros((len(coords), 0), dtype=np.float32))
    a.obsm["spatial"] = coords
    a.uns.update(window_grid(coords, ["y", "x"], 40.0))
    obs = pd.DataFrame({"valueName": "A", "label": np.arange(len(coords)),
                        "_t": np.repeat(np.arange(n_frames), len(one))})
    codes = np.tile(rng.integers(0, 2, size=len(one)), n_frames)
    return a, obs, codes


class MovieLengthTest(unittest.TestCase):
    def test_the_same_tissue_has_the_same_density_in_a_short_and_a_long_movie(self):
        dens = {}
        for n in (2, 10):
            a, obs, codes = _movie(n)
            counts, _own, _keep, problem = image_region_rows(a, obs, codes, 2, "movie", False, {})
            self.assertIsNone(problem)
            dens[n] = counts.sum(axis=1)
        np.testing.assert_allclose(dens[2], dens[10])

    def test_it_is_the_per_frame_count(self):
        a1, obs1, codes1 = _movie(1)
        one, *_ = image_region_rows(a1, obs1.assign(_t=0), codes1, 2, "movie", False, {})
        a5, obs5, codes5 = _movie(5)
        five, *_ = image_region_rows(a5, obs5, codes5, 2, "movie", False, {})
        np.testing.assert_allclose(five, one)


if __name__ == "__main__":
    unittest.main()
