"""Per-cell autofluorescence correction (`cecelia.utils.cell_af_utils`) and the feature-column writer it
lands through (`LabelPropsView.set_vars`).

The synthetic panel is built from the definition of the problem, not from the code's output: every
cell has an autofluorescence level per dye that adds to all markers on that dye, and is truly positive
for at most one marker. A correct method makes markers on one dye stop moving together while the true
positives stay above the negatives.
"""
import os
import tempfile
import unittest

import numpy as np

from cecelia.utils.cell_af_utils import correct_cell_af, dye_groups, same_dye_correlation


def _panel(n=4000, dyes=3, per_dye=6, seed=0):
    rng = np.random.default_rng(seed)
    groups = [f"dye{d}" for d in range(dyes) for _ in range(per_dye)]
    n_ch = len(groups)
    af = rng.lognormal(mean=2.0, sigma=0.8, size=(n, dyes))          # per cell, per dye
    X = np.empty((n, n_ch))
    for j, g in enumerate(groups):
        X[:, j] = af[:, int(g[3:])] + rng.normal(0, 0.5, n).clip(0)
    truth = rng.integers(-1, n_ch, size=n)                              # -1 = negative for every marker
    pos = truth >= 0
    X[np.where(pos)[0], truth[pos]] += 60.0
    return X, groups, truth


class CorrectCellAf(unittest.TestCase):
    def test_removes_the_signal_shared_within_a_dye(self):
        X, groups, _ = _panel()
        before = same_dye_correlation(X, groups)[0]
        C, info = correct_cell_af(X, groups)
        Xc = X.copy(); Xc[:, info["channels"]] = C
        after = same_dye_correlation(Xc, groups)[0]
        self.assertGreater(before, 0.5)
        self.assertLess(after, 0.15)

    def test_true_positives_stay_above_negatives(self):
        X, groups, truth = _panel()
        C, info = correct_cell_af(X, groups)
        for k, j in enumerate(info["channels"]):
            pos, neg = C[truth == j, k], C[truth != j, k]
            # every true positive above the 99th percentile of the cells that lack the marker
            self.assertGreater(np.percentile(pos, 5), np.percentile(neg, 99), f"channel {j}")

    def test_output_is_in_intensity_units_and_never_negative(self):
        X, groups, _ = _panel()
        C, _ = correct_cell_af(X, groups)
        self.assertGreaterEqual(C.min(), 0.0)
        self.assertLess(C.max(), X.max() * 1.5)

    def test_dyes_with_fewer_than_three_markers_and_excluded_channels_are_left_out(self):
        groups = ["DAPI", "a", "a", "a", "b", "b", None]
        self.assertEqual(dye_groups(groups), {"a": [1, 2, 3]})
        self.assertEqual(dye_groups(groups, exclude=[2]), {})
        X = np.random.default_rng(1).random((50, 7))
        C, info = correct_cell_af(X, groups)
        self.assertEqual(info["channels"], [1, 2, 3])
        self.assertEqual(C.shape, (50, 3))

    def test_nothing_to_correct_returns_no_columns(self):
        C, info = correct_cell_af(np.ones((10, 2)), ["a", "b"])
        self.assertEqual(C.shape, (10, 0))
        self.assertEqual(info["channels"], [])
        self.assertIsNone(same_dye_correlation(np.ones((10, 2)), ["a", "b"]))


class SetVars(unittest.TestCase):
    def _table(self, d):
        import anndata as ad
        import pandas as pd
        p = os.path.join(d, "cells.h5ad")
        a = ad.AnnData(X=np.arange(12, dtype=np.float64).reshape(4, 3),
                       obs=pd.DataFrame({"clusters.x": [1.0, 2.0, 3.0, 4.0]},
                                        index=pd.Index(["5", "6", "7", "8"], dtype=object)))
        a.var_names = ["area", "mean_intensity_0", "mean_intensity_1"]
        a.obsm["spatial"] = np.zeros((4, 2)); a.uns["spatial_cols"] = ["centroid_y", "centroid_x"]
        a.uns["intensity_measure"] = "mean"
        a.write_h5ad(p)
        return p

    def test_adds_replaces_and_clears_feature_columns(self):
        from cecelia.utils.label_props_utils import LabelPropsView
        with tempfile.TemporaryDirectory() as d:
            p = self._table(d)
            LabelPropsView(p).set_vars({"af_mean_intensity_0": [9, 9, 9, 9],
                                        "af_mean_intensity_1": [1, 2, 3, 4]}, drop_prefix="af_").save()
            v = LabelPropsView(p)
            self.assertEqual(v.var_names(), ["area", "mean_intensity_0", "mean_intensity_1",
                                             "af_mean_intensity_0", "af_mean_intensity_1"])
            df = v.as_df()
            self.assertEqual(list(df["af_mean_intensity_1"]), [1, 2, 3, 4])
            self.assertEqual(list(df["mean_intensity_1"]), [2, 5, 8, 11])      # measured column untouched
            self.assertEqual(list(df["clusters.x"]), [1, 2, 3, 4])             # obs kept
            self.assertEqual(list(v.adata.uns["spatial_cols"]), ["centroid_y", "centroid_x"])
            self.assertIn("spatial", v.adata.obsm)
            # a re-run that corrects only channel 1 must not leave channel 0's old value behind
            LabelPropsView(p).set_vars({"af_mean_intensity_1": [4, 3, 2, 1]}, drop_prefix="af_").save()
            v = LabelPropsView(p)
            self.assertNotIn("af_mean_intensity_0", v.var_names())
            self.assertEqual(list(v.as_df()["af_mean_intensity_1"]), [4, 3, 2, 1])

    def test_rejects_a_column_of_the_wrong_length(self):
        from cecelia.utils.label_props_utils import LabelPropsView
        with tempfile.TemporaryDirectory() as d:
            p = self._table(d)
            with self.assertRaises(ValueError):
                LabelPropsView(p).set_vars({"af_mean_intensity_0": [1, 2]})


if __name__ == "__main__":
    unittest.main()
