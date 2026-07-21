"""Tests for the LabelPropsView write path — focus on the categorical obs encoding
(the half of the h5ad write contract Julia can't produce)."""

import os
import tempfile
import unittest

import anndata as ad
import numpy as np
import pandas as pd

from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils import obs_utils


def _make_h5ad(path, labels):
    """A minimal label-props .h5ad: X + integer-string obs index."""
    n = len(labels)
    adata = ad.AnnData(
        X=np.zeros((n, 2), dtype=np.float64),
        obs=pd.DataFrame(index=[str(l) for l in labels]),
    )
    adata.var_names = ["f0", "f1"]
    adata.write_h5ad(path)


class AddCategoricalObsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, "x.h5ad")
        _make_h5ad(self.path, [10, 20, 30])

    def test_categorical_roundtrip_and_unset_is_code_minus_one(self):
        # label 20 is deliberately omitted -> should remain unset (code -1)
        LabelPropsView(self.path).add_categorical_obs(
            "state", labels=[10, 30], values=["A", "B"]).save()

        back = ad.read_h5ad(self.path)
        col = back.obs["state"]
        self.assertIsInstance(col.dtype, pd.CategoricalDtype)
        self.assertEqual(sorted(col.cat.categories.tolist()), ["A", "B"])
        codes = dict(zip(back.obs.index, col.cat.codes))
        self.assertEqual(back.obs.loc["10", "state"], "A")
        self.assertEqual(back.obs.loc["30", "state"], "B")
        self.assertEqual(codes["20"], -1)   # unset label -> missing category

    def test_values_stringified(self):
        # integer transition codes come through as string categories
        LabelPropsView(self.path).add_categorical_obs(
            "hmm", labels=[10, 20, 30], values=[1, 2, 1]).save()
        back = ad.read_h5ad(self.path)
        self.assertEqual(sorted(back.obs["hmm"].cat.categories.tolist()), ["1", "2"])

    def test_driver_drops_then_writes(self):
        # seed a column, then the obs_utils driver drops + rewrites it
        LabelPropsView(self.path).add_categorical_obs(
            "state", labels=[10], values=["OLD"]).save()
        obs_utils.write_categorical_obs({
            "filepath": self.path,
            "drop": ["state"],
            "columns": [{"name": "state", "labels": [20, 30], "values": ["X", "Y"]}],
        })
        back = ad.read_h5ad(self.path)
        self.assertEqual(sorted(back.obs["state"].cat.categories.tolist()), ["X", "Y"])
        self.assertEqual(back.obs.loc["20", "state"], "X")
        self.assertEqual(back.obs["state"].cat.codes[back.obs.index.get_loc("10")], -1)


class ChannelNameSelectionTest(unittest.TestCase):
    """Parity with the Julia reader: select an intensity column by its CHANNEL name and get the raw
    column back under that name (channel_names positional: index i ↔ chans[i])."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, "x.h5ad")
        a = ad.AnnData(X=np.array([[1., 2., 3.], [4., 5., 6.]], dtype=np.float64),
                       obs=pd.DataFrame(index=["0", "1"]))
        a.var_names = ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2"]
        a.uns["intensity_measure"] = "mean"
        a.write_h5ad(self.path)
        self.chans = ["CD4", "CD8", "B220"]

    def test_select_by_channel_name_resolves_to_raw(self):
        df = LabelPropsView(self.path, channel_names=self.chans).view_cols(["CD8"]).as_df()
        self.assertIn("CD8", df.columns)                        # returned under the requested channel name
        self.assertNotIn("mean_intensity_1", df.columns)        # not the raw name
        raw = LabelPropsView(self.path).view_cols(["mean_intensity_1"]).as_df()
        np.testing.assert_array_equal(df["CD8"].to_numpy(), raw["mean_intensity_1"].to_numpy())

    def test_raw_names_still_work(self):                          # gates/clustering pass raw — unchanged
        df = LabelPropsView(self.path).view_cols(["mean_intensity_2"]).as_df()
        self.assertIn("mean_intensity_2", df.columns)

    def test_channel_columns_and_rename(self):
        v = LabelPropsView(self.path, channel_names=self.chans)
        self.assertEqual(v.channel_columns(), ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2"])
        self.assertEqual(v.channel_columns(as_channel_names=True), self.chans)
        df = (LabelPropsView(self.path, channel_names=self.chans)
              .rename_channels(True).view_cols(["mean_intensity_0"]).as_df())
        self.assertIn("CD4", df.columns)                        # raw request, renamed output


if __name__ == "__main__":
    unittest.main()
