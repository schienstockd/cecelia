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


if __name__ == "__main__":
    unittest.main()
