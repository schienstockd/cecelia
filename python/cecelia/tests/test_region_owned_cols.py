"""A region re-run drops exactly the columns its record says it owns, never by name.

RESULT-REVIEW: result-2e25f95c — a run named `t2.default` lost its `spatial.comp.*.t2.default` columns
when run `default` was written, because both names end in `.default`.
RESULT-REVIEW: result-a086f03a — a dotted-name guard kept run `default`'s own stale column
`spatial.comp.CD8.eff.default` because a run `eff.default` exists.
"""
import os
import tempfile
import unittest

import anndata as ad
import numpy as np
import pandas as pd

from cecelia.utils.clustering_utils import split_back_and_write
from cecelia.utils.label_props_utils import LabelPropsView


class TestRegionOwnedCols(unittest.TestCase):
    def test_owned_by_record_not_by_name(self):
        tmp = tempfile.mkdtemp()
        p = os.path.join(tmp, "T.h5ad")
        base = ad.AnnData(X=np.zeros((2, 1)), obs=pd.DataFrame(
            {"regions.t2.default": [0, 1], "regions.eff.default": [0, 1],
             "spatial.comp.Bqc.t2.default": [.1, .2],      # sibling run t2.default
             "spatial.comp.CD8.eff.default": [.3, .4]},    # run default's old basis column
            index=["1", "2"]))
        base.var_names = ["f0"]
        base.write_h5ad(p)
        adata = ad.AnnData(np.zeros((2, 1), dtype=np.float32))
        adata.obs["uID"] = ["i"] * 2
        adata.obs["valueName"] = ["T"] * 2
        adata.obs["label"] = [1, 2]
        adata.obs["clusters"] = pd.Categorical(["0", "1"])
        seg = {"uID": "i", "valueName": "T", "propsPath": p,
               "ownedCols": ["spatial.comp.CD8.eff.default", "spatial.comp.other.default"]}
        split_back_and_write(adata, [seg], "default", col_prefix="regions",
                             extra_obs={"spatial.comp.Mqc.default": np.array([.5, .6])})
        cols = set(LabelPropsView(p).as_df().columns)
        self.assertNotIn("spatial.comp.CD8.eff.default", cols)
        self.assertIn("spatial.comp.Bqc.t2.default", cols)
        self.assertIn("spatial.comp.Mqc.default", cols)


if __name__ == "__main__":
    unittest.main()
