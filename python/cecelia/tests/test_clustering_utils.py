"""Unit tests for cecelia.utils.clustering_utils.

`cluster_seg_stats` (pure) pins the per-segment cluster distribution the QC sidecar banks: point
count, distinct clusters occupied, and the largest-cluster fraction (dominance). See qc.jl
cluster_qc_findings + COHORT_METRICS. `find_populations` gets one small end-to-end run."""
import unittest

import numpy as np
import pandas as pd

from cecelia.utils.clustering_utils import cluster_seg_stats


class TestClusterSegStats(unittest.TestCase):
    def test_even_spread(self):
        codes = np.array([0, 0, 1, 1, 2, 2])
        s = cluster_seg_stats(codes)
        self.assertEqual(s["n"], 6)
        self.assertEqual(s["nClusters"], 3)
        self.assertAlmostEqual(s["largestClusterFrac"], 2 / 6)

    def test_single_cluster(self):
        s = cluster_seg_stats(np.zeros(10, dtype=int))
        self.assertEqual(s["nClusters"], 1)
        self.assertAlmostEqual(s["largestClusterFrac"], 1.0)

    def test_dominant_cluster(self):
        codes = np.array([0] * 9 + [1])          # 90% in cluster 0
        s = cluster_seg_stats(codes)
        self.assertEqual(s["nClusters"], 2)
        self.assertAlmostEqual(s["largestClusterFrac"], 0.9)

    def test_empty(self):
        s = cluster_seg_stats(np.array([], dtype=int))
        self.assertEqual(s, {"n": 0, "nClusters": 0, "largestClusterFrac": 0.0})

    def test_non_contiguous_codes(self):
        # cluster ids need not be 0..N — count distinct
        s = cluster_seg_stats(np.array([3, 3, 7, 7, 7]))
        self.assertEqual(s["nClusters"], 2)
        self.assertAlmostEqual(s["largestClusterFrac"], 3 / 5)


class TestSplitBackExtraObs(unittest.TestCase):
    """split_back_and_write persists the region code + the composition columns (extra_obs), sliced
    per segment and aligned by label — the enabler for the region-composition heatmap (Decision 16)."""
    def test_regions_and_composition_written_per_segment(self):
        import os, tempfile
        import anndata as ad
        import pandas as pd
        from cecelia.utils.clustering_utils import split_back_and_write
        from cecelia.utils.label_props_utils import LabelPropsView

        tmp = tempfile.mkdtemp()
        # two segmentations (B, T), 2 cells each; minimal label-props h5ad per segment
        paths = {}
        for vn, labels in (("B", [1, 2]), ("T", [1, 2])):
            p = os.path.join(tmp, f"{vn}.h5ad")
            a = ad.AnnData(X=np.zeros((2, 1), dtype=np.float64),
                           obs=pd.DataFrame(index=[str(l) for l in labels]))
            a.var_names = ["f0"]; a.write_h5ad(p); paths[vn] = p
        segments = [{"uID": "img1", "valueName": vn, "propsPath": paths[vn]} for vn in ("B", "T")]

        # pooled clustered adata: 4 cells, composition over 2 basis pops, region codes
        X = np.array([[0.8, 0.2], [0.1, 0.9], [0.7, 0.3], [0.0, 1.0]], dtype=np.float32)
        adata = ad.AnnData(X)
        adata.obs["uID"] = ["img1"] * 4
        adata.obs["valueName"] = ["B", "B", "T", "T"]
        adata.obs["label"] = [1, 2, 1, 2]
        adata.obs["clusters"] = pd.Categorical(["0", "1", "0", "1"])
        comp = {f"spatial.comp.{b}.default": X[:, j] for j, b in enumerate(["Bqc", "Tqc"])}

        qc = split_back_and_write(adata, segments, "default", col_prefix="regions", extra_obs=comp)
        self.assertEqual(qc["nClusters"], 2)

        # read back segment B: region code + BOTH composition columns present, values aligned by label
        df = LabelPropsView(paths["B"]).as_df()
        self.assertIn("regions.default", df.columns)
        self.assertIn("spatial.comp.Bqc.default", df.columns)
        self.assertIn("spatial.comp.Tqc.default", df.columns)
        row1 = df[df["label"] == 1].iloc[0]
        self.assertAlmostEqual(float(row1["spatial.comp.Bqc.default"]), 0.8, places=5)
        self.assertAlmostEqual(float(row1["spatial.comp.Tqc.default"]), 0.2, places=5)


if __name__ == '__main__':
    unittest.main()


class TestFindPopulationsReadOnlyMatrix(unittest.TestCase):
    """A feature matrix built from a DataFrame (as clustPops' runner does) is read-only under
    pandas copy-on-write; the in-place transform/normalise steps must not fail writing into it.

    The graph steps are stubbed: they don't write X, and scanpy's neighbour graph segfaults on the
    macOS CI runner."""

    def test_matrix_from_dataframe(self):
        from unittest import mock

        import anndata as ad
        from cecelia.utils import clustering_utils

        def fake_leiden(adata, key_added, **_kw):
            adata.obs[key_added] = pd.Categorical(["0"] * adata.n_obs)

        rng = np.random.default_rng(0)
        df = pd.DataFrame(rng.random((60, 3)), columns=["a", "b", "c"])
        X = df[["a", "b", "c"]].to_numpy(dtype=np.float32)
        self.assertFalse(X.flags.writeable)              # the precondition the bug needs
        adata = ad.AnnData(X)
        with mock.patch.object(clustering_utils.sc.pp, "neighbors"), \
             mock.patch.object(clustering_utils.sc.tl, "leiden", side_effect=fake_leiden):
            clustering_utils.find_populations(adata, transformation="log", create_umap=False,
                                              backend="cpu")
        self.assertEqual(len(adata.obs["clusters"]), 60)
        self.assertLessEqual(float(adata.X.max()), 1.0)  # normalised in place


def _continuum_adata(n=150, k=10, seed=0):
    """A 1-D behaviour continuum (t: slow → fast) with a kNN graph built WITHOUT `sc.pp.neighbors`
    (it segfaults on the macOS CI runner). `speed` = t exactly, so it is DC1's strongest feature."""
    import anndata as ad
    from scipy.sparse import csr_matrix
    from sklearn.neighbors import kneighbors_graph

    rng = np.random.default_rng(seed)
    t = np.sort(rng.random(n))
    X = np.column_stack([t, 1 - t + rng.normal(0, .15, n), rng.normal(0, 1, n)]).astype(np.float32)
    adata = ad.AnnData(X)
    adata.var_names = ["speed", "state1", "noise"]
    adata.obs["clusters"] = pd.Categorical(np.digitize(t, [1 / 3, 2 / 3]).astype(str))
    dist = kneighbors_graph(t[:, None], k, mode="distance")   # the graph IS the continuum
    conn = (dist > 0).astype(float)
    adata.obsp["distances"] = csr_matrix(dist)
    adata.obsp["connectivities"] = csr_matrix(conn.maximum(conn.T))
    adata.uns["neighbors"] = {"connectivities_key": "connectivities", "distances_key": "distances",
                              "params": {"n_neighbors": k, "method": "umap"}}
    return adata, t


class TestBehaviourAxes(unittest.TestCase):
    """`behaviour_axes` names its own axes (sign-aligned to the top feature) and roots the path at
    the low end of DC1 — docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md, Decisions 2–3."""

    def test_path_follows_the_continuum_from_the_slow_end(self):
        from scipy.stats import spearmanr
        from cecelia.utils.clustering_utils import behaviour_axes

        adata, t = _continuum_adata()
        meta = behaviour_axes(adata)
        self.assertEqual(meta["axes"][0]["top"][0]["feature"], "speed")
        self.assertGreater(meta["axes"][0]["top"][0]["rho"], 0)      # sign-aligned: top feature positive
        self.assertEqual(adata.obsm["X_diffmap_axes"].shape, (adata.n_obs, 2))
        path = adata.obs["path"].to_numpy()
        self.assertGreater(spearmanr(path, t)[0], 0.9)
        self.assertEqual(meta["root"], {"end": "low", "cluster": "0"})
        self.assertAlmostEqual(float(np.nanmin(path)), 0.0, places=6)

    def test_reverse_roots_at_the_other_end(self):
        from scipy.stats import spearmanr
        from cecelia.utils.clustering_utils import behaviour_axes

        adata, t = _continuum_adata()
        meta = behaviour_axes(adata, reverse=True)
        self.assertLess(spearmanr(adata.obs["path"], t)[0], -0.9)
        self.assertEqual(meta["root"], {"end": "high", "cluster": "2"})

    def test_names_use_the_raw_features_when_given(self):
        from cecelia.utils.clustering_utils import behaviour_axes

        adata, _t = _continuum_adata()
        raw = np.asarray(adata.X).copy()
        adata.X = np.zeros_like(adata.X)                              # stand-in for a normalised matrix
        meta = behaviour_axes(adata, features=raw)
        self.assertEqual(meta["axes"][0]["top"][0]["feature"], "speed")

    def test_too_small_or_no_graph_is_skipped(self):
        import anndata as ad
        from cecelia.utils.clustering_utils import behaviour_axes

        self.assertIsNone(behaviour_axes(ad.AnnData(np.zeros((8, 2), dtype=np.float32))))
        adata, _t = _continuum_adata()
        del adata.uns["neighbors"]
        self.assertIsNone(behaviour_axes(adata))
        self.assertNotIn("path", adata.obs)


class TestSplitBackExtraObsm(unittest.TestCase):
    """`extra_obsm` lands per segment, aligned by label (track runs: `X_diffmap.{suffix}`)."""

    def test_embedding_written_per_segment(self):
        import os, tempfile
        import anndata as ad
        from cecelia.utils.clustering_utils import split_back_and_write
        from cecelia.utils.label_props_utils import LabelPropsView

        tmp = tempfile.mkdtemp()
        paths = {}
        for vn in ("B", "T"):
            p = os.path.join(tmp, f"{vn}__tracks.h5ad")
            a = ad.AnnData(X=np.zeros((2, 1)), obs=pd.DataFrame(index=["1", "2"]))
            a.var_names = ["f0"]; a.write_h5ad(p); paths[vn] = p
        segments = [{"uID": "img1", "valueName": vn, "propsPath": paths[vn]} for vn in ("B", "T")]
        adata = ad.AnnData(np.zeros((4, 1), dtype=np.float32))
        adata.obs["uID"] = ["img1"] * 4
        adata.obs["valueName"] = ["B", "B", "T", "T"]
        adata.obs["label"] = [1, 2, 1, 2]
        adata.obs["clusters"] = pd.Categorical(["0", "1", "0", "1"])
        emb = np.array([[0.1, 1.1], [0.2, 1.2], [0.3, 1.3], [0.4, 1.4]], dtype=np.float32)

        split_back_and_write(adata, segments, "mv", extra_obs={"path.mv": np.array([0., .5, .7, 1.])},
                             extra_obsm={"X_diffmap.mv": emb})
        view = LabelPropsView(paths["T"])
        np.testing.assert_allclose(view.obsm("X_diffmap.mv"), emb[2:], rtol=1e-6)
        df = view.as_df()
        self.assertAlmostEqual(float(df.loc[df["label"] == 2, "path.mv"].iloc[0]), 1.0)
        view.close()
