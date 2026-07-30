"""Unit tests for the pure neighbourhood-composition helper
(cecelia.utils.spatial_utils.neighbourhood_composition).

Pure/headless — no squidpy, no anndata, no I/O. Pins the composition formula: per cell, the
normalised frequency of each population among its graph neighbours (the CytoMAP / cecelia "i-niche"
feature; legacy cellRegionsStats `freq = n / sum(n)`). See docs/todo/SPATIAL_REGIONS_PLAN.md."""
import unittest

import numpy as np
import scipy.sparse as sp

from cecelia.utils.spatial_utils import neighbourhood_composition, pairwise_contact_logodds


class TestNeighbourhoodComposition(unittest.TestCase):
    def _chain(self, n):
        # undirected path graph 0-1-2-...-(n-1)
        rows, cols = [], []
        for i in range(n - 1):
            rows += [i, i + 1]
            cols += [i + 1, i]
        return sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n))

    def test_two_populations_fractions(self):
        # 4 cells in a chain; codes alternate pop 0/1. Each interior cell has 2 neighbours.
        conn = self._chain(4)
        codes = np.array([0, 1, 0, 1])
        comp = neighbourhood_composition(conn, codes, 2)
        # cell 1's neighbours are cells 0 (pop0) and 2 (pop0) → all pop0
        np.testing.assert_allclose(comp[1], [1.0, 0.0])
        # cell 2's neighbours are cells 1 (pop1) and 3 (pop1) → all pop1
        np.testing.assert_allclose(comp[2], [0.0, 1.0])
        # rows sum to 1 for non-isolated cells
        self.assertTrue(np.allclose(comp.sum(axis=1), 1.0))

    def test_mixed_neighbourhood(self):
        # star: centre 0 connected to 1(pop0), 2(pop1), 3(pop1)
        conn = sp.csr_matrix(([1, 1, 1, 1, 1, 1],
                              ([0, 1, 0, 2, 0, 3], [1, 0, 2, 0, 3, 0])), shape=(4, 4))
        codes = np.array([0, 0, 1, 1])
        comp = neighbourhood_composition(conn, codes, 2)
        np.testing.assert_allclose(comp[0], [1 / 3, 2 / 3])   # 1 of pop0, 2 of pop1

    def test_isolated_cell_all_zero(self):
        conn = sp.csr_matrix((3, 3))          # no edges
        comp = neighbourhood_composition(conn, np.array([0, 1, 0]), 2)
        self.assertTrue(np.allclose(comp, 0.0))   # isolated → all-zero row, no divide-by-zero
        self.assertEqual(comp.shape, (3, 2))

    def test_other_bin_distinguishes_unlabelled_neighbours(self):
        # star: centre 0 has neighbours 1 (pop0), 2 (UNASSIGNED), 3 (UNASSIGNED).
        conn = sp.csr_matrix(([1] * 6, ([0, 1, 0, 2, 0, 3], [1, 0, 2, 0, 3, 0])), shape=(4, 4))
        codes = np.array([0, 0, -1, -1])
        # WITHOUT the other bin the two unlabelled neighbours vanish and the cell looks purely pop0…
        comp = neighbourhood_composition(conn, codes, 2, include_other=False)
        self.assertEqual(comp.shape, (4, 2))
        np.testing.assert_allclose(comp[0], [1.0, 0.0])
        # …WITH it the row is the true neighbourhood: 1/3 pop0, 2/3 other
        comp_o = neighbourhood_composition(conn, codes, 2, include_other=True)
        self.assertEqual(comp_o.shape, (4, 3))
        np.testing.assert_allclose(comp_o[0], [1 / 3, 0.0, 2 / 3])
        self.assertTrue(np.allclose(comp_o.sum(axis=1)[:1], 1.0))


class TestPopCodesFor(unittest.TestCase):
    """Attaching a labelling to a LOADED graph — the (valueName, label) join legacy used."""
    def _obs(self):
        import pandas as pd
        return pd.DataFrame({"valueName": ["B", "B", "T", "T"], "label": [10, 11, 10, 12]})

    def test_join_is_on_value_name_and_label(self):
        from cecelia.utils.spatial_utils import pop_codes_for
        # label 10 exists in BOTH segmentations with DIFFERENT codes — keying on label alone would
        # collide, which is why the join carries value_name.
        segs = [{"valueName": "B", "labels": [10, 11], "popCodes": [0, 0]},
                {"valueName": "T", "labels": [10], "popCodes": [1]}]
        codes, coverage = pop_codes_for(self._obs(), segs, 2)
        np.testing.assert_array_equal(codes, [0, 0, 1, -1])
        self.assertAlmostEqual(coverage, 0.75)      # T/12 is in the graph but outside the basis

    def test_out_of_range_codes_are_unassigned(self):
        from cecelia.utils.spatial_utils import pop_codes_for
        segs = [{"valueName": "B", "labels": [10], "popCodes": [5]}]     # code ≥ n_pops
        codes, coverage = pop_codes_for(self._obs(), segs, 2)
        np.testing.assert_array_equal(codes, [-1, -1, -1, -1])
        self.assertEqual(coverage, 0.0)


class TestGraphRoundTrip(unittest.TestCase):
    """Persist → load: identity, coords and obsp survive, and a missing graph fails loudly rather than
    silently falling back to an in-process rebuild. Needs anndata."""
    def test_save_load_round_trip(self):
        import os
        import tempfile
        import anndata as ad
        import pandas as pd
        from cecelia.utils.spatial_utils import save_graph, load_graph, graph_meta

        coords = np.array([[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]])
        a = ad.AnnData(coords.astype(np.float32))
        a.obsm["spatial"] = coords
        a.obsp["spatial_connectivities"] = sp.csr_matrix(
            (np.ones(4), ([0, 1, 1, 2], [1, 0, 2, 1])), shape=(3, 3))
        obs = pd.DataFrame({"valueName": ["B", "B", "T"], "label": [7, 8, 9]})
        with tempfile.TemporaryDirectory() as td:
            p = os.path.join(td, "spatialGraph", "run1.h5ad")     # nested dir is created
            save_graph(a, obs, p, meta={"method": "delaunay", "radius": 30.0})
            self.assertTrue(os.path.isfile(p))
            b, obs_b = load_graph(p)
            self.assertEqual(list(obs_b["valueName"]), ["B", "B", "T"])
            self.assertEqual(list(obs_b["label"]), [7, 8, 9])
            self.assertEqual(b.obsp["spatial_connectivities"].nnz, 4)
            np.testing.assert_allclose(b.obsm["spatial"], coords)
            self.assertEqual(graph_meta(b)["method"], "delaunay")

    def test_missing_graph_raises(self):
        from cecelia.utils.spatial_utils import load_graph
        with self.assertRaises(FileNotFoundError):
            load_graph("/nonexistent/spatialGraph/nope.h5ad")


class TestPairwiseContactLogOdds(unittest.TestCase):
    def _clique(self, nodes):
        rows, cols = [], []
        for a in nodes:
            for b in nodes:
                if a < b:
                    rows += [a, b]; cols += [b, a]
        return rows, cols

    def test_two_cliques_associate_within_avoid_between(self):
        # two disjoint triangles: type 0 = {0,1,2}, type 1 = {3,4,5}, NO cross edges
        r0, c0 = self._clique([0, 1, 2])
        r1, c1 = self._clique([3, 4, 5])
        conn = sp.csr_matrix((np.ones(len(r0 + r1)), (r0 + r1, c0 + c1)), shape=(6, 6))
        codes = np.array([0, 0, 0, 1, 1, 1])
        obs, exp, lor, z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=0)
        # 3 within-type edges each, 0 cross-type
        self.assertEqual(obs[0, 0], 3); self.assertEqual(obs[1, 1], 3); self.assertEqual(obs[0, 1], 0)
        self.assertTrue(np.allclose(obs, obs.T))                    # symmetric
        # within-type association (>0), cross-type avoidance (<0)
        self.assertGreater(lor[0, 0], 0); self.assertGreater(lor[1, 1], 0)
        self.assertLess(lor[0, 1], 0)
        self.assertTrue(np.allclose(lor, lor.T))
        # n_permutations=0 → the test is skipped, not silently reported as non-significant
        self.assertTrue(np.all(np.isnan(z))); self.assertTrue(np.all(np.isnan(p)))

    def test_bipartite_associates_between(self):
        # complete bipartite: every edge is cross-type → positive off-diagonal, negative within
        rows, cols = [], []
        for a in (0, 1):
            for b in (2, 3):
                rows += [a, b]; cols += [b, a]
        conn = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(4, 4))
        codes = np.array([0, 0, 1, 1])
        obs, exp, lor, _z, _p = pairwise_contact_logodds(conn, codes, 2, n_permutations=0)
        self.assertEqual(obs[0, 1], 4); self.assertEqual(obs[0, 0], 0)
        self.assertGreater(lor[0, 1], 0); self.assertLess(lor[0, 0], 0)


class TestContactPermutationTest(unittest.TestCase):
    """The label-permutation null (squidpy nhood_enrichment scheme): graph + label counts fixed, only
    the assignment shuffled. `n` is a parameter — these pin the semantics, not a particular n."""
    def _two_cliques(self, k=4):
        """Two disjoint k-cliques, the first all pop0 and the second all pop1 (perfect segregation).
        There are NO edges between the cliques, so a 0–1 contact can only arise if a permutation makes a
        clique non-monochromatic."""
        rows, cols = [], []
        for grp in (range(k), range(k, 2 * k)):
            for a in grp:
                for b in grp:
                    if a < b:
                        rows += [a, b]; cols += [b, a]
        conn = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(2 * k, 2 * k))
        return conn, np.array([0] * k + [1] * k)

    def test_empirical_p_matches_the_exact_combinatorial_null(self):
        # GOLDEN VALUE for the null itself. With two 4-cliques and 4+4 labels, `observed[0,1] == 0`
        # is reproduced by a random relabelling exactly when BOTH cliques come out monochromatic:
        # 2 of the C(8,4)=70 assignments, i.e. p = 1/35 ≈ 0.0286. The empirical p must converge there —
        # this pins that we shuffle labels over nodes with the counts held fixed, which is the whole
        # definition of the test.
        conn, codes = self._two_cliques(4)
        _o, _e, _l, _z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=4000, random_state=1)
        self.assertAlmostEqual(p[0, 1], 1.0 / 35.0, delta=0.008)

    def test_perfect_segregation_is_significant(self):
        # 8+8 nodes: the same all-monochromatic coincidence now has probability 2/C(16,8) ≈ 1.6e-4, so no
        # permutation reproduces it and p sits at the floor 1/(n+1).
        conn, codes = self._two_cliques(8)
        _o, _e, _l, z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=500, random_state=1)
        # within-type contacts far above chance, cross-type far below
        self.assertGreater(z[0, 0], 3.0)
        self.assertLess(z[0, 1], -3.0)
        self.assertAlmostEqual(p[0, 1], 1.0 / 501.0, places=9)
        self.assertTrue(np.allclose(z, z.T))
        self.assertTrue(np.allclose(p, p.T))

    def test_p_is_never_zero_and_is_bounded(self):
        conn, codes = self._two_cliques()
        _o, _e, _l, _z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=50, random_state=2)
        finite = p[np.isfinite(p)]
        self.assertTrue(np.all(finite >= 1.0 / 51.0)) and self.assertTrue(np.all(finite <= 1.0))

    def test_random_labelling_is_not_significant(self):
        # one big clique, labels assigned at random → no spatial structure to find
        nodes = list(range(12))
        rows, cols = [], []
        for a in nodes:
            for b in nodes:
                if a < b:
                    rows += [a, b]; cols += [b, a]
        conn = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(12, 12))
        codes = np.array([0, 1] * 6)
        _o, _e, _l, z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=500, random_state=3)
        # in a clique EVERY labelling gives identical counts → null has zero variance → z undefined, p=1
        self.assertTrue(np.all(np.isnan(z)))
        self.assertTrue(np.allclose(p, 1.0))

    def test_seed_is_reproducible(self):
        conn, codes = self._two_cliques()
        a = pairwise_contact_logodds(conn, codes, 2, n_permutations=100, random_state=7)[3]
        b = pairwise_contact_logodds(conn, codes, 2, n_permutations=100, random_state=7)[3]
        np.testing.assert_allclose(a, b)

    def test_shuffle_stays_within_timepoint(self):
        # two frames, each an isolated pair; frame 0 is all pop0, frame 1 all pop1. A WITHIN-frame
        # shuffle can never produce a 0–1 contact, so the observed 0 cross-contacts is exactly the null.
        rows = [0, 1, 2, 3]; cols = [1, 0, 3, 2]
        conn = sp.csr_matrix((np.ones(4), (rows, cols)), shape=(4, 4))
        codes = np.array([0, 0, 1, 1])
        times = np.array([0, 0, 1, 1])
        _o, _e, _l, _z, p = pairwise_contact_logodds(conn, codes, 2, n_permutations=200,
                                                     random_state=4, times=times)
        self.assertAlmostEqual(p[0, 1], 1.0, places=9)   # nothing to detect once frames are respected
        # …whereas a GLOBAL shuffle mixes the frames and calls the same data significant
        _o2, _e2, _l2, _z2, p2 = pairwise_contact_logodds(conn, codes, 2, n_permutations=200,
                                                          random_state=4, times=None)
        self.assertLess(p2[0, 1], 0.5)

    def test_unassigned_nodes_excluded_from_pairs_but_kept_in_graph(self):
        # a chain 0-1-2 where node 1 is UNASSIGNED: there is then no 0–2 contact (they are not adjacent)
        # and node 1's edges contribute to no pair, so observed is all-zero.
        conn = sp.csr_matrix((np.ones(4), ([0, 1, 1, 2], [1, 0, 2, 1])), shape=(3, 3))
        codes = np.array([0, -1, 1])
        obs, _e, _l, _z, _p = pairwise_contact_logodds(conn, codes, 2, n_permutations=0)
        self.assertTrue(np.allclose(obs, 0.0))


class TestMeshUtils(unittest.TestCase):
    def _vol(self):
        v = np.zeros((6, 6, 20), dtype=np.int32)
        v[1:5, 1:5, 0:5] = 1        # cube A
        v[1:5, 1:5, 10:15] = 2      # cube far from A
        v[1:5, 1:5, 5:9] = 3        # cube adjacent to A
        return v

    def test_build_and_nearest_surface(self):
        from cecelia.utils.mesh_utils import build_label_meshes, nearest_surface
        meshes = build_label_meshes(self._vol(), [1, 2, 3], [1.0, 1.0, 1.0], min_voxels=8)
        self.assertEqual(sorted(meshes), [1, 2, 3])
        # nearest B to A is the adjacent cube 3, ~0 µm
        d, nb = nearest_surface({1: meshes[1]}, {2: meshes[2], 3: meshes[3]})[1]
        self.assertEqual(nb, 3)
        self.assertLess(d, 1.0)
        # only the far cube → ~5 µm gap
        self.assertAlmostEqual(nearest_surface({1: meshes[1]}, {2: meshes[2]})[1][0], 5.0, delta=1.0)

    def test_empty_b(self):
        from cecelia.utils.mesh_utils import build_label_meshes, nearest_surface
        meshes = build_label_meshes(self._vol(), [1], [1.0, 1.0, 1.0], min_voxels=8)
        self.assertEqual(nearest_surface({1: meshes[1]}, {})[1], (float("inf"), None))

    def test_mesh_aggregates(self):
        from cecelia.utils.mesh_utils import build_label_meshes, mesh_aggregates
        # labels 1 & 3 are adjacent (one aggregate); label 2 is far (alone)
        meshes = build_label_meshes(self._vol(), [1, 2, 3], [1.0, 1.0, 1.0], min_voxels=8)
        # 1 & 3 are face-adjacent (surface dist ~0); 2 sits ~1µm from 3. max_dist=0.5 links only 1–3.
        agg = mesh_aggregates(meshes, max_dist=0.5, min_cells=2)
        self.assertEqual(agg[1], agg[3])          # 1 & 3 in the same aggregate
        self.assertNotEqual(agg[1], 0)            # and it's a real aggregate
        self.assertEqual(agg[2], 0)               # 2 not close enough → not aggregated (min_cells=2)


class TestBlockDiagonalGraph(unittest.TestCase):
    """Per-timepoint (behaviour-region) graph: edges must never cross timepoints, even when frames
    are interleaved in row order (exercises the inverse-permutation scatter-back). Needs squidpy."""
    def test_no_cross_timepoint_edges_contiguous(self):
        from cecelia.utils.spatial_utils import build_block_diagonal_graph
        coords = np.array([[0., 0.], [1., 0.], [0., 1.],          # t=0 triangle
                           [0.1, 0.1], [1.1, 0.0], [0.0, 1.1]])   # t=1 triangle, spatially overlapping
        times = np.array([0, 0, 0, 1, 1, 1])
        conn = build_block_diagonal_graph(coords, times, method="delaunay", radius=100.0).tocoo()
        self.assertGreater(conn.nnz, 0)                            # within-frame edges exist
        for i, j in zip(conn.row, conn.col):
            self.assertEqual(times[i], times[j])                  # none cross a timepoint

    def test_interleaved_row_order_preserved(self):
        from cecelia.utils.spatial_utils import build_block_diagonal_graph
        # rows alternate t=0 (near origin) / t=1 (near (5,5)); result must stay in ORIGINAL order
        coords = np.array([[0., 0.], [5., 5.], [0.1, 0.], [5.1, 5.], [0., 0.2], [5., 5.2]])
        times = np.array([0, 1, 0, 1, 0, 1])
        conn = build_block_diagonal_graph(coords, times, method="delaunay", radius=100.0).tocoo()
        self.assertGreater(conn.nnz, 0)
        for i, j in zip(conn.row, conn.col):
            self.assertEqual(times[i], times[j])                  # 0/2/4 link only to each other, 1/3/5 likewise


if __name__ == "__main__":
    unittest.main()


class TestNapariAxisAlignment(unittest.TestCase):
    """`align_axis_vector` is the FALLBACK for when axis names are unknown: it only makes `scale` the
    right LENGTH so napari stops raising ("could not broadcast input array from shape (4,) into shape
    (3,)"). It cannot fix which dimension is which — trailing-axis trimming is right for a (z,y,x)
    volume of a (t,z,y,x) image and wrong for a (t,y,x) projection of one. The name-based aligner that
    does fix it is `expand_to_axes` (see test_layer_axes.py). Pure helpers — no napari needed."""
    def test_trims_to_the_trailing_axes(self):
        from cecelia.utils.napari_utils import align_axis_vector
        # image scale (t, z, y, x) → a 3-D (z,y,x) layer keeps z,y,x, NOT t,z,y
        self.assertEqual(align_axis_vector([1.0, 2.0, 0.5, 0.5], 3), [2.0, 0.5, 0.5])
        self.assertEqual(align_axis_vector(['s', 'um', 'um', 'um'], 3), ['um', 'um', 'um'])

    def test_passes_through_when_already_matching(self):
        from cecelia.utils.napari_utils import align_axis_vector
        v = [2.0, 0.5, 0.5]
        self.assertIs(align_axis_vector(v, 3), v)          # untouched, not copied

    def test_pads_rather_than_raising_when_shorter(self):
        from cecelia.utils.napari_utils import align_axis_vector
        # more layer axes than calibration → pad the LEADING axes with a no-op 1.0
        self.assertEqual(align_axis_vector([0.5, 0.5], 4), [1.0, 1.0, 0.5, 0.5])

    def test_preserves_tuple_type_and_handles_none(self):
        from cecelia.utils.napari_utils import align_axis_vector
        self.assertEqual(align_axis_vector((1.0, 2.0, 3.0, 4.0), 2), (3.0, 4.0))
        self.assertIsNone(align_axis_vector(None, 3))       # absent units stay absent
        self.assertEqual(align_axis_vector([1.0, 2.0], 0), [1.0, 2.0])   # unknown ndim → no change

    def test_layer_ndim_accepts_multiscale_lists(self):
        from cecelia.utils.napari_utils import layer_ndim
        a3 = np.zeros((4, 8, 8)); a4 = np.zeros((2, 4, 8, 8))
        self.assertEqual(layer_ndim(a3), 3)
        self.assertEqual(layer_ndim([a4, np.zeros((2, 4, 4, 4))]), 4)   # level 0 decides
        self.assertEqual(layer_ndim([]), 0)
        self.assertEqual(layer_ndim(None), 0)
