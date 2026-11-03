"""Unit tests for the pure neighbourhood-composition helper
(cecelia.utils.spatial_utils.neighbourhood_composition).

Pure/headless — no squidpy, no anndata, no I/O. Pins the composition formula: per cell, the
normalised frequency of each population among its graph neighbours (the CytoMAP / cecelia "i-niche"
feature; legacy cellRegionsStats `freq = n / sum(n)`). See docs/todo/SPATIAL_REGIONS_PLAN.md."""
import os
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


class TestConditionalNull(unittest.TestCase):
    """`shuffle_codes`: labels shuffled only among the named populations, every other node fixed — the
    random-labelling test (e.g. CD69+ vs CD69− among T cells, target cells held in place)."""
    def _star(self):
        # node 8 = the target (code 2, fixed), adjacent to nodes 0 and 1 only. Nodes 0–7 are T cells:
        # 0–3 CD69+ (code 0), 4–7 CD69− (code 1) — so both of the target's neighbours are CD69+.
        conn = sp.csr_matrix((np.ones(4), ([0, 8, 1, 8], [8, 0, 8, 1])), shape=(9, 9))
        return conn, np.array([0, 0, 0, 0, 1, 1, 1, 1, 2])

    def test_empirical_p_matches_the_exact_hypergeometric_null(self):
        # GOLDEN VALUE. The target's 2 neighbours are a random 2 of the 8 T cells, so the CD69+–target
        # count is hypergeometric(8, 4, 2): P(0) = P(2) = C(4,2)/C(8,2) = 6/28, mean 1. Observed 2 →
        # two-sided p = P(|X−1| ≥ 1) = 12/28 ≈ 0.4286; expected = the null mean = 1.
        conn, codes = self._star()
        obs, exp, lor, _z, p = pairwise_contact_logodds(conn, codes, 3, n_permutations=4000,
                                                        random_state=1, shuffle_codes=[0, 1])
        self.assertEqual(obs[0, 2], 2)
        self.assertAlmostEqual(exp[0, 2], 1.0, delta=0.05)
        self.assertAlmostEqual(p[0, 2], 12.0 / 28.0, delta=0.03)
        self.assertAlmostEqual(lor[0, 2], np.log(2.5 / (exp[0, 2] + 0.5)), places=9)

    def test_fixed_population_never_moves(self):
        # a global shuffle moves the target too, so target–target and the CODEX expected change; with the
        # target fixed its pair count is constant across the null → z undefined, p = 1
        conn, codes = self._star()
        _o, _e, _l, z, p = pairwise_contact_logodds(conn, codes, 3, n_permutations=200,
                                                    random_state=2, shuffle_codes=[0, 1])
        self.assertTrue(np.isnan(z[2, 2]))
        self.assertAlmostEqual(p[2, 2], 1.0)

    def test_zoned_target_is_not_an_association_under_the_conditional_null(self):
        # Zoning: the target sits in a T-cell-rich patch, so under the GLOBAL null (target shuffled out
        # into the empty area) target–T contacts look enriched. CD69 is random among T cells, so the
        # conditional null must NOT call CD69+–target significant even though the global one does.
        rng = np.random.default_rng(0)
        n_t, n_far = 40, 40                          # 40 T cells in the patch, 40 unrelated far cells
        tgt = n_t + n_far                            # one target, adjacent to every patch T cell
        rows = list(range(n_t)) + [tgt] * n_t
        cols = [tgt] * n_t + list(range(n_t))
        # far cells form a chain among themselves so they are in the graph but never touch the target
        for a in range(n_t, n_t + n_far - 1):
            rows += [a, a + 1]; cols += [a + 1, a]
        conn = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(tgt + 1, tgt + 1))
        codes = np.empty(tgt + 1, dtype=np.int64)
        codes[:n_t] = rng.permutation([0] * 20 + [1] * 20)     # CD69± random within the patch
        codes[n_t:tgt] = 3                                      # far cells: a 4th population
        codes[tgt] = 2
        _o, _e, _l, _z, p_glob = pairwise_contact_logodds(conn, codes, 4, n_permutations=500, random_state=3)
        _o, _e, lor, _z, p_cond = pairwise_contact_logodds(conn, codes, 4, n_permutations=500,
                                                           random_state=3, shuffle_codes=[0, 1])
        self.assertLess(p_glob[0, 2], 0.05)
        self.assertGreater(p_cond[0, 2], 0.5)                   # all 20 CD69+ touch it either way
        self.assertAlmostEqual(lor[0, 2], 0.0, places=6)

    def test_effect_size_needs_no_permutations(self):
        # the expected count is the EXACT permutation mean, so the descriptive fast path still works
        conn, codes = self._star()
        _o, exp, _l, z, _p = pairwise_contact_logodds(conn, codes, 3, n_permutations=0, shuffle_codes=[0, 1])
        self.assertAlmostEqual(exp[0, 2], 1.0, places=12)
        self.assertTrue(np.all(np.isnan(z)))


class TestPermutationExpected(unittest.TestCase):
    """`_permutation_expected` is the exact mean of `_contact_counts` over the permutation group —
    checked against brute-force enumeration of every distinct relabelling."""
    def _brute(self, row, col, codes, n_pops, groups):
        from itertools import permutations
        from cecelia.utils.spatial_utils import _contact_counts
        acc, n = np.zeros((n_pops, n_pops)), 0
        perms = [set(permutations(codes[g])) for g in groups]
        from itertools import product
        for combo in product(*perms):
            lab = codes.copy()
            for g, pg in zip(groups, combo):
                lab[g] = pg
            acc += _contact_counts(row, col, lab, n_pops); n += 1
        return acc / n

    def test_matches_enumeration(self):
        from cecelia.utils.spatial_utils import _permutation_expected
        rng = np.random.default_rng(5)
        n = 9
        a = sp.triu(sp.random(n, n, density=0.5, random_state=5), k=1).tocoo()
        row, col = a.row, a.col
        codes = rng.integers(-1, 3, size=n)                      # includes UNASSIGNED nodes
        assigned = np.where(codes >= 0)[0]
        for groups in ([assigned],                               # global shuffle
                       [assigned[codes[assigned] < 2]],          # conditional: pop 2 fixed
                       [assigned[:3], assigned[3:]]):            # two frames
            np.testing.assert_allclose(_permutation_expected(row, col, codes, 3, groups),
                                       self._brute(row, col, codes, 3, groups), atol=1e-12)


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

    def test_nearest_surface_per_target(self):
        # one result per target, each searched only among its own B (#1734): the far cube 2 is its
        # target's nearest even though cube 3 (the other target) is closer to A
        from cecelia.utils.mesh_utils import build_label_meshes, nearest_surface_per_target
        meshes = build_label_meshes(self._vol(), [1, 2, 3], [1.0, 1.0, 1.0], min_voxels=8)
        out = nearest_surface_per_target({1: meshes[1]}, {2: meshes[2], 3: meshes[3]},
                                         [("far", {2}), ("near", {3}), ("both", {2, 3}), ("none", {9})])
        self.assertEqual(out["far"][1][1], 2)
        self.assertAlmostEqual(out["far"][1][0], 5.0, delta=1.0)
        self.assertEqual(out["near"][1][1], 3)
        self.assertLess(out["near"][1][0], 1.0)
        self.assertEqual(out["both"][1][1], 3)                  # pooled = nearest of any
        self.assertEqual(out["none"][1], (float("inf"), None))  # a target with no meshes

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


class TestPooledImageGraphPerFrame(unittest.TestCase):
    """The persisted neighbour graph is per frame BY DEFAULT: on a real tracked timecourse, no edge links
    two frames unless pooling is asked for. Needs squidpy + the committed KDIeEm fixture."""
    _ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
        os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data", "projects"))
    _B = os.path.join(_ROOT, "testpr", "1", "KDIeEm", "labelProps", "B.h5ad")

    def _edges_by_frame(self, **kw):
        from cecelia.utils.label_props_utils import LabelPropsView
        from cecelia.utils.spatial_utils import build_pooled_image_graph
        if not os.path.isfile(self._B):
            self.skipTest("KDIeEm fixture missing")
        segs = [{"valueName": "B", "propsPath": self._B, "labels": None}]
        adata, obs = build_pooled_image_graph(segs, [1.0, 1.0, 1.0], method="delaunay", radius=30.0, **kw)
        view = LabelPropsView(self._B).only_centroid_cols()
        d = view.as_df(); tcol = view.temporal_columns()[0]; view.close()
        t_of = dict(zip(d["label"].to_numpy(), d[tcol].to_numpy()))
        t = np.array([t_of[lb] for lb in obs["label"].to_numpy()])
        conn = adata.obsp["spatial_connectivities"].tocoo()
        self.assertGreater(conn.nnz, 0)
        return t[conn.row] == t[conn.col]

    def test_default_graph_has_no_cross_frame_edges(self):
        self.assertTrue(self._edges_by_frame().all())

    def test_pooling_is_opt_in_and_does_cross_frames(self):
        self.assertFalse(self._edges_by_frame(per_timepoint=False).all())

    def test_pooled_graph_recovers_each_nodes_frame(self):
        """graph_node_times reads a pooled graph's frames back from the cell table, and scoring within
        them stops a cell matching itself one frame later (SPATIAL_PATTERN_PLAN Decision 4)."""
        from cecelia.utils.label_props_utils import LabelPropsView
        from cecelia.utils.spatial_pattern_utils import pattern_test
        from cecelia.utils.spatial_utils import build_pooled_image_graph, graph_node_times
        if not os.path.isfile(self._B):
            self.skipTest("KDIeEm fixture missing")
        segs = [{"valueName": "B", "propsPath": self._B, "labels": None}]
        adata, obs = build_pooled_image_graph(segs, [1.0, 1.0, 1.0], per_timepoint=False)
        self.assertNotIn("_t", obs.columns)
        t = graph_node_times(obs, {"B": self._B})
        view = LabelPropsView(self._B).only_centroid_cols()
        d = view.as_df(); tcol = view.temporal_columns()[0]; view.close()
        t_of = dict(zip(d["label"].to_numpy(), d[tcol].to_numpy()))
        np.testing.assert_array_equal(t, [t_of[lb] for lb in obs["label"].to_numpy()])
        self.assertIsNone(graph_node_times(obs, {}))              # no table → no frames, not zeros
        coords = adata.obsm["spatial"]
        mask = np.zeros(len(obs), bool); mask[::3] = True
        pooled = pattern_test(coords, mask, times=None, n_permutations=0)
        framed = pattern_test(coords, mask, times=t, n_permutations=0)
        self.assertGreater(framed["nnObserved"], pooled["nnObserved"])


if __name__ == "__main__":
    unittest.main()
