"""Measuring block by block gives the whole-plane result, for every object, exactly once.

`MeasureUtils.measure_from_zarr` never holds a whole timepoint: each object is measured from a crop
of the block that owns it (its bounding-box start). This pins the guarantees that design rests on
(docs/todo/MEASURE_TILING_PLAN.md, Decisions 1–5):

- objects straddling block seams and touching the image edge each get exactly one row;
- an object reaching past `MARGIN_CAP` is measured from its own crop, not dropped;
- morphology, centroids, mean/median intensity, a Gaussian pre-filter, secondary label types and the
  `pass` column all equal a single whole-plane pass;
- and the single-block run itself equals plain skimage on the whole plane, so the comparison isn't
  one implementation agreeing with itself.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import tempfile
import unittest
from unittest import mock

import anndata as ad
import numpy as np
import skimage.filters as skfilt
import skimage.measure as skmeas
from skimage.draw import disk, ellipse
from skimage.segmentation import expand_labels

from cecelia.utils import measure_utils
from cecelia.utils.measure_utils import MeasureUtils


class _Log:
    def log(self, msg):
        pass

    def progress(self, n, total):
        pass


class _Dims:
    def __init__(self, order, shape):
        self.im_dim_order = order
        self.im_dim = list(shape)


def _labels_2d(seed=0, shape=(96, 112)):
    rng = np.random.default_rng(seed)
    lab = np.zeros(shape, np.uint32)
    lid = 1
    # a grid of small cells, many of them across the 16 px seams
    for y in range(6, shape[0] - 6, 13):
        for x in range(6, shape[1] - 6, 13):
            rr, cc = disk((y + rng.integers(-2, 3), x + rng.integers(-2, 3)), rng.integers(3, 6),
                          shape=shape)
            lab[rr, cc] = lid
            lid += 1
    # cells cut by the image edge
    for y, x in ((0, 40), (shape[0] - 1, 70), (30, 0), (60, shape[1] - 1)):
        rr, cc = disk((y, x), 5, shape=shape)
        lab[rr, cc] = lid
        lid += 1
    # one long object reaching far past its block — the outlier path under a small cap
    rr, cc = ellipse(48, 56, 8, 50, shape=shape, rotation=0.3)
    lab[rr, cc] = lid
    # sparse, large ids: dense relabelling must not depend on them being 1..n
    lab[lab > 0] += 100_000
    return lab


def _run(lab_t, im_t, order, *, block, sigma=0.0, measure='mean', secondary=False,
         passes=None, cap=None, extended=False, workers=1):
    """Run the real `measure_from_zarr` on in-memory arrays; return the written AnnData."""
    tmp = tempfile.mkdtemp()
    params = dict(taskDir=tmp, outputValueName='blk', intensityMeasure=measure,
                  gaussianFilter=sigma, extendedMeasures=extended, blockSize=block)
    mu = MeasureUtils(params, _Dims(order, im_t.shape))
    labels = {'base': [lab_t]}
    if secondary:
        # a halo reaches past its cell — the owning crop must widen to hold it
        halo = np.stack([expand_labels(f, 4) - f for f in lab_t]).astype(lab_t.dtype)
        labels['halo'] = [halo]
    patch = mock.patch.object(measure_utils, 'MARGIN_CAP', cap) if cap is not None else None
    if patch:
        patch.start()
    try:
        path = mu.measure_from_zarr(labels, [im_t], _Log(), label_passes=passes, n_workers=workers)
    finally:
        if patch:
            patch.stop()
    return ad.read_h5ad(path)


def _assert_same(test, a, b):
    test.assertEqual(list(a.obs_names), list(b.obs_names))
    test.assertEqual(list(a.var_names), list(b.var_names))
    np.testing.assert_allclose(a.X, b.X, rtol=1e-5, atol=1e-4)
    for k in b.obsm:
        np.testing.assert_allclose(a.obsm[k], b.obsm[k], rtol=1e-6, atol=1e-4)
    test.assertEqual(list(a.obs.columns), list(b.obs.columns))
    for c in b.obs.columns:
        test.assertEqual(list(a.obs[c].astype(str)), list(b.obs[c].astype(str)))


class BlockOwnership2DTest(unittest.TestCase):
    order = ['T', 'C', 'Y', 'X']

    @classmethod
    def setUpClass(cls):
        lab = _labels_2d()
        cls.lab = np.stack([lab, np.roll(lab, 7, axis=1)])                  # T=2
        rng = np.random.default_rng(1)
        cls.im = rng.integers(0, 4000, size=(2, 3) + lab.shape).astype(np.uint16)
        cls.n_obj = [len(np.unique(f)) - 1 for f in cls.lab]

    def _pair(self, **kw):
        return (_run(self.lab, self.im, self.order, block=16, **kw),
                _run(self.lab, self.im, self.order, block=10_000, **kw))

    def test_every_object_exactly_once(self):
        blk, _ = self._pair()
        self.assertEqual(blk.n_obs, sum(self.n_obj))
        t = blk.obsm['temporal'][:, 0]
        for ti, n in enumerate(self.n_obj):
            ids = blk.obs_names[t == ti]
            self.assertEqual(len(ids), n)
            self.assertEqual(len(set(ids)), n)

    def test_blocks_equal_whole_plane(self):
        _assert_same(self, *self._pair())

    def test_outlier_measured_alone_not_dropped(self):
        # a cap of 8 px forces the long ellipse (and some edge cells) onto their own crops
        blk = _run(self.lab, self.im, self.order, block=16, cap=8)
        whole = _run(self.lab, self.im, self.order, block=10_000)
        _assert_same(self, blk, whole)

    def test_worker_processes_match_serial(self):
        # spawn workers can't see the parent's closures — the pass column is stamped in the parent
        ids = np.unique(self.lab)
        ids = ids[ids > 0]
        passes = [{'group': '0', 'from': int(ids[0]), 'to': int(ids[-1])}]
        kw = dict(block=16, sigma=1.0, secondary=True, passes=passes)
        _assert_same(self, _run(self.lab, self.im, self.order, workers=3, **kw),
                     _run(self.lab, self.im, self.order, **kw))

    def test_gaussian_halo_matches_whole_plane_smoothing(self):
        _assert_same(self, *self._pair(sigma=1.5))

    def test_median_and_secondary_and_pass(self):
        ids = np.unique(self.lab)
        ids = ids[ids > 0]
        mid = int(ids[len(ids) // 2])
        passes = [{'group': '0', 'from': int(ids[0]), 'to': mid},
                  {'group': '1', 'from': mid + 1, 'to': int(ids[-1])}]
        blk, whole = self._pair(measure='median', secondary=True, passes=passes)
        _assert_same(self, blk, whole)
        self.assertIn('halo_median_intensity_0', list(blk.var_names))
        self.assertIn('pass', blk.obs.columns)

    def test_whole_plane_matches_plain_skimage(self):
        whole = _run(self.lab, self.im, self.order, block=10_000, sigma=1.0)
        t = whole.obsm['temporal'][:, 0]
        frame = self.lab[0]
        sm = skfilt.gaussian(self.im[0, 2].astype(np.float32), sigma=1.0, preserve_range=True)
        ref = skmeas.regionprops_table(frame, intensity_image=sm,
                                       properties=['label', 'area', 'perimeter', 'centroid',
                                                   'solidity', 'mean_intensity'])
        got = whole[t == 0]
        self.assertEqual([int(x) for x in got.obs_names], list(ref['label']))
        col = lambda name: got.X[:, list(got.var_names).index(name)]
        np.testing.assert_allclose(col('area'), ref['area'], rtol=1e-6)
        np.testing.assert_allclose(col('perimeter'), ref['perimeter'], rtol=1e-5)
        np.testing.assert_allclose(col('solidity'), ref['solidity'], rtol=1e-5)
        np.testing.assert_allclose(col('mean_intensity_2'), ref['mean_intensity'], rtol=1e-5)
        np.testing.assert_allclose(got.obsm['spatial'][:, 0], ref['centroid-0'], rtol=1e-6)
        np.testing.assert_allclose(got.obsm['spatial'][:, 1], ref['centroid-1'], rtol=1e-6)


class BlockOwnership3DTest(unittest.TestCase):
    order = ['C', 'Z', 'Y', 'X']

    @classmethod
    def setUpClass(cls):
        rng = np.random.default_rng(2)
        lab = np.zeros((6, 48, 56), np.uint32)
        lid = 1
        for y in range(5, 44, 10):
            for x in range(5, 52, 10):
                z0 = rng.integers(0, 3)
                rr, cc = disk((y, x), rng.integers(3, 5), shape=lab.shape[1:])
                lab[z0:z0 + 3, rr, cc] = lid
                lid += 1
        cls.lab = lab
        cls.im = rng.integers(0, 4000, size=(2,) + lab.shape).astype(np.uint16)

    def test_blocks_equal_whole_volume(self):
        blk = _run(self.lab, self.im, self.order, block=16)
        whole = _run(self.lab, self.im, self.order, block=10_000)
        self.assertEqual(blk.n_obs, len(np.unique(self.lab)) - 1)
        _assert_same(self, blk, whole)

    @unittest.skipUnless(measure_utils._HAS_TRIMESH, 'trimesh not installed')
    def test_extended_mesh_measures_equal_whole_volume(self):
        blk = _run(self.lab, self.im, self.order, block=16, extended=True)
        whole = _run(self.lab, self.im, self.order, block=10_000, extended=True)
        _assert_same(self, blk, whole)

    @unittest.skipUnless(measure_utils._HAS_TRIMESH, 'trimesh not installed')
    def test_block_meshes_sit_in_the_image_frame(self):
        # qhull can be sensitive to the coordinate range it is handed (see
        # test_measure_extended_3d), so a crop's meshes must be positioned as the whole volume's
        # would be. The drift itself is platform-dependent; the positioning is not — assert that.
        from cecelia.utils import mesh_utils
        real = mesh_utils.build_label_meshes
        calls = []

        def spy(vol, labels, phys, min_voxels=8, origin=None):
            calls.append((vol.copy(), origin))
            return real(vol, labels, phys, min_voxels=min_voxels, origin=origin)

        with mock.patch.object(mesh_utils, 'build_label_meshes', spy):
            _run(self.lab, self.im, self.order, block=16, extended=True)
        self.assertTrue(any(o is not None and any(o) for _, o in calls))
        for vol, origin in calls:
            idx = np.argwhere(vol > 0) + np.asarray(origin if origin is not None else 0)
            self.assertTrue((self.lab[tuple(idx.T)] > 0).all())
        full = real(self.lab, [1, 2], (1.0, 1.0, 1.0), min_voxels=4)
        crop = real(self.lab[:, 10:30, 10:30], [1, 2], (1.0, 1.0, 1.0), min_voxels=4,
                    origin=(0, 10, 10))
        for lb in set(full) & set(crop):
            np.testing.assert_array_equal(full[lb].vertices, crop[lb].vertices)


if __name__ == '__main__':
    unittest.main()
