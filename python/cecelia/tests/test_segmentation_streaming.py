"""Characterisation + streaming-equivalence test for SegmentationUtils.predict_from_zarr.

predict_from_zarr used to hold the whole T×Z×Y×X label stack (per label type) in RAM. It now
processes ONE TIMEPOINT end-to-end (fill tiles → seam-stitch → post-process → nuc/base match →
write frame to disk) — a byte-identical reordering (every post-fill step already looped per
timepoint; the only cross-frame state is the monotonic max_labels counter). This test pins the
label output (base + nuc) with a deterministic stub predict_slice so no cellpose is needed, and
exercises seam stitching, post-processing, and nuc/base matching across multiple timepoints.

The golden fingerprints below were captured from the pre-refactor whole-stack implementation.
Run with `pixi run test-py`.
"""
import hashlib
import os
import shutil
import tempfile
import unittest

import numpy as np
import dask.array as da
import ome_types
import zarr

import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils, count_labels


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeZ="{size_z}" SizeC="{size_c}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      {''.join(f'<Channel ID="Channel:0:{c}" SamplesPerPixel="1"/>' for c in range(size_c))}
    </Pixels>
  </Image>
</OME>"""


class _StubSeg(SegmentationUtils):
    """Deterministic predict_slice for a 2D ([C,Y,X]) tile: a Y-split label pattern (top half = 1,
    bottom half = 2) depending only on the tile footprint, so cells straddle tile seams (exercises
    seam stitching) and are reproducible. 2D only — the real predict_slice transposes to cellpose's
    axis order via dim_utils; a stub that mirrored that for arbitrary 3D orders would be more test
    machinery than it's worth. The 3D-specific paths (Z post-processing, 3D stitch) are covered by
    the production cellpose path, not here."""

    def predict_slice(self, tile, model_params, norm_params=None):
        yx = tile.shape[-2:]                 # (Y, X)
        masks = np.zeros(yx, dtype=np.uint32)
        half = yx[0] // 2
        masks[:half, :] = 1
        masks[half:, :] = 2
        return masks


class _FailingSeg(_StubSeg):
    """Same stub, but dies partway through the run — a task that fails mid-stream."""

    fail_after = 2

    def predict_slice(self, tile, model_params, norm_params=None):
        self._n = getattr(self, '_n', 0) + 1
        if self._n > self.fail_after:
            raise RuntimeError('segmentation died mid-run')
        return super().predict_slice(tile, model_params, norm_params)


def _run(tmp, sizes, arr_shape, cls=_StubSeg):
    du = DimUtils(ome_types.from_xml(_ome_xml(*sizes)), use_channel_axis=True)
    du.calc_image_dimensions(arr_shape)
    shape = tuple(du.im_dim)
    rng = np.random.default_rng(0)
    im0 = rng.integers(0, 4000, size=shape, dtype=np.uint16)

    params = {
        'taskDir': tmp,
        'outputValueName': 'stub',
        'blockSize': 12, 'overlap': 4,        # 20/12 → 2 tiles in X, 24/12 → 2 in Y (seams present)
        'labelOverlap': 0.1,                   # enable seam stitching
        'matchThreshold': 0.1, 'removeUnmatched': False,
        'minCellSize': 0, 'cellSizeMax': 0,
        'labelExpansion': 0, 'labelErosion': 0,
        'clearTouchingBorder': False, 'clearDepth': False,
        'normaliseToWhole': False,
        'models': {
            '0': {'matchAs': 'base', 'cellChannels': [0]},
            '1': {'matchAs': 'nuc',  'nucChannels': [1]},
        },
    }
    seg = cls(params, du)
    counts = seg.predict_from_zarr([im0])

    labels_dir = os.path.join(tmp, 'labels')
    base = zarr.open_group(os.path.join(labels_dir, 'stub.zarr'), mode='r')['0'][:]
    nuc = zarr.open_group(os.path.join(labels_dir, 'stub_nuc.zarr'), mode='r')['0'][:]
    return counts, base, nuc


def _fingerprint(arr):
    return (tuple(arr.shape), int(arr.sum()), count_labels(arr),
            hashlib.sha1(np.ascontiguousarray(arr).tobytes()).hexdigest())


# (sizes T,Z,C,Y,X) and the array shape to feed calc_image_dimensions (size-1 axes dropped).
# 2D timeseries, channel NOT last (resolves to [T,C,Y,X]) so it exercises the input-frame vs
# label-frame axis-index distinction in read-frame-once tiling.
_CASE_2D = ((3, 1, 2, 24, 20), (3, 2, 24, 20))


class PredictFromZarrTest(unittest.TestCase):
    """base == nuc: the stub emits the same pattern on both channels, so nuc/base IoU matching makes
    nuc adopt the base IDs verbatim. Golden captured from the pre-refactor whole-stack implementation
    (verified to survive the per-frame streaming refactor AND the read-frame-once change)."""

    def test_2d_timeseries_matches_golden(self):
        d = tempfile.mkdtemp()
        try:
            counts, base, nuc = _run(d, *_CASE_2D)
        finally:
            shutil.rmtree(d, ignore_errors=True)
        gold = ((3, 24, 20), 17712, 24, '4f3ef287d29996db5ad53ad0c04357c607e7922d')
        self.assertEqual(counts, {'base': 24, 'nuc': 24})
        self.assertEqual(_fingerprint(base), gold)
        self.assertEqual(_fingerprint(nuc), gold)


class FailedRerunTest(unittest.TestCase):
    """Re-running a value_name that is ALREADY registered and then failing must leave the existing
    labels exactly as they were (docs/SEGMENTATION.md → *Stores are written staged*).

    Before staging, the writer `rmtree`d the target and streamed into it, so this left ccid.json
    advertising a store with most of its frames missing. These stores are SINGLE-LEVEL — the case
    with no error at all: the missing frames read as zeros, and measurement/tracking silently
    produced numbers from a partial segmentation.
    """

    def test_registered_labels_survive_a_failed_rerun(self):
        d = tempfile.mkdtemp()
        try:
            _, base, nuc = _run(d, *_CASE_2D)
            before = (_fingerprint(base), _fingerprint(nuc))

            with self.assertRaises(RuntimeError):
                _run(d, *_CASE_2D, cls=_FailingSeg)

            labels_dir = os.path.join(d, 'labels')
            again = tuple(_fingerprint(zarr.open_group(os.path.join(labels_dir, name), mode='r')['0'][:])
                          for name in ('stub.zarr', 'stub_nuc.zarr'))
            self.assertEqual(again, before, 'a failed re-run damaged the registered labels')

            leftover = [n for n in os.listdir(labels_dir)
                        if n.endswith((zarr_utils.STAGING_SUFFIX, zarr_utils.SUPERSEDED_SUFFIX))]
            self.assertEqual(leftover, [], 'staging debris left behind after an unwound failure')
        finally:
            shutil.rmtree(d, ignore_errors=True)


class ComputeNormParamsStreamingTest(unittest.TestCase):
    """Scale-to-whole normalisation on a SINGLE-LEVEL store (drift/AF/cellpose-corrected output)
    must derive its global percentile from a streamed histogram, not by materialising the whole
    level (the second OOM vector). For integer data that matches np.percentile over nonzero values
    to within one intensity bin (histogram CDF vs linear interpolation)."""

    def test_single_level_matches_numpy_percentile(self):
        du = DimUtils(ome_types.from_xml(_ome_xml(1, 1, 2, 41, 29)), use_channel_axis=True)
        du.calc_image_dimensions((2, 41, 29))          # C,Y,X (size-1 T,Z dropped)
        c_idx = du.dim_idx('C')
        rng = np.random.default_rng(3)
        im0 = rng.integers(0, 5000, size=tuple(du.im_dim), dtype=np.uint16)
        im0[im0 < 500] = 0                              # sprinkle background zeros

        seg = SegmentationUtils({'taskDir': tempfile.gettempdir()}, du)
        mp = {'cellChannels': [1], 'nucChannels': [], 'normalise': 99.9}
        # single-level store -> streaming histogram path
        got = seg._compute_norm_params([da.from_array(im0)], mp)

        idx = [slice(None)] * im0.ndim
        idx[c_idx] = 1
        valid = im0[tuple(idx)].ravel()
        valid = valid[valid > 0]
        lo_ref = np.percentile(valid, 0.1)
        hi_ref = np.percentile(valid, 99.9)

        self.assertIn(1, got)
        self.assertLessEqual(abs(got[1][0] - lo_ref), 1.0)
        self.assertLessEqual(abs(got[1][1] - hi_ref), 1.0)


if __name__ == "__main__":
    import json
    d = tempfile.mkdtemp()
    try:
        counts, base, nuc = _run(d, *_CASE_2D)
        print(json.dumps({'counts': counts, 'base': _fingerprint(base),
                          'nuc': _fingerprint(nuc)}, default=str))
    finally:
        shutil.rmtree(d, ignore_errors=True)


class MidRunReadabilityTest(unittest.TestCase):
    """A label store must be READABLE while the run that writes it is still going — that is what the
    napari live preview shows (`show_labels(preview=True)`).

    Two properties hold it up, and both are pinned here because the preview silently breaks if either
    changes:

    1. The store exists at its FULL final shape from before the first frame, so a viewer's dask view
       stays valid for the whole run and every refresh is a like-for-like re-read.
    2. Only level 0 is on disk until `_finalize_label_pyramid` runs, even though `.zattrs` already
       declares the whole pyramid. Asking for the declared depth therefore RAISES, and the preview
       must ask for level 0 alone.
    """

    def _store(self, tmp, nscales):
        du = DimUtils(ome_types.from_xml(_ome_xml(*_CASE_2D[0])), use_channel_axis=True)
        du.calc_image_dimensions(_CASE_2D[1])
        seg = _StubSeg({'taskDir': tmp, 'outputValueName': 'stub'}, du)
        label_axes = [ax for ax in du.im_dim_order if ax != 'C']
        label_shape = [du.im_dim[i] for i, ax in enumerate(du.im_dim_order) if ax != 'C']
        path = os.path.join(tmp, 'labels', 'stub.zarr')
        os.makedirs(os.path.dirname(path), exist_ok=True)
        _, level0, _ = seg._open_label_store(path, label_shape, label_axes, nscales)
        return path, level0, tuple(label_shape)

    def test_level0_is_full_shape_before_any_frame_is_written(self):
        d = tempfile.mkdtemp()
        try:
            path, level0, label_shape = self._store(d, 1)
            # allocated up front: the shape a preview layer binds to never changes mid-run
            self.assertEqual(tuple(level0.shape), label_shape)
            arrays, _ = zarr_utils.open_zarr(path, multiscales=1, as_dask=True)
            self.assertEqual(len(arrays), 1)
            self.assertEqual(tuple(arrays[0].shape), label_shape)
            # unwritten frames read as background, not as an error
            self.assertEqual(int(np.asarray(arrays[0][0]).sum()), 0)

            # write ONE frame and see it through a FRESH open, as a refresh does
            level0[0] = np.ones(label_shape[1:], dtype=np.uint32)
            arrays, _ = zarr_utils.open_zarr(path, multiscales=1, as_dask=True)
            self.assertEqual(int(np.asarray(arrays[0][0]).sum()), int(np.prod(label_shape[1:])))
            self.assertEqual(int(np.asarray(arrays[0][1]).sum()), 0)   # later frames still pending
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def test_declared_pyramid_depth_is_unreadable_until_finalised(self):
        d = tempfile.mkdtemp()
        try:
            path, _, _ = self._store(d, 4)
            declared = zarr_utils.read_multiscales_meta(path)['datasets']
            self.assertEqual([x['path'] for x in declared], ['0', '1', '2', '3'])
            # asking for the declared depth (what a normal show_labels does) cannot work yet
            with self.assertRaises(KeyError):
                zarr_utils.open_zarr(path, multiscales=4, as_dask=True)
            # ...nor can the default "every declared level"
            with self.assertRaises(KeyError):
                zarr_utils.open_zarr(path, as_dask=True)
            # level 0 alone is fine — this is what preview=True asks for
            arrays, _ = zarr_utils.open_zarr(path, multiscales=1, as_dask=True)
            self.assertEqual(len(arrays), 1)
        finally:
            shutil.rmtree(d, ignore_errors=True)
