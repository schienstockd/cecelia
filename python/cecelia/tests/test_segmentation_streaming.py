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


def _run(tmp, sizes, arr_shape):
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
    seg = _StubSeg(params, du)
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
