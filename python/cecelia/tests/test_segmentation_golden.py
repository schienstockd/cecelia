"""Segmentation against known truth: real cellpose on a synthetic image whose answer is built in.

`pipeline_golden.jl` starts from imported labels, so a change to segmentation (a default, the tiling,
the stitch, a cellpose bump) never reaches a stored answer there. This does: a 4 × 5 grid of bright
discs, radius 6 px, so the answer is 20 cells of 113 px per plane, each centre inside its own label.
It runs the app's own defaults for tiling and seams (`auto` overlap, `labelOverlap` 0.25,
`matchThreshold` 0.3).

Opt-in (CECELIA_SEGMENTATION_GOLDEN=1) because it loads real model weights; CI runs it weekly and on
segmentation or dependency changes (`.github/workflows/verify-segmentation-golden.yml`):

    CECELIA_SEGMENTATION_GOLDEN=1 pixi run python -m unittest cecelia.tests.test_segmentation_golden
"""
from __future__ import annotations

import os
import shutil
import tempfile
import unittest

import numpy as np
import ome_types
import zarr

from cecelia.tests.test_cellpose_smoke import _blobs, _ome_xml
from cecelia.utils import cellpose_utils as cpu
from cecelia.utils.dim_utils import DimUtils

_ENABLED = os.environ.get("CECELIA_SEGMENTATION_GOLDEN") == "1"
_Y, _X = 96, 112
#: Where `_blobs` puts its discs: the truth, by construction.
CENTRES = [(cy, cx) for cy in range(12, _Y - 8, 20) for cx in range(12, _X - 8, 20)]
#: Pixels in one disc of radius 6 (`(y-cy)^2 + (x-cx)^2 <= 36`).
DISC_PX = 113


@unittest.skipUnless(_ENABLED, "set CECELIA_SEGMENTATION_GOLDEN=1 to run real cellpose against known truth")
class SegmentationGoldenTest(unittest.TestCase):

    def setUp(self):
        self.model = "cyto3" if cpu._cellpose_major_version() == 3 else "cpsam_v2"
        self.tmp = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmp, True)

    def _segment(self, size_z: int, block_size) -> np.ndarray:
        im = np.broadcast_to(_blobs(_Y, _X), (1, 1, size_z, _Y, _X)).copy()
        du = DimUtils(ome_types.from_xml(_ome_xml(size_z, _Y, _X)), use_channel_axis=True)
        du.calc_image_dimensions(im.shape)
        params = {
            "taskDir": self.tmp, "outputValueName": "golden",
            "blockSize": block_size, "overlap": "auto", "labelOverlap": 0.25, "matchThreshold": 0.3,
            "removeUnmatched": False, "minCellSize": 0, "cellSizeMax": 0, "labelExpansion": 0,
            "labelErosion": 0, "clearTouchingBorder": False, "clearDepth": False, "normaliseToWhole": False,
            "models": {"0": {"matchAs": "base", "model": self.model, "cellChannels": [0],
                             "cellDiameter": 6, "stitchThreshold": 0.2 if size_z > 1 else 0.0}},
        }
        cpu.CellposeUtils(params, du).predict_from_zarr([im])
        labels = zarr.open_group(os.path.join(self.tmp, "labels", "golden.zarr"), mode="r")["0"][:]
        return labels.reshape(size_z, _Y, _X)

    def _assert_truth(self, labels: np.ndarray):
        ids = [i for i in np.unique(labels) if i]
        self.assertEqual(len(ids), len(CENTRES), "one label per disc: none split at a seam, none merged")
        for z, plane in enumerate(labels):
            at = [int(plane[cy, cx]) for cy, cx in CENTRES]
            self.assertNotIn(0, at, f"plane {z}: a disc centre fell in no label")
            self.assertEqual(len(set(at)), len(CENTRES), f"plane {z}: two discs share a label")
            for i in at:   # each disc's own area, within 20% of the drawn disc
                self.assertAlmostEqual(int((plane == i).sum()), DISC_PX, delta=0.2 * DISC_PX)

    def test_one_plane_one_tile(self):
        self._assert_truth(self._segment(size_z=1, block_size="auto"))

    def test_a_z_stack_in_four_tiles_stitched(self):
        # 2 × 2 tiles of 64 px: four discs sit across a seam. Z stitching keeps each cell one label.
        labels = self._segment(size_z=3, block_size=64)
        self._assert_truth(labels)
        for i in (i for i in np.unique(labels) if i):
            self.assertEqual(sum(bool((p == i).any()) for p in labels), 3, f"label {i} not stitched through z")


if __name__ == "__main__":
    unittest.main()
