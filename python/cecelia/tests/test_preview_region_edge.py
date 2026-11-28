"""The region-edge preview backend (`_preview_region_edge` in `preview/preview_worker.py`).

It must stage the shape the run measures from — `region_edge_utils.region_mask` on the region's cells —
over the WHOLE image, one label per piece, at the image's pixel grid. Expected values come from the
synthetic layout (two blocks of cells far apart), not from the code's own output. The cell table is a
stand-in for `LabelPropsView` (centroids in pixels, as stored), so no `.h5ad` is written here.
"""
import importlib.util
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import numpy as np
import pandas as pd
import ome_types

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_WORKER = Path(__file__).resolve().parents[3] / 'preview' / 'preview_worker.py'
H, W = 200, 240           # pixels, 0.5 µm each


def _load_worker():
    spec = importlib.util.spec_from_file_location('preview_worker', _WORKER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _omexml():
    return ome_types.from_xml(f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="1" SizeC="1" SizeZ="1"
            SizeY="{H}" SizeX="{W}" PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" Name="CH1" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>""")


def _block(y0, y1, x0, x1, start):
    ys, xs = np.meshgrid(np.arange(y0, y1, 4.0), np.arange(x0, x1, 4.0), indexing='ij')
    n = ys.size
    return pd.DataFrame({'label': np.arange(start, start + n), 'centroid_y': ys.ravel(), 'centroid_x': xs.ravel()})


class _FakeView:
    """`LabelPropsView(path).only_centroid_cols()` → `as_df`, `centroid_columns`, `temporal_columns`."""
    def __init__(self, df):
        self.df = df
    def only_centroid_cols(self):
        return self
    def as_df(self):
        return self.df.copy()
    def centroid_columns(self, order=None):
        return ['centroid_y', 'centroid_x']
    def temporal_columns(self):
        return []
    def close(self):
        pass


@unittest.skipUnless(_WORKER.is_file(), f'worker not present at {_WORKER}')
class RegionEdgePreviewTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.worker = _load_worker()

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        omexml = _omexml()
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [1, 1, 1, H, W]
        du.calc_image_dimensions(shape)
        self.im_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(self.im_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = 1
        ome_xml_utils.save_meta_in_zarr(self.im_path, omexml=omexml)
        # two blocks of cells 2 µm apart, 40 µm from each other: A (rows 20–80, cols 20–80), B (cols 160–220)
        self.cells = pd.concat([_block(20, 80, 20, 80, 1), _block(20, 80, 160, 220, 10000)], ignore_index=True)

    def _preview(self, region_labels, close_um):
        msg = {'type': 'preview', 'imPath': self.im_path, 'taskDir': self.dir, 'outputValueName': 'tissue',
               'funName': 'spatialAnalysis.regionEdge',
               'region': {'xy': {'X': [0, 50], 'Y': [0, 50]}, 'z': 0, 't': 0, 'ndisplay': 2},
               'params': {'propsPath': 'unused', 'regionLabels': region_labels, 'physicalSizes': [1.0, 0.5, 0.5],
                          'closeRadius': close_um, 'fillHoles': True}}
        with mock.patch('cecelia.utils.label_props_utils.LabelPropsView', return_value=_FakeView(self.cells)):
            out = self.worker.execute_command(msg)
        mask = np.asarray(zarr_utils.open_as_zarr(out['layers'][0]['path'])[0][0]).squeeze()
        return out, mask

    def test_whole_image_mask_one_label_per_piece(self):
        out, mask = self._preview(None, close_um=10.0)
        self.assertTrue(out['wholeImage'])
        self.assertEqual(out['counts'], {'base': 2})                # 40 µm gap > 2 × 10 µm
        self.assertEqual(mask.shape, (H, W))                        # the image's own pixel grid
        a, b, gap = mask[50, 50], mask[50, 190], mask[50, 120]
        self.assertTrue(a > 0 and b > 0 and a != b)
        self.assertEqual(gap, 0)
        self.assertEqual(mask[150, 50], 0)                          # below the cells: not region

    def test_a_wide_radius_bridges_the_gap(self):
        out, mask = self._preview(None, close_um=30.0)
        self.assertEqual(out['counts'], {'base': 1})
        self.assertGreater(mask[50, 120], 0)

    def test_only_the_region_cells_shape_it(self):
        out, mask = self._preview(self.cells['label'][self.cells['label'] < 10000].tolist(), close_um=10.0)
        self.assertEqual(out['counts'], {'base': 1})
        self.assertGreater(mask[50, 50], 0)
        self.assertEqual(mask[50, 190], 0)
        self.assertEqual(out['derived']['regionCells'], int((self.cells['label'] < 10000).sum()))


if __name__ == '__main__':
    unittest.main()
