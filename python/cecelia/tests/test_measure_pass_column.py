"""A multi-pass segmentation's PASS reaches the measurement, as `obs['pass']`.

The store has recorded which model group found each label since two-pass shipped
(`zarr_utils.write_label_passes`), and until now **nothing read it**. That made a two-pass result
indistinguishable from a single-pass one everywhere downstream — which is the whole point of running
two passes, since the cells-vs-fragments split is a GATING decision on the measured table, not a
segmentation parameter (docs/SEGMENTATION.md -> *Two passes = two model groups*).

Why `obs` and not `X`: a model-group key is categorical. In X it would be a float joining every
distance, PCA and clustering computation as though a pass number were a measurement.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np
import pandas as pd

from cecelia.utils.measure_utils import MeasureUtils


class _Stub(MeasureUtils):
    """`_to_anndata` only — it is the boundary where a column becomes obs, obsm or X."""

    def __init__(self, tmp):
        self.task_dir = tmp
        self.output_value_name = 'twopass'
        self.intensity_measure = 'mean'


def _frame(n=4, with_pass=True):
    df = pd.DataFrame({
        'area': np.arange(1, n + 1, dtype=float),
        'centroid-0': np.zeros(n),
        'centroid-1': np.arange(n, dtype=float),
        't': np.zeros(n),
    }, index=pd.Index(range(1, n + 1), name='label'))
    if with_pass:
        df['pass'] = ['0', '0', '1', None]
    return df


class PassColumnTest(unittest.TestCase):
    def setUp(self):
        import tempfile
        self._tmp = tempfile.TemporaryDirectory()
        self.seg = _Stub(self._tmp.name)

    def tearDown(self):
        self._tmp.cleanup()

    def _write(self, df):
        import anndata as ad
        return ad.read_h5ad(self.seg._to_anndata(df, is_3d=False, n_t=1))

    def test_pass_lands_in_obs(self):
        a = self._write(_frame())
        self.assertIn('pass', a.obs.columns)
        self.assertEqual(list(a.obs['pass'].astype(str))[:3], ['0', '0', '1'])

    def test_pass_never_lands_in_x(self):
        a = self._write(_frame())
        self.assertNotIn('pass', list(a.var.index),
                         'a model-group key must not become a measured feature')
        self.assertIn('area', list(a.var.index), 'the sweep measured nothing at all')

    def test_pass_is_categorical(self):
        a = self._write(_frame())
        self.assertEqual(a.obs['pass'].dtype.name, 'category')

    def test_an_unclaimed_label_is_named_not_dropped(self):
        """An id no recorded range covers means the store predates pass recording. "unknown" is the
        honest value; dropping the row would silently shorten the table against its own labels."""
        a = self._write(_frame())
        self.assertEqual(a.n_obs, 4)
        self.assertEqual(str(a.obs['pass'].iloc[3]), 'unknown')

    def test_a_single_pass_table_carries_no_column(self):
        """`read_label_passes` returns [] for one pass, and an all-one-value column would be noise in
        every gating dropdown. Absent must stay distinguishable from present."""
        a = self._write(_frame(with_pass=False))
        self.assertNotIn('pass', a.obs.columns)
        self.assertNotIn('pass', list(a.var.index))

    def test_the_other_special_columns_still_go_where_they_did(self):
        """The guard on the change: `pass` was added to the same exclusion list as spatial/temporal,
        which is easy to get wrong in a way that quietly moves a centroid into X."""
        a = self._write(_frame())
        self.assertIn('spatial', a.obsm)
        self.assertIn('temporal', a.obsm)
        self.assertNotIn('t', list(a.var.index))
        self.assertEqual([c for c in a.var.index if c.startswith('centroid')], [])


class MeasureFromZarrStampsThePassTest(unittest.TestCase):
    """The middle link: `measure_from_zarr` must stamp the pass while the label index still exists.

    `pd.concat(all_dfs, ignore_index=True)` discards the index, and the index IS the label id — so a
    column added after the concat cannot be attributed and one added before survives. Easy to get
    backwards, and silent either way: the table comes out the right length regardless.
    """

    @staticmethod
    def _dim_utils(shape):
        """Distinct axis sizes and real `<Channel>` entries: `calc_image_dimensions` matches the
        array shape to the OME sizes by VALUE, so a fixture whose axes share a size is ambiguous and
        raises. Hence T=2, C=2, Y=10, X=8 — the shape of the working fixture in
        `test_segmentation_streaming`, for the same reason."""
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        t, z, c, y, x = shape
        chans = ''.join(f'<Channel ID="Channel:0:{i}" SamplesPerPixel="1"/>' for i in range(c))
        xml = f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{t}" SizeZ="{z}" SizeC="{c}" SizeY="{y}" SizeX="{x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5"
            PhysicalSizeYUnit="µm" PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      {chans}
    </Pixels>
  </Image>
</OME>"""
        du = DimUtils(ome_types.from_xml(xml), use_channel_axis=True)
        du.calc_image_dimensions((t, c, y, x))
        return du

    def test_the_column_is_attributed_to_the_right_labels(self):
        import tempfile
        import anndata as ad

        class _Log:
            def log(self, *_a, **_k): pass
            def progress(self, *_a, **_k): pass

        # one timepoint, two objects: label 1 from pass '0', label 2 from pass '1'
        # T=2 (a singleton T is dropped from the shape order), and Y != X so the shape is
        # unambiguous. The second timepoint is empty and skipped, which the loop already handles.
        labels = np.zeros((2, 10, 8), dtype=np.uint32)
        labels[0, 1:3, 1:3] = 1
        labels[0, 6:8, 5:7] = 2
        image = np.full((2, 2, 10, 8), 100, dtype=np.uint16)
        entries = [{'group': '0', 'from': 1, 'to': 1}, {'group': '1', 'from': 2, 'to': 2}]

        with tempfile.TemporaryDirectory() as tmp:
            mu = MeasureUtils({'taskDir': tmp, 'outputValueName': 'twopass'},
                              self._dim_utils((2, 1, 2, 10, 8)))
            out = mu.measure_from_zarr({'base': [labels]}, [image], _Log(),
                                       label_passes=entries)
            self.assertIsNotNone(out)
            a = ad.read_h5ad(out)

        self.assertEqual(a.n_obs, 2)
        # obs index is positional, so pair the pass with the AREA-ordered rows instead: both objects
        # are 2x2, so use the centroid, which is the only thing that distinguishes them.
        by_pass = dict(zip(a.obs['pass'].astype(str), a.obsm['spatial'][:, 0]))
        self.assertEqual(set(by_pass), {'0', '1'},
                         'both passes must be named in the measured table')
        self.assertLess(by_pass['0'], by_pass['1'],
                        'pass 0 owns label 1, the object nearer the origin')


if __name__ == '__main__':
    unittest.main()
