"""`DimUtils.calc_image_dimensions` on a store whose OME-XML `DimensionOrder` doesn't describe it.

A bioformats2raw store is always TCZYX; its OME-XML may keep the source order (e.g. XYCZT). Matching
the OME sizes to the shape got it wrong two ways: the fallback applied the permutation backwards
(XYCZT on TCZYX came out T,Z,Y,C,X even with every size distinct), and equal T and Z sizes can't be
told apart by size at all. The store's own NGFF axes (`axes=zarr_utils.read_axes(path)`) decide.

Each consumer the order feeds is driven once with T == Z: segmentation's strided normalisation
(`_time_steps`, which the preview uses), stack alignment, and the T-projection runner.
"""
import importlib.util
import os
import re
import shutil
import tempfile
import types
import unittest
from pathlib import Path

import numpy as np
import ome_types

import cecelia.utils.correction_utils as cu
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils

_ROOT = Path(__file__).resolve().parents[3]
_TPROJECT = _ROOT / 'app' / 'src' / 'tasks' / 'editImages' / 'tProject_run.py'
_TCZYX = ['t', 'c', 'z', 'y', 'x']


def _ome(order, t, c, z, y, x):
    return ome_types.from_xml(f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="{order}" Type="uint16" SizeT="{t}" SizeC="{c}" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}" PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5"
    PhysicalSizeYUnit="µm" PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    {''.join(f'<Channel ID="Channel:0:{i}" SamplesPerPixel="1"/>' for i in range(c))}
    <MetadataOnly/></Pixels></Image></OME>""")


def _du(order, shape, axes=None):
    du = DimUtils(_ome(order, *shape), use_channel_axis=True)
    if axes is None:
        du.calc_image_dimensions(shape)
    else:
        du.calc_image_dimensions(shape, axes=axes)
    return du


class CalcImageDimensionsTest(unittest.TestCase):
    def test_distinct_sizes_resolve_without_store_axes(self):
        du = _du('XYCZT', (4, 2, 5, 17, 16))
        self.assertEqual(du.im_dim_order, list('TCZYX'))
        self.assertEqual(du.im_dim, [4, 2, 5, 17, 16])

    def test_store_axes_decide_when_t_and_z_tie(self):
        du = _du('XYCZT', (4, 2, 4, 17, 16), axes=_TCZYX)
        self.assertEqual(du.im_dim_order, list('TCZYX'))
        self.assertEqual((du.dim_idx('T'), du.dim_idx('Z')), (0, 2))

    def test_unusable_store_axes_fall_back_to_the_ome_xml(self):
        for axes in (None, ['y', 'x'], ['t', 'c', 'z', 'y', 'q'], ['t', 't', 'z', 'y', 'x']):
            self.assertEqual(_du('XYZCT', (4, 2, 5, 17, 16), axes=axes).im_dim_order, list('TCZYX'))


class SegmentationTimeStrideTest(unittest.TestCase):
    def test_preview_stride_lands_on_t(self):
        shape = (8, 1, 8, 16, 16)
        du = _du('XYCZT', shape, axes=_TCZYX)
        steps = SegmentationUtils._time_steps(types.SimpleNamespace(dim_utils=du), shape, 4)
        self.assertEqual(steps, {0: 2})


class StackAlignmentAxesTest(unittest.TestCase):
    def test_shift_applies_to_the_right_plane(self):
        t = z = 3
        rng = np.random.default_rng(0)
        arr = rng.integers(100, 1000, size=(t, 1, z, 24, 24)).astype(np.float32)
        du = _du('XYCZT', arr.shape, axes=_TCZYX)
        shifts = np.zeros((t, z, 2), np.float32)
        shifts[0, 2] = (3, 0)
        align = cu.StackAlignment(shifts=shifts, confidence=np.ones((t, z), np.float32),
                                  applied=np.ones((t, z), bool), ref_idx=np.zeros(t, np.int32))
        out = cu.apply_stack_alignment(arr, align, du)
        changed = {(ti, zi) for ti in range(t) for zi in range(z)
                   if not np.array_equal(out[ti, 0, zi], arr[ti, 0, zi])}
        self.assertEqual(changed, {(0, 2)})


@unittest.skipUnless(_TPROJECT.is_file(), f'runner not present at {_TPROJECT}')
class TProjectAxesTest(unittest.TestCase):
    def test_projects_along_t_when_t_and_z_tie(self):
        d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, d, ignore_errors=True)
        shape = (3, 1, 3, 8, 8)
        im = np.zeros(shape, np.uint16)
        for ti in range(3):
            for zi in range(3):
                im[ti, 0, zi] = 100 * ti + zi
        writer = DimUtils(_ome('XYCZT', *shape), use_channel_axis=True)
        writer.calc_image_dimensions(shape, axes=_TCZYX)
        im_path = os.path.join(d, 'im.ome.zarr')
        g, lv0, pchunks = zarr_utils.open_multiscales_for_writing(im_path, shape, np.uint16, writer,
                                                                  nscales=1)
        lv0[:] = im
        ome_xml_utils.save_meta_in_zarr(im_path, omexml=_ome('XYCZT', *shape))

        spec = importlib.util.spec_from_file_location('tProject_run', _TPROJECT)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        out_path = os.path.join(d, 'out.ome.zarr')
        mod.run(dict(imPath=im_path, imOutPath=out_path, op='max'))

        out = np.asarray(zarr_utils.open_as_zarr(out_path, as_dask=False)[0][0][:])
        self.assertEqual(out.shape, (1, 1, 3, 8, 8))
        np.testing.assert_array_equal(out[0, 0, :, 0, 0], [200, 201, 202])


class StoreAxesRatchetTest(unittest.TestCase):
    """Every `calc_image_dimensions` over an image store passes the store's axes (or an explicit
    `im_dim_dict`); without them a T/Z tie is resolved by guesswork."""

    def test_every_call_site_passes_axes(self):
        roots = [_ROOT / 'app' / 'src', _ROOT / 'preview', _ROOT / 'python' / 'cecelia' / 'utils']
        missing = []
        for root in roots:
            for p in root.rglob('*.py'):
                if p.name == 'dim_utils.py':
                    continue
                for n, line in enumerate(p.read_text(encoding='utf-8').splitlines(), 1):
                    if re.search(r'\.calc_image_dimensions\(', line) and not line.lstrip().startswith('#') \
                            and 'axes=' not in line and 'im_dim_dict=' not in line:
                        missing.append(f'{p.relative_to(_ROOT)}:{n}')
        self.assertEqual(missing, [])


if __name__ == '__main__':
    unittest.main()
