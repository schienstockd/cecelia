"""End-to-end test of the OME-TIFF export RUNNER — `app/src/tasks/exportImages/ome_tiff_run.py`.

**What actually has to be true.** The export exists because a figure gets rendered in Imaris, and the
route it replaces (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter) *lost the pixel sizes* —
a plain TIFF has nowhere to record Z spacing. So "the file was written" is not the property worth
testing. The property is that the `<Pixels>` block carries PhysicalSizeX/Y/Z **with units**, the
frame interval and the channel names, and that the pixels themselves are the ones asked for.

`PhysicalSizeZ` gets its own assertion in every shape below: it is the exact field the old workflow
dropped, and it is the one a reader can silently default rather than error on.

The runner is executed by path (`run_py`), never imported, so nothing else in the suite touches it.
Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import importlib.util
import json
import os
import re
import shutil
import tempfile
import unittest
from pathlib import Path

import dask.array as da
import numpy as np
import ome_types
import tifffile

import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'exportImages' / 'ome_tiff_run.py')

SIZE_T, SIZE_C, SIZE_Z, SIZE_Y, SIZE_X = 3, 2, 4, 12, 10


def _load_runner():
    """Load the runner from its path, exactly as `run_py` does (it is not an importable module)."""
    spec = importlib.util.spec_from_file_location('ome_tiff_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_OME_XML = f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{SIZE_T}" SizeC="{SIZE_C}" SizeZ="{SIZE_Z}" SizeY="{SIZE_Y}" SizeX="{SIZE_X}"
            PhysicalSizeX="0.325" PhysicalSizeY="0.325" PhysicalSizeZ="2.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" Name="DAPI" SamplesPerPixel="1"/>
      <Channel ID="Channel:0:1" Name="GFP" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""

# The calibration the Julia handler reads off ccid.json and passes IN. Deliberately NOT the same
# numbers as the store's own OME-XML above: ccid.json is the authoritative copy, so if the runner
# ever re-derived calibration from the store instead of using what it was handed, these assertions
# would catch it.
_CAL = {
    'PhysicalSizeX': 0.111, 'PhysicalSizeXUnit': 'µm',
    'PhysicalSizeY': 0.111, 'PhysicalSizeYUnit': 'µm',
    'PhysicalSizeZ': 3.5,   'PhysicalSizeZUnit': 'µm',
    'TimeIncrement': 10.0,  'TimeIncrementUnit': 's',
}


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class ExportOmeTiffRunnerTest(unittest.TestCase):

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        shape = [SIZE_T, SIZE_C, SIZE_Z, SIZE_Y, SIZE_X]
        du = DimUtils(ome_types.from_xml(_OME_XML), use_channel_axis=True)
        du.calc_image_dimensions(shape)

        # Every voxel encodes its own (t, c, z) so a wrong axis order, a dropped frame or a
        # mis-sliced channel shows up as a value mismatch rather than a shape that happens to fit.
        self.data = np.zeros(shape, dtype=np.uint16)
        for t in range(SIZE_T):
            for c in range(SIZE_C):
                for z in range(SIZE_Z):
                    self.data[t, c, z] = t * 100 + c * 10 + z

        self.im_path = os.path.join(self.dir, 'img.ome.zarr')
        src = da.from_array(self.data, chunks=(1, 1, 1, SIZE_Y, SIZE_X))
        zarr_utils.create_multiscales(src, self.im_path, dim_utils=du, nscales=1)
        ome_dir = os.path.join(self.im_path, 'OME')
        os.makedirs(ome_dir, exist_ok=True)
        with open(os.path.join(ome_dir, 'METADATA.ome.xml'), 'w', encoding='utf-8') as f:
            f.write(_OME_XML)

    # ── helpers ──────────────────────────────────────────────────────────────────────────────────

    def _export(self, **over):
        out_path = os.path.join(self.dir, over.pop('name', 'out') + '.ome.tif')
        qc_path = os.path.join(self.dir, 'qc.json')
        params = dict(imPath=self.im_path, outPath=out_path, channels=[],
                      channelNames=['DAPI', 'GFP'], zMip=False, timepoint=-1,
                      calibration=dict(_CAL), qcOutPath=qc_path, taskDir=self.dir)
        params.update(over)
        self.runner.run(params)
        return out_path, qc_path

    @staticmethod
    def _pixels_attrs(path):
        with tifffile.TiffFile(path) as tf:
            xml = tf.ome_metadata
        block = re.search(r'<Pixels[^>]*>', xml).group(0)
        return dict(re.findall(r'(\w+)="([^"]*)"', block)), re.findall(
            r'<Channel[^>]*Name="([^"]*)"', xml)

    # ── the calibration, which is the whole point ────────────────────────────────────────────────

    def test_full_export_carries_the_calibration_it_was_handed(self):
        out, _ = self._export()
        attrs, names = self._pixels_attrs(out)
        # Z first — the field the ImageJ hop dropped, and the reason this task exists.
        self.assertEqual(attrs['PhysicalSizeZ'], '3.5')
        self.assertEqual(attrs['PhysicalSizeZUnit'], 'µm')
        self.assertEqual((attrs['PhysicalSizeX'], attrs['PhysicalSizeY']), ('0.111', '0.111'))
        self.assertEqual(attrs['PhysicalSizeXUnit'], 'µm')
        self.assertEqual((attrs['TimeIncrement'], attrs['TimeIncrementUnit']), ('10.0', 's'))
        self.assertEqual(names, ['DAPI', 'GFP'])

    def test_calibration_comes_from_the_handler_not_the_store(self):
        """ccid.json is authoritative; a runner that re-read the store's OME-XML would write 0.325."""
        out, _ = self._export()
        attrs, _ = self._pixels_attrs(out)
        self.assertEqual(attrs['PhysicalSizeX'], '0.111')
        self.assertNotEqual(attrs['PhysicalSizeX'], '0.325')

    def test_missing_calibration_writes_no_physical_size_rather_than_one(self):
        """A source with no pixel size must yield an absent PhysicalSize, not a fabricated 1.0 —
        'unknown' and 'one micron' are very different claims to make to Imaris."""
        out, _ = self._export(calibration={}, name='nocal')
        attrs, _ = self._pixels_attrs(out)
        for k in ('PhysicalSizeX', 'PhysicalSizeY', 'PhysicalSizeZ'):
            self.assertNotIn(k, attrs)

    # ── the pixels ───────────────────────────────────────────────────────────────────────────────

    def test_full_export_round_trips_every_voxel(self):
        out, qc = self._export()
        back = tifffile.imread(out)
        self.assertEqual(back.shape, (SIZE_T, SIZE_C, SIZE_Z, SIZE_Y, SIZE_X))
        self.assertTrue(np.array_equal(back, self.data))
        with open(qc, encoding='utf-8') as f:
            meta = json.load(f)
        self.assertEqual(meta['axes'], 'TCZYX')
        self.assertEqual(meta['planes'], SIZE_T * SIZE_C * SIZE_Z)
        self.assertEqual(meta['sizeC'], SIZE_C)

    def test_channel_subset_keeps_order_and_names(self):
        """One channel still declares SizeC=1 with its name, so the channel identity survives.

        `tifffile.imread` squeezes singleton axes on the way back, so the shape it returns is not
        evidence about what was written — the OME-XML is.
        """
        out, qc = self._export(channels=[1], channelNames=['GFP'], name='sub')
        back = tifffile.imread(out)
        self.assertTrue(np.array_equal(back, self.data[:, 1]))
        attrs, names = self._pixels_attrs(out)
        self.assertEqual(attrs['SizeC'], '1')
        self.assertEqual(names, ['GFP'])

    def test_single_timepoint_drops_the_t_axis(self):
        out, _ = self._export(timepoint=1, name='one_t')
        back = tifffile.imread(out)
        self.assertEqual(back.shape, (SIZE_C, SIZE_Z, SIZE_Y, SIZE_X))
        self.assertTrue(np.array_equal(back, self.data[1]))

    def test_z_mip_collapses_z_and_takes_the_maximum(self):
        out, qc = self._export(zMip=True, name='mip')
        back = tifffile.imread(out)
        self.assertEqual(back.shape, (SIZE_T, SIZE_C, SIZE_Y, SIZE_X))
        self.assertTrue(np.array_equal(back, self.data.max(axis=2)))
        with open(qc, encoding='utf-8') as f:
            self.assertEqual(json.load(f)['sizeZ'], 1)

    # ── failure behaviour ────────────────────────────────────────────────────────────────────────

    def test_out_of_range_selection_is_refused_without_writing(self):
        """A bad channel/timepoint must not leave a half-file behind for the user to find."""
        for over in (dict(channels=[9], name='badc'), dict(timepoint=99, name='badt')):
            out, _ = self._export(**over)
            self.assertFalse(os.path.exists(out))
            self.assertFalse(os.path.exists(out + '.partial'))

    def test_nothing_partial_is_left_on_success(self):
        out, _ = self._export(name='clean')
        self.assertTrue(os.path.exists(out))
        self.assertFalse(os.path.exists(out + '.partial'))
