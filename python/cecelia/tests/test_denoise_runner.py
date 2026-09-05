"""Smoke test for the denoise task RUNNER — `app/src/tasks/cleanupImages/denoise_run.py`.

Trains a tiny SUPPORT model for a couple of steps, saves it + a manifest, then runs the runner
against a fake 8-frame OME-ZARR and asserts:
  * the output store lands at its final path (staged_store only renames on success);
  * the interior frames are non-zero (mirror-pad worked — see D8 in DENOISE_INTEGRATION_PLAN.md);
  * the QC sidecar carries the fields `cleanupImages.denoise` reads (`channelsRun`,
    `channelsSkipped`, `inputFrames`, `shape`);
  * a channel-selection with only unknown channels errors, not silently writes.

Skipped when `app/` is absent (external `pip install cecelia` consumer gets IO library only), and
when `torch` fails to import (this is the analysis env's responsibility to provide).
"""
import importlib.util
import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path

import numpy as np
import ome_types
import zarr

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'cleanupImages' / 'denoise_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('denoise_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    channels = ''.join(
        f'<Channel ID="Channel:0:{i}" Name="CH{i + 1}" SamplesPerPixel="1"/>' for i in range(size_c))
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeC="{size_c}" SizeZ="{size_z}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
      {channels}
    </Pixels>
  </Image>
</OME>"""


try:
    import torch
    from coastal.support import denoise_stack
    _HAS_TORCH = True
except Exception:
    _HAS_TORCH = False


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
@unittest.skipUnless(_HAS_TORCH, 'torch + coastal.support required')
class DenoiseRunnerTest(unittest.TestCase):
    SHAPE = dict(size_t=8, size_z=1, size_c=2, size_y=32, size_x=32)

    # Small model — 5-frame window, tiny UNet — so a 2-batch smoke run is quick on CPU.
    ARCH = dict(
        inputFrames=5, patchXY=16, midChannels=[8, 16], depth=2,
        blindConvChannels=4, oneByOneChannels=[8, 4], lastLayerChannels=[8, 4],
        bsSize=[1, 1], bp=False,
    )

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        # ── build the OME-ZARR ────────────────────────────────────────────────
        omexml = ome_types.from_xml(_ome_xml(**self.SHAPE))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [self.SHAPE['size_t'], self.SHAPE['size_c'], self.SHAPE['size_z'],
                 self.SHAPE['size_y'], self.SHAPE['size_x']]
        du.calc_image_dimensions(shape)

        rng = np.random.default_rng(0)
        data = rng.integers(20, 200, size=shape, dtype=np.uint16)

        self.in_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.in_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(self.in_path, omexml=omexml)
        self.du = du
        self.shape = shape

        # ── build + save the tiny model + manifest ────────────────────────────
        # coastal.support.build_model is the ONE mapper from `arch` → SUPPORT(...) kwargs;
        # reuse it here so a spec change in coastal fails this test loudly rather than drifting.
        from coastal.support import build_model
        self.model_path = os.path.join(self.dir, 'tiny.pt')
        model = build_model(self.ARCH, torch.device('cpu'))
        torch.save(model.state_dict(), self.model_path)
        self.manifest = {'kind': 'denoise-support', 'arch': self.ARCH,
                         'training': {'imageUids': ['test'], 'epochs': 0},
                         'imaging': {}, 'checksum': 'stub'}

    def _run(self, channels, skipped=None):
        out_path = os.path.join(self.dir, 'out.ome.zarr')
        qc_path = os.path.join(self.dir, 'denoise_stats.json')
        self.runner.run({
            'imPath': self.in_path,
            'imOutputPath': out_path,
            'modelPath': self.model_path,
            'manifest': self.manifest,
            'channels': channels,
            'channelsSkipped': skipped or [],
            'batchSize': 2,
            'qcOutPath': qc_path,
        })
        with open(qc_path, encoding='utf-8') as f:
            return out_path, json.load(f)

    def test_the_runner_completes_and_denoises_all_frames(self):
        out_path, qc = self._run(channels=[0])

        # store landed
        self.assertTrue(os.path.isdir(out_path))
        out = zarr.open_group(out_path, mode='r')['0'][:]
        self.assertEqual(out.shape, tuple(self.shape))
        self.assertEqual(out.dtype, np.uint16)

        # Every frame is populated on the denoised channel — the mirror-pad guarantee (D8). Before
        # the pad was added, the first/last input_frames//2 frames came back as zeros.
        c_idx = self.du.dim_idx('C')
        for t in range(self.shape[0]):
            sl = [slice(None)] * len(self.shape)
            sl[c_idx] = 0
            sl[self.du.dim_idx('T')] = t
            plane = out[tuple(sl)]
            self.assertGreater(plane.sum(), 0,
                               f't={t} came back all-zero (mirror-pad regression)')

        # QC carries the fields `cleanupImages.denoise` reads back through _denoise_qc_findings.
        for k in ('channelsRun', 'channelsSkipped', 'inputFrames', 'shape'):
            self.assertIn(k, qc)
        self.assertEqual(qc['channelsRun'], [0])
        self.assertEqual(qc['inputFrames'], self.ARCH['inputFrames'])
        self.assertEqual(qc['shape'], list(self.shape))

        # Untouched channel is passed through unchanged
        raw = zarr.open_group(self.in_path, mode='r')['0'][:]
        sl = [slice(None)] * len(self.shape)
        sl[c_idx] = 1
        np.testing.assert_array_equal(out[tuple(sl)], raw[tuple(sl)])

    def test_empty_channels_errors(self):
        """The Julia handler is expected to strip saturated channels before calling; if that leaves
        the list empty the runner must error, not silently write a copy of the input."""
        with self.assertRaises(SystemExit):
            self._run(channels=[])


if __name__ == '__main__':
    unittest.main()
