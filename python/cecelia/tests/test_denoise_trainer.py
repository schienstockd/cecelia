"""Smoke test for the denoise TRAINER — `app/src/tasks/opticalFlow/train_support_denoise_run.py`.

Runs the trainer end-to-end against a fake 12-frame single-Z OME-ZARR for 1 epoch on a tiny UNet
and asserts:
  * the `.pt` lands at its final path (atomic_path only renames on clean exit),
  * the `.json` manifest lands beside it with the fields the denoise runner reads (`arch.inputFrames`,
    `arch.midChannels`, etc.) and the ones the QC picks up (`kind`, `training`),
  * the QC sidecar carries the loss curve.

Skipped when `app/` is absent or torch/coastal.support aren't importable.
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
           / 'app' / 'src' / 'tasks' / 'opticalFlow' / 'train_support_denoise_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('train_support_denoise_run', _RUNNER)
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
    import torch  # noqa: F401
    from coastal.support import train_support  # noqa: F401
    _HAS_TORCH = True
except Exception:
    _HAS_TORCH = False


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
@unittest.skipUnless(_HAS_TORCH, 'torch + coastal.support required')
class SupportTrainerSmokeTest(unittest.TestCase):
    SHAPE = dict(size_t=12, size_z=1, size_c=2, size_y=32, size_x=32)

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

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

    def test_trainer_writes_pt_and_manifest_and_qc(self):
        model_path = os.path.join(self.dir, 'vault', 'tiny.pt')
        qc_path = os.path.join(self.dir, 'support_training.json')
        os.makedirs(os.path.dirname(model_path), exist_ok=True)

        self.runner.run({
            'movies': [{'uID': 'test', 'imPath': self.in_path}],
            'taskDir': self.dir,
            'modelPath': model_path,
            'qcOutPath': qc_path,
            'valueName': 'default',
            # Two channels pooled into one model — DENOISE_INTEGRATION_PLAN.md D3 amendment.
            # Both go into a single training run; the manifest records the list.
            'trainChannels': [0, 1],
            'channelNames': ['CH1', 'CH2'],
            'inputFrames': 5,
            'patchXY': 16,
            'epochs': 1,
            'batchSize': 2,
            'learningRate': 5e-4,
            'midChannels': [8, 16, 32],
            'depth': 3,
            'blindConvChannels': 4,
            'midZOnly': True,
        })

        # .pt landed at its final path (atomic_path only renames on clean exit)
        self.assertTrue(os.path.isfile(model_path))

        # Manifest sits beside it with the fields the denoise runner reads back
        manifest_path = os.path.splitext(model_path)[0] + '.json'
        self.assertTrue(os.path.isfile(manifest_path))
        with open(manifest_path, encoding='utf-8') as f:
            manifest = json.load(f)
        self.assertEqual(manifest['kind'], 'denoise-support')
        # The manifest carries the FULL pooled channel list — the vault label reads this.
        self.assertEqual(manifest['channels'], ['CH1', 'CH2'])
        arch = manifest['arch']
        for k in ('inputFrames', 'patchXY', 'midChannels', 'depth', 'blindConvChannels',
                  'oneByOneChannels', 'lastLayerChannels', 'bsSize', 'bp'):
            self.assertIn(k, arch, f'arch missing {k} — coastal.support.build_model reads it')
        self.assertEqual(arch['inputFrames'], 5)
        self.assertEqual(arch['midChannels'], [8, 16, 32])
        self.assertEqual(manifest['training']['imageUids'], ['test'])
        self.assertEqual(manifest['training']['channelIndices'], [0, 1])

        # QC sidecar carries the loss curve for _support_train_qc_findings
        with open(qc_path, encoding='utf-8') as f:
            qc = json.load(f)
        for k in ('finalLoss', 'firstLoss', 'lossDrop', 'epochLosses', 'epochs', 'nImages'):
            self.assertIn(k, qc, f'QC missing {k}')
        self.assertEqual(qc['epochs'], 1)
        self.assertEqual(qc['nImages'], 1)
        self.assertEqual(len(qc['epochLosses']), 1)

    def test_trainer_refuses_short_movies(self):
        """A movie with fewer than `input_frames` timepoints can't produce a centred window; the
        trainer must skip it and, if that leaves nothing, error out cleanly rather than crashing."""
        model_path = os.path.join(self.dir, 'vault', 'tiny.pt')
        os.makedirs(os.path.dirname(model_path), exist_ok=True)
        with self.assertRaises(SystemExit):
            self.runner.run({
                'movies': [{'uID': 'test', 'imPath': self.in_path}],
                'taskDir': self.dir,
                'modelPath': model_path,
                'qcOutPath': None,
                'valueName': 'default',
                'trainChannels': [0],
                'channelNames': ['CH1'],
                'inputFrames': 999,   # bigger than SHAPE.size_t
                'patchXY': 16,
                'epochs': 1,
                'batchSize': 2,
                'learningRate': 5e-4,
                'midChannels': [8, 16],
                'depth': 2,
                'blindConvChannels': 4,
                'midZOnly': True,
            })
        # nothing landed in the vault
        self.assertFalse(os.path.isfile(model_path))


if __name__ == '__main__':
    unittest.main()
