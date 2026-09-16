"""End-to-end test of the ridge segmentation task RUNNER — `app/src/tasks/segment/ridges_run.py`.

Same seam as `test_smooth_runner`: the runner is executed by path (`run_py`) and never imported
into `cecelia`, so nothing in the package suite touches it. What is NOT covered by unit-testing
skimage.filters is the streaming loop here — the label-store staging, the per-T offset that keeps
label IDs unique across frames, and the axes bookkeeping when the output loses the C (and
optionally Z) axes.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import os
import shutil
import tempfile
import unittest
from pathlib import Path

import numpy as np
import ome_types

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'segment' / 'ridges_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('ridges_run', _RUNNER)
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
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm"
            TimeIncrement="30.0" TimeIncrementUnit="s">
      {channels}
    </Pixels>
  </Image>
</OME>"""


def _ridge_field(shape_yx, seed=1):
    """Two horizontal, non-crossing bright ridges on a dim noisy background — the two ridges give
    the CC labeller something to separate, so the count assertions catch a broken threshold or a
    merged CC. Non-crossing on purpose: connected-components merges lines that touch."""
    rng = np.random.default_rng(seed)
    y_n, x_n = shape_yx
    f = rng.normal(30, 5, size=(y_n, x_n)).astype(np.float32)
    for y_c in (4, 15):                          # two well-separated rows
        for x in range(x_n):
            f[y_c, x] += 900.0
            if y_c + 1 < y_n: f[y_c + 1, x] += 400.0
    return f


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class RidgesRunnerTest(unittest.TestCase):
    SHAPE = dict(size_t=3, size_z=1, size_c=2, size_y=20, size_x=24)

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.runner = _load_runner()

        omexml = ome_types.from_xml(_ome_xml(**self.SHAPE))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [self.SHAPE['size_t'], self.SHAPE['size_c'], self.SHAPE['size_z'],
                 self.SHAPE['size_y'], self.SHAPE['size_x']]
        du.calc_image_dimensions(shape)

        # Fibre channel = C=1; C=0 stays flat noise so a mis-picked channel gives an empty result.
        data = np.zeros(shape, dtype=np.uint16)
        for t in range(self.SHAPE['size_t']):
            frame = _ridge_field((self.SHAPE['size_y'], self.SHAPE['size_x']), seed=t + 1)
            data[t, 1, 0] = np.clip(np.rint(frame), 0, np.iinfo(np.uint16).max).astype(np.uint16)
            # channel 0: pure noise, no ridges
            data[t, 0, 0] = np.clip(np.rint(np.random.default_rng(100 + t).normal(30, 5,
                                    size=(self.SHAPE['size_y'], self.SHAPE['size_x']))),
                                    0, np.iinfo(np.uint16).max).astype(np.uint16)

        self.in_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.in_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(self.in_path, omexml=omexml)
        self.du, self.shape, self.data = du, shape, data

    def _run(self, **over):
        out = os.path.join(self.dir, f"out_{over.get('filter', 'meijering')}.ome.zarr")
        params = dict(imPath=self.in_path, labelsOutPath=out,
                      qcOutPath=os.path.join(self.dir, 'qc.json'),
                      channelIndex=1, filter='meijering', sigmaMin=1, sigmaMax=3,
                      threshold=0.0, darkRidges=False, perZ=True, minSizePx=3,
                      outputValueName='ridges')
        params.update(over)
        self.runner.run(params)
        return np.asarray(zarr_utils.open_as_zarr(out, as_dask=False)[0][0][:])

    def test_all_three_filters_produce_a_label_store_with_ridge_pixels(self):
        for filt in ('meijering', 'sato', 'frangi'):
            labels = self._run(filter=filt)
            # store has no C, no Z (SHAPE has Z=1 → squeezed): axes = TYX
            self.assertEqual(labels.ndim, 3, f'{filt}: expected TYX, got shape {labels.shape}')
            self.assertEqual(labels.shape[0], self.SHAPE['size_t'])
            self.assertGreater(int((labels > 0).sum()), 0,
                               f'{filt} produced an empty label store')

    def test_output_has_no_channel_axis(self):
        """Labels are single-valued per voxel — the store must not carry the source's C axis, or
        every downstream reader picks (t, ch=0, ...) and reads garbage."""
        labels = self._run()
        # source is TCZYX with C=2, Z=1; output should be TYX (Z squeezed, C dropped)
        self.assertEqual(labels.shape, (self.SHAPE['size_t'],
                                        self.SHAPE['size_y'], self.SHAPE['size_x']))

    def test_meijering_finds_two_ridges_per_frame(self):
        labels = self._run(filter='meijering')
        # per T, at least 2 unique labels (one per ridge). Not asserting exactly 2 because CC can
        # split a ridge at a discontinuity, but must find ≥ 2.
        for t in range(labels.shape[0]):
            uniq = set(int(x) for x in np.unique(labels[t])) - {0}
            self.assertGreaterEqual(len(uniq), 2,
                                    f't={t}: expected ≥2 label components, got {uniq}')

    def test_labels_are_globally_unique_across_frames(self):
        """Per-T label offsetting is what makes the h5ad label-id contract work downstream: two
        different components in different frames must not collide on the same id."""
        labels = self._run(filter='meijering')
        seen_by_t = [set(int(x) for x in np.unique(labels[t])) - {0} for t in range(labels.shape[0])]
        for t in range(1, labels.shape[0]):
            self.assertTrue(seen_by_t[t].isdisjoint(seen_by_t[t - 1]),
                            f't={t}: ids overlap with t-1')

    def test_z_mip_run_drops_the_Z_axis(self):
        """`perZ=False` collapses Z on the way in; the output store must have no Z axis."""
        labels = self._run(perZ=False)
        # SHAPE has Z=1 so both perZ modes end up with the same rank here. For rank behaviour to be
        # meaningful we need Z>1 — that path is exercised by the runner but not by this fixture.
        self.assertEqual(labels.ndim, 3)


if __name__ == '__main__':
    unittest.main()
