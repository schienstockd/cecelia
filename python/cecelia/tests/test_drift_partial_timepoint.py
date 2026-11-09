"""Drift correction on a movie whose last timepoint the acquisition never finished.

The import records such a timepoint in ccid `meta["partialTimepoint"]` and, by default, trims it. When
the user keeps it (`keepPartialTimepoint`, recorded `trimmed = false`), its missing z-planes are zeros,
and registering them reads as movement: on a still movie with a half-empty last frame the estimator
reported a ~24 px excursion that was never there, and smeared it back over the complete frames
through the trajectory smoother. So drift is estimated on the complete frames only, and the kept
frame is placed with the last complete one — it stays in the output, the user kept it.

`estimate_drift(measured_frames=)` is the estimator half; the runner test is the wire from the
`partialTimepoint` param the Julia task passes (`kept_partial_timepoint`, drift_correct.jl).

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
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
from scipy import ndimage as ndi

import cecelia.utils.correction_utils as cu
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils

try:
    import SimpleITK as _sitk_probe          # noqa: F401  (see test_drift_estimate.py)
    _SITK_LOADS = True
except ImportError:
    _SITK_LOADS = False

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'cleanupImages' / 'drift_correct_run.py')

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{t}" SizeC="1" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" Name="CH1" SamplesPerPixel="1"/>{tail}</Pixels></Image></OME>"""

_N_T, _N_Z, _SIZE = 6, 4, 64


def _still_movie_stopped_mid_volume():
    """A still [T,C,Z,Y,X] movie whose last frame stopped after its first plane — the case the import
    probe detects. Planes differ in z, so the zeroed deep planes are a real mismatch to register."""
    rng = np.random.default_rng(2)
    base = ndi.gaussian_filter(rng.random((_N_Z, _SIZE, _SIZE)) * 3000, (0, 2, 2)) + 100
    arr = np.stack([base] * _N_T).reshape(_N_T, 1, _N_Z, _SIZE, _SIZE).astype(np.uint16)
    arr[-1, :, 1:] = 0
    return arr


def _xml(tail='<MetadataOnly/>'):
    return _OME.format(t=_N_T, z=_N_Z, y=_SIZE, x=_SIZE, tail=tail)


def _dim_utils(arr):
    du = DimUtils(ome_types.from_xml(_xml()), use_channel_axis=True)
    du.calc_image_dimensions(arr.shape)
    return du


def _excursion(positions):
    p = np.asarray(positions, dtype=float)
    return float(np.ptp(p.reshape(len(p), -1), axis=0).max())


class EstimateDriftOnCompleteFramesTest(unittest.TestCase):
    def setUp(self):
        self.arr = _still_movie_stopped_mid_volume()
        self.du = _dim_utils(self.arr)

    def test_the_half_empty_frame_reads_as_movement_when_registered(self):
        """The bug, pinned so the fix below is measured against something real."""
        self.assertGreater(_excursion(cu.estimate_drift(self.arr, 0, self.du).positions), 5.0)

    def test_a_still_movie_stays_still_and_keeps_every_frame(self):
        variants = [('multiLag', {}), ('chain', {'estimator': 'chain'}),
                    ('multiLag smoothed', {'trajectory_smooth_sigma': cu.DRIFT_TASK_SMOOTH_SIGMA}),
                    ('per-plane', {'per_plane': True})]
        if _SITK_LOADS:
            variants.append(('sitkRigid', {'estimator': 'sitkRigid'}))
        for name, kw in variants:
            with self.subTest(name):
                est = cu.estimate_drift(self.arr, 0, self.du, measured_frames=_N_T - 1, **kw)
                self.assertEqual(len(est.positions), _N_T)               # the kept frame stays
                self.assertEqual(len(est.shifts), _N_T - 1)
                self.assertLess(_excursion(est.positions), 0.5)          # sub-pixel noise only
                # placed with the last complete frame, not predicted, not measured
                np.testing.assert_array_equal(est.positions[-1], est.positions[-2])
                np.testing.assert_array_equal(est.shifts[-1], np.zeros_like(est.shifts[-1]))
                if est.angles is not None:
                    self.assertEqual(len(est.angles), _N_T)
                    self.assertEqual(est.angles[-1], est.angles[-2])

    def test_out_of_range_is_refused(self):
        for bad in (0, _N_T + 1):
            with self.subTest(bad), self.assertRaises(ValueError):
                cu.estimate_drift(self.arr, 0, self.du, measured_frames=bad)


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class DriftRunnerPartialTimepointTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        spec = importlib.util.spec_from_file_location('drift_correct_run', _RUNNER)
        self.runner = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.runner)

        self.arr = _still_movie_stopped_mid_volume()
        omexml = ome_types.from_xml(_xml(tail=''))
        du = DimUtils(omexml, use_channel_axis=True)
        du.calc_image_dimensions(list(self.arr.shape))
        self.im_path = os.path.join(self.dir, 'im.ome.zarr')
        g, lv0, pchunks = zarr_utils.open_multiscales_for_writing(
            self.im_path, self.arr.shape, np.uint16, du, nscales=1)
        lv0[:] = self.arr
        zarr_utils.write_multiscale_pyramid(g, lv0, du, 1, list(pchunks))
        ome_xml_utils.save_meta_in_zarr(self.im_path, omexml=omexml)

    def _run(self, **extra):
        out_path = os.path.join(self.dir, 'corrected.ome.zarr')
        qc_path = os.path.join(self.dir, 'drift_shifts.json')
        self.runner.run(dict(imPath=self.im_path, imCorrectionPath=out_path, driftChannel=0,
                             qcOutPath=qc_path, **extra))
        with open(qc_path, encoding='utf-8') as fh:
            doc = json.load(fh)
        levels, _ = zarr_utils.open_as_zarr(out_path, as_dask=False)
        return doc, np.asarray(levels[0][:])

    def test_the_kept_frame_is_left_out_of_the_estimate_and_kept_in_the_output(self):
        doc, out = self._run(partialTimepoint=_N_T - 1)
        shifts = np.asarray(doc['shifts'])
        self.assertEqual(len(shifts), _N_T - 1)
        self.assertLess(float(np.abs(np.cumsum(shifts, axis=0)).max()), 0.5, doc['shifts'])
        self.assertEqual(doc['heldFrames'], [_N_T - 1])
        self.assertEqual(out.shape[0], _N_T)
        # still movie, no expansion: the kept frame's one recorded plane lands where frame t-1's did
        np.testing.assert_array_equal(out[-1, :, 0], out[-2, :, 0])
        np.testing.assert_array_equal(out[-1, :, 0], self.arr[-1, :, 0])

    def test_a_partial_timepoint_that_is_not_the_last_frame_is_ignored(self):
        """A record that does not describe this store (another version's T) is not acted on."""
        doc, _ = self._run(partialTimepoint=2)
        self.assertNotIn('heldFrames', doc)


if __name__ == '__main__':
    unittest.main()
