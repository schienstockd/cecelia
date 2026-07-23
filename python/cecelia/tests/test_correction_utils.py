"""Equivalence tests for the streaming (out=) correction paths.

The drift / AF / cellpose correction tasks used to allocate the ENTIRE corrected image as one
in-RAM numpy array (drift), or build every channel + a concatenate copy (AF), which OOMs on large
time-lapses. They now stream one timepoint / one channel at a time into an on-disk zarr. These tests
pin that the streaming output is BYTE-IDENTICAL to the legacy in-RAM output — the in-RAM path is kept
as the reference oracle (out=None) and asserted equal to the streamed path (out=<zarr>).

Also covers the shared `write_multiscale_pyramid` refactor (create_multiscales now builds the pyramid
one timepoint at a time). Part of the Python analysis-env suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
import dask.array as da
import zarr
import ome_types

import cecelia.utils.zarr_utils as zu
import cecelia.utils.correction_utils as cu
from cecelia.utils.dim_utils import DimUtils


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeZ="{size_z}" SizeC="{size_c}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm"
            PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      {''.join(f'<Channel ID="Channel:0:{c}" SamplesPerPixel="1"/>' for c in range(size_c))}
    </Pixels>
  </Image>
</OME>"""


def _dim_utils(size_t, size_z, size_c, size_y, size_x):
    """DimUtils for the given sizes (size-1 axes are dropped from im_dim_order). Sizes are chosen
    distinct by the callers so the axis order resolves unambiguously."""
    du = DimUtils(ome_types.from_xml(_ome_xml(size_t, size_z, size_c, size_y, size_x)),
                  use_channel_axis=True)
    shape = [s for s in (size_t, size_z, size_c, size_y, size_x) if s != 1]
    du.calc_image_dimensions(tuple(shape))
    return du


class DriftStreamingEquivalenceTest(unittest.TestCase):
    def _run(self, du, shifts):
        rng = np.random.default_rng(0)
        arr = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)

        # reference: legacy in-RAM path
        legacy = cu.drift_correct_im(arr, du, 0, shifts=shifts, out=None)

        # streamed path into an on-disk zarr level 0
        d = tempfile.mkdtemp()
        try:
            out_shape, _ = cu.drift_correct_shape(arr, du, shifts)
            self.assertEqual(tuple(out_shape), tuple(legacy.shape))
            path = os.path.join(d, "drift.ome.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(
                path, out_shape, arr.dtype.newbyteorder('='), du, nscales=1)
            cu.drift_correct_im(arr, du, 0, shifts=shifts, out=level0)
            streamed = zarr.open_group(path, mode="r")["0"][:]
        finally:
            shutil.rmtree(d, ignore_errors=True)

        self.assertTrue(np.array_equal(legacy, streamed),
                        "streamed drift output differs from the in-RAM reference")

    def test_2d_timeseries(self):
        du = _dim_utils(size_t=5, size_z=1, size_c=2, size_y=17, size_x=13)
        shifts = np.array([[1, 2], [-1, 0], [2, -2], [0, 1]], dtype=float)   # [T-1, (Y,X)]
        self._run(du, shifts)

    def test_3d_timeseries(self):
        du = _dim_utils(size_t=4, size_z=3, size_c=2, size_y=15, size_x=11)
        shifts = np.array([[1, 1, 2], [0, -1, 1], [1, 2, -1]], dtype=float)  # [T-1, (Z,Y,X)]
        self._run(du, shifts)


class AfStreamingEquivalenceTest(unittest.TestCase):
    def _run(self, du, af_combinations):
        rng = np.random.default_rng(1)
        base = rng.integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        # feed a dask array like production (as_dask=True): each channel slice computes to a fresh
        # numpy array, so in-place denoise/rolling-ball/top-hat never aliases the input. Use an
        # independent copy per call so the two runs can't influence each other.
        def _src():
            return da.from_array(base.copy(), chunks=base.shape)

        class _Log:  # af_correct_image only calls logfile_utils.log
            def log(self, *_a, **_k):
                pass

        # reference: legacy in-RAM (dask) path
        legacy = np.asarray(cu.af_correct_image(
            _src(), af_combinations, dim_utils=du, logfile_utils=_Log(),
            apply_gaussian=True, apply_gaussian_to_others=True, out=None))

        d = tempfile.mkdtemp()
        try:
            out_shape = cu.af_correction_output_shape(base, du, af_combinations)
            self.assertEqual(tuple(out_shape), tuple(legacy.shape))
            path = os.path.join(d, "af.ome.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(
                path, out_shape, base.dtype.newbyteorder('='), du, nscales=1)
            cu.af_correct_image(
                _src(), af_combinations, dim_utils=du, logfile_utils=_Log(),
                apply_gaussian=True, apply_gaussian_to_others=True, out=level0)
            streamed = zarr.open_group(path, mode="r")["0"][:]
        finally:
            shutil.rmtree(d, ignore_errors=True)

        self.assertTrue(np.array_equal(legacy, streamed),
                        "streamed AF output differs from the in-RAM reference")

    def test_divide_plus_inverse_and_passthrough(self):
        # ch0: AF-divide by ch1 with an inverse appended; ch1: gaussian-to-others (default)
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=19, size_x=14)
        combos = {"0": {"divisionChannels": [1], "correctionMode": "divide",
                        "generateInverse": True, "channelPercentile": 80,
                        "correctionPercentile": 40}}
        self._run(du, combos)

    def test_denoise_only_channel_tv(self):
        # 'tv' denoiser. Streaming vs in-RAM must match regardless of denoiser.
        du = _dim_utils(size_t=2, size_z=1, size_c=2, size_y=16, size_x=12)
        combos = {"0": {"divisionChannels": [], "denoiseFun": "tv", "tvWeight": 0.1}}
        self._run(du, combos)

    def test_denoise_only_channel_wavelet(self):
        # 'wavelet' denoiser needs PyWavelets (pixi dep). Guards both the streaming/in-RAM
        # equivalence AND that the wavelet path is importable (was an undeclared-dep crash).
        du = _dim_utils(size_t=2, size_z=1, size_c=2, size_y=16, size_x=12)
        combos = {"0": {"divisionChannels": [], "denoiseFun": "wavelet",
                        "waveletMethod": "BayesShrink", "waveletMode": "soft"}}
        self._run(du, combos)


class PyramidRefactorTest(unittest.TestCase):
    """create_multiscales now builds the pyramid via write_multiscale_pyramid (one timepoint at a
    time). Level 1 must equal level 0 strided by 2 in X and Y — the original downsampling semantics."""

    def test_timeseries_pyramid_matches_strided_level0(self):
        du = _dim_utils(size_t=3, size_z=1, size_c=2, size_y=20, size_x=16)
        rng = np.random.default_rng(2)
        base = rng.integers(0, 65535, size=tuple(du.im_dim), dtype=np.uint16)
        src = da.from_array(base, chunks=(1, 2, 20, 16))

        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "ms.ome.zarr")
            zu.create_multiscales(src, path, dim_utils=du, nscales=2)
            g = zarr.open_group(path, mode="r")
            self.assertTrue(np.array_equal(g["0"][:], base))
            y_idx, x_idx = du.dim_idx('Y'), du.dim_idx('X')
            sel = [slice(None)] * base.ndim
            sel[y_idx] = slice(0, None, 2)
            sel[x_idx] = slice(0, None, 2)
            self.assertTrue(np.array_equal(g["1"][:], base[tuple(sel)]),
                            "pyramid level 1 is not the strided-by-2 level 0")
        finally:
            shutil.rmtree(d, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
