"""Whole-slide multiplex: where the tissue is (`tissue_utils`), the import's signal patches on it
(`data_path._signal_regions`), the pyramid written in bands (`zarr_utils._pyramid_bands`), and cycles
registered on the pyramid (`register_utils`, `register_run.output_plan`).

The synthetic slide has what the PhenoCycler scans have: exact zeros where the scanner skipped a tile,
dim glass inside the scanned tiles, and a few bright tissue sections — cycles offset by more than a
gradient optimiser recovers on its own.
"""
import importlib.util
import os
import unittest
from unittest import mock

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path, register_utils, tissue_utils, zarr_utils


def _sections(size, centres, radius, rng):
    """A nuclear-stain-like plane: sections of dense blobs on dim glass, zeros outside the scanned
    tiles (a 64-px tile grid around each section)."""
    h, w = size
    yy, xx = np.mgrid[:h, :w]
    tissue = np.zeros(size, bool)
    for cy, cx in centres:
        tissue |= (yy - cy) ** 2 + ((xx - cx) * 0.8) ** 2 < radius ** 2
    nuc = np.zeros(size)
    pts = rng.random(size) < 0.02
    nuc[pts & tissue] = rng.uniform(300, 900, int((pts & tissue).sum()))
    img = ndi.gaussian_filter(nuc, 1.5) * 12 + 40 * tissue + 60
    img += rng.normal(0, 4, size)
    scanned = ndi.binary_dilation(tissue, iterations=40)
    scanned = ndi.maximum_filter(scanned[::64, ::64], 2).repeat(64, 0).repeat(64, 1)[:h, :w]
    return np.where(scanned, np.clip(img, 1, 65535), 0).astype(np.uint16), tissue


def _pyramid(plane, n=4):
    """Levels as (t, c, z, y, x) arrays, each half the last — an NGFF pyramid in memory."""
    out = [plane[None, None, None]]
    for _ in range(n - 1):
        out.append(out[-1][..., ::2, ::2])
    return out


def _shift(plane, dx, dy, angle_deg=0.0):
    """The same slide imaged with the stage offset: moving(p) = plane(p - shift), rotated about the
    centre — so registering moving onto plane must find the fixed→moving map p -> R(p) + (dx, dy)."""
    h, w = plane.shape
    a = np.radians(angle_deg)
    m = np.array([[np.cos(a), -np.sin(a)], [np.sin(a), np.cos(a)]])   # (x, y)
    c = np.array([(w - 1) / 2, (h - 1) / 2])
    # moving(q) = plane(p) where q = m (p - c) + c + t  ->  p = m^-1 (q - c - t) + c
    mi = np.linalg.inv(m)
    t = np.array([dx, dy])
    mrc = mi[::-1, ::-1]
    off_xy = c - mi @ (c + t)
    return ndi.affine_transform(plane.astype(np.float32), mrc, offset=off_xy[::-1], order=1,
                                output_shape=(h, w)).astype(np.uint16)


class TissueMask(unittest.TestCase):
    def test_finds_the_sections_not_the_scanned_tiles(self):
        plane, tissue = _sections((600, 400), [(150, 120), (420, 260)], 70, np.random.default_rng(0))
        mask = tissue_utils.tissue_mask(plane)
        inter = (mask & tissue).sum()
        self.assertGreater(inter / tissue.sum(), 0.9)                 # the sections
        self.assertLess(mask.sum(), 1.3 * tissue.sum())               # not the tiles around them
        self.assertLess((mask & (plane == 0)).sum(), 1)               # never unscanned slide

    def test_tissue_peak_taller_than_glass(self):
        # sections fill most of the scanned area: the class Zack's triangle cut above
        plane, tissue = _sections((400, 400), [(200, 200)], 150, np.random.default_rng(1))
        mask = tissue_utils.tissue_mask(plane)
        self.assertGreater((mask & tissue).sum() / tissue.sum(), 0.9)

    def test_empty_plane(self):
        self.assertFalse(tissue_utils.tissue_mask(np.zeros((50, 50), np.uint16)).any())

    def test_coarse_level_is_the_finest_within_budget(self):
        shapes = [(1, 1, 1, 4000, 4000), (1, 1, 1, 2000, 2000), (1, 1, 1, 1000, 1000)]
        self.assertEqual(tissue_utils.coarse_level(shapes, max_px=2000 * 2000), 1)
        self.assertEqual(tissue_utils.coarse_level(shapes, max_px=10), 2)


class SignalPatchesOnTissue(unittest.TestCase):
    def test_patches_sit_on_the_tissue(self):
        h, w = 2000, 2000
        mask = np.zeros((200, 200), bool)
        mask[20:60, 130:180] = True                                   # one section, top right
        with mock.patch.multiple(data_path, SIGNAL_WHOLE_PLANE_PX=256 * 256, SIGNAL_PATCH_PX=96):
            regions = data_path._signal_regions(h, w, tissue=(mask, (10.0, 10.0)))
            grid = data_path._signal_regions(h, w)
        self.assertLess(len(regions), len(grid))
        for read, inner in regions:
            cy = read[0].start + inner[0].start + 48
            cx = read[1].start + inner[1].start + 48
            self.assertTrue(mask[min(199, cy // 10), min(199, cx // 10)], (cy, cx))

    def test_no_tissue_falls_back_to_the_grid(self):
        with mock.patch.multiple(data_path, SIGNAL_WHOLE_PLANE_PX=256 * 256, SIGNAL_PATCH_PX=96):
            empty = data_path._signal_regions(2000, 2000, tissue=(np.zeros((20, 20), bool), (100.0, 100.0)))
            grid = data_path._signal_regions(2000, 2000)
        self.assertEqual(empty, grid)


class PyramidBands(unittest.TestCase):
    def test_bands_cover_the_level_exactly(self):
        src = (0, slice(None), 0, slice(0, 1000, 2), slice(0, 600, 2))
        dest = (1, 3, 1, 500, 300)
        with mock.patch.object(zarr_utils, 'PYRAMID_BAND_ELEMENTS', 3 * 300 * 64):
            bands = zarr_utils._pyramid_bands(src, dest, (1, 1, 1, 32, 32))
        self.assertGreater(len(bands), 1)
        rows = [b[0][3] for b in bands]
        self.assertEqual(rows[0].start, 0)
        self.assertEqual(rows[-1].stop, 500)
        for a, b in zip(rows, rows[1:]):
            self.assertEqual(a.stop, b.start)
        arr = np.arange(3 * 1000 * 600).reshape(1, 3, 1, 1000, 600)
        whole = arr[:, :, :, ::2, ::2]
        out = np.zeros(dest, arr.dtype)
        for wr, rd in bands:
            rd = (slice(0, 1),) + rd[1:2] + (slice(0, 1),) + rd[3:]
            wr = (slice(0, 1),) + wr[1:2] + (slice(0, 1),) + wr[3:]
            out[wr] = arr[rd]
        np.testing.assert_array_equal(out, whole)

    def test_a_time_axis_level_is_banded_per_timepoint(self):
        # a slide's label store keeps a T of 1: its level must not be read whole either
        src = np.arange(2 * 1000 * 600, dtype=np.uint32).reshape(2, 1000, 600)
        dest = {}

        class _Group:
            def create_array(self, name, shape, chunks, dtype, **_kw):
                dest[name] = np.zeros(shape, dtype)
                return dest[name]

        class _Reads:
            shape, dtype, ndim = src.shape, src.dtype, src.ndim
            largest = 0
            def __getitem__(self, sl):
                out = src[sl]
                _Reads.largest = max(_Reads.largest, out.size)
                return out

        with mock.patch.object(zarr_utils, 'PYRAMID_BAND_ELEMENTS', 300 * 64), \
                mock.patch.object(zarr_utils, '_codec_kwargs', lambda *a, **k: {}), \
                mock.patch.object(zarr_utils, '_group_format', lambda g: 2), \
                mock.patch.object(zarr_utils, '_group_separator', lambda g: '.'):
            zarr_utils.write_multiscale_pyramid(_Group(), _Reads(), None, 2, (1, 32, 32),
                                                x_idx=2, y_idx=1, t_idx=0)
        np.testing.assert_array_equal(dest['1'], src[:, ::2, ::2])
        self.assertLessEqual(_Reads.largest, 300 * 64)

    def test_small_level_is_one_write(self):
        src = (slice(0, 10, 2), slice(0, 10, 2))
        self.assertEqual(len(zarr_utils._pyramid_bands(src, (5, 5), (5, 5))), 1)


class SlideRegistration(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            import SimpleITK  # noqa: F401
        except Exception as e:                # pragma: no cover - see register_run.py's import note
            raise unittest.SkipTest(f'SimpleITK unavailable: {e}')
        rng = np.random.default_rng(3)
        cls.ref, cls.tissue = _sections((1024, 768), [(250, 250), (700, 480)], 150, rng)
        cls.dx, cls.dy, cls.angle = -137.0, 61.0, 0.4
        cls.mov = _shift(cls.ref, cls.dx, cls.dy, cls.angle)

    def _small(self):
        return mock.patch.multiple(tissue_utils, TISSUE_LEVEL_MAX_PX=200 * 200)

    def test_recovers_the_offset_and_rotation(self):
        with self._small(), mock.patch.object(register_utils, 'REGISTER_FINE_MAX_PX', 600 * 600):
            tx, info = register_utils.estimate_affine(_pyramid(self.ref), _pyramid(self.mov), 0, 0,
                                                      (0, None, 0), 1)
        d = register_utils.describe(register_utils.affine_matrix(tx), self.ref.shape)
        self.assertEqual(len(info['levels']), 2)                       # coarse, then a finer level
        h, w = self.ref.shape
        a = np.radians(self.angle)
        c = np.array([(w - 1) / 2, (h - 1) / 2])
        self.assertAlmostEqual(d['rotationDeg'], self.angle, delta=0.1)
        self.assertAlmostEqual(d['shiftPx'][0], self.dx, delta=1.5)
        self.assertAlmostEqual(d['shiftPx'][1], self.dy, delta=1.5)
        self.assertAlmostEqual(d['scale'], 1.0, delta=0.01)

    def test_warp_and_checks(self):
        with self._small(), mock.patch.object(register_utils, 'REGISTER_FINE_MAX_PX', 600 * 600):
            tx, _ = register_utils.estimate_affine(_pyramid(self.ref), _pyramid(self.mov), 0, 0,
                                                   (0, None, 0), 1)
            out = np.zeros_like(self.ref)
            def write(ys, xs, arr):
                out[ys, xs] = arr
            with mock.patch.object(register_utils, 'WARP_TILE_PX', 256):
                n = register_utils.warp_plane(lambda ys, xs: self.mov[ys, xs], self.mov.shape, tx,
                                              self.ref.shape, write, np.dtype(np.uint16))
            self.assertGreater(n, 1)
            inner = ndi.binary_erosion(self.tissue, iterations=20) & (out > 0)
            err = np.abs(out[inner].astype(float) - self.ref[inner].astype(float))
            self.assertLess(np.median(err), 0.1 * np.median(self.ref[inner]))
            with mock.patch.multiple(register_utils, CHECK_PATCH_PX=96, CHECK_PATCHES=6):
                ok = register_utils.cycle_checks(_pyramid(self.ref), _pyramid(self.mov), 0, 0,
                                                 (0, None, 0), 1, tx, pixel_um=0.5)
                # the second section washed off in the moving cycle
                lost = self.mov.copy()
                lost_mask = _shift((self.tissue & (np.arange(1024)[:, None] > 450)).astype(np.uint16) * 1000,
                                   self.dx, self.dy, self.angle) > 500
                lost[ndi.binary_dilation(lost_mask, iterations=15)] = 60
                bad = register_utils.cycle_checks(_pyramid(self.ref), _pyramid(lost), 0, 0,
                                                  (0, None, 0), 1, tx, pixel_um=0.5)
        self.assertLess(ok['residualPx']['median'], 1.5)
        self.assertGreater(ok['residualPx']['n'], 0)
        self.assertGreater(ok['tissueCovered'], 0.9)
        self.assertGreater(ok['tissueRetained'], 0.9)
        self.assertAlmostEqual(ok['signalRatio'], 1.0, delta=0.15)
        self.assertLess(bad['tissueRetained'], 0.75)


class OutputPlan(unittest.TestCase):
    def setUp(self):
        path = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'app', 'src', 'tasks',
                            'editImages', 'register_run.py')
        spec = importlib.util.spec_from_file_location('register_run', path)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)

    def test_reference_then_each_cycle_alignment_channel_optional(self):
        self.assertEqual(self.mod.output_plan([2, 3], [0, 1], False), [(0, 0), (0, 1), (1, 0), (1, 2)])
        self.assertEqual(self.mod.output_plan([2, 3], [0, 1], True),
                         [(0, 0), (0, 1), (1, 0), (1, 1), (1, 2)])
        # the Julia handler names the channels in this order (`_register_channel_plan`, the same
        # example 1-based in app/test/suite/calibration_helpers.jl), and warns at the same residual
        jl = open(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'app', 'src', 'tasks',
                               'editImages', 'register_plan.jl'), encoding='utf-8').read()
        self.assertIn(f'const REGISTER_RESIDUAL_WARN_UM = {register_utils.RESIDUAL_WARN_UM}', jl)


if __name__ == '__main__':
    unittest.main()
