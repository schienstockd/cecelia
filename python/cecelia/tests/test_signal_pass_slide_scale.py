"""The import probe on slide-scale planes (`data_path.measure_signal`, `intensity_utils.channel_histograms`):
memory bounded by patch / row-block reads, not by the plane, and the same answers.

A slide scan's plane is gigapixels per channel; reading it whole (and the photon fit's float copies)
ran a four-channel scan out of memory. The limits are shrunk here so a small array stands in for one.
"""
import unittest
from unittest import mock

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path, intensity_utils


class _Reads:
    """An array that records the size of every read — what a zarr level would load."""

    def __init__(self, arr):
        self.arr, self.shape, self.dtype, self.ndim = arr, arr.shape, arr.dtype, arr.ndim
        self.largest = 0

    def __getitem__(self, sl):
        out = self.arr[sl]
        self.largest = max(self.largest, out.size)
        return out


def _slide(rng, gain, offset, size=1200, n_c=2):
    """`n_c` channels of Gaussian-blob cells, photons x `gain` + `offset` + read noise, as uint16 (C, Y, X)."""
    out = np.zeros((n_c, size, size), np.uint16)
    for c in range(n_c):
        im = np.zeros((size, size))
        ys, xs = rng.integers(0, size, 600), rng.integers(0, size, 600)
        im[ys, xs] = rng.uniform(20, 200, 600)
        lam = ndi.gaussian_filter(im, 3.0) * 60.0 + 0.3
        out[c] = np.clip(np.round(gain * rng.poisson(lam) + offset + rng.normal(0, 5, lam.shape)), 0, 65535)
    return out


def _channels(arr):
    return [{'saturated': False, 'topValue': int(arr[c].max()), 'noise': {'bgMedian': 100.0}}
            for c in range(arr.shape[0])]


_SMALL = dict(SIGNAL_WHOLE_PLANE_PX=256 * 256, SIGNAL_PATCH_PX=96)


class SignalPassPatches(unittest.TestCase):
    def test_a_large_plane_is_read_in_patches_and_still_finds_the_gain(self):
        arr = _slide(np.random.default_rng(0), gain=10, offset=100)
        reads = _Reads(arr)
        with mock.patch.multiple(data_path, **_SMALL):
            chans = _channels(arr)
            out = data_path.measure_signal(reads, {'C': 0, 'T': None, 'Z': None}, chans)
        side = 96 + 2 * data_path.SIGNAL_PATCH_MARGIN_PX
        self.assertLessEqual(reads.largest, side * side)          # never the 1200 x 1200 plane
        for ch in chans:
            g = ch['photons']['fit']['greyPerPhoton']
            self.assertLess(abs(g - 10) / 10, 0.15, g)
        self.assertEqual(len(out['sharedStructure']), 1)

    def test_a_microscope_frame_is_still_read_whole(self):
        self.assertEqual(data_path._signal_regions(512, 512), [((slice(0, 512), slice(0, 512)),) * 2])

    def test_patches_cover_the_plane_evenly_and_stay_inside_it(self):
        with mock.patch.multiple(data_path, **_SMALL):
            regions = data_path._signal_regions(1000, 3000)
        self.assertEqual(len(regions), data_path.SIGNAL_PATCH_GRID ** 2)
        for (ry, rx), (iy, ix) in regions:
            self.assertTrue(0 <= ry.start < ry.stop <= 1000 and 0 <= rx.start < rx.stop <= 3000)
            self.assertEqual((iy.stop - iy.start, ix.stop - ix.start), (96, 96))
        self.assertEqual(min(r[0][1].start for r in regions), 0)
        self.assertEqual(max(r[0][1].stop for r in regions), 3000)


class PhotonsUntestable(unittest.TestCase):
    def test_a_processed_channel_gets_the_reason_not_a_fit_and_still_shares_structure(self):
        arr = _slide(np.random.default_rng(1), gain=10, offset=100, size=300)
        chans = _channels(arr)
        out = data_path.measure_signal(arr, {'C': 0, 'T': None, 'Z': None}, chans,
                                       photons_untestable={'1': 'autofluorescence subtracted by the scanner'})
        self.assertIsNotNone(chans[0]['photons']['fit']['greyPerPhoton'])
        self.assertEqual(chans[1]['photons']['fit']['greyPerPhoton'], None)
        self.assertEqual(chans[1]['photons']['fit']['untestable'], 'autofluorescence subtracted by the scanner')
        self.assertIsNone(chans[1]['photons']['objects'])
        self.assertEqual(chans[1]['photons']['notMeasured'], 'autofluorescence subtracted by the scanner')
        self.assertNotIn('notMeasured', chans[0]['photons'])
        self.assertEqual(len(out['sharedStructure']), 1)


class ProcessedFloor(unittest.TestCase):
    def test_a_processed_channel_with_a_photon_counted_looking_floor_gives_no_photon_count(self):
        # a resolved photon comb (step 32, cut at 15) as `test_floor_low_discriminator` makes it: on an
        # unprocessed channel `floor_stats` reads it photon-counted
        rng = np.random.default_rng(9)
        k = rng.poisson(0.6, (2, 1, 400, 1000))
        v = np.round(k * 32 + rng.normal(0, 6, k.shape) * np.sqrt(k))
        v[v < 15] = 0
        ch = v.astype(np.uint16)
        with mock.patch.object(data_path.ome_xml_utils, 'load_ome_xml', return_value=None), \
             mock.patch.object(data_path, 'DimUtils') as du, \
             mock.patch.object(data_path.zarr_utils, 'open_as_zarr', return_value=([ch], None)), \
             mock.patch.object(data_path.zarr_utils, 'read_axes', return_value=['c', 'z', 'y', 'x']):
            du.return_value.im_dim_order = 'CZYX'
            du.return_value.dim_idx.side_effect = lambda d: 'CZYX'.index(d)
            out = data_path.measure_store('unused', photons_untestable={'1': 'autofluorescence subtracted by the scanner'})
        fl0, fl1 = out['channels'][0]['floor'], out['channels'][1]['floor']
        self.assertEqual(fl0['kind'], 'photon-counted')             # the unprocessed channel keeps its reading
        self.assertEqual((fl1['kind'], fl1['photons'], fl1['photonStep']), ('indeterminate', None, None))
        self.assertIn('autofluorescence subtracted', fl1['evidence'])


class HistogramRowBlocks(unittest.TestCase):
    def test_a_large_plane_is_read_in_row_blocks_with_the_same_histogram(self):
        arr = np.random.default_rng(2).integers(0, 4096, (2, 300, 257)).astype(np.uint16)
        reads = _Reads(arr)
        per_plane = []
        with mock.patch.object(intensity_utils, 'HIST_BLOCK_PX', 257 * 40):
            hists = intensity_utils.channel_histograms(reads, 0, on_plane=lambda c, ix, h: per_plane.append(h))
        self.assertLessEqual(reads.largest, 257 * 40)
        for c in range(2):
            np.testing.assert_array_equal(hists[c], np.bincount(arr[c].ravel(), minlength=65536))
            np.testing.assert_array_equal(per_plane[c], hists[c])   # one plane each: its own histogram, whole


if __name__ == '__main__':
    unittest.main()
