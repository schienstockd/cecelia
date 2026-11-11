"""`photon_utils` — grey levels per photon from the image's own noise, photons on objects, shared
structure — and the signal pass that feeds them (`data_path.measure_signal`).
`docs/todo/ACQUISITION_TEST_CHECK_PLAN.md`.

Golden values: synthetic Poisson-Gaussian planes with a KNOWN gain, offset and read noise. The fit
must recover the gain the image was made with; that is the property the advisor's photon numbers rest
on (Foi et al. 2008, doi:10.1109/TIP.2008.2001399, is the model)."""
import unittest

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path, photon_utils as pu


def _scene(rng, n=8, size=256, blobs=40, sigma=3.0, bg=0.3, peak=60.0):
    """Expected photons per voxel: a flat background plus Gaussian blobs (cells)."""
    out = []
    for _ in range(n):
        im = np.zeros((size, size))
        ys, xs = rng.integers(0, size, blobs), rng.integers(0, size, blobs)
        im[ys, xs] = rng.uniform(20, 200, blobs)
        out.append(ndi.gaussian_filter(im, sigma) * peak + bg)
    return out


def _acquire(rng, lam, gain, offset, read, ceiling=None):
    """Grey levels as a detector would record them: photons x gain + offset + read noise, clipped."""
    planes = []
    for l in lam:
        p = np.round(gain * rng.poisson(l) + offset + rng.normal(0, read, l.shape))
        p = np.clip(p, 0, ceiling if ceiling is not None else None)
        planes.append(p.astype(np.int64))
    return planes


class PhotonTransferTest(unittest.TestCase):
    def test_recovers_the_gain_the_image_was_made_with(self):
        # 1 grey level per photon (a photon-counting readout) up to 300 (a high-gain PMT counting few)
        for gain, offset, read in ((1, 100, 3), (10, 100, 5), (50, 56, 10), (300, 56, 10)):
            rng = np.random.default_rng(gain)
            fit = pu.photon_transfer(_acquire(rng, _scene(rng), gain, offset, read), offset=offset)
            assert fit['untestable'] is None
            assert abs(fit['greyPerPhoton'] - gain) / gain < 0.10, (gain, fit['greyPerPhoton'])
            assert fit['r2'] > 0.95
            assert fit['offset'] == offset

    def test_clipped_windows_do_not_bend_the_fit(self):
        # a ceiling at ~20 photons cuts the variance of the brightest windows; they must be dropped
        rng = np.random.default_rng(7)
        gain, offset = 100, 50
        planes = _acquire(rng, _scene(rng), gain, offset, 5, ceiling=2047)
        fit = pu.photon_transfer(planes, ceiling=2047, offset=offset)
        assert abs(fit['greyPerPhoton'] - gain) / gain < 0.10, fit['greyPerPhoton']

    def test_a_photon_counted_channel_keeps_its_zeros(self):
        # photon counting: no offset, no read noise, ~0.3 photons per voxel -> mostly exact zeros. Those
        # zeros are data; dropping their windows (right for a background clipped at 0) leaves nothing
        rng = np.random.default_rng(9)
        planes = _acquire(rng, _scene(rng, bg=0.05, peak=8.0), 30, 0, 0)
        assert np.mean([np.mean(p == 0) for p in planes]) > 0.5
        mus, vs = zip(*(pu.window_stats(p, drop_zeros=False) for p in planes))
        fit = pu.fit_photon_transfer(np.concatenate(mus), np.concatenate(vs), offset=0.0)
        assert abs(fit['greyPerPhoton'] - 30) / 30 < 0.10, fit['greyPerPhoton']
        dropped = sum(pu.window_stats(p)[0].size for p in planes)
        assert dropped < np.concatenate(mus).size / 2

    def test_photons_converts_through_the_fit(self):
        fit = {'greyPerPhoton': 600.0, 'offset': 56.0}
        assert pu.photons(4095, fit) == (4095 - 56) / 600
        assert pu.photons(4095, {'greyPerPhoton': None}) is None

    def test_a_flat_noise_free_plane_is_untestable(self):
        fit = pu.photon_transfer([np.full((128, 128), 500, dtype=np.int64)] * 4)
        assert fit['greyPerPhoton'] is None and fit['untestable']

    def test_noise_that_does_not_grow_with_signal_is_untestable(self):
        # pure read noise over a ramp: no photon term, so no gain to report
        rng = np.random.default_rng(3)
        ramp = np.tile(np.linspace(100, 3000, 256), (256, 1))
        planes = [np.round(ramp + rng.normal(0, 20, ramp.shape)).astype(np.int64) for _ in range(6)]
        fit = pu.photon_transfer(planes)
        assert fit['greyPerPhoton'] is None or fit['greyPerPhoton'] < 1.0


class ObjectPhotonsTest(unittest.TestCase):
    def test_objects_carry_the_photons_the_scene_put_there(self):
        rng = np.random.default_rng(11)
        lam = _scene(rng, n=6, bg=0.2, peak=60.0)
        gain, offset = 10, 60                                     # brightest blob ~210 photons: stays under 4095
        planes = _acquire(rng, lam, gain, offset, 8)
        fit = {'greyPerPhoton': float(gain), 'offset': float(offset)}
        sm = np.concatenate([pu.smooth_plane(p).ravel() for p in planes])
        lam_sm = np.concatenate([ndi.gaussian_filter(l, pu.SMOOTH_SIGMA_PX).ravel() for l in lam])
        obj = pu.object_photons(sm, fit, ceiling=4095, raw=np.concatenate([p.ravel() for p in planes]))
        assert 0 < obj['objectFrac'] < 0.5                        # blobs, not the whole field
        assert obj['objectLevel'] > offset
        # truth over the SAME voxels the measured mask picked: there, photons(smoothed) estimates the
        # smoothed expected photon count
        hist = np.bincount(np.clip(np.round(sm), 0, None).astype(np.int64))
        from cecelia.utils import intensity_utils
        mask = sm > intensity_utils.background_threshold(hist)
        truth = np.median(lam_sm[mask])
        assert abs(obj['p50Photons'] - truth) / truth < 0.25, (obj['p50Photons'], truth)
        assert obj['ceilingPhotons'] == (4095 - offset) / gain

    def test_untestable_fit_gives_no_object_numbers(self):
        assert pu.object_photons(np.arange(100.0), {'greyPerPhoton': None}) is None


class SharedStructureTest(unittest.TestCase):
    def test_the_same_structure_in_two_channels_correlates_and_independent_ones_do_not(self):
        rng = np.random.default_rng(5)
        a = ndi.gaussian_filter(rng.random((200, 200)), 3).ravel()
        b = ndi.gaussian_filter(rng.random((200, 200)), 3).ravel()
        r = {(d['a'], d['b']): d['r'] for d in pu.shared_structure({0: a, 1: 0.4 * a + 0.01, 2: b})}
        assert r[(0, 1)] > 0.99
        assert abs(r[(0, 2)]) < 0.2


class MeasureSignalTest(unittest.TestCase):
    def test_the_signal_pass_reports_photons_per_channel_and_shared_structure(self):
        # (T, C, Z, Y, X)
        rng = np.random.default_rng(2)
        lam = _scene(rng, n=2 * 3, size=128)
        gains = (100, 400)
        chans = [np.stack(_acquire(rng, lam, g, 50, 5)).reshape(2, 3, 128, 128) for g in gains]
        arr = np.stack(chans, axis=1)                                   # T, C, Z, Y, X
        channels = [{'saturated': False, 'topValue': int(arr[:, c].max()), 'noise': {'bgMedian': 50.0}}
                    for c in range(2)]
        out = data_path.measure_signal(arr, {'C': 1, 'T': 0, 'Z': 2}, channels)
        for c, g in enumerate(gains):
            fit = channels[c]['photons']['fit']
            assert abs(fit['greyPerPhoton'] - g) / g < 0.15, (g, fit['greyPerPhoton'])
            assert channels[c]['photons']['objects']['p50Photons'] > 0
        assert 'drift' not in out                                   # the probe estimates no drift
        assert out['sharedStructure'][0]['r'] > 0.9               # same scene in both channels


if __name__ == '__main__':
    unittest.main()
