"""End-to-end test of the preview WORKER's backends — `preview/preview_worker.py`.

**Why this file exists.** `_preview_af` called `correction_utils.af_channel_indices` after that helper
had been moved to `script_utils` and renamed. Every one of the 438 Python tests passed, the Julia suite
passed, CI passed, and AF preview was broken on `main` — failing with a bare
``AttributeError: module 'cecelia.utils.correction_utils' has no attribute 'af_channel_indices'``
that the GUI surfaced as "Preview failed" with no message.

Nothing caught it because the worker's backends had NO tests. `correction_utils` was covered thoroughly
and the module that calls it was not — the same seam that let a `KeyError` ship in the task runner
(`test_af_correct_runner.py`, written for the same reason). The worker is loaded by path here, the way
the backend launches it, so an unresolved attribute on ANY backend is a failure rather than a surprise
in a running app.

Skipped when `preview/` is absent — an external `pip install cecelia` consumer gets the IO library only.
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

_WORKER = Path(__file__).resolve().parents[3] / 'preview' / 'preview_worker.py'


def _load_worker():
    """Load the worker from its path, as the backend launches it (it is not an importable module)."""
    spec = importlib.util.spec_from_file_location('preview_worker', _WORKER)
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


@unittest.skipUnless(_WORKER.is_file(), f'worker not present at {_WORKER}')
class PreviewWorkerAfTest(unittest.TestCase):
    SHAPE = dict(size_t=2, size_z=2, size_c=4, size_y=24, size_x=20)

    @classmethod
    def setUpClass(cls):
        cls.worker = _load_worker()

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)

        omexml = ome_types.from_xml(_ome_xml(**self.SHAPE))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = [self.SHAPE['size_t'], self.SHAPE['size_c'], self.SHAPE['size_z'],
                 self.SHAPE['size_y'], self.SHAPE['size_x']]
        du.calc_image_dimensions(shape)
        self.du = du

        rng = np.random.default_rng(7)
        data = np.full(shape, 40, dtype=np.uint16)
        data += rng.integers(0, 8, size=shape, dtype=np.uint16)
        c = du.dim_idx('C')
        for ch, (y0, x0) in enumerate([(2, 2), (6, 6), (10, 4), (4, 10)]):
            sl = [slice(None)] * len(shape)
            sl[c] = slice(ch, ch + 1)
            sl[du.dim_idx('Y')] = slice(y0, y0 + 6)
            sl[du.dim_idx('X')] = slice(x0, x0 + 6)
            data[tuple(sl)] += np.uint16(800 + 100 * ch)

        self.im_path = os.path.join(self.dir, 'in.ome.zarr')
        _, level0, _ = zarr_utils.open_multiscales_for_writing(
            self.im_path, tuple(shape), np.uint16, du, nscales=1)
        level0[:] = data
        ome_xml_utils.save_meta_in_zarr(self.im_path, omexml=omexml)

    def _request(self, combos, **over):
        msg = {
            'type': 'preview', 'imPath': self.im_path, 'taskDir': self.dir,
            'funName': 'cleanupImages.afCorrect', 'outputValueName': 'afCorrected',
            'params': {'afCombinations': combos, 'backgroundMethod': 'triangle'},
            'region': {'xy': {'X': [2, 18], 'Y': [2, 20]}, 'z': 0, 't': 0, 'ndisplay': 2},
        }
        msg.update(over)
        return self.worker.execute_command(msg)

    def test_the_af_backend_returns_layers(self):
        """The regression: this raised AttributeError on a helper that had moved modules."""
        out = self._request({'1': {'competingChannels': [2, 3]},
                             '2': {'competingChannels': [1, 3]}})
        self.assertNotEqual(out.get('type'), 'error', out.get('msg'))
        self.assertEqual(len(out['layers']), 2)
        for layer in out['layers']:
            self.assertEqual(layer['kind'], 'image')
            self.assertIn('block', layer)
            self.assertIn('source', layer)        # so the bridge can mirror the channel's colormap
            self.assertEqual(layer['axes'], ['T', 'Z', 'Y', 'X'])
        # the readout the GUI shows, from the same helper the run's QC uses
        for ch in ('1', '2'):
            d = out['derived'][ch]
            for k in ('background', 'competingBackgrounds', 'saturatedFrac', 'exponent'):
                self.assertIn(k, d)

    def test_the_previewed_region_is_reported_back(self):
        out = self._request({'1': {'competingChannels': [2]}})
        self.assertEqual(out['region']['Z'], [0, 1])      # exactly one plane, never a range
        self.assertEqual(out['region']['T'], [0, 1])
        self.assertEqual(out['region']['X'], [2, 18])

    def test_the_request_channel_names_win_over_the_stores_ome_xml(self):
        """THE GREY-LAYER BUG. napari names its layers from `ccid.json`, the authoritative copy; the
        worker was naming `source` from the store's OME-XML, a copy that is routinely stale. On a real
        image the store still said CH1..CH4 while the viewer showed SHG/nuc-GFP/mem-TOM/CD169-Kat, so
        `source` pointed at a layer that does not exist, the colormap mirror silently found nothing, and
        every corrected channel rendered grey against a magenta original.

        The fixture reproduces exactly that: its OME-XML is CH1..CH4.
        """
        names = ['SHG', 'nuc-GFP', 'mem-TOM', 'CD169-Kat']
        out = self._request({'2': {'competingChannels': [3]}}, channelNames=names)
        self.assertNotEqual(out.get('type'), 'error', out.get('msg'))
        layer, = out['layers']
        self.assertEqual(layer['source'], 'mem-TOM')      # the layer napari actually has
        self.assertEqual(layer['name'], 'mem-TOM AF')     # and the corrected layer says which channel
        self.assertNotIn('CH3', layer['name'])

    def test_without_given_names_the_ome_xml_is_the_fallback(self):
        """A REPL or test driving the worker directly sends no names — that must still work, and must
        still be a FALLBACK rather than a second source of truth."""
        out = self._request({'2': {'competingChannels': [3]}})
        layer, = out['layers']
        self.assertEqual(layer['source'], 'CH3')

    def test_backgrounds_are_derived_once_per_channel_not_once_per_combination(self):
        """THE COLD-START COST. A background level depends on the image, the channel and the method —
        not on which combination is asking. Keying the whole `AfWeightStats` by (target, competitors)
        re-derived the same numbers once per combination: a 3-channel setup asks for {1,2,3} three times
        over and paid a full pass over the movie each time. Measured on `zolIMa/2h06xA`, 26.9 s per
        pass -> 80.7 s for the preview the user actually configured.

        Counted rather than timed, so it cannot go flaky on a loaded machine.
        """
        calls = []
        real = self.worker.correction_utils.af_weight_stats

        def counting(data, dim_utils, channels, **kw):
            calls.append(list(channels))
            return real(data, dim_utils, channels, **kw)

        self.worker.correction_utils.af_weight_stats = counting
        self.addCleanup(setattr, self.worker.correction_utils, 'af_weight_stats', real)
        self.worker.STATE._af.clear()

        out = self._request({'1': {'competingChannels': [2, 3]},
                             '2': {'competingChannels': [1, 3]},
                             '3': {'competingChannels': [1, 2]}})
        self.assertNotEqual(out.get('type'), 'error', out.get('msg'))
        self.assertEqual(len(out['layers']), 3)

        # ONE pass, over the union — combinations 2 and 3 are free
        self.assertEqual(len(calls), 1, f'derived {len(calls)}x, expected 1: {calls}')
        self.assertEqual(sorted(calls[0]), [1, 2, 3])

        # ...and a repeat request derives nothing at all
        calls.clear()
        self._request({'2': {'competingChannels': [1, 3]}})
        self.assertEqual(calls, [])

        # a different method is a genuinely different value, so it must MISS
        self._request({'2': {'competingChannels': [1, 3]}},
                      params={'afCombinations': {'2': {'competingChannels': [1, 3]}},
                              'backgroundMethod': 'otsu'})
        self.assertEqual(len(calls), 1, 'switching background method must miss the cache')

    def test_the_assembled_stats_match_a_single_derivation(self):
        """Assembling per-channel cache entries must produce exactly what one combined pass produces —
        otherwise the speedup silently changes the correction."""
        self.worker.STATE._af.clear()
        warm = self.worker.STATE.af_stats(
            self.im_path, None, self.du, 1, [2, 3], 'triangle')
        direct = self.worker.correction_utils.af_weight_stats(
            self.worker.STATE.image_zarr(self.im_path)[0], self.du, [1, 2, 3],
            background_method='triangle',
            spatial_stride=self.worker.AF_PREVIEW_STRIDE,
            timepoints=self.worker._preview_timepoints(self.du))
        self.assertEqual(warm.backgrounds, direct.backgrounds)
        self.assertEqual(warm.saturated, direct.saturated)
        self.assertEqual((warm.nbins, warm.exponent), (direct.nbins, direct.exponent))

    def test_the_timepoint_budget_caps_frames_and_is_a_noop_when_short(self):
        """Runs stay EXACT — the budget is the preview's alone. `None` means "read them all"."""
        self.assertIsNone(self.worker._preview_timepoints(self.du))      # 2 frames <= budget
        self.assertIsNone(self.worker._preview_timepoints(self.du, max_frames=0))
        picked = self.worker._preview_timepoints(self.du, max_frames=1)
        self.assertEqual(len(picked), 1)
        self.assertLessEqual(max(picked), self.SHAPE['size_t'] - 1)

    def test_cellpose_is_not_imported_for_an_af_preview(self):
        """AF correction is numpy; cellpose + torch cost 3.1 s the AF path never needs. The worker was
        built for segmentation and grew a second backend, so the import sat at module level and every
        backend paid for every other backend's dependencies."""
        fresh = _load_worker()
        self.assertIsNone(fresh._CELLPOSE, 'cellpose must not load at import time')
        fresh.execute_command({
            'type': 'preview', 'imPath': self.im_path, 'taskDir': self.dir,
            'funName': 'cleanupImages.afCorrect', 'outputValueName': 'afCorrected',
            'params': {'afCombinations': {'2': {'competingChannels': [3]}},
                       'backgroundMethod': 'triangle'},
            'region': {'xy': {'X': [2, 18], 'Y': [2, 20]}, 'z': 0, 't': 0, 'ndisplay': 2},
        })
        self.assertIsNone(fresh._CELLPOSE, 'an AF preview must not pull in cellpose')

    def test_a_combination_with_no_competitor_is_skipped(self):
        out = self._request({'1': {'competingChannels': [2]}, '3': {'competingChannels': []}})
        self.assertEqual(len(out['layers']), 1)

    def test_no_usable_combination_raises_rather_than_previewing_nothing(self):
        # NOTE `execute_command` RAISES; the `{"type": "error", "msg": ...}` reply is built one layer
        # out, in the WS `handle`. Worth knowing: the message only becomes a message at the socket.
        with self.assertRaises(ValueError) as ctx:
            self._request({'1': {'competingChannels': []}})
        self.assertIn('competing', str(ctx.exception))

    def test_a_channel_NAME_says_the_backend_is_stale(self):
        """The worker must give the same diagnosis the run does — a name here means the Julia translator
        never ran, which is a stale-backend symptom, not a bad parameter."""
        with self.assertRaises(ValueError) as ctx:
            self._request({'1': {'competingChannels': ['CH3']}})
        msg = str(ctx.exception)
        self.assertIn('af_combinations_for_python', msg)
        self.assertIn('restart', msg)

    def test_every_declared_backend_resolves(self):
        """Cheap guard against the class of bug this file exists for: a backend referring to a helper
        that has moved or been renamed. Does not run them — just proves each is a real callable."""
        for fun_name, fn in self.worker._BACKENDS.items():
            self.assertTrue(callable(fn), fun_name)

    def test_the_protocol_is_reported_by_ping(self):
        reply = self.worker.execute_command({'type': 'ping'})
        self.assertEqual(reply['protocol'], self.worker.PROTOCOL)
        self.assertIn('cleanupImages.afCorrect', reply['backends'])


if __name__ == '__main__':
    unittest.main()
