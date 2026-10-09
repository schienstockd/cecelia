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

    def test_the_af_backend_returns_previewImages_on_disk(self):
        """The regression: this raised AttributeError on a helper that had moved modules.
        Since P7.1, the reply carries `previewImages` — one scratch OME-Zarr per corrected channel,
        on DISK — rather than an inline block, and the browser swaps that channel's slab URL onto
        the scratch store."""
        out = self._request({'1': {'competingChannels': [2, 3]},
                             '2': {'competingChannels': [1, 3]}})
        self.assertNotEqual(out.get('type'), 'error', out.get('msg'))
        # Labels-style `layers` is unused for AF; the AF path returns `previewImages`.
        self.assertNotIn('layers', out)
        self.assertEqual(len(out['previewImages']), 2)
        seen_channels = set()
        for m in out['previewImages']:
            self.assertEqual(m['axes'], ['T', 'Z', 'Y', 'X'])
            self.assertNotIn('block', m)                          # protocol 14+ is on disk
            self.assertTrue(os.path.isdir(m['path']))              # promoted from staging
            self.assertIn(m['sourceChannel'], (1, 2))
            self.assertEqual(m['valueName'], 'afCorrected')
            self.assertIn('AF', m['name'])                         # names the source channel it corrects
            self.assertEqual(m['badge'], 'AF')                     # the viewer's compare badge
            self.assertEqual(m['shape'],
                             [self.SHAPE['size_t'], self.SHAPE['size_z'],
                              self.SHAPE['size_y'], self.SHAPE['size_x']])
            seen_channels.add(m['sourceChannel'])
        self.assertEqual(seen_channels, {1, 2})
        # the readout the GUI shows, from the same helper the run's QC uses
        for ch in ('1', '2'):
            d = out['derived'][ch]
            for k in ('background', 'competingBackgrounds', 'saturatedFrac', 'exponent'):
                self.assertIn(k, d)

    def test_scratch_store_convention(self):
        """The path is the slab route's contract. `{task_dir}/{value_name}__preview_af_ch{N}.ome.zarr`
        — keyed on the value_name AND the source channel index — is what the Julia slab route derives
        the scratch path from, so the two sides must not disagree."""
        out = self._request({'2': {'competingChannels': [3]}})
        m, = out['previewImages']
        expected = os.path.join(self.dir, 'afCorrected__preview_af_ch2.ome.zarr')
        self.assertEqual(m['path'], expected)

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
        m, = out['previewImages']
        self.assertEqual(m['name'], 'mem-TOM AF')          # the corrected entry says which channel
        self.assertNotIn('CH3', m['name'])

    def test_without_given_names_the_ome_xml_is_the_fallback(self):
        """A REPL or test driving the worker directly sends no names — that must still work, and must
        still be a FALLBACK rather than a second source of truth. The `name` field is what carries
        the channel display name; without given names, it falls back to the OME-XML's `CH3`."""
        out = self._request({'2': {'competingChannels': [3]}})
        m, = out['previewImages']
        self.assertEqual(m['name'], 'CH3 AF')

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
        self.assertEqual(len(out['previewImages']), 3)

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
        self.assertEqual(len(out['previewImages']), 1)

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

    def test_ping_names_the_env_it_was_launched_in(self):
        """Adoption matches the worker's env to the model (#1555): the backend sets `CECELIA_PY_ENV` at
        launch and needs it back, or a worker in the default env is adopted for a cellpose 3 preview."""
        old = os.environ.get('CECELIA_PY_ENV')
        os.environ['CECELIA_PY_ENV'] = 'cellpose-v3'
        try:
            reply = _load_worker().execute_command({'type': 'ping'})
        finally:
            os.environ.pop('CECELIA_PY_ENV') if old is None else os.environ.__setitem__('CECELIA_PY_ENV', old)
        self.assertEqual(reply['env'], 'cellpose-v3')
        self.assertEqual(self.worker.execute_command({'type': 'ping'})['env'],
                         old if old is not None else 'default')



@unittest.skipUnless(_WORKER.is_file(), f'worker not present at {_WORKER}')
class PreviewWorkerSmoothTest(unittest.TestCase):
    """The smoothing preview (CLEANUP_FACTS_PLAN D2) runs the RUN's compute — `smooth_utils` — over the
    tile, with the run's whole-image gain. Pinned by recomputing the run's arithmetic on the full plane
    and comparing away from the tile edge (the edge is the seam caveat every tiled preview carries)."""
    SHAPE = PreviewWorkerAfTest.SHAPE

    @classmethod
    def setUpClass(cls):
        cls.worker = _load_worker()

    def setUp(self):
        PreviewWorkerAfTest.setUp(self)          # same synthetic 4-channel uint16 store

    def _request(self, params, t=1):
        return self.worker.execute_command({
            'type': 'preview', 'imPath': self.im_path, 'taskDir': self.dir,
            'funName': 'cleanupImages.smooth', 'outputValueName': 'smoothed',
            'params': params,
            'region': {'xy': {'X': [2, 18], 'Y': [2, 20]}, 'z': 0, 't': t, 'ndisplay': 2},
        })

    def test_the_preview_is_the_runs_arithmetic_on_the_tile(self):
        import cecelia.utils.smooth_utils as smooth_utils
        params = {'channels': [1, 3], 'spatialMethod': 'gaussian', 'spatialSigma': 1.0,
                  'temporalFrames': 3, 'temporalStat': 'median', 'restoreGain': True}
        out = self._request(params)
        self.assertNotEqual(out.get('type'), 'error', out.get('msg'))
        self.assertEqual(sorted(m['sourceChannel'] for m in out['previewImages']), [1, 3])  # only the selected

        level = zarr_utils.open_as_zarr(self.im_path, as_dask=False)[0][0]
        du = self.du
        idx = {ax: du.dim_idx(ax) for ax in 'TCZ'}

        def read_plane(t, c, z):
            sl = [slice(None)] * level.ndim
            sl[idx['T']], sl[idx['C']], sl[idx['Z']] = t, c, z
            return np.asarray(level[tuple(sl)], dtype=np.float32)

        spatial_fn = smooth_utils.build_spatial_fn('gaussian', 1.0, 10.0, 3.0, 0.6)
        gain = smooth_utils.estimate_gain(read_plane, spatial_fn, [1, 3],
                                          self.SHAPE['size_t'], self.SHAPE['size_z'])[0]
        self.assertAlmostEqual(out['derived']['gain'], round(gain, 3))
        n_t = self.SHAPE['size_t']

        def spatial_at(t, c):
            return spatial_fn(read_plane(min(max(t, 0), n_t - 1), c, 0))

        full = smooth_utils.smooth_timepoint(spatial_at, [1, 3], 1, 1, 3, 'median')
        for m in out['previewImages']:
            c = m['sourceChannel']
            want, _ = smooth_utils.apply_gain(full[c], gain, np.iinfo(np.uint16).max)
            got = np.asarray(zarr_utils.open_as_zarr(m['path'], as_dask=False)[0][0])
            got = got[1, 0]                                       # T=1, Z=0 → [Y, X]
            # interior of the tile Y[2,20) X[2,18), 4 px in from the crop edge
            np.testing.assert_array_equal(got[6:16, 6:14], want[6:16, 6:14].astype(np.uint16))
            self.assertIn('smoothed', m['name'])
            self.assertEqual(m['badge'], 'Smooth')

    def test_smooth_is_a_declared_backend(self):
        reply = self.worker.execute_command({'type': 'ping'})
        self.assertIn('cleanupImages.smooth', reply['backends'])


@unittest.skipUnless(_WORKER.is_file(), f'worker not present at {_WORKER}')
class PreviewWorkerRenderTest(unittest.TestCase):
    """Stills on the shared shader (docs/todo/STILLS_WORKER_PLAN.md): the worker's `render` is the
    runner's own frames, and it does not queue behind a preview."""

    @classmethod
    def setUpClass(cls):
        from cecelia.tests.test_render_animation_run import _store
        from cecelia.tests.test_wgsl_utils import _shared_host
        _shared_host()
        cls.worker = _load_worker()
        cls.dir = tempfile.mkdtemp()
        img = np.zeros((1, 1, 16, 16, 16), np.uint16)
        img[0, 0, :, 2:8, 2:8] = 1000
        cls.zarr = _store(os.path.join(cls.dir, 'img.zarr'), img, 'tczyx')

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.dir, ignore_errors=True)

    def _params(self, name):
        st = {'t': 0, 'camera': {'angles': [0, 20, 0], 'zoom': 1.0}, 'snapH': 16,
              'specs': [{'lo': 0, 'hi': 1000, 'lut': [[0, 0, 0], [0, 1, 0]], 'visible': True}]}
        return {'zarrPath': self.zarr, 'states': [st], 'canvasH': 32, 'canvasW': 32,
                'outPaths': [os.path.join(self.dir, f'{name}.png')]}

    def test_render_is_the_runners_frame(self):
        from PIL import Image
        from cecelia.utils import wgpu_host
        from cecelia.writers import render_animation_run
        p = self._params('w')
        reply = self.worker.execute_command({'type': 'render', 'params': p})
        self.assertEqual(reply['paths'], p['outPaths'])
        want = next(render_animation_run.render_frames(p, wgpu_host.MipHost(), self.worker.script_utils.StdoutLogger()))
        np.testing.assert_array_equal(np.asarray(Image.open(p['outPaths'][0]).convert('RGB')), want)

    def test_a_render_does_not_wait_for_a_preview(self):
        import threading
        done = threading.Event()
        with self.worker._PREVIEW_LOCK:        # a preview in flight
            t = threading.Thread(target=lambda: (self.worker._execute_locked(
                {'type': 'render', 'params': self._params('busy')}), done.set()))
            t.start()
            self.assertTrue(done.wait(60), 'render queued behind the preview lock')
        t.join()

    def test_ping_takes_no_lock(self):
        with self.worker._PREVIEW_LOCK, self.worker._RENDER_LOCK:
            self.assertEqual(self.worker._execute_locked({'type': 'ping'})['protocol'], self.worker.PROTOCOL)


if __name__ == '__main__':
    unittest.main()


@unittest.skipUnless(_WORKER.is_file(), f'worker not present at {_WORKER}')
class PreviewStoreLevelsTest(unittest.TestCase):
    """A preview store has the IMAGE's levels, each equal to the strided pyramid of the full-size
    store — what a zoomed-out viewer draws, and what the run's own label pyramid will show."""

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.worker = _load_worker()
        omexml = ome_types.from_xml(_ome_xml(1, 1, 1, 21, 26))
        du = DimUtils(omexml, use_channel_axis=True)
        shape = (1, 1, 1, 21, 26)
        du.calc_image_dimensions(list(shape))
        self.im_path = os.path.join(self.dir, 'im.ome.zarr')
        g, lv0, pchunks = zarr_utils.open_multiscales_for_writing(
            self.im_path, shape, np.uint16, du, nscales=3)
        zarr_utils.write_multiscale_pyramid(g, lv0, du, 3, list(pchunks))
        ome_xml_utils.save_meta_in_zarr(self.im_path, omexml=omexml)

    def test_every_level_is_the_strided_full_store(self):
        full_shape = (1, 21, 26)
        bounds = {'Y': (5, 16), 'X': (3, 18)}                 # odd starts: alignment matters
        block = np.arange(1, 1 + 11 * 15, dtype=np.uint32).reshape(1, 11, 15)
        path = self.worker._stage_labels_store(
            block, ['T', 'Y', 'X'], full_shape, bounds, self.dir, 'vn', im_path=self.im_path)
        full = np.zeros(full_shape, dtype=np.uint32)
        full[:, 5:16, 3:18] = block
        levels, _ = zarr_utils.open_as_zarr(path, as_dask=False)
        self.assertEqual(len(levels), 3)
        for lv in range(3):
            s = 2 ** lv
            np.testing.assert_array_equal(np.asarray(levels[lv][:]), full[:, ::s, ::s],
                                          err_msg=f'level {lv}')
