"""The physical scale a flow model was trained at — `_physical_scale` in `opticalFlow/train_run.py`.

Why this is worth a test of its own: every number coastal is configured with is in PIXELS
(`cropSize`, `foregroundBlurSigma`) or FRAMES (`temporalScales`, `cumulativeWindow`), and none of
them means anything without knowing what a pixel and a frame were. A model trained on a 30 s/frame
movie applied to a 5 s/frame one sees entirely different displacements at scale 1 — the same class of
silent train/inference mismatch the manifest was created to prevent for the metric set.

So the failure this pins is not a crash, it is a plausible number. Two ways to get one:

  * **inventing a scale.** `im_physical_size`'s own default is 1.0, so a bare call writes
    "1 µm/px" for an image that recorded nothing. `None` and an explicit `physicalScaleSource: none`
    are the honest answers, and the vault UI says "unknown" for them.
  * **reporting the stack's Z step instead of the model's.** Training takes every `zSpacing`-th
    plane, so a 1 µm stack at spacing 2 means the model saw 2 µm.

The runner is executed by path (`run_py`) and never imported, so nothing else in the suite reaches it.
Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
See docs/todo/MODEL_VAULT_PLAN.md.
"""
import importlib.util
import unittest
from pathlib import Path

import ome_types

from cecelia.utils.dim_utils import DimUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'opticalFlow' / 'train_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('flow_train_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _dim_utils(*, px=None, pz=None, dt=None, unit='µm', t_unit='s', size_z=10, size_t=8, px_y=None):
    """A DimUtils over hand-built OME-XML. Attributes are OMITTED, not zeroed, when None — that is
    the case under test, and ome-types renders a missing attribute as `None`."""
    attrs = [f'SizeT="{size_t}"', f'SizeC="1"', f'SizeZ="{size_z}"', 'SizeY="16"', 'SizeX="16"']
    if px is not None:
        attrs += [f'PhysicalSizeX="{px}"', f'PhysicalSizeXUnit="{unit}"',
                  f'PhysicalSizeY="{px_y if px_y is not None else px}"',
                  f'PhysicalSizeYUnit="{unit}"']
    if pz is not None:
        attrs += [f'PhysicalSizeZ="{pz}"', f'PhysicalSizeZUnit="{unit}"']
    if dt is not None:
        attrs += [f'TimeIncrement="{dt}"', f'TimeIncrementUnit="{t_unit}"']
    xml = f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" {' '.join(attrs)}>
      <Channel ID="Channel:0:0" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>"""
    du = DimUtils(ome_types.from_xml(xml), use_channel_axis=True)
    du.calc_image_dimensions((size_t, 1, size_z, 16, 16))
    return du


@unittest.skipUnless(_RUNNER.is_file(), 'app/ not present (library-only install)')
class PhysicalScaleTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        cls.mod = _load_runner()

    def test_records_xy_z_and_frame_interval(self):
        s = self.mod._physical_scale(_dim_utils(px=0.62, pz=1.0, dt=30.0), [10, 12, 14])
        self.assertEqual(s['x'], 0.62)
        self.assertEqual(s['xUnit'], 'um')          # µ normalised by `im_physical_unit`
        self.assertEqual(s['t'], 30.0)
        self.assertEqual(s['tUnit'], 's')

    def test_z_is_the_gap_between_the_planes_trained_on(self):
        """A 1 µm stack sampled every second plane means the model saw 2 µm, not 1."""
        s = self.mod._physical_scale(_dim_utils(px=0.6, pz=1.0, dt=1.0), [10, 12, 14, 16])
        self.assertEqual(s['z'], 2.0)
        s1 = self.mod._physical_scale(_dim_utils(px=0.6, pz=1.0, dt=1.0), [10, 11, 12])
        self.assertEqual(s1['z'], 1.0)

    def test_no_scale_at_all_is_None_not_one_micron(self):
        """`im_physical_size`'s default is 1.0, so the bare call would claim a measurement."""
        self.assertIsNone(self.mod._physical_scale(_dim_utils(), [10, 12]))

    def test_2d_and_single_plane_omit_z(self):
        for planes in ([None], [14]):
            s = self.mod._physical_scale(_dim_utils(px=0.6, pz=1.0, dt=5.0), planes)
            self.assertNotIn('z', s, f'planes={planes}')
            self.assertEqual(s['x'], 0.6)

    def test_missing_z_metadata_omits_z_but_keeps_xy(self):
        s = self.mod._physical_scale(_dim_utils(px=0.6, dt=5.0), [10, 12])
        self.assertNotIn('z', s)
        self.assertEqual(s['t'], 5.0)

    def test_y_only_recorded_when_it_differs_from_x(self):
        """Repeating an equal value on every entry would bury the anisotropic case."""
        same = self.mod._physical_scale(_dim_utils(px=0.6), [None])
        self.assertNotIn('y', same)
        aniso = self.mod._physical_scale(_dim_utils(px=0.32, px_y=0.64), [None])
        self.assertEqual(aniso['y'], 0.64)

    def test_coastal_build_names_the_commit_not_just_the_version(self):
        """`coastal.__version__` is `0.1.0` and does not move; the pin in `pixi.toml` is a git
        revision described there as a hard floor. So the identifier that means anything is the commit,
        and it is what makes a published model's output reproducible at all."""
        build = self.mod._coastal_build()
        if build is None:
            self.skipTest('coastal not installed')
        self.assertIn('version', build)
        # A VCS install records the commit; a PyPI/editable one does not, and says less rather than
        # implying a snapshot it cannot name.
        if 'commit' in build:
            self.assertRegex(build['commit'], r'^[0-9a-f]{7,40}$')

    def test_units_are_recorded_not_converted(self):
        """No unit converter exists here; inventing one is a silent numeric error on the first
        non-micron file. The unit travels with the value instead."""
        s = self.mod._physical_scale(_dim_utils(px=320, dt=500, unit='nm', t_unit='ms'), [None])
        self.assertEqual((s['x'], s['xUnit']), (320.0, 'nm'))
        self.assertEqual((s['t'], s['tUnit']), (500.0, 'ms'))


class RunnerLocalsAreNotShadowedTest(unittest.TestCase):
    """The manifest's accumulators are each bound once, and to different names.

    A real bug, found by Dominik running a training job: `physicalScales`' accumulator was called
    `scales`, which is already the temporal-scale LIST six lines above it. `max(scales)` raised
    `ValueError: max() iterable argument is empty`, and had that guard not crashed first, the manifest
    would have been written with a dict of pixel sizes under `temporalScales`.

    Unit-testing `_physical_scale` in isolation could never catch this — the collision lives in the
    caller, not the callee. This is the cheap check that can: the runner needs torch, coastal and a
    real movie to execute, but its source parses here in milliseconds.

    Deliberately NOT "no local is ever rebound" — rebinding is ordinary and that check reports ten
    false positives. These are the names whose identity the manifest depends on.
    """

    #: accumulators built once before the movie loop and read again when the manifest is written
    SINGLE_BINDING = ('scales', 'phys_scales', 'used', 'planes_used', 'windows', 'crops',
                      # `scales` IS what `temporalScales` records now, in BOTH modes: the spans are
                      # `lag x ref_interval`, so they resolve back to exactly these. The three below
                      # are what `seconds_config` derives — a function's return rather than an `if` in
                      # the middle of `run`, for this test's reason.
                      'ref_interval', 'declared', 'cum_seconds')

    @classmethod
    def setUpClass(cls):
        import ast, inspect
        cls.fn = ast.parse(inspect.getsource(_load_runner().run)).body[0]

    def _bindings(self):
        import ast
        counts = {}
        for node in ast.walk(self.fn):
            if not isinstance(node, ast.Assign):
                continue
            for target in node.targets:
                names = ([target] if isinstance(target, ast.Name)
                         else [e for e in getattr(target, 'elts', []) if isinstance(e, ast.Name)])
                for n in names:
                    counts[n.id] = counts.get(n.id, 0) + 1
        return counts

    def test_each_manifest_accumulator_is_bound_exactly_once(self):
        counts = self._bindings()
        for name in self.SINGLE_BINDING:
            self.assertEqual(
                1, counts.get(name, 0),
                f'`{name}` is bound {counts.get(name, 0)} time(s) in run(). It feeds the manifest, so '
                'a second binding silently changes what gets written. Rename the newcomer.')

    def test_the_two_scale_fields_do_not_read_the_same_variable(self):
        """`temporalScales` is the frame-lag list; `physicalScales` is the per-movie µm/s dict."""
        import ast
        pairs = {}
        for node in ast.walk(self.fn):
            if not isinstance(node, ast.Dict):
                continue
            for k, v in zip(node.keys, node.values):
                if isinstance(k, ast.Constant) and k.value in ('temporalScales', 'physicalScales') \
                        and isinstance(v, ast.Name):
                    pairs[k.value] = v.id
        self.assertEqual({'temporalScales', 'physicalScales'}, set(pairs),
                         'both manifest fields should be written from a plain local')
        self.assertNotEqual(pairs['temporalScales'], pairs['physicalScales'],
                            f'both read `{pairs["temporalScales"]}` — this is the shadowing bug')



class _Log:
    """Collects what the runner said, so a test can read it back."""

    def __init__(self):
        self.lines = []

    def log(self, line):
        self.lines.append(str(line))

    def progress(self, *_a, **_k):
        pass


class _StubDimUtils:
    """Only what `reference_interval` reads: the T axis, the interval, its unit, the frame count."""

    def __init__(self, dt, n_t, unit):
        self._dt, self._n_t, self._unit = dt, n_t, unit
        self.im_dim_order = list('TCZYX')

    def is_timeseries(self):
        return True

    def dim_val(self, ax):
        return self._n_t if ax == 'T' else 4

    def im_time_increment(self, default=None):
        return self._dt if self._dt is not None else default

    def im_time_increment_unit(self, default='s'):
        return self._unit or default


def _movie(uid, dt, n_t, unit='s'):
    """A movie entry, carrying the metadata the stubbed `_open` will hand back for it."""
    return {'uID': uid, 'imPath': f'/nonexistent/{uid}.zarr', 'dt': dt, 'nT': n_t, 'unit': unit}


class SecondsConfigTest(unittest.TestCase):
    """What `seconds` mode declares — DERIVED from the lags, not typed in beside them.

    The form is one chip row of frame lags whose labels read as durations, so a span is
    `lag x the reference interval`. Two things fall out, and both are the point:

    * a span that is not a whole number of the reference movie's frames cannot be expressed at all,
      where the free-text box it replaced could ask for 20 s on a 15 s/frame movie and silently get
      15 s;
    * `round(lag x ref / ref) == lag`, so the pooled channel names are the chips the user picked and
      `temporalScales` means the same thing in both modes. That is what removed `pooled_offsets`.

    `_open` is stubbed to metadata only, which is all `reference_interval` reads — no zarr, no pixels.
    """

    def setUp(self):
        self.runner = _load_runner()
        self._real_open = self.runner._open
        self.log = _Log()

    def tearDown(self):
        self.runner._open = self._real_open

    def _config(self, mode, movies, scales, cumulative=5):
        by_path = {m['imPath']: m for m in movies}

        def _open(path):
            m = by_path[path]
            return None, _StubDimUtils(m['dt'], m['nT'], m['unit'])

        self.runner._open = _open
        return self.runner.seconds_config(mode, movies, scales, cumulative, 0, 42, self.log)

    def test_frames_mode_declares_nothing_in_seconds(self):
        """The lags ARE the setting there and mean nothing in seconds, so no reference is claimed
        rather than one being invented."""
        self.assertEqual(self._config('frames', [], [1, 2, 4, 8]), (None, [], 0.0))

    def test_the_spans_are_the_lags_at_the_coarsest_usable_rate(self):
        ref, spans, cum = self._config(
            'seconds', [_movie('slow', 15.0, 40), _movie('fast', 5.0, 40)], [1, 2, 4, 8])
        self.assertEqual((ref, spans, cum), (15.0, [15.0, 30.0, 60.0, 120.0], 75.0))

    def test_the_spans_resolve_back_to_the_lags_at_the_reference(self):
        """The identity the design rests on — no reconciling step, because the canonical channel names
        are the chips that were picked."""
        from cecelia.utils import coastal_utils
        lags = [1, 2, 3, 6]
        ref, spans, cum = self._config('seconds', [_movie('a', 12.0, 40)], lags, cumulative=4)
        back, back_cum, problem = coastal_utils.scales_from_seconds(spans, cum, ref)
        self.assertEqual((problem, back, back_cum), ('', lags, 4))
        self.assertEqual(coastal_utils.mag_rename(lags, back), {})

    def test_the_coarsest_anchor_is_what_lets_every_movie_resolve(self):
        """The reason for coarsest over finest, as an assertion rather than a comment. On the finest
        anchor the spans are as short as that movie allows and every coarser movie is refused for a
        span below one of its frames — 1 of 3 here against 3 of 3."""
        from cecelia.utils import coastal_utils
        lags, rates = [1, 2, 4], (5.0, 10.0, 15.0)
        movies = [_movie(f'r{dt:g}', dt, 60) for dt in rates]
        ref, spans, cum = self._config('seconds', movies, lags)
        self.assertEqual(ref, max(rates))
        for dt in rates:
            self.assertEqual(coastal_utils.scales_from_seconds(spans, cum, dt)[2], '',
                             f'{dt} s/frame must resolve at the coarsest anchor')
        # And the anchor nobody should pick, so the comparison cannot rot.
        finest = [l * min(rates) for l in lags]
        refused = [dt for dt in rates if coastal_utils.scales_from_seconds(finest, 0, dt)[2]]
        self.assertEqual(refused, [10.0, 15.0], 'the finest anchor must still be the worse one')

    def test_a_movie_too_short_for_the_lags_cannot_be_the_reference(self):
        """Exact here and nowhere else: at its OWN rate a candidate's lags ARE `scales`, so
        `max(scales) + 1` is precisely what it needs. Checking it now stops the reference being set by
        a movie the main loop then skips, which would record an interval nothing trained at."""
        ref, spans, _ = self._config(
            'seconds', [_movie('slow_but_short', 15.0, 6), _movie('fast', 5.0, 40)], [1, 2, 4, 8])
        self.assertEqual(ref, 5.0, 'a 6-frame movie cannot carry lag 8, coarse though it is')
        self.assertEqual(spans, [5.0, 10.0, 20.0, 40.0])

    def test_a_unit_that_is_not_seconds_cannot_be_the_reference(self):
        """The runner does not convert — see `_physical_scale`. So a ms movie is not an anchor."""
        ref, _, _ = self._config(
            'seconds', [_movie('ms', 1.0, 40, unit='ms'), _movie('ok', 15.0, 40)], [1, 2])
        self.assertEqual(ref, 15.0)

    def test_no_anchor_at_all_raises_rather_than_guessing(self):
        """A 1.0 s/frame fallback would be a measurement nobody made."""
        for movies in ([_movie('none', None, 40)],
                       [_movie('ms', 250.0, 40, unit='ms')],
                       [_movie('short', 5.0, 3)],
                       []):
            with self.assertRaises(ValueError) as ctx:
                self._config('seconds', movies, [1, 2, 4, 8])
            self.assertIn('no movie can anchor the spans', str(ctx.exception))

    def test_it_says_which_movie_anchored_the_spans(self):
        """The reference decides what every channel means, so a run that cannot be read back to the
        movie it was anchored on cannot be checked."""
        self._config('seconds', [_movie('slow', 15.0, 40), _movie('fast', 5.0, 40)], [1, 2])
        said = ' '.join(self.log.lines)
        self.assertIn('slow', said)
        self.assertIn('15 s/frame', said)

    def test_every_movie_renames_onto_the_lags(self):
        """The property that makes a mixed-rate pool trainable at all: one channel layout across
        sequences, which is what coastal's sorted-key stacking requires."""
        from cecelia.utils import coastal_utils
        lags = [1, 2, 4]
        movies = [_movie('a', 5.0, 60), _movie('b', 10.0, 60), _movie('c', 15.0, 60)]
        ref, spans, cum = self._config('seconds', movies, lags)
        self.assertEqual(ref, 15.0)
        for m in movies:
            own, _, problem = coastal_utils.scales_from_seconds(spans, cum, m['dt'])
            self.assertEqual(problem, '', m['uID'])
            renamed = coastal_utils.apply_mag_rename(
                {f'mag_{o}': o for o in own}, coastal_utils.mag_rename(lags, own))
            self.assertEqual(sorted(renamed), sorted(f'mag_{o}' for o in lags), m['uID'])


if __name__ == '__main__':
    unittest.main()
