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
                      # The POOLED offsets, which is what `temporalScales` records and therefore what
                      # every channel of the model is named after. Rebinding it is why
                      # `pooled_offsets` is a function rather than an `if` in the middle of `run`.
                      'pool_scales', 'pool_cumulative', 'ref_interval')

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



class PooledOffsetsTest(unittest.TestCase):
    """The canonical channel names a pooled training set shares.

    The failure this prevents is not a crash. Coastal names its per-scale planes `mag_{offset}` and
    stacks the metric dict by `sorted(keys)`, so a set pooled across frame rates in `seconds` mode
    has sequences whose channels mean different spans under different names — which `run`'s
    `key_sets` guard catches, and which renaming onto one set of offsets is what actually fixes.
    """

    @staticmethod
    def _p(mode, scales, cumulative, declared, cum_seconds, movie_dt):
        return _load_runner().pooled_offsets(
            mode, scales, cumulative, declared, cum_seconds, movie_dt)

    def test_frames_mode_is_the_identity_and_states_no_reference(self):
        """Every movie was read at the form's offsets, so there is nothing to reconcile — and no
        interval to claim, which is a different thing from claiming one and being wrong."""
        self.assertEqual(self._p('frames', [1, 2, 4, 8], 5, [], 0.0, {}),
                         ([1, 2, 4, 8], 5, None))

    def test_the_finest_movie_sets_the_canonical_offsets(self):
        offsets, cum, ref = self._p('seconds', [1, 2, 4, 8], 5, [5.0, 10.0, 20.0, 40.0], 25.0,
                                    {'fast': 5.0, 'slow': 15.0})
        self.assertEqual((offsets, cum, ref), ([1, 2, 4, 8], 5, 5.0))

    def test_the_finest_movie_needs_no_rename_of_its_own(self):
        """Why finest rather than mean or first: the reference is a real acquisition, so at least one
        pooled sequence is already on the canonical names and no offset rounds below one frame."""
        from cecelia.utils import coastal_utils
        movie_dt = {'fast': 2.5, 'mid': 5.0, 'slow': 15.0}
        spans = [5.0, 10.0, 20.0, 40.0]
        offsets, _, ref = self._p('seconds', [1, 2, 4, 8], 5, spans, 25.0, movie_dt)
        own, _, problem = coastal_utils.scales_from_seconds(spans, 25.0, ref)
        self.assertEqual(problem, '')
        self.assertEqual(coastal_utils.mag_rename(offsets, own), {})

    def test_every_pooled_movie_renames_onto_the_same_channel_count(self):
        """The property that makes the pool trainable at all: one channel layout across sequences."""
        from cecelia.utils import coastal_utils
        movie_dt = {'a': 2.0, 'b': 5.0, 'c': 6.0}
        spans = [10.0, 20.0, 40.0]
        offsets, _, _ = self._p('seconds', [1, 2, 4], 5, spans, 20.0, movie_dt)
        for uid, dt in movie_dt.items():
            own, _, problem = coastal_utils.scales_from_seconds(spans, 20.0, dt)
            self.assertEqual(problem, '', uid)
            renamed = coastal_utils.apply_mag_rename(
                {f'mag_{o}': o for o in own}, coastal_utils.mag_rename(offsets, own))
            self.assertEqual(sorted(renamed), sorted(f'mag_{o}' for o in offsets), uid)

    def test_a_reference_that_cannot_carry_the_spans_raises(self):
        with self.assertRaises(ValueError):
            self._p('seconds', [1, 2], 5, [10.0, 12.0], 20.0, {'a': 10.0})

if __name__ == '__main__':
    unittest.main()
