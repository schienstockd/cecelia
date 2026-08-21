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


if __name__ == '__main__':
    unittest.main()
