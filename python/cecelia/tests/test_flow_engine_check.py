"""The inference-side flow-engine check — does the WARN reach the log, and only when it should?

`test_flow_probe.py` covers the fingerprint itself. This covers the wiring: the manifest is read
once per model path, the probe is measured once per run, and each of the four cases says the right
thing — agreed, disagreed, not recorded, no manifest at all.
"""
import json
import os
import tempfile
import unittest

from cecelia.utils import flow_probe


class _Log:
    def __init__(self):
        self.lines = []

    def log(self, msg):
        self.lines.append(str(msg))

    def progress(self, *_a, **_k):
        pass

    @property
    def warnings(self):
        return [line for line in self.lines if '[WARN]' in line]


class _DimUtils:
    im_dim_order = list('TCZYX')
    _shape = (30, 2, 4, 64, 64)

    def is_timeseries(self):
        return True

    def is_3D(self):
        return True

    def dim_val(self, ax):
        return self._shape[self.im_dim_order.index(ax)]

    def dim_idx(self, ax):
        return self.im_dim_order.index(ax)

    def im_physical_size(self, ax, default=1.0):
        return default

    def im_time_increment(self, default=None):
        return default

    def im_time_increment_unit(self, default='s'):
        return default


def _fp(**metrics):
    return {'version': flow_probe.VERSION,
            'metrics': {k: list(v) for k, v in metrics.items()}}


class FlowEngineCheckTest(unittest.TestCase):
    """Each case builds a real `<name>.json` beside a `<name>.pt`, because the path under test is
    `read_manifest` -> `_check_flow_engine` and stubbing `_manifest` would skip exactly that."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.model = os.path.join(self.tmp.name, 'm.pt')
        with open(self.model, 'w', encoding='utf-8') as f:
            f.write('not a real checkpoint')
        self.addCleanup(self.tmp.cleanup)

    def _cu(self, manifest, current):
        """A CoastalUtils whose FIRST manifest read is the one the test makes.

        The manifest is written AFTER construction on purpose. `__init__` resolves the temporal
        config, which reads the manifest — so planting the file first would run the check once
        against the real engine before the test has substituted anything, and every assertion here
        would be about the second call.
        """
        from cecelia.utils.coastal_utils import CoastalUtils, manifest_path
        params = {'taskDir': self.tmp.name,
                  'models': {'0': {'model': self.model, 'cellChannels': [0]}}}
        cu = CoastalUtils(params, _DimUtils())
        if manifest is not None:
            with open(manifest_path(self.model), 'w', encoding='utf-8') as f:
                json.dump(manifest, f)
        cu.logger = self.log = _Log()
        cu._flow_fp = current
        cu._manifest_cache = {}
        return cu

    def test_matching_fingerprints_say_nothing(self):
        f = _fp(mag_1=[1.0, 4.0])
        cu = self._cu({'temporalScales': [1, 2], 'flowFingerprint': f}, f)
        cu._manifest({'model': self.model})
        self.assertEqual(self.log.warnings, [])

    def test_a_mismatch_names_the_metric(self):
        cu = self._cu({'temporalScales': [1, 2], 'flowFingerprint': _fp(mag_1=[1.0, 4.0])},
                      _fp(mag_1=[3.0, 4.0]))
        cu._manifest({'model': self.model})
        self.assertEqual(len(self.log.warnings), 1)
        self.assertIn('mag_1', self.log.warnings[0])
        self.assertIn('re-train', self.log.warnings[0].lower())

    def test_a_manifest_without_a_fingerprint_says_it_cannot_be_checked(self):
        cu = self._cu({'temporalScales': [1, 2]}, _fp(mag_1=[1.0, 4.0]))
        cu._manifest({'model': self.model})
        self.assertEqual(len(self.log.warnings), 1)
        self.assertIn('fingerprint', self.log.warnings[0])

    def test_no_manifest_at_all_is_silent(self):
        """Already reported, with better advice, by `coastal_models_for_python`."""
        cu = self._cu(None, _fp(mag_1=[1.0, 4.0]))
        cu._manifest({'model': self.model})
        self.assertEqual(self.log.warnings, [])

    def test_an_unprobeable_engine_is_not_a_mismatch(self):
        """`fingerprint()` returns {} where coastal cannot be probed. That is "unknown", not
        "different", and must not fire."""
        cu = self._cu({'temporalScales': [1, 2], 'flowFingerprint': _fp(mag_1=[1.0, 4.0])}, {})
        cu._manifest({'model': self.model})
        self.assertEqual(self.log.warnings, [])

    def test_the_warning_fires_once_per_model_path(self):
        cu = self._cu({'temporalScales': [1, 2], 'flowFingerprint': _fp(mag_1=[1.0, 4.0])},
                      _fp(mag_1=[3.0, 4.0]))
        for _ in range(4):
            cu._manifest({'model': self.model})
        self.assertEqual(len(self.log.warnings), 1)

    def test_the_probe_is_measured_at_most_once(self):
        from cecelia.utils import coastal_utils
        calls = []
        real = coastal_utils.flow_probe.fingerprint

        def counted():
            calls.append(1)
            return _fp(mag_1=[1.0, 4.0])

        cu = self._cu({'temporalScales': [1, 2], 'flowFingerprint': _fp(mag_1=[1.0, 4.0])}, None)
        coastal_utils.flow_probe.fingerprint = counted
        try:
            for _ in range(3):
                cu._manifest({'model': self.model})
                cu._manifest_cache = {}
        finally:
            coastal_utils.flow_probe.fingerprint = real
        self.assertEqual(len(calls), 1)

    def test_the_manifest_is_still_returned(self):
        """The check is a side effect; it must not change what `_manifest` gives back."""
        m = {'temporalScales': [1, 2, 4], 'flowFingerprint': _fp(mag_1=[1.0, 4.0])}
        cu = self._cu(m, _fp(mag_1=[9.0, 4.0]))
        self.assertEqual(cu._manifest({'model': self.model})['temporalScales'], [1, 2, 4])


if __name__ == '__main__':
    unittest.main()
