"""Packaging guard: the vendored btrack config must ship as package-data.

External consumers (e.g. the sibling `coastal` project) locate `cell_config.json` via
`importlib.resources` — which only works if the file is declared as package-data in
`python/pyproject.toml` (so it lands in a wheel), not merely present in the source tree. This test
fails if that declaration is dropped. See docs/todo/PY_PACKAGING_PLAN.md.
"""
import json
import unittest
from importlib.resources import files


class PackageDataTest(unittest.TestCase):
    def test_btrack_cell_config_is_packaged(self):
        cfg = files("cecelia").joinpath("tasks", "tracking", "cell_config.json")
        self.assertTrue(cfg.is_file(), f"cell_config.json not packaged/locatable: {cfg}")
        data = json.loads(cfg.read_text())
        self.assertIsInstance(data, dict)


if __name__ == "__main__":
    unittest.main()
