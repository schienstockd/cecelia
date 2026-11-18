"""The Python side of the fixture registry reads fixtures.toml / NEEDS.toml the way the Julia side does.

The facts behind each property are checked once, in app/test/suite/fixture_registry.jl.
"""
import json
import os
import unittest

from cecelia.tests.fixture_registry import fixture_for, needs, registered_fixtures, vocabulary


class FixtureRegistryTest(unittest.TestCase):
    def test_every_property_is_in_the_vocabulary(self):
        vocab = vocabulary()
        for fx in registered_fixtures():
            self.assertLessEqual(fx.properties, vocab.keys(), fx.uid)
        for nid, n in needs().items():
            self.assertLessEqual(set(n["properties"]), vocab.keys(), nid)

    def test_returns_a_match_never_a_stand_in(self):
        fx = fixture_for("cells", "movie", "tracked")
        # MAINT-EXEMPT: the registry's own tests name fixtures by uid on purpose
        self.assertEqual(fx.uid, "KDIeEm")
        self.assertTrue(os.path.isfile(fx.table))
        # the calibrated variant: a temp copy whose ccid.json carries the pixel size
        cal = fixture_for("cells", "movie", "calibrated")
        self.assertEqual(cal.uid, "KDIeEm-cal")
        with open(os.path.join(cal.dir, "ccid.json"), encoding="utf-8") as f:
            self.assertEqual(json.load(f)["meta"]["PhysicalSizeX"], "0.5")
        self.assertNotEqual(cal.project, fixture_for("cells", "movie", "calibrated").project)

    def test_no_match_skips_on_a_filed_need_and_raises_without_one(self):
        with self.assertWarns(UserWarning):
            fixture_for("cells", "still", "calibrated", "structure", need="multiplex-structure-still")
        self.assertIsNone(fixture_for("cells", "still", "calibrated", "structure", need="multiplex-structure-still"))
        with self.assertRaises(LookupError):
            fixture_for("cells", "still", "calibrated", "structure")
        with self.assertRaises(ValueError):
            fixture_for("cells", need="no-such-request")
        with self.assertRaises(ValueError):
            fixture_for("calibratd")


if __name__ == "__main__":
    unittest.main()
