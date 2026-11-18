"""Tests for `scripts/judge/fixture_requests.py` and `scripts/fixture_cut.py` — open fixture requests
matched against a projects dir, and a match cut under the size cap.

The source projects are the committed `test-data/projects` (never a real projects dir); cuts go to a
temp test-data root. Nothing spawns `claude` or `gh`. Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import json
import os
import pathlib
import sys
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
_JUDGE = _REPO / "scripts" / "judge"
PROJECTS = _REPO / "test-data" / "projects"
# MAINT-EXEMPT: the matcher's own tests name the committed images it should find
KDIEEM = PROJECTS / "testpr" / "1" / "KDIeEm"


def _load(name, where=_JUDGE):
    spec = importlib.util.spec_from_file_location(name, where / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class PropertiesTest(unittest.TestCase):
    def setUp(self):
        self.fr = _load("fixture_requests")

    def test_an_images_properties_are_read_off_its_files(self):
        p = self.fr.image_properties(KDIEEM)
        self.assertEqual(p["image"], "KDIeEm")
        self.assertEqual(p["cells"], 1377)
        self.assertLessEqual({"cells", "movie", "3d", "tracked", "track-table", "real"}, set(p["properties"]))
        # no pixel size and ch0..ch3: exactly what made it a stand-in
        self.assertNotIn("calibrated", p["properties"])
        self.assertNotIn("marker-names", p["properties"])
        self.assertNotIn("still", p["properties"])

    def test_scan_finds_every_image_with_a_cell_table(self):
        found = {im["image"] for im in self.fr.scan(PROJECTS)}
        self.assertEqual(found, {"KDIeEm", "CtMsh1"})      # ZARRFMT has stores, no cell table
        self.assertEqual(self.fr.scan(None), [])
        self.assertEqual(self.fr.scan(PROJECTS / "nope"), [])


class MatchTest(unittest.TestCase):
    def setUp(self):
        self.fr = _load("fixture_requests")
        self.images = self.fr.scan(PROJECTS)
        self.requests = {
            "movie": {"properties": ["cells", "movie", "tracked"], "why": "w", "synthetic": False},
            "still-structure": {"properties": ["cells", "still", "structure"], "why": "w", "synthetic": False},
            "calibrated-still": {"properties": ["cells", "still", "calibrated"], "why": "w", "synthetic": False},
            "confocal": {"properties": ["cells"], "technology": "confocal", "why": "w", "synthetic": False},
        }

    def test_every_checkable_property_and_the_technology_must_match(self):
        m = self.fr.match(self.requests, self.images)
        self.assertEqual([im["image"] for im in m["movie"]], ["KDIeEm"])
        # `structure` can't be read off a file: it is left to confirm, not a reason to drop the image
        self.assertEqual([im["image"] for im in m["still-structure"]], ["CtMsh1"])
        self.assertEqual(m["calibrated-still"], [])          # CONTMESH is 1.0 µm/px in x
        self.assertEqual(m["confocal"], [])                  # no image says confocal

    def test_the_published_summary_carries_no_names(self):
        m = self.fr.match(self.requests, self.images)
        s = self.fr.summary(self.requests, m, configured=True)
        text = json.dumps(s)
        for name in ("KDIeEm", "CtMsh1", "testpr", "CONTMESH", str(PROJECTS)):
            self.assertNotIn(name, text)
        by = {r["id"]: r for r in s["requests"]}
        self.assertEqual(by["movie"]["candidates"], 1)
        self.assertEqual(by["still-structure"]["confirm"], ["structure"])

    def test_no_source_is_reported_not_guessed(self):
        with mock.patch.dict(os.environ, {}, clear=False):
            os.environ.pop(self.fr.ENV, None)
            s = self.fr.run(None, write=False)
        self.assertFalse(s["configured"])
        self.assertTrue(all(r["candidates"] == 0 for r in s["requests"]))
        self.assertIn("multiplex-structure-still", {r["id"] for r in s["requests"]})

    def test_the_candidate_list_is_written_locally(self):
        with tempfile.TemporaryDirectory() as tmp, \
                mock.patch.object(self.fr, "state_dir", lambda: pathlib.Path(tmp)):
            self.fr.run(PROJECTS, write=True)
            local = json.loads((pathlib.Path(tmp) / "fixture-candidates.json").read_text(encoding="utf-8"))
        self.assertEqual(local["source"], str(PROJECTS))
        self.assertIn("multiplex-structure-still", local["matches"])


class RecordTest(unittest.TestCase):
    def test_the_record_lists_requests_and_counts(self):
        rec = _load("record")
        fixtures = {"configured": True, "requests": [
            {"id": "a", "candidates": 2, "confirm": ["structure"], "synthetic": False},
            {"id": "b", "candidates": 0, "confirm": [], "synthetic": True}]}
        record = rec.build("2026-11-18", ts="2026-11-18T00:00:00Z", sha="0" * 40, bugs=[], rules=[],
                           proposals=[], spend={"total_usd": 0}, fixture_requests=fixtures)
        self.assertEqual(rec.validate(record), [])
        md = rec._render_fixture_requests(record)
        self.assertIn("- `a` — 2 candidate image(s) · confirm by eye: structure", md)
        self.assertIn("- `b` — synthetic — an agent builds it", md)
        unconfigured = dict(record, fixture_requests=dict(fixtures, configured=False))
        self.assertTrue(any("No source projects configured" in line for line in rec._render_fixture_requests(unconfigured)))


class CutTest(unittest.TestCase):
    def setUp(self):
        sys.path.insert(0, str(_JUDGE))
        self.cut = _load("fixture_cut", _REPO / "scripts")
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()
        sys.path.remove(str(_JUDGE))

    def test_a_cut_fits_the_cap_keeps_a_window_and_drops_the_name(self):
        c = self.cut.cut(KDIEEM, "movie", test_data=self.root, max_bytes=100_000)
        out = pathlib.Path(c["dir"])
        table = out / "labelProps" / "B.h5ad"
        self.assertLessEqual(table.stat().st_size, 100_000)
        self.assertGreater(c["cells"], 0)
        self.assertLess(c["cells"], 1377)
        ccid = json.loads((out / "ccid.json").read_text(encoding="utf-8"))
        self.assertEqual(ccid["name"], "fixture-KDIeEm")
        self.assertNotIn("filepath", ccid)
        self.assertEqual(ccid["label_props"], {"B": "B.h5ad", "_active": "B"})
        self.assertTrue((out / "gating" / "B__trackclust.json").is_file())
        self.assertIn("cells", c["properties"])
        # the window is spatial: every kept cell is nearer the centre than every dropped one
        from cecelia.utils.label_props_utils import LabelPropsView
        import numpy as np
        full = LabelPropsView(str(KDIEEM / "labelProps" / "B.h5ad")).only_centroid_cols().as_df()
        kept = set(LabelPropsView(str(table)).labels())
        xy = full[["centroid_x", "centroid_y"]].to_numpy(dtype=float)
        d = np.hypot(*(xy - np.median(xy, axis=0)).T)
        labels = full["label"].to_numpy() if "label" in full else LabelPropsView(str(KDIEEM / "labelProps" / "B.h5ad")).labels()
        inside = np.array([lb in kept for lb in labels])
        self.assertLessEqual(d[inside].max(), d[~inside].min())

    def test_the_entry_names_what_it_saw(self):
        c = self.cut.cut(KDIEEM, "movie", test_data=self.root, max_bytes=100_000)
        entry = self.cut.toml_entry(c)
        self.assertIn('[fixtures.KDIeEm]', entry)
        self.assertIn('dir = "FIXPRJ/1/KDIeEm"', entry)
        self.assertIn('"movie"', entry)
        with self.assertRaises(SystemExit):           # never over an existing cut
            self.cut.cut(KDIEEM, "movie", test_data=self.root)


if __name__ == "__main__":
    unittest.main()
