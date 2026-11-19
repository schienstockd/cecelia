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
import shutil
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
        self.assertEqual(found, {"KDIeEm", "KDIeEx", "CtMsh1"})   # ZARRFMT has stores, no cell table
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
        # smallest table first: KDIeEx (933 cells) before KDIeEm (1377)
        self.assertEqual([im["image"] for im in m["movie"]], ["KDIeEx", "KDIeEm"])
        # `structure` can't be read off a file: it is left to confirm, not a reason to drop the image
        self.assertEqual([im["image"] for im in m["still-structure"]], ["CtMsh1"])
        self.assertEqual(m["calibrated-still"], [])          # CONTMESH is 1.0 µm/px in x
        self.assertEqual(m["confocal"], [])                  # no image says confocal

    def test_the_published_summary_carries_no_names(self):
        m = self.fr.match(self.requests, self.images)
        s = self.fr.summary(self.requests, m, configured=True)
        text = json.dumps(s)
        for name in ("KDIeEm", "KDIeEx", "CtMsh1", "testpr", "CONTMESH", str(PROJECTS)):
            self.assertNotIn(name, text)
        by = {r["id"]: r for r in s["requests"]}
        self.assertEqual(by["movie"]["candidates"], 2)
        self.assertEqual(by["still-structure"]["confirm"], ["structure"])

    def test_no_source_is_reported_not_guessed(self):
        with tempfile.TemporaryDirectory() as cfg, mock.patch.dict(os.environ, {}, clear=False), \
                mock.patch.object(self.fr, "config_dir", lambda root=None: cfg):    # a config with no custom.toml
            os.environ.pop(self.fr.ENV, None)
            s = self.fr.run(None, write=False)
        self.assertFalse(s["configured"])
        self.assertTrue(all(r["candidates"] == 0 for r in s["requests"]))
        self.assertIn("multiplex-structure-still", {r["id"] for r in s["requests"]})

    def test_the_dev_configs_projects_dir_is_the_fallback(self):
        with tempfile.TemporaryDirectory() as cfg, mock.patch.dict(os.environ, {}, clear=False), \
                mock.patch.object(self.fr, "config_dir", lambda root=None: cfg):
            os.environ.pop(self.fr.ENV, None)
            (pathlib.Path(cfg) / "custom.toml").write_text(
                f'[runner]\nport = 1\n\n[dirs]\nprojects = "{PROJECTS.as_posix()}"\n', encoding="utf-8")
            self.assertEqual(self.fr.source_dir(), PROJECTS)
            with mock.patch.dict(os.environ, {self.fr.ENV: "/elsewhere"}):        # the variable still wins
                self.assertEqual(self.fr.source_dir(), pathlib.Path("/elsewhere"))
            self.assertEqual(self.fr.source_dir(pathlib.Path("/arg")), pathlib.Path("/arg"))
            (pathlib.Path(cfg) / "custom.toml").write_text("[dirs]\n", encoding="utf-8")
            self.assertIsNone(self.fr.source_dir())

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
        staged = dict(fixtures, cuts_ready=1, requests=[dict(fixtures["requests"][0], cut="ready"),
                                                       dict(fixtures["requests"][1], cut="failed")])
        md = rec._render_fixture_requests(dict(record, fixture_requests=staged))
        self.assertIn("- `a` — 2 candidate image(s) · confirm by eye: structure · cut ready", md)
        self.assertTrue(any(line.startswith("**1 cut(s) ready for review**") for line in md))
        self.assertTrue(any("failed to cut" in line for line in md))


REQUESTS = {
    "movie-structure": {"properties": ["cells", "movie", "structure"], "why": "w", "synthetic": False},
    "synthetic-movie": {"properties": ["cells", "movie"], "why": "w", "synthetic": True},
    "unmatched": {"properties": ["cells", "multiplex"], "why": "w", "synthetic": False},
}
NEEDS = """# header kept
[needs.movie-structure]
properties = ["cells", "movie", "structure"]
why = "w"
status = "open"
synthetic = false

[needs.unmatched]
properties = ["cells", "multiplex"]
why = "w"
status = "open"
synthetic = false
"""


class _StageFixture(unittest.TestCase):
    """A temp staging root and a temp test-data root; the sources are the committed projects."""
    def setUp(self):
        sys.path.insert(0, str(_JUDGE))
        self.fr = _load("fixture_requests")
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name) / "cuts"
        self.test_data = pathlib.Path(self.tmp.name) / "test-data"
        (self.test_data / "projects").mkdir(parents=True)
        (self.test_data / "NEEDS.toml").write_text(NEEDS, encoding="utf-8")
        (self.test_data / "fixtures.toml").write_text("[vocabulary]\ncells = [true, \"x\"]\n", encoding="utf-8")
        self.matches = self.fr.match(REQUESTS, self.fr.scan(PROJECTS))

    def tearDown(self):
        self.tmp.cleanup()
        sys.path.remove(str(_JUDGE))


class StageTest(_StageFixture):
    def test_a_cut_is_staged_outside_the_repo_once_per_request(self):
        got = self.fr.stage_cuts(REQUESTS, self.matches, root=self.root)
        self.assertEqual(got, {"movie-structure": "ready"})           # synthetic and unmatched: nothing
        c = self.fr.staged("movie-structure", self.root)
        first = self.matches["movie-structure"][0]
        self.assertEqual(c["source"], first["dir"])                   # the smallest candidate
        self.assertEqual(c["confirm"], ["structure"])
        self.assertTrue(pathlib.Path(c["dir"]).is_relative_to(self.root))
        self.assertTrue((self.root / "movie-structure" / "projects" / c["project"] / "1" / c["uid"] / "ccid.json").is_file())
        self.assertIn(f"[fixtures.{c['uid']}]", c["entry"])
        self.assertFalse((_REPO / "test-data" / "projects" / c["project"]).exists())
        # staged already: the next pass keeps it and cuts nothing
        with mock.patch.object(self.fr, "_fixture_cut", side_effect=AssertionError("cut again")):
            self.assertEqual(self.fr.stage_cuts(REQUESTS, self.matches, root=self.root), {"movie-structure": "ready"})

    def test_a_candidate_that_will_not_cut_falls_through_to_the_next(self):
        broken = dict(self.matches["movie-structure"][0], dir=str(pathlib.Path(self.tmp.name) / "gone"))
        matches = {"movie-structure": [broken, *self.matches["movie-structure"]]}
        self.assertEqual(self.fr.stage_cuts(REQUESTS, matches, root=self.root), {"movie-structure": "ready"})
        self.assertEqual(self.fr.staged("movie-structure", self.root)["source"], self.matches["movie-structure"][0]["dir"])
        # every candidate broken: `failed`, the reasons kept locally, and the next pass tries again
        shutil.rmtree(self.root)
        self.assertEqual(self.fr.stage_cuts(REQUESTS, {"movie-structure": [broken]}, root=self.root),
                         {"movie-structure": "failed"})
        failed = json.loads((self.root / "movie-structure" / "failed.json").read_text(encoding="utf-8"))
        self.assertEqual(failed["errors"][0]["image"], broken["dir"])
        self.assertIsNone(self.fr.staged("movie-structure", self.root))

    def test_the_stores_are_cropped_only_for_a_request_that_names_one(self):
        from cecelia.tests.fixture_registry import fixture_for
        fx = fixture_for("cells", "image-store", "label-store")
        reqs = {"with-store": {"properties": ["cells", "image-store"], "why": "w", "synthetic": False},
                "table-only": {"properties": ["cells", "still"], "why": "w", "synthetic": False}}
        im = next(i for i in self.fr.scan(PROJECTS) if i["image"] == fx.uid)
        got = self.fr.stage_cuts(reqs, {k: [im] for k in reqs}, root=self.root)
        self.assertEqual(got, {"with-store": "ready", "table-only": "ready"})
        self.assertIsNotNone(self.fr.staged("with-store", self.root)["box"])
        self.assertIsNone(self.fr.staged("table-only", self.root)["box"])

    def test_a_cut_that_loses_an_asked_property_is_not_staged(self):
        from cecelia.tests.fixture_registry import fixture_for
        fx = fixture_for("cells", "image-store", "label-store")
        im = next(i for i in self.fr.scan(PROJECTS) if i["image"] == fx.uid)
        # the crop keeps 4 channels: an image with more, asked to stay `multiplex`, would come out without it
        reqs = {"r": {"properties": ["cells", "image-store", "marker-names"], "why": "w", "synthetic": False}}
        cut = self.fr._fixture_cut()
        real = cut.cut
        def drops(*a, **k):
            c = real(*a, **k)
            return dict(c, properties=[p for p in c["properties"] if p != "marker-names"])
        with mock.patch.object(self.fr, "_fixture_cut", lambda: mock.Mock(cut=drops, toml_entry=cut.toml_entry)):
            self.assertEqual(self.fr.stage_cuts(reqs, {"r": [im]}, root=self.root), {"r": "failed"})
        failed = json.loads((self.root / "r" / "failed.json").read_text(encoding="utf-8"))
        self.assertIn("no marker-names", failed["errors"][0]["error"])

    def test_the_summary_says_a_cut_is_ready_and_names_nothing(self):
        cuts = self.fr.stage_cuts(REQUESTS, self.matches, root=self.root)
        s = self.fr.summary(REQUESTS, self.matches, configured=True, cuts=cuts)
        self.assertEqual(s["cuts_ready"], 1)
        self.assertEqual({r["id"]: r.get("cut") for r in s["requests"]},
                         {"movie-structure": "ready", "synthetic-movie": None, "unmatched": None})
        text = json.dumps(s)
        # MAINT-EXEMPT: names the source images only to assert the published summary leaves them out
        for name in ("KDIeEm", "KDIeEx", "testpr", "FXKD", str(self.root), str(PROJECTS)):
            self.assertNotIn(name, text)

    def test_the_listing_points_at_the_staged_cut_and_its_accept_command(self):
        self.fr.stage_cuts(REQUESTS, self.matches, root=self.root)
        out = self.fr.render(REQUESTS, self.matches, root=self.root)
        self.assertIn("CUT READY", out)
        self.assertIn("pixi run fixture-accept -- movie-structure --confirm structure", out)
        self.assertNotIn("fixture-cut -- --image", out.split("unmatched")[0].split("synthetic-movie")[0])


class AcceptTest(_StageFixture):
    def setUp(self):
        super().setUp()
        sys.path.insert(0, str(_REPO / "scripts"))
        self.acc = _load("fixture_accept", _REPO / "scripts")
        self.fr.stage_cuts(REQUESTS, self.matches, root=self.root)
        self.c = self.fr.staged("movie-structure", self.root)

    def tearDown(self):
        sys.path.remove(str(_REPO / "scripts"))
        super().tearDown()

    def accept(self, **kw):
        return self.acc.accept("movie-structure", test_data=self.test_data, root=self.root, **kw)

    def test_a_property_no_file_shows_must_be_confirmed(self):
        with self.assertRaises(SystemExit):
            self.accept()
        with self.assertRaises(SystemExit):                # not one the request asks for
            self.accept(confirm=("structure", "regions"))
        self.assertFalse((self.test_data / "projects" / self.c["project"]).exists())
        self.assertIn('status = "open"', (self.test_data / "NEEDS.toml").read_text(encoding="utf-8"))

    def test_accepting_registers_the_fixture_and_marks_the_request_met(self):
        import tomllib
        self.accept(confirm=("structure",))
        uid = self.c["uid"]
        self.assertTrue((self.test_data / "projects" / self.c["project"] / "1" / uid / "ccid.json").is_file())
        with open(self.test_data / "fixtures.toml", "rb") as f:
            entry = tomllib.load(f)["fixtures"][uid]
        self.assertEqual(entry["dir"], f"{self.c['project']}/1/{uid}")
        # the registry's own rule: a met request's properties are all the fixture's
        self.assertLessEqual(set(REQUESTS["movie-structure"]["properties"]), set(entry["properties"]))
        needs_text = (self.test_data / "NEEDS.toml").read_text(encoding="utf-8")
        with open(self.test_data / "NEEDS.toml", "rb") as f:
            needs = tomllib.load(f)["needs"]
        self.assertEqual(needs["movie-structure"]["status"], f"met: {uid}")
        self.assertEqual(needs["unmatched"]["status"], "open")       # only that request
        self.assertTrue(needs_text.startswith("# header kept"))      # comments stay
        self.assertFalse((self.root / "movie-structure").exists())
        # nothing left to accept, and a second copy of that image is refused
        with self.assertRaises(SystemExit):
            self.accept(confirm=("structure",))

    def test_discard_drops_the_staged_cut(self):
        self.assertTrue(self.acc.discard("movie-structure", root=self.root))
        self.assertIsNone(self.fr.staged("movie-structure", self.root))
        self.assertFalse(self.acc.discard("movie-structure", root=self.root))


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
        self.assertIn('dir = "FXKDIeEm/1/KDIeEm"', entry)
        self.assertIn('"movie"', entry)
        with self.assertRaises(SystemExit):           # never over an existing cut
            self.cut.cut(KDIEEM, "movie", test_data=self.root)

    def test_stores_are_cropped_to_one_box_with_the_cells_inside_it(self):
        from cecelia.utils import zarr_utils
        from cecelia.utils.label_props_utils import LabelPropsView
        import numpy as np
        from cecelia.tests.fixture_registry import fixture_for
        fx = fixture_for("cells", "image-store", "label-store")      # what a store crop needs
        src = pathlib.Path(fx.dir)
        c = self.cut.cut(src, "cells-with-image-store", test_data=self.root, xy=8)
        out = pathlib.Path(c["dir"])
        vn = fx.value_name
        src_im = pathlib.Path(fx.image_dir) / "ccidImage.ome.zarr"
        src_lb = src / "labels" / f"{vn}.zarr"
        im = self.root / "projects" / f"FX{fx.uid}" / "0" / fx.uid / "ccidImage.ome.zarr"
        lb = out / "labels" / f"{vn}.zarr"
        # the box below is read off the source's own shape, so it holds for whichever fixture matches
        lvl = zarr_utils.open_as_zarr(str(src_im))[0][0]
        size = dict(zip(zarr_utils.read_axes(str(src_im)), lvl.shape))
        box = c["box"]
        # up to 4 channels and 16 planes from the start / around the cells, an 8 × 8 window in y/x
        self.assertEqual(box["c"], (0, min(size["c"], 4)))
        self.assertEqual(box["z"][1] - box["z"][0], min(size["z"], 16))
        self.assertEqual(box["y"][1] - box["y"][0], min(size["y"], 8))
        self.assertEqual(box["x"][1] - box["x"][0], min(size["x"], 8))
        self.assertLess(box["x"][1] - box["x"][0], size["x"])        # the window really cuts
        sl = lambda axes: tuple(slice(*box[a]) for a in axes)                      # noqa: E731
        for src, dst in ((src_im, im), (src_lb, lb)):
            axes = zarr_utils.read_axes(str(src))
            self.assertEqual(zarr_utils.read_axes(str(dst)), axes)
            self.assertEqual(zarr_utils.read_scale(str(dst)), zarr_utils.read_scale(str(src)))
            want = np.asarray(zarr_utils.open_as_zarr(str(src))[0][0][sl(axes)])
            got = np.asarray(zarr_utils.open_as_zarr(str(dst))[0][0][:])
            np.testing.assert_array_equal(got, want)
            # one chunk per frame and channel, so a 5-frame 4-channel box isn't one file over the cap
            chunks = dict(zip(axes, zarr_utils.open_as_zarr(str(dst))[0][0].chunks))
            self.assertTrue(all(chunks[a] == 1 for a in ("t", "c") if a in chunks))
            if dst == im:
                self.assertIn("c", chunks)                                   # the image has channels
        # the cells kept are the ones whose centroid is in the box, moved to its origin
        full = LabelPropsView(fx.table).only_centroid_cols().as_df()
        inside = np.ones(len(full), bool)
        for ax in ("x", "y", "z"):
            v = full[f"centroid_{ax}"].to_numpy(float)
            inside &= (v >= box[ax][0]) & (v < box[ax][1])
        self.assertTrue(0 < inside.sum() < len(full))
        cut_df = LabelPropsView(str(out / "labelProps" / f"{vn}.h5ad")).only_centroid_cols().as_df()
        self.assertEqual(sorted(cut_df["label"]), sorted(full["label"][inside]))
        joined = cut_df.merge(full, on="label", suffixes=("", "_src"))
        for ax in ("x", "y", "z"):
            np.testing.assert_allclose(joined[f"centroid_{ax}"], joined[f"centroid_{ax}_src"] - box[ax][0])
        labels = np.asarray(zarr_utils.open_as_zarr(str(lb))[0][0][:])
        for r in joined.itertuples():   # each kept cell's centroid voxel is its own label in the cut mask
            self.assertEqual(labels[int(r.centroid_z), int(r.centroid_y), int(r.centroid_x)], r.label)
        ccid = json.loads((out / "ccid.json").read_text(encoding="utf-8"))
        self.assertEqual(ccid["labels"], {vn: f"{vn}.zarr"})
        self.assertEqual(ccid["meta"]["SizeX"], "8")
        self.assertIn(f'image_dir = "FX{fx.uid}/0/{fx.uid}"', self.cut.toml_entry(c))
        self.assertIn("image-store", c["properties"])
        self.assertIn("label-store", c["properties"])

    def test_a_table_numbered_differently_from_its_mask_is_refused_and_leaves_nothing(self):
        from cecelia.tests.fixture_registry import fixture_for
        from cecelia.utils.atomic_io import write_h5ad_atomic
        from cecelia.utils.label_props_utils import LabelPropsView
        fx = fixture_for("cells", "image-store", "label-store")
        # the same project with every table label one more than its mask value
        src_proj = pathlib.Path(fx.dir).parent.parent
        proj = self.root / "src" / src_proj.name
        shutil.copytree(src_proj, proj)
        table = proj / "1" / fx.uid / "labelProps" / f"{fx.value_name}.h5ad"
        view = LabelPropsView(str(table))
        try:
            ad = view.adata.copy()
        finally:
            view.close()
        ad.obs.index = (ad.obs.index.astype(int) + 1).astype(str)
        write_h5ad_atomic(ad, str(table))
        out = self.root / "td"
        with self.assertRaises(SystemExit) as e:
            self.cut.cut(proj / "1" / fx.uid, "cells-with-image-store", test_data=out)
        self.assertIn("number cells differently", str(e.exception))
        self.assertFalse((out / "projects" / f"FX{fx.uid}").exists())
        # the honest table agrees with its mask, which is what lets the cut above be the only failure
        c = self.cut.cut(pathlib.Path(fx.dir), "cells-with-image-store", test_data=out)
        hit, n = self.cut.label_agreement(pathlib.Path(c["dir"]) / "labelProps" / f"{fx.value_name}.h5ad",
                                          pathlib.Path(c["dir"]) / "labels" / f"{fx.value_name}.zarr")
        self.assertEqual(hit, n)
        self.assertGreater(n, 0)

    def test_a_cut_with_a_file_over_the_cap_is_refused_and_leaves_nothing(self):
        from cecelia.tests.fixture_registry import fixture_for
        fx = fixture_for("cells", "image-store", "label-store")
        with self.assertRaises(SystemExit):
            self.cut.cut(pathlib.Path(fx.dir), "cells-with-image-store", test_data=self.root, max_bytes=64)
        self.assertFalse((self.root / "projects" / f"FX{fx.uid}").exists())

    def test_no_stores_cuts_the_table_alone(self):
        from cecelia.tests.fixture_registry import fixture_for
        fx = fixture_for("cells", "image-store", "label-store")
        c = self.cut.cut(pathlib.Path(fx.dir), "cells-with-image-store", test_data=self.root, stores=False)
        self.assertIsNone(c["box"])
        self.assertFalse((self.root / "projects" / f"FX{fx.uid}" / "0").exists())


if __name__ == "__main__":
    unittest.main()
