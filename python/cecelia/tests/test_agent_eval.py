"""Tests for `scripts/agent_eval/` — synthetic fixture generator + scorer (AGENT_OVERNIGHT_PLAN P1).

Checkpoint from the plan: the scorer gives 1.0 on ground-truth-as-prediction and drops on a perturbed
copy (dropped detections, broken links, scrambled states).
"""
from __future__ import annotations

import importlib.util
import json
import pathlib
import sys
import tempfile
import unittest

import numpy as np
import pandas as pd

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load(name):
    key = f"agent_eval_{name}"
    spec = importlib.util.spec_from_file_location(key, _REPO / "scripts" / "agent_eval" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[key] = mod                 # dataclasses resolve their module through sys.modules
    spec.loader.exec_module(mod)
    return mod


fixture = _load("fixture")
score = _load("score")
runner = _load("run_overnight")

SPEC = fixture.FixtureSpec(size=96, n_frames=24, n_cells=6, min_dwell=6)


def _gt():
    return pd.DataFrame(fixture.simulate(SPEC, seed=3))


def _as_pred(gt: pd.DataFrame) -> pd.DataFrame:
    """Ground truth dressed as a run's output: label per object, track_id = cell, numeric states."""
    p = gt.copy()
    p["label"] = np.arange(1, len(p) + 1)
    p["track_id"] = p["cell"].astype(float) + 100
    p["hmm"] = p["state"].map({fixture.MIGRATING: 2.0, fixture.ARRESTED: 1.0})
    return p.drop(columns=["cell", "state"])


class TestFixture(unittest.TestCase):
    def test_deterministic(self):
        self.assertEqual(fixture.simulate(SPEC, 3), fixture.simulate(SPEC, 3))
        self.assertNotEqual(fixture.simulate(SPEC, 3), fixture.simulate(SPEC, 4))

    def test_trajectories_are_plausible(self):
        gt = _gt()
        self.assertEqual(len(gt), SPEC.n_cells * SPEC.n_frames)
        self.assertEqual(set(gt["state"]), {fixture.MIGRATING, fixture.ARRESTED})
        for _, g in gt.groupby("t"):                          # cells never overlap
            xy = g[["y", "x"]].to_numpy()
            d = np.hypot(*(xy[:, None] - xy[None]).transpose(2, 0, 1))
            np.fill_diagonal(d, np.inf)
            self.assertGreaterEqual(d.min(), 2 * SPEC.radius_px)
        for _, g in gt.groupby("cell"):                       # each regime lasts ≥ min_dwell frames
            s = g.sort_values("t")["state"].to_numpy()
            runs = np.diff(np.flatnonzero(np.r_[True, s[1:] != s[:-1], True]))
            self.assertGreaterEqual(runs.min(), SPEC.min_dwell)

    def test_migrating_moves_further_than_arrested(self):
        gt = _gt().sort_values(["cell", "t"])
        step = np.hypot(gt.groupby("cell")["y"].diff(), gt.groupby("cell")["x"].diff())
        by = step.groupby(gt["state"]).mean()
        self.assertGreater(by[fixture.MIGRATING], 4 * by[fixture.ARRESTED])

    def test_render_and_write(self):
        rows = fixture.simulate(SPEC, 3)
        image, labels = fixture.render(SPEC, rows, 3)
        self.assertEqual(image.shape, (SPEC.n_frames, SPEC.size, SPEC.size))
        self.assertEqual(image.dtype, np.uint16)
        self.assertEqual(len(np.unique(labels[0])) - 1, SPEC.n_cells)
        with tempfile.TemporaryDirectory() as d:
            m = fixture.write_fixture(d, n_images=1, seed=1, spec=SPEC)
            img = m["images"][0]
            for k in ("tif", "gt", "labels"):
                self.assertTrue(pathlib.Path(img[k]).is_file(), k)
            with open(pathlib.Path(d) / "fixture.json", encoding="utf-8") as f:
                self.assertEqual(json.load(f)["images"][0]["name"], "synth1")


class TestScore(unittest.TestCase):
    def test_ground_truth_scores_one(self):
        gt = _gt()
        s = score.score_image(gt, _as_pred(gt), SPEC.radius_px, state_col="hmm")
        self.assertEqual(s["segmentation"]["f1"], 1.0)
        self.assertEqual(s["tracking"]["link_recall"], 1.0)
        self.assertEqual(s["tracking"]["link_precision"], 1.0)
        self.assertEqual(s["tracking"]["n_tracks"], SPEC.n_cells)
        self.assertEqual(s["behaviour"]["accuracy"], 1.0)

    def test_dropped_detections_lower_recall_only(self):
        gt = _gt()
        pred = _as_pred(gt).sample(frac=0.75, random_state=0)
        s = score.score_image(gt, pred, SPEC.radius_px)["segmentation"]
        self.assertAlmostEqual(s["recall"], 0.75, places=2)
        self.assertEqual(s["precision"], 1.0)

    def test_broken_and_swapped_tracks(self):
        gt = _gt()
        pred = _as_pred(gt)
        half = pred["t"] >= SPEC.n_frames // 2
        broken = pred.copy()
        broken.loc[half, "track_id"] += 1000                  # every track split in two
        s = score.score_image(gt, broken, SPEC.radius_px)["tracking"]
        self.assertLess(s["link_recall"], 1.0)
        self.assertEqual(s["link_precision"], 1.0)            # no link joins two cells
        swapped = pred.copy()                                  # tracks 100 and 101 swap identity midway
        a, b = swapped["track_id"] == 100, swapped["track_id"] == 101
        swapped.loc[half & a, "track_id"], swapped.loc[half & b, "track_id"] = 101.0, 100.0
        s = score.score_image(gt, swapped, SPEC.radius_px)["tracking"]
        self.assertLess(s["link_precision"], 1.0)

    def test_states_mapping_and_noise(self):
        gt = _gt()
        pred = _as_pred(gt)
        pred["hmm"] = 3.0 - pred["hmm"]                       # relabelled states still score 1.0
        self.assertEqual(score.score_image(gt, pred, SPEC.radius_px, "hmm")["behaviour"]["accuracy"], 1.0)
        rng = np.random.default_rng(0)
        pred["hmm"] = rng.choice([1.0, 2.0], len(pred))
        self.assertLess(score.score_image(gt, pred, SPEC.radius_px, "hmm")["behaviour"]["accuracy"], 0.7)

    def test_z_distance_counts_when_scaled(self):
        gt = _gt().assign(z=5.0)
        pred = _as_pred(gt).assign(z=6.0)                    # one z step off
        self.assertEqual(score.score_image(gt, pred, SPEC.radius_px)["segmentation"]["f1"], 1.0)
        far = score.score_image(gt, pred, SPEC.radius_px, z_scale=2 * SPEC.radius_px)["segmentation"]
        self.assertEqual(far["tp"], 0)                       # a z step worth 2 radii: too far

    def test_reference_without_states_scores_tracking_only(self):
        full = _gt()
        s = score.score_image(full.drop(columns=["state"]), _as_pred(full), SPEC.radius_px, state_col="hmm")
        self.assertIsNone(s["behaviour"])
        self.assertEqual(s["tracking"]["link_recall"], 1.0)

    def test_far_predictions_do_not_match(self):
        gt = _gt()
        pred = _as_pred(gt)
        pred["x"] += 3 * SPEC.radius_px
        self.assertEqual(score.score_image(gt, pred, SPEC.radius_px)["segmentation"]["tp"], 0)


class TestRunner(unittest.TestCase):
    def test_result_line(self):
        msg = 'All done.\n\nRESULT {"valueName": "agent1", "stateColumn": "live.cell.hmm.state.agent1"}\n'
        self.assertEqual(runner.parse_result_line(msg)["valueName"], "agent1")
        self.assertEqual(runner.parse_result_line("`RESULT {\"valueName\": \"x\"}`")["valueName"], "x")
        self.assertIsNone(runner.parse_result_line("no answer"))
        self.assertIsNone(runner.parse_result_line("RESULT {not json"))

    def test_tool_errors_counted_from_stream(self):
        ev = lambda err: json.dumps({"type": "user", "message": {"content": [   # noqa: E731
            {"type": "tool_result", "is_error": err, "content": "x"}]}})
        stream = "\n".join([ev(True), ev(False), "not json", ev(True)])
        self.assertEqual(runner.tool_errors(stream), 2)

    def test_command_is_contained(self):
        root = pathlib.Path("/tmp/agent-night")
        cmd = runner.build_command("claude", root, 7.5, None)
        self.assertIn("--strict-mcp-config", cmd)            # no observer → no real projects
        self.assertEqual(cmd[cmd.index("--max-budget-usd") + 1], "7.5")
        settings = json.loads(cmd[cmd.index("--settings") + 1])
        self.assertIn(str(root), settings["sandbox"]["filesystem"]["allowWrite"])
        self.assertIn("Write(~/**)", settings["permissions"]["deny"])

    def test_new_value_names_and_headline(self):
        with tempfile.TemporaryDirectory() as d:
            ccid = pathlib.Path(d) / "1" / "img" / "ccid.json"
            ccid.parent.mkdir(parents=True)
            ccid.write_text(json.dumps({"label_props": {"default": "d.h5ad", "agent": "a.h5ad",
                                                        "_active": "agent"}}), encoding="utf-8")
            run = {"projectDir": d, "images": [{"uid": "img"}],
                   "snapshot": {"valueNames": {"img": ["default"]}}}
            self.assertEqual(runner.new_value_names(run), {"img": ["agent"]})
        seg = lambda f1: {"segmentation": {"f1": f1, "recall": f1}, "tracking": None}   # noqa: E731
        h = runner.headline({"a": {"declared": "x", "byValueName": {"x": seg(0.5), "y": seg(0.9)}},
                             "b": {"declared": None, "byValueName": {"y": seg(0.7), "z": seg(0.1)}}})
        self.assertEqual(h["images_scored"], 2)
        self.assertAlmostEqual(h["seg_f1"], 0.6)             # declared x on a, best-F1 y on b
        self.assertIsNone(h["link_recall"])


class TestCanary(unittest.TestCase):
    def test_changed_file_and_moved_active(self):
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            (root / "1" / "img").mkdir(parents=True)
            (root / "1" / "img" / "labels").mkdir()
            prior = root / "1" / "img" / "labels" / "default.zarr"
            prior.write_bytes(b"prior")
            ccid = root / "1" / "img" / "ccid.json"
            ccid.write_text(json.dumps({"label_props": {"default": "default.h5ad", "_active": "default"}}),
                            encoding="utf-8")
            before = score.snapshot(root)
            self.assertTrue(score.check_canary(before, score.snapshot(root))["intact"])
            (root / "1" / "img" / "labels" / "agent.zarr").write_bytes(b"new")   # new files are fine
            self.assertTrue(score.check_canary(before, score.snapshot(root))["intact"])
            ccid.write_text(json.dumps({"label_props": {"default": "default.h5ad", "agent": "a.h5ad",
                                                        "_active": "agent"}}), encoding="utf-8")
            c = score.check_canary(before, score.snapshot(root))
            self.assertFalse(c["intact"])
            self.assertEqual(c["active_moved"][0]["after"], "agent")
            prior.write_bytes(b"overwritten")
            self.assertIn("1/img/labels/default.zarr", score.check_canary(before, score.snapshot(root))["changed_files"])


if __name__ == "__main__":
    unittest.main()
