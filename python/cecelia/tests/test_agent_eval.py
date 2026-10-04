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
sys.path.insert(0, str(_REPO / "scripts" / "agent_eval"))   # run_record imports its siblings by name
app_project = _load("app_project")
run_record = _load("run_record")
run_findings = _load("run_findings")

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


def _trace(*items) -> str:
    """A stream-json trace: ("text", s) | ("call", id, name, input) | ("result", id, text, is_error)."""
    rows = [{"type": "system", "subtype": "init", "model": "m", "session_id": "S1", "tools": []}]
    for it in items:
        if it[0] == "text":
            rows.append({"type": "assistant", "message": {"content": [{"type": "text", "text": it[1]}]}})
        elif it[0] == "call":
            rows.append({"type": "assistant", "message": {"content": [
                {"type": "tool_use", "id": it[1], "name": f"mcp__cecelia-autonomous__{it[2]}", "input": it[3]}]}})
        else:
            rows.append({"type": "user", "message": {"content": [
                {"type": "tool_result", "tool_use_id": it[1], "content": it[2], "is_error": it[3]}]}})
    rows.append({"type": "result", "subtype": "success", "total_cost_usd": 1.5, "num_turns": 9,
                 "result": "Done."})
    return "\n".join(json.dumps(r) for r in rows)


_RECT = {"kind": "rectangle", "x_channel": "a", "y_channel": "b", "x_min": 0, "x_max": 1, "y_min": 0, "y_max": 2}


class TestAppProject(unittest.TestCase):
    def test_copies_every_run_image_into_one_set(self):
        with tempfile.TemporaryDirectory() as d:
            projects = pathlib.Path(d)
            for uid in ("imgA", "imgB"):
                (projects / "SRC" / "0" / uid / "raw.ome.zarr").mkdir(parents=True)
                (projects / "SRC" / "1" / uid).mkdir(parents=True)
                (projects / "SRC" / "1" / uid / "ccid.json").write_text(json.dumps(
                    {"uid": uid, "name": f"name {uid}", "filepath": {"default": "raw.ome.zarr", "_active": "default"},
                     "imChannelNames": {"default": ["c0"], "_active": "default"}}), encoding="utf-8")
            info = app_project.build(projects, "SRC", ["imgA", "imgB"], "Agent run x")
            self.assertEqual([im["sourceImageUid"] for im in info["images"]], ["imgA", "imgB"])
            root = pathlib.Path(info["projectDir"])
            members = json.loads((root / "1" / info["setUid"] / "ccid.json").read_text(encoding="utf-8"))
            self.assertEqual(members["image_uids"], [im["imageUid"] for im in info["images"]])
            for im in info["images"]:
                self.assertTrue((root / "0" / im["imageUid"] / "raw.ome.zarr").is_dir())
        legacy = {"imageUid": "c1", "imageName": "n", "source": {"projectUid": "SRC", "imageUid": "imgA"}}
        self.assertEqual(app_project.copy_images(legacy),
                         [{"imageUid": "c1", "imageName": "n", "sourceImageUid": "imgA"}])


class TestRunRecord(unittest.TestCase):
    IMAGES = [{"imageUid": "cp1", "imageName": "one", "sourceImageUid": "src1"}]

    def _decisions(self, *items):
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "trace.jsonl"
            path.write_text(_trace(*items), encoding="utf-8")
            return run_record.decisions(str(path), self.IMAGES)

    def test_step_of(self):
        self.assertEqual(run_record.step_of("cleanupImages.driftCorrect"), "cleanup")
        self.assertEqual(run_record.step_of("segment.cellpose"), "segment")
        self.assertEqual(run_record.step_of("segment.measureLabels"), "measure")
        self.assertEqual(run_record.step_of("tracking.bayesian_track_measures"), "track")
        self.assertEqual(run_record.step_of("behaviour.hmm"), "behaviour")

    def test_chain_nodes_become_decisions_with_their_states(self):
        nodes = [{"id": "s1", "fn": "segment.cellpose", "params": {"outputValueName": "T"}},
                 {"id": "s2", "fn": "segment.cellpose", "params": {"outputValueName": "B"}},
                 {"id": "t1", "fn": "tracking.bayesian_track_measures", "params": {"valueName": "T"}}]
        dec = self._decisions(
            ("call", "c0", "get_image_info", {"project_uid": "P", "image_uid": "cp1"}),
            ("result", "c0", "{}", False),
            ("call", "c1", "create_chain", {"name": "bad (name)", "nodes": nodes}),
            ("result", "c1", "HTTP 400: Invalid chain name", True),
            ("text", "Fixing the name."),
            ("call", "c2", "create_chain", {"name": "ok", "nodes": nodes}),
            ("result", "c2", "{}", False),
            ("call", "c3", "run_chain", {"chain_name": "ok", "image_uids": ["cp1"]}),
            ("result", "c3", json.dumps({"runId": "R1"}), False),
            ("call", "c4", "wait_for_chain", {"run_id": "R1"}),
            ("result", "c4", json.dumps({"imageStates": {"cp1": {"s1": "done", "s2": "done", "t1": "failed"}}}), False))
        seg, trk, final = dec["sections"]
        self.assertEqual((seg["id"], seg["step"], seg["images"]), ("d01", "segment", ["src1"]))
        self.assertEqual(len(seg["units"]), 2)                     # same fn, one chain → one decision
        self.assertEqual(seg["lookedAt"], ["get_image_info(image_uid=src1)"])   # copy uid → source uid
        self.assertEqual(seg["said"], ["Fixing the name."])
        self.assertIn("HTTP 400", seg["triedFirst"][0])
        self.assertEqual(trk["units"][0]["outcome"], {"src1": "failed"})
        self.assertEqual(final["step"], "report")
        self.assertEqual([e["tool"] for e in dec["toolErrors"]], ["create_chain"])
        self.assertEqual(dec["sessionId"], "S1")

    def test_gates_merge_until_something_is_read_between(self):
        gate = lambda i, name: [("call", f"g{i}", "add_gate", {"image_uid": "cp1", "value_name": "T",   # noqa: E731
                                                              "name": name, "gate": _RECT}),
                                ("result", f"g{i}", "{}", False)]
        dec = self._decisions(*gate(1, "a"), *gate(2, "b"),
                              ("call", "h", "gate_histogram", {"image_uid": "cp1", "value_name": "T"}),
                              ("result", "h", "{}", False), *gate(3, "c"))
        gates = [s for s in dec["sections"] if s["step"] == "gate"]
        self.assertEqual([len(s["units"]) for s in gates], [2, 1])
        self.assertIn("add `/a` on T: rectangle a (linear) 0–1 × b (linear) 0–2", gates[0]["units"][0]["did"])

    def test_render_heads_each_section_and_lists_errors_apart(self):
        dec = self._decisions(("call", "x", "set_gate", {"image_uid": "cp1", "value_name": "T", "path": "/a"}),
                              ("result", "x", "Error executing tool set_gate", True))
        run = {"projectUid": "CP", "projectName": "Agent run x", "images": self.IMAGES,
               "source": {"projectUid": "SRC"}}
        md = run_record.render(run, {"brief": "b", "canary": {"intact": True}}, dec,
                               {"d01": "because"}, {})
        self.assertIn("### d01 · report · all · final message", md)
        self.assertIn("> because", md)
        self.assertIn("## Tool errors (unscored)\n\n- `set_gate`: Error executing tool set_gate", md)


class TestRunFindings(unittest.TestCase):
    def test_key_is_short_and_stable_across_runs(self):
        a = run_findings.finding_key("set_gate", "Error executing tool set_gate: HTTP 500: no pop '/a' on Y6Xkl0/9u2DLD at 2026-10-04T00:31:40")
        b = run_findings.finding_key("set_gate", "Error executing tool set_gate: HTTP 500: no pop '/b' on yMCypX/0OPUYX at 2026-10-05T01:02:03")
        self.assertEqual(a, b)
        self.assertRegex(a, r"^run-[0-9a-f]{10}$")
        self.assertNotEqual(a, run_findings.finding_key("add_gate", "HTTP 500: no pop '/a'"))

    def test_misuse_is_a_4xx_with_a_reason(self):
        self.assertTrue(run_findings.is_agent_misuse("Error executing tool create_chain: HTTP 400: Invalid chain name"))
        self.assertFalse(run_findings.is_agent_misuse("Error executing tool x: HTTP 500: boom"))
        self.assertFalse(run_findings.is_agent_misuse("Error executing tool set_gate"))   # lost message → logged
        self.assertFalse(run_findings.is_agent_misuse("HTTP 404:"))

    def test_tool_findings_one_per_key_misuse_dropped(self):
        dec = {"toolErrors": [{"tool": "set_gate", "error": "Error executing tool set_gate"}] * 3 +
               [{"tool": "create_chain", "error": "HTTP 400: Invalid chain name 'x/y'"}]}
        got = run_findings.tool_findings(dec, "R1")
        self.assertEqual([(f["tool"], f["error"]) for f in got], [("set_gate", "(no error text)")])
        self.assertNotIn("file", got[0])

    def test_backend_errors_in_the_window_with_their_repo_frame(self):
        import datetime as dt
        logs = {"logs": [
            {"level": "error", "ts": "2026-10-04T00:31:00Z", "message": "KeyError: pop",
             "detail": "Stacktrace:\n [1] f @ /home/u/cecelia/api/src/gating_api.jl:412\n"},
            {"level": "error", "ts": "2026-10-04T05:00:00Z", "message": "later, not this run", "detail": ""},
            {"level": "info", "ts": "2026-10-04T00:31:00Z", "message": "fine", "detail": ""}]}
        orig = run_findings.run_record._get
        run_findings.run_record._get = lambda *a, **k: logs
        try:
            start = dt.datetime(2026, 10, 4, 0, 30, tzinfo=dt.timezone.utc)
            got = run_findings.backend_findings("http://x", start, start + dt.timedelta(minutes=20), "R1")
        finally:
            run_findings.run_record._get = orig
        self.assertEqual([(f["file"], f["line"]) for f in got], [("api/src/gating_api.jl", 412)])

    def test_emit_writes_one_row_per_finding_with_the_run_commit(self):
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d) / "20261004T003001"
            root.mkdir()
            (root / "trace.jsonl").write_text(_trace(("call", "x", "set_gate", {"image_uid": "cp1"}),
                                                     ("result", "x", "Error executing tool set_gate", True)),
                                              encoding="utf-8")
            (root / "run.json").write_text(json.dumps({"projectUid": "CP", "images": TestRunRecord.IMAGES,
                                                       "source": {"projectUid": "SRC"}}), encoding="utf-8")
            (root / "record.json").write_text(json.dumps({"codeSha": "abc1234"}), encoding="utf-8")   # not in this repo: kept
            log = pathlib.Path(d) / "events.jsonl"
            run_findings.emit(root, None, log_path=log)
            rows = [json.loads(ln) for ln in log.read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(rows), 1)
        r = rows[0]
        self.assertEqual((r["event"], r["commit"], r["branch"], r["session"]), ("agent_run_finding", "abc1234", None, "S1"))
        self.assertEqual(set(r["payload"]), {"key", "tool", "error", "desc"})

    def test_short_sha_expands_to_the_full_one(self):
        import subprocess
        head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(_REPO), capture_output=True, text=True).stdout.strip()
        if not head:
            self.skipTest("not a git checkout")
        self.assertEqual(run_findings.full_sha(head[:8]), head)


if __name__ == "__main__":
    unittest.main()
