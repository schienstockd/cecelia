"""Tests for `scripts/agent_eval/` — synthetic fixture generator + scorer (AGENT_OVERNIGHT_PLAN P1).

Checkpoint from the plan: the scorer gives 1.0 on ground-truth-as-prediction and drops on a perturbed
copy (dropped detections, broken links, scrambled states).
"""
from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import pathlib
import sys
import tempfile
import unittest
from unittest import mock

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
stage_boards = _load("stage_boards")

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


class TestRunnerRateLimit(unittest.TestCase):
    """A run the usage limit stopped is recorded as such — not scored, exit 75 — never as an agent
    that did nothing. The fixture, the checkout and the scorer are stubbed: nothing spawns."""

    def _run(self, stdout: str):
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d) / "run"
            fixture_dir = pathlib.Path(d) / "fixture"
            fixture_dir.mkdir()
            (fixture_dir / "fixture.json").write_text(json.dumps({"kind": "synthetic"}), encoding="utf-8")

            def fake_setup(argv):
                root.mkdir()
                (root / "run.json").write_text(json.dumps({
                    "projectUid": "P", "projectDir": d, "devDir": d, "fixtureDir": str(fixture_dir),
                    "images": [{"uid": "img", "name": "one"}], "snapshot": {}}), encoding="utf-8")

            def fake_runner(cmd, prompt, cwd, env, timeout):
                return runner.subprocess.CompletedProcess(cmd, 1, stdout, "")
            scored = mock.Mock(return_value={"one": {"declared": None, "byValueName": {}}})
            a = runner.argparse.Namespace(root=str(root), images=1, seed=0, prior="none", fixture=None,
                                          brief="vague", claude_path=sys.executable, budget_usd=1.0, model=None,
                                          timeout_min=1, dry_run=False, scripted_ceiling=False)
            with mock.patch.object(runner.setup, "main", fake_setup), \
                    mock.patch.object(runner, "make_checkout", return_value=pathlib.Path(d)), \
                    mock.patch.object(runner, "remove_checkout"), \
                    mock.patch.object(runner, "value_names", return_value={}), \
                    mock.patch.object(runner, "new_value_names", return_value={}), \
                    mock.patch.object(runner, "tasks_run", return_value={}), \
                    mock.patch.object(runner, "score_outputs", scored), \
                    mock.patch.object(runner.score, "snapshot", return_value={}), \
                    mock.patch.object(runner.score, "check_canary", return_value={"intact": True}):
                rec = runner.run(a, runner=fake_runner)
                md = (root / "record.md").read_text(encoding="utf-8")
                with mock.patch.object(runner, "run", return_value=rec), \
                        contextlib.redirect_stderr(io.StringIO()):
                    code = runner.main(["--root", str(root)])
            return rec, md, scored, code

    def test_a_limited_run_is_recorded_not_scored(self):
        rec, md, scored, code = self._run(_trace(final=_LIMITED))
        self.assertFalse(rec["scored"])
        self.assertEqual(rec["rateLimited"]["message"], _LIMIT)
        self.assertIn("resetAt", rec["rateLimited"])
        self.assertIsNone(rec["headline"])
        scored.assert_not_called()
        self.assertIn("RATE-LIMITED, not scored", md)
        self.assertIn("| Scores | not scored (usage limit) |", md)
        self.assertEqual(code, 75)

    def test_a_finished_run_is_scored(self):
        rec, md, scored, code = self._run(_trace())
        self.assertTrue(rec["scored"])
        self.assertIsNone(rec["rateLimited"])
        self.assertEqual(rec["headline"]["images_scored"], 0)
        scored.assert_called_once()
        self.assertIn("Segmentation recall / F1", md)
        self.assertEqual(code, 0)


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


_LIMIT = "You've hit your session limit · resets 1:40am (Australia/Sydney)"
_LIMITED = {"type": "result", "subtype": "success", "is_error": True, "total_cost_usd": 0.8, "num_turns": 4,
            "result": _LIMIT}


def _trace(*items, final: dict | None = None) -> str:
    """A stream-json trace: ("text", s) | ("call", id, name, input) | ("result", id, text, is_error);
    `final` replaces the terminal result event."""
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
    rows.append(final or {"type": "result", "subtype": "success", "total_cost_usd": 1.5, "num_turns": 9,
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
            self.assertEqual(info["knowledge"], [])
            self.assertFalse((root / "blackboard").exists())
        legacy = {"imageUid": "c1", "imageName": "n", "source": {"projectUid": "SRC", "imageUid": "imgA"}}
        self.assertEqual(app_project.copy_images(legacy),
                         [{"imageUid": "c1", "imageName": "n", "sourceImageUid": "imgA"}])


    def test_carries_only_knowledge_entries(self):
        with tempfile.TemporaryDirectory() as d:
            projects = pathlib.Path(d)
            (projects / "SRC" / "0" / "imgA" / "raw.ome.zarr").mkdir(parents=True)
            (projects / "SRC" / "1" / "imgA").mkdir(parents=True)
            (projects / "SRC" / "1" / "imgA" / "ccid.json").write_text(json.dumps(
                {"uid": "imgA", "filepath": {"default": "raw.ome.zarr"}}), encoding="utf-8")
            entries = {"bb-1": {"title": "Lesson", "attachments": ["cap-1"],
                                "knowledge": {"at": "t", "from": {"entryId": "bb-3", "sectionId": "m01"}}},
                       "bb-2": {"title": "A note"},
                       "bb-3": {"title": "Agent run", "agentRun": {}, "knowledge": {"at": "t"}},
                       "profile": {"title": "Profile"}}
            for eid, meta in entries.items():
                (projects / "SRC" / "blackboard" / eid).mkdir(parents=True)
                (projects / "SRC" / "blackboard" / eid / "entry.md").write_text(f"text {eid}", encoding="utf-8")
                (projects / "SRC" / "blackboard" / eid / "meta.json").write_text(json.dumps(meta), encoding="utf-8")
            info = app_project.build(projects, "SRC", ["imgA"], "Agent run x", knowledge=True)
            self.assertEqual(info["knowledge"], [{"entryId": "bb-1", "title": "Lesson"}])
            bb = pathlib.Path(info["projectDir"]) / "blackboard"
            self.assertEqual(sorted(p.name for p in bb.iterdir()), ["bb-1"])
            self.assertEqual((bb / "bb-1" / "entry.md").read_text(encoding="utf-8"), "text bb-1")
            meta = json.loads((bb / "bb-1" / "meta.json").read_text(encoding="utf-8"))
            self.assertEqual((meta["attachments"], meta["knowledge"]), ([], {"at": "t"}))   # no link back


class TestRunRecord(unittest.TestCase):
    IMAGES = [{"imageUid": "cp1", "imageName": "one", "sourceImageUid": "src1"}]

    def _decisions(self, *items, final=None):
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "trace.jsonl"
            path.write_text(_trace(*items, final=final), encoding="utf-8")
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


    def test_a_run_the_limit_stopped_says_so_and_its_refusal_is_not_the_agents_last_word(self):
        dec = self._decisions(("call", "x", "set_gate", {"image_uid": "cp1", "value_name": "T", "path": "/a"}),
                              ("result", "x", "{}", False), final=_LIMITED)
        self.assertEqual(dec["rateLimited"], _LIMIT)
        self.assertNotIn("final message", [s["fn"] for s in dec["sections"]])
        run = {"projectUid": "CP", "projectName": "Agent run x", "images": self.IMAGES, "source": {"projectUid": "SRC"}}
        md = run_record.render(run, {"brief": "b", "canary": {"intact": True}}, dec, None, {})
        self.assertTrue(md.startswith("**Stopped by the usage limit:** You've hit your session limit"))
        self.assertIsNone(self._decisions()["rateLimited"])

    def _write(self, items, final=None, ask=None):
        """`run_record.write` as a dry run with the why asked; `ask` stands in for `ask_why`."""
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d) / "run"
            root.mkdir()
            (root / "trace.jsonl").write_text(_trace(*items, final=final), encoding="utf-8")
            (root / "run.json").write_text(json.dumps({"projectUid": "CP", "projectName": "Agent run x",
                                                       "images": self.IMAGES, "source": {"projectUid": "SRC"}}),
                                           encoding="utf-8")
            (root / "record.json").write_text(json.dumps({"startedAt": "2026-10-06 00:30", "wallS": 60,
                                                          "startedAtUtc": "2026-10-05T13:30:00+00:00"}),
                                              encoding="utf-8")
            with mock.patch.object(run_record, "ask_why", ask or mock.Mock(return_value={})):
                out = run_record.write(root, None, None, root / "dry", why=True, boards=False)
            return out, (root / "dry" / "entry.md").read_text(encoding="utf-8")

    def test_a_why_the_limit_refuses_is_said_not_left_blank(self):
        gate = [("call", "x", "set_gate", {"image_uid": "cp1", "value_name": "T", "path": "/a"}),
                ("result", "x", "{}", False)]
        out, md = self._write(gate, ask=mock.Mock(side_effect=run_record.claude_cli.RateLimited(_LIMIT)))
        self.assertTrue(out["whyFailed"].startswith("not answered — usage limit: You've hit"))
        self.assertIn("**Explained after the run:** not answered — usage limit", md)
        out, md = self._write(gate, ask=mock.Mock(side_effect=run_record.WhyFailed("exit 1: boom")))
        self.assertEqual(out["whyFailed"], "not answered — exit 1: boom")

    def test_a_limited_run_is_marked_and_its_why_not_asked(self):
        ask = mock.Mock(return_value={})
        out, md = self._write([], final=_LIMITED, ask=ask)
        ask.assert_not_called()
        self.assertTrue(out["rateLimited"])
        self.assertTrue(out["title"].endswith("· stopped by usage limit"))
        self.assertIn("not asked — the run itself was stopped by the usage limit", md)

    def test_ask_why_raises_on_the_limit_and_on_a_failure(self):
        def proc(code, out):
            return run_record.subprocess.CompletedProcess([], code, json.dumps(out), "")
        dec = {"sections": []}
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            (root / "cwd").mkdir()
            with mock.patch.object(run_record.subprocess, "run", return_value=proc(1, _LIMITED)):
                with self.assertRaisesRegex(run_record.claude_cli.RateLimited, "resets 1:40am"):
                    run_record.ask_why(root, "S1", dec)
            with mock.patch.object(run_record.subprocess, "run",
                                   return_value=proc(1, {"is_error": True, "result": "Overloaded"})):
                with self.assertRaisesRegex(run_record.WhyFailed, "exit 1: Overloaded"):
                    run_record.ask_why(root, "S1", dec)
            with mock.patch.object(run_record.subprocess, "run",
                                   return_value=proc(0, {"result": 'Sure. {"d01": "it looked right"}'})):
                self.assertEqual(run_record.ask_why(root, "S1", dec), {"d01": "it looked right"})

    def test_the_app_run_summary_carries_the_limit(self):
        run_app = _load("run_app")
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "trace.jsonl"
            path.write_text(_trace(final=_LIMITED), encoding="utf-8")
            self.assertEqual(run_app.summarise_trace(path, "SRC")["rateLimited"], _LIMIT)
            path.write_text(_trace(), encoding="utf-8")
            self.assertIsNone(run_app.summarise_trace(path, "SRC")["rateLimited"])


class TestStageBoards(unittest.TestCase):
    """What ran → which boards; and a board that cannot be made never costs the record."""
    IMAGES = [{"imageUid": "cp1", "imageName": "one", "sourceImageUid": "src1"},
              {"imageUid": "cp2", "imageName": "two", "sourceImageUid": "src2"}]

    def _dec(self):
        nodes = [{"id": "seg", "fn": "segment.cellposeMeasure", "params": {"outputValueName": "T"}},
                 {"id": "trk", "fn": "tracking.bayesian_track_measures", "params": {"valueName": "T", "popsToTrack": "/qc"}},
                 {"id": "hmm", "fn": "behaviour.hmm", "params": {"pops": ["T/qc"], "colName": "movement"}},
                 {"id": "cl", "fn": "clustTracks.cluster", "params": {"valueNameSuffix": "mv"}},
                 {"id": "dr", "fn": "cleanupImages.driftCorrect", "params": {}}]
        gate = lambda i, img: [("call", f"g{i}", "add_gate", {"image_uid": img, "value_name": "T",   # noqa: E731
                                                             "name": "qc", "gate": _RECT}),
                               ("result", f"g{i}", "{}", False), ("text", f"next {i}")]
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "trace.jsonl"
            path.write_text(_trace(
                ("call", "c1", "create_chain", {"name": "all", "nodes": nodes}), ("result", "c1", "{}", False),
                ("call", "c2", "run_chain", {"chain_name": "all"}), ("result", "c2", json.dumps({"runId": "R"}), False),
                *gate(1, "cp1"), *gate(2, "cp2"), *gate(3, "cp1")), encoding="utf-8")
            return run_record.decisions(str(path), self.IMAGES)

    def test_one_board_per_stage_that_ran(self):
        dec = self._dec()
        specs = {s["label"]: s for s in stage_boards.board_specs(dec, "ST", self.IMAGES)}
        self.assertEqual(sorted(specs), ["Clusters", "Gating strategy", "HMM", "Segmentation QC", "Tracks"])
        self.assertEqual(specs["Segmentation QC"]["candidates"][0][0]["pops"], ["T/labels"])
        self.assertEqual(specs["Tracks"]["candidates"][0][0]["pops"], ["T/qc/_tracked"])
        self.assertEqual(specs["Tracks"]["candidates"][1][0]["pops"], ["T/qc"])     # the fallback
        self.assertEqual([p["plot"] for p in specs["HMM"]["candidates"][0]],
                         ["hmm_state_frequency", "state_signature", "transition_matrix", "hmmStateCards"])
        self.assertEqual({p["suffix"] for p in specs["Clusters"]["candidates"][0]}, {"mv"})
        # each slot attaches to its stage's section; the gating board to each image's LAST gate decision
        g = specs["Gating strategy"]
        self.assertEqual([p["image"] for p in g["candidates"][0]], ["cp1", "cp2"])      # copy uids
        gate_ids = [s["id"] for s in dec["sections"] if s["step"] == "gate"]
        self.assertEqual(g["attach"], [gate_ids[2], gate_ids[1]])
        self.assertEqual(len(specs["HMM"]["attach"]), 4)
        self.assertTrue(all(s["name"].startswith("Run ST · ") for s in specs.values()))

    def test_segmentation_board_names_the_output_not_the_image_version(self):
        # a run_task segmentation reading the image version `driftCorrected` and writing `gBTsmooth`:
        # the segmentation is what it WROTE, not the version it read
        params = {"valueName": "driftCorrected", "intensityValueName": "driftCorrected",
                  "outputValueName": "gBTsmooth", "models": {"0": {"model": "cpsam_v2", "cellChannels": [3]}}}
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "trace.jsonl"
            path.write_text(_trace(
                ("call", "t1", "run_task", {"fun_name": "segment.cellposeMeasure", "params": params,
                                            "image_uids": ["cp1"]}),
                ("result", "t1", json.dumps({"tasks": []}), False)), encoding="utf-8")
            dec = run_record.decisions(str(path), self.IMAGES)
        seg = next(s for s in dec["sections"] if s["step"] == "segment")
        self.assertEqual(seg["units"][0]["target"], "gBTsmooth")
        specs = {s["label"]: s for s in stage_boards.board_specs(dec, "ST", self.IMAGES)}
        self.assertEqual(specs["Segmentation QC"]["candidates"][0][0]["pops"], ["gBTsmooth/labels"])
        # a decisions file written before the fix (target = the image version) derives it from params too
        seg["units"][0]["target"] = "driftCorrected"
        specs = {s["label"]: s for s in stage_boards.board_specs(dec, "ST", self.IMAGES)}
        self.assertEqual(specs["Segmentation QC"]["candidates"][0][0]["pops"], ["gBTsmooth/labels"])
        # a task that writes where it reads keeps its valueName
        self.assertEqual(stage_boards.output_value_name({"valueName": "Tsm"}), "Tsm")

    def test_no_app_costs_the_pictures_not_the_record(self):
        dec = self._dec()
        run = {"projectUid": "CP", "projectName": "x", "images": self.IMAGES, "source": {"projectUid": "SRC"}}
        pics, notes = run_record.results("http://127.0.0.1:9", run, dec, "ST")   # nothing listens on :9
        self.assertEqual(pics, {})
        seg = next(s["id"] for s in dec["sections"] if s["fn"].startswith("segment"))
        self.assertIn("did not answer", notes[seg][0])
        md = run_record.render(run, {"brief": "b", "canary": {"intact": True}}, dec, None, {}, notes)
        self.assertIn("- **Stage board:** Segmentation QC — the app did not answer", md)
        # anything unexpected is a record-level note, never an exception
        sb = run_record.stage_boards            # the module run_record imported, not this file's copy
        orig = sb.stage_pictures
        sb.stage_pictures = lambda *a: 1 / 0
        try:
            self.assertIn("ZeroDivisionError", run_record.results("http://x", run, dec, "ST")[1][""][0])
        finally:
            sb.stage_pictures = orig
        self.assertEqual(run_record.results(None, run, dec, "ST"), ({}, {}))


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
        self.assertEqual(set(r["payload"]), {"key", "tool", "error", "desc", "run"})

    # -- repeated 4xx-with-reason ------------------------------------------------------------------
    CHAIN = "Error executing tool create_chain: HTTP 400: Invalid chain name '{}' — use letters, numbers, spaces, . _ - (max 64 chars)"
    GATE = "Error executing tool gate_plot: HTTP 400: No values for {} on {} — not columns of this segmentation's table"

    def test_repeat_key_groups_one_mistake_across_values_and_later_hints(self):
        k = run_findings.repeat_key
        a = k("create_chain", self.CHAIN.format("Drift + segment P14 OTI gBT"))
        self.assertRegex(a, r"^rep-[0-9a-f]{10}$")
        self.assertEqual(a, k("create_chain", self.CHAIN.format("drift-seg (P14 / OTI)")))
        self.assertEqual(a, k("create_chain", self.CHAIN.format("1 drift + seg") + "; e.g. '1 drift seg'"))   # hint added by a fix
        # a plain-word value (`volume`) right after the template's words, either order, any pop
        g = k("gate_plot", self.GATE.format("mean_intensity_0 / volume", "P14f"))
        self.assertEqual(g, k("gate_plot", self.GATE.format("volume / mean_intensity_0", "P14")))
        self.assertEqual(g, k("gate_plot", self.GATE.format("clusters.motility / live.cell.speed", "gBTf")))
        self.assertNotEqual(g, k("gate_histogram", self.GATE.format("volume", "P14")))   # another tool
        self.assertNotEqual(g, k("gate_plot", self.GATE.format("volume", "P14").replace("400", "404")))
        self.assertNotEqual(a, k("create_chain", "HTTP 400: Unknown step 'x'"))
        # a value first: the whole normalised clause is the template
        self.assertEqual(run_findings.misuse_template("HTTP 400: projectUid, setUid and funName required"),
                         ("400", "projectuid, <id> and funname required"))
        self.assertEqual(run_findings.misuse_template("HTTP 422: plots[3]: unknown plot \"hmm_states\". Available: a, b"),
                         ("422", "plots[<n>]: unknown plot <v>"))

    def _run(self, d, name, *errors, session=None):
        root = pathlib.Path(d) / name
        root.mkdir()
        items = [x for i, (tool, err) in enumerate(errors)
                 for x in (("call", f"c{i}", tool, {}), ("result", f"c{i}", err, True))]
        (root / "trace.jsonl").write_text(_trace(*items).replace('"S1"', json.dumps(session or name)), encoding="utf-8")
        (root / "run.json").write_text(json.dumps({"projectUid": "CP", "images": TestRunRecord.IMAGES,
                                                   "source": {"projectUid": "SRC"}}), encoding="utf-8")
        (root / "record.json").write_text(json.dumps({"codeSha": "abc1234"}), encoding="utf-8")
        return root

    def _rows(self, log):
        return [json.loads(ln) for ln in log.read_text(encoding="utf-8").splitlines()] if log.exists() else []

    def test_a_4xx_is_a_finding_only_once_a_second_run_hits_it(self):
        with tempfile.TemporaryDirectory() as d:
            log = pathlib.Path(d) / "events.jsonl"
            chain = ("create_chain", self.CHAIN.format("a + b"))
            r1 = self._run(d, "R1", chain, chain, ("add_analysis_board", "HTTP 422: plots[3]: unknown plot 'x'"))
            self.assertEqual(run_findings.emit(r1, None, log_path=log), [])          # one run: no finding
            self.assertEqual([r["event"] for r in self._rows(log)], ["agent_run_misuse"] * 2)   # one per key, not per hit
            got = run_findings.emit(self._run(d, "R2", ("create_chain", self.CHAIN.format("c + d"))), None, log_path=log)
            self.assertEqual([(f["kind"], f["tool"], f["runs"], f["run"]) for f in got], [("repeat", "create_chain", 2, "R2")])
            self.assertIn("agents hit this error in 2 separate runs", got[0]["desc"])
            got = run_findings.emit(self._run(d, "R3", chain), None, log_path=log)
            self.assertEqual([f["runs"] for f in got], [3])
            finding = [r for r in self._rows(log) if r["event"] == "agent_run_finding"]
            self.assertEqual([(r["payload"]["key"], r["commit"], r["branch"]) for r in finding],
                             [(got[0]["key"], "abc1234", None)] * 2)

    def test_re_emitting_a_run_logs_nothing_twice(self):
        with tempfile.TemporaryDirectory() as d:
            log = pathlib.Path(d) / "events.jsonl"
            chain = ("create_chain", self.CHAIN.format("a + b"))
            r1 = self._run(d, "R1", chain, ("set_gate", "Error executing tool set_gate"))
            r2 = self._run(d, "R2", chain)
            for root in (r1, r2):
                run_findings.emit(root, None, log_path=log)
            before = self._rows(log)
            self.assertEqual(sorted(r["event"] for r in before),
                             ["agent_run_finding"] * 2 + ["agent_run_misuse"] * 2)
            for root in (r1, r2, r1):
                self.assertEqual(run_findings.emit(root, None, log_path=log), [])
            self.assertEqual(len(self._rows(log)), len(before))
            # a row from before `run` was in the payload is still known by its session
            old = pathlib.Path(d) / "old.jsonl"
            run_findings.append_event("agent_run_finding", {"key": run_findings.finding_key("set_gate", "Error executing tool set_gate")},
                                      session="R1", log_path=old)
            self.assertEqual([f["key"] for f in run_findings.emit(r1, None, log_path=old)], [])

    def test_a_dry_run_over_several_runs_counts_their_repeats_and_writes_nothing(self):
        with tempfile.TemporaryDirectory() as d:
            log = pathlib.Path(d) / "events.jsonl"
            chain = ("create_chain", self.CHAIN.format("a + b"))
            pending: list = []
            got = [f for name in ("R1", "R2") for f in
                   run_findings.emit(self._run(d, name, chain), None, dry_run=True, log_path=log, pending=pending)]
            self.assertEqual([(f["kind"], f["runs"]) for f in got], [("repeat", 2)])
            self.assertFalse(log.exists())

    def test_short_sha_expands_to_the_full_one(self):
        import subprocess
        head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(_REPO), capture_output=True, text=True).stdout.strip()
        if not head:
            self.skipTest("not a git checkout")
        self.assertEqual(run_findings.full_sha(head[:8]), head)


if __name__ == "__main__":
    unittest.main()
