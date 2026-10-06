"""Tests for `scripts/agent_eval/guide_run.py` — `pixi run guide-run` (docs/todo/GUIDE_RUNS_PLAN.md P3).

Nothing here starts an agent or touches the app: `run_app.py` is replaced by a fake process, the app
check by a stub, and the state dir (`~/.cecelia-effectiveness`) by a temp dir via
`CECELIA_EFFECTIVENESS_LOG`.
"""
from __future__ import annotations

import datetime as dt
import importlib.util
import json
import os
import pathlib
import signal
import sys
import tempfile
import types
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
sys.path.insert(0, str(_REPO / "scripts" / "agent_eval"))   # guide_run / run_app import their siblings


def _load(name):
    key = f"agent_eval_{name}"
    spec = importlib.util.spec_from_file_location(key, _REPO / "scripts" / "agent_eval" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[key] = mod
    spec.loader.exec_module(mod)
    return mod


guide_run = _load("guide_run")
run_app = _load("run_app")
run_record = _load("run_record")

INTRAVITAL = {"sourceProject": "tSJpBI", "sourceSet": "k58SK7", "images": ["jV6p8M", "8F20qd", "QWhG6x"],
              "title": "Intravital timelapse"}


def _args(**kw):
    base = dict(runs=1, budget_usd=5.0, knowledge=False, at=None, ask_why=True, projects_dir=None,
                timeout_s=600, api_url="http://127.0.0.1:1", discovery="on")
    return types.SimpleNamespace(**{**base, **kw})


class _StateDir(unittest.TestCase):
    """Every test writes under a temp `~/.cecelia-effectiveness`."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.state = pathlib.Path(self._tmp.name)
        self._env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.state / "events.jsonl")})
        self._env.start()

    def tearDown(self):
        self._env.stop()
        self._tmp.cleanup()

    def log_lines(self):
        p = self.state / "guide-runs.jsonl"
        return [json.loads(x) for x in p.read_text(encoding="utf-8").splitlines()] if p.exists() else []


class TestConfig(unittest.TestCase):
    def test_every_configured_guide_ships(self):
        titles = guide_run.guide_titles()
        for gid in guide_run.load_projects():
            self.assertIn(gid, titles, f"{gid} has a test project but no guide in guides.json")

    def test_intravital_entry(self):
        got = guide_run.test_project("intravital-timelapse")
        self.assertEqual({k: got[k] for k in INTRAVITAL}, INTRAVITAL)

    def test_every_guide_has_a_checklist(self):
        for gid, entry in guide_run.load_projects().items():
            self.assertTrue(entry.get("checklist"), f"{gid} has no reviewer checklist")
            self.assertTrue(all(isinstance(c, str) and c for c in entry["checklist"]))

    def test_unknown_guide_lists_the_known_ones(self):
        with self.assertRaises(guide_run.GuideRunError) as e:
            guide_run.test_project("no-such-guide")
        self.assertIn("intravital-timelapse", str(e.exception))

    def test_configured_but_not_shipped_is_refused(self):
        with self.assertRaises(guide_run.GuideRunError) as e:
            guide_run.test_project("gone", projects={"gone": INTRAVITAL}, titles={})
        self.assertIn("not a guide", str(e.exception))

    def test_paths_are_under_the_state_dir_not_tmp(self):
        with mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": "/x/eff/events.jsonl"}):
            self.assertEqual(guide_run.runs_dir(), pathlib.Path("/x/eff/app-runs"))
            self.assertEqual(guide_run.log_path(), pathlib.Path("/x/eff/guide-runs.jsonl"))
        src = (_REPO / "scripts" / "agent_eval" / "guide_run.py").read_text(encoding="utf-8")
        self.assertNotIn("/tmp", src)


class TestBrief(unittest.TestCase):
    def test_brief_text(self):
        self.assertEqual(guide_run.brief_for("Intravital timelapse"),
                         "Use the intravital timelapse guide to process the images in this project.")

    def test_acronym_keeps_capitals(self):
        self.assertEqual(guide_run.guide_name("AF correction"), "AF correction")


class TestRunAppArgs(unittest.TestCase):
    def argv(self, **kw):
        base = dict(root=pathlib.Path("R"), projects_dir="P", api_url="http://a", budget_usd=5.0,
                    knowledge=False, timeout_s=600, ask_why=True)
        return guide_run.run_app_argv("intravital-timelapse", {**INTRAVITAL, "checklist": ["look at the gate"]},
                                      **{**base, **kw})

    def parsed(self, argv):
        """What run_app.main makes of `argv` — its own parser, the run itself mocked out."""
        got = {}

        def fake_run(a):
            got["a"] = a
            return {"trace": {"costUsd": 0, "toolCallsTotal": 0, "toolErrors": 0}, "copy": {"projectUid": "C"},
                    "wallS": 0, "canary": {"intact": True}, "exitCode": 0}
        old = signal.getsignal(signal.SIGTERM)
        try:
            with mock.patch.object(run_app, "run", fake_run), mock.patch("builtins.print"):
                run_app.main(argv)
        finally:
            signal.signal(signal.SIGTERM, old)
        return got["a"]

    def test_run_app_accepts_them(self):
        a = self.parsed(self.argv())
        self.assertEqual((a.source_project, a.source_set, a.image), ("tSJpBI", "k58SK7", INTRAVITAL["images"]))
        self.assertEqual(a.brief, "Use the intravital timelapse guide to process the images in this project.")
        self.assertEqual(a.check, ["look at the gate"])
        self.assertEqual((a.guide, a.budget_usd, a.knowledge, a.ask_why, a.root, a.projects_dir),
                         ("intravital-timelapse", 5.0, False, True, "R", "P"))

    def test_knowledge_and_no_why_pass_through(self):
        a = self.parsed(self.argv(knowledge=True, ask_why=False))
        self.assertTrue(a.knowledge)
        self.assertFalse(a.ask_why)

    def test_discovery_passes_through_to_both_mcp_servers(self):
        self.assertEqual(self.parsed(self.argv()).discovery, "on")
        a = self.parsed(self.argv(discovery="off"))
        self.assertEqual(a.discovery, "off")
        cfg = run_app.mcp_config("http://a", "C", "", a.discovery)["mcpServers"]
        self.assertEqual({s["env"][run_app.DISCOVERY_ENV] for s in cfg.values()}, {"off"})
        self.assertEqual(run_app.DISCOVERY_ENV, "CECELIA_MCP_DISCOVERY")   # the name discovery.py reads


class _FakeProc:
    """Stands in for `run_app.py`: writes what a finished run leaves, or raises mid-run."""

    def __init__(self, argv, root, *, rc=0, cost=2.5, entry="bb-1", raise_on_wait=None, why_cost=None):
        self.root, self.returncode, self._rc, self._raise = root, None, rc, raise_on_wait
        self.terminated = False
        root.mkdir(parents=True)
        (root / "trace.jsonl").write_text(json.dumps({"type": "result", "total_cost_usd": cost}) + "\n",
                                          encoding="utf-8")
        if why_cost is not None:   # run_record.ask_why ran
            (root / "why.json").write_text(json.dumps({"costUsd": why_cost}), encoding="utf-8")
        if entry:
            (root / "record.json").write_text(json.dumps({"blackboard": {"entryId": entry}}), encoding="utf-8")

    def wait(self, timeout=None):
        if self._raise and not self.terminated:
            raise self._raise
        self.returncode = self._rc if not self.terminated else -15
        return self.returncode

    def poll(self):
        return self.returncode

    def terminate(self):
        self.terminated = True

    def kill(self):
        self.terminated = True


class TestRunOne(_StateDir):
    def run_one(self, **fake):
        procs = []

        def popen(cmd, cwd=None):
            root = pathlib.Path(cmd[cmd.index("--root") + 1])
            procs.append(_FakeProc(cmd, root, **fake))
            return procs[-1]
        with mock.patch.object(guide_run.subprocess, "Popen", popen), mock.patch("builtins.print"):
            rc = guide_run.run_one("intravital-timelapse", INTRAVITAL, _args(), "P", "abc123", {"dirty": False})
        return rc, procs

    def test_log_line(self):
        rc, procs = self.run_one()
        self.assertEqual(rc, 0)
        [line] = self.log_lines()
        self.assertEqual({k: line[k] for k in ("guide", "commit", "knowledge", "costUsd", "exit", "recordId", "dirty")},
                         {"guide": "intravital-timelapse", "commit": "abc123", "knowledge": False, "costUsd": 2.5,
                          "exit": 0, "recordId": "bb-1", "dirty": False})
        self.assertEqual(procs[0].root, self.state / "app-runs" / line["stamp"])
        self.assertIsInstance(line["wallS"], int)

    def test_the_why_turn_s_cost_is_logged_apart_and_in_the_total(self):
        self.run_one(why_cost=0.4)
        [line] = self.log_lines()
        self.assertEqual((line["costUsd"], line["whyCostUsd"], line["totalCostUsd"]), (2.5, 0.4, 2.9))

    def test_no_why_turn_leaves_the_total_the_run_s(self):
        self.run_one()
        [line] = self.log_lines()
        self.assertEqual((line["whyCostUsd"], line["totalCostUsd"]), (None, 2.5))

    def test_discovery_is_logged(self):
        procs = []

        def popen(cmd, cwd=None):
            procs.append(_FakeProc(cmd, pathlib.Path(cmd[cmd.index("--root") + 1])))
            return procs[-1]
        with mock.patch.object(guide_run.subprocess, "Popen", popen), mock.patch("builtins.print"):
            guide_run.run_one("intravital-timelapse", INTRAVITAL, _args(discovery="off"), "P", "abc",
                              {"discovery": "off"})
        [line] = self.log_lines()
        self.assertEqual(line["discovery"], "off")

    def test_aborted_run_is_logged_and_torn_down(self):
        with self.assertRaises(SystemExit):
            self.run_one(raise_on_wait=SystemExit(143), entry=None)
        [line] = self.log_lines()
        self.assertEqual((line["costUsd"], line["recordId"], line["exit"]), (2.5, None, -15))

    def test_log_is_append_only(self):
        self.run_one()
        with mock.patch.object(guide_run, "new_stamp", lambda: "second"):
            self.run_one()
        self.assertEqual(len(self.log_lines()), 2)


class TestBatch(_StateDir):
    def setUp(self):
        super().setUp()
        self._code = mock.patch.object(guide_run, "code_state", lambda: ("abc123", False))
        self._code.start()
        self.addCleanup(self._code.stop)

    def test_app_down_refuses_and_logs(self):
        with mock.patch.object(guide_run, "app_status", lambda url: None), \
                mock.patch.object(guide_run.subprocess, "Popen") as popen, mock.patch("sys.stderr"):
            rc = guide_run.run_batch("intravital-timelapse", INTRAVITAL, _args())
        self.assertEqual(rc, 2)
        popen.assert_not_called()
        [line] = self.log_lines()
        self.assertIn("not running", line["skipped"])
        self.assertEqual(line["discovery"], "on")       # a skipped run still says which arm it was

    def test_stops_after_a_failed_run(self):
        root = self.state / "projects"
        (root / "tSJpBI").mkdir(parents=True)
        calls = []
        with mock.patch.object(guide_run, "app_status", lambda url: {"projectsDir": str(root), "commit": "x"}), \
                mock.patch.object(guide_run, "run_one", lambda *a, **k: calls.append(a) or 1), \
                mock.patch("sys.stderr"):
            rc = guide_run.run_batch("intravital-timelapse", INTRAVITAL, _args(runs=3))
        self.assertEqual((rc, len(calls)), (1, 1))
        self.assertEqual(calls[0][3], str(root))       # the projects dir comes from the running app


class TestRecordTitle(unittest.TestCase):
    def test_title_names_the_guide_and_knowledge(self):
        rec = {"guide": "intravital-timelapse", "startedAt": "2026-10-07 02:00", "knowledgeOn": True}
        self.assertEqual(run_record.title_of({"knowledge": []}, rec, "r", "Crop"),
                         "Guide run intravital-timelapse · 2026-10-07 02:00 — Crop · with lab knowledge")

    def test_title_marks_the_discovery_off_arm(self):
        rec = {"guide": "g", "startedAt": "s", "discovery": "off"}
        self.assertTrue(run_record.title_of({"knowledge": []}, rec, "r", "Crop").endswith(" · discovery off"))

    def test_older_record_reads_as_before(self):
        rec = {"startedAt": "2026-10-05 17:12"}
        self.assertEqual(run_record.title_of({"knowledge": []}, rec, "r", "Crop"), "Agent run 2026-10-05 17:12 — Crop")
        self.assertTrue(run_record.knowledge_on({"knowledge": [{"entryId": "k"}]}, rec))


class TestRecordChecklist(unittest.TestCase):
    def test_checklist_heads_the_entry(self):
        run = {"projectUid": "C", "images": [], "knowledge": []}
        rec = {"guide": "g", "checklist": ["cells per frame"], "trace": {}}
        with mock.patch.object(run_record.app_project, "copy_images", lambda r: []):
            body = run_record.render(run, rec, {"sections": [], "toolErrors": []}, None, {})
        self.assertTrue(body.startswith("**Review checklist:**\n- cells per frame\n"))


class TestAt(unittest.TestCase):
    NOW = dt.datetime(2026, 10, 6, 14, 30)

    def test_later_today_and_tomorrow(self):
        self.assertEqual(guide_run.next_at("23:00", self.NOW), dt.datetime(2026, 10, 6, 23, 0))
        self.assertEqual(guide_run.next_at("02:00", self.NOW), dt.datetime(2026, 10, 7, 2, 0))
        self.assertEqual(guide_run.next_at("2026-10-09 01:15", self.NOW), dt.datetime(2026, 10, 9, 1, 15))

    def test_bad_or_past(self):
        for bad in ("25:00", "tonight", "2026-10-01 02:00"):
            with self.assertRaises(guide_run.GuideRunError):
                guide_run.next_at(bad, self.NOW)

    def test_timer_fires_once_and_reruns_without_at(self):
        a = _args(at="02:00", knowledge=True, runs=2)
        fwd = guide_run.forward_args("intravital-timelapse", a)
        self.assertNotIn("--at", fwd)
        self.assertEqual(fwd[:3], ["intravital-timelapse", "--runs", "2"])
        self.assertIn("--knowledge", fwd)
        self.assertEqual(fwd[fwd.index("--discovery") + 1], "on")
        argv = guide_run.schedule_argv(dt.datetime(2026, 10, 7, 2, 0), "u", ["pixi", "run", "guide-run", *fwd], "/bin")
        self.assertEqual(argv[:2], ["systemd-run", "--user"])
        self.assertIn("--on-calendar=2026-10-07 02:00:00", argv)   # a full date: one shot, not daily
        self.assertEqual(argv[argv.index("--") + 1:], ["pixi", "run", "guide-run", *fwd])


if __name__ == "__main__":
    unittest.main()
