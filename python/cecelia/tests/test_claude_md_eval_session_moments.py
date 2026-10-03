"""Tests for scripts/claude_md_eval/session_moments.py — candidate probe moments from session logs.

Synthetic session `.jsonl` fixtures in a temp `projects/` tree; no real transcripts are read.
"""
from __future__ import annotations

import importlib.util
import json
import os
import pathlib
import tempfile
import time
import unittest

_PATH = pathlib.Path(__file__).resolve().parents[3] / "scripts" / "claude_md_eval" / "session_moments.py"


def _load():
    spec = importlib.util.spec_from_file_location("session_moments", _PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


sm = _load()
SID = "11111111-2222-3333-4444-555555555555"


def owner(text: str, entry: str = "cli", **kw) -> dict:
    return {"type": "user", "entrypoint": entry, "origin": {"kind": "human"},
            "message": {"role": "user", "content": text}, "uuid": f"u-{text[:6]}", **kw}


def assistant(*items: dict, **kw) -> dict:
    return {"type": "assistant", "entrypoint": "cli", "message": {"content": list(items)}, **kw}


def text(t: str) -> dict:
    return {"type": "text", "text": t}


def tool_use(tid: str, name: str, **inp) -> dict:
    return {"type": "tool_use", "id": tid, "name": name, "input": inp}


def tool_result(tid: str, body: str) -> dict:
    return {"type": "user", "entrypoint": "cli",
            "message": {"content": [{"type": "tool_result", "tool_use_id": tid, "content": body}]}}


class _Tree:
    def __init__(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def session(self, rows: list[dict], project: str = "-home-x-proj", sid: str = SID) -> pathlib.Path:
        d = self.root / project
        d.mkdir(parents=True, exist_ok=True)
        p = d / f"{sid}.jsonl"
        p.write_text("\n".join(json.dumps(r) for r in rows) + "\n", encoding="utf-8")
        return p

    def memory(self, name: str, sid: str, project: str = "-home-x-proj") -> None:
        d = self.root / project / "memory"
        d.mkdir(parents=True, exist_ok=True)
        (d / name).write_text(f"---\nname: x\nmetadata:\n  originSessionId: {sid}\n---\nbody\n",
                              encoding="utf-8")


class ExtractMomentsTest(unittest.TestCase):
    def setUp(self):
        self.t = _Tree()
        self.addCleanup(self.t.tmp.cleanup)

    def signals(self, rows, memories=None):
        return [(m["signal"], m["turn"]) for m in sm.extract_moments(self.t.session(rows), memories)]

    def test_each_signal_fires_on_its_turn(self):
        rows = [
            owner("add a plot"),
            assistant(text("Here is a long plan."), tool_use("t1", "AskUserQuestion", questions=[])),
            tool_result("t1", "The user doesn't want to proceed with this tool use. The tool use was rejected"),
            owner("no. use the registry"),
            assistant(text("You're right, the registry is the way.")),
            owner("[Request interrupted by user]"),
        ]
        self.assertEqual(self.signals(rows),
                         [("refused", 1), ("correction", 2), ("retraction", 2), ("interrupt", 3)])

    def test_correction_is_anchored_at_the_start(self):
        rows = [owner("ok, but no rush"), owner("nothing to add"), owner("Don't touch main")]
        self.assertEqual(self.signals(rows), [("correction", 3)])

    def test_headless_sessions_are_skipped(self):
        rows = [owner("no. wrong", entry="sdk-cli"), assistant(text("You're right"))]
        self.assertEqual(self.signals(rows), [])

    def test_sidechain_turns_are_skipped(self):
        rows = [owner("start"), {**owner("no, not that"), "isSidechain": True},
                {**assistant(text("You're right")), "isSidechain": True}]
        self.assertEqual(self.signals(rows), [])

    def test_tool_results_are_not_owner_turns(self):
        rows = [owner("go"), tool_result("t1", "no such file"), owner("again please")]
        self.assertEqual(self.signals(rows), [("correction", 2)])

    def test_memory_write_marks_the_owner_turn_it_answers(self):
        rows = [owner("start"), owner("that's wrong, check the PR first"),
                assistant(tool_use("w", "Write", file_path="/h/.claude/projects/p/memory/feedback_pr.md"))]
        got = sm.extract_moments(self.t.session(rows), {SID: ["feedback_pr.md"]})
        self.assertIn(("memory", 2, "feedback_pr.md"), [(m["signal"], m["turn"], m["detail"]) for m in got])

    def test_memory_needs_a_match_on_this_session(self):
        rows = [owner("start"), assistant(tool_use("w", "Write", file_path="/m/memory/feedback_pr.md"))]
        self.assertEqual(self.signals(rows, {"other-session": ["feedback_pr.md"]}), [])

    def test_one_retraction_per_turn(self):
        rows = [owner("hm"), assistant(text("You're right.")), assistant(text("My mistake again."))]
        self.assertEqual(self.signals(rows), [("retraction", 1)])

    def test_moments_cite_ids_not_text(self):
        m = sm.extract_moments(self.t.session([owner("no, revert that")]))[0]
        self.assertEqual(set(m), {"session_id", "project", "turn", "signal", "timestamp", "uuid", "detail"})
        self.assertNotIn("revert", json.dumps(m))


class ScanTest(unittest.TestCase):
    def setUp(self):
        self.t = _Tree()
        self.addCleanup(self.t.tmp.cleanup)

    def test_window_sandboxes_and_memory_index(self):
        self.t.session([owner("no.")])
        self.t.session([owner("no.")], project="-tmp-cecelia-eval-x", sid="a" * 36)
        old = self.t.session([owner("no.")], sid="b" * 36)
        past = time.time() - 40 * 86400
        os.utime(old, (past, past))
        self.t.memory("feedback_x.md", SID)
        files = sm.session_files(self.t.root, 30)
        self.assertEqual([f.stem for f in files], [SID])
        self.assertEqual(sm.memory_sessions(self.t.root), {SID: ["feedback_x.md"]})
        moments, stats = sm.scan(self.t.root, 30)
        self.assertEqual(stats["per_signal"], {"correction": 1})
        self.assertEqual(stats["memory_sessions_in_window"], 1)


if __name__ == "__main__":
    unittest.main()
