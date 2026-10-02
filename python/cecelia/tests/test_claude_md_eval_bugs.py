"""Tests for `scripts/claude_md_eval/bugs.py` — the weekly bug sweep.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 18. The code is a throwaway git
repo; the judge is injected. Nothing spawns `claude`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import pathlib
import re
import subprocess
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_BUGS_PATH = _REPO / "scripts" / "claude_md_eval" / "bugs.py"


def _load_bugs():
    spec = importlib.util.spec_from_file_location("bugs", _BUGS_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _row(event, payload, *, ts="2026-10-01T00:00:00Z", branch=None):
    return {"event": event, "ts": ts, "branch": branch, "commit": "c" * 40, "payload": payload}


def _finding(slug, file="a.py", line=2, marker="confirmed", **kw):
    return _row("fanout_audit_finding",
                {"slug": slug, "file": file, "line": line, "desc": f"desc {slug}", "marker": marker}, **kw)


def _resolved(slug, outcome):
    return _row("fanout_audit_finding_resolved", {"slug": slug, "outcome": outcome})


def _advisory(file="a.py", line=3, desc="maybe", **kw):
    return _row("fanout_audit_advisory", {"file": file, "line": line, "desc": desc, "marker": "plausible"}, **kw)


class _Repo(unittest.TestCase):
    def setUp(self):
        self.b = _load_bugs()
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.repo = pathlib.Path(tmp.name)
        self.git("init", "-q", "-b", "main")
        (self.repo / "a.py").write_text("".join(f"line {i}\n" for i in range(1, 61)), encoding="utf-8")
        self.git("add", "a.py")
        self.git("commit", "-q", "-m", "a")
        self.sha = self.git("rev-parse", "HEAD")
        self.prompts: list[str] = []

    def git(self, *args):
        return subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *args], cwd=self.repo,
                              capture_output=True, text=True, check=True, encoding="utf-8").stdout.strip()

    def judge(self, verdict="live_bug", only=None):
        def fn(prompt):
            self.prompts.append(prompt)
            keys = re.findall(r"^FINDING (\S+) ", prompt, re.M)
            return {"items": [{"key": k, "verdict": verdict, "why": "w"} for k in keys
                              if only is None or k in only]}, 0.1
        return fn

    def sweep(self, events, previous=None, judge=None):
        return self.b.sweep(events, date="2026-10-05", sha=self.sha, previous=previous,
                            judge=judge or self.judge(), repo=self.repo)


class CandidatesTest(_Repo):
    def test_only_unhandled_fanout_findings_since_the_window(self):
        events = [_finding("fanout-00000001"), _resolved("fanout-00000001", "fixed_pre_commit"),
                  _finding("fanout-00000002"), _resolved("fanout-00000002", "false_positive"),
                  _finding("fanout-00000003"), _resolved("fanout-00000003", "shipped_with_finding"),
                  _finding("fanout-00000004"),                                        # never tagged
                  _finding("fanout-00000005", ts="2026-09-01T00:00:00Z"),             # before the window
                  _row("convention_check_finding", {"slug": "conv-00000006", "file": "a.py", "line": 1,
                                                    "desc": "d", "marker": "should reuse"}),
                  _advisory(), _advisory()]                                           # one key, twice
        keys = [c["key"] for c in self.b.candidates(events, since="2026-09-28")]
        self.assertEqual(keys[:2], ["fanout-00000003", "fanout-00000004"])
        self.assertEqual(len(keys), 3)
        self.assertTrue(keys[2].startswith("fanout-"))   # the advisory, keyed like recital slugs it


class SweepTest(_Repo):
    def test_the_judge_sees_the_code_at_the_pinned_sha_and_its_verdicts_land(self):
        events = [_finding("fanout-00000001", line=30), _finding("fanout-00000002")]
        bugs, cost = self.sweep(events, judge=self.judge(only={"fanout-00000001"}))
        self.assertIn("   30  line 30", self.prompts[0])
        self.assertEqual([(b["id"], b["key"], b["status"]) for b in bugs],
                         [("B1", "fanout-00000001", "open"), ("B2", "fanout-00000002", "open")])
        self.assertEqual(bugs[1]["why"], "not judged (the judge returned no verdict)")
        self.assertEqual(cost, 0.1)
        self.assertNotIn("excerpt", bugs[0])

    def test_a_missing_file_is_gone_and_an_unmerged_change_waits_without_the_judge(self):
        self.git("checkout", "-q", "-b", "feat/x")
        found = _finding("fanout-00000001", file="b.py", branch="feat/x") | {"commit": self.sha}
        bugs, _ = self.sweep([found])   # reviewed, not committed yet: the tip is still on main
        self.assertEqual([b["status"] for b in bugs], ["unmerged"])
        (self.repo / "b.py").write_text("new\n", encoding="utf-8")
        self.git("add", "b.py")
        self.git("commit", "-q", "-m", "b")
        self.git("checkout", "-q", "main")
        events = [found, _finding("fanout-00000002", file="gone.py")]
        bugs, cost = self.sweep(events)
        # gone.py: already fixed (removed) before it was ever listed, so not listed
        self.assertEqual({b["key"]: b["status"] for b in bugs}, {"fanout-00000001": "unmerged"})
        self.assertEqual((self.prompts, cost), ([], 0.0))
        self.git("merge", "-q", "feat/x")
        self.sha = self.git("rev-parse", "HEAD")
        bugs, _ = self.sweep([], previous={"run": {"suite_ts": "2026-10-04T00:00:00Z"}, "bugs": bugs})
        self.assertEqual([(b["key"], b["status"]) for b in bugs], [("fanout-00000001", "open")])

    def test_open_bugs_carry_until_gone_and_wont_fix_drops_out(self):
        bugs, _ = self.sweep([_finding("fanout-00000001"), _finding("fanout-00000002")])
        bugs[1]["status"] = "wont_fix"   # the owner's answer, applied before the next pass
        previous = {"run": {"suite_ts": "2026-10-05T00:00:00Z"}, "bugs": bugs}
        again, _ = self.sweep([], previous=previous, judge=self.judge("gone"))
        self.assertEqual([(b["key"], b["status"], b["first_seen"]) for b in again],
                         [("fanout-00000001", "gone", "2026-10-05")])
        self.assertEqual(self.sweep([], previous={"run": {}, "bugs": again})[0], [])

    def test_a_new_finding_already_fixed_is_not_listed(self):
        self.assertEqual(self.sweep([_finding("fanout-00000001")], judge=self.judge("gone"))[0], [])

    def test_a_failed_judge_keeps_them_open(self):
        def boom(prompt):
            raise self.b._judge.JudgeError("no claude")
        bugs, cost = self.sweep([_finding("fanout-00000001")], judge=boom)
        self.assertEqual([(b["status"], cost) for b in bugs], [("open", 0.0)])

    def test_over_the_cap_waits_open_and_unjudged(self):
        events = [_finding(f"fanout-{i:08x}", line=i + 1) for i in range(self.b.MAX_ITEMS + 2)]
        bugs, _ = self.sweep(events, judge=self.judge("not_a_bug"))
        self.assertEqual(sum(b["status"] == "dismissed" for b in bugs), self.b.MAX_ITEMS)
        self.assertEqual(sum(b["status"] == "open" for b in bugs), 2)


class OwnerAnswerTest(unittest.TestCase):
    def test_a_wont_fix_answer_lands_on_the_bug(self):
        spec = importlib.util.spec_from_file_location("review", _REPO / "scripts" / "claude_md_eval" / "review.py")
        review = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(review)
        record = {"date": "2026-10-05", "findings": [], "proposals": [],
                  "bugs": [{"id": "B1", "status": "open"}], "queue": [{"kind": "bug", "ref": "B1"}]}
        rows = [{"record": "2026-10-05", "event": "bug_status", "ref": "B1", "value": "wont_fix"}]
        self.assertEqual(review.apply_reviews(record, rows)["bugs"][0]["status"], "wont_fix")
        self.assertEqual(review.pending(record, rows), [])
        self.assertEqual(review.describe({**record, "bugs": [{
            "id": "B1", "key": "fanout-1", "file": "a.py", "line": 2, "desc": "d", "why": "w"}]},
            {"kind": "bug", "ref": "B1"})[0], "B1 · fanout-1 · a.py:2 · branch ?")


if __name__ == "__main__":
    unittest.main()
