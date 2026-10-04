"""Tests for `scripts/judge/bugs.py` — the weekly bug sweep.

Design: docs/ai-assist/WEEKLY_JUDGE.md. The code is a throwaway
git repo; the judge and the merged-PR list are injected. Nothing spawns `claude` or `gh`.

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
_BUGS_PATH = _REPO / "scripts" / "judge" / "bugs.py"


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

    def sweep(self, events, previous=None, judge=None, prs=(), **kw):
        return self.b.sweep(events, date="2026-10-05", sha=self.sha, previous=previous,
                            judge=judge or self.judge(), merged_prs=lambda since: list(prs),
                            repo=self.repo, **kw)

    def commit(self, path, text, msg="c"):
        (self.repo / path).parent.mkdir(parents=True, exist_ok=True)
        (self.repo / path).write_text(text, encoding="utf-8")
        self.git("add", path)
        self.git("commit", "-q", "-m", msg)
        return self.git("rev-parse", "HEAD")


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
                         [("B1", "fanout-00000001", "open"), ("B2", "fanout-00000002", "unjudged")])
        self.assertEqual(bugs[1]["why"], "not judged (the judge returned no verdict)")
        self.assertEqual(cost, 0.1)
        self.assertNotIn("excerpt", bugs[0])

    def test_the_window_opens_at_the_previous_pass(self):
        # the previous record's `run.ts` is where this pass's candidates start
        before = _finding("fanout-00000001", ts="2026-10-03T00:00:00Z")
        after = _finding("fanout-00000002", ts="2026-10-04T12:00:00Z")
        bugs, _ = self.sweep([before, after], previous={"run": {"ts": "2026-10-04T00:00:00Z"}, "bugs": []})
        self.assertEqual([b.get("sources") or [b["key"]] for b in bugs], [["fanout-00000002"]])
        bugs, _ = self.sweep([before, after])   # no previous record: the last WINDOW_DAYS
        self.assertEqual([b.get("sources") for b in bugs], [["fanout-00000001", "fanout-00000002"]])

    def test_a_missing_file_is_gone_and_an_unmerged_change_waits_without_the_judge(self):
        self.git("checkout", "-q", "-b", "feat/x")
        found = _finding("fanout-00000001", file="b.py", branch="feat/x") | {"commit": self.sha}
        twin = _finding("fanout-00000009", file="b.py", branch="feat/x") | {"commit": self.sha}
        bugs, _ = self.sweep([found, twin])   # reviewed, not committed yet: the tip is still on main
        self.assertEqual([(b["status"], b.get("sources")) for b in bugs],
                         [("unmerged", ["fanout-00000001", "fanout-00000009"])])
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
        bugs, _ = self.sweep([], previous={"run": {"ts": "2026-10-04T00:00:00Z"}, "bugs": bugs})
        self.assertEqual([(b["key"], b["status"]) for b in bugs], [("fanout-00000001", "open")])

    def test_open_bugs_carry_until_gone_and_wont_fix_drops_out(self):
        bugs, _ = self.sweep([_finding("fanout-00000001"), _finding("fanout-00000002", line=3)])
        bugs[1]["status"] = "wont_fix"   # the owner's answer, applied before the next pass
        previous = {"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": bugs}
        again, _ = self.sweep([], previous=previous, judge=self.judge("gone"))
        self.assertEqual([(b["key"], b["status"], b["first_seen"]) for b in again],
                         [("fanout-00000001", "gone", "2026-10-05")])
        self.assertEqual(self.sweep([], previous={"run": {}, "bugs": again})[0], [])

    def test_a_new_finding_already_fixed_is_not_listed(self):
        self.assertEqual(self.sweep([_finding("fanout-00000001")], judge=self.judge("gone"))[0], [])

    def test_a_failed_judge_leaves_them_unjudged_and_carried(self):
        def boom(prompt):
            raise self.b._judge.JudgeError("no claude")
        bugs, cost = self.sweep([_finding("fanout-00000001")], judge=boom)
        self.assertEqual([(b["status"], cost) for b in bugs], [("unjudged", 0.0)])
        again, _ = self.sweep([], previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": bugs})
        self.assertEqual([(b["key"], b["status"]) for b in again], [("fanout-00000001", "open")])

    def test_an_unjudged_bug_judged_open_later_is_newly_open_that_pass(self):
        spec = importlib.util.spec_from_file_location("record", _REPO / "scripts" / "judge" / "record.py")
        record = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(record)
        bugs, _ = self.sweep([_finding("fanout-00000001")], no_judge=True)
        later = self.b.sweep([], date="2026-10-12", sha=self.sha, previous={"run": {}, "bugs": bugs},
                             judge=self.judge(), merged_prs=lambda since: [], repo=self.repo)[0]
        self.assertEqual([(b["status"], b["first_seen"], b["opened"]) for b in later],
                         [("open", "2026-10-05", "2026-10-12")])
        self.assertTrue(record.newly_open(later[0], "2026-10-12"))
        # still open the pass after: not asked again
        after = self.b.sweep([], date="2026-10-19", sha=self.sha, previous={"run": {}, "bugs": later},
                             judge=self.judge(), merged_prs=lambda since: [], repo=self.repo)[0]
        self.assertEqual(after[0]["opened"], "2026-10-12")
        self.assertFalse(record.newly_open(after[0], "2026-10-19"))

    def test_over_the_cap_is_unjudged_not_open(self):
        events = [_finding(f"fanout-{i:08x}", line=i + 1) for i in range(self.b.MAX_ITEMS + 2)]
        bugs, _ = self.sweep(events, judge=self.judge("not_a_bug"))
        self.assertEqual(sum(b["status"] == "dismissed" for b in bugs), self.b.MAX_ITEMS)
        self.assertEqual(sum(b["status"] == "unjudged" for b in bugs), 2)
        self.assertEqual(sum(b["status"] == "open" for b in bugs), 0)

    def test_no_judge_makes_no_call(self):
        bugs, cost = self.sweep([_finding("fanout-00000001")], no_judge=True)
        self.assertEqual(([b["status"] for b in bugs], cost, self.prompts), (["unjudged"], 0.0, []))
        self.assertEqual(bugs[0]["why"], "not judged (--no-judge)")


_PY_SRC = """import os


def helper(x):
    return x + 1


def target(a, b):
    total = 0
    for i in range(a):
        total += i
    return total * b
"""


class PrefilterTest(_Repo):
    def test_findings_on_a_frozen_record_are_dropped_new_and_carried(self):
        frozen = "docs/ai-assist/judge-runs/" + "2026-09-30.md"   # split: a fixture, not a doc pointer
        self.commit(frozen, "F6 open\n")
        self.sha = self.git("rev-parse", "HEAD")
        carried = {"key": "fanout-0000000a", "file": frozen, "line": 1, "desc": "d", "status": "open",
                   "first_seen": "2026-10-02"}
        bugs, _ = self.sweep([_finding("fanout-00000001", file=frozen, line=1)],
                             previous={"run": {}, "bugs": [carried]})
        self.assertEqual((bugs, self.prompts), ([], []))

    def test_findings_on_one_function_are_one_bug_listing_every_source(self):
        self.sha = self.commit("m.py", _PY_SRC)
        events = [_finding("fanout-00000001", file="m.py", line=10) | {"commit": self.sha},
                  _finding("fanout-00000002", file="m.py", line=12) | {"commit": self.sha},
                  _finding("fanout-00000003", file="m.py", line=5) | {"commit": self.sha}]
        bugs, _ = self.sweep(events)
        by = {b["key"]: b for b in bugs}
        self.assertEqual(sorted(by), ["fanout-00000001", "fanout-00000003"])
        self.assertEqual(by["fanout-00000001"]["sources"], ["fanout-00000001", "fanout-00000002"])
        self.assertEqual([a["key"] for a in by["fanout-00000001"]["also"]], ["fanout-00000002"])
        self.assertNotIn("sources", by["fanout-00000003"])
        self.assertIn("(also raised: desc fanout-00000002)", self.prompts[0])
        # the merged one carries; a source raised again next pass doesn't come back as new
        again, _ = self.sweep([_finding("fanout-00000002", file="m.py", line=12) | {"commit": self.sha}],
                              previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": bugs})
        self.assertEqual(sorted(b["key"] for b in again), ["fanout-00000001", "fanout-00000003"])

    def test_two_findings_on_one_line_with_no_function_merge(self):
        bugs, _ = self.sweep([_finding("fanout-00000001", line=40), _finding("fanout-00000002", line=40)])
        self.assertEqual([(b["key"], b.get("sources")) for b in bugs],
                         [("fanout-00000001", ["fanout-00000001", "fanout-00000002"])])

    def test_the_judge_sees_the_whole_function_even_after_its_line_moved(self):
        at = self.commit("m.py", _PY_SRC)
        self.sha = self.commit("m.py", "".join(f"# pad {i}\n" for i in range(150)) + _PY_SRC)
        self.sweep([_finding("fanout-00000001", file="m.py", line=10) | {"commit": at}])
        self.assertIn("def target(a, b):", self.prompts[0])
        self.assertIn("return total * b", self.prompts[0])
        self.assertNotIn("def helper", self.prompts[0])
        self.assertNotIn("# pad 0", self.prompts[0])

    def test_a_function_that_no_longer_exists_is_gone_without_the_judge(self):
        at = self.commit("m.py", _PY_SRC)
        self.sha = self.commit("m.py", _PY_SRC.split("def target")[0])
        bugs, cost = self.sweep([_finding("fanout-00000001", file="m.py", line=10) | {"commit": at}])
        self.assertEqual([(b["status"], b["why"]) for b in bugs],
                         [("gone", f"`target` isn't in `m.py` at {self.sha[:8]}")])
        self.assertEqual((self.prompts, cost), ([], 0.0))


class StrandedTest(_Repo):
    def setUp(self):
        super().setUp()
        self.git("checkout", "-q", "-b", "feat/x")
        self.head_oid = self.commit("f.py", "one\n", "first")
        self.git("checkout", "-q", "main")
        self.git("merge", "-q", "--no-ff", "-m", "Merge pull request #7", "feat/x")
        self.git("checkout", "-q", "feat/x")
        self.late = self.commit("f.py", "one\ntwo\n", "pushed after the merge")
        self.git("checkout", "-q", "main")
        self.git("update-ref", "refs/remotes/origin/feat/x", "feat/x")
        self.sha = self.git("rev-parse", "HEAD")
        self.pr = {"number": 7, "headRefName": "feat/x", "headRefOid": self.head_oid,
                   "mergeCommit": {"oid": self.sha}}

    def test_a_commit_pushed_after_the_merge_is_a_bug_until_it_lands(self):
        bugs, cost = self.sweep([], prs=[self.pr])
        self.assertEqual([(b["key"], b["status"], b["commits"]) for b in bugs],
                         [("stranded-pr7", "open", [self.late])])
        self.assertIn("pushed after the merge", bugs[0]["desc"])
        self.assertEqual((self.prompts, cost), ([], 0.0))
        # landed by cherry-pick (a new SHA, the same patch): gone, and not raised again
        self.git("cherry-pick", self.late)
        self.sha = self.git("rev-parse", "HEAD")
        again, _ = self.sweep([], previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": bugs},
                              prs=[self.pr])
        self.assertEqual([(b["key"], b["status"]) for b in again], [("stranded-pr7", "gone")])

    def test_a_pr_merged_after_the_pinned_sha_is_not_scanned(self):
        self.sha = self.git("rev-parse", "HEAD~1")   # the pass pinned main before #7 merged
        self.assertEqual(self.sweep([], prs=[self.pr])[0], [])

    def test_nothing_after_the_merge_or_a_deleted_branch_is_not_stranded(self):
        self.assertEqual(self.sweep([], prs=[{**self.pr, "headRefOid": self.late}])[0], [])
        self.git("update-ref", "-d", "refs/remotes/origin/feat/x")
        self.assertEqual(self.sweep([], prs=[self.pr])[0], [])
        self.assertEqual(self.sweep([], prs=[{**self.pr, "headRefOid": "f" * 40}])[0], [])


class RenderTest(unittest.TestCase):
    def test_unjudged_bugs_render_apart_and_stranded_ones_name_their_pr(self):
        spec = importlib.util.spec_from_file_location("record", _REPO / "scripts" / "judge" / "record.py")
        record = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(record)
        common = {"desc": "d", "first_seen": "2026-10-05", "marker": "confirmed"}
        md = "\n".join(record._render_bugs([
            {**common, "id": "B1", "key": "stranded-pr7", "kind": "stranded", "pr": 7, "branch": "feat/x",
             "file": "", "line": 0, "status": "open", "why": "w"},
            {**common, "id": "B2", "key": "fanout-1", "file": "a.py", "line": 2, "status": "unjudged",
             "why": "waiting for the judge (over the per-pass cap)"}]))
        self.assertIn("### B1 · open · stranded · PR #7 `feat/x` · `stranded-pr7`", md)
        self.assertNotIn("### B2", md)
        self.assertIn("### Waiting for the judge\n\n- B2 · `a.py:2` · `fanout-1` — waiting for the judge", md)
        self.assertIn("1 open · 1 unjudged", md)


class EnclosingTest(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("enclosing", _REPO / "scripts" / "judge" / "enclosing.py")
        self.e = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.e)

    def test_python(self):
        self.assertEqual(self.e.enclosing(_PY_SRC, 10, "m.py"), ("target", 8, 12))
        self.assertEqual(self.e.enclosing(_PY_SRC, 1, "m.py"), None)
        self.assertEqual(self.e.find(_PY_SRC, "helper", "m.py"), (4, 5))

    def test_julia(self):
        src = "module M\n\nfunction f(x)\n    if x\n        1\n    end\nend\n\ng(x) = x + 1\nend\n"
        self.assertEqual(self.e.enclosing(src, 5, "a.jl"), ("f", 3, 7))
        self.assertEqual(self.e.enclosing(src, 9, "a.jl"), ("g", 9, 9))

    def test_braces_ignore_strings_comments_and_control_flow(self):
        src = ("export function outer(a: number) {\n  if (a) {\n    const s = '}'  // }\n  }\n"
               "  return a\n}\nconst arrow = (x) => {\n  return x\n}\n")
        self.assertEqual(self.e.enclosing(src, 3, "u.ts"), ("outer", 1, 6))
        self.assertEqual(self.e.enclosing(src, 8, "u.ts"), ("arrow", 7, 9))

    def test_no_language_no_function(self):
        self.assertIsNone(self.e.enclosing("a\nb\n", 1, "README.md"))


class OwnerAnswerTest(unittest.TestCase):
    def test_a_wont_fix_answer_lands_on_the_bug(self):
        spec = importlib.util.spec_from_file_location("review", _REPO / "scripts" / "judge" / "review.py")
        review = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(review)
        record = {"date": "2026-10-05", "findings": [], "proposals": [],
                  "bugs": [{"id": "B1", "status": "open"}], "queue": [{"kind": "bug", "ref": "B1"}]}
        rows = [{"record": "2026-10-05", "event": "bug_status", "ref": "B1", "value": "wont_fix"}]
        self.assertEqual(review.apply_reviews(record, rows)["bugs"][0]["status"], "wont_fix")
        self.assertEqual(review.pending(record, rows), [])
        self.assertEqual(review.describe({**record, "bugs": [{
            "id": "B1", "key": "fanout-1", "status": "open", "file": "a.py", "line": 2, "desc": "d", "why": "w"}]},
            {"kind": "bug", "ref": "B1"}, use_colour=False)[0], "  open  a.py:2  ? · fanout-1")


if __name__ == "__main__":
    unittest.main()
