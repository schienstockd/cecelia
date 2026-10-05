"""Tests for `scripts/judge/bugs.py` — the weekly bug sweep.

Design: docs/ai-assist/WEEKLY_JUDGE.md. The code is a throwaway
git repo; the judge and the merged-PR list are injected. Nothing spawns `claude` or `gh`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import contextlib
import importlib.util
import io
import pathlib
import re
import subprocess
import tempfile
import unittest
from unittest import mock

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


def _resolved(slug, outcome, reason=None):
    return _row("fanout_audit_finding_resolved",
                {"slug": slug, "outcome": outcome} | ({"reason": reason} if reason else {}))


def _advisory(file="a.py", line=3, desc="maybe", **kw):
    return _row("fanout_audit_advisory", {"file": file, "line": line, "desc": desc, "marker": "plausible"}, **kw)


def _run_error(key, tool="create_chain", error="400 Bad Request: unknown step", ts="2026-10-04T00:30:00Z", **kw):
    """An `agent_run_finding` row, shaped like the run harness writes it: no branch, the run's SHA."""
    return {"event": "agent_run_finding", "ts": ts, "branch": None, "commit": "d" * 40,
            "payload": {"key": key, "tool": tool, "error": error, "file": None, "line": None} | kw}


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
        # a false_positive is the agent's claim, so it's judged like a shipped one
        self.assertEqual(keys[:3], ["fanout-00000002", "fanout-00000003", "fanout-00000004"])
        self.assertEqual(len(keys), 4)
        self.assertTrue(keys[3].startswith("fanout-"))   # the advisory, keyed like recital slugs it


class SweepTest(_Repo):
    def test_the_judge_sees_a_false_positive_and_its_reason(self):
        events = [_finding("fanout-00000001"), _resolved("fanout-00000001", "false_positive", "caller filters it"),
                  _finding("fanout-00000002", line=40), _resolved("fanout-00000002", "false_positive")]
        bugs, _ = self.sweep(events)
        self.assertIn("desc fanout-00000001\n(tagged false_positive: caller filters it)\n", self.prompts[0])
        self.assertIn("desc fanout-00000002\n(tagged false_positive)\n", self.prompts[0])
        self.assertEqual({b["key"]: b["status"] for b in bugs},
                         {"fanout-00000001": "open", "fanout-00000002": "open"})
        self.assertEqual(bugs[0]["reason"], "caller filters it")

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

    def test_a_failed_judge_keeps_a_carried_open_bug_open_and_says_so(self):
        bugs, _ = self.sweep([_finding("fanout-00000001")])
        self.assertEqual(bugs[0]["status"], "open")
        bugs[0]["verify"] = {"verdict": "fix", "date": "2026-10-05"}

        def boom(prompt):
            raise self.b._judge.JudgeError("judge failed (exit 1): overloaded")
        failures: dict = {}
        again = self.b.sweep([_finding("fanout-00000002", line=40, file="b.py")], date="2026-10-12", sha=self.sha,
                             previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": bugs}, judge=boom,
                             merged_prs=lambda since: [], repo=self.repo, failures=failures)[0]
        by = {b["key"]: b for b in again}
        # no evidence it was fixed: still on the work list, verdict and `opened` kept
        self.assertEqual((by["fanout-00000001"]["status"], by["fanout-00000001"]["opened"],
                          by["fanout-00000001"]["verify"]["verdict"]), ("open", "2026-10-05", "fix"))
        self.assertTrue(by["fanout-00000001"]["why"].startswith("still open; not judged"))
        self.assertEqual(failures, {"sweep": "judge failed (exit 1): overloaded"})

    def test_a_usage_limit_is_not_swallowed(self):
        def limited(prompt):
            raise self.b._judge.RateLimited("You've hit your session limit")
        with self.assertRaises(self.b._judge.RateLimited):
            self.sweep([_finding("fanout-00000001")], judge=limited)

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

    def test_a_carried_open_bug_over_the_cap_stays_open(self):
        events = [_finding(f"fanout-{i:08x}", line=i + 1) for i in range(self.b.MAX_ITEMS + 2)]
        bugs, _ = self.sweep(events)
        for b in bugs:   # all on the work list, as if an earlier pass had judged the overflow too
            b.update(status="open", opened="2026-10-05")
        later = self.b.sweep([], date="2026-10-12", sha=self.sha, previous={"run": {}, "bugs": bugs},
                             judge=self.judge(), merged_prs=lambda since: [], repo=self.repo)[0]
        self.assertEqual({b["status"] for b in later}, {"open"})
        self.assertEqual(sum(b["why"].startswith("still open; waiting for the judge") for b in later), 2)

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


class LandedTest(_Repo):
    """A commit since the last pass naming a carried bug's key: `fix_landed`, git only, no verdict."""

    def setUp(self):
        super().setUp()
        self.git("commit", "-q", "--allow-empty", "-m", "early: names fanout-00000001 before the last pass")
        self.sha = self.git("rev-parse", "HEAD")
        self.first, _ = self.sweep([_finding("fanout-00000001"), _finding("fanout-00000002", line=40)])
        self.previous = {"run": {"ts": "2026-10-05T00:00:00Z", "sha": self.sha}, "bugs": self.first}
        # a PR branch merged with a merge commit, a squash with `(#N)`, and a near-miss key
        self.git("checkout", "-q", "-b", "fix-fanout-00000001")
        self.fix = self.commit("a.py", "fixed\n" * 60, "fix: the thing (fanout-00000001)")
        self.git("checkout", "-q", "main")
        self.git("merge", "-q", "--no-ff", "fix-fanout-00000001", "-m",
                 "Merge pull request #12 from me/fix-fanout-00000001")
        self.squash = self.commit("b.py", "x\n", "fix: the other (fanout-00000002) (#13)")
        self.git("commit", "-q", "--allow-empty", "-m", "unrelated: fanout-000000012 is a longer key")
        self.sha = self.git("rev-parse", "HEAD")

    def test_commits_naming_a_carried_key_since_the_last_pass_are_recorded_with_their_pr(self):
        bugs, _ = self.sweep([], previous=self.previous)
        by = {b["key"]: b for b in bugs}
        self.assertEqual(by["fanout-00000001"]["fix_landed"],
                         [{"commit": self.fix, "subject": "fix: the thing (fanout-00000001)", "pr": 12}])
        self.assertEqual(by["fanout-00000002"]["fix_landed"],
                         [{"commit": self.squash, "subject": "fix: the other (fanout-00000002) (#13)", "pr": 13}])
        # evidence, not a verdict: the judge said live, so they stay open
        self.assertEqual({b["status"] for b in bugs}, {"open"})
        self.assertIn("(a commit naming this key landed: fix: the thing (fanout-00000001) "
                      f"({self.fix[:8]}))", self.prompts[-1])

    def test_the_judge_still_decides_gone(self):
        bugs, _ = self.sweep([], previous=self.previous, judge=self.judge("gone"))
        self.assertEqual([(b["status"], len(b["fix_landed"])) for b in bugs], [("gone", 1), ("gone", 1)])
        spec = importlib.util.spec_from_file_location("record", _REPO / "scripts" / "judge" / "record.py")
        record = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(record)
        self.assertEqual(record.landed_counts(bugs), (2, 2))

    def test_a_failed_judge_keeps_the_evidence_on_the_open_bug(self):
        def boom(prompt):
            raise self.b._judge.JudgeError("down")
        bugs, _ = self.sweep([], previous=self.previous, judge=boom)
        self.assertEqual([(b["status"], b["fix_landed"][0]["pr"]) for b in bugs], [("open", 12), ("open", 13)])

    def test_no_previous_sha_looks_for_nothing_and_old_evidence_is_not_carried(self):
        self.previous["run"].pop("sha")
        bugs, _ = self.sweep([], previous=self.previous)
        self.assertFalse([b for b in bugs if b.get("fix_landed")])
        stale = [{**b, "fix_landed": [{"commit": "f" * 40, "subject": "old"}]} for b in self.first]
        bugs, _ = self.sweep([], previous={"run": {"sha": self.sha}, "bugs": stale})
        self.assertFalse([b for b in bugs if b.get("fix_landed")])   # nothing since `sha`: found afresh

    def test_a_merge_commit_alone_names_the_key(self):
        self.assertEqual(
            self.b.landed_fixes({"fix-only"}, self.previous["run"]["sha"], self.sha, self.repo), {})
        self.git("checkout", "-q", "-b", "quiet")
        quiet = self.commit("c.py", "y\n", "change with no key")
        self.git("checkout", "-q", "main")
        self.git("merge", "-q", "--no-ff", "quiet", "-m", "Merge pull request #14 from me/fix-rep-abc123")
        merge = self.git("rev-parse", "HEAD")
        got = self.b.landed_fixes({"rep-abc123"}, self.previous["run"]["sha"], merge, self.repo)
        self.assertEqual(got, {"rep-abc123": [{"commit": merge, "pr": 14,
                                               "subject": "Merge pull request #14 from me/fix-rep-abc123"}]})
        self.assertNotIn(quiet, str(got))


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


class AgentRunTest(_Repo):
    def test_one_candidate_per_error_key_counting_its_runs(self):
        events = [_run_error("run-aaaa"), _run_error("run-aaaa", ts="2026-10-04T01:00:00Z"),
                  _run_error("run-bbbb", tool="get_cohort_qc", desc="cohort QC rejects a set uid"),
                  _row("agent_run_finding", {"tool": "set_gate"})]                     # no key: skipped
        cands = {c["key"]: c for c in self.b.candidates(events, since="2026-10-01")}
        self.assertEqual(sorted(cands), ["run-aaaa", "run-bbbb"])
        a = cands["run-aaaa"]
        self.assertEqual((a["kind"], a["runs"], a["last_seen"], a["branch"]), ("agent_run", 2, "2026-10-04T01:00:00Z", None))
        self.assertEqual(a["desc"], "`create_chain` failed: 400 Bad Request: unknown step")
        self.assertEqual(cands["run-bbbb"]["desc"], "cohort QC rejects a set uid")

    def test_an_error_without_a_file_is_open_without_the_judge_and_one_with_a_file_is_judged(self):
        events = [_run_error("run-aaaa"), _run_error("run-cccc", tool="set_gate", file="a.py", line=30)]
        bugs, _ = self.sweep(events)
        self.assertEqual({b["key"]: b["status"] for b in bugs}, {"run-aaaa": "open", "run-cccc": "open"})
        self.assertEqual(len(self.prompts), 1)
        self.assertIn("FINDING run-cccc (agent run, a.py:30", self.prompts[0])
        self.assertNotIn("run-aaaa", self.prompts[0])
        self.assertEqual(next(b for b in bugs if b["key"] == "run-aaaa")["opened"], "2026-10-05")

    def test_an_error_on_a_flagged_function_is_not_merged_into_the_finding(self):
        bugs, _ = self.sweep([_finding("fanout-00000001", line=30),
                              _run_error("run-cccc", tool="set_gate", file="a.py", line=30)])
        self.assertEqual(sorted((b["key"], b.get("sources")) for b in bugs),
                         [("fanout-00000001", None), ("run-cccc", None)])

    def test_a_carried_error_hit_again_counts_the_runs_instead_of_a_new_bug(self):
        prev, _ = self.sweep([_run_error("run-aaaa")])
        previous = {"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": prev}
        bugs, _ = self.sweep([_run_error("run-aaaa", ts="2026-10-11T00:00:00Z"),
                              _run_error("run-aaaa", ts="2026-10-12T00:00:00Z")], previous=previous)
        self.assertEqual([(b["key"], b["status"], b["runs"], b["last_seen"]) for b in bugs],
                         [("run-aaaa", "open", 3, "2026-10-12T00:00:00Z")])

    def test_a_wont_fix_error_stays_closed_and_a_fixed_one_that_returns_is_new(self):
        prev, _ = self.sweep([_run_error("run-aaaa"), _run_error("run-bbbb"), _run_error("run-cccc")])
        closed = {"run-aaaa": "wont_fix", "run-bbbb": "gone", "run-cccc": "dismissed"}
        prev = [{**b, "status": closed[b["key"]]} for b in prev]
        again = [_run_error(k, ts="2026-10-11T00:00:00Z") for k in closed]
        bugs, _ = self.sweep(again, previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": prev})
        got = {b["key"]: (b["status"], b.get("muted", False), b["runs"]) for b in bugs}
        self.assertEqual(got, {"run-aaaa": ("wont_fix", True, 2), "run-bbbb": ("open", False, 1),
                               "run-cccc": ("open", False, 1)})
        bugs, _ = self.sweep([], previous={"run": {"ts": "2026-10-12T00:00:00Z"}, "bugs": bugs})
        self.assertEqual([(b["key"], b["status"]) for b in bugs],
                         [("run-bbbb", "open"), ("run-cccc", "open"), ("run-aaaa", "wont_fix")])


    def _repeat(self, runs, ts, error="HTTP 400: Invalid chain name 'a + b'"):
        return _run_error("rep-aaaa", ts=ts, kind="repeat", runs=runs, error=error, desc=f"agents hit this in {runs} runs: {error}")

    def test_a_repeated_4xx_takes_the_emitters_count_and_latest_message(self):
        events = [self._repeat(2, "2026-10-04T01:00:00Z"),
                  {**self._repeat(4, "2026-10-04T03:00:00Z", error="newer hint"), "commit": "e" * 40},
                  self._repeat(3, "2026-10-04T02:00:00Z")]   # out of order: the count never goes down
        (c,) = self.b.candidates(events, since="2026-10-01")
        self.assertEqual((c["kind"], c["marker"], c["repeat"], c["runs"], c["error"]),
                         ("agent_run", "repeated agent error", True, 4, "HTTP 400: Invalid chain name 'a + b'"))
        self.assertEqual(c["commit"], "d" * 40)   # the first sighting's: briefs say "first at commit"
        bugs, _ = self.sweep(events)
        self.assertEqual([(b["status"], b["runs"]) for b in bugs], [("open", 4)])
        self.assertIn("is the platform failing to guide them?", bugs[0]["why"])
        self.assertEqual(self.prompts, [])   # no file:line: straight to verify, like any agent-run error

    def test_a_dismissed_repeat_stays_muted_until_its_count_doubles(self):
        prev, _ = self.sweep([self._repeat(3, "2026-10-04T01:00:00Z")])
        prev = [{**b, "status": "dismissed", "verify": {"verdict": "dismiss", "effect": "the 400 names a passing name"}}
                for b in prev]
        bugs, _ = self.sweep([self._repeat(5, "2026-10-11T00:00:00Z")],
                             previous={"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": prev})
        self.assertEqual([(b["status"], b.get("muted"), b["runs"], b["closed_runs"]) for b in bugs],
                         [("dismissed", True, 5, 3)])
        self.assertEqual(bugs[0]["why"], "dismissed at 3 run(s); re-opens at 6")
        quiet, _ = self.sweep([], previous={"run": {"ts": "2026-10-12T00:00:00Z"}, "bugs": bugs})   # not hit: still muted
        self.assertEqual([(b["status"], b["closed_runs"]) for b in quiet], [("dismissed", 3)])
        bugs, _ = self.sweep([self._repeat(6, "2026-10-18T00:00:00Z")],
                             previous={"run": {"ts": "2026-10-12T00:00:00Z"}, "bugs": quiet})
        (b,) = bugs
        self.assertEqual((b["status"], b["runs"], b.get("muted"), "verify" in b, "closed_runs" in b),
                         ("open", 6, None, False, False))
        self.assertEqual(b["why"], "dismissed at 3 run(s), hit in 6 now (dismissed as: the 400 names a passing name)")

    def test_verify_keeps_a_repeat_out_of_a_runs_group(self):
        spec = importlib.util.spec_from_file_location("verify", _REPO / "scripts" / "judge" / "verify.py")
        verify = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(verify)
        common = {"kind": "agent_run", "commit": "d" * 40, "status": "open", "desc": "d"}
        groups = verify.groups([{**common, "key": "run-a"}, {**common, "key": "run-b"},
                                {**common, "key": "rep-c", "repeat": True}])
        self.assertEqual(sorted(sorted(b["key"] for b in g) for g in groups), [["rep-c"], ["run-a", "run-b"]])

    def test_a_plain_dismissed_error_is_still_not_carried(self):
        prev, _ = self.sweep([_run_error("run-aaaa")])
        bugs, _ = self.sweep([], previous={"run": {"ts": "2026-10-05T00:00:00Z"},
                                           "bugs": [{**b, "status": "dismissed"} for b in prev]})
        self.assertEqual(bugs, [])


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

    def test_an_agent_run_error_names_its_tool_and_runs_and_closed_ones_are_one_line(self):
        spec = importlib.util.spec_from_file_location("record", _REPO / "scripts" / "judge" / "record.py")
        record = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(record)
        common = {"kind": "agent_run", "marker": "agent run", "tool": "create_chain", "file": None, "line": None,
                  "desc": "`create_chain` failed: 400", "first_seen": "2026-10-05", "runs": 3,
                  "last_seen": "2026-10-12T00:00:00Z", "why": "w"}
        md = "\n".join(record._render_bugs([{**common, "id": "B1", "key": "run-aaaa", "status": "open"},
                                            {**common, "id": "B2", "key": "run-bbbb", "status": "wont_fix",
                                             "muted": True}]))
        self.assertIn("### B1 · open · `agent run · create_chain` · `run-aaaa`", md)
        self.assertIn("**Error** (`create_chain`, hit in 3 run(s), first seen 2026-10-05, last 2026-10-12)", md)
        self.assertNotIn("### B2", md)
        self.assertIn("- B2 · wont fix · `agent run · create_chain` · `run-bbbb` — hit in 3 run(s), last 2026-10-12", md)
        md = "\n".join(record._render_bugs([{**common, "id": "B1", "key": "rep-aaaa", "status": "open", "repeat": True,
                                             "marker": "repeated agent error"}]))
        self.assertIn("### B1 · open · `repeated agent error · create_chain` · `rep-aaaa`", md)
        self.assertIn("**Repeated error** (`create_chain`, hit in 3 run(s)", md)


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



class MainLimitTest(unittest.TestCase):
    """`pixi run judge-bugs` on the usage limit: one line with the reset time, exit 75 — no traceback."""

    def test_a_usage_limit_is_one_line_and_exit_75(self):
        b = _load_bugs()

        def limited(*a, **k):
            raise b._judge.RateLimited("You've hit your session limit · resets 1:40am (Australia/Sydney)")
        review = mock.Mock(applied_pass_records=lambda before: [])
        err = io.StringIO()
        with mock.patch.object(b, "sweep", limited), mock.patch.object(b, "read_events", return_value=[]), \
                mock.patch.object(b, "git_output", return_value="abc"), \
                mock.patch.object(b, "_load_sibling", return_value=review), contextlib.redirect_stderr(err):
            code = b.main(["--date", "2026-10-06"])
        self.assertEqual(code, 75)
        self.assertRegex(err.getvalue(), r"^judge-bugs: usage limit — lifts \S+: You've hit your session limit")
        self.assertNotIn("Traceback", err.getvalue())


if __name__ == "__main__":
    unittest.main()
