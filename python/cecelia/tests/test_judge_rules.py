"""Tests for `scripts/judge/rules.py` — which CLAUDE.md rules keep breaking, and what to tighten.

Design: docs/ai-assist/WEEKLY_JUDGE.md. The finding→rule judge is injected and binning runs on a
throwaway git repo. Nothing spawns `claude`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import contextlib
import importlib.util
import io
import pathlib
import subprocess
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
_RULES_PATH = _REPO / "scripts" / "judge" / "rules.py"


def _load_rules():
    spec = importlib.util.spec_from_file_location("rules", _RULES_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _finding_event(slug, ts="2026-10-01T00:00:00Z", session=None):
    return {"event": "convention_check_finding", "ts": ts, "session": session or f"s-{slug}",
            "payload": {"slug": slug, "file": "a.py", "desc": f"desc {slug}", "marker": "should reuse"}}


class TallyTest(unittest.TestCase):
    RULES = ["CLAUDE.md → *Testing*", "CLAUDE.md → *Windows compatibility*"]

    def setUp(self):
        self.r = _load_rules()

    def _rows(self, findings, mapping, bins):
        return self.r.tally(findings, [{"slug": s, "rule": r} for s, r in mapping.items()], bins, self.RULES)

    def test_sessions_not_findings_decide_a_proposal(self):
        # ten findings from one session's fanout are one pattern, not ten sessions
        one = [_finding_event(f"f{i}", session="s1") for i in range(10)]
        rows = self._rows(one, {f"f{i}": self.RULES[0] for i in range(10)}, {f"f{i}": "agent_made" for i in range(10)})
        self.assertEqual((rows[0]["findings"], rows[0]["sessions"]), (10, 1))
        self.assertEqual(self.r.proposals_from(rows), [])
        three = [_finding_event(s, session=f"s{i}") for i, s in enumerate("abc")]
        rows = self._rows(three, dict.fromkeys("abc", self.RULES[0]), dict.fromkeys("abc", "agent_made"))
        props = self.r.proposals_from(rows)
        self.assertEqual([(p["id"], p["kind"], p["rule"], p["sources"]) for p in props],
                         [("P1", "tighten", self.RULES[0], ["a", "b", "c"])])

    def test_legacy_findings_propose_a_ratchet_and_unknown_count_toward_neither(self):
        ev = [_finding_event(s, session=f"s{i}") for i, s in enumerate("abcd")]
        rows = self._rows(ev, dict.fromkeys("abcd", self.RULES[1]),
                          {"a": "legacy", "b": "legacy", "c": "legacy", "d": "unknown"})
        self.assertEqual((rows[0]["legacy"], rows[0]["agent_made"], rows[0]["sessions"]), (3, 0, 4))
        self.assertEqual([p["kind"] for p in self.r.proposals_from(rows)], ["ratchet"])

    def test_none_and_invented_rules_are_dropped(self):
        ev = [_finding_event("a"), _finding_event("b")]
        rows = self._rows(ev, {"a": "none", "b": "NOT_A_RULE.md → *Invented*"}, {})
        self.assertEqual(rows, [])

    def test_rows_sort_by_sessions(self):
        ev = [_finding_event(s, session=f"s{i}") for i, s in enumerate("abc")]
        rows = self._rows(ev, {"a": self.RULES[1], "b": self.RULES[0], "c": self.RULES[0]}, {})
        self.assertEqual([r["rule"] for r in rows], [self.RULES[0], self.RULES[1]])


class ProposeTest(unittest.TestCase):
    def setUp(self):
        self.r = _load_rules()
        self.seen = []

    def test_one_judge_call_sees_every_finding_and_the_rules(self):
        def assign(prompt):
            self.seen.append(prompt)
            return {"assignments": [{"slug": "y", "rule": "CLAUDE.md → *Testing*"}]}, 0.3
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "CLAUDE.md").write_text("## Testing\n", encoding="utf-8")
            rows, props, stats, cost = self.r.propose([_finding_event("y")], date="2026-10-02", assign=assign,
                                                      repo=pathlib.Path(d), git=lambda *a: None)
        self.assertEqual(len(self.seen), 1)
        self.assertIn("desc y", self.seen[0])
        self.assertIn("CLAUDE.md → *Testing*", self.seen[0])
        self.assertEqual((rows[0]["findings"], props, cost), (1, [], 0.3))
        self.assertNotIn("_sources", rows[0])

    def test_no_findings_skip_the_judge(self):
        out = self.r.propose([], date="2026-10-02", assign=lambda p: self.seen.append(p), git=lambda *a: None)
        self.assertEqual((out[0], out[1], out[3], self.seen), ([], [], 0.0, []))

    def test_a_failed_judge_leaves_both_lists_empty(self):
        def boom(prompt):
            raise self.r._judge.JudgeError("down")
        failures: dict = {}
        rows, props, _, cost = self.r.propose([_finding_event("y")], date="2026-10-02", assign=boom,
                                              git=lambda *a: None, failures=failures)
        self.assertEqual((rows, props, cost, failures), ([], [], 0.0, {"rules": "down"}))

    def test_a_usage_limit_is_not_swallowed(self):
        def limited(prompt):
            raise self.r._judge.RateLimited("session limit")
        with self.assertRaises(self.r._judge.RateLimited):
            self.r.propose([_finding_event("y")], date="2026-10-02", assign=limited, git=lambda *a: None)

    def _three_sessions(self, edited=None):
        ev = [_finding_event(x, ts=f"2026-09-2{i}T00:00:00Z") for i, x in enumerate("abc", 1)]
        assign = lambda p: ({"assignments": [{"slug": x, "rule": "CLAUDE.md → *Testing*"} for x in "abc"]}, 0.1)  # noqa: E731
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "CLAUDE.md").write_text("## Testing\n", encoding="utf-8")
            return self.r.propose(ev, date="2026-10-02", assign=assign, repo=pathlib.Path(d), git=lambda *a: None,
                                  edited=edited)

    def test_findings_from_before_the_sections_last_edit_dont_count(self):
        self.assertEqual([p["kind"] for p in self._three_sessions()[1]], ["tighten"])
        # tightened on 09-22, 11:00 Sydney = 09-22 00:00 UTC: findings a and b predate it, c is after
        rows, props, _, _ = self._three_sessions(lambda rl: {"CLAUDE.md → *Testing*": "2026-09-22T11:00:00+11:00"})
        self.assertEqual((rows[0]["findings"], props), (1, []))

    def test_since_edit_keeps_rules_git_cant_place_and_findings_with_no_time(self):
        f = [_finding_event("a", ts="2026-09-21T00:00:00Z"), {**_finding_event("b"), "ts": ""}]
        out = self.r.since_edit(f, [{"slug": "a", "rule": "X"}, {"slug": "a", "rule": "Y"}, {"slug": "b", "rule": "X"}],
                                {"X": "2026-09-22T00:00:00Z"})
        self.assertEqual(out, [{"slug": "a", "rule": "Y"}, {"slug": "b", "rule": "X"}])

    def test_section_edits_asks_git_for_the_sections_own_lines(self):
        text = "# T\n\n## Image / OME-ZARR (`zarr_utils`) [x]\nbody\n\n## Testing\nrun it\n"
        calls = []

        def git(*args):
            calls.append(args)
            return text if args[0] == "show" else "2026-09-22T11:00:00+11:00\n\ndiff --git a/CLAUDE.md"
        out = self.r.section_edits(["CLAUDE.md → *Image / OME-ZARR (`zarr_utils`) [x]*", "CLAUDE.md → *Testing*",
                                    "CLAUDE.md → *Gone*"], git)
        self.assertEqual(out, {"CLAUDE.md → *Image / OME-ZARR (`zarr_utils`) [x]*": "2026-09-22T11:00:00+11:00",
                               "CLAUDE.md → *Testing*": "2026-09-22T11:00:00+11:00"})
        self.assertEqual([c[c.index("-L") + 1] for c in calls if c[0] == "log"], ["3,5:CLAUDE.md", "6,7:CLAUDE.md"])
        self.assertEqual(sum(c[0] == "show" for c in calls), 1)   # one read per file

    def test_red_team_old_and_repeated_findings_are_not_counted(self):
        events = [_finding_event("fanout-c8482224"), _finding_event("x", ts="2026-08-01T00:00:00Z"),
                  _finding_event("y"), _finding_event("y")]
        kept = self.r.recent_findings(events, today=self.r._dt.date(2026, 10, 1))
        self.assertEqual([e["payload"]["slug"] for e in kept], ["y"])

    def test_rules_are_the_claude_md_sections(self):
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "CLAUDE.md").write_text("# T\n## Testing\nx\n## Git & commits\n", encoding="utf-8")
            self.assertEqual(self.r.rules(pathlib.Path(d)), ["CLAUDE.md → *Testing*", "CLAUDE.md → *Git & commits*"])


def _sh(repo, *args):
    return subprocess.run(["git", *args], cwd=repo, capture_output=True, text=True, check=True).stdout.strip()


class BinFindingTest(unittest.TestCase):
    """A throwaway repo: `base`, then the reviewed diff committed on `feat` as `child`. No network."""
    LONG = [f"    result_{i} = compute_something_long(alpha_{i}, beta_{i}, gamma_{i}, delta_{i})" for i in range(6)]

    def setUp(self):
        self.c = _load_rules()
        self.tmp = tempfile.TemporaryDirectory()
        r = self.repo = pathlib.Path(self.tmp.name)
        _sh(r, "init", "-q", "-b", "main")
        _sh(r, "config", "user.email", "t@t"); _sh(r, "config", "user.name", "t")
        (r / "a.jl").write_text("".join(f"line {i}\n" for i in range(1, 11)), encoding="utf-8")
        (r / "b.jl").write_text("untouched\n" * 5, encoding="utf-8")
        (r / "c.jl").write_text("\n".join(["head"] + self.LONG + ["tail"]) + "\n", encoding="utf-8")
        (r / "old.jl").write_text("x\n" * 4, encoding="utf-8")
        _sh(r, "add", "-A"); _sh(r, "commit", "-qm", "base")
        self.base = _sh(r, "rev-parse", "HEAD")
        _sh(r, "switch", "-qc", "feat")
        a = [f"line {i}" for i in range(1, 11)]
        a[1] = "line 2 rewritten"                              # replaced: line 2
        a[4:4] = ["new A", "new B"]                            # pure addition: lines 5-6
        a += self.LONG                                         # moved here from c.jl: lines 13-18
        (r / "a.jl").write_text("\n".join(a) + "\n", encoding="utf-8")
        (r / "c.jl").write_text("head\ntail\n", encoding="utf-8")
        (r / "new.jl").write_text("fresh\n", encoding="utf-8")
        _sh(r, "mv", "old.jl", "renamed.jl")
        _sh(r, "add", "-A"); _sh(r, "commit", "-qm", "the reviewed diff")
        self.git = self.c._git_in(self.repo)

    def tearDown(self):
        self.tmp.cleanup()

    def _bin(self, file, line, *, commit=None, event="fanout_audit_finding", marker="confirmed", desc=""):
        row = {"event": event, "commit": commit or self.base, "branch": "feat",
               "payload": {"slug": "s", "file": file, "line": str(line), "marker": marker, "desc": desc}}
        return self.c.bin_finding(row, git=self.git, children=self.c._children(self.git))

    def test_a_line_the_diff_added_is_agent_made(self):
        self.assertEqual(self._bin("a.jl", 5), "agent_made")
        self.assertEqual(self._bin("new.jl", 1), "agent_made")

    def test_a_line_the_diff_replaced_or_left_alone_is_legacy(self):
        self.assertEqual(self._bin("a.jl", 2), "legacy")
        self.assertEqual(self._bin("a.jl", 9), "legacy")
        self.assertEqual(self._bin("b.jl", 3), "legacy")

    def test_code_moved_in_from_another_file_is_legacy(self):
        self.assertEqual(self._bin("a.jl", 15), "legacy")

    def test_unresolvable_findings_are_unknown(self):
        self.assertEqual(self._bin("renamed.jl", 1), "unknown")
        self.assertEqual(self._bin("a.jl", 2, commit="0" * 40), "unknown")
        self.assertEqual(self._bin("a.jl", ""), "unknown")

    def test_convention_findings_on_additions_are_agent_made(self):
        ev = "convention_check_finding"
        self.assertEqual(self._bin("a.jl", 1, event=ev, marker="should reuse"), "agent_made")
        self.assertEqual(self._bin("a.jl", 1, event=ev, marker="wrong home"), "agent_made")
        self.assertEqual(self._bin("a.jl", 1, event=ev, marker="potential duplicate"), "unknown")

    def test_a_fix_inserted_at_the_flagged_line_is_legacy_when_the_quoted_code_was_there(self):
        # The session fixed the sibling by inserting a line above it, so the flagged line number (read
        # before the fix) now lands on a pure addition. The code the finding quotes stood there at the base.
        r = self.repo
        _sh(r, "switch", "-qc", "fixer", self.base)
        base = (r / "a.jl").read_text(encoding="utf-8").splitlines()
        base[4] = "    files = entry isa AbstractVector ? collect(entry) : [string(entry)]"
        (r / "a.jl").write_text("\n".join(base) + "\n", encoding="utf-8"); _sh(r, "add", "-A"); _sh(r, "commit", "-qm", "base2")
        base2 = _sh(r, "rev-parse", "HEAD")
        fixed = base[:4] + ["    entry = unwrap(entry)"] + base[4:]
        (r / "a.jl").write_text("\n".join(fixed) + "\n", encoding="utf-8"); _sh(r, "add", "-A"); _sh(r, "commit", "-qm", "fix")
        row = {"event": "fanout_audit_finding", "commit": base2, "branch": "fixer",
               "payload": {"slug": "s", "file": "a.jl", "line": "5", "marker": "confirmed",
                           "desc": "the reader does `entry isa AbstractVector ? collect(entry) : [string(entry)]`"}}
        bin_ = lambda: self.c.bin_finding(row, git=self.git, children=self.c._children(self.git))
        self.assertEqual(bin_(), "legacy")
        row["payload"]["desc"] = "it builds `paths = join(parts, sep)`, which the base never had"
        self.assertEqual(bin_(), "agent_made")
        row["payload"]["desc"] = "plain words like `the entry` are not code"
        self.assertEqual(bin_(), "agent_made")

    def test_a_main_merge_into_the_branch_does_not_make_every_sibling_on_branch(self):
        r = self.repo
        _sh(r, "switch", "-qc", "side", self.base)
        (r / "a.jl").write_text("".join(f"line {i}\n" for i in range(1, 11)) + "side\n", encoding="utf-8")
        _sh(r, "add", "-A"); _sh(r, "commit", "-qm", "unrelated sibling touching a.jl")
        _sh(r, "switch", "-q", "feat"); _sh(r, "merge", "-q", "--no-edit", "-X", "ours", "side")
        self.assertEqual(self._bin("a.jl", 5), "agent_made")

    def test_off_branch_children_of_a_shared_base_do_not_decide(self):
        # another worktree's commit on the same base that never touched a.jl
        _sh(self.repo, "switch", "-qc", "other", self.base)
        (self.repo / "z.jl").write_text("z\n", encoding="utf-8"); _sh(self.repo, "add", "-A"); _sh(self.repo, "commit", "-qm", "other")
        self.assertEqual(self._bin("a.jl", 5), "agent_made")



class MainLimitTest(unittest.TestCase):
    """`pixi run judge-rules` on the usage limit: one line with the reset time, exit 75 — no traceback."""

    def test_a_usage_limit_is_one_line_and_exit_75(self):
        r = _load_rules()

        def limited(*a, **k):
            raise r._judge.RateLimited("You've hit your session limit · resets 1:40am (Australia/Sydney)")
        err = io.StringIO()
        with mock.patch.object(r, "propose", limited), mock.patch.object(r, "read_events", return_value=[]), \
                contextlib.redirect_stderr(err):
            code = r.main(["--date", "2026-10-06"])
        self.assertEqual(code, 75)
        self.assertRegex(err.getvalue(), r"^judge-rules: usage limit — lifts \S+: ")
        self.assertEqual(err.getvalue().count("\n"), 1)


if __name__ == "__main__":
    unittest.main()
