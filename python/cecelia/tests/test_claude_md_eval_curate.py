"""Tests for `scripts/claude_md_eval/curate.py` — curation proposals (retire / add / setup).

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decisions 1, 8, 12 and phase 5. Records are
minimal synthetic dicts; the finding→rule judge is injected. Nothing spawns `claude`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import pathlib
import subprocess
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_CURATE_PATH = _REPO / "scripts" / "claude_md_eval" / "curate.py"


def _load_curate():
    spec = importlib.util.spec_from_file_location("curate", _CURATE_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _rec(date, scores, *, prompt_hash="h1", sandbox="s1", retries=(), findings=(), cost=1.0, rescored=None):
    """`scores` = compliant runs out of 3 per prompt, as logged; `rescored` overrides that per
    prompt for the stored rescore (no trace on disk, so it is what `rescored_now` falls back to)."""
    def verdicts(c):
        return ["compliant"] * c + ["noncompliant"] * (3 - c)
    traces = []
    for pid, c in scores.items():
        now = verdicts((rescored or {}).get(pid, c))
        traces += [{"prompt_id": pid, "cost_usd": cost if i == 0 else 0.0, "trace": None,
                    "scores": {"raw": v, "rescored": now[i]}} for i, v in enumerate(verdicts(c))]
    return {
        "date": date, "kind": "pass",
        "run": {"prompt_set": {"hash": prompt_hash}, "sandbox": sandbox, "full_catalog": True,
                "retries": list(retries)},
        "results": {"per_prompt": {pid: {"raw": {"compliant": c, "total": 3}} for pid, c in scores.items()}},
        "traces": traces,
        "findings": list(findings),
    }


def _finding_event(slug, ts="2026-10-01T00:00:00Z"):
    return {"event": "convention_check_finding", "ts": ts,
            "payload": {"slug": slug, "file": "a.py", "desc": f"desc {slug}", "marker": "should reuse"}}


class RetireTest(unittest.TestCase):
    def setUp(self):
        self.c = _load_curate()

    def _history(self, **over):
        return [_rec("2026-09-16", {"p1": 3, "canary": 3}), _rec("2026-09-23", {"p1": 3, "canary": 3}, **over)]

    def test_three_green_comparable_passes_propose_a_retire(self):
        out = self.c.retire_proposals(_rec("2026-09-30", {"p1": 3, "canary": 3}), self._history())
        self.assertEqual([(p["prompt"], p["sources"]) for p in out],
                         [("p1", ["2026-09-30", "2026-09-23", "2026-09-16"])])   # canary never

    def test_the_streak_is_read_under_one_scorer_not_as_logged(self):
        # logged green throughout, but today's scorer fails the oldest pass → no streak
        history = [_rec("2026-09-16", {"p1": 3}, rescored={"p1": 1}), _rec("2026-09-23", {"p1": 3})]
        self.assertEqual(self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), history), [])
        # and the reverse: logged red, green under today's scorer → it retires
        history = [_rec("2026-09-16", {"p1": 1}, rescored={"p1": 3}), _rec("2026-09-23", {"p1": 3})]
        self.assertEqual([p["prompt"] for p in self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), history)],
                         ["p1"])

    def test_an_infra_retry_in_the_window_blocks_it(self):
        out = self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}),
                                      self._history(retries=[{"prompt_id": "p1", "attempt": 1}]))
        self.assertEqual(out, [])

    def test_a_sandbox_or_prompt_set_change_ends_the_streak(self):
        for over in ({"sandbox": "s0"}, {"prompt_hash": "h0"}):
            self.assertEqual(self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), self._history(**over)), [], over)

    def test_one_red_pass_in_the_window_blocks_it(self):
        history = [_rec("2026-09-16", {"p1": 3}), _rec("2026-09-23", {"p1": 2})]
        self.assertEqual(self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), history), [])


class AddTest(unittest.TestCase):
    def setUp(self):
        self.c = _load_curate()
        self.rules = ["CLAUDE.md → *Testing*", "CLAUDE.md → *Windows compatibility*"]
        self.prompts = [{"id": "dir-size", "rule": "r", "rule_section": "CLAUDE.md → *Windows compatibility*"}]
        self.prompt_seen = []

    def _assign(self, mapping):
        def fn(prompt):
            self.prompt_seen.append(prompt)
            return {"assignments": [{"slug": s, "rule": r, "covered_by": c} for s, (r, c) in mapping.items()]}, 0.2
        return fn

    def _findings(self, *slugs):
        return [_finding_event(s) for s in slugs]

    def test_three_uncovered_findings_on_one_rule_propose_an_add_citing_them(self):
        out, cost = self.c.add_proposals(self._findings("a", "b", "c", "d"), rule_list=self.rules,
                                         prompts=self.prompts, assign=self._assign({
                                             "a": (self.rules[0], ""), "b": (self.rules[0], ""),
                                             "c": (self.rules[0], ""), "d": (self.rules[1], "dir-size")}))
        self.assertEqual([(p["rule"], p["sources"]) for p in out], [(self.rules[0], ["a", "b", "c"])])
        self.assertEqual(cost, 0.2)
        self.assertIn("desc a", self.prompt_seen[0])

    def test_a_covered_rule_or_an_invented_one_is_never_proposed(self):
        out, _ = self.c.add_proposals(self._findings("a", "b", "c"), rule_list=self.rules, prompts=self.prompts,
                                      assign=self._assign({"a": (self.rules[1], "dir-size"),
                                                           "b": ("NOT_A_RULE.md → *Invented*", ""),
                                                           "c": (self.rules[1], "dir-size")}))
        self.assertEqual(out, [])

    def test_fewer_than_three_findings_skip_the_judge(self):
        out, cost = self.c.add_proposals(self._findings("a", "b"), rule_list=self.rules, prompts=self.prompts,
                                         assign=self._assign({}))
        self.assertEqual((out, cost, self.prompt_seen), ([], 0.0, []))

    def test_red_team_and_old_findings_are_not_counted(self):
        events = [_finding_event("fanout-c8482224"), _finding_event("x", ts="2026-08-01T00:00:00Z"),
                  _finding_event("y")]
        kept = self.c.recent_findings(events, today=self.c._dt.date(2026, 10, 1))
        self.assertEqual([e["payload"]["slug"] for e in kept], ["y"])

    def test_an_add_over_the_cap_pairs_with_a_removal(self):
        adds = [{"kind": "add", "rule": "r", "sources": [], "summary": "Add"}]
        current = _rec("2026-09-30", {"p1": 3, "p2": 3}, cost=10.0)    # $20 already
        out = self.c.pair_with_removals(adds, [{"prompt": "p2"}], current, [])
        self.assertEqual(out[0]["paired_removal"], "p2")
        fresh = [{"kind": "add", "rule": "r", "sources": [], "summary": "Add"}]
        under = self.c.pair_with_removals(fresh, [], _rec("2026-09-30", {"p1": 3}, cost=1.0), [])
        self.assertNotIn("paired_removal", under[0])

    def test_rules_are_the_claude_md_sections_in_rule_section_form(self):
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "CLAUDE.md").write_text("# T\n## Testing\nx\n## Git & commits\n", encoding="utf-8")
            self.assertEqual(self.c.rules(pathlib.Path(d)), ["CLAUDE.md → *Testing*", "CLAUDE.md → *Git & commits*"])


class FindingProposalTest(unittest.TestCase):
    def test_only_recurring_genuine_and_scorer_findings_and_the_delta_reads_the_hypothesis(self):
        c = _load_curate()
        findings = [
            {"id": "F1", "slug": "p1", "class": "genuine", "status": "open", "recurrence": "recurring",
             "title": "t1", "proposed_fix": "x"},
            {"id": "F2", "slug": "p2", "class": "genuine", "status": "open", "recurrence": "watch",
             "title": "t2", "proposed_fix": "x"},
            {"id": "F3", "slug": "p3", "class": "decision", "status": "open", "recurrence": "recurring",
             "title": "t3", "proposed_fix": "x"},
        ]
        out = c.finding_proposals(_rec("2026-09-30", {}, findings=findings))
        self.assertEqual([(p["kind"], p["sources"]) for p in out], [("setup", ["F1"])])
        self.assertEqual(c._record._HYPOTHESIS_RE.search(out[0]["hypothesis"]).group(1), "p1")


class BinnedAddTest(unittest.TestCase):
    """Decision 21: only `agent_made` findings feed an add; `legacy` ones feed a ratchet."""
    def setUp(self):
        self.c = _load_curate()
        self.rules = ["CLAUDE.md → *Testing*", "CLAUDE.md → *Windows compatibility*"]

    def _assign(self, rows):
        return lambda prompt: ({"assignments": [{"slug": s, "rule": r, "covered_by": "", "correct_example": e}
                                                for s, r, e in rows]}, 0.1)

    def test_legacy_findings_propose_a_ratchet_and_unknown_count_toward_nothing(self):
        t, w = self.rules
        rows = [("a", t, "x.py"), ("b", t, "x.py"), ("c", t, ""), ("l1", w, ""), ("l2", w, ""), ("l3", w, ""),
                ("u", t, "")]
        bins = {"a": "agent_made", "b": "agent_made", "c": "agent_made",
                "l1": "legacy", "l2": "legacy", "l3": "legacy", "u": "unknown"}
        out, _ = self.c.add_proposals([_finding_event(s) for s, _, _ in rows], rule_list=self.rules,
                                      prompts=[], assign=self._assign(rows), bins=bins)
        self.assertEqual([(p["kind"], p["rule"], p["sources"]) for p in out],
                         [("add", t, ["a", "b", "c"]), ("ratchet", w, ["l1", "l2", "l3"])])
        self.assertEqual(out[0]["correct_example"], "x.py")
        self.assertIn("`x.py`", out[0]["summary"])
        self.assertNotIn("correct_example", out[1])

    def test_legacy_findings_never_make_an_add(self):
        t = self.rules[0]
        rows = [(s, t, "") for s in ("a", "b", "c")]
        out, _ = self.c.add_proposals([_finding_event(s) for s in "abc"], rule_list=self.rules, prompts=[],
                                      assign=self._assign(rows), bins={"a": "legacy", "b": "legacy", "c": "agent_made"})
        self.assertEqual(out, [])

    def test_ratchet_is_a_record_proposal_kind(self):
        self.assertIn("ratchet", self.c._record.PROPOSAL_KINDS)


def _sh(repo, *args):
    return subprocess.run(["git", *args], cwd=repo, capture_output=True, text=True, check=True).stdout.strip()


class BinFindingTest(unittest.TestCase):
    """A throwaway repo: `base`, then the reviewed diff committed on `feat` as `child`. No network."""
    LONG = [f"    result_{i} = compute_something_long(alpha_{i}, beta_{i}, gamma_{i}, delta_{i})" for i in range(6)]

    def setUp(self):
        self.c = _load_curate()
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


if __name__ == "__main__":
    unittest.main()
