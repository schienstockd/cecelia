"""Tests for `scripts/judge/chart.py` — the README's effectiveness chart.

Design: docs/ai-assist/AGENT_RECORD.md. No test runs `git`: `Git` gets `FakeRemote`, which keeps the
objects and the one remote ref in memory, so a test can delete the branch or fail the push and check
what the next run does.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import datetime as dt
import hashlib
import importlib.util
import json
import os
import pathlib
import re
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load_chart():
    spec = importlib.util.spec_from_file_location("chart", _REPO / "scripts" / "judge" / "chart.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class FakeRemote:
    """git plumbing in memory: objects by a hash of their content, and `origin`'s `judge-assets` ref.
    `fail_push` makes the next push fail. Every call is kept in `calls`."""

    def __init__(self):
        self.objects: dict[str, str] = {}
        self.ref: str | None = None
        self.calls: list[list[str]] = []
        self.fail_push = False

    def _put(self, text: str) -> str:
        sha = hashlib.sha1(text.encode("utf-8")).hexdigest()
        self.objects[sha] = text
        return sha

    def __call__(self, args, input):
        self.calls.append(list(args))
        if args[0] == "hash-object":
            return self._put(input) + "\n"
        if args[0] == "mktree":
            return self._put("tree\n" + input) + "\n"
        if args[0] == "commit-tree":
            # a real commit carries a timestamp: two commits of one tree differ
            return self._put(f"commit {args[1]} {args[3]} {len(self.calls)}") + "\n"
        if args[0] == "ls-remote":
            return f"{self.ref}\t{args[2]}\n" if self.ref else ""
        if args[0] == "push":
            if self.fail_push:
                self.fail_push = False
                raise RuntimeError("remote: permission denied")
            self.ref = args[3].split(":")[0]
            return ""
        raise AssertionError(f"unexpected git call {args}")

    def pushes(self) -> list[list[str]]:
        return [c for c in self.calls if c[0] == "push"]


def _ev(event, ts, payload=None, *, session="s1", branch="b", commit="c" * 40):
    return {"event": event, "ts": ts, "payload": payload or {}, "session": session, "branch": branch,
            "commit": commit, "schema_version": 1, "source": "live", "pr": None}


def _week(*, day="2026-10-06", sessions=12):
    """A week of reviews: `sessions` sessions, one change each, three findings with outcomes."""
    rows = []
    for i in range(sessions):
        rows += [_ev("fanout_audit_run", f"{day}T0{i % 10}:00:00Z", {"duration_s": 10}, session=f"s{i}", commit=f"{i:040d}"),
                 _ev("convention_check_run", f"{day}T0{i % 10}:00:01Z", {"duration_s": 10}, session=f"s{i}",
                     commit=f"{i:040d}")]
    rows += [_ev("fanout_audit_finding", f"{day}T01:00:00Z", {"slug": "fanout-1", "marker": "confirmed"}),
             _ev("fanout_audit_finding", f"{day}T01:30:00Z", {"slug": "fanout-1", "marker": "confirmed"}),   # a rerun
             _ev("convention_check_finding", f"{day}T02:00:00Z", {"slug": "conv-2", "marker": "should reuse"}),
             _ev("fanout_audit_finding", f"{day}T03:00:00Z", {"slug": "fanout-3", "marker": "confirmed"}),
             _ev("fanout_audit_finding_resolved", f"{day}T01:40:00Z", {"slug": "fanout-1", "outcome": "fixed_pre_commit"}),
             _ev("convention_check_finding_resolved", f"{day}T02:10:00Z", {"slug": "conv-2", "outcome": "false_positive"})]
    return rows


class _Fixture(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "events.jsonl")})
        env.start()
        self.addCleanup(env.stop)
        self.c = _load_chart()
        self.remote = FakeRemote()
        # never the real git: a test that forgot its fake would push to the real repo
        guard = mock.patch.object(self.c.Git, "_default_run",
                                  lambda _self, args, input: self.fail(f"reached the real git: {args}"))
        guard.start()
        self.addCleanup(guard.stop)

    def git(self, **kw):
        return self.c.Git(self.tmp, run=self.remote, **kw)


class AggregateTest(_Fixture):
    def test_empty_history_renders_a_chart_that_says_so(self):
        data = self.c.aggregate([], [])
        self.assertEqual(data["weeks"], [])
        svg = self.c.render_svg(data)
        self.assertIn("No reviews logged yet.", svg)
        self.assertTrue(svg.startswith("<svg") and svg.rstrip().endswith("</svg>"))

    def test_one_week_counts_changes_sessions_and_outcomes_once_per_slug(self):
        data = self.c.aggregate(_week(), [])
        [w] = data["weeks"]
        self.assertEqual(w["week"], "2026-10-05")
        self.assertEqual((w["changes"], w["sessions"]), (12, 12))
        self.assertEqual(w["findings"], {"fixed_pre_commit": 1, "shipped_with_finding": 0, "false_positive": 1,
                                         "dropped_no_action": 0, "none": 1})
        self.assertTrue(w["partial"])   # nothing logged after it: still filling up

    def test_an_errored_reviewer_and_a_redteam_slug_count_for_nothing(self):
        rows = [_ev("fanout_audit_run", "2026-10-06T00:00:00Z", {"duration_s": 180, "error": "timeout"}),
                _ev("fanout_audit_finding", "2026-10-06T00:00:00Z", {"slug": "fanout-c8482224"})]
        [w] = self.c.aggregate(rows, [])["weeks"]
        self.assertEqual((w["changes"], sum(w["findings"].values())), (0, 0))

    def test_an_unknown_outcome_is_bucketed_not_echoed(self):
        rows = _week() + [_ev("fanout_audit_finding_resolved", "2026-10-06T05:00:00Z",
                              {"slug": "fanout-3", "outcome": "<script>evil</script>"})]
        data = self.c.aggregate(rows, [])
        self.assertEqual(data["weeks"][0]["findings"]["none"], 1)
        self.assertNotIn("evil", self.c.render_svg(data))

    def test_weeks_between_have_zeros_and_only_the_last_is_partial(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-13")
        weeks = self.c.aggregate(rows, [])["weeks"]
        self.assertEqual([w["week"] for w in weeks], ["2026-09-28", "2026-10-05", "2026-10-12"])
        self.assertEqual(weeks[1]["changes"], 0)
        self.assertEqual([w["partial"] for w in weeks], [False, False, True])

    def test_a_rubric_change_is_noted_on_its_week_and_not_before_logging(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-06")
        edits = [dt.date(2026, 9, 1), dt.date(2026, 9, 29), dt.date(2026, 10, 7), dt.date(2026, 10, 8)]
        data = self.c.aggregate(rows, [], prompt_edits=edits, rule_edits=[dt.date(2026, 10, 7)])
        self.assertEqual([w["prompt_edits"] for w in data["weeks"]], [1, 2])
        self.assertEqual([w["rule_edits"] for w in data["weeks"]], [0, 1])

    def test_a_week_that_ended_before_the_run_is_whole(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-11")
        self.assertEqual([w["partial"] for w in self.c.aggregate(rows, [])["weeks"]], [False, True])
        whole = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 12))["weeks"]
        self.assertEqual([w["partial"] for w in whole], [False, False])

    def test_a_low_session_week_is_drawn_hollow_and_dashed_with_a_star_in_a_b_and_e(self):
        # the low week's findings under their own slugs, so they count in its week
        low_rows = [{**e, "payload": {**e["payload"], "slug": e["payload"]["slug"] + "-low"}} if "slug" in e["payload"]
                    else e for e in _week(day="2026-10-06", sessions=3)]
        rows = _week(day="2026-09-28") + low_rows + _week(day="2026-10-13")
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 20))
        low = data["weeks"][1]
        self.assertEqual(self.c._fixed(low, "fanout"), 50)   # a rate still, drawn hollow
        with mock.patch.object(self.c, "MIN_LINE_WEEKS", 1):   # lines, to see the dashes
            svg = self.c.render_svg(data)
        hollow = re.compile(r'<circle[^>]*fill="#fafaf7"')
        # panel a: changes + PRs; b: two reviewers; e: two reviewers
        self.assertEqual(len(hollow.findall(svg)), 6)
        self.assertEqual(svg.count(">*</text>"), 6)   # one beside each hollow point
        self.assertIn('stroke-dasharray="4 3"', svg)
        with mock.patch.object(self.c, "MIN_LINE_WEEKS", 1):
            whole = self.c.render_svg(self.c.aggregate(_week(day="2026-09-28") + _week(day="2026-10-06"), [],
                                                       as_of=dt.date(2026, 10, 20)))
        self.assertFalse(hollow.findall(whole))
        self.assertNotIn("dasharray", whole)
        self.assertNotIn(">*</text>", whole)

    def test_a_week_with_no_findings_has_no_fixed_share(self):
        rows = [_ev("fanout_audit_run", "2026-10-06T00:00:00Z", {"duration_s": 10})]
        [w] = self.c.aggregate(rows, [])["weeks"]
        self.assertIsNone(self.c._fixed(w, "fanout"))

    def test_review_wait_is_the_median_of_clean_runs_per_reviewer(self):
        rows = [_ev("fanout_audit_run", f"2026-10-06T0{i}:00:00Z", {"duration_s": d}, commit=f"{i:040d}")
                for i, d in enumerate((10, 30, 50, 20))]
        rows += [_ev("fanout_audit_run", "2026-10-06T09:00:00Z", {"duration_s": 180, "error": "timeout"}),
                 _ev("convention_check_run", "2026-10-06T09:00:00Z", {"duration_s": 12.25})]
        [w] = self.c.aggregate(rows, [])["weeks"]
        self.assertEqual(w["wait_s"], {"fanout": 25.0, "convention": 12.2})
        [none] = self.c.aggregate([_ev("fanout_audit_run", "2026-10-06T00:00:00Z")], [])["weeks"]
        self.assertEqual(none["wait_s"], {"fanout": None, "convention": None})

    def test_reviewer_tokens_are_summed_per_week_from_when_logging_began(self):
        tok = {"input": 1, "cache_write": 10, "cache_read": 100, "output": 1000}
        rows = _week(day="2026-09-28") + _week(day="2026-10-06") + [
            _ev("fanout_audit_run", "2026-10-07T00:00:00Z", {"duration_s": 5, "tokens": tok}),
            _ev("fanout_audit_run", "2026-10-07T01:00:00Z", {"duration_s": 5, "error": "x", "tokens": tok}),
            _ev("convention_check_run", "2026-10-08T00:00:00Z", {"duration_s": 5, "tokens": tok})]
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 20))
        self.assertEqual(data["tokens_from"], "2026-10-07")
        self.assertEqual([w["reviewer_tokens"] for w in data["weeks"]],
                         [None, {"fanout": 2222, "convention": 1111}])

    def test_under_min_line_weeks_the_points_are_dots_with_their_values_not_joined(self):
        def weeks(n):
            days = ("2026-09-28", "2026-10-05", "2026-10-12", "2026-10-19")[:n]
            rows = [r for d in days for r in _week(day=d)]
            return self.c.render_svg(self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 27)))
        joins = re.compile(r'<line x1="[\d.]+" y1="[\d.]+" x2="[\d.]+" y2="[\d.]+" stroke="#\w+" stroke-width="1.75"')
        few = weeks(self.c.MIN_LINE_WEEKS - 1)
        self.assertFalse(joins.findall(few))
        self.assertIn(">12</text>", few)   # 12 changes reviewed, printed beside its dot
        self.assertTrue(joins.findall(weeks(self.c.MIN_LINE_WEEKS)))

    def test_passes_are_every_pass_record_oldest_first_and_carry_no_text(self):
        old = {"kind": "pass", "date": "2026-10-02",
               "bugs": [{"key": "fanout-1", "status": "open", "desc": "secret text", "verify": {"verdict": "fix"}}]}
        new = {"kind": "pass", "date": "2026-10-09", "bugs": [
            {"key": "fanout-1", "status": "open", "desc": "/home/x/secret.py", "verify": {"verdict": "fix"}}]}
        failure = {"kind": "failure", "date": "2026-10-10", "bugs": []}
        data = self.c.aggregate(_week(), [new, failure, old])
        self.assertEqual([p["date"] for p in data["passes"]], ["2026-10-02", "2026-10-09"])
        svg = self.c.render_svg(data) + json.dumps(data)
        self.assertNotIn("secret", svg)

    def test_merged_prs_are_merge_commits_and_squash_subjects(self):
        log = ("2026-10-07 Merge pull request #12 from x/y\n"
               "2026-10-07 fix(thing): a squash merge (#13)\n"
               "2026-10-06 a direct commit\n"
               "2026-10-06 mentions (#14) mid-subject\n"
               "2026-09-01 Merge pull request #1 from old/branch\n")
        prs = self.c.pr_dates(log)
        self.assertEqual(prs, [dt.date(2026, 9, 1), dt.date(2026, 10, 7), dt.date(2026, 10, 7)])
        weeks = self.c.aggregate(_week(), [], prs=prs)["weeks"]
        self.assertEqual([w["prs"] for w in weeks], [2])  # the September one predates logging

    def test_findings_are_split_by_reviewer(self):
        w = self.c.aggregate(_week(), [])["weeks"][0]
        self.assertEqual(sum(w["by_reviewer"]["fanout"].values()), 2)
        self.assertEqual(w["by_reviewer"]["fanout"]["fixed_pre_commit"], 1)
        self.assertEqual(w["by_reviewer"]["convention"]["false_positive"], 1)
        self.assertEqual(w["findings"]["fixed_pre_commit"] + w["findings"]["false_positive"] + w["findings"]["none"], 3)

class JudgePassTest(_Fixture):
    @staticmethod
    def _pass(date, *bugs, tokens=None):
        return {"kind": "pass", "date": date, "bugs": list(bugs),
                "run": {"spend": {"tokens": tokens} if tokens is not None else {}}}

    def test_open_real_bugs_are_bucketed_by_age_since_first_flagged(self):
        fix = {"verdict": "fix"}
        a = lambda status, **kw: {"key": "fanout-a", "status": status, **kw}   # noqa: E731
        passes = self.c.judge_passes([
            self._pass("2026-10-01", a("open"), {"key": "fanout-g", "status": "open", "verify": {"verdict": "guard"}}),
            self._pass("2026-10-02", a("open", verify=fix),                    # flagged here, not on Oct 1
                       {"key": "fanout-d", "status": "open", "verify": {"verdict": "decide"}},
                       {"key": "strand-s", "kind": "stranded", "status": "open"}),
            self._pass("2026-10-08", a("open", verify=fix)),                    # 6 days
            self._pass("2026-10-09", a("open", verify=fix),                     # 7 days
                       {"key": "fanout-d", "status": "parked", "verify": {"verdict": "decide"}}),  # not open
            self._pass("2026-10-15", a("open", verify=fix)),                    # 13 days
            self._pass("2026-10-16", a("open", verify=fix)),                    # 14 days
        ])
        self.assertEqual([p["open"] for p in passes], [
            {"under_1w": 0, "1_2w": 0, "over_2w": 0},   # unverified and guard bugs aren't real
            {"under_1w": 3, "1_2w": 0, "over_2w": 0},
            {"under_1w": 1, "1_2w": 0, "over_2w": 0},
            {"under_1w": 0, "1_2w": 1, "over_2w": 0},
            {"under_1w": 0, "1_2w": 1, "over_2w": 0},
            {"under_1w": 0, "1_2w": 0, "over_2w": 1}])

    def test_fixed_counts_a_real_bug_the_first_pass_it_is_gone_or_dismissed_as_landed(self):
        fix = {"verdict": "fix"}
        landed = {"status": "dismissed", "verify": {"verdict": "dismiss"}, "fix_landed": [{"commit": "f" * 40}]}
        passes = self.c.judge_passes([
            self._pass("2026-10-01", {"key": "k1", "status": "open", "verify": fix},
                       {"key": "k2", "status": "open", "verify": fix},
                       {"key": "k3", "status": "open", "verify": fix},
                       {"key": "k4", "status": "gone"}),                                  # never real
            self._pass("2026-10-08", {"key": "k1", "status": "gone", "verify": fix},
                       {"key": "k2", **landed},                                           # verify found the fix
                       {"key": "k3", "status": "dismissed", "verify": {"verdict": "dismiss"}},  # flagged, then not a bug
                       {"key": "k4", "status": "gone"}),
            self._pass("2026-10-15", {"key": "k1", "status": "gone", "verify": fix},       # already counted
                       {"key": "k5", **{**landed, "status": "gone"}}),                    # found fixed on first look
        ])
        self.assertEqual([p["fixed"] for p in passes], [0, 2, 1])

    def test_a_pass_with_no_tokens_gets_a_star_at_the_baseline(self):
        tok = {"input": 0, "cache_write": 500_000, "cache_read": 1_000_000, "output": 0}
        records = [self._pass("2026-10-02"),
                   self._pass("2026-10-04", tokens={"verify": tok, "sweep": tok, "rules": {}, "total": {}}),
                   self._pass("2026-10-05", tokens={"sweep": {}, "verify": {}, "rules": {}, "total": {"input": 0}})]
        data = self.c.aggregate(_week(day="2026-09-28") + _week(day="2026-10-06"), records, as_of=dt.date(2026, 10, 20))
        self.assertEqual([p["tokens"] for p in data["passes"]],
                         [None, {"verify": 1_500_000, "sweep": 1_500_000, "rules": 0}, None])
        svg = self.c.render_svg(data)
        self.assertIn(">3.0M</text>", svg)
        stars = re.findall(r'<text x="[\d.]+" y="([\d.]+)" font-size="15" font-weight="700" fill="#6b6b66">\*</text>', svg)
        self.assertEqual(len(stars), 2)
        self.assertEqual(len(set(stars)), 1)   # both on panel d's baseline
        self.assertIn("Judge pass of Oct 2: 0 real bugs open", svg)
        self.assertIn("no token data.", svg)

    def test_open_bars_stack_young_to_old_with_a_gap_and_fixed_on_top(self):
        fix = {"verdict": "fix"}
        records = [self._pass("2026-10-01", {"key": "k1", "status": "open", "verify": fix},
                              {"key": "k2", "status": "open", "verify": fix}),
                   self._pass("2026-10-16", {"key": "k1", "status": "open", "verify": fix},
                              {"key": "k2", "status": "gone", "verify": fix},
                              {"key": "k3", "status": "open", "verify": fix})]
        data = self.c.aggregate(_week(day="2026-09-28") + _week(day="2026-10-06"), records, as_of=dt.date(2026, 10, 20))
        svg = self.c.render_svg(data)
        fills = {fill: name for _, name, _, fill in self.c.AGES}
        bars = [(float(y), float(h), fills[f]) for y, h, f in
                re.findall(r'<rect x="[\d.]+" y="([\d.]+)" width="[\d.]+" height="([\d.]+)" fill="(#\w+)"/>', svg)
                if f in fills and h != "10"]
        # Oct 1: two under a week; Oct 16: k3 under a week, k1 over two weeks on top of it
        self.assertEqual([b[2] for b in bars], ["under 1 week", "under 1 week", "over 2 weeks"])
        (y0, _, _), (y1, h1, _) = bars[1], bars[2]
        self.assertAlmostEqual(y1 + h1 + self.c._GAP, y0, delta=0.15)
        self.assertIn(">open for</text>", svg)
        legend = re.findall(r'font-size="11">(over 2 weeks|1–2 weeks|under 1 week)</text>', svg)
        self.assertEqual(legend, ["over 2 weeks", "1–2 weeks", "under 1 week"])   # the stack, top to bottom
        blue = self.c.load_hex()["blue"]   # the darker blue: sky blue text is too faint on the light surface
        self.assertRegex(svg, rf'fill="{blue}">1 fixed</text>|fill="{blue}">1</text>')


class RenderTest(_Fixture):
    def test_a_week_still_running_is_drawn_hollow_with_a_star(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-06")
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 7))
        self.assertEqual([w["ongoing"] for w in data["weeks"]], [False, True])
        svg = self.c.render_svg(data)
        self.assertIn(">Sep 28<", svg)
        self.assertIn(">Oct 5<", svg)
        self.assertIn(">*</text>", svg)
        self.assertIn("(still running)", svg)

    def test_slot_f_is_a_note_until_a_whole_week_of_reviewer_tokens_is_logged(self):
        tok = {"input": 0, "cache_write": 0, "cache_read": 300_000, "output": 0}
        logged = [_ev("fanout_audit_run", "2026-10-13T00:00:00Z", {"duration_s": 5, "tokens": tok})]
        base = _week(day="2026-09-28") + _week(day="2026-10-06")
        ongoing = self.c.render_svg(self.c.aggregate(base + logged, [], as_of=dt.date(2026, 10, 14)))
        self.assertIn("Reviewer tokens: logged since Oct 13 (#1807)", ongoing)
        self.assertNotIn("Reviewer tokens per week", ongoing)
        self.assertIn("Reviewer tokens: not logged yet", self.c.render_svg(self.c.aggregate(base, [])))
        whole = self.c.render_svg(self.c.aggregate(base + logged + _week(day="2026-10-20"), [],
                                                   as_of=dt.date(2026, 10, 27)))
        self.assertIn("Reviewer tokens per week", whole)
        self.assertIn(">0.3M</text>", whole)

    def test_thin_weeks_before_logging_got_going_are_in_the_json_not_the_picture(self):
        rows = _week(day="2026-09-21", sessions=3) + _week(day="2026-09-28") + _week(day="2026-10-06")
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 20))
        self.assertEqual(data["weeks"][0]["week"], "2026-09-21")
        svg = self.c.render_svg(data)
        self.assertNotIn(">Sep 21<", svg)
        self.assertIn(">Sep 28<", svg)
        only = self.c.render_svg(self.c.aggregate(_week(day="2026-09-21", sessions=3), [],
                                                  as_of=dt.date(2026, 10, 20)))
        self.assertIn(">Sep 21<", only)  # all there is: still drawn

    def test_same_numbers_same_bytes(self):
        data = self.c.aggregate(_week(), [])
        self.assertEqual(self.c.render_svg(data), self.c.render_svg(json.loads(json.dumps(data))))

    def test_axis_ticks_are_round_and_cover_the_data(self):
        self.assertEqual(self.c._ticks(234), [0, 100, 200, 300])
        self.assertEqual(self.c._ticks(89), [0, 25, 50, 75, 100])
        self.assertEqual(self.c._ticks(33), [0, 10, 20, 30, 40])
        self.assertEqual(self.c._ticks(0), [0, 1])
        self.assertEqual(self.c._ticks(3), [0, 1, 2, 3])
        self.assertEqual(self.c._ticks(3, percent=True), [0, 25, 50, 75, 100])

    def test_text_is_escaped_for_xml(self):
        self.assertEqual(self.c.esc('a<b & "c"\x07'), "a&lt;b &amp; &quot;c&quot;")

    def test_the_numbers_are_in_the_description(self):
        svg = self.c.render_svg(self.c.aggregate(_week(), []))
        self.assertIn('role="img"', svg)
        self.assertIn("12 changes reviewed across 12 sessions, 0 PRs merged; 3 findings, 1 fixed before commit", svg)

    def test_the_svg_parses(self):
        import xml.etree.ElementTree as ET
        rows = _week(day="2026-09-28", sessions=3) + _week(day="2026-10-06")
        ET.fromstring(self.c.render_svg(self.c.aggregate(rows, [{"kind": "pass", "date": "2026-10-09", "bugs": []}])))


class PublishTest(_Fixture):
    def setUp(self):
        super().setUp()
        self.data = self.c.aggregate(_week(), [])
        self.svg = self.c.render_svg(self.data)
        self.state = self.tmp / "chart" / "pushed.json"

    def publish(self, svg=None, **kw):
        return self.c.publish(svg or self.svg, self.data, git=self.git(**kw), state_path=self.state)

    def test_first_run_pushes_one_parentless_commit_to_judge_assets(self):
        result = self.publish()
        self.assertIn("pushed", result)
        [push] = self.remote.pushes()
        self.assertEqual(push, ["push", "--force", "origin", f"{result['pushed']}:refs/heads/judge-assets"])
        [commit] = [c for c in self.remote.calls if c[0] == "commit-tree"]
        self.assertNotIn("-p", commit)
        tree = self.remote.objects[commit[1]]
        self.assertEqual([line.split("\t")[1] for line in tree.splitlines()[1:]],
                         ["README.md", "effectiveness.json", "effectiveness.svg"])

    def test_the_flow_chart_is_published_beside_the_weekly_one_and_a_flow_change_pushes(self):
        flow = self.c._flow.render_svg(self.c._flow.aggregate([], []))
        result = self.c.publish(self.svg, self.data, git=self.git(), state_path=self.state, flow_svg=flow)
        [commit] = [c for c in self.remote.calls if c[0] == "commit-tree"]
        tree = self.remote.objects[commit[1]]
        self.assertIn("findings-flow.svg", [line.split("\t")[1] for line in tree.splitlines()[1:]])
        self.assertIn("pushed", result)
        changed = flow.replace("Recital", "Recital ")
        self.assertIn("pushed", self.c.publish(self.svg, self.data, git=self.git(), state_path=self.state,
                                               flow_svg=changed))

    def test_an_empty_history_is_published_too(self):
        empty = self.c.aggregate([], [])
        result = self.c.publish(self.c.render_svg(empty), empty, git=self.git(), state_path=self.state)
        self.assertIn("pushed", result)

    def test_a_byte_identical_chart_is_not_pushed_again(self):
        self.publish()
        self.assertEqual(self.publish(), {"skipped": "unchanged"})
        self.assertEqual(len(self.remote.pushes()), 1)

    def test_a_changed_chart_is_pushed(self):
        self.publish()
        self.assertIn("pushed", self.publish(svg=self.svg.replace("per week", "per week ")))
        self.assertEqual(len(self.remote.pushes()), 2)

    def test_a_deleted_branch_is_pushed_again(self):
        self.publish()
        self.remote.ref = None
        self.assertIn("pushed", self.publish())

    def test_a_failed_push_raises_and_records_nothing_so_the_next_run_retries(self):
        self.remote.fail_push = True
        with self.assertRaises(RuntimeError):
            self.publish()
        self.assertFalse(self.state.exists())
        self.assertIn("pushed", self.publish())

    def test_dry_run_plans_the_push_without_making_it(self):
        result = self.publish(dry=True)
        self.assertIn("planned", result)
        self.assertEqual(self.remote.pushes(), [])
        self.assertFalse(self.state.exists())


class AllowlistTest(_Fixture):
    def test_only_judge_assets_can_be_pushed(self):
        sha = "a" * 40
        self.assertTrue(self.c.allowed(["push", "--force", "origin", f"{sha}:refs/heads/judge-assets"]))
        for args in (["push", "--force", "origin", f"{sha}:refs/heads/main"],
                     ["push", "--force", "origin", f"{sha}:refs/heads/judge-assets", "--tags"],
                     ["push", "--force", "origin", "HEAD:refs/heads/judge-assets"],
                     ["push", "--force", "origin", f"{sha}:main"],
                     ["push", "origin", "main"],
                     ["commit-tree", sha, "-p", "HEAD"],
                     ["commit-tree", sha, "-m", "anything else"],
                     ["checkout", "main"], ["reset", "--hard"], ["ls-remote", "origin", "refs/heads/main"]):
            with self.subTest(args=args):
                self.assertFalse(self.c.allowed(args))
                with self.assertRaises(self.c.GitRefused):
                    self.git()(*args)


class UpdateTest(_Fixture):
    def setUp(self):
        super().setUp()
        (self.tmp / "events.jsonl").write_text("".join(json.dumps(e) + "\n" for e in _week()), encoding="utf-8")

    def test_update_writes_the_files_and_pushes(self):
        result = self.c.update(push=True, git=self.git(), out_dir=self.tmp / "out")
        self.assertIn("pushed", result)
        self.assertTrue((self.tmp / "out" / "effectiveness.svg").is_file())
        self.assertEqual(json.loads((self.tmp / "out" / "effectiveness.json").read_text(encoding="utf-8"))["weeks"][0]["changes"], 12)

    def test_a_push_failure_comes_back_as_an_error_not_a_raise(self):
        self.remote.fail_push = True
        result = self.c.update(push=True, git=self.git(), out_dir=self.tmp / "out")
        self.assertIn("permission denied", result["error"])

    def test_without_push_no_git_runs(self):
        self.c.update(push=False, git=self.git(), out_dir=self.tmp / "out")
        self.assertEqual(self.remote.calls, [])


if __name__ == "__main__":
    unittest.main()
