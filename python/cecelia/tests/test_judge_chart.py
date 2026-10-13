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

    def test_a_low_session_or_partial_week_is_drawn_hollow_and_dashed_and_gets_no_rate(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-06", sessions=3) + _week(day="2026-10-13")
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 20))
        svg = self.c.render_svg(data)
        hollow = re.compile(r'<circle[^>]*fill="none"')
        self.assertEqual(len(hollow.findall(svg)), 2)  # its counts in panel a; the four rate lines skip it
        low = data["weeks"][1]
        self.assertIsNone(self.c._per_100(low, "fanout"))
        self.assertIsNone(self.c._fixed(low, "fanout"))
        same = self.c.aggregate(_week(day="2026-09-28"), [], as_of=dt.date(2026, 10, 20))["weeks"][0]
        self.assertEqual(self.c._fixed(same, "fanout"), 50)  # the same findings, enough sessions
        self.assertIn('stroke-dasharray="4 3"', svg)
        whole = self.c.render_svg(self.c.aggregate(_week(day="2026-09-28") + _week(day="2026-10-06"), [],
                                                   as_of=dt.date(2026, 10, 20)))
        self.assertFalse(hollow.findall(whole))
        self.assertNotIn("dasharray", whole)

    def test_judge_summary_is_the_newest_pass_counts_only(self):
        old = {"kind": "pass", "date": "2026-10-02", "bugs": [{"status": "open", "desc": "secret text"}]}
        new = {"kind": "pass", "date": "2026-10-09", "bugs": [{"status": "open", "desc": "/home/x/secret.py"},
                                                              {"status": "gone"}, {"status": "dismissed"}]}
        failure = {"kind": "failure", "date": "2026-10-10", "bugs": []}
        data = self.c.aggregate(_week(), [old, new, failure])
        self.assertEqual(data["judge"], {"date": "2026-10-09", "passes": 2, "swept": ["fanout"], "open": 1,
                                         "parked": 0, "gone": 1, "dismissed": 1,
                                         "by_week": {"fanout": {}, "convention": {}}})
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

    def test_judge_verdicts_land_in_the_week_the_item_was_raised(self):
        advisory = _ev("fanout_audit_advisory", "2026-09-29T01:00:00Z",
                       {"file": "a.py", "line": 3, "marker": "plausible", "desc": "x"})
        key = self.c._bugs._key(advisory)
        rows = _week(day="2026-09-28") + [advisory] + _week(day="2026-10-06")
        record = {"kind": "pass", "date": "2026-10-09", "bugs": [
            {"key": key, "status": "parked", "first_seen": "2026-10-09"},           # an advisory, raised Sep 29
            {"key": "fanout-1", "status": "open", "first_seen": "2026-10-09"},      # a finding, raised Sep 28
            {"key": "fanout-9", "status": "gone", "first_seen": "2026-10-09"},      # not in the log: first seen
            {"key": "repeat-x", "status": "dismissed", "first_seen": "2026-10-09"},  # an agent-run error: not drawn
            {"key": "fanout-8", "status": "unjudged", "first_seen": "2026-10-09"},
            {"key": "conv-2", "kind": "duplicate", "status": "open", "first_seen": "2026-10-09"}]}  # raised Sep 28
        judge = self.c.aggregate(rows, [record])["judge"]
        self.assertEqual(judge["by_week"], {"fanout": {
            "2026-09-28": {"open": 1, "parked": 1, "gone": 0, "dismissed": 0},
            "2026-10-05": {"open": 0, "parked": 0, "gone": 1, "dismissed": 0}}, "convention": {
            "2026-09-28": {"open": 1, "parked": 0, "gone": 0, "dismissed": 0}}})
        self.assertEqual(judge["dismissed"], 1)  # the totals still count every bug


class RenderTest(_Fixture):
    def test_a_week_still_running_is_in_the_json_not_the_picture(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-06")
        data = self.c.aggregate(rows, [], as_of=dt.date(2026, 10, 7))
        self.assertEqual([w["ongoing"] for w in data["weeks"]], [False, True])
        svg = self.c.render_svg(data)
        self.assertIn(">Sep 28<", svg)
        self.assertNotIn(">Oct 5<", svg)

    def test_a_reviewer_the_judge_hasnt_swept_says_when_it_will_be(self):
        record = {"kind": "pass", "date": "2026-10-09", "run": {}, "bugs": []}
        svg = self.c.render_svg(self.c.aggregate(_week(day="2026-09-28"), [record], as_of=dt.date(2026, 10, 12)))
        self.assertIn("Judge: fan-out audit (Oct 9)", svg)
        self.assertIn("Judged from the next pass on.", svg)
        swept = {**record, "run": {"swept": ["fanout", "convention"]}}
        svg = self.c.render_svg(self.c.aggregate(_week(day="2026-09-28"), [swept], as_of=dt.date(2026, 10, 12)))
        self.assertIn("Judge: convention check (Oct 9)", svg)

    def test_judge_panel_draws_each_verdict_as_a_share_of_the_week_with_its_n(self):
        rows = _week(day="2026-09-28") + _week(day="2026-10-06")
        record = {"kind": "pass", "date": "2026-10-20", "run": {"swept": ["fanout"]}, "bugs": [
            {"key": "fanout-x1", "status": "open", "first_seen": "2026-10-07"},
            {"key": "fanout-x2", "status": "parked", "first_seen": "2026-10-07"},
            {"key": "fanout-x3", "status": "gone", "first_seen": "2026-10-07"},
            {"key": "fanout-x4", "status": "dismissed", "first_seen": "2026-10-07"}]}
        data = self.c.aggregate(rows, [record], as_of=dt.date(2026, 10, 20))
        svg = self.c.render_svg(data)
        self.assertIn("Share of unfixed notes (%)", svg)
        self.assertIn(">n = 4<", svg)  # one week judged: real bug 50 %, gone 25 %, dismissed 25 %
        self.assertEqual(svg.count(">n = "), 1)  # no n under a week the judge has nothing from
        # one stacked bar (legend swatches are 10 by 10, bars a computed width): real bug (open + parked) 50 %, gone 25 %, dismissed 25 %, bottom-up to 100 %
        bars = [(float(y), float(h), fill) for y, h, fill in
                re.findall(r'<rect x="[\d.]+" y="([\d.]+)" width="\d+\.\d" height="([\d.]+)" fill="(#\w+)"/>', svg)]
        pal = self.c.load_hex()
        self.assertEqual([b[2] for b in bars], [pal[sw] for _, _, sw in self.c.JUDGE_LINES])
        self.assertEqual([round(4 * h / sum(b[1] for b in bars)) for _, h, _ in bars], [2, 1, 1])
        for (y, _, _), (above, h, _) in zip(bars, bars[1:]):   # each segment sits on the one below
            self.assertAlmostEqual(above + h, y, delta=0.15)
        thin = self.c.aggregate(_week(day="2026-09-28") + _week(day="2026-10-06", sessions=3), [record],
                                as_of=dt.date(2026, 10, 20))
        self.assertNotIn(">n = ", self.c.render_svg(thin))  # a low-session week gets no share either

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
