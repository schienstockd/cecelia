"""`judge-gaps` (`scripts/judge/gaps_fix.py`): which coherence gaps it takes, what the agent is handed,
and what it records. Built on the `--auto` fixture: the sessions and every git / gh / pixi call are
faked, nothing reaches GitHub."""
from __future__ import annotations

import io
import json
import shutil

from cecelia.tests.test_judge_auto_fix import _AutoFixture, _load, _REPO

UPWARD = {"kind": "julia-upward", "rank": -3, "subject": "`active_profile_name`",
          "detail": "defined in ai+lab log (tier 8), called from config (tier 1)", "where": []}
UPWARD_EASY = {"kind": "julia-upward", "rank": -1, "subject": "`_last_line`", "detail": "d", "where": []}
WS = {"kind": "ws-unhandled", "subject": "`update:staged`", "where": ["api/src/update_api.jl"]}
ROUTE = {"kind": "route-uncalled", "subject": "`/api/projects/list`", "where": ["api/src/server.jl"]}


class _GapsFixture(_AutoFixture):
    def setUp(self):
        super().setUp()
        self.gf = _load("gaps_fix")
        self.gf._af = self.af   # one module: the fixture's patches (`_sleep`, `remove_worktree`) reach it
        self.tried = self.tmp / "gaps_fix.jsonl"
        arch = self.main / "scripts" / "architecture"
        arch.mkdir(parents=True)
        shutil.copy(_REPO / "scripts" / "architecture" / "gaps.py", arch / "gaps.py")
        self.write_gaps(julia=[UPWARD_EASY, UPWARD], boundaries=[ROUTE, WS])

    def write_gaps(self, **by_source):
        d = self.main / "docs" / "architecture" / "gaps"
        d.mkdir(parents=True, exist_ok=True)
        for source, items in by_source.items():
            (d / f"{source}.json").write_text(json.dumps(items), encoding="utf-8")

    def answer(self, gap, outcome="fixed"):
        return {"bugs": [{"key": self.gf.key_of(gap), "outcome": outcome, "reason": "moved it down to config"}],
                "title": "refactor(config): move active_profile_name below its callers",
                "summary": "config called up into ai+lab log; moved the function to config.", "visual": False}

    def run_gaps(self, *answers, dry_run=False, limit=None):
        out = io.StringIO()
        ran = self.gf.run(main=self.main, out=out, session=self.session(*answers), sh=self.sh(),
                          log_path=self.tried, dry_run=dry_run, limit=limit)
        return ran, out.getvalue()


class PickTest(_GapsFixture):
    def test_broken_ends_first_then_the_easiest_and_never_a_route(self):
        picked = [g["subject"] for g in self.gf.open_gaps(self.main)]
        self.assertEqual(picked, ["`update:staged`", "`active_profile_name`", "`_last_line`"])

    def test_the_brief_names_the_gap_how_to_close_it_and_the_baseline(self):
        _, out = self.run_gaps(dry_run=True, limit=1)
        self.assertIn("`update:staged`", out)
        self.assertIn("handle it where it is meant to land, or stop sending it", out)
        self.assertIn("pixi run arch-gaps-baseline", out)
        self.assertIn("never raise the baseline", out)
        self.assertNotIn("bootstrap-worktree", [c[2] for c in self.calls if len(c) > 2])   # dry run: nothing made

    def test_a_checkout_without_the_gap_list_runs_nothing(self):
        (self.main / "scripts" / "architecture" / "gaps.py").unlink()
        ran, out = self.run_gaps()
        self.assertEqual(ran, 0)
        self.assertIn("no docs/architecture gap list", out)

    def test_a_key_is_the_same_for_the_same_gap_and_differs_between_gaps(self):
        self.assertEqual(self.gf.key_of(dict(UPWARD)), self.gf.key_of(UPWARD))
        self.assertNotEqual(self.gf.key_of(UPWARD), self.gf.key_of(UPWARD_EASY))


class WorkTest(_GapsFixture):
    def test_a_fix_is_a_pr_off_main_and_is_recorded(self):
        ran, _ = self.run_gaps(self.answer(WS), {"tags": []}, limit=1)
        self.assertEqual(ran, 1)
        boot = next(c for c in self.calls if c[:3] == ["pixi", "run", "bootstrap-worktree"])
        key = self.gf.key_of(WS)
        self.assertEqual(boot[3:], [key, key, "origin/main"])
        self.assertIn("`ws-unhandled` gap `update:staged`", self.messages[0])
        self.assertIn("gaps_baseline.json", self.bodies[0])
        self.assertFalse(any(c[:3] == ["gh", "pr", "merge"] for c in self.calls))
        row = json.loads(self.tried.read_text(encoding="utf-8").splitlines()[-1])
        self.assertEqual((row["key"], row["outcome"]), (key, "fixed"))

    def test_a_gap_is_tried_once(self):
        self.run_gaps(self.answer(WS, "left"), limit=1)
        ran, _ = self.run_gaps(self.answer(UPWARD, "left"), limit=1)   # the next one, not WS again
        self.assertEqual(ran, 1)
        rows = [json.loads(r) for r in self.tried.read_text(encoding="utf-8").splitlines()]
        self.assertEqual([r["subject"] for r in rows], ["`update:staged`", "`active_profile_name`"])

    def test_a_weekly_pass_takes_at_most_weekly_sessions(self):
        self.write_gaps(julia=[dict(UPWARD, subject=f"`f{i}`") for i in range(self.gf.WEEKLY + 2)], boundaries=[])
        answers = [a for i in range(self.gf.WEEKLY)
                   for a in (self.answer(dict(UPWARD, subject=f"`f{i}`"), "left"),)]
        ran, _ = self.run_gaps(*answers)
        self.assertEqual(ran, self.gf.WEEKLY)
