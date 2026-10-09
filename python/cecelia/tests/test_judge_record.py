"""Tests for `scripts/judge/record.py` — the weekly judge's run records.

Design: docs/ai-assist/WEEKLY_JUDGE.md. The store follows `CECELIA_EFFECTIVENESS_LOG` into a temp
dir, so nothing touches `~/.cecelia-effectiveness`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import json
import os
import pathlib
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
_RECORD_PATH = _REPO / "scripts" / "judge" / "record.py"


def _load_record():
    spec = importlib.util.spec_from_file_location("record", _RECORD_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _bug(id_, status="open", **over):
    return {"id": id_, "key": f"fanout-{id_.lower()}", "status": status, "file": "a.py", "line": 3,
            "desc": f"desc {id_}", "why": "still there", "first_seen": "2026-10-05", "marker": "confirmed",
            "branch": "feat/x"} | over


class _Fixture(unittest.TestCase):
    """A temp effectiveness dir: the record store sits beside its `events.jsonl`."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "events.jsonl")})
        env.start()
        self.addCleanup(env.stop)
        self.rec = _load_record()

    def build(self, date="2026-10-05", bugs=(), proposals=(), rules=()):
        return self.rec.build(date, ts=f"{date}T13:59:00Z", sha="a" * 40, bugs=list(bugs), rules=list(rules),
                              proposals=list(proposals),
                              spend={"sweep_usd": 0.4, "verify_usd": 2.0, "rules_usd": 0.5, "total_usd": 2.9})


class RecordTest(_Fixture):
    def test_a_built_record_validates_and_round_trips(self):
        record = self.build(bugs=[_bug("B1")], proposals=[
            {"id": "P1", "kind": "tighten", "rule": "CLAUDE.md → *Testing*", "summary": "s", "sources": ["a"]}])
        self.assertEqual(self.rec.validate(record), [])
        path = self.rec.write(record)[0]
        self.assertEqual(path, self.tmp / "judge-runs" / "2026-10-05.json")
        self.assertEqual(self.rec.load(path), record)

    def test_only_a_bug_verified_decide_this_pass_is_queued(self):
        decide = {"verdict": "decide", "date": "2026-10-05", "question": "q?"}
        record = self.build(bugs=[_bug("B1", verify=decide), _bug("B2", verify={"verdict": "fix", "date": "2026-10-05"}),
                                  _bug("B3", verify=decide | {"date": "2026-09-28"})])
        self.assertEqual(record["queue"], [{"kind": "bug", "ref": "B1"}])

    def test_validation_names_the_problem(self):
        record = self.build(bugs=[_bug("B1", status="bogus")], proposals=[
            {"id": "P1", "kind": "add", "rule": "r", "summary": "s", "sources": []}])
        record["queue"] = [{"kind": "bug", "ref": "B9"}]
        errs = self.rec.validate(record)
        self.assertTrue(any("status 'bogus'" in e for e in errs))
        self.assertTrue(any("kind 'add'" in e for e in errs))
        self.assertTrue(any("names no bug" in e for e in errs))
        self.assertIn("schema_version", self.rec.validate({"schema_version": 99})[0])

    def test_a_stranded_bug_needs_no_file_line(self):
        record = self.build(bugs=[{"id": "B1", "key": "stranded-pr5", "kind": "stranded", "status": "open",
                                   "why": "w", "pr": 5, "branch": "b", "commits": ["abc"]}])
        self.assertEqual(self.rec.validate(record), [])
        self.assertIn("`abc`", self.rec.render_markdown(record))

    def test_an_existing_record_needs_force(self):
        self.rec.write(self.build())
        with self.assertRaises(self.rec.RecordError):
            self.rec.write(self.build())
        self.rec.write(self.build(), force=True)

    def test_pass_records_skip_failures_and_junk_and_respect_before(self):
        self.rec.write(self.build("2026-09-28"))
        self.rec.write(self.rec.failure_record("2026-10-05", stage="bugs", error="boom"))
        (self.tmp / "judge-runs" / "2026-10-01.json").write_text("{not json", encoding="utf-8")
        self.rec.write(self.build("2026-10-12"))
        self.assertEqual([r["date"] for r in self.rec.pass_records()], ["2026-09-28", "2026-10-12"])
        self.assertEqual([r["date"] for r in self.rec.pass_records(before="2026-10-12")], ["2026-09-28"])

    def test_the_mirror_gets_json_and_markdown(self):
        mirror = self.tmp / "mirror"
        written = self.rec.write(self.build(bugs=[_bug("B1")]), mirror=True, mirror_dir=mirror)
        self.assertEqual([p.name for p in written[1:]], ["2026-10-05.json", "2026-10-05.md"])
        self.assertEqual(json.loads((mirror / "2026-10-05.json").read_text(encoding="utf-8"))["date"], "2026-10-05")


class RenderTest(_Fixture):
    def test_bugs_rules_and_spend_render(self):
        record = self.build(
            bugs=[_bug("B1"), _bug("B2", status="unjudged", why="waiting for the judge")],
            rules=[{"rule": "CLAUDE.md → *Testing*", "findings": 5, "sessions": 3, "agent_made": 5, "legacy": 0}],
            proposals=[{"id": "P1", "kind": "tighten", "rule": "CLAUDE.md → *Testing*",
                        "summary": "CLAUDE.md → *Testing*: agents broke it in 3 sessions (5 findings)",
                        "sources": ["a", "b"]}])
        record["run"].update(rules_window_days=30, min_sessions=3)
        md = self.rec.render_markdown(record)
        self.assertIn("### B1 · open · `a.py:3` · `fanout-b1`", md)
        self.assertIn("### Waiting for the judge", md)
        self.assertIn("| CLAUDE.md → *Testing* | 5 | 3 | 5 | 0 |", md)
        self.assertIn("**P1 · tighten**", md)
        self.assertIn("$2.90 · bug sweep $0.40 · verify $2.00 · rules $0.50", md)
        self.assertIn("needs 3 different sessions", md)

    def test_parked_bugs_render_apart_and_the_backlog_shows_its_trend(self):
        guard = {"verdict": "guard", "date": "2026-10-05", "effect": "no caller passes None",
                 "trigger": "a caller passing None"}
        record = self.build(bugs=[_bug("B1"), _bug("B2", status="parked", verify=guard),
                                  _bug("B3", status="unjudged")])
        record["run"]["backlog_last"] = 64
        md = self.rec.render_markdown(record)
        self.assertIn("1 open · 1 unjudged · 1 parked", md)
        self.assertNotIn("### B2", md)
        self.assertIn("### Parked", md)
        self.assertIn("- B2 · `a.py:3` · `fanout-b2` — no caller passes None Live once: a caller passing None", md)
        self.assertIn("| Backlog | 1 waiting for the judge (last pass 64) |", md)
        self.assertEqual(self.rec.validate(record), [])

    def test_a_landed_fix_verify_dismissed_counts_as_confirmed(self):
        fix = [{"commit": "abcdef0123", "subject": "fix"}]
        bugs = [_bug("B1", status="dismissed", fix_landed=fix), _bug("B2", status="gone", fix_landed=fix),
                _bug("B3", fix_landed=fix)]
        self.assertEqual(self.rec.landed_counts(bugs), (3, 2))

    def test_a_record_without_tokens_says_so(self):
        self.assertIn("| Tokens | not recorded |", self.rec.render_markdown(self.build()))

    def test_no_rules_and_a_failure_render_plainly(self):
        self.assertIn("No reviewer findings mapped to a rule.", self.rec.render_markdown(self.build()))
        md = self.rec.render_markdown(self.rec.failure_record("2026-10-05", stage="verify", error="x"))
        self.assertIn("FAILED", md)
        self.assertIn("**verify**", md)


if __name__ == "__main__":
    unittest.main()
