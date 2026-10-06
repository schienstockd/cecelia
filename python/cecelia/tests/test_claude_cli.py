"""Tests for `cecelia.effectiveness.claude_cli` — reading a usage-limit refusal from a `claude -p`
result, when it lifts, and the one-line exit a command-line tool gives on it.

Shared by the weekly judge (`scripts/judge/`) and the autonomous runs (`scripts/agent_eval/`).
Nothing spawns `claude`. Run with `pixi run test-py`.
"""
from __future__ import annotations

import contextlib
import datetime as dt
import io
import json
import subprocess
import unittest
import zoneinfo

from cecelia.effectiveness import claude_cli

_LIMIT = "You've hit your session limit · resets 1:40am (Australia/Sydney)"


class RateLimitTest(unittest.TestCase):
    def test_a_429_or_the_limit_wording_is_a_limit(self):
        self.assertEqual(claude_cli.rate_limit({"is_error": True, "api_error_status": 429, "result": _LIMIT}), _LIMIT)
        self.assertEqual(claude_cli.rate_limit({"is_error": True, "api_error_status": 429}), "HTTP 429")
        self.assertEqual(claude_cli.rate_limit({"is_error": True, "result": "Claude usage limit reached"}),
                         "Claude usage limit reached")

    def test_a_stream_json_result_event_reads_the_same(self):
        ev = {"type": "result", "subtype": "success", "is_error": True, "result": _LIMIT, "num_turns": 12}
        self.assertEqual(claude_cli.rate_limit(ev), _LIMIT)

    def test_not_an_error_or_another_error_is_not_a_limit(self):
        self.assertIsNone(claude_cli.rate_limit({"result": "I hit the rate limit of the API, so I stopped."}))
        self.assertIsNone(claude_cli.rate_limit({"is_error": True, "api_error_status": 529, "result": "Overloaded"}))
        self.assertIsNone(claude_cli.rate_limit({}))
        self.assertIsNone(claude_cli.rate_limit(None))   # a stream with no result event

    def test_the_note_keeps_the_message_and_when_it_lifts(self):
        now = dt.datetime(2026, 10, 6, 0, 3, tzinfo=zoneinfo.ZoneInfo("Australia/Sydney"))
        self.assertEqual(claude_cli.rate_limit_note(_LIMIT, now),
                         {"message": _LIMIT, "resetAt": "2026-10-06T01:40+11:00"})

    def test_a_cli_tool_exits_75_with_one_line(self):
        now = dt.datetime(2026, 10, 6, 0, 3, tzinfo=zoneinfo.ZoneInfo("Australia/Sydney"))
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = claude_cli.limit_exit("judge-bugs", claude_cli.RateLimited(_LIMIT), now)
        self.assertEqual(code, claude_cli.EX_TEMPFAIL)
        self.assertEqual(code, 75)
        self.assertEqual(err.getvalue().count("\n"), 1)
        self.assertIn("judge-bugs: usage limit — lifts 2026-10-06T01:40+11:00", err.getvalue())


class ReadResultTest(unittest.TestCase):
    def _proc(self, stdout, rc=0):
        return subprocess.CompletedProcess(["claude"], rc, stdout=stdout, stderr="")

    def test_a_limited_result_raises(self):
        out = {"is_error": True, "api_error_status": 429, "result": "You've hit your session limit"}
        with self.assertRaises(claude_cli.RateLimited):
            claude_cli.read_result(self._proc(json.dumps(out), rc=1))

    def test_other_results_are_returned_and_junk_is_empty(self):
        out = {"is_error": True, "result": "boom"}
        self.assertEqual(claude_cli.read_result(self._proc(json.dumps(out), rc=1)), out)
        self.assertEqual(claude_cli.read_result(self._proc("not json")), {})
        self.assertEqual(claude_cli.read_result(self._proc("[1]")), {})


class ResetAtTest(unittest.TestCase):
    def _at(self, message, now):
        return claude_cli.reset_at(message, now)

    def test_the_reset_is_the_next_time_that_clock_reads_it_in_its_zone(self):
        syd = zoneinfo.ZoneInfo("Australia/Sydney")
        # 00:03 in Sydney, limit lifts 01:40 the same night
        now = dt.datetime(2026, 10, 6, 0, 3, tzinfo=syd)
        self.assertEqual(self._at(_LIMIT, now), dt.datetime(2026, 10, 6, 1, 40, tzinfo=syd))
        # already past today: tomorrow
        at = self._at("resets 1:40am (Australia/Sydney)", dt.datetime(2026, 10, 6, 9, 0, tzinfo=syd))
        self.assertEqual(at, dt.datetime(2026, 10, 7, 1, 40, tzinfo=syd))
        # a `now` in another zone (UTC) still reads the clock in Sydney: 13:03Z is 00:03 there
        at = self._at("resets 1:40am (Australia/Sydney)", dt.datetime(2026, 10, 5, 13, 3, tzinfo=dt.timezone.utc))
        self.assertEqual(at.astimezone(dt.timezone.utc), dt.datetime(2026, 10, 5, 14, 40, tzinfo=dt.timezone.utc))

    def test_pm_noon_and_midnight(self):
        utc = zoneinfo.ZoneInfo("UTC")
        now = dt.datetime(2026, 10, 6, 0, 30, tzinfo=utc)
        self.assertEqual(self._at("resets 11pm (UTC)", now).hour, 23)
        self.assertEqual(self._at("resets 12pm (UTC)", now).hour, 12)
        self.assertEqual(self._at("resets 12am (UTC)", now), dt.datetime(2026, 10, 7, 0, 0, tzinfo=utc))

    def test_an_unreadable_reset_is_an_hour_from_now(self):
        now = dt.datetime(2026, 10, 6, 0, 0, tzinfo=dt.timezone.utc)
        for msg in ("You've hit your session limit", "resets 1:40am (Not/AZone)", "resets 13:00pm (UTC)", ""):
            self.assertEqual(self._at(msg, now), now + dt.timedelta(hours=1), msg)


if __name__ == "__main__":
    unittest.main()
