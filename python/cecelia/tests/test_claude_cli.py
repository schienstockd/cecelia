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

#: The CLI's `<synthetic>` assistant turn when the limit cut a session off, verbatim from a real 2.1.291
#: transcript (~/.claude/projects/…/121acb56….jsonl, 2026-10-06; usage and ids trimmed).
_REAL_SYNTHETIC = {"type": "assistant", "message": {
    "model": "<synthetic>", "role": "assistant", "stop_reason": "stop_sequence", "type": "message",
    "content": [{"type": "text", "text": "You've hit your session limit · resets 12:10am (Australia/Sydney)"}]},
    "error": "rate_limit", "isApiErrorMessage": True, "apiErrorStatus": 429}

#: Usage-limit wordings: the real one, and forms reported for other CLI versions (not seen in a transcript here).
_LIMIT_WORDINGS = (
    "You've hit your session limit · resets 12:10am (Australia/Sydney)",   # real (396 in ~/.claude transcripts)
    "You've hit your session limit · resets 1:10pm (Australia/Sydney)",    # real: CLI, HTTP 429, 2026-10-24
    "You've hit your limit · resets 3pm (Australia/Sydney)",
    "5-hour limit reached ∙ resets 3pm",
    "Claude AI usage limit reached|1791292200",
    "weekly limit reached",
    "Limit reached · resets 9am (UTC)",
)
#: Errors that mention a limit but are not the seat's quota.
_NOT_LIMITS = (
    "Prompt is too long",
    "Context limit reached · /compact or /clear to continue",
    "context limit exceeded",
    "Error: exceeded max budget of $3.00",
    "Reached max turns (40)",
    "output token limit reached",
    "You've hit your context limit",
    "API Error: 529 Overloaded",
)


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


class WordingTest(unittest.TestCase):
    def test_every_limit_wording_is_a_limit(self):
        for text in _LIMIT_WORDINGS:
            self.assertEqual(claude_cli.rate_limit({"is_error": True, "result": text}), text, text)
            self.assertEqual(claude_cli.limit_in_text(f"some log line\n  {text}\n"), text, text)

    def test_other_limits_are_not_the_usage_limit(self):
        for text in _NOT_LIMITS:
            self.assertIsNone(claude_cli.rate_limit({"is_error": True, "result": text}), text)
            self.assertIsNone(claude_cli.limit_in_text(text), text)
        self.assertIsNone(claude_cli.limit_in_text(""))
        self.assertIsNone(claude_cli.limit_in_text(None))

    def test_the_clis_synthetic_limit_turn(self):
        self.assertEqual(claude_cli.synthetic_limit(_REAL_SYNTHETIC),
                         "You've hit your session limit · resets 12:10am (Australia/Sydney)")
        # the agent's own prose about a rate limit is not the CLI's turn
        prose = {"type": "assistant", "message": {"model": "claude-opus-5-5", "content": [
            {"type": "text", "text": "The API's rate limit stopped me."}]}}
        self.assertIsNone(claude_cli.synthetic_limit(prose))
        other = {"type": "assistant", "message": {"model": "<synthetic>", "content": [
            {"type": "text", "text": "Prompt is too long"}]}}
        self.assertIsNone(claude_cli.synthetic_limit(other))
        self.assertIsNone(claude_cli.synthetic_limit({"type": "result"}))
        self.assertIsNone(claude_cli.synthetic_limit(None))


class ReadResultTest(unittest.TestCase):
    def _proc(self, stdout, rc=0):
        return subprocess.CompletedProcess(["claude"], rc, stdout=stdout, stderr="")

    def test_a_limited_result_raises(self):
        out = {"is_error": True, "api_error_status": 429, "result": "You've hit your session limit"}
        with self.assertRaises(claude_cli.RateLimited):
            claude_cli.read_result(self._proc(json.dumps(out), rc=1))

    def test_a_run_that_died_with_the_limit_on_stderr_only_raises(self):
        proc = subprocess.CompletedProcess(["claude"], 1, stdout="", stderr="Error: 5-hour limit reached ∙ resets 3pm\n")
        with self.assertRaises(claude_cli.RateLimited) as e:
            claude_cli.read_result(proc)
        self.assertEqual(str(e.exception), "Error: 5-hour limit reached ∙ resets 3pm")
        # exit 0, or another failure on stderr: not a limit
        ok = subprocess.CompletedProcess(["claude"], 0, stdout="{}", stderr="weekly limit reached")
        self.assertEqual(claude_cli.read_result(ok), {})
        other = subprocess.CompletedProcess(["claude"], 1, stdout="", stderr="Prompt is too long")
        self.assertEqual(claude_cli.read_result(other), {})

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

    def test_the_dot_forms_and_a_unix_time(self):
        syd = zoneinfo.ZoneInfo("Australia/Sydney")
        now = dt.datetime(2026, 10, 6, 13, 0, tzinfo=syd)
        self.assertEqual(self._at("You've hit your session limit · resets 1:10pm (Australia/Sydney)", now),
                         dt.datetime(2026, 10, 6, 13, 10, tzinfo=syd))   # real, 2026-10-24
        self.assertEqual(self._at("You've hit your limit · resets 3pm (Australia/Sydney)", now),
                         dt.datetime(2026, 10, 6, 15, 0, tzinfo=syd))
        self.assertEqual(self._at("5-hour limit reached ∙ resets 3:30pm (Australia/Sydney)", now),
                         dt.datetime(2026, 10, 6, 15, 30, tzinfo=syd))
        # 1791292200 is the real `resetsAt` of the 12:10am sample: 2026-10-07 00:10 in Sydney
        at = self._at("Claude AI usage limit reached|1791292200", now)
        self.assertEqual(at, dt.datetime(2026, 10, 7, 0, 10, tzinfo=syd))
        # a Unix time already past: try again in an hour, not at once
        late = dt.datetime(2026, 10, 8, 0, 0, tzinfo=syd)
        self.assertEqual(self._at("Claude AI usage limit reached|1791292200", late), late + dt.timedelta(hours=1))

    def test_an_unreadable_reset_is_an_hour_from_now(self):
        now = dt.datetime(2026, 10, 6, 0, 0, tzinfo=dt.timezone.utc)
        for msg in ("You've hit your session limit", "resets 1:40am (Not/AZone)", "resets 13:00pm (UTC)", ""):
            self.assertEqual(self._at(msg, now), now + dt.timedelta(hours=1), msg)

    def test_a_weekly_reset_with_a_date(self):
        syd = zoneinfo.ZoneInfo("Australia/Sydney")
        now = dt.datetime(2026, 10, 24, 13, 0, tzinfo=syd)
        for msg in ("You've hit your weekly limit · resets Oct 30, 9am (Australia/Sydney)",
                    "Weekly limit reached · resets Oct 30 at 9am (Australia/Sydney)",
                    "weekly limit · resets October 30th, 9:00am (Australia/Sydney)"):
            self.assertEqual(self._at(msg, now), dt.datetime(2026, 10, 30, 9, 0, tzinfo=syd), msg)
        # "Jan 2" read on Dec 30 is next year's; a date just past or impossible is unreadable
        dec = dt.datetime(2026, 12, 30, 13, 0, tzinfo=syd)
        self.assertEqual(self._at("resets Jan 2, 9am (Australia/Sydney)", dec), dt.datetime(2027, 1, 2, 9, 0, tzinfo=syd))
        self.assertIsNone(claude_cli.parse_reset("resets Oct 23, 9am (Australia/Sydney)", now))
        self.assertIsNone(claude_cli.parse_reset("resets Feb 30, 9am (Australia/Sydney)", now))

    def test_parse_reset_says_when_it_cannot_read_one(self):
        now = dt.datetime(2026, 10, 6, 0, 0, tzinfo=dt.timezone.utc)
        for msg in ("You've hit your session limit", "weekly limit reached", "resets 1:40am (Not/AZone)", ""):
            self.assertIsNone(claude_cli.parse_reset(msg, now), msg)
        self.assertEqual(claude_cli.parse_reset("resets 1am (UTC)", now), now + dt.timedelta(hours=1))


#: A `rate_limit_event` as `claude -p --output-format stream-json` 2.1.291 writes it on every turn
#: (verbatim from ~/.cecelia-effectiveness/ladder/traces/…-r3/stream.jsonl, ids trimmed): `allowed`.
_REAL_ALLOWED_EVENT = {"type": "rate_limit_event", "rate_limit_info": {
    "status": "allowed", "resetsAt": 1791088200, "rateLimitType": "five_hour", "overageStatus": "rejected",
    "overageDisabledReason": "group_zero_credit_limit", "isUsingOverage": False,
    "unifiedWindows": {"five_hour": {"utilization": 0.25, "resetsAt": 1791088200}}}}
#: The real transcript's synthetic limit turn carries the refusal's `quotaLimits` (121acb56….jsonl).
_REAL_QUOTA = {"status": "rejected", "resetsAt": 1791292200, "unifiedRateLimitFallbackAvailable": False,
               "rateLimitType": "five_hour", "overageStatus": "rejected"}


class LimitResetsAtTest(unittest.TestCase):
    def test_only_a_refusal_names_the_reset(self):
        self.assertIsNone(claude_cli.limit_resets_at(_REAL_ALLOWED_EVENT))   # a window that didn't refuse
        rejected = {**_REAL_ALLOWED_EVENT, "rate_limit_info": {
            **_REAL_ALLOWED_EVENT["rate_limit_info"], "status": "rejected", "rateLimitType": "seven_day",
            "resetsAt": 1791700000}}
        self.assertEqual(claude_cli.limit_resets_at(rejected), 1791700000)
        self.assertEqual(claude_cli.limit_resets_at({**_REAL_SYNTHETIC, "quotaLimits": _REAL_QUOTA}), 1791292200)
        for junk in (None, {}, {"type": "rate_limit_event"}, {"quotaLimits": {"status": "rejected"}},
                     {"quotaLimits": {"status": "rejected", "resetsAt": "soon"}}):
            self.assertIsNone(claude_cli.limit_resets_at(junk), junk)

    def test_with_reset_threads_the_epoch_through_reset_at(self):
        syd = zoneinfo.ZoneInfo("Australia/Sydney")
        now = dt.datetime(2026, 10, 6, 13, 0, tzinfo=syd)
        msg = claude_cli.with_reset("You've hit your weekly limit", 1791292200)
        self.assertEqual(msg, "You've hit your weekly limit|1791292200")
        self.assertEqual(claude_cli.reset_at(msg, now), dt.datetime(2026, 10, 7, 0, 10, tzinfo=syd))
        # the epoch wins over the clock text; none to add, or one already there: unchanged
        self.assertEqual(claude_cli.reset_at(claude_cli.with_reset("resets 3pm (Australia/Sydney)", 1791292200), now),
                         dt.datetime(2026, 10, 7, 0, 10, tzinfo=syd))
        self.assertEqual(claude_cli.with_reset("limit", None), "limit")
        self.assertEqual(claude_cli.with_reset("Claude AI usage limit reached|1791292200", 1),
                         "Claude AI usage limit reached|1791292200")


if __name__ == "__main__":
    unittest.main()
