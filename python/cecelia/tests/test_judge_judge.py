"""Tests for `scripts/judge/judge.py` — the tool-less judge call and its token counts.

Design: docs/ai-assist/WEEKLY_JUDGE.md. `subprocess.run` is patched; nothing spawns `claude`.
Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import json
import pathlib
import subprocess
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load_judge():
    spec = importlib.util.spec_from_file_location("judge", _REPO / "scripts" / "judge" / "judge.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_RESULT = {"structured_output": {"items": []}, "total_cost_usd": 0.49,
           "usage": {"input_tokens": 16, "cache_creation_input_tokens": 40564,
                     "cache_read_input_tokens": 321990, "output_tokens": 5072}}


class TokensTest(unittest.TestCase):
    def setUp(self):
        self.j = _load_judge()

    def test_the_call_returns_the_cli_token_counts(self):
        done = subprocess.CompletedProcess([], 0, json.dumps(_RESULT), "")
        with mock.patch.object(self.j.subprocess, "run", return_value=done), \
                mock.patch.object(self.j, "resolve_claude_bin", return_value="/bin/claude"):
            answer, cost, used = self.j.call_judge("p", {})
        self.assertEqual((answer, cost), ({"items": []}, 0.49))
        self.assertEqual(used, {"input": 16, "cache_write": 40564, "cache_read": 321990, "output": 5072})

    def _call(self, returncode, out):
        done = subprocess.CompletedProcess([], returncode, json.dumps(out), "")
        with mock.patch.object(self.j.subprocess, "run", return_value=done), \
                mock.patch.object(self.j, "resolve_claude_bin", return_value="/bin/claude"):
            return self.j.call_judge("p", {})

    def test_a_usage_limit_raises_rate_limited_with_the_reset_time(self):
        out = {"is_error": True, "api_error_status": 429, "subtype": "success", "total_cost_usd": 0,
               "result": "You've hit your session limit · resets 1:40am (Australia/Sydney)"}
        with self.assertRaisesRegex(self.j.RateLimited, "resets 1:40am"):
            self._call(1, out)
        self.assertFalse(issubclass(self.j.RateLimited, self.j.JudgeError))   # the steps' except can't swallow it
        with self.assertRaises(self.j.RateLimited):   # no status, the CLI's wording only
            self._call(1, {"is_error": True, "result": "Claude usage limit reached"})

    def test_another_error_is_a_judge_error_naming_the_cli_message(self):
        with self.assertRaisesRegex(self.j.JudgeError, r"exit 1\): Overloaded$"):
            self._call(1, {"is_error": True, "api_error_status": 529, "result": "Overloaded"})

    def test_no_usage_is_zeros(self):
        self.assertEqual(self.j.tokens({}), {"input": 0, "cache_write": 0, "cache_read": 0, "output": 0})

    def test_add_sums_in_place_and_no_meter_keeps_nothing(self):
        meter: dict = {}
        self.j.add_tokens(meter, {"input": 1, "output": 2})
        self.j.add_tokens(meter, {"input": 3, "cache_read": 4})
        self.assertEqual(meter, {"input": 4, "cache_write": 0, "cache_read": 4, "output": 2})
        self.j.add_tokens(None, {"input": 1})

    def test_an_injected_two_tuple_has_no_tokens(self):
        self.assertEqual(self.j.unpack(({"a": 1}, 0.2)), ({"a": 1}, 0.2, {}))
        self.assertEqual(self.j.unpack(({"a": 1}, 0.2, {"output": 9})), ({"a": 1}, 0.2, {"output": 9}))


if __name__ == "__main__":
    unittest.main()
