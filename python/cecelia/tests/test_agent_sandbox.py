"""Tests for `cecelia.effectiveness.agent_sandbox` — the containment every `claude -p` agent runs in.

The agents run with `--dangerously-skip-permissions`, so these settings are their only fence.
Run with `pixi run test-py`.
"""
import json
import pathlib
import sys
import unittest

from cecelia.effectiveness import agent_sandbox


class SandboxTest(unittest.TestCase):
    def test_bash_is_sandboxed_offline_and_home_is_write_denied(self):
        s = agent_sandbox.SANDBOX_SETTINGS
        self.assertTrue(s["sandbox"]["enabled"])
        self.assertFalse(s["sandbox"]["allowUnsandboxedCommands"])
        self.assertEqual(s["sandbox"]["network"]["deniedDomains"], ["*"])
        for rule in ("Write(~/**)", "Edit(~/**)", "Read(~/.ssh/**)", "Read(~/.claude/**)", "WebFetch"):
            self.assertIn(rule, s["permissions"]["deny"])

    # Windows keeps its temp dir under the profile, and the Claude Code sandbox is Linux/macOS only.
    @unittest.skipIf(sys.platform == "win32", "agents run on Linux/macOS")
    def test_the_worktree_root_is_outside_home(self):
        # a `~/**` write deny would otherwise block the agent's own checkout
        self.assertFalse(agent_sandbox.WORKTREE_ROOT.resolve().is_relative_to(pathlib.Path.home().resolve()))


class StreamTest(unittest.TestCase):
    def test_tool_calls_in_order_and_the_result_event(self):
        lines = [
            {"type": "assistant", "message": {"content": [{"type": "text", "text": "hi"},
                                                          {"type": "tool_use", "name": "Read", "input": {"p": 1}}]}},
            {"type": "assistant", "message": {"content": [{"type": "tool_use", "name": "Bash", "input": None}]}},
            {"type": "result", "total_cost_usd": 0.42, "num_turns": 3, "result": "done"},
        ]
        stdout = "\n".join(json.dumps(x) for x in lines[:2]) + "\nnot json\n" + json.dumps(lines[2]) + "\n"
        sig = agent_sandbox.parse_stream_json(stdout)
        self.assertEqual(sig.tool_calls, [{"tool": "Read", "input": {"p": 1}}, {"tool": "Bash", "input": {}}])
        self.assertEqual((sig.cost_usd, sig.turns, sig.final_message, sig.parse_errors), (0.42, 3, "done", 1))
        self.assertIsNone(sig.rate_limited)

    def test_a_run_the_usage_limit_stopped_says_so(self):
        msg = "You've hit your session limit · resets 1:40am (Australia/Sydney)"
        lines = [{"type": "assistant", "message": {"content": [{"type": "tool_use", "name": "Bash", "input": {}}]}},
                 {"type": "result", "subtype": "success", "is_error": True, "total_cost_usd": 1.2, "num_turns": 7,
                  "result": msg}]
        sig = agent_sandbox.parse_stream_json("\n".join(json.dumps(x) for x in lines))
        self.assertEqual((sig.rate_limited, sig.turns, len(sig.tool_calls)), (msg, 7, 1))


if __name__ == "__main__":
    unittest.main()
