"""The model + session behind an MCP call: the PreToolUse hook (`caller_hook.py`) and the server half
that strips the fields and forwards them as headers (`caller.py`).

Transcript lines are trimmed from a real Claude Code transcript (2026-10-29): the `user` and
`assistant` entry shapes, and `cost-state`, which names OTHER models and must not be read as the caller.
"""
import asyncio
import io
import json
import os
import tempfile
import unittest
from unittest import mock

from mcp.server.mcpserver import Context

from cecelia_mcp import caller_hook
from cecelia_mcp.caller import CallerMCPServer, caller_headers, pop_caller
from cecelia_mcp.caller_hook import MODEL_FIELD, SESSION_FIELD

SID = "578e0b74-c216-49f3-b993-745a618bdd7e"
USER = {"type": "user", "isSidechain": False, "sessionId": SID,
        "message": {"role": "user", "content": []}}


def _assistant(model):
    return {"type": "assistant", "isSidechain": False, "sessionId": SID, "requestedModel": model,
            "message": {"role": "assistant", "model": model, "type": "message", "content": []}}


COST = {"type": "cost-state", "sessionId": SID,
        "modelUsage": {"claude-haiku-5-5": {}, "claude-sonnet-5-5": {}}}


def _write(path, entries):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        for e in entries:
            f.write(json.dumps(e) + "\n")


class HookTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.main = os.path.join(self.tmp.name, f"{SID}.jsonl")
        _write(self.main, [USER, _assistant("claude-opus-5-5"), USER, _assistant("claude-haiku-5-5"), COST])

    def tearDown(self):
        self.tmp.cleanup()

    def _event(self, **kw):
        e = {"session_id": SID, "transcript_path": self.main, "hook_event_name": "PreToolUse",
             "tool_name": "mcp__cecelia-observer__create_chain", "tool_input": {"name": "n", "nodes": [1]}}
        e.update(kw)
        return e

    def test_newest_assistant_model_and_session_added_input_kept(self):
        out = caller_hook.hook_output(self._event())
        hso = out["hookSpecificOutput"]
        self.assertEqual(hso["updatedInput"],
                         {"name": "n", "nodes": [1], MODEL_FIELD: "claude-haiku-5-5", SESSION_FIELD: SID})

    def test_never_decides_permission(self):
        # "allow" here would auto-approve every observer call and bypass the user's permission prompts
        self.assertNotIn("permissionDecision", caller_hook.hook_output(self._event())["hookSpecificOutput"])

    def test_subagent_reads_its_own_transcript(self):
        # transcript_path is the PARENT's on a subagent call; reading it would stamp the parent's model
        sub = os.path.join(self.tmp.name, SID, "subagents", "agent-aa96c73b7695364a2.jsonl")
        _write(sub, [USER, _assistant("claude-sonnet-5-5")])
        out = caller_hook.hook_output(self._event(agent_id="aa96c73b7695364a2", agent_type="general-purpose"))
        self.assertEqual(out["hookSpecificOutput"]["updatedInput"][MODEL_FIELD], "claude-sonnet-5-5")

    def test_subagent_without_transcript_leaves_model_unknown(self):
        out = caller_hook.hook_output(self._event(agent_id="nope"))
        ui = out["hookSpecificOutput"]["updatedInput"]
        self.assertNotIn(MODEL_FIELD, ui)  # never the parent's model as a guess
        self.assertEqual(ui[SESSION_FIELD], SID)

    def test_hostile_agent_id_does_not_escape(self):
        self.assertEqual(caller_hook.caller_transcript(self._event(agent_id="../../etc/passwd")), "")

    def test_model_beyond_tail_window_still_found(self):
        _write(self.main, [_assistant("claude-opus-5-5"), USER])
        with mock.patch.object(caller_hook, "_TAIL_BYTES", 40):
            self.assertEqual(caller_hook.transcript_model(self.main), "claude-opus-5-5")

    def test_fails_open_on_garbage(self):
        with mock.patch("sys.stdin", io.StringIO("not json")), \
             mock.patch("sys.stdout", new_callable=io.StringIO) as out:
            caller_hook.main()
        self.assertEqual(out.getvalue(), "")  # no output = call proceeds unmodified

    def test_no_tool_input_leaves_call_alone(self):
        self.assertIsNone(caller_hook.hook_output({"session_id": SID}))


class ServerTest(unittest.TestCase):
    def test_pop_caller_validates(self):
        args, c = pop_caller({"x": 1, MODEL_FIELD: "claude-opus-5-5", SESSION_FIELD: SID})
        self.assertEqual(args, {"x": 1})
        self.assertEqual(c, {"model": "claude-opus-5-5", "session": SID})
        args, c = pop_caller({"x": 1, MODEL_FIELD: "evil\r\nX-Injected: 1", SESSION_FIELD: 7})
        self.assertEqual((args, c), ({"x": 1}, {}))

    def test_tool_sees_headers_but_not_fields(self):
        srv = CallerMCPServer("t")
        seen = {}

        @srv.tool()
        def probe(text: str) -> str:
            seen["headers"] = caller_headers()
            return text

        asyncio.run(srv.call_tool("probe", {"text": "a", MODEL_FIELD: "claude-opus-5-5", SESSION_FIELD: SID},
                                  Context(mcp_server=srv)))
        self.assertEqual(seen["headers"], {"X-Cecelia-Model": "claude-opus-5-5", "X-Cecelia-Session": SID})
        self.assertEqual(caller_headers(), {})  # reset after the call

    def test_headers_reach_the_request(self):
        from cecelia_mcp.caller import _CALLER
        from cecelia_mcp.client import CeceliaClient
        from tests.test_client import _patch_urlopen
        token = _CALLER.set({"model": "claude-opus-5-5", "session": SID})
        try:
            with _patch_urlopen({"ok": True}) as u:
                CeceliaClient(base_url="http://x:8080")._request(
                    "POST", "/api/blackboard/create", body={"projectUid": "", "title": "t"})
        finally:
            _CALLER.reset(token)
        req = u.call_args[0][0]
        self.assertEqual(req.get_header("X-cecelia-model"), "claude-opus-5-5")
        self.assertEqual(req.get_header("X-cecelia-session"), SID)


if __name__ == "__main__":
    unittest.main()
