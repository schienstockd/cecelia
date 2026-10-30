"""Tests for `scripts/agent_eval/codrive_record.py` — a co-driven session's record from its Claude Code
transcript (docs/todo/CODRIVE_RECORD_PLAN.md P1). A synthetic transcript in the interactive format:
your messages as plain strings, a shared capture as a meta line, sub-agent lines, reminders, tool
results with a PNG — each the shape a real session writes."""
from __future__ import annotations

import base64
import importlib.util
import json
import pathlib
import re
import sys
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
sys.path.insert(0, str(_REPO / "scripts" / "agent_eval"))   # it imports its siblings by name
_spec = importlib.util.spec_from_file_location("agent_eval_codrive_record",
                                               _REPO / "scripts" / "agent_eval" / "codrive_record.py")
cr = importlib.util.module_from_spec(_spec)
sys.modules["agent_eval_codrive_record"] = cr
_spec.loader.exec_module(cr)

_PNG = base64.b64encode(b"\x89PNG\r\n\x1a\nfake").decode()
_OBS = "mcp__cecelia-observer__"


def _user(text, ts, origin="human", **kw):
    """A user-role line; `origin.kind` says who sent it, as a real transcript does."""
    return {"type": "user", "timestamp": ts, "sessionId": "sess1", "origin": {"kind": origin},
            "message": {"role": "user", "content": text}, **kw}


def _asst(blocks, ts, **kw):
    return {"type": "assistant", "timestamp": ts, "sessionId": "sess1",
            "message": {"role": "assistant", "model": "claude-test", "content": blocks}, **kw}


def _result(tid, content, ts, err=False):
    return {"type": "user", "timestamp": ts, "sessionId": "sess1",
            "message": {"role": "user", "content": [{"type": "tool_result", "tool_use_id": tid,
                                                     "content": content, "is_error": err}]}}


def _call(tid, name, inp):
    return {"type": "tool_use", "id": tid, "name": name, "input": inp}


LINES = [
    {"type": "permission-mode", "mode": "default"},
    _user("check my project<system-reminder>ignore me</system-reminder>", "2026-10-20T01:00:00Z"),
    _asst([_call("t1", _OBS + "list_projects", {})], "2026-10-20T01:00:01Z"),
    _result("t1", [{"type": "text", "text": '[{"uid": "P1", "name": "Spleen"}]'}], "2026-10-20T01:00:02Z"),
    _asst([_call("t2", _OBS + "gate_plot", {"project_uid": "P1", "image_uid": "I1"})], "2026-10-20T01:00:03Z"),
    _result("t2", [{"type": "image", "source": {"type": "base64", "media_type": "image/png", "data": _PNG}}],
            "2026-10-20T01:00:04Z"),
    _asst([_call("t3", _OBS + "mark_cells", {"project_uid": "P1", "image_uid": "I1", "label_ids": [3]})],
          "2026-10-20T01:00:05Z"),
    _result("t3", "ok", "2026-10-20T01:00:06Z"),
    _asst([_call("t4", "Bash", {"command": "ls"})], "2026-10-20T01:00:07Z"),
    _result("t4", "x", "2026-10-20T01:00:08Z"),
    _asst([{"type": "text", "text": "The CD8 gate keeps 92% of cells."}], "2026-10-20T01:00:09Z"),
    # a sub-agent's own lines are not the session
    _asst([_call("s1", _OBS + "list_images", {"project_uid": "OTHER"})], "2026-10-20T01:00:10Z", isSidechain=True),
    # coding with no Cecelia call: counted, not a section
    _user("now fix the test", "2026-10-20T01:01:00Z"),
    _asst([_call("t5", "Edit", {})], "2026-10-20T01:01:01Z"),
    _user("<task-notification>agent done</task-notification>", "2026-10-20T01:01:30Z", origin="task-notification"),
    _user("Another Claude session sent a message:\nreport from a sub-agent", "2026-10-20T01:01:40Z", origin="peer"),
    _user("#fix that gate is far too loose, it keeps debris", "2026-10-20T01:02:00Z"),
    _asst([_call("t6", _OBS + "get_qc_metrics", {"project_uid": "P1", "image_uid": "I1"})], "2026-10-20T01:02:01Z"),
    _result("t6", "boom", "2026-10-20T01:02:02Z", err=True),
    _asst([{"type": "text", "text": "You're right."}], "2026-10-20T01:02:03Z"),
    # a capture you shared arrives as a META line from a peer
    _user("Another Claude session sent a message:\n[cecelia] shared capture cap-20261020T010300-abc123 from "
          "project P1 (image I1, t=4). Read it with get_capture", "2026-10-20T01:03:00Z", origin="peer",
          isMeta=True),
    _asst([_call("t7", _OBS + "get_capture", {"project_uid": "P1", "capture_id": "cap-20261020T010300-abc123"})],
          "2026-10-20T01:03:01Z"),
    _result("t7", [{"type": "image", "source": {"type": "base64", "media_type": "image/jpeg", "data": "zz"}}],
            "2026-10-20T01:03:02Z"),
    _user("thanks, that's all", "2026-10-20T01:04:00Z", isMeta=True),     # an ordinary meta line: dropped
]


class CodriveRecordTest(unittest.TestCase):
    def setUp(self):
        self.tmp = pathlib.Path(tempfile.mkdtemp())
        self.path = self.tmp / "sess1.jsonl"
        self.path.write_text("\n".join(json.dumps(x) for x in LINES) + "\n", encoding="utf-8")

    def test_plugin_and_plain_tool_names_read_as_one_server(self):
        # the terminal plugin names its tools mcp__plugin_<plugin>_<server>__<tool> (Claude Code hooks doc)
        self.assertEqual(cr._split_tool("mcp__cecelia-observer__get_capture"), ("cecelia-observer", "get_capture"))
        self.assertEqual(cr._split_tool("mcp__plugin_cecelia_cecelia-observer__get_capture"),
                         ("cecelia-observer", "get_capture"))
        self.assertEqual(cr._split_tool("Bash"), ("", "Bash"))

    def test_exchanges_cut_at_your_messages_and_shared_captures(self):
        exs = cr.exchanges(cr.events(str(self.path)))
        self.assertEqual([e["kind"] for e in exs], ["ask", "ask", "fix", "shared"])
        self.assertEqual(exs[0]["you"], "check my project")                  # reminder stripped
        self.assertEqual([c["name"] for c in exs[0]["calls"]], ["list_projects", "gate_plot", "mark_cells"])
        self.assertEqual(exs[0]["other"], 1)                                  # Bash counted, not shown
        self.assertNotIn("list_images", [c["name"] for e in exs for c in e["calls"]])   # sidechain left out
        self.assertEqual(exs[3]["captureId"], "cap-20261020T010300-abc123")

    def test_only_cecelia_exchanges_become_sections_and_a_fix_names_what_it_corrects(self):
        secs = cr.sections(cr.exchanges(cr.events(str(self.path))))
        self.assertEqual([(s["id"], s["kind"]) for s in secs], [("s01", "ask"), ("s02", "fix"), ("s03", "shared")])
        self.assertEqual(secs[1]["corrects"], "s01")

    def test_render_heads_every_section_the_way_the_blackboard_reads_them(self):
        meta, secs = cr.build(self.path)
        self.assertEqual((meta["projectUid"], meta["projectName"], meta["fixes"]), ("P1", "Spleen", 1))
        md = cr.render(meta, secs, {})
        heads = re.findall(r"^### ([dms][0-9]{2,3}) · ", md, re.M)          # `_BB_SECTION_HEAD_RE`
        self.assertEqual(heads, ["s01", "s02", "s03"])
        self.assertIn("### s02 · fix · corrects s01 · #fix that gate is far too loose", md)
        self.assertIn("- **Pointed at:** mark_cells(", md)
        self.assertIn("- **Tried first:** get_qc_metrics(", md)
        self.assertIn("> The CD8 gate keeps 92% of cells.", md)
        self.assertNotIn("now fix the test", md)

    def test_since_until_cut_the_window(self):
        meta, secs = cr.build(self.path, since="2026-10-20T01:01:45Z")
        self.assertEqual([s["kind"] for s in secs], ["fix", "shared"])
        self.assertNotIn("corrects", secs[0])                                 # nothing before it in the window

    def test_dry_run_writes_the_entry_and_pictures_and_posts_nothing(self):
        dry, records = self.tmp / "dry", self.tmp / "records"
        with mock.patch.object(cr.run_record, "_post") as post:
            out = cr.write(self.path, None, dry, records=records)
        post.assert_not_called()
        self.assertFalse(records.exists())
        md = (dry / "entry.md").read_text(encoding="utf-8")
        self.assertTrue(md.startswith("# Co-drive session 2026-10-20 01:00 — Spleen"))
        self.assertIn("`s01-1.png` — gate_plot(", md)
        self.assertIn("`cap-20261020T010300-abc123` — get_capture(", md)    # cited, not re-uploaded
        self.assertEqual(out["sections"], 3)

    def test_write_posts_one_entry_uploads_pngs_and_keeps_a_local_copy(self):
        records = self.tmp / "records"
        with mock.patch.object(cr.run_record, "_post", return_value={"entryId": "bb-1"}) as post, \
             mock.patch.object(cr.run_record, "upload_capture", return_value="cap-up") as up:
            out = cr.write(self.path, "http://x", None, records=records)
        up.assert_called_once()
        body = post.call_args.args[2]
        self.assertEqual(body["projectUid"], "P1")
        self.assertEqual(body["attachments"], ["cap-up", "cap-20261020T010300-abc123"])
        self.assertNotIn("agentRun", body)                                    # an ordinary entry (Decision 6)
        local = json.loads((records / "sess1.json").read_text(encoding="utf-8"))
        self.assertEqual((local["entryId"], local["sections"][1]["corrects"]), ("bb-1", "s01"))
        self.assertEqual(out["entryId"], "bb-1")

    def test_recording_the_session_again_revises_its_entry(self):
        records = self.tmp / "records"
        posts = []
        def post(api, path, body):
            posts.append((path, body))
            return {"entryId": "bb-1"} if path.endswith("/create") else {"ok": True}
        with mock.patch.object(cr.run_record, "_post", side_effect=post), \
             mock.patch.object(cr.run_record, "_get", return_value={"entry": {}}), \
             mock.patch.object(cr.run_record, "upload_capture", return_value="cap-up"):
            cr.write(self.path, "http://x", None, records=records)
            out = cr.write(self.path, "http://x", None, records=records)
            # another window of the same session is its own entry
            cr.write(self.path, "http://x", None, since="2026-10-20T01:01:45Z", records=records)
        self.assertEqual([p for p, _ in posts], ["/api/blackboard/create", "/api/blackboard/revise",
                                                 "/api/blackboard/create"])
        self.assertEqual((posts[1][1]["entryId"], out["entryId"], out["revised"]), ("bb-1", "bb-1", True))

    def test_an_entry_deleted_since_is_made_again(self):
        records = self.tmp / "records"; records.mkdir()
        (records / "sess1.json").write_text(json.dumps({"entryId": "bb-gone"}), encoding="utf-8")
        gone = cr.run_record.urllib.error.HTTPError("u", 404, "Not Found", {}, None)
        with mock.patch.object(cr.run_record, "_post", return_value={"entryId": "bb-2"}) as post, \
             mock.patch.object(cr.run_record, "_get", side_effect=gone), \
             mock.patch.object(cr.run_record, "upload_capture", return_value="cap-up"):
            out = cr.write(self.path, "http://x", None, records=records)
        self.assertEqual(post.call_args.args[1], "/api/blackboard/create")
        self.assertEqual((out["entryId"], out["revised"]), ("bb-2", False))

    def test_a_session_that_never_touched_cecelia_records_nothing(self):
        p = self.tmp / "plain.jsonl"
        p.write_text(json.dumps(_user("hello", "2026-10-20T01:00:00Z")) + "\n", encoding="utf-8")
        with self.assertRaises(SystemExit):
            cr.write(p, None, self.tmp / "dry2", records=self.tmp / "r2")


if __name__ == "__main__":
    unittest.main()
