"""Commit / PR attribution: which Claude wrote it (`.claude/hooks/claude_attribution.py`,
`cecelia.effectiveness.attribution`, `claude_sessions.session_stamp`).

Pinned here:
- **A subagent's commit names the subagent's model.** Its Bash env is identical to the parent's, so
  only the PreToolUse event (`agent_id`) can tell them apart. The stamp is recorded there.
- **No fresh stamp from THIS session → no `Claude-Model`.** Never a guess from the parent or an old run.
- **Only an agent's commit with a Claude co-author is touched.** A human's commit is left alone.
- **A PR body without the exact footer is blocked, with the line to add.** An unknown model blocks nothing.

Transcript lines copy the shape of real Claude Code 2.1.312 entries (`type`, `version`, `message.model`).
"""
from __future__ import annotations

import importlib.util
import json
import pathlib
import subprocess
import tempfile
import time
import unittest

from cecelia.effectiveness.attribution import coauthor_line, display_name, footer, trailer_lines
from cecelia.effectiveness.claude_sessions import session_stamp

_REPO = pathlib.Path(__file__).resolve().parents[3]
SID = "bc0bd1e6-c093-4523-afd7-cc16266ed858"
AGENT = "a31faf5c6edc302c0"


def _load(name: str, path: pathlib.Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _assistant(model: str, version: str = "2.1.312", sidechain: bool = False) -> dict:
    return {"type": "assistant", "version": version, "sessionId": SID, "isSidechain": sidechain,
            "requestedModel": model,
            "message": {"role": "assistant", "model": model, "type": "message", "content": []}}


def _write(path: pathlib.Path, entries: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(e) + "\n" for e in entries), encoding="utf-8")


class FormatTest(unittest.TestCase):
    def test_display_name(self):
        self.assertEqual(display_name("claude-opus-5-5"), "Claude Opus 5.5")
        self.assertEqual(display_name("claude-sonnet-4-5-20250929"), "Claude Sonnet 4.5")
        self.assertEqual(display_name("something-else"), "something-else")

    def test_unknown_model_names_no_model(self):
        self.assertEqual(coauthor_line({}), "Co-Authored-By: Claude <noreply@anthropic.com>")
        self.assertEqual(trailer_lines({}), [])
        self.assertEqual(footer({}), "🤖 Generated with [Claude Code](https://claude.com/claude-code)")

    def test_full_stamp(self):
        stamp = {"model": "claude-opus-5-5", "version": "2.1.312", "session": SID}
        self.assertEqual(trailer_lines(stamp), ["Claude-Model: claude-opus-5-5", "Claude-Code: 2.1.312",
                                                f"Claude-Session: {SID}"])
        self.assertTrue(footer(stamp).endswith(" · claude-opus-5-5 · Claude Code 2.1.312"))


class SessionStampTest(unittest.TestCase):
    def test_finds_a_session_by_id(self):
        with tempfile.TemporaryDirectory() as home:
            _write(pathlib.Path(home, "projects", "-x", f"{SID}.jsonl"),
                   [_assistant("claude-opus-5-5", "2.1.300"), {"type": "user", "message": {}},
                    _assistant("claude-sonnet-5-5")])
            self.assertEqual(session_stamp(SID, pathlib.Path(home)),
                             {"model": "claude-sonnet-5-5", "version": "2.1.312", "session": SID})
            self.assertEqual(session_stamp("missing", pathlib.Path(home)), {})
            self.assertEqual(session_stamp("../x", pathlib.Path(home)), {})


class HookTest(unittest.TestCase):
    def setUp(self):
        self.attr = _load("_attr_under_test", _REPO / ".claude" / "hooks" / "claude_attribution.py")
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmp = pathlib.Path(self._tmp.name)
        self.transcript = self.tmp / "projects" / "-x" / f"{SID}.jsonl"
        _write(self.transcript, [_assistant("claude-opus-5-5")])
        _write(self.tmp / "projects" / "-x" / SID / "subagents" / f"agent-{AGENT}.jsonl",
               [_assistant("claude-haiku-5-5", sidechain=True)])
        self.repo = self.tmp / "repo"
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        self.git_dir = self.repo / ".git"
        self.env = {"CLAUDECODE": "1", "CLAUDE_CODE_SESSION_ID": SID, "AI_AGENT": "claude-code_2-1-312_agent"}
        self.msg = "fix: x\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>\n"

    def _event(self, **kw) -> dict:
        return {"session_id": SID, "transcript_path": str(self.transcript), "hook_event_name": "PreToolUse",
                "tool_name": "Bash", "tool_input": {"command": "git commit -F m"}, **kw}

    def test_subagent_commit_records_the_subagents_model(self):
        self.attr.record(self._event(agent_id=AGENT), str(self.repo))
        stamp = json.loads((self.git_dir / self.attr.STAMP_FILE).read_text(encoding="utf-8"))
        self.assertEqual(stamp["model"], "claude-haiku-5-5")

    def test_fresh_stamp_gives_all_three_trailers(self):
        self.attr.record(self._event(), str(self.repo))
        self.assertEqual(self.attr.trailers(self.msg, self.git_dir, self.env, time.time()),
                         ["Claude-Model: claude-opus-5-5", "Claude-Code: 2.1.312", f"Claude-Session: {SID}"])

    def test_stale_or_foreign_stamp_leaves_the_model_out(self):
        self.attr.record(self._event(), str(self.repo))
        late = time.time() + self.attr.MAX_AGE_S + 1
        self.assertEqual(self.attr.trailers(self.msg, self.git_dir, self.env, late),
                         ["Claude-Code: 2.1.312", f"Claude-Session: {SID}"])
        other = {**self.env, "CLAUDE_CODE_SESSION_ID": "other"}
        self.assertNotIn("Claude-Model: claude-opus-5-5",
                         self.attr.trailers(self.msg, self.git_dir, other, time.time()))

    def test_human_commit_untouched(self):
        self.attr.record(self._event(), str(self.repo))
        self.assertEqual(self.attr.trailers(self.msg, self.git_dir, {}, time.time()), [])
        self.assertEqual(self.attr.trailers("fix: x\n", self.git_dir, self.env, time.time()), [])

    def test_commit_msg_file_gets_trailers_in_the_trailer_block(self):
        self.attr.record(self._event(), str(self.repo))
        path = self.repo / "COMMIT_EDITMSG"
        msg = "fix: x\n\n_Convention check (evidence):_\n\n---\n\n**Evidence**\n\n" + self.msg.split("\n\n", 1)[1]
        path.write_text(msg, encoding="utf-8")  # a recital body's `---` is not a patch divider
        orig = self.attr._git_dir
        self.attr._git_dir = lambda cwd: self.git_dir
        try:
            self.attr.stamp_commit_msg(str(path), self.env)
            self.attr.stamp_commit_msg(str(path), self.env)  # an amend: replaced, not doubled
        finally:
            self.attr._git_dir = orig
        self.assertEqual(path.read_text(encoding="utf-8"),
                         "fix: x\n\n_Convention check (evidence):_\n\n---\n\n**Evidence**\n\n"
                         "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>\n"
                         f"Claude-Model: claude-opus-5-5\nClaude-Code: 2.1.312\nClaude-Session: {SID}\n")

    def test_pr_body_without_footer_blocks_with_the_line(self):
        stamp = {"model": "claude-opus-5-5", "version": "2.1.312"}
        body = self.repo / "body.md"
        body.write_text("Does a thing.\n\n🤖 Generated with [Claude Code](https://claude.com/claude-code)\n",
                        encoding="utf-8")
        seg = "gh pr create --base main --title t --body-file body.md"
        reason = self.attr.pr_body_problem(seg, str(self.repo), stamp)
        self.assertIn(footer(stamp), reason)
        body.write_text(f"Does a thing.\n\n{footer(stamp)}\n", encoding="utf-8")
        self.assertIsNone(self.attr.pr_body_problem(seg, str(self.repo), stamp))
        self.assertIsNotNone(self.attr.pr_body_problem("gh pr edit 5 --body 'x'", str(self.repo), stamp))
        self.assertIsNone(self.attr.pr_body_problem("gh pr create --body-file -", str(self.repo), stamp))
        self.assertIsNone(self.attr.pr_body_problem("gh pr edit 5 --add-label x", str(self.repo), stamp))
        self.assertIsNone(self.attr.pr_body_problem(seg.replace("body.md", "nope.md"), str(self.repo), stamp))
        self.assertIsNone(self.attr.pr_body_problem("gh pr edit 5 --body 'x'", str(self.repo), {}))


class GuardWiringTest(unittest.TestCase):
    """`check_commit_recital.main` (the registered PreToolUse hook) runs the PR check on a cecelia checkout."""

    def test_pr_footer_guard_on_this_checkout(self):
        hook = _load("_recital_under_test", _REPO / ".claude" / "hooks" / "check_commit_recital.py")
        with tempfile.TemporaryDirectory() as tmp:
            transcript = pathlib.Path(tmp, f"{SID}.jsonl")
            _write(transcript, [_assistant("claude-opus-5-5")])
            event = {"session_id": SID, "transcript_path": str(transcript)}
            cmd = "gh pr create --title t --body 'no footer'"   # runs in the session cwd: this checkout
            self.assertIn("claude-opus-5-5", hook.pr_footer_guard(cmd, str(_REPO), event))
            ok = f"gh pr create --title t --body '{footer({'model': 'claude-opus-5-5', 'version': '2.1.312'})}'"
            self.assertIsNone(hook.pr_footer_guard(ok, str(_REPO), event))
            self.assertIsNone(hook.pr_footer_guard("gh pr create --body x", tmp, event))  # not cecelia


if __name__ == "__main__":
    unittest.main()
