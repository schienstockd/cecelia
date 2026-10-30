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

from cecelia.effectiveness.attribution import (
    coauthor_line, display_name, footer, reply_line, reply_phrase, trailer_lines)
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

    def _trailers(self, env, now):
        return self.attr.trailers(self.attr.fresh_stamp(self.git_dir, env, now), env)

    def test_fresh_stamp_gives_all_three_trailers(self):
        self.attr.record(self._event(), str(self.repo))
        self.assertEqual(self._trailers(self.env, time.time()),
                         ["Claude-Model: claude-opus-5-5", "Claude-Code: 2.1.312", f"Claude-Session: {SID}"])

    def test_stale_or_foreign_stamp_leaves_the_model_out(self):
        self.attr.record(self._event(), str(self.repo))
        late = time.time() + self.attr.MAX_AGE_S + 1
        self.assertEqual(self._trailers(self.env, late), ["Claude-Code: 2.1.312", f"Claude-Session: {SID}"])
        other = {**self.env, "CLAUDE_CODE_SESSION_ID": "other"}
        self.assertNotIn("Claude-Model: claude-opus-5-5", self._trailers(other, time.time()))

    def test_only_an_agent_commit_with_a_claude_coauthor_counts(self):
        self.assertTrue(self.attr.is_agent_commit(self.msg, self.env))
        self.assertFalse(self.attr.is_agent_commit(self.msg, {}))            # a human's commit
        self.assertFalse(self.attr.is_agent_commit("fix: x\n", self.env))    # no Claude co-author

    def test_subagent_coauthor_line_is_corrected_to_its_own_model(self):
        # a haiku subagent copies its parent's "Claude Opus 5.5" from its instructions
        haiku = {"model": "claude-haiku-5-5"}
        self.assertEqual(self.attr.fix_coauthor(self.msg, haiku),
                         "fix: x\n\nCo-Authored-By: Claude Haiku 5.5 <noreply@anthropic.com>\n")
        self.assertEqual(self.attr.fix_coauthor(self.msg, {}), self.msg)      # no model: as typed
        human = "fix: x\n\nCo-Authored-By: Ann Lee <ann@lab.org>\n" + self.msg.split("\n\n", 1)[1]
        self.assertIn("Ann Lee <ann@lab.org>", self.attr.fix_coauthor(human, haiku))
        both = self.msg + "Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\n"
        self.assertEqual(self.attr.fix_coauthor(both, haiku), both)          # two Claudes: can't tell

    def test_human_commit_untouched(self):
        self.attr.record(self._event(), str(self.repo))
        path = self.repo / "COMMIT_EDITMSG"
        path.write_text(self.msg, encoding="utf-8")
        orig = self.attr._git_dir
        self.attr._git_dir = lambda cwd: self.git_dir
        try:
            self.attr.stamp_commit_msg(str(path), {})
        finally:
            self.attr._git_dir = orig
        self.assertEqual(path.read_text(encoding="utf-8"), self.msg)

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

    def test_gh_api_pr_body_is_checked_but_comments_are_not(self):
        stamp = {"model": "claude-opus-5-5", "version": "2.1.312"}
        ok = footer(stamp)
        (self.repo / "b.md").write_text("x", encoding="utf-8")
        (self.repo / "b.json").write_text(json.dumps({"body": f"x\n\n{ok}\n"}), encoding="utf-8")
        where = str(self.repo)
        problem = lambda seg: self.attr.pr_body_problem(seg, where, stamp)  # noqa: E731
        self.assertIsNotNone(problem("gh api repos/o/r/pulls/5 -X PATCH -f body='no footer'"))
        self.assertIsNotNone(problem("gh api -X PATCH repos/o/r/pulls/5 --raw-field body=x"))
        self.assertIsNotNone(problem("gh api repos/{owner}/{repo}/pulls/5 -X PATCH -F body=@b.md"))
        self.assertIsNone(problem(f"gh api repos/o/r/pulls/5 -X PATCH -f body='x {ok}'"))
        self.assertIsNone(problem("gh api repos/o/r/pulls/5 -X PATCH --input b.json"))
        self.assertIsNone(problem("gh api repos/o/r/pulls/5 -X PATCH -F body=@-"))          # stdin
        self.assertIsNone(problem("gh api repos/o/r/issues/5/comments -f body='a reply'"))  # not a PR body
        self.assertIsNone(problem("gh api repos/o/r/pulls/5/comments -f body='review'"))
        self.assertIsNone(problem("gh api repos/o/r/pulls/5 -X PATCH -f title=t"))


class ReplyTest(unittest.TestCase):
    """Public text under the owner's account (replies, issues) carries the attribution naming the
    model. A PR's own body has the footer instead."""

    def setUp(self):
        self.attr = _load("_attr_reply_under_test", _REPO / ".claude" / "hooks" / "claude_attribution.py")
        self.stamp = {"model": "claude-haiku-5-5", "version": "2.1.312"}
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.where = self._tmp.name

    def problem(self, seg):
        return self.attr.reply_problem(seg, self.where, self.stamp)

    def body_file(self, name, text):
        # the line has an apostrophe (`@schienstockd's`), so real replies pass it in a file
        pathlib.Path(self.where, name).write_text(text, encoding="utf-8")
        return name

    def test_line_names_the_model_and_optionally_the_fix(self):
        self.assertIn("written by Claude Code (Claude Haiku 5.5), working", reply_line(self.stamp))
        self.assertIn("— as was the fix in #12.", reply_line(self.stamp, "#12"))
        self.assertIn("https://github.com/", reply_line(self.stamp))   # a relative link breaks in a comment

    def test_unattributed_or_wrong_model_reply_is_blocked_with_the_line(self):
        reason = self.problem("gh issue comment 5 --repo other/x --body 'Fixed, thanks!'")
        self.assertIn(reply_line(self.stamp), reason)
        opus = self.body_file("o.md", reply_line({"model": "claude-opus-5-5"}))   # copied from the parent
        self.assertIsNotNone(self.problem(f"gh pr comment 7 --body-file {opus}"))
        pathlib.Path(self.where, "r.md").write_text("ok", encoding="utf-8")
        self.assertIsNotNone(self.problem("gh pr review 7 --comment --body-file r.md"))
        self.assertIsNotNone(self.problem("gh api repos/o/r/issues/5/comments -f body='hi'"))
        self.assertIsNotNone(self.problem("gh api -X POST repos/o/r/pulls/5/reviews -f body=hi"))

    def test_attributed_or_bodiless_replies_pass(self):
        good = self.body_file("g.md", "Fixed.\n\n---\n\n" + reply_line(self.stamp, "#9"))
        self.assertIsNone(self.problem(f"gh issue comment 5 --body-file {good}"))
        self.assertIsNone(self.problem(f"gh api repos/o/r/issues/comments/3 -X PATCH -F body=@{good}"))
        self.assertIsNone(self.problem(f'gh issue comment 5 --body "x {reply_phrase(self.stamp)}"'))
        self.assertIsNone(self.problem("gh pr review 7 --approve"))                    # no body
        self.assertIsNone(self.problem("gh issue comment 5 --body-file -"))           # stdin
        self.assertIsNone(self.problem("gh api repos/o/r/pulls/5 -X PATCH -f body=x"))  # a PR body: footer's job

    def test_user_report_issue_footer_counts_wrapped_as_in_the_template(self):
        footer_md = ("Body.\n\n---\n📨 *From a user report against v0.1.4. The symptom is the user's; cause"
                     " and fix were traced on `main` @ abc by Claude Code\n(Claude Haiku 5.5) on "
                     "@schienstockd's behalf. Decisions here are his.*\n")
        issue = self.body_file("i.md", footer_md)
        self.assertIsNone(self.problem(f"gh issue create --repo o/r --title t --body-file {issue}"))
        bare = self.body_file("b.md", "Body.")
        reason = self.problem(f"gh issue create --title t --body-file {bare}")
        self.assertIn("docs/USER_REPORTS.md", reason)
        self.assertIsNotNone(self.problem("gh api repos/o/r/issues -f title=t -f body=x"))
        self.assertIsNone(self.attr.reply_problem("gh issue comment 5 --body x", self.where, {}))


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
            # a reply is checked whatever the checkout: it goes out under the owner's name anywhere
            self.assertIn(reply_phrase({"model": "claude-opus-5-5"}),
                          hook.reply_guard("gh issue comment 1 --repo o/r --body hi", tmp, event))


if __name__ == "__main__":
    unittest.main()
