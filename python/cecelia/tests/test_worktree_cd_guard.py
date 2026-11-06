"""The PreToolUse guard that stops a session `cd`-ing into another live session's worktree."""
from __future__ import annotations

import io
import json
import pathlib
import subprocess
import sys
import tempfile
import unittest

from cecelia.effectiveness.claude_sessions import Session
from cecelia.effectiveness.worktree_cd_guard import blocking_reason, main, top_level_cd_targets

_REPO = pathlib.Path(__file__).resolve().parents[3]


class TopLevelCdTargets(unittest.TestCase):
    def test_finds_cd_in_command_position(self):
        self.assertEqual(top_level_cd_targets("cd ../x && git log"), ["../x"])
        self.assertEqual(top_level_cd_targets("ls; cd /a/b; cd c"), ["/a/b", "c"])
        self.assertEqual(top_level_cd_targets("ls\ncd /a"), ["/a"])
        self.assertEqual(top_level_cd_targets("cd -- /a"), ["/a"])

    def test_ignores_cds_that_cannot_move_the_shell(self):
        self.assertEqual(top_level_cd_targets("(cd /a && git log)"), [])
        self.assertEqual(top_level_cd_targets("bash -c 'cd /a && ls'"), [])
        self.assertEqual(top_level_cd_targets("cd /a | cat"), [])
        self.assertEqual(top_level_cd_targets("echo cd /a"), [])
        self.assertEqual(top_level_cd_targets("git -C /a log"), [])
        self.assertEqual(top_level_cd_targets("cd"), [])
        self.assertEqual(top_level_cd_targets("cd -"), [])

    def test_unbalanced_quotes_raise(self):
        with self.assertRaises(ValueError):
            top_level_cd_targets("cd 'oops")


class BlockingReason(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        root = pathlib.Path(self._tmp.name).resolve()
        self.mine, self.theirs, self.free = root / "mine", root / "theirs", root / "free"
        for d in (self.mine, self.theirs, self.free):
            (d / "sub").mkdir(parents=True)
            (d / ".git").write_text("gitdir: elsewhere\n", encoding="utf-8")   # a worktree's .git file
        self.workspace = root
        self.sessions = [Session(1, "me", "busy", str(self.mine), session_id="me-id"),
                         Session(2, "cecelia-7", "idle", str(self.theirs / "sub"), session_id="other-id")]

    def tearDown(self):
        self._tmp.cleanup()

    def reason(self, command, cwd=None):
        return blocking_reason(command, str(cwd or self.mine), "me-id", sessions=lambda: self.sessions)

    def test_blocks_cd_into_another_sessions_worktree(self):
        reason = self.reason(f"cd {self.theirs.as_posix()} && git log")
        self.assertIsNotNone(reason)
        self.assertIn("cecelia-7", reason)
        self.assertIn("git -C", reason)

    def test_blocks_relative_cd_from_a_non_checkout_folder(self):
        self.assertIsNotNone(self.reason("cd theirs/sub", cwd=self.workspace))

    def test_blocks_a_worktree_whose_session_shell_was_reset(self):
        """Its newest folder is the workspace root, but it was working in `theirs` a moment ago."""
        self.sessions = [Session(2, "cecelia-7", "idle", str(self.workspace), session_id="other-id",
                                 recent_cwds=(str(self.workspace), str(self.theirs)))]
        self.assertIsNotNone(self.reason(f"cd {self.theirs.as_posix()}"))

    def test_allows_cd_within_own_checkout_and_into_a_free_worktree(self):
        self.assertIsNone(self.reason(f"cd {self.mine.as_posix()}/sub"))
        self.assertIsNone(self.reason(f"cd {self.free.as_posix()} && pixi run test-py"))
        self.assertIsNone(self.reason(f"cd {self.workspace.as_posix()}"))

    def test_allows_peeks_that_do_not_move_the_session(self):
        self.assertIsNone(self.reason(f"(cd {self.theirs.as_posix()} && git log)"))
        self.assertIsNone(self.reason(f"git -C {self.theirs.as_posix()} log"))

    def test_own_session_in_the_target_does_not_block(self):
        self.sessions = [Session(1, "me", "busy", str(self.theirs), session_id="me-id")]
        self.assertIsNone(self.reason(f"cd {self.theirs.as_posix()}"))

    def test_no_session_lookup_without_a_cross_checkout_cd(self):
        def boom():
            raise AssertionError("live_sessions must not be read")
        self.assertIsNone(blocking_reason("ls && git status", str(self.mine), "me-id", sessions=boom))


class HookEntryPoint(unittest.TestCase):
    def test_non_bash_and_garbage_input_allow(self):
        self.assertEqual(main(io.StringIO(json.dumps({"tool_name": "Read"})), io.StringIO()), 0)
        self.assertEqual(main(io.StringIO("not json"), io.StringIO()), 0)

    def test_script_runs_under_a_bare_python(self):
        """Wired in the global settings for every session, it must run without the pixi env."""
        payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": "ls"}, "cwd": "/",
                              "session_id": "x"})
        proc = subprocess.run([sys.executable, "-I", str(_REPO / "scripts" / "worktree_cd_guard.py")],
                              input=payload, capture_output=True, text=True, encoding="utf-8", timeout=30)
        self.assertEqual(proc.returncode, 0, proc.stderr)


if __name__ == "__main__":
    unittest.main()
