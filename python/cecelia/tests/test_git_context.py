"""Tests for `cecelia.effectiveness.git_context`.

Small module — one caveat: every helper here is best-effort, returns None on any failure, and
must never raise. The tests pin the "returns None on failure" contract because the callers
(recital + commit hook) rely on it to degrade gracefully rather than block.
"""
from __future__ import annotations

import os
import pathlib
import shlex
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import git_context


def _completed(stdout: str, returncode: int = 0) -> subprocess.CompletedProcess:
    return subprocess.CompletedProcess(args=[], returncode=returncode, stdout=stdout, stderr="")


class CurrentHeadShaTest(unittest.TestCase):
    def test_returns_full_sha_on_success(self):
        sha = "0123456789abcdef0123456789abcdef01234567"
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run", return_value=_completed(sha + "\n")):
            self.assertEqual(git_context.current_head_sha(), sha)

    def test_returns_none_when_git_missing(self):
        with mock.patch.object(git_context.shutil, "which", return_value=None):
            self.assertIsNone(git_context.current_head_sha())

    def test_returns_none_on_nonzero_returncode(self):
        # Empty repo / not a repo — git exits non-zero. Hook degrades to allow.
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run", return_value=_completed("", returncode=128)):
            self.assertIsNone(git_context.current_head_sha())

    def test_returns_none_on_garbage_output(self):
        # A short/truncated SHA doesn't match the 40-hex regex — safer to return None than a
        # partial identifier the hook would then match against.
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run", return_value=_completed("nope\n")):
            self.assertIsNone(git_context.current_head_sha())

    def test_returns_none_on_timeout(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(
                 git_context.subprocess, "run",
                 side_effect=subprocess.TimeoutExpired(cmd="git", timeout=10.0),
             ):
            self.assertIsNone(git_context.current_head_sha())


class PrForBranchTest(unittest.TestCase):
    def test_returns_pr_number_on_success(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/gh"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("1264\n")):
            self.assertEqual(git_context.pr_for_branch("docs/x"), "#1264")

    def test_returns_none_when_gh_missing(self):
        with mock.patch.object(git_context.shutil, "which", return_value=None):
            self.assertIsNone(git_context.pr_for_branch("docs/x"))

    def test_returns_none_on_nonzero_returncode(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/gh"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("", returncode=1)):
            self.assertIsNone(git_context.pr_for_branch("docs/x"))

    def test_returns_none_on_empty_output(self):
        # No PR was ever opened for this branch — gh returns empty stdout, no error.
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/gh"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("")):
            self.assertIsNone(git_context.pr_for_branch("docs/x"))

    def test_returns_none_on_timeout(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/gh"), \
             mock.patch.object(
                 git_context.subprocess, "run",
                 side_effect=subprocess.TimeoutExpired(cmd="gh", timeout=10.0),
             ):
            self.assertIsNone(git_context.pr_for_branch("docs/x"))


class CurrentBranchTest(unittest.TestCase):
    def test_returns_branch_name_on_success(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("feat/log-branch-capture\n")):
            self.assertEqual(git_context.current_branch(), "feat/log-branch-capture")

    def test_returns_none_when_git_missing(self):
        with mock.patch.object(git_context.shutil, "which", return_value=None):
            self.assertIsNone(git_context.current_branch())

    def test_detached_head_returns_none(self):
        # `git rev-parse --abbrev-ref HEAD` returns the literal string `HEAD` in detached-HEAD
        # state. That's not a branch we can join a PR to later, so treat as no-branch.
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("HEAD\n")):
            self.assertIsNone(git_context.current_branch())

    def test_returns_none_on_nonzero_returncode(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(git_context.subprocess, "run",
                               return_value=_completed("", returncode=128)):
            self.assertIsNone(git_context.current_branch())

    def test_returns_none_on_timeout(self):
        with mock.patch.object(git_context.shutil, "which", return_value="/usr/bin/git"), \
             mock.patch.object(
                 git_context.subprocess, "run",
                 side_effect=subprocess.TimeoutExpired(cmd="git", timeout=10.0),
             ):
            self.assertIsNone(git_context.current_branch())


class InSequencerCommitTest(unittest.TestCase):
    """A real throwaway repo: the merge state is a ref git writes, so mocking git would test nothing."""

    def _repo(self, d: str) -> None:
        def git(*args):
            subprocess.run(["git", *args], cwd=d, check=True, capture_output=True, encoding="utf-8",
                           env={**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t",
                                "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@t"})
        git("init", "-q", "-b", "main")
        (pathlib.Path(d) / "a.txt").write_text("base\n", encoding="utf-8")
        git("add", "a.txt")
        git("commit", "-q", "-m", "base")
        git("checkout", "-q", "-b", "side")
        (pathlib.Path(d) / "b.txt").write_text("side\n", encoding="utf-8")
        git("add", "b.txt")
        git("commit", "-q", "-m", "side")
        git("checkout", "-q", "main")
        self.git = git

    def test_a_merge_in_progress_is_a_sequencer_commit_and_a_plain_one_is_not(self):
        with tempfile.TemporaryDirectory() as d:
            self._repo(d)
            self.assertFalse(git_context.in_sequencer_commit(cwd=d))
            self.git("merge", "--no-commit", "--no-ff", "side")
            self.assertTrue(git_context.in_sequencer_commit(cwd=d))

    def _sh(self, *words) -> str:
        """`words` as one `sh` command line — git runs hooks and editors through `sh`, Git Bash's
        on Windows, which takes `C:/x` paths."""
        return " ".join(shlex.quote(pathlib.Path(w).as_posix() if os.sep in w else w) for w in words)

    def _record_in_hook(self, d: str) -> pathlib.Path:
        """Install a `commit-msg` hook that appends what `in_sequencer_commit` says, one line per
        commit it sees — the hook's environment is where git marks a rebase's own commits."""
        out = pathlib.Path(d) / "seen.txt"
        hook = pathlib.Path(d) / ".git" / "hooks" / "commit-msg"
        python_dir = str(pathlib.Path(git_context.__file__).resolve().parents[2])
        probe = ("import sys; sys.path.insert(0, sys.argv[1]); "
                 "from cecelia.effectiveness.git_context import in_sequencer_commit as f; print(f())")
        hook.write_text(f"#!/bin/sh\n{self._sh(sys.executable, '-c', probe, python_dir)} >> "
                        f"{self._sh(str(out))}\n", encoding="utf-8", newline="\n")
        hook.chmod(0o755)
        self.git("config", "core.hooksPath", hook.parent.as_posix())
        return out

    def _rebase_first(self, d: str, verb: str) -> None:
        """`git rebase -i HEAD~1` with its one `pick` turned into `verb`."""
        script = pathlib.Path(d) / "todo_editor.py"
        script.write_text("import sys, pathlib\np = pathlib.Path(sys.argv[1])\n"
                          f"p.write_text(p.read_text().replace('pick', {verb!r}, 1))\n", encoding="utf-8")
        editors = {"GIT_SEQUENCE_EDITOR": self._sh(sys.executable, str(script)), "GIT_EDITOR": "true"}
        with mock.patch.dict(os.environ, editors):
            self.git("rebase", "-q", "-i", "HEAD~1")

    def test_a_rebase_reword_is_a_sequencer_commit(self):
        with tempfile.TemporaryDirectory() as d:
            self._repo(d)
            self.git("merge", "-q", "--ff-only", "side")
            seen = self._record_in_hook(d)
            self._rebase_first(d, "reword")
            self.assertEqual(seen.read_text(encoding="utf-8").split(), ["True"])

    def test_an_own_commit_at_a_rebase_edit_stop_is_not(self):
        with tempfile.TemporaryDirectory() as d:
            self._repo(d)
            self.git("merge", "-q", "--ff-only", "side")
            seen = self._record_in_hook(d)
            self._rebase_first(d, "edit")
            (pathlib.Path(d) / "c.txt").write_text("own\n", encoding="utf-8")
            self.git("add", "c.txt")
            self.git("commit", "-q", "-m", "own")
            self.git("rebase", "--continue")
            self.assertEqual(seen.read_text(encoding="utf-8").split(), ["False"])

    def test_not_a_repo_is_not_a_sequencer_commit(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertFalse(git_context.in_sequencer_commit(cwd=d))


if __name__ == "__main__":
    unittest.main()
