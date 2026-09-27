"""Tests for `cecelia.effectiveness.git_context`.

Small module — one caveat: every helper here is best-effort, returns None on any failure, and
must never raise. The tests pin the "returns None on failure" contract because the callers
(recital + commit hook) rely on it to degrade gracefully rather than block.
"""
from __future__ import annotations

import subprocess
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


if __name__ == "__main__":
    unittest.main()
