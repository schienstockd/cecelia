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


if __name__ == "__main__":
    unittest.main()
