"""Tests for `scripts/pre_push_check.py` — the pre-push hook that stops a merged PR's branch coming back.
The PR lookup is passed in, so no test reaches GitHub."""
from __future__ import annotations

import importlib.util
import pathlib
import sys
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_spec = importlib.util.spec_from_file_location("pre_push_check", _REPO / "scripts" / "pre_push_check.py")
H = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = H
_spec.loader.exec_module(H)

NEW, OLD, ZERO = "a" * 40, "b" * 40, "0" * 40


def push(branch, sha=NEW):
    return [f"refs/heads/local {sha} refs/heads/{branch} {ZERO}"]


def lookup(**by_branch):
    return lambda b: by_branch.get(b.replace("/", "_"), [])


class RefusalsTest(unittest.TestCase):
    def test_new_commits_on_a_merged_prs_branch_are_refused(self):
        msgs = H.refusals(push("feat/x"), lookup(feat_x=[{"number": 9, "state": "MERGED", "headRefOid": OLD}]))
        self.assertEqual(len(msgs), 1)
        self.assertIn("PR #9", msgs[0])
        self.assertIn("merged", msgs[0])

    def test_a_closed_pr_counts_too(self):
        self.assertTrue(H.refusals(push("x"), lookup(x=[{"number": 1, "state": "CLOSED", "headRefOid": OLD}])))

    def test_open_pr_no_pr_and_gh_failing_all_pass(self):
        open_ = [{"number": 1, "state": "MERGED", "headRefOid": OLD}, {"number": 2, "state": "OPEN", "headRefOid": OLD}]
        self.assertEqual(H.refusals(push("x"), lookup(x=open_)), [])
        self.assertEqual(H.refusals(push("x"), lookup()), [])
        self.assertEqual(H.refusals(push("x"), lambda b: None), [])

    def test_re_pushing_the_merged_head_passes(self):
        self.assertEqual(H.refusals(push("x"), lookup(x=[{"number": 1, "state": "MERGED", "headRefOid": NEW}])), [])

    def test_deletes_and_tags_are_not_looked_up(self):
        def boom(_):
            raise AssertionError("looked up")
        lines = [f"(delete) {ZERO} refs/heads/x {OLD}", f"refs/tags/v1 {NEW} refs/tags/v1 {ZERO}"]
        self.assertEqual(H.refusals(lines, boom), [])


if __name__ == "__main__":
    unittest.main()
