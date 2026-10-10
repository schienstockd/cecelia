"""Tests for `scripts/prune_worktrees.py` — which worktrees `pixi run prune-worktrees` may remove.

The bucket rules are tested on made-up `Facts`. One end-to-end test builds a throwaway origin +
checkout + sibling worktrees in a temp dir, with `gh` and `/proc` replaced, so no test reaches the
real repo or GitHub.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load():
    spec = importlib.util.spec_from_file_location("prune_worktrees", _REPO / "scripts" / "prune_worktrees.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod   # dataclasses look their module up here
    spec.loader.exec_module(mod)
    return mod


P = _load()


def _facts(**kw) -> "P.Facts":
    base = dict(path="/ws/cecelia-x", branch="x", head="h1", in_main=True)
    base.update(kw)
    return P.Facts(**base)


class ClassifyTest(unittest.TestCase):
    def bucket(self, **kw):
        return P.classify(_facts(**kw))[0]

    def test_merged_clean_unused_is_safe(self):
        self.assertEqual(self.bucket(), P.SAFE)

    def test_a_squash_merged_branch_is_safe_when_its_pr_merged_at_this_head(self):
        self.assertEqual(self.bucket(in_main=False, merged_heads=["h1"]), P.SAFE)

    def test_commits_pushed_after_the_merge_are_unmerged(self):
        self.assertEqual(self.bucket(in_main=False, merged_heads=["h0"]), P.UNMERGED)

    def test_no_merged_pr_and_not_in_main_is_unmerged(self):
        self.assertEqual(self.bucket(in_main=False, merged_heads=[]), P.UNMERGED)

    def test_gh_failing_never_makes_a_branch_safe(self):
        bucket, why = P.classify(_facts(in_main=False, merged_heads=None))
        self.assertEqual(bucket, P.UNMERGED)
        self.assertIn("unknown", why)

    def test_a_running_process_wins_over_merged_and_clean(self):
        self.assertEqual(self.bucket(pids=[(42, "pixi run dev")]), P.IN_USE)

    def test_no_proc_means_in_use_not_safe(self):
        self.assertEqual(self.bucket(pids=None), P.IN_USE)

    def test_a_fresh_worktree_is_never_safe(self):
        self.assertEqual(self.bucket(age_seconds=60), P.FRESH)

    def test_a_stash_on_the_branch_blocks_removal(self):
        self.assertEqual(self.bucket(stashes=1), P.STASHED)

    def test_uncommitted_work_is_dirty(self):
        self.assertEqual(self.bucket(status=[" M app/src/x.jl"]), P.DIRTY)

    def test_only_compile_output_is_junk(self):
        self.assertEqual(self.bucket(status=["?? frontend/src/A.vue.js", "?? frontend/src/u.js"],
                                     tracked_ts={"frontend/src/u.ts"}), P.JUNK)

    def test_junk_mixed_with_real_work_is_dirty(self):
        self.assertEqual(self.bucket(status=["?? frontend/src/A.vue.js", "?? notes/draft.md"]), P.DIRTY)

    def test_primary_dead_locked_and_outside_are_never_safe(self):
        self.assertEqual(self.bucket(primary=True), P.PRIMARY)
        self.assertEqual(self.bucket(exists=False), P.DEAD)
        self.assertEqual(self.bucket(locked=True), P.LOCKED)
        self.assertEqual(self.bucket(managed=False), P.OTHER)


class HelpersTest(unittest.TestCase):
    def test_is_junk(self):
        ts = {"frontend/src/utils/a.ts"}
        self.assertTrue(P.is_junk("frontend/src/X.vue.js", ts))
        self.assertTrue(P.is_junk("frontend/src/utils/a.js", ts))
        self.assertTrue(P.is_junk("frontend/src/utils/a.d.ts", ts))
        self.assertFalse(P.is_junk("frontend/src/utils/b.js", ts))   # no .ts beside it: real file
        self.assertFalse(P.is_junk("scripts/x.vue.js", ts))          # outside frontend/

    def test_parse_worktree_list(self):
        text = ("worktree /ws/main\nHEAD aaa\nbranch refs/heads/main\n\n"
                "worktree /tmp/t/judge\nHEAD bbb\ndetached\nprunable gitdir file points to non-existent location\n\n"
                "worktree /ws/cecelia-l\nHEAD ccc\nbranch refs/heads/l\nlocked\n")
        rows = P.parse_worktree_list(text)
        # OS-native separators: git prints `D:/a/x` on Windows
        self.assertEqual([r["worktree"] for r in rows],
                         [os.path.normpath(p) for p in ("/ws/main", "/tmp/t/judge", "/ws/cecelia-l")])
        self.assertIn("prunable", rows[1])
        self.assertIn("locked", rows[2])

    def test_a_process_in_a_longer_named_sibling_does_not_count(self):
        sep = os.sep
        procs = [(1, "dev", [f"{sep}ws{sep}cecelia-x2"]), (2, "vite", [f"{sep}ws{sep}cecelia-x{sep}frontend"])]
        self.assertEqual(P.pids_under(f"{sep}ws{sep}cecelia-x", procs), [(2, "vite")])

    def test_unknown_processes_stay_unknown(self):
        self.assertIsNone(P.pids_under("/ws/cecelia-x", None))


def _git(*args, cwd):
    subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *args], cwd=str(cwd),
                   check=True, capture_output=True)


class EndToEndTest(unittest.TestCase):
    """A real throwaway repo: a merged sibling is removed, the rest are left alone."""

    def setUp(self):
        self.tmp = pathlib.Path(tempfile.mkdtemp()).resolve()
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)
        origin, ws = self.tmp / "origin.git", self.tmp / "ws"
        ws.mkdir()
        _git("init", "-q", "--bare", "-b", "main", str(origin), cwd=self.tmp)
        self.primary = ws / "cecelia-feijoa"
        _git("clone", "-q", str(origin), str(self.primary), cwd=self.tmp)
        (self.primary / "f.txt").write_text("one\n", encoding="utf-8")
        _git("add", "f.txt", cwd=self.primary)
        _git("commit", "-qm", "one", cwd=self.primary)
        _git("push", "-q", "origin", "HEAD:main", cwd=self.primary)
        _git("fetch", "-q", "origin", cwd=self.primary)

        def wt(name):
            path = ws / f"cecelia-{name}"
            _git("worktree", "add", "-q", "-b", name, str(path), "origin/main", cwd=self.primary)
            return path

        self.merged, self.ahead, self.dirty, self.gone = wt("merged"), wt("ahead"), wt("dirty"), wt("gone")
        (self.ahead / "g.txt").write_text("new\n", encoding="utf-8")
        _git("add", "g.txt", cwd=self.ahead)
        _git("commit", "-qm", "unmerged", cwd=self.ahead)
        (self.dirty / "f.txt").write_text("edited\n", encoding="utf-8")
        shutil.rmtree(self.gone)
        old = 0  # epoch: every worktree is well past the fresh window
        for p in (self.merged, self.ahead, self.dirty):
            os.utime(p / ".git", (old, old))

        for name, value in (("merged_pr_heads", lambda *a, **k: []), ("scan_processes", lambda: [])):
            patcher = mock.patch.object(P, name, value)
            patcher.start()
            self.addCleanup(patcher.stop)

    def buckets(self):
        return {pathlib.Path(f.path).name: b for f, b, _ in P.survey(str(self.primary))}

    def test_survey_then_remove(self):
        self.assertEqual(self.buckets(), {
            "cecelia-feijoa": P.PRIMARY, "cecelia-merged": P.SAFE, "cecelia-ahead": P.UNMERGED,
            "cecelia-dirty": P.DIRTY, "cecelia-gone": P.DEAD})

        log = P.remove(P.survey(str(self.primary)), str(self.primary))

        self.assertFalse(self.merged.exists())
        self.assertTrue(self.ahead.exists() and self.dirty.exists())
        self.assertEqual(set(self.buckets()), {"cecelia-feijoa", "cecelia-ahead", "cecelia-dirty"})
        branches = subprocess.run(["git", "branch", "--format=%(refname:short)"], cwd=str(self.primary),
                                  capture_output=True, text=True, encoding="utf-8").stdout.split()
        self.assertNotIn("merged", branches)
        self.assertIn("ahead", branches)
        self.assertTrue(any("pruned 1" in line for line in log), log)

    def test_a_worktree_that_changed_since_the_survey_is_skipped(self):
        rows = P.survey(str(self.primary))
        (self.merged / "late.txt").write_text("written after the survey\n", encoding="utf-8")
        log = P.remove(rows, str(self.primary))
        self.assertTrue(self.merged.exists())
        self.assertTrue(any("skipped" in line and "dirty" in line for line in log), log)


if __name__ == "__main__":
    unittest.main()
