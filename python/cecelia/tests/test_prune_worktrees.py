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

    def test_a_claude_session_working_in_it_wins_over_merged_and_clean(self):
        bucket, why = P.classify(_facts(sessions=[("cecelia-56", "idle")]))
        self.assertEqual(bucket, P.IN_USE)
        self.assertIn("session cecelia-56 (idle)", why)

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


def _remote(**kw) -> "P.RemoteFacts":
    base = dict(name="feat/x", sha="s1", age_days=30.0)
    base.update(kw)
    return P.RemoteFacts(**base)


class ClassifyRemoteTest(unittest.TestCase):
    def verdict(self, **kw):
        v = P.classify_remote(_remote(**kw))
        return v and v[0]

    def test_a_branch_in_main_is_gone(self):
        self.assertEqual(self.verdict(in_main=True), P.GONE)

    def test_a_pr_merged_or_closed_at_this_tip_is_gone(self):
        for state in ("MERGED", "CLOSED"):
            self.assertEqual(self.verdict(prs=[{"number": 1, "state": state, "headRefOid": "s1"}]), P.GONE)

    def test_commits_pushed_after_the_pr_are_stray_not_gone(self):
        bucket, why = P.classify_remote(_remote(prs=[{"number": 7, "state": "MERGED", "headRefOid": "s0"}]))
        self.assertEqual(bucket, P.STRAY)
        self.assertIn("after PR #7", why)

    def test_an_open_pr_keeps_it_off_the_list(self):
        prs = [{"number": 1, "state": "MERGED", "headRefOid": "s0"}, {"number": 2, "state": "OPEN", "headRefOid": "s1"}]
        self.assertIsNone(self.verdict(prs=prs))

    def test_no_pr_is_stray_only_once_old(self):
        self.assertEqual(self.verdict(age_days=30.0), P.STRAY)
        self.assertIsNone(self.verdict(age_days=1.0))

    def test_gh_failing_never_makes_a_branch_gone(self):
        self.assertEqual(self.verdict(prs=None), P.STRAY)

    def test_main_and_judge_assets_are_never_listed(self):
        for name in P.KEEP_REMOTE:
            self.assertIsNone(self.verdict(name=name, in_main=True))


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

    def test_a_session_counts_by_its_current_folder_not_a_longer_named_sibling(self):
        sep = os.sep
        S = P.Session
        sessions = [S(1, "a", "idle", f"{sep}ws{sep}cecelia-x{sep}frontend"),
                    S(2, "b", "busy", f"{sep}ws{sep}cecelia-x2"),
                    S(3, "c", "?", None, readable=False)]
        self.assertEqual(P.sessions_under(f"{sep}ws{sep}cecelia-x", sessions), [("a", "idle")])
        # The shell was reset to the workspace root; the worktree is still among its recent folders.
        reset = [S(4, "d", "idle", f"{sep}ws", recent_cwds=(f"{sep}ws", f"{sep}ws{sep}cecelia-x"))]
        self.assertEqual(P.sessions_under(f"{sep}ws{sep}cecelia-x", reset), [("d", "idle")])

    def test_summary_names_what_can_go_and_is_silent_otherwise(self):
        row = lambda b: (_facts(), b, "")   # noqa: E731
        self.assertIsNone(P.summary([row(P.IN_USE), row(P.UNMERGED)]))
        self.assertEqual(P.summary([row(P.SAFE)]),
                         "1 removable worktree — pixi run prune-worktrees --remove")
        self.assertEqual(P.summary([row(P.SAFE), row(P.SAFE), row(P.DEAD)]),
                         "2 removable, 1 dead worktrees — pixi run prune-worktrees --remove")
        unreadable = [P.Session(9, "9", "?", None, readable=False)]
        self.assertIn("couldn't read Claude session 9", P.summary([row(P.SAFE)], unreadable))


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

        self.sessions = []   # what `live_sessions` reports during `remove`'s re-check
        for name, value in (("merged_pr_heads", lambda *a, **k: []), ("scan_processes", lambda: []),
                            ("live_sessions", lambda: self.sessions)):
            patcher = mock.patch.object(P, name, value)
            patcher.start()
            self.addCleanup(patcher.stop)

    def buckets(self):
        return {pathlib.Path(f.path).name: b for f, b, _ in P.survey(str(self.primary), self.sessions)}

    def test_survey_then_remove(self):
        self.assertEqual(self.buckets(), {
            "cecelia-feijoa": P.PRIMARY, "cecelia-merged": P.SAFE, "cecelia-ahead": P.UNMERGED,
            "cecelia-dirty": P.DIRTY, "cecelia-gone": P.DEAD})

        log = P.remove(P.survey(str(self.primary), self.sessions), str(self.primary))

        self.assertFalse(self.merged.exists())
        self.assertTrue(self.ahead.exists() and self.dirty.exists())
        self.assertEqual(set(self.buckets()), {"cecelia-feijoa", "cecelia-ahead", "cecelia-dirty"})
        branches = subprocess.run(["git", "branch", "--format=%(refname:short)"], cwd=str(self.primary),
                                  capture_output=True, text=True, encoding="utf-8").stdout.split()
        self.assertNotIn("merged", branches)
        self.assertIn("ahead", branches)
        self.assertTrue(any("pruned 1" in line for line in log), log)

    def branches(self):
        return subprocess.run(["git", "branch", "--format=%(refname:short)"], cwd=str(self.primary),
                              capture_output=True, text=True, encoding="utf-8").stdout.split()

    def test_a_branch_merged_after_the_primary_checkout_last_pulled_is_deleted(self):
        # A PR branch is pushed with `-u`, so its upstream is its own remote branch, which GitHub
        # deletes on merge. `git branch -d` then judges "merged" against the primary checkout's own
        # `main`, which lags `origin/main` — so every branch merged since its last pull was kept.
        landed = self.primary.parent / "cecelia-landed"
        _git("worktree", "add", "-q", "-b", "landed", str(landed), "origin/main", cwd=self.primary)
        (landed / "h.txt").write_text("landed\n", encoding="utf-8")
        _git("add", "h.txt", cwd=landed)
        _git("commit", "-qm", "landed", cwd=landed)
        _git("push", "-q", "-u", "origin", "landed", cwd=landed)
        _git("push", "-q", "origin", "HEAD:main", cwd=landed)          # the PR merges
        _git("push", "-q", "origin", ":landed", cwd=landed)            # and its branch is deleted
        _git("fetch", "-q", "--prune", "origin", cwd=self.primary)   # origin/main moves; the primary's main doesn't
        os.utime(landed / ".git", (0, 0))
        self.assertEqual(self.buckets()["cecelia-landed"], P.SAFE)

        log = P.remove(P.survey(str(self.primary), self.sessions), str(self.primary))

        self.assertFalse(landed.exists())
        self.assertNotIn("landed", self.branches(), log)

    def test_a_squash_merged_branch_is_deleted_at_its_pr_head(self):
        (self.ahead / "g.txt").write_text("unmerged, but its PR merged at this head\n", encoding="utf-8")
        head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(self.ahead), capture_output=True,
                              text=True, encoding="utf-8").stdout.strip()
        (self.ahead / "g.txt").unlink()
        _git("checkout", "-q", "--", ".", cwd=self.ahead)
        with mock.patch.object(P, "merged_pr_heads", lambda *a, **k: [{"headRefOid": head}]):
            self.assertEqual(self.buckets()["cecelia-ahead"], P.SAFE)
            P.remove(P.survey(str(self.primary), self.sessions), str(self.primary))
        self.assertNotIn("ahead", self.branches())

    def test_a_branch_is_kept_when_it_moved_or_is_checked_out_elsewhere(self):
        head = subprocess.run(["git", "rev-parse", "merged"], cwd=str(self.primary), capture_output=True,
                              text=True, encoding="utf-8").stdout.strip()
        self.assertIn("checked out in another worktree", P.delete_branch("merged", head, str(self.primary)))
        _git("worktree", "remove", str(self.merged), cwd=self.primary)
        self.assertIn("moved", P.delete_branch("merged", "1" * 40, str(self.primary)))
        self.assertIn("kept", P.delete_branch("merged", "0" * 40, str(self.primary)))   # zeros skip git's check
        self.assertIn("merged", self.branches())
        self.assertEqual(P.delete_branch("merged", head, str(self.primary)), " + branch deleted")

    def test_a_worktree_that_changed_since_the_survey_is_skipped(self):
        rows = P.survey(str(self.primary), self.sessions)
        (self.merged / "late.txt").write_text("written after the survey\n", encoding="utf-8")
        log = P.remove(rows, str(self.primary))
        self.assertTrue(self.merged.exists())
        self.assertTrue(any("skipped" in line and "dirty" in line for line in log), log)

    def test_a_session_in_a_merged_worktree_keeps_it(self):
        self.sessions = [P.Session(7, "cecelia-56", "idle", str(self.merged))]
        self.assertEqual(self.buckets()["cecelia-merged"], P.IN_USE)

    def test_a_session_that_moved_in_after_the_survey_stops_the_removal(self):
        rows = P.survey(str(self.primary), [])
        self.sessions = [P.Session(7, "cecelia-56", "busy", str(self.merged / "sub"))]
        log = P.remove(rows, str(self.primary))
        self.assertTrue(self.merged.exists())
        self.assertTrue(any("skipped" in line and "session cecelia-56" in line for line in log), log)


class RemoteEndToEndTest(unittest.TestCase):
    """A real bare origin: GONE branches are deleted, STRAY ones and moved ones are left."""

    def setUp(self):
        self.tmp = pathlib.Path(tempfile.mkdtemp()).resolve()
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)
        self.origin, self.primary = self.tmp / "origin.git", self.tmp / "ws" / "cecelia-feijoa"
        _git("init", "-q", "--bare", "-b", "main", str(self.origin), cwd=self.tmp)
        _git("clone", "-q", str(self.origin), str(self.primary), cwd=self.tmp)
        self.commit("f.txt", "one")
        _git("push", "-q", "origin", "HEAD:main", "HEAD:in-main", "HEAD:judge-assets", cwd=self.primary)
        self.commit("g.txt", "ahead")
        _git("push", "-q", "origin", "HEAD:squashed", "HEAD:no-pr", cwd=self.primary)
        _git("fetch", "-q", "origin", cwd=self.primary)
        self.squashed_tip = self.rev("origin/squashed")
        prs = {"squashed": [{"number": 5, "state": "MERGED", "headRefOid": self.squashed_tip}], "no-pr": []}
        patcher = mock.patch.object(P, "merged_pr_heads", lambda b, **k: prs.get(b, []))
        patcher.start()
        self.addCleanup(patcher.stop)

    def commit(self, name, text):
        (self.primary / name).write_text(text + "\n", encoding="utf-8")
        _git("add", name, cwd=self.primary)
        _git("commit", "-qm", text, cwd=self.primary)

    def rev(self, ref):
        return subprocess.run(["git", "rev-parse", ref], cwd=str(self.primary), capture_output=True,
                              text=True, encoding="utf-8").stdout.strip()

    def origin_branches(self):
        return set(subprocess.run(["git", "branch", "--format=%(refname:short)"], cwd=str(self.origin),
                                  capture_output=True, text=True, encoding="utf-8").stdout.split())

    def test_survey_then_remove(self):
        rows = P.survey_remote(str(self.primary), now=__import__("time").time())
        self.assertEqual({r.name: b for r, b, _ in rows},
                         {"in-main": P.GONE, "squashed": P.GONE})   # no-pr is under two weeks old
        log = P.remove_remote(rows, str(self.primary))
        self.assertEqual(self.origin_branches(), {"main", "judge-assets", "no-pr"}, log)

    def test_a_branch_pushed_to_since_the_survey_is_kept(self):
        rows = P.survey_remote(str(self.primary), now=__import__("time").time())
        self.commit("late.txt", "late")
        _git("push", "-q", "origin", "HEAD:squashed", cwd=self.primary)
        log = P.remove_remote(rows, str(self.primary))
        self.assertIn("squashed", self.origin_branches())
        self.assertNotIn("in-main", self.origin_branches())
        self.assertTrue(any("kept origin/squashed" in line for line in log), log)


if __name__ == "__main__":
    unittest.main()
