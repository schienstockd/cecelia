"""Tests for the CLAUDE.md compliance eval runner.

Design: `docs/todo/CLAUDE_MD_EVAL_PLAN.md`. The runner spawns `claude -p` in fresh detached
worktrees — expensive and non-deterministic, so tests inject a fake `claude_runner` and stub
the worktree-creation seams. That lets us pin the two invariants an eval readout depends on:

- **Scoring rules produce a deterministic outcome.** Same diff → same (outcome, hits) row.
  If this stops being true, no cross-run comparison in the rollup is trustworthy.
- **Every spawn emits exactly one `claude_md_eval_run` row.** No silent skips — the whole
  point of moving eval discipline into structured logging is that a missing row is a bug, not
  a "reviewer went dark" case. Plus one `claude_md_eval_pass` summary row per invocation.
- **A timed-out spawn emits an `error` row, not a crash.** One bad prompt or one hung agent
  must not wedge the whole pass.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import subprocess
import tempfile
import textwrap
import unittest
from unittest import mock

from cecelia.effectiveness import EVENT_TYPES, read_events

_REPO = pathlib.Path(__file__).resolve().parents[3]
_RUNNER_PATH = _REPO / "scripts" / "claude_md_eval" / "run_prompt.py"


def _load_runner():
    spec = importlib.util.spec_from_file_location("run_prompt", _RUNNER_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _write_prompt(dest: pathlib.Path, *, prompt_id: str, rule: str,
                  compliant: str, anti: str, body: str) -> pathlib.Path:
    path = dest / f"{prompt_id}.md"
    path.write_text(textwrap.dedent(f"""\
        ---
        id: {prompt_id}
        rule: {rule}
        compliant_signal: '{compliant}'
        anti_signal: '{anti}'
        ---
        {body}
        """), encoding="utf-8")
    return path


class PromptParseTest(unittest.TestCase):
    def setUp(self):
        self.runner = _load_runner()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmpdir = pathlib.Path(self._tmp.name)

    def test_happy_path_parses_frontmatter_and_body(self):
        p = _write_prompt(self.tmpdir, prompt_id="x", rule="R", compliant="A", anti="B",
                          body="do the thing")
        meta, body = self.runner.parse_prompt(p)
        self.assertEqual(meta["id"], "x")
        self.assertEqual(meta["rule"], "R")
        self.assertEqual(meta["compliant_signal"], "A")
        self.assertEqual(meta["anti_signal"], "B")
        self.assertEqual(body, "do the thing")

    def test_missing_frontmatter_raises(self):
        p = self.tmpdir / "no-fm.md"
        p.write_text("just a body, no delimiter", encoding="utf-8")
        with self.assertRaises(self.runner.PromptParseError):
            self.runner.parse_prompt(p)

    def test_missing_required_key_raises(self):
        # `id` and `rule` are the only always-required keys. Graders (`compliant_signal` /
        # `anti_signal` regex OR `tool_order_before_tool` / `tool_order_after_tool`) are
        # optional individually but the prompt MUST declare at least one — see the D-additions
        # in docs/todo/CLAUDE_MD_EVAL_PLAN.md (2026-09-28).
        p = self.tmpdir / "no-rule.md"
        p.write_text("---\nid: x\ncompliant_signal: A\nanti_signal: B\n---\nbody\n",
                     encoding="utf-8")
        with self.assertRaises(self.runner.PromptParseError) as cm:
            self.runner.parse_prompt(p)
        self.assertIn("rule", str(cm.exception))

    def test_prompt_with_no_grader_raises(self):
        # Neither regex signals nor tool_order fields — nothing to score against.
        p = self.tmpdir / "no-grader.md"
        p.write_text("---\nid: x\nrule: R\n---\nbody\n", encoding="utf-8")
        with self.assertRaises(self.runner.PromptParseError) as cm:
            self.runner.parse_prompt(p)
        self.assertIn("grader", str(cm.exception))

    def test_tool_order_only_prompt_parses(self):
        # Discovery-first is pure tool-order — no regex signals. Must parse cleanly.
        p = self.tmpdir / "tool-order-only.md"
        p.write_text(
            "---\nid: x\nrule: R\ntool_order_before_tool: Grep\n"
            "tool_order_before_arg_match: inventory\ntool_order_after_tool: Write\n---\nbody\n",
            encoding="utf-8",
        )
        meta, _ = self.runner.parse_prompt(p)
        self.assertEqual(meta["tool_order_before_tool"], "Grep")
        self.assertEqual(meta["tool_order_after_tool"], "Write")

    def test_comment_lines_in_frontmatter_are_skipped(self):
        p = self.tmpdir / "commented.md"
        p.write_text(
            "---\nid: x\n# a comment\nrule: R\ncompliant_signal: A\nanti_signal: B\n---\nbody\n",
            encoding="utf-8",
        )
        meta, _ = self.runner.parse_prompt(p)
        self.assertEqual(meta["rule"], "R")


class ScoreDiffTest(unittest.TestCase):
    def setUp(self):
        self.runner = _load_runner()
        self.meta = {"compliant_signal": r"LabelPropsView\(",
                     "anti_signal": r"\.read_h5ad\("}

    def test_compliant_only_is_compliant(self):
        diff = "+ view = LabelPropsView(path).filter_by_label(ids)\n"
        outcome, c, a = self.runner.score_diff(diff, self.meta)
        self.assertEqual(outcome, "compliant")
        self.assertEqual((c, a), (1, 0))

    def test_anti_signal_present_is_noncompliant_even_if_compliant_also_present(self):
        # An agent that wraps `.read_h5ad(...)` in a helper AND uses `LabelPropsView` still
        # violated the rule — the ratchet would catch the bypass. Same call here.
        diff = "+ adata = anndata.read_h5ad(path)\n+ v = LabelPropsView(path)\n"
        outcome, c, a = self.runner.score_diff(diff, self.meta)
        self.assertEqual(outcome, "noncompliant")
        self.assertGreaterEqual(a, 1)

    def test_neither_signal_present_is_noncompliant_with_zero_hits(self):
        diff = "+ pass\n"
        outcome, c, a = self.runner.score_diff(diff, self.meta)
        self.assertEqual(outcome, "noncompliant")
        self.assertEqual((c, a), (0, 0))

    def test_signals_on_deletion_lines_are_not_counted(self):
        # Real-world artefact: arm=without strips CLAUDE.md before the spawn, so the
        # diff includes `- ... LabelPropsView(...) ...` deletion lines from CLAUDE.md
        # prose. Without additions-only filtering the compliant_signal fires on those
        # deletions and reports inflated hits for a run where the agent wrote nothing
        # compliant. Surfaced by the `crop-failure` without-arm pilot (PR #1274 second
        # comment) — WITHOUT-arm reported compliant_hits=7 with zero real refs.
        diff = (
            "--- a/CLAUDE.md\n"
            "+++ /dev/null\n"
            "-Use `LabelPropsView(...)` — never `.read_h5ad(...)`.\n"
            "-The `LabelPropsView` view family covers reads AND writes.\n"
            "--- /dev/null\n"
            "+++ b/agent_output.py\n"
            "+import anndata\n"
            "+data = anndata.read_h5ad(path)\n"
        )
        outcome, c, a = self.runner.score_diff(diff, self.meta)
        # Only the agent's `read_h5ad` addition should register — the `LabelPropsView`
        # references in the deleted CLAUDE.md prose are ignored.
        self.assertEqual(c, 0)
        self.assertEqual(a, 1)
        self.assertEqual(outcome, "noncompliant")

    def test_diff_file_header_plus_lines_are_not_counted(self):
        # `+++ b/path` file header lines start with `+` but aren't content — additions-
        # only filter must skip them. Otherwise a filename containing the signal token
        # (unlikely but plausible: a file called `label_props_view.py`) would count.
        diff = (
            "--- /dev/null\n"
            "+++ b/label_props_view/LabelPropsView_helper.py\n"
            "+pass\n"
        )
        _outcome, c, a = self.runner.score_diff(diff, self.meta)
        self.assertEqual((c, a), (0, 0))


class RunOnePromptTest(unittest.TestCase):
    """End-to-end with a fake `claude_runner` + stubbed worktree/diff seams.

    The runner's public surface is `run_one_prompt`. Everything below the (worktree_root,
    claude_runner) seam is real: prompt parsing, scoring, log emission, per-run + summary
    rows. The seams stubbed out are (a) worktree creation (would touch the real git tree,
    slow), (b) diff capture (needs a real git repo). Both are stubbed via monkeypatch.
    """

    def setUp(self):
        self.runner = _load_runner()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmpdir = pathlib.Path(self._tmp.name)
        # Redirect the effectiveness log to a scratch path so this test doesn't touch the
        # user's real ~/.cecelia-effectiveness/events.jsonl.
        self.log_path = self.tmpdir / "events.jsonl"
        self._env_patch = mock.patch.dict(
            os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)},
        )
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        # Point _PROMPTS_DIR at a scratch dir so we control the prompt file.
        self._prompts_patch = mock.patch.object(self.runner, "_PROMPTS_DIR", self.tmpdir)
        self._prompts_patch.start()
        self.addCleanup(self._prompts_patch.stop)
        # Stub worktree creation — return a subdir of tmp. Stub removal.
        self._wt_seq = 0
        def fake_make(primary_repo, wt_root, prompt_id, *, arm="with"):
            # `arm` kwarg added 2026-09-28 (ablation via worktree cleanup — D-additions in
            # docs/todo/CLAUDE_MD_EVAL_PLAN.md). Accepted here but ignored: tests never
            # populate the fake worktree with a CLAUDE.md, so there's nothing to strip.
            self._wt_seq += 1
            dest = self.tmpdir / f"wt-{self._wt_seq}"
            dest.mkdir()
            return dest
        self._make_patch = mock.patch.object(self.runner, "_make_detached_worktree",
                                             side_effect=fake_make)
        self._make_patch.start()
        self.addCleanup(self._make_patch.stop)
        self._rm_patch = mock.patch.object(self.runner, "_remove_worktree", return_value=None)
        self._rm_patch.start()
        self.addCleanup(self._rm_patch.stop)
        # `commit=` on the row is the CLAUDE.md blob SHA; stub that to a fixed value so the
        # end-to-end assertion is deterministic.
        self._blob_patch = mock.patch.object(self.runner, "claude_md_blob_sha",
                                             return_value="a" * 40)
        self._blob_patch.start()
        self.addCleanup(self._blob_patch.stop)
        # `branch=` on the row is the invoker's git branch at pixi-run time; stub to a fixed
        # value so tests don't spawn a real `git rev-parse` and don't depend on which branch
        # the test process was invoked from.
        self._branch_patch = mock.patch.object(self.runner, "_current_branch",
                                               return_value="test-branch")
        self._branch_patch.start()
        self.addCleanup(self._branch_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def _write_default_prompt(self):
        return _write_prompt(self.tmpdir, prompt_id="h5ad-read", rule="R",
                             compliant=r"LabelPropsView\(", anti=r"\.read_h5ad\(",
                             body="do it")

    def test_event_types_include_the_two_eval_events(self):
        # If the closed vocabulary drops these names, every eval row will crash on append.
        self.assertIn("claude_md_eval_run", EVENT_TYPES)
        self.assertIn("claude_md_eval_pass", EVENT_TYPES)

    def test_compliant_run_emits_one_run_row_and_one_pass_row(self):
        self._write_default_prompt()
        def compliant_runner(worktree, prompt_body, *, timeout, claude_path, sandbox_log=None):
            (worktree / "diff-marker").write_text("+ LabelPropsView(path)\n", encoding="utf-8")
            return subprocess.CompletedProcess(args=[], returncode=0, stdout="", stderr="")
        with mock.patch.object(self.runner, "_capture_diff",
                               return_value="+ view = LabelPropsView(path)\n"):
            rows = self.runner.run_one_prompt(
                "h5ad-read", runs=1, timeout=60, claude_path="claude-fake",
                worktree_root=self.tmpdir, keep_worktrees=False,
                claude_runner=compliant_runner, primary_repo=self.tmpdir,
            )
        events = self._events()
        self.assertEqual(len(events), 2, f"expected 2 rows (1 run + 1 pass), got {len(events)}")
        run_row = next(e for e in events if e["event"] == "claude_md_eval_run")
        pass_row = next(e for e in events if e["event"] == "claude_md_eval_pass")
        self.assertEqual(run_row["payload"]["verdict"], "compliant")
        self.assertEqual(run_row["payload"]["compliant_hits"], 1)
        self.assertEqual(run_row["payload"]["anti_hits"], 0)
        self.assertEqual(run_row["commit"], "a" * 40)
        # Every eval row carries the invoker's branch — the rollup joins branch → PR at
        # render time so trend annotations survive even when `pr` is null at write time.
        self.assertEqual(run_row["branch"], "test-branch")
        self.assertEqual(pass_row["branch"], "test-branch")
        self.assertEqual(pass_row["payload"]["compliant"], 1)
        self.assertEqual(pass_row["payload"]["noncompliant"], 0)
        self.assertEqual(pass_row["payload"]["error"], 0)
        # Returned rows are the run rows only (not the summary).
        self.assertEqual(len(rows), 1)

    def test_noncompliant_run_is_scored_noncompliant(self):
        self._write_default_prompt()
        def anti_runner(worktree, prompt_body, *, timeout, claude_path, sandbox_log=None):
            return subprocess.CompletedProcess(args=[], returncode=0, stdout="", stderr="")
        with mock.patch.object(self.runner, "_capture_diff",
                               return_value="+ adata = anndata.read_h5ad(path)\n"):
            self.runner.run_one_prompt(
                "h5ad-read", runs=1, timeout=60, claude_path="claude-fake",
                worktree_root=self.tmpdir, keep_worktrees=False,
                claude_runner=anti_runner, primary_repo=self.tmpdir,
            )
        run_row = next(e for e in self._events() if e["event"] == "claude_md_eval_run")
        self.assertEqual(run_row["payload"]["verdict"], "noncompliant")
        self.assertEqual(run_row["payload"]["anti_hits"], 1)

    def test_missing_claude_bin_raises_clear_error_and_emits_error_row(self):
        # `default_claude_runner` with empty claude_path must NOT silently fall back to a bare
        # "claude" string (Windows-compat pitfall from convention finding conv-97a704d6 on this
        # PR). The runner catches the raise and records an `error` row so the pass continues.
        self._write_default_prompt()
        real_runner = self.runner.default_claude_runner
        with mock.patch.object(self.runner, "_capture_diff", return_value=""):
            self.runner.run_one_prompt(
                "h5ad-read", runs=1, timeout=60, claude_path=None,
                worktree_root=self.tmpdir, keep_worktrees=False,
                claude_runner=real_runner, primary_repo=self.tmpdir,
            )
        run_row = next(e for e in self._events() if e["event"] == "claude_md_eval_run")
        self.assertEqual(run_row["payload"]["verdict"], "error")
        self.assertIn("no `claude` binary", run_row["payload"]["error"])

    def test_timeout_emits_error_row_not_crash(self):
        self._write_default_prompt()
        def timeout_runner(worktree, prompt_body, *, timeout, claude_path, sandbox_log=None):
            raise subprocess.TimeoutExpired(cmd="claude", timeout=timeout)
        with mock.patch.object(self.runner, "_capture_diff", return_value=""):
            self.runner.run_one_prompt(
                "h5ad-read", runs=1, timeout=1, claude_path="claude-fake",
                worktree_root=self.tmpdir, keep_worktrees=False,
                claude_runner=timeout_runner, primary_repo=self.tmpdir,
            )
        run_row = next(e for e in self._events() if e["event"] == "claude_md_eval_run")
        self.assertEqual(run_row["payload"]["verdict"], "error")
        self.assertIn("timed out", run_row["payload"]["error"])

    def test_multiple_runs_emit_one_row_each_plus_one_summary(self):
        self._write_default_prompt()
        def alternating_runner(worktree, prompt_body, *, timeout, claude_path, sandbox_log=None):
            return subprocess.CompletedProcess(args=[], returncode=0, stdout="", stderr="")
        # Two compliant, one anti.
        diffs = iter([
            "+ LabelPropsView(path)\n",
            "+ LabelPropsView(path).as_df()\n",
            "+ adata.read_h5ad(path)\n",
        ])
        with mock.patch.object(self.runner, "_capture_diff", side_effect=lambda _: next(diffs)):
            self.runner.run_one_prompt(
                "h5ad-read", runs=3, timeout=60, claude_path="claude-fake",
                worktree_root=self.tmpdir, keep_worktrees=False,
                claude_runner=alternating_runner, primary_repo=self.tmpdir,
            )
        events = self._events()
        run_rows = [e for e in events if e["event"] == "claude_md_eval_run"]
        pass_rows = [e for e in events if e["event"] == "claude_md_eval_pass"]
        self.assertEqual(len(run_rows), 3)
        self.assertEqual(len(pass_rows), 1)
        outcomes = [r["payload"]["verdict"] for r in run_rows]
        self.assertEqual(outcomes.count("compliant"), 2)
        self.assertEqual(outcomes.count("noncompliant"), 1)
        summary = pass_rows[0]["payload"]
        self.assertEqual(summary["compliant"], 2)
        self.assertEqual(summary["noncompliant"], 1)
        self.assertEqual(summary["runs_total"], 3)


class EnsureEvalSessionTest(unittest.TestCase):
    """`_ensure_eval_session` writes a synthetic session id when the env is bare.

    The pixi subprocess doesn't inherit `CLAUDE_CODE_SESSION_ID`, so every
    `claude_md_eval_*` row historically emitted with `sess=unknown`. With a synthetic
    id, all rows of one pass share a session — which is enough to group them.
    """

    def setUp(self):
        self.runner = _load_runner()

    def test_synthesises_when_env_unset(self):
        with mock.patch.dict(os.environ, {}, clear=False):
            os.environ.pop("CLAUDE_CODE_SESSION_ID", None)
            sess = self.runner._ensure_eval_session()
            self.assertTrue(sess.startswith("eval-"))
            self.assertEqual(os.environ["CLAUDE_CODE_SESSION_ID"], sess)

    def test_respects_existing_env(self):
        with mock.patch.dict(os.environ, {"CLAUDE_CODE_SESSION_ID": "real-sess-123"}):
            sess = self.runner._ensure_eval_session()
            self.assertEqual(sess, "real-sess-123")

    def test_idempotent(self):
        with mock.patch.dict(os.environ, {}, clear=False):
            os.environ.pop("CLAUDE_CODE_SESSION_ID", None)
            first = self.runner._ensure_eval_session()
            second = self.runner._ensure_eval_session()
            self.assertEqual(first, second)


class CaptureDiffExcludesClaudeMdTest(unittest.TestCase):
    """`_capture_diff` must exclude every `**/CLAUDE.md` from the scored diff.

    In arm=without runs the sandbox worktree gets every CLAUDE.md stripped before the
    agent spawns, so `git diff --cached HEAD` would otherwise include those deletions
    and the `compliant_signal` / `anti_signal` regexes would match the DELETED prose
    (canonical util names, anti-pattern examples) — inflating hit counts on lines the
    agent never wrote. Surfaced by the `crop-failure` without-arm pilot (PR #1274):
    without the pathspec exclusion the row reported `compliant_hits=7` on an agent
    diff that contained zero references to the canonical util.

    Test uses a real git repo (subprocess `git init` + `git commit`) — the pathspec
    exclusion is a git feature, so a fake would just test the wrong thing.
    """

    def setUp(self):
        self.runner = _load_runner()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.repo = pathlib.Path(self._tmp.name)
        # Initialise a repo with a committed root CLAUDE.md + a nested frontend/CLAUDE.md.
        # Config `user.email`/`user.name` locally so the commit doesn't need a global git
        # config (CI sandboxes often don't have one).
        for cmd in (
            ["git", "init", "-q"],
            ["git", "config", "user.email", "eval@test"],
            ["git", "config", "user.name", "eval-test"],
        ):
            subprocess.run(cmd, cwd=self.repo, check=True, capture_output=True)
        (self.repo / "CLAUDE.md").write_text(
            "# Root guide\n\nUse `zarr_utils.open_as_zarr` — never `zarr.open(`.\n",
            encoding="utf-8")
        (self.repo / "frontend").mkdir()
        (self.repo / "frontend" / "CLAUDE.md").write_text(
            "# Frontend rules\n\nCall `zarr_utils.open_as_zarr` etc.\n",
            encoding="utf-8")
        subprocess.run(["git", "add", "-A"], cwd=self.repo, check=True,
                       capture_output=True)
        subprocess.run(["git", "commit", "-q", "-m", "seed"], cwd=self.repo, check=True,
                       capture_output=True)

    def test_deleted_claude_md_files_are_not_in_scored_diff(self):
        # Simulate an arm=without spawn: strip every CLAUDE.md, then have the agent add
        # a new file at the canonical scratch location.
        (self.repo / "CLAUDE.md").unlink()
        (self.repo / "frontend" / "CLAUDE.md").unlink()
        (self.repo / "python").mkdir()
        (self.repo / "python" / "crop.py").write_text(
            "from cecelia.utils import zarr_utils\n\n"
            "def crop(p):\n"
            "    return zarr_utils.open_as_zarr(p)\n",
            encoding="utf-8")
        diff = self.runner._capture_diff(self.repo)
        # The agent's addition IS in the diff (positive control — makes sure the pathspec
        # didn't accidentally exclude everything).
        self.assertIn("crop.py", diff)
        self.assertIn("zarr_utils", diff)
        # The stripped CLAUDE.md files are NOT in the diff — this is the fix under test.
        self.assertNotIn("CLAUDE.md", diff,
                         "CLAUDE.md must be excluded from the scored diff so deleted "
                         "prose doesn't inflate compliant/anti hit counts")
        # And no anti-pattern token from CLAUDE.md prose leaks in (would cause false
        # anti_hits even when the agent wrote clean code).
        self.assertNotIn("never `zarr.open", diff)


class ToolOrderWidenedMatcherTest(unittest.TestCase):
    """`tool_order_passes` accepts a list of alternatives for both before/after tools.

    The single-tool matcher missed a compliant agent that Reads `INVENTORY.md` instead
    of Grepping it, or reaches for Edit instead of Write on an existing file — the rule
    is about the discovery-before-write ordering, not the tool identity.
    """

    def setUp(self):
        transcript_path = _REPO / "scripts" / "claude_md_eval" / "transcript.py"
        spec = importlib.util.spec_from_file_location("_transcript_t", transcript_path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        self.TS = mod.TranscriptSignals

    def _sig(self, calls):
        s = self.TS()
        s.tool_calls = calls
        return s

    def test_singular_form_still_works(self):
        s = self._sig([
            {"tool": "Grep", "input": {"pattern": "inventory"}},
            {"tool": "Write", "input": {"file_path": "x.py"}},
        ])
        self.assertTrue(s.tool_order_passes("Grep", "inventory", "Write"))

    def test_list_of_before_tools_any_matches(self):
        # Agent Read INVENTORY.md instead of Grepping — should still pass.
        s = self._sig([
            {"tool": "Read", "input": {"file_path": "INVENTORY.md"}},
            {"tool": "Write", "input": {"file_path": "x.py"}},
        ])
        self.assertTrue(s.tool_order_passes(
            ["Grep", "Read", "Glob"], "INVENTORY", ["Write", "Edit"]))

    def test_list_of_after_tools_any_matches(self):
        # Agent used Edit to patch an existing file — Edit counts as the write.
        s = self._sig([
            {"tool": "Grep", "input": {"pattern": "docs/inventory"}},
            {"tool": "Edit", "input": {"file_path": "x.py"}},
        ])
        self.assertTrue(s.tool_order_passes(
            ["Grep", "Read"], "docs/inventory", ["Write", "Edit", "MultiEdit"]))

    def test_after_without_before_still_fails(self):
        # Wrote without discovering — anti case, must fail.
        s = self._sig([
            {"tool": "Edit", "input": {"file_path": "x.py"}},
        ])
        self.assertFalse(s.tool_order_passes(
            ["Grep", "Read", "Glob"], "inventory", ["Write", "Edit"]))


if __name__ == "__main__":
    unittest.main()
