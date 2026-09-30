"""Tests for the .claude/hooks/check_commit_recital.py hook.

The hook is text-pattern presence-checking on a `git commit` command. The invariants pinned
here are the ones that would either silently let an unfixed finding through, or block a
legitimate commit:

- **A commit with N findings and N outcomes passes.**
- **A commit with more findings than outcomes blocks** — the specific hole the hook exists to
  close. Named in the block message so the agent knows what to fix.
- **A commit with no findings at all passes** — the reviewer said everything's fine, no
  disclosure needed.
- **Non-`git commit` Bash calls pass** unchanged — the hook must not block routine work.
- **`CECELIA_SKIP_RECITAL_CHECK=1` bypasses** — real emergencies.
- **Malformed stdin degrades to allow, not block** — a bug in the hook must not wedge git.
- **Bare word `false_positive` in prose without a colon-reason does NOT count as an outcome
  tag** — otherwise the reviewer's own scoping doc, or a comment mentioning the term, would
  spuriously satisfy the check.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import append_event, read_events

_HOOK_PATH = pathlib.Path(__file__).resolve().parents[3] / ".claude" / "hooks" / "check_commit_recital.py"


def _load_hook():
    spec = importlib.util.spec_from_file_location("check_commit_recital", _HOOK_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class CheckTest(unittest.TestCase):
    def setUp(self):
        self.hook = _load_hook()
        # Existing outcome-tag tests don't exercise the SHA-anchored real-review gate; force
        # `_current_head_sha` to None so that gate degrades to allow. Tests for the SHA gate
        # itself live in SHAAnchoredCheckTest below.
        self._sha_patch = mock.patch.object(self.hook, "_current_head_sha", return_value=None)
        self._sha_patch.start()
        self.addCleanup(self._sha_patch.stop)

    def _check(self, command: str):
        return self.hook.check(command)

    def test_non_commit_command_passes(self):
        self.assertIsNone(self._check("ls -la"))
        self.assertIsNone(self._check("python -m unittest"))
        self.assertIsNone(self._check("git status"))

    def test_marker_quoted_in_backticks_needs_no_tag(self):
        # Same rule as the recital parser: a quoted marker is prose about the grammar.
        self.assertIsNone(self._check(
            "fix: parser accepts `[**confirmed**, no action]` and `[**should reuse**]`"))

    def test_git_commit_with_no_findings_passes(self):
        self.assertIsNone(self._check("git commit -m 'small fix'"))

    def test_git_commit_with_matching_outcomes_passes(self):
        msg = """git commit -m '
        - **file.jl:42** — foo shape [**confirmed**] [fixed_pre_commit]
        - **file.jl:99** — bar shape [**should reuse**] [shipped_with_finding: user-requested]
        '"""
        self.assertIsNone(self._check(msg))

    def test_git_commit_with_missing_outcome_blocks(self):
        # One finding, zero outcomes.
        msg = "git commit -m 'body: - **file:1** foo [**confirmed**]'"
        reason = self._check(msg)
        self.assertIsNotNone(reason)
        self.assertIn("1 reviewer finding", reason)
        self.assertIn("only 0 outcome tag", reason)
        self.assertIn("CECELIA_SKIP_RECITAL_CHECK", reason)

    def test_multiple_findings_need_multiple_outcomes(self):
        # 3 findings, 2 outcomes — under-count blocks.
        msg = """git commit -m '
        - x [**confirmed**] [fixed_pre_commit]
        - y [**should reuse**] [false_positive: not-actually-canonical]
        - z [**confirmed**]
        '"""
        self.assertIsNotNone(self._check(msg))

    def test_bare_false_positive_word_does_not_count(self):
        # Prose or code mentioning `false_positive` (no colon+reason) must NOT satisfy the check.
        msg = "git commit -m 'foo [**confirmed**] the false_positive rate is fine'"
        self.assertIsNotNone(self._check(msg))

    def test_bare_shipped_with_finding_word_does_not_count(self):
        msg = "git commit -m 'foo [**confirmed**] shipped_with_finding'"  # no colon
        self.assertIsNotNone(self._check(msg))

    def test_fixed_pre_commit_bare_counts(self):
        # fixed_pre_commit stands alone (no reason needed) — the fix is in the diff itself.
        msg = "git commit -m 'foo [**confirmed**] [fixed_pre_commit]'"
        self.assertIsNone(self._check(msg))

    def test_dropped_no_action_with_reason_counts(self):
        # The 4th outcome, previously missing from the hook's copy of the vocabulary.
        # A finding honestly tagged this way must pass, otherwise the agent is forced to
        # mis-tag it as `false_positive` to get past the gate — defeating the whole point.
        msg = "git commit -m 'foo [**confirmed**] [dropped_no_action: session ended before triage]'"
        self.assertIsNone(self._check(msg))

    def test_vocabulary_matches_effectiveness_log_module(self):
        # The hook must always accept every tag the log module accepts — no divergent copies.
        # (The reverse direction — hook accepts tags the log doesn't — is caught by log.py's own
        # UnknownOutcomeError test in test_effectiveness_log.py.)
        from cecelia.effectiveness import OUTCOME_VOCABULARY

        for tag in OUTCOME_VOCABULARY:
            body = f"[**confirmed**] [{tag}]" if tag == "fixed_pre_commit" else f"[**confirmed**] [{tag}: reason]"
            msg = f"git commit -m '{body}'"
            self.assertIsNone(self._check(msg), f"vocabulary tag not accepted: {tag}")

    def test_heredoc_style_message_is_checked(self):
        # The heredoc lives inside -m "$(cat <<'EOF' ... EOF)"; the hook sees the literal command
        # string, and every marker appears in it — so the check works despite no shell expansion.
        msg = """git commit -m "$(cat <<'EOF'
        summary line

        - **file.jl:10** foo [**confirmed**] [fixed_pre_commit]
        - **file.vue:20** bar [**should reuse**] [false_positive: bespoke by design]

        _Sibling-call audit: run_
        EOF
        )" """
        self.assertIsNone(self._check(msg))

    def test_subshell_git_commit_is_checked(self):
        # `cd path && git commit` — the hook matches on the substring, not just prefix.
        msg = "cd /some/path && git commit -m 'foo [**confirmed**]'"
        self.assertIsNotNone(self._check(msg))

    def test_git_commit_amend_is_checked(self):
        msg = "git commit --amend -m 'foo [**confirmed**]'"
        self.assertIsNotNone(self._check(msg))

    # ------- P3 (FINDINGS_EMISSION_PLAN.md) — slug-paired outcome tags -------

    def test_slug_paired_outcome_counts_as_outcome(self):
        msg = "git commit -m 'foo [**confirmed**] [fanout-abcd1234: fixed_pre_commit]'"
        self.assertIsNone(self._check(msg))

    def test_conv_slug_pair_counts(self):
        msg = "git commit -m 'foo [**should reuse**] [conv-11112222: false_positive]'"
        self.assertIsNone(self._check(msg))

    def test_mixed_bare_and_paired_outcomes_both_count(self):
        # Two findings, one bare tag and one slug pair — total covers both.
        msg = """git commit -m '
        - a [**confirmed**] [fixed_pre_commit]
        - b [**should reuse**] [conv-abcdef00: shipped_with_finding]
        '"""
        self.assertIsNone(self._check(msg))

    def test_duplicate_slug_blocks(self):
        # Same slug quoted twice — the same finding can't have two outcomes. Block explicitly
        # so the author fixes the message before shipping.
        msg = """git commit -m '
        - a [**confirmed**] [fanout-abcd1234: fixed_pre_commit]
        - b [**confirmed**] [fanout-abcd1234: false_positive]
        '"""
        reason = self._check(msg)
        self.assertIsNotNone(reason)
        self.assertIn("Duplicate slug", reason)
        self.assertIn("fanout-abcd1234", reason)

class ResolutionWritingTest(unittest.TestCase):
    """P3: slug-paired outcomes get written to the effectiveness log as resolution rows.

    These are the rows the rollup joins to pending `_finding` rows via slug — the whole
    reason the plan-doc calls the hook the 'load-bearing bit' of turning telemetry into
    evidence.
    """

    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def test_fanout_slug_pair_writes_finding_resolved_row(self):
        msg = "git commit -m 'foo [**confirmed**] [fanout-abcd1234: fixed_pre_commit]'"
        n = self.hook.write_resolutions(msg, pr="#1400")
        self.assertEqual(n, 1)
        events = self._events()
        self.assertEqual(len(events), 1)
        row = events[0]
        self.assertEqual(row["event"], "fanout_audit_finding_resolved")
        self.assertEqual(row["payload"]["slug"], "fanout-abcd1234")
        self.assertEqual(row["payload"]["outcome"], "fixed_pre_commit")
        self.assertEqual(row["pr"], "#1400")

    def test_conv_slug_pair_writes_convention_check_finding_resolved(self):
        msg = "git commit -m 'foo [**should reuse**] [conv-11112222: false_positive]'"
        self.hook.write_resolutions(msg, pr=None)
        events = self._events()
        self.assertEqual(events[0]["event"], "convention_check_finding_resolved")
        self.assertEqual(events[0]["pr"], None)

    def test_multiple_pairs_write_multiple_rows(self):
        msg = """git commit -m '
        - a [**confirmed**] [fanout-11111111: fixed_pre_commit]
        - b [**confirmed**] [fanout-22222222: shipped_with_finding]
        - c [**should reuse**] [conv-33333333: false_positive]
        '"""
        n = self.hook.write_resolutions(msg, pr="#1401")
        self.assertEqual(n, 3)
        events = self._events()
        self.assertEqual(len(events), 3)
        slugs = {e["payload"]["slug"] for e in events}
        self.assertEqual(slugs, {"fanout-11111111", "fanout-22222222", "conv-33333333"})

    def test_branch_field_lands_on_resolution_row(self):
        # The hook captures the current branch so the rollup can resolve `pr` even when
        # `write_resolutions` is called with `pr=None` (typical — first commit of a new PR
        # runs before `gh pr view` returns anything).
        msg = "git commit -m 'foo [**confirmed**] [fanout-abcd1234: fixed_pre_commit]'"
        self.hook.write_resolutions(msg, pr=None, branch="feat/log-branch-capture")
        row = self._events()[0]
        self.assertEqual(row["branch"], "feat/log-branch-capture")
        self.assertIsNone(row["pr"])

    def test_bare_outcome_tags_do_not_write_rows(self):
        # Legacy bare form has no slug, so no resolution row is possible.
        msg = "git commit -m 'foo [**confirmed**] [fixed_pre_commit]'"
        n = self.hook.write_resolutions(msg, pr=None)
        self.assertEqual(n, 0)
        self.assertEqual(self._events(), [])

    def test_non_commit_command_does_not_write(self):
        # `ls` — no git commit at all; nothing should be written even if it happens to contain
        # a slug pair (unlikely, but bounds-check).
        msg = "ls [fanout-abcd1234: fixed_pre_commit]"
        # write_resolutions doesn't itself gate on `git commit`; that's main()'s job. This test
        # documents that the caller (main) checks first. See test below for the main-level gate.
        n = self.hook.write_resolutions(msg, pr=None)
        self.assertEqual(n, 1)  # write_resolutions IS permissive by design

    def test_wrong_prefix_slug_does_not_write(self):
        # A syntactically pair-shaped tag with an unknown mechanism prefix (`xyz-…`) doesn't
        # match `_SLUG_PAIR`, so no row lands. Defensive: keeps garbage out of the log.
        msg = "git commit -m 'foo [xyz-abcd1234: fixed_pre_commit]'"
        n = self.hook.write_resolutions(msg, pr=None)
        self.assertEqual(n, 0)
        self.assertEqual(self._events(), [])


class SHAAnchoredCheckTest(unittest.TestCase):
    """The SHA-anchored real-review gate.

    A findings-carrying commit must have at least one recital `_run` row in the effectiveness
    log whose `commit` matches HEAD-at-hook-time (i.e. the parent SHA of the commit being made).
    Catches:
    - hand-typed recital body with no real `pixi run recital` invocation (no `_run` row exists);
    - a `_run` from before a rebase (SHA no longer matches the new parent);
    - a `_run` from a different branch tip (SHA doesn't match).

    Degrades to allow when HEAD SHA can't be captured — the hook must not block on its own
    failure to reach git.
    """

    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(
            os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)},
        )
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        b = mock.patch.object(self.hook, "_current_branch", return_value="feat/x")
        b.start()
        self.addCleanup(b.stop)

    def _msg_with_finding(self) -> str:
        return "git commit -m 'foo [**confirmed**] [fanout-abcd1234: fixed_pre_commit]'"

    def _patch_head(self, sha: str | None):
        p = mock.patch.object(self.hook, "_current_head_sha", return_value=sha)
        p.start()
        self.addCleanup(p.stop)

    def test_findings_with_matching_run_passes(self):
        head = "b" * 40
        self._patch_head(head)
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=head, branch="feat/x")
        self.assertIsNone(self.hook.check(self._msg_with_finding()))

    def test_findings_with_no_run_at_all_blocks(self):
        head = "c" * 40
        self._patch_head(head)
        # log is empty
        reason = self.hook.check(self._msg_with_finding())
        self.assertIsNotNone(reason)
        self.assertIn("no matching `_run` row", reason)
        self.assertIn(head[:8], reason)

    def test_findings_with_run_for_different_sha_blocks(self):
        head = "d" * 40
        other = "e" * 40
        self._patch_head(head)
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=other)
        append_event("convention_check_run", {"duration_s": 1.0}, commit=other)
        reason = self.hook.check(self._msg_with_finding())
        self.assertIsNotNone(reason)
        self.assertIn("no matching `_run` row", reason)

    def test_findings_with_null_commit_run_blocks(self):
        # Legacy rows written before this feature carry `commit: null`; they must not satisfy
        # the SHA gate for the current HEAD. Blocks so a stale log can't grandfather.
        head = "f" * 40
        self._patch_head(head)
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=None)
        self.assertIsNotNone(self.hook.check(self._msg_with_finding()))

    def test_convention_run_alone_also_satisfies(self):
        # Either mechanism's `_run` counts — a recital run always emits both, so any one
        # matching row proves recital fired.
        head = "1" * 40
        self._patch_head(head)
        append_event("convention_check_run", {"duration_s": 1.0}, commit=head, branch="feat/x")
        self.assertIsNone(self.hook.check(self._msg_with_finding()))

    def test_no_findings_skips_sha_gate_entirely(self):
        # A trivial commit (no findings) never touches the SHA gate — even if the log is empty
        # and HEAD is known, it must pass. The gate is scoped to "findings-carrying commit."
        self._patch_head("2" * 40)
        self.assertIsNone(self.hook.check("git commit -m 'small fix'"))

    def test_head_sha_none_degrades_to_allow(self):
        # `git rev-parse HEAD` failed — the hook cannot verify the review anchor, so it must
        # allow rather than block on its own failure. Prevents a broken CI from wedging git.
        self._patch_head(None)
        # No `_run` rows at all, but head_sha=None → pass.
        self.assertIsNone(self.hook.check(self._msg_with_finding()))

    def test_missing_log_file_blocks_findings_commit(self):
        # First-ever recital hasn't been run → log doesn't exist → no matching row exists.
        # A findings-bearing commit in that state IS the failure mode we want to catch
        # (someone hand-typed recital text without invoking `pixi run recital`).
        head = "3" * 40
        self._patch_head(head)
        # log_path is set in setUp but the file itself is never created here.
        self.assertFalse(self.log_path.exists())
        self.assertIsNotNone(self.hook.check(self._msg_with_finding()))

    def test_finding_event_row_does_not_satisfy_sha_gate(self):
        # Only `_run` rows count. A `_finding` row on its own must not pass the gate — if
        # someone wrote finding rows directly to the log without a matching `_run`, we still
        # want to catch that as an incomplete recital.
        head = "4" * 40
        self._patch_head(head)
        append_event(
            "fanout_audit_finding",
            {"file": "x", "line": 1, "desc": "y", "slug": "fanout-abcd1234", "marker": "confirmed"},
            commit=head,
        )
        self.assertIsNotNone(self.hook.check(self._msg_with_finding()))


class SlugFormGateTest(unittest.TestCase):
    """Bare tags write nothing to the log. When recital slugged findings for HEAD, a
    bare-tagged commit (session 94ccd376's shape) would leave them to the orphan sweep and
    log a fixed finding as `dropped_no_action` — so the hook requires the slug form."""

    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(
            os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)},
        )
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        b = mock.patch.object(self.hook, "_current_branch", return_value="feat/x")
        b.start()
        self.addCleanup(b.stop)
        self.head = "e" * 40
        p = mock.patch.object(self.hook, "_current_head_sha", return_value=self.head)
        p.start()
        self.addCleanup(p.stop)
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=self.head, branch="feat/x")

    def _emit_finding(self, slug: str, commit: str | None = None):
        append_event(
            "fanout_audit_finding",
            {"slug": slug, "file": "x.py", "line": 1, "desc": "d", "marker": "confirmed"},
            commit=commit or self.head, branch="feat/x",
        )

    def test_bare_tag_with_logged_slug_blocks_and_names_it(self):
        self._emit_finding("fanout-11111111")
        reason = self.hook.check("git commit -m 'x [**confirmed**] [fixed_pre_commit]'")
        self.assertIsNotNone(reason)
        self.assertIn("fanout-11111111", reason)

    def test_slug_form_passes(self):
        self._emit_finding("fanout-11111111")
        self.assertIsNone(self.hook.check(
            "git commit -m 'x [**confirmed**] [fanout-11111111: fixed_pre_commit]'"))

    def test_bare_tag_without_logged_slugs_passes(self):
        # Legacy path: recital logged no findings for HEAD, so bare tags are all there is.
        self.assertIsNone(self.hook.check("git commit -m 'x [**confirmed**] [fixed_pre_commit]'"))

    def test_already_resolved_slug_does_not_block_bare_tag(self):
        self._emit_finding("fanout-11111111")
        append_event("fanout_audit_finding_resolved",
                     {"slug": "fanout-11111111", "outcome": "fixed_pre_commit"}, commit=self.head, branch="feat/x")
        self.assertIsNone(self.hook.check("git commit -m 'x [**confirmed**] [fixed_pre_commit]'"))

    def test_slug_pair_alone_is_not_treated_as_bare(self):
        # Another untagged slug on the change must not trip the bare-tag gate when this
        # message tags by slug only — the outcome word inside the pair isn't a bare tag.
        self._emit_finding("fanout-11111111")
        self._emit_finding("fanout-22222222")
        self.assertIsNone(self.hook.check(
            "git commit -m 'x [**confirmed**] [fanout-11111111: fixed_pre_commit]'"))

    def test_slug_on_other_branch_same_sha_does_not_block(self):
        append_event("fanout_audit_finding", {"slug": "fanout-22222222", "file": "x.py",
                     "line": 1, "desc": "d", "marker": "confirmed"},
                     commit=self.head, branch="other-worktree")
        self.assertIsNone(self.hook.check("git commit -m 'x [**confirmed**] [fixed_pre_commit]'"))

    def test_run_from_other_branch_same_sha_does_not_satisfy_gate_2(self):
        self.log_path.unlink()
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=self.head,
                     branch="other-worktree")
        self.assertIsNotNone(self.hook.check(
            "git commit -m 'x [**confirmed**] [fanout-11111111: fixed_pre_commit]'"))

    def test_slug_on_other_head_does_not_block(self):
        self._emit_finding("fanout-11111111", commit="f" * 40)
        self.assertIsNone(self.hook.check("git commit -m 'x [**confirmed**] [fixed_pre_commit]'"))


class OrphanSlugDropTest(unittest.TestCase):
    """Auto-drop sweep for slugs the recital emitted against the current HEAD that this
    commit neither tagged nor previously resolved.

    Before this sweep, an agent could trim a finding line out of the commit body and the
    `_finding` row would stay `unresolved` in the rollup forever — the log couldn't tell
    "ignored" from "not yet handled." The sweep runs after `write_resolutions` in main(),
    so a tagged slug isn't second-guessed; only orphans get the auto-drop.
    """

    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(
            os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)},
        )
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def _emit_finding(self, slug: str, commit: str, event: str = "fanout_audit_finding",
                      branch: str = "feat/x"):
        append_event(
            event,
            {"slug": slug, "file": "x.py", "line": 1, "desc": "d", "marker": "confirmed"},
            commit=commit, branch=branch,
        )

    def test_untagged_slug_on_head_gets_auto_dropped(self):
        head = "a" * 40
        self._emit_finding("fanout-11111111", commit=head)
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'no tags in here'", pr=None, commit=head, branch="feat/x",
        )
        self.assertEqual(n, 1)
        rows = [e for e in self._events() if e["event"] == "fanout_audit_finding_resolved"]
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["payload"]["slug"], "fanout-11111111")
        self.assertEqual(rows[0]["payload"]["outcome"], "dropped_no_action")
        self.assertEqual(rows[0]["branch"], "feat/x")

    def test_tagged_slug_is_not_auto_dropped(self):
        head = "b" * 40
        self._emit_finding("fanout-22222222", commit=head)
        msg = "git commit -m 'foo [**confirmed**] [fanout-22222222: fixed_pre_commit]'"
        n = self.hook.write_dropped_for_orphan_slugs(msg, pr=None, commit=head, branch="feat/x")
        self.assertEqual(n, 0)

    def test_previously_resolved_slug_is_not_re_dropped(self):
        # A slug resolved by an earlier commit stays resolved; the sweep must not overwrite
        # it with `dropped_no_action` on a later unrelated commit.
        head = "c" * 40
        self._emit_finding("fanout-33333333", commit=head)
        append_event(
            "fanout_audit_finding_resolved",
            {"slug": "fanout-33333333", "outcome": "fixed_pre_commit"},
            commit=head, branch="feat/x",
        )
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'later unrelated commit'", pr=None, commit=head, branch="feat/x",
        )
        self.assertEqual(n, 0)

    def test_finding_on_different_head_is_ignored(self):
        # Findings from a prior HEAD belong to that HEAD's commit — this commit shouldn't
        # bulk-drop them. The `commit == current_head` filter is what enforces this.
        old_head = "d" * 40
        new_head = "e" * 40
        self._emit_finding("fanout-44444444", commit=old_head)
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'commit against new head'",
            pr=None, commit=new_head, branch="feat/x",
        )
        self.assertEqual(n, 0)

    def test_missing_head_sha_skips_sweep(self):
        # If HEAD SHA can't be captured, the sweep must degrade — broadcasting drops across
        # every unresolved slug in the log would be catastrophic (the finding→HEAD join is
        # the whole selection).
        self._emit_finding("fanout-55555555", commit="f" * 40)
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'foo'", pr=None, commit=None, branch=None,
        )
        self.assertEqual(n, 0)

    def test_convention_slug_uses_convention_resolved_event(self):
        # The mechanism prefix determines which `_resolved` event we emit; a `conv-` slug
        # must land as `convention_check_finding_resolved`, not `fanout_audit_finding_resolved`.
        head = "0" * 40
        self._emit_finding("conv-66666666", commit=head, event="convention_check_finding")
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'no tag'", pr=None, commit=head, branch="feat/x",
        )
        self.assertEqual(n, 1)
        rows = [e for e in self._events() if e["event"] == "convention_check_finding_resolved"]
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["payload"]["outcome"], "dropped_no_action")

    def test_missing_branch_skips_sweep(self):
        # Without a branch the sweep can't tell this worktree's findings from another's on
        # the same base SHA, so it writes nothing rather than guess.
        head = "2" * 40
        self._emit_finding("fanout-aaaaaaaa", commit=head)
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'no tag'", pr=None, commit=head, branch=None,
        )
        self.assertEqual(n, 0)

    def test_other_branch_on_same_base_sha_is_not_dropped(self):
        # The 2026-09-30 incident: `card-reshuffle` and `module-keepalive` both branched off
        # the same main tip; card-reshuffle's commit auto-dropped module-keepalive's finding.
        head = "3" * 40
        self._emit_finding("fanout-7d998e9e", commit=head, branch="module-keepalive")
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'no tag'", pr=None, commit=head, branch="card-reshuffle",
        )
        self.assertEqual(n, 0)
        self.assertEqual([e for e in self._events() if e["event"].endswith("_resolved")], [])

    def test_other_branch_resolution_does_not_hide_this_branch_finding(self):
        # Slugs carry no branch: two worktrees flagging the same file:line share one. The other
        # branch resolving it must not make this branch's pending finding look handled.
        head = "4" * 40
        self._emit_finding("fanout-bbbbbbbb", commit=head, branch="feat/x")
        append_event("fanout_audit_finding_resolved",
                     {"slug": "fanout-bbbbbbbb", "outcome": "fixed_pre_commit"},
                     commit=head, branch="other-worktree")
        n = self.hook.write_dropped_for_orphan_slugs(
            "no tag", pr=None, commit=head, branch="feat/x",
        )
        self.assertEqual(n, 1)

    def test_multiple_orphans_all_get_dropped(self):
        head = "1" * 40
        self._emit_finding("fanout-77777777", commit=head)
        self._emit_finding("fanout-88888888", commit=head)
        self._emit_finding("conv-99999999", commit=head, event="convention_check_finding")
        n = self.hook.write_dropped_for_orphan_slugs(
            "git commit -m 'no tags'", pr=None, commit=head, branch="feat/x",
        )
        self.assertEqual(n, 3)


class GuardTest(unittest.TestCase):
    """PreToolUse guard: only ensures the git hook can't be skipped. It never reads the message
    and never writes the log — the 2026-09-30 failure was the in-command checks writing fake
    resolution rows from a test heredoc that merely contained `git commit` + a slug pair."""

    def setUp(self):
        self.hook = _load_hook()

    def _guard(self, command: str, hooks_path: str | None = ".githooks", in_repo: bool = True):
        def fake_git(*args, cwd=None):
            if args[:2] == ("config", "--get"):
                return hooks_path
            if args[:1] == ("rev-parse",):
                return ".git" if in_repo else None
            return None
        with mock.patch.object(self.hook, "_git", side_effect=fake_git):
            return self.hook.guard(command, cwd="/x")

    def test_non_commit_passes_without_touching_git(self):
        with mock.patch.object(self.hook, "_git", side_effect=AssertionError("no git call")):
            self.assertIsNone(self.hook.guard("ls -la && grep -rn foo .", cwd="/x"))

    def test_heredoc_mentioning_a_slug_pair_is_not_checked_or_logged(self):
        # The guard allows it (hooks active) and nothing reads the text as a message.
        cmd = "cat > t.py <<'EOF'\ncheck(\"git commit -m 'x [fanout-11111111: fixed_pre_commit]'\")\nEOF"
        self.assertIsNone(self._guard(cmd))

    def test_commit_with_hooks_active_passes(self):
        self.assertIsNone(self._guard("cd wt && git commit -q -F msg.txt"))

    def test_commit_without_hooks_path_blocks(self):
        reason = self._guard("git commit -m x", hooks_path=None)
        self.assertIsNotNone(reason)
        self.assertIn("install-git-hooks", reason)

    def test_outside_a_repo_allows(self):
        self.assertIsNone(self._guard("git commit -m x", hooks_path=None, in_repo=False))

    def test_no_verify_and_dash_n_block(self):
        self.assertIsNotNone(self._guard("git commit --no-verify -m x"))
        self.assertIsNotNone(self._guard("git commit -n -m x"))
        self.assertIsNotNone(self._guard("git -c core.hooksPath=/dev/null commit -m x"))

    def test_dash_n_outside_the_commit_segment_is_fine(self):
        self.assertIsNone(self._guard("git commit -m x && git log -n 1"))
        self.assertIsNone(self._guard("git commit -m x | head -n 5"))


class CommitMsgHookTest(unittest.TestCase):
    """git `commit-msg` entry point: gates run on the real message file; resolutions written."""

    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        env.start()
        self.addCleanup(env.stop)
        # These tests run inside Claude Code (CLAUDECODE=1); the agent gate has its own tests.
        os.environ.pop("CLAUDECODE", None)
        self.head = "9" * 40
        for name, value in (("_current_head_sha", self.head), ("_current_branch", "feat/x"),
                            ("_current_pr", None), ("_in_sequencer_commit", False)):
            p = mock.patch.object(self.hook, name, return_value=value)
            p.start()
            self.addCleanup(p.stop)

    def _msg_file(self, text: str) -> str:
        path = pathlib.Path(self._tmp.name) / "COMMIT_EDITMSG"
        path.write_text(text, encoding="utf-8")
        return str(path)

    def _resolved(self):
        return [e for e in read_events(self.log_path) if e["event"].endswith("_resolved")]

    def test_slug_pair_in_message_file_writes_resolution(self):
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=self.head, branch="feat/x")
        append_event("fanout_audit_finding", {"slug": "fanout-abcd1234", "file": "x.py",
                     "line": 1, "desc": "d", "marker": "confirmed"},
                     commit=self.head, branch="feat/x")
        rc = self.hook.main(["--commit-msg", self._msg_file(
            "fix: thing\n\n- [fanout-abcd1234: fixed_pre_commit] x [**confirmed**]\n")])
        self.assertEqual(rc, 0)
        self.assertEqual([(e["payload"]["slug"], e["payload"]["outcome"]) for e in self._resolved()],
                         [("fanout-abcd1234", "fixed_pre_commit")])

    def test_untagged_finding_blocks_with_exit_1(self):
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=self.head, branch="feat/x")
        rc = self.hook.main(["--commit-msg", self._msg_file("x [**confirmed**]\n")])
        self.assertEqual(rc, 1)
        self.assertEqual(self._resolved(), [])

    def test_comment_lines_are_ignored(self):
        # git strips `#` lines from the commit; a commented-out finding must not count.
        rc = self.hook.main(["--commit-msg", self._msg_file(
            "small fix\n# - x [**confirmed**]\n")])
        self.assertEqual(rc, 0)

    def test_merge_commit_is_skipped_entirely(self):
        append_event("fanout_audit_finding", {"slug": "fanout-abcd1234", "file": "x.py",
                     "line": 1, "desc": "d", "marker": "confirmed"},
                     commit=self.head, branch="feat/x")
        with mock.patch.object(self.hook, "_in_sequencer_commit", return_value=True):
            rc = self.hook.main(["--commit-msg", self._msg_file("Merge branch 'main'\n")])
        self.assertEqual(rc, 0)
        self.assertEqual(self._resolved(), [])  # no orphan auto-drop on a merge

    def test_agent_commit_without_recital_run_blocks(self):
        # Session 3658f232 (2026-09-30): no findings, copied check trailer, no recital — passed.
        with mock.patch.dict(os.environ, {"CLAUDECODE": "1"}):
            rc = self.hook.main(["--commit-msg", self._msg_file(
                "fix: thing\n\nConvention check: no findings. Inventory check: run\n")])
        self.assertEqual(rc, 1)

    def test_agent_commit_with_recital_run_passes(self):
        append_event("convention_check_run", {"duration_s": 1.0}, commit=self.head, branch="feat/x")
        with mock.patch.dict(os.environ, {"CLAUDECODE": "1"}):
            rc = self.hook.main(["--commit-msg", self._msg_file("fix: thing\n")])
        self.assertEqual(rc, 0)

    def test_agent_gate_ignores_other_branch_run(self):
        append_event("fanout_audit_run", {"duration_s": 1.0}, commit=self.head, branch="other")
        with mock.patch.dict(os.environ, {"CLAUDECODE": "1"}):
            rc = self.hook.main(["--commit-msg", self._msg_file("fix: thing\n")])
        self.assertEqual(rc, 1)

    def test_human_commit_needs_no_recital(self):
        rc = self.hook.main(["--commit-msg", self._msg_file("fix: thing\n")])
        self.assertEqual(rc, 0)

    def test_skip_env_bypasses(self):
        with mock.patch.dict(os.environ, {"CECELIA_SKIP_RECITAL_CHECK": "1"}):
            rc = self.hook.main(["--commit-msg", self._msg_file("x [**confirmed**]\n")])
        self.assertEqual(rc, 0)


if __name__ == "__main__":
    unittest.main()
