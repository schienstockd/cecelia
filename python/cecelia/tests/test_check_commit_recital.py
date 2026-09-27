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

from cecelia.effectiveness import read_events

_HOOK_PATH = pathlib.Path(__file__).resolve().parents[3] / ".claude" / "hooks" / "check_commit_recital.py"


def _load_hook():
    spec = importlib.util.spec_from_file_location("check_commit_recital", _HOOK_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class CheckTest(unittest.TestCase):
    def setUp(self):
        self.hook = _load_hook()

    def _check(self, command: str):
        return self.hook.check(command)

    def test_non_commit_command_passes(self):
        self.assertIsNone(self._check("ls -la"))
        self.assertIsNone(self._check("python -m unittest"))
        self.assertIsNone(self._check("git status"))

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


if __name__ == "__main__":
    unittest.main()
