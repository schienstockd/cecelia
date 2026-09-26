"""Tests for the recital orchestrator.

Uses the `claude_runner` seam to bypass real subprocess calls — same pattern as
`api/test/suite/kiwi_turn.jl`'s fake engine. What the tests pin:

- **A successful run emits BOTH `_run` events** — the whole point of moving discipline into
  code is that emission is atomic with the reviewer run and can't be silently skipped.
- **A reviewer failure still emits its event** (with `error` in the payload) — the log
  records the attempt even when the reviewer subprocess fails, so a spike of errors is
  visible in the rollup.
- **A short-circuit reply** (like `_no fanout audit needed_`) passes through as a bare tail
  line, not wrapped in an evidence fold.
- **The event name flips between `sibling_audit_run` and `fanout_audit_run`** based on which
  is in the closed vocabulary — the recital survives the sibling→fanout rename (#1251)
  without a follow-up edit.
- **Outcome-tag-requiring findings emit `_finding` rows** — `**confirmed**` (fanout) and
  `**should reuse**` (convention) each land as one pending log row with a deterministic slug;
  `**plausible**` / `**potential duplicate**` do NOT, so pending↔resolution stays 1:1 with the
  hook's tag-count check (per FINDINGS_EMISSION_PLAN.md Decision 2).
- **Slugs are deterministic** across runs on the same (file, line, marker) — the author can
  re-run recital between commits and quote the same slug.
- **Recital body prefixes `[slug]` on each outcome-tag-requiring bullet** — so the author sees
  what to paste into the `[slug: outcome]` pair in the commit message.
"""
from __future__ import annotations

import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import EVENT_TYPES, read_events
from cecelia.effectiveness.recital import RecitalError, run_recital


class RecitalTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        # Avoid real `gh pr view` subprocess spawns in every test; individual tests that
        # exercise PR capture patch this explicitly.
        self._pr_patch = mock.patch("cecelia.effectiveness.recital._current_pr", return_value=None)
        self._pr_patch.start()
        self.addCleanup(self._pr_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def test_successful_run_emits_both_run_events(self):
        # The whole point of option 3 — emission can't be silently skipped.
        def fake(prompt: str) -> str:
            return "no findings"

        run_recital("some diff", claude_runner=fake)

        events = self._events()
        event_types = {e["event"] for e in events}
        # Whichever name is in the vocabulary — the recital picks the right one.
        expected_fanout = "fanout_audit_run" if "fanout_audit_run" in EVENT_TYPES else "sibling_audit_run"
        self.assertIn(expected_fanout, event_types)
        self.assertIn("convention_check_run", event_types)

    def test_run_event_carries_duration(self):
        def fake(prompt: str) -> str:
            return "no findings"

        run_recital("some diff", claude_runner=fake)
        for e in self._events():
            self.assertIn("duration_s", e["payload"])
            self.assertIsInstance(e["payload"]["duration_s"], (int, float))

    def test_reviewer_failure_still_emits_event_with_error(self):
        # Failure recording is important: rollup can see the error rate over time.
        def fake_that_fails(prompt: str) -> str:
            raise RecitalError("simulated subprocess crash")

        recital = run_recital("some diff", claude_runner=fake_that_fails)

        events = self._events()
        # BOTH reviewers "ran" (attempted) and both should have events, both with error.
        self.assertEqual(len(events), 2)
        for e in events:
            self.assertIn("error", e["payload"])
            self.assertIn("simulated subprocess crash", e["payload"]["error"])

        # The recital text should still return, with the error surfaced in each section.
        self.assertIn("RECITAL SCRIPT ERROR", recital)

    def test_short_circuit_reply_passes_through_as_tail_line(self):
        # A reviewer that returns e.g. `_no fanout audit needed_` should NOT get wrapped
        # in an evidence fold — that reply IS the tail line.
        def fake(prompt: str) -> str:
            if "FANOUT_AUDIT" in prompt or "SIBLING_CALL_AUDIT" in prompt:
                return "_no fanout audit needed_"
            return "some findings"

        recital = run_recital("some diff", claude_runner=fake)
        # Bare tail line for the fanout section.
        self.assertIn("_no fanout audit needed_", recital)
        # Convention still gets the wrapped format.
        self.assertIn("_Convention check (evidence):_", recital)
        self.assertIn("_Convention check: run_", recital)

    def test_recital_contains_both_sections(self):
        def fake(prompt: str) -> str:
            return f"reviewer output for prompt starting with: {prompt[:40]}"

        recital = run_recital("some diff", claude_runner=fake)
        # Fanout section (whichever name).
        self.assertTrue(
            "_Fanout audit (evidence):_" in recital or "_Sibling-call audit (evidence):_" in recital
        )
        # Convention section.
        self.assertIn("_Convention check (evidence):_", recital)

    def test_bare_short_circuit_reply_is_accepted(self):
        # Reviewer might emit the short-circuit tail without the `_..._` italics wrappers.
        # v1 first-live-run observed exactly this on the fanout side.
        def fake(prompt: str) -> str:
            # Only fanout short-circuits; convention returns findings so it wraps normally.
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "no sibling-call audit needed"  # bare, no underscores
            return "- **file:1** foo [**should reuse**]"

        recital = run_recital("some diff", claude_runner=fake)
        # Fanout should be the bare tail, NOT wrapped in an evidence fold.
        self.assertRegex(recital, r"_(?:Fanout audit|Sibling-call audit): no [\w -]+ needed_")
        self.assertNotIn("Fanout audit (evidence):", recital)
        self.assertNotIn("Sibling-call audit (evidence):", recital)

    def test_trailing_tail_line_in_output_is_stripped(self):
        # Even with the "no tail line" directive in the prompt, the subagent sometimes emits
        # one anyway (v1 first-live-run observed this on the convention side). Recital must
        # strip it so its own wrapper tail isn't a duplicate.
        def fake(prompt: str) -> str:
            return (
                "- **file.jl:1** — foo [**should reuse**]\n\n"
                "_Convention check: run_"
            )

        recital = run_recital("some diff", claude_runner=fake)
        # Convention section should have exactly ONE tail line for convention check.
        # (Fanout section has its own separate tail; count only convention.)
        convention_tail_count = recital.count("_Convention check: run_")
        self.assertEqual(convention_tail_count, 1,
                         f"expected 1 convention tail, got {convention_tail_count}:\n{recital}")


class FindingsEmissionTest(unittest.TestCase):
    """P1 of FINDINGS_EMISSION_PLAN.md — recital parses reviewer output for outcome-tag-
    requiring findings and emits one `_finding` row per matched bullet."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        self._env_patch = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        self._pr_patch = mock.patch("cecelia.effectiveness.recital._current_pr", return_value=None)
        self._pr_patch.start()
        self.addCleanup(self._pr_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def test_confirmed_fanout_finding_emits_one_finding_row(self):
        # Fanout `**confirmed**` bullet — the outcome-tag-requiring marker. Must produce
        # exactly one `_finding` row (in addition to the `_run` event).
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return (
                    "- **app/src/gating/handler.jl:88** — `resolve_ref` sibling not "
                    "updated to null-check [**confirmed**]"
                )
            return "no convention check needed"

        run_recital("some diff", claude_runner=fake)

        finding_events = [
            e for e in self._events()
            if e["event"] in {"fanout_audit_finding", "sibling_audit_finding"}
        ]
        self.assertEqual(len(finding_events), 1)
        row = finding_events[0]
        self.assertEqual(row["payload"]["file"], "app/src/gating/handler.jl")
        self.assertEqual(row["payload"]["line"], 88)
        self.assertEqual(row["payload"]["marker"], "confirmed")
        # Pending — no outcome yet (Decision 2 of FINDINGS_EMISSION_PLAN.md).
        self.assertNotIn("outcome", row["payload"])
        # Slug looks like `fanout-<8 hex>` per Decision 3.
        self.assertRegex(row["payload"]["slug"], r"^fanout-[0-9a-f]{8}$")

    def test_should_reuse_convention_finding_emits_one_finding_row(self):
        def fake(prompt: str) -> str:
            if "CONVENTION_CHECK" in prompt:
                return (
                    "- **frontend/src/panels/Foo.vue:12** — added `handleZarr`, closest "
                    "canonical `zarr_utils.open_as_zarr` (python/…), duplicate zarr access "
                    "[**should reuse**]"
                )
            return "no sibling-call audit needed"

        run_recital("some diff", claude_runner=fake)

        finding_events = [e for e in self._events() if e["event"] == "convention_check_finding"]
        self.assertEqual(len(finding_events), 1)
        row = finding_events[0]
        self.assertEqual(row["payload"]["file"], "frontend/src/panels/Foo.vue")
        self.assertEqual(row["payload"]["line"], 12)
        self.assertEqual(row["payload"]["marker"], "should reuse")
        self.assertRegex(row["payload"]["slug"], r"^conv-[0-9a-f]{8}$")

    def test_plausible_and_potential_duplicate_are_not_emitted(self):
        # Only outcome-tag-requiring markers produce `_finding` rows — the pending↔resolution
        # contract stays 1:1 with the hook's tag-count check.
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:1** — maybe [**plausible**]"
            return "- **bar.vue:2** — hmm [**potential duplicate**]"

        run_recital("some diff", claude_runner=fake)

        finding_events = [e for e in self._events() if e["event"].endswith("_finding")]
        self.assertEqual(finding_events, [])
        # But _run events still fire — the reviewers ran.
        run_events = [e for e in self._events() if e["event"].endswith("_run")]
        self.assertEqual(len(run_events), 2)

    def test_multiple_findings_in_one_reviewer_output_each_emit(self):
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return (
                    "- **a.jl:1** — first [**confirmed**]\n"
                    "- **b.jl:2** — second [**confirmed**]\n"
                    "- **c.jl:3** — hedged [**plausible**]"
                )
            return "no convention check needed"

        run_recital("some diff", claude_runner=fake)

        finding_events = [
            e for e in self._events()
            if e["event"] in {"fanout_audit_finding", "sibling_audit_finding"}
        ]
        self.assertEqual(len(finding_events), 2)  # plausible dropped
        self.assertEqual({e["payload"]["file"] for e in finding_events}, {"a.jl", "b.jl"})

    def test_slug_is_deterministic_across_runs(self):
        # Same finding on the same line → same slug across recital re-runs, so an author can
        # quote a stale slug from a previous recital.
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:42** — bar [**confirmed**]"
            return "no convention check needed"

        run_recital("same diff", claude_runner=fake)
        first_slug = [e for e in self._events() if e["event"].endswith("_finding")][0]["payload"]["slug"]

        # Second run — new log file to isolate, but the slug should match.
        self.log_path.unlink()
        run_recital("same diff", claude_runner=fake)
        second_slug = [e for e in self._events() if e["event"].endswith("_finding")][0]["payload"]["slug"]

        self.assertEqual(first_slug, second_slug)

    def test_recital_body_prefixes_slug_on_confirmed_finding(self):
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:42** — bar [**confirmed**]"
            return "no convention check needed"

        recital = run_recital("some diff", claude_runner=fake)
        # The bullet in the rendered recital body starts with `[slug]` so the author can copy
        # it directly into a `[slug: outcome]` pair.
        self.assertRegex(recital, r"- \[fanout-[0-9a-f]{8}\] \*\*foo\.jl:42\*\* — bar \[\*\*confirmed\*\*\]")

    def test_pr_context_captured_when_available(self):
        # When `gh pr view` returns a number, both `_run` and `_finding` rows carry `pr`.
        with mock.patch("cecelia.effectiveness.recital._current_pr", return_value="#9999"):
            def fake(prompt: str) -> str:
                if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                    return "- **foo.jl:1** — bar [**confirmed**]"
                return "no convention check needed"

            run_recital("some diff", claude_runner=fake)

        for e in self._events():
            self.assertEqual(e["pr"], "#9999")

    def test_reviewer_failure_does_not_attempt_finding_parse(self):
        # If the reviewer crashes, output is empty and no finding rows should appear —
        # only the `_run` event with `error` in payload (existing contract).
        def fake_that_fails(prompt: str) -> str:
            raise RecitalError("boom")

        run_recital("some diff", claude_runner=fake_that_fails)

        finding_events = [e for e in self._events() if e["event"].endswith("_finding")]
        self.assertEqual(finding_events, [])


if __name__ == "__main__":
    unittest.main()
