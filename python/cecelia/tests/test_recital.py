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


if __name__ == "__main__":
    unittest.main()
