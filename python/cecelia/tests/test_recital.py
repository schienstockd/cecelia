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
  `**plausible**` / `**potential duplicate**` land as slug-less `_advisory` rows instead, so the
  console shows them while pending↔resolution stays 1:1 with the hook's tag-count check (per
  FINDINGS_EMISSION_PLAN.md Decision 2).
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
        # Avoid real `gh pr view` / `git rev-parse` subprocess spawns in every test;
        # individual tests that exercise capture patch these explicitly.
        self._pr_patch = mock.patch("cecelia.effectiveness.recital._current_pr", return_value=None)
        self._pr_patch.start()
        self.addCleanup(self._pr_patch.stop)
        self._sha_patch = mock.patch(
            "cecelia.effectiveness.recital._current_head_sha", return_value=None,
        )
        self._sha_patch.start()
        self.addCleanup(self._sha_patch.stop)
        self._branch_patch = mock.patch(
            "cecelia.effectiveness.recital._current_branch", return_value=None,
        )
        self._branch_patch.start()
        self.addCleanup(self._branch_patch.stop)

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

        # Failure recording covers the reviewers; the mechanical inventory check
        # can't fail via a runner (it's a local grep) so it's out of scope for this test.
        reviewer_events = [
            e for e in self._events()
            if e["event"] in {"fanout_audit_run", "convention_check_run"}
        ]
        self.assertEqual(len(reviewer_events), 2)
        for e in reviewer_events:
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
        self._sha_patch = mock.patch(
            "cecelia.effectiveness.recital._current_head_sha", return_value=None,
        )
        self._sha_patch.start()
        self.addCleanup(self._sha_patch.stop)
        self._branch_patch = mock.patch(
            "cecelia.effectiveness.recital._current_branch", return_value=None,
        )
        self._branch_patch.start()
        self.addCleanup(self._branch_patch.stop)

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

    def test_wrong_home_convention_finding_emits_one_finding_row(self):
        def fake(prompt: str) -> str:
            if "CONVENTION_CHECK" in prompt:
                return (
                    "- **app/src/tasks/x.jl:7** — comment recites a rejected alternative, belongs "
                    "in X_PLAN.md → Locked decisions, not source [**wrong home**]\n"
                    "- **app/src/y.jl:3** — added `foo`, closest canonical `bar` [**should reuse**]"
                )
            return "no sibling-call audit needed"

        body = run_recital("some diff", claude_runner=fake)

        rows = [e["payload"] for e in self._events() if e["event"] == "convention_check_finding"]
        self.assertEqual([(r["file"], r["marker"]) for r in rows],
                         [("app/src/tasks/x.jl", "wrong home"), ("app/src/y.jl", "should reuse")])
        # Both got slugs, and neither tripped the unparsed-marker tripwire.
        self.assertEqual(body.count("- [conv-"), 2)
        self.assertNotIn("PARSE WARNING", body)

    def test_plausible_and_potential_duplicate_emit_advisory_rows_only(self):
        # Advisory markers get no `_finding` row (the pending↔resolution contract stays 1:1 with
        # the hook's tag-count check) but do get a slug-less `_advisory` row, so the console
        # shows them.
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:1** — maybe [**plausible**]"
            return "- **bar.vue:2** — hmm [**potential duplicate**]"

        body = run_recital("some diff", claude_runner=fake)

        finding_events = [e for e in self._events() if e["event"].endswith("_finding")]
        self.assertEqual(finding_events, [])
        advisories = [(e["event"], e["payload"]) for e in self._events()
                      if e["event"].endswith("_advisory")]
        self.assertEqual(advisories, [
            ("fanout_audit_advisory",
             {"file": "foo.jl", "line": 1, "desc": "maybe", "marker": "plausible"}),
            ("convention_check_advisory",
             {"file": "bar.vue", "line": 2, "desc": "hmm", "marker": "potential duplicate"}),
        ])
        # No slug in the body either — nothing for the author to tag.
        self.assertNotIn("- [fanout-", body)
        self.assertNotIn("- [conv-", body)
        reviewer_run_events = [
            e for e in self._events()
            if e["event"] in {"fanout_audit_run", "convention_check_run"}
        ]
        self.assertEqual(len(reviewer_run_events), 2)

    def test_real_plausible_bullet_shape_emits_advisory(self):
        # A real reviewer bullet shape: backticked symbol, `:LINE` in bold, the
        # marker tag bolded inside the brackets at the end of a long line.
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return (
                    "- **python/cecelia/effectiveness/console.py:509** — `_finding_head_line`, "
                    "activity pane. It still calls the head line with the default "
                    "`with_context=True`. [**plausible**]"
                )
            return "no convention check needed"

        run_recital("some diff", claude_runner=fake)

        rows = [e["payload"] for e in self._events() if e["event"] == "fanout_audit_advisory"]
        self.assertEqual([(r["file"], r["line"]) for r in rows],
                         [("python/cecelia/effectiveness/console.py", 509)])

    def test_advisory_marker_off_bullet_surfaces_parse_warning(self):
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "The activity pane may need the same fix **plausible**."
            return "no convention check needed"

        body = run_recital("some diff", claude_runner=fake)

        self.assertIn("advisory marker(s)", body)
        self.assertEqual([e for e in self._events() if e["event"].endswith("_advisory")], [])

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
        self.assertEqual(len(finding_events), 2)  # plausible is an advisory, not a finding
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

    def test_real_drifted_bullet_shapes_all_emit(self):
        # Verbatim shapes from reviewer outputs the strict regex dropped (sessions 4f331a3b,
        # b8b65e0d, 134ef9fb, 94ccd376, 2c44f2fa — 11 of 29 findings lost, none since
        # 2026-09-28 reached the log). Each must yield exactly one finding + slug.
        drifted = [
            ("- **python/cecelia/effectiveness/console.py:245-246** — `_Tally.add` bumps runs "
             "[**confirmed**]", "python/cecelia/effectiveness/console.py", 245),
            ("- **docs/archive/CLAUDE_MD_EVAL_PLAN.md:11, :309**: the cadence text still says "
             '"weekly Wednesday 00:00". [**confirmed**]', "docs/archive/CLAUDE_MD_EVAL_PLAN.md", 11),
            ("- **scripts/claude_md_eval/systemd/claude-md-eval.timer:8**: \"Pass takes ~15 min\" "
             "[**confirmed**]", "scripts/claude_md_eval/systemd/claude-md-eval.timer", 8),
            ("- **`python/cecelia/effectiveness/rollup.py:207`** — added `_pr_from_branch` "
             "[**confirmed**]", "python/cecelia/effectiveness/rollup.py", 207),
            ("- **scripts/claude_md_eval/run_plugin_eval_suite.py:44,49** — added "
             "`_list_case_ids` [**confirmed**]", "scripts/claude_md_eval/run_plugin_eval_suite.py", 44),
        ]

        def fake(prompt: str) -> str:
            if "FANOUT" in prompt:
                return "\n".join(line for line, _, _ in drifted)
            return "no convention check needed"

        recital = run_recital("some diff", claude_runner=fake)
        rows = [e["payload"] for e in self._events() if e["event"] == "fanout_audit_finding"]
        self.assertEqual([(r["file"], r["line"]) for r in rows],
                         [(f, n) for _, f, n in drifted])
        self.assertEqual(len({r["slug"] for r in rows}), len(drifted))
        for r in rows:
            self.assertIn(f"- [{r['slug']}] **", recital)
        self.assertNotIn("PARSE WARNING", recital)

    def test_range_suffix_keeps_single_line_slug(self):
        # `foo.jl:42-50` slugs the same as `foo.jl:42` — a reviewer re-run that switches to a
        # range must not orphan the slug the author already quoted.
        def fake_for(loc):
            def fake(prompt: str) -> str:
                if "FANOUT" in prompt:
                    return f"- **{loc}** — bar [**confirmed**]"
                return "no convention check needed"
            return fake

        run_recital("d", claude_runner=fake_for("foo.jl:42"))
        run_recital("d", claude_runner=fake_for("foo.jl:42-50"))
        slugs = [e["payload"]["slug"] for e in self._events() if e["event"] == "fanout_audit_finding"]
        self.assertEqual(len(slugs), 2)
        self.assertEqual(slugs[0], slugs[1])

    def test_location_less_bullet_still_emits_with_distinct_slugs(self):
        def fake(prompt: str) -> str:
            if "CONVENTION_CHECK" in prompt:
                return ("- added a JSON writer, use write_json_atomic [**should reuse**]\n"
                        "- added a zarr opener, use open_as_zarr [**should reuse**]")
            return "no fanout audit needed"

        run_recital("d", claude_runner=fake)
        rows = [e["payload"] for e in self._events() if e["event"] == "convention_check_finding"]
        self.assertEqual(len(rows), 2)
        self.assertEqual({r["file"] for r in rows}, {"?"})
        self.assertNotEqual(rows[0]["slug"], rows[1]["slug"])

    def test_marker_with_trailing_text_in_brackets_emits(self):
        # Session 290e2f6e: `[**confirmed**, no action]` — exact-match parsing dropped it.
        def fake(prompt: str) -> str:
            if "FANOUT" in prompt:
                return ("- **frontend/src/modules/cluster/ClusterHeatmapPanel.vue:54,143**: "
                        "`plotDataToCsv` called without meta. [**confirmed**, no action]")
            return "no convention check needed"

        recital = run_recital("d", claude_runner=fake)
        rows = [e["payload"] for e in self._events() if e["event"] == "fanout_audit_finding"]
        self.assertEqual([(r["file"], r["line"]) for r in rows],
                         [("frontend/src/modules/cluster/ClusterHeatmapPanel.vue", 54)])
        self.assertIn(f"- [{rows[0]['slug']}] **", recital)
        self.assertNotIn("PARSE WARNING", recital)

    def test_marker_quoted_in_backticks_is_not_a_finding(self):
        # 2026-09-30: a "checked and clear" bullet quoting the tag shape became a finding.
        def fake(prompt: str) -> str:
            if "FANOUT" in prompt:
                return ("- **Checked and clear:** `hook.py:82` counts `[**confirmed**, …]` the "
                        "same way the new parser does.")
            return "no convention check needed"

        recital = run_recital("d", claude_runner=fake)
        self.assertEqual([e for e in self._events() if e["event"] == "fanout_audit_finding"], [])
        self.assertNotIn("PARSE WARNING", recital)

    def test_unknown_bold_marker_shape_surfaces_parse_warning(self):
        # Any bold marker that didn't become a finding trips the warning, not just `[**x**]`.
        def fake(prompt: str) -> str:
            if "FANOUT" in prompt:
                return "- **foo.jl:3** — shape (**confirmed**)"
            return "no convention check needed"

        self.assertIn("RECITAL PARSE WARNING", run_recital("d", claude_runner=fake))

    def test_marker_off_bullet_surfaces_parse_warning(self):
        # Tripwire for the next drift: a marker that isn't on a bullet line can't be slugged,
        # so the recital says so instead of dropping it.
        def fake(prompt: str) -> str:
            if "FANOUT" in prompt:
                return "1. **foo.jl:3** — numbered list, not a bullet [**confirmed**]"
            return "no convention check needed"

        recital = run_recital("d", claude_runner=fake)
        self.assertIn("RECITAL PARSE WARNING", recital)
        self.assertIn("1 finding marker(s)", recital)

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

    def test_head_sha_captured_when_available(self):
        # When `git rev-parse HEAD` returns a SHA, both `_run` and `_finding` rows carry it in
        # the row-level `commit` field. That is what the SHA-anchored gate in
        # `.claude/hooks/check_commit_recital.py` matches against.
        # Scoped to fanout+convention events — `inventory_coverage_run` is emitted by a separate
        # helper (`inventory_coverage.run_inventory_check`), pinned in its own test file.
        head = "a" * 40
        reviewer_events = {"fanout_audit_run", "convention_check_run",
                           "fanout_audit_finding", "convention_check_finding"}
        with mock.patch(
            "cecelia.effectiveness.recital._current_head_sha", return_value=head,
        ):
            def fake(prompt: str) -> str:
                if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                    return "- **foo.jl:1** — bar [**confirmed**]"
                return "no convention check needed"

            run_recital("some diff", claude_runner=fake)

        for e in self._events():
            if e["event"] not in reviewer_events:
                continue
            self.assertEqual(e["commit"], head)

    def test_branch_captured_when_available(self):
        # `_current_branch()` result lands on every emitted row so the rollup can join to a
        # PR later via `gh pr list --head <branch>` — even when `pr` is null at write time.
        with mock.patch(
            "cecelia.effectiveness.recital._current_branch",
            return_value="feat/log-branch-capture",
        ):
            def fake(prompt: str) -> str:
                if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                    return "- **foo.jl:1** — bar [**confirmed**]"
                return "no convention check needed"

            run_recital("some diff", claude_runner=fake)

        for e in self._events():
            self.assertEqual(e["branch"], "feat/log-branch-capture")

    def test_branch_none_leaves_field_null(self):
        # No repo / detached HEAD → row lands with branch=null; rollup skips the gh lookup
        # for that row (can't join without a branch name).
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:1** — bar [**confirmed**]"
            return "no convention check needed"

        run_recital("some diff", claude_runner=fake)

        for e in self._events():
            self.assertIsNone(e["branch"])

    def test_head_sha_none_leaves_commit_null(self):
        # No repo / no `git` → the row still lands (best-effort), commit is null. The hook's
        # SHA gate degrades to allow when head_sha is None, so this doesn't strand commits.
        reviewer_events = {"fanout_audit_run", "convention_check_run",
                           "fanout_audit_finding", "convention_check_finding"}
        def fake(prompt: str) -> str:
            if "SIBLING_CALL" in prompt or "FANOUT" in prompt:
                return "- **foo.jl:1** — bar [**confirmed**]"
            return "no convention check needed"

        run_recital("some diff", claude_runner=fake)  # setUp already patches sha to None

        for e in self._events():
            if e["event"] not in reviewer_events:
                continue
            self.assertIsNone(e["commit"])


class DefaultRunnerEnvTest(unittest.TestCase):
    def test_reviewer_spawn_disables_observer_pairing(self):
        # A reviewer turn must not re-pair the user's project (observer MCP inherits env).
        from cecelia.effectiveness import recital
        done = mock.Mock(returncode=0, stdout="ok", stderr="")
        with mock.patch.object(recital, "_resolve_claude_bin", return_value="/bin/claude"), \
             mock.patch.object(recital.subprocess, "run", return_value=done) as run:
            recital._default_runner("prompt", timeout=5)
        self.assertEqual(run.call_args.kwargs["env"]["CECELIA_OBSERVER_NO_PAIR"], "1")


if __name__ == "__main__":
    unittest.main()


class DefaultRunnerTest(unittest.TestCase):
    """`_default_runner` passes the prompt on stdin and turns a spawn failure into
    `RecitalError`, so `_run_reviewer` logs an errored `_run` row instead of crashing."""

    def test_prompt_goes_over_stdin_not_argv(self):
        from cecelia.effectiveness import recital as r
        big = "x" * 300_000  # > Linux MAX_ARG_STRLEN (128 KB) — E2BIG if it were argv
        done = mock.Mock(returncode=0, stdout="ok\n", stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done) as run:
            self.assertEqual(r._default_runner(big), "ok")
        argv, kwargs = run.call_args.args[0], run.call_args.kwargs
        self.assertEqual(argv, ["/bin/claude", "-p"])
        self.assertEqual(kwargs["input"], big)

    def test_spawn_oserror_becomes_recital_error(self):
        from cecelia.effectiveness import recital as r
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", side_effect=OSError(7, "Argument list too long")):
            with self.assertRaises(RecitalError):
                r._default_runner("p")
