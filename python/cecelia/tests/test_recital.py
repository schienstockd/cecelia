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
from cecelia.effectiveness.claude_cli import rate_limit
from cecelia.effectiveness.recital import STAMP_RE, RecitalError, ReviewerReply, _slug, recital_stamp, reviewer_errors, run_recital


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

    def test_reviewer_errors_reads_only_the_error_blocks(self):
        # `judge-review --auto` reads a usage limit from these: a finding beside them quoting a limit's
        # wording must not look like one, and a real limit must still be found
        quoting = "- **claude_cli.py:44** — the regex now matches \"You've hit your session limit · resets 1:40am\" [**plausible**]"

        def one_errors(prompt: str) -> str:
            if "FANOUT" in prompt:
                raise RecitalError("claude exited 1; stderr:\nconnection reset\nby peer")
            return quoting

        body = run_recital("some diff", claude_runner=one_errors)
        self.assertIn("session limit", body)
        self.assertEqual(reviewer_errors(body), ["claude exited 1; stderr:\nconnection reset\nby peer"])
        self.assertIsNone(rate_limit({"is_error": True, "result": "\n".join(reviewer_errors(body))}))

        def limited(prompt: str) -> str:
            raise RecitalError("claude exited 1; stdout:\nYou've hit your session limit · resets 1:40am")

        body = run_recital("some diff", claude_runner=limited)
        self.assertEqual(len(reviewer_errors(body)), 2)
        self.assertIsNotNone(rate_limit({"is_error": True, "result": "\n".join(reviewer_errors(body))}))
    def test_run_event_carries_tokens_and_cost_from_the_reply(self):
        counts = {"input": 2, "cache_write": 100, "cache_read": 500, "output": 40}
        run_recital("some diff", claude_runner=lambda _p: ReviewerReply("no findings", counts, 0.01234))
        rows = [e for e in self._events() if e["event"] in {"fanout_audit_run", "convention_check_run"}]
        self.assertEqual(len(rows), 2)
        for e in rows:
            self.assertEqual(e["payload"]["tokens"], counts)
            self.assertEqual(e["payload"]["cost_usd"], 0.0123)

    def test_run_event_records_the_model_the_reviewer_ran_on(self):
        # Without a model field the log couldn't show which model a reviewer ran on.
        reply = ReviewerReply("no findings", {}, 0.01, ("claude-sonnet-5-5",))
        run_recital("some diff", claude_runner=lambda _p: reply)
        rows = [e for e in self._events() if e["event"] in {"fanout_audit_run", "convention_check_run"}]
        self.assertEqual([e["payload"]["models"] for e in rows], [["claude-sonnet-5-5"]] * 2)

    def test_a_bare_string_reply_logs_no_tokens(self):
        run_recital("some diff", claude_runner=lambda _p: "no findings")
        for e in self._events():
            if e["event"] in {"fanout_audit_run", "convention_check_run"}:
                self.assertNotIn("tokens", e["payload"])
                self.assertNotIn("cost_usd", e["payload"])

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
        # and no "no … needed" tail under it: that would say the reviewer ran and cleared the diff
        self.assertNotIn("no fanout audit needed", recital)
        self.assertNotIn("no convention check needed", recital)
        self.assertIn("_Fanout audit: not run — the reviewer errored_", recital)

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

    def test_body_ends_with_the_stamp_of_the_change_it_reviewed(self):
        # The text names its own change (the log rows always did), so the commit hook can tell a
        # body produced for another branch from this one's.
        sha = "c" * 40
        with mock.patch("cecelia.effectiveness.recital._current_head_sha", return_value=sha), \
             mock.patch("cecelia.effectiveness.recital._current_branch", return_value="feat/x"):
            recital = run_recital("some diff", claude_runner=lambda _p: "no findings")
        last = recital.rstrip().splitlines()[-1]
        self.assertEqual(last, recital_stamp("feat/x", sha))
        m = STAMP_RE.fullmatch(last)
        self.assertIsNotNone(m)
        self.assertEqual((m.group(1), m.group(2)), ("feat/x", sha[:12]))

    def test_stamp_marks_what_git_could_not_report(self):
        self.assertEqual(recital_stamp(None, None), "_Recital stamp: ?@?_")
        self.assertIsNotNone(STAMP_RE.fullmatch(recital_stamp(None, None)))


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
        # the hook's tag-count check) but do get an `_advisory` row, so the console shows them.
        # A plausible note's carries the slug the author MAY tag fixed and the bug sweep keys it by.
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
             {"file": "foo.jl", "line": 1, "desc": "maybe", "marker": "plausible",
              "slug": _slug("fanout", "foo.jl", 1, "plausible", "maybe")}),
            ("convention_check_advisory",
             {"file": "bar.vue", "line": 2, "desc": "hmm", "marker": "potential duplicate"}),
        ])
        # The plausible note shows its slug and the optional-tag note; a potential duplicate, which
        # no judge step re-checks, shows neither.
        self.assertIn(f"- [{_slug('fanout', 'foo.jl', 1, 'plausible', 'maybe')}] ", body)
        self.assertIn("Plausible notes need no tag", body)
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


def _diff_of(path: str) -> str:
    return (f"diff --git a/{path} b/{path}\n--- a/{path}\n+++ b/{path}\n"
            "@@ -1,1 +1,1 @@\n-old\n+new\n")


class ResultReviewTest(unittest.TestCase):
    """The third reviewer (`docs/ai-assist/RESULT_REVIEW.md`): gated on result paths, three markers."""

    setUp = FindingsEmissionTest.setUp
    _events = FindingsEmissionTest._events

    @staticmethod
    def _fake(result_reply: str):
        prompts = []

        def fake(prompt: str) -> str:
            prompts.append(prompt)
            return result_reply if "RESULT_REVIEW.md" in prompt else "no findings"
        return fake, prompts

    def test_a_diff_with_no_result_path_skips_and_says_so(self):
        fake, prompts = self._fake("must not be asked")
        body = run_recital(_diff_of("docs/DEV.md"), claude_runner=fake)
        self.assertIn("_Result review: skipped — no result paths_", body)
        self.assertFalse(any("RESULT_REVIEW.md" in p for p in prompts))
        rows = [e for e in self._events() if e["event"] == "result_review_run"]
        self.assertEqual([r["payload"]["escape_valve"] for r in rows], ["no_result_paths"])

    def test_a_test_file_under_a_result_directory_is_not_a_result_path(self):
        fake, prompts = self._fake("must not be asked")
        body = run_recital(_diff_of("python/cecelia/utils/tests/test_x.py"), claude_runner=fake)
        self.assertIn("skipped — no result paths", body)

    def test_frontend_code_that_feeds_a_result_is_gated_in(self):
        # Two of the backtest's four bugs were frontend: a task form's defaults and the viewer picker.
        from cecelia.effectiveness.recital import _RESULT_DOC, result_paths, touched_result_paths
        doc = (pathlib.Path(__file__).resolve().parents[3] / _RESULT_DOC).read_text(encoding="utf-8")
        globs = result_paths(doc)
        for path in ("frontend/src/tasks/paramValues.ts", "frontend/src/utils/viewerPick.ts",
                     "python/cecelia/utils/coastal_utils.py", "app/src/tasks/segment/coastal.json"):
            self.assertEqual(touched_result_paths(_diff_of(path), globs), [path], path)

    def test_markers_become_a_finding_two_advisories_and_the_claim_reaches_the_prompt(self):
        reply = ("- **python/cecelia/utils/coastal_utils.py:452** — two groups share a cache → "
                 "group 1 gets group 0's flow. Repro: _review/r.py, max diff 1.0 [**reproduced**]\n"
                 "- **python/cecelia/utils/coastal_utils.py:330** — clip range moves [**inferred**]\n"
                 "- **frontend/src/x.ts:9** — ribbon not drawn [**display**]")
        fake, prompts = self._fake(reply)
        body = run_recital(_diff_of("python/cecelia/utils/coastal_utils.py"), claude_runner=fake,
                           claim="reuse the flow between timepoints")
        self.assertIn('UNVERIFIED: "reuse the flow between timepoints"', next(p for p in prompts if "RESULT_REVIEW.md" in p))
        found = [e["payload"] for e in self._events() if e["event"] == "result_review_finding"]
        self.assertEqual([(f["line"], f["marker"]) for f in found], [(452, "reproduced")])
        self.assertTrue(found[0]["slug"].startswith("result-"))
        adv = [e["payload"] for e in self._events() if e["event"] == "result_review_advisory"]
        self.assertEqual([a["marker"] for a in adv], ["inferred", "display"])
        self.assertIn("slug", adv[0])      # inferred may be tagged fixed, so it carries a slug
        self.assertNotIn("slug", adv[1])   # display is listed only
        self.assertIn(f"[{found[0]['slug']}]", body)
        self.assertIn("Inferred notes need no tag", body)
        self.assertIn("_Result review: run_", body)


class DefaultRunnerEnvTest(unittest.TestCase):
    def test_reviewer_spawn_disables_observer_pairing(self):
        # A reviewer turn must not re-pair the user's project (observer MCP inherits env).
        from cecelia.effectiveness import recital
        done = mock.Mock(returncode=0, stdout='{"result": "ok"}', stderr="")
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
        done = mock.Mock(returncode=0, stdout='{"result": "ok\\n"}', stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done) as run:
            self.assertEqual(r._default_runner(big).text, "ok")
        argv, kwargs = run.call_args.args[0], run.call_args.kwargs
        self.assertEqual(argv, ["/bin/claude", "-p", "--model", "opus", "--output-format", "json"])
        self.assertEqual(kwargs["input"], big)

    def test_a_failure_with_empty_stderr_shows_stdout(self):
        # `claude -p` prints a usage-limit refusal on stdout and exits 1 with stderr empty
        from cecelia.effectiveness import recital as r
        done = mock.Mock(returncode=1, stdout="You've hit your session limit · resets 1:40am\n", stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done):
            with self.assertRaisesRegex(RecitalError, "stdout:\nYou've hit your session limit"):
                r._default_runner("p")
        # a warning on stderr doesn't hide the limit on stdout
        done.stderr = "Warning: something\n"
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done):
            with self.assertRaisesRegex(RecitalError, "stderr:\nWarning: something\nstdout:\nYou've hit your session"):
                r._default_runner("p")

    def test_the_reply_carries_the_cli_s_tokens_and_cost(self):
        from cecelia.effectiveness import recital as r
        out = ('{"type": "result", "is_error": false, "result": "- **a.py:1** — x [**confirmed**]", '
               '"total_cost_usd": 0.0412, "usage": {"input_tokens": 3, "cache_creation_input_tokens": 9000, '
               '"cache_read_input_tokens": 41000, "output_tokens": 812}}')
        done = mock.Mock(returncode=0, stdout=out, stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done):
            reply = r._default_runner("p")
        self.assertEqual(reply.text, "- **a.py:1** — x [**confirmed**]")
        self.assertEqual(reply.tokens, {"input": 3, "cache_write": 9000, "cache_read": 41000, "output": 812})
        self.assertEqual(reply.cost_usd, 0.0412)

    def test_an_error_result_with_exit_zero_is_not_read_as_findings(self):
        # `is_error` with exit 0: the CLI's message must not be parsed as the reviewer's answer
        from cecelia.effectiveness import recital as r
        done = mock.Mock(returncode=0, stdout='{"is_error": true, "result": "API Error: overloaded"}', stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done):
            with self.assertRaisesRegex(RecitalError, "stdout:\nAPI Error: overloaded"):
                r._default_runner("p")

    def test_a_limit_in_the_json_result_shows_the_message_not_the_json(self):
        from cecelia.effectiveness import recital as r
        done = mock.Mock(returncode=1, stdout='{"is_error": true, "result": "You\'ve hit your session limit · resets 1:40am"}',
                         stderr="")
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", return_value=done):
            with self.assertRaisesRegex(RecitalError, "stdout:\nYou've hit your session limit · resets 1:40am$"):
                r._default_runner("p")

    def test_spawn_oserror_becomes_recital_error(self):
        from cecelia.effectiveness import recital as r
        with mock.patch.object(r, "_resolve_claude_bin", return_value="/bin/claude"), \
                mock.patch.object(r.subprocess, "run", side_effect=OSError(7, "Argument list too long")):
            with self.assertRaises(RecitalError):
                r._default_runner("p")


class RecitalCliMergeTest(unittest.TestCase):
    """`pixi run recital` during a merge: main's reviewed code would yield findings nothing closes."""

    def _cli(self):
        import importlib.util
        path = pathlib.Path(__file__).resolve().parents[3] / "scripts" / "recital.py"
        spec = importlib.util.spec_from_file_location("recital_cli", path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod

    def test_a_merge_in_progress_skips_without_reviewing_or_logging(self):
        cli = self._cli()
        with mock.patch.object(cli, "in_sequencer_commit", return_value=True), \
             mock.patch.object(cli, "run_recital", side_effect=AssertionError("reviewed a merge")), \
             mock.patch.object(cli, "_get_diff", side_effect=AssertionError("read a merge diff")), \
             mock.patch.object(cli.sys, "argv", ["recital"]):
            self.assertEqual(cli.main(), 0)

    def test_stdin_still_reviews_what_it_is_given(self):
        cli = self._cli()
        with mock.patch.object(cli, "in_sequencer_commit", return_value=True), \
             mock.patch.object(cli, "_get_diff", return_value="diff --git a/x b/x\n"), \
             mock.patch.object(cli, "run_recital", return_value="body") as run, \
             mock.patch.object(cli, "recital_body_path", return_value=None), \
             mock.patch.object(cli.sys, "argv", ["recital", "--stdin"]):
            self.assertEqual(cli.main(), 0)
        run.assert_called_once()

    def test_body_is_saved_to_the_worktree_git_dir(self):
        # Agents append this file instead of fishing a temp file back out of /tmp.
        cli = self._cli()
        with tempfile.TemporaryDirectory() as td:
            body = pathlib.Path(td) / "recital-body.md"
            with mock.patch.object(cli, "in_sequencer_commit", return_value=False), \
                 mock.patch.object(cli, "_get_diff", return_value="diff --git a/x b/x\n"), \
                 mock.patch.object(cli, "run_recital", return_value="_Recital stamp: b@?_"), \
                 mock.patch.object(cli, "recital_body_path", return_value=body), \
                 mock.patch.object(cli.sys, "argv", ["recital"]):
                self.assertEqual(cli.main(), 0)
            self.assertEqual(body.read_text(encoding="utf-8"), "_Recital stamp: b@?_\n")

    def test_body_path_is_inside_the_git_dir(self):
        from cecelia.effectiveness import recital as r
        with mock.patch.object(r, "_git_dir", return_value=pathlib.Path("/repo/.git/worktrees/w")):
            self.assertEqual(r.recital_body_path(), pathlib.Path("/repo/.git/worktrees/w/recital-body.md"))
        with mock.patch.object(r, "_git_dir", return_value=None):
            self.assertIsNone(r.recital_body_path())


class RepeatRoundTest(unittest.TestCase):
    """A repeat round on the same change (branch @ parent SHA) reuses each reviewer's last section
    when the staged tree is unchanged, else reviews only the change since that round. Trees come
    from a real git repo: the delta is `git diff` between them."""

    def setUp(self):
        FindingsEmissionTest.setUp(self)
        self.repo = pathlib.Path(self._tmp.name) / "repo"
        self.repo.mkdir()
        self._git("init", "-q")
        cwd = os.getcwd()
        os.chdir(self.repo)  # `_tree_delta` runs git in the process cwd, as recital does
        self.addCleanup(os.chdir, cwd)
        self.rounds = pathlib.Path(self._tmp.name) / "rounds"
        self.prompts: list[str] = []
        self.reply = ("- **a.py:1** — two callers still read the old key [**confirmed**]\n\n"
                      "Evidence:\n- checked the callers in b.py")

    _events = FindingsEmissionTest._events

    def _git(self, *args: str) -> str:
        import subprocess
        return subprocess.run(["git", *args], cwd=self.repo, check=True, capture_output=True,
                              encoding="utf-8").stdout.strip()

    def _stage(self, text: str) -> str:
        (self.repo / "a.py").write_text(text, encoding="utf-8")
        self._git("add", "a.py")
        return self._git("write-tree")

    def _round(self, tree: str, *, sha: str = "a" * 40, fail: bool = False) -> str:
        def fake(prompt: str) -> str:
            self.prompts.append(prompt)
            if fail:
                raise RecitalError("claude exited 1; stdout:\nYou've hit your 5-hour limit")
            return self.reply if "FANOUT_AUDIT.md" in prompt else "no findings"
        with mock.patch("cecelia.effectiveness.recital._current_head_sha", return_value=sha), \
                mock.patch("cecelia.effectiveness.recital._current_branch", return_value="b"):
            return run_recital("some diff", claude_runner=fake, staged_tree=tree, rounds_dir=self.rounds)

    def _runs(self, event: str = "fanout_audit_run") -> list[dict]:
        return [e["payload"] for e in self._events() if e["event"] == event]

    def test_an_unchanged_tree_reuses_every_reviewer_without_spawning(self):
        tree = self._stage("x = 1\n")
        first = self._round(tree)
        self.prompts.clear()
        again = self._round(tree)
        self.assertEqual(self.prompts, [])
        self.assertIn("_Fanout audit: run — reused, staged diff unchanged_", again)
        self.assertIn(first.split("_Fanout audit (evidence):_")[1].split("[")[1][:15], again)  # same slug
        for ev in ("fanout_audit_run", "convention_check_run"):
            self.assertEqual(self._runs(ev)[-1].get("escape_valve"), "unchanged_diff", ev)

    def test_a_changed_tree_reviews_only_the_delta_and_carries_earlier_findings(self):
        self._round(self._stage("x = 1\n"))
        slug = next(e["payload"]["slug"] for e in self._events() if e["event"] == "fanout_audit_finding")
        self.prompts.clear()
        self.reply = "no fanout audit needed"
        body = self._round(self._stage("x = 1\ny = 2\n"))
        fanout_prompt = next(p for p in self.prompts if "FANOUT_AUDIT.md" in p)
        self.assertIn("REPEAT ROUND", fanout_prompt)
        delta = fanout_prompt.split("Change since the earlier round:")[1]
        self.assertIn("+y = 2", delta)
        self.assertNotIn("+x = 1", delta)
        already = fanout_prompt.split("Already reported:")[1].split("Change since")[0]
        self.assertIn(f"[{slug}]", already)                                        # told not to repeat it
        self.assertNotIn("Evidence:", already)
        self.assertNotIn("checked the callers", already)                           # evidence bullets aren't findings
        self.assertIn("earlier rounds on this change", body)
        self.assertIn(f"[{slug}]", body)                                           # still in front of the author
        self.assertEqual(self._runs()[-1].get("round"), "delta")

    def test_a_new_parent_starts_a_full_round(self):
        tree = self._stage("x = 1\n")
        self._round(tree)
        self.prompts.clear()
        self._round(self._stage("x = 2\n"), sha="c" * 40)   # committed: a new change
        self.assertTrue(self.prompts)
        self.assertFalse(any("REPEAT ROUND" in p for p in self.prompts))

    def test_an_errored_round_is_not_kept(self):
        tree = self._stage("x = 1\n")
        self._round(tree)
        self._round(self._stage("x = 1\ny = 2\n"), fail=True)
        self.prompts.clear()
        self._round(self._git("write-tree"))   # same tree as the failed round: must spawn again
        self.assertTrue(any("FANOUT_AUDIT.md" in p for p in self.prompts))
        self.assertTrue(any("REPEAT ROUND" in p for p in self.prompts))   # delta from the last good round

    def test_a_finding_that_quotes_the_error_marker_is_still_a_round(self):
        # A reviewer quoting `RECITAL SCRIPT ERROR` in a finding didn't error: its round is kept.
        self.reply = "- **a.py:1** — anchors on the `> **RECITAL SCRIPT ERROR** —` block [**confirmed**]"
        tree = self._stage("x = 1\n")
        self._round(tree)
        self.prompts.clear()
        self._round(tree)
        self.assertEqual(self.prompts, [])

    def test_a_tree_git_no_longer_has_falls_back_to_a_full_round(self):
        self._round(self._stage("x = 1\n"))
        self.prompts.clear()
        with mock.patch("cecelia.effectiveness.recital._tree_delta", return_value=None):
            body = self._round(self._stage("x = 2\n"))
        self.assertFalse(any("REPEAT ROUND" in p for p in self.prompts))
        self.assertNotIn("earlier rounds on this change", body)

    def test_no_staged_tree_means_no_rounds(self):
        tree = self._stage("x = 1\n")
        self._round(tree)
        self.prompts.clear()
        self._round(None)
        self.assertTrue(any("FANOUT_AUDIT.md" in p for p in self.prompts))


class RepeatProbingTest(unittest.TestCase):
    """Result-review rounds on one change that each reproduce a new input against the same file
    point the author at `RESULT_REVIEW.md` → *When rounds don't converge*."""

    NOTE = "_Repeat probing:"
    setUp = RepeatRoundTest.setUp
    _git = RepeatRoundTest._git
    _stage = RepeatRoundTest._stage

    def _result_round(self, tree: str, reply: str, change: str = "b@" + "a" * 40) -> str:
        from cecelia.effectiveness import recital
        rounds = recital.Rounds(self.rounds, change, tree)
        return recital._run_reviewer(
            event_name="result_review_run", finding_event_name="result_review_finding",
            advisory_event_name="result_review_advisory", mechanism="result", title="Result review",
            tail_none="no result review needed", doc_path="docs/ai-assist/RESULT_REVIEW.md",
            diff="some diff", claude_runner=lambda _p: reply, pr=None, commit=None, branch="b",
            prompt="review", rounds=rounds)

    def _repro(self, file: str, line: int) -> str:
        return f"- **{file}:{line}** — input {line} gives the wrong label [**reproduced**]"

    def _rounds(self, replies: list[str]) -> list[str]:
        return [self._result_round(self._stage(f"x = {i}\n"), r) for i, r in enumerate(replies)]

    def test_a_third_round_with_a_new_input_against_one_file_points_at_the_stop_rule(self):
        bodies = self._rounds([self._repro("u.py", n) for n in (10, 20, 30)])
        self.assertNotIn(self.NOTE, bodies[1])
        self.assertIn(self.NOTE, bodies[2])
        self.assertIn("`u.py`", bodies[2])
        self.assertIn("When rounds don't converge", bodies[2])

    def test_new_inputs_in_different_files_dont(self):
        bodies = self._rounds([self._repro(f, 10) for f in ("u.py", "v.py", "w.py")])
        self.assertNotIn(self.NOTE, bodies[2])

    def test_the_same_finding_reported_again_doesnt_count(self):
        # A full round after an errored one (or a reviewer repeating itself) re-reports the old input.
        with mock.patch("cecelia.effectiveness.recital._tree_delta", return_value=None):  # full rounds
            bodies = self._rounds([self._repro("u.py", 10)] * 3)
        self.assertNotIn(self.NOTE, bodies[2])

    def test_advisories_dont_count(self):
        inferred = "- **u.py:{}** — might mislabel [**inferred**]"
        bodies = self._rounds([inferred.format(n) for n in (10, 20, 30)])
        self.assertNotIn(self.NOTE, bodies[2])

    def test_a_commit_starts_the_count_again(self):
        self._rounds([self._repro("u.py", n) for n in (10, 20)])
        body = self._result_round(self._stage("x = 9\n"), self._repro("u.py", 30), change="b@" + "c" * 40)
        self.assertNotIn(self.NOTE, body)

    def test_backtest_sequences(self):
        # Files with a new reproduced finding per round, from the logged rounds of five branches.
        from cecelia.effectiveness.recital import _probe_note
        iu, oir = "python/cecelia/utils/intensity_utils.py", "python/cecelia/utils/oir_meta.py"
        probed_heuristic = [[iu], [iu], [], [iu], [], [iu], [iu, iu]]   # a classifier probed each round
        fired = [i for i in range(1, len(probed_heuristic) + 1) if _probe_note(probed_heuristic[:i])]
        self.assertEqual(fired[0], 4)                                    # the third round with a hit
        for real in ([[], [oir], [oir], [], [], []],                     # two real bugs, then clean
                     [["calibration.jl"], [], []],
                     [[iu], [], ["saturation_run.py"], []]):
            self.assertFalse(any(_probe_note(real[:i]) for i in range(1, len(real) + 1)), real)
