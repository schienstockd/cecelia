"""Tests for the effectiveness log's append + read + rollup contract.

The log itself is the substrate for the rollup script and (later) for usage-weighted spot-
check queries. The invariants pinned here are the ones that would silently corrupt the
downstream aggregates:

- **Unknown event types reject** — a mis-spelled `fanout_audit_findng` wouldn't be caught
  by the rollup script (which loops over known types), so it must be caught at write time.
- **Unknown outcomes reject** — same reason: an outcome outside the closed vocabulary is
  invisible to every aggregate that indexes by it.
- **Malformed lines skip cleanly on read** — a partial write at process kill would otherwise
  wedge every reader; the log must survive it.
- **The rollup handles zero events** — the very first invocation on a fresh install writes a
  placeholder public artifact, not a crash.
"""
from __future__ import annotations

import json
import pathlib
import tempfile
import unittest

from cecelia.effectiveness import (
    EVENT_TYPES,
    OUTCOME_VOCABULARY,
    SCHEMA_VERSION,
    UnknownEventError,
    UnknownOutcomeError,
    append_event,
    read_events,
    render_rollup,
)


class AppendEventTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"

    def _rows(self):
        return list(read_events(self.log_path))

    def test_append_writes_a_valid_row(self):
        row = append_event(
            "fanout_audit_run",
            {"hunks_reviewed": 3, "duration_s": 27.4},
            session="test-session",
            pr="#1234",
            commit="abc123",
            log_path=self.log_path,
        )
        self.assertEqual(row["schema_version"], SCHEMA_VERSION)
        self.assertEqual(row["event"], "fanout_audit_run")
        self.assertEqual(row["session"], "test-session")
        self.assertEqual(row["pr"], "#1234")
        self.assertEqual(row["payload"]["hunks_reviewed"], 3)
        self.assertTrue(row["ts"].endswith("Z"))

        rows = self._rows()
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0], row)

    def test_append_creates_parent_directory(self):
        # Nested path — parent doesn't exist yet.
        deep = pathlib.Path(self._tmp.name) / "a" / "b" / "c" / "events.jsonl"
        append_event("fanout_audit_run", {}, log_path=deep)
        self.assertTrue(deep.exists())

    def test_unknown_event_type_rejected(self):
        # A mis-spelled type must not silently corrupt aggregates.
        with self.assertRaises(UnknownEventError):
            append_event("fanout_audit_findng", {}, log_path=self.log_path)
        self.assertEqual(self._rows(), [])

    def test_unknown_outcome_rejected(self):
        # Outcomes outside the closed vocabulary would be invisible to every aggregate.
        with self.assertRaises(UnknownOutcomeError):
            append_event(
                "fanout_audit_finding",
                {"outcome": "kinda_fine"},
                log_path=self.log_path,
            )
        self.assertEqual(self._rows(), [])

    def test_every_event_type_and_outcome_is_writable(self):
        # The closed lists are load-bearing — an entry that can't round-trip is a bug.
        # (`retrospective_miss` has no `outcome` field per plan; omit for that one.)
        for event in EVENT_TYPES:
            payload = {"outcome": "fixed_pre_commit"} if event.endswith("_finding") or event == "ratchet_hit" else {}
            append_event(event, payload, log_path=self.log_path)
        rows = self._rows()
        self.assertEqual({r["event"] for r in rows}, set(EVENT_TYPES))

    def test_all_documented_outcomes_are_accepted(self):
        for outcome in OUTCOME_VOCABULARY:
            append_event("fanout_audit_finding", {"outcome": outcome}, log_path=self.log_path)
        self.assertEqual(len(self._rows()), len(OUTCOME_VOCABULARY))

    def test_source_defaults_to_live(self):
        append_event("fanout_audit_run", {}, log_path=self.log_path)
        self.assertEqual(self._rows()[0]["source"], "live")

    def test_retrospective_source_tag_preserved(self):
        append_event(
            "fanout_audit_finding",
            {"outcome": "fixed_pre_commit"},
            source="retrospective_2026Q3",
            log_path=self.log_path,
        )
        self.assertEqual(self._rows()[0]["source"], "retrospective_2026Q3")

    def test_read_events_skips_malformed_lines(self):
        # A partial write at process kill must not wedge the reader.
        with self.log_path.open("w", encoding="utf-8") as fh:
            fh.write('{"event": "fanout_audit_run", "schema_version": 1, "ts": "2026-01-01T00:00:00Z", "session": "s", "source": "live", "pr": null, "commit": null, "payload": {}}\n')
            fh.write("this is not valid json\n")
            fh.write('{"event": "fanout_audit_run", "schema_version": 1, "ts": "2026-01-01T00:00:01Z", "session": "s", "source": "live", "pr": null, "commit": null, "payload": {}}\n')
        rows = self._rows()
        self.assertEqual(len(rows), 2)

    def test_read_events_on_missing_file_returns_empty(self):
        missing = pathlib.Path(self._tmp.name) / "does-not-exist.jsonl"
        self.assertEqual(list(read_events(missing)), [])


class RollupTest(unittest.TestCase):
    def test_rollup_with_zero_events_renders_placeholder(self):
        # The very first invocation on a fresh install must produce a valid public artifact,
        # not a crash.
        md = render_rollup([], rendered_ts="2026-01-01T00:00:00Z")
        self.assertIn("no events logged yet", md)
        self.assertIn("What this log cannot measure", md)

    def test_rollup_counts_events_and_splits_live_vs_retrospective(self):
        events = [
            {"event": "fanout_audit_run", "source": "live", "ts": "2026-01-01T00:00:00Z", "payload": {"duration_s": 20}},
            {"event": "fanout_audit_run", "source": "live", "ts": "2026-01-02T00:00:00Z", "payload": {"duration_s": 30}},
            {"event": "fanout_audit_finding", "source": "retrospective_2026Q3", "ts": "2025-09-01T00:00:00Z", "payload": {"outcome": "fixed_pre_commit"}},
        ]
        md = render_rollup(events, rendered_ts="2026-02-01T00:00:00Z")
        self.assertIn("3 events logged", md)
        self.assertIn("2 live", md)
        self.assertIn("1 retrospective", md)
        self.assertIn("Fanout audit", md)
        self.assertIn("`fixed_pre_commit`: 1", md)

    def test_rollup_folds_old_and_new_event_names_for_fanout(self):
        # The rename from sibling_audit_* to fanout_audit_* leaves pre-rename rows in the
        # append-only log forever. Rollup must fold both under the same mechanism section.
        events = [
            {"event": "sibling_audit_run", "source": "live", "payload": {"duration_s": 18.0}},
            {"event": "fanout_audit_run", "source": "live", "payload": {"duration_s": 20.0}},
            {"event": "sibling_audit_finding", "source": "live", "payload": {"outcome": "fixed_pre_commit"}},
            {"event": "fanout_audit_finding", "source": "live", "payload": {"outcome": "false_positive"}},
        ]
        md = render_rollup(events, rendered_ts="2026-01-01T00:00:00Z")
        # ONE Fanout audit section (not two).
        self.assertEqual(md.count("## Fanout audit"), 1)
        # Both runs counted.
        self.assertIn("**2 runs**", md)
        # Both findings counted with their outcomes.
        self.assertIn("**2 findings**", md)
        self.assertIn("`fixed_pre_commit`: 1", md)
        self.assertIn("`false_positive`: 1", md)

    def test_rollup_renders_per_finding_rows_with_pr_and_outcome(self):
        # P2 of FINDINGS_EMISSION_PLAN.md: findings with resolutions render as evidence rows,
        # not just as a count. Every count in the summary must be traceable to a PR line.
        events = [
            {"event": "fanout_audit_run", "source": "live", "ts": "2026-09-26T10:00:00Z",
             "payload": {"duration_s": 18.0}},
            {"event": "fanout_audit_finding", "source": "live", "ts": "2026-09-26T10:00:01Z",
             "pr": "#1300",
             "payload": {"slug": "fanout-abcd1234", "file": "app/src/foo.jl", "line": 42,
                         "desc": "resolve_ref sibling not updated", "marker": "confirmed"}},
            # Author committed with `[fanout-abcd1234: fixed_pre_commit]` → P3 hook wrote:
            {"event": "fanout_audit_finding_resolved", "source": "live",
             "ts": "2026-09-26T10:05:00Z", "pr": "#1300", "commit": "deadbeef",
             "payload": {"slug": "fanout-abcd1234", "outcome": "fixed_pre_commit"}},
        ]
        md = render_rollup(events, rendered_ts="2026-09-26T11:00:00Z")

        # Summary counts the finding.
        self.assertIn("**1 findings**", md)
        self.assertIn("`fixed_pre_commit`: 1", md)
        # And the per-finding row shows PR + file + desc + outcome tag.
        self.assertIn("#1300", md)
        self.assertIn("`app/src/foo.jl:42`", md)
        self.assertIn("resolve_ref sibling not updated", md)
        self.assertIn("[**fixed_pre_commit**]", md)

    def test_rollup_marks_unresolved_findings(self):
        # A finding with NO matching _finding_resolved event renders as `[**unresolved**]`.
        # This surfaces slipping outcome-tag discipline (findings raised but never resolved).
        events = [
            {"event": "convention_check_finding", "source": "live", "ts": "2026-09-26T10:00:00Z",
             "pr": "#1301",
             "payload": {"slug": "conv-11112222", "file": "frontend/Foo.vue", "line": 7,
                         "desc": "hand-rolled zarr access", "marker": "should reuse"}},
        ]
        md = render_rollup(events, rendered_ts="2026-09-26T11:00:00Z")

        self.assertIn("`unresolved`: 1", md)
        self.assertIn("[**unresolved**]", md)
        self.assertIn("#1301", md)

    def test_rollup_dedupes_finding_by_slug_across_recital_reruns(self):
        # Recital re-run on the same PR emits the same slug twice — should count once, not
        # inflate the finding total.
        events = [
            {"event": "fanout_audit_finding", "source": "live", "ts": "2026-09-26T10:00:00Z",
             "pr": "#1302",
             "payload": {"slug": "fanout-aa11bb22", "file": "a.jl", "line": 1,
                         "desc": "first take", "marker": "confirmed"}},
            {"event": "fanout_audit_finding", "source": "live", "ts": "2026-09-26T10:15:00Z",
             "pr": "#1302",
             "payload": {"slug": "fanout-aa11bb22", "file": "a.jl", "line": 1,
                         "desc": "same finding, second recital", "marker": "confirmed"}},
        ]
        md = render_rollup(events, rendered_ts="2026-09-26T11:00:00Z")
        self.assertIn("**1 findings**", md)  # deduped by slug
        # Latest description wins so the rendered row reflects the current reviewer text.
        self.assertIn("same finding, second recital", md)

    def test_rollup_latest_resolution_wins(self):
        # A finding can be re-resolved (author changed their mind and shipped a follow-up
        # commit reclassifying it). Latest resolution is what the rollup shows.
        events = [
            {"event": "fanout_audit_finding", "source": "live", "ts": "2026-09-26T10:00:00Z",
             "pr": "#1303",
             "payload": {"slug": "fanout-99887766", "file": "b.jl", "line": 2,
                         "desc": "flakey", "marker": "confirmed"}},
            {"event": "fanout_audit_finding_resolved", "source": "live",
             "ts": "2026-09-26T10:05:00Z", "pr": "#1303",
             "payload": {"slug": "fanout-99887766", "outcome": "false_positive"}},
            {"event": "fanout_audit_finding_resolved", "source": "live",
             "ts": "2026-09-27T09:00:00Z", "pr": "#1303",
             "payload": {"slug": "fanout-99887766", "outcome": "fixed_pre_commit"}},
        ]
        md = render_rollup(events, rendered_ts="2026-09-27T10:00:00Z")
        self.assertIn("`fixed_pre_commit`: 1", md)
        self.assertNotIn("`false_positive`: 1", md)
        self.assertIn("[**fixed_pre_commit**]", md)

    def test_rollup_ratchet_section_orders_by_hit_count(self):
        events = [
            {"event": "ratchet_hit", "source": "live", "payload": {"ratchet_id": "zarr-access", "outcome": "fixed_pre_commit"}},
            {"event": "ratchet_hit", "source": "live", "payload": {"ratchet_id": "zarr-access", "outcome": "fixed_pre_commit"}},
            {"event": "ratchet_hit", "source": "live", "payload": {"ratchet_id": "run-py", "outcome": "false_positive"}},
        ]
        md = render_rollup(events, rendered_ts="2026-01-01T00:00:00Z")
        # zarr-access should appear before run-py in the table.
        zarr_pos = md.find("`zarr-access`")
        runpy_pos = md.find("`run-py`")
        self.assertGreater(zarr_pos, 0)
        self.assertGreater(runpy_pos, zarr_pos)


if __name__ == "__main__":
    unittest.main()
