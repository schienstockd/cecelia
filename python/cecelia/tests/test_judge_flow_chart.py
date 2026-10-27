"""Tests for `scripts/judge/flow_chart.py` — where every reviewer finding went.

Driven by hand-built log rows and pass records; nothing reads the real log or calls git.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import pathlib
import unittest
import xml.etree.ElementTree as ET

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load(name):
    spec = importlib.util.spec_from_file_location(name, _REPO / "scripts" / "judge" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _row(event, payload, ts="2026-10-01T00:00:00Z"):
    return {"event": event, "ts": ts, "payload": payload}


def _finding(slug, ts="2026-10-01T00:00:00Z"):
    return _row("fanout_audit_finding", {"slug": slug, "file": "a.py", "line": 1, "marker": "confirmed"}, ts)


def _plausible(line, slug=None):
    return _row("fanout_audit_advisory", {"file": "b.py", "line": line, "desc": "maybe", "marker": "plausible",
                                          **({"slug": slug} if slug else {})})


def _resolved(slug, outcome):
    return _row("fanout_audit_finding_resolved", {"slug": slug, "outcome": outcome}, "2026-10-02T00:00:00Z")


def _bug(key, status, verdict=None, issue=None):
    return {"key": key, "status": status, **({"verify": {"verdict": verdict}} if verdict else {}),
            **({"issue": issue} if issue else {})}


class FlowTest(unittest.TestCase):
    def setUp(self):
        self.f = _load("flow_chart")

    def test_each_column_counts_what_flows_into_it(self):
        events = [_finding("fanout-00000001"), _resolved("fanout-00000001", "fixed_pre_commit"),
                  _finding("fanout-00000002"), _resolved("fanout-00000002", "shipped_with_finding"),
                  _finding("fanout-00000003", ts="2026-09-28T00:00:00Z"),          # never tagged
                  _finding("fanout-00000001"),                                     # a re-run: one finding
                  _plausible(5), _plausible(5), _plausible(6)]
        older = {"bugs": [_bug("k1", "open")]}
        newer = {"bugs": [_bug("k1", "open", "fix", issue=7),                     # newest record wins
                          _bug("k2", "gone", "fix", issue=8), _bug("k3", "open", "decide"),
                          _bug("k4", "dismissed", "dismiss"), _bug("k5", "parked", "guard"),
                          _bug("k6", "gone"), _bug("k7", "unjudged"), _bug("k8", "wont_fix", "decide"),
                          {"key": "k9", "kind": "stranded", "status": "open", "issue": 9}]}
        d = self.f.aggregate(events, [older, newer])
        self.assertEqual(d["recital"], {"fixed": 1, "not_fixed": 2, "plausible": 2})
        self.assertEqual((d["required"], d["required_fixed"], d["first_day"]), (3, 1, "2026-09-28"))
        self.assertEqual(d["check"], {"settled": 1, "waiting": 1, "verify": 7})
        self.assertEqual(d["verify"], {"not_a_bug": 1, "parked": 1, "unchecked": 0, "real": 5})
        self.assertEqual(d["real"], {"fixed": 1, "open_issue": 2, "not_filed": 1, "wont_fix": 1})

    def test_a_bug_verify_found_fixed_is_a_real_bug_fixed_not_a_false_positive(self):
        legacy = {**_bug("k2", "dismissed", "dismiss"), "fix_landed": [{"commit": "f" * 40}]}   # before `fixed`
        d = self.f.aggregate([], [{"bugs": [_bug("k1", "gone", "fixed"), legacy, _bug("k3", "dismissed", "dismiss")]}])
        self.assertEqual((d["verify"]["real"], d["verify"]["not_a_bug"]), (2, 1))
        self.assertEqual(d["real"]["fixed"], 2)

    def test_the_lines_under_the_first_gap_add_up_to_the_bug_check(self):
        convention = lambda slug, marker: _row("convention_check_finding",
                                               {"slug": slug, "file": "c.py", "line": 1, "marker": marker})
        events = [_finding("fanout-00000001"),                                   # listed, merged with -02
                  _finding("fanout-00000002"),
                  _finding("fanout-00000003"),                                   # gone before it was listed
                  _finding("fanout-00000004", ts="2026-10-09T00:00:00Z"),       # after the last pass
                  convention("conv-00000001", "wrong home"),                     # rule count only
                  convention("conv-00000002", "should reuse"),                   # listed
                  _finding("fanout-00000005"), _resolved("fanout-00000005", "fixed_pre_commit")]  # listed, then tagged
        record = {"run": {"ts": "2026-10-05T00:00:00Z"}, "bugs": [
            {"key": "fanout-00000001", "status": "open", "sources": ["fanout-00000001", "fanout-00000002"]},
            {"key": "conv-00000002", "kind": "duplicate", "status": "open"},
            {"key": "fanout-00000005", "status": "gone"},
            {"key": "repeat-x", "kind": "agent_run", "status": "open"}]}
        d = self.f.aggregate(events, [record])
        into = d["into_check"]
        self.assertEqual(into, {"notes": 6, "rules_only": 1, "not_listed": 1, "since_pass": 1, "merged": 1,
                                "tagged_later": 1, "last_pass": "2026-10-05", "agent_run": 1, "stranded": 0})
        self.assertEqual(into["notes"], d["recital"]["not_fixed"] + d["recital"]["plausible"])
        ledger = self.f._ledger(into, sum(d["check"].values()))
        total = sum({"": 1, "+": 1, "−": -1}[sign] * n for sign, n, _ in ledger if sign != "=")
        self.assertEqual(total, ledger[-1][1])
        self.assertEqual(total, 4)
        svg = self.f.render_svg(d)
        ET.fromstring(svg)
        self.assertIn("From 6 notes to 4 bugs", svg)
        self.assertNotIn("commits pushed", svg)   # a line with nothing on it is left out

    def test_a_finding_from_before_slugs_counts_by_its_inline_outcome(self):
        old = [_row("fanout_audit_finding", {"file": "a.py", "line": 1, "outcome": "fixed_pre_commit"}),
               _row("convention_check_finding", {"file": "c.py", "line": 2, "outcome": "dropped_no_action"})]
        d = self.f.aggregate(old, [])
        self.assertEqual((d["recital"]["fixed"], d["recital"]["not_fixed"], d["required"]), (1, 1, 2))

    def test_a_plausible_note_tagged_fixed_counts_as_fixed_in_the_commit(self):
        note = _plausible(9)
        key = self.f._bugs._key(note)
        note["payload"]["slug"] = key
        d = self.f.aggregate([note, _resolved(key, "fixed_pre_commit"), _plausible(10)], [])
        self.assertEqual(d["recital"], {"fixed": 1, "not_fixed": 0, "plausible": 1})
        self.assertEqual(d["required"], 0)

    def test_same_counts_same_bytes_and_the_svg_parses_with_no_finding_text(self):
        events = [_finding("fanout-00000001"), _plausible(3)]
        a = self.f.render_svg(self.f.aggregate(events, []))
        self.assertEqual(a, self.f.render_svg(self.f.aggregate(list(events), [])))
        ET.fromstring(a)
        self.assertNotIn("a.py", a)
        self.assertNotIn("maybe", a)

    def test_nothing_logged_still_draws(self):
        svg = self.f.render_svg(self.f.aggregate([], []))
        ET.fromstring(svg)
        self.assertIn("Nothing logged yet", svg)


class FigureStyleTest(unittest.TestCase):
    """Every README figure, hand-drawn or generated, sits on the one shared surface."""

    def test_readme_figures_and_both_charts_use_the_shared_surface(self):
        style = _load("figure_style")
        for path in sorted((_REPO / "frontend" / "public").glob("readme-*.svg")):
            with self.subTest(figure=path.name):
                self.assertIn(f'fill="{style.SURFACE}"', path.read_text(encoding="utf-8"))
        chart, flow = _load("chart"), _load("flow_chart")
        data = chart.aggregate([], [])
        for name, svg in (("chart", chart.render_svg(data)), ("flow", flow.render_svg(flow.aggregate([], [])))):
            with self.subTest(figure=name):
                self.assertIn(style.surface(1, 1).split(" width")[0], svg)
                self.assertIn(f'fill="{style.SURFACE}"', svg)


if __name__ == "__main__":
    unittest.main()
