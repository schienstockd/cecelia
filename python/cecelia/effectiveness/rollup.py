"""Read the effectiveness jsonl and produce the public markdown artifact.

Per docs/todo/EFFECTIVENESS_LOG_PLAN.md the rollup runs ON-DEMAND (not heartbeated, no auto-
commit) — invoke via `pixi run audit-rollup`, review the resulting diff, commit if the update
is meaningful. The output is `docs/ai-assist/EFFECTIVENESS.md`.

v1 renders:
- Header (run date, N events, date range, retrospective vs live split).
- Per-mechanism sections (sibling-audit, convention-check, ratchets) — count + outcome
  breakdown.
- Miss-visibility section (retrospective_miss rows).
- Ceiling section — the honest "cannot measure" list, held here as a stable epilogue so
  external readers see it in the same file, not one link away.

Not rendered v1: usage-weighted spot-check heat map from `cited_doc_refs` (needs N > ~20
citations to be useful; produce once real data lands).
"""

from __future__ import annotations

import collections
import datetime as _dt
import typing as _t

_HEADER_TEMPLATE = """# AI-assist infrastructure — effectiveness

_Rendered {rendered} from `~/.cecelia-effectiveness/events.jsonl` — this file is regenerated on demand by `pixi run audit-rollup`. Not auto-committed._

**{n_total} events logged** across {n_live} live and {n_retrospective} retrospective rows{date_range}.

See [`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md) for the schema, event taxonomy, sample method, and what this log structurally cannot measure.
"""

_NO_EVENTS = """# AI-assist infrastructure — effectiveness

_Rendered {rendered} — no events logged yet._

The AI-assist infrastructure (pre-commit sibling-call audit, CLAUDE.md ratchets, convention-check reviewer) writes structured rows to `~/.cecelia-effectiveness/events.jsonl` as it runs. This page renders those rows into an aggregate view of catch rate, false-positive rate, and what the infrastructure structurally cannot measure.

See [`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md) for the schema, event taxonomy, sample method, and the honest ceiling on what this log can and cannot tell you.
"""

_CEILING = """
## What this log cannot measure

- **Silent misses.** Bugs shipped and never noticed. Unbounded, unmeasurable.
- **Cross-module private-helper cloning.** The convention-check reviewer excludes this class — opaque names don't respond to synonym greps; detection needs semantic-similarity indexing.
- **Counterfactual attribution.** "The audit flagged X, and the author fixed it" is measurable; "this bug would have shipped without the audit" is not — the author might have noticed anyway.
- **Selection bias in retrospective rows.** Merged-PR samples miss the ones that got closed unreviewed.
- **Novelty decay.** Ratchets productive at N=0 may become noise at N=100 as the codebase adapts around them. Rising FP rate over time is a signal to read, not to aggregate.
"""


def _fmt_ts(ts: str) -> str:
    """Trim ISO timestamp to date; fall back to raw if unparseable."""
    try:
        return _dt.datetime.fromisoformat(ts.replace("Z", "+00:00")).date().isoformat()
    except (ValueError, AttributeError):
        return ts


def _split_source(events: _t.Sequence[dict]) -> tuple[list[dict], list[dict]]:
    live, retro = [], []
    for e in events:
        (retro if e.get("source", "live").startswith("retrospective") else live).append(e)
    return live, retro


def _date_range(events: _t.Sequence[dict]) -> str:
    tss = [e.get("ts") for e in events if e.get("ts")]
    if not tss:
        return ""
    tss.sort()
    return f", spanning {_fmt_ts(tss[0])} → {_fmt_ts(tss[-1])}"


def _mechanism_section(
    title: str, run_event: str, finding_event: str, events: _t.Sequence[dict]
) -> str:
    runs = [e for e in events if e.get("event") == run_event]
    findings = [e for e in events if e.get("event") == finding_event]
    if not runs and not findings:
        return ""

    outcomes: collections.Counter = collections.Counter()
    for f in findings:
        outcomes[f.get("payload", {}).get("outcome") or "no_outcome"] += 1

    lines = [f"## {title}", ""]
    if runs:
        durations = [
            r.get("payload", {}).get("duration_s")
            for r in runs
            if isinstance(r.get("payload", {}).get("duration_s"), (int, float))
        ]
        median = sorted(durations)[len(durations) // 2] if durations else None
        median_txt = f" · median duration {median:.1f}s" if median is not None else ""
        lines.append(f"- **{len(runs)} runs**{median_txt}")
    if findings:
        lines.append(f"- **{len(findings)} findings** total")
        for outcome in ("fixed_pre_commit", "shipped_with_finding", "false_positive", "dropped_no_action", "no_outcome"):
            n = outcomes.get(outcome, 0)
            if n:
                lines.append(f"  - `{outcome}`: {n}")
    lines.append("")
    return "\n".join(lines)


def _ratchets_section(events: _t.Sequence[dict]) -> str:
    hits = [e for e in events if e.get("event") == "ratchet_hit"]
    if not hits:
        return ""
    by_ratchet: collections.Counter = collections.Counter()
    outcomes_by_ratchet: dict[str, collections.Counter] = collections.defaultdict(collections.Counter)
    for h in hits:
        rid = h.get("payload", {}).get("ratchet_id") or "unknown"
        by_ratchet[rid] += 1
        outcomes_by_ratchet[rid][h.get("payload", {}).get("outcome") or "no_outcome"] += 1

    lines = ["## Ratchet hits", "", f"**{len(hits)} total hits** across {len(by_ratchet)} ratchets."]
    lines.append("")
    lines.append("| Ratchet | Hits | Fixed pre-commit | False positive |")
    lines.append("|---|---:|---:|---:|")
    for rid, n in sorted(by_ratchet.items(), key=lambda x: -x[1]):
        o = outcomes_by_ratchet[rid]
        lines.append(f"| `{rid}` | {n} | {o.get('fixed_pre_commit', 0)} | {o.get('false_positive', 0)} |")
    lines.append("")
    return "\n".join(lines)


def _misses_section(events: _t.Sequence[dict]) -> str:
    misses = [e for e in events if e.get("event") == "retrospective_miss"]
    if not misses:
        return ""
    lines = ["## Known misses", "", f"**{len(misses)} retrospective misses** — bugs the infrastructure should have caught but did not. Represents the *known* portion of the miss surface; silent misses (bugs shipped and never noticed) are unbounded and unmeasurable — see *What this log cannot measure*."]
    lines.append("")
    by_class: collections.Counter = collections.Counter()
    for m in misses:
        by_class[m.get("payload", {}).get("bug_class") or "unclassified"] += 1
    for cls, n in sorted(by_class.items(), key=lambda x: -x[1]):
        lines.append(f"- `{cls}`: {n}")
    lines.append("")
    return "\n".join(lines)


def render_rollup(events: _t.Iterable[dict], *, rendered_ts: str | None = None) -> str:
    """Produce the public markdown artifact from an iterable of event rows.

    Called by `scripts/audit_rollup.py`; also directly usable in tests. `rendered_ts` lets
    tests fix a deterministic timestamp; production leaves it None and uses now-UTC.
    """
    events = list(events)
    rendered = rendered_ts or _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")

    if not events:
        return _NO_EVENTS.format(rendered=rendered) + _CEILING

    live, retro = _split_source(events)
    header = _HEADER_TEMPLATE.format(
        rendered=rendered,
        n_total=len(events),
        n_live=len(live),
        n_retrospective=len(retro),
        date_range=_date_range(events),
    )

    parts = [header]
    for section in (
        _mechanism_section("Sibling-call audit", "sibling_audit_run", "sibling_audit_finding", events),
        _mechanism_section("Convention check", "convention_check_run", "convention_check_finding", events),
        _ratchets_section(events),
        _misses_section(events),
    ):
        if section:
            parts.append(section)

    parts.append(_CEILING)
    return "\n".join(parts)
