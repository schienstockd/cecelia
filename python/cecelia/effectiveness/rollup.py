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

The AI-assist infrastructure (pre-commit fanout audit, CLAUDE.md ratchets, convention-check reviewer) writes structured rows to `~/.cecelia-effectiveness/events.jsonl` as it runs. This page renders those rows into an aggregate view of catch rate, false-positive rate, and what the infrastructure structurally cannot measure.

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


#: Display order for outcomes in the per-finding table and per-mechanism summary — the more
#: consequential outcomes lead. `unresolved` is the pending state (a `_finding` row with no
#: matching `_finding_resolved`); it sits last because a growing pile of unresolveds means
#: outcome-tag discipline is slipping and warrants attention.
_OUTCOME_DISPLAY_ORDER = (
    "fixed_pre_commit",
    "shipped_with_finding",
    "false_positive",
    "dropped_no_action",
    "unresolved",
    "no_outcome",  # only for pre-P2 finding rows that carried outcome inline
)


def _latest_resolutions_by_slug(
    events: _t.Sequence[dict], resolved_event_names: tuple[str, ...]
) -> dict[str, dict]:
    """Return the newest `_finding_resolved` row per slug, keyed by slug.

    Append-only log means an outcome can be revised in a later commit (rare but legal). The
    latest resolution wins — that's what surfaces in the rendered rollup, and the older row
    stays in the log as history a reader can walk manually.
    """
    latest: dict[str, dict] = {}
    for e in events:
        if e.get("event") not in resolved_event_names:
            continue
        slug = e.get("payload", {}).get("slug")
        if not slug:
            continue
        prev = latest.get(slug)
        if prev is None or (e.get("ts", "") > prev.get("ts", "")):
            latest[slug] = e
    return latest


def _mechanism_section(
    title: str,
    run_event: str | tuple[str, ...],
    finding_event: str | tuple[str, ...],
    events: _t.Sequence[dict],
    *,
    resolved_event: str | tuple[str, ...] = (),
) -> str:
    """Fold events matching ANY of the given event names into a single mechanism section.

    Accepts a tuple to survive event-name migrations — the fanout audit was `sibling_audit_run`
    before this rename PR, and the jsonl log is append-only, so a rollup that hardcoded the
    new name would silently drop every pre-rename row. Passing
    `("fanout_audit_run", "sibling_audit_run")` folds both under the same "Fanout audit" header.

    P2 of FINDINGS_EMISSION_PLAN.md — renders per-finding rows with PR links. Each `_finding`
    row (keyed by slug) is joined to its latest `_finding_resolved` row (via `resolved_event`,
    written by the P3 hook). Findings without a resolution render as `[unresolved]`.
    """
    run_events = (run_event,) if isinstance(run_event, str) else run_event
    finding_events = (finding_event,) if isinstance(finding_event, str) else finding_event
    resolved_events = (resolved_event,) if isinstance(resolved_event, str) else resolved_event
    runs = [e for e in events if e.get("event") in run_events]
    findings = [e for e in events if e.get("event") in finding_events]
    if not runs and not findings:
        return ""

    resolutions_by_slug = _latest_resolutions_by_slug(events, resolved_events)

    # De-dupe findings by slug (a slug may repeat across recital re-runs of the same PR — the
    # substantive information is the same, so counting once matches how a reader would count).
    # Findings without a slug (retrospective backfills, pre-P1 rows) count individually.
    findings_by_slug: dict[str, dict] = {}
    slugless_findings: list[dict] = []
    for f in findings:
        slug = f.get("payload", {}).get("slug")
        if slug:
            # Keep the newest one so the description/marker reflect the current recital.
            prev = findings_by_slug.get(slug)
            if prev is None or (f.get("ts", "") > prev.get("ts", "")):
                findings_by_slug[slug] = f
        else:
            slugless_findings.append(f)

    unique_findings = list(findings_by_slug.values()) + slugless_findings

    # Outcome summary — for slugged findings the outcome comes from the latest resolution;
    # for slugless (legacy) rows it comes from the finding payload directly.
    outcomes: collections.Counter = collections.Counter()
    for f in findings_by_slug.values():
        slug = f["payload"]["slug"]
        resolved = resolutions_by_slug.get(slug)
        outcome = (resolved or {}).get("payload", {}).get("outcome") or "unresolved"
        outcomes[outcome] += 1
    for f in slugless_findings:
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
    if unique_findings:
        lines.append(f"- **{len(unique_findings)} findings** total")
        for outcome in _OUTCOME_DISPLAY_ORDER:
            n = outcomes.get(outcome, 0)
            if n:
                lines.append(f"  - `{outcome}`: {n}")
        # Per-finding rows — grouped by outcome (resolved-order first, unresolved last), each
        # with PR link + file:line + desc + outcome tag. This is what makes the page evidence
        # rather than telemetry: every count is traceable to the PR that raised it.
        lines.append("")
        lines.append("<details><summary>Findings</summary>")
        lines.append("")
        _render_finding_rows(lines, findings_by_slug, resolutions_by_slug, slugless_findings)
        lines.append("</details>")
    lines.append("")
    return "\n".join(lines)


def _render_finding_rows(
    lines: list[str],
    findings_by_slug: dict[str, dict],
    resolutions_by_slug: dict[str, dict],
    slugless_findings: list[dict],
) -> None:
    """Append one `- <PR> — file:line — desc [**outcome**]` line per finding to `lines`.

    Groups by outcome in the display order; within a group, PR-then-file for stability across
    renders. Slugless legacy findings render last under the same grouping.
    """
    def _row_for_slug(slug: str, f: dict) -> tuple[str, str]:
        payload = f.get("payload", {})
        resolved = resolutions_by_slug.get(slug)
        outcome = (resolved or {}).get("payload", {}).get("outcome") or "unresolved"
        file = payload.get("file", "?")
        line = payload.get("line", "?")
        desc = payload.get("desc", "").strip()
        pr = (resolved or {}).get("pr") or f.get("pr")
        pr_txt = f"{pr} — " if pr else ""
        return outcome, f"- {pr_txt}`{file}:{line}` — {desc} [**{outcome}**]"

    def _row_for_slugless(f: dict) -> tuple[str, str]:
        payload = f.get("payload", {})
        outcome = payload.get("outcome") or "no_outcome"
        file = payload.get("file", "?")
        line = payload.get("line", "?")
        desc = payload.get("desc", "").strip()
        pr = f.get("pr")
        pr_txt = f"{pr} — " if pr else ""
        return outcome, f"- {pr_txt}`{file}:{line}` — {desc} [**{outcome}**]"

    rows: list[tuple[str, str]] = []
    for slug, f in findings_by_slug.items():
        rows.append(_row_for_slug(slug, f))
    for f in slugless_findings:
        rows.append(_row_for_slugless(f))

    order_index = {o: i for i, o in enumerate(_OUTCOME_DISPLAY_ORDER)}
    rows.sort(key=lambda r: (order_index.get(r[0], 99), r[1]))
    for _, row in rows:
        lines.append(row)


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
        _mechanism_section(
            "Fanout audit",
            ("fanout_audit_run", "sibling_audit_run"),
            ("fanout_audit_finding", "sibling_audit_finding"),
            events,
            resolved_event="fanout_audit_finding_resolved",
        ),
        _mechanism_section(
            "Convention check",
            "convention_check_run",
            "convention_check_finding",
            events,
            resolved_event="convention_check_finding_resolved",
        ),
        _ratchets_section(events),
        _misses_section(events),
    ):
        if section:
            parts.append(section)

    parts.append(_CEILING)
    return "\n".join(parts)
