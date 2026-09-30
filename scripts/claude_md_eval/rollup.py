"""Render CLAUDE.md compliance-eval rows from the effectiveness log into markdown.

Design: docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Rollup* (P2.5). Reads `claude_md_eval_suite`,
`claude_md_eval_run`, and `claude_md_eval_ablation` rows and produces
`docs/ai-assist/CLAUDE_MD_EVAL.md`. Called at the end of every `pixi run claude-md-eval`
pass so the artifact tracks the latest data as a side effect; standalone regen via
`pixi run claude-md-eval-rollup` re-renders without spawning agents.

**Sibling — see also `python/cecelia/effectiveness/rollup.py`.** Same input log
(`~/.cecelia-effectiveness/events.jsonl`), disjoint event families
(`claude_md_eval_*` here; `fanout_audit_finding` / `convention_check_finding` there),
disjoint output artifacts. Kept separate — different audience, different section
shape, different render cadence — but a future reader touching one may want to check
the other.

Not rendered v1:
- Cross-linking failing prompts to specific CLAUDE.md sections that changed (would need
  `git log -p` on CLAUDE.md filtered to the section headings the prompt tests). Ship if
  the plain rule text turns out not to be enough to steer investigation.
"""

from __future__ import annotations

import datetime as _dt
import typing as _t

_HEADER_TEMPLATE = """# CLAUDE.md compliance eval

_Rendered {rendered} from `~/.cecelia-effectiveness/events.jsonl` — auto-regenerated at the end of every `pixi run claude-md-eval` pass. Standalone regen via `pixi run claude-md-eval-rollup`. Not auto-committed._

Behavioral compliance signal for `CLAUDE.md`: a fresh `claude -p` agent is given a task under a rule, the diff + tool trace are scored deterministically. Design + methodology: [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../todo/CLAUDE_MD_EVAL_PLAN.md).
"""

_NO_DATA = """# CLAUDE.md compliance eval

_Rendered {rendered} — no `claude_md_eval_suite` rows in the log yet._

Run `pixi run claude-md-eval` to produce the first pass. Design: [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../todo/CLAUDE_MD_EVAL_PLAN.md).
"""

_TRAILER = """
## Reading this page

- **Compliance ≠ correctness.** A `compliant` verdict means the diff matched a
  deterministic signal for the rule; it does not mean the code works. Ratchets are the
  correctness backstop; this eval measures whether an agent *reaches for* the canonical
  path in the first place.
- **Δ discipline.** Per plan D12: N≥3 AND read at least one trace per arm before quoting
  a with/without ablation delta anywhere. Regex + tool-order graders can both pass on
  artefacts (e.g. a run where CLAUDE.md never loaded at all — see PR #1272 for the
  abandoned plugin-eval port that surfaced this failure mode).
- **Blob SHA is the CLAUDE.md the eval ran under**, not the current worktree HEAD — so
  a trend line across CLAUDE.md edits is legible even when the runner sits on a branch
  with unrelated changes.
"""


def _fmt_ts(ts: str) -> str:
    """ISO ts → 'YYYY-MM-DD HH:MM' (UTC). Falls back to raw on parse failure."""
    try:
        d = _dt.datetime.fromisoformat(ts.replace("Z", "+00:00"))
        return d.strftime("%Y-%m-%d %H:%M")
    except (ValueError, AttributeError):
        return ts


def _short_sha(sha: str | None) -> str:
    if not sha:
        return "<unknown>"
    return sha[:8]


def _signed_dollars(v: float, precision: int = 3) -> str:
    """Format a signed dollar delta as `+$0.100` / `-$0.100` / `$0.000`.

    The naive `f"${v:+.3f}"` puts the sign inside the leading `$` (`$+0.100`) which
    reads as a typo. Delta columns need sign-before-currency so the direction is the
    first thing the eye lands on.
    """
    if v > 0:
        return f"+${v:.{precision}f}"
    if v < 0:
        return f"-${abs(v):.{precision}f}"
    return f"${v:.{precision}f}"


def _latest_rule_per_prompt(events: _t.Sequence[dict]) -> dict[str, str]:
    """Walk `_run` rows to build {prompt_id: rule_text}. Latest wins.

    Rule text is authored in the prompt frontmatter and echoed on every `_run` row's
    payload — so a rule rewording lands here on the next pass without the rollup needing
    to read the catalog directly.
    """
    rules: dict[str, tuple[str, str]] = {}  # id -> (ts, rule)
    for e in events:
        if e.get("event") != "claude_md_eval_run":
            continue
        pid = e.get("payload", {}).get("prompt_id")
        rule = e.get("payload", {}).get("rule")
        ts = e.get("ts", "")
        if not pid or not rule:
            continue
        prev = rules.get(pid)
        if prev is None or ts > prev[0]:
            rules[pid] = (ts, rule)
    return {pid: r for pid, (_, r) in rules.items()}


# Suite rows written before `full_catalog` was recorded (2026-09-30): the two genuine full
# passes then ran 9 and 12 prompts; every `--only` / ablation subset ran ≤4. Legacy-only.
_LEGACY_FULL_MIN_PROMPTS = 9


# A full pass also needs N≥3 — a `--runs 1` sweep of the whole catalog is a smoke check,
# and a 1/1 must not count toward a retirement streak.
_FULL_MIN_RUNS = 3


def _is_full_pass(row: dict) -> bool:
    """True when a suite/ablation row covers the whole catalog (no `--only` / `--exclude`) at N≥3."""
    p = row.get("payload", {}) or {}
    runs = p.get("runs_per_prompt") or p.get("runs_per_arm") or 0
    if runs < _FULL_MIN_RUNS:
        return False
    if "full_catalog" in p:
        return bool(p["full_catalog"])
    return len(p.get("prompt_ids") or []) >= _LEGACY_FULL_MIN_PROMPTS


def _suite_arm(suite: dict) -> str:
    # Rows written before the ablation additions (pre-2026-09-28) carry no `arm` and ran
    # with CLAUDE.md — that matches how the runner shipped historically.
    return (suite.get("payload", {}) or {}).get("arm", "with")


def _suites(events: _t.Sequence[dict]) -> list[dict]:
    """Every `_suite` row, newest first."""
    return sorted((e for e in events if e.get("event") == "claude_md_eval_suite"),
                  key=lambda e: e.get("ts", ""), reverse=True)


def _latest_suite(events: _t.Sequence[dict]) -> dict | None:
    """The row the page leads with: newest full-catalog WITH-arm pass.

    A `--only` spot-check or an ablation's WITHOUT arm is not the state of the setup —
    rendering it as "Latest suite" is how a 1-prompt N=1 check and an errored WITHOUT
    arm each replaced the full-pass table on 2026-09-29. Falls back to the newest
    WITH-arm row, then the newest row, so a log with no full pass still renders.
    """
    suites = _suites(events)
    for pick in (lambda s: _suite_arm(s) == "with" and _is_full_pass(s),
                 lambda s: _suite_arm(s) == "with",
                 lambda s: True):
        for s in suites:
            if pick(s):
                return s
    return None


def _latest_ablation(events: _t.Sequence[dict]) -> dict | None:
    """Newest full-catalog ablation, else the newest ablation — same scoping as `_latest_suite`."""
    rows = sorted((e for e in events if e.get("event") == "claude_md_eval_ablation"),
                  key=lambda e: e.get("ts", ""), reverse=True)
    return next((r for r in rows if _is_full_pass(r)), rows[0] if rows else None)


def _suite_section(suite: dict, rules: dict[str, str]) -> str:
    p = suite.get("payload", {})
    pids: list[str] = p.get("prompt_ids") or []
    per_prompt: dict = p.get("per_prompt") or {}
    totals: dict = p.get("totals") or {}
    runs = p.get("runs_per_prompt") or 0
    arm = p.get("arm", "with")
    ts = _fmt_ts(suite.get("ts", ""))
    sha = _short_sha(suite.get("commit"))
    total_cost = totals.get("cost_usd")

    lines = [
        "## Latest suite",
        "",
        f"- **When:** {ts} UTC",
        f"- **CLAUDE.md blob:** `{sha}`",
        f"- **Arm:** `{arm}` · **Runs per prompt:** {runs}",
        f"- **Scope:** {'full catalog' if _is_full_pass(suite) else 'partial (`--only` / ablation subset) — no full pass logged yet'}",
    ]
    if total_cost is not None:
        lines.append(f"- **Total spend:** ${total_cost:.2f}")
    lines.append("")
    lines.append("| Prompt | Compliant / Total | Errors | Cost | Rule |")
    lines.append("|---|---:|---:|---:|---|")
    for pid in pids:
        r = per_prompt.get(pid) or {}
        c = r.get("compliant", 0)
        n = r.get("noncompliant", 0)
        err = r.get("error", 0)
        total = c + n + err
        cost = r.get("cost_usd")
        cost_txt = f"${cost:.3f}" if isinstance(cost, (int, float)) else "—"
        rule = rules.get(pid, "").replace("|", "\\|")
        # Truncate a long rule to keep the table scannable; the full text is on the
        # prompt file itself, one line above the frontmatter.
        if len(rule) > 90:
            rule = rule[:87] + "…"
        lines.append(f"| `{pid}` | {c}/{total} | {err} | {cost_txt} | {rule} |")
    lines.append(f"| **TOTAL** | **{totals.get('compliant', 0)}/"
                 f"{totals.get('compliant', 0) + totals.get('noncompliant', 0) + totals.get('error', 0)}** "
                 f"| {totals.get('error', 0)} | "
                 f"{('$' + f'{total_cost:.2f}') if total_cost is not None else '—'} | |")
    lines.append("")
    return "\n".join(lines)


def _ablation_arm_errored(per_prompt: dict, runs: int, arm: str) -> bool:
    """Did ≥50% of runs in an arm error?

    2026-09-29 case: claude 2.1.284 auto-updated between the WITH and WITHOUT passes,
    the initial pairing/auth path threw on every WITHOUT spawn (exit 1, ~3s, empty
    stderr), and the rollup rendered a bogus Δ=+4 that any reader would misread as
    "CLAUDE.md is +4 compliant." When most of an arm errored, the numeric Δ is not
    evidence about CLAUDE.md — it is evidence the arm broke. Suppress rather than
    publish.

    Prefers the explicit `{arm}_error` counts recorded by `run_ablation.py`. Falls
    back to a cost-based sentinel for ablation rows written before those counts were
    added (`{arm}_cost_usd` ≪ the other arm's cost).
    """
    if runs <= 0:
        return False
    key = f"{arm}_error"
    if any(key in r for r in per_prompt.values()):
        errored = sum(1 for r in per_prompt.values() if r.get(key, 0) >= runs)
        return errored >= max(1, len(per_prompt) // 2)
    # Fallback for legacy rows without per-arm error counts.
    this_cost = sum(r.get(f"{arm}_cost_usd", 0.0) for r in per_prompt.values())
    other = "with" if arm == "without" else "without"
    other_cost = sum(r.get(f"{other}_cost_usd", 0.0) for r in per_prompt.values())
    return other_cost >= 1.0 and this_cost < 0.05 * other_cost


def _ablation_section(ablation: dict) -> str:
    p = ablation.get("payload", {})
    pids: list[str] = p.get("prompt_ids") or []
    per_prompt: dict = p.get("per_prompt") or {}
    totals: dict = p.get("totals") or {}
    runs = p.get("runs_per_arm") or 0
    ts = _fmt_ts(ablation.get("ts", ""))
    sha = _short_sha(ablation.get("commit"))

    without_errored = _ablation_arm_errored(per_prompt, runs, "without")
    with_errored = _ablation_arm_errored(per_prompt, runs, "with")
    if without_errored or with_errored:
        broken = "WITHOUT" if without_errored else "WITH"
        return "\n".join([
            "## Latest ablation (with vs without CLAUDE.md)",
            "",
            f"- **When:** {ts} UTC · **Blob:** `{sha}` · **Runs per arm:** {runs}",
            "",
            f"> ⚠ **{broken} arm errored across ≥50% of runs — Δ suppressed.** "
            f"An arm-wide error means the numeric delta is not evidence about "
            f"CLAUDE.md; it is evidence the arm broke. Re-run "
            f"`pixi run claude-md-eval-ablation` once the underlying cause is "
            f"fixed. First known case: 2026-09-29 claude 2.1.284 post-update "
            f"pairing/auth transient (errored 12/12 WITHOUT runs).",
            "",
        ])

    lines = [
        "## Latest ablation (with vs without CLAUDE.md)",
        "",
        f"- **When:** {ts} UTC · **Blob:** `{sha}` · **Runs per arm:** {runs}",
        "",
        f"> Δ discipline: N≥3 AND at least one trace per arm inspected before quoting these numbers as evidence anywhere (plan D12). This table is data, not conclusion.",
        "",
        "| Prompt | With | Without | ΔCompliant | With $ | Without $ | Δ$ |",
        "|---|---:|---:|---:|---:|---:|---:|",
    ]
    for pid in pids:
        r = per_prompt.get(pid) or {}
        w = r.get("with_compliant", 0)
        wo = r.get("without_compliant", 0)
        dc = r.get("delta_compliant", 0)
        wc = r.get("with_cost_usd", 0.0)
        woc = r.get("without_cost_usd", 0.0)
        dcost = r.get("delta_cost_usd", 0.0)
        lines.append(f"| `{pid}` | {w}/{runs} | {wo}/{runs} | {dc:+d} | "
                     f"${wc:.3f} | ${woc:.3f} | {_signed_dollars(dcost)} |")
    lines.append(f"| **TOTAL** | **{totals.get('with_compliant', 0)}** | "
                 f"**{totals.get('without_compliant', 0)}** | "
                 f"**{totals.get('delta_compliant', 0):+d}** | "
                 f"${totals.get('with_cost_usd', 0.0):.2f} | "
                 f"${totals.get('without_cost_usd', 0.0):.2f} | "
                 f"{_signed_dollars(totals.get('delta_cost_usd', 0.0), precision=2)} |")
    lines.append("")
    return "\n".join(lines)


def _adhoc_section(events: _t.Sequence[dict], suite: dict) -> str:
    """Suite rows newer than the one the page leads with — spot-checks, ablation arms."""
    newer = [s for s in _suites(events) if s.get("ts", "") > suite.get("ts", "")]
    if not newer:
        return ""
    lines = ["## Ad-hoc runs since", "",
             "_Subsets and ablation arms — not the state of the setup; shown so a spot-check "
             "after an intervention is visible before the next full pass._", "",
             "| When (UTC) | Arm | N | Prompts | Compliant / Total |",
             "|---|---|---:|---|---:|"]
    for s in newer:
        p = s.get("payload", {}) or {}
        t = p.get("totals") or {}
        c = t.get("compliant", 0)
        total = c + t.get("noncompliant", 0) + t.get("error", 0)
        pids = ", ".join(f"`{pid}`" for pid in p.get("prompt_ids") or [])
        lines.append(f"| {_fmt_ts(s.get('ts', ''))} | {_suite_arm(s)} | "
                     f"{p.get('runs_per_prompt') or 0} | {pids} | {c}/{total} |")
    lines.append("")
    return "\n".join(lines)


def _failing_section(suite: dict, rules: dict[str, str]) -> str:
    """Any prompt with <100% compliance in the latest pass gets a paragraph.

    Kept short: prompt id, compliant/total, rule text. The "what did the agent do
    instead" belongs in a per-run trace inspection, not the rollup summary.
    """
    p = suite.get("payload", {})
    per_prompt: dict = p.get("per_prompt") or {}
    pids: list[str] = p.get("prompt_ids") or []
    failing = []
    for pid in pids:
        r = per_prompt.get(pid) or {}
        c = r.get("compliant", 0)
        total = c + r.get("noncompliant", 0) + r.get("error", 0)
        if total > 0 and c < total:
            failing.append((pid, c, total))
    if not failing:
        return ""
    lines = ["## Failing rules (latest suite)", "",
             "_Each run's diff + tool log is kept under "
             "`~/.cecelia-effectiveness/traces/<ts>-<prompt>-<arm>-r<n>/` — read the trace "
             "for *why* before changing anything, and fix the dev setup, not the probe._", ""]
    for pid, c, total in failing:
        rule = rules.get(pid, "").strip()
        lines.append(f"### `{pid}` — {c}/{total} compliant")
        if rule:
            lines.append(f"Rule: {rule}")
        lines.append("")
    return "\n".join(lines)


def _trend_section(events: _t.Sequence[dict], *, max_rows: int = 8) -> str:
    """One row per full pass, most recent first. Blob-SHA changes are marked.

    Columns are the union of prompt ids seen across the recent passes — a prompt added
    later shows `—` on older rows, which is honest (that prompt didn't run then).
    """
    # Full WITH-arm passes only — subsets would read as regressions in the columns they
    # skipped, and WITHOUT arms belong to the ablation section.
    suites = [s for s in _suites(events) if _suite_arm(s) == "with" and _is_full_pass(s)]
    if len(suites) < 2:
        # A single-row trend is just the latest-suite table restated. Suppress until
        # we have at least two passes.
        return ""
    suites = suites[:max_rows]

    # Column order: union of prompt_ids across the rendered passes, sorted for stability.
    pids: set[str] = set()
    for s in suites:
        for pid in s.get("payload", {}).get("prompt_ids") or []:
            pids.add(pid)
    pid_cols = sorted(pids)

    lines = ["## Trend (recent passes)", ""]
    lines.append("| When (UTC) | Blob | Arm | " + " | ".join(f"`{pid}`" for pid in pid_cols) + " | Total |")
    lines.append("|---|---|---|" + "---:|" * len(pid_cols) + "---:|")
    prev_sha: str | None = None
    for s in suites:
        p = s.get("payload", {}) or {}
        per_prompt = p.get("per_prompt") or {}
        pid_map = {pid: (per_prompt.get(pid) or {}) for pid in pid_cols}
        cells = []
        for pid in pid_cols:
            r = pid_map[pid]
            c = r.get("compliant", 0)
            total = c + r.get("noncompliant", 0) + r.get("error", 0)
            cells.append(f"{c}/{total}" if total else "—")
        totals = p.get("totals") or {}
        tc = totals.get("compliant", 0)
        tt = tc + totals.get("noncompliant", 0) + totals.get("error", 0)
        sha = _short_sha(s.get("commit"))
        marker = " →" if prev_sha and prev_sha != sha else ""
        prev_sha = sha
        arm = p.get("arm", "with")
        lines.append(f"| {_fmt_ts(s.get('ts', ''))} | `{sha}`{marker} | {arm} | "
                     + " | ".join(cells) + f" | {tc}/{tt} |")
    lines.append("")
    lines.append("_`→` next to a blob SHA marks a CLAUDE.md edit between passes — the change most likely to have moved the numbers on that row._")
    lines.append("")
    return "\n".join(lines)


def render_eval_rollup(events: _t.Iterable[dict],
                       *, rendered_ts: str | None = None) -> str:
    """Produce the markdown artifact from an iterable of event rows.

    `rendered_ts` lets tests pin a deterministic timestamp; production leaves it None
    and uses now-UTC. Only `claude_md_eval_*` events are examined; other rows are
    ignored (this rollup is a narrow view of one event family).
    """
    events = list(events)
    rendered = rendered_ts or _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")

    suite = _latest_suite(events)
    if suite is None:
        return _NO_DATA.format(rendered=rendered)

    rules = _latest_rule_per_prompt(events)
    ablation = _latest_ablation(events)
    parts = [_HEADER_TEMPLATE.format(rendered=rendered),
             _suite_section(suite, rules)]
    if ablation:
        parts.append(_ablation_section(ablation))
    adhoc = _adhoc_section(events, suite)
    if adhoc:
        parts.append(adhoc)
    failing = _failing_section(suite, rules)
    if failing:
        parts.append(failing)
    trend = _trend_section(events)
    if trend:
        parts.append(trend)
    parts.append(_TRAILER)
    return "\n".join(parts)
