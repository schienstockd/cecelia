"""Live console for the AI-assist effectiveness log — the recital-side twin of `pixi run console`.

`pixi run console` (`api/task_console.jl`) is the read-only live view of scheduler tasks over the
API WebSocket. This is the same shape one level up: a read-only live view of the reviewer
agents (fanout audit + convention check + citation-currency) as they emit `_run` / `_finding` /
`_finding_resolved` rows to `~/.cecelia-effectiveness/events.jsonl`. Same conventions as the task
console: default is a TTY-friendly colour stream, `--stream` (auto-set when stdout isn't a TTY)
falls back to plain text so `pixi run recital-console | tee out.log` gives a clean file.

The alternative that this replaces is `tail -fn+1 ~/.cecelia-effectiveness/events.jsonl | jq`,
which shows every raw field of every row. The console filters to the recital-relevant events,
renders each as one short line (findings get an indented second line for the description), and
colour-codes by mechanism and marker so the flagged rows stand out at a glance — the point
being to see WHAT was flagged without reading the full markdown roundup.

Not shared with `_render_finding_rows` in `rollup.py`: that renders markdown grouped by outcome
across the whole log, this renders one ANSI row per event as it lands. Both read the payload
fields the same way (`file`, `line`, `desc`, `marker`, `slug`, `outcome`) — the shape contract
lives in `log.py::EVENT_TYPES` and the payload docs in `docs/todo/EFFECTIVENESS_LOG_PLAN.md`.
"""

from __future__ import annotations

import argparse
import collections
import datetime as _dt
import json
import pathlib
import re
import shutil
import sys
import textwrap
import time
import typing as _t

from .log import OUTCOME_DISPLAY_ORDER, default_log_path, read_events

# ── Palette ────────────────────────────────────────────────────────────────────────────────
# Swatches loaded from `share/console_palette.json` via `palette.py`, so this console and
# `api/task_console.jl` render as one system with no drift between "green" here and
# "green" there. Semantic mapping (which swatch is "confirmed", which is "fixed") stays
# local — the task console maps the same swatches to "running"/"queued"/etc.
#
# Truecolor SGR — the standard ANSI 8-colour red/green a terminal theme decides for the
# user isn't necessarily CVD-safe. Source & rationale: `palette.py` docstring.
from .palette import (
    BLUE as _BLUE,
    BLUISH_GREEN as _BLUISH_GREEN,
    GREY as _GREY,
    ORANGE as _ORANGE,
    REDDISH_PURPLE as _REDDISH_PURPLE,
    SKY_BLUE as _SKY_BLUE,
    VERMILLION as _VERMILLION,
    YELLOW as _YELLOW,
)

_RESET = "\033[0m"
_BOLD = "\033[1m"
_DIM = "\033[2m"

# Legacy aliases — call sites below reference `_RED`/`_GREEN`/`_CYAN`/`_MAGENTA` by their
# standard names; rebinding to the CVD-safe swatch is a one-line swap for each. The names
# stay because the semantic (alert / positive / info / distinctive) is what matters at the
# use site, not the exact hue.
_RED = _VERMILLION
_GREEN = _BLUISH_GREEN
_CYAN = _SKY_BLUE
_MAGENTA = _REDDISH_PURPLE


def _col(code: str, s: str, *, use_colour: bool) -> str:
    return f"{code}{s}{_RESET}" if use_colour else s


# ── Event → (short_label, colour) — leftmost fixed-width tag, so mechanisms line up as a
# category column. Same grouping as rollup.py; four-letter labels keep the row tight enough
# to fit description hints on the same line most of the time.
_MECH_WIDTH = 4
_MECHANISM_STYLE: dict[str, tuple[str, str]] = {
    "fanout_audit": ("fnut", _BLUE),
    "sibling_audit": ("fnut", _BLUE),  # pre-rename rows fold under the same header (see rollup.py)
    "convention_check": ("conv", _MAGENTA),
    "citation_currency": ("cite", _CYAN),
    "ratchet_hit": ("ratc", _YELLOW),
    "claude_md_eval": ("cmd ", _GREY),
    "human_override": ("ovrd", _GREY),
    "retrospective_miss": ("miss", _RED),
    "plan_logged": ("plan", _GREY),
    "prompt_logged": ("prmt", _GREY),
    "attention_tick": ("attn", _GREY),
}


def _mechanism_of(event: str) -> tuple[str, str]:
    """Return (short_label, colour) for an event name. Unknown events fall back to grey."""
    for prefix, style in _MECHANISM_STYLE.items():
        if event == prefix or event.startswith(prefix + "_"):
            return style
    return (event[:_MECH_WIDTH].ljust(_MECH_WIDTH), _GREY)


#: Reverse index: short label (`fnut`/`conv`/…) → colour. Built at import so a tally row (which
#: knows only the label, not the source event name) can pick the same colour the event stream
#: used. Kept as a mapping instead of a second `_mechanism_of("cite_run")` walk — that lookup
#: would fail, because the event prefix is `citation_currency` and the label is `cite`.
_LABEL_COLOUR: dict[str, str] = {label.strip(): colour for label, colour in _MECHANISM_STYLE.values()}


def _colour_for_label(label: str) -> str:
    return _LABEL_COLOUR.get(label.strip(), _GREY)


# ── Outcome / marker colours — the marker on a finding says how confident the reviewer is;
# the outcome on a resolved row says what the author did about it. Colours cover every entry
# in the shared `OUTCOME_DISPLAY_ORDER` from `log.py` (both renderers walk that list).
_MARKER_COLOUR: dict[str, str] = {
    "confirmed": _VERMILLION,          # alert semantic — the finding is confirmed
    "should reuse": _ORANGE,           # warning semantic — reviewer thinks there's a better path
    "plausible": _ORANGE,              # sibling-audit legacy marker; keep tolerant
}
_OUTCOME_COLOUR: dict[str, str] = {
    "fixed_pre_commit": _GREEN,
    "shipped_with_finding": _YELLOW,
    "false_positive": _GREY,
    "dropped_no_action": _GREY,
    "unresolved": _RED,
}


def _fmt_hms(ts: str) -> str:
    """Trim ISO timestamp to HH:MM:SS in the reader's local time.

    The log stores UTC (see `log.py::_iso_now`). The console reads it in a local terminal,
    where a UTC clock looked odd next to the machine's own — a reviewer that ran at 15:00
    local rendered as "05:00" for a Sydney reader, which broke the "when did this happen"
    intuition. `.astimezone()` with no arg converts to the process's local zone.
    """
    try:
        return _dt.datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone().strftime("%H:%M:%S")
    except (ValueError, AttributeError):
        return ts[-8:] if ts else "--:--:--"


def _parse_since(spec: str) -> _dt.datetime:
    """Parse `--since` spec (e.g. `1h`, `30m`, `2d`, `today`, an ISO timestamp) → UTC datetime.

    Simple grammar — the plans call for readable knobs, not a full date lib. Raises `ValueError`
    on anything that doesn't match, so a typo fails loud at CLI parse time rather than silently
    matching zero events.
    """
    spec = spec.strip()
    now = _dt.datetime.now(_dt.timezone.utc)
    if spec.lower() == "today":
        return now.replace(hour=0, minute=0, second=0, microsecond=0)
    # `--since 1H` reads as `1h` — case-fold only for the relative-unit grammar; the ISO
    # form is case-sensitive (`fromisoformat` rejects a lowercase `t` / `z`).
    m = re.fullmatch(r"(\d+)([smhd])", spec.lower())
    if m:
        n = int(m.group(1))
        unit = m.group(2)
        delta = {"s": _dt.timedelta(seconds=n), "m": _dt.timedelta(minutes=n),
                 "h": _dt.timedelta(hours=n), "d": _dt.timedelta(days=n)}[unit]
        return now - delta
    # Fall through to ISO parse (with or without trailing Z).
    return _dt.datetime.fromisoformat(spec.replace("Z", "+00:00"))


def _ts_dt(ts: str) -> _dt.datetime | None:
    """Parse the row's `ts` into a UTC datetime; None on failure (row is kept anyway)."""
    try:
        return _dt.datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except (ValueError, AttributeError):
        return None


def _fmt_duration(seconds: float) -> str:
    """Compact duration — `0.1s` / `12.3s` / `1m 23s`. Matches the roundup's `median N.Ns`."""
    if seconds < 60:
        return f"{seconds:.1f}s"
    m, s = divmod(int(seconds), 60)
    return f"{m}m {s:02d}s"


def _fmt_context(event: dict) -> str:
    """Compact anchor to the change under review — `#1263 feat/foo@abc1234`.

    PR wins over branch (a PR uniquely names both), so once a branch has an open PR the row
    shrinks to `#1263@abc1234`. `commit` is truncated to 7 chars to match `git log --oneline`.
    Branches with a `type/` prefix (`feat/`, `fix/`, `trace/`) drop the prefix — the type is
    already implicit in the mechanism (e.g. `fnut` on a `fix/` branch), and the trailing
    slug is what the eye is looking for.
    """
    parts = []
    pr = event.get("pr")
    branch = event.get("branch") or ""
    commit = (event.get("commit") or "")[:7]
    if pr:
        parts.append(pr)
    elif branch:
        # Drop the leading `type/` if present — noise in the console context column.
        short = branch.split("/", 1)[1] if "/" in branch else branch
        parts.append(short)
    if commit:
        parts.append(f"@{commit}" if parts else commit)
    return "".join(parts) if len(parts) == 2 else " ".join(parts)


# Column widths — fixed so rows read as a table.
_VERB_WIDTH = 4       # RUN/FIND/RSLV/HIT/MISS — abbreviated so the column is uniform
_DESC_INDENT = 6      # matches `HH:MM:SS  TAG  ` prefix visually
_DEFAULT_WIDTH = 100  # hard-wrap target for the description; overridable by --width

#: How often (seconds) the dashboard redraws when nothing new has landed. Matches the task
#: console's 0.5s throttle — fast enough that a Ctrl-C feels responsive, slow enough to
#: not chew CPU on an idle log.
_REFRESH_TICK = 0.5

#: How many recent events / findings the dashboard panes hold. `pixi run console` bounds its
#: EVENTS at 200 and LOGS at 400; recital is much lower-volume, so smaller caps suffice.
_MAX_EVENTS = 40
_MAX_FINDINGS = 6


class _Tally:
    """Rolling counters over the events the console has seen — the equivalent of
    `task_console.jl::TALLY` for the recital side.

    Grouped by mechanism (`fnut`/`conv`/`cite`/`ratc`), each mechanism carries:
      - `runs`: how many `_run` events landed (including the ones we dropped as quiet).
      - `findings`: total finding events, broken down by marker (`confirmed`/`should reuse`).
      - `resolved`: total `_finding_resolved` events, broken down by outcome.

    So a reader glancing at the tally sees: how much work the reviewers did, how much of it
    flagged something, and what the author did about it — the three questions the `_run`
    stream alone doesn't answer.
    """

    def __init__(self) -> None:
        self.runs: dict[str, int] = {}
        self.findings: dict[str, dict[str, int]] = {}
        self.resolved: dict[str, dict[str, int]] = {}

    def _key(self, event_name: str) -> str:
        label, _ = _mechanism_of(event_name)
        return label.strip()

    def add(self, event: dict) -> None:
        name = event.get("event", "")
        payload = event.get("payload", {}) or {}
        key = self._key(name)
        if name.endswith("_run"):
            self.runs[key] = self.runs.get(key, 0) + 1
        elif name.endswith("_finding_resolved"):
            outcome = payload.get("outcome", "unresolved")
            self.resolved.setdefault(key, {})[outcome] = self.resolved.get(key, {}).get(outcome, 0) + 1
        elif name.endswith("_finding"):
            marker = payload.get("marker", "?")
            self.findings.setdefault(key, {})[marker] = self.findings.get(key, {}).get(marker, 0) + 1
        elif name == "ratchet_hit":
            self.findings.setdefault(key, {})["hit"] = self.findings.get(key, {}).get("hit", 0) + 1
            outcome = payload.get("outcome")
            if outcome:
                self.resolved.setdefault(key, {})[outcome] = self.resolved.get(key, {}).get(outcome, 0) + 1

    def is_empty(self) -> bool:
        return not (self.runs or self.findings or self.resolved)


def _tally_row(tally: _Tally, mech: str, *, use_colour: bool) -> str:
    """Render one mechanism's tally as a compact single line, colour-coded."""
    label = mech + " " * (_MECH_WIDTH - len(mech))
    tag = _col(_colour_for_label(mech), label, use_colour=use_colour)
    parts: list[str] = []
    runs = tally.runs.get(mech, 0)
    if runs:
        parts.append(f"{runs} run{'' if runs == 1 else 's'}")
    fnd = tally.findings.get(mech, {})
    total_findings = sum(fnd.values())
    if total_findings:
        breakdown = ", ".join(f"{n} {marker}" for marker, n in sorted(fnd.items(), key=lambda x: -x[1]))
        noun = "findings" if total_findings != 1 else "finding"
        marker_col = _RED if "confirmed" in fnd else _YELLOW
        parts.append(_col(marker_col, f"{total_findings} {noun}", use_colour=use_colour) + f" ({breakdown})")
    res = tally.resolved.get(mech, {})
    if res:
        for outcome, n in sorted(res.items(), key=lambda x: -x[1]):
            parts.append(_col(_OUTCOME_COLOUR.get(outcome, _GREY),
                              f"{n} {outcome}", use_colour=use_colour))
    return f"{tag}  " + _col(_DIM, " · ", use_colour=use_colour).join(parts)


class DashboardState:
    """The state one dashboard frame renders from — tally + recent events + recent findings.

    Same shape as `task_console.jl`'s in-memory state (TASKS/TALLY/EVENTS/LOGS): counters
    updated on every event, plus two bounded ring buffers for the scrolling panes at the
    bottom. Findings are held as raw event dicts (not pre-rendered strings) so the pane can
    re-wrap them if the terminal width changes between frames.
    """

    def __init__(self, *, max_events: int = _MAX_EVENTS, max_findings: int = _MAX_FINDINGS):
        self.tally = _Tally()
        self.events: collections.deque[dict] = collections.deque(maxlen=max_events)
        self.findings: collections.deque[dict] = collections.deque(maxlen=max_findings)
        self.started_at = _dt.datetime.now(_dt.timezone.utc)
        self.last_event_ts: str | None = None

    def add(self, event: dict) -> None:
        self.tally.add(event)
        # Only push to the event pane if the event has a renderable line (i.e. not a meta row
        # `format_event` would drop). Otherwise the pane fills with invisible rows — the same
        # noise reduction reason `format_event` returns None on those.
        if event.get("event", "").endswith("_finding"):
            self.findings.append(event)
        self.events.append(event)
        self.last_event_ts = event.get("ts") or self.last_event_ts


def _hr(label: str, width: int, *, use_colour: bool) -> str:
    """`── label ─────────` divider — same look as task_console.jl's `── activity ──`."""
    dash_count = max(0, width - len(label) - 4)
    return _col(_DIM, f"── {label} " + "─" * dash_count, use_colour=use_colour)


def _finding_head_line(event: dict, *, use_colour: bool) -> str:
    """Head-line rendering of a `_finding` event — no description, for the activity pane.

    Extracted because both the activity pane (head only) and the findings pane (head + desc)
    render it, and keeping the ordering/spacing in one place stops the two panes from
    drifting apart visually.
    """
    label, colour = _mechanism_of(event.get("event", ""))
    payload = event.get("payload", {}) or {}
    marker = payload.get("marker", "?")
    marker_col = _MARKER_COLOUR.get(marker, _YELLOW)
    ctx = _fmt_context(event)
    ctx_str = "  " + _col(_DIM, ctx, use_colour=use_colour) if ctx else ""
    return (f"{_col(_GREY, _fmt_hms(event.get('ts', '')), use_colour=use_colour)} "
            f"{_col(colour, label, use_colour=use_colour)} "
            f"{_col(_BOLD, 'FIND'.ljust(_VERB_WIDTH), use_colour=use_colour)} "
            f"{_col(marker_col, marker, use_colour=use_colour)}  "
            f"{payload.get('file', '?')}:{payload.get('line', '?')}"
            f"{ctx_str}")


def _render_finding_block(event: dict, *, width: int, use_colour: bool,
                          desc_line_cap: int) -> list[str]:
    """One finding for the findings pane — head + wrapped description, description capped.

    `desc_line_cap` bounds how many wrapped lines a single description contributes; anything
    beyond that is elided with `…`. Without a per-finding cap one long description eats the
    whole pane and pushes the counters off-screen — the failure this whole refactor exists
    to fix.
    """
    out = [_finding_head_line(event, use_colour=use_colour)]
    desc = ((event.get("payload") or {}).get("desc") or "").strip()
    if not desc or desc_line_cap <= 0:
        return out
    indent = " " * _DESC_INDENT
    first = indent + _col(_DIM, "↳ ", use_colour=use_colour)
    wrapped = _wrap_desc(" ".join(desc.split()), width=width - 2,
                         indent=indent + "  ", first_prefix=first).splitlines()
    if len(wrapped) > desc_line_cap:
        wrapped = wrapped[:desc_line_cap - 1] + [indent + "  " + _col(_DIM, "…", use_colour=use_colour)]
    out.extend(wrapped)
    return out


def render_dashboard(state: DashboardState, log_path: pathlib.Path, *,
                     width: int, height: int = 40, use_colour: bool = True) -> str:
    """Build the full-screen dashboard as one string, sized to fit `height` rows.

    Layout (mirrors `task_console.jl::render()`):
      - Title line: name · log path · current local time
      - Header counters: total runs · total findings by marker · total resolved by outcome
      - `── by mechanism ──` block: one row per mechanism
      - `── recent findings ──` pane: newest N findings with capped descriptions
      - `── activity ──` pane: newest M event lines

    Height budgeting — the same problem the task console solves: fixed chrome (title + counter
    line + section headers + blanks) is subtracted first, then the remainder is split between
    the findings pane (priority — the "what was flagged" the cockpit exists for) and the
    activity pane. If the terminal is genuinely tiny (< ~15 rows) the activity pane collapses
    to zero and the findings pane keeps one finding; below that only the counters remain.
    """
    # Local time — matches per-event rows (`_fmt_hms`), so the reader compares like-for-like.
    now = _dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # ── Chrome (unbudgeted — a terminal that can't fit the counters isn't usable anyway) ──
    title = _col(_BOLD, "Cecelia recital console", use_colour=use_colour)
    path_str = _col(_DIM, str(log_path), use_colour=use_colour)
    time_str = _col(_GREY, now, use_colour=use_colour)
    chrome: list[str] = [f"{title}  {path_str}   {time_str}"]

    total_runs = sum(state.tally.runs.values())
    total_findings = sum(sum(v.values()) for v in state.tally.findings.values())
    confirmed = sum(v.get("confirmed", 0) for v in state.tally.findings.values())
    should_reuse = sum(v.get("should reuse", 0) for v in state.tally.findings.values())
    header_parts: list[str] = []
    if total_runs:
        header_parts.append(_col(_CYAN, f"{total_runs} runs", use_colour=use_colour))
    if total_findings:
        header_parts.append(_col(_RED if confirmed else _YELLOW,
                                 f"{total_findings} findings", use_colour=use_colour))
    if confirmed:
        header_parts.append(_col(_RED, f"{confirmed} confirmed", use_colour=use_colour))
    if should_reuse:
        header_parts.append(_col(_YELLOW, f"{should_reuse} should reuse", use_colour=use_colour))
    for outcome in OUTCOME_DISPLAY_ORDER:
        n = sum(v.get(outcome, 0) for v in state.tally.resolved.values())
        if n:
            header_parts.append(_col(_OUTCOME_COLOUR.get(outcome, _GREY),
                                     f"{n} {outcome}", use_colour=use_colour))
    chrome.append(_col(_DIM, " · ", use_colour=use_colour).join(header_parts) or
                  _col(_DIM, "no events yet", use_colour=use_colour))
    chrome.append("")

    # By-mechanism block (compact — one row per mechanism, still counts as chrome so it
    # doesn't compete with the findings pane for the row budget).
    seen_mechs = sorted(set(state.tally.runs) | set(state.tally.findings) | set(state.tally.resolved))
    if seen_mechs:
        chrome.append(_hr("by mechanism", width, use_colour=use_colour))
        for mech in seen_mechs:
            chrome.append("  " + _tally_row(state.tally, mech, use_colour=use_colour))
        chrome.append("")

    # ── Budgeted panes ────────────────────────────────────────────────────────────────────
    # Reserve one line per pane header + one trailing newline slot.
    # `budget` = rows left after chrome and pane headers.
    have_findings = bool(state.findings)
    header_rows = (1 if have_findings else 0) + 1  # findings header + activity header
    budget = max(0, height - len(chrome) - header_rows)

    findings_block: list[str] = []
    if have_findings and budget > 0:
        # Findings pane gets roughly two-thirds of the pane budget, with a floor so it never
        # collapses to zero if there are findings to show.
        findings_budget = max(2, (budget * 2) // 3)
        # Per-finding cap keeps one long description from starving other findings.
        per_finding_cap = 4  # head + up to 3 desc lines with `…` if longer
        # Newest first, so a fresh flag appears at the top of the pane.
        for f in reversed(state.findings):
            block = _render_finding_block(f, width=width, use_colour=use_colour,
                                          desc_line_cap=per_finding_cap - 1)
            if len(findings_block) + len(block) > findings_budget:
                # Room for at least the head line? Show it truncated; otherwise stop.
                room = findings_budget - len(findings_block)
                if room >= 1:
                    findings_block.extend(block[:room])
                break
            findings_block.extend(block)
        findings_block.insert(0, _hr("recent findings", width, use_colour=use_colour))

    activity_budget = max(0, height - len(chrome) - len(findings_block) - 1)  # -1 for activity hdr
    activity_block: list[str] = [_hr("activity", width, use_colour=use_colour)]
    if activity_budget <= 0:
        activity_block = []  # terminal too small for both panes; drop activity entirely
    else:
        shown = 0
        for e in reversed(state.events):
            if shown >= activity_budget:
                break
            if e.get("event", "").endswith("_finding"):
                activity_block.append(_finding_head_line(e, use_colour=use_colour))
            else:
                rendered = format_event(e, use_colour=use_colour, width=width - 2)
                if rendered:
                    # First line only — a finding block's second line is already covered
                    # by the findings pane above.
                    activity_block.append(rendered.splitlines()[0])
                else:
                    continue  # dropped meta event; don't count toward the cap
            shown += 1
        if not state.events:
            activity_block.append(_col(_DIM, "  waiting for events…", use_colour=use_colour))

    return "\n".join(chrome + findings_block + activity_block)


def _wrap_desc(desc: str, *, width: int, indent: str, first_prefix: str) -> str:
    """Wrap a description with a hanging indent so it reads as one paragraph, not raw reflow.

    The terminal's own wrap breaks mid-word and re-continues at column 0, which reads as a
    new event — the whole reason the raw `tail | jq` view was hard to skim. `textwrap` gives
    us a proper hanging indent (`indent` on every line after the first).
    """
    desc = " ".join(desc.split())  # collapse internal whitespace incl. newlines
    return textwrap.fill(
        desc, width=width,
        initial_indent=first_prefix,
        subsequent_indent=indent,
        break_long_words=False,
        break_on_hyphens=False,
    )


def format_event(event: dict, *, use_colour: bool = True,
                 width: int = _DEFAULT_WIDTH) -> str | None:
    """Render one event row as a printable console line, or return None to skip.

    Return contract — None means "don't print this row". Rows carrying no reviewer signal
    (`plan_logged`/`prompt_logged`, quiet citation-currency runs) are dropped rather than
    padded with placeholder text, on the same principle that motivates the console over
    `tail | jq`: less noise, not more.
    """
    name = event.get("event", "")
    label, colour = _mechanism_of(name)
    ts = _col(_GREY, _fmt_hms(event.get("ts", "")), use_colour=use_colour)
    tag = _col(colour, label, use_colour=use_colour)
    payload = event.get("payload", {}) or {}
    context = _fmt_context(event)
    ctx_str = "  " + _col(_DIM, context, use_colour=use_colour) if context else ""

    def _verb(v: str) -> str:
        return _col(_BOLD, v.ljust(_VERB_WIDTH), use_colour=use_colour)

    if name.endswith("_finding"):
        marker = payload.get("marker", "?")
        marker_col = _MARKER_COLOUR.get(marker, _YELLOW)
        marker_str = _col(marker_col, f"[{marker}]", use_colour=use_colour)
        slug = payload.get("slug", "")
        slug_str = _col(_DIM, slug, use_colour=use_colour) if slug else ""
        file_line = f"{payload.get('file', '?')}:{payload.get('line', '?')}"
        head = f"{ts} {tag} {_verb('FIND')} {marker_str}  {file_line}  {slug_str}{ctx_str}"
        desc = (payload.get("desc") or "").strip()
        if not desc:
            return head
        indent = " " * _DESC_INDENT
        first = indent + _col(_DIM, "↳ ", use_colour=use_colour)
        # Colour codes don't count against textwrap's width — measure with plain first-indent.
        wrapped = _wrap_desc(desc, width=width, indent=indent + "  ", first_prefix=first)
        return f"{head}\n{wrapped}"

    if name.endswith("_finding_resolved"):
        outcome = payload.get("outcome", "?")
        outcome_col = _OUTCOME_COLOUR.get(outcome, _GREY)
        outcome_str = _col(outcome_col, outcome, use_colour=use_colour)
        slug = payload.get("slug", "")
        slug_str = _col(_DIM, slug, use_colour=use_colour) if slug else ""
        return f"{ts} {tag} {_verb('RSLV')} {outcome_str}  {slug_str}{ctx_str}"

    if name.endswith("_run"):
        duration = payload.get("duration_s")
        dur_str = _fmt_duration(duration) if isinstance(duration, (int, float)) else "?"
        # Errored recital runs — `recital.py::_run_reviewer` puts the exception string in
        # `payload.error` when the reviewer subprocess fails hard (non-zero exit, timeout,
        # missing CLI) but still emits the `_run` row so the fold is legible. Show them as
        # `ERR` in red so the reader (and Sonnet reviewing the stream) can tell a timed-out
        # spawn from a genuine 3-minute review. Also honours `payload.verdict == "error"`
        # from `claude_md_eval_run`, which uses the same signal in a different field.
        errored = (isinstance(payload.get("error"), str) and bool(payload["error"])) \
            or payload.get("verdict") == "error"
        run_verb = _col(_RED, "ERR ", use_colour=use_colour) if errored \
            else _verb("RUN")
        if name == "citation_currency_run":
            # Signal-only extras — a quiet citation-currency run (0 warnings, 0 staged files)
            # has nothing for a reader to act on, so drop the whole row. Same principle for
            # empty convention / fanout runs: the `_finding` rows carry the signal, `_run`
            # says only "the reviewer ran" and matters chiefly when there are counts to
            # eyeball. Errored runs render regardless — an ERR row IS the signal.
            warnings = payload.get("warnings_emitted", 0) or 0
            staged = payload.get("staged_files_checked", 0) or 0
            if warnings == 0 and staged == 0 and not errored:
                return None
            extras = []
            if warnings:
                extras.append(_col(_YELLOW, f"{warnings} warn", use_colour=use_colour))
            if staged:
                extras.append(f"{staged} staged")
            extras_str = "  " + "  ".join(extras) if extras else ""
            return f"{ts} {tag} {run_verb} {dur_str}{extras_str}{ctx_str}"
        # For fanout/convention runs the timing is the whole `_run` payload — findings render
        # separately. Suppress extras to keep the row narrow.
        return f"{ts} {tag} {run_verb} {dur_str}{ctx_str}"

    if name == "ratchet_hit":
        rid = payload.get("ratchet_id", "?")
        outcome = payload.get("outcome", "")
        outcome_str = "  " + _col(_OUTCOME_COLOUR.get(outcome, _GREY),
                                  outcome, use_colour=use_colour) if outcome else ""
        return f"{ts} {tag} {_verb('HIT')} {rid}{outcome_str}{ctx_str}"

    if name == "retrospective_miss":
        bug_class = payload.get("bug_class", "?")
        return f"{ts} {tag} {_verb('MISS')} {bug_class}{ctx_str}"

    # Silently drop meta rows (plan_logged/prompt_logged/attention_tick — they carry no per-run
    # signal a reviewer would flag). Unknown events fall through here too so a schema addition
    # doesn't crash an already-running console; they can be re-mapped explicitly when useful.
    return None


def _tail(events: _t.Sequence[dict], n: int, since: _dt.datetime | None) -> list[dict]:
    """Apply the initial-backlog filter — `--since` beats `--tail` when both are supplied.

    The typical run is `--tail 50` (last N events), but on a busy day the user wants "today"
    instead, and `--since today` should show ALL of today's rows even if that is more than
    50. Symmetric to `journalctl` conventions.
    """
    if since is not None:
        return [e for e in events if (_ts_dt(e.get("ts", "")) or since) >= since]
    if n <= 0:
        return list(events)
    return list(events)[-n:]


def follow_events(
    log_path: pathlib.Path,
    *,
    poll_interval: float = 0.5,
    start_offset: int | None = None,
    stop_after_one_pass: bool = False,
) -> _t.Iterator[dict]:
    """Yield new events as they land in `log_path`. Starts at `start_offset` bytes.

    `stop_after_one_pass` is the seam that lets tests drive the loop without a real clock —
    they set it True, and the generator returns after emitting the events already on disk.
    Production leaves it False, so the loop runs until the process is interrupted.

    Rotated / truncated file → the offset resets to 0, so the console keeps working across a
    log rotation. A malformed line (partial write at process kill) is skipped silently by
    `read_events`; the same generator handles the follow case, so the behaviour is identical
    to the batch reader — the failure that motivated this is a partial jsonl line wedging
    every downstream consumer at once.
    """
    if not log_path.exists():
        # Wait for the file to appear — the log is created on first `append_event`, which may
        # not have happened yet on a fresh install.
        if stop_after_one_pass:
            return
        while not log_path.exists():
            time.sleep(poll_interval)

    offset = start_offset if start_offset is not None else log_path.stat().st_size
    while True:
        try:
            size = log_path.stat().st_size
        except FileNotFoundError:
            if stop_after_one_pass:
                return
            time.sleep(poll_interval)
            continue
        if size < offset:
            # File was rotated or truncated — reread from the top.
            offset = 0
        if size > offset:
            with log_path.open("r", encoding="utf-8") as fh:
                fh.seek(offset)
                for line in fh:
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        yield json.loads(line)
                    except json.JSONDecodeError:
                        continue
                offset = fh.tell()
        if stop_after_one_pass:
            return
        time.sleep(poll_interval)


def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="recital-console",
        description=(
            "Live view of the AI-assist effectiveness log — the recital-side twin of "
            "`pixi run console`. TTY: full-screen dashboard (counters + recent findings + "
            "activity pane). Non-TTY / --stream: append-only formatted event stream, one "
            "row per event. Auto-picks the right mode based on stdout."
        ),
    )
    p.add_argument("--tail", type=int, default=50, metavar="N",
                   help="Seed the state from the last N events on startup (default: 50; "
                        "0 = whole history). Both modes use it.")
    p.add_argument("--since", metavar="SPEC",
                   help="Seed the state from events at or after this time (1h, 30m, 2d, "
                        "today, or an ISO timestamp). Overrides --tail for the backlog.")
    p.add_argument("--no-follow", action="store_true",
                   help="Print the seed state and exit; don't wait for new events. In TTY "
                        "mode this renders the dashboard once and exits.")
    p.add_argument("--stream", action="store_true",
                   help="Force append-only stream mode (no dashboard, no ANSI). Auto-enabled "
                        "when stdout isn't a TTY, so `... | tee out.log` works out of the box.")
    p.add_argument("--log-path", type=pathlib.Path,
                   help="Override the log path (default: $CECELIA_EFFECTIVENESS_LOG or "
                        "~/.cecelia-effectiveness/events.jsonl).")
    p.add_argument("--width", type=int, default=0, metavar="COLS",
                   help="Wrap descriptions at COLS (default: terminal width, min 60).")
    return p


def _terminal_size(default: tuple[int, int] = (_DEFAULT_WIDTH, 40)) -> tuple[int, int]:
    """Return `(cols, rows)`, floor-clamped so a truly tiny report still renders the counters."""
    try:
        size = shutil.get_terminal_size(default)
        return (max(60, size.columns), max(15, size.lines))
    except (ValueError, OSError):
        return default


def _terminal_width(default: int = _DEFAULT_WIDTH) -> int:
    return _terminal_size((default, 40))[0]


def _run_stream_mode(seed_events: _t.Iterable[dict], log_path: pathlib.Path,
                     *, out: _t.TextIO, use_colour: bool, follow: bool, width: int) -> int:
    """Append-only formatted event stream. What `... | tee out.log` produces."""
    for event in seed_events:
        line = format_event(event, use_colour=use_colour, width=width)
        if line is not None:
            print(line, file=out, flush=True)
    if not follow:
        return 0
    try:
        start = log_path.stat().st_size if log_path.exists() else 0
        for event in follow_events(log_path, start_offset=start):
            line = format_event(event, use_colour=use_colour, width=width)
            if line is not None:
                print(line, file=out, flush=True)
    except KeyboardInterrupt:
        pass
    return 0


def _run_dashboard_mode(seed_events: _t.Iterable[dict], log_path: pathlib.Path,
                        *, out: _t.TextIO, follow: bool) -> int:
    """Full-screen dashboard — clear + redraw on every event and every _REFRESH_TICK.

    The refresh tick exists to keep the title line's `HH:MM:SS` clock alive even when nothing
    is landing — same reason task_console.jl redraws on a timer, not just on frames.
    """
    state = DashboardState()
    for event in seed_events:
        state.add(event)

    def _paint() -> None:
        width, height = _terminal_size()
        # `\033[2J\033[3J\033[H` — clear the viewport AND the scrollback (`\033[3J`), then
        # home the cursor. Without the scrollback clear the previous frame lives one page
        # up and Shift-PgUp reveals a ghost trail. `\033[?25l` hides the cursor so it
        # doesn't jitter mid-paint; restored on exit. The `-1` on height reserves one row
        # for the trailing newline `print` adds — otherwise the last row of the dashboard
        # sits under the terminal's own bottom line and the topmost row is scrolled off.
        out.write("\033[?25l\033[2J\033[3J\033[H")
        out.write(render_dashboard(state, log_path, width=width, height=height - 1,
                                    use_colour=True))
        out.write("\n")
        out.flush()

    _paint()
    if not follow:
        # Leave the last frame on screen; still restore cursor.
        out.write("\033[?25h")
        out.flush()
        return 0

    try:
        start = log_path.stat().st_size if log_path.exists() else 0
        gen = follow_events(log_path, start_offset=start, poll_interval=_REFRESH_TICK)
        last_paint = time.monotonic()
        # The follow generator blocks on `time.sleep(poll_interval)` between polls, so it
        # yields at least every _REFRESH_TICK when idle — that gives the loop a natural
        # heartbeat for the title-line clock. On busy periods it yields per event, so the
        # dashboard updates as soon as a new row lands.
        for event in gen:
            state.add(event)
            _paint()
            last_paint = time.monotonic()
    except KeyboardInterrupt:
        pass
    finally:
        # Restore the cursor no matter how we exit — leaving it hidden across a `pixi run`
        # is the worst debug session I never want to repeat.
        out.write("\033[?25h")
        out.flush()
    _ = last_paint  # silence linter — kept for future "n seconds since paint" HUD line
    return 0


def main(argv: _t.Sequence[str] | None = None,
         *, stdout: _t.TextIO | None = None,
         is_tty: bool | None = None) -> int:
    """Entry point. `stdout`/`is_tty` are test seams — production uses `sys.stdout`.

    Return code convention: 0 on clean exit (Ctrl-C or end of --no-follow); non-zero only on
    a bad CLI arg (argparse handles the exit itself).
    """
    args = _build_parser().parse_args(argv)
    out = stdout if stdout is not None else sys.stdout
    tty = is_tty if is_tty is not None else out.isatty()

    log_path = args.log_path or default_log_path()
    since = _parse_since(args.since) if args.since else None
    width = args.width if args.width > 0 else _terminal_width()

    seed = _tail(list(read_events(log_path)), args.tail, since)

    if args.stream or not tty:
        # Pipe-friendly append-only view. Colour is off by default when piped (a `| tee`
        # target should be plain), on when explicitly connected to a TTY-but-forced-stream.
        return _run_stream_mode(seed, log_path, out=out,
                                use_colour=tty and not args.stream,
                                follow=not args.no_follow, width=width)

    return _run_dashboard_mode(seed, log_path, out=out, follow=not args.no_follow)


if __name__ == "__main__":
    raise SystemExit(main())
