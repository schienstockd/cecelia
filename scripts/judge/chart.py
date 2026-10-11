#!/usr/bin/env python3
"""Effectiveness chart — the README's public picture of how the agents' pre-commit reviews go, per week.

Design and what it can't tell you: docs/ai-assist/AGENT_RECORD.md. Operator notes:
docs/ai-assist/WEEKLY_JUDGE.md → *The chart*.

Three steps, each its own function so a test can drive it:

1. `aggregate` — the effectiveness log, the judge's pass records and `git log` → per-week numbers.
   Aggregates only: no finding text, path, uid, branch or session id leaves this step.
2. `render_svg` — those numbers → one SVG, byte-for-byte deterministic (same numbers, same bytes).
   No LLM, no free text from the log: every label is ours or a number, and still escaped.
3. `publish` — an orphan single-commit `judge-assets` branch, force-pushed, built with git plumbing
   (no checkout is touched). Skipped when the SVG is unchanged and the branch still holds the last
   push. The `git` runner is allowlisted to exactly those plumbing calls and that one refspec, so
   the owner's token can't reach `main` through it.

The weekly pass calls `update` best-effort after its record is written: a chart failure is printed
and kept on the record (`run.chart`), never fails the judge.

Usage:
    pixi run effectiveness-chart                  # render to ~/.cecelia-effectiveness/chart/
    pixi run effectiveness-chart -- --dry-run     # also print the git calls a push would make
    pixi run effectiveness-chart -- --push        # render and publish to `judge-assets`
"""
from __future__ import annotations

import argparse
import collections
import datetime as _dt
import hashlib
import html
import importlib.util as _importlib_util
import json
import math
import pathlib
import re
import shutil
import subprocess
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.log import OUTCOME_DISPLAY_ORDER, OUTCOME_VOCABULARY, is_errored_run  # noqa: E402
from cecelia.effectiveness.palette import load_hex  # noqa: E402
from cecelia.effectiveness.rollup import _REDTEAM_SLUGS, _latest_resolutions_by_slug  # noqa: E402
from cecelia.effectiveness.state import state_dir  # noqa: E402
from cecelia.utils.atomic_io import write_atomic, write_json_atomic  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")
_bugs = _load_sibling("bugs")

BRANCH = "judge-assets"
REF = f"refs/heads/{BRANCH}"
SVG_NAME, DATA_NAME = "effectiveness.svg", "effectiveness.json"
#: Weeks shown, newest last. Older weeks stay in the JSON beside the SVG.
MAX_WEEKS = 12
#: A week with fewer sessions than this is drawn hollow, like a partial one.
LOW_SESSIONS = 10
#: The reviewer prompts (the rubric) and the rules they check against: an edit is noted on its week.
PROMPT_DOCS = ("docs/ai-assist/FANOUT_AUDIT.md", "docs/ai-assist/CONVENTION_CHECK.md")
RULE_DOCS = ("CLAUDE.md",)
_RUN_EVENTS = ("fanout_audit_run", "convention_check_run")
_FINDING_EVENTS = ("fanout_audit_finding", "convention_check_finding")
_RESOLVED_EVENTS = ("fanout_audit_finding_resolved", "convention_check_finding_resolved")
#: Legend text and swatch per outcome in the log's closed vocabulary, plus `none`: no outcome logged,
#: or one this map doesn't know. The chart never echoes a value from the log.
_OUTCOME_LOOK = {
    "fixed_pre_commit": ("fixed before commit", "bluish_green"),
    "shipped_with_finding": ("shipped knowingly", "orange"),
    "false_positive": ("called a false positive", "sky_blue"),
    "dropped_no_action": ("dropped", "vermillion"),
}
assert set(_OUTCOME_LOOK) == OUTCOME_VOCABULARY, "a new outcome needs a legend entry and a swatch here"
#: Outcome buckets, in the log's display order, then `none`: the JSON's and the description's split.
OUTCOMES = tuple((k, *_OUTCOME_LOOK[k]) for k in OUTCOME_DISPLAY_ORDER if k in _OUTCOME_LOOK) \
    + (("none", "no outcome logged", "grey"),)
_OUTCOME_KEYS = set(_OUTCOME_LOOK)
#: Ink for every label: mid grey, legible on GitHub's light and dark README backgrounds alike. The
#: background stays transparent, so one file serves both themes.
INK = "#7F7F7F"
#: The two reviewers: key, legend name, swatch.
REVIEWERS = (("fanout", "Fan-out audit", "blue"), ("convention", "Convention check", "orange"))
#: Merged PRs, beside the changes reviewed: how many checks went into each PR.
PR_SWATCH = "sky_blue"
_REVIEWER_OF = {"fanout_audit_finding": "fanout", "convention_check_finding": "convention"}


# ── 1. aggregate ───────────────────────────────────────────────────────────────────────────────

def week_of(day: _dt.date) -> _dt.date:
    """The Monday of `day`'s ISO week."""
    return day - _dt.timedelta(days=day.weekday())


def _day(ts: str | None) -> _dt.date | None:
    try:
        return _dt.date.fromisoformat((ts or "")[:10])
    except ValueError:
        return None


def doc_edits(paths: _t.Sequence[str], *, repo: pathlib.Path = _REPO, ref: str = "HEAD") -> list[_dt.date]:
    """The dates the commits on `ref`'s first-parent line that touched `paths` landed: one per merged
    PR, not one per commit inside it. An empty list when git can't answer."""
    out = git_output("log", "--first-parent", "--format=%cs", ref, "--", *paths, cwd=str(repo))
    return sorted(d for d in (_day(line) for line in (out or "").splitlines()) if d)


def pr_dates(log: str) -> list[_dt.date]:
    """The days PRs merged, from `git log --first-parent --format='%cs %s'` output."""
    out = []
    for line in log.splitlines():
        day, _, subject = line.partition(" ")
        # a merge commit, or a squash merge's `(#123)` subject: the judge's own two patterns
        if (_bugs._MERGE_PR.match(subject) or _bugs._PR_IN_SUBJECT.search(subject)) and _day(day):
            out.append(_day(day))
    return sorted(out)


def pr_merges(*, repo: pathlib.Path = _REPO, ref: str = "HEAD") -> list[_dt.date]:
    """The days PRs merged into `ref`. An empty list when git can't answer."""
    return pr_dates(git_output("log", "--first-parent", "--format=%cs %s", ref, cwd=str(repo)) or "")


def aggregate(events: _t.Iterable[dict], records: _t.Sequence[dict], *,
              prompt_edits: _t.Sequence[_dt.date] = (), rule_edits: _t.Sequence[_dt.date] = (),
              prs: _t.Sequence[_dt.date] = (), as_of: _dt.date | None = None) -> dict:
    """Per-week numbers for the chart; JSON-ready and free of any text from the log.

    A *change reviewed* is one branch + parent commit a recital reviewed (reruns on the same change
    count once; an errored reviewer counts none). A *finding* is one fanout or convention finding,
    once per slug, in the week it was first raised, with its latest logged outcome. A week is
    *partial* if logging began inside it, or it hasn't ended by `as_of` (default: the last logged day)."""
    events = list(events)
    changes: dict[_dt.date, set] = collections.defaultdict(set)
    sessions: dict[_dt.date, set] = collections.defaultdict(set)
    first: dict[str, dict] = {}
    slugless: list[dict] = []
    raised: dict[str, _dt.date] = {}  # judge key → week raised, for every finding and advisory
    days: list[_dt.date] = []
    for e in events:
        day = _day(e.get("ts"))
        if day is None:
            continue
        days.append(day)
        name, payload = e.get("event"), e.get("payload") or {}
        if name in _RUN_EVENTS and not is_errored_run(payload):
            changes[week_of(day)].add((e.get("branch"), e.get("commit")))
            sessions[week_of(day)].add(e.get("session"))
        if name in ("fanout_audit_finding", "fanout_audit_advisory"):
            key = _bugs._key(e)
            raised[key] = min(raised.get(key, week_of(day)), week_of(day))
        if name in _FINDING_EVENTS:
            slug = payload.get("slug")
            if slug in _REDTEAM_SLUGS:
                continue
            if not slug:
                slugless.append(e)
            elif slug not in first or e.get("ts", "") < first[slug].get("ts", ""):
                first[slug] = e
    resolved = _latest_resolutions_by_slug(events, _RESOLVED_EVENTS)
    found: dict[tuple, collections.Counter] = collections.defaultdict(collections.Counter)
    for e, outcome in [(e, ((resolved.get(s) or {}).get("payload") or {}).get("outcome")) for s, e in first.items()] \
            + [(e, (e.get("payload") or {}).get("outcome")) for e in slugless]:
        found[week_of(_day(e["ts"])), _REVIEWER_OF[e["event"]]][outcome if outcome in _OUTCOME_KEYS else "none"] += 1

    if not days:
        return {"weeks": [], "first_day": None, "last_day": None, "sessions": 0,
                "judge": _judge_summary(records, raised)}
    first_day, last_day = min(days), max(days)
    as_of = max(as_of or last_day, last_day)
    weeks = []
    monday = week_of(first_day)
    while monday <= last_day:
        sunday = monday + _dt.timedelta(days=6)
        by_reviewer = {r: {k: found[monday, r][k] for k, _, _ in OUTCOMES} for r, _, _ in REVIEWERS}
        weeks.append({
            "week": monday.isoformat(),
            "partial": monday < first_day or sunday >= as_of,
            "changes": len(changes[monday]),
            "prs": sum(max(monday, first_day) <= d <= sunday for d in prs),
            "sessions": len(sessions[monday] - {None}),
            "findings": {k: sum(c[k] for c in by_reviewer.values()) for k, _, _ in OUTCOMES},
            "by_reviewer": by_reviewer,
            "prompt_edits": sum(max(monday, first_day) <= d <= sunday for d in prompt_edits),
            "rule_edits": sum(max(monday, first_day) <= d <= sunday for d in rule_edits),
        })
        monday += _dt.timedelta(days=7)
    all_sessions = set().union(*sessions.values()) - {None} if sessions else set()
    return {"weeks": weeks, "first_day": first_day.isoformat(), "last_day": last_day.isoformat(),
            "sessions": len(all_sessions), "judge": _judge_summary(records, raised)}


#: The bug statuses the chart's judge line names: the ones with a verdict. `unjudged` / `unmerged`
#: are still waiting for one, and `wont_fix` is the owner's answer, not the judge's.
JUDGE_STATUSES = ("open", "parked", "gone", "dismissed")
assert set(JUDGE_STATUSES) <= set(_record.BUG_STATUSES)
#: The judge panel's lines: a verdict, the statuses it pools, its swatch. Open and parked are both
#: real bugs (parked: can't happen in today's code yet).
JUDGE_LINES = (
    ("real bug", ("open", "parked"), "vermillion"),
    ("gone", ("gone",), "bluish_green"),
    ("dismissed", ("dismissed",), "grey"),
)
assert {s for _, group, _ in JUDGE_LINES for s in group} == set(JUDGE_STATUSES)


def _judge_summary(records: _t.Sequence[dict], raised: dict[str, _dt.date] | None = None) -> dict | None:
    """The newest pass record's bug counts by status: what the judge found when it checked the
    findings that shipped against `main`. `by_week` splits the fan-out items among them (unfixed
    findings and advisories: the only reviewer output the judge re-checks) by the week they were
    raised, from `raised` (judge key → Monday) or else the week the judge first saw them."""
    passes = [r for r in records if r.get("kind") == "pass"]
    if not passes:
        return None
    last = max(passes, key=lambda r: r.get("date", ""))
    bugs = last.get("bugs") or []
    counts = collections.Counter(b.get("status") for b in bugs)
    by_week: dict[str, collections.Counter] = collections.defaultdict(collections.Counter)
    for b in bugs:
        key, status = str(b.get("key") or ""), b.get("status")
        if not key.startswith("fanout-") or status not in JUDGE_STATUSES:
            continue
        monday = (raised or {}).get(key) or (_day(b.get("first_seen")) and week_of(_day(b["first_seen"])))
        if monday:
            by_week[monday.isoformat()][status] += 1
    return {"date": last.get("date"), "passes": len(passes),
            **{s: counts.get(s, 0) for s in JUDGE_STATUSES},
            "by_week": {w: {s: c[s] for s in JUDGE_STATUSES} for w, c in sorted(by_week.items())}}


# ── 2. render ──────────────────────────────────────────────────────────────────────────────────

_CONTROL = re.compile(r"[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]")


def esc(text: _t.Any) -> str:
    """Text for an SVG text node or attribute: XML-escaped, control characters dropped."""
    return html.escape(_CONTROL.sub("", str(text)), quote=True)


def _fmt_day(d: str) -> str:
    day = _dt.date.fromisoformat(d)
    return f"{day:%b} {day.day}"


def _week_label(monday: str) -> str:
    start = _dt.date.fromisoformat(monday)
    return f"{_fmt_day(monday)}–{_fmt_day((start + _dt.timedelta(days=6)).isoformat())}"


def describe(data: dict) -> str:
    """The chart in words: the SVG's `<desc>`, for a screen reader or anyone who can't see the lines."""
    if not data["weeks"]:
        return "No reviews logged yet."
    parts = []
    for w in data["weeks"]:
        f = w["findings"]
        total = sum(f.values())
        split = "; ".join(f"{name}: {sum(w['by_reviewer'][r].values())} findings, "
                          f"{w['by_reviewer'][r]['fixed_pre_commit']} fixed before commit"
                          for r, name, _ in REVIEWERS)
        parts.append(f"Week of {_week_label(w['week'])}{' (partial)' if w['partial'] else ''}: "
                     f"{w['changes']} changes reviewed across {w['sessions']} sessions, {w['prs']} PRs merged; "
                     f"{total} findings, "
                     + ", ".join(f"{f[k]} {label}" for k, label, _ in OUTCOMES) + f" ({split}).")
    j = data.get("judge")
    if j:
        parts.append(f"Weekly judge, pass of {j['date']}: {j['open']} open bugs, {j['parked']} parked, "
                     f"{j['gone']} gone, {j['dismissed']} dismissed.")
        for week, c in j.get("by_week", {}).items():
            parts.append(f"Fan-out findings and advisories nobody fixed, raised the week of {_week_label(week)}: "
                         + ", ".join(f"{sum(c[s] for s in group)} {name}" for name, group, _ in JUDGE_LINES) + ".")
    return " ".join(parts)


def _total(c: dict) -> int:
    return sum(c.values())


def _per_100(w: dict, r: str) -> float | None:
    n = _total(w["by_reviewer"][r])
    return 100 * n / w["changes"] if w["changes"] and n else None


def _fixed(w: dict, r: str) -> float | None:
    c = w["by_reviewer"][r]
    return 100 * c["fixed_pre_commit"] / _total(c) if _total(c) else None


def thin(w: dict) -> bool:
    """A week drawn hollow and dashed: still filling up, or too few sessions to lean on."""
    return w["partial"] or w["sessions"] < LOW_SESSIONS


def _ends(w: dict) -> str:
    return (_dt.date.fromisoformat(w["week"]) + _dt.timedelta(days=6)).isoformat()


def _ticks(peak: float, *, percent: bool = False) -> list[float]:
    """Round axis ticks from 0 to at least `peak`: steps of 1, 2, 2.5 or 5 × 10ⁿ, four or so."""
    if percent:
        return [0, 25, 50, 75, 100]
    peak = max(peak, 1)
    raw = max(peak / 4, 1)  # counts: no fractional steps
    mag = 10 ** math.floor(math.log10(raw))
    step = next(m * mag for m in (1, 2, 2.5, 5, 10) if m * mag >= raw)
    return [k * step for k in range(math.ceil(peak / step - 1e-9) + 1)]


def _panel(add, *, x: float, y: float, w: float, letter: str, title: str, ylabel: str, weeks: list[dict],
           series: list[tuple[str, str, list, list]], percent: bool = False, xlabel: bool = False) -> None:
    """One panel at (x, y), drawn like a paper figure: a letter, a title, a legend, a y axis with
    round ticks and its label, x ticks per week. `series` is (legend name, colour, value per week
    (None = no point), hollow per week)."""
    top, bot = y + 40, y + 170
    ax, x1 = x + 58, x + w - 10
    add(f'<text x="{x:.1f}" y="{y:.1f}" font-size="15" font-weight="700">{esc(letter)}</text>')
    add(f'<text x="{x + 18:.1f}" y="{y:.1f}" font-size="13" font-weight="700">{esc(title)}</text>')
    lx = ax
    for name, colour, _, _ in series:
        add(f'<line x1="{lx:.1f}" x2="{lx + 16:.1f}" y1="{y + 19:.1f}" y2="{y + 19:.1f}" stroke="{colour}" '
            f'stroke-width="2"/>')
        add(f'<circle cx="{lx + 8:.1f}" cy="{y + 19:.1f}" r="3" fill="{colour}"/>')
        add(f'<text x="{lx + 21:.1f}" y="{y + 23:.1f}" font-size="11">{esc(name)}</text>')
        lx += 21 + 6.2 * len(name) + 18
    ticks = _ticks(max([v for _, _, vals, _ in series for v in vals if v is not None] or [1]), percent=percent)
    peak = ticks[-1]
    # axes: left and bottom spines, ticks outward
    add(f'<path d="M{ax:.1f},{top:.1f} V{bot:.1f} H{x1:.1f}" fill="none" stroke="{INK}" stroke-width="1"/>')
    for t in ticks:
        ty = bot - (bot - top) * t / peak
        add(f'<line x1="{ax - 4:.1f}" x2="{ax:.1f}" y1="{ty:.1f}" y2="{ty:.1f}" stroke="{INK}"/>')
        add(f'<text x="{ax - 7:.1f}" y="{ty + 4:.1f}" font-size="11" text-anchor="end">{t:g}</text>')
    cy = (top + bot) / 2
    add(f'<text x="{x + 12:.1f}" y="{cy:.1f}" font-size="11" text-anchor="middle" '
        f'transform="rotate(-90 {x + 12:.1f} {cy:.1f})">{esc(ylabel)}</text>')
    n = len(weeks)
    every = n <= 6  # a date under every tick while they fit; past that, the first and the last

    def at(i: int, v: float) -> tuple[float, float]:
        px = (ax + 20 + x1) / 2 if n == 1 else ax + 20 + (x1 - ax - 30) * i / (n - 1)
        return px, bot - (bot - top) * v / peak

    for i, wk in enumerate(weeks):
        px = at(i, 0)[0]
        add(f'<line x1="{px:.1f}" x2="{px:.1f}" y1="{bot:.1f}" y2="{bot + 4:.1f}" stroke="{INK}"/>')
        if every or i in (0, n - 1):
            add(f'<text x="{px:.1f}" y="{bot + 17:.1f}" font-size="11" text-anchor="middle">'
                f'{esc(_fmt_day(wk["week"]))}</text>')
    if xlabel:
        add(f'<text x="{(ax + x1) / 2:.1f}" y="{bot + 35:.1f}" font-size="11" text-anchor="middle">Week starting</text>')
    for _, colour, vals, hollow in series:
        for i in range(n - 1):
            if vals[i] is None or vals[i + 1] is None:
                continue
            (xa, ya), (xb, yb) = at(i, vals[i]), at(i + 1, vals[i + 1])
            dash = ' stroke-dasharray="4 3"' if hollow[i] or hollow[i + 1] else ""
            add(f'<line x1="{xa:.1f}" y1="{ya:.1f}" x2="{xb:.1f}" y2="{yb:.1f}" stroke="{colour}" '
                f'stroke-width="1.75"{dash}/>')
        for i, v in enumerate(vals):
            if v is None:
                continue
            px, py = at(i, v)
            fill = 'fill="none" stroke-width="1.5"' if hollow[i] else f'fill="{colour}" stroke-width="0"'
            add(f'<circle cx="{px:.1f}" cy="{py:.1f}" r="3.5" stroke="{colour}" {fill}/>')


def render_svg(data: dict, *, palette: dict[str, str] | None = None) -> str:
    """The chart. Same `data`, same bytes: no clock, no random ids, fixed number formatting."""
    pal = palette or load_hex()
    W, H = 960, 560
    left = 20
    weeks = data["weeks"][-MAX_WEEKS:]
    out: list[str] = []
    add = out.append
    title = "Agent pre-commit reviews, per week"
    add(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" role="img" '
        f'aria-labelledby="t d" font-family="Helvetica, Arial, sans-serif" fill="{INK}">')
    add(f'<title id="t">{esc(title)}</title>')
    add(f'<desc id="d">{esc(describe(data))}</desc>')
    add(f'<text x="{left}" y="30" font-size="17" font-weight="700">{esc(title)}</text>')
    since = (f"Since {_fmt_day(data['first_day'])}, {data['first_day'][:4]} · the reviewer prompts still change most weeks"
             if data["first_day"] else "Nothing logged yet")
    add(f'<text x="{left}" y="50" font-size="12">{esc(since)}</text>')

    if not weeks:
        add(f'<text x="{W // 2}" y="{H // 2}" font-size="15" text-anchor="middle">No reviews logged yet.</text>')
        add(_footer(data, left, H))
        add("</svg>")
        return "\n".join(out) + "\n"

    gap = 50
    pw = (W - 2 * left - gap) / 2
    col = (left, left + pw + gap)
    row = (88, 318)
    hollow = [thin(w) for w in weeks]

    def by_reviewer(value) -> list:
        return [(name, pal[swatch], [value(w, r) for w in weeks], hollow) for r, name, swatch in REVIEWERS]

    _panel(add, x=col[0], y=row[0], w=pw, letter="a", title="Changes reviewed and PRs merged",
           ylabel="Count per week", weeks=weeks,
           series=[("changes reviewed", INK, [w["changes"] for w in weeks], hollow),
                   ("PRs merged", pal[PR_SWATCH], [w["prs"] for w in weeks], hollow)])
    _panel(add, x=col[1], y=row[0], w=pw, letter="b", title="Finding rate", ylabel="Findings per 100 changes",
           weeks=weeks, series=by_reviewer(_per_100))
    _panel(add, x=col[0], y=row[1], w=pw, letter="c", title="Findings fixed before commit",
           ylabel="Fixed before commit (%)", weeks=weeks, series=by_reviewer(_fixed), percent=True, xlabel=True)
    j = data.get("judge")
    if j:
        judged = j.get("by_week", {})
        # a week the judge's pass fell inside is only partly judged
        late = [h or _ends(w) >= j["date"] for h, w in zip(hollow, weeks)]
        _panel(add, x=col[1], y=row[1], w=pw, letter="d",
               title=f"Judge's verdict on fan-out notes nobody fixed ({_fmt_day(j['date'])})",
               ylabel="Notes (n)", weeks=weeks, xlabel=True,
               series=[(name, pal[swatch],
                        # no point for a week the judge has nothing from: not swept, or not yet
                        [sum(judged[w["week"]][s] for s in group) if w["week"] in judged else None
                         for w in weeks], late)
                       for name, group, swatch in JUDGE_LINES])
    else:
        add(f'<text x="{col[1]:.1f}" y="{row[1]}" font-size="13" font-weight="700">Weekly judge: no pass yet.</text>')
    add(_footer(data, left, H))
    add("</svg>")
    return "\n".join(out) + "\n"


def _footer(data: dict, x: int, H: int) -> str:
    through = (f"Data through {_fmt_day(data['last_day'])} · {data['sessions']} sessions"
               if data["last_day"] else "No data yet")
    return (f'<text x="{x}" y="{H - 12}" font-size="11">{esc(through)} · hollow = partial week or under '
            f'{LOW_SESSIONS} sessions · how it is counted: docs/ai-assist/AGENT_RECORD.md</text>')


# ── 3. publish ─────────────────────────────────────────────────────────────────────────────────

class GitRefused(RuntimeError):
    """A `git` call outside the allowlist: the pass runs on the owner's credentials."""


class GitError(RuntimeError):
    pass


_SHA = re.compile(r"^[0-9a-f]{40}$")
_COMMIT_MSG = re.compile(r"^chart: (data through \d{4}-\d{2}-\d{2}|no data yet)$")


def allowed(args: _t.Sequence[str]) -> bool:
    """The plumbing `publish` needs and nothing else: write a blob, a tree and a parentless commit,
    read the branch, and force-push a commit to `judge-assets` — never another ref."""
    args = list(args)
    if args == ["hash-object", "-w", "--stdin"] or args == ["mktree"]:
        return True
    if args == ["ls-remote", "origin", REF]:
        return True
    if len(args) == 4 and args[0] == "commit-tree" and _SHA.match(args[1]) and args[2] == "-m" \
            and _COMMIT_MSG.match(args[3]):
        return True
    if len(args) == 4 and args[:3] == ["push", "--force", "origin"]:
        src, _, dst = args[3].partition(":")
        return bool(_SHA.match(src)) and dst == REF
    return False


class Git:
    """Every `git` call `publish` makes. `run(args, input)` does it (a fake in tests); `dry` runs the
    read (`ls-remote`) and the local object writes, and collects the push as `planned`."""

    def __init__(self, repo: pathlib.Path = _REPO,
                 run: _t.Callable[[list[str], str | None], str] | None = None, *, dry: bool = False):
        self.repo, self.dry = repo, dry
        self._run = run or self._default_run
        self.planned: list[list[str]] = []

    def _default_run(self, args: list[str], input: str | None) -> str:
        git = shutil.which("git") or "git"
        try:
            proc = subprocess.run([git, *args], cwd=str(self.repo), capture_output=True, text=True,
                                  encoding="utf-8", input=input, timeout=120, check=False)
        except (OSError, subprocess.TimeoutExpired) as e:
            raise GitError(f"git {args[0]} failed: {e}") from e
        if proc.returncode != 0:
            raise GitError(f"git {args[0]} exited {proc.returncode}: {(proc.stderr or proc.stdout).strip()[-400:]}")
        return proc.stdout

    def __call__(self, *args: str, input: str | None = None) -> str:
        args = list(args)
        if not allowed(args):
            raise GitRefused(f"git {' '.join(args[:3])} is not on the chart's allowlist")
        if self.dry and args[0] == "push":
            self.planned.append(args)
            return ""
        return self._run(args, input)


BRANCH_README = """# judge-assets

Generated by the weekly judge (`scripts/judge/chart.py`). One commit, force-pushed each week:
history is not kept here. Do not edit; do not merge into `main`.

- `effectiveness.svg` — the chart the main README embeds.
- `effectiveness.json` — the numbers behind it (aggregates only).

Method and limits: `docs/ai-assist/AGENT_RECORD.md` on `main`.
"""


def chart_dir() -> pathlib.Path:
    return state_dir() / "chart"


def publish(svg: str, data: dict, *, git: Git, state_path: pathlib.Path) -> dict:
    """Put `svg` and `data` on `judge-assets` as its only commit. Returns what happened:
    `{"pushed": sha}` or `{"skipped": why}`. Raises on a failed git call; `update` catches it."""
    digest = hashlib.sha256(svg.encode("utf-8")).hexdigest()
    try:
        last = json.loads(state_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        last = {}
    remote = (git("ls-remote", "origin", REF).split() or [None])[0]
    if last.get("svg_sha256") == digest and remote and remote == last.get("commit"):
        return {"skipped": "unchanged"}
    files = {"README.md": BRANCH_README, DATA_NAME: json.dumps(data, indent=2, sort_keys=True) + "\n",
             SVG_NAME: svg}
    entries = []
    for name in sorted(files):
        blob = git("hash-object", "-w", "--stdin", input=files[name]).strip()
        entries.append(f"100644 blob {blob}\t{name}\n")
    tree = git("mktree", input="".join(entries)).strip()
    message = f"chart: data through {data['last_day']}" if data["last_day"] else "chart: no data yet"
    commit = git("commit-tree", tree, "-m", message).strip()
    git("push", "--force", "origin", f"{commit}:{REF}")
    if git.dry:
        return {"planned": commit}
    write_json_atomic(state_path, {"svg_sha256": digest, "commit": commit})
    return {"pushed": commit}


def update(*, push: bool, repo: pathlib.Path = _REPO, git: Git | None = None, dry: bool = False,
           out_dir: pathlib.Path | None = None) -> dict:
    """Render from the local log and records, write the files beside the log, and push if asked.
    Never raises: what happened, or the error, comes back as a dict for the pass's record."""
    try:
        data = aggregate(read_events(), _record.pass_records(),
                         prompt_edits=doc_edits(PROMPT_DOCS, repo=repo), rule_edits=doc_edits(RULE_DOCS, repo=repo),
                         prs=pr_merges(repo=repo), as_of=_dt.date.today())
        svg = render_svg(data)
        out_dir = out_dir or chart_dir()
        out_dir.mkdir(parents=True, exist_ok=True)
        with write_atomic(out_dir / SVG_NAME) as f:
            f.write(svg)
        write_json_atomic(out_dir / DATA_NAME, data, indent=2, sort_keys=True)
        result: dict = {"path": str(out_dir / SVG_NAME), "weeks": len(data["weeks"])}
        if push or dry:
            g = git or Git(repo, dry=dry)
            result.update(publish(svg, data, git=g, state_path=out_dir / "pushed.json"))
            if dry:
                result["calls"] = [" ".join(c) for c in g.planned]
        return result
    except Exception as e:  # noqa: BLE001 — a chart failure must never fail the judge
        return {"error": f"{type(e).__name__}: {e}"}


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--push", action="store_true", help=f"publish to the `{BRANCH}` branch")
    ap.add_argument("--dry-run", action="store_true", help="render, and print the push without making it")
    ap.add_argument("--out", type=pathlib.Path, help=f"where to write the SVG + JSON (default {chart_dir()})")
    args = ap.parse_args(argv)
    result = update(push=args.push and not args.dry_run, dry=args.dry_run, out_dir=args.out)
    print(json.dumps(result, indent=2))
    return 1 if "error" in result else 0


if __name__ == "__main__":
    sys.exit(main())
