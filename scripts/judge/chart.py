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

BRANCH = "judge-assets"
REF = f"refs/heads/{BRANCH}"
SVG_NAME, DATA_NAME = "effectiveness.svg", "effectiveness.json"
#: Weeks shown, newest last. Older weeks stay in the JSON beside the SVG.
MAX_WEEKS = 12
#: A week with fewer sessions than this says so under its bars.
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
#: The bar's segments, bottom first: the log's display order, then `none`.
OUTCOMES = tuple((k, *_OUTCOME_LOOK[k]) for k in OUTCOME_DISPLAY_ORDER if k in _OUTCOME_LOOK) \
    + (("none", "no outcome logged", "grey"),)
_OUTCOME_KEYS = set(_OUTCOME_LOOK)
#: Ink for every label: mid grey, legible on GitHub's light and dark README backgrounds alike. The
#: background stays transparent, so one file serves both themes.
INK = "#7F7F7F"
REVIEW_SWATCH = "blue"


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


def aggregate(events: _t.Iterable[dict], records: _t.Sequence[dict], *,
              prompt_edits: _t.Sequence[_dt.date] = (), rule_edits: _t.Sequence[_dt.date] = ()) -> dict:
    """Per-week numbers for the chart; JSON-ready and free of any text from the log.

    A *change reviewed* is one branch + parent commit a recital reviewed (reruns on the same change
    count once; an errored reviewer counts none). A *finding* is one fanout or convention finding,
    once per slug, in the week it was first raised, with its latest logged outcome."""
    events = list(events)
    changes: dict[_dt.date, set] = collections.defaultdict(set)
    sessions: dict[_dt.date, set] = collections.defaultdict(set)
    first: dict[str, dict] = {}
    slugless: list[dict] = []
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
        elif name in _FINDING_EVENTS:
            slug = payload.get("slug")
            if slug in _REDTEAM_SLUGS:
                continue
            if not slug:
                slugless.append(e)
            elif slug not in first or e.get("ts", "") < first[slug].get("ts", ""):
                first[slug] = e
    resolved = _latest_resolutions_by_slug(events, _RESOLVED_EVENTS)
    found: dict[_dt.date, collections.Counter] = collections.defaultdict(collections.Counter)
    for slug, e in first.items():
        outcome = ((resolved.get(slug) or {}).get("payload") or {}).get("outcome")
        found[week_of(_day(e["ts"]))][outcome if outcome in _OUTCOME_KEYS else "none"] += 1
    for e in slugless:
        outcome = (e.get("payload") or {}).get("outcome")
        found[week_of(_day(e["ts"]))][outcome if outcome in _OUTCOME_KEYS else "none"] += 1

    if not days:
        return {"weeks": [], "first_day": None, "last_day": None, "sessions": 0, "judge": _judge_summary(records)}
    first_day, last_day = min(days), max(days)
    weeks = []
    monday = week_of(first_day)
    while monday <= last_day:
        sunday = monday + _dt.timedelta(days=6)
        weeks.append({
            "week": monday.isoformat(),
            # the newest week is still filling up: nothing after it says it's over
            "partial": monday < first_day or sunday >= last_day,
            "changes": len(changes[monday]),
            "sessions": len(sessions[monday] - {None}),
            "findings": {k: found[monday][k] for k, _, _ in OUTCOMES},
            "prompt_edits": sum(max(monday, first_day) <= d <= sunday for d in prompt_edits),
            "rule_edits": sum(max(monday, first_day) <= d <= sunday for d in rule_edits),
        })
        monday += _dt.timedelta(days=7)
    all_sessions = set().union(*sessions.values()) - {None} if sessions else set()
    return {"weeks": weeks, "first_day": first_day.isoformat(), "last_day": last_day.isoformat(),
            "sessions": len(all_sessions), "judge": _judge_summary(records)}


#: The bug statuses the chart's judge line names: the ones with a verdict. `unjudged` / `unmerged`
#: are still waiting for one, and `wont_fix` is the owner's answer, not the judge's.
JUDGE_STATUSES = ("open", "parked", "gone", "dismissed")
assert set(JUDGE_STATUSES) <= set(_record.BUG_STATUSES)


def _judge_summary(records: _t.Sequence[dict]) -> dict | None:
    """The newest pass record's bug counts by status: what the judge found when it checked the
    findings that shipped against `main`."""
    passes = [r for r in records if r.get("kind") == "pass"]
    if not passes:
        return None
    last = max(passes, key=lambda r: r.get("date", ""))
    counts = collections.Counter(b.get("status") for b in last.get("bugs") or [])
    return {"date": last.get("date"), "passes": len(passes),
            **{s: counts.get(s, 0) for s in JUDGE_STATUSES}}


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
    """The chart in words: the SVG's `<desc>`, for a screen reader or anyone who can't see the bars."""
    if not data["weeks"]:
        return "No reviews logged yet."
    parts = []
    for w in data["weeks"]:
        f = w["findings"]
        total = sum(f.values())
        parts.append(f"Week of {_week_label(w['week'])}{' (partial)' if w['partial'] else ''}: "
                     f"{w['changes']} changes reviewed across {w['sessions']} sessions; {total} findings, "
                     + ", ".join(f"{f[k]} {label}" for k, label, _ in OUTCOMES) + ".")
    j = data.get("judge")
    if j:
        parts.append(f"Weekly judge, pass of {j['date']}: {j['open']} open bugs, {j['parked']} parked, "
                     f"{j['gone']} gone, {j['dismissed']} dismissed.")
    return " ".join(parts)


def render_svg(data: dict, *, palette: dict[str, str] | None = None) -> str:
    """The chart. Same `data`, same bytes: no clock, no random ids, fixed number formatting."""
    pal = palette or load_hex()
    W, H = 960, 540
    x0, x1 = 70, W - 30
    weeks = data["weeks"][-MAX_WEEKS:]
    out: list[str] = []
    add = out.append
    title = "How the agents' pre-commit reviews went, week by week"
    add(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" role="img" '
        f'aria-labelledby="t d" font-family="-apple-system, \'Segoe UI\', Helvetica, Arial, sans-serif" '
        f'fill="{INK}">')
    add(f'<title id="t">{esc(title)}</title>')
    add(f'<desc id="d">{esc(describe(data))}</desc>')
    add(f'<text x="{x0 - 50}" y="30" font-size="18" font-weight="700">{esc(title)}</text>')
    since = f"logging began {_fmt_day(data['first_day'])}, {data['first_day'][:4]}" if data["first_day"] else "nothing logged yet"
    add(f'<text x="{x0 - 50}" y="52" font-size="13">Early days: {esc(since)}. The reviewer prompts still '
        f'change most weeks, so weeks are not strictly comparable. No trend is drawn.</text>')

    if not weeks:
        add(f'<text x="{W // 2}" y="{H // 2}" font-size="15" text-anchor="middle">No reviews logged yet.</text>')
        add(_footer(data, x0 - 50, H))
        add("</svg>")
        return "\n".join(out) + "\n"

    slot = min(150.0, (x1 - x0) / len(weeks))
    bar = min(56.0, slot * 0.6)

    def cx(i: int) -> float:
        return x0 + slot * (i + 0.5)

    # panel A: changes reviewed
    a_top, a_bot = 100, 170
    add(f'<text x="{x0 - 50}" y="{a_top - 18}" font-size="13" font-weight="700">Changes reviewed before commit</text>')
    add(f'<line x1="{x0}" x2="{x1}" y1="{a_bot}" y2="{a_bot}" stroke="{INK}" stroke-opacity="0.4"/>')
    top = max(w["changes"] for w in weeks) or 1
    for i, w in enumerate(weeks):
        h = (a_bot - a_top) * w["changes"] / top
        op = ' fill-opacity="0.5"' if w["partial"] else ""
        if h:
            add(f'<rect x="{cx(i) - bar / 2:.1f}" y="{a_bot - h:.1f}" width="{bar:.1f}" height="{h:.1f}" '
                f'rx="2" fill="{pal[REVIEW_SWATCH]}"{op}/>')
        add(f'<text x="{cx(i):.1f}" y="{a_bot - h - 5:.1f}" font-size="12" text-anchor="middle">{w["changes"]}</text>')

    # panel B: what happened to the findings, as a share of the week's findings
    b_top, b_bot = 255, 400
    add(f'<text x="{x0 - 50}" y="{b_top - 53}" font-size="13" font-weight="700">What happened to the reviewers\' '
        f'findings (share of the week\'s findings)</text>')
    lx = x0 - 50
    for key, label, swatch in OUTCOMES:
        add(f'<rect x="{lx}" y="{b_top - 45}" width="10" height="10" rx="2" fill="{pal[swatch]}"/>')
        add(f'<text x="{lx + 14}" y="{b_top - 36}" font-size="12">{esc(label)}</text>')
        lx += 14 + 6 * len(label) + 22
    for frac, lab in ((0.0, "0%"), (0.5, "50%"), (1.0, "100%")):
        y = b_bot - (b_bot - b_top) * frac
        add(f'<line x1="{x0}" x2="{x1}" y1="{y:.1f}" y2="{y:.1f}" stroke="{INK}" stroke-opacity="0.2"/>')
        add(f'<text x="{x0 - 8}" y="{y + 4:.1f}" font-size="11" text-anchor="end">{lab}</text>')
    for i, w in enumerate(weeks):
        total = sum(w["findings"].values())
        op = ' fill-opacity="0.5"' if w["partial"] else ""
        y = float(b_bot)
        for key, _, swatch in OUTCOMES:
            n = w["findings"][key]
            if not n:
                continue
            h = (b_bot - b_top) * n / total
            add(f'<rect x="{cx(i) - bar / 2:.1f}" y="{y - h + 1:.1f}" width="{bar:.1f}" height="{max(h - 2, 1):.1f}" '
                f'fill="{pal[swatch]}"{op}/>')
            if h >= 15:
                add(f'<text x="{cx(i) + bar / 2 + 4:.1f}" y="{y - h / 2 + 4:.1f}" font-size="11">{n}</text>')
            y -= h
        if not total:
            add(f'<text x="{cx(i):.1f}" y="{(b_top + b_bot) / 2:.1f}" font-size="11" text-anchor="middle">no findings</text>')
            continue
        add(f'<text x="{cx(i):.1f}" y="{b_top - 18}" font-size="11" text-anchor="middle">{total} findings</text>')
        if w["changes"]:
            add(f'<text x="{cx(i):.1f}" y="{b_top - 6}" font-size="10" text-anchor="middle">'
                f'{100 * total / w["changes"]:.0f} per 100 changes</text>')

    # per-week notes: when, partial, few sessions, rubric/rule edits
    for i, w in enumerate(weeks):
        notes = [_week_label(w["week"]), f"{w['sessions']} sessions"]
        if w["partial"]:
            notes.append("partial week")
        elif w["sessions"] < LOW_SESSIONS:
            notes.append(f"few sessions (<{LOW_SESSIONS})")
        if w["prompt_edits"]:
            notes.append(f"reviewer prompts edited ×{w['prompt_edits']}" if slot >= 140 else f"prompts edited ×{w['prompt_edits']}")
        if w["rule_edits"]:
            notes.append(f"CLAUDE.md edited ×{w['rule_edits']}")
        for j, note in enumerate(notes):
            weight = ' font-weight="700"' if j == 0 else ""
            add(f'<text x="{cx(i):.1f}" y="{b_bot + 18 + 14 * j}" font-size="{12 if j == 0 else 10}" '
                f'text-anchor="middle"{weight}>{esc(note)}</text>')
    add(_footer(data, x0 - 50, H))
    add("</svg>")
    return "\n".join(out) + "\n"


def _footer(data: dict, x: int, H: int) -> str:
    j = data.get("judge")
    judge = (f"Weekly judge, pass of {_fmt_day(j['date'])}: of the unfixed findings it re-checked on main, "
             f"{j['open']} are open bugs, {j['parked']} parked (can't happen yet), {j['gone']} gone, "
             f"{j['dismissed']} dismissed."
             if j else "Weekly judge: no pass yet.")
    through = (f"Data through {_fmt_day(data['last_day'])}, {data['last_day'][:4]} · {data['sessions']} sessions"
               if data["last_day"] else "No data yet")
    return (f'<text x="{x}" y="{H - 28}" font-size="11">{esc(judge)}</text>\n'
            f'<text x="{x}" y="{H - 12}" font-size="11">{esc(through)} · method and limits: '
            f'docs/ai-assist/AGENT_RECORD.md</text>')


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
                         prompt_edits=doc_edits(PROMPT_DOCS, repo=repo), rule_edits=doc_edits(RULE_DOCS, repo=repo))
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
