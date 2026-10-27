"""Findings flow — the README's picture of where every reviewer finding went, start to finish.

Four columns, each a stacked bar to its own scale (the last is a few dozen against the first's
hundreds), joined by a band from the part that flows on:

1. **Recital** — findings since logging began: fixed in the commit, not fixed, plausible notes.
2. **Bug check** — the weekly judge's bugs, with the steps from column 1's unfixed notes printed below
   (notes on one function merge into one bug; one already gone when the judge read it isn't listed).
3. **Verify agent** — what the agents it sent bugs to said.
4. **Real bugs** — fixed, open with an issue, not filed yet, won't fix.

`chart.py` draws it beside the weekly chart and publishes both. Aggregates only, as there: no finding
text, path, branch or session id. Same counts, same bytes.
"""
from __future__ import annotations

import importlib.util as _importlib_util
import pathlib
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.log import FINDING_EVENTS, latest_resolutions_by_slug  # noqa: E402
from cecelia.effectiveness.palette import load_hex  # noqa: E402
from cecelia.effectiveness.rollup import _REDTEAM_SLUGS  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_bugs = _load_sibling("bugs")
_issues = _load_sibling("issues")
_record = _load_sibling("record")
_style = _load_sibling("figure_style")
_verify = _load_sibling("verify")

FLOW_NAME = "findings-flow.svg"
_PLAUSIBLE_EVENT = "fanout_audit_advisory"
_FIXED = "fixed_pre_commit"


#: Bug-check / verify column per verify verdict. A bug with one went to a verify agent.
#: `fixed` is a real bug already fixed: real, but not filed (its status is `gone`, so it counts as fixed).
_BY_VERDICT = {"fix": "real", "decide": "real", "fixed": "real", "guard": "parked", "dismiss": "not_a_bug"}
assert set(_BY_VERDICT) == set(_verify.VERDICTS), "a new verdict needs a column here"
assert {v for v, c in _BY_VERDICT.items() if c == "real"} - {"fixed"} == set(_issues.FILED_VERDICTS)
#: Bug-check column per status, for a bug no verify agent has seen. `open` / `parked` are waiting on one.
_BY_STATUS = {"unmerged": "waiting", "unjudged": "waiting", "gone": "settled", "dismissed": "settled",
              "wont_fix": "settled", "open": "verify", "parked": "verify"}
assert set(_BY_STATUS) == set(_record.BUG_STATUSES), "a new bug status needs a column here"
#: A real bug's segment per status; any other status is still open (issue filed, or not yet).
_REAL_BY_STATUS = {"gone": "fixed", "wont_fix": "wont_fix", "dismissed": "wont_fix"}


def _bug_class(b: dict) -> tuple[str, str]:
    """(bug-check segment, verify segment) for one bug at its newest record."""
    verdict = _issues._verdict(b)          # a stranded commit counts as `fix`, as when its issue is filed
    if verdict in _BY_VERDICT:
        return "verify", _BY_VERDICT[verdict]
    column = _BY_STATUS.get(b.get("status"), "waiting")
    return column, "unchecked" if column == "verify" else ""


def _real_class(b: dict) -> str:
    if _record.verified_fixed(b):   # `dismissed` when a repeated error, or an older record's dismiss
        return "fixed"
    return _REAL_BY_STATUS.get(b.get("status")) or ("open_issue" if b.get("issue") else "not_filed")


def aggregate(events: _t.Iterable[dict], records: _t.Sequence[dict]) -> dict:
    """The counts behind the picture. `records`: the judge's pass records, oldest first."""
    events = list(events)
    resolved = latest_resolutions_by_slug(events)
    outcome = lambda key: ((resolved.get(key) or {}).get("payload") or {}).get("outcome")  # noqa: E731
    required: dict[str, dict] = {}
    slugless: list[dict] = []      # rows from before slugs, outcome inline — counted like `chart.aggregate`
    plausible: dict[str, dict] = {}
    for e in events:
        p = e.get("payload") or {}
        if e.get("event") in FINDING_EVENTS and not p.get("slug"):
            slugless.append(e)
        elif e.get("event") in FINDING_EVENTS and p["slug"] not in _REDTEAM_SLUGS:
            required.setdefault(p["slug"], e)
        elif e.get("event") == _PLAUSIBLE_EVENT and p.get("marker") == "plausible":
            plausible.setdefault(_bugs._key(e), e)
    # the notes behind the Recital's not-fixed and untagged-plausible segments, by the sweep's key
    unfixed = {**{s: e for s, e in required.items() if outcome(s) != _FIXED},
               **{_bugs._key(e): e for e in slugless if (e.get("payload") or {}).get("outcome") != _FIXED},
               **{k: e for k, e in plausible.items() if outcome(k) != _FIXED}}
    days = sorted(e["ts"][:10] for e in (*required.values(), *slugless, *plausible.values()) if e.get("ts"))
    req_fixed = (sum(outcome(s) == _FIXED for s in required)
                 + sum((e.get("payload") or {}).get("outcome") == _FIXED for e in slugless))
    n_required = len(required) + len(slugless)
    pl_fixed = sum(outcome(k) == _FIXED for k in plausible)

    latest: dict[str, dict] = {}
    for record in records:
        for b in record.get("bugs") or []:
            if b.get("key"):
                latest[b["key"]] = b
    check = {"settled": 0, "waiting": 0, "verify": 0}
    verify = {"not_a_bug": 0, "parked": 0, "unchecked": 0, "real": 0}
    real = {"fixed": 0, "open_issue": 0, "not_filed": 0, "wont_fix": 0}
    into = _into_check(unfixed, latest.values(), max((r.get("run") or {}).get("ts") or "" for r in records)
                       if records else "")
    for b in latest.values():
        col, vcol = _bug_class(b)
        check[col] += 1
        if vcol:
            verify[vcol] += 1
        if vcol == "real":
            real[_real_class(b)] += 1
    return {
        "first_day": days[0] if days else None, "last_day": days[-1] if days else None,
        "recital": {"fixed": req_fixed + pl_fixed, "not_fixed": n_required - req_fixed,
                    "plausible": len(plausible) - pl_fixed},
        "required": n_required, "required_fixed": req_fixed,
        "plausible_total": len(plausible), "plausible_fixed": pl_fixed,
        "check": check, "verify": verify, "real": real, "into_check": into,
    }


def _into_check(unfixed: dict[str, dict], bugs: _t.Iterable[dict], last_pass: str) -> dict[str, int]:
    """How the Recital's unfixed notes become the bug check's bugs, line by line: the step a reader
    can't see from the two column totals. `unfixed`: those notes by the sweep's key; `bugs`: every
    bug at its newest record; `last_pass`: the newest pass's `run.ts`. The lines add up:
    `notes - rules_only - not_listed - since_pass - merged + tagged_later + agent_run + stranded`
    is the bug check's total."""
    bugs = list(bugs)
    eligible = {k for k, e in unfixed.items() if _bugs.swept(e)}
    reached: set[str] = set()
    from_notes = tagged_later = 0
    for b in bugs:
        # findings waiting on stranded commits reached that bug: merged into it, like one function's notes
        reached |= {w["key"] for w in b.get("waiting", [])} & eligible
        if b.get("kind") in ("agent_run", "stranded"):
            continue
        mine = set(b.get("sources") or [b["key"]]) & eligible
        reached |= mine
        from_notes += bool(mine)
        tagged_later += not mine       # its notes were tagged fixed after the judge listed it
    unseen = [unfixed[k] for k in eligible - reached]
    since = sum((e.get("ts") or "") >= last_pass for e in unseen)
    return {"notes": len(unfixed), "rules_only": len(unfixed) - len(eligible),
            "not_listed": len(unseen) - since, "since_pass": since, "merged": len(reached) - from_notes,
            "tagged_later": tagged_later, "last_pass": last_pass[:10] or None,
            "agent_run": sum(b.get("kind") == "agent_run" for b in bugs),
            "stranded": sum(b.get("kind") == "stranded" for b in bugs)}


def _fmt_day(day: str) -> str:
    y, m, d = day.split("-")
    months = "January February March April May June July August September October November December".split()
    return f"{int(d)} {months[int(m) - 1]} {y}"


def render_svg(data: dict, *, palette: dict[str, str] | None = None) -> str:
    """The picture. Every label is ours or a number."""
    pal = palette or load_hex()
    flow_on = pal["reddish_purple"]
    rec, chk, ver, rl = data["recital"], data["check"], data["verify"], data["real"]
    n_check, n_verify, n_real = sum(chk.values()), sum(ver.values()), sum(rl.values())
    cols = [
        ("Recital", f"{sum(rec.values())} findings", [
            (rec["fixed"], pal["sky_blue"], "fixed in the commit"),
            (rec["not_fixed"], pal["orange"], "not fixed"),
            (rec["plausible"], pal["blue"], "plausible notes, untagged")], (1, 2)),
        ("Bug check", f"{n_check} bugs", [
            (chk["settled"], _style.NEUTRAL, "already gone, or not a bug"),
            (chk["waiting"], _style.NEUTRAL_LIGHT, "waiting (unmerged, backlog)"),
            (chk["verify"], flow_on, "to a verify agent")], (2, 2)),
        ("Verify agent", f"{n_verify} bugs", [
            (ver["not_a_bug"], _style.NEUTRAL, "not a bug"),
            (ver["parked"], _style.NEUTRAL_LIGHT, "parked: not live yet"),
            (ver["unchecked"], _style.NEUTRAL_LIGHT, "not checked yet"),
            (ver["real"], flow_on, "real")], (3, 3)),
        ("Real bugs", f"{n_real} bugs", [
            (rl["fixed"], pal["sky_blue"], "fixed"),
            (rl["open_issue"], pal["orange"], "open, issue filed"),
            (rl["not_filed"], _style.NEUTRAL_LIGHT, "open, not filed yet"),
            (rl["wont_fix"], _style.NEUTRAL, "won't fix")], None),
    ]
    ledger = _ledger(data["into_check"], n_check)
    W, H, top, bh, bw = 1000, 436 + 31 + 17 * len(ledger), 114, 250, 22
    xs = (24, 290, 540, 790)
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" role="img" '
           f'font-family="{_style.FONT}" fill="{_style.INK}">',
           "<title>Where the reviewers' findings went</title>",
           _style.surface(W, H)]
    geo = []
    for _, _, segs, _ in cols:
        total = sum(n for n, _, _ in segs) or 1
        y, g = top, []
        for n, _, _ in segs:
            h = bh * n / total
            g.append((y, h))
            y += h
        geo.append(g)
    for i, (*_, nxt) in enumerate(cols):
        if not nxt:
            continue
        a, b = nxt
        y0, y1 = geo[i][a][0], geo[i][b][0] + geo[i][b][1]
        x0, x1 = xs[i] + bw, xs[i + 1]
        m = (x0 + x1) / 2
        out.append(f'<path d="M{x0},{y0:.1f} C{m},{y0:.1f} {m},{top} {x1},{top} L{x1},{top + bh} '
                   f'C{m},{top + bh} {m},{y1:.1f} {x0},{y1:.1f} Z" fill="{flow_on}" fill-opacity="0.13"/>')
    for (title, total_line, segs, _), x, g in zip(cols, xs, geo):
        total = sum(n for n, _, _ in segs)
        out.append(f'<text x="{x}" y="70" font-size="16" font-weight="700">{title}</text>')
        out.append(f'<text x="{x}" y="90" font-size="13" fill="{_style.DIM}">{total_line}</text>')
        last = -99.0
        for (n, colour, label), (y, h) in zip(segs, g):
            if n:
                out.append(f'<rect x="{x}" y="{y + 1:.1f}" width="{bw}" height="{max(h - 2, 1.5):.1f}" '
                           f'rx="3" fill="{colour}"/>')
            ly = max(y + h / 2 + 5, last + 17)
            last = ly
            pct = round(100 * n / total) if total else 0
            out.append(f'<text x="{x + bw + 8}" y="{ly:.1f}" font-size="13"><tspan font-weight="700">{n}</tspan> '
                       f'{label} <tspan fill="{_style.DIM}">{pct}%</tspan></text>')
            if label == "fixed in the commit" and data["required"]:
                share = round(100 * data["required_fixed"] / data["required"])
                last = ly + 17
                out.append(f'<text x="{x + bw + 8}" y="{last:.1f}" font-size="12" fill="{_style.DIM}">'
                           f'{share}% of the {data["required"]} that must be resolved</text>')
    out += [f'<text x="24" y="22" font-size="13" font-weight="600">Before each commit</text>',
            f'<text x="24" y="38" font-size="12" fill="{_style.DIM}">in the session that commits</text>',
            f'<path d="M24,46 L240,46" stroke="{_style.DIM}" stroke-width="1.5"/>',
            f'<text x="290" y="22" font-size="13" font-weight="600">Weekly judge</text>',
            f'<text x="290" y="38" font-size="12" fill="{_style.DIM}">unattended: checks, verifies, files the issues</text>',
            f'<path d="M290,46 L{W - 24},46" stroke="{_style.DIM}" stroke-width="1.5"/>']
    # the step the two totals can't explain: how the unfixed notes became the bug check's bugs
    y = 404
    out.append(f'<text x="24" y="{y}" font-size="13" font-weight="700">From {ledger[0][1]} notes to {n_check} '
               'bugs</text>')
    for sign, n, label in ledger:
        y += 17
        if sign == "=":
            y += 5
            out.append(f'<path d="M30,{y - 15} L300,{y - 15}" stroke="{_style.DIM}" stroke-width="1"/>')
        bold = ' font-weight="700"' if sign in ("", "=") else ""
        out.append(f'<text x="70" y="{y}" font-size="13" text-anchor="end"{bold}>{sign} {n}</text>'
                   f'<text x="80" y="{y}" font-size="13">{label}</text>')
    span = (f"{_fmt_day(data['first_day'])} to {_fmt_day(data['last_day'])}" if data["first_day"]
            else "Nothing logged yet")
    out += [f'<text x="24" y="{H - 16}" font-size="12" fill="{_style.DIM}">{span}. Each column is drawn to its own '
            'scale; the shaded band is what flows on to the next.</text>',
            "</svg>"]
    return "\n".join(out) + "\n"


def _ledger(into: dict, bugs: int) -> list[tuple[str, int, str]]:
    """`_into_check` as lines to print, down to `bugs` (the bug check's total): (sign, count, label),
    a line with nothing on it left out."""
    since = (f"logged since the last pass ({_fmt_day(into['last_pass'])}): judged at the next"
             if into["last_pass"] else "logged before the first pass: judged at it")
    lines = [("", into["notes"], "not fixed, or plausible and untagged"),
             ("−", into["rules_only"], "wrong home: these feed only the rule count"),
             ("−", into["not_listed"], "already gone when the judge first read them: not listed"),
             ("−", into["since_pass"], since),
             ("−", into["merged"], "on a function another note names, or waiting on stranded commits: one bug"),
             ("+", into["tagged_later"], "tagged fixed only after the judge had listed them"),
             ("+", into["agent_run"], "errors hit by unattended agent runs"),
             ("+", into["stranded"], "commits made on a PR's branch after it merged"),
             ("=", bugs, "bugs in the bug check")]
    return [ln for ln in lines if ln[1] or ln[0] in ("", "=")]
