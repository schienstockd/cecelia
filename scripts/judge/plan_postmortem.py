#!/usr/bin/env python3
"""Plan post-mortem: what later edits to parked plans say about the plan review.

A plan review (`docs/ai-assist/PLAN_REVIEW.md`) runs before a plan is presented and at its commit.
Whether it was right shows only later, when the plan is edited on main: a phase gets built, a
decision is revised, another session corrects a claim. This step reads those edits and sorts
what they show into cases:

- `vindicated`: a finding tagged through (`false_positive`, `shipped_with_finding`,
  `dropped_no_action`, or no tag) that a later edit fixed after all. The author was wrong to dismiss it.
- `miss`: a later edit corrects an error (a claim, an overlap, a broken decision) that was already
  in the plan when it was reviewed, and that no finding flagged. The reviewer missed it.
- `noise`: a `false_positive` finding no later edit has vindicated (mechanical, no judge call).

One tool-less judge call per merged change that edited an existing plan; a change is judged once
(the records keep the SHAs). Cases accumulate across passes. When one kind of case on one marker
spans `MIN_PLANS` plans, the step proposes an edit, which `judge-review` shows beside the rule
proposals (`[e]` starts a session briefed on the cases):

- `plan_miss` / `plan_noise` → the reviewer prompt in `PLAN_REVIEW.md`, per marker;
- `plan_dismissed` → the authors' conventions in `docs/todo/README.md`.

As with the rules step, a case counts only after the target doc's last edit and the owner's last
answer to a proposal for it. Proposals only: nothing here edits a doc.
"""
from __future__ import annotations

import argparse
import datetime as _dt
import fnmatch
import importlib.util as _importlib_util
import json
import pathlib
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.log import OUTCOME_VOCABULARY, latest_resolutions_by_slug  # noqa: E402
from cecelia.effectiveness.recital import MARKERS  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_judge = _load_sibling("judge")
_rules = _load_sibling("rules")

PLAN_GLOB = "docs/todo/*_PLAN.md"
#: Plans change slowly: a wide window, and two plans (not the rules step's three sessions) for a pattern.
WINDOW_DAYS = 90
MIN_PLANS = 2
#: A marker's `false_positive` tags across this many findings (and `MIN_PLANS` plans) read as noise.
MIN_NOISE = 3
#: Outcomes that leave a finding unfixed at its commit: every outcome but a fix, and no outcome at all.
TAGGED_THROUGH = frozenset(OUTCOME_VOCABULARY - {"fixed_pre_commit"}) | {""}
KINDS = ("plan_miss", "plan_noise", "plan_dismissed")
_REVIEWER_DOC = "docs/ai-assist/PLAN_REVIEW.md"
_AUTHOR_DOC = "docs/todo/README.md"
#: Diff text per judge call; a longer plan rewrite is cut, and the brief says so.
MAX_DIFF_CHARS = 40_000

GitRun = _rules.GitRun

SCHEMA = {
    "type": "object",
    "properties": {
        "vindicated": {"type": "array", "items": {
            "type": "object", "properties": {"slug": {"type": "string"}, "why": {"type": "string"}},
            "required": ["slug", "why"]}},
        "misses": {"type": "array", "items": {
            "type": "object",
            "properties": {"marker": {"type": "string", "enum": list(MARKERS["plan"])},
                           "what": {"type": "string"}, "lesson": {"type": "string"}},
            "required": ["marker", "what", "lesson"]}},
    },
    "required": ["vindicated", "misses"],
}

_BRIEF = """You judge one merged change to a parked design plan, after the fact. The plan was reviewed \
earlier by an automated plan reviewer; its findings and what the author tagged each one are listed below, \
then the diff the change made to the plan.

Answer two questions, strictly from the diff:

1. vindicated: which listed findings did this diff fix after all, although the author had tagged them \
false_positive, shipped_with_finding, dropped_no_action or left them untagged? A finding is fixed when the \
diff changes the plan at that point in the way the finding asked, or removes what it objected to. Give the \
slug and one line on what changed. A finding tagged fixed_pre_commit never belongs here.

2. misses: which edits in the diff CORRECT AN ERROR that was already in the plan when it was reviewed, \
and that no listed finding flagged? An error is: a claim about code, data or another plan that was false \
("wrong claim"); a piece proposed that already existed or another plan designed ("already exists"); a \
decision, null, control or kill criterion that failed at its own goal ("design flaw"). NOT a miss: a design \
revised by choice, a phase marked built, new scope, rewording, a status line, anything added that was not \
there when reviewed, or anything a listed finding already covered. When unsure, leave it out. For each \
miss: the marker, what the error was (one line, cite the diff), and the lesson for the reviewer (one line: \
what check would have caught it).

Most diffs have no vindicated findings and no misses. Empty lists are the expected answer.

"""


def _git_in(repo: pathlib.Path) -> GitRun:
    # `replace`: a plan diff with a stray non-UTF-8 byte is still read, as recital reads the staged diff
    return lambda *args: git_output(*args, cwd=str(repo), errors="replace")


def _utc(ts: str) -> _dt.datetime:
    return _dt.datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone(_dt.timezone.utc)


def plan_edits(git: GitRun, sha: str, *, since: str, judged: _t.Collection[str] = ()) -> list[dict]:
    """Merged changes on `sha`'s first-parent line since `since` that MODIFIED an existing plan, one per
    (change, plan), oldest first: `{"sha", "ts", "plan"}`. A merge diffs against its first parent, so a PR
    is one change; a plan the change creates is left out (nothing earlier to have reviewed)."""
    out = git("log", "--first-parent", "-m", "--name-status", "--format=@%H\t%cI", f"--since={since}", sha,
              "--", PLAN_GLOB) or ""
    edits, cur = [], None
    for ln in out.splitlines():
        if ln.startswith("@"):
            h, _, ts = ln[1:].partition("\t")
            cur = (h, ts)
        elif cur and ln[:1] == "M":
            path = ln.split("\t")[-1]
            if fnmatch.fnmatch(path, PLAN_GLOB) and cur[0] not in judged:
                edits.append({"sha": cur[0], "ts": cur[1], "plan": path})
    return list(reversed(edits))


def reviewed_edits(edits: _t.Sequence[dict], events: _t.Iterable[dict]) -> list[dict]:
    """The edits a plan review ran before, on the same plan. The rest are left out, not marked judged:
    they can never become judgeable (a review can't run before an edit retroactively), so skipping them
    each pass costs nothing and keeps hundreds of SHAs off the published record."""
    _, reviewed = plan_findings(events)
    return [e for e in edits if any(t and _utc(t) < _utc(e["ts"]) for t in reviewed.get(e["plan"], []))]


def plan_findings(events: _t.Iterable[dict]) -> tuple[list[dict], dict[str, list[str]]]:
    """(every `plan_review_finding` row, plan → timestamps of the plan reviews that ran on it)."""
    findings, reviewed = [], {}
    for e in events:
        if e.get("event") == "plan_review_finding":
            findings.append(e)
        elif e.get("event") == "plan_review_run" and not (e.get("payload") or {}).get("escape_valve"):
            for p in (e.get("payload") or {}).get("plans") or []:
                reviewed.setdefault(p, []).append(e.get("ts") or "")
    return findings, reviewed


def outcome_of(slug: str, resolved: _t.Mapping[str, dict]) -> str:
    return ((resolved.get(slug) or {}).get("payload") or {}).get("outcome") or ""


def _brief(edit: dict, before: _t.Sequence[dict], resolved: _t.Mapping[str, dict], diff: str) -> str:
    cut = len(diff) > MAX_DIFF_CHARS
    lines = [f"PLAN: {edit['plan']}  (change {edit['sha'][:12]}, merged {edit['ts'][:10]})", "", "FINDINGS:"]
    for e in before:
        p = e.get("payload") or {}
        lines.append(f"- {p.get('slug')} line {p.get('line')} [{p.get('marker')}] "
                     f"tagged {outcome_of(p.get('slug', ''), resolved) or 'untagged'}: {str(p.get('desc', ''))[:500]}")
    if not before:
        lines.append("(none: the reviews that ran on this plan raised nothing)")
    lines += ["", "DIFF" + (" (cut: the change rewrote more of the plan than fits)" if cut else "") + ":", "",
              diff[:MAX_DIFF_CHARS]]
    return _BRIEF + "\n".join(lines)


def default_judge(prompt: str) -> tuple[dict, float, dict]:
    return _judge.call_judge(prompt, SCHEMA, budget_usd=1.0)


def judge_edits(edits: _t.Sequence[dict], events: _t.Sequence[dict], *, git: GitRun,
                judge: _t.Callable | None = None, meter: dict | None = None,
                failures: dict | None = None) -> tuple[list[dict], list[str], float]:
    """(cases, judged SHAs, cost). Pass only `reviewed_edits`: an edit with no plan review before it has
    nothing to hold to account. A failed call stops the step (`failures["plan_postmortem"]`); the edits
    not judged wait for the next pass."""
    findings, _ = plan_findings(events)
    resolved = latest_resolutions_by_slug(events, ("plan_review_finding_resolved",))
    cases, judged, cost = [], [], 0.0
    for edit in edits:
        before = [e for e in findings if (e.get("payload") or {}).get("file") == edit["plan"]
                  and _utc(e.get("ts") or edit["ts"]) < _utc(edit["ts"])]
        diff = git("diff", f"{edit['sha']}^1", edit["sha"], "--", edit["plan"]) or ""
        if not diff.strip():
            judged.append(edit["sha"])
            continue
        try:
            answer, spent, used = _judge.unpack((judge or default_judge)(_brief(edit, before, resolved, diff)))
        except _judge.JudgeError as e:
            print(f"plan post-mortem: judge failed ({e})", file=sys.stderr)
            if failures is not None:
                failures["plan_postmortem"] = str(e)
            break
        _judge.add_tokens(meter, used)
        cost += spent
        judged.append(edit["sha"])
        by_slug = {(e.get("payload") or {}).get("slug"): e.get("payload") or {} for e in before}
        for v in answer.get("vindicated") or []:
            p = by_slug.get(v.get("slug"))
            if p and outcome_of(v["slug"], resolved) in TAGGED_THROUGH:   # never trust a slug it made up
                cases.append({"kind": "vindicated", "plan": edit["plan"], "marker": p.get("marker"),
                              "sha": edit["sha"], "ts": edit["ts"], "slug": v["slug"],
                              "what": f"{outcome_of(v['slug'], resolved) or 'untagged'}, fixed later: {v.get('why', '')}"})
        for m in answer.get("misses") or []:
            if m.get("marker") in MARKERS["plan"]:
                cases.append({"kind": "miss", "plan": edit["plan"], "marker": m["marker"], "sha": edit["sha"],
                              "ts": edit["ts"], "what": m.get("what", ""), "lesson": m.get("lesson", "")})
    return cases, judged, cost


def noise_cases(events: _t.Sequence[dict], vindicated: _t.Collection[str]) -> list[dict]:
    """A `false_positive` plan finding no later edit has vindicated: one case each, dated by its tag."""
    findings, _ = plan_findings(events)
    resolved = latest_resolutions_by_slug(events, ("plan_review_finding_resolved",))
    out = []
    for e in findings:
        p = e.get("payload") or {}
        r = resolved.get(p.get("slug"))
        if r and outcome_of(p.get("slug", ""), resolved) == "false_positive" and p.get("slug") not in vindicated:
            out.append({"kind": "noise", "plan": p.get("file"), "marker": p.get("marker"), "slug": p.get("slug"),
                        "ts": r.get("ts") or e.get("ts") or "",
                        "what": f"tagged false_positive: {(r.get('payload') or {}).get('reason', '')}"})
    return out


def target(kind: str, marker: str | None) -> str:
    """The doc and section a proposal edits, as the rule screen names it (`file → *section*`)."""
    if kind == "plan_dismissed":
        return f"{_AUTHOR_DOC} → *Conventions*"
    return f"{_REVIEWER_DOC} → *Reviewer prompt* ({marker})"


_KIND_OF = {"miss": "plan_miss", "noise": "plan_noise", "vindicated": "plan_dismissed"}


def proposals_from(cases: _t.Sequence[dict], *, cut: _t.Mapping[str, str]) -> list[dict]:
    """One proposal per (kind, marker) whose cases after `cut[target]` span `MIN_PLANS` plans (and, for
    noise, `MIN_NOISE` findings). `cut`: target → the later of its doc's last edit and the owner's last
    answer to a proposal for it. Ids are `M1`, `M2`… (the rules step's are `P…`)."""
    groups: dict[tuple[str, str | None], list[dict]] = {}
    for c in cases:
        kind = _KIND_OF[c["kind"]]
        key = (kind, None if kind == "plan_dismissed" else c.get("marker"))
        since = cut.get(target(*key))
        if since and c.get("ts") and _utc(c["ts"]) <= _utc(since):
            continue
        groups.setdefault(key, []).append(c)
    out = []
    for (kind, marker), cs in sorted(groups.items(), key=lambda kv: (KINDS.index(kv[0][0]), kv[0][1] or "")):
        plans = sorted({c["plan"] for c in cs})
        if len(plans) < MIN_PLANS or (kind == "plan_noise" and len(cs) < MIN_NOISE):
            continue
        what = {"plan_miss": f"the reviewer missed {marker} errors that later edits corrected",
                "plan_noise": f"authors tagged {marker} findings false_positive and no edit proved them wrong",
                "plan_dismissed": "authors tagged plan findings through that later edits fixed after all"}[kind]
        out.append({"kind": kind, "rule": target(kind, marker),
                    "summary": f"{target(kind, marker)}: {what}, in {len(plans)} plans ({len(cs)} cases)",
                    "sources": [c.get("slug") or f"{c['sha'][:12]}:{c['plan']}" for c in cs],
                    "cases": cs})
    for i, p in enumerate(out, 1):
        p["id"] = f"M{i}"
    return out


def doc_edits(git: GitRun, sha: str) -> dict[str, str]:
    """Each proposal target → when its doc last changed at `sha`. Per file, not per section: a plan
    review doc is short, and any edit to it is a reason to start counting again."""
    out = {}
    for doc in (_REVIEWER_DOC, _AUTHOR_DOC):
        ts = (git("log", "-1", "--format=%cI", sha, "--", doc) or "").strip()
        if ts:
            for kind in KINDS:
                for marker in (MARKERS["plan"] if kind != "plan_dismissed" else (None,)):
                    if target(kind, marker).startswith(doc):
                        out[target(kind, marker)] = ts
    return out


def run(events: _t.Sequence[dict], *, date: str, sha: str, history: _t.Sequence[dict] = (),
        judge: _t.Callable | None = None, git: GitRun | None = None, meter: dict | None = None,
        failures: dict | None = None, answered: _t.Mapping[str, str] | None = None,
        repo: pathlib.Path = _REPO) -> tuple[dict, list[dict], float]:
    """(the record's `plan_postmortem` section, proposals, cost). `history`: earlier pass records, for
    the SHAs already judged and the cases already found. `answered`: target → the owner's last answer."""
    git = git or _git_in(repo)
    events = list(events)
    earlier = [r.get("plan_postmortem") or {} for r in history]
    judged_before = {s for r in earlier for s in r.get("judged") or []}
    since = (_dt.date.fromisoformat(date) - _dt.timedelta(days=WINDOW_DAYS)).isoformat()
    edits = reviewed_edits(plan_edits(git, sha, since=since, judged=judged_before), events)
    cases, judged, cost = judge_edits(edits, events, git=git, judge=judge, meter=meter, failures=failures)
    found = [c for r in earlier for c in r.get("cases") or []] + cases
    window = [c for c in found if c.get("ts", "")[:10] >= since]
    vindicated = {c.get("slug") for c in found if c["kind"] == "vindicated"}
    noise = [c for c in noise_cases(events, vindicated) if c["ts"][:10] >= since]
    cut = _rules.latest(doc_edits(git, sha), answered or {})
    props = proposals_from(window + noise, cut=cut)
    section = {"edits": len(edits), "judged": judged, "cases": cases, "noise": len(noise)}
    return section, props, cost


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--date", help="as of this date (default: today)")
    ap.add_argument("--ref", default="origin/main")
    args = ap.parse_args(argv)
    date = args.date or _dt.date.today().isoformat()
    sha = (git_output("rev-parse", args.ref, cwd=str(_REPO)) or "").strip()
    try:
        record = _load_sibling("record")
        section, props, cost = run(list(read_events()), date=date, sha=sha,
                                   history=record.pass_records(before=args.date))
    except _judge.RateLimited as e:
        return _judge.limit_exit("judge-plans", e)
    print(json.dumps({"plan_postmortem": section, "proposals": props}, indent=2, ensure_ascii=False))
    print(f"{section['edits']} plan edit(s), {len(section['cases'])} case(s), {len(props)} proposal(s); "
          f"judge ${cost:.2f}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
