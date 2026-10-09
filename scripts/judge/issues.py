#!/usr/bin/env python3
"""Weekly judge — the issue mirror: each actionable bug gets a GitHub issue that opens and closes with it.

Design: docs/todo/JUDGE_WORKFLOW_PLAN.md (D7–D11, D13–D15). The local record is the only source.
Issues are written, never read: the one thing read back is each judge issue's number, title and
open/closed state, from the REST list filtered by label `judge-bug` and creator = the `gh` user, and
a number counts only if the record maps the bug to it. Bodies, comments and labels are never read,
so an outsider's text can't reach an agent, and an edit is overwritten at the bug's next change.

What gets an issue: an `open` bug verify called `fix` or `decide`, stranded commits, and a repeated
agent error. A filed issue then follows its bug: closed when it is `gone` (completed) or `dismissed`
/ `wont_fix` / `parked` (not planned), reopened when it is live again, labelled `fix-landed` when a
commit names its key. Every issue is locked once filed, so only collaborators can comment on it.

Bodies are built from the record's structured fields (D11); the prose fields are defused (`@name`
and `#N` into code spans), home paths and project/image uids are stripped, and a backstop refuses
to file a body that still carries any of those. Every `gh` call goes through `Gh`, which allows only
the issue subcommands this needs and GETs on two API paths (D14). Bodies reach `gh` on stdin
(`--body-file -`), never argv.

Usage:
    pixi run judge-issues                  # what the newest record would file, close, reopen; nothing written
    pixi run judge-issues -- --apply       # do it, and record the issue numbers in the record
"""
from __future__ import annotations

import argparse
import hashlib
import importlib.util as _importlib_util
import json
import pathlib
import re
import shutil
import subprocess
import sys
import time
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.git_context import git_output  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")
_run_reviews = _load_sibling("run_reviews")

LABEL = "judge-bug"
#: Every label the mirror sets, created on first use (`gh label create --force` is idempotent).
LABELS = {LABEL: "5319e7", "fix": "d73a4a", "decide": "fbca04", "fix-landed": "0e8a16", "parked": "c5def5"}
#: Issues created per pass, and the gap between creates. More are reported and filed next pass.
MAX_CREATES = 20
CREATE_GAP_S = 1.0
#: Verdicts whose open bug gets an issue.
FILED_VERDICTS = ("fix", "decide")
_PAGE = 100
_TITLE_KEY = re.compile(r"^\[([\w-]+)\]")


# ── gh, allowlisted ────────────────────────────────────────────────────────────────────────────

class GhRefused(RuntimeError):
    """A `gh` call outside the allowlist: the pass's token is the owner's, so this is what bounds it."""


class GhError(RuntimeError):
    pass


_ALLOWED = {("issue", "create"), ("issue", "edit"), ("issue", "close"), ("issue", "reopen"),
            ("issue", "comment"), ("issue", "lock"), ("label", "create")}


def allowed(args: _t.Sequence[str], repo: str) -> bool:
    """D14: the issue subcommands the mirror needs, `label create`, and a GET of `user` or this repo's
    issue list. No other `gh` call runs, whatever asks for it."""
    if tuple(args[:2]) in _ALLOWED:
        return True
    if args[:1] == ["api"] and len(args) == 2:   # a bare path is a GET; any flag (-X, -f, --input) is not
        path = args[1]
        return path == "user" or path.split("?")[0] == f"repos/{repo}/issues"
    return False


def _default_run(args: list[str], input: str | None) -> str:
    gh = shutil.which("gh") or "gh"
    try:
        proc = subprocess.run([gh, *args], cwd=str(_REPO), capture_output=True, text=True, encoding="utf-8",
                              input=input, timeout=120, check=False)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise GhError(f"gh {' '.join(args[:2])} failed: {e}") from e
    if proc.returncode != 0:
        raise GhError(f"gh {' '.join(args[:2])} exited {proc.returncode}: {(proc.stderr or proc.stdout).strip()[-400:]}")
    return proc.stdout


class Gh:
    """Every `gh` call the mirror makes. `run(args, input)` does the call (a fake in tests); `dry`
    runs only the reads and collects the writes as `planned`."""

    def __init__(self, repo: str, run: _t.Callable[[list[str], str | None], str] | None = None, *, dry: bool = False):
        self.repo, self._run, self.dry = repo, run or _default_run, dry
        self.planned: list[tuple[list[str], str | None]] = []

    def __call__(self, *args: str, input: str | None = None) -> str:
        args = list(args)
        if not allowed(args, self.repo):
            raise GhRefused(f"gh {' '.join(args[:3])} is not on the mirror's allowlist")
        if args[0] != "api":
            args += ["--repo", self.repo]
        if self.dry and args[0] != "api":
            self.planned.append((args, input))
            return ""
        return self._run(args, input)

    def login(self) -> str:
        return json.loads(self("api", "user"))["login"]

    def judge_issues(self, owner: str) -> dict[int, dict]:
        """Every issue labelled `judge-bug` that `owner` opened, by number: `{state, title}`. The REST
        list, not search (which lags), and each one's author checked again."""
        out: dict[int, dict] = {}
        page = 1
        while True:
            batch = json.loads(self("api", f"repos/{self.repo}/issues?labels={LABEL}&creator={owner}"
                                           f"&state=all&per_page={_PAGE}&page={page}") or "[]")
            for i in batch:
                if "pull_request" not in i and (i.get("user") or {}).get("login") == owner:
                    out[i["number"]] = {"state": i["state"], "title": i["title"]}
            if len(batch) < _PAGE:
                return out
            page += 1


def repo_slug(repo: pathlib.Path = _REPO) -> str:
    """`owner/name` from the `origin` remote (git, so no `gh` call is spent on it)."""
    url = git_output("remote", "get-url", "origin", cwd=str(repo)) or ""
    m = re.search(r"github\.com[:/]([^/]+/[^/]+?)(?:\.git)?/?$", url.strip())
    if not m:
        raise GhError(f"origin {url!r} is not a GitHub repo")
    return m[1]


# ── what an issue says (D11) ───────────────────────────────────────────────────────────────────

_CODE_SPAN = re.compile(r"(```.*?```|`[^`\n]*`)", re.S)
_MENTION = re.compile(r"(?<![\w`])@[A-Za-z0-9][\w-]*(?:/[\w.-]+)?")
_ISSUE_REF = re.compile(r"(?<![\w&`])(?:[\w.-]+/[\w.-]+)?#\d+\b|\bGH-\d+\b")
_HOME = re.compile(r"(?:/home/|/Users/|[A-Za-z]:\\Users\\)[^/\\\s`]+")


def known_uids(projects: pathlib.Path | None = None) -> set[str]:
    """Project, set and image uids on this machine (`{projects}/{proj}/1/{uid}`): stripped from bodies."""
    root = projects or _run_reviews.projects_dir()
    names = {p.name for p in root.glob("*")} | {p.name for p in root.glob("*/1/*")} if root.is_dir() else set()
    return {n for n in names if re.fullmatch(r"[A-Za-z0-9]{6}", n)}


def defuse(text: str, uids: _t.Collection[str] = ()) -> str:
    """Prose from the record, safe for a public body: `@name` and `#N` outside code go inside a code
    span, where GitHub neither notifies nor cross-links; home paths become `~`; uids become `<uid>`."""
    parts = _CODE_SPAN.split(text or "")
    for i in range(0, len(parts), 2):   # even parts are outside code
        parts[i] = _ISSUE_REF.sub(lambda m: f"`{m[0]}`", _MENTION.sub(lambda m: f"`{m[0]}`", parts[i]))
    out = _HOME.sub("~", "".join(parts))
    for u in uids:
        out = re.sub(rf"\b{re.escape(u)}\b", "<uid>", out)
    return out


def leaks(text: str, uids: _t.Collection[str] = ()) -> list[str]:
    """The backstop: what a filled body still carries that it mustn't. Any hit and it isn't filed."""
    prose = _CODE_SPAN.sub("", text)
    found = [f"mention {m}" for m in _MENTION.findall(prose)] + [f"reference {m}" for m in _ISSUE_REF.findall(prose)]
    found += [f"home path {m}" for m in _HOME.findall(text)]
    found += [f"uid {u}" for u in uids if re.search(rf"\b{re.escape(u)}\b", text)]
    return found


def _verdict(b: dict) -> str | None:
    if b.get("kind") == "stranded":
        return "fix"
    return (b.get("verify") or {}).get("verdict")


def filed(b: dict) -> bool:
    """D7: an `open` bug that needs a person: verify said fix or decide, stranded commits, a repeated
    agent error."""
    return b["status"] == "open" and (_verdict(b) in FILED_VERDICTS or b.get("kind") == "stranded"
                                      or bool(b.get("repeat")))


def title(b: dict) -> str:
    """Structured only, no prose: the key (which recovery finds it by), the verdict, where."""
    where = (f"stranded commits, PR {b.get('pr')}" if b.get("kind") == "stranded" else
             f"{b['file']}:{b['line']}" if b.get("file") else
             f"{'repeated ' if b.get('repeat') else ''}agent error in {b.get('tool') or '?'}")
    return f"[{b['key']}] {_verdict(b) or 'open'}: {where}"


def _agent_error(b: dict) -> str:
    """An agent run's error by its form only (D11): the tool, the HTTP status, the repeat template."""
    m = re.match(r"HTTP (\d{3})", b.get("error") or "")
    return (f"`{b.get('tool') or '?'}` failed" + (f" with HTTP {m[1]}" if m else "")
            + (f": {b['template']}" if b.get("template") else "")
            + (f", in {b['runs']} separate runs" if (b.get("runs") or 0) > 1 else ""))


def body(b: dict, *, repo: str, sha: str, uids: _t.Collection[str] = ()) -> str:
    v = b.get("verify") or {}
    d = lambda s: defuse(s, uids)   # noqa: E731
    lines = [f"**Judge bug** `{b['key']}` · verdict **{_verdict(b) or 'open'}** · first seen {b.get('first_seen') or '?'}", ""]
    if b.get("kind") == "stranded":
        lines += [f"**Where:** commits pushed to `{b.get('branch')}` after https://github.com/{repo}/pull/{b.get('pr')} "
                  "merged, which never reached main: " + ", ".join(f"`{c[:8]}`" for c in b.get("commits", [])), ""]
    elif b.get("file"):
        lines += [f"**Where:** [`{b['file']}:{b['line']}`](https://github.com/{repo}/blob/{sha}/{b['file']}#L{b['line']})"
                  f" at `{sha[:8]}`", ""]
    else:
        lines += [f"**Where:** {_agent_error(b)}", ""]
    lines += [f"**Effect:** {d(v['effect'])}", ""] if v.get("effect") else []
    lines += [f"**Question:** {d(v['question'])}", ""] if v.get("question") else []
    lines += [f"**Evidence:** {d(v['evidence'])}", ""] if v.get("evidence") else []
    if b.get("file") and b.get("desc"):   # an agent error's text is its raw message: not quoted (D11)
        lines += ["**Finding** (reviewer output, quoted):", ""] + [f"> {ln}" if ln else ">" for ln in d(b["desc"]).splitlines()] + [""]
    if b.get("fix_landed"):
        lines += ["**Fix landed:** " + ", ".join(f"`{c['commit'][:8]}`" for c in b["fix_landed"])
                  + ", awaiting the judge's re-check.", ""]
    lines += ["---", "Filed by the weekly judge from its local record. This issue is a mirror: its text, "
              "comments and labels are never read back, and the next change to the bug rewrites it. "
              "Answer it in `pixi run judge-review`; fix briefs say `Refs` and never close it."]
    return "\n".join(lines) + "\n"


def safe(text: str, fallback: str, uids: _t.Collection[str] = ()) -> str:
    """A comment built partly from prose: `text`, or `fallback` if the backstop finds anything in it."""
    return fallback if leaks(text, uids) else text


def _sha(text: str) -> str:
    return hashlib.sha1(text.encode("utf-8")).hexdigest()[:12]


# ── the mirror ─────────────────────────────────────────────────────────────────────────────────

def _closing(b: dict, uids: _t.Collection[str] = ()) -> tuple[str, str] | None:
    """(reason, comment) for a filed bug whose issue should now be closed, or None."""
    v = b.get("verify") or {}
    if b["status"] == "gone":
        return "completed", safe(f"Confirmed gone by the judge: {defuse(b.get('why') or '', uids)}",
                                 "Confirmed gone by the judge.", uids)
    if b["status"] == "dismissed":
        return "not planned", safe("Dismissed by verify" + (f": {defuse(v['effect'], uids)}" if v.get("effect") else "."),
                                   "Dismissed by verify.", uids)
    if b["status"] == "wont_fix":
        return "not planned", "Closed as won't fix in `pixi run judge-review`."
    if b["status"] == "parked":
        return "not planned", safe("Parked: verify found it can't happen yet"
                                   + (f"; live once {defuse(v['trigger'], uids)}" if v.get("trigger") else "") + ".",
                                   "Parked: verify found it can't happen yet.", uids)
    return None


def _labels(b: dict) -> list[str]:
    out = [LABEL]
    out += [_verdict(b)] if _verdict(b) in FILED_VERDICTS else []
    out += ["fix-landed"] if b.get("fix_landed") else []
    out += ["parked"] if b["status"] == "parked" else []
    return out


def mirror(record: dict, *, gh: Gh, previous: dict | None = None, persist: _t.Callable[[dict], None] | None = None,
           uids: _t.Collection[str] = (), sleep: _t.Callable[[float], None] = time.sleep,
           max_creates: int = MAX_CREATES) -> dict:
    """Bring the judge's issues in line with `record`; returns what it did (`report`).

    Each bug's issue is `bug["issue"] = {number, state, labels, body, landed}`, carried with the bug.
    Before a create the bug is marked `{"status": "pending"}` and `persist`ed; a pass that dies before
    the number is recorded is recovered by the next one, which finds the issue by the key in its title
    (D10). A bug in `previous` that this record no longer lists (a `wont_fix`, say) has its open
    issue closed too.
    """
    owner = gh.login()
    listed = gh.judge_issues(owner)
    by_key = {m[1]: n for n, i in listed.items() if (m := _TITLE_KEY.match(i["title"]))}
    sha = record["run"]["sha"]
    report: dict[str, list] = {k: [] for k in ("filed", "updated", "closed", "reopened", "held", "over_cap", "missing")}
    labels_made: set[str] = set()
    creates = 0

    def save():
        if persist and not gh.dry:
            persist(record)

    def label(names):
        for n in names:
            if n not in labels_made:
                gh("label", "create", n, "--color", LABELS[n], "--force")
                labels_made.add(n)

    lead = {k: b["key"] for b in record["bugs"] for k in (b.get("sources") or [b["key"]])}
    dropped = [b for b in (previous or {}).get("bugs", [])
               if b["key"] not in {x["key"] for x in record["bugs"]} and (b.get("issue") or {}).get("number")]
    for b, gone_from_record in [*((b, False) for b in record["bugs"]), *((b, True) for b in dropped)]:
        issue = dict(b.get("issue") or {})
        n = issue.get("number")
        if not n and (issue.get("status") == "pending" or filed(b)):
            n = by_key.get(b["key"])   # recovery: filed, then the pass died before recording it
            if n:
                issue = {"number": n, "state": listed[n]["state"], "labels": [LABEL], "adopted": True}
        if n and n not in listed:      # deleted, relabelled, or never the owner's: never touched
            report["missing"].append(b["key"])
            continue
        if gone_from_record:           # a `wont_fix`, or merged into another bug this pass
            if listed[n]["state"] == "open":
                why = ("Closed as won't fix in `pixi run judge-review`." if b["status"] == "wont_fix" else
                       f"Merged into the judge's bug `{lead[b['key']]}`." if b["key"] in lead else
                       "No longer tracked by the judge.")
                gh("issue", "close", str(n), "--reason", "not planned", "--comment", why)
                report["closed"].append(b["key"])
            continue
        if not n:
            if not filed(b):
                continue
            text, head = body(b, repo=gh.repo, sha=sha, uids=uids), title(b)
            hits = leaks(head + "\n" + text, uids)
            if hits:
                report["held"].append({"key": b["key"], "why": "; ".join(hits)})
                continue
            if creates >= max_creates:
                report["over_cap"].append(b["key"])
                continue
            if creates:
                sleep(CREATE_GAP_S)
            creates += 1
            label(_labels(b))
            if not gh.dry:
                b["issue"] = {"status": "pending"}
                save()
            out = gh("issue", "create", "--title", head, "--body-file", "-",
                     *[a for lb in _labels(b) for a in ("--label", lb)], input=text)
            if gh.dry:
                gh("issue", "lock", "<new>")
                report["filed"].append(b["key"])
                continue
            n = int(out.strip().rstrip("/").rsplit("/", 1)[-1])
            gh("issue", "lock", str(n))
            b["issue"] = {"number": n, "state": "open", "labels": _labels(b), "body": _sha(text),
                          "landed": [c["commit"] for c in b.get("fix_landed", [])]}
            save()
            report["filed"].append(b["key"])
            continue
        state = listed[n]["state"]
        close = _closing(b, uids)
        if close and state == "open":
            reason, comment = close
            if b["status"] == "parked":
                label(["parked"])
                gh("issue", "edit", str(n), "--add-label", "parked")
                issue["labels"] = [*(issue.get("labels") or []), "parked"]
            gh("issue", "close", str(n), "--reason", reason, "--comment", comment)
            issue.update(state="closed")
            report["closed"].append(b["key"])
        elif filed(b) and state == "closed":
            gh("issue", "reopen", str(n), "--comment", safe(f"Reopened by the judge: live again ({defuse(b.get('why') or '', uids)}).",
                                                        "Reopened by the judge: live again.", uids))
            issue.update(state="open")
            report["reopened"].append(b["key"])
        if filed(b):
            text, want = body(b, repo=gh.repo, sha=sha, uids=uids), _labels(b)
            hits = leaks(text, uids)
            if hits:
                report["held"].append({"key": b["key"], "why": "; ".join(hits)})
            elif _sha(text) != issue.get("body") or want != issue.get("labels"):
                have = issue.get("labels") or []
                label(want)
                gh("issue", "edit", str(n), "--title", title(b), "--body-file", "-",
                   *[a for lb in want if lb not in have for a in ("--add-label", lb)],
                   *[a for lb in have if lb not in want for a in ("--remove-label", lb)], input=text)
                issue.update(body=_sha(text), labels=want)
                report["updated"].append(b["key"])
            new = [c for c in b.get("fix_landed", []) if c["commit"] not in issue.get("landed", [])]
            if new:
                gh("issue", "comment", str(n), "--body-file", "-", input="A commit naming this bug's key landed: "
                   + ", ".join(f"`{c['commit'][:8]}`" + (f" (https://github.com/{gh.repo}/pull/{c['pr']})"
                                                        if c.get("pr") else "") for c in new)
                   + ". It stays open until the judge confirms it gone.\n")
                issue["landed"] = [*issue.get("landed", []), *(c["commit"] for c in new)]
        if not gh.dry:
            b["issue"] = issue
    save()
    return report


def report_lines(report: dict) -> list[str]:
    out = [f"{k.replace('_', ' ')}: {len(v)}" for k, v in report.items() if v]
    out += [f"  held {h['key']}: {h['why']}" for h in report.get("held", [])]
    return out or ["nothing to do"]


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--date", help="record to mirror (default: the newest pass record)")
    ap.add_argument("--apply", action="store_true", help="file, close and reopen for real; record the numbers")
    args = ap.parse_args(argv)
    records = _record.pass_records()
    if args.date:
        records = [r for r in records if r["date"] <= args.date]
    if not records:
        print("judge-issues: no pass record in the store", file=sys.stderr)
        return 1
    record, previous = records[-1], (records[-2] if len(records) > 1 else None)
    gh = Gh(repo_slug(), dry=not args.apply)
    report = mirror(record, gh=gh, previous=previous, uids=known_uids(),
                    persist=lambda r: _record.write(r, force=True))
    if gh.dry:
        for cmd, text in gh.planned:
            print("gh " + " ".join(json.dumps(a) if " " in a else a for a in cmd))
            if text:
                print("".join(f"    {ln}\n" for ln in text.splitlines()))
    print(f"{record['date']}: " + "; ".join(report_lines(report)) + ("" if args.apply else " (dry run: nothing written)"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
