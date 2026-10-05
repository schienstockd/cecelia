#!/usr/bin/env python3
"""Commit-recital hook — every reviewer finding in a commit's recital must carry an outcome tag,
and slug-paired outcomes get written to the effectiveness log as resolution rows.

Two entry points, one file
--------------------------
- **git `commit-msg` hook** (`.githooks/commit-msg` → `--commit-msg <file>`) — where the checks
  and log writes run. Git hands over the ACTUAL message file, in the worktree being committed
  to, only when a commit really happens. Activated once per clone by `pixi run
  install-git-hooks` (`core.hooksPath=.githooks`, shared by every worktree; each worktree runs
  its own checkout's copy).
- **Claude Code PreToolUse guard** (`.claude/settings.json`, no args, tool-call JSON on stdin) —
  only makes sure the git hook can't be skipped: blocks a `git commit` Bash command when
  `core.hooksPath` isn't set, or when it passes `--no-verify` / `-n` / a `core.hooksPath`
  override.

Why the checks moved (2026-09-30): the PreToolUse version matched the Bash COMMAND TEXT. It fired
on test heredocs that merely contained `git commit` + a slug pair (writing fake resolution rows),
never saw a `git commit -F file` message, and read the branch/SHA from the session's cwd rather
than the worktree a `cd other && git commit` actually committed in.

What it enforces
----------------
The convention-check reviewer is advisory by design (docs/todo/CONVENTION_CHECK_PLAN.md locked
decision #4) — findings land in the reservations recital but do not block a commit. That is
toothless in autonomous mode: nothing challenges a `**should reuse`` finding that the agent
silently commits over. Fanout audit is the same shape for its own findings.

This hook enforces these gates on a commit whose message carries reviewer findings:

1. **Outcome-tag presence.** For every `**confirmed**` fanout / `**should reuse**` or
   `**wrong home**` convention finding line, the message must carry an outcome tag from the
   closed vocabulary — `fixed_pre_commit`, or `shipped_with_finding` / `false_positive` /
   `dropped_no_action` with a reason (slug-paired: `[<slug>: false_positive: <reason>]`).
   Deliberately mechanical text-matching, not semantic judgment: an agent could still write
   `false_positive: reasons` untruthfully. The weekly bug sweep (`scripts/judge/bugs.py`) is what
   checks a `false_positive` against the code, with its reason in front of the judge.
2. **SHA-anchored real-review.** A findings-carrying commit must have at least one recital
   `_run` row in the effectiveness log with `commit == HEAD-at-hook-time` — i.e. recital
   actually ran, and against the same tree the commit is being made on top of. Catches
   hand-typed recital bodies with no real `pixi run recital` invocation, and stale reviews
   from before a rebase. If HEAD SHA can't be captured (not a repo, git missing), the check
   degrades to allow rather than block on its own failure.
3. **Every logged finding tagged.** Each `_finding` row recital logged for this change (parent
   SHA + branch) needs a `[slug: outcome]` pair in this commit or an earlier one on the
   branch — even when the message no longer quotes the finding. A finding fixed and trimmed
   from the message must still say `fixed_pre_commit`; nothing closes it on the agent's behalf.

P3 of FINDINGS_EMISSION_PLAN.md — log writing
---------------------------------------------
When the message quotes a slug in a `[slug: outcome]` pair (e.g. `[fanout-abcd1234: fixed_pre_commit]`),
the hook ALSO writes a matching `_finding_resolved` row (with the tag's `reason`, when it has one) to `~/.cecelia-effectiveness/events.jsonl`.
That is what turns the recital's pending `_finding` rows into evidence in the rollup — see
`python/cecelia/effectiveness/rollup.py`. Log-write failure is not a commit blocker (best-effort);
the presence-check is the only gate.

Merge / cherry-pick / revert commits are skipped entirely: their messages are git-generated,
and gate 3 must not demand tags for a branch's pending findings because a merge landed.

Escape valve
------------
`CECELIA_SKIP_RECITAL_CHECK=1 git commit …` bypasses (git passes its environment to the hook).
For real emergencies only.
"""
from __future__ import annotations

import json
import os
import pathlib
import re
import sys

#: The hook lives at `.claude/hooks/check_commit_recital.py`; the effectiveness log module
#: lives at `python/cecelia/effectiveness/log.py`. Reach the canonical vocabulary rather than
#: keeping a second literal copy — a divergent copy was the exact class of bug that
#: fanout + convention-check both caught on this hook's first draft.
_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO_ROOT / "python"))
from cecelia.effectiveness import OUTCOME_VOCABULARY, append_event, read_events  # noqa: E402
from cecelia.effectiveness.recital import MARKERS, outside_code  # noqa: E402
from cecelia.effectiveness.git_context import (  # noqa: E402
    current_branch as _current_branch,
    current_head_sha as _current_head_sha,
    current_pr as _current_pr,
    git_output as _git,
)

#: Every finding line in the recital carries one of these bold markers — built from recital's
#: `MARKERS`, so a new outcome-tagged marker can't be emitted there and missed here.
_FINDING_MARKERS = re.compile(
    r"\*\*(?:" + "|".join(re.escape(m) for ms in MARKERS.values() for m in ms) + r")\*\*"
)

#: `fixed_pre_commit` stands alone (the fix is in the diff). Every other outcome needs a
#: colon-prefixed reason, so a bare word appearing in prose or code doesn't count as a tag.
#: The standalone-vs-with-reason split is a hook convention, not a vocabulary property, so it
#: lives here rather than in `log.py`.
_STANDALONE_OUTCOMES = frozenset({"fixed_pre_commit"})
_REASON_OUTCOMES = OUTCOME_VOCABULARY - _STANDALONE_OUTCOMES

_alternation_reason = "|".join(sorted(map(re.escape, _REASON_OUTCOMES)))
_alternation_standalone = "|".join(sorted(map(re.escape, _STANDALONE_OUTCOMES)))

#: Slug-paired outcome — `[<slug>: <outcome>]` or `[<slug>: <outcome>: <reason>]`, where slug is
#: `<mechanism>-<8 hex>`. The reason runs to the closing `]` (so it can't contain one) and is written
#: to the resolution row: the bug sweep shows a `false_positive`'s reason to its judge, which checks
#: it against the code. Every outcome but `fixed_pre_commit` needs one (`check`).
_alternation_all = "|".join(sorted(map(re.escape, OUTCOME_VOCABULARY)))
_SLUG_PAIR = re.compile(
    rf"\[((?:fanout|conv)-[0-9a-f]{{8}}):\s*({_alternation_all})(?:\s*:\s*([^\]\n]*?))?\s*\]"
)

#: Legacy bare outcome tags — accepted for counting so pre-P3 messages (or bespoke commits
#: without slugs) don't spuriously block. Slug pairs are counted separately then folded into
#: the total.
_BARE_OUTCOME_TAGS = re.compile(
    rf"\b(?:(?:{_alternation_standalone})\b|(?:{_alternation_reason}):\s*\S)"
)


def _outcome_help() -> str:
    """Build the help text listing every outcome, so a new vocabulary entry appears here for free."""
    lines = ["Slug-paired form (from `pixi run recital`):"]
    for tag in sorted(_STANDALONE_OUTCOMES):
        lines.append(f"  - `[<slug>: {tag}]`  # e.g. `[fanout-abcd1234: {tag}]`")
    for tag in sorted(_REASON_OUTCOMES):
        lines.append(f"  - `[<slug>: {tag}: <reason>]`")
    lines.append("Legacy bare form (no slug):")
    for tag in sorted(_STANDALONE_OUTCOMES):
        lines.append(f"  - `[{tag}]`")
    for tag in sorted(_REASON_OUTCOMES):
        lines.append(f"  - `[{tag}: <reason>]`")
    return "\n".join(lines)

#: One `git … commit …` invocation inside a Bash command, up to the next `;`/`&`/`|`/newline —
#: subshells count (`cd path && git commit …`), and so do global options (`git -c k=v commit`).
_GIT_COMMIT_SEGMENT = re.compile(r"\bgit\b(?:\s+-\S+(?:\s+[^\s;&|-]\S*)?)*\s+commit\b[^;&|\n]*")

#: `_run` events the SHA-anchored check considers. A recital run always emits both; findings-
#: carrying commits must have at least one row with `commit == HEAD-at-hook-time`.
_RUN_EVENTS = frozenset({"fanout_audit_run", "convention_check_run"})

_EXIT_ALLOW = 0
_EXIT_BLOCK = 2  # PreToolUse convention: non-zero = block; stderr shown to Claude.


def _load_tool_call() -> dict:
    """Read the hook's stdin JSON. Return {} on malformed input so we degrade to allow rather
    than block on our own bug."""
    try:
        return json.load(sys.stdin)
    except (json.JSONDecodeError, ValueError):
        return {}


def _parse_pairs(command: str) -> list[tuple[str, str, str]]:
    """Return the (slug, outcome, reason) triples the commit message quotes; reason "" when absent."""
    return [(m.group(1), m.group(2), (m.group(3) or "").strip()) for m in _SLUG_PAIR.finditer(command)]


def _finding_event_for_slug(slug: str) -> str | None:
    """Map a slug prefix to its `_finding_resolved` event name."""
    if slug.startswith("fanout-"):
        return "fanout_audit_finding_resolved"
    if slug.startswith("conv-"):
        return "convention_check_finding_resolved"
    return None


def _same_change(row: dict, head_sha: str, branch: str | None) -> bool:
    """True if `row` was written against this change — same parent SHA AND same branch.

    SHA alone is not an identity: parallel worktrees branch off the same `origin/main` tip,
    so their recitals share a parent SHA. Joining on SHA only let one session's commit
    auto-drop another session's finding (2026-09-30: `card-reshuffle` dropped
    `module-keepalive`'s `fanout-7d998e9e`). `branch=None` (detached HEAD / git failure)
    falls back to SHA only; gate 3 skips instead (it would block on another worktree's findings).
    """
    if row.get("commit") != head_sha:
        return False
    return branch is None or row.get("branch") == branch


def _has_matching_run(head_sha: str, branch: str | None = None) -> bool:
    """True if the log has a `_run` row for this change (see `_same_change`).

    Log-read failure counts as "no matching run" — a missing log means recital never
    emitted anything for this HEAD, which is exactly what the check is meant to catch.
    """
    try:
        for row in read_events():
            if row.get("event") in _RUN_EVENTS and _same_change(row, head_sha, branch):
                return True
    except OSError:
        pass
    return False


def check(command: str) -> str | None:
    """Return None if the commit message `command` is allowed; else a human-readable reason.

    Three gates:
    1. **Outcome-tag presence** — every finding marker (recital's `MARKERS`)
       needs a matching outcome tag (bare or slug-paired). Duplicate slugs are rejected.
    2. **SHA-anchored real-review** — a findings-carrying commit must have at least one
       recital `_run` row in the effectiveness log with `commit == HEAD-at-hook-time` (i.e.
       recital ran against the same tree the commit is being made on top of). Catches
       hand-typed recital bodies with no real `pixi run recital` invocation, and stale
       reviews from before a rebase.
    3. **Every logged slug tagged** — if the log holds `_finding` rows for this change that
       neither this message nor an earlier commit on the branch tags by slug, block and list
       them — whether the message tags bare, or dropped the finding line altogether.

    Gates 2 and 3 are skipped when HEAD SHA cannot be captured (not a repo, git missing) — degrade
    to allow rather than block on our own failure.
    """
    # Same rule as the recital parser: a marker quoted in backticks is prose, not a finding.
    findings = _FINDING_MARKERS.findall(outside_code(command))
    # Strip slug pairs first: the outcome word inside `[fanout-…: fixed_pre_commit]` also
    # matches the bare pattern, which made every slug-tagged commit look bare-tagged.
    bare_outcomes = _BARE_OUTCOME_TAGS.findall(_SLUG_PAIR.sub(" ", command))
    pairs = _parse_pairs(command)

    # Duplicate slugs in one commit are a red flag — the same finding can't have two outcomes.
    # Block explicitly so the author de-dupes rather than silently taking whichever the log
    # happens to keep as "latest".
    slugs = [p[0] for p in pairs]
    dup_slugs = sorted({s for s in slugs if slugs.count(s) > 1})
    if dup_slugs:
        return (
            f"Duplicate slug(s) in commit message: {', '.join(dup_slugs)}. Each slug must "
            "appear at most once — the same finding cannot have two outcomes."
        )

    # The reason is what the bug sweep's judge weighs a `false_positive` against, and what tells a
    # deliberate drop from an abandoned one. The legacy bare form already required it.
    no_reason = [f"[{s}: {o}]" for s, o, r in pairs if o in _REASON_OUTCOMES and not r]
    if no_reason:
        return (
            f"Outcome tag(s) with no reason: {', '.join(no_reason)}. Every outcome except "
            "`fixed_pre_commit` carries a one-line reason inside the tag, e.g. "
            "`[fanout-abcd1234: false_positive: the caller already filters empty rows]` "
            "(no `]` in the reason)."
        )

    total_outcomes = len(bare_outcomes) + len(set(slugs))

    if len(findings) > total_outcomes:
        return (
            f"Recital carries {len(findings)} reviewer finding(s) marked "
            f"{' / '.join(f'**{m}**' for ms in MARKERS.values() for m in ms)} "
            f"but only {total_outcomes} outcome tag(s). "
            "Every finding needs one:\n"
            f"{_outcome_help()}\n"
            "Add the outcome tag to each finding line, then retry. "
            "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1 git commit …`."
        )

    if findings:
        head_sha, branch = _current_head_sha(), _current_branch()
        if head_sha is not None and not _has_matching_run(head_sha, branch):
            return (
                f"Recital carries {len(findings)} reviewer finding(s) but no matching "
                f"`_run` row is in the effectiveness log for HEAD {head_sha[:8]}. Run "
                "`pixi run recital` against the current tree, then retry — a hand-typed "
                "recital body without a real reviewer invocation is what this check exists "
                "to catch. After a rebase, re-run recital: the parent SHA changed. "
                "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1 git commit …`."
            )
    # Gate 3: every finding recital logged for this change needs its slug tagged — bare tags
    # write nothing to the log, and a finding trimmed from the message (often because it was
    # fixed) would stay pending with no record of what happened to it.
    head_sha, branch = _current_head_sha(), _current_branch()
    if head_sha is not None and branch is not None:
        untagged = _untagged_slugs_on_head(head_sha, branch, set(slugs))
        if untagged:
            listed = "\n".join(f"  - `{slug}` {_where(row)}" for slug, row in sorted(untagged.items()))
            return (
                f"Recital logged {len(untagged)} finding(s) for HEAD {head_sha[:8]} that this "
                f"commit doesn't tag by slug:\n{listed}\n"
                "Tag each one `[<slug>: fixed_pre_commit]` if you fixed it, else "
                "`[<slug>: shipped_with_finding | false_positive | dropped_no_action: <reason>]` "
                "— even when the message no longer quotes the finding. Bare tags like "
                "`[fixed_pre_commit]` don't reach the log. "
                "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1 git commit …`."
            )

    return None


def write_resolutions(
    command: str, *, pr: str | None = None, commit: str | None = None,
    branch: str | None = None,
) -> int:
    """Write one `_finding_resolved` row per slug-paired outcome in the commit message.

    Returns the number of rows written. Log-write failures are swallowed so a bad log path
    can't wedge the commit — the presence-check is the only gate. `pr`, `commit`, and
    `branch` are captured from `gh pr view`, `git rev-parse HEAD`, and `git rev-parse
    --abbrev-ref HEAD`; None on any failure. `commit` here is the parent SHA of the commit
    being made (HEAD hasn't advanced yet when git runs the `commit-msg` hook), matching the SHA the recital
    `_run` row was written with. `branch` lets the rollup join to a PR later even when `pr`
    is null at write time (typical — findings land pre-commit).
    """
    written = 0
    for slug, outcome, reason in _parse_pairs(command):
        event = _finding_event_for_slug(slug)
        if event is None:
            continue  # unknown mechanism prefix — silently skip; hook regex won't match anyway
        try:
            append_event(event, {"slug": slug, "outcome": outcome, **({"reason": reason} if reason else {})},
                         pr=pr, commit=commit, branch=branch)
            written += 1
        except Exception:  # noqa: BLE001 — best-effort emission
            pass
    return written


#: Finding-emitting events gate 3 considers. `_finding` events land in the log atomically with
#: the reviewer spawn (`recital.py::_run_reviewer`).
_FINDING_EVENTS = frozenset({"fanout_audit_finding", "convention_check_finding"})
#: Their resolution counterparts.
_RESOLVED_EVENTS = frozenset({
    "fanout_audit_finding_resolved", "convention_check_finding_resolved",
})


def _where(row: dict) -> str:
    """`file:line (marker)` of a `_finding` row, for naming it in a block message."""
    p = row.get("payload") or {}
    loc = f"{p.get('file') or '?'}:{p['line']}" if p.get("line") else (p.get("file") or "?")
    return f"{loc} ({p.get('marker') or '?'})"


def _untagged_slugs_on_head(head_sha: str, branch: str,
                            tagged_slugs: set[str]) -> dict[str, dict]:
    """Slugs the recital emitted for this change (`_same_change`: SHA + branch) that neither
    `tagged_slugs` nor any prior resolution covers, mapped to their `_finding` row.
    Log-read failure → empty."""
    findings_on_head: dict[str, dict] = {}
    resolved_slugs: set[str] = set()
    try:
        for row in read_events():
            event = row.get("event")
            slug = (row.get("payload") or {}).get("slug")
            if not slug:
                continue
            if event in _FINDING_EVENTS and _same_change(row, head_sha, branch):
                findings_on_head.setdefault(slug, row)
            elif event in _RESOLVED_EVENTS and row.get("branch") == branch:
                # Same branch, any SHA: an earlier commit on this branch may have resolved it.
                # Not other branches — slugs carry no branch, so two worktrees flagging the
                # same file:line share a slug, and one's resolution mustn't hide the other's.
                resolved_slugs.add(slug)
    except OSError:
        return {}
    return {s: e for s, e in findings_on_head.items()
            if s not in resolved_slugs and s not in tagged_slugs}


#: Git state files present while a git-generated commit is being made. Their messages carry no
#: recital, and gate 3 on them would demand tags for the branch's pending findings.
_SEQUENCER_STATE = ("MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD")

_HOOKS_PATH = ".githooks"


def _read_message(path: str) -> str:
    """The commit message, minus git's `#` comment lines (default `cleanup=strip` drops them
    from the commit anyway; a commented-out finding must not count)."""
    text = pathlib.Path(path).read_text(encoding="utf-8", errors="replace")
    return "\n".join(line for line in text.splitlines() if not line.startswith("#"))


def _in_sequencer_commit() -> bool:
    return any(_git("rev-parse", "-q", "--verify", ref) for ref in _SEQUENCER_STATE)


def agent_recital_gate() -> str | None:
    """An AGENT's commit needs a real recital run for this change, findings or not.

    Gate 2 in `check` only fires when the message carries finding markers, so a commit with no
    findings needed no recital at all — and on 2026-09-30 a session copied the previous commit's
    `Convention check: … Inventory check: run` trailer without running recital, and it went
    through. Claude Code sets `CLAUDECODE=1` in every Bash command it runs and git passes the
    environment to hooks, so this applies to agent commits only; a human's commit is unaffected.
    Degrades to allow when the SHA can't be read.
    """
    if os.environ.get("CLAUDECODE") != "1":
        return None
    head_sha, branch = _current_head_sha(), _current_branch()
    if head_sha is None or _has_matching_run(head_sha, branch):
        return None
    return (f"no recital run is logged for this change (HEAD {head_sha[:8]}, branch "
            f"{branch or '?'}). Run `pixi run recital` on the staged diff and append its output "
            "— never copy another commit's check trailer. For a throwaway commit (a WIP to "
            "rebase), `CECELIA_SKIP_RECITAL_CHECK=1 git commit …`.")


def commit_msg_main(message_path: str) -> int:
    """git `commit-msg` hook: run the gates on the real message, then write resolutions.

    Git runs this at the worktree top level with HEAD still at the parent, so the SHA and
    branch `git_context` reads are the commit's own — no cwd guessing. Non-zero exit aborts
    the commit and git prints our stderr.
    """
    if os.environ.get("CECELIA_SKIP_RECITAL_CHECK") == "1" or _in_sequencer_commit():
        return _EXIT_ALLOW
    message = _read_message(message_path)
    reason = agent_recital_gate() or check(message)
    if reason is not None:
        print(f"check_commit_recital: BLOCKED — {reason}", file=sys.stderr)
        return 1
    # Best-effort: log failures don't block the commit.
    pr, head_sha, branch = _current_pr(), _current_head_sha(), _current_branch()
    write_resolutions(message, pr=pr, commit=head_sha, branch=branch)
    return _EXIT_ALLOW


#: Ways a `git commit` command skips or redirects the commit-msg hook.
_SKIPS_HOOKS = re.compile(r"(?:^|\s)(?:--no-verify|-n)(?=\s|$)|core\.hooksPath")


def guard(command: str, cwd: str | None) -> str | None:
    """PreToolUse guard: None to allow, else why this `git commit` would dodge the git hook.

    Text-matching is fine here because a false positive only BLOCKS (the agent rewrites the
    command); it never writes to the log, which is what the old in-command checks got wrong.
    """
    # `git [-c k=v …] commit …` segments; only their own flags count (`&& head -n 5` doesn't).
    segments = _GIT_COMMIT_SEGMENT.findall(command)
    if not segments:
        return None
    if any(_SKIPS_HOOKS.search(seg) for seg in segments):
        return ("this `git commit` skips the recital git hook (`--no-verify` / `-n` / a "
                "`core.hooksPath` override). Commit normally; for a real emergency use "
                "`CECELIA_SKIP_RECITAL_CHECK=1 git commit …`.")
    hooks_path = _git("config", "--get", "core.hooksPath", cwd=cwd)
    in_repo = _git("rev-parse", "--git-dir", cwd=cwd) is not None
    if in_repo and hooks_path != _HOOKS_PATH:
        return (f"the recital git hook isn't active in this clone (`core.hooksPath` is "
                f"{hooks_path or 'unset'}, needs `{_HOOKS_PATH}`). Run `pixi run "
                "install-git-hooks` once, then retry.")
    return None


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    if len(argv) == 2 and argv[0] == "--commit-msg":
        return commit_msg_main(argv[1])

    data = _load_tool_call()
    if data.get("tool_name") != "Bash":
        return _EXIT_ALLOW
    reason = guard(data.get("tool_input", {}).get("command", ""), data.get("cwd"))
    if reason is not None:
        print(f"check_commit_recital: BLOCKED — {reason}", file=sys.stderr)
        return _EXIT_BLOCK
    return _EXIT_ALLOW


if __name__ == "__main__":
    sys.exit(main())
