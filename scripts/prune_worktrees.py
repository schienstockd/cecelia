"""`pixi run prune-worktrees` — say which sibling worktrees can go; with `--remove`, remove only those.

Every worktree lands in ONE bucket. Only SAFE (merged, clean, unused, no stash) and DEAD (the
folder is gone) are ever acted on, and only with `git worktree remove` / `git worktree prune` /
`git branch -d` — never `--force`, `branch -D` or `kill`. Anything that needs one of those is
printed as a command for a person to run. Design and the reasons behind each rule:
docs/todo/WORKTREE_CLEANUP_PLAN.md.

Fact gathering (git, gh, /proc) is kept apart from `classify`, which is pure, so the tests drive
the bucket rules with made-up facts and never touch a real repo.
"""
from __future__ import annotations

import argparse
import dataclasses
import os
import pathlib
import shutil
import sys
import time

_REPO = pathlib.Path(__file__).resolve().parents[1]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.git_context import git_output, main_checkout, merged_pr_heads  # noqa: E402

#: A worktree created this recently is never SAFE: an agent that has just bootstrapped one has no
#: commits or edits yet, so it reads as merged + clean until its first change.
FRESH_SECONDS = 24 * 3600

PRIMARY, DEAD, LOCKED, OTHER, FRESH, IN_USE, UNMERGED, STASHED, JUNK, DIRTY, SAFE = (
    "primary", "dead", "locked", "other", "fresh", "in use", "unmerged", "stashed", "junk",
    "dirty", "safe")
#: Report order: what needs a person first, what the tool handles last.
BUCKET_ORDER = (IN_USE, DIRTY, JUNK, STASHED, UNMERGED, FRESH, LOCKED, OTHER, PRIMARY, DEAD, SAFE)


@dataclasses.dataclass
class Facts:
    """Everything `classify` needs about one worktree. `pids is None` = couldn't look (no /proc)."""
    path: str
    branch: str | None = None
    head: str | None = None
    primary: bool = False
    exists: bool = True
    locked: bool = False
    managed: bool = True             # lives under the primary checkout's parent directory
    age_seconds: float = 1e9
    in_main: bool = False            # HEAD is an ancestor of origin/main
    merged_heads: list[str] | None = dataclasses.field(default_factory=list)  # None = gh failed
    status: list[str] = dataclasses.field(default_factory=list)              # porcelain lines
    tracked_ts: set[str] = dataclasses.field(default_factory=set)            # for the junk rule
    pids: list[tuple[int, str]] | None = dataclasses.field(default_factory=list)
    stashes: int = 0


def is_junk(rel: str, tracked_ts: set[str]) -> bool:
    """Compile output under `frontend/` that a stray `vue-tsc`/`tsc` emit leaves next to its source."""
    if not rel.startswith("frontend/"):
        return False
    if rel.endswith((".vue.js", ".vue.d.ts")):
        return True
    for ext in (".d.ts", ".js"):
        if rel.endswith(ext):
            return rel[: -len(ext)] + ".ts" in tracked_ts
    return False


def _short(cmd: str) -> str:
    """`/long/path/to/julia --project app.jl` → `julia --project app.jl`, cut to 40 characters."""
    exe, _, rest = cmd.partition(" ")
    return f"{os.path.basename(exe)} {rest}".strip()[:40]


def classify(f: Facts) -> tuple[str, str]:
    """(bucket, one-line reason). First match wins; SAFE only when nothing else applies."""
    if f.primary:
        return PRIMARY, "the main checkout"
    if not f.exists:
        return DEAD, "folder is gone"
    if f.locked:
        return LOCKED, "git worktree lock"
    if not f.managed:
        return OTHER, "outside the worktree folder"
    if f.age_seconds < FRESH_SECONDS:
        later, why = classify(dataclasses.replace(f, age_seconds=FRESH_SECONDS))
        return FRESH, f"created {f.age_seconds / 3600:.1f} h ago; otherwise {later}: {why}"
    if f.pids is None:
        return IN_USE, "can't check for processes on this OS"
    if f.pids:
        return IN_USE, ", ".join(f"{pid} {_short(cmd)}" for pid, cmd in f.pids[:3]) + \
            (f" (+{len(f.pids) - 3})" if len(f.pids) > 3 else "")
    if not f.in_main:
        if f.merged_heads is None:
            return UNMERGED, "not in origin/main; PR state unknown (gh failed)"
        if f.head not in f.merged_heads:
            why = "commits after the merged PR" if f.merged_heads else "not in origin/main, no merged PR"
            return UNMERGED, why
    if f.stashes:
        return STASHED, f"{f.stashes} stash entr{'y' if f.stashes == 1 else 'ies'}"
    if f.status:
        untracked = [l[3:] for l in f.status if l.startswith("?? ")]
        if len(untracked) == len(f.status) and all(is_junk(p, f.tracked_ts) for p in untracked):
            return JUNK, f"{len(untracked)} compile-output files"
        return DIRTY, f"{len(f.status)} uncommitted change{'s' if len(f.status) != 1 else ''}"
    return SAFE, "merged, clean, unused"


# ── fact gathering ──────────────────────────────────────────────────────────────────────────

def parse_worktree_list(text: str) -> list[dict]:
    """`git worktree list --porcelain` → one dict per entry (`worktree`, `HEAD`, `branch`, flags).

    `worktree` is normalised to OS-native separators: git prints `D:/a/x` on Windows, which would
    otherwise never compare equal to a `pathlib` path."""
    out = []
    for block in text.split("\n\n"):
        entry: dict = {}
        for line in block.splitlines():
            key, _, val = line.partition(" ")
            entry[key] = val
        if "worktree" in entry:
            entry["worktree"] = os.path.normpath(entry["worktree"])
            out.append(entry)
    return out


def scan_processes(own_pid: int | None = None) -> list[tuple[int, str, list[str]]] | None:
    """(pid, command, paths it touches) for every readable process, or None where there is no /proc."""
    proc = pathlib.Path("/proc")
    if not proc.is_dir():
        return None
    own_pid = os.getpid() if own_pid is None else own_pid
    out = []
    for d in proc.iterdir():
        if not d.name.isdigit() or int(d.name) == own_pid:
            continue
        paths = []
        for link in ("cwd", "exe"):
            try:
                paths.append(os.readlink(d / link))
            except OSError:
                pass
        try:
            argv = (d / "cmdline").read_bytes().split(b"\0")
        except OSError:
            argv = []
        args = [a.decode("utf-8", "replace") for a in argv if a]
        if paths or args:
            out.append((int(d.name), " ".join(args) or "?", paths + args))
    return out


def _under(path: str, root: str) -> bool:
    path, root = os.path.normcase(path), os.path.normcase(root).rstrip(os.sep)
    return path == root or path.startswith(root + os.sep)


def pids_under(root: str, procs: list[tuple[int, str, list[str]]] | None) -> list[tuple[int, str]] | None:
    if procs is None:
        return None
    return [(pid, cmd) for pid, cmd, paths in procs
            if any(_under(p, root) or root + os.sep in p for p in paths)]


def stash_counts(primary: str) -> dict[str, int]:
    """Stash entries per branch, from the `WIP on <branch>:` / `On <branch>:` subject git writes."""
    counts: dict[str, int] = {}
    for subject in (git_output("stash", "list", "--format=%gs", cwd=primary) or "").splitlines():
        for prefix in ("WIP on ", "On "):
            if subject.startswith(prefix):
                branch = subject[len(prefix):].split(":", 1)[0]
                counts[branch] = counts.get(branch, 0) + 1
                break
    return counts


def gather(entry: dict, primary: str, procs, stashes: dict[str, int], now: float) -> Facts:
    path = entry["worktree"]
    branch = entry.get("branch", "").removeprefix("refs/heads/") or None
    f = Facts(path=path, branch=branch, head=entry.get("HEAD"), primary=os.path.normcase(path) == os.path.normcase(primary),
              locked="locked" in entry, exists=pathlib.Path(path).is_dir(),
              managed=_under(path, str(pathlib.Path(primary).parent)))
    if f.primary or not f.exists or f.locked or not f.managed:
        return f
    git_file = pathlib.Path(path) / ".git"
    f.age_seconds = now - git_file.stat().st_mtime if git_file.exists() else 1e9
    f.pids = pids_under(path, procs)
    f.in_main = git_output("merge-base", "--is-ancestor", f.head or "HEAD", "origin/main", cwd=primary) is not None
    if not f.in_main:
        f.merged_heads = [] if branch is None else (
            None if (prs := merged_pr_heads(branch, cwd=primary)) is None
            else [p.get("headRefOid") for p in prs])
    f.stashes = stashes.get(branch, 0) if branch else 0
    status = git_output("status", "--porcelain", "--untracked-files=all", cwd=path)
    f.status = [l for l in (status or "").splitlines() if l.strip()] if status is not None else ["?? (git status failed)"]
    if any(l.startswith("?? frontend/") for l in f.status):
        f.tracked_ts = set((git_output("ls-files", "frontend", cwd=path) or "").splitlines())
    return f


def survey(primary: str) -> list[tuple[Facts, str, str]]:
    entries = parse_worktree_list(git_output("worktree", "list", "--porcelain", cwd=primary) or "")
    procs, stashes, now = scan_processes(), stash_counts(primary), time.time()
    return [(f, *classify(f)) for f in (gather(e, primary, procs, stashes, now) for e in entries)]


# ── report + removal ────────────────────────────────────────────────────────────────────────

def _name(f: Facts, primary: str) -> str:
    parent = str(pathlib.Path(primary).parent)
    return os.path.relpath(f.path, parent) if f.managed else f.path


def report(rows: list[tuple[Facts, str, str]], primary: str) -> str:
    lines = []
    for bucket in BUCKET_ORDER:
        group = [(f, why) for f, b, why in rows if b == bucket]
        if not group:
            continue
        lines.append(f"\n{bucket.upper()} ({len(group)})")
        if bucket == DEAD:
            lines.append("  git worktree prune  — entries whose folder no longer exists")
            continue
        for f, why in group:
            lines.append(f"  {_name(f, primary):<44} {(f.branch or '(detached)')[:36]:<36}  {why}")
            if bucket == JUNK:
                lines.append(f"    ! git worktree remove --force {f.path}")
    return "\n".join(lines)


def disk_free(path: str) -> str:
    u = shutil.disk_usage(path)
    return f"{u.free / 1e9:.0f} GB free ({100 * u.used / u.total:.0f}% used)"


def remove(rows, primary: str) -> list[str]:
    """Remove SAFE worktrees (re-checked first) and prune DEAD entries. Returns one line per action."""
    log = []
    if any(b == DEAD for _, b, _ in rows):
        ok = git_output("worktree", "prune", cwd=primary) is not None
        log.append(f"pruned {sum(b == DEAD for _, b, _ in rows)} dead entries" if ok else "git worktree prune failed")
    for f, bucket, _ in rows:
        if bucket != SAFE:
            continue
        entry = next((e for e in parse_worktree_list(git_output("worktree", "list", "--porcelain", cwd=primary) or "")
                      if e["worktree"] == f.path), None)
        now_bucket, why = classify(gather(entry, primary, scan_processes(), stash_counts(primary), time.time())) \
            if entry else (DEAD, "gone")
        if now_bucket != SAFE:
            log.append(f"skipped {f.path}: now {now_bucket} ({why})")
            continue
        if git_output("worktree", "remove", f.path, cwd=primary) is None:
            log.append(f"FAILED  git worktree remove {f.path}")
            continue
        note = ""
        if f.branch:
            note = (" + branch deleted" if git_output("branch", "-d", f.branch, cwd=primary) is not None
                    else f" (branch {f.branch} kept: git branch -d refused)")
        log.append(f"removed {f.path}{note}")
    return log


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--remove", action="store_true", help="remove SAFE worktrees and prune DEAD entries")
    ap.add_argument("--no-fetch", action="store_true", help="skip `git fetch --prune origin`")
    args = ap.parse_args(argv)
    primary_path = main_checkout(str(_REPO))
    if primary_path is None:
        print("not inside a git repository", file=sys.stderr)
        return 2
    primary = str(primary_path)
    if not args.no_fetch and git_output("fetch", "--prune", "--quiet", "origin", cwd=primary) is None:
        print("! git fetch failed — merged state is as of the last fetch", file=sys.stderr)
    before = disk_free(primary)
    rows = survey(primary)
    print(report(rows, primary))
    if args.remove:
        print()
        for line in remove(rows, primary):
            print(line)
        print(f"\ndisk: {before} → {disk_free(primary)}")
    else:
        n = sum(b in (SAFE, DEAD) for _, b, _ in rows)
        print(f"\ndisk: {before}.  {n} removable — rerun with --remove." if n else f"\ndisk: {before}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
