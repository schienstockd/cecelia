"""git `pre-push` hook (`.githooks/pre-push`): refuse to push to a branch whose PR already merged or closed.

GitHub deletes a branch when its PR merges. A later push to the same name brings it back, carrying
commits no PR shows and nobody reviews — the main way stray branches pile up on origin
(docs/todo/WORKTREE_CLEANUP_PLAN.md decision 15). The fix is a fresh branch off origin/main and a new PR, so the hook says that.

Lets the push through when the branch has an open PR, no PR yet, when the push deletes the branch,
when it re-pushes the exact head the PR ended at, or when `gh` can't answer (offline is not a
reason to block). `git push --no-verify` skips it.
"""
from __future__ import annotations

import pathlib
import sys
from typing import Callable

_REPO = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.git_context import merged_pr_heads  # noqa: E402


def refusals(stdin_lines: list[str], prs_for: Callable[[str], list[dict] | None]) -> list[str]:
    """One message per pushed branch that must not be pushed. `stdin_lines` is what git hands a
    pre-push hook: `<local ref> <local sha> <remote ref> <remote sha>` per ref."""
    out = []
    for line in stdin_lines:
        parts = line.split()
        if len(parts) != 4:
            continue
        _, local_sha, remote_ref, _ = parts
        if not local_sha.strip("0") or not remote_ref.startswith("refs/heads/"):
            continue    # a delete, or a tag
        branch = remote_ref.removeprefix("refs/heads/")
        prs = prs_for(branch)
        if not prs or any(p.get("state") == "OPEN" for p in prs):
            continue
        done = [p for p in prs if p.get("state") in ("MERGED", "CLOSED")]
        if not done or any(p.get("headRefOid") == local_sha for p in done):
            continue
        p = done[0]
        out.append(f"pre-push: PR #{p['number']} from `{branch}` is already {p['state'].lower()} — this "
                   "push would bring the branch back with commits no PR shows.\n"
                   "  Branch fresh off origin/main (`pixi run bootstrap-worktree <name>`, or "
                   "`git switch -c <new> origin/main` + cherry-pick) and open a new PR.\n"
                   "  Reopened the PR on purpose? Reopen it on GitHub first, then push.")
    return out


def main() -> int:
    msgs = refusals(sys.stdin.read().splitlines(),
                    lambda b: merged_pr_heads(b, cwd=str(_REPO), state="all"))
    for m in msgs:
        print(m, file=sys.stderr)
    return 1 if msgs else 0


if __name__ == "__main__":
    sys.exit(main())
