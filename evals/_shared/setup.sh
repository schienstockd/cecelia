#!/bin/bash
# Shared scaffold — every eval case symlinks its own setup.sh → this file. Populates the
# sandbox with a fresh checkout of the source repo so the agent can grep/read the same
# code + docs a normal `claude -p` invocation in the source worktree sees.
#
# Env vars read from the plugin-eval sandbox (only the `CLAUDE_CODE_*` prefix passes
# through — D11 of docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md):
#   CLAUDE_CODE_EVAL_SOURCE_REPO   — path to source repo (default: walk up from BASH_SOURCE)
#   CLAUDE_CODE_EVAL_CLAUDE_MD_ARM — `with` (default) or `without` for the ablation swap
set -euo pipefail

# Resolve source repo from the scaffold script's own on-disk location — plugin-eval's
# sandbox strips arbitrary shell env vars (only CLAUDE_CODE_*/ANTHROPIC_*/etc pass
# through), so an EVAL_SOURCE_REPO env var doesn't reach us here. Env var override is
# still supported for out-of-tree eval runs.
_this_script="$(readlink -f "${BASH_SOURCE[0]}")"
_plugin_root="$(cd "$(dirname "$_this_script")/../.." && pwd)"
SRC="${CLAUDE_CODE_EVAL_SOURCE_REPO:-$_plugin_root}"
ARM="${CLAUDE_CODE_EVAL_CLAUDE_MD_ARM:-with}"

# Clone via tmp subdir because `git clone` refuses to target a non-empty cwd (plugin-eval
# may have staged its own files in the sandbox root before the scaffold fires).
# `file://` forces a real (non-hardlinked) clone that honours --depth=1 — drops the
# .git payload from ~65MB (full history) to ~few MB (shallow). Local `--local` clones
# hardlink the object DB and silently ignore --depth (git warns about it).
git clone --depth=1 --quiet "file://$SRC" .eval-src
shopt -s dotglob
mv .eval-src/* .
rmdir .eval-src
shopt -u dotglob

# Ablation: strip every CLAUDE.md (root + nested frontend/app) for the `without` arm.
# D8 in docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md — binary swap, no per-section stripping.
if [ "$ARM" = "without" ]; then
  find . -name 'CLAUDE.md' -not -path './.git/*' -delete
fi

# Fresh commit so `git diff HEAD` after the agent runs captures exactly the agent's changes.
git config user.email "eval@cecelia.local"
git config user.name "eval-scaffold"
git add -A
git commit -q --allow-empty -m "eval scaffold (arm=$ARM)"

echo "scaffold: source=$SRC arm=$ARM files=$(git ls-files | wc -l)" >&2
