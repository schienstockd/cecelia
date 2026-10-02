#!/usr/bin/env bash
# Scheduled (systemd user timer, Monday 23:59) `pixi run claude-md-eval` pass.
#
# Wraps the suite driver with (a) logging to `~/.cecelia-effectiveness/cron/`,
# (b) a `nice`/`ionice` niceness bump so a mid-run pass doesn't fight interactive
# work, (c) a lockfile so overlapping timer fires don't double-spawn.
#
# Design: docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Cadence* / *P3 cron*.
#
# By default the pass is supervised (`claude-md-eval-supervise`: pinned origin/main, failures
# triaged, a run record written — docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md). `--no-supervise`
# runs the bare suite from this checkout, as before.
#
# Installed as a systemd user timer via `scripts/claude_md_eval/systemd/*` — see
# that dir's install instructions. Not run directly by the timer; the .service
# unit calls this script.
#
# Fails loudly (exit non-zero) on any error so `systemctl --user status
# claude-md-eval.service` shows a red bar the next time the user looks.

set -euo pipefail

# `--pinned` is how the timer calls it (Decision 16): this checkout IS the supervisor's worktree,
# which the unit's ExecStartPre has just reset to origin/main, so the pass pins HEAD rather than
# re-resolving origin/main and resetting the files this script and the supervisor run from.
TASK=claude-md-eval-supervise
TASK_ARGS=()
case "${1:-}" in
    --no-supervise) TASK=claude-md-eval ;;
    --pinned) TASK_ARGS=(--ref HEAD) ;;
esac

# Repo root — this script lives at `<repo>/scripts/claude_md_eval/cron_pass.sh`.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

LOG_DIR="${CECELIA_EVAL_CRON_LOG_DIR:-$HOME/.cecelia-effectiveness/cron}"
mkdir -p "$LOG_DIR"

# A checkout too old to have the supervisor would run something else under its name. Fail loudly.
if [ "$TASK" = claude-md-eval-supervise ] && [ ! -f "$SCRIPT_DIR/supervise.py" ]; then
    echo "$(date -Is) $REPO_ROOT has no scripts/claude_md_eval/supervise.py; refusing to run" \
        | tee -a "$LOG_DIR/eval-$(date -u +%Y%m%dT%H%M%SZ).log" >&2
    exit 1
fi

TS="$(date -u +%Y%m%dT%H%M%SZ)"
LOG_FILE="$LOG_DIR/eval-$TS.log"

LOCK="$LOG_DIR/.lock"
exec 200>"$LOCK"
if ! flock -n 200; then
    echo "$(date -Is) another claude-md-eval cron pass is already running; exiting" \
        | tee -a "$LOG_FILE" >&2
    exit 0
fi

# Runs `pixi run $TASK` at the default N=3. Ablation stays manual — it
# needs trace inspection per D12 discipline, which cron can't do.
{
    echo "=== cron pass started $(date -Is) ==="
    echo "repo: $REPO_ROOT"
    echo "pixi: $(command -v pixi || echo '(not found)')"
    cd "$REPO_ROOT"
    echo "task: $TASK"
    nice -n 10 ionice -c 3 pixi run "$TASK" ${TASK_ARGS[@]+"${TASK_ARGS[@]}"}
    echo "=== cron pass finished $(date -Is) ==="
} 2>&1 | tee -a "$LOG_FILE"
