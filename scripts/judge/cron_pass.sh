#!/usr/bin/env bash
# Scheduled (systemd user timer, Monday 23:59) `pixi run judge-weekly` pass.
#
# Wraps the pass with (a) logging to `~/.cecelia-effectiveness/cron/`, (b) a `nice`/`ionice` bump so
# a mid-run pass doesn't fight interactive work, (c) the lock the overnight agent runs share
# (`scripts/agent_eval/cron_night.sh`), so the two never overlap. `--pinned` checks the commit this
# checkout sits on (the unit has just reset it to origin/main) instead of fetching again.
#
# Called by systemd/weekly-judge.service — see that dir's README. Fails loudly (exit non-zero) so
# `systemctl --user status weekly-judge.service` shows red. Design: docs/ai-assist/WEEKLY_JUDGE.md.

set -euo pipefail

TASK_ARGS=()
case "${1:-}" in
    --pinned) TASK_ARGS=(--ref HEAD) ;;
esac

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_DIR="${CECELIA_EVAL_CRON_LOG_DIR:-$HOME/.cecelia-effectiveness/cron}"
mkdir -p "$LOG_DIR"
TS="$(date -u +%Y%m%dT%H%M%SZ)"
LOG_FILE="$LOG_DIR/judge-$TS.log"

exec 200>"$LOG_DIR/.lock"
if ! flock -n 200; then
    echo "$(date -Is) another cron pass holds the lock; exiting" | tee -a "$LOG_FILE" >&2
    exit 0
fi

{
    echo "=== judge pass started $(date -Is) ==="
    echo "repo: $REPO_ROOT"
    echo "pixi: $(command -v pixi || echo '(not found)')"
    cd "$REPO_ROOT"
    nice -n 10 ionice -c 3 pixi run judge-weekly ${TASK_ARGS[@]+"${TASK_ARGS[@]}"}
    echo "=== judge pass finished $(date -Is) ==="
} 2>&1 | tee -a "$LOG_FILE"
