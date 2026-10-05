#!/usr/bin/env bash
# Nightly agent run (docs/todo/AGENT_OVERNIGHT_PLAN.md → P4): one vague + one guided brief on a fresh
# synthetic fixture, each a sandboxed `claude -p` capped by --budget-usd. Records land in
# ~/.cecelia-effectiveness/agent-runs/<stamp>-<brief>.{json,md}; the run roots under
# $CECELIA_AGENT_NIGHT_ROOT (default /tmp/cecelia-agent-night — outside ~, which the sandbox
# write-denies) are pruned after 7 days.
#
# Shares the weekly judge's lock, so it never overlaps the Tuesday pass (a busy lock = skip, exit 0).
# Called by systemd/agent-eval-night.service; fails loudly so `systemctl --user status` shows red.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUDGET="${CECELIA_AGENT_NIGHT_BUDGET:-5}"
BRIEFS="${CECELIA_AGENT_NIGHT_BRIEFS:-vague guided}"
RUN_ROOT="${CECELIA_AGENT_NIGHT_ROOT:-/tmp/cecelia-agent-night}"
EXTRA_ARGS="${CECELIA_AGENT_NIGHT_ARGS:-}"      # e.g. --scripted-ceiling to test the wiring at $0

LOG_DIR="${CECELIA_EVAL_CRON_LOG_DIR:-$HOME/.cecelia-effectiveness/cron}"
# beside the effectiveness log, like judge-runs/ (python/cecelia/effectiveness/judge_staleness.py)
EFF_DIR="$(dirname "${CECELIA_EFFECTIVENESS_LOG:-$HOME/.cecelia-effectiveness/events.jsonl}")"
STORE="${CECELIA_AGENT_NIGHT_STORE:-$EFF_DIR/agent-runs}"
mkdir -p "$LOG_DIR" "$STORE" "$RUN_ROOT"
TS="$(date -u +%Y%m%dT%H%M%SZ)"
LOG_FILE="$LOG_DIR/agent-night-$TS.log"

exec 200>"$LOG_DIR/.lock"
if ! flock -n 200; then
    echo "$(date -Is) the cron lock is held (a judge pass is running); skipping tonight" | tee -a "$LOG_FILE" >&2
    exit 0
fi

{
    echo "=== agent night started $(date -Is) — briefs: $BRIEFS, budget \$$BUDGET each ==="
    cd "$REPO_ROOT"
    find "$RUN_ROOT" -mindepth 1 -maxdepth 1 -type d -mtime +7 -exec rm -rf {} + 2>/dev/null || true
    for brief in $BRIEFS; do
        root="$RUN_ROOT/$TS-$brief"
        echo "--- $brief → $root"
        nice -n 10 ionice -c 3 pixi run agent-eval-run --root "$root" --brief "$brief" --budget-usd "$BUDGET" $EXTRA_ARGS
        cp "$root/record.json" "$STORE/$TS-$brief.json"
        cp "$root/record.md" "$STORE/$TS-$brief.md"
    done
    echo "=== agent night finished $(date -Is) ==="
} 2>&1 | tee -a "$LOG_FILE"
