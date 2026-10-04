#!/usr/bin/env bash
# One app-tier agent run (scripts/agent_eval/run_app.py) from cron: brings this checkout to
# origin/main (fixes merged since the last run take effect), then runs the agent against the RUNNING
# app on a fresh copy of the run images. Records land in $CECELIA_AGENT_APP_ROOT/<stamp>/ (record.json,
# trace.jsonl, decisions.json) and as a blackboard entry in the source project (the reviewable result);
# the copy stays in the projects dir until deleted.
#
# crontab:  30 0 * * *  $HOME/cc-workspace/cecelia/cecelia-agent-night/scripts/agent_eval/cron_app.sh
# Skips (exit 0) when the app is not up or another run holds the lock.

set -euo pipefail
export PATH="$HOME/.pixi/bin:$HOME/.local/bin:$HOME/.juliaup/bin:/usr/local/bin:/usr/bin:/bin"

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ROOT="${CECELIA_AGENT_APP_ROOT:-/tmp/cecelia-agent-app}"
PROJECTS="${CECELIA_AGENT_APP_PROJECTS:-$HOME/cecelia-feijoa/projects}"
SOURCE="${CECELIA_AGENT_APP_SOURCE:-tSJpBI}"
# the run images: tSJpBI's "Crop" set, mouse M1. M2's crops (jV6p8M 8F20qd QWhG6x) are held out
# (docs/todo/AGENT_RUN_REVIEW_PLAN.md Decision 1).
IMAGES="${CECELIA_AGENT_APP_IMAGES:-yDfwP7 UJS0Hz dvFmih 3vBHp8}"
SOURCE_SET="${CECELIA_AGENT_APP_SOURCE_SET:-k58SK7}"
# 1 = carry the source project's lab-knowledge entries into the copy (AGENT_RUN_REVIEW_PLAN P4)
KNOWLEDGE="${CECELIA_AGENT_APP_KNOWLEDGE:-0}"
BUDGET="${CECELIA_AGENT_APP_BUDGET:-15}"
API="${CECELIA_API_URL:-http://127.0.0.1:8080}"

mkdir -p "$ROOT"
STAMP="$(date +%Y%m%dT%H%M%S)"
LOG="$ROOT/cron-$STAMP.log"

exec 200>"$ROOT/.lock"
if ! flock -n 200; then
    echo "$(date -Is) another agent run holds the lock; skipping" >>"$LOG"
    exit 0
fi

{
    echo "=== agent app run $(date -Is) — $SOURCE: $IMAGES, knowledge $KNOWLEDGE, budget \$$BUDGET ==="
    if ! curl -sf -m 5 "$API/api/tasks" >/dev/null; then
        echo "the app is not answering at $API; skipping"
        exit 0
    fi
    cd "$REPO"
    git fetch -q origin main && git checkout -q --detach origin/main
    git log -1 --format='code: %h %s'
    .pixi/envs/default/bin/python3 scripts/agent_eval/run_app.py --projects-dir "$PROJECTS" \
        --source-project "$SOURCE" --source-set "$SOURCE_SET" $(printf -- '--image %s ' $IMAGES) \
        $([ "$KNOWLEDGE" = 1 ] && echo --knowledge) --root "$ROOT/$STAMP" --budget-usd "$BUDGET"
    echo "=== finished $(date -Is) ==="
} >>"$LOG" 2>&1
