#!/usr/bin/env bash
# Scheduled (systemd user timer, Tuesday 23:59) `pixi run judge-weekly` pass.
#
# Wraps the pass with (a) logging to `~/.cecelia-effectiveness/cron/`, (b) a `nice`/`ionice` bump so
# a mid-run pass doesn't fight interactive work, (c) the lock the overnight agent runs share
# (`scripts/agent_eval/cron_night.sh`), so the two never overlap. `--pinned` checks the commit this
# checkout sits on (the unit has just reset it to origin/main) instead of fetching again.
#
# (d) The usage limit. A pass that hits it exits 75 (`EX_TEMPFAIL`) and writes `judge-ratelimit.json`
# with the reset time and whether to `retry` (weekly.py decides: attempts left, reset within 8h).
# This waits until 5 min after the reset and reruns the whole pass, at most MAX_ATTEMPTS times. The
# lock stays held through the wait: an overnight agent run would hit the same limit. A rerun pins
# the same commit: `--ref HEAD` is this checkout, and a pass that stops on the limit never commits.
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
# beside events.jsonl, like weekly.py's `ratelimit_path()`
STATE_DIR="$(dirname "${CECELIA_EFFECTIVENESS_LOG:-$HOME/.cecelia-effectiveness/events.jsonl}")"
LIMIT_FILE="$STATE_DIR/judge-ratelimit.json"
MAX_ATTEMPTS=3
EX_TEMPFAIL=75
AFTER_RESET_SEC=300
SLEEP="${CECELIA_JUDGE_SLEEP:-sleep}"   # tests stand `true` in
mkdir -p "$LOG_DIR"
TS="$(date -u +%Y%m%dT%H%M%SZ)"
LOG_FILE="$LOG_DIR/judge-$TS.log"

log() { echo "$(date -Is) $*" | tee -a "$LOG_FILE" >&2; }

run_pass() {   # $1 = attempt; returns the pass's exit code
    local attempt=$1
    {
        echo "=== judge pass started $(date -Is) (attempt $attempt/$MAX_ATTEMPTS) ==="
        echo "repo: $REPO_ROOT"
        echo "pixi: $(command -v pixi || echo '(not found)')"
        cd "$REPO_ROOT"
        rc=0
        JUDGE_RETRY_LEFT=$((MAX_ATTEMPTS - attempt)) \
            nice -n 10 ionice -c 3 pixi run judge-weekly ${TASK_ARGS[@]+"${TASK_ARGS[@]}"} || rc=$?
        echo "=== judge pass finished $(date -Is) (exit $rc) ==="
        exit "$rc"
    } 2>&1 | tee -a "$LOG_FILE"
    return "${PIPESTATUS[0]}"
}

main() {
    exec 200>"$LOG_DIR/.lock"
    if ! flock -n 200; then
        log "another cron pass holds the lock; exiting"
        exit 0
    fi
    local attempt=1 rc retry reset reset_s wait_s
    while :; do
        rm -f "$LIMIT_FILE"
        rc=0
        run_pass "$attempt" || rc=$?
        if [ "$rc" -ne "$EX_TEMPFAIL" ] || [ ! -f "$LIMIT_FILE" ]; then
            return "$rc"
        fi
        read -r retry reset < <(python3 -c 'import json, sys
d = json.load(open(sys.argv[1], encoding="utf-8"))
print("1" if d.get("retry") else "0", d.get("reset") or "")' "$LIMIT_FILE") || retry=0
        if [ "$retry" != 1 ] || [ "$attempt" -ge "$MAX_ATTEMPTS" ]; then
            log "usage limit, lifts ${reset:-?}: no retry (attempt $attempt/$MAX_ATTEMPTS); the failure is on the status issue (or a FAILED PR)"
            return "$rc"
        fi
        reset_s=$(date -d "$reset" +%s 2>/dev/null) || reset_s=$(( $(date +%s) + 3600 ))   # unreadable: an hour
        wait_s=$(( reset_s + AFTER_RESET_SEC - $(date +%s) ))
        [ "$wait_s" -lt 0 ] && wait_s=0
        log "usage limit, lifts $reset: waiting ${wait_s}s, then attempt $((attempt + 1))/$MAX_ATTEMPTS (lock held)"
        "$SLEEP" "$wait_s"
        attempt=$((attempt + 1))
    done
}

# Everything runs from here, so bash has read the whole file before the pass resets this checkout.
main "$@"
