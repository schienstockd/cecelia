# Weekly judge timer (Tuesday 23:59)

Systemd user timer that runs the weekly judge (`pixi run judge-weekly`) every Tuesday at 23:59 local
time. What the pass does: [`docs/ai-assist/WEEKLY_JUDGE.md`](../../../docs/ai-assist/WEEKLY_JUDGE.md).

## Install (Linux + systemd)

The timer runs from the pass's own worktree, `~/.cecelia-effectiveness/judge-worktree`, never from a
dev checkout. The unit resets that worktree to `origin/main` before each pass. It has to exist first,
so run one pass by hand from any up-to-date checkout:

```bash
pixi run judge-weekly -- --no-pr   # creates the worktree + its .pixi; spends real API $
```

Then install the units:

```bash
mkdir -p ~/.config/systemd/user
cp scripts/judge/systemd/weekly-judge.{service,timer} ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now weekly-judge.timer
loginctl enable-linger "$USER"   # fire when you're not logged in
```

Coming from the CLAUDE.md eval timer, remove it first — its script is gone, so it would fail weekly:

```bash
systemctl --user disable --now claude-md-eval.timer
rm ~/.config/systemd/user/claude-md-eval.{service,timer}
systemctl --user daemon-reload
```

Verify:

```bash
systemctl --user list-timers weekly-judge.timer
systemctl --user start weekly-judge.service       # a pass now (spends real API $)
journalctl --user -u weekly-judge.service -f
```

Won't fire on battery (`ConditionACPower=true`), and won't fire on boot for a missed Tuesday
(`Persistent=` omitted): the next pass sweeps from the last record, so nothing is lost.

## Issues are the default

The pass files bugs as GitHub issues, comments on a pinned *Judge status* issue, and opens a PR only
for rule proposals ([`WEEKLY_JUDGE.md`](../../../docs/ai-assist/WEEKLY_JUDGE.md) step 7). An
`Environment=JUDGE_ISSUES=1` drop-in from before it was the default is harmless. To go back to a
record PR each week, set the switch off in your local unit (not the repo's copy):

```bash
systemctl --user edit weekly-judge.service   # add the two lines below
#   [Service]
#   Environment=JUDGE_ISSUES=0
```

`pixi run judge-issues` prints what the mirror would do for the newest record (reads only);
`-- --apply` does it now, without waiting for the next pass.

**The usage limit.** A pass that hits it exits 75. `cron_pass.sh` sleeps until 5 min after the reset
the CLI names and reruns the pass (3 attempts at most, a reset at most 8h off), holding the cron lock,
so that night's agent run is skipped. That is why `TimeoutStartSec` is 12h. Each wait is in the cron
log: `usage limit, lifts …: waiting …s, then attempt 2/3`. Details:
[`WEEKLY_JUDGE.md`](../../../docs/ai-assist/WEEKLY_JUDGE.md) → *Waiting out the limit*.

## Adjust, uninstall

`systemctl --user edit weekly-judge.service` to override `REPO` or `PATH`;
`systemctl --user edit weekly-judge.timer` to override `OnCalendar=` (`systemd.time(7)`).

```bash
systemctl --user disable --now weekly-judge.timer
rm ~/.config/systemd/user/weekly-judge.{service,timer}
systemctl --user daemon-reload
```

macOS: wire `scripts/judge/cron_pass.sh` into a `launchd` plist yourself. Windows: run
`pixi run judge-weekly` by hand or from Task Scheduler via WSL.
