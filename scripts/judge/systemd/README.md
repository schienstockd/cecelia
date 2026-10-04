# Weekly judge timer (Monday 23:59)

Systemd user timer that runs the weekly judge (`pixi run judge-weekly`) every Monday at 23:59 local
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

Won't fire on battery (`ConditionACPower=true`), and won't fire on boot for a missed Monday
(`Persistent=` omitted): the next pass sweeps from the last record, so nothing is lost.

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
