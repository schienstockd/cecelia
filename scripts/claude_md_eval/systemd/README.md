# Scheduled CLAUDE.md compliance eval (Monday 23:59)

Systemd user timer that fires `pixi run claude-md-eval` every Monday at 23:59
local time ("Monday midnight" colloquially — chosen over `Mon 00:00` so that
installing the timer on a Monday afternoon triggers a fire that same night
rather than a week later). See [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../../../docs/todo/CLAUDE_MD_EVAL_PLAN.md)
→ *Cadence* for the design.

## Install (Linux + systemd)

The timer runs from the supervisor's own worktree, `~/.cecelia-effectiveness/eval-worktree`, never
from a dev checkout: a dev checkout falls behind `main` and carries edits, so the timer would run
whatever driver it happened to hold. The unit resets that worktree to `origin/main` before each pass.
It has to exist first, so run one supervised pass by hand from any up-to-date checkout:

```bash
pixi run claude-md-eval-supervise -- --no-pr   # creates the worktree + its .pixi; spends real API $
```

Then install the units:

```bash
mkdir -p ~/.config/systemd/user
cp scripts/claude_md_eval/systemd/claude-md-eval.{service,timer} ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now claude-md-eval.timer

# Enable lingering so the timer fires when the user isn't logged in.
loginctl enable-linger "$USER"
```

Verify:

```bash
systemctl --user list-timers claude-md-eval.timer
# → NEXT column shows the next Monday 23:59 local.

# Dry-run the pass without waiting (spends real API $):
systemctl --user start claude-md-eval.service
journalctl --user -u claude-md-eval.service -f
```

## What it does

- `ExecStartPre` fetches and resets the worktree to `origin/main`, then runs its
  `scripts/claude_md_eval/cron_pass.sh --pinned`: `pixi run claude-md-eval-supervise --ref HEAD`
  under `nice -n 10 ionice -c 3`, with a lockfile so overlapping fires can't double-spawn. It
  refuses to run if the checkout has no `supervise.py`.
- The supervisor runs the suite, triages failures, writes the run record to
  `~/.cecelia-effectiveness/eval-runs/<date>.json` and opens one `eval-run/<date>` PR
  (`docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md`).

Won't fire on battery (`ConditionACPower=true` in the .service). Won't fire
retroactively on boot (`Persistent=` intentionally omitted from the timer)
— a machine that was off at 23:59 Monday will NOT trigger a pass when it
next powers on; that row silently drops from the trend. Deliberate: the
alternative surprised the user with a paid pass on next boot of an old
machine. Runs the suite only, not the ablation — ablation needs trace
inspection per D12 discipline, which cron can't do.

## Adjust for your setup

If `pixi` / `claude` live outside the covered defaults (`~/.pixi/bin` for
`pixi`, `~/.local/bin` for `claude`), or the eval store is not `~/.cecelia-effectiveness`:

```bash
systemctl --user edit claude-md-eval.service
# Then add:
#   [Service]
#   Environment=REPO=/path/to/the/eval-worktree
#   Environment=PATH=/wherever/pixi/lives:/usr/local/bin:/usr/bin:/bin
```

To pick a different weekday or time, `systemctl --user edit claude-md-eval.timer`
and override `OnCalendar=`. Format: `systemd.time(7)` — `Wed 00:00`,
`Mon,Wed,Fri *-*-* 03:00`, etc.

## Uninstall

```bash
systemctl --user disable --now claude-md-eval.timer
rm ~/.config/systemd/user/claude-md-eval.{service,timer}
systemctl --user daemon-reload
```

## Not Linux?

- **macOS** — the equivalent is a `launchd` plist. Not shipped here; the wrapper
  script (`cron_pass.sh`) works fine, wire it into launchd yourself.
- **Windows** — Task Scheduler + `wsl.exe pixi run claude-md-eval`, or run the
  eval manually. The wrapper script assumes bash / GNU coreutils.
