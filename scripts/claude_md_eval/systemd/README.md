# Scheduled CLAUDE.md compliance eval (Monday 23:59)

Systemd user timer that fires `pixi run claude-md-eval` every Monday at 23:59
local time ("Monday midnight" colloquially — chosen over `Mon 00:00` so that
installing the timer on a Monday afternoon triggers a fire that same night
rather than a week later). See [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../../../docs/todo/CLAUDE_MD_EVAL_PLAN.md)
→ *Cadence* for the design.

## Install (Linux + systemd)

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

## Pre-merge test (this branch)

Before PR #1274 lands, the wrapper + unit files live only on
`feat/indirect-eval-tier`, not on `cecelia-feijoa/main`. To fire the timer
from that branch tonight without waiting for the merge:

```bash
# Copy from the branch's checkout, not main.
cp ~/cc-workspace/cecelia/cecelia-indirect-eval/scripts/claude_md_eval/systemd/claude-md-eval.{service,timer} \
   ~/.config/systemd/user/

# Override REPO so ExecStart points at the branch worktree, not the main checkout.
systemctl --user edit claude-md-eval.service
# In the editor, add:
#   [Service]
#   Environment=REPO=/home/dominik/cc-workspace/cecelia/cecelia-indirect-eval

systemctl --user daemon-reload
systemctl --user enable --now claude-md-eval.timer
systemctl --user list-timers claude-md-eval.timer
# NEXT should be tonight 23:59 local.
```

Post-merge, remove the override with `systemctl --user revert claude-md-eval.service`
so the timer runs from the standard `cecelia-feijoa` checkout.

## What it does

- Runs `scripts/claude_md_eval/cron_pass.sh`, which is `pixi run claude-md-eval`
  under `nice -n 10 ionice -c 3` with a lockfile so overlapping fires can't
  double-spawn.
- Emits `_run` / `_suite` rows to `~/.cecelia-effectiveness/events.jsonl` and
  auto-renders `docs/ai-assist/CLAUDE_MD_EVAL.md` at the end of the pass.
- Leaves the regenerated rollup uncommitted — the user reviews the `git diff`
  when they want to inspect the trend, same as manual runs.

Won't fire on battery (`ConditionACPower=true` in the .service). Won't fire
retroactively on boot (`Persistent=` intentionally omitted from the timer)
— a machine that was off at 23:59 Monday will NOT trigger a pass when it
next powers on; that row silently drops from the trend. Deliberate: the
alternative surprised the user with a paid pass on next boot of an old
machine. Runs the suite only, not the ablation — ablation needs trace
inspection per D12 discipline, which cron can't do.

## Adjust for your setup

If your checkout lives somewhere other than `~/cc-workspace/cecelia/cecelia-feijoa`,
or if `pixi` / `claude` live outside the covered defaults (`~/.pixi/bin` for
`pixi`, `~/.local/bin` for `claude`):

```bash
systemctl --user edit claude-md-eval.service
# Then add:
#   [Service]
#   Environment=REPO=/path/to/your/checkout
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
