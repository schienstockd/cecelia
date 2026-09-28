# Scheduled CLAUDE.md compliance eval (Wednesday midnight)

Systemd user timer that fires `pixi run claude-md-eval` every Wednesday at 00:00
local time. See [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../../../docs/todo/CLAUDE_MD_EVAL_PLAN.md)
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
# → NEXT column shows the next Wednesday 00:00.

# Dry-run the pass without waiting for Wednesday:
systemctl --user start claude-md-eval.service
journalctl --user -u claude-md-eval.service -f
```

## What it does

- Runs `scripts/claude_md_eval/cron_pass.sh`, which is `pixi run claude-md-eval`
  under `nice -n 10 ionice -c 3` with a lockfile so overlapping fires can't
  double-spawn.
- Emits `_run` / `_suite` rows to `~/.cecelia-effectiveness/events.jsonl` and
  auto-renders `docs/ai-assist/CLAUDE_MD_EVAL.md` at the end of the pass.
- Leaves the regenerated rollup uncommitted — the user reviews the `git diff`
  when they want to inspect the trend, same as manual runs.

Won't fire on battery (`ConditionACPower=true` in the .service). Runs the
suite only, not the ablation — ablation needs trace inspection per D12
discipline, which cron can't do.

## Adjust for your setup

If your checkout lives somewhere other than `~/cc-workspace/cecelia/cecelia-feijoa`,
or if `pixi` / `claude` live outside `~/.local/bin`:

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
