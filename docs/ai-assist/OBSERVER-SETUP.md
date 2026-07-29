# Connecting Claude Code to the in-app observer

The in-app observer ("Ask Claude" / "Watch" in the lab-log panel) spawns your local **Claude Code**
CLI. **There is no MCP setup and no config to edit** — Cecelia generates the MCP config per run and
hands it to `claude -p`. You only need Claude Code itself installed and logged in.

## What you need (once)

1. **Install Claude Code** so `claude` is on your PATH.
   See <https://docs.anthropic.com/en/docs/claude-code/setup>.
2. **Log in**: run `claude` once in a terminal and complete the sign-in.

That's it. Reopen the lab-log panel — "Ask Claude" and "Watch" are enabled.

## Two states you might hit

The panel checks only that `claude` is **on PATH**, not that it's authenticated, so:

- **"Claude Code not detected"** — the CLI isn't installed / not on PATH → step 1 above.
- **"Claude Code isn't logged in"** — installed but a run failed to authenticate → step 2 (`claude`
  once to log in). This shows after a failed pass; the failure is also recorded in the panel's
  **Claude activity** log.

## Choosing the model

The picker next to "Ask Claude" selects the model (Haiku / Sonnet / **Sonnet default** / Opus). Opus
is overkill for the observer's work; Haiku is the cheapest for frequent Watch passes. A machine-wide
default can be set via `config.toml` `[ai] model`; the CLI binary via `[ai] agent_bin`.

## Chatting in your own terminal — one click

The above is all you need for the **in-app** feature. To have a full back-and-forth session about the
project in your own terminal, Cecelia sets Claude Code up for you:

1. Lab-log panel → **Set up my terminal**. (This button sits where *Chat to Claude* will be — until
   your terminal is set up, that IS the next step, so it isn't tucked away in the info dialog.)
2. Run `claude` in a terminal.
3. The toolbar button is now **Chat to Claude** — it copies a starter prompt; paste it in.

There is nothing to copy and no path to type. The button POSTs `/api/observer/register`, which runs
`claude mcp add-json cecelia-observer <spec> -s user` with the same server spec the in-app agent uses,
so **every** later `claude` session has the observer tools. Cecelia must be running (`pixi run dev`)
for those tools to reach the Julia API.

**How Cecelia knows.** `GET /api/observer/status` reads Claude Code's own config (`~/.claude.json`, or
`$CLAUDE_CONFIG_DIR/.claude.json`) and compares the registered `cecelia-observer` entry against what
this install needs → `terminal.state` of `missing`, `stale`, or `current`. It is a plain file read: the
`claude mcp get`/`list` commands health-check every configured server, which would spawn our Python MCP
process every time the panel refreshes. Only user-scope registrations count — per-directory (`local`)
ones would work in one folder only.

**`stale` shows "Fix terminal setup".** An entry left behind by an older checkout (different `.pixi`
interpreter) or a different port still *looks* registered to Claude Code, but the tools can't reach this
Cecelia — and it fails quietly. That's treated as not-set-up rather than offering a Chat button that
appears to work.

**Safe to click again.** `add-json` refuses a name that already exists, so re-syncing removes first —
click the button after moving the checkout, changing the Python env, or changing the port and the
registered paths are refreshed.

**About your Claude config.** `~/.claude.json` is your main Claude Code config (project history, caches,
auth state), so Cecelia never edits it directly: every write is `claude mcp …`, letting the tool that
owns the file do its own read-modify-write on the one `cecelia-observer` key. Detection is a read.
Three further guards: an already-correct entry is a **no-op** (nothing is rewritten); a first-time setup
runs **no** `remove` at all; and when a re-sync does remove an old entry and the replacement fails, the
old entry is **restored** (and the dialog says whether that worked) — so a failed click can't leave you
with less than you had. `claude mcp remove cecelia-observer -s user` undoes the whole thing.

**If it fails** the dialog falls back to showing the generated config's real path:

```bash
claude --mcp-config <config_dir>/observer-mcp.json   # ~/.cecelia/… installed, $CECELIA_DEV_DIR/… in dev
```

which loads the server for that one session. The dialog only ever prints the *resolved* path — copy it
from there rather than typing the placeholder above.
