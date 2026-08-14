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
3. Ask away — `check my current project in cecelia` is enough. The server briefs the session itself
   (`mcp/cecelia_mcp/guidance.py`; see OBSERVER.md → *The hand-off is one line*), so there is no prompt
   to paste and nothing to remember.

The toolbar button is now **Chat to Claude**. It copies one line naming the project you have open —
`I'm working in the Cecelia project <name> (<uid>). Have a look at it…` — for when you want *that*
project rather than the one you opened most recently.

There is nothing to copy and no path to type. The button POSTs `/api/observer/register`, which runs
`claude mcp add-json cecelia-observer <spec> -s user` with the same server spec the in-app agent uses,
so **every** later `claude` session has the observer tools. Cecelia must be running (`pixi run dev`)
for those tools to reach the Julia API.

**How Cecelia knows.** `GET /api/observer/status` reads Claude Code's own config (`~/.claude.json`, or
`$CLAUDE_CONFIG_DIR/.claude.json`) and compares the registered `cecelia-observer` entry against what
this install needs → `terminal.state` of `missing`, `stale`, `shadowed`, or `current`. It is a plain file
read: the `claude mcp get`/`list` commands health-check every configured server, which would spawn our
Python MCP process every time the panel refreshes.

**`stale` shows "Fix terminal setup".** An entry left behind by an older checkout (different `.pixi`
interpreter) or a different port still *looks* registered to Claude Code, but the tools can't reach this
Cecelia — and it fails quietly. That's treated as not-set-up rather than offering a Chat button that
appears to work.

**`shadowed` shows it too — and this one masqueraded as a broken button.** We register at `user` scope,
but Claude Code resolves **`local` scope first** (`projects[<dir>].mcpServers` in the same file). A
leftover local entry therefore *overrides* a perfectly correct user-scope registration for every session
started in that directory. When it points at a checkout that no longer exists, the server never starts —
`ENOENT` on the interpreter — so `claude` comes up with no observer tools at all.

Reading only the top level made this invisible: status reported `current` and the toolbar offered *Chat
to Claude* while every session was in fact broken. So the status route now also reads local scope
(`read_local_observer_specs`) and reports the offending folders as `terminal.shadowedDirs`. A local entry
that *matches* what we'd register is left alone — it resolves to the same server, so it breaks nothing.

Clicking **Fix terminal setup** clears the shadowing entries, each via `claude mcp remove
cecelia-observer -s local` spawned *in* that directory (the command acts on its cwd). It runs only after
the user-scope entry is known good — clearing first and then failing to add would leave you with
nothing — and names each folder it cleaned in the panel. Claude Code diagnoses the same conflict itself
if you ever want to confirm by hand:

```
$ claude mcp list
cecelia-observer: … ✘ Failed to connect — ENOENT … posix_spawn '…/.pixi/envs/default/bin/python'
├ Server "cecelia-observer" is defined in multiple scopes with different endpoints: user (…), local (…)
└ Keep the correct endpoint and remove the others: `claude mcp remove cecelia-observer -s local`
```

**Safe to click again.** `add-json` refuses a name that already exists, so re-syncing removes first —
click the button after moving the checkout, changing the Python env, or changing the port and the
registered paths are refreshed.

**About your Claude config.** `~/.claude.json` is your main Claude Code config (project history, caches,
auth state), so Cecelia never edits it directly: every write is `claude mcp …`, letting the tool that
owns the file do its own read-modify-write on the one `cecelia-observer` key. Detection is a read.
Four further guards: an already-correct entry is a **no-op** (nothing is rewritten); a first-time setup
runs **no** `remove` at all; when a re-sync does remove an old entry and the replacement fails, the
old entry is **restored** (and the dialog says whether that worked) — so a failed click can't leave you
with less than you had; and the only entries the button ever *deletes* are `cecelia-observer` ones that
would shadow it (never a matching one, never another server), each reported by folder.
`claude mcp remove cecelia-observer -s user` undoes the whole thing.

**If it fails** the dialog falls back to showing the generated config's real path:

```bash
claude --mcp-config <config_dir>/observer-mcp.json   # ~/.cecelia/… installed, $CECELIA_DEV_DIR/… in dev
```

which loads the server for that one session. The dialog only ever prints the *resolved* path — copy it
from there rather than typing the placeholder above.
