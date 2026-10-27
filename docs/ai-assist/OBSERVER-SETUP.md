# Connecting Claude Code to Cecelia

Cecelia's in-app assistant (**Kiwi**'s prompt box) spawns your local **Claude Code** CLI. **There is no
MCP setup and no config to edit** — Cecelia generates the MCP config per run and hands it to `claude -p`.
You only need Claude Code itself installed and logged in. (The lab-log panel's old "Ask Claude" button
was removed 2026-09-24; asking in the app is Kiwi.)

## What you need (once)

1. **Install Claude Code** so `claude` is on your PATH.
   See <https://docs.anthropic.com/en/docs/claude-code/setup>.
2. **Log in**: run `claude` once in a terminal and complete the sign-in.

## Two states you might hit

The app checks only that `claude` is **on PATH**, not that it's authenticated, so:

- **"Claude Code not detected"** — the CLI isn't installed / not on PATH → step 1 above.
- **"Claude Code isn't logged in"** — installed but a Kiwi turn failed to authenticate → step 2
  (`claude` once to log in). Settings → MCP connections shows it after such a failure.

## Choosing the model

The picker in Kiwi's prompt box selects the model (Haiku / Sonnet / **Sonnet default** / Opus). A
machine-wide default can be set via `config.toml` `[ai] model`; the CLI binary via `[ai] agent_bin`.

## Chatting in your own terminal — one click

The above is all you need for the **in-app** feature. To have a full back-and-forth session about the
project in your own terminal, Cecelia sets Claude Code up for you:

1. Kiwi → Assistant → **Set up** (once). Same button becomes **Fix** if the registered entry drifts
   (stale/shadowed). The action lives next to the Terminal state indicator it acts on.
2. Run `claude` in a terminal.
3. Ask away — `check my current project in cecelia` is enough. The server briefs the session itself
   (`mcp/cecelia_mcp/guidance.py`; see OBSERVER.md → *The hand-off is one line*), so there is no prompt
   to paste and nothing to remember.

Chat handoff lives on Kiwi too (**Chat** row → **Copy chat starter**). It copies one line naming the
project you have open — `I'm working in the Cecelia project <name> (<uid>). Have a look at it…` — for
when you want *that* project rather than the one you opened most recently.

There is nothing to copy and no path to type. The button POSTs `/api/observer/register`, which runs
`claude mcp add-json cecelia-observer <spec> -s user` with the same server spec the in-app agent uses,
so **every** later `claude` session has the observer tools. Cecelia must be running (`pixi run dev`)
for those tools to reach the Julia API.

**How Cecelia knows.** `GET /api/observer/status` reads Claude Code's own config (`~/.claude.json`, or
`$CLAUDE_CONFIG_DIR/.claude.json`) and compares the registered `cecelia-observer` entry against what
this install needs → `terminal.state` of `missing`, `stale`, `shadowed`, or `current`. It is a plain file
read: the `claude mcp get`/`list` commands health-check every configured server, which would spawn our
Python MCP process every time the panel refreshes.

**`stale` flips the Terminal-row button to "Fix".** An entry left behind by an older checkout
(different `.pixi` interpreter) or a different port still *looks* registered to Claude Code, but the
tools can't reach this Cecelia — and it fails quietly. That's treated as not-set-up rather than
letting the row read as ready.

**`shadowed` shows it too — and this one masqueraded as a broken button.** We register at `user` scope,
but Claude Code resolves **`local` scope first** (`projects[<dir>].mcpServers` in the same file). A
leftover local entry therefore *overrides* a perfectly correct user-scope registration for every session
started in that directory. When it points at a checkout that no longer exists, the server never starts —
`ENOENT` on the interpreter — so `claude` comes up with no observer tools at all.

Reading only the top level made this invisible: status reported `current` and Kiwi read as ready
while every session was in fact broken. So the status route now also reads local scope
(`read_local_observer_specs`) and reports the offending folders as `terminal.shadowedDirs`. A local entry
that *matches* what we'd register is left alone — it resolves to the same server, so it breaks nothing.

Clicking **Fix** (Kiwi → Assistant → Terminal) clears the shadowing entries, each via
`claude mcp remove cecelia-observer -s local` spawned *in* that directory (the command acts on its cwd).
It runs only after the user-scope entry is known good — clearing first and then failing to add would
leave you with nothing — and names each folder it cleaned in the panel. Claude Code diagnoses the same
conflict itself if you ever want to confirm by hand:

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

## When your terminal session works on Cecelia itself

A `claude` session in your terminal can do more than talk about your project. It can read Cecelia's
code, fix an error you hit, or write a step Cecelia doesn't have. That's welcome. These rules make sure
the work survives the next update and stays checkable. The observer's instructions carry the short form
to every session (`mcp/cecelia_mcp/guidance.py`); this is the long form.

- **Need a step Cecelia doesn't have?** Write it as a [custom module](../CUSTOM_MODULES.md), or a
  plugin if others should run it. It then runs as a task like any other, with a log, QC, its parameters
  recorded on the image, and a place in the analysis lineage. Code run by hand next to the project gets
  none of that.
- **Never hand-edit Cecelia's own files or your project's data files.** That means the code in the install,
  `ccid.json`, `.h5ad` cell tables and the image or label stores. A hand edit skips the checks the app
  makes on every write: atomic writes, name collisions, versioning. If a task already does the job, use
  it; importing a label image made elsewhere is **Segment → Import labels** (`segment.importLabels`).
  If none does, that's a custom module or a report.
- **Fixed a bug locally?** Keep working, but the next update overwrites the patch. Send it to Dominik as a
  report with the patch attached ([`USER_REPORTS.md`](../USER_REPORTS.md) is how reports are handled).
  The cause is verified on `main` before anything ships, and the fix then reaches everyone.
- **Just looking at your data, or making a figure?** Read it through Cecelia's accessors (`init_object`,
  `pop_df`, `label_props`) or ask for a notebook (Notebooks page). Nothing goes back into the project
  except through a task.

Why this exists: a session that patched code, registered label stores by hand and drew figures outside
the app produced every one of those gaps as a request, and each became a feature. The same work done
as a module or a report would have reached everyone sooner, without a hand-made state to undo.
