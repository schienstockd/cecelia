# Connecting Claude Code to Cecelia

Cecelia's in-app assistant (**Kiwi**'s prompt box) spawns your local **Claude Code** CLI. **There is no
MCP setup and no config to edit** — Cecelia generates the MCP config per run and hands it to `claude -p`.
You only need Claude Code itself installed and logged in. (The lab-log panel's old "Ask Claude" button
was removed 2026-09-24; asking in the app is Kiwi.)

## What you need (once)

1. **Install Claude Code** so `claude` is on your PATH.
   See <https://docs.anthropic.com/en/docs/claude-code/setup>.
2. **Log in from Kiwi**: the account row → **Log in**. Cecelia runs `claude auth login` for your
   Cecelia profile itself (a private browser window, with a paste-code link as fallback), so each
   profile bills its own account. A login made in a terminal is the terminal's, not your profile's.

## Two states you might hit

- **"Claude Code not detected"** — the CLI isn't installed / not on PATH → step 1 above.
- **"Not logged in"** on Kiwi's account row (or `Profile <name> is not logged in`) → step 2.

## Choosing the model

The picker in Kiwi's prompt box selects the model (Haiku / Sonnet / **Sonnet default** / Opus). A
machine-wide default can be set via `config.toml` `[ai] model`; the CLI binary via `[ai] agent_bin`.

## Chatting in your own terminal — one click

The above is all you need for the **in-app** feature. To have a full back-and-forth session about the
project in your own terminal, Cecelia sets Claude Code up for you:

1. Kiwi → **Chat** row → **Start in terminal**. It copies one command; paste it in a terminal. The
   command starts `claude` under your Cecelia profile's login (`kiwi_terminal_command`) with the chat
   starter as its first message, so the session opens already looking at the project you have open,
   and pairs on its first observer call. If the MCP entry is missing or stale, the button registers it
   first (the same action as Assistant → **Set up** / **Fix**).
2. Ask away. The server briefs the session itself (`mcp/cecelia_mcp/guidance.py`; see OBSERVER.md →
   *The hand-off is one line*), so there is nothing else to paste.

A plain `claude` works too once the entry is registered — `check my current project in cecelia` is
enough — but it runs under the terminal's own login, not your profile's. For a session that is already
running, the chat icon beside it (**Copy chat starter**) copies just the opener: `I'm working in the Cecelia project <name>
(<uid>). Have a look at it…`.

On Windows the command is `powershell -NoProfile -EncodedCommand …`: an encoded script reads the same
whether you paste it into PowerShell or cmd.exe.

There is nothing to copy and no path to type. The button POSTs `/api/observer/register`, which installs
the **Cecelia Claude Code plugin** (`mcp/claude-plugin/`) through Claude Code's own CLI:
`claude plugin marketplace add <install>/mcp/claude-plugin`, then
`claude plugin install cecelia@cecelia-local --config python=… --config api_url=… …`. **Every** later
`claude` session then has the observer tools. Cecelia must be running (`pixi run dev`) for those tools
to reach the Julia API.

**Why a plugin and not `claude mcp add-json`.** The plugin carries two things: the observer MCP server,
and a **caller hook** (`mcp/cecelia_mcp/caller_hook.py`) that tells the server which model is making
each call, so a Claude write records `model` and `session` in its authorship stamp. An MCP registration
can't carry a hook, and hooks live in your `settings.json`, which Cecelia never edits itself. The plugin
manifest is static; every machine-specific value (interpreter, `mcp/` dir, API URL, token-file path) is
a plugin option set at install. Inside a plugin the tools are named
`mcp__plugin_cecelia_cecelia-observer__<tool>`.

**How Cecelia knows.** `GET /api/observer/status` reads Claude Code's own files (`settings.json` for
`enabledPlugins` / `pluginConfigs` / `extraKnownMarketplaces`, `plugins/installed_plugins.json` for the
installed version; under the active Kiwi profile's dir, `~/.claude` for the default profile) → `terminal.state` of
`missing`, `stale`, `shadowed`, or `current`. It is a plain file read: the CLI's list commands
health-check every configured server, which would spawn our Python MCP process every time the panel
refreshes.

**`stale` flips the Terminal-row button to "Fix".** The plugin is installed but from another install's
folder, at an older plugin version, or with different options (a moved checkout, a changed port). It
still *looks* installed to Claude Code, but the tools can't reach this Cecelia — and it fails quietly.
An older **`claude mcp add-json` registration** also reads as stale: it gives the tools without the
caller hook, and beside the plugin it would list every tool twice. **Fix** installs the plugin first and
only then removes the old registration.

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
It runs only after the plugin is known good — clearing first and then failing to install would
leave you with nothing — and names each folder it cleaned in the panel. Claude Code diagnoses the same
conflict itself if you ever want to confirm by hand:

```
$ claude mcp list
cecelia-observer: … ✘ Failed to connect — ENOENT … posix_spawn '…/.pixi/envs/default/bin/python'
├ Server "cecelia-observer" is defined in multiple scopes with different endpoints: user (…), local (…)
└ Keep the correct endpoint and remove the others: `claude mcp remove cecelia-observer -s local`
```

**Safe to click again.** `claude plugin install` on an installed plugin is a no-op that keeps its old
options, so a re-sync is uninstall + install — click the button after moving the checkout, changing the
Python env, or changing the port and the options are refreshed. A marketplace registered at another
install's folder is removed and re-added at this one.

**About your Claude config.** `~/.claude.json` and `settings.json` are your main Claude Code config, so
Cecelia never edits them directly: every write is `claude plugin …` / `claude mcp remove …`, letting the
tool that owns the files do its own read-modify-write. Detection is a read. Further guards: an
already-current plugin is a **no-op** (nothing is rewritten); when a re-sync's install fails, the
previous options are **re-installed** (and the dialog says whether that worked) — so a failed click
can't leave you with less than you had; the old `add-json` registration is removed only **after** the
plugin is in; and the only MCP entries the button ever *deletes* are `cecelia-observer` ones that would
shadow it, each reported by folder. `claude plugin uninstall cecelia@cecelia-local` and
`claude plugin marketplace remove cecelia-local` undo the whole thing.

**Kiwi's own turns** don't use the plugin: they pass `--mcp-config` + `--strict-mcp-config` (which
ignores plugin servers) and load the same caller hook with `--settings`
(`<config_dir>/observer-hook-settings.json`), so their writes record the model too.

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
