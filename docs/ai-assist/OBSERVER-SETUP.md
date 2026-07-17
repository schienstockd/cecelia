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

## Using the observer tools from your own terminal (optional)

The above is all you need for the **in-app** feature. If you want to drive the same read-only
observer tools from an **interactive** `claude` session (outside Cecelia), that's the one case where
you register the MCP server yourself — see [`mcp/README.md`](../../mcp/README.md) → *Wire into Claude
Code*. (`pixi run dev` must be running so the server can reach the Julia API.)
