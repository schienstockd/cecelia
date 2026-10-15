> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome:** done on branch `agent-icons-archive` (2026-10-15), not by this brief's design.
> Three premises didn't match the code: the colour never encoded state (it was a hash of the
> session id into 4 shapes × 7 Okabe-Ito colours, the state column's own hues, so it looked like
> state); it was already truecolor; and the hash was there so `scripts/statusline_badge.py`, a
> separate process per window, draws the same mark. What shipped: a plain `●` in one of 7
> CVD-safe hues (Paul Tol's 'bright'), told from the state colours by column, also on each finding's branch
> row (`● c4`, from #1692); the lowest free slot per live session, saved in
> `~/.cache/cecelia-agents/badge_slots.json`, which the pane and the statusline both read. The
> console's positive colour (pass, done, fixed, resolved) moved from green to sky blue, away from
> red, and the `invt`/`mlnt`/`cite` labels left sky blue for grey. Not done: no 256-colour fallback (the console is truecolor throughout), no LRU or style
> axis past 7 (colours wrap), no collapsing of done rows.

# Task: fix agent icons in the agents console table

## Problem

The agents table in the terminal console shows a colored shape next to each agent name (`●`, `◆`, `■`). It doesn't work:

- Icons repeat a lot with many agents (three orange in one screenshot).
- Shapes are too small in a terminal to tell apart.
- Color currently seems to encode state (orange = idle, blue = done). That duplicates the state column and can't also identify agents.
- Not a priority feature, but it's visually distracting.

## Goal

The icon should be pure **identity**: help the eye separate agents in the list at a glance. State is already in its own column, so it must not drive the icon color.

## Step 1: investigate (don't edit yet)

1. Find where the agents table and its icon/color logic are rendered.
2. Report: how icon shape and color are currently chosen, whether ANSI 16 names or fixed 256/truecolor values are used, and how agents are spawned and pruned (so slots can be freed).
3. Check how many agents can be live at once in practice.

## Step 2: implement

- **Color the agent ID itself** (e.g. `cecelia-c4`) in the agent's assigned color. Drop the shape; if a marker stays, use a plain `●`.
- **Fixed palette of 8-10 clearly distinct hues** using 256-color or truecolor values, not ANSI names (those shift with the terminal theme). Avoid near-neighbors (orange/yellow, blue/purple).
- **Sequential assignment, not hashing.** On spawn, take the first palette slot not used by a live agent. Hashing guarantees collisions.
- **Free the slot** when the agent is pruned so colors recycle.
- **Overflow (more agents than palette slots):** reuse the least-recently-used slot so adjacent rows never match. Optionally add a second axis via text style (normal / bold / underline), not shapes.
- **Many agents (10s):** sort by recency and collapse idle/done agents into a summary line (e.g. `+14 done`) rather than trying to make every icon unique.
- Do not touch the state column.

## Constraints

- Keep it small. This is a cosmetic fix; no new abstractions beyond a palette list and a slot allocator.
- Dev terminal is Ubuntu GNOME Terminal (truecolor supported), but keep a 256-color fallback.
- Add a test for slot allocation (first-free, recycle on prune, LRU on overflow) if the codebase has a test pattern for this module.

## Deliverable

A short summary of what you found, what you changed, and anything you chose not to do.
