# Observer — live validation checklist

The MCP observer (Slices A–C, see [`OBSERVER.md`](OBSERVER.md)) is fully unit-tested but has **not**
been run end-to-end against a live Claude Code session. Everything can be unit-tested *except* the one
thing that matters most: Claude connecting over stdio, calling a tool, and getting a real response
from the running Julia server — and whether the pull-based "sit next to me" loop actually surfaces
useful signal in practice. This doc is that manual pass. Run it whenever the observer changes shape.

It has two jobs:
1. **Confirm the stack works end-to-end** — reads, the 10-attempts pattern, note/lab-log events, the
   no-mutation guarantee.
2. **Measure real token cost** so the Slice C throttle defaults (`surfaceCap`, per-observation token
   estimate in `mcp/cecelia_mcp/monitor.py`) can be calibrated against a number instead of a guess.

---

## Prerequisites

**A backend with Slice B's events on `:8080`.** Slice B added `fun` to the `task:status` frame plus the
`image_note_added` / `lab_log_entry_added` broadcasts. Any checkout at or ahead of the Slice B merge
(#178) has them — `main`, or a feature branch rebased on it. Slice C added **no** backend changes, so
running the backend from the Slice C worktree is fine.

- Backend: `pixi run dev` from a checkout with Slice B (the Slice C worktree works).
- Frontend: if the worktree has no `frontend/node_modules`, don't install them — run `pixi run frontend`
  from a checkout that does (it only proxies `/api` + `/ws` to `:8080`; the backend branch is what
  matters, not the frontend's). One backend only — don't start a second on `:8080`.

**The observer wired into a Claude session.** The MCP server just talks HTTP/WS to `:8080`, so it's
independent of which checkout's backend is running. Point it at the code you want to validate (use the
main env's Python; a worktree has no `.pixi`):

```
claude mcp add cecelia-observer \
  --env PYTHONPATH=<abs-path-to>/mcp \
  -- <abs-path-to>/.pixi/envs/default/bin/python -m cecelia_mcp.server
```

e.g. `PYTHONPATH=/home/dominik/cc-workspace/cecelia/mcp-observer-slice-c/mcp` and the pixi Python at
`/home/dominik/cc-workspace/cecelia/cecelia-pineapple/.pixi/envs/default/bin/python`. Override the API
with `CECELIA_API_URL` if not `http://127.0.0.1:8080`.

Then start a **fresh** `claude` session as the observer ("just sit next to me / watch what I'm doing")
with a project open in the app.

---

## Validation script

Against the test project (`NRUBxU` / set `jFWePN`, images `KDIeEm` / `CHWgkH` / `hVNx8o`), or any real
project you have open.

| # | In the observer session… | In the app… | Expect |
|---|---|---|---|
| a | "Describe project NRUBxU" | — | correct name / kind / image count / per-status breakdown — reads work end-to-end |
| b | "Read the task history, and the segment log for KDIeEm" | — | real history + log content |
| c | "poll for observations" | **re-run one task on KDIeEm 4×** (chain node or module page) | one `repeat_attempts` for that `(image, fn)`, `attempts: 4`, with `completed`/`failed` tally |
| d | "poll again" | **add a note** to CHWgkH | an `image_note_added` (`imageUid`, `note`) |
| e | "what are your observer stats?" | — | `surfacedCount`, `surfaceCap`, `throttled`, `estimatedTokens`, `enabled` |
| f | "turn the observer off" then "poll" | re-run a task again | `poll` returns nothing; counting continues (turn it back on → history intact) |
| g | "delete image X" / "run cellpose for me" | — | it declines / has no such tool — the no-mutation guarantee holds |

### What to record
- **Did each observation actually surface?** (c, d) — the core question.
- **Real token cost of the session** (from Claude Code's own usage readout) vs. the number of
  observations surfaced → this calibrates `surfaceCap` and `EST_TOKENS_PER_OBSERVATION`.
- **How often the throttle fires** in a realistic session → informs the open decision below.

### Pass criteria
- Reads (a, b) correct without being told where to look.
- `repeat_attempts` fires at the 4th run (c); `image_note_added` fires (d).
- Off switch silences surfacing but preserves counting (f).
- No mutation possible beyond the lab-log append (g).

---

## Open decision to settle from this run

**The throttle's silent lab-log flush** (`_flush_to_lab_log` in `server.py`). When `surfaceCap` is hit,
suppressed observations are appended to the lab log as a `[Claude]` block so nothing is lost. Concern:
it's an *unprompted* `[Claude]` write (vs. Claude deciding to log), it blurs provenance (mechanical
spillover looks like considered notes), and it can recreate the noise the throttle exists to suppress.
Options, to pick based on how often the throttle actually fires:

- **(a)** keep the full dump, `[Claude]` tag (current);
- **(b)** log a single summary line ("observer throttled — N further patterns suppressed") — *leaning*;
- **(c)** use a distinct tag (e.g. `[Observer]`) so it never reads as a Claude decision;
- **(d)** drop silently.

If the throttle rarely fires, this barely matters; if it fires constantly, (b)/(c) become clearly right.
