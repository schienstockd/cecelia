# Cecelia MCP observer server (Phase 1 — read-only)

A [Model Context Protocol](https://modelcontextprotocol.io) server that gives Claude **read-only**
access to a running Cecelia project — project state, images, task logs, QC, and the lab log — plus a
single **append-only** write to the lab log. This is Phase 1 ("Observer") of the arc in
[`docs/ai-assist/OBSERVER.md`](../docs/ai-assist/OBSERVER.md); the write phases (`submit_task`,
`adjust_params`, `acknowledge_flag`) are deliberately **not** wired here.

It's a **standalone stdio process** (Python + [FastMCP](https://github.com/modelcontextprotocol/python-sdk))
that talks to the Julia API over HTTP. It is separate infra, not part of the `cecelia` Python package
(one language per top-level dir).

## Layout

```
mcp/
  cecelia_mcp/
    client.py    # read-only HTTP client + the ALLOW-LIST (stdlib only; the no-mutation guarantee)
    monitor.py   # pure session monitor: 10-attempts pattern + WS frame → observation (no I/O)
    wsclient.py  # thin WS listener that feeds the monitor from ws://…/ws
    server.py    # FastMCP server — wires the client into 8 read tools + poll_observations + append_lab_log
  tests/
    test_client.py    # stdlib unittest, HTTP mocked
    test_monitor.py   # the 10-attempts pattern + frame normalization (pure, no socket)
```

## Tools

| Tool | Backing route | Returns |
|---|---|---|
| `get_project_info(project_uid)` | `GET /api/images` | name, kind, image count, sets, per-status breakdown |
| `list_images(project_uid)` | `GET /api/images` | every image: uid, name, status, set |
| `get_image_info(project_uid, image_uid)` | `GET /api/images/meta` | channels, dims, physical sizes, labels, QC, run log, note |
| `get_image_notes(project_uid, image_uid)` | `GET /api/images/meta` | the user's note for the image |
| `get_qc_metrics(project_uid, image_uid)` | `GET /api/images/meta` | per-image QC flags/metrics |
| `get_task_log(project_uid, image_uid, fun)` | `GET /api/images/tasklog` | raw log text for one task fn on one image |
| `get_task_history(project_uid, limit=100)` | `GET /api/tasks/history` | recent runs across all images, newest first |
| `get_cohort_qc(project_uid, set_uid, fun_name, value_name=None)` | `GET /api/qc/cohort` | per-set mean/SD + z-scored outliers over a task's banked metric; no `value_name` → `byValueName` map |
| `get_analysis_lineage(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/lineage` | synthesized pipeline: per-image `steps` + seg/track/cluster/gating links, project chains/boards, roll-up |
| `get_populations(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/populations` | per-image population definitions: tree + gate geometry / filter rule (defs only; counts are the measure slice) |
| `get_measure_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/measures` | per-population phenotype (intensities+morphology) + motility summaries (median/quantiles/mean/n); gated pops else base |
| `get_behaviour_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/behaviour` | per-image HMM state distribution (fraction per state) + transition counts |
| `get_cluster_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/clusters` | per clustering run: n clusters, sizes, largest fraction, feature list (cell=clustPops, track=clustTracks) |
| `read_lab_log(project_uid)` | `GET /api/lablog` | the full lab-log markdown |
| `get_recent_logs(level="", limit=100)` | `GET /api/logs/recent` | recent backend console lines (server `@info`/`@warn`/`@error`) — where a Julia-side task crash lands (not in `get_task_log`) |
| `poll_observations(project_uid)` | *(in-process, WS-fed)* | `{observations, stats}` since the last poll — the "sit next to me" signal (see below) |
| `set_observer_active(active)` | *(in-process)* | the off switch — stop/resume surfacing; counting continues while off |
| `get_observer_stats()` | *(in-process)* | session throttle/cost state without draining (surfaced count, cap, throttled, token estimate) |
| `append_lab_log(project_uid, lines)` | `POST /api/lablog/append` | **the only write** — appends a dated `[Claude]` entry, append-only |

## Live observation — the 10-attempts pattern (Slice B)

Beyond the on-demand reads, the server keeps a **session monitor** fed by the API's WebSocket event
stream (`ws://…/ws`, best-effort — reconnects on its own, never blocks the read tools). It watches for
patterns the user is too close to notice and exposes them through **`poll_observations`** — a *pull*
tool Claude calls periodically while watching a project (MCP is client-pull; unsolicited server→client
push is a later slice). Each poll returns `{observations, stats}`, where `observations` is a
(usually empty) list of:

- **`repeat_attempts`** — the same function has run **>3 times on one image this session**
  (`imageUid`, `fn`, `attempts`, `completed`/`failed` tallies, `lastOutcome`). The core signal.
  Counting is session-scoped and **launch-path-agnostic**: whiteboard chain nodes and module-page
  single tasks land in the same tally (both terminal outcomes are counted once each).
- **`image_note_added`** — the user added a note to an image (`imageUid`, `note`).
- **`lab_log_entry_added`** — a user (non-`[Claude]`/`[Cecelia]`) lab-log entry appeared (`summary`).

The monitor is pure and unit-tested (`tests/test_monitor.py`); the WS listener only decodes frames and
feeds it. The connection is receive-only — no mutation path is added.

### Throttle, token cost & off switch (Slice C)

To bound token cost (the doc's honest caveat: heavy sessions can be ~10× the ~10-20-event estimate),
the monitor caps how many observations it surfaces per session (`surfaceCap`, default 20). Once the
cap is hit it **goes quiet**: `poll_observations` returns no new `observations`, and the suppressed
patterns are appended to the **lab log** silently instead (a single compact `[Claude]` block) — so
nothing is lost, but no chat tokens are spent narrating them. `stats` (also via `get_observer_stats`)
reports `surfacedCount`, `surfaceCap`, `throttled`, `enabled`, and an `estimatedTokens` running gauge
(surfaced × ~2.5k — an estimate; the server can't see Claude's real usage). **`set_observer_active(false)`**
is the off switch: surfacing stops but attempt counting keeps running, so re-enabling resumes with full
history.

## The no-mutation guarantee

Every request goes through `ALLOWED_ROUTES` in `client.py`. The only non-GET route on it is
`POST /api/lablog/append` (append-only, itself server-guarded). A call to any other route raises
`DisallowedRoute` — so if a future tool ever wires in a mutating route, the test suite fails loudly
rather than a project being silently mutated. `test_client.py` asserts append is the sole write.

## Running it

1. Start the Cecelia backend: `pixi run dev` (serves the API on `:8080`).
2. Start this server: `pixi run mcp` (stdio). Override the API with `CECELIA_API_URL`.

### Wire into Claude Code

Add to your Claude Code MCP config (adjust the absolute path to this checkout):

```json
{
  "mcpServers": {
    "cecelia-observer": {
      "command": "pixi",
      "args": ["run", "mcp"],
      "cwd": "/home/dominik/cc-workspace/cecelia/cecelia-pineapple"
    }
  }
}
```

Then, with a project open, ask e.g. *"describe my project"*, *"what failed on image 7?"*, or
*"note in the lab log that I switched cellpose diameter to 30 because the nuclei were undersegmented."*

## Tests

`pixi run test-mcp` — stdlib `unittest`, HTTP mocked, no server required.
