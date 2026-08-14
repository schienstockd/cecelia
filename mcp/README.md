# Cecelia MCP observer server

A [Model Context Protocol](https://modelcontextprotocol.io) server that gives Claude **read** access
to a running Cecelia project — project state, images, task logs, QC, lineage, populations, measures,
and the lab log — plus a small set of **additive** writes: lab-log entries, Pluto notebooks, and a
whiteboard chain template. See *The no-mutation guarantee* below for the exact list and why each one
is recoverable.

Claude can **design** work but never **start** it: there is no `submit_task` and no way to launch a
chain run. That is the arc in [`docs/ai-assist/OBSERVER.md`](../docs/ai-assist/OBSERVER.md) — Phase 1
(observe) and Phase 2 (actionable assist) have shipped; `submit_task` / `adjust_params` /
`acknowledge_flag` remain deliberately **not** wired.

It's a **standalone stdio process** (Python + [FastMCP](https://github.com/modelcontextprotocol/python-sdk))
that talks to the Julia API over HTTP. It is separate infra, not part of the `cecelia` Python package
(one language per top-level dir).

## Layout

```
mcp/
  cecelia_mcp/
    client.py    # read-only HTTP client + the ALLOW-LIST (stdlib only; the no-mutation guarantee)
    guidance.py  # what the server TELLS a session about its own toolset (see below)
    monitor.py   # pure session monitor: 10-attempts pattern + WS frame → observation (no I/O)
    wsclient.py  # thin WS listener that feeds the monitor from ws://…/ws
    server.py    # FastMCP server — wires the client into the read tools + poll_observations + the additive writes
  tests/
    test_client.py    # stdlib unittest, HTTP mocked
    test_monitor.py   # the 10-attempts pattern + frame normalization (pure, no socket)
    test_server.py    # tool registration + the guidance guard (every tool is named; instructions stay small)
    test_guarantees.py # a promise in the prose must name the test that backs it (see below)
```

## The server briefs the session — `guidance.py`

`check my current project in cecelia` is a sufficient prompt. There is nothing to paste, because the
server carries its own instructions:

- **`SERVER_INSTRUCTIONS`** → `FastMCP(instructions=…)`, delivered in the `initialize` response and
  landing in the client's system prompt. It only has to get the assistant to the front door: resolve
  the project with `list_projects` (most-recently-opened first), then call `get_session_briefing`.
  **Keep it short** — the observer is registered user-scope, so this is in context for every `claude`
  session on the machine, Cecelia-related or not. `test_server.py` holds the budget.
- **`BRIEFING_GUIDANCE`** → merged into `get_session_briefing`'s response as `guidance`. The long form:
  the grouping discipline before any cross-image figure, the add-only rules for boards, the
  designs-but-never-runs rule for chains, how to open. Costs nothing until a session opens a project.

Per-tool detail belongs in the tool's own docstring (also always in context); `guidance.py` is only
for what spans tools. **A new tool must be named there** or the assistant never offers it — enforced by
`GuidanceTest` in `mcp/tests/test_server.py`, with a three-tool exemption for the observer's own
autonomous-loop bookkeeping. The in-app observer has its own prompt (`app/src/ai/observer_prompt.jl`)
carrying only the watch loop and the lab-log discipline, with the matching guard in `app/test/suite.jl`.

### A promise must name the test that backs it

This prose is Python; the behaviour it describes is Julia. Nothing tied the two, so a promise could be
false for months — and one was: both this docstring and the guidance claimed the server "rejects …
rather than writing a board that renders blank" while the expander happily wrote one that did.
`tests/test_guarantees.py` holds the list of server guarantees the prose makes, each with the test that
proves it, and fails on a new unlisted one. Two rules keep it honest:

1. **State only guarantees the assistant ACTS on.** "Add-only, cannot rename or delete" changes what it
   tells the user; "the server refuses to write a blank board" changed nothing — it submits and either
   gets a 422 or doesn't. That sentence was deleted rather than tested. A guarantee that only reassures
   can be wrong, and being right buys nothing.
2. **Say what to DO, not what the server promises.** "A 422 names what was available — read it and
   resubmit" is worth more than any assurance, and cannot rot.

## Tools

| Tool | Backing route | Returns |
|---|---|---|
| `get_project_info(project_uid)` | `GET /api/images` | name, kind, image count, sets, per-status breakdown |
| `list_images(project_uid)` | `GET /api/images` | every image: uid, name, status, set |
| `get_image_info(project_uid, image_uid)` | `GET /api/images/meta` | channels, dims, physical sizes, labels, QC, run log, note |
| `get_image_notes(project_uid, image_uid)` | `GET /api/images/meta` | the user's note for the image |
| `get_qc_metrics(project_uid, image_uid)` | `GET /api/images/meta` | per-image QC flags/metrics |
| `get_task_log(project_uid, image_uid, fun)` | `GET /api/images/tasklog` | raw log text for one task fn on one image |
| `get_task_history(project_uid, limit=100)` | `GET /api/tasks/history` | recent runs across all images, newest first; each row carries the run's `params` (tuning trail) for parameter suggestions |
| `get_module_params(category="")` | `GET /api/tasks/definitions` | task param specs (valid ranges/defaults/types) for parameter suggestions; pass the category (fun_name prefix). Project-independent |
| `get_available_plots(module="")` | `GET /api/plots/definitions` | plot types the board can render (chart types, data needs, scope modes) — to suggest a visualization. Project-independent |
| `get_cohort_qc(project_uid, set_uid, fun_name, value_name=None)` | `GET /api/qc/cohort` | per-set mean/SD + z-scored outliers over a task's banked metric; no `value_name` → `byValueName` map |
| `get_analysis_lineage(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/lineage` | synthesized pipeline: per-image `steps` + seg/track/cluster/gating links, project chains/boards, roll-up |
| `get_populations(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/populations` | per-image population definitions: tree + gate geometry / filter rule (defs only; counts are the measure slice) |
| `get_measure_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/measures` | per-population phenotype (intensities+morphology) + motility summaries (median/quantiles/mean/n); gated pops else base |
| `get_behaviour_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/behaviour` | per-image HMM state distribution (fraction per state) + transition counts |
| `get_cluster_summary(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/clusters` | per clustering run: n clusters, sizes, largest fraction, feature list (cell=clustPops, track=clustTracks) |
| `get_spatial_stats(project_uid, image_uid="", set_uid="")` | `GET /api/analysis/spatial` | region-clustering runs (regions.{suffix}: n regions, sizes) + pairwise cell-type contact log-odds (association/avoidance per population pair) |
| `get_chains(project_uid)` | `GET /api/analysis/chains` | whiteboard chains: wired templates (node DAG + task fns) + recent runs with node-outcome roll-ups |
| `get_repl_api()` | `GET /api/repl/api` | notebook/REPL data-access surface: the read accessors + their live docstrings + the `docs/REPL.md` cookbook (write rules). Read before generating `using Cecelia` code. Project-independent |
| `get_session_briefing(project_uid)` | `GET /api/observer/briefing` | session startup context: project name + image count + `excludedCount`, flagged images (warn/fail QC — each with `included`, each finding with its `fun`), recent lab-log entries (7 days), and the `guidance`. Call first when a chat begins |
| `read_lab_log(project_uid)` | `GET /api/lablog` | the full lab-log markdown |
| `get_recent_logs(level="", limit=100)` | `GET /api/logs/recent` | recent backend console lines (server `@info`/`@warn`/`@error`) — where a Julia-side task crash lands (not in `get_task_log`) |
| `poll_observations(project_uid)` | *(in-process, WS-fed)* | `{observations, stats}` since the last poll — the "sit next to me" signal (see below) |
| `set_observer_active(active)` | *(in-process)* | the off switch — stop/resume surfacing; counting continues while off |
| `get_observer_stats()` | *(in-process)* | session throttle/cost state without draining (surfaced count, cap, throttled, token estimate) |
| `append_lab_log(project_uid, lines)` | `POST /api/lablog/append` | **write 1/2** — appends a dated `[Claude]` entry, append-only |
| `create_notebook(project_uid, name, cells, description="")` | `POST /api/notebooks/write` | **write 2/2** — serialises Julia `cells` into a runnable Pluto notebook (env-activation cell prepended, snapshot v1). Create-only (409 on an existing name); the user then edits/owns it in Pluto |
| `add_analysis_board(project_uid, name, plots, template="", compare_by="")` | `POST /api/boards/add` | **write 6/6** — ADDS one Analysis board (a figure on `/analysis`). Add-only: cannot modify, rename, reorder or delete a board, so it lands beside the user's own and is one click to delete. `compare_by` is what it compares across images — `"per_image"`, `"summarised"`, or an image ATTRIBUTE name (`"Mouse"`, or `"Treatment,Mouse"`); omitting it gives a single-image board, not a cross-image figure. 409 on a duplicate name; server-**validated** against the project (unknown plot id / chart the spec doesn't offer / measure it doesn't carry / population that doesn't exist / a `popType` that cannot reach the named populations / an attribute the project lacks → 422 naming what was available), because a bad `tkey` renders an EMPTY panel with no error. NOT `/api/projects/boards`, the browser's whole-document autosave |
| `create_chain(project_uid, name, nodes, edges, start_targets=None)` | `POST /api/chains/create` | **write 5/6** — authors a whiteboard chain TEMPLATE (the wired task DAG). Create-only (409) + server-**validated** (unknown task / dangling edge / cycle / out-of-range param → 400 naming the offender). Params may be sparse. **There is no tool to run it** — the user launches it from the whiteboard |

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

Every request goes through `ALLOWED_ROUTES` in `client.py`. The non-GET routes on it are
`POST /api/lablog/append` (append-only), `POST /api/notebooks/{write,describe,revise}` (create-only /
description-text-only / snapshot-then-overwrite) and `POST /api/chains/create` (create-only +
validated). All are **additive / non-destructive**: no allow-listed route can edit or delete project
data (no h5ad, gates, ccid.json, or an existing notebook's content or chain). A call to any other
route raises `DisallowedRoute` — so if a future tool ever wires in a mutating route, the test suite
fails loudly rather than a project being silently mutated. `test_client.py` asserts the exact write
set, and changing that list is the deliberate gate on widening what Claude can do.

### Claude designs; the user runs

There is no `run_chain` / `submit_task` tool, and adding one is not a small decision — a chain run's
nodes replace the store/h5ad for their `value_name`, so it would be the first action here that can
destroy results. Today that boundary is enforced by the **transport**, not by a rule: launching a
chain is the WebSocket message `chain:run` (`api/src/sockets.jl`) with no HTTP route at all, and this
client speaks only HTTP. `test_client.py` and `test_server.py` pin both halves (no launch route
reachable, no launch tool registered).

Two related routes are also deliberately absent: `/api/chains/save` (an unguarded overwrite — that
one is the whiteboard saving the user's own canvas) and `/api/chains/{rename,delete}`. Renaming or
removing the user's chain is an in-place mutation; both are GUI-only. A "revision" from Claude is a
NEW chain beside the original, which the user compares on the canvas and then keeps or deletes.

Note what makes that cheap for chains specifically: a `ChainRun` stores a **content-hashed copy** of
the template it ran, so past runs are unaffected by any later edit, rename or delete of a template.
Chains need no snapshot/versioning of the kind notebooks have — see `docs/SCHEDULER.md` →
*Template vs run record* and *Who may author a template, and who may run one*.

## Running it

1. Start the Cecelia backend: `pixi run dev` (serves the API on `:8080`).
2. Start this server: `pixi run mcp` (stdio). Override the API with `CECELIA_API_URL`.

### Wire into Claude Code

**You don't have to do this by hand.** The lab-log panel's info dialog has a **Set up my terminal**
button: it POSTs `/api/observer/register`, which runs `claude mcp add-json cecelia-observer <spec> -s user`
with the spec below already resolved (python bin, `PYTHONPATH`, API port), so plain `claude` gets the
tools in every session. It removes-then-adds, so it's also the re-sync after a move/reinstall. The
config file it uses for the *in-app* agent is `<config_dir>/observer-mcp.json`
(`api/src/observer_api.jl` → `_write_observer_mcp_config`), which also works as
`claude --mcp-config <path>` for a single session. See
[`../docs/ai-assist/OBSERVER-SETUP.md`](../docs/ai-assist/OBSERVER-SETUP.md). The manual route below is
for when you'd rather edit the config yourself.

Add to your Claude Code MCP config (adjust the absolute path to this checkout):

```json
{
  "mcpServers": {
    "cecelia-observer": {
      "command": "pixi",
      "args": ["run", "mcp"],
      "cwd": "/home/dominik/cc-workspace/cecelia/cecelia-feijoa"
    }
  }
}
```

Then, with a project open, ask e.g. *"describe my project"*, *"what failed on image 7?"*, or
*"note in the lab log that I switched cellpose diameter to 30 because the nuclei were undersegmented."*

## Tests

`pixi run test-mcp` — stdlib `unittest`, HTTP mocked, no server required.
