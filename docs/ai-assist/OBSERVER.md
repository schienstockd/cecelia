# Phase: MCP Server — Claude Integration Infrastructure

This is Phase 1 of a three-phase Claude integration arc. Build the infrastructure here. The behaviour that runs on top of it is defined in `QC-PROCESS.md` and `LAB-LOG.md`. All three documents form one system — read them together.

---

## The arc

**Phase 1 — Observer** (this prompt)
Claude sits next to the user. Watches what's happening. Reads task logs, image notes, project state. Asks questions. Notices patterns. Stays silent unless something is worth saying. Builds the lab log. No mutations, no autonomy.

**Phase 2 — Designer** (after API and chain scheduler are stable)
Claude proposes analysis strategies, suggests parameter adjustments, flags before things go wrong rather than after. Write-capable MCP with human confirmation on every action.

**Phase 3 — Analyst** (after Phase 2 is proven)
The overnight analyst. "I have 20 images, two treatment groups, I need to know if there's a behaviour difference by tomorrow." Claude runs the pipeline, flags edge cases, writes the morning summary. User reviews results, not a blank screen.

---

## Data sources Claude reads

The value of the observer session depends entirely on what Claude can see. Claude must have access to:

- **Project state** — images, processing status per stage, label props available
- **Task logs** — every task writes a log file per image. Claude reads these. A task that failed 10 times leaves 10 log files. Claude notices.
- **Image notes** — users can add notes to individual images in Cecelia. These are first-class data for Claude — the user's own words about what they observed.
- **Lab log** — the accumulated cross-session memory (see `LAB-LOG.md`)
- **QC metrics** — per-stage quality flags computed after each task (see `QC-PROCESS.md`)

## MCP tools to implement

**Read-only (Phase 1 — implement now):**
```
get_project_info          → name, version, image count, current stage
list_images               → UIDs, names, processing status per stage, any user notes
get_image_info            → one image: channels, dims, label props, task history
get_task_log              → log content for a specific task run on a specific image
get_task_history          → recent tasks across all images: function, status, attempt count, timestamp
get_image_notes           → user-written notes for a specific image
read_lab_log              → full lab log content
get_qc_metrics            → per-image QC flags for a given stage
```

**Write (Phase 1 — lab log only):**
```
append_lab_log            → append a dated [Claude] entry. Append-only, never edits existing content.
```

**Write (Phase 2 — deferred):**
```
submit_task               → propose and submit a task (requires user confirmation)
adjust_params             → propose parameter adjustment for a flagged image
acknowledge_flag          → user acknowledges a QC flag, pipeline proceeds
```

Only expose endpoints that already exist and are stable in the Julia API. Add missing routes as thin read-only endpoints before wiring into MCP.

## Transport

stdio — standard for local MCP servers. Claude Code connects over stdin/stdout. The MCP server starts alongside the Julia API or independently for sessions where the GUI isn't running.

## The observer session (sit next to me mode)

> **Implementation note (2026-07, `feat/observer-remove-watch`):** the in-app auto-firing "Watch"
> was **removed**. In practice most task completions had nothing worth flagging, so the auto passes
> were token noise the user stopped reading. Claude is now **on-demand only** — the "Ask Claude"
> button runs one pass. Deterministic, always-on reporting is **Cecelia's** job (the `capture_context!`
> digests + QC traffic lights), not Claude's. The design below is the original vision; the event-push
> machinery (`monitor.py`) remains for the repeat-failure signal + a possible future opt-in. See
> `docs/todo/QC_OBSERVER_PLAN.md`.

The observer session is a Claude chat window with MCP access to the running Cecelia project. The user opens it and says "just sit next to me" or "watch what I'm doing." Claude doesn't wait to be asked — it monitors and surfaces observations when something is worth saying.

**Event-driven, not continuous.** Claude does not stream logs in real time — that would be prohibitively expensive in tokens. Instead, the MCP server pushes event notifications to Claude when specific things happen:

```
task_completed            → image UID, function name, outcome, attempt count
task_failed               → image UID, function name, error summary, attempt count
image_note_added          → image UID, note content
qc_flag_fired             → image UID, stage, metric, value, cohort comparison
lab_log_entry_added       → new entry summary (user-written entries only)
```

On each event, Claude receives a compact context packet (the event + relevant recent history for that image) and decides: stay silent, or surface an observation. Most events → silence. Claude speaks when the pattern is worth naming.

**The 10-attempts pattern.** If `task_completed` or `task_failed` fires with attempt_count > 3 for the same function on the same image, Claude surfaces it: "You've run cellpose on image 7 ten times. The outcome has been consistent each time. Want to talk through what you're trying to achieve?" This is the core value — pattern recognition across attempts that the user is too close to notice.

**The why question.** When a user does something unexpected — a parameter choice that deviates from prior runs, an image note that suggests an unusual decision — Claude asks why. Not to judge, to record. The answer goes in the lab log. Lab heads want figures. This is how the methodology doesn't get lost.

## Token cost

The event-driven approach keeps costs manageable. Each event triggers one Claude call with a compact context packet — not a full project dump. Estimate: 10-20 events per analysis session, each ~2-3k tokens of context. Comparable to a normal chat session. The observer session does not stream continuously.

**Honest caveat**: this estimate is optimistic for heavy sessions. 20 images with multiple failed tasks, cohort stats, and QC flags could be 10x that. Measure actual token cost in practice and implement a throttle — a configurable max events per session before Claude stops surfacing observations and just logs silently. The user should be able to see running token cost and disable the observer session if it becomes prohibitive.

## Early calibration warning

The observer session becomes more valuable over time, not immediately. Early in a project, before the lab log is rich and before Claude has seen the cohort, its sense of what's "unexpected" is poorly calibrated. It may ask obvious questions or miss genuinely unusual decisions. Set user expectations accordingly — the first few sessions are about building context, not catching errors.

## Implementation plan

> Grounded against the codebase as of 2026-07. **No MCP server exists yet — this is greenfield.**
> The good news: most read tools map to routes that already exist, so Phase 1 is mostly a thin MCP
> adapter + a handful of new read-only endpoints + one new event.

### 1. MCP server — separate process, stdio

- Build the MCP server as a **standalone process** (stdio transport, as specified) that talks to the running Julia API over `http://127.0.0.1:8080` (HTTP) and `ws://127.0.0.1:8080/ws` (events). It is **not** in-process Julia — that keeps `api/` Julia-only and matches the stdio-transport decision.
- **Language — decided: Python**, using the official `mcp` SDK (FastMCP), in a new top-level `mcp/` dir. MCP has two standard, Anthropic-maintained SDKs — TypeScript (the reference implementation) and Python; both are first-class. Python wins here because it is already a first-class, pixi-managed ecosystem in this repo, so the server needs no new toolchain, no second CI lane, and can run from the same env — whereas TypeScript would add a standalone Node project purely for this. Note the `mcp/` dir is **separate infra**, not part of the `cecelia` analysis package (which is unrelated IO), consistent with the one-language-per-top-level-dir rule.
- The server holds one persistent WS connection for events (§4) and makes on-demand HTTP GETs for the read tools (§2). No Julia dependency is added to `app/`/`api/`.

### 2. Read tools → routes (most already exist)

| MCP tool | Backing route | Status |
|---|---|---|
| `get_project_info` | `GET /api/projects` (`routes.jl:313`) + `POST /api/projects/load` (`routes.jl:340-411`, richest state) | **exists** |
| `list_images` | `sets[].images` from `/api/projects/load` (`_image_payload`, `routes.jl:977-1024`) | **exists** (optional thin `GET /api/images?projectUid` for a lighter call) |
| `get_image_info` | `GET /api/images/meta?projectUid&imageUid` (`routes.jl:668-684`) — returns channels/dims/physical sizes/labels/`note`, plus `qc=read_all_qc(img)` and `runLog=read_run_log(img)` | **exists** |
| `get_image_notes` | the `note` field inside `_image_payload` (`routes.jl` note field); no separate call needed | **exists** |
| `get_qc_metrics` | `GET /api/gating/stats` (`gating_api.jl:364`) for gating; `qc` block in `_image_payload` for per-stage findings; per-set cohort sidecar (`QC-PROCESS.md` step 3) | **partly new** (cohort route) |
| `get_task_log` | **NEW** `GET /api/images/tasklog?projectUid&imageUid&fun` → reads `{img._dir}/logs/{fun}.log` (`scheduler.jl:321-335`) | **new** |
| `get_task_history` | `runLog` in `_image_payload` gives per-image history; **NEW** `GET /api/tasks/history?projectUid` to aggregate across images (and, once step 9 of `QC-PROCESS.md` lands, attempt counts) | **partly new** |
| `read_lab_log` | **NEW** `GET /api/lablog?projectUid` (`LAB-LOG.md` step 3) | **new** |
| `append_lab_log` (write) | **NEW** `POST /api/lablog/append` (`LAB-LOG.md` step 4), lock-guarded, append-only | **new** |

- Adding each new route = write `api_*(req)` + one `elseif` in `handle_http`'s GET/POST block (`api/src/server.jl:168-391`) + **server restart** (api/ is not Revise-tracked). Response shape: `(status, JSON3.write((; …)))` (`docs/API.md:36-47`).
- **No-mutation guarantee**: the MCP server only ever calls the read routes above + `POST /api/lablog/append`. It never touches task submission, gate CRUD, or inclusion routes. Enforce by allow-listing the exact routes in the MCP adapter (Phase 1).

### 3. Read-only enforcement

Phase 1 exposes exactly nine tools, eight read + `append_lab_log`. `submit_task`/`adjust_params`/`acknowledge_flag` are Phase 2 and must not be wired now. The append route is the *only* write path and is itself append-only (`LAB-LOG.md` step 4).

### 4. Event push — subscribe-and-rebroadcast

- The backend already bridges the package event bus to WS: `subscribe_chain_events!`/`_fire_chain_event!` (`app/src/events.jl`) → re-broadcast as `chain:node:*` frames in `api/src/server.jl:113-164`, plus task frames `task:status`/`task:log`/`task:result`/`task:progress` (`api/src/sockets.jl:1-21`). The MCP server subscribes to the WS and maps frames to the spec's observer events:
  - `task_completed`/`task_failed` ← existing `task:status` (`sockets.jl:15`) and chain `node:done`/`node:failed` (`server.jl:139-164`). **Reuse.**
  - `qc_flag_fired` ← **NEW** backend event (`QC-PROCESS.md` step 8): fire a `node:flagged`/`qc:flag` from `_update_node_state!`, add a bridge subscriber.
  - `image_note_added` ← **NEW** broadcast added to `api_images_inclusion_set` (`routes.jl:891`).
  - `lab_log_entry_added` ← **NEW** broadcast from the append route, user entries only (`LAB-LOG.md` step 4).
- **Event-driven, not streaming**: the MCP server receives frames, assembles a compact context packet (event + recent history for that image via the §2 read tools), and decides speak-vs-silent. Firing stays outside `run._lock` (SCHEDULER.md invariant #2); WS delivery is the existing bounded drop-on-full per-client queue (`server.jl:34-62`).

### 5. The 10-attempts pattern — needs an attempt counter

The core signal (">3 runs of the same fn on the same image") has **no data source today**: the run log records successes only, no retry count (`app/src/run_log.jl`), and `ImageNodeState` has no counter. Add a per-`(uid,node)` attempt counter persisted in `run.json` (`QC-PROCESS.md` step 9) **or** derive it in the MCP server's session memory by counting `node:running`/`node:failed` frames. This is a hard dependency for the pattern — build it with the event push.

### 6. Token cost & throttle

Implement the configurable per-session cap in the MCP server: after N surfaced observations, stop emitting to the chat and only append to the lab log (Silent-equivalent). Report running token cost per session and expose an off switch, per the doc's honest caveat that heavy sessions can be ~10× the estimate.

### 7. Build order

1. ✅ **DONE** — New read routes: `get_task_log` (`GET /api/images/tasklog`), `get_task_history` (`GET /api/tasks/history`), plus a read-only `GET /api/images` project listing (so `list_images`/`get_project_info` avoid `/projects/load`'s `lastOpenedAt` write). `read_lab_log`/`append_lab_log` already existed. Thin, no scheduler change.
2. ✅ **DONE** — MCP server skeleton (`mcp/`, Python + FastMCP, stdio) + the eight read tools + `append_lab_log`, allow-listed in `cecelia_mcp/client.py` (the no-mutation guarantee; append is the sole write). `pixi run mcp` / `pixi run test-mcp`. See `mcp/README.md`.
3. ✅ **DONE (Slice B)** — Attempt counter (§5) + `image_note_added`/`lab_log_entry_added` events (§4).
   Counter lives in the **MCP server's session memory** (the doc's "OR" option), fed by the WS stream
   via `mcp/cecelia_mcp/monitor.py` (pure, unit-tested) + `wsclient.py` (thin listener), surfaced by
   the `poll_observations` pull tool. Backend: `fun` added to the `task:status` frame (so module-page
   runs are attributable, `sockets.jl`); `image_note_added` broadcast from `api_images_inclusion_set`
   and `lab_log_entry_added` (user entries only) from `api_lablog_append` (`routes.jl`). **Deferred:**
   `qc_flag_fired` — needs the `:flagged` node state that doesn't exist yet (`QC-PROCESS.md` step 1/8);
   ships with the QC work. Also deferred: server→client *push* (MCP is client-pull; `poll_observations`
   is the reliable Phase-1 surface — validate live push before building the continuous version).
4. ✅ **DONE (Slice C)** — Throttle + token reporting (§6), all in `mcp/cecelia_mcp/monitor.py`
   (pure, unit-tested). Configurable per-session `surfaceCap` (default 20): once that many
   observations have been surfaced, `poll_observations` goes quiet and suppressed patterns are flushed
   to the lab log as a compact `[Claude]` block (silent-equivalent) rather than spending chat tokens.
   Running per-session `stats` (`get_observer_stats` / on every poll): surfaced count, cap, throttled,
   and an `estimatedTokens` gauge (surfaced × ~2.5k — an estimate; the server can't see Claude's real
   usage). Off switch: `set_observer_active(false)` stops surfacing but keeps counting. **Calibration**
   (the cap and per-observation token estimate) are defaults to tune against a real live run.
   Also in this slice: **`get_recent_logs`** (`GET /api/logs/recent`, allow-listed) — the backend
   console ring (server `@info`/`@warn`/`@error`). A **Julia-side task crash lands here, not in
   `get_task_log`** (which only captures the Python subprocess's stdout), so when a task keeps failing
   with an empty task log the observer can pull the real error. Added because the first live session
   hit exactly this blind spot. Complementary durable fix (separate PR): tee the scheduler's caught
   task exception into the per-image `.log` so `get_task_log` shows Julia failures too.
5. ✅ **DONE (compute + route + MCP tool)** — Cohort stats: per-image objective metrics are banked by
   the tasks (segment/measure/tracking → `write_qc`; `app/src/qc_cohort.jl` aggregates them per
   `CciaSet`), surfaced via `GET /api/qc/cohort?projectUid&setUid&funName[&valueName][&sdThreshold]`
   (mean/SD + `z`-scored outliers over the *included* images, advisory, recompute-on-demand + set
   sidecar), and exposed to the observer as the **`get_cohort_qc`** MCP tool (the prompt tells it to
   call this before calling any run an "anomaly vs the set"). **Remaining:** auto-recompute on
   stage-complete (the "cohort complete" trigger, `QC-PROCESS.md` step 3) — today it's on-demand; and
   per-image cohort findings (write outliers back through `write_qc` so they badge on the image).
   `qc_flag_fired` still waits on the `:flagged` state (`QC-PROCESS.md` step 1).

> **Status (Slice C):** steps 1–4 landed (minus `qc_flag_fired`, which waits on QC flag state).
> **Validated end-to-end against live Claude Code (2026-07-17):** with a real project open, the
> observer independently flagged "segment.cellposeMeasure has run 11×, all failed" — `repeat_attempts`
> firing through `poll_observations` and Claude acting on it, unprompted. The observer can describe
> project state, read task logs + history + the lab log + the backend console (`get_recent_logs`),
> append `[Claude]` entries, detect the 10-attempts pattern live (chain + module-page, session-scoped),
> surface notes / lab-log entries, and throttle itself (cap → silent lab-log logging, token estimate,
> off switch) — and can do nothing else (allow-list enforced + tested; WS + console are receive-only).
> Remaining: the cohort QC route (step 5, waits on `QC-PROCESS.md` step 3) and `qc_flag_fired`; and
> calibrating the cap/token estimate against measured cost (note: retry storms inflate attempt counts,
> so an "N attempts" may be fewer real user actions than it looks).

## Verify

- Connect Claude to MCP, open a project — Claude describes project state correctly without being told
- Run a task that fails 3 times — Claude surfaces the pattern without being asked
- Add an image note — Claude acknowledges it and incorporates it in context
- Append to lab log via MCP — entry appears with correct [Claude] tag and date
- Confirm no mutations possible beyond lab log append
- Confirm token usage is reported per session and throttle fires correctly
