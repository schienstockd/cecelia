# Agent runs in a GUI session: plan

**Status:** built (2026-11-28) — P1–P3 in one PR; not yet used in a real session. Checked against the
code twice before building; the second pass found D4 mostly built and the chain pre-flight weaker than
`run_replaces` (see *Second check*; the pre-flight now uses it too). Open questions answered by the builder's defaults (end of file).
Code: `mcp/cecelia_mcp/gui_mode.py`, `autonomous.py` (`gui_project`, `check_writes`), `run_write_check`
(`app/src/tasks/chain/preflight.jl`) behind `POST /api/tasks/write-check`, `adoptsForeignTask`
(`frontend/src/utils/runningTasks.ts`), the plugin's second server (`plugin.json` 1.1.0).

## Second check (2026-11-28)

- **D4 is mostly built.** Every task/chain frame is broadcast to every socket (`api/src/sockets.jl`
  `_broadcast_task` → `broadcast_ws`). Chain nodes from any socket already get a live row
  (`stores/ws.ts` `chain:node:*` → `addFromChainEvent`). `adoptInFlight` (`ws.ts`) already pulls the
  scheduler's in-flight set into the list, but only on (re)connect and project switch. The gap is single
  tasks only: `task:status` for an unknown id hits `setStatus`'s `if (!t) return`. Fix = an unknown id
  triggers a (debounced) `adoptInFlight()`.
- **The chain pre-flight did not catch the case that started this** (fixed since: it calls
  `run_replaces` with the resolved params). `/api/chains/preflight` warned "replaces the existing" only for image versions and label sets
  (`_preflight_fields` returns `nothing` for other namespaces), so a `clustRegions` / clustering / HMM
  re-run under an existing name passed silently — for the user's own chain runs too. `run_replaces`
  covers those (it also reads the run log), so D3 and the pre-flight both use it.
- **D2 holds.** `caller_hook.py` never emits `permissionDecision`, and `observer_plugin.jl` writes
  `enabledPlugins` / `pluginConfigs`, no allow-list.
- **New cost for D1:** `autonomous_server.py` also registers the whole gating and figure toolsets
  (`register_gating_tools`, `register_figure_tools`). Added to the plugin as-is, the GUI session gets
  every gating/figure tool twice. GUI mode registers only `get_lock`, `run_task`, `run_chain`, `wait_*`.

## Goal

Let Claude, co-driving with the user in the GUI (observer plugin), **start** a task or chain run
instead of only designing a chain the user then presses Run on. Narrowly: only runs whose every output
is a **new** name, and only with the user approving each call.

Trigger: in a co-driving session Claude had to hand over a one-node chain (`clustRegions`, new suffix
`gBT`) for the user to press Run on. The design step was real work; the press was not.

## Where things stand (checked 2026-11-28)

| Piece | Where | State |
|---|---|---|
| Observer cannot start work | `mcp/cecelia_mcp/guidance.py:56-58`, `mcp/tests/test_server.py:327` `test_no_tool_can_start_work`, `docs/inventory/MCP.md` | By design: launching is a WebSocket message and the observer speaks HTTP only |
| Autonomous server | `mcp/cecelia_mcp/autonomous.py`, `autonomous_server.py` | Has `run_task` / `run_chain` / `wait_*`, a project lock (`CECELIA_MCP_PROJECT`, unset = refuse all), an optional output prefix, and refuses `overwrite=true` without the prefix (`autonomous.py` `output_name_violations`). Meant for disposable projects, plus `pixi run chat` (no GUI, `HEADLESS_MODE_PLAN.md` D2) |
| AI switch | `api/src/server.jl:962-969` (`_from_claude` + `ai_refusal`) | AI off refuses every Claude request, WebSocket upgrade included |
| Authorship | `caller_stamp()` / `author_stamp()` | Runs started by Claude are stamped with model + session |
| What a run replaces | `run_replaces(fun_name, img, params)` (`app/src/run_log.jl:468`) | Already answers "does this run overwrite something that exists" per task |
| Task Manager | `frontend/src/stores/tasks.ts` `setStatus` (`if (!t) return`), `stores/ws.ts` | A run started from another socket gets no live row; it appears only after a reconnect or project switch |

## What a run can damage

- **Re-using a name silently overwrites.** `clustRegions`, cell clustering and HMM (`colName`) have no
  `overwrite` param: the same `valueNameSuffix` / `colName` rewrites `regions.<suffix>` etc.
  (`docs/audit/agent-sandbox-value-name.md` L2). The `overwrite=true` guard does not catch these.
- **Segmentation / measure runs move `_active`** (L1; `measure_labels.jl`, `tasks/task/composite.jl`),
  so the user's next session opens on Claude's segmentation. `labels` / `label_props` writers overwrite
  and never version (`app/CLAUDE.md`, versioned variable pattern).
- **vN versioning does not help.** `keep_previous_version()` covers only the pixel writers, and nothing
  forces it on for agent runs.
- **GPU contention.** The `gpu` pool has limit 1, so a Claude cellpose run queues the user's own.

## Proposed decisions

### D1: reuse `cecelia-autonomous` in the GUI session, in a "GUI lock" mode
No new server. The plugin adds the autonomous server; in GUI mode its lock is the project the user has
open (or each call's project, checked against the open one; see open question 1), with no prefix.
Rejected: a per-session grant in Preferences (repeats the AI switch), and an in-app "Claude wants to
run X — Allow?" dialog (a pending-request store, a WS frame, a dialog, its mount, an `AI_SURFACES`
entry; mostly repeats Claude Code's own per-call prompt). See costs below.

### D2: approval is the existing per-call prompt
Claude Code asks before every tool call and the app writes no MCP allow-list, so each `run_task` is
approved by the user at the keyboard. This is the gate `HEADLESS_MODE_PLAN.md` D2 already accepts.
`docs/ai-assist/OBSERVER.md` Phase 2 ("human confirmation on every action") and the co-driving audit
("Launch step: ask with cost", `docs/archive/OPUS_AUDIT_CLAUDE_CO_DRIVING_REPORT.md:77`) point the
same way.

### D3: two hard refusals in GUI mode
1. **Any run where `run_replaces` is not `nothing`.** Covers the silent-overwrite tasks without a
   per-task list. Needs a dry-run route so the MCP side can ask before submitting.
2. **Any task that writes `labels`, `label_props` or `filepath`** (segmentation, measure, pixel
   cleanup). These change what the user's images open on.

The refusal message names a free name to use instead, as the autonomous server already does.

### D4: the run shows up live in the Task Manager
Adopt unknown run ids in `stores/tasks.ts` / `stores/ws.ts` when a frame arrives for them, for single
tasks and chain nodes (`chain:node:*`, not yet checked). Without this the user cannot see or cancel
Claude's run without reloading, which defeats D2.

### D5: guidance and the guarantee test change together
`guidance.py`'s "nothing here can START it" and `test_no_tool_can_start_work` are the code-level form of
the old stance. Rewrite both in the same change, so the test pins the new boundary (GUI mode exposes
`run_task` / `run_chain`; it still refuses D3) rather than being deleted.

## Costs (touchpoints, counted by the review agent)

| Option | Code | Tests | Docs | Total |
|---|---|---|---|---|
| Keep as is | 0 | 0 | 0 | 0 |
| **D1–D5 (recommended)** | 7 | 4 | 3 | ~14 |
| + per-session grant | +~4 | | | ~18 |
| + in-app Allow dialog | +~5 | | | ~19 |

Code: `mcp/claude-plugin/cecelia/.claude-plugin/plugin.json` (add the server, bump the version),
`autonomous.py`, `autonomous_server.py`, `guidance.py`, `server.py`, a dry-run route for `run_replaces`
(`api/src/task_validate_api.jl` plus registration), `frontend/src/stores/ws.ts`.
Tests: `mcp/tests/test_server.py`, `mcp/tests/test_autonomous.py`, a `ws.ts` test, a Julia route test.
Docs: `docs/inventory/MCP.md`, `docs/todo/HEADLESS_MODE_PLAN.md`, `docs/ai-assist/OBSERVER.md`.

## Phases

- **P1:** D3's dry-run route + refusals in `autonomous.py` (unit-tested, no GUI change).
- **P2:** D4 live adoption of foreign single-task ids (chain nodes already work; see *Second check*).
- **P3:** D1 GUI-lock mode + plugin wiring + D5 guidance/test rewrite. Ship only after P2.

## What this reverses

No recorded decision (agent-overnight is parked, and is about unattended runs). It reverses the
code-level stance in `guidance.py`, `test_no_tool_can_start_work`, `docs/inventory/MCP.md` ("never in a
GUI user's session") and `HEADLESS_MODE_PLAN.md` gap 2. Kiwi is unaffected: it gets read tools only
(`tools.json` `_about`).

## Open questions — the defaults built (change if they turn out wrong)

1. **Lock: each call's project is checked against the project open in the GUI**, i.e. the newest
   `lastOpenedAt` from `GET /api/projects`, asked per call. A switch in the app moves the lock.
2. **"Don't ask again" is accepted** as the user's choice; no in-app dialog.
3. **No pool restriction.** D3 already refuses segmentation (the gpu pool's work), and the user approves
   each call; the guidance asks Claude to say what a long or GPU run is before starting it.

## What D3 refuses, as built

`gui_mode.write_refusals` on `/api/tasks/write-check`'s answer (`run_write_check`, whose params go
through `_will_run_params`: `resolve_run_params` as `run_task` does, plus a composite's step defaults,
so an omitted name is the default name the run will write):
- an output in the `labels` namespace, written or written into (segmentation, measure, tracking, AF
  cells) — covers `labels` / `label_props`;
- an image version (namespace "") other than a fixed `default` that is not written into (that is a new
  image: bin, crop, projections) — covers `filepath`;
- anything `run_replaces` reports on any target image, with `free` (`<name>_2`, …, `free_label_name`'s naming) named in the refusal.
A run that names no output (`output` absent) is allowed. Chains: every node is checked against all
target images before `chain:run` is sent. `overwrite: true` stays refused (no prefix in GUI mode).
Limits: a namespace not on the image (regions, clusters, models, …) is "replaced" only if the run log
(or its archive) records the run that made it; an output from before the run log is not seen.

The chain pre-flight's own replace warning now calls `run_replaces` too, on each node's resolved params
(follow-up PR), so the user's chain runs see a taken regions / clusters / HMM name as well, spelled out
or left at its default. Known gap, not changed: the run log's own stamp at open (`run_provenance` from
`scheduler/jobs.jl`) resolves a composite without its step defaults, so a composite run with its name
left out records no `replaces` and its entry names no output for a later `run_replaces` to match.
