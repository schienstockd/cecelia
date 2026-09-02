# Cecelia Feijoa — Architecture Reference

---

## Layer boundary (Package / API / GUI)

The hard rule: **Cecelia.jl must be usable headless from the Julia REPL with zero knowledge of HTTP, WebSockets, or Vue.**

```
cecelia-feijoa/
  app/          Julia package — Cecelia.jl + each task's co-located Python runner
                (app/src/tasks/<cat>/<name>_run.py, run by path via run_py)
  api/          Julia API server — depends on Cecelia.jl as Pkg dependency
  frontend/     Vue 3 — unchanged
  python/       Installable Python package `cecelia` — IO LIBRARY only: analysis/IO helpers
                (cecelia.utils) + writers. NO task runners. Top-level sibling of app/.
```

**`python/` is the installable IO library; task runners live with their task in `app/`.** A task's
`_run.py` is run by file path (never imported), so it doesn't make `app/` an importable Python
package. The Python helpers (`cecelia.utils` + writers) are a
*separate*, pip-installable **IO library** at `python/cecelia/` (moved there from the old `app/py/`
in 2026-07 so external consumers can `pip install cecelia`), holding **no task runners**. `run_py`
(`app/src/py_runner.jl`) resolves `"tasks/…"` under `app/src/` (co-located runners) and other
scripts (e.g. `"writers/…"`) under `python/cecelia/`, and sets `PYTHONPATH=python/` so every runner
can `import cecelia.*`. Dependency split: light IO deps in `python/pyproject.toml`; heavy/conda/
per-platform deps in `pixi.toml`. See [`docs/todo/PY_PACKAGING_PLAN.md`](todo/PY_PACKAGING_PLAN.md).

### Every language boundary carries a version

Julia and Python meet at two places, and **each one can end up with the two halves running different
code**. One holds a long-lived Python process that is *adopted* rather than relaunched (it outlives a
backend crash or Ctrl-C on purpose); the other spawns Python fresh every run, so it is the **Julia**
half that goes stale — `app/src` is Revise-tracked and a branch switch or a merge under a live server
does not always reload it.

| Boundary | Julia | Python | Mismatch → |
|---|---|---|---|
| preview worker (:7656) | `PREVIEW_PROTOCOL` (`app/src/preview.jl`) | `PROTOCOL` (`preview/preview_worker.py`) | refuse to adopt, relaunch |
| task params (`run_py`) | `PY_CONTRACT_VERSION` (`app/src/py_runner.jl`) | `CONTRACT_VERSION` (`cecelia.utils.script_utils`) | the runner exits, naming the restart |

**Bump both sides together**, and add a row rather than a bespoke check. The pairs are asserted equal by
the `language boundaries agree on their protocol` testset — before it existed the preview pair had been
bumped by hand three times and the fourth was nearly missed.

**When to bump is BEHAVIOURAL, not structural: whenever an adopted older peer would answer
differently.** A changed message shape is the obvious case; a bug fixed *inside* an adopted process is
the one that gets missed, and it is the worse of the two. `PREVIEW_PROTOCOL` 5 is exactly that — the
reply shape is byte-identical to 4, and the only change is that AF preview no longer dies on an
`AttributeError`. Left unbumped, a backend carrying the fix adopts the broken worker and serves the bug,
which is how the fix appeared not to work at all. The version is the only thing that can refuse a
process we did not start, so anything we would not want served from the old code has to move it.

Why versions at all, rather than trusting the processes to match: a mismatch is **never a clean
failure**. A stale peer answers the handshake perfectly and then misreads the actual work. The three
real occurrences read as `unexpected keyword argument 'mask'`, a bare `Preview failed`, and
`invalid literal for int() with base 10: 'CH3'` — none of which named the cause. The params guard lives
in `script_params` (the one function every runner already calls) so a new runner is covered without
doing anything, and an ABSENT `CECELIA_PY_CONTRACT` is allowed through so replaying a saved params file
by hand still works.

### …and both ends of a resident-Python socket carry the same frame cap

Every message on the preview leg is **one JSON frame carrying a whole payload** — a set of AF-corrected
channels, a contact sheet of PNGs. Both ends cap the size of a frame they will accept, the two caps
are independent, and one number governs both:
`WS_MAX_FRAME_SIZE` (`app/src/utils.jl`) = `WS_MAX_SIZE` in `preview_worker.py`, asserted equal by the
`resident python legs agree on their frame cap` testset.

This is the protocol-version failure in a value nobody thought of as a version. The Python side had been
raised to 64 MiB with a comment saying the 1 MiB default "is not a graceful degradation — the server
rejects the frame and closes the connection, so the preview would fail on big images only". All of that
was equally true of the Julia side, which nobody set: HTTP.jl's client default is 16 MiB, so the
**backend was the narrow leg**. A whole-frame flow-metrics sheet on a 1044×1102 movie is 36.3 MB in one
frame, and it arrived as `websocket closed with status 1009: message too large` — on every image except
the 418×434 one the panel had been developed against.

A cap is a backstop, not a budget. A producer that can emit tens of MB still has to bound what it
sends — see `FLOW_INSPECT_MAX_PX` (`api/src/optical_flow_api.jl`), which crops the flow sheet rather
than relying on the transport to survive it.

### Layer ownership

| Concern | Layer | Notes |
|---------|-------|-------|
| Data model (project/image/set structs, ccid.json) | Package | `model/image.jl`, `model/set.jl`, `model/project.jl` |
| Config loading, path helpers | Package | `config.jl` |
| Versioned-variable helpers | Package | `helpers.jl` |
| Lockfile / `with_transaction` | Package | `model/project.jl` |
| Task abstract type + validation | Package | `tasks/task.jl` |
| Concrete task implementations | Package | `tasks/<category>/<name>.jl` |
| Task param specs (JSON) | Package | `tasks/<category>/<name>.json` — single source of truth |
| REST route handlers | API | `api/src/routes.jl` |
| WS task queue + status push | API | `api/src/sockets.jl` |
| HTTP/WS server entry point | API | `api/src/server.jl` |
| Serving task param specs to Vue | API | `GET /api/tasks/definitions?category=X` |
| UI rendering, param forms | GUI | Vue, Pinia — no param definitions duplicated here |

### REPL-runnable contract

```julia
using Cecelia
init_cecelia!()

# Load by UIDs
img = init_object("proj-uid", "img-uid")

# Type dispatch
run_task(ImportOmezarr(), img, Dict("pyramidLevels" => 2))

# String dispatch (ergonomic for notebooks/scripts)
run_task("proj-uid", "img-uid"; fun_name="importImages.omezarr", params=Dict("pyramidLevels" => 2))

# Batch
run_tasks("proj-uid", ["uid-1", "uid-2"]; fun_name="cleanupImages.smooth", params=params)
```

Both forms validate params against the co-located `.json` spec before running. Invalid params throw `ParamValidationError` with the constraint that was violated.

**Side channels are dependency-injected, not hard-wired.** `_run_task` takes `on_log`,
`on_progress`, and `on_process` callbacks (defaults: `println`, no-op, no-op). The task is
sink-agnostic — it never references the WebSocket. The API binds these to `ws_log`/`ws_progress`
and the process registry (`api/src/sockets.jl`); a REPL/test caller uses the defaults or injects
its own. This is the mechanism that keeps the package runnable headless — a task that called
`ws_progress` directly would break the contract and fail with no WS running.

### Audit table (post-cleanup)

| File | Classification | Reason |
|------|---------------|--------|
| `app/src/config.jl` | PACKAGE | Config loading, path helpers — no HTTP |
| `app/src/utils.jl` | PACKAGE | `gen_uid`, `_dir_bytes` — pure utilities |
| `app/src/helpers.jl` | PACKAGE | Versioned-dict helpers — pure |
| `app/src/model/image.jl` | PACKAGE | CciaImage; round-trips every ccid.json field (status, attr, imChannelNames, filepath) |
| `app/src/model/set.jl` | PACKAGE | CciaSet, `init_object`, `delete_image!` |
| `app/src/model/project.jl` | PACKAGE | CciaProject, `delete_set!`, naive `with_transaction` lockfile |
| `app/src/tasks/task.jl` | PACKAGE | Abstract type, validation, REPL contract |
| `app/src/tasks/importImages/omezarr.jl` | PACKAGE | bf2raw via callbacks — no WS |
| `app/src/tasks/importImages/remove.jl` | PACKAGE | File deletion + ccid.json update — no WS |
| `app/src/tasks/cleanupImages/smooth.jl` | PACKAGE | Python subprocess via callbacks — no WS |
| `app/src/tasks/*/*.json` | PACKAGE | Param specs — served via API, not bundled in Vue |
| `app/src/gating/population_manager.jl` | PACKAGE | Gating engine + `pop_df` unified accessor (cell/track) — pure, no HTTP |
| `app/src/plotting/plot_data.jl` | PACKAGE | Summary-plot aggregation (`pop_df` → histogram bins / frequency counts) — pure `Dict`, no HTTP |
| `app/src/plotDefinitions/*.json` | PACKAGE | Plot-type specs (one plot per file) — served via API like task specs, not bundled in Vue |
| `api/src/server.jl` | API | HTTP+WS entry point, router |
| `api/src/routes.jl` | API | REST handlers — delegate to package; shape responses only |
| `api/src/gating_api.jl` | API | Gating/plot HTTP routes — delegate to `pop_df`/`plot_summary_data`; serve plot specs (`GET /api/plots/definitions`), aggregate (`POST /api/plot_data`) |
| `api/src/sockets.jl` | API | WS task queue, process registry, `_kill_tree`, status push |

The plotting canvas obeys the same boundary as everything else: **aggregation is a PACKAGE function**
(`plot_summary_data`, headless-testable, returns a plain data structure), the **route is a thin API
wrapper** (parse request → call package → serialise), **plot specs are PACKAGE JSON served over a GET
route** (Vue keeps no copy), and the **Vega-Lite rendering is frontend-only** (the package never emits
chart config, only aggregated numbers). A summary panel that needed bespoke Julia HTTP code, or a Vue
component that computed bins from raw cells, would be a boundary violation.

> **Cleanup (this audit):** the pre-separation monolith orphaned in `app/src/` —
> `server.jl`, `tasks.jl`, `projects.jl`, `metadata_handler.jl` — was deleted
> (none were loaded by `Cecelia.jl` or the live server; one even `include`d an already-deleted file).
> Task implementations were consolidated from flat `tasks/*.jl` into co-located
> `tasks/<category>/<name>.{jl,json}`, matching the module pattern.

---

## Data model ↔ ccid.json contract

See [`docs/OBJECTMODEL.md`](OBJECTMODEL.md) for the full schema, versioned field conventions, and disk layout.

**Invariants to enforce here:**

- `save!` must write back **every** persisted field; a field not in the struct is silently dropped on next save. When you add a field: update the struct, `save!`, `_load_image`/`_load_set`, and add a round-trip test in `app/test/runtests.jl`.
- `api/src/routes.jl` does **not** parse or mutate `ccid.json` directly. All mutations go through package functions (`delete_image!`, `init_object` → mutate → `save!`). The only exception is project discovery (`_scan_projects_raw`), which reads `project.json` directly as a lightweight listing.

---

## QC (quality-control) sidecars

A task can exit 0 yet produce output that's quietly wrong (e.g. drift correction on a reference channel
that didn't track → the canvas balloons). The **QC layer** (`app/src/qc.jl`, image-owned) lets any task
emit **advisory** findings about the output it produced.

- **Convention:** one JSON per (task, output) at `1/{uid}/qc/{funName}/{valueName}.json`
  (no-value_name tasks → `"default"`). Written via `write_qc(img, fun_name, value_name, findings; …)`,
  read via `read_qc` / `read_all_qc`.
- **Contract:** each file has a generic `findings` list — `{level ("info"|"warn"), code, short, long,
  detail?}`. `"error"` is reserved: **QC never fails or gates a task.**
- **Boundary:** the **backend computes** findings (thresholds live in Julia — one source of truth); the
  **GUI only renders** them (badge + tooltip). `api_images_meta`'s `_image_payload` exposes
  `qc = read_all_qc(img)`; `frontend/src/lib/qc.ts` aggregates into a badge (ImageTable; MetadataPanel +
  whiteboard are later phases).
- **First producer:** `cleanupImages.driftCorrect` — persists the applied per-frame drift and flags a
  registration whose own measurements contradict each other (`drift.unreliable`), frames it could not
  register at all (`drift.unregistered_frames`), a large inter-frame jump (`drift.jump`) or abnormal
  XY canvas growth (`drift.canvas_expansion`).
- **A finding is worth more when it can't be fooled by the thing it is checking.** The first three
  drift findings above read the *trajectory*, so they can only compare it against an expectation of
  how much drift is normal — which is why a movie that genuinely moves a lot looks like a broken one.
  `drift.unreliable` instead compares the registration against **itself** (`shift(a→b) + shift(b→c)`
  must equal `shift(a→c)`), needs no ground truth, and separated every movie on this machine that
  registered (0.13–0.39 px RMS) from the one that did not (24 px) by ~60×. Prefer a self-consistency
  check over a threshold on the answer whenever the task can produce one.

(Where the data *is*, as opposed to how good it is, is not a QC concern — see
*Valid box* below.)

Full design + phased plan: [`docs/todo/QC_PLAN.md`](todo/QC_PLAN.md).

---

## Valid box — which part of a store is data

A task may write a canvas larger than its data. Drift correction expands the canvas to hold the
whole trajectory and drops each frame into a **zeroed** canvas at that frame's own offset, so the
rest is padding — 3–56% of the z planes on real movies here, one going from 8 to 18. NGFF has no way to
say where the data is, so without this a consumer either treats padding as background (it will skew
any background estimate, and it borders real signal) or pays to process it.

**One question, one answer, on the store.** `zarr_utils.write_valid_box` / `read_valid_box`, under a
namespaced `cecelia` attr next to the pixels:

```python
box = zarr_utils.read_valid_box(path)                  # None  → the whole store is valid
box = zarr_utils.read_valid_box(path, timepoint=t)     # {'Z': (5, 13), 'Y': …, 'X': …}
box = zarr_utils.read_valid_box(path, level=1)         # rescaled to that pyramid level
```

Design points, each of which was a candidate mistake:

- **On the store, not in the producer's QC sidecar.** QC is advisory and task-scoped; a consumer
  would have to know *which* task padded and where its JSON lives. This is a property of the data,
  so it travels with a copy or an export.
- **`None` means "all valid",** which is every store that never padded. That is what lets a
  consumer have one code path instead of special-casing drift output.
- **The producer passes the numbers it placed the pixels with** — for drift,
  `correction_utils.drift_frame_slices`, the call the writer itself uses. Not a second derivation
  that can disagree. (Same discipline as *Calibration — three copies, one stamp* in `CLAUDE.md`.)
- **Level-0 coordinates,** rescaled on read by the same `DOWNSAMPLED_AXES` rule the NGFF scale uses;
  start floors and stop ceils so a level never crops real data.
- **One producer, but every derived store must carry it.** Drift correction is the only task that
  *computes* a box. It is not the only one that has to think about one — see the propagation rule
  below.

**Two traps.** The box is per timepoint, and each frame sits at its own offset *because* the
correction aligned them in the shared canvas — cropping each frame to its own box puts them back out
of register. Crop to a common region or not at all. And the intersection across all timepoints can
be **empty**, which is not hypothetical: it is true for 4 of the 9 movies in `kSUFux`, where the
z-drift exceeded the 8-plane stack depth.

### Propagating it — the rule for anyone writing a derived store

**A box that does not survive the pipeline buys nothing.** Drift correction writes one; what people
then segment is a *smoothed* or *AF-corrected* version of that store. If the box stops there, every
consumer sees "all valid" and the padding is processed anyway — which is exactly what happened:
`af_correct` and the (since-removed) `cellpose_correct` dropped it silently, and `smooth` carried it via
`read_valid_box(path)`, which on a per-frame box returns the **union over frames** — nearly the whole
canvas once the window drifts. The box survived in name while losing the only thing that made it
useful.

So there is **one call**, and it is unconditional:

```python
zarr_utils.carry_valid_box(src, staging)      # after the store exists; returns whether it carried
```

Never `read_valid_box` + `write_valid_box` — that is the union bug. `carry_valid_box` preserves
per-frame boxes as per-frame, and **self-refuses when the geometry moved**: it compares only the
axes the box speaks about, so a label store that legitimately dropped C still carries, while a crop,
a resize or a Z-MIP does not. A per-frame box additionally needs T at the same length, or the frame
index it is keyed by means nothing. That is why the caller never branches on its own mode —
`segment.branching` makes the same call whether it keeps Z in 3D or flattens it, and gets the right
answer both times.

**Absent is always safe.** A consumer reads `None` as "all valid" and merely does more work, never
the wrong work. The failure mode this guards against is not danger, it is *silence* — so silence is
what is forbidden: a runner that opens a `staged_store` must call `carry_valid_box`/`write_valid_box`
or carry a `VALID-BOX-EXEMPT: <why>` comment. Enforced by
`python/cecelia/tests/test_valid_box_propagation.py`, which also fails the read-without-timepoint
union pattern outright.

### Consuming it

Narrowing to the box belongs at the boundary the work goes through, once, not in each algorithm
behind it — segmentation does it in `SegmentationUtils.predict_from_zarr`, so every backend gets it
by implementing nothing (`docs/SEGMENTATION.md` → *Skipping the padding a drift correction added*).
The rule that boundary has to hold: **if it hands out several arrays for the same unit of work, all
of them narrow together.** Segmentation hands a temporal backend both a tile and a window through
time, read from different places, and narrowing one of them was enough to break it.

---

## Viewer

The browser viewer (`frontend/src/lib/webgpu`) drives image display, overlays and recording. See
`docs/todo/WEB_VIEWER_PLAN.md` for the migration and the current architecture.

---

## Linked brushing (viewer → gating)

```
Draw a region on the viewer's cell-selection layer
  → viewer POSTs /api/viewer/pick-rect {labels:[…], zLo, zHi, …}
  → Julia stores as transient "Viewer selection" pop; broadcast_ws gating:popmap
  → frontend ws.ts → gating store: tree gains the transient pop → flow plots highlight those cells
```

Julia stays the sole gate evaluator; the viewer only draws regions and displays membership.
See `docs/POPULATION.md` for transient pops.

Components: `ws.on(...)` in `onMounted`, `ws.off(...)` in `onUnmounted`.
This replaces R/Shiny's pattern of writing files + `reactiveFileReader`. No polling.

---

## WS broadcast

`broadcast_ws(msg)` in `server.jl` sends JSON to all connected browser tabs.
Clients tracked in `_ws_clients::Set` guarded by `_ws_clients_lock`.
`handle_ws` registers on connect, deregisters on disconnect.

---

## The log rail — one record shape, one tee, one way to start a child

Everything the app can say about itself reaches the bottom console, and there is exactly **one** way to
get it there: **a component reports by LOGGING, and the sink decides where that goes.** The package
emits ordinary `@info`/`@warn`/`@error` (it must stay usable headless — see *Layer boundary*); a server
installs `Cecelia.TeeLogger` to forward each record to its transport. Record shape, tee, ring and
child-process capture all live in **`app/src/log_stream.jl`**, because two servers need the identical
four and a second copy is how the two consoles would drift.

| Producer | Carrier | `source` |
|---|---|---|
| API server's own logs | `TeeLogger` → `server:log` | `backend` |
| preview worker :7656, Pluto :7660 | `spawn_logged` → the same tee | `preview` · `notebooks` |
| detached task runner :7657 | its own tee → `runner:log` on `/events` → relayed into the API ring; raw stdio inherited → terminal | `runner` |
| a task's Python | `run_py` → `on_log` → `task:log` (the task drawer) | *n/a — per-task, not console* |

### `run(cmd; wait = false)` swallows stdio — use `spawn_logged`

Julia's non-blocking `run` does **not** inherit stdio; it sends both streams to devnull
(`spawn_opts_swallow`). Long-lived children started that way (e.g. the preview worker's
`traceback.print_exc()`, its `print(..., flush=True)` diagnostics) went **nowhere at all** — not to
the console, and not to the `pixi run dev` terminal either. Anyone looking for them reasonably
assumed the terminal had them.

So `spawn_logged(source, cmd)` is the only sanctioned way to start a long-lived child. It pipes both
streams into the logger a line at a time and reassembles a Python traceback into **one** record
(header + frames in `detail`) — split per line, the frames classify as `info`, which the default view
hides, and the console would show a bare `Traceback (most recent call last):` with nothing under it.
Pinned by the *"long-lived children are spawned onto the log rail, never swallowed"* testset.

**The runner is the one deliberate exception, and must stay one.** It is spawned `detach = true`
because it has to outlive the backend; a pipe this process owns becomes a broken pipe on exactly the
restart it exists to survive. So it splits its output across two carriers — `@info`/`@warn`/`@error`
over `runner:log` to the console, and raw `stdout`/`stderr` **inherited** so it reaches the calling
terminal. Inheriting is safe where a pipe is not: the fd belongs to the terminal, not to us, and a
detached child keeps writing to it after we exit. It is also the right home for a **segfault dump**,
which the C runtime writes while the process dies — a pipe read by that same process is the worst
possible destination for the one output you most need. Both carriers are asserted by the same testset,
so the exemption cannot quietly decay back into silence. Details: `docs/RUNNER.md`.

**Corollary — "not `spawn_logged`" never means "leave it at the default".** The default for a
non-blocking `run` is devnull, so any spawn that opts out of the rail must still name its streams
explicitly. `api/dev.jl` had both of its long-lived spawns in this position and got one of them wrong:
Vite was launched with a bare `run(cmd; wait = false)` under a comment reading *"inherits stdio → Vite
logs into this terminal"*, so its build errors were discarded — and the resulting absence of Vite's
"ready" banner was written up as output buffering rather than as the discard it was. Both spawns there
are now explicit and pinned by a test.

### An exception is formatted, never interpolated

`@error "…" exception = (e, catch_backtrace())` is the most valuable line the backend can produce, and
the previous tee flattened kwargs with `"$k = $v"` — which rendered it as **857 characters of raw
`Ptr{Nothing}`** in the collapsed row. A backtrace is a thing to format (`showerror(io, e, bt)`), and
the formatted form belongs in `detail`. `log_record` also promotes the exception's first line into the
message, because half the call sites read `@warn "show_labels failed" exception = e` and the one word
that matters (`BoundsError`, `KeyError`) was otherwise a click away.

### A log line carries a sequence, because the transport is lossy

`broadcast_ws` **drops** a frame for a client whose queue is full rather than block a worker thread.
Task frames survive that — `GET /api/tasks/recent` reconciles them. Log lines had no equivalent: a
dropped line was gone, and nothing anywhere could tell it had happened. Every record now gets a
monotonic `seq` from a `LogRing`, so a client that receives `n+2` after `n` knows it missed one and
asks for the gap (`GET /api/logs/recent?since=n`). The ring also carries a `ringId`: a restarted server
counts from 1 again, and a client holding the old cursor would otherwise treat the new ring's first N
records as ones it already had — a restart that silently ate its own startup.

---

## WS message type reference

| Direction | Type | Payload | Handler |
|-----------|------|---------|---------|
| S→C | `task:status` | `taskId`, `status`, `imageUid` | `ws.ts` → task store |
| S→C | `task:log` | `taskId`, `log` (string) | `ws.ts` → task store |
| S→C | `task:progress` | `taskId`, `progress` (0–1 float) | `ws.ts` → `setProgress` |
| S→C | `task:result` | `taskId`, `imageUid`, `meta` (Dict) | `ws.ts` → image/task store |
| C→S | `task:run` | `taskId`, `projectUid`, `imageUid`, `module`, `task`, `params`, `poolName` | `sockets.jl` → `handle_task_run`; `poolName` (string) overrides the task-spec `resource_pool` when non-empty — set by the pool dropdown in `TaskRunner.vue` |
| C→S | `task:cancel` | `taskId` | `sockets.jl` → `kill_task` — kills the individual task subprocess |
| C→S | `chain:run` | `projectUid`, `chain`, `imageUids` | `sockets.jl` → `handle_chain_run` — starts a chain run in a `Threads.@spawn`; does not block the WS thread |
| C→S | `chain:cancel` | `runId` | `sockets.jl` → `cancel_chain_run!(runId)` — sets the cancelled flag; nodes check it between steps (does **not** kill currently-executing subprocesses — see `docs/TODO.md` → *Set-scope / incremental node subprocesses not killed on chain cancel*) |
| S→C | `chain:run:started` | `runId`, `projectUid` | broadcast when a chain run begins |
| S→C | `chain:run:done` | `runId`, `projectUid` | broadcast when `run_chain` returns successfully |
| S→C | `chain:run:failed` | `runId`, `projectUid`, `error` | broadcast on unhandled error in `run_chain` |
| S→C | `chain:node:queued` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params` | `server.jl` event-bus subscriber → all clients; `ws.ts` → `taskStore.addFromChainEvent`. Node submitted to its pool, waiting for a worker slot (no `startedAt` yet) |
| S→C | `chain:node:running` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params` | Same path. Fired when a pool worker actually starts the job — this is when `startedAt`/elapsed begins |
| S→C | `chain:node:done` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `params`, `result` | same path as above |
| S→C | `chain:node:failed` | `runId`, `projectUid`, `imageUid`, `nodeId`, `fn`, `status` | same path; `status` is `failed`/`skipped`/`cancelled` (the frontend maps `cancelled` to a cancelled entry, not failed) |
| S→C | `server:log` | `seq`, `ts`, `level`, `source`, `message`, `detail?` | `ws.ts` → `log.pushServer`. Everything the backend side says — its own logs and every child process's output. `seq` lets a client detect a dropped frame and refetch it; see *The log rail* |

### `task:result` — mandatory fields for filepath-producing tasks

Tasks that write an image version **must** include `valueName` and `filename` in `meta`:
```json
{ "valueName": "default", "filename": "ccidImage.ome.zarr" }
```
`ws.ts` uses these to update `img.filepaths[valueName]` in the Pinia project store **in-memory**. Without this, `valueNameSelection` widgets (e.g. in cellpose correction) show nothing — the store is only refreshed from disk when the project is reloaded. The on-disk `ccid.json` is always written by the Julia task regardless; the `task:result` is the in-session live update.

Tasks that also write OME dimensions can merge both into one return:
```julia
merge(zarr_meta, Dict{String,Any}("valueName" => value_name, "filename" => basename(zarr_out)))
```
---

## REST endpoint reference

| Method | Path | Handler | Purpose |
|--------|------|---------|---------|
| `GET` | `/api/chains?projectUid=X` | `api_chains_list` | List chain template names for a project |
| `GET` | `/api/chains/get?projectUid=X&name=Y` | `api_chains_get` | Fetch a chain template JSON |
| `POST` | `/api/chains/save` | `api_chains_save` | Write a chain template JSON (`{projectUid, template}`) |
| `GET` | `/api/pools` | `api_pools_list` | Return initialized scheduler worker pools as `[{name, limit}]`. Used by `TaskRunner.vue` and `ChainModule.vue` to populate pool dropdowns. Returns only pools that have been explicitly initialized from the cecelia config `pools` section. |

For the full chain of existing project/image/task REST routes see `api/src/routes.jl`.

---

## Language boundaries

```
Vue 3 + Pinia   →  UI only; no analysis logic
Julia            →  task dispatch, gating, statistics, REST/WS API, HPC
Python           →  image I/O, PyTorch, Cellpose
```

The Julia half is one process in production and optionally **two in dev**: the API server, and a
detached **task runner** that owns the resource pools so a backend restart does not kill work in
flight. Same code, driven through the same sink-agnostic `execute_task`/`execute_chain`; the API server
relays its frames rather than translating them. Dev-only on purpose — see [`docs/RUNNER.md`](RUNNER.md).

**Never add Rust or a fourth language.** The rationale:
Python owns all image I/O and ML regardless of what orchestrates it. The remaining work (gating, HMM,
spatial stats, clustering) maps 1:1 from R to Julia's scientific ecosystem (StatsBase, Distributions,
Distances). There is no systems performance argument for Rust when the computation is iterative research
workflows over scientific data structures.

(The distribution shell does not change this: Cecelia ships via a conda `constructor` installer +
browser, with the Julia server serving the built frontend — no Rust, no Electron, no bundled browser.
See `docs/SHIPPING.md`.)

---

## H5AD / LabelProps layout

```
{proj}/1/{uid}/labelProps/{valueName}.h5ad
```

| HDF5 group | Content |
|------------|---------|
| `/obs` | Per-cell DataFrame (`label`, population assignments) |
| `/var` | Channel metadata (var names = intensity column names in `X`) |
| `/X` | float32 n_cells × n_channels intensity matrix |
| `/obsm` | Named arrays: `spatial` (centroids), `X_umap`, `temporal` |
| `/obsp` | Sparse: `spatial_connectivities`, `spatial_distances` |
| `/uns` | Metadata: `intensity_measure`, `spatial_cols` |

Julia reads via HDF5.jl. Writes go through a **Python subprocess** (`python_bin_path()` running an
`anndata.write_h5ad` script) — anndata's encoding spec (categoricals, sparse CSR, encoding attrs) is
complex to reimplement and changes between versions, so we don't write H5AD from Julia directly.
(No in-process PythonCall — the package has no Python dependency; all Python is out-of-process.)

### LabelProps Julia reader (`app/src/label_props.jl`)

Fluent/pipeable lazy reader (`label_props(...) |> verb |> as_df`), mirrors Python `LabelPropsView`;
HDF5 opens inside `as_df` (in a `do` block) — no leaked handles. **This `LabelProps` chain is the
standard idiom for all cell-level data access** — see `docs/DATAMODEL.md` for the verbs and rules.

Intensity columns in `X`: `{measure}_intensity_{i}` where `i` is channel index and `measure` comes
from `uns.intensity_measure` (default `"mean"`).

---

## Gating engine (planned — replaces flowWorkspace/cytolib)

Lives in Julia, operates on H5AD files written by Python tasks.

- **Gate types**: rectangle, polygon, ellipse, quadrant, boolean combination
- **Population hierarchy**: tree; each node is a named population with a boolean mask over its parent
- **Gate coordinates**: always in transformed space (logicle/biex first, gates drawn on top — same convention as FlowJo)
- **Storage**: per-segmentation sidecar `gating/{value_name}.json` (NOT `ccid.json`); population tree = `{name, gate, children: [...]}`
- **Viewer integration**: *linked brushing*, not gate-drawing — the viewer draws an image region → POSTs the inside cells' label IDs to `/api/viewer/pick-rect` → Julia mirrors them as a transient population and broadcasts `gating:popmap` (see flow above + `docs/POPULATION.md`)
- **logicle transform**: port Logicle.cpp (Parks et al. 2006); Python implementations available as reference

---

## OME-ZARR dual-format

> Moved here from `CLAUDE.md` (2026-08-20).


Two layouts coexist — the reader handles both:

| Source | Layout | `multiscales` location |
|--------|--------|------------------------|
| bioformats2raw | Series wrapper: data at `zarr/0/[level]` | `zarr/0/.zattrs` |
| `create_multiscales()` | Flat: data at `zarr/[level]` | root `.zattrs` |

Detection is **structural** — does `path/0` carry a `multiscales` attr, not what the path ends in.
Both layouts have a `0/` child (a group in one, the level-0 *array* in the other), so the suffix
tells you nothing. One resolver per language, and everything goes through it:

| Language | Resolver |
|---|---|
| Python | `zarr_utils.py` → `series_base` (used by `zarr_data_to_list`/`open_as_zarr`/`open_zarr`) |
| Julia | `app/src/tasks/importImages/omezarr.jl` → `series_base` (used by `read_ome_metadata`, `update_ome_scale!`) |

Never assume one format, and never hand-roll the check — always go through the readers (see
*Image / OME-ZARR access — always go through `zarr_utils`* above).

**The trap this cost us twice:** hardcoding `zarr/0/.zattrs`. For a flat store that path *exists*
(the level-0 array's own, empty `.zattrs`), so the code doesn't error — it finds no `multiscales`
and returns silently. First it made `resync_ome_meta!` a no-op on any image with a processed
variant active; then it made `sync_zarr_calibration!` land only its OME-XML half on the 8-bit
import + crop outputs, leaving a store whose XML said `TimeIncrement="10.0"` while its NGFF t axis
said `scale: 1.0` — and the viewer, which prefers NGFF, rendered 1 s/frame.

Callers of `read_ome_metadata` should still resolve `img_filepath(img, VERSIONED_DEFAULT_VAL)` —
the `"default"` zarr, not the active one. That is no longer a layout limitation: physical size and
timing are acquisition properties, and the default is the store the importer syncs its corrections
into, so reading a processed variant would make the answer depend on what happens to be selected
for viewing.

---


---

## Repository layout

> Moved here from `CLAUDE.md` (2026-08-20) — CLAUDE.md keeps only the invariants that a session can violate.


```
cecelia-feijoa/
  app/          Julia package — Cecelia.jl (Revise-tracked) + each task's co-located Python
                runner (app/src/tasks/<cat>/<name>_run.py, run by path via run_py).
  api/          Julia API server scripts — NOT a package, NOT Revise-tracked
  frontend/     Vue 3 (Vite, TypeScript, Pinia, PrimeVue)
  python/       Installable Python package `cecelia` (pyproject.toml here) — the IO LIBRARY only:
                analysis/IO helpers (cecelia.utils) + writers. NO task runners. Top-level, sibling
                to app/. This is what an external consumer (coastal) `pip install`s.
  preview/      Task-preview worker (preview_worker.py, :7656) — resident process that runs a task's
                real compute over the visible region. Runtime process, like mcp/.
  mcp/          Python MCP observer server (read-only Claude access to a running project) — separate infra
  pixi.toml     Python env + run templates (`pixi run dev|prod|frontend|stop`)
  docs/         Extended architecture and design reference
```

**What lives where.** `api/`/`frontend/` are single-ecosystem. `app/` is **the app** — Julia *plus*
each task's co-located Python runner; `python/` is the **installable IO library** (no task code):

| Dir | Language | What it is |
|---|---|---|
| `app/` | Julia (+ task Python) | The `Cecelia.jl` package: data model, scheduler, gating, and tasks. Each task is **three co-located files** — `app/src/tasks/<cat>/<name>.jl` + `.json` + (optional) `<name>_run.py`. The `_run.py` is run by path via `run_py` (never imported), so it doesn't make `app/` an importable Python package. `Project.toml`/`Manifest.toml`. |
| `api/` | Julia | HTTP/WS server scripts (`include`d, not a package). |
| `frontend/` | Vue/TS | The browser UI. |
| `python/` | Python | The installable **`cecelia`** IO library — **no task runners**: `python/cecelia/utils/*` (zarr/OME/dim/label-props/tracking/… helpers) + `python/cecelia/writers/*` (h5ad write-side). `python/pyproject.toml` ships only `cecelia` + `cecelia.utils`. This is what coastal `pip install`s. |
| `preview/` | Python | The task-preview worker (`:7656`): runs a task's own compute over one visible region so params can be judged before a full run. Resident (17.7 s of imports), un-pooled, returns the mask block rather than writing a store. Imports `cecelia`; not part of it. |
| `mcp/` | Python | The MCP observer server (`cecelia_mcp`): read-only Claude access to a running project over stdio, talking to the Julia API. Separate infra, not part of the `cecelia` package. `pixi run mcp` / `pixi run test-mcp`. See `mcp/README.md`, `docs/ai-assist/OBSERVER.md`. |

> **⚠️ Structural shifts.** (2026-07) The Python helpers moved `app/py/` → top-level `python/cecelia/`
> and were made a pip-installable package (import name `cecelia`) so external consumers — e.g. the
> sibling `coastal` project — can `pip install cecelia` and `import cecelia.utils.zarr_utils` with no
> `sys.path`/`PYTHONPATH` hack. (Later) The **task runners moved back out** of the package into
> `app/src/tasks/<cat>/`, co-located with their `.jl`, so `python/cecelia/` is the **IO library only**
> — coastal never pulls task code. `run_py` resolves `"tasks/…"` under `app/src/` and everything else
> (e.g. `"writers/…"`) under `python/cecelia/`, and sets `PYTHONPATH=python/` so runners still
> `import cecelia.*`. Dependency split:
> the light IO deps live in `python/pyproject.toml`; the heavy/conda/per-platform deps live in
> `pixi.toml` (each pin in exactly one file). Full design: [`docs/todo/PY_PACKAGING_PLAN.md`](todo/PY_PACKAGING_PLAN.md).

**Critical**: `api/src/*.jl` files are `include`d by the server script — they are **not** Revise-tracked. Changes to them require a server restart. Only changes to `app/src/` (the Cecelia package) are picked up by Revise.

**Adding a Julia dependency to `app/`**: `Cecelia` is path-sourced by **three** separate environments,
each with its own committed `Manifest.toml` that pins Cecelia's full dependency graph — so a new dep
must be re-resolved into **all three** and all three manifests committed together, or whichever env was
missed fails to precompile Cecelia (`ArgumentError: Package Cecelia does not have <Dep> in its
dependencies`). `Pkg.instantiate()` alone does NOT do this — it honours the existing (stale) manifest;
you need `Pkg.resolve()` (the `*-instantiate` tasks below now resolve-then-instantiate for exactly this
reason). After editing `app/Project.toml` (or `Pkg.add`-ing in `app/`):

| Env | Command | Manifest to commit |
|---|---|---|
| `app/` (package + `test-pkg`) | `pixi run julia-instantiate` | `app/Manifest.toml` |
| `api/` (WS server) | `cd api && julia --project -e 'using Pkg; Pkg.resolve()'` | `api/Manifest.toml` |
| `pixi run frontend`… `pluto/` (notebooks) | `pixi run notebooks-instantiate` | `pluto/Manifest.toml` |

Miss one and it precompiles fine everywhere else but dies in that one env — which is exactly how a
`Clustering`/`NearestNeighbors` add shipped a stale `pluto/Manifest.toml` and broke every notebook.

### Data layout
```
{proj}/0/{uid}/    image data (OME-ZARR, written by bioformats2raw)
{proj}/1/{uid}/    metadata (ccid.json, labels, labelProps/)
```

### Ports
- `8080` — Julia WS/HTTP server
- `5173` — Vite dev (proxies `/ws` → `8080`)
- `7656` — Task-preview worker WS (`preview/preview_worker.py`)
- `7657` — Detached task runner (`api/runner.jl`, dev only — see `docs/RUNNER.md`)
- `7660` — Pluto notebooks server

The runner's port is **fixed and deliberately outlives the API server**, so two checkouts that share a
`CECELIA_DEV_DIR` (a worktree with a copied `.env`) share it too and cannot both run `pixi run dev`.
The second one's runner stands down with a one-line message rather than a stack trace; override with
`CECELIA_RUNNER_PORT` if you genuinely need two.

