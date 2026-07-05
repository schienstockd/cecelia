# Phase: Enforce Package / API / GUI Separation

Reference CLAUDE.md and ARCHITECTURE.md for full context before starting. This phase does not add features — it separates existing code into three layers with a hard boundary rule.

## The rule

> **Cecelia.jl must be usable headless from the Julia REPL with zero knowledge of HTTP, WebSockets, or Vue.**

If a function in `src/` references a request, a socket, a session, or a frontend-shaped payload, it belongs in the API layer, not the package.

## Target structure

This is a reference shape, not a hard spec — you've already implemented parts of this (napari integration, task manager, module pages). Where the existing structure differs from this sketch but already satisfies the boundary rule above, keep what's there and note the deviation in your audit rather than reshuffling working code to match this layout exactly.

```
Cecelia.jl/
  src/
    Cecelia.jl          # module entry, exports
    data_model.jl        # project/image/version structs
    project.jl           # project load/save/version logic
    tasks.jl              # task struct + dispatch (local/Distributed/HPC)
    io/
      zarr.jl
      anndata.jl
      gating.jl
      tracks.jl
    modules/
      <category>/<name>.jl   # analysis functions, one per file
      <category>/<name>.json # param spec, co-located with the function
  test/                  # runnable without any server, real fixture data

api/                     # Cecelia.jl is a dependency, not merged in
  src/
    routes.jl             # REST endpoints, thin — validate, call Cecelia.jl, serialize
    sockets.jl             # WebSocket task status push
    serializers.jl         # struct → JSON, JSON → struct param parsing

frontend/                # Vue, unchanged
```

## Step 1 — Audit (do this first, report back, don't edit yet)

Go through every file currently in the merged codebase and classify each function as one of:
- **PACKAGE** — pure domain/computation, no I/O to a client
- **API** — request/response shaping, route handlers, socket message handling
- **AMBIGUOUS** — flag these explicitly, don't guess

Output a table: `file → function → classification → reason`. Pay particular attention to:
- task dispatch code (the boundary between "run a task" [package] and "report task status to a client" [API] is usually where the original R `taskCore.R`/`taskLauncher.R`/`taskProcess.R` split should map)
- the module system (`modules/<category>/<name>.jl` + `.json` spec — the spec format itself is package-owned; *serving* it to Vue is API-owned). Cecelia.jl must validate `params` against the `.json` spec constraints (type, min/max, required, enum, etc.) before running, regardless of caller. Vue's form already prevents invalid input at the UI layer, but the REPL has no such guardrail — a REPL call with an out-of-range or wrong-type param must fail with a clear validation error, not run silently or crash deep in the function body. This validation lives in `run_task`/`run_tasks`, not duplicated per-module.
- anything touching the napari IPC bridge — napari is now WebSocket-controlled, but the **package must still be able to drive it directly from the REPL**, the same way `vignettes/run_animation.Rmd` drives napari from R with no Shiny involved. The websocket *transport* (connection lifecycle, multi-client broadcast) is API-owned; the napari *control protocol* (what messages mean, what commands exist) is package-owned. Cecelia.jl should open its own socket connection to napari and send/receive these messages directly when called from the REPL — it should not require the `api/` server to be running.
- Python/PythonCall interop (segmentation, torch, zarr) — same rule as napari: the PythonCall setup and call wrappers are package-owned, so REPL users can run segmentation or any Python-backed module function standalone. If this currently lives under `api/` or is tangled with server startup, flag it explicitly in the audit.
- logging — per-image/per-task log files (`writeLog`-equivalent) must be package-owned. A REPL-run task and a GUI-run task produce identical logs in identical locations; logging is not something the API layer adds on top.
- HPC/SSH job submission — package-owned (mirrors original `R/sshUtils.R`, which was never part of `inst/app/`). Easy to misclassify as "infrastructure = API" — it isn't, REPL users submit HPC jobs directly too.

## Step 1.5 — REPL-runnable contract (non-negotiable for every module function)

Every module function must be callable from the REPL exactly like the original `cciaObj$runTask()` (single image) and `runTasks()` (image set, batch). This is not optional cleanup — it's a hard requirement carried over from the R package, and several vignettes depend on this pattern. Concretely:

```julia
# single image
run_task(project, image_id; fun_name="behaviourAnalysis.cumulativeChange", params=params)

# image set / batch
run_tasks(project, image_ids; fun_name=..., params=..., parallel=true)
```

Both must work with `api/` not running, no websocket, no Vue. The API layer's task endpoints are a thin wrapper that call these same functions and stream status over the socket — they don't contain separate logic.

## Step 1.6 — Lightweight concurrency guard

Concurrent REPL + API access to the same project isn't being solved properly this phase (see Out of scope), but the original R package had a naive lockfile mechanism on the persistent object (`reactivePersistentObject.R`: `lockFile()`, `startTransaction()`, `finishTransaction()`) to prevent silent corruption. Port the equivalent to `data_model.jl`/`project.jl`.

D and Claude (chat) sketched a starting point: a lock file containing `pid:timestamp`, stale-lock reclaim if the owning process is dead or the lock is older than a threshold, ownership check before release, and a `with_transaction(f, obj) do ... end` wrapper so the lock can't leak if an exception is thrown mid-task — which the original R version was vulnerable to. Treat this as a reference sketch, not a spec — use your own judgment on the right primitives here (e.g. whether `kill -0` PID-liveness is the right staleness check, whether timestamp-based staleness alone is safer/simpler, whether this belongs on `PersistentObject` generally or only on project-level state). Flag any tradeoffs you see.

## Step 1.7 — REPL workflow shape (reference, not a hard rule)

The vignette `.Rmd` files show the actual day-to-day REPL pattern researchers use, beyond just `runTask`/`runTasks`. Worth preserving the shape of this workflow, not the literal R API:

1. **Init**: load package + set environment (`devtools::load_all()` + `cciaUse(path, initConda=FALSE)`) → Julia equivalent is presumably `using Cecelia` + a config/env setup call. Should be two clear, separate steps: load code, then point at a workspace.
2. **Object resolution**: `initCciaObject(pID, uID, versionID, ...)` resolves a project + a specific object (single image *or* an image set) by ID. `uIDs <- names(cciaObj$cciaObjects())` then expands a set into its individual member IDs — this expand-a-set-into-members step shows up constantly and should have a clean Julia equivalent.
3. **Run**: `runTask`/`runTasks` as already covered.
4. **Get data back out**: this is the part not yet explicitly covered — `popDT()` (population data table, filtered, with caching/`flushCache`, across a set via `uIDs`), `summary()` (experiment metadata), `tracksMeasures()` (track-level measures). These are read accessors, not task runs, and need to be just as REPL-native and just as set-aware (single object or `uIDs=...` across a set) as the task functions.

Also worth noting: real param lists in these vignettes are messy — researchers comment out alternatives and iterate (`# numStates = 3,` next to the active `numStates = 2,`). That's normal exploratory workflow, not a code smell to design against. The REPL ergonomics should optimize for fast rerun-with-different-params, not for ceremony.

Treat this section as describing the *shape* of the workflow to preserve, not a spec to replicate literally — use your judgment on the cleanest Julia idiom for each step.

## Step 2 — Extract Cecelia.jl

- Move PACKAGE-classified code into `Cecelia.jl/src/` per the target structure above
- Every module function keeps the `{struct, json spec, run(params, task_object)}` pattern already established
- Write a smoke test: load a real project, run one module function, assert output — all via `using Cecelia`, no API, no Vue
- No function in `src/` takes an `HTTP.Request` or returns a pre-serialized JSON string — it returns a Julia struct/value, API layer serializes it

## Step 3 — Extract the API layer

- `api/` depends on `Cecelia.jl` (Pkg dependency, not file copy)
- Route handlers do: parse request → call into Cecelia.jl with native types → serialize result → respond
- WebSocket task-status push subscribes to whatever task-state mechanism Cecelia.jl exposes (e.g. a Channel or callback) — don't let Cecelia.jl know sockets exist
- For AMBIGUOUS items from the audit: default to splitting them — push the computational part into Cecelia.jl, leave a thin wrapper in `api/`

## Step 4 — Verify

- Cecelia.jl test suite passes with `api/` not loaded at all
- Vue frontend works unchanged against the new `api/` (routes/payloads identical to before this phase)
- A REPL call to `run_task` with an invalid param (wrong type, out of range, missing required field) fails with a clear error citing the spec constraint — does not run, does not crash inside the module function
- Spec is single-sourced from Cecelia.jl's `.json` files; Vue never hand-maintains param definitions independently
- Update ARCHITECTURE.md with the finalized layer boundary and the audit table from Step 1

## Out of scope this phase

No new features, no Rust/Julia/Python boundary changes, no UI changes. Pure structural extraction. Concurrent REPL + API access to the same project is a known constraint, not handled here — the user is responsible for not running two processes against one project simultaneously.
