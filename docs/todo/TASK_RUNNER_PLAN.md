# Detached task runner — plan

**Status:** Phases 1 and 2 BUILT and verified (2026-08-13), dev-only. Phase 0 was a dead end and is
recorded as one. **The durable "how it works" now lives in [`docs/RUNNER.md`](../RUNNER.md)** — read
that first; this file is kept for the decisions and the routes not taken, which the reference doc does
not carry.

What is left: Phase 3's target badge (moot while dev-only — see D3d), the on-disk spool (D4, deferred),
and Phase 4's remote target.

## Goal

Move task execution out of the API server process, so **restarting the backend does not lose work in
flight**. A separate long-lived **runner** owns the resource pools, the task registry and the chain
executor; the API server becomes a client of it and can restart, crash, or switch worktree underneath
a running segmentation.

Driver is dev, and — settled in D3d after building it — that is *all* it is. `api/src/*.jl` is not
hot-reloaded, so most backend edits mean a restart, and a restart killed every running task. The
"processing continues after the app is closed" idea (D3b) was explored and dropped: a prod install has
no Restart button, so there is no benefit there to protect, and a prod user leaves the app running
anyway. The job record still carries a `target` so a remote runner (Phase 4) needs no reshaping.

Not a goal: making the *first* run faster, changing what a task computes, or building the remote
target. See *HPC — what this must not preclude*.

---

## Why this is possible at all

A task already externalises nearly everything it needs. This is not a coincidence — the system was
built so the **browser** can disconnect and recover. This plan is the same seam one layer down, so
that the **server** can:

| Already on disk | Where |
|---|---|
| Task params | `<image>/tasks/<fun>.json` (`task_run_dir`, `run_py`) |
| Task log | `{img._dir}/logs/{fun_name}.log` (`scheduler.jl`) |
| Results | ccid.json / zarr / h5ad — the task's real output |
| Chain state + resume | `<project>/settings/chains/runs/<run_id>/run.json`, `params_hash`, `_reset_stale_nodes!` |
| Terminal outcomes, replayable | `task_outcomes.jl` → `GET /api/tasks/recent`, `note_task_started!` |
| Both-directions reconcile | `frontend/src/utils/taskReconcile.ts`, `api/task_console.jl` |

The old R version had this property by accident: `TaskLauncher::runLocalTask` built a command string
that a **fresh R process** ran from a params file (`docs/archive/` context: `R/taskLauncher.R`). Julia's
`_run_task` runs *inside* the server instead. That is the whole regression, and the reason HPC felt
like a different thing rather than another target.

### The bug this sits next to (fix independently)

`_stop_children_for_exit` (`api/src/app_api.jl`) stops napari / preview / notebooks but **does not
touch task subprocesses**, and in-app Restart is `exit(RESTART_EXIT_CODE)`. So a Settings → Restart
during a segmentation most likely leaves an **orphaned** python child still holding the GPU and still
writing labels, with the Julia post-step (`register_label_files!`, `write_qc`, `append_run_log!`) never
running — output on disk, unregistered. Ctrl-C on `pixi run dev` kills it (process group);
`pixi run stop` kills it (`_kill_listeners_on_port(8080)` → `_kill_tree`).

*Read from source, not tested.* File it as a `docs/TODO.md` item and fix it whether or not this plan
proceeds: today's restart is not "the task is lost", it is "the task is loose".

---

## Decisions (locked 2026-08-13)

### D1 — The runner owns execution. The app does not get a second scheduler.

Pools are per-process. If the app can still execute tasks itself, `gpu = 1` silently becomes `gpu = 2`
and the whole point of the `gpu` pool is gone. So once a runner exists, **all** GUI-launched task and
chain execution goes through it.

**The in-process path stays** — `run_task` / `run_chain` remain the library API, used by the REPL
(`docs/REPL.md`) and by the package testsets, which must keep running with no server and no runner.
The runner is a *server around* `scheduler.jl`, not a fork of it. One implementation, two entry points.

### D2 — The runner is a resident child, addressed like the ones we already have.

Copy `app/src/preview.jl` / `app/src/napari.jl` verbatim in shape: fixed port, `send(w, msg)` one
message one reply, launch-then-ping, **adopt if already listening**, protocol-version guard.

- Port **7657** (7655 napari · 7656 preview · **7657 runner** · 7660 notebooks).
- Surfaced in `api_diagnostics` as `runnerPort`. That is load-bearing: `api/test/runtests.jl` asserts
  `length(child) == 3` against `_stop_children_for_exit`, so adding the port **fails the test** until
  the exit wiring is written — which is exactly the guard that stopped the last zombie shipping.
- `RUNNER_PROTOCOL` follows `PREVIEW_PROTOCOL`'s **behavioural** bump rule: bump whenever an adopted
  older runner would answer differently — including a *bug fixed inside it*.

### D3 — Lifecycle: survives Restart always; survives Quit only when it has work.

Every other resident child is stopped by `_stop_children_for_exit`, which both Quit **and** Restart
call. The runner must split them:

| Route | Runner |
|---|---|
| `POST /api/app/restart` (Settings → System, dev) | **left running** |
| `POST /api/app/switch-worktree` | **left running** (and now definitely stale — see D5) |
| `POST /api/app/shutdown` (Quit button, sidebar + Settings) | **idle → stopped; busy → see D3b** |
| `pixi run stop` ("stop everything") | **stopped** — add 7657 to all three OS variants |
| `pixi run stop-backend` | **left running** |
| new `pixi run runner` / `pixi run stop-runner` | explicit dev control |

Restart — the thing that happens twenty times a day in dev — no longer costs a segmentation, and a dev
gets independent control from the shell. A normal user with an idle queue never thinks about it: it
starts with the app and dies with the app.

Implementation note: `_stop_children_for_exit` currently serves both routes. It needs a
`stop_runner::Bool` argument (or a sibling `_stop_children_for_restart`), and the runtests assertion
above has to be taught which children belong to which route, or it will pass while the asymmetry is
broken.

### D3b — ~~The runner is launched detached, so quitting the app can leave work running.~~
**NOT BUILT — superseded by D3d (2026-08-13). The process side exists; the feature does not.**

*Kept because the reasoning below is still correct if the premise ever changes, and because the
`detach = true` launch it argued for is in the code. What changed is who it is for: with the runner
dev-only, "quit and keep processing" has nobody to serve — a developer restarts, they do not quit.*



Launch it in its own process group / session (`setsid`, `CREATE_NEW_PROCESS_GROUP` on Windows) with no
controlling terminal — so it survives the app exiting rather than dying with it. Then Quit with work in
flight is a **choice**, not a loss:

> *3 tasks still running.* **Quit and keep processing** · **Stop everything and quit** · Cancel

On next launch the app **adopts** it (D2's adopt-if-listening, already the pattern) and reports what
happened while it was away — the outcome bank (`recent_tasks`) plus the spool already hold exactly that,
so this is a read, not new bookkeeping.

This is the production answer to "run things while the app is closed" **without installing a service**:
no boot integration, no privileged install step, no fourth thing in the conda bundle. What it does not
buy: surviving a reboot or a logout, and no notification while the app is closed (see D10).

The real hazard is invisibility — a detached process holding the GPU and writing into projects with no
UI attached. Non-negotiable mitigations: a PID/port file in the spool dir, the runner listed by
`pixi run stop`, a System-panel row that shows an adopted runner as adopted, and a first-launch "while
you were away" summary. A runner you cannot find from the GUI is a bug, not a feature.

### D3c — The runner exits when it has no work and no audience.

Launched detached, nothing stops it. Left alone it is a Julia process holding GPU memory with no
window, no cancel and no way to find it, **forever**. The work finishing is fine; the idle process
afterwards is not.

So it exits after `RUNNER_IDLE_EXIT_SECONDS` (600) with **neither work nor a subscriber**. Both
conditions are load-bearing: work alone would kill it between two tasks of a batch, and a subscriber
alone would kill it during the ~45 s of a backend restart — the exact thing it exists to survive. The
cost of being generous is one cold start if you come back after a long gap.

Plus a `runner.json` (pid, port, commit) in the config dir, so a stray runner is findable rather than
folklore.

### D3d — The runner is DEV-ONLY, and that is not a temporary state.

The benefit is "a backend restart does not cost a running task". **A production install has no Restart
button** (`v-if="diag?.dev"`, both the sidebar and Settings), so the benefit is unreachable there —
while every failure mode is a prod problem: an idle process with no window, no cancel, nothing to find
it by. In dev there is a terminal and `pixi run stop-runner`.

And a prod user does not need it. They leave the app running; **closing the browser tab was never what
stopped a task** — the app's own orientation tour says so. The gap D3b tried to fill is not a gap.

`runner_enabled()` therefore requires `is_dev_session()`, and the Settings toggle hides outside dev
like the Restart button it sits under. Asserted in `app/test/suite.jl`, because "it is only a setting,
what harm" is exactly how this drifts back on.

If someone does ask for overnight processing, D3b is a day or two — the detached launch is already
there. Do not build it before they ask.

### D10 — The regress ends at the filesystem. Do not ship a third process to watch the second.

"Who monitors the monitor" has no answer in userspace, and the temptation is to add one more daemon.
D4's spool already is the monitor: **state lives on disk, and whoever is alive reads it.** A job stays
`queued` through a runner crash and is picked up by the next runner that starts; a terminal outcome is
banked and replayed to whoever asks later. A crash costs the one job in flight — the same as today, with
the server.

A watchdog would add exactly two things, and each has a better home:

- **Restart the runner if it dies** → the OS already does this. Ship a *documented, opt-in* systemd user
  unit / launchd agent / Windows service for someone deliberately turning a box into a processing
  server. That is the same host-side work as the eventual remote target (see *HPC*), so it is one
  feature at two lifetimes — not a second design.
- **Tell me when it finished while the app was closed** → that is a *notifier*, not a supervisor. Out of
  scope; revisit only if the detached mode gets real use.

Locked so a future session does not "improve" this by adding a supervisor.

### D4 — Durable spool on disk is the source of truth; the socket is only for promptness.

> *Sequencing note (2026-08-13): the design below stands, but Phase 1 ships without it — see the
> deviation recorded under that phase. Nothing in Phase 1 may assume the spool exists.*

Both ends must be able to restart, so the queue cannot live only in a socket or only in memory.

```
<config_dir>/runner/
  jobs/<job_id>.json          submitted job: target, fun_name, project uid, image uids,
                              params, pool, chain_run_id, submitted_at, submitter commit
  status/<job_id>.json        queued | running | done | failed | cancelled, timestamps, result
  cancel/<job_id>             touch-file — cancel that survives both ends restarting
```

Params and logs are **not** duplicated here — they stay where they already are, under the image
(`<image>/tasks/`, `{img._dir}/logs/`). That matters for HPC: those per-image artifacts are what a
stage-in/stage-out would ship, and they should not fork.

`config_dir()` resolves to `CECELIA_DEV_DIR` in dev and `~/.cecelia` in prod, so a dev runner and an
installed runner get **separate queues for free** — no extra rule, and no chance of a dev job landing
in a user's install.

### D5 — Staleness is reported, never prevented, and never auto-fixed.

The runner pins the code it started with. That is the point, and it is also the tax: you will fix a
bug, hit Run, and get the old behaviour. `preview.jl`'s own comment already documents this class for a
much smaller worker — *"invisible to any check but this one"*.

`api_diagnostics` already has the mechanism (`commit`, `commitCurrent`, `stale`). The runner reports
its own `commit` + `RUNNER_PROTOCOL` + `PY_CONTRACT_VERSION`; the System panel row shows both and
offers **Restart when idle** — drain the queue, then relaunch. **Never auto-restart a runner with work
in flight**, and never silently: a job's result carries the runner commit that produced it.

### D6 — The runner does not run Revise.

Tempting (app/src edits would reach it for free) and wrong: redefining a task function mid-run gives
you a run half on old code and half on new, with nothing anywhere saying so. That is the exact failure
mode this plan exists to remove. The runner pins, reports (D5), and is restarted deliberately.

### D7 — Task preview never goes through the runner.

Already an invariant: a preview that queues behind a full segmentation is not a preview
(`app/src/preview.jl`, `docs/todo/TASK_PREVIEW_PLAN.md`). Restated here because "two resident Julia-ish
workers, let's unify them" is the obvious wrong idea and it would be a regression, not a cleanup.

### D8 — Background jobs (`jobs.jl`) stay in the app.

Export / import / data patches are project- or bundle-scoped, hold no pool slot, and import has no
project at all until it finishes (`docs/JOBS.md`). Moving them drags `project_io.jl` across the
boundary and buys nothing. *Open question:* a 40-minute export also dies on restart — if that turns out
to hurt, the answer is probably a second, dumber spool, not putting them in the task runner.

### D9 — `target` exists on the job record from day one; the UI shows a badge, not a dead selector.

> *Superseded in part by D3d: with the runner dev-only there is no user-facing badge to ship. The
> field stays on the record.*


`target: "local"` on every job. The Task Runner shows it as a **read-only badge** beside the pool chip
— which earns its place with one target, because in dev it answers "did this run on the runner, and on
which commit?". It becomes a selector when a second target exists, not before.

---

## Architecture

```
browser ──ws──► API server (:8080) ──ws──► runner (:7657) ──► pools ──► _run_task ──► run_py ──► python
                    │  relay only              │
                    │                          └── <config_dir>/runner/{jobs,status,cancel}
                    └── on (re)connect: reconcile from the runner's snapshot + recent
```

**The app becomes a relay, not an origin.** The runner emits the same frames the scheduler emits today
— `task:log` / `task:progress` / `task:status` and the four `chain:node:*` events — and the app
forwards them to browsers verbatim. No new client-facing protocol; `ws.ts`, the task store and the task
console do not change.

**Reconcile is the same pattern, one layer down.** The frontend already recovers from dropped frames by
reconciling `GET /api/tasks` + `GET /api/tasks/recent` in *both* directions. The app now does exactly
that against the runner on connect. Terminal outcomes are already banked and replayable
(`task_outcomes.jl`), and the "sinks, not producers" rule for banking them still holds — the sinks just
move into the runner.

**Pools become per-runner.** `list_pools` / `pool_status` / `set_pool_limit!` proxy to the target's
runner; `PoolThrottle.vue` is unchanged as long as `/api/pools` proxies. `set_pool_limit!`'s merged
write to `custom.toml` happens on the **runner's** config — correct, since that is the box whose cores
are being rationed, and a live design consequence for a remote target.

---

## HPC — what this must not preclude

Not being built. Folded in now because two decisions are cheap today and expensive later.

**Constraint 1 — the job record must be project-relative, never host-absolute.** Store project uid +
image uids + value names + params. Do **not** store `/home/dominik/...`. `_run_task` already builds
`im_path` itself from the ccid; keep that resolution on the *runner* side, against the runner's own
`projects_dir()`. This is what the R version's `envParams("local")$dirs$task` vs `envParams("hpc")$dirs$task`
was doing by hand, and it is the difference between a one-line resolve and a rewrite.

**Constraint 2 — the runner's bind address is config, not a loopback constant.** But a non-loopback
bind must not ship without auth. There is a precedent to copy rather than invent:
`_host_is_loopback` / `replEnabled` / `replAvailable` gate the REPL console exactly this way.

**The two futures are very different, and it is worth knowing which one before designing the target
abstraction:**

- **Shared storage** — the internal box mounts the same projects dir. Then a remote target is nearly
  free: the same runner, elsewhere, with `target` as a scheduling hint. The `network` pool already
  exists for this and has no tasks assigned (`docs/SCHEDULER.md`).
- **Separate storage** — real HPC. Then `target` is a **filesystem boundary**, and every job needs a
  stage-in/stage-out phase. The R version paid this per module (`upload.R` / `retrieve.R` under each of
  `importImages/`, `segment/`, `tracking/`, `clustPopulations/`, `spatialAnalysis/`, `pixcl/`) — that
  per-module cost is the honest estimate, and it is a separate project.

The local runner exercises **neither**. It buys the dispatch abstraction and close to nothing else
toward HPC; claiming otherwise would be the mistake.

---

## Phases

### Phase 0 — ~~try the cheap fix first~~ **DONE 2026-08-13: the premise was wrong. Do not retry.**

The idea was that most restarts are `api/src/*.jl` edits which Revise does not track, so making that
layer `includet`-tracked would remove most of them. Built, tested live, **reverted**. Two findings, in
the order they were found:

1. **`includet` does not follow nested `include` calls.** `api/dev.jl` already `includet`s
   `src/server.jl`, but its 21 `include("*.jl")` sub-modules were invisible to Revise — which is the
   real reason `api/src` never hot-reloaded. Routing them through `includet` (a one-line shim, plain
   `include` when Revise is absent) does fix the *tracking*: all 21 then appear in
   `Revise.watched_files` with parsed signatures.

2. **…and it changes nothing, because the dev server has no revision trigger at all.** Revise's
   watcher fills `revision_queue`; the thing that *drains* it is a hook on the **REPL prompt**.
   `dev.jl` launches the backend as `julia -e "using Revise; includet(...)"`, which blocks forever in
   `HTTP.listen`. No REPL, no `revise()`, queue fills and sits. Verified live: an edit to
   `api_diagnostics` never reached `GET /api/diagnostics`; the one time a reload did take effect in a
   test harness was the run where `Revise.revise()` was called explicitly.

**The consequence worth carrying forward:** `app/src` is tracked the same way and has the same missing
trigger, so **"Revise hot-reloads `app/src` on save" is not true in the running dev server either** —
it would only ever have worked from a REPL. `py_runner.jl`'s hedge ("Revise does not always reload
`app/src` … under a live server") is the same observation, recorded as an occasional glitch.

A 1 s background `Revise.revise()` poller in `start()` would close it. Deliberately **not** done: it
was scope creep on the way to something else, and it invites a worse failure mode — a *partially*
reloaded server, where a `const` or struct edit silently doesn't apply and you cannot tell that from
your own bug. Worth doing on its own merits, as its own change, with its own decision.

> **Net effect on this plan: the case for the runner is stronger.** Backend restarts are not a dev-loop
> wart to be optimised away — they are how this server works. So stop making them cost a running task.

### Phase 1 — the runner, tasks only

Runner entry (`app/src/runner/`, `api/runner.jl` launch script) + protocol (D2);
`handle_task_run` / `task:cancel` / `task:restart` go through it; `/api/tasks`, `/api/tasks/recent`,
`/api/pools` proxy. Chains still run in-app. System panel row + `pixi run runner` / `stop-runner` +
the Quit/Restart asymmetry (D3) land here — without them the thing is not controllable.

> **Deviation from D4, taken 2026-08-13: no on-disk spool in Phase 1.** This phase's checkpoint needs
> none of it — the runner's own in-memory `list_tasks()` / `recent_tasks()` already answer "what is
> running" and "how did it end", and they are already published. The spool earns its place when a job
> must survive the **runner** restarting (a queued job outliving a crash, an outcome replayed to a
> client that missed it while the runner was down), which is Phase 2/3. D4 stands as the design; it is
> sequenced later, not dropped. What Phase 1 does keep from it: the runner reports a port + pid so the
> app can find, adopt and stop a process it did not start.

> **Also settled here: the runner runs in the `api/` environment** (`api/runner.jl`), not a new one.
> `Cecelia` is path-sourced by three environments, each with its own committed `Manifest.toml` that has
> to be re-resolved together (`CLAUDE.md` → *Adding a Julia dependency to `app/`*). The runner needs
> exactly what the API server needs — Cecelia, HTTP, JSON3 — so a fourth manifest would be pure
> maintenance cost for no isolation.

> **Checkpoint: MET 2026-08-13.** Verified twice, and the two runs cover different halves.
>
> *Headless, isolated pair (backend :8081 / runner :7697, throwaway config):* a task submitted through
> the real `task:run` path executed in the runner's registry; the backend was **`kill -9`'d** mid-run
> (harder than a restart) and the task kept going; it **finished while the backend was dead**, leaving
> nothing on the event stream at all; a restarted backend adopted the runner and reported
> `started_at`/`finished_at` **byte-identical** to the runner's own record. That last part is the
> whole reason the timestamps are passed through rather than re-derived.
>
> *In the GUI (Dominik):* a real **cellpose segmentation survived Settings → Restart** — the case the
> headless run could not reach, because it is the one with a Python child and a Julia post-step
> (`register_label_files!`, `write_qc`, `append_run_log!`) on the far side of the restart.
>
> Two bugs came out of driving it that no test had caught, both only reachable by running two
> instances at once: only the *server* honoured `CECELIA_RUNNER_PORT` (so an overridden client pinged
> a dead port and fell back to in-process **forever** — a launch that looks fine and runs nothing on
> the runner), and `/ping` reported the constant rather than the port it had bound.
>
> Still unverified: the worktree switch, and Quit-stops-it (asserted in source, never executed).

### Phase 2 — chains

Move `run_chain` behind the boundary. This is the bigger half: barriers, incremental watchers and the
event bus all live where execution lives, and `api/src/server.jl`'s four `chain:node:*` subscriptions
become a relay from the runner. Feasible because chain runs are already disk-durable and resumable —
but it is "move `chain.jl` behind a process boundary", not "wrap a function".

> **Checkpoint:** restart the backend mid-chain; the Live tab repopulates from the runner with correct
> per-node status, and a set-scope barrier still releases.

### Phase 3 — surfacing

Target badge (D9), staleness row polish (D5), docs promotion: `docs/SCHEDULER.md` gains a "where this
runs" section, `docs/JOBS.md` gains the third mechanism, `docs/ARCHITECTURE.md`'s language table gets
the process split, and the durable half of this file moves to `docs/RUNNER.md`.

### Phase 4 — not scheduled

Two things, gated on the shared-vs-separate-storage question above: a second **target**, and the opt-in
**service unit** (D10) that turns a box into a runner nobody has to launch. Same host-side work.

---

## Touchpoints

| File | Change |
|---|---|
| `app/src/runner/*.jl` (new) | spool format, job record, runner server, client |
| `runner/runner.jl` (new) | launch script, `-t auto`, pins its commit |
| `app/src/tasks/scheduler.jl` | unchanged as a library; gains the runner as a second caller |
| `app/src/tasks/chain.jl` | unchanged as a library (Phase 2 moves its *caller*) |
| `api/src/sockets.jl` | `handle_task_run`, `handle_task_cancel`, `handle_task_restart`, `handle_chain_run`, `handle_chain_cancel` → submit + relay |
| `api/src/server.jl` | the four chain-event subscriptions become a relay from the runner |
| `api/src/app_api.jl` | `_stop_children_for_exit` splits Quit vs Restart (D3); `api_diagnostics` gains `runnerPort` + runner commit |
| `api/src/routes.jl` | `/api/tasks`, `/api/tasks/recent`, `/api/pools`, `/api/pools/set` proxy |
| `api/test/runtests.jl` | child-count assertion (currently `== 3`) must encode the Quit/Restart split |
| `pixi.toml` | `runner` task; `stop-runner`; port 7657 into `stop` — **three OS variants each** |
| `frontend/src/modules/SettingsModule.vue` | a `svc-row` for the runner: state pill, port, commit, adopted-vs-ours, Restart-when-idle |
| `frontend/src/components/ConfirmButton.vue` callers (Quit) | the busy-runner three-way prompt (D3b) |
| `frontend/src/tasks/TaskRunner.vue` | target badge beside the pool chip |
| `docs/SCHEDULER.md`, `docs/JOBS.md`, `docs/ARCHITECTURE.md`, `docs/DEV.md` | the process split |

---

## Risks

- **Cold start is Julia, not Python.** The preview worker's adopt-if-listening exists to dodge 17.7 s of
  Python imports; a runner's cold start is Julia load + precompile, which is worse. Mitigation is D3 —
  start it *with the app*, never lazily on the first task. Measure it; if it materially delays app
  start, consider starting it in the background after `/api/health` is up.
- **Two Julia processes.** Double the resident memory and the depot pressure, and the runner needs
  `-t auto` too. Unmeasured.
- **The tests must keep passing with no runner.** The pool/chain testsets run in-process (D1). If any
  of them ends up needing a runner, the boundary is in the wrong place.
- **Windows / macOS.** `_kill_listeners_on_port` already handles all three; the `pixi.toml` stop tasks
  are duplicated per OS and the new port has to land in each.
- **Two-process debugging.** A failure now has two logs. The server-log tee (`server:log` frames,
  `SERVICE_PANEL_PLAN.md` D6) should carry the runner's too, or the console lies by omission.
- **A detached runner is a process users can lose** (D3b). Every mitigation there is load-bearing; the
  failure mode is a machine quietly pinned at 100% GPU with no window open. Worth deciding whether
  detached-on-quit is **opt-in** for the first release rather than the default.

## Open questions

1. Phase 0's number — does `includet` on `api/src` make this optional?
2. Shared vs separate storage for the eventual second box (decides D9's shape, see *HPC* above).
3. Do background jobs (export/import) need the same durability (D8)?
4. Does the runner get its own `custom.toml` pool limits, or read the app's? D4 implies its own; that is
   right for a remote box and slightly surprising locally.
5. Is detached-on-quit (D3b) the **default** or opt-in for the first release? Default is the better
   feature; opt-in is the safer first ship, given the invisibility hazard.
