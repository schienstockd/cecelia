# The detached task runner

**A development tool.** It moves task and chain execution into a second Julia process, so restarting
the backend does not kill work in flight. `api/src/*.jl` is not hot-reloaded (see `CLAUDE.md` →
*Hot reload*), so a backend edit means a restart — and a restart used to cost you a running
segmentation. This is the fix: not fewer restarts, but restarts that cost nothing.

Design record, including the routes not taken: [`docs/todo/TASK_RUNNER_PLAN.md`](todo/TASK_RUNNER_PLAN.md).

---

## Using it

```bash
pixi run dev            # then Settings → System → "Run tasks in a separate process"
pixi run dev-runner     # same thing with CECELIA_RUNNER=1, for a one-off session
```

The setting (`[runner].enabled` in `custom.toml`) sticks; `CECELIA_RUNNER` overrides it for one
session and the toggle disables itself with a hint when it is set — a control that silently would not
apply is worse than no control.

| | |
|---|---|
| Port | **7657** (7655 napari · 7656 preview · 7660 notebooks) |
| Start | with the backend, or `pixi run runner` standalone |
| Stop | `pixi run stop-runner`, `pixi run stop`, Quit, or Ctrl-C on the supervisor |
| Survives | **Settings → Restart** and the worktree switch |
| Watch it with no backend | `CECELIA_PORT=7657 pixi run console` |

### The loop this changes

**Settings → System → Restart replaces `Ctrl-C` + `pixi run dev`.** It relaunches the backend *and*
gives you a fresh napari bridge, preview worker and Pluto — the only thing it leaves running is the
runner. The full change → what-to-press table is in [`docs/DEV.md`](DEV.md); the runner-specific part:

| you changed | what to press |
|---|---|
| anything except task code | **Restart** |
| task code — what actually runs on an image | **Restart**, then **Restart** on the runner row |

The runner keeps the code it started with. That is the point, and it is the tax: the Settings → System
row shows **"old code"** with its commit when it is behind the backend, because otherwise "my fix isn't
working" and "my fix isn't loaded" are indistinguishable. Its Restart refuses while it still has work;
press again to force.

If you are iterating on task code and would rather not think about it, turn the toggle off — tasks then
run in the backend and Restart is enough for everything.

### If it is not there when you press Run

The task **runs in the backend instead** — it works and finishes normally, it just dies with the next
Restart, which is the one thing you enabled the runner to avoid. That is a surprise worth surfacing, so
the Run panel shows **"Task runner down"** with a **Start** button whenever the runner is enabled and
not answering. Nothing else on that page would tell you.

It cannot go missing while you are working: the idle exit requires **no subscriber**, and the backend
holds one open. So this only appears after `pixi run stop-runner`, or if the runner crashed.

---

## Why dev-only

The benefit is "a restart does not cost a running task". **A production install has no Restart button**
(`v-if="diag?.dev"`, both the sidebar and Settings), so the benefit is unreachable there — while every
failure mode is a prod problem: an idle process with no window, no cancel, nothing to find it by. In dev
there is a terminal and `pixi run stop-runner`.

And a prod user does not need it. They leave the app running, and **closing the browser tab was never
what stopped a task** — the app's own orientation tour says so.

So `runner_enabled()` requires `is_dev_session()`, and the toggle hides outside dev. Asserted in
`app/test/suite.jl`, because *"it is only a setting, what harm"* is exactly how this drifts back on.

"Quit and keep processing" (plan D3b) is **not built**, on purpose. The detached launch it needs is
already there, so it is a day or two if someone actually asks for overnight processing — but not before.

---

## How it works

```
browser ──ws──► API server (:8080) ──ws──► runner (:7657) ──► pools ──► _run_task ──► run_py ──► python
                    │  relay only              │
                    └── on (re)connect: reconcile from /api/tasks + /api/tasks/recent
```

**One implementation, two callers.** `execute_task` and `execute_chain` (`app/src/runner/execute.jl`)
are sink-agnostic: scope dispatch, the pre-job throw guard, the result→terminal-status ordering all
live there, and the API server and the runner both call them. A second copy would get set-vs-image or
the ordering subtly wrong.

**The API server is a relay, not an origin.** The runner emits the frame shapes the server already
broadcasts (`task:log` / `task:progress` / `task:status` / `task:result`, and `chain:node:*`), so they
go into the existing `ws_*` sinks unchanged. `ws.ts`, the task store and the task console are untouched.

**It speaks the task-rail API.** `/api/tasks`, `/api/tasks/recent`, `/api/pools`, `/api/health` — the
API server's own paths and bare-array shapes — which is what makes `CECELIA_PORT=7657 pixi run console`
work with no console changes. Control is `POST /submit`, `/cancel`, `/submit-chain`, `/cancel-chain`,
`/pools/set`; `/ping` reports identity; the WS carries the event stream.

**Adoption is the normal path**, unlike the preview worker. The runner is meant to outlive a restart, so
on nearly every start-up one is already running with our work in it. Killing and relaunching would
destroy exactly what it protects, so a protocol mismatch is *reported*, never repaired.

### Three things that were bugs first

**It banks terminal outcomes.** `docs/SCHEDULER.md`'s rule is to bank at the rail's *sinks*, not its
producers — and the runner is a new carrier. Its terminal frame is the only announcement that a task
ended, and it is droppable. Unbanked (as the first version was), `/api/tasks/recent` came back empty and
a backend that restarted mid-run could never learn how the task finished; it pinned the row at `running`
forever.

**Timestamps are passed through, never re-derived.** `ws_status` and `record_task_outcome!` take
optional `started_at`/`finished_at` for a frame *this* process did not produce. Re-deriving them stamps
a task the runner has been running for twenty minutes as starting when the relay first saw it, and
resets every elapsed timer on reconnect.

**Both halves read `CECELIA_RUNNER_PORT`.** Only the server honoured it once, so an overridden client
pinged a dead port and fell back to in-process **forever** — a launch that looks fine and runs nothing
on the runner.

### Chain runs are claimed

A chain run mutates `run.json` as it goes (per-node status, `params_hash`, resume bookkeeping), so two
processes on the same run id corrupt each other silently, surfacing later as a resume doing the wrong
thing. The runner claims a run id for the duration and refuses a second submission of it.

Which is why **a refusal is not a fallback**. A 409 and a transport failure look identical from a `try`;
treating the first as "no runner" would start a second execution of a live run. `_submit_chain_to_runner`
returns three states — `:accepted`, `:refused`, `:unavailable` — not two. Only resumes need claiming: a
fresh run's id does not exist until `run_chain` mints it.

### It exits when it has no work and no audience

Launched detached (`detach = true`), nothing stops it — left alone it is a process holding GPU memory
with no window, forever. So it exits after `RUNNER_IDLE_EXIT_SECONDS` (600) with **neither work nor a
subscriber**. Both conditions are load-bearing: work alone would kill it between two tasks of a batch;
a subscriber alone would kill it during the ~45 s of a backend restart, which is the thing it exists to
survive. A `runner.json` (pid, port, commit) in the config dir makes a stray one findable.

### Lifecycle — the one asymmetry

`_stop_children_for_exit(; stop_runner)` is the split. Quit stops it; **Restart and the worktree switch
do not**, and that is the entire feature. Every other resident child is stopped by both. `api/test`
asserts all three directions, including that Quit does *not* opt out — the copy-paste this guards.

`dev.jl`'s `CHILD_PORTS` includes 7657, so Ctrl-C takes it: once the supervisor is gone nothing can
reach it again.

---

## What is NOT on the runner

| | why |
|---|---|
| **Background jobs** (export/import, data patches) | project/bundle-scoped, no pool slot, and import has no project until it finishes. `docs/JOBS.md`, plan D8 |
| **Task preview** | must never queue behind a full run — it is on the un-pooled rail by design. `app/src/preview.jl`, plan D7 |
| **REPL and tests** | `run_task`/`run_chain` stay the library API, usable with no server and no runner |

There is also **no on-disk spool**: a job submitted while the runner is down falls back to in-process,
and a runner crash loses its in-flight task. Deferred, not dropped — plan D4.

---

## File map

| File | Role |
|---|---|
| `app/src/runner/execute.jl` | `TaskRequest`/`ChainRequest` + `execute_task`/`execute_chain` — sink-agnostic |
| `app/src/runner/chain_frames.jl` | `subscribe_chain_frames!` — the `chain:node:*` frames, and the bank |
| `app/src/runner/server.jl` | the runner process: routes, fan-out, claims, idle watchdog |
| `app/src/runner/client.jl` | the API server's side: launch/adopt/ping, submit, cancel, subscribe |
| `api/runner.jl` | launch script (runs in the `api/` environment — no fourth Manifest) |
| `api/src/runner_api.jl` | relay, reconcile, `/api/runner/{status,restart,enabled}` |
| `app/test/runner_e2e.jl` | `pixi run test-runner` — a REAL second process, task + chain |
