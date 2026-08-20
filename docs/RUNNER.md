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

**It gets started.** A submit that finds the runner enabled but not answering (`:unavailable`)
relaunches it and submits there; only if the relaunch fails does the task run in the backend. The row
stays `queued` for the cold start (~45 s), which is what it is.

That is a change from falling back immediately, and the reason is what falling back cost. It was
*silent*: a runner that died mid-session turned every later run into one that dies with the next
restart — the single thing enabling the runner prevents — and the only tell was a 20-second-polled
**"Task runner down"** label on the Run panel. Nobody queueing six segmentations is watching that
label. Six were lost to exactly this, and the project held no record that they had ever started.

The label and its **Start** button still exist for the persistent state. What is new alongside them:

- **The death is announced.** `runner_subscribe!` used to swallow the dropped connection and retry
  forever in silence. It now `@warn`s once on a connected→gone transition (and `@info`s on reconnect),
  which the server's `BroadcastLogger` tees to the browser as `server:log`. Once, on the transition —
  the same loop spins while a cold runner precompiles, and a per-retry warning would bury the event
  under its own noise.
- **A run that dies anyway is recorded** as `interrupted` — see *What a lost task leaves behind*.

Two things worth knowing about the relaunch: it is behind `_RUNNER_RELAUNCH_LOCK`, because pressing Run
on a set submits one task per image and six concurrent cold starts racing for one port is not a
recovery; and it never happens on `:refused`, which is a *live* runner's answer and nothing to repair.

It should not go missing while you are working — the idle exit requires **no subscriber** and the
backend holds one open — so this means `pixi run stop-runner`, or a crash.

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

### Losing the port is a normal outcome, not a crash

Because the runner outlives the API server, a fresh `pixi run dev` frequently finds one already there.
`runner_launch!` adopts anything that ANSWERS — but a cold runner pays Julia load plus Cecelia
precompilation (~45 s) before it binds, and during that window it answers neither a ping nor a state
file. So a second launch looks justified, and one of the two then loses the bind.

Three things used to go wrong at that point, and all three are fixed:

- The loser died with a `TaskFailedException` stack trace out of `HTTP.listen`, which reads as a broken
  app. It now checks for an incumbent first and, failing that, catches the bind and **exits 0 with one
  line naming the pid and commit that hold the port**.
- Worse, the loser wrote `runner.json` with its OWN pid *before* binding — clobbering the incumbent's
  record — and its `atexit` hook then **deleted** the file. A collision therefore left the *surviving*
  runner with no state file, which is exactly the "a stray runner is folklore" case the file exists to
  prevent. The order is now **own first, claim second**, so a runner only ever writes the record for a
  port it actually owns.
- And "bind first" was not enough, because **a returned `Server` is not proof of a bound port.**
  `HTTP.listen!(handler, host, port)` builds a `Server` and spawns a task that does the bind; that
  task's failure path `notify`s the ready `Event` *before* it rethrows, so the caller can resume while
  the doomed task is still unwinding and `listen!`'s own `istaskdone && istaskfailed` guard sees a task
  that has not finished failing. The `EADDRINUSE` then arrives at `wait(server)` — after the state file
  was claimed. Both original symptoms, back, and scheduler-dependent: green on Linux and Windows CI,
  red on macOS. Ownership is therefore proven by **asking**: `/ping` reports the responder's PID, so
  "something answers" and "*we* answer" are distinguishable (`_runner_owns_port`) — which is the
  distinction that matters, since the failure mode is another runner holding the port. A lost race pays
  the deadline before exiting, which costs nothing: that process is leaving.
- **The budget has to fit the measurement, and the first version's did not** — which turned the guard
  into "stand down always". A freshly returned `HTTP.listen!` server does not serve its first
  *in-process* request for ~1.2 s (its accept path is still compiling), and the session's first
  `HTTP.get` compiles for about as long. A 2 s per-attempt timeout inside a 3 s deadline therefore
  bought exactly ONE attempt, which could not succeed: nothing held the port, the runner announced that
  another process had, and every task fell back to in-process on a normal `pixi run dev`. The fix is
  cheap attempts (0.5 s) inside a generous deadline (10 s), so the loop actually loops. Only a
  **non-HTTP squatter** can reach the deadline — a runner that answers is rejected on the first attempt
  by its pid, and a live one was caught by the cheap `runner_ping` long before.

The stand-down path is pinned by *"runner_serve stands down when the port is taken"* in
`app/test/suite.jl`, which holds an ephemeral port with a plain socket that never speaks HTTP — so
nothing answers, and the path is exercised on every platform rather than only where the scheduler loses.
**Its twin matters just as much**: *"`_runner_owns_port` recognises a port WE just bound"* is what the
budget regression got past, because a guard tested only from the "port taken" side looks correct when it
fires for every input. Both directions, or neither is pinned.

`runner_launch!` also reports **whose** runner answered: it compares the pid on the wire with
`Libc.getpid` of the child it spawned, and says "adopted" rather than "started" when they differ. It
used to log "started" with the incumbent's pid while the process it had launched died in the same
terminal — two different pids in one log, which sent the reader hunting for a bug in the wrong one.

**A shared `CECELIA_DEV_DIR` shares this port.** A worktree with a copied `.env` resolves the same
`config_dir()`, so two checkouts cannot both run `pixi run dev`; the second's runner stands down. Set
`CECELIA_RUNNER_PORT` for a genuinely independent pair.

### Lifecycle — the one asymmetry

`_stop_children_for_exit(; stop_runner)` is the split. Quit stops it; **Restart and the worktree switch
do not**, and that is the entire feature. Every other resident child is stopped by both. `api/test`
asserts all three directions, including that Quit does *not* opt out — the copy-paste this guards.

`dev.jl`'s `CHILD_PORTS` includes 7657, so Ctrl-C takes it: once the supervisor is gone nothing can
reach it again.

**A crash is the third case, and it relaunches.** The supervisors (`api/dev.jl`, prod's `app.py`) used
to treat every death that was not the restart sentinel as "the user is done" — so a backend segfault
ran the teardown above and reaped the runner, meaning a crash cost exactly the running segmentation the
runner exists to protect. Both now classify the death (`_crash_death` / `_crashed`, kept in step and
tested in `api/test`):

| how the backend died | supervisor |
|---|---|
| `_exit_now(42)` — Restart / worktree switch | relaunch (the loop's main job) |
| `_exit_now(0)` — in-app Quit or Ctrl-C; `SIGTERM`/`SIGKILL` — `pixi run stop` | stop, teardown takes the runner |
| a **fault** signal (SEGV/ABRT/BUS/FPE/ILL) or any other nonzero exit | **relaunch, children left running** |

> **The exit code is the only channel carrying intent, so it has to survive.** Every exit path uses
> `_exit_now` (POSIX `_exit`) rather than `exit`, because `exit` tears down the JIT and the thread pool
> under live threads and *segfaults* when a worker is mid-compile — which lands the process in the
> third row. An in-app Quit that faulted on the way out was classified as a crash and **relaunched**,
> so Quit did not quit. Ctrl-C gets the teardown too, via an `atexit` hook in `start` — measured
> before the fix, a Ctrl-C left this runner alive on :7657 with nothing able to reach it again.
> See `docs/DEV.md` → *Stopping the app*.

The distinction that matters is the middle row: relaunching on `SIGTERM`/`SIGKILL` would make
`pixi run stop` unable to stop the app, because the supervisor would keep bringing it back. Bounded by
`CRASH_LIMIT` faults inside `CRASH_WINDOW` (3 in 60 s) — a server that cannot boot at all stops with
the reason on screen instead of looping, and then the teardown *does* take the runner. Every resident
child is adopt-or-launch, so the relaunched server picks the runner (and napari, the preview worker,
Pluto) straight back up.

---

## What is NOT on the runner

| | why |
|---|---|
| **Background jobs** (export/import, data patches) | project/bundle-scoped, no pool slot, and import has no project until it finishes. `docs/JOBS.md`, plan D8 |
| **Task preview** | must never queue behind a full run — it is on the un-pooled rail by design. `app/src/preview.jl`, plan D7 |
| **REPL and tests** | `run_task`/`run_chain` stay the library API, usable with no server and no runner |

There is also **no on-disk spool**: a runner crash loses its in-flight task, and a job submitted while
the runner cannot be brought back runs in-process. Deferred, not dropped — plan D4. (A submit no longer
*starts* by falling back — it relaunches the runner first; see *If it is not there when you press Run*.
That narrows the window, it does not close it: nothing survives the runner dying mid-task.)

### What a lost task leaves behind

Because there is no spool, "lost" is a real state and the only defence is that it is *visible*. Two
things that were once silent:

**The fallback is not exempt from the exit.** A task running in-process (the fallback above) dies with
the backend on Restart *and* on the worktree switch — both `exit` this process. `_stop_children_for_exit`
used to skip cancelling in-process tasks on exactly those two paths, gated on the same `stop_runner`
flag that protects the runner. But the gate never protected anything here: the runner's tasks are not in
this process's `_TASKS`, so that loop never reached them. All the gate did was decide whether the
in-process task died *tidily* — and skipping the cancel orphaned its Python child to keep burning GPU
into a `.partial` nobody would promote, with no terminal status recorded. The cancel is now
unconditional; `api/test` pins that it is not re-gated.

**And a run that dies anyway is reaped, not forgotten.** The run log is opened when a task starts, so a
task lost to a runner Ctrl-C or crash leaves a `running` entry that becomes `interrupted` at the next
project open. That is the difference between "three of my six segmentations are gone" being answerable
from the project and not — see `docs/SCHEDULER.md` → *a run is logged twice*.

---

### Where its own output goes — two carriers, on purpose

The runner is spawned `detach = true`, and a non-blocking `run` sends stdio to **devnull** — so until
the log rail landed, **everything the runner said went nowhere**. Not "to the terminal": nowhere. The
process most likely to be holding your segmentation was the one that could not tell you anything.

It has two kinds of output and they need different carriers, because it is the one child that must
outlive the process watching it:

| | Goes to | How |
|---|---|---|
| `@info` / `@warn` / `@error` | the **app console** | the same `TeeLogger` → `runner:log` on its `/events` stream → relayed by the API server |
| raw `stdout`/`stderr` — `println`, an unhandled `@spawn` task error, precompile chatter, a segfault dump | the **calling terminal** | stdio inherited from the backend at spawn |

**Why not `spawn_logged` for the raw half.** That hands the child a pipe the *backend* reads, which
becomes a broken pipe on exactly the restart the runner exists to survive. Inheriting the backend's
stdio has no such problem: the fd belongs to the terminal, not to the backend, so a detached child goes
on writing to it long after the backend is gone (verified). And the output you most need — a crash dump
— is written by the C runtime *while the process dies*, so a pipe read by that same process is the
worst possible destination for it. That is the argument for keeping raw stderr on a terminal rather
than routing everything into the console.

**This is not a dev/prod switch, and could not be one:** `runner_enabled()` requires
`is_dev_session()`, so there is no runner in prod at all (see *Why dev-only*). Both carriers are simply
correct in every launch mode we have — including `pixi run runner` standalone, where the terminal half
was already working because the runner *is* the foreground process there.

**The console half is gap-filled.** The runner keeps a `LogRing` of its own, served at
`GET /api/logs/recent?since=<seq>` on :7657. The API server relays live records into its console ring
under `source = "runner"` and, on every (re)connect, pulls what it missed — a third question alongside
"what is in flight" and "how did things end", and for the same reason: **a runner routinely works with
no subscriber**, so anything it said during a backend restart reached nobody. `_RUNNER_LOG_SEQ` is the
cursor, and it resets on a relaunch because a fresh runner starts counting at 1.

With no backend at all, `CECELIA_PORT=7657 pixi run console` still shows the task rail directly.

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
