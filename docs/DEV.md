# Development workflow

How we change this repo: branches, commits, pull requests, releases. For *what* the code
does see the per-area docs linked from [`CLAUDE.md`](../CLAUDE.md); for the dev loop and
testing commands see the **Development** and **Testing** sections of `CLAUDE.md`.

Repository: `git@github.com:schienstockd/cecelia.git` (default branch **`main`**).

## Golden rule — never commit or push to `main`

`main` is protected by convention. **All work lands via a feature branch + pull request**, even
docs and one-line fixes. Never `git commit`/`git push` directly onto `main`. Releases are tagged
off `main` *after* the PR has merged (see below).

Agents (Claude Code): **ask before every commit and before opening/pushing a PR — explicitly,
each time.** Do not commit or push proactively, even mid-task or after a general "go ahead" to do
the work: approval to *make a change* is not approval to *commit* it. First show the file list and
the proposed commit message(s) (and the branch), then wait for confirmation. If the current branch
is `main`, branch first.

## Branches

Branch off the latest `main`, named with a conventional-commit-style prefix matching the change:

```
feat/<short-slug>      # new feature        e.g. feat/leiden-clustering
fix/<short-slug>       # bug fix            e.g. fix/track-id-nan
docs/<short-slug>      # documentation      e.g. docs/ci-badges
chore/<short-slug>     # deps, tooling, infra  e.g. chore/drop-pythoncall
refactor/<short-slug>  # behaviour-preserving cleanup
```

```bash
git switch main && git pull
git switch -c feat/<short-slug>
```

Keep a branch scoped to one logical change. Don't pile an unrelated fix onto a branch that
already has someone else's work in progress — branch again.

## Commits

Conventional-commits style, matching the existing history (`feat(import): …`,
`docs(readme): …`, `chore: …`):

```
<type>(<scope>): <imperative summary>
```

`type` ∈ `feat | fix | docs | chore | refactor | test | perf`. Scope is optional but
encouraged (`import`, `update`, `gating`, …).

When a commit is authored by Claude Code, end the message with the trailer:

```
Co-Authored-By: Claude <model> <noreply@anthropic.com>
```

`<model>` is the model **actually writing that commit** (`Claude Opus 5`, …) — read it off the
running session, don't copy it from here. This line named one version for a long time and kept
naming it after that model was superseded, which made the trailer a record of when the rule was
written rather than of who wrote the commit.

Ship the test in the same commit as the code (see `CLAUDE.md` → **Testing**), and update the
relevant doc in the same change (see `CLAUDE.md` → the *Keep the docs current* table).

### State reservations before committing (agents)

Every time you're told to commit or push — **including** when Dominik asks *"what's the PR url?"* (that
request itself is the go-ahead to commit + push, so don't stall on extra `git status` round-trips) —
**first volunteer your honest reservations about the change**, in the same turn, before running the
commit. Not a reassurance; a short, prioritized list that separates:

- **Unverified — "go look"**: what you did *not* actually exercise. The most common one: the change
  was typechecked/tested/built but **never run in a browser / driven end-to-end**; also any shipped
  component you refactored and didn't re-verify (a regression surface).
- **Real limitations**: perf/fetch-volume concerns, edge cases you didn't handle, options that are
  silent no-ops, stale-state paths.

If any reservation is material, pause for Dominik's call; if there are genuinely none, say "no
reservations" and proceed. This is not the same as re-asking permission every turn — state the risk
once, then act on the go-ahead. Dominik added this rule after reservations surfaced only when he asked
"any reservations?" *after* a merge — they belong at the decision point instead.

## Pull requests

Open a PR against `main` for review; **Dominik reviews and merges** (PR #1 merged this way). An agent
**asks first** (see the golden rule) before pushing the branch or opening the PR.

- The `gh` CLI **is installed and authenticated** in the agent environment, so an agent pushes the
  branch and opens the PR itself with `gh pr create`, then relays the PR URL. (This section used to
  say `gh` was absent and that the agent should hand over the `pull/new/<branch>` link for Dominik to
  open and paste a body into — that stopped being true, and the stale instruction had agents doing
  the manual dance for no reason.)
- **Always write a complete PR body** — for *every* branch, not just large ones. The commit message
  and the PR body serve different readers (reviewers skim the PR on GitHub), so a body is always
  worth giving — short for small branches, but never omitted. Pass it as a file
  (`gh pr create --body-file <path>`) rather than inline, so markdown and newlines survive the shell.
- End PR bodies (when an agent drafts one) with:

  ```
  🤖 Generated with [Claude Code](https://claude.com/claude-code)
  ```

```bash
git push -u origin feat/<short-slug>
gh pr create --base main --title "<type>(<scope>): <summary>" --body-file <path>
# relay the PR URL it prints
```

### Agent-authored public replies are attributed

Anything an agent posts to a **public surface under Dominik's account** — an issue comment, a
discussion reply, a review comment on someone else's PR — ends with an attribution line naming the
model:

```markdown
---

*This reply was written by Claude Code (<model>), working in this repo on @schienstockd's behalf —
as was the fix in #<pr>. Cecelia is openly AI-assisted; see [`CLAUDE.md`](../CLAUDE.md). Anything
here that needs a human decision is his.*
```

`<model>` is the model **actually writing that reply** (`Claude Opus 5`, …) — read it off the running
session, don't copy a version out of this file. Same rule, and the same reason, as the commit trailer
above: this doc pinned one model there for months and kept pinning it after that model was
superseded, so what shipped recorded when the rule was written rather than who wrote the thing.

The objection is not the AI involvement — this project says it is AI-assisted on the front page.
It is the **implied claim of authorship**: the comment carries Dominik's name and an outside reader
assumes he typed it. Prompted by the reply on #540 (2026-08-13), which went out unmarked and read,
in his words, like AI.

Scope: outward-facing text landing under his identity. **Not** commit messages (they already carry
`Co-Authored-By: Claude <model>`), not PR bodies (the `🤖 Generated with Claude Code` trailer above
does the same job), not code comments, not chat.

## What to press after you change something

Nothing in the Julia halves is hot-reloaded — Revise is loaded but nothing ever drains its revision
queue, because a server has no REPL prompt to hook (see `docs/RUNNER.md` and the plan's Phase 0). So a
code change means a restart. The question is only *which* restart.

**Settings → System → Restart is the replacement for `Ctrl-C` + `pixi run dev`.** It exits the backend
with a sentinel the supervisor relaunches from, and on the way out it stops napari, the preview worker
and Pluto — so they come back fresh too. The one thing it deliberately leaves alone is the **task
runner**, which is the whole point of it.

| you changed | what to press |
|---|---|
| `frontend/` | nothing — Vite hot-reloads |
| `api/src/*.jl` (routes, sockets, handlers) | **Restart** |
| `app/src/*.jl` (the package) | **Restart** |
| `app/src/tasks/**` or a `*_run.py` — the code that runs on an image | **Restart**, then **Restart** on the Task runner row |
| `napari/napari_bridge.py` or `api/src/napari_api.jl` | **Restart** (it stops napari too), then reopen the image to reload its layers |
| `preview/preview_worker.py` | **Restart** (it stops the preview worker too) |
| `pluto/` | **Restart**, or just the Notebooks row |

The runner is the exception because it is **built** to survive a restart — otherwise a backend edit
would still cost you a running segmentation. The tax is that it keeps the code it started with: its row
shows **"old code"** with its commit when it is behind, and its Restart refuses while it still has work
(press again to force). If you are iterating on task code and would rather not think about it, turn the
runner off and Restart is enough for everything.

`Ctrl-C` + `pixi run dev` still works and is still the right move when a `struct` change or a crash has
left the process wedged — it just costs you whatever the runner was doing, because the supervisor's
teardown takes it too.

Why the runner is dev-only, and how it works: [`docs/RUNNER.md`](RUNNER.md).

## CI

Every push/PR runs `.github/workflows/ci.yml` — a smoke test from a fresh checkout, as **three
parallel jobs per OS**, split by what each needs installed:

| job | installs | runs |
|---|---|---|
| `julia` | pixi + Julia + depot cache | **package tests** (the long pole) |
| `server` | pixi + Julia + Node | API tests → frontend build → **frontend tests (Vitest)** → server serves `/api/health` + the frontend |
| `python` | pixi only | **Python tests** → MCP tests |

Jobs run concurrently, so wall-clock is the longest job, not the sum. It runs as a
**matrix on Linux, Windows and macOS-arm64** (`fail-fast: false`), so a
platform-specific install/build/boot failure is caught in CI rather than by a tester — e.g. a PyPI
dep with no macOS wheel falling back to a source build. The repo is public, so
GitHub-hosted runners are free on all OSes (no minute metering — the multipliers only bill private
repos). All steps run under `bash` (Git Bash on the Windows runner). Keep it green before requesting
a merge. See `docs/SHIPPING.md` for the release pipeline.

**Adding a job is not free — it costs cache budget.** The Pixi env and the Julia depot are both
cached and share GitHub's **10 GB per-repo budget**, which the three pixi caches alone fill to
~6.4 GB. `julia-actions/cache` keys on the job name, so every *Julia-using* job adds one ~100 MB
depot cache per run per OS (today: 2 jobs × 3 OSes = 6). Over-splitting evicts the depot caches
before pixi's — pixi's are touched by every job and survive — which silently restores the ~200 s
precompile that #430 removed. Keep suites that need no Julia in the `python` job, and read the
budget note at the top of `ci.yml` before adding a cache or a fourth job.

> **A PR with no checks is not a passing PR.** `pull_request` is intentionally left **unfiltered** so
> a *stacked* PR — one whose base is another feature branch, not `main` — still runs. It used to be
> filtered to `[main]`, and a stacked PR then matched no trigger and showed **zero checks**, which reads
> as green-by-absence: nothing on the page says the suite never ran. That is how #409 — the PR that
> added the package tests to CI — got reviewed with no CI. If you ever see a PR with no checks, that is
> the bug; `gh pr checks <n>` saying *"no checks reported"* is the tell. (`push` stays filtered to
> `main` so a branch push doesn't double-run alongside its own PR.)

> **Frontend typecheck — use `vue-tsc -b`, never `vue-tsc --noEmit`.** The frontend uses TS **project
> references**, and `vue-tsc --noEmit` on the root config **silently skips the `.vue` files** (exits 0
> with real errors). Verify types with **`npm run typecheck`** (`vue-tsc -b`) or `npm run build` —
> which is exactly what CI runs. **Don't merge a red CI**: a merged type error here (an unimported
> `ref`, a store method not in the store's `return`, …) throws a `ReferenceError` in a component's
> `<script setup>`, which Vue surfaces as **`Maximum recursive updates exceeded in component
> <ModuleLayout>`** and **blanks every module page** — it reads like a reactivity loop, but the root is
> a setup exception in the child component. (This cost hours on 2026-07-10; CI *had* caught both errors,
> the PR was merged red.)

## Releases

Cut **off `main`** after the relevant PRs have merged, by pushing a tag:

- `v*` tag → `.github/workflows/release.yml` builds the OS-independent `cecelia.tar.gz` bundle
  and publishes a GitHub Release with the install scripts.
- **Hyphenated tags are prereleases** (`v0.1.0-rc1`); a clean `vX.Y.Z` is the public release that
  makes the `releases/latest` install one-liner resolve.

*When* to cut one and *what the version means* — the cadence (a ~2-week heartbeat + event triggers),
the rc-vs-release distinction, and the pre-1.0 versioning rules — live in
[`docs/RELEASING.md`](RELEASING.md). Rationale and the full packaging/update model live in
[`docs/SHIPPING.md`](SHIPPING.md).

## Tests

Four categories, one per language layer. **All four run in CI** (`.github/workflows/ci.yml`) on every
OS in the matrix, and each has a `pixi run` task that runs the whole suite:

- **Package (headless Cecelia):** `pixi run test-pkg`. The data model, persistence, task dispatch,
  scheduler + chain logic. Some testsets `@test_skip` when their `test-data/` fixtures are absent.
  **Add testsets to `app/test/suite.jl`**, not `runtests.jl` — the latter is just the preamble plus
  the one aggregating `@testset` that includes the body.
- **API adapters:** `pixi run test-api` (`api/test/runtests.jl`). Loads `server.jl` with
  `CECELIA_NO_SERVE=1` so the handlers + shared state (`_BOUND_HOST`, `_repl_on`, …) are defined
  without binding a socket, then calls handlers directly (no live server, no ports). Fixture-free.
  Covers diagnostics + the debug-console gating/eval; extend it as more adapters gain logic worth pinning.
- **Frontend:** `pixi run test-frontend` (Vitest, `frontend/` — or `npm test` there directly).
  **Scope is deliberately narrow: pure logic extracted out of `.vue` SFCs into `src/utils/*`**
  (e.g. `startDot.ts` — the chain start-dot save/reload round-trip, which mirrors the Julia
  `_prune_to_start` pruning; a two-sided contract that can silently drift). **No component mounting,
  no jsdom, no DOM/E2E** — those are a separate, heavier decision (`@vue/test-utils`, a DOM shim) not
  taken here. Vitest is zero-config on top of the existing Vite toolchain, so the category stays cheap.
  The convention this enforces: **keep testable logic in plain `.ts` modules, not the component**, so it
  can be unit-tested without mounting Vue.
- **Python (analysis env):** `pixi run test-py` — the Pixi-env Python code Julia drives via `run_py`
  (segmentation, measurement, corrections, the zarr/dask I/O layer): anywhere logic can silently produce
  wrong data on disk. stdlib `unittest`, auto-discovered from `python/cecelia/tests/test_*.py`
  (`python -m unittest discover`) and run as one suite — add a `TestCase` whenever you touch `python/cecelia/**`
  data logic worth pinning; the suite grows with it. **Deliberately no `pytest` dependency** (it isn't in
  the analysis env and shouldn't ship to users just for tests). Must run via `pixi run` so the env's
  `python` + `numpy`/`dask`/`zarr` resolve. First member: `test_zarr_store.py` (the `create_multiscales`
  chunk-aligned store round-trip).

**Both Julia suites run at `-O0`, and the package body lives in its own file — both are compile-time
fixes.** The suite spent the large majority of its wall clock inside the Julia compiler, not running
assertions (measured: ~11s of actual test work in a ~200s run). Two causes, both fixed: an 8k-line
`@testset begin … end` is a single top-level expression that Julia lowers and compiles *in full*
before the first assertion runs (~90s), and optimising code that executes exactly once is waste
(~2/3 of the remainder). Package suite 202s → 69s, API 67s → 42s, same assertions. `-O0` does not
de-optimise what's under test — `Cecelia`'s methods come from the `-O2` pkgimage built at install;
only in-session code (the test file) drops to `-O0`. If a timing-sensitive test ever turns flaky in
CI, that flag is the first suspect.

**Trap — `@testset` reseeds the global RNG, so `gen_uid()` is NOT unique across testsets.** Julia's
`Test` seeds each testset deterministically, which means two testsets that both
`create_project! → add_set! → add_image!` are handed the **same uid sequence** and land in the
**same directory on disk**. It stays invisible until a test asserts on a directory's *contents*
(`img_branch_value_names` did, and read the previous testset's files). If a testset writes files it
will then read back, pass an explicit `uid=` to `add_image!` rather than trusting `gen_uid`.

## Dev worktree switch (Settings → System)

When several git worktrees exist (the branch-preview workflow), **Settings → System → Worktree** lists
them and lets you relaunch dev from another checkout **without the console**. Dev + supervised only
(`pixi run dev`, which sets `CECELIA_SUPERVISED`). Mechanism: `POST /api/app/switch-worktree` records the
target's `api/` dir in a sentinel file and exits with the restart code; the `api/dev.jl` supervisor
relaunches *from that dir* and the page reconnects when `/api/health` is back — same lifecycle as Restart.

**`pixi run dev` supervises BOTH the backend and the frontend (Vite),** so a switch relaunches **both**
from the target worktree — the served frontend follows the branch too, not just the backend. Because of
that, **don't also run `pixi run frontend`** alongside `pixi run dev` (two Vites would clash on :5173);
`pixi run frontend` stays for running Vite standalone (e.g. previewing yet another worktree on a spare
port). A plain Restart bounces only the backend (the frontend keeps its HMR state); only a *switch*
relaunches Vite. Prod (`app.py`) is backend-only and doesn't offer the switch (control hidden when not
dev/supervised).

### A worktree's Python env can point at ANOTHER checkout

`pixi.toml` installs the helper package as an editable path dep (`cecelia = { path = "python" }`), which
setuptools implements as a **meta-path finder holding one absolute path**. The uv cache key derives from
the relative `./python`, so across worktrees the entry collides and **the first checkout to build it
wins** — a fresh `pixi install` in a new worktree can hand you an env whose `import cecelia` resolves
into a *different* worktree. Nothing errors.

That splits the app in half. Anything launched **by path** (napari bridge, preview worker, `run_py` task
runners) runs *this* checkout's files; anything imported **by name** comes from the other one. They agree
until one side gains a helper the other lacks, and then the error names the symptom, not the cause:
`AttributeError: module 'cecelia.utils.correction_utils' has no attribute 'af_derived_values'` — raised
by a worker whose own file had the caller.

**This is prevented structurally, not by remembering it.** `PYTHONPATH` is searched *before* the editable
finder, so pinning it makes the collision harmless. Three layers, covering the two ways Python gets
started here:

| Layer | Covers | Where |
|---|---|---|
| `[activation.env] PYTHONPATH = "$PIXI_PROJECT_ROOT/python"` | every `pixi run` — `test-py`, `python`, `dev`, a REPL | `pixi.toml` |
| launchers set it explicitly | processes the Julia server spawns, which never go through `pixi run` | `run_py`, `preview.jl`, `napari.jl` |
| a test that fails naming both paths | the tripwire, if both of the above are ever bypassed | `python/cecelia/tests/test_env_wiring.py` |

`$PIXI_PROJECT_ROOT` expands per project, so each worktree points at itself with nothing to configure.
Verified by deliberately repointing a worktree's finder at another checkout: the bare interpreter then
imported the wrong one, and `pixi run` still imported the right one.

Repairing an env is therefore optional now (only a bare interpreter outside `pixi run` is affected):

```bash
pixi run python -m pip install -e python --no-deps --no-build-isolation
```

Note that `rm -rf .pixi && pixi install` alone does **not** fix a mis-wired env — the collision is in
the shared uv cache, not in the env — which is exactly why this needed a structural answer rather than a
documented ritual.

## Diagnostics & debug console

**Settings → Diagnostics** (always on) shows server threads, Julia version, memory, the bound
host/port, and the projects dir — read from `GET /api/diagnostics`. Use it to confirm the API is
multithreaded (`threads` > 1; the server also logs `threads=N` at startup) and to see the bind.

**Settings → Developer → "Enable debug console"** exposes a Julia REPL that evaluates in the running
server's `Main` (so `Cecelia`, `projects_dir`, scheduler/napari state are all in scope) — e.g.
`Threads.nthreads()`, inspecting a `label_props`, poking a `CciaImage`. It returns the value plus
captured `stdout`/`stderr`.

Because that is arbitrary code execution, it is gated:

- **Hard gate — a loopback bind.** Eval runs only when the server is bound to `127.0.0.1`/`::1`. A
  `0.0.0.0` (network-reachable) server refuses it regardless of the toggle — the OS won't accept a
  remote connection to a loopback socket, so there's no header to spoof. The default bind is loopback
  (`CECELIA_HOST`), so it works out of the box locally; deliberately exposing the server with
  `CECELIA_HOST=0.0.0.0` disables the console.
- **Runtime toggle.** The Settings switch flips a server flag via `POST /api/repl/config` (no
  restart); it seeds from `CECELIA_REPL=1`. Off by default. It is *not* a security boundary — the
  loopback gate is.

Implementation: `api/src/repl_api.jl`. `redirect_stdout` is process-global, so evals are serialised
under a lock and drained by an async pipe reader.

## Data patches (Settings → Data patches)

One-off data migrations the user can run from the GUI — e.g. rewriting every labelProps h5ad to a new
on-disk convention when the format changes. Unlike a `CciaTask` (image-scoped, scheduler-run), a patch
operates on a whole **project** and is confined to the **currently open one**. It streams over the same
task WS rail (`task:log`/`task:progress`/`task:status`), so it shows live output + progress + a working
Stop, and also appears in the Tasks list — like an HPC-task spin-off. The Tasks list's **Cancel** works
on it (`task:cancel` also calls `cancel_maintenance!`, alongside the batch-movie canceller), but
**Rerun is hidden** for `module: 'maintenance'` entries — rerun goes through the scheduler and a patch
has no scheduler fun_name, so relaunch it from Settings → Data patches instead.

**Adding a patch:** add a `MaintenancePatch` entry to `MAINTENANCE_PATCHES` in `app/src/maintenance.jl`
(stable `id`, title, description, and the `_run.py` module path under `python/cecelia/`), and write that
Python runner in the `run_py` style (reads `{root, apply}` from the params JSON — `root` is the project
dir; `apply=false` is a dry run; emit `[PROGRESS] n/total` + log lines). The Settings section lists
patches from `GET /api/maintenance/patches` and launches one via the `maintenance:run` WS message
(cancel via `maintenance:cancel`). Registry + runner: `app/src/maintenance.jl`
(`run_maintenance_patch`/`cancel_maintenance!`); WS handler: `api/src/sockets.jl`
(`handle_maintenance_run`). Example patch: `store-debris` (the leftover-store sweep).
