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
Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
```

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

- The `gh` CLI is **not installed in the agent environment**. An agent therefore **pushes the
  branch and relays the PR-creation URL** (the `https://github.com/schienstockd/cecelia/pull/new/<branch>`
  link printed by `git push`) for Dominik to open — it does not attempt `gh pr create`.
- **Always relay a complete, paste-ready PR body** — for *every* branch, not just large ones.
  Because `gh` is absent, GitHub receives **no** description automatically; the body is text Dominik
  pastes into the PR form. The commit message and the PR body serve different readers (reviewers
  skim the PR on GitHub), so a body is always worth giving — short for small branches, but never
  omitted. (Don't leave it to a per-branch judgement call; that produced inconsistent PRs.)
- End PR bodies (when an agent drafts one) with:

  ```
  🤖 Generated with [Claude Code](https://claude.com/claude-code)
  ```

```bash
git push -u origin feat/<short-slug>
# relay the "Create a pull request" URL git prints
```

## CI

Every push/PR runs `.github/workflows/ci.yml` (smoke: fresh checkout → `pixi install` →
`julia … instantiate` → API tests → **Python tests** → frontend build → **frontend tests (Vitest)** → server serves
`/api/health` + the frontend). It runs the
**full chain as a matrix on Linux, Windows and macOS-arm64** (`fail-fast: false`), so a
platform-specific install/build/boot failure is caught in CI rather than by a tester — e.g. a PyPI
dep with no macOS wheel falling back to a source build (TODO #00062). The repo is public, so
GitHub-hosted runners are free on all OSes (no minute metering — the multipliers only bill private
repos). All steps run under `bash` (Git Bash on the Windows runner). Keep it green before requesting
a merge. See `docs/SHIPPING.md` for the release pipeline.

## Releases

Cut **off `main`** after the relevant PRs have merged, by pushing a tag:

- `v*` tag → `.github/workflows/release.yml` builds the OS-independent `cecelia.tar.gz` bundle
  and publishes a GitHub Release with the install scripts.
- **Hyphenated tags are prereleases** (`v0.1.0-rc1`); a clean `vX.Y.Z` is the public release that
  makes the `releases/latest` install one-liner resolve.

Rationale and the full packaging/update model live in [`docs/SHIPPING.md`](SHIPPING.md).

## Tests

Four categories, one per language layer. **All four run in CI** (`.github/workflows/ci.yml`) on every
OS in the matrix, and each has a `pixi run` task that runs the whole suite:

- **Package (headless Cecelia):** `pixi run test-pkg` (`app/test/runtests.jl`). The data model,
  persistence, task dispatch, scheduler + chain logic. Some testsets `@test_skip` when their
  `test-data/` fixtures are absent.
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
