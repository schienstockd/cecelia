# GUI tests: shared components in a real browser, and every bug that reaches the eye

**Status:** in progress — P1 (harness + SelectionTable sticky header) #2370; P2 (recital: frontend
passes-on-base + `no browser test`) on `feat/gui-tests-test-value`.
Open questions answered by default (2026-11-19, "work through the plan autonomously"): advisory first,
Chromium only, `fix/*` + `.vue`/CSS/layout-util trigger, smoke with `testpr`, base gap closed in P2.

## Goal

A rendering or interaction bug that reached the maintainer's eyes is caught by a test from then on.
Shared components that many pages use get real-browser tests of their layout and interaction. One cheap
smoke test says every route renders. Page layouts are **not** tested: the frontend churns, and a page
layout test would rot faster than it catches anything.

## What exists (measured 2026-11-19, at `a54499fea`)

- **Frontend tests:** 434 vitest files, ~5,500 `it`/`test` calls, all in Node. None mounts a component
  (`vite.config.ts`: "No test mounts components"). 54 files read source as text (`?raw` or
  `readFileSync`), 30 of them `.vue` files, to enforce conventions (`cssTokens`, `cssScenarios`,
  `iconLegend`, `continuousControls`). Nothing checks rendered geometry or a click.
- **The motivating bug class.** SelectionTable places its pinned header and frozen columns by two
  mechanisms that can disagree: `.sized th { position: relative }` against `.sel-sticky`, the `fit`
  prop (`content` puts the table in an `overflow-x` wrapper, which traps CSS sticky), and now
  `stickyHeader`'s hand lift (`utils/stickyHead.ts`) under a CollapsibleSection bar that publishes
  `--cc-sticky-above`. Each round was found by eye. The pure maths (`headerLift`) has a Node test; the
  part that broke, the browser's sticky behaviour, has none. Today's repro was a static page in headless
  Chromium via `scripts/lib/chromium.mjs`.
- **Browser tooling.** `playwright-core` 1.65.0 is a frontend devDependency; `scripts/lib/chromium.mjs`
  drives a SYSTEM Chromium (`CECELIA_CHROMIUM`, no download). `scripts/lib/app_page.mjs` (`signIn`,
  `openProject`, `settle`, `land`, `refuseSlotZero`) drives the real app on an isolated instance for
  `gui-capture`, `doc-shots`, `doc-sheet`. Screenshots only, no assertions.
- **CI.** `.github/workflows/ci.yml` runs `npm test` inside the `server` job on Linux, Windows and macOS,
  after `npm run build`. The same job boots `pixi run prod` on the runner's :8080, signs in with
  `~/.cecelia/api-token` and curls `/`. `release.yml` already prints PDFs with the runner's Chrome via
  `playwright-core`, so `ubuntu-latest` has a usable Chrome at `/usr/bin/google-chrome` (on
  `findChromium`'s list). Nothing installs Chromium.
- **Fixtures.** `test-data/fixtures.toml` + `NEEDS.toml`, resolved by `fixture_for(...)` in Julia
  (`app/test/fixture_registry.jl`) and Python (`cecelia.tests.fixture_registry`). Committed, 2.4 MB,
  project `testpr` (KDIeEm, KDIeEx, …), so CI has it. No JS resolver and no CLI.
- **ResizeObserver loops.** `utils/roLoopTrace.ts` attributes "ResizeObserver loop completed…" to the
  observer's creation site, installed in `main.ts` under `import.meta.env.DEV` only.
- **Test value check** (`python/cecelia/effectiveness/test_value.py`, run by `pixi run recital`).
  `source_changes` counts only `app/src/` and `python/cecelia/`, so **a frontend change never gets
  "passes on base"**. A new `.test.ts` beside a `.vue` fix is never watched failing.

## Decisions (proposed 2026-11-19)

1. **Vitest browser mode, playwright provider, system Chromium.** Not a hand-built Vite page driven by
   `playwright-core`.
   - **Why.** The test runs inside the page: direct DOM, `getBoundingClientRect`, the same `expect`,
     the same Vite plugins (`rangeFillTransform`, the feijoa alias), watch mode and one failure report.
     A hand-built page means our own runner: a fixture registry of components, a Node process calling
     `page.evaluate` for every assertion, and a second place where the Vite config is duplicated. It
     would cost more than the dependencies.
   - **Deps (devDependencies only; `frontend/dist` and the bundle never contain them).**
     `@vitest/browser-playwright` (MIT; pulls `@vitest/browser`, MIT) and `playwright` (Apache-2.0),
     **pinned to 1.65.0** to match `playwright-core`. Both licences are GPL-3-compatible. The browser is
     `launchOptions.executablePath = findChromium()`, so no browser is downloaded. No `@vue/test-utils`:
     a ~20-line `mountInto(component, props, hostHtml)` uses `createApp`. Checked against the installed
     vitest 5.0.5: `@vitest/browser-playwright@5.0.5` exists on npm, MIT.
2. **One app setup, shared.** `main.ts` gains `installAppPlugins(app)` (pinia, `v-tooltip`,
   `v-range-fill`). Both the app and `mountInto` call it. Without it, `rangeFillTransform` injects a
   directive the test app doesn't register. A copy would drift.
3. **The browser project never reaches a backend.** `vite.config.ts`'s `server.proxy` forwards `/api`
   and `/ws` to `CECELIA_PORT ?? 8080`, which is the maintainer's live backend. The `browser` project
   does **not** inherit `server`. `mountInto` replaces `fetch` and `WebSocket` with a stub that fails the
   test on any call not declared by that test. (`profileStorage` POSTs `/api/profile/settings/patch` on
   write, so SelectionTable's column widths would otherwise hit it.) A Node guard test reads the resolved
   browser config: no proxy, port ≠ 5173. The port matters on its own: `server` sets `strictPort: true`
   on `frontendPort`, so an inherited block would fail to start beside the maintainer's dev server on
   5173. The browser project takes a free port.
4. **Geometry, not pixels.** Assertions compare boxes (header top = pin line ± 1 px; frozen header cell
   left = frozen body cell left). No screenshot diffing: fonts, scrollbars and antialiasing differ between
   the maintainer's Chrome and the runner's. Fixed viewport and `deviceScaleFactor: 1`. Waits use a
   `frames(n)` helper (the lift is `rafCoalesce`d), never a sleep.
5. **Two modes, one place.** Component mode is `frontend/src/**/*.browser.test.ts` in the vitest `browser`
   project. App mode is `scripts/gui_smoke.mjs` built on `app_page.mjs` and run against an isolated
   instance only (`refuseSlotZero`). App mode holds the route smoke and nothing page-specific
   (Decision 8).
6. **Separate CI job, Linux only, advisory first.** A new `gui` job on `ubuntu-latest` needs only Node:
   checkout, `setup-node` with the npm cache, `npm ci`, `npm run test:browser`. It adds no pixi and no
   Julia depot, so the 10 GB cache budget is unchanged. Expected 2–3 min. `npm test` becomes
   `vitest run --project unit`, so the fast suite and its three-OS run are unchanged. The job is not a
   required check until two weeks of runs show its flake rate, then it is flipped (same path as the
   convention check). No Windows or macOS: the browser is Chromium on every OS. No Firefox (open
   question 2).
7. **Bugs grow the suite, enforced by the recital, not by prose.** `test_value.py` gains a frontend leg:
   - **passes on base** also covers `frontend/src/**` source with added `*.test.ts` /
     `*.browser.test.ts`. The added tests run against `HEAD`'s `frontend/src` with the staged tests on
     top, reusing `node_modules`. This closes the gap above for Node tests too.
   - **New finding `no browser test`.** It fires on a `fix/*` branch whose diff touches a `.vue` file,
     `style.css`, or a layout util (`stickyHead`, `anchorPosition`, `columnFill`, `rafCoalesce` users),
     when the diff adds no `*.browser.test.ts`. It takes the usual tag: add the test, or
     `[tv-…: false_positive: not a rendering/interaction bug — <why>]`. The commit-msg hook already
     refuses an untagged finding. The branch prefix is the trigger because the recital runs before the
     message exists. Fixes already use `fix/` branches (40 in this clone, local + remote).
   - CLAUDE.md's *Test value* line gains one clause. No weekly-judge sweep: after a merge there is no
     base.
8. **Route smoke, one test.** Every route in the router: it renders `.cc-main`, has no `pageerror`, no
   `console.error`, and no "ResizeObserver loop" error event, with and without the `testpr` project
   open. The routes come from the router itself: `main.ts`'s inline array moves to `router/routes.ts`, so
   a new page is covered without an edit. Skipped by name with a reason: `/viewer-window` and
   `/board-render` (headless Chromium has no WebGPU canvas, per GUIDE_SCREENSHOTS_PLAN), and the
   redirects. On CI it runs as a Linux step in the `server` job, after the existing health check and
   before `pixi run stop`. The server is already up and signed in, so it costs a browser launch, about
   1 min. Locally it runs on an isolated instance. The project comes from `fixture_for`: the Python
   registry gets a `python -m cecelia.tests.fixture_registry <props> --json` entry (one resolver, no JS
   copy), and the script copies `test-data/projects/<project>` into the instance's projects dir through
   `projectsDir` / `copyProject`, lifted from `scripts/docshots/fixture.mjs` (module-private today) into
   `scripts/lib/` so both callers share the one guard that refuses a dir outside `--config-dir`. In prod builds the RO trace is off. The smoke
   still catches the bare error event; only the attribution is DEV-only.

## Architecture

```
frontend/
  vite.config.ts            test.projects: unit (node, excludes *.browser.test.ts) · browser (chromium, no proxy)
  src/test/browser/
    mount.ts                mountInto(), installAppPlugins(), stubbed fetch/WebSocket, frames(n)
    roLoop.ts               setup file: any ResizeObserver loop error fails the test (roLoopTrace attributes it)
  src/components/*.browser.test.ts
scripts/gui_smoke.mjs       app mode: routes × {no project, testpr}; app_page.mjs + chromium.mjs
scripts/lib/fixture_copy.mjs  projectsDir + copyProject, lifted from docshots/fixture.mjs (P4)
pixi.toml                   test-gui = npm run test:browser · test-gui-app = node scripts/gui_smoke.mjs
.github/workflows/ci.yml    job `gui` (Linux, Node only) + one smoke step in `server` (Linux)
python/cecelia/effectiveness/test_value.py   frontend leg + `no browser test`
```

## Primitives, ranked by consumers (template tags across 252 `.vue` files)

| Component | Files using it | Browser test covers |
|---|---|---|
| `v-tooltip` (directive) | 190 | placement at viewport edges, hides on scroll/route change, no stale tip after unmount |
| ChipSelect | 47 | keyboard, select-all tri-state, reorder, wrapping in a narrow rail |
| CcToggle | 42 | click/keyboard, label hit area, disabled |
| TeleportPopover | 35 | placement vs anchor (flip at edges), follows anchor scroll, Esc/outside close, stacking over BaseModal |
| BaseModal | 34 | focus trap, Esc, padded scroll body, nested popover |
| SelectionTable | 23 | sticky header (PR 1), frozen columns + horizontal scroll, `fit` fill/content, resize handle, selection clicks vs buttons in cells |
| CollapsibleSection | 21 | `sticky` bar + `--cc-sticky-above`, one-open accordion, `fill` |
| ModuleLayout | 18 | pinned Images bar + table, needs a stubbed project store: last |

InlineNote (24) is presentational: no test. ConfirmButton (15) and ConfirmDeleteButton (12) get one
interaction test each, in the BaseModal/TeleportPopover PR.

## Phases (~7 PRs)

| Phase | PRs | What | Checkpoint |
|---|---|---|---|
| P1 | 1 | Harness + SelectionTable sticky header (below) | the test fails on `origin/main` and passes on the fix branch, locally and in the `gui` job |
| P2 | 1 | `test_value.py` frontend leg + `no browser test` + CLAUDE.md clause | its own tests (injected `on_base`, as today); recital on a sample `fix/` diff shows the finding |
| P3 | 3 | Primitives: (a) SelectionTable rest + CollapsibleSection; (b) TeleportPopover + BaseModal + confirm buttons; (c) ChipSelect + CcToggle + `v-tooltip` | each PR's tests watched failing against a mutated component |
| P4 | 1 | Route smoke + `router/routes.ts` + fixture CLI | green on CI Linux; a deliberately thrown error in one module fails it |
| P5 | 1 | Flip `gui` to required, if the flake record allows | two weeks of runs |

P1 must land on top of the sticky-header fix, or in it. P2 can go in parallel with P3.

## PR 1, exact scope

- devDeps: `@vitest/browser-playwright`, `playwright@1.65.0`. Vitest `projects` in `vite.config.ts`
  (Decisions 1, 3). Scripts: `test` → `--project unit`, `test:browser`. Pixi task `test-gui`.
- `src/test/browser/mount.ts` and `roLoop.ts`. `installAppPlugins` extracted from `main.ts`.
- Guard (Node): the browser project has no proxy and port ≠ 5173. `fetch` is stubbed.
- CI job `gui` (Decision 6), not required.
- `SelectionTable.browser.test.ts`: a sized table, 40 rows, two sticky leading columns and a pick column.
  For each scenario: scroll the box that really scrolls vertically by 300 px, wait 2 frames, then
  assert (i) the header's top is at the pin line ± 1 px, (ii) the header does not overlap the first
  visible row, (iii) at rest (`scrollTop = 0`) the header sits in the table's flow (lift 0), and
  (iv) after a horizontal scroll, each frozen `th`'s left equals its column's `td` left. Expected values
  come from the scroll box's geometry, never from `headerLift`.
  1. **plain**: the table directly in a vertically scrolling div (CSS sticky path; pin line = scroll
     box's top).
  2. **inside an `overflow-x` wrapper**: `fit="content"` with columns wider than the box, inside a
     vertical scroller (lift path; pin line = outer box's top).
  3. **under a pinned CollapsibleSection bar**: `<CollapsibleSection sticky>` wrapping case 2, as
     ImageTable does (pin line = outer box's top + bar height).
  4. **padded BaseModal body**: the table in a `BaseModal` whose body scrolls and has padding (pin line =
     the body's padding-box top; no row paints in the strip above the header).
- Watched failing: run on `origin/main`'s SelectionTable. State which scenarios fail there: 2 and 3 at
  least. Any scenario that passes on main stays as a regression guard, and the PR says which.
- Docs: `docs/DEV.md` → *Testing* gets a *Browser tests* paragraph. `docs/inventory/` lists `mount.ts`.

## Open questions

1. **Required check.** Advisory for two weeks, then flip (Decision 6), or required from day one?
2. **Firefox.** Playwright cannot drive system Firefox; it needs its patched build (~80 MB, CI download).
   Chromium only until a Firefox-only bug reaches your eyes?
3. **Trigger for `no browser test`.** A `fix/*` branch plus a `.vue`/CSS/layout-util change (cheap,
   some false positives, each costing one tag line), or every `.vue` change on any branch?
4. **Smoke scope.** Routes with and without `testpr` open (Decision 8), or empty state only (no
   custom.toml write on the CI runner)?
5. **Node frontend tests and "passes on base".** Close that gap in P2 (proposed), or as its own PR?

## Risks

- **Vitest browser-mode churn.** The API moved between 3 → 4 (providers split out). Pin exact versions.
  `update-deps.yml` / Dependabot bumps of vitest then exercise the `gui` job.
- **Flake from timing.** rAF-coalesced lifts and ResizeObserver settle. Use `frames(n)` and geometry
  tolerances. The first two weeks measure it (Decision 6).
- **Chrome version skew.** The runner's Chrome is not the maintainer's. Geometry assertions survive
  that; pixel diffs would not, which is why there are none.
- **Port clash with the dev server.** `strictPort` on 5173 (Decision 3). Guarded by the same Node test.
- **Leaking to the live backend.** Covered by Decision 3 and its guard. It is the one way a browser test
  could touch the maintainer's :8080.
- **Third-party RO loops** (`regl-scatterplot`) failing unrelated tests. The setup file names the
  creation site via `roLoopTrace`; an allow-list entry names the dependency, never the message.
- **Recital time.** passes-on-base for a browser test adds a Chromium launch (~15–30 s). It runs beside
  the reviewers, as the Julia leg does.
- **Primitive redesigns rewrite their tests.** Accepted: these tests are the component's behaviour
  contract, and a redesign that changes the contract should say so in a test diff.
