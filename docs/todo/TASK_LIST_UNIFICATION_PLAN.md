# Task list unification — one canonical list for both task surfaces

**Status:** built, phases 0–4 — branches `refactor/task-list-canonical` (Phase 0, PR #576) and
`refactor/task-rows` (phases 1–4). **Not yet seen in a browser**; the sidebar table is the part that
needs eyes. Promote the durable parts into `docs/UI.md` and retire this file once it has been used
in anger.

## Goal

The app shows tasks in two places, and they are **two independently hand-rolled lists** that share no
component, no row markup, no selection idiom and no selection colour:

| Surface | File | Shape today |
|---|---|---|
| Module-page sidebar (every module page, + batch movies, + animation) | `frontend/src/tasks/TaskList.vue` | a **card stack** — `.task-item` boxes, rounded, per-status border tint, log expanded inline as a `<pre>` |
| Task Manager page (`/tasks`) | `frontend/src/modules/TasksModule.vue` | a **flat row list** — `.tm-row`, a selected row + a detail log pane on the right |

Neither uses `components/SelectionTable.vue`, which `docs/ui/PRIMITIVES.md` names as the
canonical control for **ANY table of rows and columns — pick one, pick many, or a plain list**. This is
the divergent-re-implementation trap in `CLAUDE.md`, on the two surfaces where it is most visible: they
show the same objects, from the same store, side by side in the same session.

Both surfaces move to `SelectionTable`.

## What the drift actually cost — the visible symptom

`/tasks` highlights the selected row in **purple**:

```css
/* modules/TasksModule.vue */
.tm-row.selected::before { … width: 3px; background: var(--cc-accent); }   /* #a78bfa */
```

`SelectionTable` highlights the selected row in **amber**, with the *same* left-rule idiom:

```css
/* components/SelectionTable.vue */
.sel-on td:first-child { box-shadow: inset 2px 0 0 var(--cc-selected); }   /* #ff8c1a */
```

So `/tasks` is a hand-rolled copy of the canonical selection affordance, in the wrong colour. Per
`style.css` the two tokens are not interchangeable and say opposite things:

- `--cc-accent` (purple) — **form/control chrome**. A selected row is not a control.
- `--cc-selected` (amber) — **the selection/active highlight for boxes, panels and rows**.

This is drift, not a decision. `git log` dates it: `.tm-row.selected::before` is from the **initial
commit**; `--cc-selected` arrived **2026-07-14** and `SelectionTable` **2026-08-03**. `/tasks` was
simply never revisited. Adopting the canonical table deletes the rule and the question with it.

## Decisions

1. **Both surfaces render through `SelectionTable`** (`selectionMode`: `none` for the sidebar, `single`
   for `/tasks`, whose row selection drives the log pane). No second list component, no fork.

2. **The sidebar becomes a real table, header and all** (Dominik, 2026-08-15). `SelectionTable` renders
   its `<thead>` unconditionally and **no `headerless` prop is being added** — the point of the change
   is that the two surfaces look like each other and like every other list in the app, and an
   exception carved for the narrow panel is how the next divergence starts. The sidebar gains sortable
   columns as a side effect, which it has never had.

3. **The sidebar's columns are allowed to outgrow the panel.** The task-runner panel defaults to 280px
   (`usePanelResize`, min 200 / max 600). Use the documented pattern for exactly this — `fit="content"`
   + `columnWidthKey` + an `overflow-x: auto` wrapper (`docs/UI.md` → *Drag-resizable table columns*).
   Column widths then persist per user and are draggable. `actionsWidth` **must** be declared: under
   `table-layout: fixed` the trailing column gets only the leftover, and this row carries up to five
   buttons.

4. **The two log presentations stay different, deliberately.** The sidebar expands the log **inline**
   (`#row-detail`); `/tasks` keeps its **side detail pane**. A 280px panel has no room for a detail
   pane, and the manager page's whole layout is the pane. Unifying the *list* does not mean unifying
   the *log surface* — do not "finish the job" by forcing one of these onto the other.

5. **Row objects are built by a shared pure helper**, `frontend/src/utils/taskRows.ts`, with a vitest
   beside it. `SelectionTable` renders `row[key]` and sorts by row fields, so both surfaces need a
   `TaskEntry` → row mapping; that mapping (label, image text, the raw `elapsedMs` behind the formatted
   `elapsed`, the module tag) is the same on both and must not be written twice. This is also the only
   way the logic is testable — frontend tests are pure `.ts` only, no component mounting
   (`CLAUDE.md` → *Testing*).

6. **Per-status row tint via `rowClass` + `:deep()`.** The established precedent is `ImageTable`'s
   `.row-excluded` (`:row-class` on the table, `:deep(.row-excluded)` in the caller's scoped CSS).
   The sidebar's four status tints (`st-running`/`st-failed`/`st-done`/`st-cancelled`) port to it
   directly; the rounded per-card border does not survive and is not replaced.

7. **Progress gets a `CcProgressBar` primitive and its own narrow column.** Two parts, because
   measuring turned a one-consumer question into a rule-of-three one.

   **7a — extract the primitive.** There are **four** hand-rolled determinate progress bars, and they
   have already drifted on every axis except their colour tokens:

   | Site | height | radius | transition | min-width | fill markup | % maths |
   |---|---|---|---|---|---|---|
   | `tasks/TaskList.vue` `.task-progress` | 3px | — | 0.25s ease | 2px | `<div>` | `(p*100).toFixed(1)` |
   | `modules/TasksModule.vue` `.log-progress` | 3px | — | 0.25s ease | 2px | `<div>` | `(p*100).toFixed(1)` |
   | `modules/SettingsModule.vue` `.patch-bar` | 4px | `--cc-radius-xs` | 0.2s | — | `<span>` | `p*100`, unrounded |
   | `components/ProjectPanel.vue` `.pp-io-bar` | 4px | `--cc-radius-xs` | 0.2s | — | `<div>` | `Math.round(p*100)` |

   Two heights, two radii, two transitions, `min-width` on half of them, and three different ways to
   turn a 0–1 fraction into a width. Nobody chose any of those against the others; each was chosen
   once, alone — the same shape as the `.cc-row` extraction in `docs/todo/UX_PRIMITIVES_PLAN.md`.
   `CcProgressBar` takes `:value` (0–1) and a `size` (`thin` 3px / `bar` 4px+radius, since 3px flush
   inside a row and 4px rounded as a standalone element are both real), and owns the clamping and the
   rounding. Track `--cc-surface-2`, fill `--cc-accent` — the two things that did *not* drift.

   **7b — where it sits in the row.** A narrow `fixed` **`progress` column** via `#cell-progress`,
   rendered only while the row is running. `ProjectPanel` is the precedent and the closest analogue —
   a task row with a label, a status and an inline `flex: 0 0 90px` bar, not a bar spanning the row.

   **Rejected: riding `#row-detail` with a widened `isExpanded`.** `#row-detail` is one `<tr>` gated by
   one predicate, so progress and the expanded log share it; making `isExpanded` return true for a
   running row the user never expanded overloads the predicate into "has anything below it" and leaves
   the next reader with a name that lies. **Also rejected: a `rowStyle`/`#row-underlay` prop on
   `SelectionTable`** for a full-row gradient underlay — that is new canonical-component API invented
   for one caller, which UX_PRIMITIVES_PLAN is explicit about not doing.

   **Consequence: `/tasks` gains a per-row bar it does not have today** (it currently shows progress
   only in the log pane). That is the point — one column definition, both surfaces.

8. **Row actions become always-visible on both.** They are always visible in the sidebar and
   hover-revealed (`opacity: 0`) on `/tasks` — a difference nobody chose. `SelectionTable`'s `#actions`
   has no hover-reveal, so adopting it settles it toward always-visible. Note that `ImageTable`'s
   hover-reveal is called out in UX_PRIMITIVES_PLAN as *deliberately bespoke*; that is about a
   different control and does not extend here.

## Columns

**`/tasks` (`selectionMode: 'single'`, `#actions`)**

| key | label | notes |
|---|---|---|
| `status` | — | `fixed`, ~28px, `#cell-status` → the `TASK_STATUS` icon. No label. |
| `module` | Module | `sortable`. `#cell-module` → the `cc-module-tag` pill (`moduleTagStyle`). New: not sortable today. |
| `task` | Task | `sortable`, `ellipsis`. `#cell-task` → `#seq` + chain pill + label. |
| `image` | Image | `sortable`, `ellipsis`. `#cell-image` → the `foreignProject` prefix + `imageName`. |
| `progress` | — | `fixed`, ~70px, no label. `#cell-progress` → `CcProgressBar size="thin"`, rendered only while running (blank otherwise, so the column doesn't read as "0%"). |
| `elapsed` | Time | `sortable` via `sortKey: 'elapsedMs'` — the formatted string must never be sorted as text. |

**Sidebar (`selectionMode: 'none'`, `#actions`, `#row-detail`)** — same set minus `module` (the list is
already scoped to one module, so the column would hold one repeated value). `#row-detail` now carries
**only** the expanded log, and `isExpanded` means exactly what it says.

## Phases

Each phase is independently shippable and independently viewable in the browser.

### Phase 0 — `CcProgressBar` (independent of everything else) — **DONE**
- [x] Add `frontend/src/components/CcProgressBar.vue` — `:value` (0–1, clamped), `size`
      (`thin` | `bar`). Owns the track, the fill, the transition and the fraction → width maths.
- [x] `frontend/src/utils/progress.ts` (`progressWidth`) + `progress.test.ts` — the sum that had
      drifted three ways, pinned.
- [x] Migrate all four sites (Decision 7a): `TaskList`, `TasksModule` (log pane), `SettingsModule`
      (`.patch-bar`), `ProjectPanel` (`.pp-io-bar`). Eight scoped rules deleted; the three classes that
      survive are geometry only (`flex-shrink`, `margin-bottom`, `flex: 0 0 90px`).
- [x] `docs/ui/PRIMITIVES.md` (+ a row for the "working, no fraction" case, so the next
      session doesn't animate this one) + `INVENTORY.md`.
- [x] `npm run typecheck` clean; `npx vitest run` 123 files / 1506 tests green.
- **Two deliberate behaviour changes, both merges of a split nobody chose:** one transition for all
  bars (0.2s ease — was 0.25s on two of them), and `min-width: 2px` on the fill everywhere (was on
  two of four), so a just-started job shows a sliver rather than an empty track. **Not yet seen in a
  browser** — Settings → Data patches and the project panel's export row are where to look.

### Phase 1 — the shared row helper — **DONE**
- [x] Add `frontend/src/utils/taskRows.ts`: `taskRow(t, ctx)` / `taskRows(...)` → the row object
      (`id`, `seq`, `status`, `module`, `task`, `image`, `imageUid`, `projectLabel`, `chainLabel`,
      `chainTip`, `elapsed`, `elapsedMs`, `progress`, `hasProgress`, `canRerun`, `entry`).
- [x] Add `frontend/src/utils/taskRows.test.ts` — blanks vs zero, `elapsedMs` vs `elapsed` (with the
      `4m 12s` < `59s` case asserted through `sortRows`), the foreign-project label, chain fallback.
- [x] Shipped together with Phase 2 rather than alone — an exported helper with no consumer is dead
      code for a reviewer to puzzle over, and the repo's rule is the test ships with the code.

### Phase 2 — `/tasks` onto `SelectionTable` — **DONE**

- [x] Replace `.tm-list` / `.tm-row` with `SelectionTable` (`single`, `sortStorageKey`,
      `columnWidthKey`, `actionsWidth`, `fit="content"`, `#empty`).
- [x] Delete `.tm-row`, `.tm-row.selected`, `.tm-row.selected::before`, `.row-body`, `.row-top`,
      `.row-label`, `.row-image`, `.tm-empty`, `.row-actions` and the hover-reveal. **The purple
      selection rule went with them** — the reported symptom is fixed.
- [x] Keep the toolbar, the filter chips, the two toggles, the throttle popover and the whole log pane
      exactly as they are.
- [x] Add the `progress` column (Decision 7b) — new to this surface.
- [x] `.chain-pill`'s raw `#a78bfa22` → `color-mix(…)` (was a loose end below; the file was open).
- [x] `npm run typecheck` clean; `npx vitest run` 124 files / 1518 tests green.
- **Two things to look at in a browser:** the list pane went **340px → 460px** to fit six columns
  (it eats that much from the log pane), and the row actions are now **always visible** rather than
  hover-revealed (Decision 8). Sorting and per-column drag-resize are both new here.

### Phase 3 — the sidebar onto `SelectionTable` — **DONE**
- [x] Replace `.task-item` with `SelectionTable` (`none`, `fit="content"`, `columnWidthKey`,
      `#row-detail` for the expanded log only, `#cell-progress` for the bar — Decision 7b).
- [x] Port the status tints to `rowClass` + `:deep()` — keyed off `TASK_STATUS[...].tone`, so the four
      raw hexes the card stack carried (`#1e3a5f18`, `#7f1d1d18`, `#14532d55`, `#3f3f4666`) are gone
      and the tint comes from the same tokens as the status light. `st-done` had only a border colour,
      which a table row has nothing to do with — dropped; the icon already says it.
- [x] Port the jump / expand / cancel / rerun / copy / dismiss buttons (jump stays in the Task cell,
      where it reads as belonging to the task's identity; the rest to `#actions`).
- [x] Keep the heading row and its two list-wide actions (cancel-all, clear-finished).
- [x] Deleted a dead rule while there: `.task-item:hover .jump-btn { display: inline-flex }` had no
      `display: none` base to reverse, so it never did anything.
- [x] `npm run typecheck` clean; `npx vitest run` 124 files / 1518 tests green.
- **The card look is gone** — rounded per-status bordered cards become table rows. That is the point,
  and it is also the biggest visual change in the whole plan. **Needs Dominik's eyes in a browser**:
  a 280px-wide table with six columns and five row buttons is what cannot be judged from a diff.
- [ ] Verify all three hosts in the browser: `TaskRunner` (every module page), `BatchMoviesPanel`,
      `AnimationPanel`.

### Phase 4 — docs + the stale notes — **DONE**
- [x] `docs/UI.md` — both surfaces documented, including why the sidebar's two differences follow from
      its width.
- [x] `docs/todo/UX_PRIMITIVES_PLAN.md` → *Per-row disclosure in a list* — its n=2 reasoning is now
      n=1 (`ErrorConsole` alone), rewritten rather than left asserting something untrue.
- [x] `INVENTORY.md` — `utils/taskRows.ts` (+ `CcProgressBar` in Phase 0).
- [x] `pixi run test-frontend` — the CSS-scenario exact lists needed no adjustment.

## Loose ends found on the way (not blockers)

- ~~`.chain-pill`'s raw `#a78bfa22`~~ — done in Phase 2.
- **Indeterminate** progress is out of scope. `CcProgressBar` is determinate only (a 0–1 value); a
  task that reports no fraction shows an empty progress cell, exactly as it shows no bar today. In a
  task row the "working, no number" cue is already the running status icon (`lib/taskStatus.ts`,
  `--cc-active` blue) — nothing needs a second one. (`components/plots/PlotSpinner.vue` is *not* the
  general primitive for this; it is a plot-area overlay driven by `useDelayedLoading`.)
