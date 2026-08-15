# Task list unification — one canonical list for both task surfaces

**Status:** planning — branch `refactor/task-list-canonical`. Nothing built yet.

## Goal

The app shows tasks in two places, and they are **two independently hand-rolled lists** that share no
component, no row markup, no selection idiom and no selection colour:

| Surface | File | Shape today |
|---|---|---|
| Module-page sidebar (every module page, + batch movies, + animation) | `frontend/src/tasks/TaskList.vue` | a **card stack** — `.task-item` boxes, rounded, per-status border tint, log expanded inline as a `<pre>` |
| Task Manager page (`/tasks`) | `frontend/src/modules/TasksModule.vue` | a **flat row list** — `.tm-row`, a selected row + a detail log pane on the right |

Neither uses `components/SelectionTable.vue`, which `docs/UI.md` → *UX primitive catalog* names as the
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
- [x] `docs/UI.md` UX-primitive catalog (+ a row for the "working, no fraction" case, so the next
      session doesn't animate this one) + `INVENTORY.md`.
- [x] `npm run typecheck` clean; `npx vitest run` 123 files / 1506 tests green.
- **Two deliberate behaviour changes, both merges of a split nobody chose:** one transition for all
  bars (0.2s ease — was 0.25s on two of them), and `min-width: 2px` on the fill everywhere (was on
  two of four), so a just-started job shows a sliver rather than an empty track. **Not yet seen in a
  browser** — Settings → Data patches and the project panel's export row are where to look.

### Phase 1 — the shared row helper
- [ ] Add `frontend/src/utils/taskRows.ts`: `taskRow(t, ctx)` → the row object (`id`, `label`, `seq`,
      `imageText`, `elapsed`, `elapsedMs`, `module`, `chainLabel`, `status`), plus the two predicates
      the surfaces already share in spirit (`isRunningWithProgress`).
- [ ] Add `frontend/src/utils/taskRows.test.ts` — blank/formatting cases, `elapsedMs` vs `elapsed`,
      the foreign-project prefix.
- [ ] `pixi run test-frontend` green.
- **Checkpoint:** no visual change yet; both surfaces still hand-rolled.

### Phase 2 — `/tasks` onto `SelectionTable`
- [ ] Replace `.tm-list` / `.tm-row` with `SelectionTable` (`single`, `sortStorageKey`,
      `columnWidthKey`, `actionsWidth`, `#empty`).
- [ ] Delete `.tm-row`, `.tm-row.selected`, `.tm-row.selected::before`, `.row-icon`, `.row-body`,
      `.row-top`, `.row-label`, `.row-actions` and the hover-reveal. **The purple selection rule goes
      with them** — this is where the reported symptom is fixed.
- [ ] Keep the toolbar, the filter chips, the two toggles, the throttle popover and the whole log pane
      exactly as they are.
- [ ] Add the `progress` column (Decision 7b) — new to this surface.
- **Checkpoint:** `/tasks` selection is amber; the list sorts; the log pane behaves as before; a
  running row shows its bar in the list, not only in the pane.

### Phase 3 — the sidebar onto `SelectionTable`
- [ ] Replace `.task-item` with `SelectionTable` (`none`, `fit="content"`, `columnWidthKey`,
      `#row-detail` for the expanded log only, `#cell-progress` for the bar — Decision 7b).
- [ ] Port the status tints to `rowClass` + `:deep()`; port the jump / expand / cancel / rerun / copy /
      dismiss buttons to `#actions`.
- [ ] Keep the heading row and its two list-wide actions (cancel-all, clear-finished) — they belong to
      `TaskList` itself and are why `BatchMoviesPanel` and `AnimationPanel` get them.
- [ ] Verify all three hosts: `TaskRunner` (every module page), `BatchMoviesPanel`, `AnimationPanel`.
- **Checkpoint:** the sidebar and `/tasks` are visibly the same list. **Needs Dominik's eyes in a
  browser before merge** — a 280px table is the part of this that cannot be judged from the diff.

### Phase 4 — docs + the stale notes
- [ ] `docs/UI.md` — the task-list sections (*Task list scoping*, the chain-badge note) still describe
      the card stack.
- [ ] `docs/todo/UX_PRIMITIVES_PLAN.md` → *Deliberately not extracted* → **Per-row disclosure in a
      list** cites `TaskList` as one of its two samples. Once `TaskList` uses `#row-detail`, that entry
      is down to one site (`ErrorConsole`) and its n=2 reasoning no longer describes reality. Rewrite
      it; do not leave it asserting something that stopped being true.
- [ ] `INVENTORY.md` — add the `utils/taskRows.ts` line (`CcProgressBar` landed in Phase 0).
- [ ] `pixi run test-frontend` — the CSS-scenario detectors are exact lists that fail on *improvement*
      too. Removing hand-rolled rules will move counts; lower them in the same change.

## Loose ends found on the way (not blockers)

- `.chain-pill` (`TasksModule.vue`) uses the raw hex `#a78bfa22` — `--cc-accent` at ~13% alpha. It
  escapes `findRawColours` only because the 8-digit form isn't an exact token match. Should be
  `color-mix(in srgb, var(--cc-accent) 13%, transparent)`. Purple is right here: it is a *badge*, not a
  selection.
- **Indeterminate** progress is out of scope. `CcProgressBar` is determinate only (a 0–1 value); a
  task that reports no fraction shows an empty progress cell, exactly as it shows no bar today. In a
  task row the "working, no number" cue is already the running status icon (`lib/taskStatus.ts`,
  `--cc-active` blue) — nothing needs a second one. (`components/plots/PlotSpinner.vue` is *not* the
  general primitive for this; it is a plot-area overlay driven by `useDelayedLoading`.)
