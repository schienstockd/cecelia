# In-app guide system — bubble walkthroughs

Status: **built** (P1–P4) · branch `work/guide-system` · durable docs promoted to `docs/UI.md` → *Guides*
and `INVENTORY.md`. This file stays as the decision record — the *why* behind D1–D10 and the
reservations, which the reference docs deliberately don't repeat.

**What shipped, against the plan.** All four phases, 11 guides (the 7 planned + 4 obvious candidates —
see *The guides*). Changes made while building, each for a reason found in the code:

- **The runtime reads the route from `location.hash`, not `useRoute()`.** No other store touches
  vue-router, and a store that needs router injection context would be the first. Hash history means
  the hash *is* the route.
- **A `~250ms` poll while a guide is open** — not in the plan, and unavoidable: two gate kinds are DOM
  reads (`anchorValue` over `TaskRunner`'s `<select>`; "is the anchor on screen yet"), which Vue cannot
  track. One interval beats teaching six components to publish their local state.
- **Auto-advance is armed per step**, on entry, and only when the gate starts out unsatisfied — so a
  step already satisfied when you arrive shows a tick and waits for `Next`. (The first attempt watched
  for a false→true transition on the gate alone; see the browser findings below for why that isn't the
  same thing.)
- **`images.qcDot` replaced a `layout.plotsSection` step in the drift guide** — Cleanup has no `#plots`
  slot, so the planned "did it work?" step would have pointed at nothing and claimed something false.
  The drift QC is real (`drift.unreliable`/`drift.jump`, `app/src/qc.jl`); it surfaces on the row.
- **`PREREQ.projectOpen` carries no `fixGuide`.** The plan had it point at the import guide, which needs
  an open project itself — circular. The anchor ratchet caught it on first run.
- **No "show me the run" button on the parked bubble.** It would have had to un-collapse the functions
  panel and scroll the task list — the guide reaching into app state, which D1 forbids. It says where to
  look instead.
- **Found in the browser, by Dominik, within minutes of first use — all three now covered:**
  1. *The route never updated.* `createWebHashHistory` navigates by `pushState`, which fires no
     `hashchange`, so `currentPath` sat at the boot path and every routed step said "back to
     /manage-images". Now polled + re-read on start; the parsing half is `routePathFromHash` (tested).
  2. *The guide fast-forwarded to the last step on its own.* Auto-advance watched for a false→true
     gate transition, which across a step change compares the new step's gate to the OLD step's — so
     landing on an already-satisfied step read as "just satisfied". Now armed per step, on entry.
  3. *"Pick a set" was a dead end with no sets.* Nothing to select, so the gate could never pass. Now
     a `reveal` pointing at **New set** — the D5 mechanism, no new machinery, and it applies to the
     shared builder too, so every module-task guide inherits it. Needed one new `GuideCtx` field
     (`setCount`): "which set is active" and "are there any" are different questions.
- **Layout reworked after first sight** (Dominik): the picker is divided rows, not 11 boxed cards. The
  action column was a stretch column that also held the "X first" button, so `Start` took the width of
  its widest sibling and the right edge zigzagged; met prerequisites were listed as chips, which was
  pure wrapping for no information. Now: fixed-width action column, readiness + step count in one
  right-aligned meta slot, and only *missing* prerequisites get a line with the fix inline on it.
- **The FileBrowser grew a horizontal scrollbar** (pre-existing, surfaced by the import guide): the
  table was `width: 100%` with the default `table-layout: auto`, where 100% is only a MINIMUM, so one
  long filename widened it past the modal. Beside it sat a dead `.col-name { flex: 1 }` — `flex` does
  nothing on a table cell, which is why the intended flexible sizing never happened. Now
  `table-layout: fixed` + `overflow-wrap: anywhere` on the name cell. The horizontal scroll also made
  the ring visibly track the scroll, so the guide now points at the select-all checkbox rather than
  ringing the whole table, and the ring is clamped to the viewport.
- **The task steps pointed at the wrong control when the panel was collapsed a different way.** A
  control in the functions panel can be unusable three ways, each needing different advice, and the
  first version knew about one: `.task-runner.pane-bottom` hides the function select, the parameters
  AND the Run button (fix: the pane toggles, `layout.paneBar`), `.pane-top` hides the task list, and
  the parameters block does not exist at all until a function is chosen (`v-if="taskDef"` — fix: point
  back at the dropdown). Pointing at the right-panel handle in the pane case is worse than useless:
  clicking it hides everything. `reveal` now takes a LIST of causes, first match wins, with the last as
  the fallback for an unforeseen way of hiding something. Needed `anchorReachable` in `GuideCtx` — "in
  the DOM" and "on screen" are different questions with different fixes.
- **The import guide described a flow that does not exist.** It said adding files starts conversion
  "straight away" and then sent the user to open the image in napari — which cannot work, because the
  eye is disabled until the image reads `done`. In reality **"Add images" only registers rows**
  (`POST /api/images/register`); converting to OME-Zarr is an ordinary task run
  (`importImages.omezarr`) the user dispatches through the normal TaskRunner. This was an invented
  flow, not a UI bug: the guide asserted behaviour nobody had checked. The convert phase is now the
  SHARED task-run block — `taskRunSteps()`, extracted out of `moduleTaskGuide` so the import guide
  splices it in mid-sequence rather than becoming a seventh hand-written copy — and the ratchet
  registration moved into that block, so the import guide's selection scope is checked too.
- **The Segment guide taught the wrong function** — the same invented-flow error as import, one layer
  down. It picked plain `segment.cellpose`, which produces labels with **no measures**, then ended with
  "now gate on these" and a QC step; but gating/tracking/clustering all read labelProps, and the
  `segmentation_qc` plot is declared on the MEASURE step. So its own ending could not work. It now
  teaches the composite `segment.cellposeMeasure` ("Cellpose segment + measure"), with a `funHint`
  saying why not the plain one beside it in the dropdown. A composite reports to the task rail under its
  own `fun_name` (the frontend stamps the selected def's), so the park step still resolves.
  **Ratchet:** `app/test/suite.jl` → *guide catalogue names real tasks* — every `funName` a guide names
  must be in `_fun_name_map()`, and its `taskKey` must equal that spec's `task`. Julia-side because the
  specs live there and the frontend holds no copy (the same reasoning as the task-spec `tip` check).
  Choosing the *wrong* function is a judgement no test can make; naming an unreal one is.
- **Segmentation previews before it runs.** `withPreview` adds a step at the preview control — real
  compute over just the region napari is showing, seconds instead of minutes, which is how you judge
  the diameter and channels. The control is `v-if`'d out unless exactly one image is selected, so the
  step reveals "tick a single row" rather than pointing at a button that isn't rendered.
- **The ring framed the wrong region for a tall control.** `getBoundingClientRect()` ignores clipping,
  so `TaskRunner`'s parameters block reported the full height it has inside a shorter scrolling panel
  and the ring was drawn mostly outside the panel (Dominik, 2026-08-12). `visibleRect()` intersects the
  anchor with every clipping ancestor and the viewport; the ring AND the bubble placement now follow
  what is actually on screen.
- **`PREREQ.imageImported` hand-rolled a second definition of "imported"** (`status === 'done'`), so a
  project full of long-since-converted images reported it MISSING and every guide read as blocked
  (Dominik, 2026-08-12). `status` is the transient conversion-job state; the canonical answer is
  `isImported` (`utils/inclusion.ts` — does the image HAVE a converted file), which is what the image
  table uses to decide whether the napari eye is enabled. The import guide's own gates and copy went
  the same way: they referenced a "Status column reads done", but that column shows the per-MODULE task
  status and reads "—" for an image with none. Third instance in this build of the same failure — a
  predicate invented instead of looked up.
- **Clustering is TWO guides, because it is two pages.** Cells (`clustPops.cluster`, needs a
  segmentation) and tracks (`clustTracks.cluster`, needs TRACKING) share the Leiden/UMAP engine but not
  their input: one row per cell vs one per track, with cell measures aggregated per track. A user who
  has only segmented cannot use the track one, so collapsing them would have made the prereq a lie.
  Both param lists now come from their specs rather than from memory.
- **Tracking teaches its composite too** (`tracking.bayesian_track_measures`, "Bayesian track +
  measures"), for the same reason as segment: bare tracks carry no per-track measures, and speed /
  displacement / angle are exactly what the HMM fits and what track clustering and track gating read.
  The downstream guides' selection steps now say the tracking must have measured.
- **A finished run draws nothing on the image until you toggle it.** The napari steps implied the
  overlay appears with the image; it does not — each segmentation has its own row in the Viewer panel
  with per-overlay switches, and until you flip one napari shows the raw channels and the run looks like
  it did nothing (Dominik, 2026-08-12). Segment and track now walk open-in-napari → open the Viewer
  panel → **switch the overlay on** → judge it, with `viewer.toggleLabels` / `viewer.toggleTracks`.
- **Both cluster guides end by defining populations from the result.** Clustering leaves numbered
  clusters, which are not populations: you open the population manager, create a population and tick
  cluster IDs into it (no gate to draw). That is the step that makes the result usable anywhere a gated
  population is, so stopping at the UMAP left the guide short of the point (Dominik, 2026-08-12).
- **One anchor id can match several live elements**, and taking the first in DOM order was wrong: each
  floating gating plot carries its own axis controls, so the ring landed on plot 1 while the user worked
  in plot 2 — and because the ring sits above the app, it drew straight across the panel in front.
  `rankAnchorCandidates` (pure, tested) prefers visible → inside the active panel (`.panel.active`) →
  unoccluded, keeping DOM order on a tie so a per-row anchor still points at the first row.
- **The unanchored fallback names the failing anchor in its tooltip**, so "it didn't highlight
  anything" is a precise report next time rather than a guess.
- **The whole guide surface is `--cc-guide` (whitish), matching the lab-log panel** — the bubble
  border, the ring, and the compass mark in the header and dialog title. A new token, which also gave
  the lab log's hardcoded `rgba(255,255,255,0.6)` a home. (The compass was briefly green, reusing
  `--cc-viewer`; white won because the mark and the surface it opens then read as one thing, and
  against a bare button's dim grey a soft white still stands out.)
- **Three house ratchets rejected the first version of the new components** (an undeclared re-armed
  timer, a hand-rolled `.cc-row`, two `cc-eyebrow` colour overrides). All three were fixed at the
  source rather than allow-listed.

## Goal

Replace "can you send me a screencast" with an in-app, click-through guide. A **compass button next
to the Cecelia brand mark** opens a modal listing the basics — import images, drift correct, segment,
gate populations, track cells, make a notebook, build plots. Starting one closes the modal and puts a
**bubble beside the real control**; doing the thing pops the next bubble. The user learns on **their
own data**, in the real app, at their own pace — which is exactly what a silent screencast can't do.

Reference: `docs/UI.md` (UX primitive catalog · Onboarding · Explainer sketches), `INVENTORY.md`,
`docs/todo/ONBOARDING_PLAN.md` (the setup wizard + `HintCallout` this builds on),
`docs/todo/WHATS_NEW_PLAN.md` (the tip catalogue this links to).

---

## What already exists (do not rebuild)

| Need | Already built |
|---|---|
| Anchored, body-teleported, viewport-clamped floating box | `components/TeleportPopover.vue` — its `reposition()` is the maths we want |
| Centred modal shell | `components/BaseModal.vue` |
| Static how-to content as data + a card renderer | `lib/tips.ts` + `lib/whatsNew.ts` + `components/WhatNewCard.vue` |
| One-shot dismissible hint | `components/HintCallout.vue` (`cc.hint.<id>` in localStorage) |
| "What has been done to this image" | `CciaImage.runLog` + `utils/runLog.ts` (`funsRun`/`wasProcessedWith`/`funsRunAcross`) |
| What outputs an image has | `CciaImage.labels` / `branchLabels` / `spatialGraphs` / `qc`, already client-side in `stores/project.ts` |
| Live task state (queued/running/done/failed, per module + project) | `stores/tasks.ts` (`forModule`, `lastStarted`) + the `task:status` WS event |
| Route table with labels, hash history | `main.ts`; sidebar nav is data-driven (`groups` in `AppSidebar.vue`), so nav items are addressable as `a[href="#/segment"]` — no new markup there |
| User-facing copy ratchet | `utils/uiCopy.ts` + its test |

**The gap is not the plumbing — it is (a) a bubble with a tour event model, (b) stable anchors in the
markup, (c) the step catalogue.** Everything a guide needs to *observe* is already in a store.

`lib/tips.ts` already carries the exact topic list Dominik named (segmentation, tracking, HMM,
gate-then-napari) as short text cards. **The guide is the interactive form of a tip, not a second
catalogue** — see D7.

---

## Locked decisions

### D1 — The guide points and observes. It never clicks, selects, or navigates.

The runtime has **no write access to app state**. It does not select an image for you, does not
switch pages, does not press Run. Consequences, all of them good:

- **It cannot break a project.** No path exists from a guide to a mutation, so "the guide started a
  12-minute segmentation on the wrong image" is structurally impossible.
- **Navigating is part of what's being taught.** A step that needs another page anchors to the
  *sidebar nav item* and waits for the click, rather than teleporting the user somewhere they didn't
  ask to go and can't retrace.
- One-way data flow: stores → guide, never back. The guide is a reader, like the analysis canvas.

### D2 — A step advances on the real action; `Next` is the escape hatch

Each step declares how it completes. Four gate kinds, in order of preference:

| Gate | Meaning | Source |
|---|---|---|
| `when(ctx)` | a predicate over the stores — *an image is selected*, *the task select is on `segment.cellpose`*, *this image has labels* | `stores/project`, `projectMeta`, `settings`, `route` |
| `clickAnchor` | advance when the anchor element is clicked — for controls with no observable end state | one DOM listener on the resolved anchor |
| `awaitTask({ fun })` | **park** until a matching task reaches `done` | `stores/tasks` + `task:status` |
| none | pure prose — `Next` only | — |

`Next` is always present and always enabled. A gate makes the bubble *wait and confirm*; it never
traps anyone. A satisfied gate auto-advances after a beat (~400ms, so the user sees their action
acknowledged rather than the bubble vanishing under the cursor).

### D3 — Long jobs park; the guide resumes when the task finishes

`awaitTask` turns the bubble into a spinner + "Segmenting — I'll pick up when it's done", moved off
the Run button and onto the **task rail** (which is where the user should be looking anyway). So the
segment → check QC → gate story stays *one* guide instead of three that each need re-discovering.

Three exits, all defined, none silent:
- **done** → advance to the next step.
- **failed** → bubble turns to the fail state: what failed, and a button that opens the task log.
  Guide stays open at that step; the user can retry and it re-arms.
- **user exits / closes** → guide state is dropped. No resurrection on next launch (a half-finished
  tour reappearing days later is worse than not having one).

### D4 — Anchors are explicit `data-guide` attributes, ratcheted by a test

There is **no** `data-testid` anywhere in the frontend today, and CSS-selector anchors
(`.cc-btn-primary` inside `TaskRunner`) break the first time someone restyles a button — silently,
and only for the user being onboarded, who won't report it.

So: `data-guide="segment.run"` on each target, namespaced `<guide-area>.<control>`. And because a
broken anchor is invisible from the inside, **a test asserts every anchor id in the catalogue occurs
in the source** (grep over `frontend/src/**`, the same reflex as the `uiCopy` ratchet and the
`no_bare_write_h5ad` detector). Renaming a button then fails CI instead of quietly killing a guide.

Belt and braces: an unresolvable anchor at runtime **degrades to a centred card with the same copy**.
A guide never dead-ends on a missing button.

### D5 — A step can require its target to be *reachable*, not just present

This app hides things by design: `settings.rightPanelCollapsed` folds the whole TaskRunner away,
`CollapsibleSection`s collapse, `FloatingPanel`s (Viewer, Lab log) default closed, and panels scroll.
Pointing at a `display: none` button is the obvious way for this to look broken.

So the resolver checks *visibility*, not just existence, and a step may carry a `reveal` precondition
— a predicate + its own one-line bubble ("Expand the task panel — the handle on the right edge") that
is inserted ahead of the step when the target is hidden. Scroll-into-view is automatic
(`scrollIntoView({ block: 'center' })` on the resolved element).

### D6 — Prerequisites are declared per guide, auto-checked, and shown before you start

Guides run on the user's own data (D1 — there is no demo project; see R2). So the picker modal must
tell you *up front* what a guide needs, and check it:

```ts
prereqs: [PREREQ.projectOpen, PREREQ.imagesInSet, PREREQ.segmented]
```

Each `Prereq` is `{ id, label, ok(ctx), fixGuide? }` — a short human label, a predicate, and
optionally the guide that satisfies it. The modal renders ✓ / ✗ per row and, on a miss, offers
**"Start *Import images* first"** rather than a dead Start button. Every predicate resolves from data
already in the stores — no new endpoint:

| Prereq | Predicate |
|---|---|
| a project is open | `projectMeta.current != null` |
| the set has images | active set `images.length > 0` |
| an image finished converting | some image `status === 'done'` |
| something is segmented | some image with a non-empty `labels` |
| something is tracked | `funsRun(runLog)` intersects the `track.*` funs |
| populations exist | gating sidecar via the existing population fetch (the one case that needs a call) |

Start is never *blocked* — a miss is a warning plus a better suggestion. The user may know better.

### D7 — One content catalogue: a tip card can launch its guide

`lib/tips.ts` already describes these same topics. Duplicating that prose into step definitions
guarantees the two drift. Instead: add `guideId?: string` to `WhatNewCard`, and `WhatNewCard.vue`
grows a **"Show me" button** when it is set → starts the guide, closes the modal. The tip stays the
one-paragraph summary + sketch; the guide is the click-through. Content lives in one place per topic,
and the What's New modal becomes a second discovery surface for free.

### D8 — Three of the guides are one parameterised builder, not three step lists

Drift correct, segment and track are the *same* five moves — pick set → select image(s) → choose the
function in `TaskRunner`'s `<select>` → set params → Run → watch the rail. That skeleton lives in
`ModuleLayout` + `TaskRunner`, i.e. **two files and four anchors**, shared by every module page.

So `lib/guides/moduleTask.ts` exports a builder:

```ts
moduleTaskGuide({
  id: 'segment-an-image', route: '/segment', navLabel: 'Segment',
  fun: 'segment.cellpose', funLabel: 'Cellpose',
  prereqs: [PREREQ.projectOpen, PREREQ.imageConverted],
  params: [{ key: 'models', text: '…' }, …],   // the only genuinely per-guide prose
  after: [ … ],                                 // QC / napari steps
})
```

A new module page gets a guide in ~10 lines. Writing the fourth one by hand is how this becomes four
diverging variants (`docs/UI.md` → generalise by *scenario*, not per widget).

### D9 — Ring + bubble, no dimmed backdrop

A spotlight overlay has to fight `FloatingPanel`'s stacking (`utils/panelStack.ts`), `cc-popover`'s
`z-index: 1000`, PrimeVue overlays and the napari window. And dimming is hostile when the point is
for the user to *work* — they need to read a value in the panel the overlay just greyed out.

So: a **highlight ring** positioned over the target (a `position: fixed` outline, `pointer-events:
none`) plus the bubble. Nothing is blocked, nothing is dimmed, the app stays fully usable mid-guide.
One new stacking level above `1000`, registered in `style.css` next to the existing z tokens.

### D10 — `GuideBubble` is its own component; the *positioning maths* is extracted and shared

`TeleportPopover` is 80% right and 20% wrong, and the 20% is the event model: it dismisses on
outside-click (for a guide, clicking outside is usually *the action being taught*), offers only
`bottom-start`/`bottom-end`, and has no arrow, footer or ring. Bending it to do both would give one
component two contradictory dismissal contracts.

Following the `FloatingPanel` vs `useFloatingPanel` precedent (deliberately separate, different event
model), the split is:

- **new** `utils/anchorPosition.ts` — `anchorRect → { top, left, side }`, viewport-clamped with
  flip-when-it-would-overflow, all four sides. Pure ⇒ unit-tested. **`TeleportPopover` is refactored
  to call it**, so there is one positioner, not two. (Its current logic *is* this, minus two sides.)
- **new** `components/GuideBubble.vue` — teleported bubble + arrow + ring + footer (`3 / 8`, Back,
  Next / waiting, Skip step, Exit). Dismisses on `Escape` and Exit only.

Refactoring `TeleportPopover` (7 call sites) is deliberately in scope: adding a second positioner
next to it is how the "my popover gets clipped" bug comes back in a new file.

---

## Shape

```
frontend/src/
  stores/guide.ts                 active guide · step index · waiting/failed state · completed set
  lib/guides/
    index.ts                      GUIDES catalogue (grouped like the sidebar) + PREREQ registry
    types.ts                      GuideDef · GuideStep · Gate · Prereq
    moduleTask.ts                 D8 builder (drift correct · segment · track · future modules)
    importImages.ts               \
    gatePopulations.ts             |  the guides whose shape is genuinely their own
    notebook.ts                    |
    plots.ts                      /
    guides.test.ts                every anchor id exists in source (D4) · prereqs are pure · no
                                  step lacks both copy and a gate
  components/
    GuideBubble.vue               the bubble (D10)
    GuidesDialog.vue              BaseModal picker: groups · prereq ✓/✗ · Start · done ticks
  utils/anchorPosition.ts         extracted positioner + test (D10)
```

- **One mount in `App.vue`**, beside `WhatsNewDialog` — the bubble must survive route changes.
- Header gets one button in `AppHeader.vue`: `pi-compass`, right of the brand mark, tooltip
  `Guides — walk through the basics`. **Deliberately not a `?`**: the brand mark already opens
  What's New + tips, and a `?` already means "what is this Claude panel" in `LabLogPanel`. Three
  different `?`s in one app is worse than one new icon.
- Completion is remembered per guide (`cc.guide.<id>.done`, the `HintCallout` idiom) — a tick in the
  picker, nothing more. No nagging, no badge (`WHATS_NEW_PLAN` D3 already ruled out a fifth surface).

### Anchors to add

~35–45 attributes, concentrated in shared components — which is why D8 pays off:

| File | Anchors | Serves |
|---|---|---|
| `components/SetBar.vue` | set select, new set | every guide |
| `components/ImageTable.vue` | row checkbox, eye (open in napari), info, run tag | every guide |
| `components/ModuleLayout.vue` | right-panel handle, filter, plots section | every guide (D5 reveals) |
| `tasks/TaskRunner.vue` | function select, params area, Run, pool select | drift · segment · track |
| `tasks/TaskList.vue` | the task rail, a task's log button | D3 park + fail |
| `modules/ManageImagesModule.vue` + `FileBrowser.vue` | Add images, picker confirm | import |
| `modules/gate/GatePlotPanel.vue`, `canvas/PopulationManager.vue` | axes, draw-gate, pop tree | gate |
| `modules/NotebooksModule.vue`, `NotebookTable.vue` | launch server, new notebook, open | notebook |
| `canvas/TabbedCanvas.vue`, `SummaryPanel.vue`, `PlotOptions.vue` | add panel, plot type, options | plots |
| `components/ViewerPanel.vue` | overlay toggles | segment · track · gate |

---

## The guides

Grouped as the sidebar is. `†` = built by the D8 builder.

1. **Import images** — the cold-start one; the only guide whose prereq is just "a project is open".
   Project panel → new/select set → Add images → file picker → watch conversion → the run tag.
2. **Drift correct an image** † — `/cleanup`, and the guide that best shows the *version* model (a
   correction makes a new version, the active one is what everything downstream reads).
3. **Segment an image** † — `/segment`, Cellpose, then the QC panel, then the mask in napari.
4. **Create and gate populations** — `/gate`: axes + transform, draw a gate, name it, the population
   tree, gate on the next pair. Its own shape (canvas, not TaskRunner).
5. **Track cells** † — `/track`, btrack; parks on a long run; ends at tracks-in-napari.
6. **Create a notebook** — `/notebooks`: launch the server, new notebook, what's pre-loaded, open it.
7. **Build plots** — `/analysis`: add a panel, pick populations, plot type, options, export. Says
   out loud that the page is read-only, or a user goes looking for the gate tools here.

Plus four **obvious candidates** added in the same pass, each answering a question users arrive with:

8. **Fix pixel size and channel names** — `/metadata`. The most common first failure: wrong pixel size
   silently corrupts every µm measurement downstream, and the import guide already points here.
9. **Classify behaviour states** † — `/behaviour`, HMM. The natural end of the tracking arc, and
   `lib/tips.ts` already teases it.
10. **Cluster cells into phenotypes** † — `/clust-cells`. "How do I find cell types without gating by
    hand." The heatmap step is what turns "cluster 4" into "CD4 T cell".
11. **Run a pipeline over a whole set** — `/chain`. "I have forty images, am I really doing this one at
    a time?" The difference between a demo and a working pipeline.

Still-obvious candidates NOT built, deliberately: **record a movie** (three surfaces — animation, batch
movies, movies — so it needs its own scoping pass, and it is the most-requested *output*, which makes it
the best next one), **spatial neighbour analysis**, and **export/share a project**.

Copy budget per step: **one sentence, plus at most 2–4 imperative lines** — the same shape as a tip
card (see R4). The control is right there; the bubble is not the place for prose.

---

## Phases

Each phase is shippable and leaves the app working.

**P1 — the runtime, plus *Import images* end to end.** `anchorPosition.ts` (+ `TeleportPopover`
refactor), `GuideBubble.vue`, `stores/guide.ts`, `GuidesDialog.vue`, the header button, the D4 test,
the import guide, anchors in `SetBar`/`ImageTable`/`ManageImagesModule`/`FileBrowser`. Docs:
`docs/UI.md` section + `INVENTORY.md` line. **Stop here for Dominik to look at it** (R3).

**P2 — `moduleTask.ts` + drift correct, segment, track.** Anchors in `ModuleLayout`, `TaskRunner`,
`TaskList`, `ViewerPanel`. This is where `awaitTask` (D3) and `reveal` (D5) get real use.

**P3 — gate populations · notebook · build plots.** The three bespoke shapes.

**P4 — link the tips (D7).** `guideId` on `WhatNewCard` + the "Show me" button.

---

## Reservations — all of them, up front

**R0 — nothing here has been seen in a browser.** The whole system typechecks and the full suite passes
(1362 tests), but bubble placement, the ring, and the feel of auto-advance are exactly what a test cannot
judge. R3 below is the plan's own warning about this and it still stands — with the difference that P1's
"ship one guide first" no longer applies, because all 11 were written in one pass on the assumption that
the shape is right. If the feel is wrong, the fix is in `GuideBubble.vue` + `anchorPosition.ts` and the
step definitions are unaffected; if the *step granularity* is wrong, that is 11 files of prose.

**R1 — napari is a separate native window; no bubble can reach into it.** This is the one place a
screencast genuinely beats this system. Segmentation, tracking and gating all end with *look at it in
napari*, and the guide can only get as far as Cecelia's `ViewerPanel` toggles and then *describe*
what will appear. Mitigations, in order: point at the `ViewerPanel` control that causes it; lean on
the feijoa sketch for that topic (`docs/UI.md` → *Explainer sketches*, which already illustrate
segmentation/tracking); accept prose for the rest. If this turns out to be the weak half, the answer
is probably a short silent clip embedded in a step — **not in this plan**, and worth deciding only
after you've used P2.

**R2 — no demo data, so guide quality depends on what the user already has.** Per your call, guides
run on real data. The prereq system (D6) makes that *honest* rather than confusing, but a user who
installs Cecelia and opens Guides on an empty project can only usefully run guide #1. That is the
correct trade (bundling a demo image set is its own project — data choice, licensing, install size,
and a fake project they then have to delete) but it does mean **Guides is not a substitute for the
first-launch wizard**, and shouldn't grow into one.

**R3 — bubble placement and the ring are visual, and CI cannot check them.** Positioning maths gets a
unit test; "does it look right next to a collapsed panel edge, over a scrolling table, beside a
`FloatingPanel`" does not. So P1 deliberately ships **one** guide: you look at it, and I write the
other six only once the feel is right. I'd rather build one twice than seven once.

**R4 — guide copy is a new class of user-facing text and will fight the copy ratchet.**
`utils/uiCopy.ts` enforces ~90 chars and single sentences; a guide step is legitimately longer. This
needs an explicit carve-out in `docs/UI.md` with a *shape* (the empty-state precedent: a defined form,
not a blank cheque) — one sentence + 2–4 imperative lines — plus a decision on whether
`guides/*.ts` is in or out of the extractor's scope. Left unstated, either the ratchet fails or guide
prose quietly bloats.

**R5 — ~64 step definitions couple to the UI, and the UI moves.** This is the real ongoing cost, and
no amount of design removes it. It is held down by: the D4 anchor test (a rename fails CI), D8 (three
guides are ~10 lines each), and the missing-anchor fallback (worst case degrades to a card, not a
crash). Still — a page redesign means revisiting its guide, and that should be an expected line item
rather than a surprise.

**R6 — `TeleportPopover` has 7 existing call sites** and P1 refactors its positioner. The maths is
being *moved*, not changed (plus two new sides), but popover placement is exactly the kind of thing
that regresses invisibly. Its existing behaviour is worth a look at those 7 sites in P1, and the
pure positioner test is written against the current behaviour first.

**R7 — `clickAnchor` steps are the fragile gate kind.** A DOM listener on a resolved element can
miss if the element is re-created between resolve and click (a `v-for` re-render, a `KeepAlive`
restore). Prefer `when()` over stores wherever a state change is observable; `clickAnchor` is for
controls with genuinely no end state, and it always has `Next` behind it.
