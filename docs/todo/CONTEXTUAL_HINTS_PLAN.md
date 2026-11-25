# Contextual hints: how a user finds out a hidden gesture exists

**Status:** **in progress.** Planned 2026-11-24 (`origin/main` e703aa12). P1 done 2026-11-25: both
bugs fixed and the visible-surface rule is in `docs/UI.md` → *Hidden gestures need a visible surface*.
P2 done 2026-11-25 (#2530): `.cc-gesture-line`, every §3 surface and §4 fix, `HintCallout` deleted.
**P3 re-scored 2026-11-26: 0 of 9 hint targets left without a visible surface, so the hint system
(§5–§7) is not built.** Reopen it only if use shows people still miss a surfaced gesture.
**Decided 2026-11-25: clarity first.** Each hidden gesture gets a visible surface (§3, §4) before any
hint system. The hint system (§5–§7) is kept as a costed fallback, built only if the re-score in §8
P3 leaves more than five targets that a visible surface cannot fix.
**Brief:** [`docs/archive/CONTEXTUAL_HINTS_AUDIT_PROMPT.md`](../archive/CONTEXTUAL_HINTS_AUDIT_PROMPT.md).
Read its banner: one premise had moved. **Sibling:** [`GUI_CLARITY_PLAN.md`](GUI_CLARITY_PLAN.md)
(density, cutting, buried checks). This plan does not repeat it.

**Why clarity first.** Most of the audit's findings are GUI defects, not missing hints: two bugs, a key
legend that is defined and never rendered, missing shortcut rows, empty states that do not name the
gesture. A hint on an unclear control papers over it, and hints go stale (the last set was retired).
The hint system costs a registry, a store, a signal in every gesture handler, caps, an opt-out and a
staleness test. That is worth it for a dozen hints, not for three.

**The premise that moved.** The brief asks for a hint system as if none had existed. One was built
and its hints were retired on 2026-08-13 (ONBOARDING_PLAN P4). The renderer, `HintCallout.vue`, and
`ModuleLayout`'s `hint` / `hint-key` props remain with zero callers. `docs/UI.md` →
*First-use hints* now sets the bar any hint must clear:

> An **interaction affordance with no other surface**, something invisible in the markup that no live
> check could answer. Never pipeline ordering, and never a prerequisite.

The old hints failed because each one was a static sentence fired on first visit, whatever the state.
This plan replaces that bar with a rule about the GUI itself (P1 writes it into `docs/UI.md`, where
the pre-commit convention check enforces it):

> **Every gesture or bulk action has a visible surface:** a label, a menu entry, an inline gesture
> line, or a row in a shortcuts table. A tooltip alone is not one. Ordering and prerequisites are
> state questions, answered by live checks, never by a hint.

Ordering and check prompts (for example "check acquisition after import") stay with the clarity
plan's checks cluster and pre-run checkpoint.

## 1. Existing coverage

| Surface | Covers | Where |
|---|---|---|
| Guides (38) | Pipeline walkthroughs on real data: pick set, select, choose function, run, read result | `lib/guides/index.ts`; `docs/UI.md` → *Guides* |
| Recipes | Which pipeline for which data | `lib/guides/recipes.ts` |
| Tip of the day | One modal card a day at launch, opt-out `cc.tipsOnLaunch` | `lib/tips.ts`, `WhatsNewDialog.vue` |
| Tooltips | About 1,450 `v-tooltip` sites; most hidden gestures are named only here | everywhere |
| Icon / colour legends | What a glyph or a colour means | `lib/iconLegend.ts` (ratcheted by test) |
| Viewer shortcuts | Mouse and keyboard table, opened from Help ▾ or the panel foot | `ViewerWindow.vue:3887-3899` (`SHORTCUTS`), popover `:7217` |
| Inline gesture text | Gate "hold Shift to adjust gates", empty canvas "drag plots by their title · resize from the corner", chain "Drag to pan · Scroll to zoom · Double-click edge to remove" | `GatePlotPanel.vue:600`, `GatingPlots.vue:636`, `ChainModule.vue:1960` |
| Kiwi pointing | Claude rings a `data-guide` anchor (`point_at_ui`) | `PointerBubble.vue`, `mcp/cecelia_mcp/server.py:2263` |

**What the guides do not reach.** They teach the forward path once. They do not teach the gestures a
user needs on the tenth visit: modifier-click, double-click and right-click, keys, and bulk shortcuts
for a repeated action. The gating guide covers drawing (`gatePopulations.ts`, `gate.drawTool`), not
editing a gate in place, the ⋯ menu, Copy or stepping.

## 2. Inventory and scores

About 215 elements walked across four areas: shell and image table, gating, board and plots, viewer
popout and the other pages. Below are the 40 that are neither plainly labelled nor already explained.
The rest are labelled buttons with a clear tooltip.

V = visibility, S = self-explanation, St = stakes, F = frequency (0–3 each). P = (V+S)×(St+F).
**LW** = a manual long way: a repeated action that a bulk control or gesture does in one step.
Verdict: **hint**, **GUI** (fix in the GUI, §4), **fine**, or **gap** (the shortcut does not exist).
Since the clarity-first decision, a **hint** row is answered by its visible surface in §3 first.

| # | Page | Element | Where | Revealed by | V S St F | P | Verdict |
|---|---|---|---|---|---|---|---|
| 1 | every image-table page | Ctrl-click adds a row, Shift-click picks a range, press-and-drag sweeps. A plain click **replaces** the selection | `SelectionTable.vue:290-379` | modifier, drag | 3 3 2 3 | 30 | **hint** · LW |
| 2 | board / plots | Claim check: state a claim from this plot, rule-checked against its data | `ClaimStrip.vue:121-127`, mounted `SummaryPanel.vue:1553` | icon only, in an auto-hidden footer | 3 3 3 2 | 30 | **hint** (beacon) |
| 3 | Gate | Edit a gate in place: click to select, drag the body to move, double-click an edge to add a vertex, right-click a vertex to delete it | `GateOverlay.vue:205-222, 650-676` | click, double-click, right-click | 3 3 1 3 | 24 | **hint** · LW (delete and redraw) |
| 4 | correction cockpit | Picks come from the viewer's Select mode, and Shift-click adds one. The empty state only says "Pick cells in the viewer" | `CorrectionCockpit.vue:595-601, 658-659` | another window, modifier | 3 3 2 2 | 24 | **GUI** |
| 5 | viewer | Select mode: Shift-click adds a cell, Alt-click toggles one | `ViewerWindow.vue:3530-3537` | mode menu, modifier | 3 2 2 2 | 20 | **hint** (P2) |
| 6 | every TaskRunner page | Copy settings from a previous run on another image | `TaskRunner.vue:1114-1116` | micro icon | 2 2 2 3 | 20 | **hint** · LW |
| 7 | every image-table page | ⚠ in the Name header selects all flagged rows; Resync re-reads all of them | `ImageTable.vue:646, 651` | icon only, conditional | 2 3 2 2 | 20 | **hint** · LW |
| 8 | Metadata | Assign value / Assign channels writes to **every** image when none are selected | `MetadataPanel.vue:399-402, 454-458` | tooltip only | 2 2 3 2 | 20 | **GUI** |
| 9 | every image-table page | Include/Exclude is only in each row's ⋯ menu; there is no bulk version for a selection | `ImageTable.vue:704, 855-879` | hover menu | 3 2 2 2 | 20 | **gap** · LW |
| 10 | sidebar | Pages outside the set's recipe are dimmed, with no word on why | `AppSidebar.vue:130`, CSS `:192` | visual only | 2 3 1 3 | 20 | **GUI** |
| 11 | Chains | Adding a node is drag-only (palette to canvas); connecting is a drag between handles | `ChainModule.vue:1882-1888, 1960-1962` | drag | 3 2 1 3 | 20 | **GUI** (empty canvas) |
| 12 | board / plots | Drag lassoes dots, Shift-drag draws a rectangle; the picked cells light up in the viewer | `PlotChart.vue:168-191` | drag, modifier | 3 3 1 2 | 18 | **hint** · LW (dot by dot) |
| 13 | track timeline | Keys `l b r a`, Backspace, Enter, Esc; Shift-click a lane to join | `TrackSchemeView.vue:887, 975`; `trackSchemeKeymap.ts:81` | keys | 3 3 1 2 | 18 | **GUI** (`KEY_HINT` is never rendered) |
| 14 | Gate | Copy this gating to the other images in the set (optionally with plot layout) | `GatingPlots.vue:631`; `GatingCopyDialog.vue` | labelled "Copy" | 1 2 2 3 | 15 | **hint** · LW (redraw per image) |
| 15 | Gate | Pop ⋯ menu: Move under…, Convert rect⇄polygon, Show the gate's plot, Combine with… | `PopulationManager.vue:790-839` | hover-only menu | 3 2 1 2 | 15 | **hint** (P2) · LW |
| 16 | board | compare: per image / pooled / by attribute: one plot for every selected image | `LayoutCanvas.vue:761-767` | labelled select | 1 2 2 3 | 15 | **hint** (P2) · LW |
| 17 | viewer | ROI review keys ↑↓ A R F U 1-9, live only while ROIs is open | `ViewerWindow.vue:5749-5755`; `RoiSection.vue:173-194` | keys | 2 2 1 2 | 12 | **GUI** (legend) |
| 18 | Gate | Double-click a bisector cut: switches between fixed and valley | `GateOverlay.vue:647-649` | double-click | 3 3 1 1 | 12 | fine (rare; the valley chip shows on create) |
| 19 | Chains (live) | Clicking a node sets "resume from" | `ChainModule.vue:524-533` | click, no tip | 3 3 1 1 | 12 | **GUI** (tooltip) |
| 20 | plots | Controls auto-hide until hover; the pin cycles auto / shown / hidden | `CanvasPanel.vue:59-63, 224-226` | hover | 2 1 1 3 | 12 | fine (hover is found naturally) |
| 21 | viewer | AF badge: click to A/B previewed vs source pixels | `VwChannelsSection.vue:183-188` | badge + tooltip | 2 2 2 1 | 12 | fine |
| 22 | any task page | Sweep: one run per value of a parameter | `TaskRunner.vue:1136-1141` | labelled, conditional | 1 2 1 2 | 9 | **hint** (P2) · LW (re-run by hand) |
| 23 | Gate | « » step images, keeping segmentation and layout | `GatingPlots.vue:625-630` | icon only | 2 1 0 3 | 9 | fine (tooltip) |
| 24 | Gate | Esc cancels a draw, or deselects a gate | `GateOverlay.vue:745-758` | key | 3 1 0 2 | 8 | fine (Esc is convention) |
| 25 | Gate | Hold Shift while a tool is armed to adjust a gate | `GatePlotPanel.vue:600` | modifier | 1 1 1 3 | 8 | fine (inline text + guide) |
| 26 | Batch movies | "fill from view" copies channel colours and overlays from the viewer | `BatchMoviesPanel.vue:579-583` | small text button | 2 2 0 2 | 8 | **hint** (P2) · LW (swatch by swatch) |
| 27 | Metadata | Click a cell to edit; Enter saves, Esc cancels | `ImageTable.vue:785-788` | click on text | 2 2 0 2 | 8 | fine (outline on hover) |
| 28 | Metadata | From table: set attributes for every image from a CSV | `MetadataPanel.vue:355` | labelled | 1 1 1 2 | 6 | **hint** (P2) · LW (value by value) |
| 29 | viewer | Alt+wheel sizes the lens; a click pins it | `ViewerWindow.vue:3849-3853, 3529` | wheel, click | 3 3 0 1 | 6 | **GUI** (legend) |
| 30 | any canvas | Shift+wheel zooms, Shift ± steps, Shift 0 resets | `useCanvasZoom.ts:68-85` | keys | 3 3 0 1 | 6 | fine (the slider does the same) |
| 31 | Gate / clusters | Undo/redo Ctrl+Z, Ctrl+Shift+Z | `PopulationManager.vue:205-213` | key + icon | 1 1 1 2 | 6 | fine |
| 32 | floating panels | Double-click a header to maximise; resize from any edge | `FloatingPanel.vue:171, 196-203` | double-click, drag | 3 3 0 1 | 6 | fine |
| 33 | Welcome | Double-click a project row to open it | `ProjectPanel.vue:396-402` | double-click | 2 1 0 2 | 6 | fine (row tip) |
| 34 | board | Duplicate board (plots + layout) | `TabbedCanvas.vue:279-280` | micro icon | 2 2 0 1 | 4 | fine |
| 35 | header | Ctrl K jump box | `JumpSearch.vue:78` | key | 1 1 0 2 | 4 | fine (`<kbd>` shown) |
| 36 | Kiwi | Ctrl+Enter asks | `KiwiAsk.vue:119` | key | 1 1 0 2 | 4 | fine (tip) |
| 37 | viewer | Channel name tooltip says "Show this channel in the composite"; the span has no click handler | `VwChannelsSection.vue:174-175` | — | — | — | **GUI** (wrong copy) |
| 38 | board | Duplicate plot does nothing, silently, when no slot is free | `LayoutCanvas.vue:296-297` | — | — | — | **GUI** (silent no-op) |
| 39 | Gate | A viewer selection shows as a transient pop with no "save as population"; the long way is redrawing it as a gate | `PopulationManager.vue:864-869` | — | — | — | **gap** · LW |
| 40 | Blackboard / Animation | No "mark all" on section verdicts; no row-wide toggle across keyframes | `SectionVerdict.vue:59-60`; `AnimationTimeline.vue:160-176` | — | — | — | **gap** · LW |

## 3. A visible surface for each hint target

The hint rows from §2. Each gets the cheapest visible surface; the §5 registry entry stays as the
fallback if the surface does not work.

**One primitive for the gesture line.** Rows #1, #3, #12 and §4 #4 all add an inline gesture line.
The only one today is a local span, `.gate-hint` with `<kbd>` (`GatePlotPanel.vue:600`). P2 starts by
extracting it into a shared `GestureLine` (muted, 2xs, `<kbd>` for keys) with a row in
`docs/ui/PRIMITIVES.md`, so the pages do not hand-roll four variants.

| # | Target | Visible surface | Where |
|---|---|---|---|
| 1 | Image-table multi-pick | A selection line in the table foot: "1 selected · Ctrl-click adds, Shift-click a range, drag sweeps" | `SelectionTable.vue`, `ImageTable.vue` |
| 7 | Select flagged | With flagged rows, the ⚠ header icon becomes a labelled button "Select N flagged" | `ImageTable.vue:646-651` |
| 6 | Copy task settings | The micro icon becomes a labelled button "Copy from…" in the Parameters heading | `TaskRunner.vue:1114-1116` |
| 14 | Copy gating to the set | Label "Copy" → "Copy to set…" | `GatingPlots.vue:631` |
| 3 | Edit a gate in place | When a gate is selected, the existing inline gesture line reads "drag to move · double-click an edge adds a point · right-click a point deletes it" | `GatePlotPanel.vue:600` |
| 12 | Plot lasso | An inline gesture line in the plot's hover controls: "drag lassoes · Shift-drag a rectangle" | `PlotChart.vue`, `CanvasPanel.vue` |
| 2 | Claim check | A word label "Claim" on the icon. Surfacing it out of the auto-hidden footer is clarity P3 (*surface the checks*) | `ClaimStrip.vue:121-127` |
| 5 | Viewer Shift/Alt-click | Rows in the viewer `SHORTCUTS` table, and the Select entry of the mode menu says "Shift adds · Alt toggles" | `ViewerWindow.vue:3530-3537, 3887-3899` |
| 15 | Pop ⋯ menu | Show ⋯ on the selected row, not only on hover | `PopulationManager.vue:790-839` |
| 16, 22, 26, 28 | Compare, Sweep, fill from view, From table | Already labelled; fine | — |

## 4. Fix in the GUI

| # | Fix | Where |
|---|---|---|
| 37 | **Wrong copy:** the channel-name tooltip promises a click that does nothing. Make the name toggle visibility or drop the tip | `VwChannelsSection.vue:174-175` |
| 38 | **Silent no-op:** Duplicate with a full board. Disable it with "No free slot — add a row in ⚙" | `LayoutCanvas.vue:296-297` |
| 13 | Render `KEY_HINT` in the Join/Split/Remove/Add tooltips ("Join (l)"), and have a blocked Join say "Shift-click a second lane" | `trackSchemeKeymap.ts:81`; `lib/trackCorrection.ts:289` |
| 17, 29 | Add the missing rows to the viewer `SHORTCUTS` table: Alt+wheel (lens size), click (pin lens), Esc (clear ruler), Shift+drag in Measure, Shift+wheel in 3D with a z-window, the ROI keys | `ViewerWindow.vue:3887-3899` |
| — | Check the Shortcuts popover when the side panel is folded: it anchors to `keysBtn` inside `VwLookBox` (inferred, not run) | `ViewerWindow.vue:7217` |
| 4 | Cockpit empty state names the gesture: "Select mode in the viewer; Shift-click adds a cell." | `CorrectionCockpit.vue:658-659` |
| 8 | With nothing selected, label the buttons "Assign to all N" | `MetadataPanel.vue:399-402, 454-458` |
| 10 | The dimmed page's tip says "Not in this set's recipe" | `AppSidebar.vue:130` |
| 11 | Empty chain canvas: "Drag a step here from the left · drag between dots to connect" | `ChainModule.vue:1960-1962` |
| 19 | Live node tooltip: "Click to resume from here" | `ChainModule.vue:524-533` |
| 9, 39, 40 | Missing bulk actions (exclude a selection, save a viewer selection as a population, mark all sections, a row toggle across keyframes). Feature decisions, not hints | as listed in §2 |

## 5. Deferred: hint registry

**Built only if §8 P3 says so.** In §5–§7, "P1" and "P2" mean the hint system's own first slice and
expansion, not §8's phases. `claim-check` is dropped (decision 2). Registry entries, ready to paste
into `lib/hints.ts` (§6). The first seven were the first slice; P2 rows follow. Trigger kinds are
explained in §6. Each signal named here is one call at the code that already handles the gesture.

```ts
// ── P1: first slice (image-table pages, Gate, board) ──
{ id: 'table-multi-pick', page: '*table', anchor: 'images.table',
  trigger: { signal: 'table.plainPick', count: 3, withinS: 20 },   // 3 plain clicks, each replacing a 1-row pick
  retire: ['table.modPick'],                                          // any Ctrl/Shift-click or drag sweep
  text: 'Ctrl-click adds a row, Shift-click a range, or drag across rows.' },
  // P30 · prevents running on one image while believing several are picked

{ id: 'table-select-flagged', page: '*table', anchor: 'images.selectFlagged',       // NEW data-guide
  trigger: { signal: 'image.sizeDialogOpen', count: 2, withinS: 300, when: 'flagged>=3' },
  retire: ['table.selectFlagged', 'table.resyncFlagged'],
  text: 'This selects every flagged image at once.' },
  // P20 · LW: fixing calibration image by image

{ id: 'task-copy-settings', page: '*task', anchor: 'task.copySettings',               // NEW data-guide
  trigger: { signal: 'task.paramEdited', count: 3, withinS: 120, when: 'priorRunInSet' },
  retire: ['task.copySettingsOpen'],
  text: 'Copy settings from an earlier run instead of retyping them.' },
  // P20 · LW: retyping params per image

{ id: 'gate-copy-to-set', page: '/gate', anchor: 'gate.copy',                         // NEW data-guide
  trigger: { signal: 'gate.added', distinctImages: 2, withinS: 1800 },               // gates drawn on a 2nd image of the set
  retire: ['gate.copyOpen'],
  text: 'Copy this gating to the other images in the set.' },
  // P15 · LW: redrawing the same gates per image

{ id: 'gate-edit-in-place', page: '/gate', anchor: 'gate.plot',
  trigger: { signal: 'gate.redrawn', count: 1 },                     // a pop deleted, then the same name drawn within 2 min
  retire: ['gate.vertexEdit', 'gate.bodyMove'],
  text: 'Drag a gate to move it; double-click an edge adds a point.' },
  // P24 · LW: delete and redraw to adjust

{ id: 'plot-lasso', page: '/analysis', anchor: 'plot:*',             // the clicked plot's own anchor
  trigger: { signal: 'plot.dotPick', count: 3, withinS: 30, samePlot: true },
  retire: ['plot.brush'],
  text: 'Drag to lasso several dots; Shift-drag draws a rectangle.' },
  // P18 · LW: dot-by-dot picking

{ id: 'claim-check', page: '/analysis', anchor: 'plot.claim',                         // NEW data-guide
  trigger: { beacon: true, when: 'statsPlotRendered' },              // passive dot; text on hover/focus
  retire: ['claim.open'],
  text: 'State what this plot shows; Cecelia checks it against the data.' },
  // P30 · prevents exporting p-values with no check on n or replicates

// ── P2: expansion ──
{ id: 'viewer-select-add', page: 'viewer', anchor: 'viewer.interactMode',            // NEW data-guide
  trigger: { signal: 'viewer.plainPick', count: 3, withinS: 15 }, retire: ['viewer.modPick'],
  text: 'Shift-click adds a cell; Alt-click toggles one.' },
{ id: 'pop-move-under', page: '/gate', anchor: 'popmanager.row',
  trigger: { signal: 'gate.redrawnUnderOtherParent', count: 1 }, retire: ['pop.moveUnder', 'pop.convert'],
  text: 'The ⋯ menu moves or reshapes a population without redrawing.' },
{ id: 'board-compare', page: '/analysis', anchor: 'board.compare',                    // NEW data-guide
  trigger: { signal: 'board.samePlotAdded', count: 3 }, retire: ['board.compareChanged'],
  text: 'compare → per image draws every selected image in one plot.' },
{ id: 'task-sweep', page: '*task', anchor: 'task.sweep',                              // NEW data-guide
  trigger: { signal: 'task.rerunOneParamChanged', count: 2 }, retire: ['task.sweepOn'],
  text: 'Sweep runs once per value of one parameter.' },
{ id: 'metadata-from-table', page: '/metadata', anchor: 'metadata.fromTable',
  trigger: { signal: 'metadata.assignSingle', count: 4, withinS: 300 }, retire: ['metadata.fromTableOpen'],
  text: 'From table sets attributes for every image from a CSV.' },
{ id: 'movies-fill-from-view', page: '/batch-movies', anchor: 'batchMovies.fillFromView', // NEW data-guide
  trigger: { signal: 'batchMovies.swatchSet', count: 3, withinS: 120 }, retire: ['batchMovies.fillFromView'],
  text: 'fill from view copies the viewer\'s colours and overlays.' },
```

**Rejected as hints:** #18, #20, #21, #23–25, #27, #30–36 are fine as they are, and §4 fixes the GUI
rows. Some failures are not hint targets at all: the acquisition check after import is ordering, and
is clarity slice 7's job; the stats finding on a plot is clarity P2's `PlotNotice`.

## 6. Deferred: system design

**Smallest system: one registry, one store, one variant of an existing bubble.** No new positioning,
anchor scheme or storage.

| Piece | Reuses | New |
|---|---|---|
| Registry `lib/hints.ts` | the `data-guide` anchor scheme (`utils/guideAnchor.ts`) | `HintDef {id, page, anchor, trigger, retire, text}`; adding a hint = one entry |
| Signals | — | `hintSignal(name, ctx?)`: one line in the handler that already runs the gesture (for example `GateOverlay` vertex insert → `hintSignal('gate.vertexEdit')`). This is also the feature link (§7) |
| Manager `stores/hints.ts` | `profileStorage` (per profile, mirrored to `settings.toml`) | keeps a short ring buffer of signals, evaluates triggers, enforces caps, retires on use |
| Bubble | `PointerBubble.vue`: placement, reachability, z-layers, the anchor poll | a `hint` variant with **Got it** and **Don't show tips**; no pulse under `prefers-reduced-motion` |
| Retired | — | **`HintCallout.vue` and `ModuleLayout`'s `hint` / `hint-key` props are deleted in §8 P2** (dead code either way), so there is at most one hint renderer. An inline banner above a panel is the shape the retired set had; a hint here sits at the control it names |

**Triggers**, in order of preference:
1. **Signal**: N of a signal within T seconds, optionally with a `when` predicate the store already
   knows (flagged count, a prior run in the set). Every P1 hint except one uses this.
2. **Beacon**: a 6 px dot on the control; the text appears on hover or focus. It never moves in or
   points. Used only for `claim-check`, where there is no wrong gesture to react to.

Idle triggers and a tips drawer are not built. The idle trigger was rejected because it fires on
someone who is thinking, which is the annoyance the brief wants to avoid. The drawer was rejected
because Help ▾ → Guides is already the place to pull help from.

**Caps** (enforced in the store, never per hint):
- at most 1 hint per page visit, and 3 per session (`sessionStorage`, per window);
- never while a guide runs (`guide.active`), a modal is open, or Kiwi is pointing (`uiMarks` non-empty);
- never in the first 60 s after launch, when tip of the day may be up;
- a shown hint is never shown again (`cc.hint.<id>` = `shown | dismissed | retired`);
- a feature used before its hint fires retires the hint, and it is never shown. Use is recorded even
  with tips off.

**State.** Per-hint flags are `profileStorage` keys `cc.hint.<id>` (reusing HintCallout's
namespace). The opt-out is a settings-store preference, `hintsOn` in `PROFILE_KEYS`
(`stores/settings.ts:53`), the same shape as `tipsOnLaunch`, and Preferences binds it beside "Tip of
the day" (`PreferencesModal.vue:363`). "Show tips again" clears `cc.hint.*`.

**Placement.** Beside the anchor via `anchorPosition` (shared with `TeleportPopover`), above or to the
side, and never in the bottom-right corner, which holds the Next box footer and the toasts. If a placed
rect would intersect `[data-guide="layout.nextStep"]` or `[data-guide="images.checks"]`, take the
opposite side. In the **viewer popout** (a bare route; `App.vue:201` mounts only `GuideBubble` there),
mount the hint layer in the bare branch for `/viewer-window` only, and show the text in the
`VwHeader` spacer (`.vwh-grow`, `VwHeader.vue:149`) with a beacon dot on the anchor, so nothing covers
the image.

**Accessibility.** `role="status"`, `aria-live="polite"`, no focus move. The bubble's two buttons are
keyboard-reachable; Esc dismisses only when focus is inside the bubble, so it does not compete with
the gate, maximise and guide Esc handlers. Beacons are focusable, with the text as an `aria-label`.

**Visual.** The `PointerBubble` scale, not `GuideBubble`: one line, no step counter, no ring, no
backdrop. Chrome is `--cc-guide`, so a hint and a guide read as one family.

**Claude/Kiwi.** Same anchors, so `point_at_ui` already reaches every hint's control. A hint's text is
the shortest true sentence about a gesture, which is also what Claude needs when a user asks "how do
I…". P3 exports the registry to `get_guide` (generated JSON, the way `user-docs` is generated), so Kiwi
quotes it rather than writing new help text. That is not part of P1.

## 7. Deferred: keeping hints current

| Mechanism | Cost | Decision |
|---|---|---|
| **Anchor test**: every registry anchor exists in source | none; extend `lib/guides/guides.test.ts`'s anchor ratchet to also iterate `HINTS` | **yes** |
| **Stable anchors**: `data-guide` only | 6 new ids for P1/P2; already the convention | **yes** |
| **Signal test**: every signal a hint triggers or retires on is emitted somewhere (`hintSignal('…')` in source), and every emitted signal is used by a hint, in both directions like `iconLegend.test.ts` | ~30 lines of test, no CI time | **yes**: this is the feature link. Delete or rename the gesture and its `hintSignal` goes with it, so CI fails |
| **Coverage check**: every control has an entry or `hint: none` | would force a decision on ~1,450 tooltip sites; mostly noise | **no** |
| **PR-time reviewer** | a `claude -p` call per commit | **no**: the signal test catches the removals; a reviewer would only add judgement on new controls, which the agent rule asks for anyway |
| **Text drift**: hint text vs behaviour | an advisory maintainability-lint note when a diff changes a function containing `hintSignal(` without touching `lib/hints.ts` | **later (P3)**, and only after a replay over merged PRs, the bar every lint sub-check meets (`maintainability_lint.py` header) |

**Agent rule** (add to `frontend/CLAUDE.md`; the full contract goes in `docs/UI.md` → *First-use hints*,
which this replaces once built):

> **Hints (`lib/hints.ts`).** If you change a gesture, key or bulk action that has a `hintSignal(...)`
> call, update that hint's `text` in the same change. Add a hint only for a hidden gesture or bulk
> action (modifier, double-click, right-click, key, drag) that the label cannot carry; fire it on a
> signal, never on page visit. Ordering and prerequisites are never hints.

## 8. Phasing

- **P1 (small PR):** the two bugs, #37 (channel tooltip promises a click that does nothing) and #38
  (Duplicate silently does nothing on a full board), and the visible-surface rule into `docs/UI.md` →
  *First-use hints*, replacing the "no other surface" bar.
- **P2 (small PRs, a page or two each):** first the shared `GestureLine` (§3), then the §3 visible
  surfaces and the §4 fixes. Delete
  `HintCallout.vue` and `ModuleLayout`'s `hint` / `hint-key` props (zero callers). Look before merge:
  each new line or label on the real page.
- **P3, re-score:** re-walk the §2 hint rows against the shipped GUI. Build the §5–§7 hint system only
  if **more than five** targets are still invisible after their visible surface; fewer than that get
  a better surface instead. If built, it ships with the §7 tests and the agent rule, and P3 of the old
  phasing (the `get_guide` export, the text-drift lint after replay) follows only if it earns it.
- **Feature decisions, not this plan:** the §2 gaps (#9, #39, #40).

**P3 result (2026-11-26, read against `main` after #2530).** Every §3 row now has its surface in the
shipped code: #1 set-row gesture line (`ModuleLayout.vue`), #7 "N flagged" (`ImageTable.vue`), #6
"Copy from…" (`TaskRunner.vue`), #14 "Copy to set…" (`GatingPlots.vue`), #3 the selected gate's
gestures (`GateOverlay.vue` + `utils/gateGestures.ts`), #12 the hover lasso line (`PlotChart.vue`),
#2 "Claim" (`ClaimStrip.vue`), #5 the viewer Shortcuts rows, #15 ⋯ on the selected pop row. Count left
invisible: 0, below the threshold of five, so §5–§7 stay a reference design. This is a read of the
code, not of use: if someone still misses a surfaced gesture, fix the surface first, and reconsider
the hint system only if several do. Still open and outside this plan: the §2 gaps (#9, #39, #40).

## 9. Risks and decisions

- **Clutter.** Visible surfaces add words. Each is one short line, shown where the gesture happens
  (a selection line only with a selection, the gate line only with a gate selected), so the page at
  rest does not grow. `docs/UI.md`'s brief-copy rule applies.
- **The rule over-reaching.** "A tooltip alone is not a surface" could demand labels on every icon.
  The rule covers gestures and bulk actions (modifier, double-click, right-click, key, drag, act on
  many), not ordinary buttons, whose tooltip names them.
- **If the hint system is built:** annoyance and measurement are covered by the §6 caps; there is no
  UI telemetry, and a hint's `cc.hint.<id>` record (`shown → retired`) is the only evidence.

**Decided 2026-11-25:**
1. **The `docs/UI.md` bar** is replaced by the visible-surface rule above (P1), not loosened to allow
   signal-fired hints.
2. **Claim check** gets a word label (§3 #2), and surfacing it belongs to clarity P3. No beacon.
