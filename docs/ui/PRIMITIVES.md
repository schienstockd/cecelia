# UX primitive catalog — CHECK BEFORE BUILDING (mandatory)

> Split out of `docs/UI.md` (2026-08-20) because it is the one section read on **every** frontend
> task. It used to cost ~62k tokens to reach it inside a 229 KB file. Everything else about the
> frontend is still in [`docs/UI.md`](../UI.md); this file holds only the mandatory lookup.


Before you render **any** of these controls, use the canonical component/utility below — **do not
hand-roll a variant**. A new copy of a primitive that already has a canonical form is a bug (same rule
as the H5AD/zarr/`run_py` single-helpers; see `CLAUDE.md` → *Before implementing anything*). This is
the one glanceable lookup; each row links to its detailed section. Unification status for the few
primitives still being extracted lives in `docs/todo/UX_PRIMITIVES_PLAN.md`.

| Need | Use | Never |
|------|-----|-------|
| Button | `.cc-btn` + `-primary`/`-ghost`/`-bare`/`-danger`/`-danger-ghost` (`style.css`) | scoped `.btn-sm`/`.btn-primary` in a component |
| Icon-only button | `.cc-btn` + `-bare`\|`-ghost` + `-icon` (+ `-micro`/`-dense`/`-lg`) | a per-file `.icon-btn`/`.opt-btn`/`.gear` class |
| Engaged / pressed toggle button | `.cc-btn-on` (+ `-on-tint` washed, `-on-solid` filled) | a scoped `.on`/`.active` colour rule |
| Joined strip of related buttons | `.cc-btn-group` wrapping ordinary `.cc-btn`s | a hand-rolled `.seg { } .seg button { }` block |
| On/off option (applies on flip) | `components/CcToggle.vue` | a native checkbox styled as a switch |
| Select from a list (multi/single) | native `<input type="checkbox">`, or `ChipSelect` for chips | a column of toggle switches |
| Chips / segmented picker | `components/ChipSelect.vue` | hand-rolled pill/`.seg` rows |
| Colour picker dropdown | `components/SwatchSelect.vue` | a bespoke swatch grid |
| A field whose value is user-invented but usually a REPEAT — an output name, an attribute value, a tag | `components/SuggestInput.vue` (task specs reach it via param type `valueNameInput` + a `namespace`) | a bare text input (no recall, and a typo silently creates a second thing), a `<select>` (you can never enter a new one), or a native `<datalist>` (see below) |
| Filtering rows by their attributes | `components/AttrFilterPanel.vue` + `utils/attrFilter.ts` (`v-model` an `AttrFilterState`, `:rows` anything with an `attr` bag) | a second set of chip rows + Apply/Reset/Invert, or a per-page matching clause |
| **ANY table of rows and columns** — pick one, pick many, or a plain list | `components/SelectionTable.vue` — `selectionMode` `single` (default, a radio) / `multi` (checkboxes, `v-model:selected`) / `none` (a list; `@row-click` is what a row means). Per-column `sortable`, `sortKey` for a formatted cell, `ellipsis` for a long one; `#cell-<key>` to render one cell yourself, `#actions` for row buttons | a `<select>` that hides the trade-off, or a hand-rolled `<table>` — four of those existed only because `multi`/`none` didn't, and none of them could sort or resize |
| Sorting a list by a clicked header | `utils/sortRows.ts` — `sortRows(rows, valueOf, dir)` + `cycleSort`/`sortIconFor` | a per-table comparator, or an inline asc/desc/off cycle |
| A dense list in a panel with no width to spare | `SelectionTable`'s `density="compact"` — one step down the type scale, ~half the cell padding, a narrower radio gutter. Nothing else changes, so a compact list still agrees with every other one about selection and sorting | `:deep()` on `th`/`td` from the caller, rediscovering a padding number per panel |
| Drag-resizable table columns | `composables/useColumnResize.ts` (`SelectionTable` opts in via `columnWidthKey`; per-column `width` for the starting size, `actionsWidth` for the trailing button column, and a reset-widths button appears in the header). Pair it with `fit="content"` + an `overflow-x: auto` wrapper when the columns must keep their declared width and the panel should SCROLL — **mandatory with `sticky` columns**, since a squeezed table renders its columns narrower than the offsets the pinning is computed from. Otherwise leave `fit` at `fill`: the declared widths are then starting hints and the table is exactly its panel. Choose by what should give — `content` makes the panel scroll, `fill` makes the columns squeeze. Picking `content` for a table that ought to fit is what put a horizontal scrollbar in the 280px task sidebar and pushed the manager's Time column off-screen | a per-table mousemove drag + an unpersisted widths `ref`, or `width: max-content` in the caller's stylesheet (a single class loses to `.sel-table.sized`) |
| Movie output options (fps + size + filename) | `components/MovieOutputControls.vue` (`v-model:fps` / `v-model:sizeX` / `v-model:sizeY` / `v-model:suffix`, `canvasX`/`canvasY` for the placeholder) | a per-panel set of sliders/fields |
| Movie title-card options (on/off + duration + note) | `components/TitleCardControls.vue` (`v-model` a `TitleCardCfg`) | a per-panel toggle + duration slider + note input |
| The ⚙ that holds those two blocks | `components/MovieOptionsButton.vue` — the button + tooltip + popover; the blocks go in its slot | a second gear, or the blocks laid out flat in a page header |
| Which image VERSIONS a movie records (incl. side-by-side) | `components/MovieCompareControls.vue` (`v-model:versions`/`:layout`/`:contrast`, `available`) | a per-panel version `<select>`, or a separate "compare" switch beside a list |
| Modal / dialog | `components/BaseModal.vue` | a hand-rolled `position:fixed` backdrop |
| Popover / dropdown menu | `components/TeleportPopover.vue` | an absolutely-positioned panel |
| The ITEM LIST inside a row's ⋯ overflow menu | `.cc-actions-menu` + `.cc-actions-item` (+ `.cc-actions-head` for a section label, `.danger`, `.armed`) in `style.css`, inside a `TeleportPopover flush`; destructive items arm in place with `ConfirmButton`. Hosts: `ImageTable`, `canvas/PopulationManager` | a per-component `.menu-item` block, or a dialog for a one-click action |
| Tabs | `components/canvas/TabbedCanvas.vue` | a hand-rolled tab strip |
| Standalone module page (not the image-table layout) | `components/ModulePage.vue` — a `#controls` slot + content, `layout="flow\|scroll\|fill"` | a per-page `.x-page`/`.x-head` wrapper, a page `<h1>`, or a descriptive subtitle paragraph |
| Collapsible section (chevron + heading) | `components/CollapsibleSection.vue`, or `.cc-section-toggle` for the bare row without the panel-bar chrome | a per-file chevron toggle |
| Confirm / destructive-confirm | `components/ConfirmButton.vue` / `ConfirmDeleteButton.vue` | `window.confirm` or an inline arm flag |
| Range slider (min+max) | `components/RangeSlider.vue` | a hand-rolled dual-thumb range |
| Single-value slider | a plain `<input type="range">` — the global base themes it | a wrapper component (there is deliberately none) |
| Loading state in a plot area | `components/plots/PlotSpinner.vue` (delayed — see *Plot loading state*) | an immediate inline spinner |
| **Determinate progress** (a 0–1 fraction — a task, a patch, an export) | `components/CcProgressBar.vue` — `:value` (0–1, clamped, NaN-safe), `size` `thin` (3px, flush in a row/card) \| `bar` (4px, rounded, standalone). Width maths in `utils/progress.ts`. Caller keeps its own geometry (`flex`, `margin`) | a per-file track+fill pair — four of those existed on two heights, two radii, two transitions and three different fraction→width sums |
| "Working", with no fraction to show | nothing, or the surface's existing cue — a running task row already says it via `lib/taskStatus.ts` | animating `CcProgressBar` to fake an indeterminate bar |
| Transient "just did a thing" feedback | `useToast()` — the one `<Toast />` in `App.vue` | a second notification system |
| Copy-to-clipboard (+ the "Copied!" flash) | `composables/useCopyFlash.ts` — `copy(text[, key])` + `isCopied([key])`; `utils/clipboard.ts` for the bare write | `navigator.clipboard.writeText` + a per-file `ref` and `setTimeout` |
| Side panel of two stacked halves, either expandable to the whole panel | `composables/usePaneExpand.ts` + `components/PaneExpandBar.vue` (`utils/paneExpand.ts`) — see *Two-half side panels* | a per-panel mode `ref` + its own pair of toggle buttons |
| Right-hand panel that folds away and can be dragged wider | `components/CollapsiblePanel.vue` (`storageKey` + `label`; drag-to-resize via `composables/usePanelResize.ts`) — see *Collapsible side panels*. **The content inside fills it (`flex: 1; min-width: 0`) and must not set a width of its own** | an inline handle + `v-show` + its own mousemove drag, or a slot child with its own `usePanelResize` — two widths and two handles on one edge, so dragging the outer one shifts the content instead of reflowing it |
| A panel resized from its RIGHT edge (a left-hand pane, e.g. the `/tasks` list) | `usePanelResize({ edge: 'right' })` — the same composable; `edge` only flips the sign of the drag | a second composable, or negating the delta at the call site |
| Draggable / detached panel | `components/FloatingPanel.vue` | a bespoke `position:fixed` panel |
| A figure of these settings, behind a button | `components/ParamFigure.vue` (the toggle + the float + `VisualAid`); the figure itself comes from a producer — `tasks/paramVis.ts` for a group, a `tasks/paramFigures.ts` entry for one param | a second `ref` + `FloatingPanel` + `VisualAid` trio in another branch of the renderer |
| Dismissible first-use hint | `components/HintCallout.vue` | a one-off info box |
| A short line with its reasoning on hover | `components/InlineNote.vue` | an `<i class="pi …"/> {{ text }}` + `v-tooltip` by hand (four sites had one each, two already drifted off the severity model) |
| Explaining what a dropdown OPTION means | a `help` field on the option in the task JSON (`utils/optionHelp.ts` renders it) | overloading the param `tip`, or a param advisory — an advisory carries a severity, and `ok` draws a green check claiming a verdict nobody reached |
| Teaching a multi-step workflow | a `GuideDef` in `lib/guides/` (a `moduleTaskGuide({…})` call when the page is ModuleLayout + TaskRunner) — see *Guides* | a page full of explanatory prose, a bespoke tour component, or hand-writing the five standard task-runner steps a fourth time |
| Placing any floating box beside an anchor element | `utils/anchorPosition.ts` — `placeBox({anchor, box, viewport, placement})` + `arrowOffset`; `TeleportPopover` and `GuideBubble` both call it | a second `getBoundingClientRect` → clamp → flip block (this is the "my popover gets clipped" bug, extracted) |
| "This page was just filled in from X — Undo" | `components/RestoreNotice.vue` (+ `composables/useMovieRestore.ts` for the movie case) | `HintCallout` (a permanent per-id hint, not a per-action one) or a toast (no Undo, gone in 3s) |
| QC severity (ok/warn/fail) | `lib/severity.ts` + `--cc-sev-*` tokens | a hand-typed traffic-light colour |
| Task/chain status (5-state) | `lib/taskStatus.ts` (`TASK_STATUS`) | a per-file status→icon/colour map |
| Reducing an image's SEVERAL runs to one status badge | `lib/taskStatus.ts` → `rollupTaskStatus` (live > terminal, then most recent) | `.find(t => t.imageUid === uid)` — that is store insertion order, which `adopt()` reshuffles |
| Choosing an ICON for anything | `frontend/src/lib/iconLegend.ts` — find the meaning, use its glyph | a glyph that "looks right", or a second glyph for a meaning that already has one |
| Badge / pill / tag naming WHICH MODULE OR TASK something came from | `.cc-module-tag` (+ `-mod` / `-fun` parts) in `style.css`, tinted by `utils/taskModule.ts` → `moduleTagStyle(module)` | a scoped `.x-badge`/`.x-pill`/`.x-tag` rule, or `moduleColor(m) + '33'` inline (guarded by a detector in `taskModule.test.ts`) |
| Making an accent colour readable as text on its own tint | `utils/colour.ts` → `readableOn(colour, bg)` (+ `composite`/`contrastRatio`/`luminance`, WCAG 2.1) | swapping the accent for `--cc-text` (throws the identity away), or eyeballing a lighter hex |

**Semantic role utilities** (global classes in `style.css` — *compose* them, add only layout in scoped CSS). These generalise recurring text/surface **scenarios** rather than a component per widget:

| Scenario | Use | Never |
|------|-----|-------|
| Secondary / muted text (hint, subtitle, caption, meta) | `.cc-muted` (+ a `.cc-fs-*` step) | a scoped `color: var(--cc-text-dim); font-size: …` |
| Small dim label beside a control | `.cc-muted` — same scenario, no separate utility | a per-file `.*-lbl`/`.*-label` |
| Meta line carrying a WARNING or an ERROR | `.cc-muted-warn` / `.cc-muted-error` (+ a `.cc-fs-*` step) | `.cc-muted` plus a scoped `color:`, or an inline `style="color: var(--cc-sev-fail)"` |
| Empty / "nothing here yet" state | `.cc-empty` (+ `-inline` one-liner / `-overlay` over a plot / `-lg` rich page empty) | a new `.*-empty` class |
| A row of items that must WRAP in a narrow container — toolbar, control bar, option row, chip list, legend | `.cc-row` (+ `-tight` dense chrome / `-loose` page bar); keep the row's own padding/border in its scoped rule | a scoped `display:flex; align-items:center; flex-wrap:wrap; gap:…` |
| A label+input, slider+readout, or `X × Y` that must not split across lines | `.cc-row-group` inside a `.cc-row` | letting the row wrap between a label and its control |
| An on/off toggle whose caption sits OUTSIDE it (a row label, an eyebrow) | `CcToggle` + `aria-label` — a tooltip is not a name, and the hidden `<input>` has no text of its own (enforced by `unnamedToggles`) | relying on the row's `v-tooltip`, which covers hover help but leaves the control unnamed |
| Several such groups STACKED, whose labels should read as a column | `.cc-lbl-col` on each label (+ `.cc-row-group-top` on a group whose content wraps); override the width with `--cc-lbl-col` | letting each label size itself, so every control starts at a different x |
| Numeric value readout beside a control | `.cc-readout` (+ `-strong` prominent; + a `.cc-fs-*` step) | a bespoke `.*-val`/`.*-num` |
| Eyebrow / section label (uppercase dim heading) | `.cc-eyebrow` (base is 11px; + a `.cc-fs-*` step) | a scoped uppercase-heading rule |
| Card / panel / surface container | `.cc-card` (+ `-2` when it sits *on* a surface-1 panel) | a scoped `surface + 1px border + radius` block |
| Corner radius | `--cc-radius-xs/sm/md/lg/pill` | a raw `rem`/`px` radius |
| Small text size | the `--cc-fs-*` token in CSS, or the `.cc-fs-*` class in markup | a raw `rem`/`px` font-size (incl. inline `style=`) |
| Compact input / select / textarea | `.cc-input-xs` (11px) / `.cc-input-2xs` (10px) — sets size AND padding. **The base is already 12px**, so most fields need neither | a scoped class re-typing the base's border/colour/background to change the size |
| A colour a token already holds | that token — `var(--cc-accent)`, not `#a78bfa` | a hex literal, **or** a `var(--x, #hex)` fallback (add the token, never a fallback) |

**Pick a scenario, then a size.** `.cc-muted .cc-fs-xs` · `.cc-eyebrow .cc-fs-2xs` ·
`.cc-readout .cc-fs-2xs` · `.cc-empty-inline .cc-fs-3xs`. The size ladder is ONE shared set of classes
(`.cc-fs-lg/-md/-sm/-xs/-2xs/-3xs`, the same steps as the `--cc-fs-*` tokens), not a per-scenario one:
`.cc-muted-2xs`, `.cc-eyebrow-2xs` and `.cc-readout-2xs` were three names for one declaration, and
naming them per scenario only made you guess which to reach for. Modifiers that carry real semantics
DO stay on their scenario — `.cc-readout-strong` is prominence, `.cc-empty-inline` is layout.

**The step is named, not relative** (`-xs`, not `-dense`). A relative name can only express the steps
someone thought of: `.cc-muted` had no 11px step — the single largest cluster of hand-rolled muted text
in the app — because "dense" was already spent on 10px, and nothing named the step *above* the base. Reach for the modifier instead of re-declaring the
scenario locally: baking a value into the base is what stranded ~10 sites as "bespoke" before. Per-site
*emphasis* (`font-style: italic`) and *geometry* (width/margin/flex/padding) still belong in scoped CSS.

**There are no raw sizes or radii left, and the tests keep it that way.** Both scales were derived from
the actual distribution rather than guessed: 33 distinct font-size spellings collapsed onto 6 steps (98%
of them were already within 0.5px of a step), and 15 radius spellings onto 5. `--cc-radius-sm` was retuned
`0.25rem`→`0.3rem` because 4.8px is the modal radius, which halved the worst-case shift. A literal
`font-size`/`border-radius` anywhere — scoped CSS **or** an inline `style=` — now fails
`utils/cssScenarios.test.ts`. Exempt by rule: display type (>15px), pill radii, `0`, and `em` (which is
deliberately container-relative, e.g. `ViewLegend` scaling with the export).

**Icon buttons are a fixed square**, so a toolbar row lines up regardless of glyph width — that's why
`-icon` is a modifier rather than per-site padding (48 sites had each discovered they needed a fixed box,
at nine different sizes). `-bare` is transparent/dim-until-hover, `-ghost` is its boxed counterpart; tone
comes from `-danger-ghost` or a scoped `color` for the one-offs (the napari viewer green). A `<button>`
whose whole content is an icon and which doesn't use `.cc-btn` fails `utils/cssScenarios.test.ts`.

> **Where the checks live:** `frontend/src/utils/cssScenarios.ts` and `cssTokens.ts`, each with a
> `.test.ts` beside it (`pixi run test-frontend`). Full index of what each detector owns and what its
> bar is: [`docs/todo/UX_PRIMITIVES_PLAN.md`](../todo/UX_PRIMITIVES_PLAN.md) → *The detectors*. Need a
> count for this area? Read those — never re-derive one by grep.

**Never re-declare a utility in scoped CSS — compose it.** A `<style scoped>` rule whose selector *is*
a global utility (`.cc-muted { color: …; font-size: … }`) outranks the global one on specificity, because
scoping adds `[data-v-…]`. The component then silently stops tracking the utility: change the global and
this one place doesn't follow. It happens by accident during migration — rename the class in the template,
rename the *rule* alongside it instead of deleting it — which is exactly how `LegacyMigrateDialog` ended up
shadowing `.cc-muted` with a byte-identical copy. `utils/cssScenarios.test.ts` now fails on it, with no
allow-list: per-site layout (`.cc-muted { margin-top: 0.3rem }`), descendants (`.panel .cc-muted`) and
modifier compounds (`.cc-btn-bare.viewer-green`) are all legal by construction, so anything it reports is
the bug. Add layout in scoped CSS; never re-state a property the utility itself declares.

**A reason is not an exemption.** The shadowed-utility allow-list carried nine entries, each with a note
that read as settled — "a tier down", "muted layout, danger colour" — and six of them were scenarios the
axis already had room for: four wanted `.cc-muted` + a `.cc-fs-*` step, one restated the utility's own
value, and one wanted `.cc-muted-error`, which did not exist only because `-warn` had shipped alone. That
last one is the tell: a missing family member gets hand-rolled, so the SECOND site spells it a way no
ratchet can see (an inline `style="color: …"`). Three entries remain, on the two grounds no utility can
express: a size driven by a runtime CSS var (`--gate-font`, the vis Font size slider) and a deliberate
`color: inherit`. Before adding an entry, check the utility does not already exist, or is not one
modifier away from existing.

**Declare before you watch.** A `watch` SOURCE runs immediately — that first call is how Vue
collects the dependencies, with or without `immediate: true` — so a source naming a `const` declared
below it throws `ReferenceError: can't access lexical declaration 'x' before initialization` during
`setup`. TypeScript cannot see it (the binding exists, it is just not initialised), the dev server
serves the module, and every test passes. What you get is a blank page and a console-only clue.

It is worth its own checker because of the blast radius: the throw aborts the PARENT's patch, so
innocent siblings vanish with it. One mis-ordered line in `FlowMetricsView` blanked the whole flow
canvas — the plot panels and the model vault, which is a sibling and had nothing to do with it — and
it read as a data problem. `utils/setupOrder.ts` (`setupOrderHazards`) ratchets it to zero with no
allow-list. It checks the source only (a callback runs later and may name anything), all of
`watchEffect`, and top-level calls only.

**A tier that most sites override is the wrong default.** The form-control base was `--cc-fs-md` (= body)
and read as "the fields are too big" in every dialog — twice reported from the running app. The fix was
not another opt-in class: **33 form controls across 24 files had each hand-written
`font-size: var(--cc-fs-sm)`**, while exactly *one* site had ever adopted the density class. When two
thirds of the population corrects the default by hand, the default is wrong. The base is `--cc-fs-sm`
(12px) now, the density steps re-pitched below it, and those 33 declarations are gone as provable no-ops.
The same rounding caused the tooltip regression (`0.72rem` → nearest step, which happened to be the larger
one → now `--cc-fs-xs`). **When tokenising a value that sits between two steps, check which side the
element belongs on** — dense chrome rounds down, not to the nearest.

**Re-implementing a scenario is a test failure, not a style opinion.** `utils/cssScenarios.test.ts`
detects a scoped rule that spells out a canonical utility's defining declarations — a dim colour plus a
hard-coded size *is* `.cc-muted` — and holds a per-file baseline that **may shrink and must never grow**.
Touch a file, migrate its rules and lower its number; add a new one and the suite names it. Card chrome
is deliberately *not* checked: `surface + border + radius` is the shape of a card, an input, a chip, a
badge and an icon-button alike, and ~60% of matches wanted `.cc-btn`/`ChipSelect` instead, so it stays a
review-time rule.

**Standalone pages use `ModulePage`; the image-table pages use `ModuleLayout`.** 16 of the 23 module
pages are built on `ModuleLayout` and were already consistent. The standalone ones were not: Notebooks,
Animation and Movies had each grown their own frame — three h1 sizes (1.1 / 1.15 / 1.4rem), two paddings,
two subtitle widths, and `.nb-header`/`.anim-head`/`.mov-head` doing the same flex-space-between under
different names. (The h1 sizes escaped the size sweep because `findRawValues` exempts anything over 15px
as display type.) `ModulePage` fixes controls and spacing; `layout="flow|scroll|fill"` is the one
real axis — whether the page flows, scrolls itself, or is a full-height pane whose child scrolls. Per-page
extras go on the call site as a class (Vue puts the parent's scope ID on a child's root, so a scoped rule
still applies).

**A page whose work is per-image belongs on `ModuleLayout`, however it started.** Animation moved
across (Dominik, 2026-08-10): as a standalone page it read whichever image napari happened to have
open, so its empty state — *"open an image in napari to start capturing keyframes"* — could only be
acted on by navigating to some other page, picking an image there, and coming back. The image table IS
that action, and it carries the set bar, the filters and the eye with it. Which changes what the page's
own chrome is for: with a side panel, the render options no longer need the `MovieOptionsButton` gear
(that exists for the viewer, which has no panel), and the timeline goes in the standard `#plots` canvas.
The test: if the page acts on one image, the table is the picker — do not invent a second one, and do
not make "it's open elsewhere" a precondition the page cannot satisfy.

**Do not write a page subtitle — or a page title.** All three carried a paragraph explaining the feature
to a first-time reader: permanent noise on a screen its owner uses daily, and the clearest tell that a
page was AI-written. The `<h1>` went the same way (Dominik, 2026-08-10) — the sidebar names the page and
highlights it, so a heading repeating that word is chrome the daily user reads past forever. `ModulePage`
therefore has neither, and Settings dropped its own `.page-title` to match. The controls say what the
page is; the explanation belongs in `docs/`. Same rule as tooltips and QC findings: if you are tempted to
explain in the UI, that text goes in the relevant `docs/<AREA>.md` instead.

**Tokens live on `:root`, and that is load-bearing.** `.cc-dark` is a `<div>` inside `<body>`
(`App.vue`'s shell), so anything a library appends to `document.body` is a *sibling* of it and inherits
nothing declared there. PrimeVue's tooltip does exactly that — so while the scale sat on `.cc-dark`,
every `var(--cc-*)` in the tooltip override was invalid at computed-value time and the tooltip rendered
at the browser default **16px**, with `<body>`'s own `font-size` dead the same way. Declared ≠ reachable,
and the symptoms are identical, which is why the token guard stayed green throughout. If you style
anything that mounts outside the app shell (a portal, teleport, or library overlay), check that the
properties it references resolve *there*. `utils/cssTokens.test.ts` now fails if the global scale is
declared anywhere but `:root`.

**Every custom property you reference must be declared somewhere.** An undeclared one does not warn —
it makes the whole declaration invalid at computed-value time, so `var(--cc-text-muted, #888)` silently
freezes a hard-coded grey that never tracks the theme, and a fallback-less `background: var(--cc-surface)`
drops the *entire* `background` shorthand (a `<select>` lost both its fill and the global custom caret this
way). `utils/cssTokens.test.ts` fails the build on any such reference — add the token, don't add a fallback.
It checks **all** `--*` properties, not just `--cc-*` (a stray `--text-muted` had been hiding behind the
prefix), and counts a component-local declaration — including an inline `:style="{ '--foo': … }"` for a
dynamic value — as valid.

If what you need isn't here and isn't obviously covered by an existing component, grep first
(`docs/inventory/FRONTEND.md`); only build new if the search is genuinely empty, and then add it here +
to `INVENTORY.md` in the same change.

---


---

**Unification status + what's not yet extracted:** [`docs/todo/UX_PRIMITIVES_PLAN.md`](../todo/UX_PRIMITIVES_PLAN.md).
**Copy budgets for whatever you just rendered:** [`docs/ui/COPY.md`](COPY.md).
