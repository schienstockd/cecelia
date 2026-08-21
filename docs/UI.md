# Cecelia UI Guide

Frontend conventions, component catalog, and how to add new UI features. Purely Vue/CSS — the
language boundary and WS protocol are in `ARCHITECTURE.md`.

> **This file is large — read a slice, not the whole thing.** `grep -n '^#\{2,3\} ' docs/UI.md`
> gives you the section index with line numbers, then `sed -n 'START,ENDp' docs/UI.md` reads just the
> one you need. See the routing table below to pick the section.

**The two MANDATORY lookups now live in their own files** (they are read on every frontend task, so
they are not buried in here):

| Mandatory before you… | Read |
|---|---|
| render **any** button, toggle, slider, dialog, popover, tabs, chips, empty state, spinner, badge, collapsible | [`docs/ui/PRIMITIVES.md`](ui/PRIMITIVES.md) — the UX primitive catalog |
| write **any** user-facing text: labels, tooltips, tips, empty states, QC findings | [`docs/ui/COPY.md`](ui/COPY.md) — copy budgets + tooltip coverage |

Both are enforced by tests, so skipping them fails the build.

## Section routing — the rest of this file

| Looking for | Section |
|---|---|
| Colours, radii, font sizes, fixed dimensions | **Design tokens** |
| Buttons · inputs · toggles · chips | **Button utilities** · **Form controls** |
| Modals · confirms · deletes · popovers | **Modals & dialogs** |
| Floating windows, legends | **Floating panels** · **View legend** |
| Building a new module page (route, sidebar, layout) | **Adding a new module page** |
| The image table, task runner, sidebar, viewer | **ImageTable** · **TaskRunner** · **AppSidebar** · **ViewerPanel** |
| Adding a plot to a page or the board | **Adding a plot or visualization panel** |
| Floating/draggable plot panels, tile & cascade | **Shared canvas shell** |
| Making a plot option survive navigation | **Persisting view state — the three scopes** |
| Keeping a plot fresh after a task | **Data freshness — task-refresh** |
| The chain whiteboard | **Chain whiteboard** |

**Neighbouring docs — this one does not restate them.** `docs/PLOTS.md` (plot-spec schema, chart
types), `docs/ANALYSIS.md` (the Analysis board: tabs, plates, export), `docs/POPULATION.md` (gating
model + the gating plot stack's internals), `docs/MODULES.md` (task JSON + param widgets),
`docs/NAPARI.md` (viewer process + layers), `docs/todo/UX_PRIMITIVES_PLAN.md` (unification status).

---

## Design tokens

All tokens live in `frontend/src/style.css` under `.cc-dark` (always applied at the `<body>` level).

| Token | Value | Use |
|-------|-------|-----|
| `--cc-bg` | `#0f1117` | Page background |
| `--cc-surface-1` | `#161b22` | Sidebar, panels |
| `--cc-surface-2` | `#21262d` | Hover states, inset boxes |
| `--cc-text` | `#e6edf3` | Primary text |
| `--cc-text-dim` | `#7d8590` | Secondary text, labels |
| `--cc-border` | `#30363d` | All borders |
| `--cc-accent` | `#a78bfa` | Active elements, buttons, links |
| `--cc-accent-strong` | `#7c3aed` | Deeper violet — the border of an engaged/active control |
| `--cc-accent-soft` | `#c4b5fd` | Pale violet — text on an accent-tinted surface |
| `--cc-accent-tint` / `-tint-2` | `#2d1b69` / `#3b2382` | The tinted "option is on" surface, and its hover step (`.cc-btn-on-tint`) |
| `--cc-selected` | `#ff8c1a` | Amber selection/active highlight for BOXES (panels, cards, timeline keyframes) — distinct from `--cc-accent` (form controls) |
| `--cc-warn` | `#f59e0b` | Amber that is *not* a severity — a decorative/identity hue (a chain node's colour, a keyframe badge) |
| `--cc-danger` | `#ef4444` | The **destructive-action** tone (a delete button's hover/armed state) — an action, not a status |
| `--cc-viewer` | `#22c55e` | Green accent for the napari viewer controls button + its floating-panel border (stands apart from purple chrome) |
| `--cc-sev-ok` | `#0ca30c` | Severity **ok** (QC/traffic-light). Colour-blind-safe status palette |
| `--cc-sev-warn` | `#fab219` | Severity **warn** — any *status indicator* saying "heads up" (a validation warning, a stale-data strip, an advisory axis flag) |
| `--cc-sev-fail` | `#d03b3b` | Severity **fail** — any *status indicator* saying "this is broken" (an invalid field, an error dot, a failed task) |

| `--cc-mono` | system monospace stack | Log output, code |

**Scales and fixed sizes.** Never write a raw `rem`/`px` for these — `cssScenarios.test.ts` fails the build.

| Token | Value | Use |
|-------|-------|-----|
| `--cc-radius-xs` `-sm` `-md` `-lg` `-pill` | `0.2` `0.3` `0.4` `0.5rem` `999px` | chips/swatches · buttons/inputs · cards/panels/dialogs · large dialogs · pills |
| `--cc-fs-3xs` `-2xs` `-xs` `-sm` `-md` `-lg` | `0.56` `0.62` `0.68` `0.75` `0.82` `0.9rem` | ≈9 · 10 · 11 · 12px · body · 14px |
| `--cc-header-h` · `--cc-sidebar-w` · `--cc-runner-w` | `40px` · `190px` · `280px` | Header · sidebar · TaskRunner panel |
| `--cc-console-bar-h` · `--cc-console-open-h` | `30px` · `210px` | Console collapsed · expanded |

### Toast notifications (transient foreground feedback)

PrimeVue `<Toast />` is mounted once in `App.vue` (registered via `ToastService` in `main.ts`); call
`useToast()` anywhere. **Do not add a second notification system.** Toast is for a foreground action
the user just triggered and is waiting on (a cohort check, a longer save) — NOT for background
scheduler progress (that's the task manager) nor for every lab-log entry (those badge). Severity maps
to the traffic-light scale: `info` (in progress) · `success` (done, all-clear) · `warn` (done, findings)
· `error` (failed). First consumer: `CohortCheckButton.vue`.

The four notification surfaces — pick the one that fits, don't invent a fifth:
- **Toast** — transient, auto-dismiss; a foreground op in progress / just done.
- **Badge** — persistent "needs attention" (unseen lab-log entry, QC warning) until acknowledged.
- **Lab log entry** — durable record, kept across sessions.
- **Traffic light** — per-image summary state in the image table, always current.
- **Scale column** — the image's calibration (`0.5 µm · 30s`), or **not set** in severity amber when
  it has none. Always present, sortable, and sorted so the uncalibrated images come FIRST — finding
  them is the reason to sort it. Clicking "not set" opens the physical-size editor, the same dialog
  the warning triangle opens. An image with no pixel size cannot run anything that measures in
  microns (`requires.scale`, `docs/MODULES.md`), and that state is DERIVED from the metadata — never
  the exclusion flag, which is a user's choice and carries a user's note.

### Severity (QC / traffic-light) — colour is never the only cue

`--cc-sev-ok`/`-warn`/`-fail` are the ONE severity palette (colour-blind-safe). Import the mapping from
`frontend/src/lib/severity.ts` (`SEVERITY`, `worstSeverity`, `severityFor`); the lab-log glyph counterpart
is `qc.jl` `severity_symbol` (✅/⚠️/❌, never 🟢🟡🔴). **Never render a severity as colour alone** — always
pair the hue with a shape-distinct icon + label. See `docs/todo/QC_OBSERVER_PLAN.md`.

**Which amber/red? The split is status vs not.** If the colour states *the condition of something*
(valid/invalid, fresh/stale, ok/warn/fail) it is a severity → `--cc-sev-*`; opting out silently costs
colour-blind separation. If it is a destructive **action**'s tone (a delete button) or a decorative
identity hue (a chain node), it is not a severity → `--cc-warn`/`--cc-danger`.

---

## Hard requirements

**Tooltips: every control a user *sets*, and every icon-only button, carries a `v-tooltip`.** Place
it where it reads best (`.left` / `.top` / `.bottom` / `.right`) — there is no default side. A button
with a visible caption does **not** need one. CellProfiler is the reference for *density*, one line
each. The exact scope, what counts as coverage, and the ratchet are in [`docs/ui/COPY.md`](ui/COPY.md) → *Tooltip coverage* —
**that section is the rule; this is the pointer.** Don't restate it here.

All errors go to `useLogStore().error(msg, { source, detail })`.
Task failures must never be silent — errors must reach the console bar visible to the user.

## The console

One component — `components/ErrorConsole.vue` over the `log` store — mounted in two places: the docked
bar at the bottom of the app shell, and (with the `fill` prop) full-window in the standalone **console
window**. Do not build a second console. The window is a `bare` route (`/console`, `meta.bare` →
`App.vue` renders it without the shell) opened via `window.open(origin + pathname + '#/console', …)`
from the docked console bar's pop-out (↗) button; being a separate browser window it's a fresh app
instance with its own WS.

**It shows every producer in the app, not just the backend.** That was not always true — see
`docs/ARCHITECTURE.md` → *The log rail* for the server half and for what used to be discarded.

| Chip | What it carries | `source` values |
|---|---|---|
| **App** | this browser: user actions, fetch failures, Vue render errors, unhandled rejections | the ~19 fine-grained UI tags (`manageImages`, `gating`, `movies`, …), `ws`, `frontend` |
| **Backend** | the Julia server's own `@info`/`@warn`/`@error` | `backend` |
| **Tasks** | a failed task or chain step (the full run log stays in the task drawer) | `task`, `chain` |
| **Napari** · **Preview** · **Runner** · **Notebooks** | each child process's stdout/stderr | `napari`, `preview`, `runner`, `notebooks` |

Four rules, all of them load-bearing:

1. **`source` is a closed set on the server side.** `SERVER_LOG_SOURCES` in `utils/logFilter.ts`
   mirrors `LOG_SOURCES` in `app/src/log_stream.jl`, asserted by the *"log sources agree across
   languages"* testset. A frontend call site may still use any tag it likes — unknown tags group under
   **App**, so a new panel needs no change here to be reachable.
2. **The child chips are OFF by default, and an error is never hidden by a chip.** The bridge prints a
   line per label layer; that is narration, not news. Switching a child off stops the narration, and
   its errors still appear. A console that can silently withhold a stacktrace is the thing this design
   exists to prevent (`isVisible`, `utils/logFilter.ts`).
3. **Autoscroll follows only while you are at the bottom.** Scrolling up pauses it; scrolling back
   resumes. Unconditional jump-to-bottom was fine when the console was quiet and is unusable now.
4. **Backfill is the store's job, on every mount and every reconnect.** `log.backfill()` →
   `GET /api/logs/recent?since=<seq>`. The docked console never used to do this, so the console
   actually in front of you started blank on every page load while the pop-out showed history.

Filtering, grouping, gap detection and the copy format are pure functions in `utils/logFilter.ts`
(unit-tested) — the SFC only renders.

## Settings → System (service control panel)

`SettingsModule.vue` has a **System** section: one row per runtime component (Application / Napari /
Notebooks) with a status pill (Running / Starting… / Stopped, polled every ~4 s from the existing
`/api/{napari,notebooks}/status` endpoints — ephemeral UI state, a plain `ref`, NOT persisted) and
start/stop/restart buttons that reuse the existing control endpoints, plus a
global **Quit** (`POST /api/app/shutdown`, behind a two-click `ConfirmButton` — see *No native browser
dialogs* below). (The pop-out **console window** is
launched from the docked console bar, not this panel.) Status→verb/label mapping is the pure,
unit-tested `utils/serviceStatus.ts`. Backend self-restart is planned (see
`docs/todo/SERVICE_PANEL_PLAN.md`, Phase 3).

---

## Button utilities

Global classes in `style.css`. `.cc-btn` is always the base, plus modifiers on **four independent
axes** — compose them; never re-declare a button in a component's scoped `<style>`.

| Axis | Modifiers |
|---|---|
| Tone | `-primary` · `-ghost` · `-bare` (transparent, dim-until-hover) · `-danger` · `-danger-ghost` |
| Density | `-micro` · `-dense` · `-lg` |
| Shape | `-icon` (fixed square, so a toolbar row aligns regardless of glyph width) |
| State | `-on` (+ `-on-tint` washed / `-on-solid` filled) for an engaged toggle button |

`.cc-btn-group` joins a strip of them. All support `:disabled` (opacity 0.35) and `v-tooltip`.

```html
<button class="cc-btn cc-btn-ghost" @click="…">Apply</button>
<button class="cc-btn cc-btn-primary" @click="…"><i class="pi pi-plus" /> Add images</button>
<button class="cc-btn cc-btn-danger" @click="…">Delete project</button>          <!-- standalone -->
<button class="cc-btn cc-btn-danger-ghost" @click="…">Delete</button>            <!-- inline in a bar -->
<button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip="'Settings'"><i class="pi pi-cog" /></button>
```

**Never hand-roll `.btn-sm` / `.btn-primary` / `.btn-danger` in scoped CSS.** A class that *looks*
shared but is re-declared per file drifts (the danger colour was `#b91c1c` in one place and `#7f1d1d44`
in three; disabled opacity varied 0.35/0.4/0.55), and a page that used the class without its own copy
rendered a raw browser button. `cssScenarios.test.ts` now fails on an icon-only `<button>` that skips
`.cc-btn`.

## Form controls

`style.css` styles **all native form controls app-wide** — bare `<select>`, `<input type="text|
number|search|…">`, `<textarea>` get the consistent surface/border/rounded look, accent
focus ring, a custom `<select>` chevron (the native arrow is hidden via `appearance:none`), and
accent-tinted `range`/`checkbox`/`radio`. **Do not re-declare background/border/border-radius/
padding/outline on inputs in component styles** — it diverges from the rest of the app (this was
the "old-school inputs look inconsistent" bug). Keep only layout in scoped styles (width, flex,
`min-width`) plus state modifiers (`.input-error`, `[readonly]`, `:disabled`, `.mono`). If a
`<select>` sets `background:` (shorthand) it will wipe the chevron — use `background-color`.

### On/off toggles — `CcToggle`

`components/CcToggle.vue` is the ONE boolean on/off switch. Use it for an **immediate boolean
OPTION** — a setting that applies the moment you flip it (autoplay, loop, show-legend, title card,
dark theme, "pool to groups", …). It's a styled `<input type="checkbox">` under the hood, so it
works with `v-model` (or `:model-value` + `@update:model-value` when you also run a side-effect
handler), keyboard focus, and `disabled`. Label via the `label` prop or the default slot (slot wins);
add a tooltip at the call site with `v-tooltip`. It renders its own `<label>`, so **don't nest it
inside another `<label>`** — for a label-left / control-right settings row, make the row a `<div>` and
drop in a label-less `<CcToggle>` (see `PlotOptions`/`SummaryPanel` `.po-row`/`.sp-pop-row`).

**When NOT to use it — keep a native `<input type="checkbox">`:** a multi-SELECT list or a value
staged as part of a form (image / channel / feature / measure pickers, "select all", per-row
selection). A column of sliding switches reads worse and misuses the on/off affordance. So the rule
is: **toggle = one immediate option; checkbox = selection from a list.** Don't hand-roll another.

### Suggesting what you already use — `SuggestInput`

`components/SuggestInput.vue` is the ONE control for a field whose value the user invents but
usually repeats. Type freely; what already exists is offered **on focus, and narrowed as you type**.
Callers today:

| surface | offers | why it matters |
|---|---|---|
| task params (`valueNameInput` + `namespace`) | names in that namespace | re-running onto an existing label set vs creating a third one |
| Metadata → *Assign value* | values already used for that attribute | attribute values are the **grouping axis**: a typo does not error, it invents a cohort group |
| Movies → row tag editor | `tagsInUse` (`separator=","`) | matches what the bulk panel's `ChipSelect` already offered |

Its twin is a strict `<select>` (`valueNameSelection` in a task spec), right where the value must
already exist and wrong where a new one is legal — you could never enter it.

**Not for a field whose value must be UNIQUE.** Population names, notebook names and model renames all
reject a duplicate, so offering the existing ones suggests something guaranteed to fail. `markExisting`
(an accent border when the text matches) is the affordance there, not a list.

> **Do not "simplify" this back to a native `<datalist>`.** It is the obvious implementation and it
> was the first one. A `<datalist>` popup is browser **chrome**: it renders at the browser's own UI
> font (~16px), ignores every `--cc-*` token, and next to a `0.82rem` input the options are roughly
> twice the size of the field they belong to. No selector reaches it — not a scoped rule, not a
> global one. The escalation to a real popover was paid for by seeing it on screen, not anticipated.

Built on `TeleportPopover`, so it inherits teleporting, positioning, theme tokens, outside-click and
Escape, and owns only the list and the keys. Three behaviours worth knowing before changing it:

- **Opens on focus, showing everything already in use.** It opened on typing at first, to keep an
  untouched form clear of a popover nobody asked for — which gets the question backwards: *"what did I
  call the other one?"* is exactly what you cannot answer if the list appears only once you can spell
  it. Focus shows all (an empty query filters nothing), the first keystroke narrows, and a field with
  no history opens nothing and behaves like a plain input.
- **Nothing is highlighted after a keystroke** (`moveHighlight` returns `-1`). You are naming something
  NEW until an arrow key says otherwise, so Enter must not silently accept a suggestion.
- **Rows accept on `mousedown`, not `click`** — the input's `@blur` closes the popover first, so a
  `click` handler would never fire.
- **A multi-value field (`separator`) drops what is already in the box** (`withoutChosen`), so the
  same tag cannot be added twice from the list and it gets shorter as you pick. The token at the
  CARET is exempt — it is what you are typing, not something you chose.
- **Accepting with Enter must not also fire the caller's own Enter handler.** Several callers commit
  on Enter (the movie tag cell saves and closes; Metadata assigns the value), and a caller's listener
  is a SIBLING on the same input — so `stopPropagation` does not reach it and `stopImmediatePropagation`
  is required, plus a one-shot swallow of the following `keyup`. `v-bind="$attrs"` is therefore bound
  LAST, so this component's listeners run first.
- **`separator` makes it multi-value** (tags): suggestions complete the token at the caret, so
  accepting one does not replace the tags already typed.
- **It exposes `focus()`** — a `ref` to a component is not the `<input>`, so an inline-edit helper
  calling `el.focus()` would silently do nothing.

### Selection chips / segmented controls — `ChipSelect`

`components/ChipSelect.vue` is the ONE canonical inline selector — use it for any pill/capsule or
segmented button-row that picks from a set. Two variants: `variant="pill"` (wrapping
capsules, the default) and `variant="segmented"` (a joined control). `multiple` for multi-select
(`modelValue` is an ordered `string[]`; single-select is a `string`); add `reorderable` (pill only)
for drag-to-reorder. Per-option `icon` / `tip` / `disabled` / `badge` (a count) / `accent` (a
semantic colour — rendered as a readable tint). Pure logic in `utils/chipSelect.ts` (tested).

`select-all` (multiple only, opt-in) prepends an **All** chip that fills the selection or empties a
full one, with an `n/total` badge so "how many are on?" stops being a counting exercise on a long
channel list. It is dashed, because it acts *on* the set rather than being a member of it. The rule
worth knowing: from a **partial** selection it COMPLETES rather than clears — throwing away picks is
the one outcome nobody wants from a bulk control — and disabled options are excluded from both the
tally and the fill, or the toggle strands at partial with no way to reach all. Every chip multi-select
in the task-param form turns it on (`ParamRenderer`: `chipSelect`, `channelSelection`,
`labelPropsColsSelection`, multi `popSelection`); on a grouped list each group gets its own All.

Active colour is `--cc-accent`. **Don't** use it for: independent-boolean toolbars that also fire
actions or open dropdowns (e.g. `ModuleLayout`'s filter-toggle bar, gate arrange/nav clusters),
colour-swatch grids (`PopulationManager` palette, `SwatchSelect`), the cluster-assignment matrix
(cross-population-exclusive + integral solid colour), or reorderable tab strips (`TabbedCanvas`).

### Continuous controls — the effect is coalesced, never per event (mandatory)

A slider is a **burst source**. `<input type="range">` emits an event per pixel of travel, so a short
drag is 20–60 events, and a drag handle or a wheel gesture is the same shape. Writing a value 60 times
costs nothing. Doing anything *slow* 60 times reads as a bug, always the same way: **the thing you are
dragging keeps moving for seconds after you let go**, working through requests you already superseded.

**The rule.** A continuous control's `@input` may write state. Any effect beyond that — a request, a
napari command, a full chart rebuild — must be one of:

| | Use | When |
|---|---|---|
| **Coalesced — a request** | `utils/debouncedLatest.ts` | Someone is waiting for the answer (a preview, a plot fetch, a live napari push). One run per burst, never two in flight, and the running call gets `isCurrent()` so a superseded reply can't land. Keep the wait SHORT (~80 ms) for something judged by watching — this is coalescing, not deferral. |
| **Coalesced — a paint** | `utils/rafCoalesce.ts` | The effect is pure drawing (`PlotChart`, `useCanvasZoom`). The frame is the right unit: the last value before the browser paints is the only one worth drawing, and there is no result to keep. `peek()` exposes the pending value so steps within one frame compound instead of cancelling. |
| **Coalesced — a write** | `utils/debouncedSave.ts` | Write-behind autosave (boards, canvases, animations). Nothing waits on it, but a RESTORE writes the same state a user edit does — `duringRestore()` suppresses the echo for a window derived from the debounce, so the two can't drift apart. |
| **On release** | `@change` instead of `@input` | The effect is expensive and there is nothing to see mid-drag. `@input` still writes the value so the readout tracks the thumb; `@change` fires once, on release. See `PoolThrottle` and the napari-dots slider in `PopulationManager`. |

Three helpers rather than one because the three differ in what happens to superseded work: a request
keeps a result and must discard the stale one, a paint has no result at all, and a write has no result
but does have a restore to defend against. **Do not hand-roll a fourth** `setTimeout` + sequence-token
pair — that is what these were extracted from.

**Put the coalescing at the SINK, not at each call site.** There is one napari viewer, so
`utils/napariOverlays.ts` owns one scheduler per live endpoint (`pushZView`, `pushLabelContour`) and a
second call site cannot reintroduce the spam. Same reflex as every other cross-cutting helper here: one
way to do it, and the second way is the bug. A slider three components away from the sink can't be
audited by reading either file — only the sink can hold the guarantee.

Enforced by `utils/continuousControls.test.ts`: it scans every SFC for range inputs, and a handler that
*calls* something (rather than writing a value or emitting) must name where its effect lands. It also
pins the live napari endpoints to their one owner. It cannot follow an `emit` into the parent — which is
exactly how the z-slider bug got in — so the sink-side rule above is the part that actually holds.

**A `ResizeObserver` callback may MEASURE, never write layout.** Same rule, structural version: a
callback that resizes an observed element during delivery is what the browser reports as
`ResizeObserver loop completed with undelivered notifications` — and it reports it after the FIRST
write, so a settle guard (`> 1px`) bounds the loop but never silences the message. Two fixes, by shape:
a plot that renders into its host uses `composables/usePlotResize.ts` (rAF + skip a render the size did
not ask for); a box that sizes ITSELF schedules the write through `rafCoalesce` (`CanvasPanel`'s square
panels — the gating plot panels — did it inline and that was the rail's mystery error). In DEV,
`utils/roLoopTrace.ts` wraps the constructor and names the observer that did it, ours or a dependency's.
Exemptions live in `continuousControls.test.ts` with a reason each, and a reason has to be about the
WRITE, not about looping.

**"It measures, it writes nothing" is the reason that fools you.** An observer reports the CONTENT box,
so sizing a CHILD from the measurement is a write to the parent's box whenever a scrollbar is involved:
grow the child past an `overflow: auto` box, a scrollbar appears, and the box the observer reports
shrinks — while the BORDER box never moves. The Movies player's zoom did exactly that, and it stayed
anonymous through a release because the trace compared border boxes only; it now measures the inner box
too and says "a scrollbar appeared or went". So the rule for a *measuring* callback: if the measurement
sizes anything inside the observed element, coalesce it like any other write. Both conditional
exemptions are pinned to their callback LINE, not to prose.

**Say that a slow result is coming.** A control whose effect is coalesced looks broken if nothing
changes for 200 ms. Pair it with the delayed spinner + stale dimming (see *Plot loading state* below).

---

## Icons — one meaning per glyph, and the glossary is the reference

`frontend/src/lib/iconLegend.ts` is THE list of what every glyph in this app means, grouped by family.
**Consult it before choosing an icon**: find the meaning you need and use its glyph, or — if the meaning
is genuinely new — add an entry saying what it means. Users read the same list from the **key** in the
header (`pi-key`, beside the Guides compass, rendered by `IconLegendDialog.vue`).

It cannot rot, because `iconLegend.test.ts` scans every glyph actually rendered under `frontend/src`
(comments stripped) and fails when one is **missing from the list**, or **listed and rendered nowhere**.
A new icon therefore fails the suite until somebody says what it means.

Two rules, both learned the hard way in the 2026-08-17 audit (126 glyphs, ~600 uses):

- **One meaning per glyph.** `pi-replay` meant both "run it again" (task re-run, notebook restore) *and*
  "cancel" (the canvas confirm pairs' Keep button, now `pi-undo`). `pi-sliders-h` meant both "Settings"
  and "napari viewer controls" — 40 px apart in the same sidebar; Settings is now `pi-cog`. A cog
  labelled "Run history" is now `pi-history`.
- **One glyph per meaning.** The busy state was split almost exactly 50/50 between `pi-spin pi-cog` (29
  uses) and `pi-spin pi-spinner` (28) with nothing choosing between them; it is now **always
  `pi-spin pi-spinner`**, which frees `pi-cog` to mean only settings/options. `pi-spin` is a *modifier*,
  not a glyph. "Edit" was split between `pi-pencil` and `pi-file-edit`; it is `pi-pencil`.

## Modals & dialogs — always use `BaseModal`

**Every centred modal/dialog is built on `frontend/src/components/BaseModal.vue`. Never hand-roll an
overlay (`position:fixed; inset:0`)** — that copy-paste produced four near-identical shells before this
existed. We do **not** use PrimeVue Dialog.

`BaseModal` provides the dimmed overlay, the centred surface box, the header (icon + title + ✕), and
close-on-✕ / click-outside / **Escape**. You provide the content via slots.

- **Props:** `title` (string), `icon` (a PrimeIcons class, e.g. `pi-box`), `width` (CSS, default
  `480px`), `height` (optional fixed CSS height; omit to size to content, capped at `90vh`).
- **Slots:** default = the scrolling **body**; `#footer` = pinned action row; `#toolbar` = a pinned row
  under the header (search bars, tabs, breadcrumbs); `#title` = override the whole title area (e.g. to
  add an info-dot tooltip). The body scrolls; header/toolbar/footer stay pinned.
- **Emits:** `close` — the host owns visibility (`v-if` + `@close`).

Minimal dialog — copy this:

```vue
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
const emit = defineEmits<{ (e: 'close'): void }>()
</script>

<template>
  <BaseModal title="My dialog" icon="pi-cog" width="520px" @close="emit('close')">
    <div style="padding: 1rem">…body…</div>            <!-- scrolls -->
    <template #footer>
      <span style="flex:1" />                            <!-- push buttons right -->
      <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-primary cc-btn-dense" @click="…">Save</button>
    </template>
  </BaseModal>
</template>
```

Host it with `v-if`: `<MyDialog v-if="show" @close="show = false" />`. Put dialog-specific CSS in the
child's scoped `<style>`; the shell (overlay/box/header/footer) is BaseModal's — don't restyle it.
Working examples: `PackagesDialog.vue` (toolbar + body), `PhysicalSizeDialog.vue` (`#title` slot +
footer), `FileBrowser.vue` (toolbar + footer), `ClaudeOverviewDialog.vue` (a static how-to — content
from `lib/claudeOverview.ts`, opened by the `?` button in the lab-log toolbar), `ImageMetadataDialog.vue`
(read-only "everything we know about this image" — original source file path + dimensions/calibration/
channels/files/attrs, opened by the info icon on every `ImageTable` row). *(An in-canvas overlay
like `GateOverlay` is a different thing — that's `position:absolute` inside a plot, not a modal.)*

### No native browser dialogs — use `ConfirmButton` for confirms

**Never use `window.confirm` / `alert` / `prompt`.** Native dialogs look out of place (OS-styled, not
our theme), block the JS thread, and can't be positioned or styled. For a destructive-action confirm,
use **`frontend/src/components/ConfirmButton.vue`** — a logic-only wrapper with a **scoped slot**: the
first click arms it, showing **Confirm + Cancel** in place; `@confirm` fires only on the second click
(auto-disarms on an outside click / timeout).

**The host renders the buttons** (via the slot props `{ armed, arm, confirm, cancel }`), NOT the
component — this is deliberate: a child component's rendered DOM does **not** receive a parent's
*scoped* CSS, so if `ConfirmButton` rendered the button, host `.footer-btn` / `.btn-danger` styling
wouldn't reach it (this bit us once — the Quit button rendered unstyled). Rendering the buttons in the
host keeps them in the host's style scope. The wrapper is `display:contents`, so the buttons lay out as
if direct children of the host.

```vue
<ConfirmButton @confirm="doDelete" v-slot="{ armed, arm, confirm, cancel }">
  <button v-if="!armed" class="cc-btn cc-btn-danger cc-btn-dense" :disabled="!selected" @click="arm"
          v-tooltip.bottom="'Delete…'"><i class="pi pi-trash" /></button>
  <template v-else>
    <button class="cc-btn cc-btn-danger cc-btn-dense" @click="confirm">Confirm</button>
    <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="cancel">Cancel</button>
  </template>
</ConfirmButton>
```

`needsConfirm=false` makes `arm` fire immediately with no arm step (e.g. closing an already-empty
board). Used by the sidebar/Settings **Quit** and the board close in `TabbedCanvas`. For a bigger
modal decision (not a single button), use `BaseModal`.

#### Delete affordance — `ConfirmDeleteButton` (the app-wide standard)

For a **destructive icon delete** (label set, population, attribute, notebook, chain, node, …) use
**`frontend/src/components/ConfirmDeleteButton.vue`** — the ONE delete affordance. It's a single icon
button that arms on the first click (**trash → warning triangle, solid danger fill**) and fires
`@confirm` on the second (the ViewerPanel labels pattern D picked as the standard). It **wraps**
`ConfirmButton` for the arm/confirm/dismiss logic and renders its own self-contained chrome (`.cc-del`)
— self-styled *because* it must look identical everywhere (and hosts' scoped `.opt-btn`/`.pm-icon`/
`.wb-btn` classes can't reach a button rendered inside it anyway). Don't hand-roll a per-site
icon-flip or a Confirm+Cancel pair for deletes; that inconsistency is exactly what this replaced.

```vue
<ConfirmDeleteButton title="Delete population"
                     armed-title="Click again to delete this population"
                     @confirm="deletePop(path)" />
```

Props: `title` / `armedTitle` (tooltips), `disabled`, `needsConfirm`, `autoDismissMs`; default slot →
a text label beside the icon (e.g. "Delete set"). Tooltip position is PrimeVue's default + its
out-of-bounds flip (a `tip` prop can't drive position — dynamic directive modifiers aren't possible).
For a host with a **hover-reveal** row action, target the inner button with `:deep(.cc-del)` (see
`ViewerPanel`). The louder **named** text confirms for whole-image / whole-set deletion (`ImageTable`,
`SetBar`: "Delete NAME? [Confirm] [Cancel]") are a deliberate higher tier and stay as-is.

### Coord-fixed plots — 1:1 square

Plots whose axes must stay isotropic (the cluster **UMAP**, the **gating** scatter) render as a
**1:1 square**, so the embedding/flow cloud never warps and HTML overlays (centroid labels, facet
titles, gate labels) line up with the canvas dots.

- **`components/plots/SquarePlot.vue`** — the shared square *primitive*: a container-query box sized to
  `min(100cqw, 100cqh)`, centred. Use it to square a plot whose canvas fills the box with no internal
  padding (UMAP wraps its plot in it).
- **Gating** can't use `SquarePlot`: (a) its axis labels live in the capture box's asymmetric padding
  and the PNG export reads `.panel-plot`'s `offsetLeft/Top` (zoom-immune), so a positioned wrapper /
  squaring the *outer* box would break the export or leave the *dots* rectangular; (b) `SquarePlot`'s
  container-query needs a **definite parent height**, which the montage tiles (content-driven height)
  don't have. So `GateScatterCell` squares **`.panel-plot`** with **`aspect-ratio: 1`** — ONE method
  across both gating contexts (the gate module page *and* the montage tiles).
- **`CanvasPanel :square="true"`** — the shared **resize-box** logic: snaps a *free-floating* panel's
  height to its width on resize so the square plot fills it with no blank space. Used identically by the
  gate plot + pairs panels (pass it directly) and the UMAP (opts in via `interactiveViews.ts` →
  `square: true`, forwarded by `InteractivePanel`). No-op when docked (the board grid owns slot size) or
  collapsed. This is the "same 1:1 resize box for gating and UMAP".

---

## Floating panels — `FloatingPanel`

**A floating, draggable, resizable, collapsible box that floats above the app content
(`position: fixed`).** Use it for tool controls that should be reachable on any page rather than
pinned into the sidebar. `frontend/src/components/FloatingPanel.vue` is generic (not viewer-specific);
the **napari Viewer controls** are its first consumer — mounted in `App.vue`, toggled by the sidebar's
"Viewer controls" button (`settings.viewerPanelOpen`, persisted):

```vue
<FloatingPanel v-if="settings.viewerPanelOpen" title="Viewer" icon="pi-eye" storage-key="viewer"
               @close="settings.viewerPanelOpen = false">
  <ViewerPanel />
</FloatingPanel>
```

- **Parent owns visibility** (`v-if` + `@close`); the panel owns position/size/collapsed/maximised,
  persisted per `storageKey` under `cc.floating.<storageKey>` (reopens where you left it). Drag by the
  header, resize from the bottom-right grip, collapse to header-only, **maximise** (button, or
  double-click the header). Maximised fills the width and everything below the app header; the panel
  neither drags nor resizes in that state, and `x/y/w/h` keep the restore geometry so un-maximising
  returns it where it was, even across a reload.
- **The top bound is the app header, not zero** — `utils/panelBounds.ts`. Panels stack from
  `PANEL_Z_BASE` = 60 and `AppHeader` is `z-index: 100`, so a panel dragged above the header's bottom
  edge is not merely hidden: the header paints over it and eats its pointer events. The first thing to
  disappear is the panel's own header, which is the only handle you can drag it back by — so the panel
  became **unrecoverable without clearing localStorage**. The bound was `0` in two places (the drag path
  and the mount/resize path); both are now one call, and a panel saved in the bad position self-heals on
  next mount. **Don't reintroduce a bare `0`, and don't inline the maths a third time** — the pure
  version is unit-tested (`panelBounds.test.ts`).
- **Stacking** — panels start at z-index 60 (above content and the right panel, below
  modals/console) and are ordered **most-recently-touched on top**: opening a panel or pressing
  anywhere inside it raises it above its siblings, so two open panels no longer stack by DOM
  declaration order. The ordering lives in [`utils/panelStack.ts`](../frontend/src/utils/panelStack.ts)
  (`PANEL_Z_BASE` + one step per open panel, always well below the modal layer); `FloatingPanel`
  binds the result inline, so don't reintroduce a flat `z-index` in its stylesheet.
- Rationale: the viewer controls grew (populations, tracks, colour-by + legend) and crowded the left
  nav; a floating panel frees the nav and lets you place the controls beside the napari window.

## View legend — `ViewLegend` + `utils/viewLegend`

The shared **legend backbone** for describing what a napari view shows as colour swatches — image
**channels** (by colormap), **populations**, and a categorical **colour-by**. One model, many consumers
(the analysis-board image strip, the animation page, later movie overlays), so a colour reads the same
everywhere.

- **`utils/viewLegend.ts`** (pure, unit-tested) — `LegendItem`/`LegendSection` types; `channelLegend(layers)`
  (visible single-hue channel layers → swatches, via `napariColormap.ts`); `viewLegendSections({channels,
  populations, colourBy})` (drops empty groups, stable channel→pop→colour-by order).
- **`components/ViewLegend.vue`** — presentational: renders `LegendSection[]` as grouped swatches.
  Style-light (text inherits `color`, sizes with parent `font-size`), so each host styles it via its
  container (e.g. the image-strip overlay makes it white-on-dark). Section headings show only when there
  is more than one section.

The viewer panel's **colour-by** legend is deliberately NOT this component — it's an *editor*
(recolourable swatches), not a static legend.

## Pinia array reactivity

Use `splice()` to mutate arrays in place inside setup stores.
**Do not** replace the ref (`store.items = store.items.filter(...)`) — Vue loses reactivity.

---

## Adding a new module page

A module page is the full screen that opens when the user clicks a sidebar item.
The standard layout is: **SetBar** (top bar) + **image panel** (left, scrollable image list) + **right panel** (task runner, metadata editor, or a custom panel).

**Convention — attributes + filtering.** Every module page **except Import** must show
attribute columns and allow filtering by them: pass `:show-attrs="true" :show-filter="true"`
to `ModuleLayout`. Import is the only exception (images are imported there, before attrs
exist) — it uses `:show-filter="false"`. Metadata shows attrs but omits the filter (it's
where attrs are edited). Pages that operate on a single image (e.g. gating) add
`:single-select="true"` — this is independent of attrs/filter and composes with them.

### 1 — Create the Vue file

Use `ModuleLayout` (see below). The minimal template is:

```vue
<!-- frontend/src/modules/SegmentModule.vue -->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs, reload } = useTaskDefs('segment')   // category = the task JSON's fun_name prefix
</script>

<template>
  <ModuleLayout module="segment" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner :defs="defs" :on-reload-defs="reload" module="segment"
        :selected-uids="selectedUids" :selected-names="selectedNames" />
    </template>
  </ModuleLayout>
</template>
```

**Props to consider:**

| Prop | Default | When to change |
|------|---------|----------------|
| `module` | — | Always set; passed to ImageTable for per-module column config |
| `allow-manage` | `false` | `true` for Import (New/Rename/Delete set controls visible) |
| `show-attrs` | `false` | `true` for modules where attr columns (treatment, genotype…) are useful |
| `show-filter` | `true` | `false` for Import and other modules where filtering doesn't apply |
| `no-set-hint` | `"Select a set…"` | Custom empty-state message |

**Slots:**

- `#actions="{ hasSet, setUid, selectedUids, selectUids }"` — items injected into the action bar before the image count (e.g. "Add images" button). `hasSet` is `true` when a set is active — use it to disable the button. The selection is passed in so a bar item can **act on it** (Import's Copy / Move / Delete, see *File operations*); `selectUids([])` clears it afterwards.
- `#right="{ setUid, selectedUids, selectedNames }"` — the right-hand panel. All three slot props are computed inside `ModuleLayout`; the module page does not need its own refs for them.
- `#plots="{ setUid, selectedUids, selectedNames, selectUids, orderedUids }"` — **the module's plot canvas.** `ModuleLayout` wraps it in ONE consistent, collapse-persisted `CollapsibleSection` (label via the `plotsLabel` prop, default `'Plots'`). **Do not wrap it yourself** — this is what makes every module page's plot canvas collapse the same way. This is the canonical place for the summary/gating/cluster canvas. `selectUids(uids)` drives the table selection from the canvas; `orderedUids` is the visible image list in table order (filtered/hide-excluded applied) — used by the gating prev/next (`«`/`»`) buttons to step selection through the list.
- `#below-table="{ setUid, selectedUids, selectedNames, selectUids }"` — extra *custom* content below the plots (rare). Wrap each piece in `<CollapsibleSection>` yourself; multiple sections supported.

If you need the active set in the module page itself (e.g. Import's file-browser guard), import `useProjectStore` and call `project.activeSet()` directly.

### Adding the plot canvas below the image table

Put the canvas in the `#plots` slot — nothing else. `ModuleLayout` gives it a consistent, collapsible, **collapse-persisted** section (per module, under `cc-plots-open:<module>`); pass `plots-label` to rename the header:

```vue
<ModuleLayout module="behaviourAnalysis" :show-attrs="true" plots-label="Plots">
  <template #plots="{ selectedUids }">
    <SummaryCanvas :image-uids="selectedUids" module="behaviourAnalysis" />
  </template>
</ModuleLayout>
```

Every module page uses this same slot, so the plot canvas collapses identically everywhere — don't hand-wrap a `CollapsibleSection` in the module (that's exactly the divergence this replaced: SegmentModule once rendered its canvas un-collapsible). The image table itself is in a `CollapsibleSection` ("Images") managed by `ModuleLayout`; all sections scroll together and the panel collapses horizontally with the ‹/› button.

`CollapsibleSection` props:
- `label` — section heading (uppercased in the toggle bar)
- `defaultOpen` — whether open on mount (default: `true`)
- `maxHeight` — CSS `max-height` for the body (default: `'320px'`; pass `'none'` to allow full growth). With `'none'` the body is `overflow-y: visible` (not a scroll container) so a `position: sticky` descendant sticks to the outer page scroll instead of a box that never scrolls.
- `storageKey` — when set, the open/closed state persists in localStorage under this key (the `#plots` wrapper uses this so a collapsed canvas stays collapsed across navigation)

### Two-half side panels — `usePaneExpand` + `PaneExpandBar`

A recurring **scenario**, not a widget: a right-hand panel made of two stacked halves where, on a laptop
screen, neither gets enough vertical room. Both panels that have this shape use the same primitive:

| Panel | Top half | Bottom half | Storage key |
|---|---|---|---|
| `tasks/TaskRunner.vue` (every module page) | function + params + run + pool | the module's task list | `cc-taskrunner-pane` |
| `modules/batchmovies/BatchMoviesPanel.vue` | the movie config | the batch's task list | `cc-batchmovies-pane` |

Three modes — `split` (default), `top`, `bottom` — persisted per panel. Each half's button also
*un*-expands it, so whichever half is hidden its own button brings it back and there is no state the user
can't click out of. The recipe is the mode on the root plus one CSS rule per half:

```ts
const { pane, toggle } = usePaneExpand('cc-mypanel-pane')
```
```vue
<div class="mypanel" :class="'pane-' + pane">
  <PaneExpandBar :pane="pane" top-label="movie config" bottom-label="task list"
                 top-icon="pi-cog" bottom-icon="pi-bars" @toggle="toggle" />
  …
```
```css
.mypanel.pane-bottom > .my-config { display: none; }   /* bottom expanded → hide the top half */
.mypanel.pane-top    > .my-tasks  { display: none; }
```

- **A rule per half, not a guard per element.** A half is usually several sibling elements, so
  `v-show` on each means a section added later is silently left visible — the rule matches by class and
  covers it. Give a single-component half a plain wrapper div (`BatchMoviesPanel`'s `.bm-tasks`) rather
  than reaching into the child's root from scoped CSS.
- **Never `v-if`.** Unmounting a config half discards whatever its children have fetched (population
  lists, model lists) and refetches on the way back. `display: none` keeps them alive.
- **The bar owns the tooltip wording**, so every panel phrases the action identically; the consumer only
  names its halves (lower-case, short — they go straight into "Expand the …").
- **Growth is the consumer's** — the primitive decides what's visible, not how the survivor uses the
  space. `TaskRunner` lifts its `params-section` `max-height` cap under `.pane-top`; `BatchMoviesPanel`
  needs nothing, because `ModuleLayout`'s `.right-slot` already scrolls.
- Anything in **neither** half stays visible in every mode — `BatchMoviesPanel`'s "napari is busy" banner
  is deliberately outside both, since it matters most while you are watching the task list.
- **The bar's default slot is a readout for the hidden half.** Expanding a half means losing sight of the
  other one, so put back the one thing you'd miss rather than nothing: `TaskRunner` shows
  `3 running · 1 queued` there while the task list is collapsed (and nothing when it's visible, which
  would just restate the list). One line, `.cc-readout`, no new row — the bar is already paid for.

### Collapsible side panels — `CollapsiblePanel`

The panel *around* those halves. A right-hand panel with a full-height handle on its left edge that
folds the content away, and a drag strip that resizes it. `components/CollapsiblePanel.vue`:

```vue
<CollapsiblePanel storage-key="cc.movies.width" label="movie list" :default-width="320">
  …content…
</CollapsiblePanel>
```

- **Collapse is ONE shared flag** — `settings.rightPanelCollapsed`. Folding the panel away on one page
  folds it on all of them, which is what module pages have always done; a new panel joins that rule
  rather than teaching the user a second one. Don't add a per-panel collapse key.
- **Width is per panel** (`storageKey` → localStorage). Panels hold different things, so one shared
  width would be wrong for all of them. `defaultWidth: null` (the default) sizes to content until the
  user drags — that is how module pages behave when they don't set `rightDefaultWidth`.
- `label` is what the panel holds, lower-case, and goes straight into "Show/Hide …".
- **`v-show`, never `v-if`** for the content — same reason as the two-half rule above.
- The drag half is `composables/usePanelResize.ts`, which `TaskRunner`/`MetadataPanel` still use
  directly (they resize but don't collapse). Bind its `widthStyle`, not `width`: an auto-width panel
  has no number, and every consumer would otherwise spell the same null check.

**Popovers — use `TeleportPopover`, don't hand-roll an absolute one.** Any ⚙/dropdown popover that
lives inside a panel (canvas, table, plot) WILL be clipped by the panel's `overflow`/scroll/transform.
`TeleportPopover` (`components/TeleportPopover.vue`) teleports to `<body>` so it escapes all of that,
positions `fixed` from an anchor element, re-anchors on scroll/resize, carries the `.cc-dark` theme
tokens, and dismisses on outside-click/Escape. Usage: `<TeleportPopover v-model="open" :anchor="btnEl"
placement="bottom-end">…</TeleportPopover>` where `btnEl` is a template ref on the trigger. The
popover owns only the shell (surface/border/shadow/position); the slot supplies the content + its own
inner styling. It clamps to the viewport and flips above when there's no room below. Reuse this rather
than another absolute/fixed popover that will clip or need its own dismiss/positioning logic — it is
the single implementation, used by the image-strip settings, the image-table run-log cog, the board
grid-size + custom-plate popovers, the summary-plot options, the gating-strategy options, and the
gate-pairs channel picker.

### 2 — Register the route

`frontend/src/main.ts` — **lazy-load the component** (see *Route-level code splitting* below); do **not**
add a static `import` at the top:
```ts
{ path: '/segment', component: () => import('./modules/SegmentModule.vue'), meta: { label: 'Segment' } },
```

#### Route-level code splitting

Every module page uses `component: () => import('./modules/…')` so each becomes its own chunk fetched on
navigation, not part of the initial `index` bundle. This matters: eagerly importing all pages once put
the whole app (Chain whiteboard + `@vue-flow`, the plot stack, every modal) into a single ~1.2 MB chunk
at boot; lazy routes cut the **initial** JS to ~240 KB (~54 KB gzip, an ~87% drop). A new page **must**
follow the lazy form — a static top-of-file `import X from './modules/X.vue'` silently pulls that page
(and its deps) back into the boot bundle.

Same rule for a **heavy library used on one screen**: dynamic-import it at the call site rather than at
module top, so it splits into its own on-demand chunk. Precedents: `@observablehq/plot`
(`await import('@observablehq/plot')` in `PlotChart`/cluster panels) and `pdf-lib`
(`await import('pdf-lib')` inside `plots/pdf.ts`'s export function — loads only when the user exports).

### 3 — Add the sidebar entry

`frontend/src/components/AppSidebar.vue`, inside the relevant `groups` array:
```ts
{
  to:               '/segment',
  label:            'Segment',
  icon:             'pi-th-large',
  tip:              'Run cell segmentation.',
  requiresProject:  true,
  // disabled: true, soon: true,   ← add while not yet implemented
}
```

`requiresProject: true` greys the item and shows a lock badge when no project is open.

### 4 — Add the task category (backend)

See `CLAUDE.md` (Adding a new Python task) for the Julia + Python side.
The frontend never maintains a copy of task definitions — they're fetched from `/api/tasks/definitions?category=segment`.

> **Tracking page** (`frontend/src/modules/TrackingModule.vue`, route `/track`) is a plain
> `ModuleLayout` + `TaskRunner` page in the Analysis group **after Gate**. It uses the
> `popSelection` param widget (added to `ParamRenderer.vue`): a dropdown listing
> `NONE (whole segmentation)` plus the flow population paths for the selected image +
> chosen segmentation (fetched from `/api/gating/popmap`). The widget reads its sibling
> `valueName` value and the selected image via the extended `ParamContext`
> (`{ images, projectUid, values }` from `TaskRunner`). It emits a population path string;
> the Julia handler resolves membership. See `docs/MODULES.md` (Param types) and
> `docs/TRACKING.md`.

---

## Onboarding — setup wizard, first-use hints, empty states

New-user UX (see `docs/todo/ONBOARDING_PLAN.md`):

- **First-launch setup wizard** — `frontend/src/modules/SetupModule.vue`, a `bare` route `/setup`
  (full-window, no shell). The boot guard in `main.ts` (`router.beforeEach`) asks the backend once via
  `appControl.refreshStartup()` (reads `/api/diagnostics` `setupRequired`); while setup is required
  every route redirects to `/setup`, and once done `/setup` bounces back to `/import`. The wizard
  picks a projects dir (`GET /api/setup/defaults`, live `GET /api/setup/validate`, `POST
  /api/setup/init`); the backend writes `custom.toml` (`Cecelia.set_projects_dir!`).
- **First-use hints** — `frontend/src/components/HintCallout.vue`: a one-line, dismiss-permanently
  callout keyed by id in `localStorage` (`cc.hint.<id>`). Module pages declare one via `ModuleLayout`'s
  `hint` + `hint-key` props (don't hand-roll it per page).

  **There are currently none, and that is the intended state.** `ONBOARDING_PLAN.md` P4 specified four
  and a fifth arrived later; on review they were doing three unrelated jobs and only one of them was a
  job nothing else could do. The bar for adding one:

  > An **interaction affordance with no other surface** — something invisible in the markup that no
  > live check could answer. Never pipeline ordering, and never a prerequisite.

  Ordering and prerequisites are **state** questions, and `lib/guides/prereqs.ts` answers them live in
  the guide picker while QC answers them per image after a run. A static sentence cannot: "Segment all
  timepoints first" is wrong for exactly the user who already segmented, and they are the ones who see
  it, because it fires on first visit regardless of state. That is the same substitution written up
  twice in `prereqs.ts` — `status === 'done'` for "imported", a run-log scan for "tracked" — a static
  answer to a state question, in a third costume.

  Where the four went: the gating draw affordance (the one that met the bar) folded into the gating
  guide's *Pick a shape* step, which is now the only place either half of it is stated; segment,
  tracking and optical flow were dropped as already covered by the sidebar order, the guide prereqs and
  the orientation tour; and the global "closing the tab does not stop the backend" hint became the
  tour's closing step, beside the Quit button it is about. A fact about a button belongs next to the
  button.
- **Empty states** — exactly two, and they already exist: `ProjectPanel.vue` (`.pp-empty.cc-empty`,
  no projects) and `ImageTable.vue` (`.cc-empty.cc-empty-lg`, no images). Extend the copy there;
  don't add a parallel component. These are the ONE carve-out from [`docs/ui/COPY.md`](ui/COPY.md): that
  budget exists because prose on a page you use daily is noise forever, which doesn't apply to a
  state a user sees once, before they know the app reads CZI. Everywhere else the budget holds.

  **The carve-out has a shape, not a blank cheque** — both existing states already fit it, so match
  them rather than inventing a third form:

  > a **title** (`No images yet`) · **at most two short lines** of orientation · **one CTA button**.

  Anything past that is the thing the budget exists to stop. The carve-out is for the *first-run*
  states above only: a "no populations yet" or "no results yet" empty state is an ordinary one and
  gets the one-line budget.
- **Shutdown** — reuse the existing sidebar-footer Quit (bottom-left) / Settings control
  (`appControl.quit()`); do **not** add another. Onboarding only *points at* it via the hint.

## Guides — bubble walkthroughs (the compass in the header)

The in-app answer to "can you send me a screencast": a **compass button beside the brand mark** opens
`GuidesDialog.vue`, and starting a guide puts a bubble beside the real control, on the user's own data.
Design + locked decisions: `docs/todo/GUIDE_SYSTEM_PLAN.md`.

**The one rule: a guide POINTS AND OBSERVES.** It never clicks, selects, navigates or runs anything.
There is no action field in the step type, so a guide cannot start a 12-minute segmentation on the
wrong image. A step that needs another page anchors to the *sidebar nav item* and waits for the click —
which is how navigating gets taught rather than done for you.

| Piece | Where |
|---|---|
| Catalogue (register a new guide here) | `frontend/src/lib/guides/index.ts` |
| Step / prereq / gate types | `lib/guides/types.ts` |
| Prerequisite registry | `lib/guides/prereqs.ts` |
| **The builder for "run a function" pages** | `lib/guides/moduleTask.ts` — `moduleTaskGuide()` for a whole page guide, `taskRunSteps()` for just the run-a-task block when a guide needs it mid-sequence (the import guide's convert phase) |
| Runtime (which guide, which step, what it waits for) | `stores/guide.ts` |
| Bubble + ring | `components/GuideBubble.vue` — chrome is `--cc-guide` (whitish), the same accent as the lab-log panel. Both the ring and the placement use `visibleRect()`, the anchor intersected with every clipping ancestor: `getBoundingClientRect()` ignores clipping, so a control taller than the panel scrolling it would otherwise be framed mostly off-panel. The compass MARK (header button + dialog title) uses the same token, so the mark and the surface it opens read as one thing. Deliberately not `--cc-accent`: purple is form/control chrome, so a purple ring round a purple button reads as part of the control rather than as a pointer at it |
| Picker | `components/GuidesDialog.vue`, open flag in `lib/guideOpen.ts` |
| Anchor resolution / reachability | `utils/guideAnchor.ts` |
| Positioning (shared with `TeleportPopover`) | `utils/anchorPosition.ts` |

**Adding a guide.** If the page is a `ModuleLayout` + `TaskRunner` one, it is a
`moduleTaskGuide({…})` call — **do not hand-write the standard steps** (pick set → tick images →
choose function → set params → Run → watch the rail). Those steps live in two shared components and
five anchors, so drift correct / segment / track / cluster / behaviour are ~15 lines each. Writing the
fourth by hand is how a pattern becomes four variants. A page with a genuinely different shape (the
gating canvas, the notebook server, the chain whiteboard) gets its own `GuideDef` file.

**A guide that runs a task PART-WAY through splices in `taskRunSteps()`** rather than restating those
steps. The import guide is the case: "Add images" only registers rows (`POST /api/images/register`), and
converting them to OME-Zarr is an ordinary task run (`importImages.omezarr`) the user dispatches through
the same furniture — so the guide is file-picking, then the shared block, then "now it says done".
`withSet: false` drops the set step when the guide has already covered it; `selectTitle`/`selectText`
reword the selection step for the context; `funHint` explains why THIS function and not the
near-identically named neighbour in the dropdown; `withPreview` inserts a "preview it first" step
before Run (only for a task the backend declares `task_previewable` — a composite inherits it from any
step, which is how segment+measure qualifies). Every call registers itself in `TASK_RUN_USES`, which is what
the selection-scope ratchet iterates.

**Anchors are `data-guide="<area>.<control>"` attributes**, namespaced, added to the markup at the
control. **One id may match several live elements** — every gating plot panel carries its own axis
controls, every table row its own eye — so `resolveAnchor` ranks them via `rankAnchorCandidates`
(visible → inside `.panel.active` → unoccluded, DOM order on a tie) instead of taking the first. Two
schemes:

- `data-guide="task.run"` → `[data-guide="task.run"]`
- `nav:/segment` → `a[href="#/segment"]` — the sidebar is data-driven, so **nav items need no attribute**

`lib/guides/guides.test.ts` asserts **every anchor id in the catalogue exists in the source**, and that
every `nav:`/`route` names a real route in `main.ts`. That ratchet is the point: a renamed button
otherwise breaks a guide silently, for the one user being onboarded, who will not report it. A missing
anchor at runtime degrades to a centred card with the same copy — never a dead-end.

**How a step completes** (`Next` is always available regardless — a gate makes the bubble confirm the
action, it never traps anyone):

| Gate | Use it for |
|---|---|
| `when(ctx)` | anything observable in a store — an image is selected, this image has labels. **Prefer this.** |
| `clickAnchor` | a control with no observable end state. Fragile by nature (a `v-for` re-render swaps the node), so only when `when` can't answer |
| `awaitTask({fun, label})` | park on a long run: the bubble becomes a spinner on the task rail and picks up on `done`; `failed`/`cancelled` gets its own state |
| `reveal({needed, anchor, text})` | the target is unreachable OR does not apply yet. **Pass an ARRAY when a control can be unusable for unrelated reasons** — the runtime shows the first cause whose `needed` is true, so each gets its own advice. `TaskRunner`'s Run button has three: the right panel is folded (→ panel handle), the runner's pane half is collapsed (→ pane toggles), the control isn't in the DOM yet (→ whatever creates it). A step with no cause matching but a present-yet-hidden anchor falls back to the last declared cause, so an unforeseen way of hiding a control still gives advice |

**The route is polled, not just listened for.** vue-router navigates a hash history with
`history.pushState`, which fires no `hashchange` — a listener-only version sits at the boot path
forever and every routed step reports "you are on another page". `routePathFromHash` is re-read on the
poll (and on `hashchange`/`popstate`, and when a guide starts).

**Auto-advance is armed per step.** It fires only when a gate becomes satisfied *while its step is
showing*; a step already satisfied on arrival shows a tick and waits for `Next`. Watching for a
false→true transition on the gate alone compares the new step's gate against the old step's, which let
the guide walk itself through everything the user had already done.

Predicates see a flat `GuideCtx` snapshot, never a store directly — a step that imports a store is a
step that can mutate one. `ctx.anchorValue(id)` covers controls that report to no store (`TaskRunner`'s
function `<select>`); `anchorExists` / `anchorReachable` separate "not in the DOM yet" from "hidden by
something", which need different advice. All three are DOM reads, so the store runs a ~250ms poll while
a guide is open.

**Prerequisites are shown, never enforced.** Each guide declares them from `PREREQ`; the picker checks
them live and offers the guide that fixes a miss. Start stays enabled — the user may know something we
can't see. Every predicate must be answerable from `CciaImage` + its `runLog` with **no request**, or it
belongs in the guide's prose instead — **and must reuse the canonical predicate** (`isImported` from
`utils/inclusion.ts`, `funsRun` from `utils/runLog.ts`), never a second answer to the same question. A
hand-rolled `status === 'done'` reported "no imported images" for a project full of them, because
`status` is the transient conversion-job state and not the record of the outcome.

**A guide's prose is an ASSERTION about the app, and the ratchets cannot check it.** They verify that
anchors and routes and fun names exist, that scopes match and that copy fits the budget — not that a
sentence is true. Every content bug in this system so far has been an invented fact (conversion is
automatic; cellpose measures; `status` means imported; there is one kind of clustering). So: **look up
each claim before writing it**, and prefer the canonical predicate (`isImported`, `funsRun`) over a
plausible-looking one.

**A prereq asks about STATE, so never answer it from provenance.** The run log
(`funsRun`) says which runs *this app executed*; it is silent about data that arrived any other way.
`tracked` shipped as a scan for `tracking.*` and told a project migrated from the R version — tracks on
disk, already clustered — that it "needs a tracked image". Prereqs now read what exists
(`trackValueNames`, `labels`, `filepaths` via `isImported`); `runLog` is for showing history, not for
gating. Second time this substitution bit, after `status` for imported. Two structural halves of that discipline are enforced — `funName`/`taskKey` must
be a real matching pair, and teaching the bare half of a composite must be declared in
`app/test/suite.jl` with a reason (see *a guide teaching a composite's bare half is declared*).

**Two boundaries to keep in mind when writing steps:**

- **napari is a separate window** — no bubble can point into it. Guides stop at the `ViewerPanel`
  control that puts something on screen and then *describe* what to look for. This is the one place a
  screencast genuinely beats the system.
- **No demo data.** Guides run on real projects, which is what the prerequisite system makes honest.
  Guides are not a substitute for the first-launch wizard and shouldn't grow into one.

### Recipes — "what are you trying to do?" (the second axis over the same guides)

The guides are indexed on ONE axis: *where in the pipeline am I* (the picker's Start / Data /
Populations / … groups, mirroring the sidebar). A **recipe** is the other axis — *which pipeline is
mine*: an ordered list of existing guides with a one-line reason attached to each, rendered as the
first section of `GuidesDialog`. Catalogue: `lib/guides/recipes.ts`. Design:
`docs/todo/WORKFLOW_RECIPES_PLAN.md`.

**The reasons are the product.** `segmentGuide` cannot say "use the motion segmentation instead" — it
is the cellpose guide. A recipe is the one place where "for this data, that tool, and here is why" gets
said once, instead of as a tip on every affected control. So a `why` states the FORK, not a summary of
the step, and `whenThisIsYou` is a recognition test ("photon-limited movie of moving cells inside
tissue") rather than a description.

**A recipe adds no runtime.** Starting a step starts the ordinary guide with the ordinary bubble;
`RecipeStep.guide` is a `GuideDef` id and `guides.test.ts` fails on one that does not resolve. Per-row
readiness is `guide.prereqsMet()` over the steps' own prereqs — including the derived "your view
profile hides this page" miss — so there is nothing recipe-specific to check.

**A scenario we have not written is a REQUEST, not a stub.** A `wanted: true` entry is a title plus a
link to the `recipe_request.yml` issue form (`recipeRequestUrl()` in `lib/links.ts` — outward URLs live
there, never inline in an SFC), because the forks in a real recipe come from measuring real data: the
intravital one is only writable because `SEG_QUALITY_PLAN.md` measured this lab's own movies. What the
request asks for is what we cannot guess — what they image, what they want out of it, and an example
image. The ask is stated ONCE above the request links, not as a sentence per row.

Copy budget: `whenThisIsYou` ≤100 chars, each `why` ≤110, both enforced in `guides.test.ts`. A number
appears only where it has been measured, and the plan cites where.

**Called "recipe", not "scenario", in the code** — `utils/cssScenarios.ts` and *pick a scenario, then a
size* already own that word here, and one grep should not return both concepts. The picker heading is
the user-facing wording and is a copy decision, not a rename of the type.

**Task names do not belong in `recipes.ts`.** `app/test/suite.jl` reads every non-test `.ts` in
`lib/guides/` as one blob and pairs the `funName`/`taskKey` literals in it against the Julia registry —
a task name pasted into a recipe would be counted as a guide's. A recipe names guides; the guides name
tasks. Pinned from the recipes side too, in `guides.test.ts`.

### The orientation tour — the one guide with no prerequisites

`lib/guides/tour.ts` (**Find your way around**, group `Start`, first in the picker) is the exception to
everything above, on purpose. Every other guide teaches a pipeline step and therefore needs data; this
one points **only at chrome** — the header, the sidebar CTAs, the console, three panels in Settings —
so it works on a first launch with an empty project, which is exactly when it runs. `prereqs: []`, and
`guides.test.ts` pins both that and the absence of any data-dependent anchor family (`images.`, `set.`,
`board.`, `popmanager.`, `viewer.`, …) in its steps. **Do not add a step to it that points at an image
table, a set or a plot** — the moment one does, the tour breaks for the person it was written for.

It has three ways in, and they are deliberately not three implementations:

| Entry | Where | Behaviour |
|---|---|---|
| The compass, like any guide | `AppHeader.vue` | listed first in the picker |
| "Show me" on the **about Cecelia** card | `lib/tips.ts` → `guideId` | the existing tip↔guide link (D7); no new plumbing |
| **Automatic, once ever** | `App.vue` | starts when the What's New dialog is closed for the first time |

The first-launch trigger needs no new persisted flag: `settings.tipsLastShown` is `''` until the daily
launch tip has fired once, ever, so reading it *before* the date stamp is the first-launch signal.
App.vue watches `isWhatsNewOpen` rather than the dialog's `@close` emit, because `WhatNewCard`'s
"Show me" closes the dialog itself — hanging off the emit would leave the flag unconsumed and the tour
would ambush the user days later, the next time they closed What's New from the header. Two guards:
`setupRequired === false` (the `/setup` route is `bare` — no header or sidebar to tour) and
`!guide.active` (don't replace a guide the user explicitly asked for).

**Pointing at a control that only sometimes exists is an anchor bug, not a `reveal` case.** The Settings
storage step anchors on **Scan storage**, not on "Free up space": the latter is behind
`v-if="storage.reclaimable.length"`, so it does not exist until a scan has run and never exists on a
project with nothing to reclaim. `reveal` is for a target that is unreachable *right now*; a target
that may never render needs a different anchor.

**Two mutually-exclusive elements may share one anchor id.** `console.bar` is on both the collapsed
`.console-bar` and the open `.console-panel` toolbar in `ErrorConsole.vue`. Only one is ever in the DOM,
so resolution cannot pick wrong — and anchoring only the collapsed bar would leave the tour pointing at
nothing for anyone who already had the console open.

### The header's outward links

The compass sits in a row of three: **guides → GitHub issues → Zulip chat**, reading left to right as
"walk me through it" → "this is broken" → "does anyone know?". The two links are `<a target="_blank">`
and take `cc-btn-bare`'s muted colour rather than `--cc-guide`, so they sit a step quieter than the
compass instead of competing with it for the same glance.

Every outward URL lives in **`lib/links.ts`** — `CECELIA_REPO_URL`, `CECELIA_ISSUES_URL` (the list, for
a browse-first entry point), `CECELIA_NEW_ISSUE_URL` (the form, for a "report this" action),
`CECELIA_RELEASES_URL`, `CECELIA_CHAT_URL`. There were three hardcoded `github.com/schienstockd/cecelia`
literals across the frontend before it and the header was about to add two more; a repo rename is
pending (`docs/SHIPPING.md` → *Repo swap*) and should be one edit, not a grep. `lib/links.ts` is string
constants only — anything that *asks* GitHub something (the update check) stays in
`stores/appControl.ts`.

A What's New tip card can carry `guideId` to render a **"Show me"** button that starts the matching
guide — so a topic is described once (tip = the summary, guide = the click-through) instead of twice.

## Explainer sketches + tips (What's New modal)

The tip-of-the-day / release-notes cards (`components/WhatNewCard.vue`, content in `lib/tips.ts` +
`lib/whatsNew.ts`) render an animated sketch through `<SketchCanvas>` from **feijoa** — a sibling
sketchbook repo (`github:schienstockd/feijoa`), consumed as a git dependency. A card points at one by id
(`sketchAnimation: { id: 'claude_mcp' }`); an id the catalogue doesn't have falls through to a grey
"Animation coming soon" placeholder.

**Adding a sketch is a TWO-repo change, and skipping the second half fails invisibly:**

1. Author it in feijoa (`~/cc-workspace/feijoa/src/sketches/<name>.ts`), register it in
   `src/sketches/index.ts` (map + `sketchList` + named export), `npm run typecheck`, **push `main`**.
2. In cecelia: `npm update feijoa` in `frontend/`, and **commit the changed `package-lock.json`**.

Step 2 is the one that gets forgotten. `frontend/package.json` declares the branch
(`github:schienstockd/feijoa#main`) but the **lock pins a commit sha**, and that's what installs — Linux
CI and `release.yml` run `npm ci` (which also trips on lockfile drift), the installers run `npm install`
and keep the locked sha. Meanwhile **dev resolves feijoa through the sibling-checkout Vite alias in
`vite.config.ts`**, so a new sketch renders perfectly on your machine while every release build shows the
grey placeholder. Verify with `ls frontend/node_modules/feijoa/src/sketches/` after the update, not by
looking at the dev server.

Tip copy follows [`docs/ui/COPY.md`](ui/COPY.md): a one-paragraph description plus 2-4 imperative steps. The
sketch carries the explanation; the card is not the place for prose.

Rationale + the sketch-act format: `docs/todo/SKETCH_ENGINE_PLAN.md`.

## Filtering rows by attribute — `AttrFilterPanel`

`frontend/src/components/AttrFilterPanel.vue` + `frontend/src/utils/attrFilter.ts`

A chip row per attribute key, then **Apply / Reset / Invert**. Two surfaces use it: the image table's
Filter dropdown (`ModuleLayout`) and the Movies list. It was the image table's alone and inline; nothing
about it is about images, so it takes `:rows` — anything carrying an `attr` bag — and a `noun` for its
tooltips. It renders **nothing** when the rows have no attributes, so a host can place it unguarded.

- **Draft vs applied is deliberate.** Picking chips does not narrow the list; Apply does. Narrowing on
  every click makes a multi-attribute filter a fight, because each partial selection hides the rows the
  next chip would have come from. `AttrFilterState` carries both, plus `invert`.
- **ALL keys, ANY value within a key** — a row is "control AND 4h", and picking two values of one
  attribute means either. A missing attribute reads as `''`, which is what makes the "never annotated"
  chip (rendered `—`) work. `invert` flips the whole verdict, not each clause.
- The host owns **whether it is open** (its own Filter button, in its own button idiom) and where the
  state lives — per module in `localStorage` for the image table, `cc.movies.attrFilter` for Movies.
- The panel shell is `.cc-filter-panel` / `-rows` / `-row` / `-key` / `-chips` in `style.css`, shared
  with `ModuleLayout`'s *Processed with* row, which is the same scenario.

---

## Movies player — what happens when a movie ends

`frontend/src/modules/MoviesModule.vue`

**At end** is ONE segmented control (`ChipSelect`) with three states — **Stop** · **Loop** · **Next** —
not a Loop toggle beside an Advance toggle. They are mutually exclusive outcomes of the same moment, so
two booleans could ask for both. `Loop` is the `<video>` element's own attribute; `Next` selects the
following row and plays it **whatever Autoplay says** (Autoplay is about picking a movie by hand). The
chain STOPS at the end of the list rather than wrapping, and stops if the playing movie is filtered
out — neither is a reason to jump back to the top of a list the user narrowed.

*Next* means the next movie **as shown** — filtered and sorted — so the page owns the sort
(`v-model:sort`, the ImageTable pattern) and orders the rows itself with the shared `sortRows`, rather
than the table sorting privately where nothing else can read the order. Persisted under the same
`cc.movies.sort` key the table used (`parseSortState`), so an existing sort survives. The setting is
`settings.moviesEndMode`, which migrates the `cc.moviesLoop` boolean it replaced.

---

## Movies list — the Details columns

`frontend/src/modules/MoviesModule.vue`

A **Details** toggle adds the SOURCE IMAGE's channels and attributes as columns beside each movie, the
same shape the image table's own attribute view has. Off by default: they say nothing until a project
has attributes, and the list lives in a side panel where every column costs width.

- **Which image a movie is of** is banked in the registry (`imageUid`, `api/src/movies_api.jl`), and
  resolved from the FILENAME for everything recorded before that — a batch file terminates with the uid,
  a viewer file starts with the image name (`utils/movies.ts` → `resolveMovieImageUid`). Two images that
  share a name resolve to nothing rather than to a guess.
- **The channel columns are slots**, so column *N* means the same thing on every row. The `image` /
  `in movie` picker switches what fills them: the image's own channels, or only the ones that movie
  shows (`channels`, banked by the recorder). A blank cell in `in movie` mode is information.
- The table scrolls inside the card, not the card itself, and the Movie column is pinned — with Details
  on the table is wider than the panel.

---

## ModuleLayout component

`frontend/src/components/ModuleLayout.vue`

Owns the full two-column layout, SetBar, image selection state, attr filtering, and the `filteredUids` / `selectedUids` / `selectedNames` derived state. Module pages receive these as slot props — they do not need their own refs.

**Selection is remembered across navigation.** The run-table checkbox selection is persisted in the project store (`getImageSelection`/`setImageSelection`, keyed by `${module}|${setUid}`), so leaving a module page and coming back restores it. `ImageTable` is the writer (seeds from the store on mount / set switch, commits on every toggle); `ModuleLayout` reads it to initialise `selectedUids` and to restore on set switch. Keying by module name keeps each page's selection its own (e.g. gating's single-select doesn't bleed into segment). It's in-memory/session-scoped and cleared on project load/close. This is generic — every module page gets it for free via `ModuleLayout`.

The filter panel renders automatically when `show-filter="true"` and the active set has either images with `attr` values or images with a run history. It disappears when there is neither, so it is safe to leave enabled even for modules that may or may not have attrs.

**Two filter families in the one dropdown:**
- **Attributes** — chips per attr key/value (Apply/Reset/Invert). The shared `AttrFilterPanel` (above),
  not this component's own; `ModuleLayout` holds the state and decides what it narrows.
- **Processed with** — a function picker + an *ever* / *last run* mode, to narrow the list to the images a given function has been run on. This answers "which images have I already denoised/segmented?" It is **derived** from each image's automatic run log (`CciaImage.runLog`) via the pure helpers in `frontend/src/utils/runLog.ts` (`wasProcessedWith`, `funsRunAcross`) — there is deliberately **no** separate persisted status attribute to keep in sync; the run log is the single source of truth. Only functions that have actually been run across the set are offered, and both modes ignore **failed** runs (a failed run left no output). The same run log backs the **run tag** shown beside each image's UID in `ImageTable` — a task-manager-style module pill for the image's last **successful** run (`lastSuccessfulRun` + `taskDefs.labelFor`, coloured from the shared `frontend/src/utils/taskModule.ts` palette that also colours the task manager) — and the per-row run-history cog popover. All three filter/hide states persist per module in `localStorage`.

**Collapsible chrome (free up working space).** Two persisted toggles, both in the `settings` store (`localStorage`):
- **Left nav** — the `pi-bars` button in `AppHeader` toggles `settings.sidebarCollapsed`; `AppSidebar` `v-show`s its `<nav>` off, so the main canvas reclaims the full width. (The `v-show` lives on the `<nav>`, not on the `<AppSidebar>` element — the component has two root nodes, so a component-level `v-show` has no single root to bind and silently no-ops.)
- **Right panel** — `ModuleLayout` wraps the `#right` slot (TaskRunner / MetadataPanel / custom) with a thin always-visible left-edge handle (`pi-angle-double-*`) that toggles `settings.rightPanelCollapsed`. Collapsed → only the handle remains; the function/tasks panel folds away to the right. Every module page gets this for free.

Both default expanded and persist across sessions/navigation.

**Left-panel collapse — two axes.** *Horizontal* (‹/›) shrinks the whole left panel to a 2.4rem strip.
*Vertical* collapses each section inside it: the image table ("Images") and the module's `#plots` slot,
each a `CollapsibleSection`. Both the plots wrapper (`cc-plots-open:<module>`) and any section given a
`storageKey` persist their open/closed state in localStorage; a section without one is transient. The
panel body scrolls when the sections together exceed the height.

---

## ImageTable component

`frontend/src/components/ImageTable.vue`

| Prop | Type | Notes |
|------|------|-------|
| `setUid` | `string` | Required. Drives the image list from the project store. |
| `module` | `string?` | Selects per-module column config (status column label, etc.) |
| `show-attrs` | `bool` | Show attr columns. Default: `false`. |
| `filter-uids` | `string[]?` | When set, only these UIDs are shown. Managed by `ModuleLayout`. |

Emits `selectionChange(uids: string[])`. `ModuleLayout` handles this internally.

### The module status badge rolls up N runs, it does not pick one

With `module` set the table shows a per-module status column — and an image routinely has **several**
tasks in that module: a failed run, its re-run, a second value name. One cell, so the set is reduced by
`rollupTaskStatus` (`lib/taskStatus.ts`, the five-state twin of `worstSeverity`):

- **Live beats terminal, and running beats queued.** A run in flight is the state you can still act on.
- **Among terminal states the most recent run wins** (`finishedAt`, then `seq`) — *not* a severity order.
  A failure surfaces anyway, because until you re-run it *is* the latest outcome; but ranking `failed`
  above `done` outright would leave a successful re-run badged Failed for the rest of the session, which
  is the more misleading of the two errors. Severity only breaks a tie between runs finishing in the
  same second.

It previously took `forModule(...).find(t => t.imageUid === img.uid)` — whichever row sat first in the
store array, i.e. insertion order, which `adopt()` reshuffles on every reconnect. With more than one run
the badge's tooltip lists them all (status · label → output value name), because the badge cannot say
*which* run failed and that is the question that sends you to the Tasks page.

**File operations live in the action bar, not in the rows.** Copy / Move / Delete act on the whole
checkbox **selection** and are rendered by `components/ImageFileActions.vue` in the Manage images page's
`#actions` slot — next to *Add images*, where a file manager puts them. Two rules follow:

- **They are Import-only.** Creating, re-filing and removing images is import-time curation; no other
  module page mounts `ImageFileActions`, so an analysis page cannot delete or move an image. **Crop to
  new image…** stays in the row's ⋯ menu (it needs the one image you clicked) but is likewise gated to
  `module === 'import'` — it creates an image too.
- **The ⋯ menu is per-image only.** Metadata, physical size, crop, copy UID, include/exclude, run
  history — anything that applies to *one* row. An action that reads "do this to the images I ticked"
  belongs in the action bar. That split is what keeps the menu short; it previously held Copy and Move
  as well, each one image at a time.

Every dialog takes the *selection*: `CopyDialog.vue` (`images: CciaImage[]`) dispatches one
`editImages.copyImage` task per image, and Move walks `/api/images/move` per image (both routes are
per-image).

### Deleting is one modal with four scopes

**There are exactly two places that delete image data**, and that is a deliberate ceiling
(`docs/todo/IMAGE_DELETE_PLAN.md`): the Manage images page's **Delete** modal (`DeleteImagesDialog.vue`) for
anything per-image, and **Settings → Storage** for the automatic whole-project reclaim. It used to be
five, spread across four screens.

The modal offers four scopes, radio-selected because they answer different questions and must not be
silently combinable. Each maps to one route, and `ImageFileActions.vue` runs it over the selection:

| Scope | Deletes | Route |
|---|---|---|
| Whole images | the image stores **and** everything derived | `/api/images/delete` |
| Versions | specific image versions + which one stays active | `/api/images/version/remove` |
| Label sets | specific segmentations + their measurements | `/api/images/labels/delete` |
| All analysis | everything derived, keeping the images | `/api/images/analysis/reset` |

Three rules that are easy to get wrong in a template live in the pure, tested
`utils/imageDelete.ts` — **use it, don't re-derive them**:

- Names are offered as the **union** across the selection, each chip badged `k/n` when only some
  images carry it, and skipped for the images that don't. An intersection hides the name entirely:
  select three images where two carry `B` and one doesn't, and `B` becomes undeletable until you
  re-select. The badge is what keeps the skip visible rather than silent.
- `default` is removed **last**, so `remove_image_version!`'s safe-primary un-import lands at the end
  of the loop rather than mid-way.
- The version that stays active is resolved **per image** (`resolveNewActive(own, removing, preferred,
  current)`). With a union list the user's pick may not exist on every image, and writing it into
  `_active` there would leave `ccid.json` naming a version that was never registered. For the same
  reason the "becomes un-imported" warning counts *images*, not the selection.

**One conflict blocks; a skip only warns.** The distinction is whether the user's stated intent can be
honoured:

- **Blocking** (`activeMismatches` → confirm greyed): an image keeps a version but *not* the one chosen
  to stay active. Substituting another version per image would look like it worked while quietly
  leaving that image on something the user didn't choose, so the modal says which count is affected and
  waits. An image that loses *every* version is **not** a conflict — it has no active to set, and that
  un-import is a legitimate outcome, warned about separately.
- **Non-blocking** (`partialNames` → a note): a version or label set that simply isn't on every selected
  image. It is applied where present and skipped elsewhere, which is the whole point of union
  semantics; the chip badge plus the note make the skip visible.

The versions scope **pre-selects every non-active version** — once a corrected version exists, the raw
import and the intermediates are what you no longer need.

Deleting a label set takes its **companions**: the registered labels zarr, the branch-label zarr, and
every `labelProps/` sidecar derived from that name (`{vn}__tracks.h5ad`, `{vn}__branch.h5ad`,
`{vn}.clustfeatures.json`, …). Prefix-driven (`{vn}.` / `{vn}__`) so a companion added later is swept
too, and so value_name `B` can't eat `B2.h5ad` — every `labelProps/` filename is built from the props
path (`img_label_props_path` / `img_track_props_path` / `img_branch_props_path`, and clustfeatures via
an extension swap), so the prefix rule is exhaustive by construction rather than by inspection.

**What a label-set delete deliberately does NOT take:** `gating/{vn}.json` — gate polygons are user
work, not output, so re-running the segmentation under the same name brings the strategy back. And
`spatialGraph/{suffix}.h5ad` / `spatialStats/{suffix}.json`, which are keyed by **run suffix, not
value_name** (the graph pools across segmentations), so there is no per-value_name file to take.

The modal **collects a plan and emits it**; the execution, the `k/N` readout and the toast stay in
`ImageFileActions`. Its own footer button carries the arm/confirm, so no scope is ever one click from
deleting. Two surfaces were removed once it covered them: the napari **ViewerPanel** no longer deletes
a label set (the viewer shows and hides layers; it does not curate the disk), and the
`importImages.remove` **task** is `hidden` from the module page — see *MODULES.md → hidden tasks*; it
stays registered, REPL-runnable and valid as a chain node.

**Progress on a bulk action comes from one of two places, never neither.** Copy goes over the **task
rail**, so the task console, the progress bar and the universal toast are free — but a batch must use
`taskStore.addMany(items, toastLabel)`, not N × `add()`, or `lastStarted` fires one "running in the
background" toast *per image*. Move and Delete are plain HTTP loops with no rail entry, so they report
themselves: a `k/N` `.cc-readout` beside the buttons while the loop runs, plus a `useToast()` line at
the end (and on partial failure, "k of N — see the log"). A destructive loop with no visible counter is
indistinguishable from a hang; `rm -r` on a multi-GB zarr is seconds per image. The "existing set OR new set by name" destination — the dropdown plus its collision check —
is the shared `utils/setDestination.ts` (`resolveSetDestination`/`destinationParams`), used by both;
don't re-validate it inline.

**Metadata warning icon.** A row shows a `pi-exclamation-triangle` next to the image name when
`metadataWarning(img)` (`frontend/src/lib/imageMetadataWarnings.ts` — the single source of truth,
shared with `PhysicalSizeDialog`'s inline warning so the two never disagree) flags missing/suspect
physical size or time-interval metadata. This includes `physicalSizeZCorrected` (the import-time
ImageJ-TIFF Z-spacing auto-fix, `omezarr.jl`) — an auto-corrected value stays flagged for human
confirmation even when it now looks plausible, since the source tag it was derived from (the
file's own ImageJ `spacing`/`unit`) isn't independently verifiable and has been observed to be a
placeholder rather than a real per-slice calibration on real data. Clicking the icon opens
`PhysicalSizeDialog.vue` right there (own local `physSizeDialogUid` ref — no page navigation),
focused on that image with the current checkbox selection carried in as the target set for
Apply/Fill-flagged. Shown on every module page — the icon isn't gated behind `showAttrs`/`module`.

**Frozen left columns.** The table lives in a `.table-scroll` (`overflow-x: auto`) wrapper; the
checkbox, viewer-eye, and **name** columns are `position: sticky` at fixed left offsets (0 / 36 / 68px)
so the image identity stays put while the channel/attr columns scroll (Excel-style freeze). Frozen
cells carry an opaque per-row background (`--row-bg`, set for hover/selected) so scrolled columns pass
under them; the header row sits above the body via `z-index`.

**Dimension columns.** A **Z** column (z-slice count) shows only when some image in the set is a
z-stack (`sizeZ > 1`); a **Duration** column (timelapse span = `(sizeT − 1) × timeIncrement`, formatted
via `utils/imageTable.ts → timelapseDuration`) shows only when some image is a timelapse (`sizeT > 1`)
— so 2D single-timepoint sets aren't cluttered with empty columns. All fields come straight from the
`CciaImage` payload (`sizeZ`/`sizeT`/`timeIncrement`/`timeIncrementUnit`).

**Excluded images are selectable on the import + metadata pages only** (`module === 'import' |
'metadata'` → `canSelectExcluded`): you curate/edit metadata there, including on excluded images, so
their checkboxes are enabled and select-all includes them. Everywhere else the selection stays the
runnable (included) subset (`includedUids`).

**CSV export** lives in `ModuleLayout`'s table-tools bar (next to Filter/Excluded): `exportCsv` →
`utils/imageTable.ts → imageTableCsvRows` (pure, tested) → `rowsToCsv`/`downloadBlob`
(`plots/export.ts`). It exports **every** image including excluded ones (flagged `Excluded` + the
`Exclusion note`), one aligned column per channel (`Channel 1…N`, value = the channel name) plus
Z/frames/duration/pixel-size and one column per attr.

**QC badge.** Separate from the metadata warning (which is import-metadata-specific), a row shows a
`pi-flag` **QC** badge when `qcSummary(img)` (`frontend/src/lib/qc.ts`) finds any QC finding on the
image. QC is the general "we processed this, but the output looks off" layer: the **backend** computes
findings per (task, output) into `1/{uid}/qc/{funName}/{valueName}.json` (see ARCHITECTURE → *QC
sidecars* and `docs/todo/QC_PLAN.md`); `qc.ts` only aggregates + formats them. It's **advisory** —
never blocks. `warn` findings tint amber; `info` are neutral. (MetadataPanel + chain-whiteboard
surfaces are later phases.)

The badge hover lists the findings **grouped by the task that raised them**, each group headed by the
shared `.cc-module-tag` pill (`qcTooltipHtml` + `groupByTask` in `lib/qc.ts`, labelled via
`taskDefs.labelFor` and tinted via `taskModule.moduleTagStyle`) — the same pill as the task manager's
row and the image table's run tag, so "which step" reads identically in all three places. An image
routinely carries findings from several steps at once — `p6t4mC` has one each from import, drift and AF
— and the flat list named none of them, so there was no way to tell which step to go back to. Two
consequences worth knowing before touching it:
- It is the **one structured tooltip in the app**: v-tooltip's object form with `escape: false` and a
  `class: 'qc-tip'` that scopes the block layout away from the ~200 phrase tooltips. A tooltip cannot
  render a badge otherwise. Every interpolated string is escaped in `qcTooltipHtml` — that is not
  optional, and a new field rendered there must go through `esc` too.
- The plain-text `QcSummary.long` is still produced (and still what MCP/lab-log style consumers would
  want), but it is **not** what the tooltip shows: `.p-tooltip-text` sets no `white-space`, so its
  `\n`s collapsed and three findings rendered as one run-on paragraph.

**Include / exclude an image.** Any image can be excluded from further processing/analysis — the
systematic successor to the old R app's `Include=Y/N` keyword (`CciaImage.included`, default `true`;
optional free-text `note`). The rule lives in ONE place — `frontend/src/utils/inclusion.ts`
(`isExcluded`/`isIncluded`/`includedUids`), so graying, select-all, and run-selection all agree.
- **Greyed, not hidden.** Excluded rows render dimmed (`.row-excluded`) with a persistent `pi-ban`
  **Excluded** badge (its tooltip shows the note), an editable note line under the UID, and an
  always-visible include/exclude toggle (`.incl-toggle`). Every other row shows the toggle on hover.
- **Unselectable.** Excluded images can't be checkbox-selected — dropped from select-all, "select
  flagged", and the remembered selection on reseed. Since every run (single task *and* chain) builds
  from the selection, this makes exclusion honored everywhere. `ChainModule`'s run list mirrors it
  (greyed, auto-select-all uses the included subset).
- **Hard-skipped in the backend too.** Belt-and-suspenders for run paths that bypass the checkboxes
  (chain resume, REPL): `_drop_excluded` (`api/src/sockets.jl`) filters excluded uids before dispatch
  and logs each skip. Set via `POST /api/images/inclusion/set`; `project.setInclusion` reflects it live.
- **Hide-excluded toggle.** One of the row filters below (default: show excluded rows, greyed).

**Row filters — add one by adding a table row, not a component.** The on/off toggles next to the
**Filter** button that hide image-table rows (**Excluded** / **Imported** / **Starred**) are declared
as data in `frontend/src/utils/rowFilters.ts` (`ROW_FILTERS`) and rendered by ONE `v-for` in
`ModuleLayout`. Each entry supplies its id, label, both icons, the `hides(img)` predicate, its count,
whether the button is worth showing at all, and a two-state tooltip. `ModuleLayout` holds a single
`rowFilterActive` bag, persisted per module under `cc-hide-<id>:<module>`.
**Do not hand-write a new toggle** — that is how this became three near-identical blocks of ref +
watch + computed + template. Add a `RowFilterDef`; the persistence, the button, the count and the
`filteredUids` clause all follow. Active filters AND together, and combine with the attribute and
processed-with filters.

**Star.** A plain per-image bookmark (`CciaImage.starred`, `isStarred`) — click the star in any row,
any number per set. It drives the Starred row filter and **nothing else**: no effect on selection,
runs, or processing. (It replaced a set-level single "reference image" nomination that an import-time
intensity window was derived from; that whole mechanism is gone — see `docs/FUTURE.md`.)

**Attribute extraction — regex + builder** (`MetadataPanel.vue` → *Extract via regex*). Pulls an
attr value out of each image's filename (or original path) with a JavaScript regex: the first
capture group `()` is used if present, else the whole match (`extractWith` in
`frontend/src/utils/regexBuilder.ts` — the single extractor, so the live preview equals the applied
result). The field's tooltip carries a brief example for people who don't know regex. There is **one** regex
input with **one** live preview (`regexSample → regexPreview` against the first target image); a
collapsible **Builder** with two modes — **Split into fields** (separator × 1st/2nd/3rd or
3rd-/2nd-last/last field × drop-extension, `buildFieldRegex`) and **Around a marker** (extract a token *preceded/followed by*
context via lookbehind/lookahead, `buildLookaroundRegex`). Each context side is a **literal text +
a class that varies** (so "M" `+ number` → `(?<=M\d+)` anchors M1b/M2a/M4f without hardcoding the
mouse number → `b`/`a`/`f`); the extract token is a class or a raw custom pattern. Both modes write
straight into that same field on any change, so it's a way to construct the visible regex, not a
second input.
The user then watches the preview and can hand-edit the pattern. The pure builder/extract logic
lives in the util (Vitest-covered); the component only wires refs.

*The **Original path** source is `oriPath` — `meta.ori_path`, the location the image was imported
from — resolved by `regexSampleFor`, never `filepath`.* `filepath` is the converted store inside the
project (`ccidImage.ome.zarr`, or `ccidDriftCorrected.ome.zarr` once a processed version is active),
which is the same uninformative name for every image; matching against it made the option useless.
The point of the path source is the **upstream folders** people organise by — `…/20260714/M1b-MERTK.ori`
→ the imaging date. That is what the from-the-end field positions and the `/ folder` separator are
for: an absolute path has a variable number of leading folders, so the containing folder is only
reachable as the *2nd-last* field. The folder separator is the **class `[/\]`** — either character
splits — because a Windows `ori_path` can be a drive letter, a UNC share, or (the browse route and
Julia's `joinpath` disagreeing) mixed. `stripExt` applies to the **last field only**, the one an
extension can be on; anywhere else it would mangle a legitimately dotted token such as a
`2026.07.16` date folder, so the *no ext* toggle is hidden for the other positions. Images with no
recorded `ori_path` fall back to the name, and the preview shows the string actually matched.

**Physical size & timing editor** (`frontend/src/components/PhysicalSizeDialog.vue`) is a modal,
not a sidebar section — the first version crammed six fields + long explanatory paragraphs into
the 280px `MetadataPanel` sidebar and was unreadable. Built on the shared `BaseModal` shell (see
*Modals & dialogs* above — no PrimeVue Dialog). Explanatory text lives in tooltips
(the header's `pi-info-circle`, per-field labels, button tooltips), not inline paragraphs.
Actions all write only the toggled fields (X/Y/Z/Δt chips — untick what's already correct so a fix
to one axis doesn't also rewrite ones that are fine): **Apply** (to the selection it was opened
with, or just the focused image if none), **Copy to selected** (the other selected images),
**Fill flagged** (only the *other* selected images that currently show a warning — the
batch-fix-from-a-known-good-reference workflow). Also reachable via an "Open editor" button in
`MetadataPanel`'s sidebar (no specific image clicked — focuses the first selected/set image)
alongside a flagged-count badge for the set. When a target already has processed versions or
segmentations, a second (informational) line via `downstreamArtifactsNote` reminds that those were
built with the current calibration and must be re-run — corrections/measurements read pixel size
from the zarr, not this dialog, so an edit doesn't reach them retroactively.

**Name-column header buttons** (`ImageTable.vue`, next to "Name"): a `pi-exclamation-triangle`
toggle to select/deselect every currently-flagged image in one click (`selectFlagged`, amber when
active, shown on every module page), and a `pi-sync` **"Resync flagged from file"** button
(`resyncFlagged` → `POST /api/images/meta/resync`), shown only on `module === 'metadata' | 'import'`
(same gating as the page-icon "open editor" button), for images that were imported *before*
physical-size/timing `meta` was tracked at all. Their OME-ZARR is already correct, so this
re-derives `meta` straight from the `"default"` (original bioformats2raw) zarr, deliberately never
whichever version is currently `active` — drift/cellpose-correct outputs carry no OME calibration
metadata at all, see docs/ARCHITECTURE.md → *OME-ZARR dual-format* — rather than asking the user to type
known-good values back in or re-import. Both header buttons operate on `flaggedUids`, not the
checkbox selection.

**Inline cell editing** (`ImageTable.vue`). Attribute cells, **channel-name** cells, and the
exclusion **note** are all click-to-edit through ONE generic core (`startEdit`/`commitEdit`/
`cancelEdit`/`focusEditInput`, keyed `${uid}:${namespacedKey}`) — each field only supplies a
`save*(val)` persister (`saveAttr` → `attr/set`, `saveChannel` → `channelnames`, `saveNote` →
`inclusion/set`). Add a new editable cell by reusing the core + a saver, never a second edit
lifecycle. Channel edits replace one index in the image's name list and re-send the whole list
(the endpoint is list-valued); a cell is editable only up to the image's channel count
(`channelEditable`). This is why the metadata panel's channel section has no "copy to all" button —
naming is done per-cell in the table (bulk-assign-to-selection via the textarea remains).

**Attribute + channel editing is Metadata-page-only.** The attr/channel columns are *shown*
read-only on every page that sets `show-attrs` (so you can see the metadata in context), but they're
only *editable* where `ModuleLayout` is given `:editable-meta="true"` — i.e. `MetadataModule`. This
keeps metadata a single place to change (no accidental edits from the segment/track/cluster pages).
The exclusion note + include/exclude toggle stay editable everywhere (excluding an image from
processing is a per-page action, not metadata).

---

## TaskRunner component

`frontend/src/tasks/TaskRunner.vue`

Fetches task definitions for a category, renders parameter forms, and submits tasks over WebSocket.
Always rendered in the `#right` slot of `ModuleLayout`.

**Right-sidebar resize** is a shared composable — `usePanelResize` (`frontend/src/composables/
usePanelResize.ts`): a left-edge drag handle, min/max clamp, and (with a `storageKey`) width
persisted to localStorage. Used by both `TaskRunner` (`cc-taskrunner-width`) and `MetadataPanel`
(`cc-metadata-width`) so the behaviour isn't reimplemented per panel — add a resizable panel by
calling it, not by copying the drag math. (MetadataPanel wraps a non-scrolling outer element around
its scrolling body so the handle stays put while the panel scrolls.)

| Prop | Type | Notes |
|------|------|-------|
| `defs` | `TaskDef[]` | From `useTaskDefs('categoryName')` |
| `module` | `string` | Passed through to task dispatch |
| `selected-uids` | `string[]` | Images the task will run on |
| `selected-names` | `string[]` | Display names matching `selectedUids` |

Task definitions are loaded once per session via `useTaskDefs`, which calls `GET /api/tasks/definitions?category=X`.

**Pool dropdown**: a `<select>` populated from `GET /api/pools`. On task switch, automatically
selects the pool matching the task def's `resource_pool` field. The chosen pool name is sent as
`poolName` in the `task:run` WS message, which `handle_task_run` in `sockets.jl` passes to
`run_task` as the `pool_name` override kwarg. The old concurrent-task slider
(`task:setLimit` / `tasksLimit`) has been removed entirely.

**The two task surfaces render through one list and one row mapper.** The `/tasks` manager
(`TasksModule.vue`) is a `SelectionTable` in `single` mode — a row IS selected, and what it selects is
what the log pane shows — so its selected-row highlight is the table's own amber `--cc-selected`
rather than the purple left rule it hand-rolled for a long time (`--cc-accent` is form-control chrome;
see `docs/todo/TASK_LIST_UNIFICATION_PLAN.md` for the dating). Both surfaces flatten `TaskEntry` to
row fields through **`utils/taskRows.ts`**, because `SelectionTable` renders `row[key]` and sorts by
row FIELDS — including the raw `elapsedMs` behind the formatted `elapsed`, since `4m 12s` sorts before
`59s` as text. Where the two lists genuinely differ they differ in their `#cell-*` slots (the manager
prefixes the image with a foreign-project label, the sidebar shows a uid chip), never in a second copy
of the derivation.

The per-module sidebar (`TaskList.vue`, also hosted by `BatchMoviesPanel` and `AnimationPanel`) is the
same table in `none` mode — a row there isn't selected, the buttons act — with two differences that
follow from the panel being ~280px wide:

- **the log expands in place**, through `#row-detail`, instead of into a side pane. `isExpanded` is
  only ever "the user opened this row": the running-task bar is its **own column**, not a second
  tenant of the detail row, so the predicate keeps meaning what it says.
- **`fit="content"` + `overflow-x`** on its own wrapper, so the columns can outgrow the panel and be
  dragged, rather than the panel being pushed wider (the containment `.task-list` needed as a card
  stack, for the same reason).

Its per-status row tint comes from `TASK_STATUS[...].tone` via `rowClass` + `:deep()` (the
`ImageTable` `.row-excluded` precedent) — that `tone` field exists precisely so a component tints its
own chrome from the same tokens as the status light, rather than the raw hexes the card stack carried.

**Task list scoping.** `useTaskStore().forModule(module, projectUid?)` and `clearFinished(module,
projectUid?)` take an optional `projectUid` — `TaskList.vue`/`TaskRunner.vue` always pass the
current project's uid so switching projects doesn't leave a previous project's (e.g. cancelled)
tasks visible in the module sidebar. `TaskEntry.projectUid` is what makes the filter possible; it's
stamped on every entry at `add()`/`addFromChainEvent()`.

The global `/tasks` manager (`TasksModule.vue`) used to omit it, as the cross-project view. It was
reported as a bug and it was one: the store is never cleared on project open (a run keeps reporting
into the tab that launched it, and `runningTasks.ts` adopts the backend's in-flight set on connect),
so after a switch the manager listed the previous project's runs with **nothing on a row to say so**
— a Smoothing run from the project you just left sitting above the training run you were watching.
It now scopes to the open project by default (`settings.tasksThisProjectOnly`, a toggle in its
toolbar), and the rule lives in `utils/taskScope.ts` because it has two exceptions worth pinning:

- a row with **no** project (`projectUid: ''`) is never hidden — a project *import* has none yet, and
  it is the job that creates one;
- an **export** names a project that is usually not the open one, dispatched from the project panel
  against any project on disk. That is a genuine cross-project row, which is why the scope is a
  toggle and not a rule — with it off, a foreign row is labelled with its project's name.

**A task frame only writes into its OWN project.** `task:result` does more than update a row: for a
task that produces a new image (`cropImage`/`copyImage` report `newImageUid` + `setUid`) it folds the
image into the project store so it appears without a reload. That was unconditional, and a task
outlives the switch that leaves it running — so one finishing in the project you just left would
`ensureSet` **its** set into the project you just opened and add its image to it. Foreign images in
the image table, indistinguishable from your own, until the next project load wiped them. The frame's
`projectUid` is now checked against the open one (`utils/taskScope.frameTargetsOpenProject`); a frame
that names no project still writes, since it is unattributable and refusing would drop legitimate
updates. `project.loadedProjectUid` records which project the loaded sets came from, and `App.vue`
logs a named mismatch if the two ever disagree again — the store previously held no clue at all, so
"the table is showing another project" had nothing to report itself with.

**Cancel all** — a `pi-times-circle` button next to "Clear finished" in the Tasks section header,
shown only when the current module+project has running/queued tasks. Cancels every one of them via
the same per-task path as the individual cancel button (`task:cancel`/`chain:cancel` over WS,
deduping so a multi-node chain run only sends one `chain:cancel`).

---

## Adding a plot or visualization panel

**First: is it a summary plot?** If the data is server-aggregated, you do not write a component at all —
drop a `app/src/plotDefinitions/<id>.json` and it appears in every "+ Plot" picker. See `docs/PLOTS.md`
→ *Hosting — ONE way*. The rest of this section is for the cases that need their own component.

**Where it goes.** Left column (`#plots` slot — `ModuleLayout` wraps it in the shared collapsible
section) for canvases that belong beside the image list; `#right` slot for a panel alongside or instead
of `TaskRunner`. Both slots hand you `setUid` / `selectedUids` / `selectedNames`, so a panel needs no
refs of its own. Reserve `#below-table` for rare extra custom content. Fetch over REST in
`onMounted`/`watch`, or subscribe with `ws.on` (see *WS events*).

Plot libraries in use — **two renderers, split by job**:
- **2D canvas** (no library) — **per-cell dot plots**: the gating scatter and the UMAP. Every point is
  drawn coloured by its LOCAL density (`plots/density.ts` `pointDensities` → the blue-heat ramp in
  `plots/flowColors.ts`) — that per-point colouring is the FlowJo/OMIQ look; contours come from
  **d3-contour** (`plots/contour.ts`). `PlotLayers` draws dots-or-contours plus population overlays and
  `GateScatterCell` composites it with `GateOverlay`. There is **no WebGL**: regl-scatterplot was
  removed (it survives only as an unused `package.json` entry). A 2D canvas suffices because the cloud
  is non-interactive, and export re-renders the same content at any scale instead of screen-grabbing a
  GPU buffer. See *Gating page* and `docs/PLOTS.md` §0.
- **Observable Plot** (`@observablehq/plot`, SVG) — **summary charts**: histogram, box/violin/beeswarm,
  bar, frequency/stacked, and (roadmap) heatmaps/tiled maps via `Plot.cell`/`Plot.raster`. Used
  wherever the data is **server-aggregated** (tiny payloads) and the ggplot `theme_classic` look /
  beeswarm / resize matter more than raw point throughput. See "Analysis-plot canvas (summary plots,
  Observable Plot)" and `docs/PLOTS.md` §0.

Why two: a per-point renderer is needed to draw every cell and sketch gates on it; an SVG
grammar-of-graphics library gives the cleaner publication look for pre-aggregated summaries. Never add
or swap a charting library without updating `docs/PLOTS.md` §0 (which owns the rationale) and this list.

### Plot loading state — delayed spinner

Heavy plots (a slow `/api/plot_data`, a big point fetch) must show they're working — a blank
panel reads as "frozen". But a spinner that flashes on every quick plot is worse noise. So the rule:
**a delayed spinner, never an immediate one.**

- `composables/useDelayedLoading.ts` — `useDelayedLoading(loadingRef, delayMs = 350)` → a `show` ref
  that flips true ONLY if loading stays true past the threshold, and clears instantly when it ends.
  Fast/cheap plots finish before 350 ms, so they never flash it; only genuinely heavy loads reveal it.
  Use `toRef(props, 'loading')` when the loading state is a prop.
- `components/plots/PlotSpinner.vue` — the shared wheel overlay. Put it inside a `position: relative`
  container: `<PlotSpinner v-if="showSpinner" label="Loading…" />`. It's `pointer-events: none`, so it
  never blocks the plot underneath, and honours `prefers-reduced-motion`.

Do **not** hand-roll per-plot "…" text or an immediate spinner. **Small/embedded plots stay out**: the
gate montage tiles (compact `GateScatterCell`, rendered by `GateMontage`) keep an unobtrusive dot, not a
wheel per tile — gate the overlay on `!compact` (or equivalent). Wired today in `SummaryPanel`, the
full-size `GateScatterCell` (Gate page), and both flow plots (via `useFlowPlanes`, which owns the
decision so the two cannot disagree); `UmapView` has its own empty-state wheel. New heavy plots: reuse
these two primitives.

**A slow plot should also say its content is STALE.** The flow plots dim the planes while a render is
queued or running (`.planes-stale`, tied to the same delayed flag as the wheel). Without it a slow
render is indistinguishable from a control that did nothing — which is how a debounced slider reads as
broken. Gate it on the delayed flag, never on raw loading, or it flickers on every drag.

**Gate scatters — one renderer, three hosts.** `components/plots/GateScatterCell.vue` is the ONE
scatter+gate body (2D-canvas dots + contour/pop-colour layer + gate overlay). The interactive Gate page
(`GatePlotPanel`, `mode` = rectangle/polygon) and every read-only montage tile (`mode="off"`) share it.
**Render modes** via the shared `RenderModeToggle.vue`: `points` (per-point pseudocolour), `contour`
(rings only — the fast path, dot pass skipped), `outliers` (rings + dots for the sparse tail, FlowJo /
old-R "contour ± outliers"). The maths is the pure, unit-tested `plots/density.ts` + `plots/contour.ts`.

Montages go through `components/plots/GateMontage.vue` — a grid of `GateScatterCell` tiles owning the
per-tile fetch (`plotmeta`/`plotdata`/`stats`), transpose reuse, optional coloured population overlays
and PNG/PDF export. Two tile producers: `GatingStrategyView` (tree-derived, responsive wrap) and
`GatePairsPanel` (a `ggpairs` matrix, `cols` set). A tile's `role` is `scatter` (fetch + render, the
default — so tree-derived defs need no role), `diagonal` (a labelled name cell, no fetch) or `corr` (an
upper-triangle Pearson-r cell reusing its mirror's points, no fetch). In matrix mode tiles get
`hideAxisLabels`, since the diagonal already names each channel. **Add a new gate-montage view by
building `PanelDef[]` and rendering `<GateMontage>` — never a second gate renderer.**

The gate scatter's axis chrome is HTML (tick labels + rotated axis names), so it doesn't inherit
Plot's `style.fontSize`. It takes an explicit **`fontSize`** prop (default 11) exposed as the
`--gate-font` CSS var and used by the tick/axis-name rules (so the vis **Font size** slider works on
the board's gating-strategy plot); `GatingStrategyView` forwards `vis.fontSize` through `GateMontage`.
Gate `%` labels (`GateOverlay.drawGateLabel`) are clamped to the plot box on **both** axes — vertical
fallback (above→below→inside) plus a horizontal clamp on the centred text — so a gate at the edge
doesn't clip the trailing `…%`.

### Generic plot-integration interface (reuse across surfaces)

A plot is defined **once** and appears on any surface — module page, **Analysis board**, and (future)
the **chain whiteboard** (`docs/SCHEDULER.md`) — via a flag. **No per-plot host wiring.** This is how you
"drop a plot onto the board" without touching `LayoutCanvas`/`ClusterPlots`.

**The contract a plot component must honour:**
- **Self-contained**: renders from a standard prop bag + persisted `state`, and **seeds its own defaults**
  (e.g. `ClusterHeatmapPanel` seeds `features` from the run — never rely on the host to seed). Persist
  every user-settable option in `state` (see "Persisting view state").
- **Standard bag**: `projectUid, setUid, imageUids, vis, state` (+ for cluster plots `popType, suffix,
  shownPops`; + panel chrome `index, active, docked, persistKey`).
- **Export hooks** for the board's PDF/CSV: `exportImage()` → a plot-only **light-theme** PNG (dark theme
  is on-screen only), and `getCsv()` → the shown data. (Interactive views may instead expose
  `exportFormats`/`exportAs`.)

**Two registries carry the surface "checkboxes":**
- `components/canvas/interactiveViews.ts` — interactive VIEWS (hosted by `InteractivePanel`), page flags
  `clusterPage` / `opticalFlowPage`, board flag `analysisBoard` + `boardGroup` (which board optgroup:
  `interactive` (default) / `clustering` / `image`), the plugin flag `pluginPage`, the `rail` the plot
  needs, an optional `initialState()` seed for a new panel's state bag, and — for a view that slices by
  population — `popTypes`, the population FAMILIES it offers (same shape as a summary spec's
  `dataSource.popTypes`, read by the same `plots/popTypes.ts` functions, so the rail lists the family
  the ACTIVE plot can draw). See `docs/ANALYSIS.md` → *The rail*.
  A view that declares `popTypes` must also OFFER the family in its own controls — `PopFamilySelect` bound
  to `usePopFamily`, which is the one resolution shared by the control and the request. A read-only copy of
  that resolution pins the panel to whichever family the registry lists first, and every population the
  user ticks under a different one is then filtered out of the request with no error anywhere
  (`docs/TRACKING.md` → *Which picker, and why not the gating tree*).
- `modules/cluster/clusterPanels.ts` — summary-family cluster PANELS (wrap `CanvasPanel`), flags
  `analysisBoard` / `trackOnly` / `needsCols`, plus a `props(ctx)` mapper so the host binds panel-specific
  props generically.

**Hosts render from the registries**: each builds its `+Plot` picker by filtering on its own flag and
renders every slot with one generic `<component :is v-bind>`. So adding a plot to a surface = write the
component to the contract + one registry line + tick the flag. The cluster page (`ClusterPlots.vue`), the
Optical Flow page (`opticalFlow/FlowPlots.vue`) and the board (`LayoutCanvas.vue`) do this identically —
there is no "cluster page way" and "board way", and a future chain-whiteboard host consumes the same
registries rather than re-wiring plots per node.

**RULE: a host names no view key.** Build the picker with `pageViews(flag)` / `boardViews(group)`; never
a local key list. The board once filtered a hardcoded `ANALYSIS_VIEWS`/`IMAGE_VIEWS` array, which made
the flag a lie — `flowModel` set `analysisBoard: true` and never appeared, with nothing failing.
`interactiveViews.test.ts` now fails if any view id shows up as a literal in `LayoutCanvas.vue`.

**`pluginPage` is the one flag a PLUGIN can tick.** A plugin names a view by its stable id in
`plugin.json` → `contributions.views`, and `SummaryCanvas` offers it under **Interactive** on that
plugin's custom module page (`docs/CUSTOM_MODULES.md`; PLUGINS_PLAN Decision 11). This is why the flag
is an opt-in rather than "any registered id": that page renders the summary canvas's own population
picker and no other rail, so a plugin-nameable view must be self-contained (`rail: 'none'`, ratcheted),
and `trackScheme` MUTATES — a manifest must not be able to request it. Ticking it makes that view's
**id** public: rename it and installed plugins get a "Plot not available here" notice instead of a plot.

**`docked` is the contract's chrome switch** — a panel reads it to hide what only makes sense
free-floating (its own Export dropdown), since the board exports via PDF/CSV instead. Details:
`docs/ANALYSIS.md` → *`docked` — the chrome switch*.

**Exception — the gating page (`gate/GatingPlots.vue`) is intentionally NOT registry-hosted.** It is a
single, *write-capable* gate-drawing workspace (`GatePlotPanel` draws/edits gates), not a multi-type
read-only plot host — the opposite of the board contract. The board hosts gating **read-only** via
`GatingStrategyView` (an interactive-registry view, `analysisBoard: true`). Don't try to fold the
gate-drawing surface into the registry.

See **`docs/ANALYSIS.md`** for the Analysis board itself (tabs, comic-plate layout, persistence keys,
the read-only cluster manager, and PDF/CSV export incl. the shared hi-res raster path).

### Auto-hide panel controls (plot fills the whole box)

`CanvasPanel` gives its **plot the whole box** and overlays the control surfaces, revealing them only on
hover (or when pinned). This is why a board plot — and its PDF export — fills its slot instead of being
squashed by a stack of dropdowns (the squashed plot exported as a clipped sliver; see `docs/ANALYSIS.md`).

- **Default ON** (`autoHide` prop, default `true`). The `#actions` (top) and `#footer` (bottom) slots
  render as absolute overlay strips over the body; a **pin** toggle (`pi-thumbtack`, next to the drag
  icon) keeps them visible. Pin/collapse are transient local refs (chrome preferences), not persisted.
- **Interactive views whose toolbar lives INSIDE the body** (`GatingStrategyView` `.gs-bar`, `UmapView`
  `.uv-ctrl` — which carries the cluster-label **and** population-legend toggles, each persisted per
  panel in `state`, `ImageStripView` `.is-bar`, `FlowMetricsView` `.fmv-ctrl`) opt in by tagging that bar `.cc-panel-controls` **and** giving
  their root `position: relative` — the global rule in `style.css` (`.panel:hover`/`.panel.controls-pinned`)
  then auto-hides it by the same trigger. One mechanism for every control surface; don't add a second.
- **Opt OUT with `:auto-hide="false"`** where you interact with the plot constantly and controls popping
  over it would fight the tools — the gate-**drawing** panels (`GatePlotPanel`/`GatePairsPanel`) do this,
  so their render-mode / gate tools stay in flow.
- **Capture safety**: a `.capturing` ancestor (set on the board grid during export) force-hides every
  `.cc-panel-controls`, so a pinned/hovered strip never leaks into a snapshot.

### Canvas zoom (fit-to-view)

Every plot canvas — the Analysis board's fixed grid AND **all** free-floating module canvases
(`SummaryCanvas`, `GatingPlots`, `ClusterPlots`) — shares one visual zoom, so a big workspace fits the
screen without hiding the sidebar. `composables/useCanvasZoom.ts` owns the `zoom` ref + `fitWidth`/`fitHeight`;
`components/canvas/CanvasZoomControl.vue` is the shared slider/fit/% control. It's a **CSS
`transform: scale`** — purely visual: it never resizes a plot's own canvas or changes what's exported
(the export re-renders at full logical resolution; the board neutralises the zoom during PDF capture).

- **Fixed-grid board**: the grid scales inside a `.lc-zoom` footprint (sized to the scaled dims so the
  viewport scrolls); auto-fits width on first render if the board would overflow.
- **Free-floating canvases**: the panels scale inside a `.sc-zoom`/`.gp-zoom`/`.cp-zoom` layer; the
  population manager sits OUTSIDE it so the control panel stays full-size. Because panels are dragged in
  screen px, the host `provide()`s the zoom under `CANVAS_ZOOM_KEY` and `CanvasPanel` injects it into
  `useFloatingPanel`, which divides drag deltas by the zoom (else a panel moves `zoom`× too fast).
  - **Workspace grows on zoom-out** (`composables/useCanvasWorkspace.ts`): the zoom layer is sized to
    `viewport / min(zoom, 1)`, so zooming OUT enlarges the *logical* workspace (Tile spreads into it, a
    panel can be dragged across it) instead of shrinking everything into the top-left and wasting the
    page — the layer is the panels' `offsetParent`, so `useFloatingPanel`'s clamp and `useCanvasPanels`'
    `arrangeGrid` both use the enlarged size. At ≥ 100% it stays viewport-sized (zoom-in inspects). "Fit"
    fits the actual plot bounding box (`useCanvasPanels.contentBounds`), not the zoom-dependent workspace.

**Zoom shortcuts** (all canvases, wired once in `useCanvasZoom`): **shift + mouse-wheel** over the canvas
zooms; **shift +/-** steps; **shift + 0** resets. Keys are ignored while typing in an input.

### Arrange + close: one button group, every canvas

`components/canvas/CanvasArrangeButtons.vue` is the toolbar group every free-floating plot canvas
renders — **Tile · Cascade · Close all** — emitting to `useCanvasPanels`' `arrangeGrid` /
`arrangeCascade` / `removeAll`. The Tile/Cascade pair had been copied verbatim into all four hosts
(`SummaryCanvas`, `GatingPlots`, `ClusterPlots`, `FlowPlots`); the rest of each toolbar legitimately
differs, so only this group is shared. A host that drives `useCanvasPanels` but renders its own
arrange buttons fails the *every canvas host offers Close all* testset — the point being that a bulk
close a user asks for on one page has to appear on all of them.

**Close all arms first.** It is destructive and unrecoverable (panel state + persisted geometry), so
it goes through `ConfirmButton` like `TabbedCanvas`'s "Close board" — including `needs-confirm` being
false when the canvas is already empty, since a confirmation protecting nothing is just a click.
`removeAll` is scoped to the canvas's **current key**, so other images'/segmentations' canvases keep
their panels, and it drops each panel's persisted geometry exactly as `remove` does. A host that
wraps `remove` with extra per-panel cleanup must wrap `removeAll` too — `SummaryCanvas` does, to clear
the id-keyed `readouts` map.

### Show/hide the population manager

The floating manager (`PopulationManager` on gate/tracking + cluster pages, `SeriesPicker` on summary
pages, `FlowModelVault` on the optical-flow page) has a **toggle** next to the arrange-windows icons on
**every** module canvas that has one (`SummaryCanvas`, `GatingPlots`, `ClusterPlots`, `FlowPlots`),
persisted per canvas in the `shared` bag (`shared.showManager`, default shown). Wrap the manager
`v-if="showManager"`. One key name across all four — the flow canvas called it `showVault` until the
rail work showed that was the same switch under a second name. The icon names the CONTENTS
(`pi-sitemap` for populations, `pi-database` for the model vault).

The **Analysis board** has no such toggle: its rail is always shown and swaps by the active slot's
`rail` (`docs/ANALYSIS.md` → *The rail*).

---

## WS events — frontend side

Subscribe in `onMounted`, unsubscribe in `onUnmounted`:

```ts
import { ws } from '../ws'

onMounted(() => {
  ws.on('napari:event:mySignal', (data) => { ... })
})
onUnmounted(() => {
  ws.off('napari:event:mySignal')
})
```

For task results, the `task:result` message updates `img.filepaths[valueName]` in the Pinia project store automatically (handled in `ws.ts`). Panels that need to re-fetch when a task changes data should use **`useDataRefresh`** (see *Data freshness* below), not a hand-rolled watch.

Full WS message-type reference is in `ARCHITECTURE.md`.

### A dropped terminal frame is recovered, not tolerated

**Every frame reaches listeners through one function — `dispatch(data)` in `stores/ws.ts`.** `onmessage`
just parses and calls it. That matters because a task's terminal frame (`task:status` done/failed, or
`chain:node:done`/`failed` for a whiteboard node) is the ONE frame carrying its outcome, and the server
drops frames for a slow client **by design** (per-client drop-on-full queue — `docs/API.md`). Lose it and
the store pinned the task at `running` forever *and* silently skipped everything hanging off completion:
the image status, `bumpDataVersion` (so plots never auto-refresh), `refreshImageMeta`, the napari reload,
the observer's completion watch. Five listeners, one missing frame.

So while this tab has work in flight, the ws store polls `GET /api/tasks/recent` (the rail's banked terminal
frames — every producer, jobs and batch movies included) and **re-emits the frame that went missing**
through that same `dispatch`. The reconstruction lives in `utils/taskReconcile.ts`. Rules worth keeping:

- **Rebuild the carrier the socket would have used**, not a stand-in: a chain run emits no `task:status`
  at all, so a chain node is recovered as `chain:node:*`. Swapping carriers would be a behaviour change.
- **A chain row is keyed by a synthetic `runId::nodeId::imageUid`**, so matching goes through
  `backendTaskId` (the `taskId` the chain frames carry) while addressing uses the store id.
- **Only act on an outcome the server can NAME.** A task that vanished without one (the backend restarted
  under us) is left alone — never guess a completion.
- **A late real frame for a recovered task is swallowed** (`recovered` set, keyed by scheduler task id):
  re-running the side effects would refetch plots, reload napari, and double-count an observer attempt.
- Adding a new completion listener needs none of this — subscribe with `ws.on` as usual and a recovered
  frame reaches you like any other. Do **not** add a second poller; `taskReconcile.ts` owns
  `/api/tasks/recent` and `utils/runningTasks.ts` owns `/api/tasks`.

### …and work that started before this tab did is adopted, not ignored

The other half of the same problem. The `tasks` store is built purely from WS events **this tab**
received, and nothing ever asked what was already running — so a page reload mid-run (or a second tab, or
the app opened on another machine) showed an **empty** task list while the backend segmented 20 images,
and each terminal frame then landed on a row that didn't exist (`setStatus` matches by id and returns
early). The tasks never appeared, not even as they finished — while the plots refreshed anyway, because
`bumpDataVersion` keys off the frame's `imageUid` rather than a row.

So on every (re)connect — and again when a project loads, since the socket usually opens first — the ws
store fetches `GET /api/tasks` and adopts the in-flight set (`adoptableTasks` in `utils/runningTasks.ts`,
the module that already owns that endpoint). Adopted rows show a true elapsed (the snapshot carries
`started_at`), take live progress/log frames from then on, and **can be cancelled** — `task:cancel` goes
by the scheduler's own id.

- **They support Re-run, because the snapshot carries the params the run was submitted with**
  (`list_tasks()` → `GET /api/tasks`). `rerun()` sends `params`, so without them the button would
  silently relaunch the task with the JSON spec's defaults — which is why the row is only offered Re-run
  once they are known. A snapshot that carries none (a backend predating the field, or a param set that
  can't be written as JSON — the route publishes `null` rather than a partial one) sets `paramsUnknown`
  on the row, which withholds the button. **No badge marks that**: it needs a backend older than the
  field to happen at all, and a permanent icon on every adopted row to explain a case nobody meets is
  the kind of standing UI noise [`docs/ui/COPY.md`](ui/COPY.md) exists to prevent.
- **One predicate decides it — `canRerunTask` (`utils/taskRerun.ts`).** Both surfaces that draw the
  button (the per-module `TaskList`, the `/tasks` manager) call it. They had their own copies and had
  already drifted: the manager offered Re-run on a **chain node**, whose `params` are `{}` because chain
  rows are built from `chain:node:*` frames — so the click relaunched the node standalone on defaults.
- **The log backfills from disk on first open** (`utils/taskLogBackfill.ts`). The scheduler tees every line
  to `{img._dir}/logs/{fun_name}.log`, so the output from before this tab connected is not lost — but that
  file is CUMULATIVE (one per image+fun, appended by every run), so the fetch passes the task's
  `started_at` as `since` and the server slices it. Slicing is server-side because the file's stamps are
  local time and the server is the process whose clock wrote them (`_tasklog_since`). Fetched lazily, on
  the click that opens the log — twenty adopted rows must not fire twenty requests for output nobody
  asked to see. No `started_at` (a queued task, an older backend) → no fetch, because the unsliced file
  would show a previous run's output as this row's.
  **Gated on `logSynced`, never on "the log looks empty".** The empty test was the original gate and it
  silently stopped working: a backend restart adopts rows that are *already producing output*, live lines
  land within the second, and by the time the user clicked the row it was non-empty — so opening a
  two-hour run after a restart showed only its last few minutes. A still-running row re-syncs on each
  open (the file has grown since), and the log header carries a **Reload log from disk** button for any
  row with a start, so a run *this tab* launched and then lost the backend under can also be read whole.
- **A known row's STATUS is reconciled even though the row itself is skipped** (`staleInFlightStatuses`).
  Adoption skips a row the tab already has because its entry is richer — but richer is not current: a task
  admitted to a pool slot while the socket was down never delivered its `queued → running` frame, and
  nothing else revisits it, so the row sat at Queued for the rest of the run (and the image table read
  "some active, some queued" with nothing actually queued). Deliberately **one** transition,
  `queued → running`: an ended row belongs to the outcome poll and a cancelled one is sticky by the
  user's choice, so repairing only the transition with no other owner keeps this from becoming a second,
  competing source of truth.
- **Chain nodes are adopted under the key their own frames use** (`runId::nodeId::imageUid`), so the next
  `chain:node:*` frame updates that row instead of adding a second one — which is why `list_tasks()`
  reports `chain_node_id`. A node with no node id is skipped: a **set-scope** node bypasses `run_task`, so
  it has no record at all. (The chain *board* recovers a reloaded run separately and more completely, from
  the run's own persisted state via `/api/chains/run` — it has every node, not just the in-flight ones.
  This is only the task list's copy.)
- **Also skipped**: a row this tab already tracks (its own entry is richer — matched on the scheduler id,
  which for a chain row lives on `backendTaskId`), an image the loaded project doesn't have (the snapshot
  carries no `projectUid`, so it may be another project's work), and anything not `queued`/`running`.
- **It does NOT copy the console's retire-on-miss rule.** `api/task_console.jl` drops a row that vanishes
  from the snapshot and tallies it "ended", because it may never see the terminal frame; the browser has
  the outcome poll above and recovers the *real* outcome instead of guessing.
- `runningTaskCount()` still counts the **whole** snapshot, including the rows adoption drops — "is the
  backend busy?" is a different question from "what can this tab show?", and a quit must warn about a
  chain node mid-write.

### Task elapsed time — the backend's timestamps, one formatter, one clock

**A task's start and end come from the backend, not from when this tab received a frame.** `task:status`
and `chain:node:*` carry `startedAt`/`finishedAt`, and the recovered frames carry the outcome row's
(`docs/API.md` → *Elapsed time is served, not guessed*). `stores/ws.ts` parses them with `parseRailTime`
and passes them to `tasks.setStatus(id, status, { startedAt, finishedAt })`, which **prefers them over
stamping `new Date()`** — a recovered terminal frame arrives seconds or minutes late, so stamping arrival
inflated every recovered task's duration by the poll delay. `new Date()` remains the fallback for a
producer whose start the backend never noted.

Two shared pieces, and a new elapsed counter must use both rather than hand-rolling a fourth copy (there
were three, and they had already drifted in what they printed):

| Need | Use |
|---|---|
| parse / format / compute an elapsed | `utils/taskElapsed.ts` — `parseRailTime`, `formatTaskDuration`, `taskElapsed(startedAt, finishedAt, now)` |
| a reactive `now` that ticks | `composables/useNowTick()` — ONE shared 1s interval, reference-counted, released with the component scope |

`useNowTick` exists because each counter owning a `setInterval` means N timers on N phases (two counters
on screen disagreeing by up to a second) and N chances to leak one. The counting logic lives in
`utils/nowTick.ts` so it is testable without mounting a component. Consumers today: `tasks/TaskList.vue`,
`modules/TasksModule.vue`, `components/ChainLiveNode.vue`.

Known gap: the `tasks` store is built from WS events only, so a tab opened mid-run has no row for work
already in flight — nothing to time. Rebuilding rows from `GET /api/tasks` would be a separate change.

---

## Data freshness — task-refresh (no per-plot reload buttons)

A task can rewrite data **in place** (same `value_name` / clustering `suffix`), so `img.filepaths`
doesn't change and a plot keyed on it never re-fetches. Rather than give every plot a manual reload
button, plots auto-refresh off a **targeted, per-image version signal**:

- `stores/project.ts` holds `dataVersion: Record<imageUid, number>`. On a successful task (`ws.ts`,
  `task:status == 'done'`) it bumps the touched image(s) — `bumpDataVersion(uid)`. A **set/combined**
  task reports all its members in the status message's `imageUids` (the backend sends the member list,
  not just the representative — see `api/src/sockets.jl`), so every member is bumped.
- Plots subscribe with the **one primitive**, `composables/useDataRefresh.ts`:
  ```ts
  useDataRefresh(() => props.imageUids, load)   // refetch only when a task touches one of THESE images
  ```
  It watches `project.dataVersionFor(theirImages)` and calls the reload fn only when an image *that plot
  shows* changed — never on unrelated tasks. Used by `useSummaryData`, `UmapView`, the cluster panels
  (heatmap / HMM) and `GatingStrategyView`. **Do not** re-import the store and hand-weave a `dataVersion`
  watch in a new plot — call `useDataRefresh`.
- Gated by the global **`autoRefreshOnTask`** setting (Settings → Interface, on by default). Because
  `useDataRefresh` is the single chokepoint, that one toggle governs every plot; off → plots refresh on
  the next navigation / input change instead.

This mirrors the older gate path (`gating:popmap` → `reloadToken`) and the old R app's success-time
`retrieveState`. The **napari viewer** refresh is a separate, data-vs-image path — see `docs/NAPARI.md`.

---

## AppSidebar

`frontend/src/components/AppSidebar.vue`

All nav group headings are collapsible buttons. Clicking a heading toggles the group open/closed;
a chevron icon (`pi-chevron-down` / `pi-chevron-right`) reflects the current state.

The **napari viewer controls** are NOT in the sidebar — the sidebar only carries the button that
toggles them. They live in a `FloatingPanel` mounted in `App.vue`; see *Floating panels* above and
*ViewerPanel component* below.

### The nav catalogue lives outside the SFC

`frontend/src/lib/navGroups.ts` holds `NAV_GROUPS` (the static groups, in pipeline order),
`customNavGroup(categories)` and `allNavGroups(categories)`. **Add a page there, not in the SFC** —
three surfaces read the same list and must agree: the sidebar renders it, the view-profile editor
offers it (you can only curate pages that exist), and the guide picker checks a guide's pages against
it. Route paths must match `frontend/src/main.ts` — **pinned by `lib/navGroups.test.ts`**, which reads
the router's route table as source and fails both ways: a catalogue path the router cannot route, and a
routed page missing from the menu without an entry in that test's stated-exception list.

### View profiles — a curated sidebar

A **view profile** is a named, ordered SUBSET of the nav catalogue, so someone doing narrow work
(gating + behaviour on already-segmented data) isn't navigating 20 items. Definitions are drop-in
files (`<config_dir>/profiles/<id>.json`, served by `GET /api/profiles`); the *selection* is per user
(`settings.viewProfile`, `cc.viewProfile`). Built in the GUI — Settings → Interface → **View profile** is a
`ChipSelect` of the profiles plus "All pages", and **Edit** opens `ViewProfileEditor.vue`: one
reorderable `ChipSelect` per sidebar group, where the selection is the pages and the chip order is
their order, with `selectAll` for all/none and `ConfirmDeleteButton` for delete.

- Filtering is pure and tested: `utils/viewProfiles.ts` (`applyProfile`, `unknownPaths`,
  `hiddenGuideRoutes`). The sidebar renders `shownGroups`, never `allGroups`.
- **It is decluttering, NOT access control.** A hidden page still opens by URL, and no route guard
  consults a profile. Never treat a profile as a permission.
- The active profile shows as a badge under the project name in the sidebar, **only when it is not
  "All pages"** — a badge for the default state is noise forever. It is its own row below `.proj-info`,
  never a second line inside it: that row centres the folder icon and the ⋯ button against the name, so
  growing it moves all three.
- **`/` is a neutral welcome page** (`modules/WelcomeModule.vue`, a greyed brand watermark), NOT a
  redirect. A `redirect` on the `/` record resolves before any guard — i.e. before the profile list has
  arrived — so a profile-derived landing page bounced on a cold boot. No page is "the start".
- A guide whose steps visit hidden pages gets one **derived prereq** in the picker
  (`stores/guide.ts` → `profilePrereq`) — an amber "needs pages your view profile hides (…)" line,
  counted in "N missing", with **Start still working**. Derived from the guide's own `steps`, so a new
  guide is covered without declaring anything.
- A listed path the app no longer has is dropped from the menu and named in Settings — a profile that
  quietly shrinks gives the user nothing to act on.

Full design: `docs/todo/VIEW_PROFILES_PLAN.md`.

### Nav item reference

```ts
interface NavItem {
  to:               string      // Vue Router path
  label:            string      // sidebar label
  icon:             string      // PrimeIcons class e.g. 'pi-th-large'
  tip:              string      // tooltip text (required)
  disabled?:        boolean     // grey out the link entirely
  soon?:            boolean     // adds a "soon" badge
  requiresProject?: boolean     // grey + lock when no project open
}
```

Icons: browse at https://primevue.org/icons — use the `pi-*` name, prefix with `pi` in the class list: `['pi', item.icon]`.

---

## ViewerPanel component

`frontend/src/components/ViewerPanel.vue`

Shows the current napari image and switches between versions (value names). Mounted in `App.vue`
inside a `FloatingPanel` (`storage-key="viewer"`), toggled by the sidebar's "Viewer controls" button
(`settings.viewerPanelOpen`, persisted) — it was a sidebar group once and outgrew the 190px nav.

**State**: image name, `valueName` dropdown (options from `img.filepaths` keys in the project
store). Changing `valueName` auto-opens the image in Napari via the REST `/api/napari/open`
endpoint.

**Auto-refresh**: subscribes to `task:status` WS events in `onMounted`; when a task transitions
to `"done"` the viewer refreshes its image data so newly written versions appear immediately.

**Populations sub-menu** (per-pop-type point toggles, after a `.opt-sep` divider): one icon per
CELL-grained pop type — `flow` (`pi-chart-scatter`) and `clust` (`pi-palette`) — each showing that
pop type's populations as coloured cell-centroid Points in napari. **Icons match the sidebar module
nav** (Gate/Cluster-cells/Track/Cluster-tracks) so a pop type reads the same everywhere. POSTs
`/api/napari/show-populations` with `popType` + `show` and **blank valueName → the server resolves
the ACTIVE segmentation** (the one gating/clustering write to; sending `labelNames[0]` was a bug —
the first label set isn't necessarily active, so clust pops never resolved). The bridge namespaces
layers by `(popType)` so flow and clust coexist.
State is per-pop-type and **remembered** (`settings.popVisible`/`setPopVisible`, keyed by pop type;
default off), auto-applied on image open, and **re-pushed on every `gating:popmap`** for the changed
pop type so the overlay tracks edits live. Only cell-grained types are here: `track`/`trackclust`
are track-grained (membership is track_ids, not cell labels) so points would be wrong — their viz is
**ribbons** via `show-tracks` (two more toggles: `pi-directions` = gated track pops, `pi-sitemap` =
trackclust cluster pops; both route through `pushTracks`, which sends `showGatedTracks` +
`showTrackclust` in one call, and the bridge namespaces Tracks layers by `(popType)`). Per-pop
visibility and the dot-size slider live in the population manager — see the gating section.
(`docs/NAPARI.md` — linked brushing.) **Icon convention**: append new toggles at the end of the
row; group unrelated toggles behind an `.opt-sep` divider.

---

## Task definition fields — resource_pool

The TypeScript type `TaskDef` has `resource_pool?: string` (optional string). Every task JSON
in `app/src/tasks/<category>/<name>.json` should include this field:

```json
{ "resource_pool": "cpu" }     // general CPU compute — most tasks
{ "resource_pool": "gpu" }     // the GPU — cellpose family (limit 1)
{ "resource_pool": "io" }      // local disk — import/convert/crop
{ "resource_pool": "network" } // remote/SMB — reserved for HPC, unused today
```

The `tasksLimit` field and the concurrent-task slider have been removed. `TaskDef` no longer
has a `tasksLimit` field. The pool dropdown in `TaskRunner.vue` reads `resource_pool` and
pre-selects the matching pool from `/api/pools`.

---

## Chain whiteboard

Route `/chain` → `frontend/src/modules/ChainModule.vue`.

The whiteboard is the visual authoring tool for chain templates. It reads and writes the same `chains/<name>.json` format that `run_chain` and `save_chain_template!` use from the REPL — one format, **three** authoring paths: the whiteboard, the REPL, and Claude via the MCP `create_chain`. The whiteboard overwrites (`POST /api/chains/save`, the user saving their own canvas) while the MCP route is create-only, but **both are validated** — save used to write verbatim, and an unwired start dot went through it unnoticed (`docs/API.md` → `/api/chains/save`). **Nothing but the whiteboard can start a run.** See `docs/SCHEDULER.md` → *Who may author a template, and who may run one*.

The action row is **two `.cc-btn-group` strips** — chain-file actions (New / Rename / Delete) and canvas actions (Start dot / Tidy / Reload / Save). Seven free-floating icon buttons did not fit the 190px palette, and the grouping says which belong together. Two things made the last button in each strip sit visibly apart: Save carried `cc-btn-dense-save`, a class that exists nowhere (so it missed `.cc-btn-icon.cc-btn-dense`'s 1.25rem square and rendered full size), and `ConfirmDeleteButton` is the one group member that is not a `.cc-btn` — it keeps `.cc-del` because it owns its armed/danger states, and on its own that brings a radius, a transparent border and no background. `style.css` now composes `.cc-btn-group > button.cc-del` into the strip (element+class, so it outspecifies the component's scoped rule whatever order the sheets land in), squaring it only when it is icon-only so a labelled delete still sizes to its text. **A new group member that is not a `.cc-btn` needs the same treatment** — the group's joining rules key off `.cc-btn`.

**Node positions and Tidy.** `positions` is whiteboard-only sidecar data, so a template authored elsewhere (the REPL, or Claude via the MCP `create_chain`) has none. `applyTemplate` then lays the DAG out via `layoutDag` (`utils/dagLayout.ts`) instead of stacking every node in one row, which hid a fan-out — and reviewing the graph is the whole safety model for an authored chain. Per-node fallback, so a partially-positioned file still places what it knows.

**The start dot is laid out WITH the graph.** It is not a template node, so `layoutDag` never sees it — and its fallback position used to be a hardcoded constant. The two were then placed by unrelated mechanisms, and with `EDITOR_GRID.originY` at 120 against the dot's y of 40 an authored chain always opened with the dot parked in the corner and a long dashed swoop to the first task. It reads as *"the start node is not connected"* — the one thing a reviewer must be able to see at a glance — and it was wrong: the edge was there all along. `startDotPosition` (`utils/startDot.ts`) now places it one depth step left of its first target, on the same row. A persisted `positions['__start__']` still wins, so a dot the user moved stays put.

**Tidy** (`pi-sitemap`) re-runs that layout on demand, via a `TeleportPopover` listing `LAYOUT_VARIANTS` — direction (left-to-right / top-to-bottom) × spacing (normal / compact), flat, so every combination is one click with no hidden state. A popover of *actions*, deliberately not a `ChipSelect` — each row fires and none persists as a selection. Compact trims the two axes **asymmetrically**: a task node is up to ~182px wide, so the flow axis has little room and most of the saving comes from the across-axis (a uniform scale factor would have overlapped horizontally). Unlike the automatic path it **overwrites** existing positions, so it stays a button the user presses; it doesn't save, so ↻ restores the previous arrangement until they hit Save. `utils/dagLayout.ts` is the ONE geometry — the Live tab's run grid uses the same `layerLanes`.

**A model this chain will train.** A `model` select is enumerated from the global vault server-side (`_inject_dynamic_options!` → `list_coastal_models`), which is right everywhere except inside a chain that trains the model it then segments with: at author time the vault has nothing to offer, so the wiring could not be expressed at all — the user picked "None" and the run failed at the segment step with *"No optical-flow model selected"*. `withChainProducedModels` (`utils/chainModelOptions.ts`) extends any select declaring `field: "models"` with what an **upstream** node produces, labelled *"(trained in this chain)"* so a name with no file behind it does not read as an available model. Appended, never replacing, and never duplicating a real vault entry. Ancestors only — a model trained later, or on a branch that has not joined, would wire a run that cannot work. The server accepts the same forward reference (`_chain_produced_names`, `docs/SCHEDULER.md`); **neither half is any use alone** — validation must accept it and the picker must offer it.

`ChainModule` is wrapped in `<KeepAlive>` in `App.vue` so navigating to other pages and back does **not** reset unsaved edits. Edits only clear on an explicit reload (↻ button) or chain switch.

### Layout — Edit tab

```
Left (190px)               Center (flex)             Right (260px, opens on click)
────────────────           ──────────────────────    ───────────────────────────
Chain selector             @vue-flow/core canvas     Node config panel
[New|Rename|Delete]        Node palette drop target  - Scope select
[Start dot|Tidy|Reload|Save]
Task palette               Background grid           - Barrier policy (set nodes)
(by category,              Nodes + edges             - Resource pool dropdown
draggable)                                             (from /api/pools)
────────────                                         - ParamRenderer for params
Run table (bottom)
- Set selector
- Image checkbox list
- Run chain button
```

The ↻ Reload button explicitly discards unsaved edits and reloads the chain from disk. Save (💾) writes the current canvas state to disk.

The **Run table** is pinned at the bottom of the palette sidebar. Select a set, check/uncheck individual images, then click "Run chain". Images default to all-selected when you switch sets. The run table auto-seeds from the first available set on project open.

The canvas uses `v-show` (not `v-if`) so VueFlow's state is preserved when switching to the Live tab and back.

### Layout — Live tab

The Live tab shows real-time status of chain nodes received via WebSocket. Each `chain:node:*`
event upserts a task in the task store (keyed `runId::nodeId::imageUid`). The Live canvas renders
these as a grid: one row per `nodeId`, one column per `imageUid`.

**Run selector**: a dropdown showing `"chainName / runId"` for each known run. Auto-switches to
the newest run when a new `chain:run:started` event arrives.

**Queued vs running**: the backend emits `chain:node:queued` when a node is submitted to its pool
and `chain:node:running` only when a worker actually starts it. A node waiting for a (e.g. GPU)
slot shows as `:queued` with no elapsed time; it flips to `:running` at the real start. With a
`gpu = 1` pool and three images, the grid shows one running and two queued — not three running.

**Elapsed timer**: `ChainLiveNode.vue` ticks elapsed time via a local `setInterval` using
`startedAt` / `finishedAt` passed as epoch milliseconds. `startedAt` is stamped on the `running`
event (real slot acquisition), so each node's elapsed reflects its own duration. A `new Date()`
call inside a Vue `computed` is not reactive and would freeze — use `setInterval` + a
`ref(Date.now())` tick instead.

**Node labels**: `ChainLiveNode.vue` shows the human-friendly `label`, resolved in `ws.ts` from the
task-defs store (`useTaskDefsStore().labelFor(fn)`) before calling `taskStore.addFromChainEvent`,
falling back to `fn.split('.').pop()` only if defs haven't loaded yet.

**Cancel**: a `chain:node:failed` event with `status === 'cancelled'` maps to a `cancelled` entry
(not `failed`). `setStatus` makes user-initiated `cancelled` sticky, so a late backend event can't
flip a cancelled task back to running/done/green.

**Resume / resume-from-here**: the Live toolbar has a **Resume** button (`resumeRun`) that re-runs
the selected run — WS `chain:run` with `runId` (no `chain`/`imageUids` needed; the backend restores
them from the run). By default it re-runs only failed / unfinished / params-changed nodes (see
`docs/SCHEDULER.md` → *Resume*). Clicking a **task node** picks it as the **start node**
(`restartNodeId`, a chain-template node id); the button then sends `startNode` too, force-re-running
that node **and everything downstream** even if `:done` — so it's obvious *where* a resume begins.
The picked node (solid accent + "resume from" badge) and its descendants (`rerunNodeIds`, dashed
accent) are highlighted; a ✕ clears the pick. Resume is disabled while the run is busy (`resumeBusy`
— any node running/queued). A resumed run **merges** live status over the persisted snapshot
(`selectedRunTasks`), so skipped `:done` nodes stay on the graph while the re-run section updates
live, rather than the graph collapsing to only the re-run nodes.

The tab badge shows the count of currently-running nodes.

### Node types

| VueFlow type | Julia scope | Visual cue |
|---|---|---|
| `"task"` | `"image"` or `"incremental"` | Purple accent border, solid (image) or dashed (incremental) |
| `"picnic"` | `"set"` | Amber/orange border, ◆ badge, barrier policy shown |
| `"start"` | (not a task) | UML initial node — a filled dot; drag + link to the first task(s). Moveable, source-only |
| `"live"` | (live view only) | Status-colored header bar; grey=queued, blue=running, green=done, red=failed, grey=cancelled |

Custom node components: `ChainTaskNode.vue`, `ChainStartNode.vue`, `ChainPicnicNode.vue`, `ChainLiveNode.vue`.

**Start dot (UML initial node).** A moveable dot (reserved id `__start__`, one per chain) marking where a
run begins — added by the toolbar button and **by default on a new chain** (which then centers/zooms on
it so it's obviously visible). You link it to the task(s) a run should start from; **only tasks reachable
from it run**, the rest stay in the editor as drafts (backend `_prune_to_start`, `docs/SCHEDULER.md`). So
drop it mid-chain to run just the later tasks, or link it to one branch and leave another as a draft. It's
not a task: excluded from `nodes` on save and recorded as `startTargets` (the linked node ids); its
position persists under `positions['__start__']`. No start dot / unlinked ⇒ `startTargets` empty ⇒ run the
whole chain (backward-compatible). The config panel shows only a hint for it (no scope/params).

### Chain JSON format

The whiteboard sends the standard `{name, nodes[], edges[]}` template format plus optional `positions:
{nodeId: {x, y}}` and `startTargets: string[]` (the UML start-dot links) fields. The backend preserves all
fields verbatim (the scheduler ignores unknown fields when loading). Positions are purely a whiteboard
concern; `startTargets` drives which subgraph a run executes (`_prune_to_start`).

### Per-node param form

`ParamRenderer.vue` is the shared param-rendering component (the "DynamicWidget" referenced in design docs). The whiteboard config panel uses it directly — the same component used in `TaskRunner`. Don't build a second param-form implementation for the whiteboard.

### API endpoints

| Method | Path | Purpose |
|---|---|---|
| `GET` | `/api/chains?projectUid=X` | List template names |
| `GET` | `/api/chains/get?projectUid=X&name=Y` | Fetch template JSON |
| `POST` | `/api/chains/save` `{projectUid, template}` | Write template JSON |

### Chain → task store bridge

Chain events flow: `_update_node_state!` (Julia) → `subscribe_chain_events!` subscriber in `server.jl` → `broadcast_ws` → `ws.ts` `chain:node:*` handler → `taskStore.addFromChainEvent(...)`.

The synthetic task ID is `runId::nodeId::imageUid` — stable across updates so the same entry is updated in place. Chain tasks appear in `TaskList` with a purple `pi-sitemap` badge. The rerun button is suppressed for chain tasks (they're driven by `run_chain`, not the task queue).

`addFromChainEvent` stores `label` from `opts.label` (which may be empty — the backend events
don't include a `label` field yet). Fallback is `fn.split('.').pop()`.

**Cancel from TaskList**: when `t.chainRunId` is set, the cancel button sends `chain:cancel {runId}`
over WS and calls `cancelChainRun(runId)` in the task store (which marks all tasks with that
`chainRunId` as `:cancelled`). Without `chainRunId`, the standard `task:cancel {taskId}` path
is used. Tooltip text adjusts: "Stop chain run" vs "Cancel task".

**Cancel status stickiness**: `stores/tasks.ts` `setStatus` guards against overwriting a
user-initiated `'cancelled'` status with any other status. Processes that don't die immediately
and finish naturally won't flip the task back to green.

### Adding a new node to the canvas

Drag from the left palette. On drop, the node is added at the drop position with default param values from the task definition. The node type defaults to `"task"` (image scope); change scope in the config panel to convert to a picnic node.

### REPL ↔ whiteboard round-trip

A chain built in the REPL with `make_chain` / `save_chain_template!` opens on the whiteboard unchanged (nodes positioned in order, left to right). A chain saved from the whiteboard runs correctly with `run_chain(proj, uids; chain="name")` — the extra `positions` field is ignored by the scheduler.

---

## Analysis-plot canvas (summary plots, Observable Plot)

The summary-plot surface — distributions/frequencies of cell & track measures — built on the shared
canvas shell (see *Shared canvas shell*). **Charting library: Observable Plot
(`@observablehq/plot`)** — chosen over Vega-Lite (jitter/resize/look walls) and Plotly (removed); see
`docs/PLOTS.md` §0 for the rationale. All plot data is **server-aggregated** (`POST /api/plot_data`
→ histogram bins / frequency counts / box stats / downsampled raw points), so Vue never receives raw
cells and payloads stay tiny — see `docs/API.md` and `docs/ARCHITECTURE.md` (layer boundary:
aggregation is a PACKAGE function, the route is thin, rendering is frontend-only).

- **`components/plots/PlotChart.vue`** — renders with Observable Plot (lazy-imported). Props: `data`
  (the `/api/plot_data` response) + `opts` (`BuildOpts`); it calls `plots/plot.ts`'s
  `buildPlotOptions(Plot, data, opts)` to get a `Plot.plot()` options object, injects the panel's
  width/height, and appends the node. Resize is trivial (no Vega signal graph): a `ResizeObserver` on
  the host just re-renders with the new size. Exposes `toImageURL('png'|'svg')` — SVG serialises the
  node (native), PNG rasterises it at the DPR-aware `EXPORT_SCALE`. The summaries counterpart of the 2D-canvas dot plots.

> **The Analysis board itself is documented in `docs/ANALYSIS.md`** — tabs, the three stores
> (`analysisTabs` / `analysisLayout` / `canvasPanels`), plate layout, persistence keys, and export. A
> stale summary lived here and had already drifted (it credited `canvasPanels` with the layout and
> called persistence a manual Save; `analysisLayout` owns the layout and autosaves). One owner: that doc.

These canvas components are **generic** (`components/canvas/`, NOT under a module) so every module
page — and the Analysis board — reuses them unchanged:
- **`components/canvas/SummaryPanel.vue`** — one summary plot, wrapping `CanvasPanel`. Layout: the
  **controls row** (`#actions`) holds a **measure dropdown** (from the spec's `measureOptions`) and a
  **chart-type dropdown** (from `chartTypes`, shown when >1); the secondary options — **Split by**
  (groupBy, discovered from obs columns) and the per-chart param (histogram → bins; bar → error metric;
  frequency → proportion) — live in a **⚙ options popover** so the bar never clips at min width. The
  **footer** (`#footer`) holds the utility actions: a **duplicate** button (clones the panel's full
  state so you can change one thing) and the **export** dropdown. Fetches `/api/plot_data`, then passes
  `result` + a `BuildOpts` to `PlotChart`. Chart
  types (by measure type): numeric → `histogram`, `boxplot` (+ beeswarm raw-point overlay that sits on
  the box by construction), `violin` (client-side KDE), `bar` (mean ± selectable SD/SEM/95% CI),
  `strip`/beeswarm; categorical → `frequency`, `stacked`, `stacked100`. An **export** dropdown saves
  the shown plot as **CSV** (the aggregated data, via `plotDataToCsv`), **PNG** or **SVG** (`PlotChart`
  exposes `toImageURL`). Visual properties come from the host via the `vis` prop (`VisProps`). See
  `docs/PLOTS.md`.
- **`components/canvas/SeriesPicker.vue`** — the summary canvas's **read-only** population picker
  (distinct from the gating `PopulationManager`, which is single-tree + mutating). Lists the
  populations available across the selected images, **grouped by segmentation** (`value_name`), from
  `GET /api/plots/populations`. Eye-selecting a population makes it a plot series; because the list
  spans segmentations, populations from **different segmentations** can be overlaid on one plot.
  Selection is keyed by `tkey(valueName, pop)` (`plots/series.ts`). A footer **global/local scope**
  toggle and an **Options** box (log scale, legend, point size/opacity — `VisProps`) both obey that
  scope: global = one value shared by every plot, local = the active plot only (mirrors the gating
  manager's plot-options model).
- **`components/canvas/CanvasSidePanel.vue`** — the **shared chrome** for a canvas SIDE PANEL, the box
  beside the plots that manages what they show: the draggable/collapsible container + top-right
  placement (`useFloatingPanel`), the header (`icon` · `title` · `count` · collapse), and two **opt-in**
  plot-only parts — the global/local **scope footer** (pass `scope`) and the `PlotOptions` block (pass
  `vis`). `SeriesPicker`, `PopulationManager` and `FlowModelVault` wrap it; the differing LIST is the
  default slot, host-specific controls (the gating manager's gate/viewer options) go in `#options`, and
  `width` sets the starting width for a table-shaped list — applied **once on mount, never as a bound
  `:style`**: CSS `resize` works by writing `style.width` on the element, so a reactive width binding
  re-applies the prop on the next render and the box snaps back mid-drag. **Resizable by the corner** (CSS `resize`,
  the same idiom as `CanvasPanel` — never a hand-rolled grip): the box clips, the LIST is the one
  flexible row and scrolls, so dragging taller shows more rows rather than more empty box; capped at
  `90vh` so a long list can't run off the canvas, and docked mode keeps the list's own `60vh` cap
  because it has no box height to fill. Size and position are **not** persisted yet (unlike
  `CanvasPanel`, which does it via `persistKey` + the `canvasPanels` geom store).
  Slotted rows keep their own component's scoped CSS; the shell owns only the chrome. **Was `PopulationPanelShell`** until the model vault showed the chrome was
  never population-specific — a manager of non-plot-series things simply omits the `vis` block (`scope`
  is passed by all three: a model is picked per plot exactly as a highlight set is).
  **Use this, not `FloatingPanel`, for anything scoped to a canvas**: `FloatingPanel` is the app's
  viewport window layer (Viewer, Lab log), so a canvas manager put there fights them for the corner.
  **CSS prefix = owning component**, because scoped styles mean a slotted row carries the CONSUMER's
  scope id and nothing in the shell can reach it: `csp-` (shell, root `.canvas-side-panel`), `pm-`
  (`PopulationManager`), `pick-` (`SeriesPicker`), `vault-` (`FlowModelVault`). All four were `pm-`
  when the shell *was* the population manager, which made four components read as one.
- **`components/canvas/canvasManager.ts`** — the **role contract** a host may rely on from any manager:
  `CanvasManagerChrome` (`scope`, `docked`) + `update:scope`, and `RailKind` — which manager a PLOT
  needs (`'pops' | 'clusterPops' | 'flowModels' | 'none'`), declared on the plot's registry entry and
  resolved by the host. The **selection is deliberately not in the contract**: the three managers
  disagree on arity and emit shape (`SeriesPicker` holds `string[]` + a 3-arg `toggle`;
  `PopulationManager` a `string` parent plus a separate `highlighted[]`; `FlowModelVault` a `string` +
  `update:selected`), so hoisting one `selected` would misdescribe two of them. See
  `docs/todo/CANVAS_MANAGER_RAIL_PLAN.md`.
- **`components/canvas/PlotOptions.vue`** — the **shared** `VisProps` styling controls (collapsible
  Layout / Points / Colours / Labels sub-sections; props `vis`, emits `update:vis`). Embedded by BOTH
  `SeriesPicker` (summary canvas) and `PopulationManager` (gating / cluster canvas), so the styling
  knobs live in ONE place. `PopulationManager` renders it only when the host passes a `vis` bag (the
  cluster canvas does; the gate canvas doesn't) — the "add plot styling to the pop manager" keyword.
  The universal Analysis board (`/analysis`) gets the same controls for free.
- **`plots/export.ts`** — the ONE plot-export module: PNG/SVG rasterise, CSV, and the true-vector SVG
  builders. Two capture paths, because they solve different problems: `elementToImageURL` wraps a
  style-inlined clone in an SVG `<foreignObject>` (catches an HTML overlay legend alongside the `<svg>`),
  while `plotHostToImageURL` composites every `<canvas>` then the overlay on top — canvas pixels can't
  go through `foreignObject`. Two DPR-aware scales: `EXPORT_SCALE` (vector) and the higher `RASTER_SCALE`,
  where every stacked canvas **re-renders its content at export scale** rather than being upscaled, so a
  dot plot exports crisp and cannot clip. Full API + the two subtleties
  that bit us (clearing ancestor backgrounds in the overlay pass; capturing the axis-margin wrapper, not
  the inner plot box) are indexed in `docs/inventory/FRONTEND.md` → *Plot export*; board figure export is `docs/ANALYSIS.md`.
- **`plots/overlays.ts`** — the **shared** themed legend / title overlays (`legendOverlay`,
  `titleOverlay`). Canvas plots render a BARE `<svg>` and float the legend/title as absolute overlays
  with the theme ink — Observable Plot's inline `legend: true` wraps the chart in a `<figure>` whose
  swatch legend sits on a white ground (light-grey text → invisible on the dark theme) and eats layout
  height (clips the axis). Used by `PlotChart` AND the cluster HMM panels; the host must be
  `position: relative` and ship the `.plot-legend-overlay` / `.plot-title-overlay` scoped CSS.
- **`components/canvas/SummaryCanvas.vue`** — the workspace (`useCanvasPanels` + `CanvasPanel` +
  `SeriesPicker`). The **"+ Plot" picker** lists plot types from the registry
  (`GET /api/plots/definitions?module=…`). **Series come from the picker's eye-selection** — each is
  a `{valueName, pop}` target, sent to `/api/plot_data` as `series:[…]`. A **"compare" selector**
  (shown when a set is active **and >1 image is selected**) switches the **data source**: *this
  image* / *per image* (one series per selected image) / *pooled* — orthogonal to the chart type, so
  any chart works with any scope. Per-image series are coloured by image (stable palette); else by
  population colour. Has its own populations fetch + selection state (not the gating store), but
  **subscribes to `gating:popmap`** so gate edits (gate page, napari, other clients) live-refresh the
  population list and re-pull the panels' data. Series are keyed by every varying dimension (image ·
  segmentation · pop) so populations sharing a path across segmentations get **separate** boxes/bars
  (no overlap).
- **`modules/BehaviourModule.vue`** — route `/behaviour`, sidebar "Behaviour". Minimal page (full
  HMM/behaviour pipeline later): `ModuleLayout module="behaviourAnalysis"` (**multi-select** —
  unlike gating's single-select — so several images can be compared) + `SummaryCanvas` below the
  table. Doubles as the clean test ground for the canvas. **Comparison plots live here, not in the
  Tracking module** (Tracking hosts the interactive track-gating canvas only).

**Data source ⊥ chart type.** The plot spec defines the **data source** (popType, granularity,
`measureOptions`) and the **chart types valid for it** (`chartTypes`); the user picks the chart type
in the panel and the data scope (single/cross-image) in the canvas. The two compose freely.

**Plot specs** live in `app/src/plotDefinitions/*.json` (one data source per file) and are served like
task defs, so adding a plot type is a JSON drop with no UI code. Schema + the current set:
`docs/PLOTS.md`.

## Gating page (2D-canvas scatter + gate overlay)

`frontend/src/modules/GatingModule.vue` — route `/gate`. Pick ONE image in the table; the
gating workspace renders **below the table** (`#plots` slot, wide left column),
mirroring the old `flowPlotManager` layout. `gate/GatingPlots.vue` is the container:
page-level **segmentation (value_name) select** + a **"+ Plot" button** + **Tile/Cascade** window-
arrange icons (ImageJ-style: grid vs staggered), and a full-height **`.gp-canvas`** workspace
(`min-height: 80vh`) holding **free-floating, draggable, resizable** `gate/GatePlotPanel.vue` boxes
plus the floating `components/canvas/PopulationManager.vue`. Arrange works by pushing an `arrange`
command (`{x,y,w,h,seq}`) to each panel — position is otherwise drag-controlled and size
resize-controlled, so the command sets both imperatively (the `seq` bump forces re-apply). Plots
are an array keyed by stable id (no fixed count); per-plot state (displayed parent, local
highlight) lives in `GatingPlots` keyed by id. State otherwise lives in `stores/gating.ts` (tree,
columns, stats, CRUD, `applyBroadcast` for the `gating:popmap` WS push; `valueName` self-heals to a
real segmentation). API: `docs/API.md` gating routes.

**Track-property gating reuses the SAME canvas (`popType` prop) — no clone.** `GatingPlots` takes a
`popType` prop (`'flow'` default | `'track'`); `TrackingModule.vue` (route `/track`) renders it in
its `#plots` slot as `<GatingPlots :image-uid pop-type="track" />` (active when exactly one
image is selected, alongside the task runner in `#right`). `popType` only changes (a) the data source
the store/API read — flow cells vs the per-track table, handled server-side (`docs/API.md` →
`popType=track`) — and (b) the napari overlay: flow shows the cell-selection brush (linked brushing)
+ Points layers, track shows a **"Tracks"** button (`g.showTracks()` → napari Tracks layers via
`POST /api/napari/show-tracks`). `GatePlotPanel` and `PopulationManager` are shared unchanged; two
small popType-driven touches: panels default the axis transform to **linear** for track (motility is
continuous, not logicle-scaled), and the manager hides its **"Napari dots"** point-size option for
track (tracks are ribbons, not points — `popType` prop). The store gained `cellMeasures` /
`trackAggregates` (track `/channels` fields, for building `{measure}.{agg}` axes) and
`showTracks` / `refreshNapari` (the latter routes the per-pop visibility re-push to Tracks vs Points
by `popType`).

## Interactive vs summary plots

Two plot families share the canvas shell; the distinction matters for where a new plot type plugs in:

- **Summary** — server-aggregated (`POST /api/plot_data`), drawn by the ONE generic `PlotChart`
  (Observable Plot). Histogram, bar, boxplot, **heatmap/matrix**, frequency. Add one = drop a
  plot-def JSON (`app/src/plotDefinitions/`); no UI code. Hosted by `SummaryPanel`.
- **Interactive** — client-side 2D-canvas point clouds with per-point interaction, each with its own
  data endpoint + rendering. Gating scatter, **UMAP**. These can't be a single generic renderer, so
  they live in a **registry** of self-contained view components:
  **`components/canvas/interactiveViews.ts`** → `INTERACTIVE_VIEWS = { umap: { label, component } }`.
  A view (e.g. **`components/plots/UmapView.vue`**) fetches + renders + owns its controls; the generic
  **`components/canvas/InteractivePanel.vue`** wraps any view in `CanvasPanel` and spreads the plot
  `context` (project/images/popType/suffix) + the panel's persisted `state` onto it. **Adding an
  interactive plot = one `XView.vue` + one registry line** — no panel/canvas changes. (Shared infra,
  so the future universal canvas reuses it.)

## Cluster pages (UMAP + heatmap on the shared canvas)

`ClusterCellsModule` (`/clust-cells`, popType `clust`) and `ClusterTracksModule` (`/clust-tracks`,
`trackclust`) — one page per granularity, mirroring the gate/track split. Each is `ModuleLayout`
(multi-select; clustering is set-scope) + `TaskRunner` + a `#plots`-slot `modules/cluster/ClusterPlots.vue`
canvas, the cluster analogue of `GatingPlots` (`useCanvasPanels` keyed `clust:${popType}` + "+ Plot" +
Tile/Cascade). The picker lists every `INTERACTIVE_VIEWS` entry plus the summary **Heatmap**, routed by
family — interactive → `InteractivePanel`, summary → `ClusterHeatmapPanel`.

UI conventions specific to these pages:

- **`suffix` is page-level** — a dropdown of the discovered `clusters.{suffix}` runs, one at a time like
  a segmentation, persisted in the canvas `shared` bag.
- **Heatmap features are exactly what the run clustered on**, read from the `{props}.clustfeatures.json`
  sidecar via `GET /api/gating/channels`; channel rows aggregate by RAW name and relabel via `nameMap`.
- **The population manager is the shared, pop_type-agnostic one.** A cluster pop has no gate — it is a
  filter on `clusters.{suffix}` — so in cluster mode the manager shows "Add population" plus per-pop
  **cluster-ID toggle chips**, and ticking a chip moves that cluster out of any other pop (a cluster
  lives in at most one). Writes mirror across the run's images (`mirrorUids`); a banner names selected
  images outside the run, with a "Select clustered images" button driving `selectUids`.
- **Highlight → overlays.** The manager's per-pop eye feeds `shownPops`: UMAP recolours from cached codes
  (no refetch), the heatmap switches its columns from clusters to populations. Scope (global/local) works
  as on the gating canvas.
- **HMM behaviour plots (track clustering only)** — `ClusterHmmStatesPanel` (100%-stacked state
  frequencies) and `ClusterHmmTransitionsPanel` (from→to dot grid). Categorical behaviour, so they are
  filtered out of the heatmap's numeric features.

The clustering model behind all of this — run membership (`partOf`), co-clustered value_names, the
per-run sidecar, set-pooled aggregation — is `docs/POPULATION.md` and `docs/todo/CLUSTERING_PLAN.md`.


---

## Shared canvas shell

Reused by the gating, track-gating, summary and universal canvases — the floating-panel mechanics are
factored out of the gating page so every module canvas reuses them unchanged:
- **`composables/useFloatingPanel.ts`** — drag-to-move + clamp-to-`offsetParent` + Tile/Cascade
  `arrange` handling for any floating panel (one implementation; was duplicated in the plot panel
  and the manager).
- **`components/canvas/CanvasPanel.vue`** — the generic panel chrome, stacked in rows: a **title row**
  (the whole row is the drag handle, like `PopulationManager`; holds title + collapse + remove —
  buttons `@mousedown.stop` so they don't drag), an optional **controls row** (`#actions` slot, which
  `flex-wrap`s so it never clips at min width), the **body** (default slot), and an optional **footer
  row** (`#footer` slot, for utility actions). `resize:both`, active border. `GatePlotPanel` and
  `SummaryPanel` wrap their content in it.
- **`composables/useCanvasPanels.ts`** — the workspace logic: the panels array (`{id, arrange,
  state}` with host-owned per-panel `state`), `add`/`remove`/`arrangeGrid`/`arrangeCascade`, the
  active panel, and a per-canvas **`shared`** bag for canvas-level options. **Takes a `key`** (e.g.
  `summary:behaviourAnalysis`, `gate:flow`); everything lives in the **`canvasPanels` store** under
  that key, so open plots **persist across navigation** (re-binds the same panels instead of starting
  empty). Cleared on project open/close. ⚠️ **Seed default panels only when the canvas is empty**
  (`if (panels.value.length === 0) add()`) — an unconditional `add()` in `onMounted` stacks duplicates
  every remount (the Gate↔Tracking 2→4→6 bug).

---

## Stores are HMR-aware — one line per store, and it is not optional

Every Pinia store ends with

```ts
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useThingStore, import.meta.hot))
```

A setup-store instance is **not** replaced when its module hot-reloads — it keeps the shape it had at
page load. So adding a field to a store and saving leaves the *component* reloaded into the version that
reads the new field, while the live store still lacks it, and the page dies on code that is correct in
the source:

```
UI error (render function): can't access property "length", $setup.customModules.clashes is undefined
```

`clashes` is `ref([])` and cannot be undefined from a cold load, which is exactly what makes this
expensive: nothing is wrong, a reload fixes it, so it reads as a ghost. `acceptHMRUpdate` must be called
in the store's OWN module (it needs that module's `import.meta.hot`), so it is per-file boilerplate and
will be forgotten — `stores/hmr.test.ts` fails when a store lacks the line, or names a different store
in it (registering A's updater in B corrupts A and leaves B stale).

Dev-only, like the proxy note below: nothing in a production build reaches either.

## The dev proxy is quiet when the backend is down

`vite.config.ts` wraps Vite's own logger (`createLogger()`) and turns `ECONNREFUSED`/`ECONNRESET` proxy
stacks into one line; the predicate is `utils/devProxyNoise.ts`, unit-tested. `pixi run dev` supervises
the backend and Settings → System Restart stops and starts it, so every session has windows where :8080
is not listening — and `TaskRunner.vue` polls `/api/runner/status` on a timer from every module page,
so the default handler printed a Node stack per poll into the same terminal the log rail writes to.
Noise on a normal action teaches you to ignore the log, and then a real proxy error goes unread.
Anything that is not those two codes still prints in full, and Vite still answers the request (502 for
`/api`, socket end for `/ws`), so a client `catch` runs when it should.

**Why the logger and not a proxy `error` listener.** The first version attached one via
`server.proxy.configure` and could not work: `proxyMiddleware` calls `opts.configure(proxy)` and only
*then* attaches its own logging listener, so ours ran first and Vite's still printed. Node calls every
listener; no ordering suppresses a sibling. It also only ever covered `http proxy error` — `/ws` has
two more spellings (`ws proxy error:`, `ws proxy socket error:`), and the socket one does not even
contain the substring `proxy error`.

## Persisting view state — the three scopes (important; read before adding any plot option)

Every user-settable option MUST live in a persisted bag, or it silently resets on remount (a plain
`ref()` in a canvas/panel component does NOT survive navigation). There are three scopes, all backed
by the `canvasPanels` store and keyed per canvas.

**The canvas key embeds the object it belongs to (module pages)** — `summary:{module}:{setUid}`,
`gate:{popType}:{imageUid}:{valueName}` (per segmentation too), `clust:{popType}:{setUid}`,
`flow:model:{imageUid}`. **A new prefix must be added to `MODULE_PREFIXES` in
`stores/canvasPanels.ts`** or the canvas works but never persists.

> **Which object — set or image?** Ask what the canvas is *about*, not which one is selected.
> **Gating is per image**: a gate belongs to one (image, value_name), so `gate:` is image-keyed.
> **Summary plots are per SET**: the plots are cross-image by design — per-image / pooled /
> by-attribute is precisely what the `compare` control chooses — so the layout must not be image-keyed
> as well. `summary:` was image-keyed until 2026-08-15, which tied the SAVED LAYOUT to whichever image
> happened to be **first in the selection**: re-ticking silently swapped your whole canvas, and ticking
> five images showed the first one's. Both halves of that were invisible. Canvases saved under the old
> per-image keys are not ported — there is no honest merge from N per-image layouts into one.
> Pinned by the *summary canvas is set-scoped, gating canvas is image-scoped* testset.
>
> The matching data-side default: `compareMode` seeds to **`per_image`**, not `image` (which means
> "the first selected image only"). `canCompare` already gates it on more than one image being
> selected, so a single-image page is unchanged.
`useCanvasPanels` takes a **reactive** key
(Ref/getter) and rebinds to that object's own entry when the selection changes — so each image keeps
its own plots/selections instead of the old single shared-per-module entry being pruned. Add
`imageUid` (or set/value_name) to a NEW canvas's key the same way. The `/analysis` board keeps its own
`analysis:{projectUid}:tab:{id}` key (persisted separately — see below).

**Persistence is per-image AND survives reload** (debounced autosave). The store groups the
module-page entries (`summary:`/`gate:`/`clust:` keys) BY OBJECT and writes each with its object at
**`{proj}/1/{objUid}/moduleCanvases.json`** (like `ccid.json`/`labelProps` — locality, and it's
removed when the object is deleted), ~400 ms after any change (off the interaction path — no
perceptible lag). The object is the image (summary/gate) or set (clust) the canvas is scoped to,
parsed from the canvas key's 3rd segment. `api_projects_load` reassembles the per-object files into
one keyed map; `projectMeta.openProject` restores it after `loadFromApi`'s clear. The board still
persists separately to `settings/analysisBoards.json` (manual Save). Nothing to wire per page.

The three scopes:

1. **Per-panel** (chart type, measure, bins, error metric, …) → the panel's own `state` object
   (`CanvasItem.state`). `SummaryPanel` receives it as the `ui` prop and reads/writes it via computed
   get/set; each field falls back to the spec default until the user changes it.
2. **Per-canvas / global-scope** (the global selection, vis props, compare mode, scope toggle;
   gating's highlight set, line width, …) → the per-canvas **`shared`** bag.
3. **Geometry** (drag position + size) → the `geom` record, keyed `${canvasKey}:${panelId}`
   (`CanvasPanel` writes it on drag/resize; restored on mount).

**The mechanism: `composables/useViewState.ts` (Shiny-`reactiveValues`-style).** Pass it the `shared`
bag `Ref` + a `defaults` literal; it seeds missing keys and returns one ref per option, so **every
option declared in `defaults` persists automatically — there is nothing to wire per-field**. The
convention is therefore forget-proof: *put every option in the `defaults` object*; that single step is
all that's needed. Do **not** introduce a bare `ref()` for a user option in a canvas component. The
returned refs track the bag's **identity**, so when the per-image key rebinds `shared` to another
image's entry, global-scope state follows the image too (no remount / per-page code needed).

```ts
const { compareMode, scope, sel: gSel, vis: gVis } = useViewState(shared, {
  compareMode: 'image' as 'image' | 'per_image' | 'summarised',
  scope: 'global' as 'global' | 'local',
  sel: [] as string[],
  vis: defaultVis() as VisProps,
})   // each is a Ref backed by the persisted bag; setting .value persists across navigation
```
Used by `SummaryCanvas` and `GatingPlots`. In-memory/session-scoped (survives in-app navigation, not
a hard browser reload — same as the panels); cleared on project open/close.
- **`components/canvas/PopulationManager.vue`** — the shared, pop_type-agnostic manager (renders
  whatever `g.popType` the store holds — flow/live/clust; not flow-only). Plot-options (gate labels,
  line width, axis) are passed in by the host canvas since they belong to the plot panels.

Each **`GatePlotPanel`** is `position:absolute`, **dragged by its title** (clamped on-screen like
the manager) and **resized from its corner** (`resize:both`; the plot area is `flex:1` and the
canvas layers re-render via `ResizeObserver`). Self-contained (own X/Y column + transform
on **stacked rows**, parent-population select, **render mode**, gate mode) with a **"−"** in the
header to remove it. New gates are added under that panel's selected parent population. Click a
panel to make it **active** (orange border); the active panel follows the population you select in
the manager (sets it as the displayed parent).

Plot stack — two superimposed 2D canvases, both mapping data→pixel through the same `viewExtents`
so they stay aligned (`xMin`→left, `xMax`→right, `yMax`→top). There is no third (WebGL) layer any more:
- **`components/plots/PlotLayers.vue`** — the base. In `points` mode it draws every cell coloured by
  its local density; in `contour`/`outliers` mode it draws d3-contour rings (plus the sparse tail).
  Also draws the **population-colour overlay** (per-pop dots or contours). Bucketing points by colour
  keeps `fillStyle` writes to ~64 rather than one per point.
- **`components/plots/GateOverlay.vue`** — canvas2D (top). **Draws** new **rectangle** (drag)
  and **polygon** (click vertices, double-click/click-near-start to close; Esc cancels) gates,
  and **edits** existing ones: move / resize rectangles (corner + edge handles), drag polygon
  vertices, double-click an edge to insert a vertex, right-click a vertex to delete. Live local
  redraw while dragging; persists (`pop/set-gate`) only on release. Emits `draw`/`edit` only on
  explicit user completion — programmatic repaint never emits, so no re-entrancy loop (the old
  Plotly `flowNumGateUpdates` guard is unneeded).

**Render modes** (mirror old `cciaConf fcs.gating.plotTypes`): `points` = FlowJo *pseudocolour*
(density-coloured points); `contour` = density contours over faint points. Highlighting
populations in their colours (the **eye** in the manager) overlays on top of either mode.

Workflow: pick X/Y columns + per-axis transform (linear/log/asinh/logicle) → click a parent
population in the manager (sets it as the active plot's parent) → draw a gate → name it → it's
POSTed (`pop/add`), recomputed server-side, and appears in the manager with count + %-of-parent.
Edit a gate by dragging its handles → `pop/set-gate` on release. The manager (draggable,
clamped on-screen, collapsible) does recolour (`pop/update`), inline rename (`pop/rename`),
delete (`pop/delete`, cascades), and per-plot colour **highlight** (see below).

### Gating plot — rendering & UX hacks

Moved to **`docs/POPULATION.md`** → *Gating plot — rendering & UX hacks*: the client-side density and
contour maths, gate hit-testing without stealing pointer events, and cross-plot propagation. Those are
gating-model internals rather than UI conventions. **Read them before touching `PlotLayers` /
`GateOverlay`.**

## Auto-overridden settings — never silent

When the app cannot honour a chosen option and substitutes another, it says so. Silently substituting
leaves the user looking at a plot that disagrees with its own controls, with no way to tell whether the
setting is broken or the data made it impossible.

One mechanism — `frontend/src/plots/autoOverride.ts`:

1. Build an `AutoOverride` (`{ setting, from, to, why }`) **where the substitution is decided** — nothing
   downstream knows the reason.
2. Mark the affected control with the shared **`.cc-auto-override`** utility (amber, `style.css`).
3. Use **`overrideTooltip(o, fallback)`** for its hover text, so the explanation can't be left out. Where
   there is no single control to mark, `overrideNote([...])` gives a one-line footer for the panel.

Today's overrides:

| Where | Substitution | Decided by |
|---|---|---|
| Gate plot / gate pairs | axis transform → `linear` when the measure's range can't take logicle | the server (`plotmeta` reports the transform it USED) |
| Any summary plot | x tick labels → rotated when they wouldn't fit their bands | `needsXRotation` (measured label widths vs the panel width) |
| Any summary plot | `Facet by` → `None` on a chart that composites its series into one frame (histogram / frequency family / heatmap) | `NON_FACETING_CHARTS` in `plots/plot.ts` → `_facetIgnored` |

This replaced two ad-hoc copies. `GatePlotPanel` and `GatePairsPanel` each did their own
preferred-vs-used comparison with their own amber class and their own wording — and `GatePlotPanel`'s
transform select was tooltipped just "Axis transform", so the amber announced that *something* had
happened without ever saying what. A third case (auto-rotation) was the point at which a third variant
stopped being acceptable.

### Notices about a render — `PlotNotice`

An auto-override is one kind of message; the other is a **caution or observation about what was just
drawn** — it will be heavy, or it came back partly empty. That is not an override (nothing was
substituted) and not an error (no request failed), and it had three shapes before it had one:
`GatePairsPanel`'s tinted `.pairs-warn` banner, `SummaryPanel`'s `.sp-foot-note` chip (amber with a
triangle for overrides, muted and icon-less for empty series — the two disagreed on whether a notice
has an icon), and the Flow views' bare `.cc-muted-warn` paragraphs.

**`components/canvas/PlotNotice.vue` is the one affordance.** Two variants, because the placements are
genuinely different: `chip` sits inline in the panel's chrome row, `banner` is a full-width tinted bar
above the plot for something you should see *before* waiting for the render. Tone is `warn` (amber +
triangle) or `muted`. The text says WHAT, the tooltip says what to DO — the same split
`overrideTooltip` uses. Add a notice here rather than a fourth span. (The Flow views' `error`
paragraphs are deliberately left alone: a failed request is a different thing.)

**The predicates are pure and live in `plots/renderLoad.ts`** (`facetLoad`, `explodeLoad`, with their
thresholds as named exports; the pairs matrix keeps `estimateMatrixLoad` beside its own geometry).
A load predicate **never blocks** — it feeds a notice and the user decides. A threshold is a guess
about someone else's screen and data, and silently refusing to draw what was asked for is worse than
drawing something slow.

Today's notices: **faceting into more than `FACET_PANELS_HEAVY` panels** (each becomes a sliver, which
defeats the comparison faceting is for), and **"Show series" about to add more than
`EXPLODE_PLOTS_HEAVY` plots** (unlike facet panels these are real canvas plots that each fetch their
own data and persist until closed).

**A marked control SHOWS the effective value and WRITES the preference** (`effectiveOf`). This is the
half that's easy to miss: an ambered control still displaying the value that was *not* used reads as
"your setting is being ignored". The gating transform selects have always done it — the select's getter
reads the transform the server USED, its setter writes the user's preference — and the rotate toggle sat
at *off* beside a rotated plot until it did the same. The control is then effectively stuck while the
override holds, which is correct (the plot really is rotated) and lifts on its own when the cause does:
a wider panel, shorter labels, a compatible measure. In **Global** vis scope the amber reflects the
ACTIVE plot while the toggle governs every plot — same convention as the stats-test readout beside it.

**Mark the CONTROL, not just the plot.** The notice reaches two places, and both matter: the affected
control in the population picker goes amber with `overrideTooltip` (so the toggle never sits at *off*
beside a rotated plot), and the panel shows a short footer note. Both read the same
`PlotReadout` — `{ stats, overrides }`, threaded as **one object** through
`SummaryPanel → host → SeriesPicker → CanvasSidePanel → PlotOptions`. Parallel props are how the
first attempt failed: the override was emitted and the toggle never heard about it.

**A panel notice belongs in the panel CHROME.** `.sp-body` is `overflow: hidden` with a `height: 100%`
chart in it, so a sibling rendered after the chart is pushed out of view — the first version of these
notes was emitted correctly and simply never seen. Put them in the `#footer` slot.

**Auto-rotation is a decision, not a guess.** Each of `n` categories gets an equal band of the plotting
area, so a label wider than its band must collide with its neighbour; `needsXRotation` measures the
widest label with the same canvas text metric the axis margins use. It needs the panel width, which the
option builders don't have — `PlotChart` passes `plotWidth` (the same value it hands `Plot.plot`), and
the builder reports the outcome back on `_autoRotatedX` → `@auto-override` → the panel's note.
