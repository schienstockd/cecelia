# Cecelia UI Guide

Frontend conventions, component catalog, and how to add new UI features.
The language-boundary and WS protocol are in `ARCHITECTURE.md`; this document is purely Vue/CSS.

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
| `--cc-selected` | `#ff8c1a` | Amber selection/active highlight for BOXES (panels, cards, timeline keyframes) — distinct from `--cc-accent` (form controls) |
| `--cc-warn` | `#f59e0b` | Amber warnings (e.g. heavy-load hints) |
| `--cc-danger` | `#ef4444` | Destructive / error state (delete, invalid) |
| `--cc-viewer` | `#22c55e` | Green accent for the napari viewer controls button + its floating-panel border (stands apart from purple chrome) |
| `--cc-sev-ok` | `#0ca30c` | Severity **ok** (QC/traffic-light). Colour-blind-safe status palette |
| `--cc-sev-warn` | `#fab219` | Severity **warn** (a QC warn finding) |
| `--cc-sev-fail` | `#d03b3b` | Severity **fail** (a task failed) |
| `--cc-mono` | system monospace stack | Log output, code |
| `--cc-header-h` | `40px` | Fixed header height |
| `--cc-sidebar-w` | `190px` | Fixed sidebar width |
| `--cc-runner-w` | `280px` | TaskRunner panel width |
| `--cc-console-bar-h` | `30px` | Collapsed console height |
| `--cc-console-open-h` | `210px` | Expanded console height |

### Severity (QC / traffic-light) — colour is never the only cue

`--cc-sev-ok`/`--cc-sev-warn`/`--cc-sev-fail` are the ONE canonical severity palette
(colour-blind-safe hues from the dataviz status palette). **Never render a severity as colour alone**
— always pair the hue with a shape-distinct icon + label. The canonical mapping lives in
`frontend/src/lib/severity.ts` (`SEVERITY`, `worstSeverity`, `severityFor`); the lab-log glyph
counterpart is `qc.jl` `severity_symbol` (✅/⚠️/❌ — shape-distinct, never 🟢🟡🔴). Any new severity
UI imports these; do not hand-pick a green/amber/red or a coloured dot. See
`docs/todo/QC_OBSERVER_PLAN.md`.

---

## Hard requirements

Every interactive element must carry a `v-tooltip.right="'Description'"`.
CellProfiler is the reference for tooltip density — if a button does something non-obvious, it has a tooltip.

All errors go to `useLogStore().error(msg, { source, detail })`.
Task failures must never be silent — errors must reach the console bar visible to the user.

The **console** is one component — `components/ErrorConsole.vue` over the `log` store — mounted in two
places: the docked bar at the bottom of the app shell, and (with the `fill` prop) full-window in the
standalone **console window**. Do not build a second console. The window is a `bare` route
(`/console`, `meta.bare` → `App.vue` renders it without the shell) opened via
`window.open(origin + pathname + '#/console', …)` from the docked console bar's pop-out (↗) button;
being a separate browser window it's a fresh app instance with its own WS, and it backfills recent lines from
`GET /api/logs/recent` on open. The stream includes the backend's own logs (WS `server:log`, see
`docs/API.md`), so it's a real "pixi console", not just task logs.

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

Global classes defined in `style.css` — use these everywhere instead of scoped button styles.

```html
<!-- Default ghost (for secondary actions like filter Apply/Reset) -->
<button class="cc-btn cc-btn-ghost" @click="...">Apply</button>

<!-- Primary (for the main CTA like "Add images") -->
<button class="cc-btn cc-btn-primary" @click="...">
  <i class="pi pi-plus" /> Add images
</button>
```

Both support `:disabled` (opacity 0.35, not-allowed cursor) and work with `v-tooltip`.

## Form controls

`style.css` styles **all native form controls app-wide** — bare `<select>`, `<input type="text|
number|search|…">`, `<textarea>` get the consistent surface/border/rounded look, accent
focus ring, a custom `<select>` chevron (the native arrow is hidden via `appearance:none`), and
accent-tinted `range`/`checkbox`/`radio`. **Do not re-declare background/border/border-radius/
padding/outline on inputs in component styles** — it diverges from the rest of the app (this was
the "old-school inputs look inconsistent" bug). Keep only layout in scoped styles (width, flex,
`min-width`) plus state modifiers (`.input-error`, `[readonly]`, `:disabled`, `.mono`). If a
`<select>` sets `background:` (shorthand) it will wipe the chevron — use `background-color`.

---

## Modals & dialogs — always use `BaseModal`

**Every centred modal/dialog is built on `frontend/src/components/BaseModal.vue`. Never hand-roll an
overlay (`position:fixed; inset:0`) again** — that copy-paste is how we ended up with four near-identical
shells (`ProjectPanel`, `PhysicalSizeDialog`, `FileBrowser`, …). We do **not** use PrimeVue Dialog.

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
      <button class="btn-ghost btn-sm" @click="emit('close')">Cancel</button>
      <button class="btn-primary btn-sm" @click="…">Save</button>
    </template>
  </BaseModal>
</template>
```

Host it with `v-if`: `<MyDialog v-if="show" @close="show = false" />`. Put dialog-specific CSS in the
child's scoped `<style>`; the shell (overlay/box/header/footer) is BaseModal's — don't restyle it.
Working examples: `PackagesDialog.vue` (toolbar + body), `PhysicalSizeDialog.vue` (`#title` slot +
footer), `FileBrowser.vue` (toolbar + footer). *(An in-canvas overlay like `GateOverlay` is a
different thing — that's `position:absolute` inside a plot, not a modal.)*

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
  <button v-if="!armed" class="btn-danger btn-sm" :disabled="!selected" @click="arm"
          v-tooltip.bottom="'Delete…'"><i class="pi pi-trash" /></button>
  <template v-else>
    <button class="btn-danger btn-sm" @click="confirm">Confirm</button>
    <button class="btn-ghost btn-sm" @click="cancel">Cancel</button>
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

- **Parent owns visibility** (`v-if` + `@close`); the panel owns position/size/collapsed, persisted per
  `storageKey` under `cc.floating.<storageKey>` (reopens where you left it). Drag by the header, resize
  from the bottom-right grip, collapse to header-only. Position is clamped into the viewport on mount +
  window resize so a stale/off-screen box always comes back.
- **z-index 60** — above content and the right panel, below modals/console.
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

const defs = useTaskDefs('segmentImages')
</script>

<template>
  <ModuleLayout module="segment" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner :defs="defs" module="segment"
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
| `allow-delete` | `false` | `true` for Import (per-image delete button in table) |
| `show-attrs` | `false` | `true` for modules where attr columns (treatment, genotype…) are useful |
| `show-filter` | `true` | `false` for Import and other modules where filtering doesn't apply |
| `no-set-hint` | `"Select a set…"` | Custom empty-state message |

**Slots:**

- `#actions="{ hasSet }"` — items injected into the action bar before the image count (e.g. "Add images" button). `hasSet` is `true` when a set is active — use it to disable the button.
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
The frontend never maintains a copy of task definitions — they're fetched from `/api/tasks/definitions?category=segmentImages`.

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
  `hint` + `hint-key` props (don't hand-roll it per page); the global "use the bottom-left Quit button,
  not the browser tab" hint is in `App.vue`.
- **Empty states** — already exist: `ProjectPanel.vue` `.pp-empty` (no projects) and `ImageTable.vue`
  `.empty-state` (no images). Enrich the copy there; don't add a parallel component.
- **Shutdown** — reuse the existing sidebar-footer Quit (bottom-left) / Settings control
  (`appControl.quit()`); do **not** add another. Onboarding only *points at* it via the hint.

## ModuleLayout component

`frontend/src/components/ModuleLayout.vue`

Owns the full two-column layout, SetBar, image selection state, attr filtering, and the `filteredUids` / `selectedUids` / `selectedNames` derived state. Module pages receive these as slot props — they do not need their own refs.

**Selection is remembered across navigation.** The run-table checkbox selection is persisted in the project store (`getImageSelection`/`setImageSelection`, keyed by `${module}|${setUid}`), so leaving a module page and coming back restores it. `ImageTable` is the writer (seeds from the store on mount / set switch, commits on every toggle); `ModuleLayout` reads it to initialise `selectedUids` and to restore on set switch. Keying by module name keeps each page's selection its own (e.g. gating's single-select doesn't bleed into segment). It's in-memory/session-scoped and cleared on project load/close. This is generic — every module page gets it for free via `ModuleLayout`.

The attr filter panel renders automatically when `show-filter="true"` and the active set has images with `attr` values. It disappears when there are no attr keys, so it is safe to leave enabled even for modules that may or may not have attrs.

**Collapsible chrome (free up working space).** Two persisted toggles, both in the `settings` store (`localStorage`):
- **Left nav** — the `pi-bars` button in `AppHeader` toggles `settings.sidebarCollapsed`; `AppSidebar` `v-show`s its `<nav>` off, so the main canvas reclaims the full width. (The `v-show` lives on the `<nav>`, not on the `<AppSidebar>` element — the component has two root nodes, so a component-level `v-show` has no single root to bind and silently no-ops.)
- **Right panel** — `ModuleLayout` wraps the `#right` slot (TaskRunner / MetadataPanel / custom) with a thin always-visible left-edge handle (`pi-angle-double-*`) that toggles `settings.rightPanelCollapsed`. Collapsed → only the handle remains; the function/tasks panel folds away to the right. Every module page gets this for free.

Both default expanded and persist across sessions/navigation.

---

## ImageTable component

`frontend/src/components/ImageTable.vue`

| Prop | Type | Notes |
|------|------|-------|
| `setUid` | `string` | Required. Drives the image list from the project store. |
| `module` | `string?` | Selects per-module column config (status column label, etc.) |
| `allow-delete` | `bool` | Show per-row delete button. Default: `false`. |
| `show-attrs` | `bool` | Show attr columns. Default: `false`. |
| `filter-uids` | `string[]?` | When set, only these UIDs are shown. Managed by `ModuleLayout`. |

Emits `selectionChange(uids: string[])`. `ModuleLayout` handles this internally.

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
sidecars* and `docs/todo/QC_PLAN.md`); `qc.ts` only aggregates + formats them. The badge hover shows
the finding detail (e.g. drift correction's jump / canvas-expansion). It's **advisory** — never blocks.
`warn` findings tint amber; `info` are neutral. (MetadataPanel + chain-whiteboard surfaces are later phases.)

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
- **Hide-excluded toggle.** Next to the **Filter** button, `ModuleLayout` shows an **Excluded N**
  toggle (`pi-eye`/`pi-eye-slash`) that hides excluded rows entirely (default: show them greyed).
  Persisted per module like the filter panel.

**Attribute extraction — regex + builder** (`MetadataPanel.vue` → *Extract via regex*). Pulls an
attr value out of each image's filename (or original path) with a JavaScript regex: the first
capture group `()` is used if present, else the whole match (`extractWith` in
`frontend/src/utils/regexBuilder.ts` — the single extractor, so the live preview equals the applied
result). The field's tooltip carries a brief example for people who don't know regex. There is **one** regex
input with **one** live preview (`regexSample → regexPreview` against the first target image); a
collapsible **Builder** with two modes — **Split into fields** (separator × 1st/2nd/3rd/last field ×
drop-extension, `buildFieldRegex`) and **Around a marker** (extract a token *preceded/followed by*
context via lookbehind/lookahead, `buildLookaroundRegex`). Each context side is a **literal text +
a class that varies** (so "M" `+ number` → `(?<=M\d+)` anchors M1b/M2a/M4f without hardcoding the
mouse number → `b`/`a`/`f`); the extract token is a class or a raw custom pattern. Both modes write
straight into that same field on any change, so it's a way to construct the visible regex, not a
second input.
The user then watches the preview and can hand-edit the pattern. The pure builder/extract logic
lives in the util (Vitest-covered); the component only wires refs.

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
metadata at all, see CLAUDE.md → *OME-ZARR dual-format* — rather than asking the user to type
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

**Task list scoping.** `useTaskStore().forModule(module, projectUid?)` and `clearFinished(module,
projectUid?)` take an optional `projectUid` — `TaskList.vue`/`TaskRunner.vue` always pass the
current project's uid so switching projects doesn't leave a previous project's (e.g. cancelled)
tasks visible in the module sidebar. The global `/tasks` manager (`TasksModule.vue`) intentionally
omits it — that page is the cross-project view. `TaskEntry.projectUid` is what makes the filter
possible; it's stamped on every entry at `add()`/`addFromChainEvent()`.

**Cancel all** — a `pi-times-circle` button next to "Clear finished" in the Tasks section header,
shown only when the current module+project has running/queued tasks. Cancels every one of them via
the same per-task path as the individual cancel button (`task:cancel`/`chain:cancel` over WS,
deduping so a multi-node chain run only sends one `chain:cancel`).

---

## Adding a plot or visualization panel

Plots go either **in the left column** (below the image table, for compact summary visualizations) or **in the right panel** (alongside or instead of `TaskRunner`).

**Left column** — put the plot canvas in the `#plots` slot (ModuleLayout wraps it in the shared collapsible section — see above); reserve `#below-table` for rare extra custom content. Best for summary/gating/cluster canvases that sit naturally next to the image list.

**Right panel** — use the `#right` slot:

1. Create a panel component, e.g. `frontend/src/modules/gating/GatingPanel.vue`.
2. Use it in the module's `#right` slot — you have access to `setUid` and `selectedUids` from slot props.
3. Data comes from the backend via either:
   - **REST** — `fetch('/api/...')` inside `onMounted` / `watch`.
   - **WebSocket event** — `ws.on('myEvent', handler)` in `onMounted`, `ws.off(...)` in `onUnmounted`.
     See `ARCHITECTURE.md` (Napari → Julia event flow) for the WS event pattern.

Plot libraries in use — **two engines, split by job** (Plotly was removed):
- **regl-scatterplot** (WebGL) — **per-cell scatter**: gating plots + UMAP. Renders 100k–1M+ points
  at 60fps with lasso/rectangle select; gate shapes are a canvas2D overlay in data coords on top
  (`ScatterGL` + `PlotLayers` + `GateOverlay`). Used wherever **every cell** is drawn and/or gates
  are sketched. See "Gating page (WebGL scatter + gate overlay)" and `docs/POPULATION.md`.
- **Observable Plot** (`@observablehq/plot`, SVG) — **summary charts**: histogram, box/violin/beeswarm,
  bar, frequency/stacked, and (roadmap) heatmaps/tiled maps via `Plot.cell`/`Plot.raster`. Used
  wherever the data is **server-aggregated** (tiny payloads) and the ggplot `theme_classic` look /
  beeswarm / resize matter more than raw point throughput. See "Analysis-plot canvas (summary plots,
  Observable Plot)" and `docs/PLOTS.md` §0.

Why two: WebGL is necessary to render every cell and draw gates interactively; an SVG grammar-of-
graphics lib gives the cleaner publication look for pre-aggregated summaries. Neither does the other's
job well, so we keep both. Never add or swap a charting library without updating this doc, `docs/PLOTS.md`,
and the `cecelia-charting-decision` rationale.

### Plot loading state — delayed spinner

Heavy plots (a slow `/api/plot_data`, a big WebGL point fetch) must show they're working — a blank
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
wheel per tile — gate the overlay on `!compact` (or equivalent). Wired today in `SummaryPanel` and the
full-size `GateScatterCell` (Gate page); `UmapView` has its own empty-state wheel. New heavy plots: reuse
these two primitives.

**Gate scatters — one renderer, three hosts.** `components/plots/GateScatterCell.vue` is the ONE
scatter+gate body (WebGL points + contour/pop-colour layer + canvas2D gate overlay). The interactive
Gate page (`GatePlotPanel`, `mode` = rectangle/polygon) and every read-only montage tile (`mode="off"`)
share it. **Render modes** (`points` | `contour` | `outliers`, chosen via the shared
`components/plots/RenderModeToggle.vue`): `points` = WebGL pseudocolour cloud; `contour` = density
contours only, with the WebGL cloud SKIPPED (`GateScatterCell` passes `null` points to `ScatterGL`) so
it's the fast path; `outliers` = contours + individual dots for the sparse tail (FlowJo / old-R
"contour ± outliers"). The contour/outlier maths (density grid + low-density subset) is the pure,
unit-tested `plots/density.ts`, shared by `PlotLayers`. Montages of tiles go through `components/plots/GateMontage.vue` — a grid of `GateScatterCell`
tiles that owns the per-tile fetch (`plotmeta`/`plotdata`/`stats`), transpose reuse, optional coloured
population overlays (`highlight`), and PNG/PDF export. It has two tile producers: `GatingStrategyView`
(tree-derived, responsive wrap) and `GatePairsPanel` (a `ggpairs` matrix, `cols` set). A tile carries a
`role`: `scatter` (fetches + renders — the default, so tree-derived defs need no role), `diagonal` (a
labelled name cell, no fetch), or `corr` (an upper-triangle Pearson-r cell reusing its mirror scatter's
points, no fetch). In matrix mode `GateScatterCell` gets `hideAxisLabels` (the diagonal names each
channel, so per-tile axis labels only clutter/clip). Add a new gate-montage view by building `PanelDef[]`
and rendering `<GateMontage>` — never a second gate renderer.

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
- `components/canvas/interactiveViews.ts` — WebGL/interactive VIEWS (hosted by `InteractivePanel`), flags
  `clusterPage` / `analysisBoard`.
- `modules/cluster/clusterPanels.ts` — summary-family cluster PANELS (wrap `CanvasPanel`), flags
  `analysisBoard` / `trackOnly` / `needsCols`, plus a `props(ctx)` mapper so the host binds panel-specific
  props generically.

**Hosts render from the registries**: each builds its `+Plot` picker by filtering on its own flag, and
renders each slot with one generic `<component :is v-bind>`. Adding a plot to a surface = write the
component to the contract + one registry line + tick the flag. When you add the chain-whiteboard as a
host, it consumes the *same* registries + contract — do not re-wire plots per node.

**One mechanism across every surface — no per-page divergence.** The module page and the board host the
*same* components the *same* way; there is not "the cluster page's way" and "the board's way":
- **Cluster page** (`ClusterPlots.vue`) and **Analysis board** (`LayoutCanvas.vue`) both discover plots
  from `INTERACTIVE_VIEWS` + `CLUSTER_PANELS` and render them with the identical generic
  `<component :is v-bind>` (see each file's `clusterPanelProps`). Panel chrome differs only by `docked`
  (a grid slot drops its own reload/controls/export; a floating panel keeps them).
- **Behaviour / summary pages** (`SummaryCanvas.vue`) and the board's summary slots both render every
  summary plot through one `SummaryPanel` driven by the server plot-spec registry
  (`GET /api/plots/definitions`) — already a single mechanism.
- **`docked` is the contract's chrome switch.** A panel reads `docked` to hide anything that only makes
  sense free-floating (e.g. the per-plot Export dropdown) — the board re-fetches on context change and
  exports via PDF/CSV, so that chrome is redundant in a slot. `SummaryPanel` and the cluster panels use
  it; `InteractivePanel` can forward it to a view that needs it (none do today — plots stay fresh via
  `useDataRefresh`, so there are no per-plot reload buttons to hide).

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
  panel in `state`, `ImageStripView` `.is-bar`) opt in by tagging that bar `.cc-panel-controls` **and** giving
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

### Show/hide the population manager

The floating population manager (`PopulationManager` on gate/tracking + cluster pages, `SeriesPicker`
on summary pages) has a **toggle** (`pi-sitemap`) next to the arrange-windows icons on **every** module
canvas that has one (`SummaryCanvas`, `GatingPlots`, `ClusterPlots`), persisted per canvas in the
`shared` bag (`shared.showManager`, default shown). Wrap the manager `v-if="showManager"`.

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

**ViewerPanel** is rendered at the bottom of the sidebar as its own collapsible group (also
a chevron-toggled heading). See ViewerPanel component section below.

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

Shows the current Napari image and lets the user switch between versions (value names). Rendered
at the bottom of `AppSidebar` in its own collapsible group.

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

**History**: previously the viewer logic lived inline in `TaskRunner.vue`; it is now a standalone
component so it works regardless of which module page is active.

---

## Task definition fields — resource_pool

The TypeScript type `TaskDef` has `resource_pool?: string` (optional string). Every task JSON
in `app/src/tasks/<category>/<name>.json` should include this field:

```json
{ "resource_pool": "gpu" }    // GPU-bound tasks (cellpose_correct)
{ "resource_pool": "io" }     // I/O-bound tasks (omezarr)
{ "resource_pool": "default" }// everything else
```

The `tasksLimit` field and the concurrent-task slider have been removed. `TaskDef` no longer
has a `tasksLimit` field. The pool dropdown in `TaskRunner.vue` reads `resource_pool` and
pre-selects the matching pool from `/api/pools`.

---

## Chain whiteboard

Route `/chain` → `frontend/src/modules/ChainModule.vue`.

The whiteboard is the visual authoring tool for chain templates. It reads and writes the same `chains/<name>.json` format that `run_chain` and `save_chain_template!` use from the REPL — one format, two authoring paths.

`ChainModule` is wrapped in `<KeepAlive>` in `App.vue` so navigating to other pages and back does **not** reset unsaved edits. Edits only clear on an explicit reload (↻ button) or chain switch.

### Layout — Edit tab

```
Left (190px)               Center (flex)             Right (260px, opens on click)
────────────────           ──────────────────────    ───────────────────────────
Chain selector             @vue-flow/core canvas     Node config panel
+ New / Reload / Save      Node palette drop target  - Scope select
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
don't include a `label` field yet; see TODO #00018). Fallback is `fn.split('.').pop()`.

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
canvas shell (see the gating page's "Shared canvas shell"). **Charting library: Observable Plot
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
  node (native), PNG rasterises it at the DPR-aware `EXPORT_SCALE`. The summaries equivalent of `ScatterGL` for the big point
  clouds.
The universal **Analysis board** (`/analysis`, `AnalysisModule.vue`) is **multipage**: `TabbedCanvas`
wraps N independent boards, each a `SummaryCanvas` with no `module` prop (→ all plot specs). The tab
LIST (names/order/active) lives in the `analysisTabs` store; each board's plots persist in
`canvasPanels` under the canvas key `analysis:{projectUid}:tab:{id}` — i.e. tabs reuse the whole
existing canvas machinery via `SummaryCanvas`'s optional `canvasKey` prop (which overrides the default
`summary:{module|universal}` namespace and MUST be paired with a `:key` so setup re-runs on switch).
Only the active board is mounted; switching tabs re-binds that board's stored panels. Boards are
per-project — `TabbedCanvas` is `:key`ed by projectUid, and `analysisTabs`/`canvasPanels` are cleared
on project open/close (`stores/project.ts`). Closing a tab calls `canvasPanels.drop(key)`. In-memory
like the rest of the canvas state (survives navigation, not a full reload). Roadmap for the rest of the
multipage feature (interactive plots in the universal canvas, gating-strategy plot, multipage PDF):
`docs/todo/ANALYSIS_CANVAS_PLAN.md`.

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
- **`components/canvas/PopulationPanelShell.vue`** — the **shared chrome** for the floating population
  panels: the draggable/collapsible container + top-right placement (`useFloatingPanel`), the header
  (icon · title · count · collapse), the global/local **scope footer**, and the optional `PlotOptions`
  block (rendered when the host passes a `vis` bag). Both `SeriesPicker` and `PopulationManager` wrap
  it — the differing population LIST is the default slot; host-specific controls (the gating manager's
  gate/viewer options) go in the `#options` slot. Slotted rows keep their own component's scoped CSS;
  the shell owns only the chrome. One place for the chrome → the universal analysis board reuses it.
- **`components/canvas/PlotOptions.vue`** — the **shared** `VisProps` styling controls (collapsible
  Layout / Points / Colours / Labels sub-sections; props `vis`, emits `update:vis`). Embedded by BOTH
  `SeriesPicker` (summary canvas) and `PopulationManager` (gating / cluster canvas), so the styling
  knobs live in ONE place. `PopulationManager` renders it only when the host passes a `vis` bag (the
  cluster canvas does; the gate canvas doesn't) — the "add plot styling to the pop manager" keyword.
  The universal Analysis board (`/analysis`) gets the same controls for free.
- **`plots/export.ts`** — the **shared** plot-export plumbing (`svgToImageURL` PNG/SVG rasterise,
  `svgOf`, `downloadDataUrl`/`downloadBlob`, `rowsToCsv`, and `elementToImageURL`). Used by
  `PlotChart.toImageURL` AND the bespoke cluster panels, so a plot that renders its own `<svg>` exports
  identically to the generic renderer. `elementToImageURL(el, type, bg)` captures a whole host element
  (plot `<svg>` **plus** HTML overlay legend/title) by wrapping a style-inlined clone in an SVG
  `<foreignObject>` — the HMM panels use it so their overlay legends appear in the exported image
  (plain `svgToImageURL` captures only the svg). `plotHostToImageURL(host, bg)` exports **WebGL /
  canvas2D** plots (UMAP scatter, gate plot): it composites every `<canvas>` in the host (pass 1) then
  the HTML/SVG overlay layer on top (pass 2), because canvas pixels can't be serialised via
  `foreignObject`. This needs the WebGL context created with `preserveDrawingBuffer` — regl-scatterplot
  already does this, so `ScatterGL` needs no context pre-creation. **Subtlety (#00061):** the pass-2
  overlay clone hides the `<canvas>` (`blankCanvases`) but must also **clear the canvas's ancestor
  backgrounds**, or an opaque plot-area background (e.g. UMAP's `#0d0b1a`) paints over the pass-1
  scatter and the point cloud vanishes from the PNG. **Crispness:** two DPR-aware scales —
  `EXPORT_SCALE = min(4, 2×DPR)` for the SVG rasterise path (vector, crisp at any factor) and a higher
  `RASTER_SCALE = min(8, 4×DPR)` for `plotHostToImageURL`. A WebGL canvas can't be upscaled crisply
  from its CSS×DPR backing store, so `plotHostToImageURL(host, bg, { hiRes })` takes a resolver and
  **every stacked canvas re-renders itself at export scale**: `ScatterGL.exportCanvas` via
  regl-scatterplot's `export({scale})` (on a transparent ground so the cloud composites cleanly), and
  the gate plot's canvas2D layers (`PlotLayers`, `GateOverlay`) re-paint their content onto a scale×
  offscreen canvas. The captured host must also include any axis-label
  margins (the gate plot exports a `.plot-capture` wrapper, not the inner `.panel-plot`, so the axis
  names aren't clipped). Every cluster plot (UMAP, heatmap, HMM states/transitions) and the gate
  plot carry a footer **duplicate** and/or **export** dropdown. A registered interactive view opts into
  export by exposing `exportFormats: string[]` + `exportAs(kind)` via `defineExpose`; `InteractivePanel`
  surfaces the dropdown generically (UMAP → PNG + CSV). `plots/plot.ts` also exports `paletteRange(vis, n)` (palette → colour list, or
  `null` for 'standard') so bespoke panels honour the palette knob without re-deriving it.
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

**Plot specs** live in `app/src/plotDefinitions/*.json` (PACKAGE; one data source per file —
`{id,label,module,family,chartTypes,dataSource,scopeModes,params}`), served like task defs. Adding a
plot type = drop in a JSON, no new UI code. Current: `track_measures` (continuous track measures —
`chartTypes:[histogram,boxplot,bar]`, `granularity:track`) and `hmm_state_frequency`
(`live.cell.hmm.state.*`, `chartTypes:[frequency]`; mock column pending the behaviour module).

## Gating page (WebGL scatter + gate overlay)

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
- **Interactive** — client/WebGL point clouds with per-point interaction (regl `ScatterGL`), each with
  its own data endpoint + rendering. Gating scatter, **UMAP**. These can't be a single generic
  renderer, so they live in a **registry** of self-contained view components:
  **`components/canvas/interactiveViews.ts`** → `INTERACTIVE_VIEWS = { umap: { label, component } }`.
  A view (e.g. **`components/plots/UmapView.vue`**) fetches + renders + owns its controls; the generic
  **`components/canvas/InteractivePanel.vue`** wraps any view in `CanvasPanel` and spreads the plot
  `context` (project/images/popType/suffix) + the panel's persisted `state` onto it. **Adding an
  interactive plot = one `XView.vue` + one registry line** — no panel/canvas changes. (Shared infra,
  so the future universal canvas reuses it.)

## Cluster pages (UMAP + heatmap on the shared canvas)

`modules/ClusterCellsModule.vue` (route `/clust-cells`, popType `clust`) and
`modules/ClusterTracksModule.vue` (`/clust-tracks`, popType `trackclust`) — one page per granularity,
mirroring the gate/track split. Each is `ModuleLayout` (multi-select — clustering is set-scope) +
`TaskRunner` (`clustPops` / `clustTracks`) + a `#plots`-slot **`modules/cluster/ClusterPlots.vue`**
canvas (the cluster analogue of `GatingPlots`: `useCanvasPanels` keyed `clust:${popType}` + a "+ Plot"
picker + Tile/Cascade). The **"+ Plot"** picker lists every interactive view (`INTERACTIVE_VIEWS`) +
the summary **Heatmap**; each panel is routed by family — interactive → `InteractivePanel` (e.g.
**UMAP**: `obsm['X_umap.{suffix}']` coloured by cluster via `ScatterGL` `category` mode, legend +
toggleable cluster-number labels at each cluster centroid), summary → **`ClusterHeatmapPanel`**
(clusters × features via the matrix aggregation + `PlotChart`).
- **suffix is PAGE-LEVEL** — a dropdown of the discovered `clusters.{suffix}` runs (one shown at a
  time, like a segmentation), persisted in the canvas `shared` bag. Discovered from
  `GET /api/gating/channels` → `clusterSuffixes`.
- **Heatmap features = exactly what the run clustered on** — persisted per run as a
  `{props}.clustfeatures.json` sidecar (clustPops/clustTracks write it), surfaced via the channels
  endpoint's `clusterFeatures`. Channel rows aggregate by RAW name (the matrix passes
  `raw_channel_names`) and are relabelled to display names via the channels `nameMap`.
- Set-scope ⇒ panels pool across the selected images (shared UMAP space + cluster numbering; heatmap
  pools via `setUid`).
- **Cluster population-manager** — the shared floating `PopulationManager` (pop_type-agnostic) mounted
  in this canvas, driven by the gating store with `popType` `clust`/`trackclust`. A cluster pop has no
  gate: it's a filter on `clusters.{suffix}`, so the manager (in cluster mode, via its `clusterIds` +
  `suffix` props) shows an **"Add population"** button and, per pop, a row of **cluster-ID toggle
  chips**. Ticking a chip assigns that cluster to the pop and **removes it from any other** (a cluster
  lives in at most one pop — mirrors old R `setClusterForPop`); persisted via `pop/update`'s filter
  patch (`{values}`).
  - **Set-wide writes**: cluster pops apply to every selected image *in the run*. The store's
    `mirrorUids` replays each pop mutation across them (the filter is image-independent), so no manual
    "populate to set" step (we dropped the old `propagatePopMap` crutch). The primary image drives the
    displayed tree/stats.
  - **Run membership (`partOf`)**: a cluster pop only makes sense for images that were clustered
    together. clustPops/clustTracks record the run's uIDs in the `clustfeatures` sidecar (`{suffix →
    {features, partOf}}`), surfaced as `clusterMembers`. `ClusterPlots` writes only to the selected
    images in `partOf` and shows a banner naming any selected images that aren't in the run (and any
    run images not selected), with a **"Select clustered images"** button that sets the selection to
    exactly the run's images. That button uses a `selectUids(uids)` callback `ModuleLayout` exposes on
    both the `#plots` and `#below-table` slots — it writes the shared selection store, and `ImageTable`
    watches the store and re-seeds its checkboxes (so plot-canvas content can drive the selection generically).
  - **Highlight → overlays**: the manager's per-pop **eye** toggles a `highlighted` set (persisted in
    the canvas `shared` bag). `ClusterPlots` resolves it to `shownPops` ({path, name, colour,
    clusterIds}) and feeds it to the views:
    - **UMAP** (`UmapView`) colours each point by the population owning its cluster (grey for the
      rest); legend switches to the shown pops + an "other" row. Recolours from cached codes — no
      refetch on toggle.
    - **Heatmap** (`ClusterHeatmapPanel`) switches its columns from raw clusters to the shown
      populations: it requests the matrix with `pops = shownPops` and `category="pop"` (the pop_df
      `pop` column), i.e. a per-population profile. No pops shown → per-cluster as before. (Reloads
      when a pop's cluster **assignment** changes, not just its path.)
  - **Scope** (the manager's global/local footer) works here like gating: GLOBAL = one highlight set
    across all panels; LOCAL = each panel keeps its own (`state.hl`), so different UMAP/heatmap panels
    can show different pops. `ClusterPlots` mirrors `GatingPlots`' `scope`/`panelHL`/`activeHL`.
  - Plots run on the run-member images (`validUids`), and the set-pooled aggregation **skips images
    where a pop is absent** (`_pop_df` guards `has_pop`), so a pop defined on a subset of the pooled
    set never errors ("pop_membership: not found").
- **HMM behaviour plots (track clustering only)** — two extra "+ Plot" types, since HMM state/
  transition columns are categorical behaviour, not numeric heatmap material (they're filtered OUT of
  the heatmap's features). Both follow the highlight/scope like every cluster plot (per-panel
  `shownPops`); no pops highlighted → an empty-state prompt.
  - **HMM states** (`ClusterHmmStatesPanel`) — per shown population, a 100%-stacked horizontal bar of
    the within-population `live.cell.hmm.state.<m>` frequencies (reuses the `frequency` aggregation;
    self-rendered with Observable Plot since the layout is a transpose of the shared stacked bar).
  - **HMM transitions** (`ClusterHmmTransitionsPanel`) — per shown population (faceted), a from→to dot
    grid sized/coloured by transition frequency (one `crosstab` request per pop, split on `_`).
  - Backend: `pop_df(trackclust, …, granularity=:cell)` now carries requested **cell** columns onto
    the member cells (`_expand_tracks_to_cells(…; cell_cols)`), so the cell-level HMM obs is available
    per member cell. Ports `behaviourDTx.Rmd`.
- **clustTracks "Cluster on" picker**: pick BASE measures (like the old R) — whole-track motility is
  used directly; each cell measure (channels/morphology) and behaviour column (HMM state/transitions)
  is aggregated to ALL per-track stats by the task automatically (no `×mean/×median` in the list). The
  `labelPropsColsSelection` widget is popType-aware (was hardcoded to flow), so track clustering shows
  the track feature universe. Plus a `minTracklength` filter.

**Shared canvas shell (reused by the gating, track-gating, summary and universal canvases).** The
floating-panel mechanics are factored out of the gating page so other module canvases reuse them
unchanged:
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
  every remount (the Gate↔Tracking 2→4→6 bug, #00044).

### Persisting view state — the three scopes (important; read before adding any plot option)

Every user-settable option MUST live in a persisted bag, or it silently resets on remount (a plain
`ref()` in a canvas/panel component does NOT survive navigation). There are three scopes, all backed
by the `canvasPanels` store and keyed per canvas.

**The canvas key is per-image (module pages).** Module-page canvases embed the active object in their
key — `summary:{module}:{imageUid}`, `gate:{popType}:{imageUid}:{valueName}` (per segmentation too),
`clust:{popType}:{setUid}` (clustering is set-scope). `useCanvasPanels` takes a **reactive** key
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
WebGL/canvas2D layers re-render via `ResizeObserver`). Self-contained (own X/Y column + transform
on **stacked rows**, parent-population select, **render mode**, gate mode) with a **"−"** in the
header to remove it. New gates are added under that panel's selected parent population. Click a
panel to make it **active** (orange border); the active panel follows the population you select in
the manager (sets it as the displayed parent).

Plot stack — three superimposed layers, all mapped data→pixel through the **live** view
extents so they stay aligned through pan/zoom (`xMin`→left, `xMax`→right, `yMax`→top):
- **`components/plots/ScatterGL.vue`** — `regl-scatterplot` (WebGL, millions of points).
  Takes interleaved `Float32Array` `[x,y,…]` (already transformed) + fixed extents and
  **normalises to `[-1,1]`** for `draw()` — regl positions draw points as DEVICE coords `[-1,1]`,
  so raw data units would pin every point to the `+1,+1` corner. Sets `aspectRatio = width/height`
  (updated on resize) so the net x-scale is 1 (regl's default square aspect would letterbox a
  rectangular plot), and `cameraIsFixed: true` so the identity camera maps `[-1,1]` to the canvas
  edges — matching the overlays exactly (see the alignment bullet below). Lazy-imports the lib;
  `destroy()` + `ResizeObserver` cleanup in `onBeforeUnmount`. **First WebGL component — follow it.**
- **`components/plots/PlotLayers.vue`** — `canvas2D`. Density **contours** (marching squares)
  and the **population-colour overlay** (per-pop dots or per-pop contours).
- **`components/plots/GateOverlay.vue`** — `canvas2D` (top). **Draws** new **rectangle** (drag)
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

### Gating plot — rendering & UX hacks (read before touching the plot stack)

These are deliberate shortcuts; know them before changing the plot components.

- **Pseudocolour is computed client-side, smoothed.** ScatterGL bins the transformed points into
  a 160×160 grid, **box-blurs** it (≈ KDE), log-scales, then **bilinear-samples** the grid per
  point → regl's `colorBy:'valueA'`. The FlowJo **blue-heat ramp** (R `.flowColorRampBlueHeat`,
  `flowHelpers.R:775`) is interpolated to **256 stops** so the gradient is smooth (not 5 bands),
  and the low end is lifted off pure black (`#0b1a4d`) so sparse points stay visible on the dark
  background. No server density call for points.
- **Contours are client-side too.** PlotLayers builds a 64×64 (lightly box-blurred ≈ KDE)
  density grid and runs **marching squares** at normalised z = `1 − [0.95,0.90,0.75,0.5]`
  (R `.flowContourLines`, `flowHelpers.R:46`). The `/api/gating/density` endpoint exists but
  the canvas path doesn't use it.
- **Population overlay reuses `plotdata`, no new endpoint.** For each highlighted pop, the panel
  GETs `plotdata?pop=<path>` — Julia still owns membership; the client only colours the returned
  subset (dots in `points` mode, a contour in `contour` mode). Heavy for very large/low-
  selectivity pops (canvas2D dots) — acceptable for gated subpops.
- **Gate editing without stealing pan/zoom.** regl (below) owns pan/zoom. In edit (off) mode the
  overlay sets its own `pointer-events:'auto'` ONLY while the cursor is over a gate handle/body —
  detected via a *bubbled* `mousemove` on the parent container (events bubble up from the regl
  canvas even though the overlay is `'none'`) — and `'none'` otherwise, so empty-space clicks
  reach regl. During a drag it shows a local `draft` gate; on release it emits `edit` and keeps
  the draft until the authoritative tree arrives (a `gates`-prop watch clears it) → no snap-back
  flash.
- **Smooth cross-plot propagation via per-pop versions.** Editing pop A's gate in plot 1 must
  update plot 2 if it shows A — *without a full reload*. `stores/gating.ts` funnels every tree
  update through `setTree()`, which diffs gate/filter signatures vs the previous tree and bumps a
  per-path `popVersion` for changed pops **and their descendants** (parent∩child). Each panel
  watches the version of its displayed parent (→ refetch base points only, no `plotmeta`, so no
  axis flicker) and of its highlighted pops (→ refetch just those layers). regl redraw is
  instant, so it's smooth. `setTree` is the *single* tree-update entry (HTTP response **and** WS
  broadcast both call it) — the diff is idempotent, so the duplicate doesn't double-fire.
- **Scope governs ALL manager options.** A bottom-of-manager toggle — **icons only** (globe =
  **global**, default; pin = **local**), no "Scope" label, sitting below the Options box — decides
  whether every option — highlighted pops (the **eye**, not the old
  global `show` flag), gate labels, line width, axis mode — applies to **all plots** (global, one
  shared value) or **only the active plot** (local, the plot's own copy). `GatingPlots` keeps a
  global value and a per-`Plot` copy of each; `panelX(p)`/`activeX` computeds pick by scope, and
  edits route to the global value or the active plot. The manager's controls reflect the active
  scope, so they change as you switch the active plot in local mode (cf. the old per-box
  `popLeaves`). Each `GatePlotPanel` just consumes the resolved props (`highlight`,
  `gateLineWidth`, `gateLabels`, `axisFromZero`).
- **Gates show on the matching axis pair, either orientation.** A panel draws the child gates of
  its displayed population whose two channels are the plot's two channels — in **either order**
  (R `.flowMatchGatingParamsForPop`). `orientGate()` returns the gate as-is for the same
  orientation, or **transposed** (swap x/y coords + channels + transforms) when the axes are
  flipped, so changing X/Y to a combo that has a gate reveals it (reactively, via `currentGates`).
- **Gate display options** live in `GatingPlots` (apply to all plots) and are edited in the
  manager's collapsible **Options** box (grouped into `plot` / `viewer` sub-headings): a **gate-labels** toggle (subtle
  population name centred above each gate — bold with a dark halo for legibility, drawn on
  `GateOverlay`; **on by default**) and a **line-width** slider (gate stroke thickness). Passed to
  `GateOverlay` as `showLabels` / `lineWidth`. An **Axis** toggle — **whole-dataset scale**
  (double-diagonal icon, default) vs **autoscale-to-pop** (single-diagonal) — adds `x0/y0=1` to
  the plot queries. With it on, `plotmeta` sets each axis to `[transformed(0), transformed(full-
  dataset max)]` (the max comes from *all* cells, not the displayed pop) so the axis stays fixed
  when you select a population (FlowJo behaviour); off = autoscale to the displayed pop's extent.
- **Displayed parent is owned by `GatingPlots` (per panel), not the panel.** So the manager can
  highlight the active plot's parent row and switch the highlight when you change active plots.
  Clicking a population row sets it as the active plot's parent; clicking it **again resets to
  root** (toggle). The panel's `parent` is a writable computed over the `update:parent` event.
- **Channel-name labels in the UI.** The X/Y selects and axis titles show the resolved channel
  name (`store.colLabel`: maps an intensity column to `channelNames[i]`, others unchanged) while
  the API still receives the raw column name. Mirrors `pop_df`'s default resolution (POPULATION.md).
- **Axis ticks** are abbreviated client-side (`262144` → `262.1k`, k/M/G) so labels don't crowd,
  and rendered with real tick marks + baseline lines; the plot has generous margins so axis
  titles don't sit on the box edge.
- **Smoother pseudocolour**: see the density bullet above (160² grid, box-blurred, bilinear-
  sampled, 256-stop ramp, lifted low end).
- **Fixed camera → exact overlay alignment (pan/zoom disabled).** Getting the WebGL points to
  line up with the canvas2D overlays under regl's camera/aspect/scale machinery proved fragile
  (providing x/y scales made regl re-fit the camera and zoom in, drifting dots off the gates so
  gating caught no cells). The robust fix: **no x/y scales**, `aspectRatio = w/h` (uniform fill),
  and `cameraIsFixed: true` — the camera stays at identity, so `[-1,1]` maps straight to the
  canvas edges and the overlays' plain extents mapping matches exactly. `viewExtents` therefore
  always equals `extents`. Trade-off: **no interactive pan/zoom** for now; re-enabling it needs
  the overlays to replicate regl's full `projectionLocal·cameraView·model` transform (the
  `view`-event camera matrix), not a domain readout. (The contour grid's binning and `cellToData`
  must still use the same `/G` bin-centre convention.)
- **Selection is suppressed.** regl single-click selection would paint points blue, which we
  don't want. ScatterGL subscribes to `select` and immediately `deselect()`s, and sets
  `pointColorActive`/`pointColorHover` to the neutral colour. Pan/zoom stay enabled.
- **Active panel + manager link.** `GatingPlots` tracks an `activePanel` index (orange border
  via `.panel.active`); clicking a panel activates it; the active panel watches `selectedPop`
  and sets its parent so the manager selection "shows up on the plot".
- **PopulationManager drag is clamped** to its offset-parent (keeps a 24px grab strip on every
  edge, never above the top) so the floating window can't be dragged off-frame and lost.
- **Napari linked brushing.** The gating bar has a `pi-pencil` *select in napari* button
  (`store.startCellSelection`) that adds a draw layer in napari, plus a **Z toggle** beside it
  (`store.napariZMode` `'stack'`/`'slice'`) and, in slice mode, a **±N stepper**
  (`store.napariZWindow`) — `slice` restricts the spatial selection to cells within ±N z-slices of
  the live napari z, `stack` selects across the whole stack. Changing either **re-evaluates the
  active selection live** (`store.updateSelectionScope` → `/api/napari/selection-scope`) and applies
  to the next one. (The old gating-bar `pi-palette`
  *show populations* button was removed — it duplicated the ViewerPanel master toggle and confused
  users; showing pops is now the ViewerPanel toggle alone, remembered via
  `settings.napariShowPopulations`.) Drawing a region selects those cells spatially → they arrive
  back (via `gating:popmap`) as a **transient "Napari selection" population** (cyan `pi-map-marker`
  row, no rename) which `GatingPlots` auto-adds to the highlight set, so the spatially-selected
  cells light up in channel space. Its row has a **trash button** (`store.clearNapariSelection` →
  `/api/napari/stop-selection`) that clears the selection, removes its `Cell selection` Shapes
  layer from napari, and is then pruned from every plot's highlight set (so a plot with no other
  selection reverts from the dimmed backdrop to normal pseudocolour/contour). It's never persisted,
  so there's no pop to delete. The manager's per-pop `pi-images` column toggles a pop's napari visibility
  (its `show` flag) and re-pushes via `store.refreshNapariPops` (silent); the Options box has a
  **Napari dots** size slider (per-set `settings.get/setPointSize(setUid)`, re-pushes on release). The ViewerPanel
  *Show populations* toggle re-pushes live on every `gating:popmap` while shown, so the napari
  overlay tracks gate edits / pop add-remove as you gate. The transient selection pop is **not**
  pushed back into napari (it would steal the active layer mid-draw). See `docs/NAPARI.md` and
  `docs/POPULATION.md`.

### ModuleLayout — left panel layout

The left panel has two collapse mechanisms:
- **Horizontal** (‹/›) — shrinks the entire left panel to a 2.4rem strip. Useful for maximizing the right panel.
- **Vertical** — each section within the panel (image table, plots) has its own toggle header. The image table is always in a "Images" `CollapsibleSection`; the plot canvas is the module's `#plots` slot, which `ModuleLayout` wraps in one consistent collapse-persisted `CollapsibleSection` (label via `plotsLabel`). Extra `#below-table` content is wrapped by the module itself.

The whole left panel body scrolls vertically if sections together exceed available height. Collapsed state is local to the component and resets on navigation.
