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
| `--cc-mono` | system monospace stack | Log output, code |
| `--cc-header-h` | `40px` | Fixed header height |
| `--cc-sidebar-w` | `190px` | Fixed sidebar width |
| `--cc-runner-w` | `280px` | TaskRunner panel width |
| `--cc-console-bar-h` | `30px` | Collapsed console height |
| `--cc-console-open-h` | `210px` | Expanded console height |

---

## Hard requirements

Every interactive element must carry a `v-tooltip.right="'Description'"`.
CellProfiler is the reference for tooltip density — if a button does something non-obvious, it has a tooltip.

All errors go to `useLogStore().error(msg, { source, detail })`.
Task failures must never be silent — errors must reach the console bar visible to the user.

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
- `#below-table="{ setUid, selectedUids, selectedNames }"` — content rendered below the image table in the left column. Each piece should be wrapped in `<CollapsibleSection>` so users can toggle it independently. Multiple sections are supported.

If you need the active set in the module page itself (e.g. Import's file-browser guard), import `useProjectStore` and call `project.activeSet()` directly.

### Adding plots below the image table

Wrap each plot in `<CollapsibleSection>` and place it in the `#below-table` slot:

```vue
<template #below-table>
  <CollapsibleSection label="Intensity histogram">
    <IntensityHistogram :before="histBefore" :after="histAfter" x-label="Pixel intensity" />
  </CollapsibleSection>
  <CollapsibleSection label="Channel correlation" :default-open="false">
    <ChannelCorrelation :set-uid="setUid" />
  </CollapsibleSection>
</template>
```

The image table itself is already in a `CollapsibleSection` ("Images") managed by `ModuleLayout`. All sections scroll together in the left panel; the panel collapses horizontally with the ‹/› button.

`CollapsibleSection` props:
- `label` — section heading (uppercased in the toggle bar)
- `defaultOpen` — whether open on mount (default: `true`)
- `maxHeight` — CSS `max-height` for the body (default: `'320px'`; pass `'none'` to allow full growth)

### 2 — Register the route

`frontend/src/main.ts`:
```ts
import SegmentModule from './modules/SegmentModule.vue'
// ...
{ path: '/segment', component: SegmentModule, meta: { label: 'Segment' } },
```

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

## ModuleLayout component

`frontend/src/components/ModuleLayout.vue`

Owns the full two-column layout, SetBar, image selection state, attr filtering, and the `filteredUids` / `selectedUids` / `selectedNames` derived state. Module pages receive these as slot props — they do not need their own refs.

**Selection is remembered across navigation.** The run-table checkbox selection is persisted in the project store (`getImageSelection`/`setImageSelection`, keyed by `${module}|${setUid}`), so leaving a module page and coming back restores it. `ImageTable` is the writer (seeds from the store on mount / set switch, commits on every toggle); `ModuleLayout` reads it to initialise `selectedUids` and to restore on set switch. Keying by module name keeps each page's selection its own (e.g. gating's single-select doesn't bleed into segment). It's in-memory/session-scoped and cleared on project load/close. This is generic — every module page gets it for free via `ModuleLayout`.

The attr filter panel renders automatically when `show-filter="true"` and the active set has images with `attr` values. It disappears when there are no attr keys, so it is safe to leave enabled even for modules that may or may not have attrs.

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

---

## TaskRunner component

`frontend/src/tasks/TaskRunner.vue`

Fetches task definitions for a category, renders parameter forms, and submits tasks over WebSocket.
Always rendered in the `#right` slot of `ModuleLayout`.

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

---

## Adding a plot or visualization panel

Plots go either **in the left column** (below the image table, for compact summary visualizations) or **in the right panel** (alongside or instead of `TaskRunner`).

**Left column** — use the `#below-table` slot with `<CollapsibleSection>` wrappers (see above). Best for histograms, intensity plots, and other per-run summaries that sit naturally next to the image list.

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

For task results, the `task:result` message updates `img.filepaths[valueName]` in the Pinia project store automatically (handled in `ws.ts`). Panels that react to new results should `watch` the relevant image store field.

Full WS message-type reference is in `ARCHITECTURE.md`.

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

**Show populations** (`pi-palette` toggle, far right of the options row, after a `.opt-sep`
divider): the single master on/off for displaying the gating populations as coloured cell-centroid
Points layers in napari. POSTs `/api/napari/show-populations` with `show:true`/`false` (off clears
the layers). Its state is **remembered** (`settings.napariShowPopulations`, default on) and
auto-applied when an image opens, so populations show on open without re-clicking. While on, it
**re-pushes on every `gating:popmap`** (subscribed in `onMounted`) so the overlay tracks gating
live. Per-pop visibility and the dot-size slider live in the population manager — see the gating
section.
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

The tab badge shows the count of currently-running nodes.

### Node types

| VueFlow type | Julia scope | Visual cue |
|---|---|---|
| `"task"` | `"image"` or `"incremental"` | Purple accent border, solid (image) or dashed (incremental) |
| `"picnic"` | `"set"` | Amber/orange border, ◆ badge, barrier policy shown |
| `"live"` | (live view only) | Status-colored header bar; grey=queued, blue=running, green=done, red=failed, grey=cancelled |

Custom node components: `ChainTaskNode.vue`, `ChainPicnicNode.vue`, `ChainLiveNode.vue`.

### Chain JSON format

The whiteboard sends the standard `{name, nodes[], edges[]}` template format plus an optional `positions: {nodeId: {x, y}}` field. The backend preserves all fields verbatim (the scheduler ignores unknown fields when loading). Positions are purely a whiteboard concern — they survive save/load cycles but don't affect chain execution.

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
  node (native), PNG rasterises it 2×. The summaries equivalent of `ScatterGL` for the big point
  clouds.
These canvas components are **generic** (`components/canvas/`, NOT under a module) so every module
page — and the universal canvas (Phase 4) — reuses them unchanged:
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
gating workspace renders **below the table** (`#below-table` slot, wide left column),
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
its `#below-table` slot as `<GatingPlots :image-uid pop-type="track" />` (active when exactly one
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
`TaskRunner` (`clustPops` / `clustTracks`) + a below-table **`modules/cluster/ClusterPlots.vue`**
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
  pools via `setUid`). The cluster population-manager (tick cluster IDs → a `clust`/`trackclust` pop
  via `pop/update` filter) lands in this canvas next.

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
by the `canvasPanels` store and keyed per canvas:

1. **Per-panel** (chart type, measure, bins, error metric, …) → the panel's own `state` object
   (`CanvasItem.state`). `SummaryPanel` receives it as the `ui` prop and reads/writes it via computed
   get/set; each field falls back to the spec default until the user changes it.
2. **Per-canvas / global-scope** (the global selection, vis props, compare mode, scope toggle;
   gating's highlight set, line width, …) → the per-canvas **`shared`** bag.
3. **Geometry** (drag position + size) → the `geom` record, keyed `${canvasKey}:${panelId}`
   (`CanvasPanel` writes it on drag/resize; restored on mount).

**The mechanism: `composables/useViewState.ts` (Shiny-`reactiveValues`-style).** Pass it the `shared`
bag + a `defaults` literal; it seeds missing keys and returns `toRefs`, so **every option declared in
`defaults` persists automatically — there is nothing to wire per-field**. The convention is therefore
forget-proof: *put every option in the `defaults` object*; that single step is all that's needed. Do
**not** introduce a bare `ref()` for a user option in a canvas component.

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
  **Napari dots** size slider (`settings.napariPointSize`, re-pushes on release). The ViewerPanel
  *Show populations* toggle re-pushes live on every `gating:popmap` while shown, so the napari
  overlay tracks gate edits / pop add-remove as you gate. The transient selection pop is **not**
  pushed back into napari (it would steal the active layer mid-draw). See `docs/NAPARI.md` and
  `docs/POPULATION.md`.

### ModuleLayout — left panel layout

The left panel has two collapse mechanisms:
- **Horizontal** (‹/›) — shrinks the entire left panel to a 2.4rem strip. Useful for maximizing the right panel.
- **Vertical** — each section within the panel (image table, plots) has its own toggle header. The image table is always in a "Images" `CollapsibleSection`; below-table content is whatever the module puts in `#below-table` slot (each wrapped in `<CollapsibleSection>`).

The whole left panel body scrolls vertically if sections together exceed available height. Collapsed state is local to the component and resets on navigation.
