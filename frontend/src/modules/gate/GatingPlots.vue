<!--
  Gating workspace below the image table — the ONE gating canvas, reused for both flow gating (Gate
  page, popType="flow") and track-property gating (Tracking page, popType="track"). Page-level
  controls: segmentation (value_name) select + a "+ Plot" button (no fixed count). Plots are
  free-floating, draggable, resizable boxes (GatePlotPanel → CanvasPanel) added/removed dynamically;
  each has a "−" to remove itself. The floating PopulationManager sits on top. Per-plot state
  (displayed parent, local highlight) is owned here keyed by a stable id (via useCanvasPanels), so
  the manager can drive the ACTIVE plot.

  popType only changes (a) the data source the store/API reads (flow cells vs the per-track table —
  handled server-side) and (b) the napari overlay: flow offers cell-selection linked brushing
  (transient pops) + Points layers; track offers a "Show tracks" push (napari Tracks layers). The
  panel/manager components and the workspace logic (useCanvasPanels: add/remove, Tile/Cascade,
  active panel) are shared as-is — no track-specific clone.
-->
<script setup lang="ts">
import { ref, computed, watch, provide, onMounted, onUnmounted, useTemplateRef } from 'vue'
import { useGatingStore } from '../../stores/gating'
import { useWsStore } from '../../stores/ws'
import { useProjectStore } from '../../stores/project'
import { useNapariOpen } from '../../composables/useNapariOpen'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useCanvasWorkspace } from '../../composables/useCanvasWorkspace'
import { useViewState } from '../../composables/useViewState'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import GatePlotPanel from './GatePlotPanel.vue'
import GatePairsPanel from './GatePairsPanel.vue'
import PopulationManager from '../../components/canvas/PopulationManager.vue'
import CanvasZoomControl from '../../components/canvas/CanvasZoomControl.vue'
import GatingCopyDialog from './GatingCopyDialog.vue'
import type { FlatPop } from '../../stores/gating'

const props = withDefaults(defineProps<{
  imageUid: string | null
  popType?: string
  orderedUids?: string[]                        // visible images in table order (for prev/next)
  selectUids?: (uids: string[]) => void          // drive the table selection (ModuleLayout)
}>(), { popType: 'flow', orderedUids: () => [] })
const isTrack = computed(() => props.popType === 'track')
const g = useGatingStore()
const ws = useWsStore()
const project = useProjectStore()
const { openInNapari } = useNapariOpen()

// ── Scope ─────────────────────────────────────────────────────────────────────
// EVERY manager option (highlighted pops, gate labels, line width, axis) obeys this:
// GLOBAL → one shared value applied to all plots; LOCAL → the active plot's own value.
// per-plot copies of every scoped option (used when scope = 'local')
// per-plot copies also carry the axis config (channels/transforms/render mode) so those persist per
// plot across navigation — like the summary panels' `ui` bag. Bare refs in the panel reset on remount.
type GateKind = 'linear' | 'log' | 'asinh' | 'logicle'
// A panel is either a single gate scatter (default, drawable) or a read-only channel-pairs matrix.
// `channels` is the pairs plot's selected list; the single plot ignores it (and vice-versa for x/y).
interface PlotState { kind: 'single' | 'pairs'; parent: string; hl: string[]; lineWidth: number; labels: boolean; fromZero: boolean
  x: string; y: string; xt?: GateKind; yt?: GateKind; renderMode: 'points' | 'contour' | 'outliers'; channels: string[] }
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')   // the visible viewport (zoom + fit measure it)
const zoomRef = useTemplateRef<HTMLElement>('zoomRef')       // the scaled workspace (panels' offsetParent)
// Per-image + segmentation: gating populations are per-value_name, so each (image, segmentation) keeps
// its own plots/parents/highlights and the canvas rebinds when either the image or the segmentation
// (g.valueName) changes.
const ckey = computed(() => `gate:${props.popType}:${props.imageUid ?? 'none'}:${g.valueName}`)
// Axis transforms (xt/yt) are intentionally LEFT UNSET here so the panels' own per-axis default
// fires: GatePlotPanel/GatePairsPanel resolve `ui.xt ?? axisDefaultTransform(col)` (linear for
// spatial/centroid axes via the store's isLinearAxis, logicle for flow intensities). That fallback
// only runs while ui.xt is undefined — pre-seeding a concrete transform here would pin logicle and
// silently defeat it. Channels (x/y) start empty; the panel picks index-based defaults once columns
// load (see ensureChannels).
const { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade, contentBounds } =
  useCanvasPanels<PlotState>(zoomRef, () =>
    ({ kind: 'single', parent: 'root', hl: [], lineWidth: 1.5, labels: true, fromZero: true,
       x: '', y: '', renderMode: 'points', channels: [] }), ckey)
// show/hide the floating population manager — persisted per canvas in the `shared` bag (default shown)
const showManager = computed<boolean>({ get: () => (shared.value.showManager as boolean) ?? true, set: v => (shared.value.showManager = v) })

// visual zoom (shared control): scale the free-floating plot workspace to see everything at once. Fit
// fits the actual plot bounding box; drag is zoom-corrected via the injected zoom. The workspace GROWS
// when zoomed out (useCanvasWorkspace); the population manager sits OUTSIDE the zoom layer (full-size).
const { zoom, fitWidth, fitHeight, setZoom, reset: resetZoom } = useCanvasZoom(canvasRef,
  () => ({ w: contentBounds.value.w || null, h: contentBounds.value.h }))
provide(CANVAS_ZOOM_KEY, zoom)
const { workspaceStyle } = useCanvasWorkspace(canvasRef, zoom)
// add a read-only channel-pairs matrix panel (same canvas, same shared options as a single plot)
function addPairs() { const id = add(); const p = panels.value.find(x => x.id === id); if (p) p.state.kind = 'pairs' }

// global-scope values live in the canvas `shared` bag via useViewState, so they PERSIST across
// navigation with no per-field wiring (the highlighted pops were resetting on remount). Add an
// option to the defaults below and it persists automatically — see useViewState.ts.
const { scope, hl: gHL, lineWidth: gLineWidth, labels: gLabels, fromZero: gFromZero } = useViewState(shared, {
  scope: 'global' as 'global' | 'local',     // global = one value for every plot; local = active plot only
  hl: [] as string[],                         // global-scope highlighted pop paths
  lineWidth: 1.5, labels: true, fromZero: true,
})

// effective value for a given plot (what the panel renders with)
const panelHL = (s: PlotState) => scope.value === 'global' ? gHL.value : s.hl
const panelLineWidth = (s: PlotState) => scope.value === 'global' ? gLineWidth.value : s.lineWidth
const panelLabels = (s: PlotState) => scope.value === 'global' ? gLabels.value : s.labels
const panelFromZero = (s: PlotState) => scope.value === 'global' ? gFromZero.value : s.fromZero

// what the manager shows/edits = the active scope's value
const activeHL = computed(() => scope.value === 'global' ? gHL.value : (activePanel.value?.state.hl ?? []))
const activeLineWidth = computed(() => scope.value === 'global' ? gLineWidth.value : (activePanel.value?.state.lineWidth ?? 1.5))
const activeLabels = computed(() => scope.value === 'global' ? gLabels.value : (activePanel.value?.state.labels ?? true))
const activeFromZero = computed(() => scope.value === 'global' ? gFromZero.value : (activePanel.value?.state.fromZero ?? true))

// edits route to the global value or the active plot depending on scope
const toggle = (arr: string[], v: string) => arr.includes(v) ? arr.filter(x => x !== v) : [...arr, v]
function toggleHighlight(path: string) {
  if (scope.value === 'global') gHL.value = toggle(gHL.value, path)
  else if (activePanel.value) activePanel.value.state.hl = toggle(activePanel.value.state.hl, path)
}
function setLineWidth(v: number) { if (scope.value === 'global') gLineWidth.value = v; else if (activePanel.value) activePanel.value.state.lineWidth = v }
function setLabels(v: boolean)   { if (scope.value === 'global') gLabels.value = v;    else if (activePanel.value) activePanel.value.state.labels = v }
function setFromZero(v: boolean) { if (scope.value === 'global') gFromZero.value = v;  else if (activePanel.value) activePanel.value.state.fromZero = v }

// manager highlights the ACTIVE plot's population; clicking it again resets to root
const selected = computed(() => activePanel.value?.state.parent ?? 'root')
function setParent(id: number, v: string) { const p = panels.value.find(x => x.id === id); if (p) p.state.parent = v }
function onPickPop(path: string) {
  const s = activePanel.value?.state
  if (s) s.parent = s.parent === path ? 'root' : path
}

// Prev/next image navigation: step the table selection through the visible image list, so gating a
// batch is just << / >>, not a manual re-pick each time. Stops at the ends (no wrap) — the buttons
// disable there. Changing the selection re-drives imageUid via ModuleLayout (see `load`).
const navIndex = computed(() => props.imageUid ? props.orderedUids.indexOf(props.imageUid) : -1)
const hasPrev  = computed(() => navIndex.value > 0)
const hasNext  = computed(() => navIndex.value >= 0 && navIndex.value < props.orderedUids.length - 1)
function navTo(delta: number) {
  const i = navIndex.value + delta
  if (i < 0 || i >= props.orderedUids.length) return
  const uid = props.orderedUids[i]
  props.selectUids?.([uid])                    // switch the gating plots to the next image
  // follow along in the viewer IF napari is currently showing an image — so gating a batch keeps the
  // image in sync too, not just the plot. Don't force-launch napari when it isn't open.
  if (project.napariImageUid) openInNapari(uid, setUid.value)
}

// "Copy gating strategy to other images" dialog (per current pop type; see GatingCopyDialog).
const showCopy = ref(false)
const setUid = computed(() => g.napariSetUid())   // the set the gated image belongs to

// Defining-plot: open the plot where a pop's gate was drawn — a new single panel showing the pop's
// PARENT (the cloud the gate was drawn on) on the gate's own channels + transforms. The gate outline
// appears automatically (it's a child of that parent, server-projected). Works for flow or track.
function showDefiningPlot(pop: FlatPop) {
  if (!pop.gate) return
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (!p) return
  p.state.kind = 'single'
  p.state.parent = pop.parent
  p.state.x = pop.gate.x_channel
  p.state.y = pop.gate.y_channel
  p.state.xt = pop.gate.x_transform.kind
  p.state.yt = pop.gate.y_transform.kind
}

async function load() {
  if (props.imageUid) await g.selectImage(props.imageUid, g.valueName, props.popType)
}
function onBroadcast(d: unknown) { g.applyBroadcast(d as any) }

watch(() => props.imageUid, load)
// a transient pop (napari cell selection) appears → auto-highlight it on every plot so the
// spatially-selected cells light up in channel space immediately (linked brushing)
watch(() => g.transientPaths, (paths) => {
  for (const p of paths) if (!gHL.value.includes(p)) gHL.value = [...gHL.value, p]
}, { deep: true })
// a pop disappeared (cleared napari selection, deleted pop) → drop it from highlights and any
// plot displaying it. Without this a stale highlight keeps showPops true, so the base plot stays
// dimmed/flat with no overlay to load (grey) instead of reverting to pseudocolour/contour.
watch(() => g.flat.map(p => p.path).join('\n'), () => {
  const exist = new Set(g.flat.map(p => p.path))
  gHL.value = gHL.value.filter(p => exist.has(p))
  for (const p of panels.value) {
    p.state.hl = p.state.hl.filter(x => exist.has(x))
    if (p.state.parent !== 'root' && !exist.has(p.state.parent)) p.state.parent = 'root'
  }
})
// WS (re)connect resync: the transient napari-selection pop lives ONLY in the server's in-memory
// registry (never persisted — see docs/POPULATION.md), so a backend restart wipes it. But the client's
// tree (and the persisted highlight referencing it) survive, so without a resync the stale selection
// keeps a plot greyed on the same image. On a RECONNECT (not the first connect — onMounted already
// loaded) refetch the popmap; the fresh tree drops the transient pop and the prune watch above clears
// the dangling highlight. `everConnected` seeded from the current status so a reconnect is detected even
// when the page mounts already-connected.
let everConnected = ws.status === 'connected'
watch(() => ws.status, (s) => {
  if (s !== 'connected') return
  if (everConnected && props.imageUid) load()
  everConnected = true
})
onMounted(() => { ws.on('gating:popmap', onBroadcast); load() })
// Seed two starter plots for any (image, segmentation) that has none yet — on first bind AND after an
// image/segmentation switch (the reactive key rebinds to a fresh entry; the component doesn't remount).
// Gated on valueNames being loaded so we don't seed a transient placeholder key, and skipped for
// restored canvases (they come back non-empty). Persisted per (image, value_name), so no 2→4→6 stacking.
watch([ckey, () => g.valueNames.length], () => {
  if (props.imageUid && g.valueName && g.valueNames.includes(g.valueName) && panels.value.length === 0) {
    add(); add()
  }
}, { immediate: true })
onUnmounted(() => ws.off('gating:popmap', onBroadcast))
</script>

<template>
  <div class="gating-plots">
    <div v-if="!props.imageUid" class="gp-empty">Select one image above to gate.</div>
    <template v-else>
      <div class="gp-bar">
        <label>segmentation
          <select v-model="g.valueName" v-tooltip.bottom="'Which segmentation (labelProps) to gate on'"
                  @change="g.selectImage(props.imageUid!, g.valueName, props.popType)">
            <option v-for="v in g.valueNames" :key="v" :value="v">{{ v }}</option>
          </select>
        </label>
        <button class="cc-btn cc-btn-primary" v-tooltip.bottom="'Add a plot'" @click="add">
          <i class="pi pi-plus" /> Plot
        </button>
        <button class="cc-btn cc-btn-primary" v-tooltip.bottom="'Add a read-only channel-pairs matrix (ggpairs): compare a set of channels against each other'"
                @click="addPairs">
          <i class="pi pi-plus" /> Pairs
        </button>
        <!-- FLOW: spatial cell-selection brush (linked brushing → transient cell pop). -->
        <div v-if="!isTrack" class="seg">
          <!-- showing populations in napari is the ViewerPanel's palette toggle (remembered);
               here we only offer the spatial cell-selection brush. -->
          <button v-tooltip.bottom="'Draw a region on the napari image to highlight those cells here'"
                  @click="g.startCellSelection"><i class="pi pi-pencil" /></button>
          <!-- z scope: whole stack (default) vs only the current z-slice (± window). Changing this
               re-evaluates any active selection live (and applies to the next one). -->
          <button :class="{ on: g.napariZMode === 'slice' }"
                  v-tooltip.bottom="g.napariZMode === 'slice'
                    ? `Selecting cells from the current z-slice ±${g.napariZWindow} — click for the whole stack`
                    : 'Selecting cells across the whole z-stack — click to restrict to the current z-slice'"
                  @click="g.napariZMode = g.napariZMode === 'slice' ? 'stack' : 'slice'">
            <i class="pi pi-clone" /> Z
          </button>
        </div>
        <label v-if="!isTrack && g.napariZMode === 'slice'" class="zwin"
               v-tooltip.bottom="'z-slice window: include cells within ± this many slices of the current z (0 = current slice only)'">
          ±<input type="number" min="0" max="50" step="1" v-model.number="g.napariZWindow" />
        </label>
        <div class="seg">
          <button v-tooltip.bottom="'Tile in a grid'" @click="arrangeGrid"><i class="pi pi-th-large" /></button>
          <button v-tooltip.bottom="'Cascade windows'" @click="arrangeCascade"><i class="pi pi-clone" /></button>
        </div>
        <div class="seg">
          <button :class="{ on: showManager }" @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the population manager' : 'Show the population manager'">
            <i class="pi pi-sitemap" />
          </button>
        </div>
        <div class="seg" v-tooltip.bottom="'Step to the previous / next image in the list'">
          <button :disabled="!hasPrev" @click="navTo(-1)" aria-label="Previous image">&laquo;</button>
          <button :disabled="!hasNext" @click="navTo(1)" aria-label="Next image">&raquo;</button>
        </div>
        <button class="cc-btn" v-tooltip.bottom="'Copy this gating to other images in the set'"
                @click="showCopy = true"><i class="pi pi-copy" /> Copy</button>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
        <span class="gp-hint">drag plots by their title · resize from the corner</span>
      </div>
      <div ref="canvasRef" class="gp-canvas">
        <!-- scaled workspace: the plots zoom together; the population manager stays full-size (below) -->
        <div ref="zoomRef" class="gp-zoom" :style="workspaceStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <GatePairsPanel v-if="p.state.kind === 'pairs'" :index="i" :arrange="p.arrange"
                          :active="p.id === activeId" :parent="p.state.parent" :highlight="panelHL(p.state)"
                          :gate-line-width="panelLineWidth(p.state)" :gate-labels="panelLabels(p.state)" :axis-from-zero="panelFromZero(p.state)"
                          :ui="p.state" :persist-key="`${ckey}:${p.id}`"
                          @activate="activeId = p.id" @update:parent="setParent(p.id, $event)" @remove="remove(p.id)" />
          <GatePlotPanel v-else :index="i" :arrange="p.arrange"
                         :active="p.id === activeId" :parent="p.state.parent" :highlight="panelHL(p.state)"
                         :gate-line-width="panelLineWidth(p.state)" :gate-labels="panelLabels(p.state)" :axis-from-zero="panelFromZero(p.state)"
                         :ui="p.state" :persist-key="`${ckey}:${p.id}`"
                         @activate="activeId = p.id" @update:parent="setParent(p.id, $event)" @remove="remove(p.id)" />
        </template>
        </div>
        <PopulationManager v-if="showManager" :selected="selected" :highlighted="activeHL" :scope="scope" :pop-type="props.popType"
                           :line-width="activeLineWidth" :gate-labels="activeLabels" :axis-from-zero="activeFromZero"
                           @update:selected="onPickPop" @update:scope="scope = $event" @toggle-highlight="toggleHighlight"
                           @update:line-width="setLineWidth" @update:gate-labels="setLabels"
                           @update:axis-from-zero="setFromZero" @show-defining-plot="showDefiningPlot" />
      </div>
    </template>
    <GatingCopyDialog v-if="showCopy" :set-uid="setUid" :source-uid="props.imageUid!"
                      :value-name="g.valueName" :pop-type="props.popType" @close="showCopy = false" />
  </div>
</template>

<style scoped>
/* fill all available height so the plot workspace isn't capped */
.gating-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.gp-empty { padding: 20px; color: var(--cc-text-dim); }
.gp-bar { display: flex; align-items: center; gap: 14px; padding: 8px 4px; font-size: 12px; flex-shrink: 0; }
.gp-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.gp-bar select { min-width: 9rem; }   /* visual styling from the global form base */
.gp-hint { color: var(--cc-text-dim); font-size: 11px; opacity: 0.7; }
/* window-arrange segmented icons */
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 5px 9px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.seg button.on { background: var(--cc-accent); color: #fff; }
.seg button:disabled { opacity: 0.35; cursor: default; }
.seg button:disabled:hover { color: var(--cc-text-dim); }
/* z-slice window stepper (shown only in slice mode) */
.zwin { display: flex; align-items: center; gap: 2px; color: var(--cc-text-dim); }
.zwin input { width: 3.2rem; font-size: 12px; padding: 3px 4px; }
/* free-floating plot workspace: panels + manager are absolutely positioned within */
.gp-canvas { position: relative; flex: 1; min-height: 70vh; }
/* the scaled workspace fills the canvas (offsetParent for the floating plot panels); transform inline */
/* scaled workspace (offsetParent for panels); size + transform set inline by useCanvasWorkspace.
   min 100% so it always at least fills the viewport (like the old inset:0) even before the JS size
   lands — else a 0 measurement collapses it and drag pins panels to the top-left. */
.gp-zoom { position: absolute; top: 0; left: 0; min-width: 100%; min-height: 100%; }
</style>
