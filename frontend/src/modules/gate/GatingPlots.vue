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
import { computed, watch, onMounted, onUnmounted, useTemplateRef } from 'vue'
import { useGatingStore } from '../../stores/gating'
import { useWsStore } from '../../stores/ws'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useViewState } from '../../composables/useViewState'
import GatePlotPanel from './GatePlotPanel.vue'
import PopulationManager from '../../components/canvas/PopulationManager.vue'

const props = withDefaults(defineProps<{ imageUid: string | null; popType?: string }>(),
  { popType: 'flow' })
const isTrack = computed(() => props.popType === 'track')
const g = useGatingStore()
const ws = useWsStore()

// ── Scope ─────────────────────────────────────────────────────────────────────
// EVERY manager option (highlighted pops, gate labels, line width, axis) obeys this:
// GLOBAL → one shared value applied to all plots; LOCAL → the active plot's own value.
// per-plot copies of every scoped option (used when scope = 'local')
interface PlotState { parent: string; hl: string[]; lineWidth: number; labels: boolean; fromZero: boolean }
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')
const { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<PlotState>(canvasRef, () =>
    ({ parent: 'root', hl: [], lineWidth: 1.5, labels: true, fromZero: true }),
    `gate:${props.popType}`)

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
onMounted(() => {
  ws.on('gating:popmap', onBroadcast); load()
  // seed two plots ONLY when this canvas is empty — panels persist per `gate:${popType}` across
  // navigation, so an unconditional add() stacks two more every time the page re-mounts (switching
  // Gate↔Tracking: 2→4→6…).
  if (panels.value.length === 0) { add(); add() }
})
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
        <!-- FLOW: spatial cell-selection brush (linked brushing → transient cell pop). -->
        <div v-if="!isTrack" class="seg" v-tooltip.bottom="'Napari linked brushing'">
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
        <!-- TRACK: push the gated tracks to napari as Tracks layers (needs a running napari + timecourse). -->
        <button v-if="isTrack" class="cc-btn cc-btn-ghost"
                v-tooltip.bottom="'Show the gated tracks in napari (Tracks layers)'"
                @click="g.showTracks()"><i class="pi pi-share-alt" /> Tracks</button>
        <div class="seg" v-tooltip.bottom="'Arrange windows'">
          <button v-tooltip.bottom="'Tile in a grid'" @click="arrangeGrid"><i class="pi pi-th-large" /></button>
          <button v-tooltip.bottom="'Cascade windows'" @click="arrangeCascade"><i class="pi pi-clone" /></button>
        </div>
        <span class="gp-hint">drag plots by their title · resize from the corner</span>
      </div>
      <div ref="canvasRef" class="gp-canvas">
        <GatePlotPanel v-for="(p, i) in panels" :key="p.id" :index="i" :arrange="p.arrange"
                       :active="p.id === activeId" :parent="p.state.parent" :highlight="panelHL(p.state)"
                       :gate-line-width="panelLineWidth(p.state)" :gate-labels="panelLabels(p.state)" :axis-from-zero="panelFromZero(p.state)"
                       :persist-key="`gate:${props.popType}:${p.id}`"
                       @activate="activeId = p.id" @update:parent="setParent(p.id, $event)" @remove="remove(p.id)" />
        <PopulationManager :selected="selected" :highlighted="activeHL" :scope="scope" :pop-type="props.popType"
                           :line-width="activeLineWidth" :gate-labels="activeLabels" :axis-from-zero="activeFromZero"
                           @update:selected="onPickPop" @update:scope="scope = $event" @toggle-highlight="toggleHighlight"
                           @update:line-width="setLineWidth" @update:gate-labels="setLabels"
                           @update:axis-from-zero="setFromZero" />
      </div>
    </template>
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
/* z-slice window stepper (shown only in slice mode) */
.zwin { display: flex; align-items: center; gap: 2px; color: var(--cc-text-dim); }
.zwin input { width: 3.2rem; font-size: 12px; padding: 3px 4px; }
/* free-floating plot workspace: panels + manager are absolutely positioned within */
.gp-canvas { position: relative; flex: 1; min-height: 70vh; }
</style>
