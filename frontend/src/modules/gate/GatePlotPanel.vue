<!--
  One modular gating plot (mirrors the old flowPlotManager "gating box"): its own X/Y column +
  transform, parent-population select, render mode + gate-draw tools, a WebGL scatter (ScatterGL)
  with a contour/population layer (PlotLayers) and a canvas2D gate overlay (GateOverlay), plus
  inline naming to persist a drawn shape.

  Render modes (R: cciaConf fcs.gating.plotTypes — pseudocolour/contour/raster):
   - points   → FlowJo pseudocolour (density-coloured points)
   - contour  → density contours over faint points
  Independent "pop colours" toggle overlays the visible child populations in their colours
  (works with either mode). The active panel (orange border) also follows the population the
  user selects in the manager.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef } from 'vue'
import { useGatingStore, isReservedPopName, type GateSpec, type TransformSpec } from '../../stores/gating'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import GateScatterCell from '../../components/plots/GateScatterCell.vue'
import RenderModeToggle, { type RenderMode } from '../../components/plots/RenderModeToggle.vue'
import type { PopLayer } from '../../components/plots/PlotLayers.vue'
import { orientGate } from '../../plots/gateGeometry'
import { downloadDataUrl } from '../../plots/export'

const props = defineProps<{
  index: number; active: boolean; parent: string; highlight: string[]
  gateLineWidth: number; gateLabels: boolean; axisFromZero: boolean
  // persisted per-plot axis config (owned by GatingPlots' PlotState) — channels, transforms, render
  // mode. Read/written directly like the summary panels' `ui` bag so these survive navigation.
  ui: { x?: string; y?: string; xt?: 'linear' | 'log' | 'asinh' | 'logicle'
        yt?: 'linear' | 'log' | 'asinh' | 'logicle'; renderMode?: RenderMode }
  // window-arrangement command (Tile/Cascade); seq bumps to force re-apply
  arrange?: ArrangeCmd | null
  persistKey?: string        // CanvasPanel geometry persistence key
}>()
const emit = defineEmits<{ activate: [number]; 'update:parent': [string]; remove: [] }>()
const g = useGatingStore()
const log = useLogStore()

// the free-floating chrome (drag/clamp/resize/arrange/active/remove) lives in CanvasPanel now.

type Kind = 'linear' | 'log' | 'asinh' | 'logicle'
const TRANSFORMS: Kind[] = ['linear', 'log', 'asinh', 'logicle']
// track properties (motility, per-track aggregates) are plain continuous values → linear by
// default; flow intensities default to logicle (FlowJo). User can switch either per axis.
const defaultTransform: Kind = g.popType === 'track' ? 'linear' : 'logicle'
// axis config reads/writes the persisted `ui` bag (owned by GatingPlots) so it survives remount.
const xChan = computed({ get: () => props.ui.x ?? '', set: v => { props.ui.x = v } })
const yChan = computed({ get: () => props.ui.y ?? '', set: v => { props.ui.y = v } })
const xt = computed<Kind>({ get: () => props.ui.xt ?? defaultTransform, set: v => { props.ui.xt = v } })
const yt = computed<Kind>({ get: () => props.ui.yt ?? defaultTransform, set: v => { props.ui.yt = v } })
const renderMode = computed<RenderMode>({ get: () => props.ui.renderMode ?? 'points', set: v => { props.ui.renderMode = v } })
// displayed population is owned by GatingPlots (per-panel) so the manager can highlight it
const parent = computed({ get: () => props.parent, set: v => emit('update:parent', v) })
// draw tool is transient interaction state (not persisted): reopening a plot shouldn't leave a tool armed
const mode = ref<'off' | 'rectangle' | 'polygon'>('off')
// pop-colour overlay is driven by the populations checked for THIS panel in the manager
const showPops = computed(() => (props.highlight?.length ?? 0) > 0)

const points = ref<Float32Array | null>(null)
const extents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })            // fixed (full data range)
const viewExtents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })        // = extents (camera fixed)
const viewTick = ref(0)
const xTicks = ref<{ pos: number; label: string }[]>([])
const yTicks = ref<{ pos: number; label: string }[]>([])
const popLayers = ref<PopLayer[]>([])
const loading = ref(false)
const pending = ref<Partial<GateSpec> | null>(null)
const newName = ref('')

const parentOptions = computed(() => ['root', ...g.flat.map(p => p.path)])
const tspec = (k: Kind): TransformSpec => k === 'logicle' ? { kind: k, T: 262144, W: 0.5, M: 4.5, A: 0 } : { kind: k }
const axisQ = (p: string, k: Kind) => k === 'logicle'
  ? `&${p}t=logicle&${p}T=262144&${p}W=0.5&${p}M=4.5&${p}A=0` : `&${p}t=${k}`

// query for a given population on the current axes
function plotQ(pop: string) {
  const z = props.axisFromZero ? 1 : 0
  return `projectUid=${g.projectUid()}&imageUid=${g.imageUid}&valueName=${g.valueName}&popType=${g.popType}` +
    `&x=${encodeURIComponent(xChan.value)}&y=${encodeURIComponent(yChan.value)}` +
    `&pop=${encodeURIComponent(pop)}${axisQ('x', xt.value)}${axisQ('y', yt.value)}&x0=${z}&y0=${z}`
}
const baseQ = computed(() => plotQ(parent.value))

// child gates of this panel's parent that live on the current axis pair → outlines (for edit).
// orientGate is shared with the read-only gating-strategy plot (plots/gateGeometry.ts).
const currentGates = computed(() =>
  g.flat.filter(p => p.parent === parent.value && p.gate)
        .map(p => ({ path: p.path, colour: p.colour, gate: orientGate(p.gate!, xChan.value, yChan.value) }))
        .filter((p): p is { path: string; colour: string; gate: GateSpec } => p.gate !== null))

async function fetchBuf(q: string): Promise<Float32Array> {
  const buf = await (await fetch(`/api/gating/plotdata?${q}`)).arrayBuffer()
  return new Float32Array(buf)
}

// axes/extents/ticks — only when X/Y/transform/parent change (NOT on membership change)
async function fetchMeta() {
  const meta = await (await fetch(`/api/gating/plotmeta?${baseQ.value}`)).json() as {
    xExtent: [number, number]; yExtent: [number, number]
    xTicks: { pos: number; label: string }[]; yTicks: { pos: number; label: string }[] }
  extents.value = { xMin: meta.xExtent[0], xMax: meta.xExtent[1], yMin: meta.yExtent[0], yMax: meta.yExtent[1] }
  viewExtents.value = { ...extents.value }
  xTicks.value = meta.xTicks; yTicks.value = meta.yTicks
}
// just the base population points (cheap; regl redraw is instant → smooth membership updates)
async function fetchPoints() {
  if (!g.imageUid || !xChan.value || !yChan.value) return
  points.value = await fetchBuf(baseQ.value)
}

// full reload: axes + points + layers (axes / parent / image / value-name change)
async function fetchPlot() {
  if (!g.imageUid || !xChan.value || !yChan.value) return
  loading.value = true
  try {
    await fetchMeta()
    await fetchPoints()
    await loadPopLayers()
  } catch (e) {
    log.error(`Gating plot: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
  } finally { loading.value = false }
}

// membership refresh (another plot changed a gate on the pop we're showing) — no axis reload
async function refreshMembership() {
  try {
    if (parent.value !== 'root') await fetchPoints()
    await loadPopLayers()
  } catch (e) {
    log.error(`Gating refresh: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
  }
}

// per-population subset points (server owns membership) for the colour overlay — the pops
// highlighted (eye) for THIS panel in the manager, each plotted on the current axes
async function loadPopLayers() {
  const hl = props.highlight ?? []
  if (!hl.length) { popLayers.value = []; return }
  try {
    popLayers.value = await Promise.all(hl.map(async path =>
      ({ path, colour: g.flat.find(p => p.path === path)?.colour ?? '#22d3ee', points: await fetchBuf(plotQ(path)) })))
  } catch (e) {
    log.error(`Gating pop layers: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
  }
}

function onDraw(geom: Partial<GateSpec>) {
  pending.value = { ...geom, x_channel: xChan.value, y_channel: yChan.value,
                    x_transform: tspec(xt.value), y_transform: tspec(yt.value) }
  // keep the draw tool armed (rectangle/polygon) so you can gate repeatedly without re-selecting it.
  // To adjust a gate without disarming, hold Shift over the plot — GateOverlay grabs/moves/resizes the
  // gate under the cursor while armed (see its onDown/onMove); release Shift to keep drawing.
  newName.value = ''
}
const nameReserved = computed(() => isReservedPopName(newName.value))
async function confirmGate() {
  if (!pending.value || !newName.value.trim() || nameReserved.value) return
  const palette = ['#ef4444','#f59e0b','#10b981','#3b82f6','#a78bfa','#ec4899','#14b8a6','#eab308']
  const ok = await g.addPop(newName.value.trim(), pending.value as GateSpec, parent.value, palette[g.flat.length % palette.length])
  pending.value = null
  if (ok) loadPopLayers()   // new child → its outline is reactive; refresh its colour layer
}

// existing gate moved/resized/vertex-edited on the canvas → persist (server recomputes + broadcasts)
async function onEdit(e: { path: string; gate: GateSpec }) {
  await g.setGate(e.path, e.gate)
}

// export the plot as PNG — GateScatterCell owns the composite (canvas pixels + HTML/canvas2D overlays,
// each layer re-rendered hi-res); we just name the file.
const cell = useTemplateRef<{ exportImage(bg?: string): Promise<string | null> }>('cell')
function exportPng() {
  const stem = `gate_${xChan.value}_${yChan.value}`.replace(/[^\w.-]+/g, '_')
  cell.value?.exportImage('#0d0b1a').then(url => url && downloadDataUrl(`${stem}.png`, url))
}

function ensureChannels() {
  const cols = g.columns
  if (!cols.length) return
  if (!cols.includes(xChan.value)) xChan.value = g.channels[(props.index * 2) % Math.max(1, g.channels.length)] ?? cols[0]
  if (!cols.includes(yChan.value)) yChan.value = g.channels[(props.index * 2 + 1) % Math.max(1, g.channels.length)] ?? cols[Math.min(1, cols.length - 1)]
}
// store readiness (channels/image/segmentation): pick default axes then load. `immediate` so the
// first appearance fetches whether the store became ready BEFORE this panel mounted (values already
// set → fires now) or AFTER (fires again on change) — previously the plot stayed empty on first open
// until the user nudged a dropdown.
watch([() => g.columns, () => g.imageUid, () => g.valueName],
      () => { ensureChannels(); fetchPlot() }, { immediate: true, flush: 'post' })
watch([xChan, yChan, xt, yt, parent, () => props.axisFromZero], fetchPlot)
watch(() => props.highlight, loadPopLayers, { deep: true })
// another plot changed the gate of the population we display (or an ancestor) → refresh smoothly
const parentVersion = computed(() => g.popVersion[parent.value] ?? 0)
watch(parentVersion, refreshMembership)
// a highlighted pop's membership changed elsewhere → refresh just its colour layer
const hlVersion = computed(() => (props.highlight ?? []).reduce((s, p) => s + (g.popVersion[p] ?? 0), 0))
watch(hlVersion, loadPopLayers)
// initial load is handled by the { immediate: true } store-readiness watch above.
</script>

<template>
  <CanvasPanel :index="index" :active="props.active" :arrange="props.arrange" :title="`Plot ${index + 1}`"
               :persist-key="props.persistKey"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <!-- header tools (render mode + gate-draw tools) sit in the panel title bar -->
    <template #actions>
      <RenderModeToggle v-model="renderMode" />
      <span class="ctrl-sep" />
      <button class="cc-btn cc-btn-ghost" :class="{ on: mode === 'rectangle' }" v-tooltip.bottom="'Rectangle gate'"
              @click="mode = mode === 'rectangle' ? 'off' : 'rectangle'"><i class="pi pi-stop" /></button>
      <button class="cc-btn cc-btn-ghost" :class="{ on: mode === 'polygon' }"
              v-tooltip.bottom="'Polygon gate (click vertices, double-click to close)'"
              @click="mode = mode === 'polygon' ? 'off' : 'polygon'"><i class="pi pi-share-alt" /></button>
    </template>
    <!-- utility actions (export) in the footer, like the summary / cluster panels -->
    <template #footer>
      <select class="gp-export" v-tooltip.top="'Export the shown plot'" :disabled="!points"
              @change="($event.target as HTMLSelectElement).value === 'png' && exportPng(); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="png">Image (PNG)</option>
      </select>
    </template>
    <!-- controls: one row per axis (X, Y) + the displayed population, stacked so they don't wrap -->
    <div class="panel-ctrl">
      <label class="ax-row"><span class="ax-lbl">X</span>
        <select class="ax-chan" v-model="xChan"><option v-for="c in g.columns" :key="c" :value="c">{{ g.colLabel(c) }}</option></select>
        <select class="tsel" v-model="xt"><option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select></label>
      <label class="ax-row"><span class="ax-lbl">Y</span>
        <select class="ax-chan" v-model="yChan"><option v-for="c in g.columns" :key="c" :value="c">{{ g.colLabel(c) }}</option></select>
        <select class="tsel" v-model="yt"><option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select></label>
      <label class="ax-row"><span class="ax-lbl">pop</span>
        <select class="ax-chan" v-model="parent" v-tooltip.bottom="'Population to display; new gates are its children'">
        <option v-for="p in parentOptions" :key="p" :value="p">{{ p }}</option></select></label>
    </div>
    <GateScatterCell ref="cell" :points="points" :extents="extents" :view-extents="viewExtents"
                     :x-ticks="xTicks" :y-ticks="yTicks" :gates="currentGates"
                     :x-label="g.colLabel(xChan)" :y-label="g.colLabel(yChan)"
                     :pop-layers="popLayers" :render-mode="renderMode" :show-pops="showPops"
                     :mode="mode" :gate-line-width="gateLineWidth" :gate-labels="gateLabels"
                     :view-tick="viewTick" :loading="loading"
                     @draw="onDraw" @edit="onEdit" @cancel="mode = 'off'">
      <!-- brief affordance: the draw tool stays armed, hold Shift to grab/move/resize a gate -->
      <div v-if="mode !== 'off' && !pending" class="gate-hint">hold <kbd>Shift</kbd> to adjust gates</div>
      <div v-if="pending" class="panel-name">
        <span>new {{ pending.kind }}</span>
        <input v-model="newName" placeholder="name…" autofocus
               :class="{ 'name-invalid': nameReserved }"
               @keyup.enter="confirmGate" @keyup.esc="pending = null" />
        <button class="cc-btn cc-btn-primary" :disabled="!newName.trim() || nameReserved" @click="confirmGate">Add</button>
        <button class="cc-btn cc-btn-ghost" @click="pending = null">×</button>
        <span v-if="nameReserved" class="name-hint">names can't start with “_” (reserved for tracked / clustering)</span>
      </div>
    </GateScatterCell>
  </CanvasPanel>
</template>

<style scoped>
/* the floating chrome (.panel / .panel-head / title / remove) lives in CanvasPanel; the styles
   below are gating-specific: the axis controls, the plot body, ticks/axes, and the header tools
   passed into CanvasPanel's #actions slot (.seg / .ctrl-sep / .cc-btn.on — slot content keeps
   this component's scoped styles). */
/* one row per axis so the controls don't wrap into a floating line as the panel narrows */
.panel-ctrl { display: flex; flex-direction: column; gap: 6px; padding: 6px 8px;
  border-bottom: 1px solid var(--cc-border); font-size: 12px; }
.ax-row { display: flex; align-items: center; gap: 6px; }
.ax-lbl { width: 1.8rem; color: var(--cc-text-dim); flex-shrink: 0; }
/* fixed widths so the controls don't stretch when the plot is resized */
.ax-chan { width: 9rem; flex: none; }
.ax-row .tsel { flex-shrink: 0; }
.panel-ctrl label { display: flex; align-items: center; gap: 4px; color: var(--cc-text-dim); font-size: 12px; }
/* fixed widths so the bar doesn't reflow when the selected option text changes; the rest
   (background, border, chevron, focus) comes from the global form base in style.css */
.panel-ctrl select { width: 8rem; font-size: 12px; padding-top: 2px; padding-bottom: 2px; }
.panel-ctrl select.tsel { width: 5.5rem; }
.panel-ctrl select:focus { outline: none; border-color: var(--cc-accent); }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 3px 8px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button.on { background: var(--cc-accent); color: #fff; }
.cc-btn.on { border-color: var(--cc-accent); color: var(--cc-accent); }
.gp-export { font-size: 12px; max-width: 7rem; }
/* the plot body (scatter/layers/gate + ticks/axes + PNG export) lives in GateScatterCell now. */
.panel-name { position: absolute; top: 4px; left: 4px; display: flex; align-items: center; gap: 5px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-accent); border-radius: 4px; padding: 4px 6px; font-size: 11px; }
.panel-name input { background: var(--cc-bg); color: var(--cc-text); border: 1px solid var(--cc-border); border-radius: 3px; padding: 1px 5px; width: 90px; }
.panel-name input.name-invalid { border-color: var(--cc-danger, #ef4444); }
.name-hint { color: var(--cc-danger, #ef4444); font-size: 10px; max-width: 150px; line-height: 1.2; }
/* subtle draw-mode affordance (top-right of the plot); mirrors .panel-name but muted and non-interactive */
.gate-hint { position: absolute; top: 4px; right: 4px; pointer-events: none; display: flex; align-items: center; gap: 4px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 4px; padding: 2px 6px;
  font-size: 10px; color: var(--cc-text-dim); }
.gate-hint kbd { font: inherit; background: var(--cc-surface-2); border: 1px solid var(--cc-border); border-radius: 3px;
  padding: 0 3px; color: var(--cc-text); }
</style>
