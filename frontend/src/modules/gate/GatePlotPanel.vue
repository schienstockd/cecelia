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
import { downloadDataUrl } from '../../plots/export'
import { childGateSignature } from '../../utils/childGateSig'
import { coalesceByKey } from '../../utils/coalesce'
import { useDataRefresh } from '../../composables/useDataRefresh'

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
// server flag: a track-grained plot (popType track/trackclust) on a segmentation that hasn't been
// tracked → show a "track first" message instead of an empty plot. Set from plotmeta.
const notTracked = ref(false)
const pending = ref<Partial<GateSpec> | null>(null)
const newName = ref('')

const parentOptions = computed(() => ['root', ...g.flat.map(p => p.path)])
const tspec = (k: Kind): TransformSpec => k === 'logicle' ? { kind: k, T: 262144, W: 0.5, M: 4.5, A: 0 } : { kind: k }
const axisQ = (p: string, k: Kind) => k === 'logicle'
  ? `&${p}t=logicle&${p}T=262144&${p}W=0.5&${p}M=4.5&${p}A=0` : `&${p}t=${k}`

// query for a given population on given axis transforms
function plotQ(pop: string, xk: Kind, yk: Kind) {
  const z = props.axisFromZero ? 1 : 0
  return `projectUid=${g.projectUid()}&imageUid=${g.imageUid}&valueName=${g.valueName}&popType=${g.popType}` +
    `&x=${encodeURIComponent(xChan.value)}&y=${encodeURIComponent(yChan.value)}` +
    `&pop=${encodeURIComponent(pop)}${axisQ('x', xk)}${axisQ('y', yk)}&x0=${z}&y0=${z}&autoLinear=1`
}
// meta is fetched with the PREFERRED transforms (xt/yt); the server decides what's actually usable and
// reports it as usedX/usedY (a non-linear transform that would collapse a bounded/0–1 measure → linear).
const metaQ = computed(() => plotQ(parent.value, xt.value, yt.value))

// The transform the server actually USED for each axis (from plotmeta). It differs from the preferred
// xt/yt when the measure's range can't use it (auto-linearised): the axis select then shows this and
// goes amber. It reverts to the preference automatically on a compatible measure (server re-decides).
const effXt = ref<Kind>(xt.value)
const effYt = ref<Kind>(yt.value)
const xCoerced = computed(() => effXt.value !== xt.value)
const yCoerced = computed(() => effYt.value !== yt.value)
// the axis dropdown DISPLAYS the effective transform; changing it sets the user's PREFERENCE (persisted)
const xtSel = computed<Kind>({ get: () => effXt.value, set: v => { xt.value = v } })
const ytSel = computed<Kind>({ get: () => effYt.value, set: v => { yt.value = v } })

// child-gate outlines for the current axes, already projected into the effective display transform by
// the server (plotmeta) — the client has no transform math, so it can't re-project a gate drawn under a
// different transform onto these axes. Colour/path arrive with them; we attach the current channels +
// effective transforms so a drag-edit round-trips (a moved gate is re-stored in the displayed transform).
interface SrvGate { path: string; colour: string; kind: 'rectangle' | 'polygon'
  x_min?: number; x_max?: number; y_min?: number; y_max?: number; vertices?: [number, number][] }
const serverGates = ref<SrvGate[]>([])
const currentGates = computed(() => serverGates.value.map(s => ({
  path: s.path, colour: s.colour,
  gate: { kind: s.kind, x_channel: xChan.value, y_channel: yChan.value,
          x_transform: tspec(effXt.value), y_transform: tspec(effYt.value),
          x_min: s.x_min, x_max: s.x_max, y_min: s.y_min, y_max: s.y_max, vertices: s.vertices } as GateSpec })))

async function fetchBuf(q: string): Promise<Float32Array> {
  const buf = await (await fetch(`/api/gating/plotdata?${q}`)).arrayBuffer()
  return new Float32Array(buf)
}

// axes/extents/ticks + effective transforms + projected gate outlines — only when X/Y/transform/parent
// change (NOT on membership change). Sends the PREFERRED transform; adopts what the server used.
async function fetchMeta() {
  const key = metaQ.value                    // snapshot: drop the response if the view moved on
  const meta = await (await fetch(`/api/gating/plotmeta?${key}`)).json() as {
    xExtent: [number, number]; yExtent: [number, number]
    xTicks: { pos: number; label: string }[]; yTicks: { pos: number; label: string }[]
    usedX?: Kind; usedY?: Kind; gates?: SrvGate[]; tracked?: boolean }
  // A newer fetch (image/segmentation/axis/parent switch) is already in flight — a late stale meta
  // would otherwise overwrite the fresh extents/ticks/gates (last-writer race), leaving the plot on
  // the wrong axes or blank until the user nudged a control. (Mirrors fetchGatesFor's key guard.)
  if (key !== metaQ.value) return
  notTracked.value = meta.tracked === false
  extents.value = { xMin: meta.xExtent[0], xMax: meta.xExtent[1], yMin: meta.yExtent[0], yMax: meta.yExtent[1] }
  viewExtents.value = { ...extents.value }
  xTicks.value = meta.xTicks; yTicks.value = meta.yTicks
  effXt.value = meta.usedX ?? xt.value
  effYt.value = meta.usedY ?? yt.value
  serverGates.value = meta.gates ?? []
}
// refresh ONLY the server-projected child-gate outlines — gates come from plotmeta now, so a gate
// added/edited/deleted (here or on another plot) needs a re-fetch to appear. Keeps the current axes
// (no extent/tick reset), so it's cheap enough to run on every membership change.
// Coalesced (see utils/coalesce): adding a gate triggers this both from confirmGate (awaited, so the
// outline is painted before the call returns) and from the childGateSig watcher. metaQ is child-set-
// independent and the watcher flush always beats the network round-trip, so both share one in-flight
// request — one plotmeta call, not two. Keyed on metaQ so an axis change (different key) never reuses
// a stale in-flight promise.
const fetchGatesFor = coalesceByKey(async (key: string) => {
  const meta = await (await fetch(`/api/gating/plotmeta?${key}`)).json() as { gates?: SrvGate[] }
  // drop a late response whose axes/parent (its metaQ key) no longer match the current view — else a
  // fetchGates in flight for the old axes could overwrite a fresh fetchMeta's outlines (last-writer race).
  if (key === metaQ.value) serverGates.value = meta.gates ?? []
})
function fetchGates(): Promise<void> {
  if (!g.imageUid || !xChan.value || !yChan.value) return Promise.resolve()
  return fetchGatesFor(metaQ.value)
}
// just the base population points (cheap; regl redraw is instant → smooth membership updates). Uses the
// EFFECTIVE transforms so the cloud matches the extent + projected gates fetchMeta set.
async function fetchPoints() {
  if (!g.imageUid || !xChan.value || !yChan.value) return
  const key = metaQ.value                    // snapshot the view; drop a stale response
  const buf = await fetchBuf(plotQ(parent.value, effXt.value, effYt.value))
  // same last-writer guard as fetchMeta: if the image/segmentation/axis/parent changed while this
  // was in flight, an out-of-order resolve must NOT clobber the current cloud (the intermittent
  // "blank until I toggle a control" gap on image switch).
  if (key !== metaQ.value) return
  points.value = buf
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
    await fetchGates()                    // outlines are server-projected → refresh them on any gate change
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
      ({ path, colour: g.flat.find(p => p.path === path)?.colour ?? '#22d3ee',
         points: await fetchBuf(plotQ(path, effXt.value, effYt.value)) })))
  } catch (e) {
    log.error(`Gating pop layers: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
  }
}

function onDraw(geom: Partial<GateSpec>) {
  // stamp the EFFECTIVE transform (what the axis is actually displayed in), not the preference — the
  // geometry was drawn in that space, so this is what membership must test against.
  pending.value = { ...geom, x_channel: xChan.value, y_channel: yChan.value,
                    x_transform: tspec(effXt.value), y_transform: tspec(effYt.value) }
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
  // await the outline so it's painted before we return; the childGateSig watcher fires too but
  // coalesces into this same request (see fetchGates). loadPopLayers pulls the new pop's colour layer.
  if (ok) {
    try { await fetchGates() } catch (e) {
      log.error(`Gating add: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
    }
    loadPopLayers()
  }

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
// the outlines we draw are the DIRECT CHILDREN of `parent`. Their set/geometry changes when a child
// is added, deleted, or edited — here, on another plot, from napari, or via a WS broadcast — but that
// bumps the CHILD's popVersion, never the displayed parent's, so neither parentVersion nor fetchPlot
// (axis/parent watch) fires and a deleted child's outline would linger. Watch a signature of the
// parent's children and refresh on any change. Use fetchMeta (not the outlines-only fetchGates) so the
// autoscale re-runs: dragging a gate BEYOND the current axes regrows the extent to fit it, so the gate
// snaps back on-plot on release — no more toggling the segmentation to force a redraw. fetchMeta also
// returns the outlines, so this covers add/delete too. Signature logic → utils/childGateSig.ts (tested).
const childGateSig = computed(() => childGateSignature(g.flat, parent.value))
watch(childGateSig, fetchMeta)
// a highlighted pop's membership changed elsewhere → refresh just its colour layer
const hlVersion = computed(() => (props.highlight ?? []).reduce((s, p) => s + (g.popVersion[p] ?? 0), 0))
watch(hlVersion, loadPopLayers)
// a task finished on the image we show (e.g. tracking → track_id appears, so a track plot goes from
// "not tracked" to populated) → full reload. Same universal mechanism every other plot uses; gated by
// the global autoRefreshOnTask setting. Interactive gating was the one plot family not wired in.
useDataRefresh(() => (g.imageUid ? [g.imageUid] : []), () => { fetchPlot() })
// initial load is handled by the { immediate: true } store-readiness watch above.
</script>

<template>
  <!-- auto-hide OFF: you draw gates on this canvas constantly, so the render-mode / gate tools stay
       in flow rather than popping over the plot on hover -->
  <CanvasPanel :index="index" :active="props.active" :arrange="props.arrange" :title="`Plot ${index + 1}`"
               :persist-key="props.persistKey" :auto-hide="false"
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
        <select class="tsel" :class="{ 'tsel-amber': xCoerced }" v-model="xtSel" v-tooltip.bottom="'Axis transform'">
          <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select>
        <i v-if="xCoerced" class="pi pi-exclamation-triangle ax-warn"
           v-tooltip.bottom="`${g.colLabel(xChan)}’s range is too small for ${xt} — shown linear`" /></label>
      <label class="ax-row"><span class="ax-lbl">Y</span>
        <select class="ax-chan" v-model="yChan"><option v-for="c in g.columns" :key="c" :value="c">{{ g.colLabel(c) }}</option></select>
        <select class="tsel" :class="{ 'tsel-amber': yCoerced }" v-model="ytSel" v-tooltip.bottom="'Axis transform'">
          <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select>
        <i v-if="yCoerced" class="pi pi-exclamation-triangle ax-warn"
           v-tooltip.bottom="`${g.colLabel(yChan)}’s range is too small for ${yt} — shown linear`" /></label>
      <label class="ax-row"><span class="ax-lbl">pop</span>
        <select class="ax-chan" v-model="parent" v-tooltip.bottom="'Population to display; new gates are its children'">
        <option v-for="p in parentOptions" :key="p" :value="p">{{ p }}</option></select>
        <!-- affordance beside the pop selector (out of the plot): the draw tool stays armed, hold Shift to grab/move/resize -->
        <span v-if="mode !== 'off' && !pending" class="gate-hint">hold <kbd>Shift</kbd> to adjust gates</span></label>
    </div>
    <GateScatterCell ref="cell" :points="points" :extents="extents" :view-extents="viewExtents"
                     :x-ticks="xTicks" :y-ticks="yTicks" :gates="currentGates"
                     :x-label="g.colLabel(xChan)" :y-label="g.colLabel(yChan)"
                     :pop-layers="popLayers" :render-mode="renderMode" :show-pops="showPops"
                     :mode="mode" :gate-line-width="gateLineWidth" :gate-labels="gateLabels"
                     :view-tick="viewTick" :loading="loading"
                     @draw="onDraw" @edit="onEdit" @cancel="mode = 'off'">
      <!-- untracked segmentation on a track plot: nothing to show, point the user at tracking -->
      <div v-if="notTracked" class="gate-empty">
        <i class="pi pi-share-alt" />
        <span>Not tracked yet — run tracking on this segmentation first.</span>
      </div>
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
/* amber: this axis' preferred transform was auto-linearised because the measure's range can't use it */
.panel-ctrl select.tsel.tsel-amber { border-color: #f59e0b; color: #f59e0b; }
.ax-warn { color: #f59e0b; font-size: 0.7rem; flex-shrink: 0; cursor: help; }
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
/* inline affordance beside the pop selector (was overlaid on the plot, which obscured gating) */
.gate-hint { margin-left: 2px; pointer-events: none; display: inline-flex; align-items: center; gap: 4px;
  font-size: 10px; color: var(--cc-text-dim); white-space: nowrap; }

/* centred empty-state over the plot: track-grained pop type on an untracked segmentation */
.gate-empty { position: absolute; inset: 0; margin: auto; width: max-content; height: max-content;
  display: flex; align-items: center; gap: 6px; padding: 8px 12px; pointer-events: none;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 6px;
  font-size: 12px; color: var(--cc-text-dim); }
.gate-hint kbd { font: inherit; background: var(--cc-surface-2); border: 1px solid var(--cc-border); border-radius: 3px;
  padding: 0 3px; color: var(--cc-text); }
</style>
