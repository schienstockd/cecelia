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
import ScatterGL from '../../components/plots/ScatterGL.vue'
import PlotLayers, { type PopLayer } from '../../components/plots/PlotLayers.vue'
import GateOverlay from '../../components/plots/GateOverlay.vue'
import { plotHostToImageURL, downloadDataUrl } from '../../plots/export'

const props = defineProps<{
  index: number; active: boolean; parent: string; highlight: string[]
  gateLineWidth: number; gateLabels: boolean; axisFromZero: boolean
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
const xChan = ref(''); const yChan = ref('')
// track properties (motility, per-track aggregates) are plain continuous values → linear by
// default; flow intensities default to logicle (FlowJo). User can switch either per axis.
const defaultTransform: Kind = g.popType === 'track' ? 'linear' : 'logicle'
const xt = ref<Kind>(defaultTransform); const yt = ref<Kind>(defaultTransform)
// displayed population is owned by GatingPlots (per-panel) so the manager can highlight it
const parent = computed({ get: () => props.parent, set: v => emit('update:parent', v) })
const mode = ref<'off' | 'rectangle' | 'polygon'>('off')
const renderMode = ref<'points' | 'contour'>('points')
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

// Orient a child's gate to the current plot axes. The gate matches if its two channels are the
// plot's two channels in EITHER order (R: .flowMatchGatingParamsForPop, order-independent); when
// swapped, transpose the coords so it draws correctly. Returns null if it's a different axis pair.
function orientGate(gate: GateSpec, xc: string, yc: string): GateSpec | null {
  if (gate.x_channel === xc && gate.y_channel === yc) return gate
  if (gate.x_channel === yc && gate.y_channel === xc) {
    const base = { ...gate, x_channel: xc, y_channel: yc, x_transform: gate.y_transform, y_transform: gate.x_transform }
    return gate.kind === 'rectangle'
      ? { ...base, x_min: gate.y_min, x_max: gate.y_max, y_min: gate.x_min, y_max: gate.x_max }
      : { ...base, vertices: gate.vertices?.map(v => [v[1], v[0]] as [number, number]) }
  }
  return null
}
// child gates of this panel's parent that live on the current axis pair → outlines (for edit)
const currentGates = computed(() =>
  g.flat.filter(p => p.parent === parent.value && p.gate)
        .map(p => ({ path: p.path, colour: p.colour, gate: orientGate(p.gate!, xChan.value, yChan.value) }))
        .filter((p): p is { path: string; colour: string; gate: GateSpec } => p.gate !== null))

// base render look: density for plain points; dim/flat backdrop for contour or pop-colour modes
const baseColorMode = computed<'density' | 'flat'>(() =>
  renderMode.value === 'points' && !showPops.value ? 'density' : 'flat')
const baseOpacity = computed(() => renderMode.value === 'contour' ? 0.22 : showPops.value ? 0.35 : 1)
const basePointSize = computed(() => renderMode.value === 'contour' ? 2 : 3)

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
  // keep the draw tool armed (rectangle/polygon) so you can gate repeatedly without re-selecting it;
  // it's only turned off explicitly (toggle the tool button, or cancel/Esc).
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

// ticks positioned against the LIVE view extents so labels track pan/zoom
const tickX = (pos: number) => `${((pos - viewExtents.value.xMin) / Math.max(1e-9, viewExtents.value.xMax - viewExtents.value.xMin)) * 100}%`
const tickY = (pos: number) => `${(1 - (pos - viewExtents.value.yMin) / Math.max(1e-9, viewExtents.value.yMax - viewExtents.value.yMin)) * 100}%`
// abbreviate big numbers (262144 → 262.1k) so axis labels don't crowd; leave small ones as-is
function fmtTick(label: string): string {
  const n = parseFloat(label)
  if (!isFinite(n)) return label
  const a = Math.abs(n), trim = (s: string) => s.replace(/\.0$/, '')
  if (a >= 1e9) return trim((n / 1e9).toFixed(1)) + 'G'
  if (a >= 1e6) return trim((n / 1e6).toFixed(1)) + 'M'
  if (a >= 1e3) return trim((n / 1e3).toFixed(1)) + 'k'
  return label
}

// export the plot area (WebGL scatter + contour/pop layers + gate overlay + axis ticks + labels) as a
// PNG — composited via plots/export.ts (canvas pixels + HTML/canvas2D overlays). The capture host is
// `.plot-capture` (which INCLUDES the axis-label margins), not `.panel-plot`, so the x/y axis names
// aren't clipped off the bottom/left edge. The scatter is re-rendered hi-res so points stay crisp.
const plotEl = useTemplateRef<HTMLElement>('plotEl')
type LayerExport = { exportCanvas(scale: number): Promise<HTMLCanvasElement | null>; getCanvas(): HTMLCanvasElement | null }
const scatterRef = useTemplateRef<LayerExport>('scatterRef')
const layersRef = useTemplateRef<LayerExport>('layersRef')
const overlayRef = useTemplateRef<LayerExport>('overlayRef')
// each stacked canvas (WebGL scatter + canvas2D contours/gates) re-renders itself at export scale so
// nothing is upscaled from the screen-DPR backing store; route each live canvas to its layer.
const hiRes = async (cv: HTMLCanvasElement, scale: number) => {
  for (const r of [scatterRef, layersRef, overlayRef]) {
    if (cv === r.value?.getCanvas()) return (await r.value?.exportCanvas(scale)) ?? null
  }
  return null
}
function exportPng() {
  const stem = `gate_${xChan.value}_${yChan.value}`.replace(/[^\w.-]+/g, '_')
  plotHostToImageURL(plotEl.value, '#0d0b1a', { hiRes }).then(url => url && downloadDataUrl(`${stem}.png`, url))
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
      <div class="seg" v-tooltip.bottom="'Render: pseudocolour points / density contour'">
        <button :class="{ on: renderMode === 'points' }" @click="renderMode = 'points'"><i class="pi pi-circle-fill" /></button>
        <button :class="{ on: renderMode === 'contour' }" @click="renderMode = 'contour'"><i class="pi pi-chart-line" /></button>
      </div>
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
    <div ref="plotEl" class="plot-capture">
     <div class="panel-plot">
      <ScatterGL ref="scatterRef" :points="points" :extents="extents" :color-mode="baseColorMode"
                 :opacity="baseOpacity" :point-size="basePointSize" />
      <PlotLayers ref="layersRef" :view-extents="viewExtents" :render-mode="renderMode" :base-points="points"
                  :pop-layers="popLayers" :show-pops="showPops" :view-tick="viewTick" />
      <GateOverlay ref="overlayRef" :extents="viewExtents" :mode="mode" :gates="currentGates" :view-tick="viewTick"
                   :line-width="gateLineWidth" :show-labels="gateLabels"
                   @draw="onDraw" @edit="onEdit" @cancel="mode = 'off'" />
      <span class="axisline axisline-x" />
      <span class="axisline axisline-y" />
      <span v-for="t in xTicks" :key="'x'+t.pos" class="xtick" :style="{ left: tickX(t.pos) }">
        <span class="xtick-mark" /><span class="xtick-lbl">{{ fmtTick(t.label) }}</span>
      </span>
      <span v-for="t in yTicks" :key="'y'+t.pos" class="ytick" :style="{ top: tickY(t.pos) }">
        <span class="ytick-lbl">{{ fmtTick(t.label) }}</span><span class="ytick-mark" />
      </span>
      <span class="axis-x">{{ g.colLabel(xChan) }}</span>
      <span class="axis-y">{{ g.colLabel(yChan) }}</span>
      <div v-if="loading" class="panel-loading">…</div>
      <div v-if="pending" class="panel-name">
        <span>new {{ pending.kind }}</span>
        <input v-model="newName" placeholder="name…" autofocus
               :class="{ 'name-invalid': nameReserved }"
               @keyup.enter="confirmGate" @keyup.esc="pending = null" />
        <button class="cc-btn cc-btn-primary" :disabled="!newName.trim() || nameReserved" @click="confirmGate">Add</button>
        <button class="cc-btn cc-btn-ghost" @click="pending = null">×</button>
        <span v-if="nameReserved" class="name-hint">names can't start with “_” (reserved for tracked / clustering)</span>
      </div>
     </div>
    </div>
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
/* capture host for PNG export: the axis labels/ticks live in this padding (relative to .panel-plot,
   at negative offsets), so exporting THIS element — not .panel-plot — includes the x/y axis names
   instead of clipping them at the edge (#00061). The label-space that used to be .panel-plot's
   margin is now this padding. min-height is modest so plot + padding + footer all fit inside the
   default panel height without the panel's overflow:hidden clipping the bottom label. */
.plot-capture { position: relative; flex: 1; min-height: 218px; min-width: 0; display: flex; box-sizing: border-box;
  padding: 18px 22px 50px 84px; }
/* min-width:0 so this flex-row item can shrink below the scatter canvas's intrinsic width — without
   it the panel grows on resize-right but won't shrink back on resize-left. */
.panel-plot { position: relative; flex: 1; min-height: 150px; min-width: 0; }
.axisline { position: absolute; background: var(--cc-border); pointer-events: none; }
.axisline-x { left: 0; right: 0; bottom: 0; height: 1px; }
.axisline-y { left: 0; top: 0; bottom: 0; width: 1px; }
/* x ticks: mark hangs below the baseline, label under it */
.xtick { position: absolute; bottom: 0; transform: translate(-50%, 100%); display: flex; flex-direction: column; align-items: center; pointer-events: none; }
.xtick-mark { width: 1px; height: 5px; background: var(--cc-text-dim); }
.xtick-lbl { margin-top: 3px; font-size: 10px; color: var(--cc-text-dim); white-space: nowrap; }
/* y ticks: label then mark, anchored so the mark touches the left axis */
.ytick { position: absolute; left: 0; transform: translate(-100%, -50%); display: flex; align-items: center; pointer-events: none; }
.ytick-mark { width: 5px; height: 1px; background: var(--cc-text-dim); }
.ytick-lbl { margin-right: 3px; font-size: 10px; color: var(--cc-text-dim); white-space: nowrap; }
.axis-x { position: absolute; bottom: -40px; left: 50%; transform: translateX(-50%); font-size: 13px; font-weight: 600; color: var(--cc-text); }
/* vertical text via writing-mode (rotate's origin offsets by half the text width → overlap) */
.axis-y { position: absolute; left: -66px; top: 50%; transform: translateY(-50%) rotate(180deg);
  writing-mode: vertical-rl; font-size: 13px; font-weight: 600; color: var(--cc-text); }
.gp-export { font-size: 12px; max-width: 7rem; }
.panel-loading { position: absolute; top: 4px; right: 6px; font-size: 11px; color: var(--cc-text-dim); }
.panel-name { position: absolute; top: 4px; left: 4px; display: flex; align-items: center; gap: 5px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-accent); border-radius: 4px; padding: 4px 6px; font-size: 11px; }
.panel-name input { background: var(--cc-bg); color: var(--cc-text); border: 1px solid var(--cc-border); border-radius: 3px; padding: 1px 5px; width: 90px; }
.panel-name input.name-invalid { border-color: var(--cc-danger, #ef4444); }
.name-hint { color: var(--cc-danger, #ef4444); font-size: 10px; max-width: 150px; line-height: 1.2; }
</style>
