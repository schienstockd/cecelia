<!--
  One modular gating plot (mirrors the old flowPlotManager "gating box"): its own X/Y column +
  transform, parent-population select, render mode + gate-draw tools, a 2D-canvas dot plot
  with a contour/population layer (PlotLayers) and a canvas2D gate overlay (GateOverlay), plus
  inline naming to persist a drawn shape.

  Render modes (R: cciaConf fcs.gating.plotTypes — pseudocolour/contour/raster):
   - points   → FlowJo pseudocolour (density-coloured points)
   - contour  → density contours over faint points
  Independent "pop colours" toggle overlays the visible child populations in their colours
  (works with either mode). The active panel (orange border) also follows the population the
  user selects in the manager.

  A third measure can be painted onto the dots as COLOUR (FlowJo's colour-by-parameter) — pick it in
  the `colour` row. It replaces the density pseudocolour in points mode and brings a colour bar;
  the ramp's range comes from the whole dataset, so it doesn't re-map as you walk the tree.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef } from 'vue'
import { useGatingStore, type GateSpec, type TransformSpec } from '../../stores/gating'
import { popNameError } from '../../utils/popName'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import GateScatterCell from '../../components/plots/GateScatterCell.vue'
import RenderModeToggle, { type RenderMode } from '../../components/plots/RenderModeToggle.vue'
import ChipSelect, { type ChipOption } from '../../components/ChipSelect.vue'
import type { PopLayer } from '../../components/plots/PlotLayers.vue'
import { downloadDataUrl, downloadText, rowsToCsv, svgSizeWarning } from '../../plots/export'
import { childGateSignature } from '../../utils/childGateSig'
import { axisLabelWithUnit, isImageYAxis } from '../../utils/gatingAxes'
import { measureGroups, groupedCols } from '../../utils/measureGroups'
import { coalesceByKey } from '../../utils/coalesce'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { transformOverride, overrideTooltip } from '../../plots/autoOverride'
import { splitXYZ } from '../../plots/valueColour'

const props = defineProps<{
  index: number; active: boolean; parent: string; highlight: string[]
  gateLineWidth: number; gateLabels: boolean; axisFromZero: boolean; dotSize: number
  // persisted per-plot axis config (owned by GatingPlots' PlotState) — channels, transforms, render
  // mode. Read/written directly like the summary panels' `ui` bag so these survive navigation.
  ui: { x?: string; y?: string; xt?: 'linear' | 'log' | 'asinh' | 'logicle'
        yt?: 'linear' | 'log' | 'asinh' | 'logicle'; renderMode?: RenderMode
        // colour-by: the third measure painted as the dot colour ('' = density pseudocolour)
        z?: string; zt?: 'linear' | 'log' | 'asinh' | 'logicle' }
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
// the ONE default-scale rule, shared with the pairs matrix and both colour-by rows (store:
// `defaultTransformFor` — logicle for a flow intensity, linear for a track property or a raw coordinate)
const axisDefaultTransform = (col: string): Kind => g.defaultTransformFor(col)
// axis config reads/writes the persisted `ui` bag (owned by GatingPlots) so it survives remount.
// Picking a NEW axis re-derives its transform (linear for spatial/centroid, logicle for flow) — the
// transform follows the parameter, FlowJo-style. Without this a once-set transform sticks across axis
// changes (e.g. logicle stays when you switch back to centroid_x). Only fires on user picks via the
// axis <select>'s v-model setter; loading a saved gate mutates ui.x/ui.xt directly (GatingPlots
// openPop), so a gate keeps the transform it was drawn in.
const xChan = computed({ get: () => props.ui.x ?? '', set: v => { props.ui.x = v; props.ui.xt = axisDefaultTransform(v) } })
const yChan = computed({ get: () => props.ui.y ?? '', set: v => { props.ui.y = v; props.ui.yt = axisDefaultTransform(v) } })
const xt = computed<Kind>({ get: () => props.ui.xt ?? axisDefaultTransform(xChan.value), set: v => { props.ui.xt = v } })
const yt = computed<Kind>({ get: () => props.ui.yt ?? axisDefaultTransform(yChan.value), set: v => { props.ui.yt = v } })
// COLOUR BY (optional third measure). Same measure list and same transform default as an axis — a
// marker is colour-ramped in logicle like it is plotted, a centroid linearly — because the ramp is an
// axis in everything but geometry. '' = off (dots keep their local-density pseudocolour).
const zChan = computed({ get: () => props.ui.z ?? '', set: v => { props.ui.z = v; props.ui.zt = v ? axisDefaultTransform(v) : undefined } })
const zt = computed<Kind>({ get: () => props.ui.zt ?? axisDefaultTransform(zChan.value), set: v => { props.ui.zt = v } })
const renderMode = computed<RenderMode>({ get: () => props.ui.renderMode ?? 'points', set: v => { props.ui.renderMode = v } })
// displayed population is owned by GatingPlots (per-panel) so the manager can highlight it
const parent = computed({ get: () => props.parent, set: v => emit('update:parent', v) })
// draw tool is transient interaction state (not persisted): reopening a plot shouldn't leave a tool armed
const mode = ref<'off' | 'rectangle' | 'polygon'>('off')
// draw-tool selector: 'off' maps to the ChipSelect empty selection ('' via allowEmpty — re-clicking
// the armed tool disarms it back to 'off', preserving the old per-button toggle behaviour).
const DRAW_MODES: ChipOption[] = [
  { value: 'rectangle', label: '', icon: 'pi pi-stop',      tip: 'Rectangle gate' },
  { value: 'polygon',   label: '', icon: 'pi pi-share-alt', tip: 'Polygon gate (click vertices, double-click to close)' },
]
// pop-colour overlay is driven by the populations checked for THIS panel in the manager
const showPops = computed(() => (props.highlight?.length ?? 0) > 0)

const points = ref<Float32Array | null>(null)
const extents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })            // fixed (full data range)
const viewExtents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })        // = extents (camera fixed)
const viewTick = ref(0)
const xUnit = ref(''); const yUnit = ref(''); const zUnit = ref('')   // served by plotmeta; '' when the measure has no length unit
const xTicks = ref<{ pos: number; label: string }[]>([])
const yTicks = ref<{ pos: number; label: string }[]>([])
const popLayers = ref<PopLayer[]>([])
// colour-by: one transformed value per base point, plus the ramp's whole-dataset range and its
// raw-value labels (both served by plotmeta — the client has no transform math to invert a logicle
// ramp itself). Cleared whenever the colour measure is off.
const baseValues = ref<Float32Array | null>(null)
const valueExtent = ref<[number, number] | null>(null)
const valueTicks = ref<{ pos: number; label: string }[]>([])
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

// Colour-by paints in the two PER-EVENT modes: `points` (each dot takes the ramp) and `binned` (the
// mean of the measure per cell). Contour rings and the outlier tail describe a distribution, not
// per-cell values, so there is nothing to colour there — and the third column stays off the wire.
const colourOn = computed(() =>
  !!zChan.value && (renderMode.value === 'points' || renderMode.value === 'binned'))
// query for a given population on given axis transforms. `zk` adds the colour-by measure (a third
// value per point); the child-POP overlays never ask for it — they're drawn in the pop's own colour.
function plotQ(pop: string, xk: Kind, yk: Kind, zk?: Kind) {
  const z0 = props.axisFromZero ? 1 : 0
  return `projectUid=${g.projectUid()}&imageUid=${g.imageUid}&valueName=${g.valueName}&popType=${g.popType}` +
    `&x=${encodeURIComponent(xChan.value)}&y=${encodeURIComponent(yChan.value)}` +
    `&pop=${encodeURIComponent(pop)}${axisQ('x', xk)}${axisQ('y', yk)}&x0=${z0}&y0=${z0}&autoLinear=1` +
    (zk && colourOn.value ? `&z=${encodeURIComponent(zChan.value)}${axisQ('z', zk)}` : '')
}
// meta is fetched with the PREFERRED transforms (xt/yt/zt); the server decides what's actually usable
// and reports it as usedX/usedY/usedZ (a non-linear transform that would collapse a bounded/0–1
// measure → linear).
const metaQ = computed(() => plotQ(parent.value, xt.value, yt.value, zt.value))

// The transform the server actually USED for each axis (from plotmeta). It differs from the preferred
// xt/yt when the measure's range can't use it (auto-linearised): the axis select then shows this and
// goes amber. It reverts to the preference automatically on a compatible measure (server re-decides).
const effXt = ref<Kind>(xt.value)
const effYt = ref<Kind>(yt.value)
const effZt = ref<Kind>(zt.value)
// the shared "we substituted a setting" shape — one wording, one amber marker, and the WHY is never
// optional (the select's tooltip used to say only "Axis transform"). See plots/autoOverride.ts.
const xOverride = computed(() => transformOverride(xt.value, effXt.value))
const yOverride = computed(() => transformOverride(yt.value, effYt.value))
const zOverride = computed(() => colourOn.value ? transformOverride(zt.value, effZt.value) : null)
// the axis dropdown DISPLAYS the effective transform; changing it sets the user's PREFERENCE (persisted)
const xtSel = computed<Kind>({ get: () => effXt.value, set: v => { xt.value = v } })
const ytSel = computed<Kind>({ get: () => effYt.value, set: v => { yt.value = v } })
const ztSel = computed<Kind>({ get: () => effZt.value, set: v => { zt.value = v } })

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
    usedX?: Kind; usedY?: Kind; gates?: SrvGate[]; tracked?: boolean
    xUnit?: string; yUnit?: string; zUnit?: string
    zExtent?: [number, number] | null; zTicks?: { pos: number; label: string }[]; usedZ?: Kind }
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
  // when there is no colour measure the select just shows the preference — reporting the server's
  // "linear" default for a ramp nobody asked for would amber a control for no reason
  effZt.value = colourOn.value ? (meta.usedZ ?? zt.value) : zt.value
  // the ramp's range is the server's (whole dataset, in the transform it actually used); null means it
  // has nothing to colour by — the dots fall back to density rather than to a made-up range
  valueExtent.value = colourOn.value ? (meta.zExtent ?? null) : null
  valueTicks.value = colourOn.value ? (meta.zTicks ?? []) : []
  // the unit the SERVER put the values in (µm / px for a spatial axis, '' otherwise) — never guessed
  // here, so the axis label can't claim µm while the numbers are pixels
  xUnit.value = meta.xUnit ?? ''; yUnit.value = meta.yUnit ?? ''; zUnit.value = meta.zUnit ?? ''
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
  // plotmeta already answered `tracked: false` — a track-grained plot of an untracked segmentation
  // has no points, so don't ask for them (the server's own comment says the client skips the empty
  // data reads; only the message half was wired up). CLEAR them: leaving the previous image's cloud
  // under the "Not tracked yet" message would read as data.
  if (notTracked.value) { points.value = new Float32Array(0); baseValues.value = null; return }
  const key = metaQ.value                    // snapshot the view; drop a stale response
  const colour = colourOn.value              // snapshot too: the response's stride depends on it
  const buf = await fetchBuf(plotQ(parent.value, effXt.value, effYt.value, effZt.value))
  // same last-writer guard as fetchMeta: if the image/segmentation/axis/parent changed while this
  // was in flight, an out-of-order resolve must NOT clobber the current cloud (the intermittent
  // "blank until I toggle a control" gap on image switch).
  if (key !== metaQ.value) return
  // colour-by → the body is [x,y,z] TRIPLES, split into the pairs the renderer draws plus the parallel
  // value array (same index = same dot; the server read all three columns in one pass to guarantee it)
  if (colour) { const { points: pts, values } = splitXYZ(buf); points.value = pts; baseValues.value = values }
  else { points.value = buf; baseValues.value = null }
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
  if (!hl.length || notTracked.value) { popLayers.value = []; return }   // untracked → nothing to overlay
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
// reserved-prefix + same-list duplicate, live as the user types; a cross-pop-type collision (e.g. a
// region already named this) is rejected by the server (pop_name_conflict) and surfaced as a toast.
const nameError = computed(() => popNameError(newName.value, g.flat.map(p => p.name)))
async function confirmGate() {
  if (!pending.value || nameError.value) return
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

// export the plot — GateScatterCell owns the image composite (PNG = canvas pixels + overlays; SVG =
// true-vector dots/gates/axes, docs/PLOTS.md); CSV = the shown per-event channel values + population
// (Prism-ready). We name the file + assemble the CSV rows here.
const cell = useTemplateRef<{
  exportImage(bg?: string): Promise<string | null>
  exportSvg(bg?: string, light?: boolean): string
}>('cell')
function exportAs(kind: string) {
  const stem = `gate_${xChan.value}_${yChan.value}`.replace(/[^\w.-]+/g, '_')
  if (kind === 'png') cell.value?.exportImage('#0d0b1a').then(url => url && downloadDataUrl(`${stem}.png`, url))
  else if (kind === 'svg') {
    const svg = cell.value?.exportSvg('#ffffff', true)
    if (svg) { const w = svgSizeWarning(svg, 'This gating plot'); if (w) log.warn(w, { source: 'gating' }); downloadText(`${stem}.svg`, svg, 'image/svg+xml') }
  }
  else if (kind === 'csv') { const csv = buildCsv(); if (csv) downloadText(`${stem}.csv`, csv, 'text/csv') }
}
// per-event rows for Prism: the base (parent) population's points, then each shown child-pop overlay's
// points, each row = {x-channel value, y-channel value, population}. Headers use the channel LABELS.
function buildCsv(): string {
  const pts = points.value; if (!pts) return ''
  const xName = g.colLabel(xChan.value) || 'x'
  const yRaw = g.colLabel(yChan.value) || 'y'
  const yName = yRaw === xName ? `${yRaw} (y)` : yRaw
  // the colour-by measure is a real per-event column, so it belongs in the export too (blank for the
  // child-pop overlay rows — those are separate fetches, drawn in the pop colour, and carry no value)
  const zVals = baseValues.value
  const zRaw = g.colLabel(zChan.value) || 'colour'
  const zName = [xName, yName].includes(zRaw) ? `${zRaw} (colour)` : zRaw
  const rows: Record<string, unknown>[] = []
  const push = (arr: Float32Array, pop: string, vals: Float32Array | null = null) => {
    for (let i = 0; i < arr.length / 2; i++) rows.push({ [xName]: arr[2 * i], [yName]: arr[2 * i + 1],
      ...(vals ? { [zName]: vals[i] } : {}), population: pop })
  }
  push(pts, parent.value === 'root' ? 'root' : (parent.value.split('/').filter(Boolean).pop() ?? parent.value),
       colourOn.value && zVals?.length === pts.length / 2 ? zVals : null)
  if (showPops.value) for (const pl of popLayers.value) push(pl.points, pl.path.split('/').filter(Boolean).pop() ?? pl.path)
  return rowsToCsv(rows)
}

// The X/Y options, headed by family (Morphology · Channels · Spatial / Time) instead of one flat run
// in which a shape descriptor and a marker looked alike — utils/measureGroups.ts, shared with the
// pairs picker, the population manager and the clustering feature picker.
const axisGroups = computed(() => measureGroups({
  columns: g.columns, channels: g.channels, spatialAxes: g.spatialAxes, popType: g.popType }))

function ensureChannels() {
  const cols = g.columns
  if (!cols.length) return
  // spatial/temporal axes are valid selections too — don't reset a persisted centroid_x/… axis
  const valid = groupedCols(axisGroups.value)
  if (!valid.includes(xChan.value)) xChan.value = g.channels[(props.index * 2) % Math.max(1, g.channels.length)] ?? cols[0]
  if (!valid.includes(yChan.value)) yChan.value = g.channels[(props.index * 2 + 1) % Math.max(1, g.channels.length)] ?? cols[Math.min(1, cols.length - 1)]
}
// store readiness (channels/image/segmentation): pick default axes then load. `immediate` so the
// first appearance fetches whether the store became ready BEFORE this panel mounted (values already
// set → fires now) or AFTER (fires again on change) — previously the plot stayed empty on first open
// until the user nudged a dropdown.
watch([() => g.columns, () => g.imageUid, () => g.valueName],
      () => { ensureChannels(); fetchPlot() }, { immediate: true, flush: 'post' })
// zChan/zt/colourOn are in here because the colour measure changes what the POINTS request returns
// (triples vs pairs) and what plotmeta reports for the ramp — not just how the dots are painted.
watch([xChan, yChan, xt, yt, zChan, zt, colourOn, parent, () => props.axisFromZero], fetchPlot)
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
               :persist-key="props.persistKey" :auto-hide="false" :square="true"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <!-- Controls are FIXED (in-flow, above the plot) — you draw gates with the cursor ON the plot, so
         auto-hiding on hover would cover the very area you're drawing. CanvasPanel :square squares the
         PLOT REGION (.panel-main) below these fixed controls, so the plot stays 1:1 with a visible
         x-axis and no blank space. All controls sit in #actions (one in-flow block). -->
    <template #actions>
      <RenderModeToggle v-model="renderMode" :colour-by="!!zChan" />
      <span class="ctrl-sep" />
      <ChipSelect variant="segmented" allow-empty :options="DRAW_MODES" data-guide="gate.drawTool"
                  :model-value="mode === 'off' ? '' : mode" aria-label="Gate draw tool"
                  @update:model-value="v => mode = (v || 'off') as typeof mode" />
      <!-- axis (X, Y) + displayed population — one row each, stacked so they don't wrap awkwardly -->
      <div class="panel-ctrl" data-guide="gate.axes">
        <label class="ax-row cc-muted"><span class="ax-lbl">X</span>
          <select class="ax-chan" v-model="xChan" v-tooltip.bottom="'Measure on the X axis'">
            <optgroup v-for="grp in axisGroups" :key="grp.title" :label="grp.title">
              <option v-for="c in grp.cols" :key="c" :value="c">{{ g.colLabel(c) }}</option>
            </optgroup>
          </select>
          <select class="tsel" :class="{ 'cc-auto-override': !!xOverride }" v-model="xtSel"
                  v-tooltip.bottom="overrideTooltip(xOverride, 'Axis transform')">
            <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select>
          <i v-if="xOverride" class="pi pi-exclamation-triangle ax-warn"
             v-tooltip.bottom="overrideTooltip(xOverride, '')" /></label>
        <label class="ax-row cc-muted"><span class="ax-lbl">Y</span>
          <select class="ax-chan" v-model="yChan" v-tooltip.bottom="'Measure on the Y axis'">
            <optgroup v-for="grp in axisGroups" :key="grp.title" :label="grp.title">
              <option v-for="c in grp.cols" :key="c" :value="c">{{ g.colLabel(c) }}</option>
            </optgroup>
          </select>
          <select class="tsel" :class="{ 'cc-auto-override': !!yOverride }" v-model="ytSel"
                  v-tooltip.bottom="overrideTooltip(yOverride, 'Axis transform')">
            <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select>
          <i v-if="yOverride" class="pi pi-exclamation-triangle ax-warn"
             v-tooltip.bottom="overrideTooltip(yOverride, '')" /></label>
        <label class="ax-row cc-muted"><span class="ax-lbl">colour</span>
          <select class="ax-chan" v-model="zChan"
                  v-tooltip.bottom="'Colour the dots by a third measure (points / binned)'">
            <option value="">density</option>
            <optgroup v-for="grp in axisGroups" :key="grp.title" :label="grp.title">
              <option v-for="c in grp.cols" :key="c" :value="c">{{ g.colLabel(c) }}</option>
            </optgroup>
          </select>
          <select class="tsel" :class="{ 'cc-auto-override': !!zOverride }" v-model="ztSel" :disabled="!colourOn"
                  v-tooltip.bottom="overrideTooltip(zOverride, 'Colour scale')">
            <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option></select>
          <i v-if="zOverride" class="pi pi-exclamation-triangle ax-warn"
             v-tooltip.bottom="overrideTooltip(zOverride, '')" /></label>
        <label class="ax-row cc-muted"><span class="ax-lbl">pop</span>
          <select class="ax-chan" v-model="parent" v-tooltip.bottom="'Population to show; new gates are its children'">
          <option v-for="p in parentOptions" :key="p" :value="p">{{ p }}</option></select>
          <span v-if="mode !== 'off' && !pending" class="gate-hint cc-muted cc-fs-2xs">hold <kbd>Shift</kbd> to adjust gates</span></label>
      </div>
    </template>
    <!-- utility actions (export) in the footer, like the summary / cluster panels -->
    <template #footer>
      <select class="gp-export" v-tooltip.top="'Export the shown plot'" :disabled="!points"
              @change="exportAs(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="csv">Data (CSV)</option>
        <option value="png">Image (PNG)</option>
        <option value="svg">Image (SVG)</option>
      </select>
    </template>
    <GateScatterCell ref="cell" :points="points" :extents="extents" :view-extents="viewExtents"
                     :flip-y="isImageYAxis(yChan)"
                     :x-ticks="xTicks" :y-ticks="yTicks" :gates="currentGates"
                     :x-label="axisLabelWithUnit(g.colLabel(xChan), xUnit)"
                     :y-label="axisLabelWithUnit(g.colLabel(yChan), yUnit)"
                     :base-values="baseValues" :value-extent="valueExtent" :value-ticks="valueTicks"
                     :value-label="axisLabelWithUnit(g.colLabel(zChan), zUnit)"
                     :pop-layers="popLayers" :render-mode="renderMode" :show-pops="showPops"
                     :dot-size="props.dotSize"
                     :mode="mode" :gate-line-width="gateLineWidth" :gate-labels="gateLabels"
                     :view-tick="viewTick" :loading="loading"
                     @draw="onDraw" @edit="onEdit" @cancel="mode = 'off'">
      <!-- untracked segmentation on a track plot: nothing to show, point the user at tracking -->
      <div v-if="notTracked" class="gate-empty cc-empty-inline cc-card">
        <i class="pi pi-share-alt" />
        <span>Not tracked yet — run tracking on this segmentation first.</span>
      </div>
      <div v-if="pending" class="panel-name" data-guide="gate.name">
        <span>new {{ pending.kind }}</span>
        <input v-model="newName" placeholder="name…" autofocus v-tooltip.top="'Name for the new gate'"
               :class="{ 'name-invalid': !!nameError && !!newName.trim() }"
               @keyup.enter="confirmGate" @keyup.esc="pending = null" />
        <button class="cc-btn cc-btn-primary" :disabled="!!nameError" @click="confirmGate">Add</button>
        <button class="cc-btn cc-btn-ghost" @click="pending = null">×</button>
        <span v-if="nameError && newName.trim()" class="name-hint">{{ nameError }}</span>
      </div>
    </GateScatterCell>
  </CanvasPanel>
</template>

<style scoped>
/* the floating chrome (.panel / .panel-head / title / remove) lives in CanvasPanel; the styles
   below are gating-specific: the axis controls, the plot body, ticks/axes, and the header tools
   passed into CanvasPanel's #actions slot (.ctrl-sep — slot content keeps
   this component's scoped styles). */
/* axis controls now live in the auto-hide overlay (#actions); take a full line below the icon tools
   (flex-basis:100% within the flex-wrap overlay), one row per axis so they don't wrap awkwardly */
.panel-ctrl { flex-basis: 100%; display: flex; flex-direction: column; gap: 6px; font-size: var(--cc-fs-sm); }
.ax-row { display: flex; align-items: center; gap: 6px; }
/* wide enough for the longest row label ("colour") so all four rows' selects still line up */
.ax-lbl { width: 3.4rem; color: var(--cc-text-dim); flex-shrink: 0; }
/* fixed widths so the controls don't stretch when the plot is resized */
.ax-chan { width: 9rem; flex: none; }
.ax-row .tsel { flex-shrink: 0; }
/* + cc-muted on each label — only the row layout is this panel's business */
.panel-ctrl label { display: flex; align-items: center; gap: 4px; }
/* fixed widths so the bar doesn't reflow when the selected option text changes; the rest
   (background, border, chevron, focus) comes from the global form base in style.css */
.panel-ctrl select { width: 8rem; padding-top: 2px; padding-bottom: 2px; }
.panel-ctrl select.tsel { width: 5.5rem; }
/* the amber marker for an auto-overridden control is the shared .cc-auto-override utility (style.css) */
.ax-warn { color: var(--cc-sev-warn); font-size: var(--cc-fs-xs); flex-shrink: 0; cursor: help; }
.panel-ctrl select:focus { border-color: var(--cc-accent); }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
.gp-export { max-width: 7rem; }
/* the plot body (scatter/layers/gate + ticks/axes + PNG export) lives in GateScatterCell now. */
.panel-name { position: absolute; top: 4px; left: 4px; display: flex; align-items: center; gap: 5px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-accent); border-radius: var(--cc-radius-xs); padding: 4px 6px; font-size: var(--cc-fs-xs); }
.panel-name input { background: var(--cc-bg); border-radius: var(--cc-radius-xs); padding: 1px 5px; width: 90px; }
.panel-name input.name-invalid { border-color: var(--cc-sev-fail); }
.name-hint { color: var(--cc-sev-fail); font-size: var(--cc-fs-2xs); max-width: 150px; line-height: 1.2; }
/* subtle draw-mode affordance (top-right of the plot); mirrors .panel-name but muted and non-interactive */
/* inline affordance beside the pop selector (was overlaid on the plot, which obscured gating) */
.gate-hint { margin-left: 2px; pointer-events: none; display: inline-flex; align-items: center; gap: 4px; white-space: nowrap; }

/* centred empty-state over the plot: track-grained pop type on an untracked segmentation.
   + .cc-empty-inline .cc-card — an inline empty wearing card chrome; only the centring is local */
.gate-empty { position: absolute; inset: 0; margin: auto; width: max-content; height: max-content;
  padding: 8px 12px; pointer-events: none; font-size: var(--cc-fs-sm); }
.gate-hint kbd { font: inherit; background: var(--cc-surface-2); border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs);
  padding: 0 3px; color: var(--cc-text); }
</style>
