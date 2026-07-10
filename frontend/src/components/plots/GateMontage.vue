<!--
  The ONE gate-montage renderer: a grid of read-only GateScatterCell tiles, each showing a parent
  population's cells as a density scatter on a channel pair with the child gate outlines that define
  populations there. Extracted from GatingStrategyView so BOTH montage producers share it
  (feedback_use_existing_framework):
    • the gating-strategy plot (Analysis board) — tree-derived tiles, responsive wrap layout;
    • the channel-pairs matrix (Gate/Tracking pages) — channel-product tiles, strict N×N matrix.
  The host owns WHICH tiles to render (`defs`, built by tree walk or buildPairDefs); this owns the
  per-tile fetch (plotmeta/plotdata/stats), the transpose reuse (mirror tiles share one fetch), the
  optional coloured population overlays (`highlight` — the "show pops" / napari-brushing layers, same as
  the normal gating plot), the layout, and PNG/PDF export. Store-agnostic (the board fetches its own
  tree independent of the gating store), so all reactivity is driven by props.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef } from 'vue'
import type { GateSpec, TransformSpec } from '../../stores/gating'
import { plotHostToImageURL } from '../../plots/export'
import GateScatterCell from './GateScatterCell.vue'
import type { PopLayer } from './PlotLayers.vue'
import {
  type PanelDef, type PanelChild, type MontageId, type Ext, type Tick, type SrvGate,
  idQ, plotQ, canonicalOrient, transposePoints, transposeExt, pearson, effSpec, transposeGate,
} from '../../plots/montage'

const isScatter = (d: PanelDef) => (d.role ?? 'scatter') === 'scatter'

const props = withDefaults(defineProps<{
  projectUid: string; imageUid: string; valueName: string; popType: string
  defs: PanelDef[]
  colLabel: (col: string) => string
  renderMode?: 'points' | 'contour' | 'outliers'
  gateLabels?: boolean
  gateLineWidth?: number
  // coloured population overlays drawn on every tile (the manager's "eye" pops + transient napari
  // selection). Host resolves the colour; empty on the read-only board.
  highlight?: { path: string; colour: string }[]
  // null → responsive wrap (gating-strategy montage); N → strict N-column matrix (channel pairs)
  cols?: number | null
  // true (default) → whole-dataset axis (x0=1), tiles align on a fixed scale; false → autoscale each
  // axis to the population. The gating-strategy montage always wants the fixed scale (tiles have
  // different parents), so it leaves this at the default; the pairs matrix honours the page's toggle.
  axisFromZero?: boolean
  // bump to force a data refresh when tiles are unchanged but membership moved (ancestor gate edit,
  // napari selection) — the parent's point cloud can change without any def changing.
  reloadKey?: string | number
}>(), {
  renderMode: 'points', gateLabels: true, gateLineWidth: 1.5,
  highlight: () => [], cols: null, axisFromZero: true, reloadKey: 0,
})
// true when ≥1 tile's preferred transform was auto-linearised (host shows an amber hint on its control)
const emit = defineEmits<{ coerced: [boolean] }>()

const montageId = computed<MontageId>(() => ({
  projectUid: props.projectUid, imageUid: props.imageUid, valueName: props.valueName, popType: props.popType }))
const single = computed(() => props.cols == null && props.defs.length === 1)
const gridStyle = computed(() => props.cols ? { gridTemplateColumns: `repeat(${props.cols}, minmax(0, 1fr))` } : {})

interface PanelData {
  points: Float32Array; extents: Ext; xTicks: Tick[]; yTicks: Tick[]
  gates: { path: string; colour: string; gate: GateSpec; label: string }[]
  popLayers: PopLayer[]
}
const panelData = ref<Record<string, PanelData>>({})
// Pearson r per canonical pair (groupKey) — computed once from each scatter tile's points and reused by
// its upper-triangle mirror (the corr cell), so the whole matrix costs nothing extra.
const corrByGroup = ref<Record<string, number | null>>({})
const corrFor = (d: PanelDef) => corrByGroup.value[canonicalOrient(d).groupKey]
const loading = ref(false)
const err = ref('')
let loadTok = 0

// per-run fetch memo → mirror tiles (a,b)/(b,a) and repeated highlight/stat lookups hit the network once.
async function loadPanels() {
  const defs = props.defs.filter(isScatter)
  if (!props.imageUid || !defs.length) { panelData.value = {}; corrByGroup.value = {}; err.value = ''; return }
  const tok = ++loadTok
  loading.value = true; err.value = ''
  const id = montageId.value
  interface MetaData { extents: Ext; xTicks: Tick[]; yTicks: Tick[]
    effA: TransformSpec; effB: TransformSpec; coerced: boolean; gates: SrvGate[] }
  const metaCache = new Map<string, Promise<MetaData>>()
  const ptsCache = new Map<string, Promise<Float32Array>>()
  const statCache = new Map<string, Promise<number | undefined>>()

  // meta uses the whole-dataset axis (x0=1) + autoLinear (server may swap a collapsing transform → linear
  // and report usedX/usedY) → pop-independent; cache by canonical pair. Carries the EFFECTIVE transforms
  // (for the point fetch) and the server-projected child-gate outlines (canonical orientation).
  const metaFor = (o: ReturnType<typeof canonicalOrient>, pop: string) => {
    if (!metaCache.has(o.groupKey)) metaCache.set(o.groupKey, (async () => {
      const m = await (await fetch(`/api/gating/plotmeta?${plotQ(id, pop, o.a, o.b, o.ta, o.tb, props.axisFromZero, true)}`)).json() as {
        xExtent: [number, number]; yExtent: [number, number]; xTicks: Tick[]; yTicks: Tick[]
        usedX?: string; usedY?: string; gates?: SrvGate[] }
      return { extents: { xMin: m.xExtent[0], xMax: m.xExtent[1], yMin: m.yExtent[0], yMax: m.yExtent[1] },
               xTicks: m.xTicks, yTicks: m.yTicks,
               effA: effSpec(m.usedX, o.ta), effB: effSpec(m.usedY, o.tb),
               coerced: (!!m.usedX && m.usedX !== o.ta.kind) || (!!m.usedY && m.usedY !== o.tb.kind),
               gates: m.gates ?? [] } as MetaData
    })())
    return metaCache.get(o.groupKey)!
  }
  // points fetched with the EFFECTIVE transforms so the cloud matches the extent + projected gates.
  const ptsFor = (o: ReturnType<typeof canonicalOrient>, pop: string, effA: TransformSpec, effB: TransformSpec) => {
    const key = `${pop}|${o.groupKey}`
    if (!ptsCache.has(key)) ptsCache.set(key, (async () =>
      new Float32Array(await (await fetch(`/api/gating/plotdata?${plotQ(id, pop, o.a, o.b, effA, effB, props.axisFromZero)}`)).arrayBuffer()))())
    return ptsCache.get(key)!
  }
  const labelFor = async (c: PanelChild): Promise<string> => {
    if (!statCache.has(c.path)) statCache.set(c.path, (async () => {
      try {
        const s = await (await fetch(`/api/gating/stats?${idQ(id)}&pop=${encodeURIComponent(c.path)}`)).json() as { pctParent?: number }
        return typeof s.pctParent === 'number' ? s.pctParent : undefined   // ALREADY a percentage (backend)
      } catch { return undefined }
    })())
    const pct = await statCache.get(c.path)!
    return pct == null ? c.name : `${c.name}  ${pct.toFixed(1)}%`
  }

  const corrMap: Record<string, number | null> = {}
  let anyCoerced = false
  try {
    const entries = await Promise.all(defs.map(async d => {
      const o = canonicalOrient(d)
      const m = await metaFor(o, d.parentPath)          // effective transforms decided here…
      const ptsRaw = await ptsFor(o, d.parentPath, m.effA, m.effB)   // …then points fetched with them
      if (m.coerced) anyCoerced = true
      corrMap[o.groupKey] = pearson(ptsRaw)   // r is orientation-invariant → compute on the canonical cloud
      // outlines come from the server (projected into the effective transform), keyed by path; merge the
      // child's name/colour/label. Transpose for the mirror tile like the points/extents.
      const gmap = new Map(m.gates.map(sg => [sg.path, o.swap ? transposeGate(sg) : sg]))
      const gates = (await Promise.all(d.children.map(async c => {
        const sg = gmap.get(c.path)
        if (!sg) return null
        const gate = { kind: sg.kind, x_channel: d.xChan, y_channel: d.yChan,
          x_transform: o.swap ? m.effB : m.effA, y_transform: o.swap ? m.effA : m.effB,
          x_min: sg.x_min, x_max: sg.x_max, y_min: sg.y_min, y_max: sg.y_max, vertices: sg.vertices } as GateSpec
        return { path: c.path, colour: c.colour, gate, label: await labelFor(c) }
      }))).filter((g): g is { path: string; colour: string; gate: GateSpec; label: string } => g !== null)
      const popLayers = await Promise.all((props.highlight ?? []).map(async h => {
        const raw = await ptsFor(o, h.path, m.effA, m.effB)
        return { path: h.path, colour: h.colour, points: o.swap ? transposePoints(raw) : raw }
      }))
      const data: PanelData = {
        points: o.swap ? transposePoints(ptsRaw) : ptsRaw,
        extents: o.swap ? transposeExt(m.extents) : m.extents,
        xTicks: o.swap ? m.yTicks : m.xTicks,
        yTicks: o.swap ? m.xTicks : m.yTicks,
        gates, popLayers,
      }
      return [d.key, data] as const
    }))
    if (tok === loadTok) { panelData.value = Object.fromEntries(entries); corrByGroup.value = corrMap; emit('coerced', anyCoerced) }
  } catch (e) {
    if (tok === loadTok) { err.value = e instanceof Error ? e.message : String(e); panelData.value = {}; corrByGroup.value = {} }
  } finally { if (tok === loadTok) loading.value = false }
}

// One reactive signal covering everything that changes a tile's DATA (axes, transforms, parent, gate
// specs, highlights, forced membership refresh). Tile keys alone miss transform/gate edits, so hash the
// material fields — cheap for the tile counts a montage holds.
const sig = computed(() => JSON.stringify({
  id: montageId.value,
  defs: props.defs.filter(isScatter).map(d =>
    ({ k: d.key, p: d.parentPath, x: d.xChan, y: d.yChan, xt: d.xt, yt: d.yt, c: d.children.map(c => [c.path, c.gate]) })),
  hl: props.highlight, rk: props.reloadKey, fz: props.axisFromZero,
}))
watch(sig, loadPanels, { immediate: true })

// ── export: plot-only image (single cell hi-res, or the whole grid on white for the board PDF) ──────
const gridRef = useTemplateRef<HTMLElement>('gridRef')
type CellExport = {
  exportImage(bg?: string, light?: boolean): Promise<string | null>
  hiRes(cv: HTMLCanvasElement, scale: number): Promise<CanvasImageSource | null>
}
const cellRefs = new Map<string, CellExport>()
function setCellRef(key: string, el: unknown) { if (el) cellRefs.set(key, el as CellExport); else cellRefs.delete(key) }
async function exportImage(bg = '#ffffff', light = true): Promise<string | null> {
  const defs = props.defs.filter(isScatter)
  if (defs.length === 1) return (await cellRefs.get(defs[0].key)?.exportImage(bg, light)) ?? null
  const el = gridRef.value
  if (!el) return null
  const cells = [...cellRefs.values()]
  const hiRes = async (cv: HTMLCanvasElement, scale: number) => {
    for (const c of cells) { const r = await c.hiRes?.(cv, scale); if (r) return r }
    return null
  }
  const scale = Math.min(8, Math.max(4, Math.ceil(2800 / (el.clientWidth || 500))))
  if (light) el.classList.add('cc-light')
  try { return await plotHostToImageURL(el, bg, { hiRes, scale }) } finally { if (light) el.classList.remove('cc-light') }
}
defineExpose({ exportImage })

const titleFor = (parentPath: string) => (parentPath === 'root' ? 'all events (root)' : parentPath)
// upper-triangle correlation cell (ggpairs): show r, scaling the text with |r| so strong pairs stand out
const fmtCorr = (r: number | null | undefined) => (r == null ? '–' : (r >= 0 ? '' : '−') + Math.abs(r).toFixed(2))
const corrFont = (r: number | null | undefined) => `${Math.round(13 + Math.abs(r ?? 0) * 13)}px`
</script>

<template>
  <div ref="gridRef" class="gm-grid" :class="{ single, matrix: cols != null }" :style="gridStyle">
    <div v-if="err" class="gm-msg">{{ err }}</div>
    <div v-else-if="!defs.length" class="gm-msg"><slot name="empty">Nothing to show.</slot></div>
    <template v-for="d in defs" :key="d.key">
      <!-- DIAGONAL (ggpairs): the channel name — labels its whole row and column -->
      <div v-if="d.role === 'diagonal'" class="gm-cell gm-diag"><span>{{ colLabel(d.xChan) }}</span></div>
      <!-- UPPER triangle (ggpairs): the pair's correlation, reused from its mirror scatter -->
      <div v-else-if="d.role === 'corr'" class="gm-cell gm-corr"
           v-tooltip.top="`corr(${colLabel(d.xChan)}, ${colLabel(d.yChan)})`">
        <span class="gm-corr-k">Corr</span>
        <span class="gm-corr-v" :style="{ fontSize: corrFont(corrFor(d)) }">{{ fmtCorr(corrFor(d)) }}</span>
      </div>
      <div v-else class="gm-cell">
        <div v-if="cols == null" class="gm-title" v-tooltip.top="`derived from ${titleFor(d.parentPath)}`">{{ titleFor(d.parentPath) }}</div>
        <GateScatterCell v-if="panelData[d.key]" class="gm-plot" :ref="el => setCellRef(d.key, el)"
                         :points="panelData[d.key].points" :extents="panelData[d.key].extents"
                         :view-extents="panelData[d.key].extents"
                         :x-ticks="panelData[d.key].xTicks" :y-ticks="panelData[d.key].yTicks"
                         :gates="panelData[d.key].gates" :x-label="colLabel(d.xChan)" :y-label="colLabel(d.yChan)"
                         :pop-layers="panelData[d.key].popLayers" :show-pops="(highlight?.length ?? 0) > 0"
                         :render-mode="renderMode" mode="off" :gate-labels="gateLabels"
                         :gate-line-width="gateLineWidth" :compact="!single" :readonly="true"
                         :hide-axis-labels="cols != null" />
        <div v-else class="gm-loading">…</div>
      </div>
    </template>
  </div>
</template>

<style scoped>
/* Two layouts. WRAP (gating-strategy): responsive squares that fill the width and wrap. MATRIX
   (channel pairs): a strict N-column grid (columns set inline) so every channel lines up in a row/col. */
.gm-grid { flex: 1; min-height: 0; overflow: auto; padding: 6px; display: grid; gap: 8px;
  grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); align-content: start; }
.gm-grid.matrix { grid-auto-rows: max-content; }
.gm-grid.single { grid-template-columns: 1fr; grid-template-rows: 1fr; overflow: hidden; }
/* light theme for PDF export: dark ink on white */
.gm-grid.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; --cc-bg: #fff; --cc-surface-2: #f0f0f3; }
.gm-cell { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--cc-border);
  border-radius: 5px; overflow: hidden; background: var(--cc-bg); }
/* montage plot area is SQUARE. Cap the width in wrap mode so a wide column doesn't make it so tall the
   x-axis label drops out of view; in matrix mode fill the column so the grid stays aligned. */
.gm-plot { flex: none; width: 100%; max-width: 320px; aspect-ratio: 1; margin: 0 auto; }
.gm-grid.matrix .gm-plot { max-width: none; }
.gm-grid.single .gm-cell { container-type: size; }
.gm-grid.single .gm-plot { flex: none; aspect-ratio: 1; min-height: 0; max-width: none;
  width: min(100cqw, calc(100cqh - 26px)); margin: 0 auto; }
/* diagonal label cell (matrix): channel name centred, matches the tile square */
.gm-diag { align-items: center; justify-content: center; aspect-ratio: 1; background: var(--cc-surface-2);
  color: var(--cc-text); font-weight: 700; font-size: 12px; padding: 4px; text-align: center; word-break: break-word; }
/* upper-triangle correlation cell (ggpairs): "Corr" label + the value, text scaled by |r| */
.gm-corr { align-items: center; justify-content: center; aspect-ratio: 1; gap: 2px; border-color: var(--cc-border); }
.gm-corr-k { font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--cc-text-dim); }
.gm-corr-v { font-weight: 700; color: var(--cc-text); font-variant-numeric: tabular-nums; line-height: 1; }
.gm-title { flex-shrink: 0; font-size: 11px; font-weight: 700; padding: 3px 6px; color: var(--cc-text-dim);
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.gm-loading { flex: 1; display: flex; align-items: center; justify-content: center; color: var(--cc-text-dim); aspect-ratio: 1; }
.gm-msg { grid-column: 1 / -1; padding: 16px; color: var(--cc-text-dim); font-size: 13px; }
</style>
