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
import type { GateSpec } from '../../stores/gating'
import { plotHostToImageURL } from '../../plots/export'
import GateScatterCell from './GateScatterCell.vue'
import type { PopLayer } from './PlotLayers.vue'
import {
  type PanelDef, type PanelChild, type MontageId, type Ext, type Tick,
  idQ, plotQ, canonicalOrient, transposePoints, transposeExt,
} from '../../plots/montage'

const props = withDefaults(defineProps<{
  projectUid: string; imageUid: string; valueName: string; popType: string
  defs: PanelDef[]
  colLabel: (col: string) => string
  renderMode?: 'points' | 'contour'
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
const loading = ref(false)
const err = ref('')
let loadTok = 0

// per-run fetch memo → mirror tiles (a,b)/(b,a) and repeated highlight/stat lookups hit the network once.
async function loadPanels() {
  const defs = props.defs.filter(d => !d.diagonal)
  if (!props.imageUid || !defs.length) { panelData.value = {}; err.value = ''; return }
  const tok = ++loadTok
  loading.value = true; err.value = ''
  const id = montageId.value
  const metaCache = new Map<string, Promise<{ extents: Ext; xTicks: Tick[]; yTicks: Tick[] }>>()
  const ptsCache = new Map<string, Promise<Float32Array>>()
  const statCache = new Map<string, Promise<number | undefined>>()

  // meta uses the whole-dataset axis (x0=1 in plotQ) → pop-independent range; cache by canonical pair.
  const metaFor = (o: ReturnType<typeof canonicalOrient>, pop: string) => {
    if (!metaCache.has(o.groupKey)) metaCache.set(o.groupKey, (async () => {
      const m = await (await fetch(`/api/gating/plotmeta?${plotQ(id, pop, o.a, o.b, o.ta, o.tb, props.axisFromZero)}`)).json() as {
        xExtent: [number, number]; yExtent: [number, number]; xTicks: Tick[]; yTicks: Tick[] }
      return { extents: { xMin: m.xExtent[0], xMax: m.xExtent[1], yMin: m.yExtent[0], yMax: m.yExtent[1] },
               xTicks: m.xTicks, yTicks: m.yTicks }
    })())
    return metaCache.get(o.groupKey)!
  }
  const ptsFor = (o: ReturnType<typeof canonicalOrient>, pop: string) => {
    const key = `${pop}|${o.groupKey}`
    if (!ptsCache.has(key)) ptsCache.set(key, (async () =>
      new Float32Array(await (await fetch(`/api/gating/plotdata?${plotQ(id, pop, o.a, o.b, o.ta, o.tb, props.axisFromZero)}`)).arrayBuffer()))())
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

  try {
    const entries = await Promise.all(defs.map(async d => {
      const o = canonicalOrient(d)
      const [m, ptsRaw] = await Promise.all([metaFor(o, d.parentPath), ptsFor(o, d.parentPath)])
      const gates = await Promise.all(d.children.map(async c =>
        ({ path: c.path, colour: c.colour, gate: c.gate, label: await labelFor(c) })))
      const popLayers = await Promise.all((props.highlight ?? []).map(async h => {
        const raw = await ptsFor(o, h.path)
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
    if (tok === loadTok) panelData.value = Object.fromEntries(entries)
  } catch (e) {
    if (tok === loadTok) { err.value = e instanceof Error ? e.message : String(e); panelData.value = {} }
  } finally { if (tok === loadTok) loading.value = false }
}

// One reactive signal covering everything that changes a tile's DATA (axes, transforms, parent, gate
// specs, highlights, forced membership refresh). Tile keys alone miss transform/gate edits, so hash the
// material fields — cheap for the tile counts a montage holds.
const sig = computed(() => JSON.stringify({
  id: montageId.value,
  defs: props.defs.filter(d => !d.diagonal).map(d =>
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
  const defs = props.defs.filter(d => !d.diagonal)
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
</script>

<template>
  <div ref="gridRef" class="gm-grid" :class="{ single, matrix: cols != null }" :style="gridStyle">
    <div v-if="err" class="gm-msg">{{ err }}</div>
    <div v-else-if="!defs.length" class="gm-msg"><slot name="empty">Nothing to show.</slot></div>
    <template v-for="d in defs" :key="d.key">
      <!-- diagonal (channel vs itself, pairs matrix): a labelled cell, R pairs()-style -->
      <div v-if="d.diagonal" class="gm-cell gm-diag"><span>{{ colLabel(d.xChan) }}</span></div>
      <div v-else class="gm-cell">
        <div v-if="cols == null" class="gm-title" v-tooltip.top="`derived from ${titleFor(d.parentPath)}`">{{ titleFor(d.parentPath) }}</div>
        <GateScatterCell v-if="panelData[d.key]" class="gm-plot" :ref="el => setCellRef(d.key, el)"
                         :points="panelData[d.key].points" :extents="panelData[d.key].extents"
                         :view-extents="panelData[d.key].extents"
                         :x-ticks="panelData[d.key].xTicks" :y-ticks="panelData[d.key].yTicks"
                         :gates="panelData[d.key].gates" :x-label="colLabel(d.xChan)" :y-label="colLabel(d.yChan)"
                         :pop-layers="panelData[d.key].popLayers" :show-pops="(highlight?.length ?? 0) > 0"
                         :render-mode="renderMode" mode="off" :gate-labels="gateLabels"
                         :gate-line-width="gateLineWidth" :compact="!single" :readonly="true" />
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
.gm-title { flex-shrink: 0; font-size: 11px; font-weight: 700; padding: 3px 6px; color: var(--cc-text-dim);
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.gm-loading { flex: 1; display: flex; align-items: center; justify-content: center; color: var(--cc-text-dim); aspect-ratio: 1; }
.gm-msg { grid-column: 1 / -1; padding: 16px; color: var(--cc-text-dim); font-size: 13px; }
</style>
