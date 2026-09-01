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
  the normal gating plot), the optional COLOUR-BY measure (`colourBy` — every tile's dots take the ramp
  from one third measure, with ONE legend for the grid rather than a bar per tile), the layout, and
  PNG/PDF export. Store-agnostic (the board fetches its own
  tree independent of the gating store), so all reactivity is driven by props.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef } from 'vue'
import { isImageYAxis } from '../../utils/gatingAxes'
import type { GateSpec, TransformSpec } from '../../stores/gating'
import { elementToImageURL, loadImg, svgDoc, svgText, rowsToCsv } from '../../plots/export'
import GateScatterCell from './GateScatterCell.vue'
import type { PopLayer } from './PlotLayers.vue'
import {
  type PanelDef, type PanelChild, type MontageId, type Ext, type Tick, type SrvGate, type ColourBy,
  idQ, plotQ, canonicalOrient, transposePoints, transposeExt, pearson, effSpec, transposeGate,
} from '../../plots/montage'
import { splitXYZ } from '../../plots/valueColour'
import { DOT_R } from '../../plots/density'
import ColourBarLegend from './ColourBarLegend.vue'
import type { RenderMode } from './RenderModeToggle.vue'

const isScatter = (d: PanelDef) => (d.role ?? 'scatter') === 'scatter'

const props = withDefaults(defineProps<{
  projectUid: string; imageUid: string; valueName: string; popType: string
  defs: PanelDef[]
  colLabel: (col: string) => string
  renderMode?: RenderMode
  gateLabels?: boolean
  gateLineWidth?: number
  // coloured population overlays drawn on every tile (the manager's "eye" pops + transient napari
  // selection). Host resolves the colour; empty on the read-only board.
  highlight?: { path: string; colour: string }[]
  // COLOUR BY a third measure, for the whole grid (null = the local-density pseudocolour). The ramp's
  // range is whole-dataset, so every tile's colours mean the same thing and ONE legend describes them.
  colourBy?: ColourBy | null
  // null → responsive wrap (gating-strategy montage); N → strict N-column matrix (channel pairs)
  cols?: number | null
  // true (default) → whole-dataset axis (x0=1), tiles align on a fixed scale; false → autoscale each
  // axis to the population. The gating-strategy montage always wants the fixed scale (tiles have
  // different parents), so it leaves this at the default; the pairs matrix honours the page's toggle.
  axisFromZero?: boolean
  // bump to force a data refresh when tiles are unchanged but membership moved (ancestor gate edit,
  // napari selection) — the parent's point cloud can change without any def changing.
  reloadKey?: string | number
  fontSize?: number                              // axis font size (px) forwarded to each tile (vis slider)
  dotSize?: number                               // dot radius (px) forwarded to each tile (see plots/density DOT_R)
}>(), {
  renderMode: 'points', gateLabels: true, gateLineWidth: 1.5,
  highlight: () => [], colourBy: null, cols: null, axisFromZero: true, reloadKey: 0, fontSize: 11,
  dotSize: DOT_R,
})
// true when ≥1 tile's preferred transform was auto-linearised (host shows an amber hint on its control)
const emit = defineEmits<{ coerced: [boolean] }>()

const montageId = computed<MontageId>(() => ({
  projectUid: props.projectUid, imageUid: props.imageUid, valueName: props.valueName, popType: props.popType }))
const single = computed(() => props.cols == null && props.defs.length === 1)
// matrix (cols) → fixed N-column strict grid; wrap → N × M that fills the slot (see nRow/nCol below);
// single is handled by the .single class rule.
const gridStyle = computed(() => {
  if (props.cols != null) return { gridTemplateColumns: `repeat(${props.cols}, minmax(0, 1fr))` }
  if (single.value || !props.defs.length) return {}
  return { gridTemplateColumns: `repeat(${nCol.value}, minmax(0, 1fr))`,
           gridTemplateRows: `repeat(${nRow.value}, minmax(0, 1fr))` }
})

interface PanelData {
  points: Float32Array; extents: Ext; xTicks: Tick[]; yTicks: Tick[]
  gates: { path: string; colour: string; gate: GateSpec; label: string }[]
  popLayers: PopLayer[]
  values: Float32Array | null            // colour-by value per point (null = colouring by density)
}
const panelData = ref<Record<string, PanelData>>({})
// The colour ramp for the WHOLE grid: range + raw-value labels, as plotmeta served them. It depends on
// the measure and the dataset, not on a tile's axes, so every tile's meta reports the same one and the
// legend is drawn once. null → nothing to colour by (no measure, or not a column on this table).
const valueRamp = ref<{ extent: [number, number]; ticks: Tick[] } | null>(null)
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
  if (!props.imageUid || !defs.length) {
    panelData.value = {}; corrByGroup.value = {}; valueRamp.value = null; err.value = ''; return
  }
  const tok = ++loadTok
  loading.value = true; err.value = ''
  const id = montageId.value
  // ONE colour-by for the grid (null = colour by density). Fetched whatever the render mode is: the
  // mode only decides whether the dots USE the values (PlotLayers), and refetching every tile on a
  // points↔contour toggle would be a worse trade than carrying one extra column.
  const colour: ColourBy | null = props.colourBy?.col ? props.colourBy : null
  interface MetaData { extents: Ext; xTicks: Tick[]; yTicks: Tick[]
    effA: TransformSpec; effB: TransformSpec; coerced: boolean; gates: SrvGate[]
    // colour-by: the ramp the server decided (whole-dataset range + raw labels) and the transform it
    // actually used, which the point fetch has to repeat or the values won't match the legend
    ramp: { extent: [number, number]; ticks: Tick[] } | null; effZ: ColourBy | null }
  const metaCache = new Map<string, Promise<MetaData>>()
  const ptsCache = new Map<string, Promise<{ points: Float32Array; values: Float32Array | null }>>()
  const statCache = new Map<string, Promise<number | undefined>>()

  // meta uses the whole-dataset axis (x0=1) + autoLinear (server may swap a collapsing transform → linear
  // and report usedX/usedY) → pop-independent; cache by canonical pair. Carries the EFFECTIVE transforms
  // (for the point fetch) and the server-projected child-gate outlines (canonical orientation).
  const metaFor = (o: ReturnType<typeof canonicalOrient>, pop: string) => {
    if (!metaCache.has(o.groupKey)) metaCache.set(o.groupKey, (async () => {
      const m = await (await fetch(`/api/gating/plotmeta?${plotQ(id, pop, o.a, o.b, o.ta, o.tb, props.axisFromZero, true, colour)}`)).json() as {
        xExtent: [number, number]; yExtent: [number, number]; xTicks: Tick[]; yTicks: Tick[]
        usedX?: string; usedY?: string; gates?: SrvGate[]
        zExtent?: [number, number] | null; zTicks?: Tick[]; usedZ?: string }
      return { extents: { xMin: m.xExtent[0], xMax: m.xExtent[1], yMin: m.yExtent[0], yMax: m.yExtent[1] },
               xTicks: m.xTicks, yTicks: m.yTicks,
               effA: effSpec(m.usedX, o.ta), effB: effSpec(m.usedY, o.tb),
               coerced: (!!m.usedX && m.usedX !== o.ta.kind) || (!!m.usedY && m.usedY !== o.tb.kind),
               gates: m.gates ?? [],
               ramp: colour && m.zExtent ? { extent: m.zExtent, ticks: m.zTicks ?? [] } : null,
               effZ: colour ? { col: colour.col, t: effSpec(m.usedZ, colour.t) } : null } as MetaData
    })())
    return metaCache.get(o.groupKey)!
  }
  // points fetched with the EFFECTIVE transforms so the cloud matches the extent + projected gates.
  const ptsFor = (o: ReturnType<typeof canonicalOrient>, pop: string, effA: TransformSpec, effB: TransformSpec,
                  effZ: ColourBy | null) => {
    const key = `${pop}|${o.groupKey}`
    if (!ptsCache.has(key)) ptsCache.set(key, (async () => {
      const buf = new Float32Array(await (await fetch(
        `/api/gating/plotdata?${plotQ(id, pop, o.a, o.b, effA, effB, props.axisFromZero, false, effZ)}`)).arrayBuffer())
      // colour-by → TRIPLES: split into the pairs the tiles draw plus the parallel values (the server
      // read all three columns in one pass, so index i is the same cell in both)
      return effZ ? splitXYZ(buf) : { points: buf, values: null }
    })())
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
  let ramp: { extent: [number, number]; ticks: Tick[] } | null = null   // one ramp for every tile
  try {
    const entries = await Promise.all(defs.map(async d => {
      const o = canonicalOrient(d)
      const m = await metaFor(o, d.parentPath)          // effective transforms decided here…
      const { points: ptsRaw, values } = await ptsFor(o, d.parentPath, m.effA, m.effB, m.effZ)   // …then points, with them
      if (m.coerced) anyCoerced = true
      if (m.ramp) ramp = m.ramp                        // identical across tiles (whole-dataset range)
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
        // An overlay is drawn in its POPULATION's colour, so it ignores the values — but it asks with
        // the SAME params as the base, because the fetch memo is keyed by (pop, pair): a highlight on
        // the tile's own parent is the same request, and two shapes of it under one key would leave
        // which one wins to whichever tile got there first.
        const raw = (await ptsFor(o, h.path, m.effA, m.effB, m.effZ)).points
        return { path: h.path, colour: h.colour, points: o.swap ? transposePoints(raw) : raw }
      }))
      const data: PanelData = {
        points: o.swap ? transposePoints(ptsRaw) : ptsRaw,
        extents: o.swap ? transposeExt(m.extents) : m.extents,
        xTicks: o.swap ? m.yTicks : m.xTicks,
        yTicks: o.swap ? m.xTicks : m.yTicks,
        gates, popLayers,
        values,                                        // per-point, orientation-invariant → no transpose
      }
      return [d.key, data] as const
    }))
    if (tok === loadTok) {
      panelData.value = Object.fromEntries(entries); corrByGroup.value = corrMap
      valueRamp.value = ramp; emit('coerced', anyCoerced)
    }
  } catch (e) {
    if (tok === loadTok) { err.value = e instanceof Error ? e.message : String(e); panelData.value = {}; corrByGroup.value = {}; valueRamp.value = null }
  } finally { if (tok === loadTok) loading.value = false }
}

// One reactive signal covering everything that changes a tile's DATA (axes, transforms, parent, gate
// specs, highlights, forced membership refresh). Tile keys alone miss transform/gate edits, so hash the
// material fields — cheap for the tile counts a montage holds.
const sig = computed(() => JSON.stringify({
  id: montageId.value,
  defs: props.defs.filter(isScatter).map(d =>
    ({ k: d.key, p: d.parentPath, x: d.xChan, y: d.yChan, xt: d.xt, yt: d.yt, c: d.children.map(c => [c.path, c.gate]) })),
  hl: props.highlight, rk: props.reloadKey, fz: props.axisFromZero, cb: props.colourBy,
}))
watch(sig, loadPanels, { immediate: true })

// ONE legend for the grid — see ColourBarLegend. Only when the dots are actually taking the ramp:
// contour/outlier modes describe a distribution, so PlotLayers ignores the values there and a bar would
// label a colour nothing on screen uses.
// `single` is a full-size plot in a board slot, not a montage cell, so it keeps the bar INSIDE the plot
// (like the Gate page's plot) and the strip stays out of the way — one legend either way, never two.
const showLegend = computed(() => !!valueRamp.value && !single.value &&
                                 (props.renderMode === 'points' || props.renderMode === 'binned'))
const legendLabel = computed(() => props.colourBy?.col ? props.colLabel(props.colourBy.col) : '')

// ── export: plot-only image (single cell hi-res, or the whole grid on white for the board PDF) ──────
// `hostRef` wraps the legend strip + the grid: it is what BOTH export paths capture, so the legend
// travels with the figure. Tile rects are measured against it, so the geometry is unchanged.
const hostRef = useTemplateRef<HTMLElement>('hostRef')
const legendRef = useTemplateRef<{ svgBody(ink: string): string; getEl(): HTMLElement | null }>('legendRef')
type CellExport = {
  exportImage(bg?: string, light?: boolean): Promise<string | null>
  hiRes(cv: HTMLCanvasElement, scale: number): Promise<CanvasImageSource | null>
  getHost(): HTMLElement | null
  exportSvg(bg?: string, light?: boolean): string
  exportSvgBody(light?: boolean): string
}
const cellRefs = new Map<string, CellExport>()
function setCellRef(key: string, el: unknown) { if (el) cellRefs.set(key, el as CellExport); else cellRefs.delete(key) }
async function exportImage(bg = '#ffffff', light = true): Promise<string | null> {
  const defs = props.defs.filter(isScatter)
  // a single tile keeps its OWN bar (it is a full-size plot, not a montage cell), so it exports itself
  if (defs.length === 1) return (await cellRefs.get(defs[0].key)?.exportImage(bg, light)) ?? null
  const el = hostRef.value
  if (!el) return null
  // Multi-tile: composite each scatter tile's UNIFIED export image (dots + gate + axis on one canvas —
  // see GateScatterCell.exportImage) at its grid rect, OVER an HTML overlay that carries the non-scatter
  // cells (ggpairs diagonal names, correlation cells, per-tile titles). This avoids the old per-canvas
  // composite whose separately-rasterised HTML axis desynced from the dots (the gating-PDF scale bug).
  if (light) el.classList.add('cc-light')
  try {
    const gr = el.getBoundingClientRect()
    const k = el.clientWidth ? gr.width / el.clientWidth : 1   // ancestor zoom (untransform the rects)
    const w = el.clientWidth, h = el.clientHeight
    const scale = Math.min(8, Math.max(4, Math.ceil(2800 / (w || 500))))
    const out = document.createElement('canvas')
    out.width = Math.round(w * scale); out.height = Math.round(h * scale)
    const ctx = out.getContext('2d'); if (!ctx) return null
    ctx.scale(scale, scale)
    if (bg && bg !== 'transparent') { ctx.fillStyle = bg; ctx.fillRect(0, 0, w, h) }
    // base layer: the grid's HTML (non-scatter cells + titles); scatter tiles get overwritten next
    const overlayUrl = await elementToImageURL(el, 'svg', 'transparent', { blankCanvases: true })
    if (overlayUrl) { const img = await loadImg(overlayUrl); if (img) ctx.drawImage(img, 0, 0, w, h) }
    // overwrite each scatter tile's region with its unified plot image
    for (const cell of cellRefs.values()) {
      const host = cell.getHost?.(); if (!host) continue
      const url = await cell.exportImage(bg, false)   // .cc-light already on the grid ancestor
      const img = url ? await loadImg(url) : null; if (!img) continue
      const r = host.getBoundingClientRect()
      ctx.drawImage(img, (r.left - gr.left) / k, (r.top - gr.top) / k, r.width / k, r.height / k)
    }
    return out.toDataURL('image/png')
  } finally { if (light) el.classList.remove('cc-light') }
}
// TRUE-VECTOR SVG (docs/PLOTS.md): single tile → the cell's own full SVG; multi-tile → stitch each
// scatter tile's vector BODY (dots/gates/axes, in cell-capture coords) translated to its grid rect, plus
// the ggpairs non-scatter cells (diagonal channel names + correlation values) as SVG <text>. Same rect
// math as exportImage (getBoundingClientRect ÷ ancestor-zoom k), so it lines up identically.
function exportSvg(bg = '#ffffff', light = true): string {
  const scatterDefs = props.defs.filter(isScatter)
  const el = hostRef.value; if (!el) return ''
  if (scatterDefs.length === 1) return cellRefs.get(scatterDefs[0].key)?.exportSvg?.(bg, light) ?? ''
  if (light) el.classList.add('cc-light')
  try {
    const gr = el.getBoundingClientRect()
    const k = el.clientWidth ? gr.width / el.clientWidth : 1
    const w = el.clientWidth, h = el.clientHeight
    let body = ''
    for (const cell of cellRefs.values()) {
      const host = cell.getHost?.(); if (!host) continue
      const cbody = cell.exportSvgBody?.(false); if (!cbody) continue     // grid already .cc-light
      const r = host.getBoundingClientRect()
      body += `<g transform="translate(${(r.left - gr.left) / k} ${(r.top - gr.top) / k})">${cbody}</g>`
    }
    const ink = getComputedStyle(el).getPropertyValue('--cc-text').trim() || '#111'
    // the colour-bar legend, as the SAME vector body it draws on screen, translated to where it sits
    const lel = legendRef.value?.getEl()
    if (lel) {
      const lr = lel.getBoundingClientRect()
      const dim = getComputedStyle(lel).getPropertyValue('--cc-text-dim').trim() || ink
      body += `<g transform="translate(${(lr.left - gr.left) / k} ${(lr.top - gr.top) / k})">` +
              `${legendRef.value?.svgBody(dim) ?? ''}</g>`
    }
    for (const node of Array.from(el.querySelectorAll('.gm-diag, .gm-corr'))) {
      const span = node.querySelector('.gm-corr-v') ?? node.querySelector('span') ?? node
      const txt = (span.textContent ?? '').trim(); if (!txt) continue
      const r = (node as HTMLElement).getBoundingClientRect()
      const isDiag = (node as HTMLElement).classList.contains('gm-diag')
      body += svgText((r.left + r.width / 2 - gr.left) / k, (r.top + r.height / 2 - gr.top) / k, txt,
                      { fill: ink, size: 13, anchor: 'middle', weight: isDiag ? 700 : 400 })
    }
    return svgDoc({ width: w, height: h, background: bg, body })
  } finally { if (light) el.classList.remove('cc-light') }
}
// per-event CSV across all scatter tiles (Prism-ready): one row per event per pair, tagged with the
// channel labels + parent population — a tidy long table you can filter/pivot in Prism.
function exportCsv(): string {
  const rows: Record<string, unknown>[] = []
  for (const d of props.defs.filter(isScatter)) {
    const pd = panelData.value[d.key]; if (!pd?.points) continue
    const xL = props.colLabel(d.xChan), yL = props.colLabel(d.yChan)
    const pop = d.parentPath === 'root' ? 'root' : (d.parentPath.split('/').filter(Boolean).pop() ?? d.parentPath)
    const p = pd.points, v = pd.values
    const zL = props.colourBy?.col ? props.colLabel(props.colourBy.col) : ''
    for (let i = 0; i < p.length / 2; i++) rows.push({ xChan: xL, yChan: yL, x: p[2 * i], y: p[2 * i + 1],
      ...(v && zL ? { colourChan: zL, colour: v[i] } : {}), population: pop })
  }
  return rowsToCsv(rows)
}
defineExpose({ exportImage, exportSvg, exportCsv })

const titleFor = (parentPath: string) => (parentPath === 'root' ? 'all events (root)' : parentPath)
// upper-triangle correlation cell (ggpairs): show r, scaling the text with |r| so strong pairs stand out
const fmtCorr = (r: number | null | undefined) => (r == null ? '–' : (r >= 0 ? '' : '−') + Math.abs(r).toFixed(2))
const corrFont = (r: number | null | undefined) => `${Math.round(13 + Math.abs(r ?? 0) * 13)}px`

// ── WRAP: fixed N × M grid that FILLS the slot (no scroll) ───────────────────────────────────────────
// Ports the old R plotFlowGating model (R/flowHelpers.R:.flowPlotGatedRaster + ggpubr::ggarrange nrow/
// ncol) — the whole figure sizes to the render surface, so PDF export is just a picture of the DOM.
// `auto-fill` scrolls at small slots; a decided N × M can't. Auto-shape ≈ square: nCol = ceil(√count),
// nRow = ceil(count / nCol). Matrix (cols != null) and single set their template inline / by class.
const nCol = computed(() => Math.max(1, Math.ceil(Math.sqrt(Math.max(1, props.defs.length)))))
const nRow = computed(() => Math.max(1, Math.ceil(props.defs.length / nCol.value)))
</script>

<template>
  <div ref="hostRef" class="gm-host">
    <!-- ONE colour bar for the whole grid (never per tile — see ColourBarLegend) -->
    <div v-if="showLegend" class="gm-legend">
      <ColourBarLegend ref="legendRef" :extent="valueRamp!.extent" :ticks="valueRamp!.ticks" :label="legendLabel" />
    </div>
    <div class="gm-grid" :class="{ single, matrix: cols != null }" :style="gridStyle">
      <div v-if="err" class="gm-msg cc-muted cc-fs-md">{{ err }}</div>
      <div v-else-if="!defs.length" class="gm-msg cc-muted cc-fs-md"><slot name="empty">Nothing to show.</slot></div>
      <template v-for="d in defs" :key="d.key">
        <!-- DIAGONAL (ggpairs): the channel name — labels its whole row and column -->
        <div v-if="d.role === 'diagonal'" class="gm-cell gm-diag"><span>{{ colLabel(d.xChan) }}</span></div>
        <!-- UPPER triangle (ggpairs): the pair's correlation, reused from its mirror scatter -->
        <div v-else-if="d.role === 'corr'" class="gm-cell gm-corr"
             v-tooltip.top="`corr(${colLabel(d.xChan)}, ${colLabel(d.yChan)})`">
          <span class="gm-corr-k cc-eyebrow cc-fs-2xs">Corr</span>
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
                           :hide-axis-labels="cols != null" :font-size="fontSize" :dot-size="dotSize"
                           :base-values="panelData[d.key].values" :value-extent="valueRamp?.extent ?? null"
                           :value-ticks="valueRamp?.ticks" :value-label="legendLabel"
                           :value-legend="single"
                           :flip-y="isImageYAxis(d.yChan)" />
          <div v-else class="gm-loading">…</div>
        </div>
      </template>
    </div>
  </div>
</template>

<style scoped>
/* the capture host: the colour-bar legend strip (if any) above the scrolling grid. BOTH export paths
   capture THIS element, so the legend travels with the figure. */
.gm-host { flex: 1; min-height: 0; display: flex; flex-direction: column; }
.gm-legend { flex: none; display: flex; justify-content: flex-end; padding: 2px 8px 0; }
/* Three layouts. WRAP (gating-strategy): fixed N × M grid (set inline from nRow/nCol) that FILLS
   the slot without scrolling — same model as R's ggarrange nrow × ncol in the old plotFlowGating
   module; a scrolling grid can't render to PDF cleanly. MATRIX (channel pairs): strict N-column
   grid (columns set inline), can scroll vertically at high channel counts (16×16). SINGLE: one
   plot filling the slot. */
.gm-grid { flex: 1; min-height: 0; padding: 6px; display: grid; gap: 8px; overflow: hidden;
  align-content: stretch; }
.gm-grid.matrix { grid-auto-rows: max-content; overflow: auto; align-content: start; }
.gm-grid.single { grid-template-columns: 1fr; grid-template-rows: 1fr; overflow: hidden; }
/* light theme for PDF export: dark ink on white — set on the CAPTURE host, so the grid, its tiles and
   the legend strip all inherit the flipped vars */
.gm-host.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; --cc-bg: #fff; --cc-surface-2: #f0f0f3; }
.gm-cell { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); overflow: hidden; background: var(--cc-bg); }
/* montage plot area is SQUARE — but the SQUARE is the DOTS (.panel-plot in GateScatterCell), not this
   outer box: the capture's axis padding is asymmetric (room for the rotated y-name + the x-name below),
   so squaring the OUTER box made the dots taller than wide ("flow plots elongate on export"). We size
   the width here and let GateScatterCell square its .panel-plot; the outer box heights to fit. No cap —
   let the plot grow with the cell so the board's scale drives the plot's scale (not a fixed 320-px lid).
   EXCEPTION — the pairs MATRIX keeps a square OUTER box (aspect-ratio:1) so scatter tiles line up with
   the square diagonal/correlation cells; its axis padding is tiny+near-symmetric so the dots stay square. */
/* WRAP: .gm-cell owns the square (aspect-ratio above). The plot-capture fills the remaining vertical
   space of the cell (below the .gm-title). MATRIX: keeps its own outer-square via aspect-ratio here. */
.gm-plot { flex: 1; width: 100%; min-height: 0; }
.gm-grid.matrix .gm-plot { flex: none; aspect-ratio: 1; }
/* WRAP fill-mode: turn OFF GateScatterCell's default "dots square" rule so plot-capture stretches
   to fill the tile and panel-plot uses the tile's aspect (not aspect-ratio:1 — that made plot-capture
   content-driven and its x-axis label at bottom:-15 spilled past .gm-cell's overflow:hidden and got
   clipped). Matrix / single opt out via the selector. */
.gm-grid:not(.matrix):not(.single) :deep(.plot-capture.gm-plot) {
  flex: 1; min-height: 0; align-items: stretch; }
.gm-grid:not(.matrix):not(.single) :deep(.plot-capture.gm-plot .panel-plot) {
  flex: 1; aspect-ratio: auto; width: 100%; min-height: 0; }
/* single tile (board gating-strategy): FILL the slot. The plot-capture fills the cell and CENTRES a
   square sized to the cell's SMALLER dimension (like the UMAP square) — the old rule made a
   width-square anchored to the TOP with big blank space below, and the full 84px y-name padding left
   the dots tiny in a small slot. Tighter axis padding here so the dots reclaim the space. :deep reaches
   into GateScatterCell (single mode is board-only, so this doesn't touch the matrix / gate page). */
.gm-grid.single .gm-cell { container-type: size; }
.gm-grid.single .gm-plot { flex: 1; min-height: 0; width: 100%; }
/* padding leaves room for the axis NAMES at their offsets (x-name bottom:-40, y-name left:-66) + ticks
   so neither clips; the height budget below subtracts it so the square still fits the cell. */
.gm-grid.single :deep(.plot-capture.gm-plot) {
  flex: 1; min-height: 0; align-items: center; justify-content: center; padding: 12px 14px 46px 50px; }
.gm-grid.single :deep(.plot-capture.gm-plot .panel-plot) {
  flex: none; aspect-ratio: 1; min-height: 0; width: min(100%, calc(100cqh - 82px)); }
/* diagonal label cell (matrix): channel name centred, matches the tile square */
.gm-diag { align-items: center; justify-content: center; aspect-ratio: 1; background: var(--cc-surface-2);
  color: var(--cc-text); font-weight: 700; font-size: var(--cc-fs-sm); padding: 4px; text-align: center; word-break: break-word; }
/* upper-triangle correlation cell (ggpairs): "Corr" label + the value, text scaled by |r| */
.gm-corr { align-items: center; justify-content: center; aspect-ratio: 1; gap: 2px; border-color: var(--cc-border); }

.gm-corr-v { font-weight: 700; color: var(--cc-text); font-variant-numeric: tabular-nums; line-height: 1; }
.gm-title { flex-shrink: 0; font-size: var(--cc-fs-xs); font-weight: 700; padding: 3px 6px; color: var(--cc-text-dim);
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.gm-loading { flex: 1; display: flex; align-items: center; justify-content: center; color: var(--cc-text-dim); aspect-ratio: 1; }
.gm-msg { grid-column: 1 / -1; padding: 16px; }
</style>
