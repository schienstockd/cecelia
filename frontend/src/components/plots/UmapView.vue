<!--
  UMAP interactive view (one entry in the interactive-view registry; see components/canvas/
  interactiveViews.ts). Self-contained: fetches the joint embedding + cluster codes for the selected
  image(s) (GET /api/plots/umap → binary Float32 [x,y,code,…]), renders a colour-by-cluster WebGL
  scatter (ScatterGL `category` mode) with a legend, and owns its own controls (cluster-number label
  toggle). Clustering is set-scope, so all selected images share one UMAP space + cluster numbering —
  we fetch per image and concatenate.

  This is an INTERACTIVE plot (client/WebGL point cloud), distinct from SUMMARY plots (server-
  aggregated, drawn by PlotChart). It is hosted by the generic InteractivePanel, which gives it the
  CanvasPanel chrome — this component knows nothing about panels/canvas, only how to draw a UMAP.

  Props are the generic plot `context` (projectUid/imageUids/setUid/popType/suffix) plus `state` —
  the panel's persisted per-panel options bag (here just `labels`).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import { useLogStore } from '../../stores/log'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { plotHostToImageURL, rasterPlotToImageURL, downloadDataUrl, downloadBlob, rowsToCsv } from '../../plots/export'
import type { VisProps } from '../../plots/plot'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  popType: 'clust' | 'trackclust'; suffix: string
  // populations whose eye is on in the manager: colour their clusters in the pop colour, grey the
  // rest. Empty → plain colour-by-cluster. Live from the gating store via ClusterPlots' viewContext.
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  vis?: VisProps                 // canvas plot styling — here we honour the dark-theme knob
  state: { labels?: boolean; legend?: boolean }
}>()
const log = useLogStore()
const labels = computed({ get: () => props.state.labels !== false, set: v => (props.state.labels = v) })
// show/hide the population legend — persisted per panel (default on). On the Analysis canvas it can
// eat ~half the plot, so let the user reclaim that space.
const showLegend = computed({ get: () => props.state.legend !== false, set: v => (props.state.legend = v) })
const unit = computed(() => (props.popType === 'trackclust' ? 'tracks' : 'cells'))

// `forceLight` flips to a light render for the board's PDF export (dark theme is on-screen only) —
// like the cluster panels. It re-themes the OVERLAYS (label chips, legend ink) + the composite ground,
// but deliberately NOT the WebGL scatter's own background (see `scatterGround`).
const forceLight = ref(false)
// On-screen scatter ground — the WebGL layer's background. Deliberately independent of `forceLight`:
// changing ScatterGL's background prop fires an async re-render that races `exportCanvas` and snapshots
// a BLANK point cloud. The export goes light via the composite `bg` arg + transparent host instead
// (exactly like the gating cell, which never re-grounds its scatter for export).
const screenDark = computed(() => props.vis?.darkTheme !== false)
const scatterGround = computed(() => (screenDark.value ? '#0d0b1a' : '#ffffff'))
// dark-theme knob (default dark): drives the label chip / legend ink AND the composite ground used as
// the PNG background (points are drawn transparent, so this fill IS the exported ground).
const dark = computed(() => !forceLight.value && screenDark.value)
const ground = computed(() => (dark.value ? '#0d0b1a' : '#ffffff'))
// label chip + legend ink follow the theme so they stay legible on the exported ground (a light
// ground with the app's light legend ink was invisible — #00061 follow-up).
const labelStyle = computed(() => dark.value
  ? { color: '#111', background: 'rgba(255,255,255,0.85)', borderColor: 'rgba(0,0,0,0.35)' }
  : { color: '#f5f5f5', background: 'rgba(20,20,28,0.82)', borderColor: 'rgba(255,255,255,0.35)' })
const legendInk = computed(() => (dark.value ? '#e6e6e6' : '#111'))
// legend box background follows the theme too — in light mode the dark ink sat on the app's dark
// panel surface (unreadable on the canvas); give it a light box so the pop names are legible.
const legendBg = computed(() => (dark.value ? 'transparent' : '#ffffff'))

const PALETTE = [
  '#ef4444', '#f59e0b', '#10b981', '#3b82f6', '#a78bfa', '#ec4899', '#14b8a6', '#eab308',
  '#f97316', '#22d3ee', '#84cc16', '#8b5cf6', '#f43f5e', '#06b6d4', '#a3e635', '#d946ef',
  '#fb7185', '#2dd4bf', '#fbbf24', '#60a5fa',
]
const UNCLUSTERED = '#555a6e'
const points = ref<Float32Array | null>(null)
const categories = ref<Float32Array | null>(null)
const palette = ref<string[]>([])
const legend = ref<{ label: string; colour: string; n: number }[]>([])
const centroids = ref<{ label: string; x: number; y: number }[]>([])
const extents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })
const loading = ref(false)
const err = ref('')
const total = computed(() => (points.value ? points.value.length / 2 : 0))
const lx = (x: number) => `${((x - extents.value.xMin) / Math.max(1e-9, extents.value.xMax - extents.value.xMin)) * 100}%`
const ly = (y: number) => `${(1 - (y - extents.value.yMin) / Math.max(1e-9, extents.value.yMax - extents.value.yMin)) * 100}%`

// ── 2D dot render (no WebGL) ──────────────────────────────────────────────────────────────────────
// Draw each point via the SAME data→px map as the labels (lx/ly), so cluster labels sit exactly on
// their dots — regl-scatterplot's aspectRatio fill drifted from the HTML-stretched labels. Colour by
// category (palette index), bucketed so fillStyle is set once per cluster.
const dotsEl = useTemplateRef<HTMLCanvasElement>('dotsEl')
let dctx: CanvasRenderingContext2D | null = null
let dro: ResizeObserver | null = null
const DOT_R = 2
function paintDots(c: CanvasRenderingContext2D, w: number, h: number) {
  const pts = points.value, cats = categories.value, pal = palette.value
  if (!pts || !cats || !pal.length) return
  const { xMin, xMax, yMin, yMax } = extents.value
  const xr = xMax > xMin ? xMax - xMin : 1, yr = yMax > yMin ? yMax - yMin : 1
  const n = pts.length / 2, s = DOT_R * 2
  const groups: number[][] = pal.map(() => [])
  for (let i = 0; i < n; i++) { const g = groups[cats[i]]; if (g) g.push(i) }
  c.globalAlpha = 0.9
  for (let gi = 0; gi < pal.length; gi++) {
    const g = groups[gi]; if (!g.length) continue
    c.fillStyle = pal[gi]
    for (const i of g) {
      const px = ((pts[2 * i] - xMin) / xr) * w, py = (1 - (pts[2 * i + 1] - yMin) / yr) * h
      c.fillRect(px - DOT_R, py - DOT_R, s, s)
    }
  }
  c.globalAlpha = 1
}
function redraw() {
  const el = dotsEl.value; if (!el) return
  if (!dctx) dctx = el.getContext('2d')
  if (!dctx) return
  const dpr = window.devicePixelRatio || 1
  const w = el.clientWidth, h = el.clientHeight
  el.width = Math.max(1, w * dpr); el.height = Math.max(1, h * dpr)
  dctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  dctx.clearRect(0, 0, w, h)
  paintDots(dctx, w, h)
}
// hi-res export: re-paint at scale× onto an offscreen canvas (crisp; a 2D canvas can't clip)
async function exportDots(scale: number): Promise<HTMLCanvasElement | null> {
  const el = dotsEl.value; if (!el) return null
  const w = el.clientWidth, h = el.clientHeight; if (!w || !h) return null
  const off = document.createElement('canvas')
  off.width = Math.max(1, Math.round(w * scale)); off.height = Math.max(1, Math.round(h * scale))
  const octx = off.getContext('2d'); if (!octx) return null
  octx.setTransform(scale, 0, 0, scale, 0, 0)
  paintDots(octx, w, h)
  return off
}

// per-point cluster codes + their tallies, kept so highlighting recolours without refetching.
const codesRef = ref<Float32Array | null>(null)
let countsMap = new Map<number, number>()
let distinctCodes: number[] = []
let sumX = new Map<number, number>(), sumY = new Map<number, number>()   // per-cluster centroid sums

// (re)compute the per-point colours + legend from the cached codes. When pops are shown, colour each
// cluster by the population that owns it and grey the rest; otherwise plain colour-by-cluster.
function recolour() {
  const codes = codesRef.value
  if (!codes) return
  const counts = countsMap, distinct = distinctCodes
  const colourFor = new Map<number, string>(), idxOf = new Map<number, number>()
  const pops = props.shownPops ?? []
  if (pops.length > 0) {
    const clusterColour = new Map<number, string>()
    for (const p of pops) for (const id of p.clusterIds) clusterColour.set(id, p.colour)
    distinct.forEach((c, i) => { idxOf.set(c, i); colourFor.set(c, clusterColour.get(c) ?? UNCLUSTERED) })
    const popN = pops.map(p => ({ label: p.name, colour: p.colour,
      n: p.clusterIds.reduce((s, id) => s + (counts.get(id) ?? 0), 0) }))
    const shownIds = new Set(pops.flatMap(p => p.clusterIds))
    const otherN = distinct.filter(c => !shownIds.has(c)).reduce((s, c) => s + (counts.get(c) ?? 0), 0)
    legend.value = otherN ? [...popN, { label: 'other', colour: UNCLUSTERED, n: otherN }] : popN
  } else {
    let pi = 0
    distinct.forEach((c, i) => { idxOf.set(c, i); colourFor.set(c, c < 0 ? UNCLUSTERED : PALETTE[pi++ % PALETTE.length]) })
    legend.value = distinct.map(c => ({ label: c < 0 ? 'unclustered' : `cluster ${c}`, colour: colourFor.get(c)!, n: counts.get(c)! }))
  }
  const cats = new Float32Array(codes.length)
  for (let i = 0; i < codes.length; i++) cats[i] = idxOf.get(codes[i])!
  palette.value = distinct.map(c => colourFor.get(c)!)
  categories.value = cats
  // labels at centroids: pop NAMES (at each pop's centroid) when pops are shown, else cluster numbers
  if (pops.length > 0) {
    centroids.value = pops.map(p => {
      let sx = 0, sy = 0, nn = 0
      for (const id of p.clusterIds) { sx += sumX.get(id) ?? 0; sy += sumY.get(id) ?? 0; nn += counts.get(id) ?? 0 }
      return nn > 0 ? { label: p.name, x: sx / nn, y: sy / nn } : null
    }).filter((c): c is { label: string; x: number; y: number } => c !== null)
  } else {
    centroids.value = distinct.filter(c => c >= 0)
      .map(c => ({ label: `${c}`, x: sumX.get(c)! / counts.get(c)!, y: sumY.get(c)! / counts.get(c)! }))
  }
}

async function load() {
  err.value = ''
  if (!props.projectUid || !props.imageUids.length) { points.value = null; codesRef.value = null; legend.value = []; centroids.value = []; return }
  loading.value = true
  try {
    const triples: number[] = []
    for (const uid of props.imageUids) {
      const q = new URLSearchParams({ projectUid: props.projectUid, imageUid: uid, popType: props.popType, suffix: props.suffix })
      const res = await fetch(`/api/plots/umap?${q}`)
      if (!res.ok) continue
      const f = new Float32Array(await res.arrayBuffer())
      for (let i = 0; i < f.length; i++) triples.push(f[i])
    }
    const n = Math.floor(triples.length / 3)
    if (n === 0) {
      points.value = null; categories.value = null; legend.value = []; centroids.value = []
      err.value = `No UMAP at suffix “${props.suffix}”. Run clustering with “Calculate UMAP” enabled.`
      return
    }
    const pts = new Float32Array(n * 2), codes = new Float32Array(n)
    let xMin = Infinity, xMax = -Infinity, yMin = Infinity, yMax = -Infinity
    for (let i = 0; i < n; i++) {
      const x = triples[3 * i], y = triples[3 * i + 1]
      pts[2 * i] = x; pts[2 * i + 1] = y; codes[i] = triples[3 * i + 2]
      if (x < xMin) xMin = x; if (x > xMax) xMax = x
      if (y < yMin) yMin = y; if (y > yMax) yMax = y
    }
    const px = (xMax - xMin || 1) * 0.04, py = (yMax - yMin || 1) * 0.04
    extents.value = { xMin: xMin - px, xMax: xMax + px, yMin: yMin - py, yMax: yMax + py }
    points.value = pts
    codesRef.value = codes
    countsMap = new Map<number, number>()
    for (let i = 0; i < n; i++) countsMap.set(codes[i], (countsMap.get(codes[i]) ?? 0) + 1)
    distinctCodes = [...countsMap.keys()].sort((a, b) => a - b)
    // per-cluster centroid sums (recolour turns these into cluster-number or pop-name labels)
    sumX = new Map<number, number>(); sumY = new Map<number, number>()
    for (let i = 0; i < n; i++) {
      const c = codes[i]; if (c < 0) continue
      sumX.set(c, (sumX.get(c) ?? 0) + pts[2 * i]); sumY.set(c, (sumY.get(c) ?? 0) + pts[2 * i + 1])
    }
    recolour()
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`UMAP: ${err.value}`, { source: 'cluster' })
  } finally { loading.value = false }
}

watch([() => props.projectUid, () => props.imageUids.join(','), () => props.popType, () => props.suffix], load)
useDataRefresh(() => props.imageUids, load)   // refetch when a task finishes on one of THESE images
// highlight a pop / tick clusters → recolour from cached codes (no refetch)
watch(() => JSON.stringify((props.shownPops ?? []).map(p => [p.colour, p.clusterIds])), recolour)
// redraw the 2D dots when the data / colouring / extents change
watch([points, categories, palette, extents], () => nextTick(redraw), { deep: true })
onMounted(() => {
  load()
  if (dotsEl.value) { dro = new ResizeObserver(redraw); dro.observe(dotsEl.value); redraw() }
})
onBeforeUnmount(() => { dro?.disconnect(); dro = null })

// ── export (surfaced by the host InteractivePanel via defineExpose) ──
// PNG composites the 2D dot canvas + the HTML labels/legend (plots/export.ts); CSV is x,y,cluster.
const plotEl = useTemplateRef<HTMLElement>('plotEl')
// re-render the dots at export resolution so the cloud is crisp (not a 2×-upscaled 1× canvas)
const hiRes = async (cv: HTMLCanvasElement, scale: number) =>
  cv === dotsEl.value ? (await exportDots(scale)) : null
function exportAs(kind: string) {
  const stem = `umap_${props.suffix}`.replace(/[^\w.-]+/g, '_')
  if (kind === 'png') plotHostToImageURL(plotEl.value, ground.value, { hiRes }).then(url => url && downloadDataUrl(`${stem}.png`, url))
  else if (kind === 'csv') {
    const pts = points.value, codes = codesRef.value
    if (!pts || !codes) return
    const rows = Array.from({ length: codes.length }, (_, i) =>
      ({ x: pts[2 * i], y: pts[2 * i + 1], cluster: codes[i] < 0 ? '' : codes[i] }))
    downloadBlob(`${stem}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  }
}
// board export (surfaced by InteractivePanel.exportImage → the PDF): a plot-only, LIGHT-theme, HI-RES
// PNG via the SHARED raster-export helper (rasterPlotToImageURL — the same crisp fixed-resolution path
// as the gating scatter, GateScatterCell.exportImage), with the theme forced light so it reads on white.
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick()   // let the ground + label/legend ink flip to light before we rasterize the host
  try { return await rasterPlotToImageURL(plotEl.value, ground.value, hiRes) }
  finally { forceLight.value = false }
}
defineExpose({ exportFormats: ['png', 'csv'], exportAs, exportImage })
</script>

<template>
  <div class="uv">
    <div class="uv-ctrl cc-panel-controls">
      <button class="cc-btn cc-btn-ghost" :class="{ on: labels }" @click="labels = !labels"
              v-tooltip.bottom="'Toggle cluster-number labels'"><i class="pi pi-tag" /> #</button>
      <button class="cc-btn cc-btn-ghost" :class="{ on: showLegend }" @click="showLegend = !showLegend"
              v-tooltip.bottom="'Toggle the population legend'"><i class="pi pi-list" /></button>
      <span class="uv-spacer" />
      <span v-if="total" class="uv-count">{{ total.toLocaleString() }} {{ unit }} · {{ legend.length }} clusters</span>
    </div>
    <div ref="plotEl" class="uv-body">
      <!-- during export the inner ground MUST be transparent, else the overlay pass (host→SVG) paints
           this opaque div OVER the transparent hi-res points (the points-missing bug). On-screen it
           carries the scatter ground for the letterbox margins. -->
      <div class="uv-plot" :style="{ background: forceLight ? 'transparent' : scatterGround }">
        <template v-if="points && points.length">
          <canvas ref="dotsEl" class="uv-canvas" />
          <span v-for="c in centroids" v-show="labels" :key="c.label" class="uv-label"
                :style="{ left: lx(c.x), top: ly(c.y), ...labelStyle }">{{ c.label }}</span>
        </template>
        <div v-else class="uv-empty">
          <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-chart-scatter']" />
          <p>{{ loading ? 'Loading…' : (err || 'Select clustered image(s) to view the UMAP.') }}</p>
        </div>
      </div>
      <div v-if="showLegend && legend.length" class="uv-legend" :style="{ color: legendInk, background: legendBg }">
        <div v-for="l in legend" :key="l.label" class="leg-row">
          <span class="leg-dot" :style="{ background: l.colour }" />
          <span class="leg-lbl">{{ l.label }}</span>
          <span class="leg-n">{{ l.n.toLocaleString() }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .uv-ctrl (.cc-panel-controls) anchors to the plot box */
.uv { position: relative; display: flex; flex-direction: column; flex: 1; min-height: 0; }
.uv-ctrl { display: flex; align-items: center; gap: 8px; padding: 4px 6px; font-size: 12px; color: var(--cc-text-dim); }
/* active (ticked) label toggle: filled accent so it's clearly on/off */
.uv-ctrl .cc-btn.on { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }
.uv-spacer { flex: 1; }
.uv-count { font-variant-numeric: tabular-nums; }
.uv-body { display: flex; flex: 1; min-height: 0; gap: 8px; padding: 0 6px 6px; }
.uv-plot { position: relative; flex: 1; min-height: 0; background: #0d0b1a; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.uv-canvas { position: absolute; inset: 0; width: 100%; height: 100%; }
.uv-label { position: absolute; transform: translate(-50%, -50%); pointer-events: none; font-size: 11px; font-weight: 700;
  color: #111; background: rgba(255,255,255,0.85); border: 1px solid rgba(0,0,0,0.35); border-radius: 3px; padding: 0 4px; line-height: 1.4; z-index: 2; }
.uv-empty { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; color: var(--cc-text-dim); text-align: center; padding: 1rem; }
.uv-empty .pi { font-size: 1.4rem; opacity: 0.6; }
.uv-empty p { margin: 0; font-size: 0.8rem; max-width: 22rem; }
.uv-legend { width: 9.5rem; flex-shrink: 0; overflow-y: auto; border: 1px solid var(--cc-border); border-radius: 5px; padding: 5px; }
.leg-row { display: flex; align-items: center; gap: 5px; padding: 1px 2px; font-size: 11px; }
.leg-dot { width: 0.65rem; height: 0.65rem; border-radius: 50%; flex-shrink: 0; }
/* legend ink is themed inline on .uv-legend (light on dark ground, dark on light) so it stays
   legible in the exported PNG; children inherit it. */
.leg-lbl { flex: 1; color: inherit; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.leg-n { color: inherit; opacity: 0.7; font-variant-numeric: tabular-nums; }
</style>
