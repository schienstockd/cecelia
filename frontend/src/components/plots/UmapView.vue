<!--
  UMAP interactive view (one entry in the interactive-view registry; see components/canvas/
  interactiveViews.ts). Self-contained: fetches the joint embedding for the selected image(s)
  (GET /api/plots/umap → binary Float32 [x, y, clusterCode, popIdx] per point), renders a 2D-canvas
  scatter with a legend, and owns its own controls. Points can be coloured by CLUSTER (default),
  by POPULATION (membership of the picked cell-type pops → popIdx, resolved server-side) or by an
  image ATTRIBUTE (client-side uid→attr join) — "where do the tracked pops / treatments fall on the
  embedding" (docs/todo/UMAP_COLOUR_FACET_PLAN.md). Clustering is set-scope, so all selected images
  share one UMAP space; we fetch per image (tagging each point with its image) and concatenate.

  This is an INTERACTIVE plot (client/WebGL point cloud), distinct from SUMMARY plots (server-
  aggregated, drawn by PlotChart). It is hosted by the generic InteractivePanel, which gives it the
  CanvasPanel chrome — this component knows nothing about panels/canvas, only how to draw a UMAP.

  Props are the generic plot `context` (projectUid/imageUids/setUid/popType/suffix) plus `state` —
  the panel's persisted per-panel options bag (here just `labels`).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import { useLogStore } from '../../stores/log'
import { useProjectStore } from '../../stores/project'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { plotHostToImageURL, rasterPlotToImageURL, downloadDataUrl, downloadBlob, rowsToCsv } from '../../plots/export'
import { paletteRange, type VisProps } from '../../plots/plot'
import { tkey, parseTkey } from '../../plots/series'
import type { SegmentationPops } from '../../plots/types'
import TeleportPopover from '../TeleportPopover.vue'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  popType: 'clust' | 'trackclust'; suffix: string
  // populations whose eye is on in the manager: colour their clusters in the pop colour, grey the
  // rest. Empty → plain colour-by-cluster. Live from the gating store via ClusterPlots' viewContext.
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  vis?: VisProps                 // canvas plot styling — we honour the dark-theme knob + the palette choice
  // colourBy: how to colour the embedding — 'cluster' (default), 'population' (membership of the picked
  // populations — colourPops tkeys), or 'attribute' (each point's image attribute — colourAttr). See
  // docs/todo/UMAP_COLOUR_FACET_PLAN.md.
  state: { labels?: boolean; legend?: boolean
           colourBy?: 'cluster' | 'population' | 'attribute'
           colourPops?: string[]; colourAttr?: string }
}>()
const log = useLogStore()
const project = useProjectStore()
const colourBy = computed({ get: () => props.state.colourBy ?? 'cluster', set: v => (props.state.colourBy = v) })
const colourPops = computed<string[]>({ get: () => props.state.colourPops ?? [], set: v => (props.state.colourPops = v) })
const colourAttr = computed({ get: () => props.state.colourAttr ?? '', set: v => (props.state.colourAttr = v) })
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
// current on-screen plot box (px) — tracked so the label map matches the canvas one under resize
const boxW = ref(0), boxH = ref(0)
// data→px with a SINGLE uniform scale (letterboxed/centred), so the embedding stays isotropic — a UMAP
// warps if x and y stretch independently to fill a non-square box (e.g. after hiding the legend). Used
// by BOTH the dot canvas and the HTML labels so they stay aligned.
function mapPx(x: number, y: number, w: number, h: number): [number, number] {
  const { xMin, xMax, yMin, yMax } = extents.value
  const xr = xMax > xMin ? xMax - xMin : 1, yr = yMax > yMin ? yMax - yMin : 1
  const sc = Math.min(w / xr, h / yr) || 0
  const offX = (w - xr * sc) / 2, offY = (h - yr * sc) / 2
  return [offX + (x - xMin) * sc, offY + (yMax - y) * sc]   // y flips (screen down)
}
const lx = (x: number) => `${mapPx(x, 0, boxW.value, boxH.value)[0]}px`
const ly = (y: number) => `${mapPx(0, y, boxW.value, boxH.value)[1]}px`

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
  const n = pts.length / 2, s = DOT_R * 2
  const groups: number[][] = pal.map(() => [])
  for (let i = 0; i < n; i++) { const g = groups[cats[i]]; if (g) g.push(i) }
  c.globalAlpha = 0.9
  for (let gi = 0; gi < pal.length; gi++) {
    const g = groups[gi]; if (!g.length) continue
    c.fillStyle = pal[gi]
    for (const i of g) {
      const [px, py] = mapPx(pts[2 * i], pts[2 * i + 1], w, h)
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
  boxW.value = w; boxH.value = h   // keep the label map in sync with the canvas box
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
// per-point POPULATION index (colour-by-population; -1 = in none of the picked pops) and per-point
// IMAGE index into props.imageUids (colour-by-attribute joins this → the image's attr client-side).
const popIdxRef = ref<Float32Array | null>(null)
let pointImg: Uint16Array | null = null

// ── colour-by pickers (populations + image attributes), reusing the shared endpoints/helpers ────────
const pickerOpen = ref(false)
const popPickBtn = useTemplateRef<HTMLElement>('popPickBtn')
// populations available to colour by — GRANULARITY-matched to the embedding (trackclust → track-grained
// pops, clust → cell-grained), clusters excluded (we colour by the INPUT cell-type pops, not the
// clusters). Same /api/plots/populations picker the summary canvas uses.
const popGroups = ref<SegmentationPops[]>([])
async function loadPopGroups() {
  if (!props.projectUid || !props.imageUids.length) { popGroups.value = []; return }
  const p = new URLSearchParams({ projectUid: props.projectUid, popType: props.popType,
    popScope: props.popType === 'trackclust' ? 'tracks' : 'cells', includeClusters: 'false' })
  if (props.setUid) { p.set('setUid', props.setUid); p.set('imageUids', props.imageUids.join(',')) }
  else p.set('imageUid', props.imageUids[0])
  try { popGroups.value = await (await fetch(`/api/plots/populations?${p}`)).json() }
  catch { popGroups.value = [] }
}
// attributes available to colour by — derived from the SAME client-side source (project store) the
// colouring reads, so the picker can't offer an attr we can't resolve (and needs no setUid, unlike
// GET /api/plots/attrs — which left the picker empty when the set wasn't the active one).
const attrs = computed(() => project.imageAttrsFor(props.imageUids))
// a colour-pop is stored as a tkey (popType::valueName+pop); the endpoint wants "popType~vnPrefixedPath"
const popToken = (key: string) => { const t = parseTkey(key); return `${t.popType}~${t.valueName}${t.pop}` }
const popLabelForKey = (key: string) => {
  const t = parseTkey(key)
  const grp = popGroups.value.find(g => g.valueName === t.valueName)
  const nm = grp?.populations.find(pp => pp.path === t.pop)?.name ?? (t.pop.split('/').pop() || t.pop)
  return `${t.valueName}·${nm}`
}
const togglePop = (vn: string, pop: string, pt: string) => {
  const k = tkey(pt, vn, pop), cur = colourPops.value
  colourPops.value = cur.includes(k) ? cur.filter(x => x !== k) : [...cur, k]
}
const isPopOn = (vn: string, pop: string, pt: string) => colourPops.value.includes(tkey(pt, vn, pop))

// Generic key→colour path shared by colour-by-population and colour-by-attribute: colour each point by
// an integer key (popIdx, or an attribute-value index), grey the "unassigned" bucket (key < 0), and
// build the legend + centroids from the keys present. Honours the pop-manager palette choice
// (paletteRange). Keeps ONE colouring impl instead of a bespoke one per mode.
function recolourByKey(keyArr: ArrayLike<number>, labelFor: (k: number) => string, unassignedLabel: string) {
  const pts = points.value; if (!pts) return
  const n = keyArr.length
  const counts = new Map<number, number>(), sX = new Map<number, number>(), sY = new Map<number, number>()
  for (let i = 0; i < n; i++) {
    const k = keyArr[i]
    counts.set(k, (counts.get(k) ?? 0) + 1)
    sX.set(k, (sX.get(k) ?? 0) + pts[2 * i]); sY.set(k, (sY.get(k) ?? 0) + pts[2 * i + 1])
  }
  const distinct = [...counts.keys()].sort((a, b) => a - b)   // -1 (unassigned) sorts first
  const pal = (props.vis ? paletteRange(props.vis, distinct.filter(k => k >= 0).length) : null) ?? PALETTE
  const idxOf = new Map<number, number>(), colourFor = new Map<number, string>()
  let pi = 0
  distinct.forEach((k, i) => { idxOf.set(k, i); colourFor.set(k, k < 0 ? UNCLUSTERED : pal[pi++ % pal.length]) })
  const cats = new Float32Array(n)
  for (let i = 0; i < n; i++) cats[i] = idxOf.get(keyArr[i])!
  palette.value = distinct.map(k => colourFor.get(k)!)
  categories.value = cats
  legend.value = distinct.map(k => ({ label: k < 0 ? unassignedLabel : labelFor(k), colour: colourFor.get(k)!, n: counts.get(k)! }))
  centroids.value = distinct.filter(k => k >= 0)
    .map(k => ({ label: labelFor(k), x: sX.get(k)! / counts.get(k)!, y: sY.get(k)! / counts.get(k)! }))
}

// colour-by-attribute: build a per-point key from each point's image attribute value (distinct values
// → 0..m-1, empty → -1), then colour by key. Uses the client-side uid→attr join (project store).
function recolourByAttr() {
  const img = pointImg, pts = points.value
  if (!img || !pts) return
  const vals: string[] = [], keyOf = new Map<string, number>()
  const key = new Int32Array(pts.length / 2)
  for (let i = 0; i < key.length; i++) {
    const v = project.imageAttr(props.imageUids[img[i]] ?? '')[colourAttr.value] ?? ''
    if (!v) { key[i] = -1; continue }
    let k = keyOf.get(v); if (k === undefined) { k = vals.length; vals.push(v); keyOf.set(v, k) }
    key[i] = k
  }
  recolourByKey(key, k => vals[k], 'n/a')
}

// (re)compute the per-point colours + legend. Population / attribute modes go through the generic
// key path; cluster mode keeps its shownPops-aware behaviour (colour each cluster by the population
// that owns it and grey the rest; otherwise plain colour-by-cluster).
function recolour() {
  if (colourBy.value === 'population') {
    const pk = popIdxRef.value
    if (pk) recolourByKey(pk, k => popLabelForKey(colourPops.value[k] ?? ''), 'other')
    return
  }
  if (colourBy.value === 'attribute') { recolourByAttr(); return }
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
    // colour-by-cluster: honour the pop manager's palette choice (vis.palette / userColors — e.g.
    // "distinct"); fall back to the built-in PALETTE for the default 'standard' (paletteRange → null).
    const pal = (props.vis ? paletteRange(props.vis, distinct.filter(c => c >= 0).length) : null) ?? PALETTE
    let pi = 0
    distinct.forEach((c, i) => { idxOf.set(c, i); colourFor.set(c, c < 0 ? UNCLUSTERED : pal[pi++ % pal.length]) })
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
    // wire = flat Float32 [x, y, clusterCode, popIdx] per point (16 B). We fetch per image (so we can
    // tag each point with its image → colour/facet by attribute) and concatenate. colourPops (only in
    // population mode) asks the server to resolve per-point membership → popIdx.
    const cp = colourBy.value === 'population' ? colourPops.value.map(popToken).filter(Boolean) : []
    const quads: number[] = []
    const imgOf: number[] = []
    for (let ui = 0; ui < props.imageUids.length; ui++) {
      const q = new URLSearchParams({ projectUid: props.projectUid, imageUid: props.imageUids[ui], popType: props.popType, suffix: props.suffix })
      if (cp.length) q.set('colourPops', cp.join(','))
      const res = await fetch(`/api/plots/umap?${q}`)
      if (!res.ok) continue
      const f = new Float32Array(await res.arrayBuffer())
      for (let i = 0; i < f.length; i++) quads.push(f[i])
      for (let i = 0; i < f.length / 4; i++) imgOf.push(ui)
    }
    const n = Math.floor(quads.length / 4)
    if (n === 0) {
      points.value = null; categories.value = null; legend.value = []; centroids.value = []
      err.value = `No UMAP at suffix “${props.suffix}”. Run clustering with “Calculate UMAP” enabled.`
      return
    }
    const pts = new Float32Array(n * 2), codes = new Float32Array(n), popIdx = new Float32Array(n)
    const img = new Uint16Array(n)
    let xMin = Infinity, xMax = -Infinity, yMin = Infinity, yMax = -Infinity
    for (let i = 0; i < n; i++) {
      const x = quads[4 * i], y = quads[4 * i + 1]
      pts[2 * i] = x; pts[2 * i + 1] = y; codes[i] = quads[4 * i + 2]; popIdx[i] = quads[4 * i + 3]; img[i] = imgOf[i]
      if (x < xMin) xMin = x; if (x > xMax) xMax = x
      if (y < yMin) yMin = y; if (y > yMax) yMax = y
    }
    const px = (xMax - xMin || 1) * 0.04, py = (yMax - yMin || 1) * 0.04
    extents.value = { xMin: xMin - px, xMax: xMax + px, yMin: yMin - py, yMax: yMax + py }
    points.value = pts
    codesRef.value = codes
    popIdxRef.value = popIdx
    pointImg = img
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

watch([() => props.projectUid, () => props.imageUids.join(','), () => props.popType, () => props.suffix],
  () => { load(); loadPopGroups() })
useDataRefresh(() => props.imageUids, load)   // refetch when a task finishes on one of THESE images
// highlight a pop / tick clusters → recolour from cached codes (no refetch)
watch(() => JSON.stringify((props.shownPops ?? []).map(p => [p.colour, p.clusterIds])), recolour)
// re-colour when the pop manager's palette choice changes (e.g. standard → distinct)
watch(() => [props.vis?.palette, props.vis?.userColors], recolour)
// colour-by: switching to population needs the per-point popIdx (refetch); cluster/attribute just
// recolour from cached data. Changing the picked populations refetches (server resolves membership).
watch(colourBy, v => { v === 'population' ? load() : recolour() })
watch(() => colourPops.value.join(','), () => { if (colourBy.value === 'population') load() })
watch(colourAttr, () => { if (colourBy.value === 'attribute') recolour() })
// lazily fill the population picker when that mode is first entered (attrs is a store-derived computed)
watch(colourBy, v => { if (v === 'population' && !popGroups.value.length) loadPopGroups() }, { immediate: true })
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
              v-tooltip.bottom="'Toggle centroid labels'"><i class="pi pi-tag" /> #</button>
      <button class="cc-btn cc-btn-ghost" :class="{ on: showLegend }" @click="showLegend = !showLegend"
              v-tooltip.bottom="'Toggle the legend'"><i class="pi pi-list" /></button>
      <!-- colour the embedding by cluster (default), by the picked populations' membership, or by an
           image attribute — reproduces the paper's "where do the tracked pops / treatments fall". -->
      <label class="uv-cby" v-tooltip.bottom="'Colour points by'">
        <i class="pi pi-palette" />
        <select v-model="colourBy">
          <option value="cluster">cluster</option>
          <option value="population">population</option>
          <option value="attribute">attribute</option>
        </select>
      </label>
      <!-- population mode: pick which populations to colour by (grouped by segmentation) -->
      <template v-if="colourBy === 'population'">
        <button ref="popPickBtn" class="cc-btn cc-btn-ghost" :class="{ on: pickerOpen }"
                @click="pickerOpen = !pickerOpen" v-tooltip.bottom="'Choose populations'">
          <i class="pi pi-sitemap" /> {{ colourPops.length || 'pick' }}
        </button>
        <TeleportPopover v-model="pickerOpen" :anchor="popPickBtn" placement="bottom-start">
          <div class="uv-pop">
            <div v-if="!popGroups.length" class="uv-pop-empty">No populations in the clustered segmentations.</div>
            <template v-for="grp in popGroups" :key="grp.valueName">
              <div v-if="grp.populations.length" class="uv-pop-head">{{ grp.valueName }}</div>
              <div v-for="p in grp.populations" :key="p.popType + grp.valueName + p.path"
                   class="uv-pop-row" :class="{ on: isPopOn(grp.valueName, p.path, p.popType) }"
                   @click="togglePop(grp.valueName, p.path, p.popType)">
                <i :class="isPopOn(grp.valueName, p.path, p.popType) ? 'pi pi-check-square' : 'pi pi-stop'" />
                <span class="uv-pop-name">{{ p.name }}</span>
                <span v-if="p.popType !== 'live'" class="uv-pop-tag">{{ p.popType }}</span>
              </div>
            </template>
          </div>
        </TeleportPopover>
      </template>
      <!-- attribute mode: pick which image attribute to colour by -->
      <select v-else-if="colourBy === 'attribute'" v-model="colourAttr" class="uv-attr"
              v-tooltip.bottom="'Image attribute'">
        <option value="" disabled>attribute…</option>
        <option v-for="a in attrs" :key="a.name" :value="a.name">{{ a.name }}</option>
      </select>
      <span class="uv-spacer" />
      <span v-if="total" class="uv-count">{{ total.toLocaleString() }} {{ unit }} · {{ legend.length }} {{ colourBy === 'cluster' ? 'clusters' : 'groups' }}</span>
    </div>
    <div ref="plotEl" class="uv-body">
      <!-- during export the inner ground MUST be transparent, else the overlay pass (host→SVG) paints
           this opaque div OVER the transparent hi-res points (the points-missing bug). On-screen it
           carries the scatter ground for the letterbox margins. -->
      <!-- forceLight = board/PDF export: drop the plot's bounding-box border (+ rounding) so the
           exported figure has no frame around the UMAP; on-screen it keeps the border. -->
      <div class="uv-plot" :style="{ background: forceLight ? 'transparent' : scatterGround,
                                     border: forceLight ? 'none' : undefined,
                                     borderRadius: forceLight ? '0' : undefined }">
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
/* colour-by controls */
.uv-cby { display: inline-flex; align-items: center; gap: 4px; color: var(--cc-text-dim); }
.uv-cby select, .uv-attr { font-size: 12px; padding: 2px 4px; }
.uv-attr { max-width: 9rem; }
/* population picker popover (inner layout only — TeleportPopover gives surface/border/shadow) */
.uv-pop { width: 15rem; max-height: 18rem; overflow-y: auto; }
.uv-pop-empty { padding: 10px; color: var(--cc-text-dim); font-size: 12px; }
.uv-pop-head { padding: 4px 8px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.06em; position: sticky; top: 0; }
.uv-pop-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px; cursor: pointer; font-size: 12px; color: var(--cc-text); }
.uv-pop-row:hover { background: var(--cc-surface-2); }
.uv-pop-row.on { color: var(--cc-accent); }
.uv-pop-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.uv-pop-tag { font-size: 9px; text-transform: uppercase; color: var(--cc-text-dim); border: 1px solid var(--cc-border); border-radius: 3px; padding: 0 3px; }
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
