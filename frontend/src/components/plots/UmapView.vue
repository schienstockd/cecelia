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
import { plotHostToImageURL, rasterPlotToImageURL, downloadDataUrl, downloadBlob, rowsToCsv,
         svgDoc, svgCircles, svgText, svgRect, downloadText, svgSizeWarning } from '../../plots/export'
import { paletteRange, PALETTES, type VisProps } from '../../plots/plot'
import { tkey, parseTkey } from '../../plots/series'
import type { SegmentationPops } from '../../plots/types'
import TeleportPopover from '../TeleportPopover.vue'
import SquarePlot from './SquarePlot.vue'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  popType: 'clust' | 'trackclust' | 'region'; suffix: string
  // populations whose eye is on in the manager: colour their clusters in the pop colour, grey the
  // rest. Empty → plain colour-by-cluster. Live from the gating store via ClusterPlots' viewContext.
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  vis?: VisProps                 // canvas plot styling — we honour the dark-theme knob + the palette choice
  // colourBy: how to colour the embedding — 'cluster' (default), 'population' (membership of the picked
  // populations — colourPops tkeys), or 'attribute' (each point's image attribute — colourAttr).
  // facetBy: split into small multiples — 'none' (default), 'attribute' (facetAttr) or 'population'.
  // See docs/todo/UMAP_COLOUR_FACET_PLAN.md.
  state: { labels?: boolean
           colourBy?: 'cluster' | 'population' | 'attribute'
           colourPops?: string[]; colourAttr?: string
           facetBy?: 'none' | 'attribute' | 'population'; facetAttr?: string }
}>()
const log = useLogStore()
const project = useProjectStore()
const colourBy = computed({ get: () => props.state.colourBy ?? 'cluster', set: v => (props.state.colourBy = v) })
const colourPops = computed<string[]>({ get: () => props.state.colourPops ?? [], set: v => (props.state.colourPops = v) })
const colourAttr = computed({ get: () => props.state.colourAttr ?? '', set: v => (props.state.colourAttr = v) })
const facetBy = computed({ get: () => props.state.facetBy ?? 'none', set: v => (props.state.facetBy = v) })
const facetAttr = computed({ get: () => props.state.facetAttr ?? '', set: v => (props.state.facetAttr = v) })
// the population dimension (popIdx) is needed whenever EITHER colour OR facet is by population
const usePopIdx = computed(() => colourBy.value === 'population' || facetBy.value === 'population')
const labels = computed({ get: () => props.state.labels !== false, set: v => (props.state.labels = v) })
// show/hide the population legend — follows the picker's Legend option (vis.legend), like the other
// plots, rather than a UMAP-only toggle. Default on when no vis is supplied.
const showLegend = computed(() => props.vis?.legend !== false)
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
// cluster label chip: the ggplot `geom_label` look (white box, thin black border, black text) in BOTH
// themes — it reads on the dark on-screen ground AND the white PDF export (the old light-export chip
// was a dark box with white text, which didn't match the R figures). See geom_label_repel in
// plotClustersUMAPServer.R.
const labelStyle = computed(() => ({ color: '#111', background: 'rgba(255,255,255,0.9)', borderColor: 'rgba(0,0,0,0.55)' }))
const legendInk = computed(() => (dark.value ? '#e6e6e6' : '#111'))
// legend box background follows the theme too — in light mode the dark ink sat on the app's dark
// panel surface (unreadable on the canvas); give it a light box so the pop names are legible.
const legendBg = computed(() => (dark.value ? 'transparent' : '#ffffff'))

// default cluster colours = the Cecelia house palette (the R behaviour-figure colours: yellow /
// steel-blue / crimson / grey lead), so the UMAP matches the published look out of the box. Extra
// hues follow for runs with >8 clusters. The pop-manager palette knob still overrides via paletteRange.
const PALETTE = [
  ...PALETTES.cecelia,
  '#ef4444', '#10b981', '#a78bfa', '#f97316', '#84cc16', '#8b5cf6', '#f43f5e', '#06b6d4',
  '#a3e635', '#d946ef', '#fb7185', '#2dd4bf',
]
const UNCLUSTERED = '#555a6e'
// faint whole-cloud backdrop behind each facet — dark grey on the dark ground, light grey on the light
// (export) ground, so it always reads as a subtle "shape" layer rather than competing with the dots.
const ghostColour = () => (dark.value ? '#3a3f4b' : '#d4d7dd')
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

// ── faceting: split points into small multiples by an attribute value / population ──────────────────
// facetBy='attribute' groups by each point's image attribute (facetAttr); 'population' by its popIdx;
// 'none' → one facet. All facets share the SAME extents + colour map (comparable), and a single legend.
const FACET_TITLE_H = 15   // px reserved at each cell's top for its title
// facets present in the data (first-appearance order; the 'n/a'/'other' catch-all sorts last). The
// per-point label source is precomputed once per pass (attr: per-IMAGE value; population: per-popIdx
// label) so this stays O(n), not O(n·images).
const facets = computed<{ label: string; idx: number[] }[]>(() => {
  const pts = points.value, img = pointImg.value
  if (!pts || facetBy.value === 'none') return [{ label: '', idx: [] }]   // idx unused when single
  const n = pts.length / 2
  const attrByImg = facetBy.value === 'attribute'
    ? props.imageUids.map(uid => project.imageAttr(uid)[facetAttr.value] || 'n/a') : null
  const popLabel = facetBy.value === 'population'
    ? colourPops.value.map(k => popLabelForKey(k)) : null
  const labelOf = (i: number): string => {
    if (attrByImg) return attrByImg[img?.[i] ?? -1] ?? 'n/a'
    if (popLabel) { const k = popIdxRef.value ? popIdxRef.value[i] : -1; return k >= 0 ? (popLabel[k] ?? 'other') : 'other' }
    return ''
  }
  const order: string[] = [], byLabel = new Map<string, number[]>()
  for (let i = 0; i < n; i++) {
    const l = labelOf(i)
    let a = byLabel.get(l); if (!a) { a = []; byLabel.set(l, a); order.push(l) }
    a.push(i)
  }
  const tail = (l: string) => (l === 'n/a' || l === 'other' ? 1 : 0)
  order.sort((a, b) => tail(a) - tail(b))
  return order.map(l => ({ label: l, idx: byLabel.get(l)! }))
})
const facetGrid = (nf: number) => { const cols = Math.ceil(Math.sqrt(nf)); return { cols, rows: Math.ceil(nf / cols) } }
// cell rect + inner plot sub-rect (below the title strip) for facet fi in a w×h box
function facetCell(fi: number, nf: number, w: number, h: number) {
  const { cols } = facetGrid(nf)
  const col = fi % cols, row = Math.floor(fi / cols)
  const cw = w / cols, ch = h / facetGrid(nf).rows
  const ox = col * cw, oy = row * ch, th = nf > 1 ? FACET_TITLE_H : 0, pad = nf > 1 ? 4 : 0
  return { ox, oy, cw, ch, px: ox + pad, py: oy + th, pw: cw - 2 * pad, ph: ch - th - pad }
}
// data→px into an arbitrary sub-rect, SHARED extents (uniform isotropic scale, letterboxed)
function mapRect(x: number, y: number, r: { px: number; py: number; pw: number; ph: number }): [number, number] {
  const { xMin, xMax, yMin, yMax } = extents.value
  const xr = xMax > xMin ? xMax - xMin : 1, yr = yMax > yMin ? yMax - yMin : 1
  const sc = Math.min(r.pw / xr, r.ph / yr) || 0
  const offX = (r.pw - xr * sc) / 2, offY = (r.ph - yr * sc) / 2
  return [r.px + offX + (x - xMin) * sc, r.py + offY + (yMax - y) * sc]
}
// facet titles as HTML overlay (CSS px), composited into the export like the centroid labels/legend
const facetTitles = computed(() => {
  const fs = facets.value
  if (facetBy.value === 'none' || fs.length <= 1) return []
  return fs.map((f, fi) => { const c = facetCell(fi, fs.length, boxW.value, boxH.value)
    return { label: f.label, x: c.ox + c.cw / 2, y: c.oy + 1 } })
})

// ── 2D dot render (no WebGL) ──────────────────────────────────────────────────────────────────────
// Draw each point via the SAME data→px map as the labels (lx/ly), so cluster labels sit exactly on
// their dots — regl-scatterplot's aspectRatio fill drifted from the HTML-stretched labels. Colour by
// category (palette index), bucketed so fillStyle is set once per cluster.
const dotsEl = useTemplateRef<HTMLCanvasElement>('dotsEl')
// the square plot box — ALWAYS rendered (unlike the canvas, which is behind v-if until data loads), so
// the ResizeObserver attaches at mount and resizes actually fire redraw (the "labels don't reposition"
// bug: the observer was on the conditional canvas and never attached when the panel started empty).
const plotBoxEl = useTemplateRef<HTMLElement>('plotBoxEl')
let dctx: CanvasRenderingContext2D | null = null
let dro: ResizeObserver | null = null
// point radius + opacity follow the pop-manager knobs (vis.pointSize / vis.pointOpacity); the redraw
// watch below fires when they change so the canvas repaints without a refetch.
const dotR = computed(() => Math.max(0.5, props.vis?.pointSize ?? 2))
const dotAlpha = computed(() => Math.min(1, Math.max(0.05, props.vis?.pointOpacity ?? 0.9)))
const TAU = Math.PI * 2
// draw one filled circle (fillStyle is set by the caller, once per colour bucket)
function dot(c: CanvasRenderingContext2D, px: number, py: number, r: number) {
  c.beginPath(); c.arc(px, py, r, 0, TAU); c.fill()
}
// centroid labels + facet titles scale with the pop-manager font-size slider (vis.fontSize)
const labelFont = computed(() => props.vis?.fontSize ?? 11)
// paint a set of point indices via `toPx`, bucketed by category so fillStyle is set once per colour
function paintInto(c: CanvasRenderingContext2D, idx: number[], toPx: (i: number) => [number, number]) {
  const cats = categories.value!, pal = palette.value
  const groups: number[][] = pal.map(() => [])
  for (const i of idx) { const g = groups[cats[i]]; if (g) g.push(i) }
  for (let gi = 0; gi < pal.length; gi++) {
    const g = groups[gi]; if (!g.length) continue
    c.fillStyle = pal[gi]
    for (const i of g) { const [px, py] = toPx(i); dot(c, px, py, dotR.value) }
  }
}
function paintDots(c: CanvasRenderingContext2D, w: number, h: number) {
  const pts = points.value, cats = categories.value, pal = palette.value
  if (!pts || !cats || !pal.length) return
  const alpha = dotAlpha.value
  c.globalAlpha = alpha
  const fs = facets.value
  if (facetBy.value === 'none' || fs.length <= 1) {
    const n = pts.length / 2, groups: number[][] = pal.map(() => [])
    for (let i = 0; i < n; i++) { const g = groups[cats[i]]; if (g) g.push(i) }
    for (let gi = 0; gi < pal.length; gi++) {
      const g = groups[gi]; if (!g.length) continue
      c.fillStyle = pal[gi]
      for (const i of g) { const [px, py] = mapPx(pts[2 * i], pts[2 * i + 1], w, h); dot(c, px, py, dotR.value) }
    }
  } else {
    const n = pts.length / 2
    for (let fi = 0; fi < fs.length; fi++) {
      const cell = facetCell(fi, fs.length, w, h)
      // GHOST: the whole cloud in faint grey behind each facet, so the UMAP shape stays legible and
      // facets are comparable (you see where this facet's points sit within the full embedding).
      c.globalAlpha = alpha; c.fillStyle = ghostColour()
      for (let i = 0; i < n; i++) { const [px, py] = mapRect(pts[2 * i], pts[2 * i + 1], cell); dot(c, px, py, dotR.value) }
      paintInto(c, fs[fi].idx, i => mapRect(pts[2 * i], pts[2 * i + 1], cell))
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
const pointImg = ref<Uint16Array | null>(null)   // reactive so facets/attr colouring recompute on load

// ── colour-by pickers (populations + image attributes), reusing the shared endpoints/helpers ────────
// colour + facet controls live in ONE options popover (the bar gets crowded, esp. docked on the board)
const optsOpen = ref(false)
const optsBtn = useTemplateRef<HTMLElement>('optsBtn')
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
  const img = pointImg.value, pts = points.value
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
    const cp = usePopIdx.value ? colourPops.value.map(popToken).filter(Boolean) : []
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
    pointImg.value = img
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
// colour-by re-colours from cached data; the fetch (for popIdx) is driven by usePopIdx below, so this
// avoids a double load when switching to population also flips usePopIdx.
watch(colourBy, () => recolour())
watch(() => colourPops.value.join(','), () => { if (usePopIdx.value) load() })
watch(colourAttr, () => { if (colourBy.value === 'attribute') recolour() })
// usePopIdx flips (colour OR facet by population toggled on/off) → refetch to get/drop popIdx
watch(usePopIdx, () => load())
// facet controls only re-partition + redraw (no refetch, except the popIdx case above); attribute
// faceting reads the store-derived facets computed, so a redraw is enough
watch([facetBy, facetAttr, facets], () => nextTick(redraw))
// lazily fill the population picker when population mode (colour or facet) is first entered
watch(usePopIdx, v => { if (v && !popGroups.value.length) loadPopGroups() }, { immediate: true })
// redraw the 2D dots when the data / colouring / extents change; also on theme flip (the facet ghost
// backdrop is theme-coloured — dark grey on dark, light grey on light/export)
watch([points, categories, palette, extents], () => nextTick(redraw), { deep: true })
watch(dark, () => nextTick(redraw))
// styling knobs that only affect the canvas paint (no refetch): point size + opacity
watch([() => props.vis?.pointSize, () => props.vis?.pointOpacity], () => nextTick(redraw))
onMounted(() => {
  load()
  // observe the STABLE square box (present from mount) so a panel resize always repaints + repositions
  // the HTML labels/titles (which read boxW/boxH set in redraw)
  if (plotBoxEl.value) { dro = new ResizeObserver(() => redraw()); dro.observe(plotBoxEl.value) }
  nextTick(redraw)
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
  else if (kind === 'svg') {
    const svg = exportSvg()
    if (svg) {
      const warn = svgSizeWarning(svg, `UMAP (${total.value.toLocaleString()} ${unit.value})`)
      if (warn) log.warn(warn, { source: 'cluster' })
      downloadText(`${stem}.svg`, svg, 'image/svg+xml')
    }
  }
  else if (kind === 'csv') {
    const pts = points.value, codes = codesRef.value
    if (!pts || !codes) return
    const rows = Array.from({ length: codes.length }, (_, i) =>
      ({ x: pts[2 * i], y: pts[2 * i + 1], cluster: codes[i] < 0 ? '' : codes[i] }))
    downloadBlob(`${stem}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  }
}

// TRUE-VECTOR SVG export (docs/PLOTS.md): re-emit the SAME colour-bucketed dots as `<circle>`s grouped
// by cluster/population — one `<g fill=…>` per group, so selecting a group in Illustrator and changing
// its fill recolours that whole cluster at once. Uses the identical mapPx/mapRect maps + facet layout as
// the on-screen paint, so the vector output matches the canvas 1:1. Always light-themed (figure on white).
function exportSvg(): string {
  const pts = points.value, cats = categories.value, pal = palette.value
  if (!pts || !cats || !pal.length) return ''
  const S = Math.round(boxH.value || boxW.value || 600)   // square plot area (vector = resolution-free)
  const LW = (showLegend.value && legend.value.length) ? 150 : 0
  const alpha = dotAlpha.value, r = dotR.value, fsz = labelFont.value
  const ghostLight = '#d4d7dd'
  // ggplot geom_label look: white box + thin dark border + dark text (matches labelStyle, both themes)
  const chip = (px: number, py: number, s: string): string => {
    const w = s.length * fsz * 0.6 + 8, h = fsz + 6
    return svgRect(px - w / 2, py - h / 2, w, h, { fill: '#ffffff', stroke: 'rgba(0,0,0,0.55)', width: 0.8, rx: 3, opacity: 0.9 }) +
           svgText(px, py + fsz * 0.35, s, { fill: '#111', size: fsz, anchor: 'middle', weight: 700 })
  }
  let body = ''
  const fs = facets.value
  if (facetBy.value === 'none' || fs.length <= 1) {
    const n = pts.length / 2, groups: number[][] = pal.map(() => [])
    for (let i = 0; i < n; i++) { const g = groups[cats[i]]; if (g) g.push(i) }
    for (let gi = 0; gi < pal.length; gi++) {
      const g = groups[gi]; if (!g.length) continue
      body += svgCircles(g.map(i => mapPx(pts[2 * i], pts[2 * i + 1], S, S)),
                         { fill: pal[gi], opacity: alpha, r, label: legend.value[gi]?.label })
    }
    if (labels.value) for (const c of centroids.value) { const [px, py] = mapPx(c.x, c.y, S, S); body += chip(px, py, c.label) }
  } else {
    const n = pts.length / 2
    for (let fi = 0; fi < fs.length; fi++) {
      const cell = facetCell(fi, fs.length, S, S)
      const ghost: [number, number][] = []
      for (let i = 0; i < n; i++) ghost.push(mapRect(pts[2 * i], pts[2 * i + 1], cell))
      body += svgCircles(ghost, { fill: ghostLight, opacity: alpha, r, label: 'ghost' })
      const groups: number[][] = pal.map(() => [])
      for (const i of fs[fi].idx) { const g = groups[cats[i]]; if (g) g.push(i) }
      for (let gi = 0; gi < pal.length; gi++) {
        const g = groups[gi]; if (!g.length) continue
        body += svgCircles(g.map(i => mapRect(pts[2 * i], pts[2 * i + 1], cell)),
                           { fill: pal[gi], opacity: alpha, r, label: legend.value[gi]?.label })
      }
      body += svgText(cell.ox + cell.cw / 2, cell.oy + fsz, fs[fi].label, { fill: '#111', size: fsz, anchor: 'middle', weight: 700 })
    }
  }
  // legend column (swatch + label + count per row) as vector, right of the plot
  if (LW) {
    let ly0 = 14
    for (const l of legend.value) {
      body += svgRect(S + 8, ly0 - 8, 9, 9, { fill: l.colour, rx: 1 })
      body += svgText(S + 21, ly0, `${l.label}  (${l.n.toLocaleString()})`, { fill: '#111', size: 11 })
      ly0 += 15
    }
  }
  return svgDoc({ width: S + LW, height: S, background: '#ffffff', body })
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
defineExpose({ exportFormats: ['png', 'svg', 'csv'], exportAs, exportImage })
</script>

<template>
  <div class="uv">
    <div class="uv-ctrl cc-panel-controls">
      <button class="cc-btn cc-btn-ghost" :class="{ on: labels }" @click="labels = !labels"
              v-tooltip.bottom="'Toggle centroid labels'"><i class="pi pi-tag" /> #</button>
      <!-- legend visibility follows the picker's Legend option (vis.legend) — no separate toggle here -->
      <!-- colour + facet controls live in a single options popover to keep the (often docked) bar tidy -->
      <button ref="optsBtn" class="cc-btn cc-btn-ghost" :class="{ on: optsOpen }" @click="optsOpen = !optsOpen"
              v-tooltip.bottom="'Colour & facet options'"><i class="pi pi-palette" /> options</button>
      <TeleportPopover v-model="optsOpen" :anchor="optsBtn" placement="bottom-start">
        <div class="uv-opts">
          <label class="uv-opt"><span>Colour by</span>
            <select v-model="colourBy">
              <option value="cluster">cluster</option>
              <option value="population">population</option>
              <option value="attribute">attribute</option>
            </select>
          </label>
          <label v-if="colourBy === 'attribute'" class="uv-opt"><span>Colour attribute</span>
            <select v-model="colourAttr">
              <option value="" disabled>attribute…</option>
              <option v-for="a in attrs" :key="a.name" :value="a.name">{{ a.name }}</option>
            </select>
          </label>
          <label class="uv-opt"><span>Facet by</span>
            <select v-model="facetBy">
              <option value="none">no facet</option>
              <option value="attribute">attribute</option>
              <option value="population">population</option>
            </select>
          </label>
          <label v-if="facetBy === 'attribute'" class="uv-opt"><span>Facet attribute</span>
            <select v-model="facetAttr">
              <option value="" disabled>attribute…</option>
              <option v-for="a in attrs" :key="a.name" :value="a.name">{{ a.name }}</option>
            </select>
          </label>
          <!-- populations to colour/facet by (shown whenever EITHER uses population) -->
          <template v-if="usePopIdx">
            <div class="uv-opt-sep">Populations</div>
            <div class="uv-pop">
              <div v-if="!popGroups.length" class="uv-pop-empty cc-muted">No populations in the clustered segmentations.</div>
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
          </template>
        </div>
      </TeleportPopover>
      <span class="uv-spacer" />
      <span v-if="total" class="uv-count">{{ total.toLocaleString() }} {{ unit }} · {{ legend.length }} {{ colourBy === 'cluster' ? 'clusters' : 'groups' }}</span>
    </div>
    <div ref="plotEl" class="uv-body">
      <!-- SquarePlot keeps the embedding a 1:1 square (centred in the panel), so it never warps and the
           HTML overlays (centroid labels / facet titles) line up with the canvas dots exactly. -->
      <SquarePlot class="uv-square">
        <!-- during export the inner ground MUST be transparent, else the overlay pass (host→SVG) paints
             this opaque div OVER the transparent hi-res points. forceLight = board/PDF export: drop the
             bounding-box border so the exported figure has no frame; on-screen it keeps the border. -->
        <div ref="plotBoxEl" class="uv-plot" :style="{ background: forceLight ? 'transparent' : scatterGround,
                                       border: forceLight ? 'none' : undefined,
                                       borderRadius: forceLight ? '0' : undefined }">
          <template v-if="points && points.length">
            <canvas ref="dotsEl" class="uv-canvas" />
            <!-- centroid labels only when NOT faceted (their full-box positions don't map into cells) -->
            <span v-for="c in centroids" v-show="labels && facetBy === 'none'" :key="c.label" class="uv-label"
                  :style="{ left: lx(c.x), top: ly(c.y), fontSize: labelFont + 'px', ...labelStyle }">{{ c.label }}</span>
            <!-- per-facet titles (small multiples) -->
            <span v-for="(t, ti) in facetTitles" :key="'f'+ti" class="uv-facet-title"
                  :style="{ left: t.x + 'px', top: t.y + 'px', fontSize: labelFont + 'px', color: legendInk }">{{ t.label }}</span>
          </template>
          <div v-else class="uv-empty">
            <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-chart-scatter']" />
            <p>{{ loading ? 'Loading…' : (err || 'Select clustered image(s) to view the UMAP.') }}</p>
          </div>
        </div>
      </SquarePlot>
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
/* colour & facet options popover (inner layout only — TeleportPopover gives surface/border/shadow) */
.uv-opts { width: 15rem; display: flex; flex-direction: column; gap: 8px; padding: 10px; }
.uv-opt { display: flex; align-items: center; justify-content: space-between; gap: 8px; font-size: 12px; color: var(--cc-text-dim); }
.uv-opt select { font-size: 12px; padding: 2px 4px; max-width: 8.5rem; }
.uv-opt-sep { font-size: 10px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--cc-text-dim);
  border-top: 1px solid var(--cc-border); padding-top: 6px; margin-top: 2px; }
/* population checklist (inside the options popover) */
.uv-pop { max-height: 14rem; overflow-y: auto; border: 1px solid var(--cc-border); border-radius: 5px; }
.uv-pop-empty { padding: 10px; }   /* + .cc-muted */
.uv-pop-head { padding: 4px 8px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.06em; position: sticky; top: 0; }
.uv-pop-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px; cursor: pointer; font-size: 12px; color: var(--cc-text); }
.uv-pop-row:hover { background: var(--cc-surface-2); }
.uv-pop-row.on { color: var(--cc-accent); }
.uv-pop-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.uv-pop-tag { font-size: 9px; text-transform: uppercase; color: var(--cc-text-dim); border: 1px solid var(--cc-border); border-radius: 3px; padding: 0 3px; }
.uv-body { display: flex; flex: 1; min-height: 0; gap: 8px; padding: 0 6px 6px; }
/* .uv-square (SquarePlot) provides the centred 1:1 box; .uv-plot fills it and carries the ground/frame */
.uv-plot { position: absolute; inset: 0; background: #0d0b1a; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.uv-canvas { position: absolute; inset: 0; width: 100%; height: 100%; }
.uv-label { position: absolute; transform: translate(-50%, -50%); pointer-events: none; font-weight: 700;
  color: #111; background: rgba(255,255,255,0.9); border: 1px solid rgba(0,0,0,0.55); border-radius: 3px; padding: 0 4px; line-height: 1.4; z-index: 2; }
/* small-multiples facet title: centred at the top of each cell (positioned in CSS px from facetTitles) */
.uv-facet-title { position: absolute; transform: translateX(-50%); pointer-events: none; z-index: 2;
  font-size: 10px; font-weight: 700; white-space: nowrap; }
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
