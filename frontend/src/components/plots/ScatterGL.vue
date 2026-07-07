<!--
  WebGL scatter (regl-scatterplot) for flow-cytometry-scale point clouds.
  Receives already-transformed interleaved points [x0,y0,x1,y1,…] plus the (fixed) transformed
  extents, normalises to regl's [-1,1] space, and draws.

  FlowJo/OMIQ-style rendering:
   - colorMode='density' → per-point local density (client-side binning, log-scaled) mapped
     across the blue-heat ramp via regl's `colorBy:'valueA'` (pseudocolour). See docs/UI.md.
   - colorMode='flat'    → single dim grey (used as a faint backdrop under contours / pop layers).

  Zoom sync: we hand a minimal x/y scale to regl (it only ever calls .domain()/.range()).
  regl mutates the scale DOMAIN as the user pans/zooms; on every 'view' we emit the live
  visible data extents so the canvas2D overlays (PlotLayers, GateOverlay) can rescale with us.

  Selection is disabled (gating uses explicit gates) — we deselect on any select and neutralise
  the active/hover colours so clicks never paint points blue.
-->
<script setup lang="ts">
import { watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
const props = withDefaults(defineProps<{
  points: Float32Array | null              // interleaved [x,y,...] in transformed coords
  extents: Ext                             // FIXED extents that map to the initial [-1,1] view
  colorMode?: 'density' | 'flat' | 'category'
  pointSize?: number
  opacity?: number
  flatColor?: string
  // category mode (e.g. colour-by-cluster on a UMAP): one palette-index per point (parallel to
  // `points`) + the colour palette. ScatterGL stays dumb — the caller maps codes→palette slots.
  categories?: Float32Array | null
  palette?: string[]
  backgroundColor?: string                 // scatter ground; caller themes it (UMAP dark/light)
}>(), { colorMode: 'density', pointSize: 3, opacity: 1, flatColor: '#8b8b8b',
        categories: null, palette: () => [], backgroundColor: '#0d0b1a' })

// FlowJo "pseudocolour" blue-heat ramp (R: .flowColorRampBlueHeat, flowHelpers.R:775),
// low end lifted off pure black so sparse points stay visible on the dark background, and
// interpolated to 256 stops so regl shows a smooth gradient (not 5 hard bands).
function hexRgb(h: string): [number, number, number] {
  return [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)]
}
function buildRamp(anchors: string[], n: number): string[] {
  const rgb = anchors.map(hexRgb)
  const out: string[] = []
  const hex = (n: number) => n.toString(16).padStart(2, '0')
  for (let i = 0; i < n; i++) {
    const t = (i / (n - 1)) * (rgb.length - 1)
    const k = Math.min(rgb.length - 2, Math.floor(t)), f = t - k
    const c = [0, 1, 2].map(j => Math.round(rgb[k][j] + (rgb[k + 1][j] - rgb[k][j]) * f))
    out.push(`#${hex(c[0])}${hex(c[1])}${hex(c[2])}`)   // hex — regl's colour parser needs it
  }
  return out
}
const RAMP = buildRamp(['#0b1a4d', '#1793ff', '#04fa00', '#ffa805', '#ff3856'], 256)

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
// regl-scatterplot has TS types but the instance is dynamic; keep it loosely typed.
let scatterplot: any = null
let ro: ResizeObserver | null = null

async function getCreate() {
  const mod = await import('regl-scatterplot')
  return (mod.default ?? mod) as any
}

// regl-scatterplot positions draw() points as DEVICE coords [-1,1] (the x/y scales we provide
// are only for zoom reporting, not positioning) — so normalise with the FIXED extents. The
// canvas2D overlays map with the same extents (xMin→left, xMax→right, yMax→top), so they align.
function xy(): { x: Float32Array; y: Float32Array } {
  const p = props.points
  if (!p || p.length === 0) return { x: new Float32Array(0), y: new Float32Array(0) }
  const { xMin, xMax, yMin, yMax } = props.extents
  const xs = xMax > xMin ? xMax - xMin : 1, ys = yMax > yMin ? yMax - yMin : 1
  const n = p.length / 2
  const x = new Float32Array(n)
  const y = new Float32Array(n)
  for (let i = 0; i < n; i++) {
    x[i] = (2 * (p[2 * i] - xMin)) / xs - 1
    y[i] = (2 * (p[2 * i + 1] - yMin)) / ys - 1   // regl y up → yMax maps to +1 (top)
  }
  return { x, y }
}

// separable box blur (a couple of passes ≈ Gaussian) → smooth KDE-like density
function boxBlur(g: Float32Array, G: number, passes: number, r: number) {
  const tmp = new Float32Array(G * G)
  for (let pass = 0; pass < passes; pass++) {
    for (let y = 0; y < G; y++) for (let x = 0; x < G; x++) {     // horizontal
      let s = 0, c = 0
      for (let dx = -r; dx <= r; dx++) { const nx = x + dx; if (nx >= 0 && nx < G) { s += g[y * G + nx]; c++ } }
      tmp[y * G + x] = s / c
    }
    for (let y = 0; y < G; y++) for (let x = 0; x < G; x++) {     // vertical
      let s = 0, c = 0
      for (let dy = -r; dy <= r; dy++) { const ny = y + dy; if (ny >= 0 && ny < G) { s += tmp[ny * G + x]; c++ } }
      g[y * G + x] = s / c
    }
  }
}

// per-point local density (log-scaled, 0..1): bin → blur → bilinear-sample per point
function density(): Float32Array {
  const p = props.points!
  const n = p.length / 2
  const G = 160
  const { xMin, xMax, yMin, yMax } = props.extents
  const xs = xMax > xMin ? xMax - xMin : 1, ys = yMax > yMin ? yMax - yMin : 1
  const grid = new Float32Array(G * G)
  const fx = new Float32Array(n), fy = new Float32Array(n)        // fractional grid coords
  const clampf = (v: number) => v < 0 ? 0 : v > G - 1 ? G - 1 : v
  for (let i = 0; i < n; i++) {
    const px = p[2 * i], py = p[2 * i + 1]
    if (!Number.isFinite(px) || !Number.isFinite(py)) { fx[i] = 0; fy[i] = 0; continue }  // NaN/Inf → skip binning
    const gx = clampf(((px - xMin) / xs) * (G - 1))
    const gy = clampf(((py - yMin) / ys) * (G - 1))
    fx[i] = gx; fy[i] = gy
    grid[Math.floor(gy) * G + Math.floor(gx)] += 1
  }
  boxBlur(grid, G, 2, 2)
  let max = 1e-9
  for (let i = 0; i < grid.length; i++) { grid[i] = Math.log1p(grid[i]); if (grid[i] > max) max = grid[i] }
  const z = new Float32Array(n)
  for (let i = 0; i < n; i++) {
    const x0 = Math.floor(fx[i]), y0 = Math.floor(fy[i])
    const x1 = Math.min(x0 + 1, G - 1), y1 = Math.min(y0 + 1, G - 1)
    const tx = fx[i] - x0, ty = fy[i] - y0
    const a = grid[y0 * G + x0], b = grid[y0 * G + x1], c = grid[y1 * G + x0], d = grid[y1 * G + x1]
    z[i] = ((a * (1 - tx) + b * tx) * (1 - ty) + (c * (1 - tx) + d * tx) * ty) / max
  }
  return z
}

// regl keeps the data square by default (dataAspectRatio=1), which letterboxes our rectangular
// plot. Setting aspectRatio = width/height makes the net x-scale 1, so the normalised [-1,1]
// fills the canvas uniformly — and with a FIXED camera (no pan/zoom) it maps straight to the
// canvas edges, exactly matching the canvas2D overlays' plain extents mapping. (Providing
// x/y scales here made regl re-fit the camera to the domain and zoom in → dots drifted off the
// gates, so we don't use scales; pan/zoom is disabled for guaranteed alignment.)
// regl pins the canvas to the px size we pass (inline width/height), so it no longer follows the
// container CSS — we must measure & observe the PARENT box (panel-plot) for resizes to take.
function box(): { w: number; h: number } {
  const p = canvasEl.value?.parentElement
  const r = (p ?? canvasEl.value!).getBoundingClientRect()
  return { w: Math.max(1, r.width), h: Math.max(1, r.height) }
}
function aspect(): number { const { w, h } = box(); return Math.max(1e-6, w / h) }

async function ensure() {
  if (scatterplot || !canvasEl.value) return
  const createScatterplot = await getCreate()
  // NB: regl-scatterplot already creates its WebGL context with `preserveDrawingBuffer: true`, so the
  // canvas is directly readable for image export (plotHostToImageURL) — no context pre-creation needed.
  const { w, h } = box()
  scatterplot = createScatterplot({
    canvas: canvasEl.value,
    width: w,
    height: h,
    aspectRatio: aspect(),         // fill the rectangle (no square letterboxing) → overlays align
    pointSize: props.pointSize,
    opacity: props.opacity,
    backgroundColor: props.backgroundColor,
    lassoInitiator: false,         // gating uses explicit gates, never lasso
    cameraIsFixed: true,           // lock the camera at identity → [-1,1] maps to canvas edges
  })
  // kill selection: deselect immediately and neutralise active/hover so clicks never go blue
  scatterplot.set({ pointColorActive: props.flatColor, pointColorHover: props.flatColor })
  scatterplot.subscribe('select', () => scatterplot.deselect())
}

async function render() {
  await ensure()
  if (!scatterplot) return
  // sync the regl size to the current box on every draw: on first appearance the floating panel may
  // not have laid out when the initial render fires, so the scatter would draw at a stale/zero size
  // and stay blank until some later event (changing a dropdown) forced a redraw. Cheap no-op when
  // the size is unchanged.
  resize()
  await drawCurrent(1)
}

// draw the current encoding (density / category / flat). `pointMult` scales the point size — the export
// path enlarges the backing store by `scale`, so points must grow by the same factor to keep the look.
async function drawCurrent(pointMult = 1) {
  if (!scatterplot) return
  const { x, y } = xy()
  const ps = props.pointSize * pointMult
  if (props.colorMode === 'category' && x.length && (props.palette?.length ?? 0) > 0) {
    // colour-by-category (e.g. cluster on a UMAP): map each point's palette index → [0,1] so regl
    // picks pointColor[index]. The caller supplies one palette slot per category (incl. unclustered).
    const pal = props.palette!
    const cats = props.categories ?? new Float32Array(x.length)
    const denom = Math.max(1, pal.length - 1)
    const v = new Float32Array(x.length)
    for (let i = 0; i < x.length; i++) v[i] = (cats[i] ?? 0) / denom
    scatterplot.set({ colorBy: 'valueA', pointColor: pal, opacity: props.opacity, pointSize: ps })
    await scatterplot.draw({ x, y, valueA: v })
  } else if (props.colorMode === 'density' && x.length) {
    scatterplot.set({ colorBy: 'valueA', pointColor: RAMP, opacity: props.opacity, pointSize: ps })
    await scatterplot.draw({ x, y, valueA: density() })
  } else {
    scatterplot.set({ colorBy: null, pointColor: props.flatColor, opacity: props.opacity, pointSize: ps })
    await scatterplot.draw({ x, y })
  }
}

function resize() {
  if (!scatterplot || !canvasEl.value) return
  const { w, h } = box()
  scatterplot.set({ width: w, height: h, aspectRatio: aspect() })
}

// hi-res export: enlarge the BACKING STORE by `scale`× and redraw, so the point cloud is captured at
// high resolution (the on-screen backing is only CSS×DPR → compositing the live canvas upscales →
// pixelated). We DON'T use regl's own `export({scale})`: it drops our custom `aspectRatio` on its
// internal resize, so the fixed-camera gate scatter renders off-clip and returns a blank frame (hence
// the old screen-res-snapshot fallback that looked soft). Instead we resize the canvas ourselves,
// KEEPING aspectRatio, hold the on-screen DISPLAY size (so there's no visible jump), redraw on a
// transparent ground (points-only → composites over the host fill), snapshot, then restore.
async function exportCanvas(scale: number): Promise<HTMLCanvasElement | null> {
  if (!scatterplot || !canvasEl.value) return null
  const live = canvasEl.value
  const { w, h } = box()
  // guaranteed fallback: the current on-screen frame (screen resolution)
  const fallback = document.createElement('canvas'); fallback.width = live.width; fallback.height = live.height
  fallback.getContext('2d')?.drawImage(live, 0, 0)
  const cssW = live.style.width, cssH = live.style.height
  try {
    scatterplot.set({ backgroundColor: [0, 0, 0, 0], width: Math.round(w * scale), height: Math.round(h * scale), aspectRatio: aspect() })
    live.style.width = cssW || `${w}px`; live.style.height = cssH || `${h}px`   // keep the on-screen size
    await drawCurrent(scale)   // redraw the same encoding, point size scaled to match the big backing
    const snap = document.createElement('canvas'); snap.width = live.width; snap.height = live.height
    snap.getContext('2d')?.drawImage(live, 0, 0)
    return snap.width > fallback.width ? snap : fallback
  } catch {
    return fallback
  } finally {
    // restore the on-screen render (size + opaque ground + point size)
    scatterplot.set({ backgroundColor: props.backgroundColor, width: w, height: h, aspectRatio: aspect() })
    live.style.width = cssW; live.style.height = cssH
    render()
  }
}
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value })

watch(() => props.points, render)
// new data → re-render (camera is fixed; extents drive the [-1,1] normalisation)
watch(() => props.extents, render, { deep: true })
watch([() => props.colorMode, () => props.opacity, () => props.pointSize, () => props.flatColor], render)
watch([() => props.categories, () => props.palette], render)
watch(() => props.backgroundColor, bg => { scatterplot?.set({ backgroundColor: bg }); render() })

onMounted(() => {
  // defer the first render one frame: a freshly-opened floating panel hasn't laid out yet, so box()
  // would measure 0×0 and regl would draw nothing until some later reflow (the "toggle a pop and it
  // appears" symptom). rAF lets layout settle so the initial draw is at the real size.
  requestAnimationFrame(() => render())
  // observe the PARENT (panel-plot) — the canvas itself is pinned by regl and won't fire resizes
  const target = canvasEl.value?.parentElement ?? canvasEl.value
  if (target) {
    ro = new ResizeObserver(() => { resize(); render() })
    ro.observe(target)
  }
})
onBeforeUnmount(() => {
  ro?.disconnect(); ro = null
  scatterplot?.destroy?.(); scatterplot = null
})
</script>

<template>
  <canvas ref="canvasEl" class="scatter-gl" />
</template>

<style scoped>
.scatter-gl { width: 100%; height: 100%; display: block; }
</style>
