<!--
  The gating base renderer + overlays, all on ONE 2D canvas (no WebGL — see docs/PLOTS.md). Replaces the
  old regl point cloud for gating: the base population is a FlowJo/OMIQ-style DENSITY RASTER (points
  mode) or clean d3-contour rings (contour/outliers mode). Sits between the (now-removed) WebGL layer
  and GateOverlay (gate drawing). Everything maps data→px via the LIVE viewExtents, so it rescales on
  zoom / axis-extent change; export re-renders the same 2D content at scale (crisp, cannot clip).

    • points   → density raster of the base population (dimmed when showing pops).
    • contour  → nested contour rings of the base density.
    • outliers → contour rings + the sparse-tail dots the rings don't enclose.
    • showPops → each visible child population drawn in its colour, ALWAYS as dots (any base mode).
    • baseValues → COLOUR BY a third measure (FlowJo's colour-by-parameter): in points mode the dots
                 take the ramp from that measure instead of from local density, plus a colour bar.
  Subsets come from the server (plotdata?pop=…), so Julia still owns membership; we only colour.
-->
<script setup lang="ts">
import { watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { densityGrid, pointDensities, outlierPoints, DENSITY_GRID, CONTOUR_LEVELS, DOT_R, type Ext } from '../../plots/density'
import { dataToPx, gridToPx, type PxBox } from '../../plots/axisMap'
import { densityContours } from '../../plots/contour'
import { BLUE_HEAT_RGB, heatCss } from '../../plots/flowColors'
import { normValues, barTicks, fitLabel, barStops, colourBarSvg } from '../../plots/valueColour'
import { svgImage, svgCircles, svgPath } from '../../plots/export'
import { paintDimmed } from '../../plots/dimLayer'

export interface PopLayer { path: string; colour: string; points: Float32Array }

const props = defineProps<{
  viewExtents: Ext                          // live (zoom-synced) data extents
  renderMode: 'points' | 'contour' | 'outliers'
  // y is an IMAGE row coordinate (grows downward) — see plots/axisMap.ts
  flipY?: boolean
  basePoints: Float32Array | null           // base population points
  popLayers: PopLayer[]                      // visible child pops to colour
  showPops: boolean
  viewTick: number                           // bump → redraw (camera moved)
  // COLOUR BY a third measure: one already-transformed value per base point (same index = same dot,
  // guaranteed by the server reading x/y/z in one pass) + the ramp's range/labels from plotmeta.
  // Absent → the base keeps its local-density pseudocolour.
  baseValues?: Float32Array | null
  valueExtent?: [number, number] | null      // [lo, hi] in TRANSFORMED value space
  valueTicks?: { pos: number; label: string }[]   // raw-value labels at transformed positions
  valueLabel?: string                        // the measure's name, captioning the colour bar
  // false → colour the dots but draw NO bar: a montage host draws ONE bar for the whole grid, because
  // every tile shares the ramp and a per-tile bar would eat a 200px tile (GateMontage).
  valueLegend?: boolean
  // dot radius in px (default `DOT_R`). It scales EVERY dot on the layer — the base speckle, the
  // population overlays and the outlier tail — by the same factor, so their relative sizes (an overlay
  // reads as bigger than the base it sits on) hold at any setting.
  dotSize?: number
}>()

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
let ctx: CanvasRenderingContext2D | null = null
let ro: ResizeObserver | null = null

const G = DENSITY_GRID
const LEVELS = CONTOUR_LEVELS

function size() { const c = canvasEl.value!; return { w: c.clientWidth, h: c.clientHeight } }
// base contour/outlier ink resolved from the themed CSS var so it flips DARK-on-white for the light PDF
// export (.cc-light on an ancestor) and light-on-dark on screen — a hardcoded grey was invisible on the
// white export. Falls back to a mid slate that reads on either ground.
function ink(): string {
  const el = canvasEl.value
  const v = el && getComputedStyle(el).getPropertyValue('--cc-text-dim').trim()
  return v || '#64748b'
}
// ONE mapping for every layer on this canvas — see plots/axisMap.ts. `flipY` is set when the y axis
// is an image ROW coordinate, which grows downward; the points, the density raster, the contour rings
// and the gate outlines drawn over them all take it from the same place, so they cannot disagree.
const box = (): PxBox => ({ ...size(), flipY: props.flipY })
const toPx = (vx: number, vy: number) => dataToPx(props.viewExtents, box(), vx, vy)
// d3-contour ring coord (grid space [0,G], col=x row=y) → px
const ringToPx = (gx: number, gy: number) => gridToPx(props.viewExtents, box(), G, gx, gy)

// FlowJo/OMIQ pseudocolour DOT plot: each point drawn at its position, coloured by its LOCAL density
// via the blue-heat ramp — point resolution, no blocky cells. Bucketed by colour so we set fillStyle
// ~B times, not once per point (fast for 100k+ points).
const DOT_BUCKETS = 64
// every radius on this layer scales with the knob, relative to the default look
const dotK = () => Math.max(0.1, props.dotSize ?? DOT_R) / DOT_R
const baseR = () => DOT_R * dotK()
const POP_R = 1.5, OUTLIER_R = 1.3
// The 0..1 the ramp is indexed by, per point: the COLOUR-BY measure when one is given (normalised over
// the served whole-dataset range, so the colours don't re-map as you walk the population tree), else
// the point's own local density. One source of `t` → one paint loop for both modes, on screen and in
// both export paths; the colour bar reads the same normalisation. NaN = the measure is missing there.
function dotRamp(points: Float32Array): Float32Array {
  const v = props.baseValues, ext = props.valueExtent
  return v && ext && v.length === points.length / 2
    ? normValues(v, ext)
    : pointDensities(points, props.viewExtents)
}
const colourBy = () => {
  const v = props.baseValues, ext = props.valueExtent, pts = props.basePoints
  return !!(v && ext && pts && v.length === pts.length / 2 && props.renderMode === 'points')
}
const showBar = () => colourBy() && props.valueLegend !== false
function paintDensityDots(points: Float32Array) {
  const c = ctx!
  const t = dotRamp(points)
  const r = baseR(), n = points.length / 2
  const groups: number[][] = Array.from({ length: DOT_BUCKETS }, () => [])
  const missing: number[] = []                       // no value for this cell — NOT the ramp's floor
  for (let i = 0; i < n; i++) {
    isFinite(t[i]) ? groups[Math.min(DOT_BUCKETS - 1, Math.floor(t[i] * DOT_BUCKETS))].push(i)
                   : missing.push(i)
  }
  const s = r * 2
  const stamp = (g: number[]) => {
    for (const i of g) { const [px, py] = toPx(points[2 * i], points[2 * i + 1]); c.fillRect(px - r, py - r, s, s) }
  }
  if (missing.length) { c.fillStyle = ink(); stamp(missing) }
  for (let b = 0; b < DOT_BUCKETS; b++) {
    const g = groups[b]; if (!g.length) continue
    const ci = Math.min(255, Math.round((b / (DOT_BUCKETS - 1)) * 255))
    c.fillStyle = `rgb(${BLUE_HEAT_RGB[ci * 3]},${BLUE_HEAT_RGB[ci * 3 + 1]},${BLUE_HEAT_RGB[ci * 3 + 2]})`
    stamp(g)
  }
}
// The "dim under pop overlays" backdrop goes through plots/dimLayer: a dot plot CANNOT be dimmed by
// setting globalAlpha and then stamping ~10k dots, because that dims each DOT and overlaps composite
// back up to 1-(1-alpha)^k. The dense core returned to full opacity, so the wash disappeared exactly
// where the cells are — a napari selection lit up cyan over a base that was supposed to grey out. (The
// raster renderer this replaced got it right for free: it was a single drawImage.)
function drawDensityDots(points: Float32Array, alpha = 1) {
  if (alpha < 1) {
    const { w, h } = size()
    const host = ctx!
    // toPx/size() read props, not the ctx transform, so the offscreen geometry matches the direct paint
    const done = paintDimmed(host, w, h, alpha, (octx) => { ctx = octx; paintDensityDots(points); ctx = host })
    if (done) return                                   // no offscreen → fall through: undimmed beats blank
  }
  paintDensityDots(points)
}

// clean nested contour rings (d3-contour on the blurred grid). Outer levels faint → inner solid.
function drawContours(points: Float32Array, colour: string) {
  const c = ctx!
  const grid = densityGrid(points, props.viewExtents, G)
  const levels = densityContours(grid, G, LEVELS)
  c.strokeStyle = colour; c.lineWidth = 1.2; c.lineJoin = 'round'; c.lineCap = 'round'
  levels.forEach((lvl, i) => {
    c.globalAlpha = 0.5 + 0.5 * (i / Math.max(1, LEVELS.length - 1))
    c.beginPath()
    for (const ring of lvl.rings) {
      if (ring.length < 2) continue
      let [px, py] = ringToPx(ring[0][0], ring[0][1]); c.moveTo(px, py)
      for (let k = 1; k < ring.length; k++) { [px, py] = ringToPx(ring[k][0], ring[k][1]); c.lineTo(px, py) }
      c.closePath()
    }
    c.stroke()
  })
  c.globalAlpha = 1
}

function drawDots(points: Float32Array, colour: string, r = POP_R * dotK()) {
  const c = ctx!; c.fillStyle = colour
  const n = points.length / 2, s = r * 2
  for (let i = 0; i < n; i++) {
    const [px, py] = toPx(points[2 * i], points[2 * i + 1])
    c.fillRect(px - r, py - r, s, s)
  }
}
// "contour + outliers": the sparse-tail points the contours don't enclose, drawn as clear dots (they
// were barely visible before — bumped alpha + size so the tail reads like the old WebGL render did)
function drawOutliers(points: Float32Array, colour: string) {
  ctx!.globalAlpha = 0.8
  drawDots(outlierPoints(points, props.viewExtents, G), colour, OUTLIER_R * dotK())
  ctx!.globalAlpha = 1
}

// ── COLOUR BAR (the colour-by legend) ───────────────────────────────────────────────────────────────
// Sits INSIDE the plot area, top-right: the gating plot's chrome is a fixed asymmetric padding sized
// for the x/y axis names (GateScatterCell), so there is no third gutter to put a legend in, and
// widening one would shrink the dots in every montage tile too. It draws LAST, so the dimmed base
// can't wash it out, and it is part of the canvas — so the PNG export gets it for free.
const BAR_W = 9, BAR_PAD = 8, BAR_FS = 9
function barBox() {
  const { w, h } = size()
  return { x: w - BAR_PAD - BAR_W, y: BAR_PAD + BAR_FS + 4, w: BAR_W,
           h: Math.max(36, Math.min(110, Math.round(h * 0.32))) }
}
function paintColourBar() {
  const c = ctx!, ext = props.valueExtent
  if (!ext) return
  const b = barBox(), { w } = size()
  // the SAME bands the SVG bar emits (plots/valueColour `barStops`), as gradient stops
  const stops = barStops('v')
  const grad = c.createLinearGradient(0, b.y, 0, b.y + b.h)
  stops.forEach((t, i) => grad.addColorStop(i / (stops.length - 1), heatCss(t)))
  c.fillStyle = grad; c.fillRect(b.x, b.y, b.w, b.h)
  c.strokeStyle = ink(); c.lineWidth = 0.6; c.strokeRect(b.x, b.y, b.w, b.h)
  c.fillStyle = ink(); c.font = `${BAR_FS}px system-ui, sans-serif`
  c.textAlign = 'right'; c.textBaseline = 'middle'
  for (const t of barTicks(props.valueTicks ?? [], ext)) {
    c.fillText(t.label, b.x - 3, b.y + (1 - t.frac) * b.h)
  }
  if (props.valueLabel) {
    c.textBaseline = 'alphabetic'
    c.fillText(fitLabel(props.valueLabel, Math.max(30, w * 0.45), t => c.measureText(t).width),
               b.x + b.w, b.y - 4)
  }
}
// the same bar as TRUE VECTOR for the SVG export — the shared builder, so the figure's legend cannot
// describe the ramp differently from the canvas one above
const barSvg = (): string => {
  const ext = props.valueExtent
  return ext ? colourBarSvg(barBox(), { extent: ext, ticks: props.valueTicks ?? [],
                                        label: props.valueLabel, ink: ink(), fontSize: BAR_FS }) : ''
}

function paintContent() {
  ctx!.lineJoin = 'round'
  const mode = props.renderMode
  // BASE population
  if (props.basePoints?.length) {
    if (mode === 'points') drawDensityDots(props.basePoints, props.showPops ? 0.4 : 1)   // dim under pop overlays
    else {
      const base = ink()
      drawContours(props.basePoints, base)
      if (mode === 'outliers') drawOutliers(props.basePoints, base)
    }
  }
  // child POPULATION overlays — ALWAYS DOTS, whatever the base mode is. The render mode answers "what
  // shape is this cloud", which is a question about a distribution; an overlay answers "where are THESE
  // cells", which is per-cell. Contouring a small overlay produces rings around individual points — a
  // 3-cell napari selection came out as three sets of concentric circles — and a KDE of a handful of
  // events is not a density estimate. Dots also keep the categorical layer true-vector on SVG export in
  // every mode. The base keeps its rings (and its outlier tail), so a contour figure stays a contour figure.
  if (props.showPops) for (const pop of props.popLayers) {
    if (pop.points?.length) drawDots(pop.points, pop.colour)
  }
  if (showBar()) paintColourBar()          // last: the legend is never dimmed by the base wash
}

function draw() {
  if (!ctx || !canvasEl.value) return
  const dpr = window.devicePixelRatio || 1
  const { w, h } = size()
  canvasEl.value.width = Math.max(1, w * dpr); canvasEl.value.height = Math.max(1, h * dpr)
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.clearRect(0, 0, w, h)
  paintContent()
}

// hi-res export: re-paint the SAME 2D content onto a scale× offscreen canvas (crisp; a 2D canvas can't
// clip like the old WebGL re-render). Swap the module ctx so the draw helpers target the offscreen.
async function exportCanvas(scale: number): Promise<HTMLCanvasElement | null> {
  if (!canvasEl.value) return null
  const { w, h } = size(); if (!w || !h) return null
  const off = document.createElement('canvas')
  off.width = Math.max(1, Math.round(w * scale)); off.height = Math.max(1, Math.round(h * scale))
  const octx = off.getContext('2d'); if (!octx) return null
  const saved = ctx
  ctx = octx
  octx.setTransform(scale, 0, 0, scale, 0, 0)
  octx.clearRect(0, 0, w, h)
  paintContent()
  ctx = saved                                  // the live canvas's context object was never touched
  return off
}
// ── TRUE-VECTOR SVG export (docs/PLOTS.md) ──────────────────────────────────────────────────────────
// Emit this layer's content as an SVG body in LOCAL plot-area coords [0..w, 0..h] (the host translates
// it into the capture). The DENSITY BASE is a blue-heat heatmap, not categorical — in points mode it's
// embedded as a raster <image> (decision: don't vectorise 100k–1M events); contour/outlier bases are
// already vector paths. CATEGORICAL child-population overlays are always TRUE VECTOR (one <g fill=…> per
// pop → recolourable in Illustrator). Reuses the same toPx/ringToPx maps as the on-screen paint.
const svgR1 = (n: number) => Math.round(n * 10) / 10
// render ONLY the density base to an offscreen canvas → PNG data URL (points-mode raster base layer)
function renderBaseRasterUrl(scale = 4): string | null {
  if (!canvasEl.value || props.renderMode !== 'points' || !props.basePoints?.length) return null
  const { w, h } = size(); if (!w || !h) return null
  const off = document.createElement('canvas')
  off.width = Math.max(1, Math.round(w * scale)); off.height = Math.max(1, Math.round(h * scale))
  const octx = off.getContext('2d'); if (!octx) return null
  const saved = ctx; ctx = octx
  octx.setTransform(scale, 0, 0, scale, 0, 0); octx.clearRect(0, 0, w, h)
  drawDensityDots(props.basePoints, props.showPops ? 0.4 : 1)   // base only — no colour bar (vector, above)
  ctx = saved
  return off.toDataURL('image/png')
}
// contour rings of `points` as vector <path>s (outer faint → inner solid), same levels/opacity as canvas
function contoursSvg(points: Float32Array, colour: string): string {
  const grid = densityGrid(points, props.viewExtents, G)
  const levels = densityContours(grid, G, LEVELS)
  let out = ''
  levels.forEach((lvl, i) => {
    const op = 0.5 + 0.5 * (i / Math.max(1, LEVELS.length - 1))
    let d = ''
    for (const ring of lvl.rings) {
      if (ring.length < 2) continue
      let [px, py] = ringToPx(ring[0][0], ring[0][1]); d += `M${svgR1(px)} ${svgR1(py)}`
      for (let k = 1; k < ring.length; k++) { [px, py] = ringToPx(ring[k][0], ring[k][1]); d += `L${svgR1(px)} ${svgR1(py)}` }
      d += 'Z'
    }
    out += svgPath(d, { stroke: colour, width: 1.2, opacity: op })
  })
  return out
}
// a set of points as a true-vector circle group (child-pop dots + the outlier tail)
function dotsSvg(points: Float32Array, colour: string, r: number, opacity = 1): string {
  const n = points.length / 2, pxs: [number, number][] = new Array(n)
  for (let i = 0; i < n; i++) pxs[i] = toPx(points[2 * i], points[2 * i + 1])
  return svgCircles(pxs, { fill: colour, r, opacity })
}
function exportSvgContent(): string {
  const { w, h } = size(); if (!w || !h) return ''
  const mode = props.renderMode
  let body = ''
  if (props.basePoints?.length) {
    if (mode === 'points') { const url = renderBaseRasterUrl(); if (url) body += svgImage(url, 0, 0, w, h) }
    else {
      body += contoursSvg(props.basePoints, ink())
      if (mode === 'outliers') body += dotsSvg(outlierPoints(props.basePoints, props.viewExtents, G), ink(), OUTLIER_R * dotK(), 0.8)
    }
  }
  if (props.showPops) {
    for (const pop of props.popLayers) {
      if (!pop.points?.length) continue
      body += dotsSvg(pop.points, pop.colour, POP_R * dotK())   // categorical → vector, dots in EVERY mode (paintContent)
    }
  }
  if (showBar()) body += barSvg()                       // same order as paintContent
  return body
}
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value, exportSvgContent })

watch(() => [props.viewExtents, props.renderMode, props.basePoints, props.popLayers, props.showPops, props.viewTick,
             props.baseValues, props.valueExtent, props.valueTicks, props.valueLabel, props.valueLegend,
             props.dotSize],
      draw, { deep: true })
onMounted(() => {
  ctx = canvasEl.value!.getContext('2d')
  draw()
  ro = new ResizeObserver(draw); ro.observe(canvasEl.value!)
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null })
</script>

<template>
  <canvas ref="canvasEl" class="plot-layers" />
</template>

<style scoped>
.plot-layers { position: absolute; inset: 0; width: 100%; height: 100%; pointer-events: none; }
</style>
