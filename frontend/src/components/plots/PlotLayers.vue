<!--
  The gating base renderer + overlays, all on ONE 2D canvas (no WebGL — see docs/PLOTS.md). Replaces the
  old regl point cloud for gating: the base population is a FlowJo/OMIQ-style DENSITY RASTER (points
  mode) or clean d3-contour rings (contour/outliers mode). Sits between the (now-removed) WebGL layer
  and GateOverlay (gate drawing). Everything maps data→px via the LIVE viewExtents, so it rescales on
  zoom / axis-extent change; export re-renders the same 2D content at scale (crisp, cannot clip).

    • points   → density raster of the base population (dimmed when showing pops).
    • contour  → nested contour rings of the base density.
    • outliers → contour rings + the sparse-tail dots the rings don't enclose.
    • showPops → each visible child population drawn in its colour (dots in points mode, rings otherwise).
  Subsets come from the server (plotdata?pop=…), so Julia still owns membership; we only colour.
-->
<script setup lang="ts">
import { watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { densityGrid, pointDensities, outlierPoints, DENSITY_GRID, CONTOUR_LEVELS, type Ext } from '../../plots/density'
import { densityContours } from '../../plots/contour'
import { BLUE_HEAT_RGB } from '../../plots/flowColors'
import { svgImage, svgCircles, svgPath } from '../../plots/export'

export interface PopLayer { path: string; colour: string; points: Float32Array }

const props = defineProps<{
  viewExtents: Ext                          // live (zoom-synced) data extents
  renderMode: 'points' | 'contour' | 'outliers'
  basePoints: Float32Array | null           // base population points
  popLayers: PopLayer[]                      // visible child pops to colour
  showPops: boolean
  viewTick: number                           // bump → redraw (camera moved)
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
function toPx(vx: number, vy: number): [number, number] {
  const { w, h } = size(); const { xMin, xMax, yMin, yMax } = props.viewExtents
  const xs = xMax > xMin ? xMax - xMin : 1, ys = yMax > yMin ? yMax - yMin : 1
  return [((vx - xMin) / xs) * w, (1 - (vy - yMin) / ys) * h]
}
// d3-contour ring coord (grid space [0,G], col=x row=y) → px
function gridToPx(gx: number, gy: number): [number, number] {
  const { xMin, xMax, yMin, yMax } = props.viewExtents
  return toPx(xMin + (gx / G) * (xMax - xMin), yMin + (gy / G) * (yMax - yMin))
}

// FlowJo/OMIQ pseudocolour DOT plot: each point drawn at its position, coloured by its LOCAL density
// via the blue-heat ramp — point resolution, no blocky cells. Bucketed by colour so we set fillStyle
// ~B times, not once per point (fast for 100k+ points).
const DOT_BUCKETS = 64
const DOT_R = 0.7
function drawDensityDots(points: Float32Array, alpha = 1) {
  const c = ctx!
  const t = pointDensities(points, props.viewExtents)
  const n = points.length / 2
  const groups: number[][] = Array.from({ length: DOT_BUCKETS }, () => [])
  for (let i = 0; i < n; i++) groups[Math.min(DOT_BUCKETS - 1, Math.floor(t[i] * DOT_BUCKETS))].push(i)
  const s = DOT_R * 2
  c.save(); c.globalAlpha = alpha
  for (let b = 0; b < DOT_BUCKETS; b++) {
    const g = groups[b]; if (!g.length) continue
    const ci = Math.min(255, Math.round((b / (DOT_BUCKETS - 1)) * 255))
    c.fillStyle = `rgb(${BLUE_HEAT_RGB[ci * 3]},${BLUE_HEAT_RGB[ci * 3 + 1]},${BLUE_HEAT_RGB[ci * 3 + 2]})`
    for (const i of g) { const [px, py] = toPx(points[2 * i], points[2 * i + 1]); c.fillRect(px - DOT_R, py - DOT_R, s, s) }
  }
  c.restore()
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
      let [px, py] = gridToPx(ring[0][0], ring[0][1]); c.moveTo(px, py)
      for (let k = 1; k < ring.length; k++) { [px, py] = gridToPx(ring[k][0], ring[k][1]); c.lineTo(px, py) }
      c.closePath()
    }
    c.stroke()
  })
  c.globalAlpha = 1
}

function drawDots(points: Float32Array, colour: string, r = 1.5) {
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
  drawDots(outlierPoints(points, props.viewExtents, G), colour, 1.3)
  ctx!.globalAlpha = 1
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
  // child POPULATION overlays
  if (props.showPops) {
    for (const pop of props.popLayers) {
      if (!pop.points?.length) continue
      if (mode === 'points') drawDots(pop.points, pop.colour)
      else { drawContours(pop.points, pop.colour); if (mode === 'outliers') drawOutliers(pop.points, pop.colour) }
    }
  }
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
// pop → recolourable in Illustrator). Reuses the same toPx/gridToPx maps as the on-screen paint.
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
  drawDensityDots(props.basePoints, props.showPops ? 0.4 : 1)   // base only (same as paintContent points branch)
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
      let [px, py] = gridToPx(ring[0][0], ring[0][1]); d += `M${svgR1(px)} ${svgR1(py)}`
      for (let k = 1; k < ring.length; k++) { [px, py] = gridToPx(ring[k][0], ring[k][1]); d += `L${svgR1(px)} ${svgR1(py)}` }
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
      if (mode === 'outliers') body += dotsSvg(outlierPoints(props.basePoints, props.viewExtents, G), ink(), 1.3, 0.8)
    }
  }
  if (props.showPops) {
    for (const pop of props.popLayers) {
      if (!pop.points?.length) continue
      if (mode === 'points') body += dotsSvg(pop.points, pop.colour, 1.5)     // categorical → vector
      else { body += contoursSvg(pop.points, pop.colour); if (mode === 'outliers') body += dotsSvg(outlierPoints(pop.points, props.viewExtents, G), pop.colour, 1.3, 0.8) }
    }
  }
  return body
}
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value, exportSvgContent })

watch(() => [props.viewExtents, props.renderMode, props.basePoints, props.popLayers, props.showPops, props.viewTick],
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
