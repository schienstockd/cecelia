<!--
  canvas2D render layer between ScatterGL (base points) and GateOverlay (gate drawing).
  Two FlowJo-style jobs, both rescaling on zoom (it maps data→px via the LIVE viewExtents):

   1. Contour mode  → marching-squares contours of the base population's density, at
      normalised z = 1 − [0.95, 0.90, 0.75, 0.5]  (R: .flowContourLines, flowHelpers.R:46).
   2. Population colour overlay (showPops) → for each visible child population on these axes,
      draw its cells in the population colour: as dots (points mode) or as a contour (contour
      mode). Works for BOTH base modes. Subsets come from the server (plotdata?pop=…), so Julia
      still owns membership; we only colour.
-->
<script setup lang="ts">
import { watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { densityGrid as computeDensityGrid, outlierPoints, DENSITY_GRID, CONTOUR_LEVELS, type Ext } from '../../plots/density'

export interface PopLayer { path: string; colour: string; points: Float32Array }

const props = defineProps<{
  viewExtents: Ext                          // live (zoom-synced) data extents
  renderMode: 'points' | 'contour' | 'outliers'   // contour = contours only; outliers = contours + tail dots
  basePoints: Float32Array | null           // base population points (for base contour)
  popLayers: PopLayer[]                      // visible child pops to colour
  showPops: boolean
  viewTick: number                           // bump → redraw (camera moved)
}>()
// contour-family modes (contours drawn, no full WebGL point cloud); `outliers` adds the sparse-tail dots
const isContour = () => props.renderMode === 'contour' || props.renderMode === 'outliers'

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
let ctx: CanvasRenderingContext2D | null = null
let ro: ResizeObserver | null = null

const G = DENSITY_GRID                       // contour grid resolution (shared with density.ts)
const LEVELS = CONTOUR_LEVELS                // 1 − confidence levels (outer→inner)

function size() { const c = canvasEl.value!; return { w: c.clientWidth, h: c.clientHeight } }
function toPx(vx: number, vy: number): [number, number] {
  const { w, h } = size(); const { xMin, xMax, yMin, yMax } = props.viewExtents
  const xs = xMax > xMin ? xMax - xMin : 1, ys = yMax > yMin ? yMax - yMin : 1
  return [((vx - xMin) / xs) * w, (1 - (vy - yMin) / ys) * h]
}

// density grid (normalised 0..1) over the current view — shared pure helper (plots/density.ts)
const densityGrid = (points: Float32Array) => computeDensityGrid(points, props.viewExtents, G)

// grid cell coords → data coords. Samples sit at bin CENTRES ((i+0.5)/G), matching the
// binning above (floor((v-min)/range*G)); using /(G-1) here mis-scaled contours vs the points.
function cellToData(gx: number, gy: number): [number, number] {
  const { xMin, xMax, yMin, yMax } = props.viewExtents
  return [xMin + ((gx + 0.5) / G) * (xMax - xMin), yMin + ((gy + 0.5) / G) * (yMax - yMin)]
}

// marching squares: stroke contour segments for one level
function strokeContour(z: Float32Array, level: number) {
  const c = ctx!
  const interp = (a: number, b: number) => (a === b ? 0.5 : (level - a) / (b - a))
  c.beginPath()
  for (let y = 0; y < G - 1; y++) for (let x = 0; x < G - 1; x++) {
    const tl = z[y * G + x], tr = z[y * G + x + 1], br = z[(y + 1) * G + x + 1], bl = z[(y + 1) * G + x]
    let idx = 0
    if (tl > level) idx |= 8; if (tr > level) idx |= 4; if (br > level) idx |= 2; if (bl > level) idx |= 1
    if (idx === 0 || idx === 15) continue
    // edge crossing points in cell-space → data → px
    const top: [number, number]    = [x + interp(tl, tr), y]
    const right: [number, number]  = [x + 1, y + interp(tr, br)]
    const bottom: [number, number] = [x + interp(bl, br), y + 1]
    const left: [number, number]   = [x, y + interp(tl, bl)]
    const seg = (a: [number, number], b: [number, number]) => {
      const [ax, ay] = toPx(...cellToData(a[0], a[1])), [bx, by] = toPx(...cellToData(b[0], b[1]))
      c.moveTo(ax, ay); c.lineTo(bx, by)
    }
    switch (idx) {
      case 1: case 14: seg(left, bottom); break
      case 2: case 13: seg(bottom, right); break
      case 3: case 12: seg(left, right); break
      case 4: case 11: seg(top, right); break
      case 5: seg(left, top); seg(bottom, right); break
      case 6: case 9: seg(top, bottom); break
      case 7: case 8: seg(left, top); break
      case 10: seg(left, bottom); seg(top, right); break
    }
  }
  c.stroke()
}

function drawContours(points: Float32Array, colour: string) {
  const z = densityGrid(points)
  for (let i = 0; i < LEVELS.length; i++) {
    ctx!.strokeStyle = colour
    ctx!.globalAlpha = 0.45 + 0.55 * (i / (LEVELS.length - 1))   // outer faint → inner solid
    ctx!.lineWidth = 1.2
    strokeContour(z, LEVELS[i])
  }
  ctx!.globalAlpha = 1
}

function drawDots(points: Float32Array, colour: string, r = 1.5) {
  const c = ctx!; c.fillStyle = colour
  const n = points.length / 2
  const s = r * 2
  for (let i = 0; i < n; i++) {
    const [px, py] = toPx(points[2 * i], points[2 * i + 1])
    c.fillRect(px - r, py - r, s, s)
  }
}
// "contour + outliers": individual dots for the sparse-tail points the contours don't enclose (R:
// contour ± outliers). Small + semi-transparent so the contours stay the main read.
function drawOutliers(points: Float32Array, colour: string) {
  const pts = outlierPoints(points, props.viewExtents, G)
  ctx!.globalAlpha = 0.55
  drawDots(pts, colour, 1)
  ctx!.globalAlpha = 1
}

// paint the layer content (contours + pop overlay) with the current `ctx`; toPx uses size() (CSS px)
// so it's resolution-independent — the transform on the target ctx sets the actual pixel density.
function paintContent() {
  ctx!.lineJoin = 'round'
  if (isContour() && props.basePoints?.length) {
    drawContours(props.basePoints, '#cbd5e1')
    if (props.renderMode === 'outliers') drawOutliers(props.basePoints, '#cbd5e1')
  }
  if (props.showPops) {
    for (const pop of props.popLayers) {
      if (!pop.points?.length) continue
      if (isContour()) {
        drawContours(pop.points, pop.colour)
        if (props.renderMode === 'outliers') drawOutliers(pop.points, pop.colour)
      } else {
        drawDots(pop.points, pop.colour)
      }
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

// hi-res export (see plots/export.ts): re-paint the same content onto a scale× offscreen canvas so
// the contours/dots are crisp instead of the compositor upscaling the screen-DPR canvas. We swap the
// module `ctx` to the offscreen context so the existing draw helpers target it, then restore.
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
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value })

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
