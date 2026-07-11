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
import { densityGrid, densityImageData, outlierPoints, DENSITY_GRID, CONTOUR_LEVELS, type Ext } from '../../plots/density'
import { densityContours } from '../../plots/contour'

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

// FlowJo pseudocolour density raster: build the G×G RGBA image, then upscale (smoothed) to the plot rect
function drawRaster(points: Float32Array, alpha = 1) {
  const c = ctx!
  const img = densityImageData(points, props.viewExtents, G)
  const off = document.createElement('canvas'); off.width = G; off.height = G
  const octx = off.getContext('2d')!
  const idata = octx.createImageData(G, G); idata.data.set(img.data)   // avoids ImageData-ctor buffer typing
  octx.putImageData(idata, 0, 0)
  const { w, h } = size()
  c.save()
  c.globalAlpha = alpha; c.imageSmoothingEnabled = true; c.imageSmoothingQuality = 'high'
  c.drawImage(off, 0, 0, G, G, 0, 0, w, h)
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
// "contour + outliers": the sparse-tail points the contours don't enclose, drawn as subtle dots
function drawOutliers(points: Float32Array, colour: string) {
  ctx!.globalAlpha = 0.3
  drawDots(outlierPoints(points, props.viewExtents, G), colour, 0.6)
  ctx!.globalAlpha = 1
}

function paintContent() {
  ctx!.lineJoin = 'round'
  const mode = props.renderMode
  // BASE population
  if (props.basePoints?.length) {
    if (mode === 'points') drawRaster(props.basePoints, props.showPops ? 0.4 : 1)   // dim under pop overlays
    else {
      drawContours(props.basePoints, '#cbd5e1')
      if (mode === 'outliers') drawOutliers(props.basePoints, '#cbd5e1')
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
