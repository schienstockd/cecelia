<!--
  The 1D gate plot's data layer: one channel's histogram in place of PlotLayers' dots, drawn on the
  same plot body (GateScatterCell `histogram`) under the same GateOverlay. The host bins the values
  (plots/histGeometry.ts `histCounts`) so the count axis it labels and the bars drawn here agree. The base
  population is filled; each highlighted population is its own outline in its colour, on the same
  counts scale — backgating on a histogram. Exposes the layer contract GateScatterCell exports through
  (`exportCanvas` / `getCanvas` / `exportSvgContent`). docs/POPULATION.md → 1D gates.
-->
<script setup lang="ts">
import { watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { observeBoxChanges } from '../../composables/usePlotResize'
import { dataToPx } from '../../plots/axisMap'
import { svgPath } from '../../plots/export'
import { PAPER_INK } from '../../plots/fixedColours'
import { parseHex } from '../../utils/colour'

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
export type HistData = { lo: number; hi: number; base: number[]; layers: { colour: string; counts: number[] }[] }
const props = defineProps<{
  viewExtents: Ext
  hist: HistData | null
  showPops: boolean
  viewTick: number
}>()

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
let ctx: CanvasRenderingContext2D | null = null
let ro: ResizeObserver | null = null

const size = () => ({ w: canvasEl.value?.clientWidth ?? 0, h: canvasEl.value?.clientHeight ?? 0 })
function ink(): string {
  const v = canvasEl.value && getComputedStyle(canvasEl.value).getPropertyValue('--cc-text-dim').trim()
  return v || PAPER_INK
}
// COLOUR-OK: a tint computed from a token's hex
const tint = (hex: string, a: number) => { const c = parseHex(hex); return c ? `rgba(${c[0]},${c[1]},${c[2]},${a})` : hex }

// the step outline of one count series, in px: up and across each bin
function stepPx(counts: number[], w: number, h: number): [number, number][] {
  const hd = props.hist!, e = props.viewExtents, bw = (hd.hi - hd.lo) / counts.length
  const at = (x: number, y: number) => dataToPx(e, { w, h }, x, y)
  const pts: [number, number][] = [at(hd.lo, 0)]
  counts.forEach((c, i) => { pts.push(at(hd.lo + i * bw, c), at(hd.lo + (i + 1) * bw, c)) })
  pts.push(at(hd.hi, 0))
  return pts
}
const series = () => {
  const hd = props.hist
  if (!hd) return []
  return [{ counts: hd.base, colour: ink(), fill: true },
          ...(props.showPops ? hd.layers.map(l => ({ counts: l.counts, colour: l.colour, fill: false })) : [])]
}

function paint(c: CanvasRenderingContext2D, w: number, h: number) {
  c.clearRect(0, 0, w, h)
  for (const s of series()) {
    const pts = stepPx(s.counts, w, h)
    c.beginPath(); pts.forEach(([x, y], i) => i ? c.lineTo(x, y) : c.moveTo(x, y))
    if (s.fill) { c.fillStyle = tint(s.colour, 0.35); c.fill() }
    c.strokeStyle = s.colour; c.lineWidth = s.fill ? 1 : 1.5; c.stroke()
  }
}
function draw() {
  const cv = canvasEl.value; if (!ctx || !cv) return
  const dpr = window.devicePixelRatio || 1, { w, h } = size()
  cv.width = Math.max(1, w * dpr); cv.height = Math.max(1, h * dpr)
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  paint(ctx, w, h)
}

async function exportCanvas(scale: number): Promise<HTMLCanvasElement | null> {
  const { w, h } = size(); if (!w || !h) return null
  const off = document.createElement('canvas')
  off.width = Math.round(w * scale); off.height = Math.round(h * scale)
  const c = off.getContext('2d'); if (!c) return null
  c.setTransform(scale, 0, 0, scale, 0, 0)
  paint(c, w, h)
  return off
}
function exportSvgContent(): string {
  const { w, h } = size(); if (!w || !h) return ''
  const r1 = (n: number) => Math.round(n * 10) / 10
  return series().map(s => {
    const d = stepPx(s.counts, w, h).map(([x, y], i) => `${i ? 'L' : 'M'}${r1(x)} ${r1(y)}`).join('')
    return svgPath(d, s.fill ? { stroke: s.colour, fill: s.colour, opacity: 0.35, width: 1 }
                             : { stroke: s.colour, width: 1.5 })
  }).join('')
}
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value, exportSvgContent })

watch(() => [props.hist, props.viewExtents, props.showPops, props.viewTick], draw, { deep: true })
onMounted(() => { ctx = canvasEl.value!.getContext('2d'); draw(); ro = observeBoxChanges(canvasEl.value!, draw) })
onBeforeUnmount(() => { ro?.disconnect(); ro = null })
</script>

<template>
  <canvas ref="canvasEl" class="hist-layer" />
</template>

<style scoped>
.hist-layer { position: absolute; inset: 0; width: 100%; height: 100%; }
</style>
