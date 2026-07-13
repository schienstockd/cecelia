<!--
  The shared scatter+gate PLOT BODY: a WebGL scatter (ScatterGL) + contour/population layer
  (PlotLayers) + canvas2D gate layer (GateOverlay), with axis lines/ticks/labels and PNG export. This
  is the ONE gate-scatter renderer — extracted from GatePlotPanel so the read-only gating-strategy plot
  (Analysis board) reuses it instead of forking a second one (feedback_use_existing_framework).

  It renders whatever it's given: the host owns data fetching, axis selection, and gate state. Interactive
  use (Gate page) passes `mode` = rectangle/polygon and handles @draw/@edit; read-only use (Analysis
  canvas) passes `mode='off'` so nothing can be drawn or edited. Host-specific overlays (e.g. the gate
  naming input) go in the default slot (absolutely positioned inside the plot body).

  Exposes `exportImage(bg)` → composites the stacked canvases + HTML/canvas2D overlays to a PNG data URL
  (each layer re-renders hi-res), so both hosts export identically.
-->
<script setup lang="ts">
import { useTemplateRef, toRef } from 'vue'
import type { GateSpec } from '../../stores/gating'
import PlotSpinner from './PlotSpinner.vue'
import { useDelayedLoading } from '../../composables/useDelayedLoading'
import PlotLayers, { type PopLayer } from './PlotLayers.vue'
import GateOverlay from './GateOverlay.vue'

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }

const props = withDefaults(defineProps<{
  points: Float32Array | null
  extents: Ext                                   // fixed full-data range (ScatterGL)
  viewExtents: Ext                               // live (zoom-synced) extents (PlotLayers/GateOverlay/ticks)
  xTicks: { pos: number; label: string }[]
  yTicks: { pos: number; label: string }[]
  gates?: { path: string; colour: string; gate: GateSpec; label?: string }[]   // gate outlines (label overrides name)
  xLabel: string
  yLabel: string
  popLayers?: PopLayer[]                         // coloured child-pop overlays
  renderMode?: 'points' | 'contour' | 'outliers'   // contour = contours only; outliers = + tail dots
  showPops?: boolean
  mode?: 'off' | 'rectangle' | 'polygon'         // 'off' = read-only (no draw/edit)
  gateLineWidth?: number
  gateLabels?: boolean
  viewTick?: number
  loading?: boolean
  compact?: boolean                              // tight chrome for small montage panels (gating strategy)
  readonly?: boolean                             // static gates — no move/resize (read-only Analysis board)
  hideAxisLabels?: boolean                       // drop the x/y axis-name labels (pairs matrix — the
                                                 // diagonal names each channel, so per-tile labels only
                                                 // clutter/clip); also reclaims their padding
  fontSize?: number                              // base axis font size (px) — the vis slider; scales the
                                                 // tick labels + axis names (--gate-font). Default 11.
}>(), {
  gates: () => [], popLayers: () => [], renderMode: 'points', showPops: false,
  mode: 'off', gateLineWidth: 1.5, gateLabels: false, viewTick: 0, loading: false, compact: false, readonly: false,
  hideAxisLabels: false, fontSize: 11,
})
const emit = defineEmits<{ draw: [Partial<GateSpec>]; edit: [{ path: string; gate: GateSpec }]; cancel: [] }>()

// delayed spinner for the full-size scatter only; compact montage cells (gating strategy) are small
// plots — they keep the unobtrusive dot rather than a wheel over every tile.
const showSpinner = useDelayedLoading(toRef(props, 'loading'))

// ticks positioned against the LIVE view extents so labels track pan/zoom
const tickX = (pos: number, e: Ext) => `${((pos - e.xMin) / Math.max(1e-9, e.xMax - e.xMin)) * 100}%`
const tickY = (pos: number, e: Ext) => `${(1 - (pos - e.yMin) / Math.max(1e-9, e.yMax - e.yMin)) * 100}%`
// abbreviate big numbers (262144 → 262.1k) so axis labels don't crowd; leave small ones as-is
function fmtTick(label: string): string {
  const n = parseFloat(label)
  if (!isFinite(n)) return label
  const a = Math.abs(n), trim = (s: string) => s.replace(/\.0$/, '')
  if (a >= 1e9) return trim((n / 1e9).toFixed(1)) + 'G'
  if (a >= 1e6) return trim((n / 1e6).toFixed(1)) + 'M'
  if (a >= 1e3) return trim((n / 1e3).toFixed(1)) + 'k'
  return label
}

// export: each stacked canvas (WebGL scatter + canvas2D contours/gates) re-renders itself at export
// scale so nothing is upscaled from the screen-DPR backing store; route each live canvas to its layer.
const hostEl = useTemplateRef<HTMLElement>('hostEl')
const panelPlotEl = useTemplateRef<HTMLElement>('panelPlotEl')
type LayerExport = { exportCanvas(scale: number): Promise<HTMLCanvasElement | null>; getCanvas(): HTMLCanvasElement | null }
const layersRef = useTemplateRef<LayerExport>('layersRef')
const overlayRef = useTemplateRef<LayerExport>('overlayRef')
const hiRes = async (cv: HTMLCanvasElement, scale: number) => {
  for (const r of [layersRef, overlayRef]) {
    if (cv === r.value?.getCanvas()) return (await r.value?.exportCanvas(scale)) ?? null
  }
  return null
}

// Draw the axis (lines, tick marks + labels, axis names) onto the export ctx, using the SAME
// data→plot-area mapping as the dot/gate canvases. This is the whole point of the unified export: the
// axis is NOT a separately-composited HTML overlay any more (that desynced from the canvas in the PDF —
// "dots and axis on a different scale"), it's painted on the same canvas from the same viewExtents, so
// it can't drift. `pr` = plot-area rect (the panel-plot) within the capture, in CSS px.
function drawAxes(c: CanvasRenderingContext2D, pr: { x: number; y: number; w: number; h: number }) {
  const e = props.viewExtents
  const xs = e.xMax > e.xMin ? e.xMax - e.xMin : 1, ys = e.yMax > e.yMin ? e.yMax - e.yMin : 1
  const px = (v: number) => pr.x + ((v - e.xMin) / xs) * pr.w
  const py = (v: number) => pr.y + (1 - (v - e.yMin) / ys) * pr.h
  const cssVar = (n: string, fb: string) => {
    const v = hostEl.value && getComputedStyle(hostEl.value).getPropertyValue(n).trim()
    return v || fb
  }
  const ink = cssVar('--cc-text', '#111'), dim = cssVar('--cc-text-dim', '#555'), border = cssVar('--cc-border', '#c9ccd1')
  const tickFont = Math.max(7, props.fontSize - 1), nameFont = props.fontSize + 2, MARK = 5, GAP = 3
  const bottom = pr.y + pr.h, left = pr.x
  c.save()
  // axis lines (L: left + bottom)
  c.strokeStyle = border; c.lineWidth = 1
  c.beginPath(); c.moveTo(left, pr.y); c.lineTo(left, bottom); c.moveTo(left, bottom); c.lineTo(pr.x + pr.w, bottom); c.stroke()
  // x ticks
  c.fillStyle = dim; c.strokeStyle = dim; c.font = `${tickFont}px system-ui, sans-serif`
  c.textAlign = 'center'; c.textBaseline = 'top'
  for (const t of props.xTicks) {
    const x = px(t.pos)
    c.beginPath(); c.moveTo(x, bottom); c.lineTo(x, bottom + MARK); c.stroke()
    if (!props.compact) c.fillText(fmtTick(t.label), x, bottom + MARK + GAP)
  }
  // y ticks
  c.textAlign = 'right'; c.textBaseline = 'middle'
  for (const t of props.yTicks) {
    const y = py(t.pos)
    c.beginPath(); c.moveTo(left, y); c.lineTo(left - MARK, y); c.stroke()
    if (!props.compact) c.fillText(fmtTick(t.label), left - MARK - GAP, y)
  }
  // axis names (skip when hidden — pairs matrix names each channel on the diagonal)
  if (!props.hideAxisLabels) {
    c.fillStyle = ink; c.font = `600 ${nameFont}px system-ui, sans-serif`
    c.textAlign = 'center'; c.textBaseline = 'alphabetic'
    c.fillText(props.xLabel, pr.x + pr.w / 2, bottom + MARK + GAP + tickFont + nameFont + 6)
    c.save(); c.translate(Math.max(nameFont, left - 44), pr.y + pr.h / 2); c.rotate(-Math.PI / 2)
    c.textBaseline = 'top'; c.fillText(props.yLabel, 0, 0); c.restore()
  }
  c.restore()
}

// `light` = flip the ink/border vars (via .cc-light) so ticks/axis names read dark on a white ground —
// used by the PDF export (dark theme is only for on-screen display). UNIFIED export: one canvas holding
// the dots + gate + axis (all from the same viewExtents), instead of compositing a canvas + a cloned
// HTML axis overlay (which desynced in the PDF). Aim for a ~2200px long side so a small slot isn't soft.
async function exportImage(bg = '#0d0b1a', light = false): Promise<string | null> {
  const host = hostEl.value, pp = panelPlotEl.value
  if (!host || !pp) return null
  if (light) host.classList.add('cc-light')
  try {
    const capW = host.clientWidth, capH = host.clientHeight
    if (!capW || !capH) return null
    const scale = Math.min(14, Math.max(4, Math.ceil(2200 / Math.max(capW, capH))))
    const out = document.createElement('canvas')
    out.width = Math.round(capW * scale); out.height = Math.round(capH * scale)
    const ctx = out.getContext('2d'); if (!ctx) return null
    ctx.scale(scale, scale)
    if (bg && bg !== 'transparent') { ctx.fillStyle = bg; ctx.fillRect(0, 0, capW, capH) }
    // plot-area rect within the capture (panel-plot, offset by the capture's padding); offsetLeft/Top +
    // clientWidth/Height are the UNTRANSFORMED layout box, so this is immune to any ancestor zoom.
    const pr = { x: pp.offsetLeft, y: pp.offsetTop, w: pp.clientWidth, h: pp.clientHeight }
    for (const r of [layersRef, overlayRef]) {   // dots first, then gates — both fill the plot-area
      const layer = await r.value?.exportCanvas(scale)
      if (layer) ctx.drawImage(layer, pr.x, pr.y, pr.w, pr.h)
    }
    drawAxes(ctx, pr)
    return out.toDataURL('image/png')
  } finally { if (light) host.classList.remove('cc-light') }
}
// `hiRes` is exposed so a host that captures a LARGER element containing several of these cells (the
// gating-strategy MONTAGE grid) can still re-render each cell's canvases at export scale. `getHost`
// lets the montage place each tile's UNIFIED export image at the tile's grid rect.
defineExpose({ exportImage, hiRes, getHost: () => hostEl.value })

</script>

<template>
  <div ref="hostEl" class="plot-capture" :class="{ compact, 'no-axis': hideAxisLabels }"
       :style="{ '--gate-font': `${fontSize}px` }">
    <div ref="panelPlotEl" class="panel-plot">
      <!-- base cloud (density raster / contours), child-pop overlays, and outliers — all 2D, no WebGL -->
      <PlotLayers ref="layersRef" :view-extents="viewExtents" :render-mode="renderMode" :base-points="points"
                  :pop-layers="popLayers" :show-pops="showPops" :view-tick="viewTick" />
      <GateOverlay ref="overlayRef" :extents="viewExtents" :mode="mode" :gates="gates" :view-tick="viewTick"
                   :line-width="gateLineWidth" :show-labels="gateLabels" :readonly="readonly"
                   @draw="emit('draw', $event)" @edit="emit('edit', $event)" @cancel="emit('cancel')" />
      <span class="axisline axisline-x" />
      <span class="axisline axisline-y" />
      <span v-for="t in xTicks" :key="'x'+t.pos" class="xtick" :style="{ left: tickX(t.pos, viewExtents) }">
        <span class="xtick-mark" /><span class="xtick-lbl">{{ fmtTick(t.label) }}</span>
      </span>
      <span v-for="t in yTicks" :key="'y'+t.pos" class="ytick" :style="{ top: tickY(t.pos, viewExtents) }">
        <span class="ytick-lbl">{{ fmtTick(t.label) }}</span><span class="ytick-mark" />
      </span>
      <span v-if="!hideAxisLabels" class="axis-x">{{ xLabel }}</span>
      <span v-if="!hideAxisLabels" class="axis-y">{{ yLabel }}</span>
      <PlotSpinner v-if="showSpinner && !compact" label="Loading…" />
      <div v-else-if="loading && compact" class="panel-loading">…</div>
      <slot />
    </div>
  </div>
</template>

<style scoped>
/* capture host for PNG export: the axis labels/ticks live in this padding (relative to .panel-plot,
   at negative offsets), so exporting THIS element — not .panel-plot — includes the x/y axis names
   instead of clipping them at the edge (#00061). */
.plot-capture { position: relative; flex: 1; min-height: 218px; min-width: 0; display: flex; box-sizing: border-box;
  padding: 18px 22px 50px 84px; }
/* light theme for PDF export (dark theme is on-screen only): dark ink/border on the white ground */
.plot-capture.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; }
/* compact: tight chrome for small montage squares (gating-strategy) — smaller padding, no min-height,
   axis names pulled in, tick labels hidden to avoid crowding */
.plot-capture.compact { min-height: 0; padding: 10px 8px 20px 30px; }
.plot-capture.compact .axis-x { bottom: -15px; font-size: calc(var(--gate-font, 11px) - 1px); }
.plot-capture.compact .axis-y { left: -24px; font-size: calc(var(--gate-font, 11px) - 1px); }
.plot-capture.compact .xtick-lbl, .plot-capture.compact .ytick-lbl { display: none; }
/* compact tiles can be smaller than the full-size 150px plot floor — lift it so the plot shrinks to the
   (square) tile instead of overflowing and being clipped by the cell (the pairs matrix packs small tiles). */
.plot-capture.compact .panel-plot { min-height: 0; }
/* montage tiles (board gating-strategy + wrap): the SQUARE must be the PLOT AREA (.panel-plot, the dots),
   NOT this outer capture box. The axis padding is asymmetric (84+22 horizontal for the rotated y-name vs
   18+50 vertical for the x-name), so squaring the outer box left the dots ~38px taller than wide — the
   "flow plots elongate on export" bug. Square .panel-plot instead and let the capture height to fit; the
   dots then stay square on screen AND in every export path (single capW/capH + the multi-tile host rect
   both reflect the real geometry). align-items:flex-start so the aspect-ratio drives the height rather
   than flex stretch. The pairs MATRIX (.no-axis) keeps its own square-cell alignment — excluded here. */
.plot-capture.gm-plot:not(.no-axis), .plot-capture.compact:not(.no-axis) { flex: none; min-height: 0; align-items: flex-start; }
.plot-capture.gm-plot:not(.no-axis) .panel-plot,
.plot-capture.compact:not(.no-axis) .panel-plot { flex: none; width: 100%; min-height: 0; aspect-ratio: 1; }
/* GATE MODULE PAGE (the default full-size cell — no gm-plot/compact/no-axis): the DOTS must be a 1:1
   square too — SAME mechanism as the montage above (aspect-ratio on .panel-plot), so gating has ONE
   dots-square method across its contexts. (aspect-ratio, not SquarePlot's container-query, because the
   montage tiles have a content-driven height where cqh is indeterminate; using it here too keeps them
   uniform. It also keeps .panel-plot a direct offset-child of .plot-capture, which the zoom-immune PNG
   export relies on.) The square fills the capture width and centres; the panel itself is snapped 1:1 by
   CanvasPanel :square, so there's no blank space. */
.plot-capture:not(.gm-plot):not(.compact):not(.no-axis) { align-items: center; justify-content: center; }
.plot-capture:not(.gm-plot):not(.compact):not(.no-axis) .panel-plot {
  flex: none; width: 100%; min-height: 0; aspect-ratio: 1; }
/* no-axis (pairs matrix): no axis-name labels → drop the padding they lived in so the scatter fills
   the tile. Small uniform inset just for the axis lines / tick marks. */
.plot-capture.no-axis { padding: 6px 6px 10px 12px; }
/* min-width:0 so this flex-row item can shrink below the scatter canvas's intrinsic width. */
.panel-plot { position: relative; flex: 1; min-height: 150px; min-width: 0; }
.axisline { position: absolute; background: var(--cc-border); pointer-events: none; }
.axisline-x { left: 0; right: 0; bottom: 0; height: 1px; }
.axisline-y { left: 0; top: 0; bottom: 0; width: 1px; }
.xtick { position: absolute; bottom: 0; transform: translate(-50%, 100%); display: flex; flex-direction: column; align-items: center; pointer-events: none; }
.xtick-mark { width: 1px; height: 5px; background: var(--cc-text-dim); }
.xtick-lbl { margin-top: 3px; font-size: calc(var(--gate-font, 11px) - 1px); color: var(--cc-text-dim); white-space: nowrap; }
.ytick { position: absolute; left: 0; transform: translate(-100%, -50%); display: flex; align-items: center; pointer-events: none; }
.ytick-mark { width: 5px; height: 1px; background: var(--cc-text-dim); }
.ytick-lbl { margin-right: 3px; font-size: calc(var(--gate-font, 11px) - 1px); color: var(--cc-text-dim); white-space: nowrap; }
/* nowrap so a long channel name (e.g. "Bcells-ubiTom") stays on ONE line in the axis gutter instead of
   wrapping into the plot; it sits in the .plot-capture padding, which overflows visibly if needed */
.axis-x { position: absolute; bottom: -40px; left: 50%; transform: translateX(-50%); white-space: nowrap; font-size: calc(var(--gate-font, 11px) + 2px); font-weight: 600; color: var(--cc-text); }
/* vertical text via writing-mode (rotate's origin offsets by half the text width → overlap) */
.axis-y { position: absolute; left: -66px; top: 50%; transform: translateY(-50%) rotate(180deg);
  writing-mode: vertical-rl; white-space: nowrap; font-size: calc(var(--gate-font, 11px) + 2px); font-weight: 600; color: var(--cc-text); }
.panel-loading { position: absolute; top: 4px; right: 6px; font-size: 11px; color: var(--cc-text-dim); }
</style>
