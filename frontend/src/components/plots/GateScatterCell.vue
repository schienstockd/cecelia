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
import ScatterGL from './ScatterGL.vue'
import PlotSpinner from './PlotSpinner.vue'
import { useDelayedLoading } from '../../composables/useDelayedLoading'
import PlotLayers, { type PopLayer } from './PlotLayers.vue'
import GateOverlay from './GateOverlay.vue'
import { rasterPlotToImageURL } from '../../plots/export'

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
  renderMode?: 'points' | 'contour'
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
}>(), {
  gates: () => [], popLayers: () => [], renderMode: 'points', showPops: false,
  mode: 'off', gateLineWidth: 1.5, gateLabels: false, viewTick: 0, loading: false, compact: false, readonly: false,
  hideAxisLabels: false,
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
type LayerExport = { exportCanvas(scale: number): Promise<HTMLCanvasElement | null>; getCanvas(): HTMLCanvasElement | null }
const scatterRef = useTemplateRef<LayerExport>('scatterRef')
const layersRef = useTemplateRef<LayerExport>('layersRef')
const overlayRef = useTemplateRef<LayerExport>('overlayRef')
const hiRes = async (cv: HTMLCanvasElement, scale: number) => {
  for (const r of [scatterRef, layersRef, overlayRef]) {
    if (cv === r.value?.getCanvas()) return (await r.value?.exportCanvas(scale)) ?? null
  }
  return null
}
// `light` = flip the ink/border vars (via .cc-light) so ticks/axis names read dark on a white ground —
// used by the PDF export (dark theme is only for on-screen display). The crisp fixed-resolution raster
// export (aim for a ~2200px long side so a small slot doesn't export soft) is the SHARED helper
// rasterPlotToImageURL — the same path the cluster UMAP uses.
async function exportImage(bg = '#0d0b1a', light = false): Promise<string | null> {
  const el = hostEl.value
  if (light) el?.classList.add('cc-light')
  try { return await rasterPlotToImageURL(el, bg, hiRes) }
  finally { if (light) el?.classList.remove('cc-light') }
}
// `hiRes` is exposed so a host that captures a LARGER element containing several of these cells (the
// gating-strategy MONTAGE grid) can still re-render each cell's canvases at export scale — otherwise the
// montage would composite every subplot at screen resolution.
defineExpose({ exportImage, hiRes })

// base render look: density for plain points; dim/flat backdrop for contour or pop-colour modes
function baseColorMode(renderMode: string, showPops: boolean): 'density' | 'flat' {
  return renderMode === 'points' && !showPops ? 'density' : 'flat'
}
</script>

<template>
  <div ref="hostEl" class="plot-capture" :class="{ compact, 'no-axis': hideAxisLabels }">
    <div class="panel-plot">
      <ScatterGL ref="scatterRef" :points="points" :extents="extents"
                 :color-mode="baseColorMode(renderMode, showPops)"
                 :opacity="renderMode === 'contour' ? 0.22 : showPops ? 0.35 : 1"
                 :point-size="renderMode === 'contour' ? 2 : 3" />
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
.plot-capture.compact .axis-x { bottom: -15px; font-size: 10px; }
.plot-capture.compact .axis-y { left: -24px; font-size: 10px; }
.plot-capture.compact .xtick-lbl, .plot-capture.compact .ytick-lbl { display: none; }
/* compact tiles can be smaller than the full-size 150px plot floor — lift it so the plot shrinks to the
   (square) tile instead of overflowing and being clipped by the cell (the pairs matrix packs small tiles). */
.plot-capture.compact .panel-plot { min-height: 0; }
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
.xtick-lbl { margin-top: 3px; font-size: 10px; color: var(--cc-text-dim); white-space: nowrap; }
.ytick { position: absolute; left: 0; transform: translate(-100%, -50%); display: flex; align-items: center; pointer-events: none; }
.ytick-mark { width: 5px; height: 1px; background: var(--cc-text-dim); }
.ytick-lbl { margin-right: 3px; font-size: 10px; color: var(--cc-text-dim); white-space: nowrap; }
.axis-x { position: absolute; bottom: -40px; left: 50%; transform: translateX(-50%); font-size: 13px; font-weight: 600; color: var(--cc-text); }
/* vertical text via writing-mode (rotate's origin offsets by half the text width → overlap) */
.axis-y { position: absolute; left: -66px; top: 50%; transform: translateY(-50%) rotate(180deg);
  writing-mode: vertical-rl; font-size: 13px; font-weight: 600; color: var(--cc-text); }
.panel-loading { position: absolute; top: 4px; right: 6px; font-size: 11px; color: var(--cc-text-dim); }
</style>
