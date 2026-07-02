<!--
  Observable Plot renderer for the analysis-plot canvas (summary panels: histogram, boxplot, violin,
  beeswarm/strip, bar, frequency/stacked). Lazy-imports @observablehq/plot (heavy, like vega-embed
  was) and hands it to plots/plot.ts's buildPlotOptions; the builders return a Plot options object,
  this component injects width/height from the panel box and calls Plot.plot().

  Resize is trivial here (the Vega pain point): Plot has no signal graph — we just re-render with the
  parent's current width/height whenever the ResizeObserver fires. The big point clouds stay on
  regl-scatterplot; this is server-aggregated summaries only, so re-rendering on resize is cheap.
-->
<script setup lang="ts">
import { computed, watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { buildPlotOptions, type BuildOpts } from '../../plots/plot'
import { svgToImageURL, svgOf } from '../../plots/export'
import { legendOverlay, titleOverlay } from '../../plots/overlays'
import type { PlotDataResponse } from '../../plots/types'

const props = defineProps<{ data: PlotDataResponse | null; opts: BuildOpts }>()
const host = useTemplateRef<HTMLElement>('host')
// @observablehq/plot is loosely typed for our purposes; keep it as any (its types are large).
let Plot: any = null                                   // eslint-disable-line @typescript-eslint/no-explicit-any
let node: HTMLElement | SVGElement | null = null
let ro: ResizeObserver | null = null

let legendNode: HTMLElement | null = null
let titleNode: HTMLElement | null = null

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  const base = props.data ? buildPlotOptions(Plot, props.data, props.opts) as any : null
  // size from the panel body; fall back to sensible defaults before layout settles
  const w = Math.max(160, host.value.clientWidth || 320)
  const h = Math.max(140, host.value.clientHeight || 260)
  node?.remove(); node = null
  legendNode?.remove(); legendNode = null
  titleNode?.remove(); titleNode = null
  if (!base) return
  // The colour scale carries no `legend` (plot.ts), so Plot returns a BARE <svg> sized exactly to the
  // panel — the bottom x-axis can't be clipped. We draw the legend ourselves as an absolute overlay
  // (consumes no layout height), so it never pushes the axis out of view.
  node = Plot.plot({ ...base, width: w, height: h }) as SVGElement
  host.value.append(node)

  const ink = props.opts.darkTheme ? '#e6e6e6' : '#111'
  if (props.opts.legend && base._colorLegend) {
    // continuous colour legend for matrix/heatmap (plot.ts stashes the colour scale in `_colorLegend`).
    legendNode = legendOverlay(Plot, base._colorLegend.color, ink)
    if (legendNode) host.value.append(legendNode)
  } else {
    // deduped legend (plot.ts `_legend`) — one entry per DISTINCT colour, not per series key
    const leg = base._legend ?? base.color
    const dom: string[] = leg?.domain ?? []
    if (props.opts.legend && dom.length > 1) {
      legendNode = legendOverlay(Plot, { domain: leg.domain, range: leg.range }, ink)
      if (legendNode) host.value.append(legendNode)
    }
  }
  // title as an overlay (top-left) with the theme ink — see plot.ts note on why not opts.title
  if (props.opts.title) { titleNode = titleOverlay(props.opts.title, ink); host.value.append(titleNode) }
}

// host background follows the dark-theme flag so there are no white gaps around a dark plot
const hostBg = computed(() => (props.opts?.darkTheme ? '#1f2226' : 'white'))

// expose image export to the host panel (shared helper — see plots/export.ts). SVG = native
// serialisation (crisp); PNG = rasterise onto a 2× canvas over white.
// `light` = build a one-off LIGHT-theme node (dark ink on white) for PDF export, without disturbing the
// on-screen (dark-theme) chart — dark theme is only for webpage display. Legend/title overlays are HTML
// (not in the SVG), so — as with the existing per-plot PNG export — they're omitted from the image.
async function toImageURL(type: 'png' | 'svg', light = false): Promise<string | null> {
  if (!light) return svgToImageURL(svgOf(node as Element | null), type)
  if (!host.value) return null
  if (!Plot) Plot = await import('@observablehq/plot')
  const base = props.data ? buildPlotOptions(Plot, props.data, { ...props.opts, darkTheme: false }) as any : null   // eslint-disable-line @typescript-eslint/no-explicit-any
  if (!base) return null
  const w = Math.max(160, host.value.clientWidth || 320)
  const h = Math.max(140, host.value.clientHeight || 260)
  const off = Plot.plot({ ...base, width: w, height: h }) as SVGElement
  return svgToImageURL(svgOf(off as unknown as Element), type)
}
defineExpose({ toImageURL })

watch(() => [props.data, props.opts], render, { deep: true })
onMounted(() => {
  render()
  if (host.value && typeof ResizeObserver !== 'undefined') {
    ro = new ResizeObserver(() => render())
    ro.observe(host.value)
  }
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null; node?.remove(); node = null; legendNode?.remove(); legendNode = null; titleNode?.remove(); titleNode = null })
</script>

<template><div ref="host" class="plot-host" :style="{ background: hostBg }" /></template>

<style scoped>
/* white plot ground (theme_classic) — fills the panel body. position:relative anchors the legend
   overlay. color:#111 so any HTML text (legend) is dark on the white ground (not the app's light grey). */
.plot-host { position: relative; width: 100%; height: 100%; background: white; border-radius: 3px; overflow: hidden; color: #111; }
.plot-host :deep(svg) { display: block; }
/* legend drawn as an absolute overlay (top-right) so it never eats height / clips the x-axis.
   colour is set inline by PlotChart (theme ink); descendants inherit it (force inherit so Plot's
   own swatch styles don't override the dark-theme ink). */
.plot-host :deep(.plot-legend-overlay) {
  position: absolute; top: 4px; right: 6px; display: flex; flex-wrap: wrap; gap: 2px 10px;
  max-width: 58%; justify-content: flex-end; border-radius: 3px; padding: 1px 4px;
}
.plot-host :deep(.plot-legend-overlay *) { color: inherit !important; }
/* title overlay (top-left), theme ink set inline */
.plot-host :deep(.plot-title-overlay) {
  position: absolute; top: 4px; left: 8px; max-width: 60%; font-weight: 600; font-size: 12px;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
</style>
