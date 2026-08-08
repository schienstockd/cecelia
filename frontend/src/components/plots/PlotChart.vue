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
import { xRotationOverride, sameOverrides, type AutoOverride } from '../../plots/autoOverride'
import type { PlotDataResponse } from '../../plots/types'

const props = defineProps<{ data: PlotDataResponse | null; opts: BuildOpts }>()
// settings the RENDERER had to substitute (today: rotating x tick labels that wouldn't fit). Reported
// up so the host can say so — a plot silently disagreeing with its own controls is the thing we avoid.
// See plots/autoOverride.ts.
const emit = defineEmits<{ 'auto-override': [AutoOverride[]] }>()
const host = useTemplateRef<HTMLElement>('host')
// @observablehq/plot is loosely typed for our purposes; keep it as any (its types are large).
let Plot: any = null                                   // eslint-disable-line @typescript-eslint/no-explicit-any
let node: HTMLElement | SVGElement | null = null
let ro: ResizeObserver | null = null

let legendNode: HTMLElement | null = null
let titleNode: HTMLElement | null = null
// Measured height of the rendered legend overlay. The legend is absolute HTML, so how many rows it
// wraps to depends on the label texts and the panel width — neither of which the option builder can
// see, and guessing "3 entries per row" is what made the reserved band look arbitrary (3 long labels
// wrap to 2 rows, one row was reserved, the second sat on the frame). So: render, measure, and if the
// reservation was wrong, render ONCE more with the real number. Remembered across renders so a resize
// starts from the last known height instead of flashing the estimate again.
let legendH = 0
// last set announced to the host — the emit is change-gated against it (see below). `null` until the
// first render, so a REMOUNT always re-announces (the host keeps its own copy and would otherwise
// stay on the previous chart's note).
let lastOverrides: AutoOverride[] | null = null

async function render(pass = 0) {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  // size from the panel body; fall back to sensible defaults before layout settles. Measured BEFORE the
  // build because the builder needs the width to decide whether the x tick labels fit their bands.
  const w = Math.max(160, host.value.clientWidth || 320)
  const h = Math.max(140, host.value.clientHeight || 260)
  const buildOpts = { ...props.opts, plotWidth: w, ...(legendH > 0 ? { legendHeight: legendH } : {}) }
  const base = props.data ? buildPlotOptions(Plot, props.data, buildOpts) as any : null
  node?.remove(); node = null
  legendNode?.remove(); legendNode = null
  titleNode?.remove(); titleNode = null
  if (!base) return
  // The colour scale carries no `legend` (plot.ts), so Plot returns a BARE <svg> sized exactly to the
  // panel — the bottom x-axis can't be clipped. We draw the legend ourselves as an absolute overlay
  // (consumes no layout height), so it never pushes the axis out of view.
  node = Plot.plot({ ...base, width: w, height: h }) as SVGElement
  // Observable Plot's tooltip (`tip: true`) fills its background rect from the CSS var
  // `--plot-background` (default white), NOT from style.background — so in dark theme the tip was
  // white-on-white (light ink over a white rect). Point the var at the theme ground so the tip rect
  // matches the plot ink.
  ;(node as SVGElement).style.setProperty('--plot-background', props.opts?.darkTheme ? '#1f2226' : 'white')
  host.value.append(node)
  // report any setting the builder substituted (`_autoRotatedX`) — but only when it actually CHANGED.
  // The host stores this and the board stores the host's readout, so an unconditional emit makes every
  // render a state write, which renders again. See sameOverrides in plots/autoOverride.ts.
  const overrides = [xRotationOverride(!!base._autoRotatedX, !!props.opts.rotateXLabel)]
    .filter(Boolean) as AutoOverride[]
  if (!lastOverrides || !sameOverrides(overrides, lastOverrides)) {
    lastOverrides = overrides; emit('auto-override', overrides)
  }

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

  // …now that the legend is in the document, measure it. One corrective re-render at most (`pass`),
  // so a legend whose height depends on the reserved margin can't oscillate.
  const measured = legendNode ? Math.ceil(legendNode.getBoundingClientRect().height) : 0
  if (pass === 0 && measured > 0 && Math.abs(measured - legendH) > 1) {
    legendH = measured
    await render(1)
  } else if (measured === 0 && legendH !== 0) {
    legendH = 0                                  // legend gone (single series / toggled off)
  }
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

watch(() => [props.data, props.opts], () => render(), { deep: true })
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
.plot-host { position: relative; width: 100%; height: 100%; background: white; border-radius: var(--cc-radius-xs); overflow: hidden; color: #111; }
.plot-host :deep(svg) { display: block; }
/* legend drawn as an absolute overlay (top-right) so it never eats height / clips the x-axis.
   colour is set inline by PlotChart (theme ink); descendants inherit it (force inherit so Plot's
   own swatch styles don't override the dark-theme ink). */
/* legend + title overlay POSITIONING is global (style.css `.plot-legend-overlay` / `.plot-title-overlay`)
   — two hosts draw them and the copies had drifted. All this host owes them is `position: relative`
   (above) and the ink colour, which PlotChart sets inline. */
</style>
