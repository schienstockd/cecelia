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
import { ref, computed, watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { buildPlotOptions, type BuildOpts, type CldOverlay } from '../../plots/plot'
import { svgToImageURL, svgOf } from '../../plots/export'
import { legendOverlay, titleOverlay } from '../../plots/overlays'
import TeleportPopover from '../TeleportPopover.vue'
import type { PlotDataResponse } from '../../plots/types'

const props = defineProps<{ data: PlotDataResponse | null; opts: BuildOpts }>()
const host = useTemplateRef<HTMLElement>('host')
// @observablehq/plot is loosely typed for our purposes; keep it as any (its types are large).
let Plot: any = null                                   // eslint-disable-line @typescript-eslint/no-explicit-any
let node: HTMLElement | SVGElement | null = null
let ro: ResizeObserver | null = null

let legendNode: HTMLElement | null = null
let titleNode: HTMLElement | null = null

// Compact-Letter-Display overlay (docs/todo/STATS_ANNOTATIONS_PLAN.md). One letter per group,
// positioned in-container over the plot SVG; each letter is a hover-anchor for a TeleportPopover
// listing the group's ns neighbours. Populated in render() from base._cld + node.scale(…).
interface CldPlaced { label: string; letter: string; left: number; top: number; ns: string[] }
const cldItems = ref<CldPlaced[]>([])
const cldInk = ref<string>('#111')                    // theme ink for the overlays (see render())
const cldLetterRefs = ref<HTMLElement[]>([])
const hoverIdx = ref<number>(-1)                      // which letter is hovered (drives popover)
const hoverAnchor = computed<HTMLElement | null>(() => hoverIdx.value >= 0 ? cldLetterRefs.value[hoverIdx.value] ?? null : null)
const hoverItem = computed<CldPlaced | null>(() => hoverIdx.value >= 0 ? cldItems.value[hoverIdx.value] ?? null : null)

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
  // Observable Plot's tooltip (`tip: true`) fills its background rect from the CSS var
  // `--plot-background` (default white), NOT from style.background — so in dark theme the tip was
  // white-on-white (light ink over a white rect). Point the var at the theme ground so the tip rect
  // matches the plot ink.
  ;(node as SVGElement).style.setProperty('--plot-background', props.opts?.darkTheme ? '#1f2226' : 'white')
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

  // CLD letters as HTML overlays — each letter is a hover anchor for TeleportPopover (below).
  // Resolve position-axis integer index → pixels via the plot's scale (Observable Plot exposes
  // scale.apply(v)); same for the measure axis at extent.max + headroom. Falls back to nothing when
  // useLetters is off (base._cld = null).
  const cld = (base as unknown as { _cld?: CldOverlay | null })._cld ?? null
  if (cld && cld.items.length && node) {
    const posScale = (node as any).scale(cld.rotate ? 'y' : 'x') as { apply?(v: number): number } | undefined   // eslint-disable-line @typescript-eslint/no-explicit-any
    const measScale = (node as any).scale(cld.rotate ? 'x' : 'y') as { apply?(v: number): number } | undefined  // eslint-disable-line @typescript-eslint/no-explicit-any
    const ext = Math.max(1e-9, cld.measExtent.max - cld.measExtent.min)
    const measVal = cld.measExtent.max + ext * 0.05                       // 5% headroom, matches STATS_HEADROOM
    const measPx = measScale?.apply?.(measVal) ?? 0
    cldItems.value = cld.items.map(it => {
      const posPx = posScale?.apply?.(it.pos) ?? 0
      const left = cld.rotate ? measPx : posPx
      const top  = cld.rotate ? posPx  : measPx
      return { label: it.label, letter: it.letter, left, top, ns: it.ns }
    })
    cldInk.value = ink
  } else {
    cldItems.value = []
  }
  hoverIdx.value = -1
}

// Inject the CLD letters as SVG text marks on the export path — the on-screen chart keeps HTML
// overlays (needed so each letter can anchor a TeleportPopover), but a rasterised export needs the
// letters IN the SVG or they vanish. Called from toImageURL before Plot.plot().
function _injectCldMarks(Plot: any, base: any, ink: string): void {   // eslint-disable-line @typescript-eslint/no-explicit-any
  const cld = base._cld as CldOverlay | null
  if (!cld || !cld.items.length) return
  const ext = Math.max(1e-9, cld.measExtent.max - cld.measExtent.min)
  const measVal = cld.measExtent.max + ext * 0.05
  const marks = base.marks as unknown[]
  for (const it of cld.items) {
    if (cld.rotate) {
      marks.push(Plot.text([{ x: measVal, y: it.pos, label: it.letter }],
                           { x: 'x', y: 'y', text: 'label', textAnchor: 'start', dx: 8,
                             fontSize: 12, fontWeight: 700, fill: ink }))
    } else {
      marks.push(Plot.text([{ x: it.pos, y: measVal, label: it.letter }],
                           { x: 'x', y: 'y', text: 'label', textAnchor: 'middle', dy: -6,
                             fontSize: 12, fontWeight: 700, fill: ink }))
    }
  }
}

// host background follows the dark-theme flag so there are no white gaps around a dark plot
const hostBg = computed(() => (props.opts?.darkTheme ? '#1f2226' : 'white'))

// expose image export to the host panel (shared helper — see plots/export.ts). SVG = native
// serialisation (crisp); PNG = rasterise onto a 2× canvas over white.
// `light` = build a one-off LIGHT-theme node (dark ink on white) for PDF export, without disturbing the
// on-screen (dark-theme) chart — dark theme is only for webpage display.
//
// CLD letters live as HTML overlays on-screen (so they can anchor TeleportPopover on hover), which
// means they'd vanish on an SVG-only export. Rebuild the plot for BOTH export paths and inject the
// letters as SVG text marks first, using the ink for the target theme. Legend/title overlays are
// separately HTML too and still omitted — parity with the earlier behaviour.
async function toImageURL(type: 'png' | 'svg', light = false): Promise<string | null> {
  if (!host.value || !props.data) return null
  if (!Plot) Plot = await import('@observablehq/plot')
  const targetOpts = light ? { ...props.opts, darkTheme: false } : props.opts
  const base = buildPlotOptions(Plot, props.data, targetOpts) as any   // eslint-disable-line @typescript-eslint/no-explicit-any
  if (!base) return null
  const ink = targetOpts.darkTheme ? '#e6e6e6' : '#111'
  _injectCldMarks(Plot, base, ink)
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

<template>
  <div ref="host" class="plot-host" :style="{ background: hostBg }">
    <!-- CLD letters (docs/todo/STATS_ANNOTATIONS_PLAN.md). Positioned in the plot's own coord space via
         node.scale(…).apply(…) — see render() above. Each letter opens a TeleportPopover on hover
         (canonical popover primitive; no SVG-native tooltip). -->
    <span
      v-for="(c, i) in cldItems"
      :key="c.label"
      :ref="el => { if (el) cldLetterRefs[i] = el as HTMLElement }"
      class="cld-letter"
      :style="{ left: c.left + 'px', top: c.top + 'px', color: cldInk }"
      @mouseenter="hoverIdx = i"
      @mouseleave="hoverIdx === i && (hoverIdx = -1)"
    >{{ c.letter }}</span>
    <TeleportPopover :model-value="hoverIdx >= 0 && !!hoverAnchor" :anchor="hoverAnchor" placement="bottom-start"
                     @update:model-value="v => { if (!v) hoverIdx = -1 }">
      <div class="cld-pop">
        <div class="cld-pop-head">
          <span class="cld-letter-mini">{{ hoverItem?.letter }}</span>
          <span class="cld-pop-title">{{ hoverItem?.label }}</span>
        </div>
        <div v-if="(hoverItem?.ns.length ?? 0) === 0" class="cld-pop-empty cc-muted cc-fs-xs">
          Significantly different from every other group.
        </div>
        <div v-else class="cld-pop-body">
          <div class="cc-muted cc-fs-xs">Not different from:</div>
          <ul class="cld-pop-list">
            <li v-for="n in hoverItem?.ns" :key="n">{{ n }}</li>
          </ul>
        </div>
      </div>
    </TeleportPopover>
  </div>
</template>

<style scoped>
/* white plot ground (theme_classic) — fills the panel body. position:relative anchors the legend
   overlay. color:#111 so any HTML text (legend) is dark on the white ground (not the app's light grey). */
.plot-host { position: relative; width: 100%; height: 100%; background: white; border-radius: var(--cc-radius-xs); overflow: hidden; color: #111; }
.plot-host :deep(svg) { display: block; }
/* legend drawn as an absolute overlay (top-right) so it never eats height / clips the x-axis.
   colour is set inline by PlotChart (theme ink); descendants inherit it (force inherit so Plot's
   own swatch styles don't override the dark-theme ink). */
.plot-host :deep(.plot-legend-overlay) {
  position: absolute; top: 4px; right: 6px; display: flex; flex-wrap: wrap; gap: 2px 10px;
  max-width: 58%; justify-content: flex-end; border-radius: var(--cc-radius-xs); padding: 1px 4px;
}
.plot-host :deep(.plot-legend-overlay *) { color: inherit !important; }
/* title overlay (top-left), theme ink set inline */
.plot-host :deep(.plot-title-overlay) {
  position: absolute; top: 4px; left: 8px; max-width: 60%; font-weight: 600; font-size: var(--cc-fs-sm);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}

/* Compact-Letter-Display letters — one per group, absolutely positioned over the plot at the
   coordinate `render()` computed from the plot's own scales. Centred on (left, top); hover shows
   a TeleportPopover with the ns-neighbour list. */
.cld-letter {
  position: absolute;
  transform: translate(-50%, -50%);
  font-weight: 700; font-size: var(--cc-fs-md);
  padding: 1px 5px;
  border-radius: var(--cc-radius-xs);
  cursor: help;
  user-select: none;
  color: inherit;                  /* theme ink, matches axis text */
}
.cld-letter:hover { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.cld-pop { padding: 8px 10px; max-width: 260px; }
.cld-pop-head { display: flex; align-items: center; gap: 8px; margin-bottom: 4px; }
.cld-letter-mini {
  font-weight: 700; font-size: var(--cc-fs-sm);
  padding: 0 5px; border-radius: var(--cc-radius-xs);
  background: color-mix(in srgb, var(--cc-accent) 22%, transparent);
}
.cld-pop-title { font-size: var(--cc-fs-sm); font-weight: 500; }
.cld-pop-list { margin: 2px 0 0; padding-left: 16px; font-size: var(--cc-fs-xs); }
.cld-pop-list li { margin: 1px 0; }
</style>
