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
import { buildPlotOptions, type BuildOpts, facetMode } from '../../plots/plot'
import { svgToImageURL, svgOf } from '../../plots/export'
import { applyPlotTheme, legendOverlay, plotTheme, titleOverlay } from '../../plots/overlays'
import { xRotationOverride, facetOverride, sameOverrides, type AutoOverride } from '../../plots/autoOverride'
import { rafCoalesce } from '../../utils/rafCoalesce'
import type { PlotDataResponse } from '../../plots/types'
import { clientAxisRectOf } from '../../plots/plotAxisRect'
import type { FrameRect } from '../../plots/frame'

const props = defineProps<{ data: PlotDataResponse | null; opts: BuildOpts }>()
// settings the RENDERER had to substitute (today: rotating x tick labels that wouldn't fit). Reported
// up so the host can say so — a plot silently disagreeing with its own controls is the thing we avoid.
// See plots/autoOverride.ts.
// `point-click` — a jitter/scatter dot was clicked. Payload carries the point identity + the
// source (image, valueName) so the host can mirror into PickHighlight / TrackHighlight for the
// RIGHT image without guessing (LINKED_BRUSHING_PLAN.md Option B write side).
// `point-brush` — user drag-selected a region over brushable dots. Plain drag = freeform lasso
// (polygon), shift+drag = axis-aligned rect. Payload is one entry per hit SERIES `(uid, vn,
// pop)` — not per (uid, vn) — because two populations under the same segmentation collide on
// the shorter key, and a lasso on one would silently drop into a bag that the renderer then
// hit-tests against the other. Same delivery model as point-click (host writes the shared bag +
// mirrors) but a whole set at once.
const emit = defineEmits<{
  'auto-override': [AutoOverride[]]
  'point-click': [{ id: number; kind: 'track' | 'cell'; imageUid: string; valueName: string; pop: string }]
  'point-brush': [{ kind: 'track' | 'cell'; sources: Array<{ imageUid: string; valueName: string; pop: string; ids: number[] }> }]
}>()
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
  // Point Plot's `--plot-background` at the theme ground so a `tip: true` rect matches the plot ink
  // rather than staying white — see `applyPlotTheme`, which every Plot.plot() site now shares.
  applyPlotTheme(node as SVGElement, !!props.opts?.darkTheme)
  host.value.append(node)
  // Delegated click on brushable dots (LINKED_BRUSHING_PLAN.md Option B write side). Every
  // brushable dot carries class `cc-brush-dot` (set by the renderer) and Observable Plot binds
  // the row object to `__data__`. `pointIdKind` on the response tells us which scope to write.
  // Skipped when the response has no `pointIdKind` (no ids emitted) — the dots exist, but
  // clicking them does nothing (no state to write; a listener that emits `null` would just
  // wake up the host for no reason).
  const kind = props.data?.pointIdKind
  if (kind) {
    node.addEventListener('click', (e) => {
      // Identity is on data-* attrs, not `__data__` — Plot binds the row INDEX (a number) to
      // `__data__`, so reading `.pointId` off it always gives undefined. The stamping happens in
      // the mark's `render` hook (plot.ts → `stampBrushIds`). A click on the group's background
      // (between dots) has no data-pid, so the guard drops it.
      const target = e.target as Element | null
      if (!target || target.tagName !== 'circle') return
      if (!target.parentElement?.classList?.contains('cc-brush-dot')) return
      const pid = target.getAttribute('data-pid')
      if (pid == null || pid === '') return
      emit('point-click', {
        id: Number(pid), kind,
        imageUid: target.getAttribute('data-uid') ?? '',
        valueName: target.getAttribute('data-vn') ?? '',
        pop: target.getAttribute('data-pop') ?? '',
      })
    })
    // A visible affordance: brushable dots take a pointer cursor. Fills a gap where dots on the
    // boxplot look inert until you hover them; a real "grab a point" gesture needs a cue. Also
    // set the crosshair cursor during a brush drag (see below) and hide the brush rect stroke
    // when idle.
    const style = document.createElement('style')
    style.textContent = `
      .cc-brush-dot { cursor: pointer; }
      svg.cc-brushing, svg.cc-brushing * { cursor: crosshair !important; }
      .cc-brush-rect,
      .cc-brush-lasso { fill: color-mix(in srgb, currentColor 15%, transparent);
                        stroke: currentColor; stroke-width: 1; stroke-dasharray: 3 3;
                        pointer-events: none; }
    `
    node.prepend(style)

    // Freeform LASSO (plain drag) + axis-aligned RECT (shift+drag) — Option B bulk write side.
    // Lasso is the default gesture because a freeform loop is what "select this cluster" looks
    // like on a jitter cloud; the rect stays for axis-aligned pulls (e.g. "everything above y=X").
    //
    // COORDINATE SPACE: all hit-testing happens in CLIENT (screen) coordinates. Reason: Observable
    // Plot wraps each mark group in `<g transform="translate(margin.left, margin.top)">`, so a
    // circle's `cx`/`cy` attributes are in the GROUP's local space — not the svg root's. Mixing
    // those with mouse positions produced by `getScreenCTM().inverse()` (which land in the svg
    // root's space) misses every dot. Client coordinates are the one space both sides can share
    // without walking a CTM chain: mouse events already give clientX/clientY, and each circle's
    // `getBoundingClientRect()` puts it in the same frame. The visible overlay (rect / path) is
    // still drawn in svg-root space via inverse-CTM'd screen points, so the shape follows the
    // cursor regardless of the group transform.
    //
    // `svg` is captured from module-scope `node` so the closures don't re-narrow — the listeners
    // are attached to THIS svg instance and are removed when it is.
    const svg = node as SVGSVGElement
    type Pt = { x: number; y: number }         // client-space (px in the viewport)
    type Svg = { x: number; y: number }        // svg-root user-space (for the overlay path)
    let brushMode: 'rect' | 'lasso' | null = null
    let brushStartClient: Pt | null = null
    let brushRect: SVGRectElement | null = null
    let brushStartSvg: Svg | null = null       // for drawing the rect overlay
    let lassoPath: SVGPathElement | null = null
    let lassoClient: Pt[] = []                 // hit-test vertices (client coords)
    let lassoSvg: Svg[] = []                   // drawing vertices (svg-root coords)

    const toSvg = (e: MouseEvent): Svg | null => {
      const pt = svg.createSVGPoint()
      pt.x = e.clientX; pt.y = e.clientY
      const ctm = svg.getScreenCTM()?.inverse()
      if (!ctm) return null
      const p = pt.matrixTransform(ctm)
      return { x: p.x, y: p.y }
    }

    svg.addEventListener('mousedown', (e) => {
      if (e.button !== 0) return
      if ((e.target as Element).closest('.cc-brush-dot circle')) return  // click on dot handles itself
      const s = toSvg(e); if (!s) return
      brushStartClient = { x: e.clientX, y: e.clientY }
      brushStartSvg = s
      brushMode = e.shiftKey ? 'rect' : 'lasso'
      if (brushMode === 'rect') {
        brushRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect')
        brushRect.setAttribute('class', 'cc-brush-rect')
        brushRect.setAttribute('x', String(s.x)); brushRect.setAttribute('y', String(s.y))
        brushRect.setAttribute('width', '0');    brushRect.setAttribute('height', '0')
        svg.appendChild(brushRect)
      } else {
        lassoClient = [{ x: e.clientX, y: e.clientY }]
        lassoSvg = [s]
        lassoPath = document.createElementNS('http://www.w3.org/2000/svg', 'path')
        lassoPath.setAttribute('class', 'cc-brush-lasso')
        lassoPath.setAttribute('d', `M ${s.x} ${s.y}`)
        svg.appendChild(lassoPath)
      }
      svg.classList.add('cc-brushing')
      e.preventDefault()
    })

    svg.addEventListener('mousemove', (e) => {
      if (!brushMode || !brushStartSvg) return
      const s = toSvg(e); if (!s) return
      if (brushMode === 'rect' && brushRect) {
        const x = Math.min(brushStartSvg.x, s.x), y = Math.min(brushStartSvg.y, s.y)
        const w = Math.abs(s.x - brushStartSvg.x), h = Math.abs(s.y - brushStartSvg.y)
        brushRect.setAttribute('x', String(x)); brushRect.setAttribute('y', String(y))
        brushRect.setAttribute('width', String(w)); brushRect.setAttribute('height', String(h))
      } else if (brushMode === 'lasso' && lassoPath) {
        // Skip points closer than ~2 client-px to the last vertex — a raw mousemove stream at
        // sub-pixel spacing produces thousands of vertices that make point-in-polygon slower and
        // add nothing to the shape. Threshold squared to avoid a sqrt per event.
        const last = lassoClient[lassoClient.length - 1]
        if ((e.clientX - last.x) ** 2 + (e.clientY - last.y) ** 2 < 4) return
        lassoClient.push({ x: e.clientX, y: e.clientY })
        lassoSvg.push(s)
        lassoPath.setAttribute('d', `M ${lassoSvg[0].x} ${lassoSvg[0].y} ` +
          lassoSvg.slice(1).map(q => `L ${q.x} ${q.y}`).join(' ') + ' Z')
      }
    })

    // Even-odd ray-cast: standard closed-polygon test. `poly` is the lasso vertex list; the
    // implicit closing edge (last → first) is handled by the wrap-around index `j`.
    const pointInPoly = (poly: Pt[], x: number, y: number): boolean => {
      let inside = false
      for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
        const xi = poly[i].x, yi = poly[i].y, xj = poly[j].x, yj = poly[j].y
        const intersect = ((yi > y) !== (yj > y)) &&
          (x < ((xj - xi) * (y - yi)) / (yj - yi + 1e-12) + xi)
        if (intersect) inside = !inside
      }
      return inside
    }

    // `hit` takes CLIENT coords (see COORDINATE SPACE note above). Each circle's client center is
    // read from `getBoundingClientRect()` — one call per dot, once per drag. Identity comes from
    // data-* attrs stamped by the mark's `render` hook (plot.ts → `stampBrushIds`), NOT from
    // `__data__` — Plot binds the row INDEX (a number) there, not the row object.
    const emitHits = (hit: (cx: number, cy: number) => boolean) => {
      // Keyed by the FULL source tuple `(uid, vn, pop)` — collapsing to just `(uid, vn)` used
      // to drop cross-vn hits on the same image (last one wins on a plain uid map), and
      // collapsing to just `uid` was one bug further along the same line. `\u0000` separator
      // is used only inside this map; the wire format is a plain array of tuples.
      const groups: Record<string, { imageUid: string; valueName: string; pop: string; ids: number[] }> = {}
      const circles = svg.querySelectorAll('g.cc-brush-dot circle, .cc-brush-dot > circle')
      for (let i = 0; i < circles.length; i++) {
        const c = circles[i] as SVGCircleElement
        const r = c.getBoundingClientRect()
        const cx = r.left + r.width / 2
        const cy = r.top + r.height / 2
        if (!hit(cx, cy)) continue
        const pid = c.getAttribute('data-pid')
        if (pid == null || pid === '') continue
        const id = Number(pid)
        const uid = c.getAttribute('data-uid') ?? ''
        const vn  = c.getAttribute('data-vn')  ?? ''
        const pop = c.getAttribute('data-pop') ?? ''
        const key = `${uid}\u0000${vn}\u0000${pop}`
        const g = groups[key] ?? (groups[key] = { imageUid: uid, valueName: vn, pop, ids: [] })
        g.ids.push(id)
      }
      // Dedupe per group before emitting — a swarm can render the same id twice at close-by
      // positions after downsample, and duplicates would swell the bag pointlessly.
      const sources = Object.values(groups).map(g => ({
        imageUid: g.imageUid, valueName: g.valueName, pop: g.pop,
        ids: Array.from(new Set(g.ids)),
      }))
      // TEMPORARY diag — one line per emit naming each source's (uid, vn, pop) + hit count.
      // Also samples one hit CIRCLE's data-* attrs so we can see what the DOM actually has.
      // Remove once the (uid, vn, pop) fix is verified end-to-end.
      if (sources.length) {
        const sample = svg.querySelector('g.cc-brush-dot circle, .cc-brush-dot > circle') as SVGCircleElement | null
        // eslint-disable-next-line no-console
        console.log('[cc-brush emit]',
          { kind, sources: sources.map(s => ({ uid: s.imageUid, vn: s.valueName, pop: s.pop, n: s.ids.length, sample: s.ids.slice(0, 5) })),
            sampleDom: sample ? { uid: sample.getAttribute('data-uid'), vn: sample.getAttribute('data-vn'),
                                  pop: sample.getAttribute('data-pop'), pid: sample.getAttribute('data-pid') } : null })
      }
      if (sources.length) emit('point-brush', { kind, sources })
    }

    const finishBrush = (e: MouseEvent) => {
      if (!brushMode || !brushStartClient) return
      const mode = brushMode
      if (mode === 'rect' && brushRect) {
        const startC = brushStartClient
        const cx0 = Math.min(startC.x, e.clientX), cx1 = Math.max(startC.x, e.clientX)
        const cy0 = Math.min(startC.y, e.clientY), cy1 = Math.max(startC.y, e.clientY)
        brushRect.remove(); brushRect = null
        // Reset mode BEFORE emitting so a downstream re-render can't re-enter mid-teardown.
        brushMode = null; brushStartClient = null; brushStartSvg = null; svg.classList.remove('cc-brushing')
        // A degenerate rect (a click, not a drag) is dropped — the click handler already fires.
        if ((cx1 - cx0) < 2 && (cy1 - cy0) < 2) return
        emitHits((cx, cy) => cx >= cx0 && cx <= cx1 && cy >= cy0 && cy <= cy1)
      } else if (mode === 'lasso' && lassoPath) {
        const pts = lassoClient
        lassoPath.remove(); lassoPath = null; lassoClient = []; lassoSvg = []
        brushMode = null; brushStartClient = null; brushStartSvg = null; svg.classList.remove('cc-brushing')
        // Need at least a triangle for a meaningful polygon; anything less was a stray drag.
        if (pts.length < 3) return
        emitHits((cx, cy) => pointInPoly(pts, cx, cy))
      }
    }
    svg.addEventListener('mouseup', finishBrush)
    svg.addEventListener('mouseleave', finishBrush)
  }
  // report any setting the builder substituted (`_autoRotatedX`) — but only when it actually CHANGED.
  // The host stores this and the board stores the host's readout, so an unconditional emit makes every
  // render a state write, which renders again. See sameOverrides in plots/autoOverride.ts.
  const overrides = [
    xRotationOverride(!!base._autoRotatedX, !!props.opts.rotateXLabel),
    facetOverride(!!base._facetIgnored, facetMode(props.opts)),
  ].filter(Boolean) as AutoOverride[]
  if (!lastOverrides || !sameOverrides(overrides, lastOverrides)) {
    lastOverrides = overrides; emit('auto-override', overrides)
  }

  const ink = plotTheme(!!props.opts.darkTheme).ink
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
  // renders are coalesced to a frame, so `node` can be one frame behind the current props — an export
  // must never serialise the PREVIOUS chart. The light path rebuilds from props anyway.
  if (!light) { await frame.flush(); return svgToImageURL(svgOf(node as Element | null), type) }
  if (!host.value) return null
  if (!Plot) Plot = await import('@observablehq/plot')
  const base = props.data ? buildPlotOptions(Plot, props.data, { ...props.opts, darkTheme: false }) as any : null   // eslint-disable-line @typescript-eslint/no-explicit-any
  if (!base) return null
  const w = Math.max(160, host.value.clientWidth || 320)
  const h = Math.max(140, host.value.clientHeight || 260)
  const off = Plot.plot({ ...base, width: w, height: h }) as SVGElement
  // Light by construction (`darkTheme: false` above), so this only restates Plot's own default — but
  // stated, so the ratchet holds and an export never inherits a ground nobody chose.
  applyPlotTheme(off, false)
  return svgToImageURL(svgOf(off as unknown as Element), type)
}
// The rendered plot's AXIS RECT in client space — Observable Plot attaches `scale(name)` to the
// returned node; `range` on `scale('x')/'y'` is the axis edge in SVG px. Returned as a `FrameRect`
// (nullable) so a cluster panel's `getFrame()` can wrap it in `rectFrame` for point-out. Reads live
// (no cache) — a resize / re-render swaps `node`, so any snapshot would go stale on the next paint.
function axisRect(): FrameRect | null {
  const n = node as (Element & { scale?: (name: string) => { range?: readonly number[] } | null }) | null
  if (!n || typeof n.scale !== 'function') return null
  return clientAxisRectOf(n, n.scale('x'), n.scale('y'))
}
defineExpose({ toImageURL, axisRect })

// Coalesce to at most ONE render per animation frame (docs/UI.md → "Continuous controls"). Both
// triggers are burst sources: a styling slider (point size, font size, x angle) fires per pixel of
// travel, and the ResizeObserver fires per frame while a panel/row-height is dragged — and a board can
// hold up to 36 slots, each of which rebuilds its whole Plot from scratch. Rendering per event queued
// tens of superseded rebuilds and the board kept redrawing well after the mouse was released.
// `render(0)`: never the legend's corrective second pass, which drives itself from inside `render`.
const frame = rafCoalesce(() => render(0))
const scheduleRender = () => frame.schedule()

watch(() => [props.data, props.opts], scheduleRender, { deep: true })
onMounted(() => {
  render()
  if (host.value && typeof ResizeObserver !== 'undefined') {
    ro = new ResizeObserver(scheduleRender)
    ro.observe(host.value)
  }
})
onBeforeUnmount(() => { frame.cancel(); ro?.disconnect(); ro = null; node?.remove(); node = null; legendNode?.remove(); legendNode = null; titleNode?.remove(); titleNode = null })
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
