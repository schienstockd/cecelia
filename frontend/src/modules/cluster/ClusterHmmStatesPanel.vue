<!--
  HMM states — a per-population / per-cluster behaviour plot for TRACK clustering (ports
  behaviourDTx.Rmd's HMM state bar). Two modes, mirroring the heatmap:
   • pops highlighted (eye) → one 100%-stacked bar per shown population.
   • none highlighted     → one bar per CLUSTER (like the heatmap's per-cluster fallback).
  In both, the bar is the within-group frequency of each cell-level HMM state
  (live.cell.hmm.state.<measure>), coloured by state. Follows the manager's global/local scope.

  Data: the shared `frequency` aggregation (POST /api/plot_data, popType=trackclust,
  granularity=cell → member cells carry their HMM state; scope=summarised pools images). Per-cluster
  mode adds groupBy=clusters.<suffix> + collapseSeries (one series per cluster, over root). Rendering
  is self-contained (lazy Observable Plot, like UmapView/ScatterGL) because the layout is a transpose
  of the generic stacked bar (x = group, fill = state) — not worth a new shared chart type.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import { defaultVis, paletteRange, type VisProps } from '../../plots/plot'
import { elementToImageURL, downloadDataUrl, downloadBlob, rowsToCsv } from '../../plots/export'
import { legendOverlay, titleOverlay } from '../../plots/overlays'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  suffix: string
  shownPops: { path: string; name: string; colour: string; clusterIds: number[] }[]
  hmmCols: string[]                       // live.cell.hmm.state.<measure>
  vis?: VisProps                          // plot styling (from the pop manager, global/local scope)
  state: { measure?: string }
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()
const log = useLogStore()
const v = computed<VisProps>(() => props.vis ?? defaultVis())

const measure = computed({
  get: () => (props.state.measure && props.hmmCols.includes(props.state.measure)) ? props.state.measure : (props.hmmCols[0] ?? ''),
  set: v => (props.state.measure = v),
})
const shortName = (c: string) => c.replace(/^live\.cell\.hmm\.state\./, '')

const host = useTemplateRef<HTMLElement>('host')
const loading = ref(false)
const err = ref('')
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let Plot: any = null
let node: (HTMLElement | SVGElement) | null = null
let legendNode: HTMLElement | null = null
let titleNode: HTMLElement | null = null
let ro: ResizeObserver | null = null
// host background follows the dark-theme flag (no white border around a dark plot), like PlotChart
const hostBg = computed(() => (v.value.darkTheme ? '#1f2226' : 'white'))
// reactive so the empty-state / host v-show re-evaluate when data arrives (a bare array does not).
const rows = ref<{ group: string; state: string; freq: number }[]>([])
const cats = ref<string[]>([])           // HMM state levels, in order (for the colour domain)

async function load() {
  err.value = ''
  rows.value = []
  if (!props.projectUid || !props.imageUids.length) { render(); return }
  if (!measure.value) { err.value = 'No HMM state columns found for this run.'; render(); return }
  const popMode = props.shownPops.length > 0
  loading.value = true
  try {
    const body: Record<string, unknown> = {
      projectUid: props.projectUid, popType: 'trackclust', granularity: 'cell',
      chartType: 'frequency', measure: measure.value, normalize: true, scope: 'summarised',
      // per-population when pops are shown; else per-cluster (group root by the cluster column).
      pops: popMode ? props.shownPops.map(p => p.path) : ['root'],
      ...(popMode ? {} : { groupBy: `clusters.${props.suffix}`, collapseSeries: true }),
    }
    if (props.setUid) { body.setUid = props.setUid; if (props.imageUids.length) body.imageUids = props.imageUids }
    else if (props.imageUids[0]) body.imageUid = props.imageUids[0]
    const res = await fetch('/api/plot_data', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    if (!res.ok) { err.value = (await res.json()).error ?? res.statusText; render(); return }
    const r = await res.json() as { categories?: string[]; series?: { pop?: string; value_name?: string; group?: string; values?: number[] }[] }
    cats.value = r.categories ?? []
    // pop series come back as "value_name + path"; match to the shown pop for a clean label.
    const labelOf = (s: { pop?: string; value_name?: string; group?: string }) => popMode
      ? (props.shownPops.find(p => String(s.pop) === (String(s.value_name ?? '') + p.path) || String(s.pop) === p.path)?.name ?? String(s.pop))
      : `cluster ${s.group}`
    const out: { group: string; state: string; freq: number }[] = []
    for (const s of r.series ?? []) {
      const g = labelOf(s)
      cats.value.forEach((c, i) => out.push({ group: g, state: `state ${c}`, freq: (s.values ?? [])[i] ?? 0 }))
    }
    rows.value = out
    render()
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`HMM states: ${err.value}`, { source: 'cluster' })
    render()
  } finally { loading.value = false }
}

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  node?.remove(); node = null
  legendNode?.remove(); legendNode = null
  titleNode?.remove(); titleNode = null
  if (!rows.value.length) return
  const w = Math.max(200, host.value.clientWidth || 360)
  const h = Math.max(160, host.value.clientHeight || 260)
  const o = v.value
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  // palette knob → explicit colours for the state levels; 'standard' (null) keeps a categorical scheme
  const domain = cats.value.map(c => `state ${c}`)
  const range = paletteRange(o, domain.length)
  const colorScale = range ? { domain, range } : { scheme: 'Tableau10', domain }
  // NB: no inline `legend` — that wraps the chart in a <figure> whose swatch legend sits on a white
  // ground with light-grey text (invisible on dark). Render a bare svg + a themed overlay, like PlotChart.
  // Build+measure the legend FIRST and reserve exactly its height above the plot (also room for a title).
  // reserve a top band for the overlay legend / title (fixed — the legend is an absolute overlay with
  // no layout height, and may be measured before layout). Swatches are a single short row → ~28.
  const topPad = Math.max(o.legend && domain.length > 1 ? 28 : 0, o.title ? 24 : 0, 12)
  if (o.legend && domain.length > 1) { legendNode = legendOverlay(Plot, colorScale, fg); if (legendNode) host.value.append(legendNode) }
  node = Plot.plot({
    width: w, height: h, marginLeft: 90, marginRight: 12, marginTop: topPad,
    style: { background: bg, color: fg, fontSize: `${o.fontSize || 11}px` },
    x: { label: o.labX || 'HMM proportion', domain: [0, 1], grid: o.grid },
    y: { label: o.labY || '' },
    color: colorScale,
    marks: [
      Plot.barX(rows.value, { y: 'group', x: 'freq', fill: 'state', order: 'state', tip: true }),
      Plot.ruleX([0, 1], { stroke: 'currentColor' }),
    ],
  }) as SVGElement
  host.value.append(node)
  if (legendNode) host.value.append(legendNode)   // move legend to the end → it paints ON TOP of the plot
  if (o.title) { titleNode = titleOverlay(o.title, fg); host.value.append(titleNode) }
}

// export the shown chart (PNG/SVG) or its data (CSV) — shared helpers, like SummaryPanel
function exportAs(kind: string) {
  const stem = `hmm_states_${measure.value}`.replace(/[^\w.-]+/g, '_')
  if (kind === 'csv') { if (rows.value.length) downloadBlob(`${stem}.csv`, new Blob([rowsToCsv(rows.value)], { type: 'text/csv' })) }
  else if (kind === 'png' || kind === 'svg') elementToImageURL(host.value, kind, hostBg.value).then(url => url && downloadDataUrl(`${stem}.${kind}`, url))
}

watch([() => props.projectUid, () => props.imageUids.join(','), () => props.setUid, () => props.suffix,
       () => measure.value, () => JSON.stringify(props.shownPops.map(p => [p.path, p.clusterIds]))], load)
// styling is render-only (no refetch) — re-render when the vis bag changes
watch(v, render, { deep: true })
onMounted(() => {
  load()
  if (host.value && typeof ResizeObserver !== 'undefined') { ro = new ResizeObserver(() => render()); ro.observe(host.value) }
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null; node?.remove(); node = null; legendNode?.remove(); legendNode = null; titleNode?.remove(); titleNode = null })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey" title="HMM states"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <template #actions>
      <select v-if="hmmCols.length > 1" v-model="measure" class="cc-sel"
              v-tooltip.bottom="'Which HMM measure to show'">
        <option v-for="c in hmmCols" :key="c" :value="c">{{ shortName(c) }}</option>
      </select>
      <button class="cc-btn cc-btn-ghost" @click="load" v-tooltip.bottom="'Reload'"><i class="pi pi-refresh" /></button>
    </template>
    <!-- utility actions (duplicate / export) in the footer, like SummaryPanel -->
    <template #footer>
      <button class="hmm-iconbtn" type="button" @click="emit('duplicate')"
              v-tooltip.top="'Duplicate this plot (same settings) to tweak one thing'"><i class="pi pi-copy" /></button>
      <select class="hmm-export" v-tooltip.top="'Export the shown plot'" :disabled="!rows.length"
              @change="exportAs(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="csv">Data (CSV)</option>
        <option value="png">Image (PNG)</option>
        <option value="svg">Image (SVG)</option>
      </select>
    </template>
    <div class="hmm-body">
      <div v-show="rows.length" ref="host" class="hmm-host" :style="{ background: hostBg }" />
      <div v-if="!rows.length" class="hmm-empty">
        <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-chart-bar']" />
        <p>{{ loading ? 'Loading…' : (err || 'No HMM states to show.') }}</p>
      </div>
    </div>
  </CanvasPanel>
</template>

<style scoped>
.hmm-body { display: flex; flex: 1; min-height: 0; padding: 6px; }
.hmm-host { position: relative; flex: 1; min-height: 0; background: white; border-radius: 3px; overflow: hidden; }
.hmm-host :deep(svg) { display: block; }
/* themed legend/title overlays (float over the plot, theme ink) — see plots/overlays.ts */
.hmm-host :deep(.plot-legend-overlay) { position: absolute; top: 4px; right: 6px; display: flex; flex-wrap: wrap;
  gap: 2px 10px; max-width: 62%; justify-content: flex-end; border-radius: 3px; padding: 1px 4px; }
.hmm-host :deep(.plot-legend-overlay *) { color: inherit !important; }
.hmm-host :deep(.plot-title-overlay) { position: absolute; top: 4px; left: 8px; max-width: 70%; font-weight: 600;
  font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.cc-sel { font-size: 12px; }
.hmm-iconbtn { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.7rem; }
.hmm-iconbtn:hover { color: var(--cc-text); border-color: #484f58; }
.hmm-export { font-size: 12px; max-width: 7rem; }
.hmm-empty { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; color: var(--cc-text-dim); text-align: center; padding: 1rem; }
.hmm-empty .pi { font-size: 1.4rem; opacity: 0.6; }
.hmm-empty p { margin: 0; font-size: 0.8rem; max-width: 22rem; }
</style>
