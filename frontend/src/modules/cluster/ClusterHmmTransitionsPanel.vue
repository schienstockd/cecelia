<!--
  HMM transitions — a per-population / per-cluster behaviour plot for TRACK clustering (ports
  behaviourDTx.Rmd's transitions heat). Two modes, mirroring the heatmap:
   • pops highlighted (eye) → one from→to dot grid per shown population.
   • none highlighted     → one grid per CLUSTER (per-cluster fallback).
  Each facet is a from-state × to-state grid of dots sized + coloured by within-group transition
  frequency (live.cell.hmm.transitions.<measure>, whose values are "from_to"). Follows the manager's
  global/local scope.

  Data: the shared `frequency` aggregation (POST /api/plot_data, popType=trackclust, granularity=cell
  → member cells carry their transition; scope=summarised pools images). One series per group (pop, or
  cluster via groupBy=clusters.<suffix> + collapseSeries over root); each category "from_to" is split
  on the first "_" and rendered as a faceted dot grid (self-contained Observable Plot).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, useTemplateRef, nextTick } from 'vue'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import { defaultVis, type VisProps } from '../../plots/plot'
import { elementToImageURL, downloadDataUrl, downloadBlob, rowsToCsv } from '../../plots/export'
import { titleOverlay } from '../../plots/overlays'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  suffix: string
  shownPops: { path: string; name: string; colour: string; clusterIds: number[] }[]
  hmmCols: string[]                       // live.cell.hmm.transitions.<measure>
  vis?: VisProps                          // plot styling (from the pop manager, global/local scope)
  state: { measure?: string }
  docked?: boolean                        // fill a grid slot (Analysis board) instead of free-floating
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()
const log = useLogStore()
const v = computed<VisProps>(() => props.vis ?? defaultVis())

const measure = computed({
  get: () => (props.state.measure && props.hmmCols.includes(props.state.measure)) ? props.state.measure : (props.hmmCols[0] ?? ''),
  set: v => (props.state.measure = v),
})
const shortName = (c: string) => c.replace(/^live\.cell\.hmm\.transitions\./, '')

const host = useTemplateRef<HTMLElement>('host')
const loading = ref(false)
const err = ref('')
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let Plot: any = null
let node: (HTMLElement | SVGElement) | null = null
let legendNode: HTMLElement | null = null
let titleNode: HTMLElement | null = null
let ro: ResizeObserver | null = null
// `forceLight` flips to a light render for the board's PDF export (dark theme is on-screen only)
const forceLight = ref(false)
const effDark = computed(() => forceLight.value ? false : v.value.darkTheme)
const hostBg = computed(() => (effDark.value ? '#1f2226' : 'white'))
// reactive so the empty-state / host v-show re-evaluate when data arrives (a bare array does not).
const rows = ref<{ group: string; from: string; to: string; freq: number }[]>([])

async function load() {
  err.value = ''
  rows.value = []
  if (!props.projectUid || !props.imageUids.length) { render(); return }
  if (!measure.value) { err.value = 'No HMM transition columns found for this run.'; render(); return }
  const popMode = props.shownPops.length > 0
  loading.value = true
  try {
    const body: Record<string, unknown> = {
      projectUid: props.projectUid, popType: 'trackclust', granularity: 'cell',
      chartType: 'frequency', measure: measure.value, normalize: true, scope: 'summarised',
      pops: popMode ? props.shownPops.map(p => p.path) : ['root'],
      ...(popMode ? {} : { groupBy: `clusters.${props.suffix}`, collapseSeries: true }),
    }
    if (props.setUid) { body.setUid = props.setUid; if (props.imageUids.length) body.imageUids = props.imageUids }
    else if (props.imageUids[0]) body.imageUid = props.imageUids[0]
    const res = await fetch('/api/plot_data', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    if (!res.ok) { err.value = (await res.json()).error ?? res.statusText; render(); return }
    const r = await res.json() as { categories?: string[]; series?: { pop?: string; value_name?: string; group?: string; values?: number[] }[] }
    const cats = r.categories ?? []
    const labelOf = (s: { pop?: string; value_name?: string; group?: string }) => popMode
      ? (props.shownPops.find(p => String(s.pop) === (String(s.value_name ?? '') + p.path) || String(s.pop) === p.path)?.name ?? String(s.pop))
      : `cluster ${s.group}`
    const out: { group: string; from: string; to: string; freq: number }[] = []
    for (const s of r.series ?? []) {
      const g = labelOf(s)
      cats.forEach((c, i) => {
        const sep = c.indexOf('_')                        // "from_to" — split on the first "_" (matches crosstab)
        if (sep < 0) return
        out.push({ group: g, from: c.slice(0, sep), to: c.slice(sep + 1), freq: (s.values ?? [])[i] ?? 0 })
      })
    }
    rows.value = out
    render()
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`HMM transitions: ${err.value}`, { source: 'cluster' })
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
  const w = Math.max(220, host.value.clientWidth || 380)
  const h = Math.max(180, host.value.clientHeight || 280)
  const o = v.value
  const fg = effDark.value ? '#e6e6e6' : '#111'
  const bg = effDark.value ? '#1f2226' : 'white'
  const hi = Math.max(1e-9, ...rows.value.map(r => r.freq))
  const colorScale = { scheme: 'YlOrRd', label: 'freq', domain: [0, hi] }
  // The continuous colour ramp is placed as a VERTICAL bar in the right margin (not a top overlay) so
  // it never overlaps the faceted grid / facet-label row. Reserve `marginRight` for it; the top margin
  // only needs room for facet labels (+ title).
  const topPad = Math.max(o.title ? 24 : 0, 20)
  node = Plot.plot({
    width: w, height: h, marginLeft: 40, marginBottom: 40, marginTop: topPad, marginRight: o.legend ? 52 : 12, grid: o.grid,
    style: { background: bg, color: fg, fontSize: `${o.fontSize || 11}px` },
    x: { label: o.labX || 'to state', type: 'point' },
    y: { label: o.labY || 'from state', type: 'point', reverse: true },
    r: { range: [1, 10] },
    color: colorScale,
    fx: { label: null },
    marks: [
      Plot.dot(rows.value, { fx: 'group', x: 'to', y: 'from', r: 'freq', fill: 'freq', stroke: 'black', strokeWidth: 0.4, tip: true }),
    ],
  }) as SVGElement
  host.value.append(node)
  if (o.legend) { legendNode = vRampLegend(hi, fg); host.value.append(legendNode) }
  if (o.title) { titleNode = titleOverlay(o.title, fg); host.value.append(titleNode) }
}

// vertical YlOrRd colour ramp for the right margin (max at top, 0 at bottom). Matches the plot's
// `scheme: 'YlOrRd'` colour encoding; an HTML overlay so it's captured on export (foreignObject).
function vRampLegend(hi: number, ink: string): HTMLElement {
  const el = document.createElement('div')
  el.className = 'hmm-vlegend'; el.style.color = ink
  const fmt = (x: number) => Math.abs(x) >= 1 ? x.toFixed(1) : x.toFixed(2)
  el.innerHTML = `<span class="vl-cap">freq</span><span class="vl-hi">${fmt(hi)}</span>` +
                 `<span class="vl-ramp"></span><span class="vl-lo">0</span>`
  return el
}

// export the shown chart (PNG/SVG) or its data (CSV) — shared helpers, like SummaryPanel
function exportAs(kind: string) {
  const stem = `hmm_transitions_${measure.value}`.replace(/[^\w.-]+/g, '_')
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

// board export: a plot-only LIGHT re-render → PNG, and the shown rows as CSV (generic panel contract)
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const url = await elementToImageURL(host.value, 'png', '#ffffff')
  forceLight.value = false; await render()
  return url
}
function getCsv(): string | null { return rows.value.length ? rowsToCsv(rows.value) : null }
defineExpose({ exportImage, getCsv })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey" title="HMM transitions"
               :docked="docked" @activate="emit('activate', $event)" @remove="emit('remove')">
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
      <select v-if="!docked" class="hmm-export" v-tooltip.top="'Export the shown plot'" :disabled="!rows.length"
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
        <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-circle']" />
        <p>{{ loading ? 'Loading…' : (err || 'No HMM transitions to show.') }}</p>
      </div>
    </div>
  </CanvasPanel>
</template>

<style scoped>
.hmm-body { display: flex; flex: 1; min-height: 0; padding: 6px; }
.hmm-host { position: relative; flex: 1; min-height: 0; background: white; border-radius: 3px; overflow: hidden; }
.hmm-host :deep(svg) { display: block; }
/* vertical colour ramp in the right margin (max at top, 0 at bottom) */
.hmm-host :deep(.hmm-vlegend) { position: absolute; top: 50%; right: 5px; transform: translateY(-50%);
  display: flex; flex-direction: column; align-items: center; gap: 3px; font-size: 10px; line-height: 1; }
.hmm-host :deep(.hmm-vlegend .vl-ramp) { width: 11px; height: 90px; border-radius: 2px; border: 1px solid rgba(128,128,128,0.4);
  background: linear-gradient(to top, #ffffcc, #ffeda0, #fed976, #feb24c, #fd8d3c, #fc4e2a, #e31a1c, #b10026); }
.hmm-host :deep(.hmm-vlegend .vl-cap) { font-weight: 600; }
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
