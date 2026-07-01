<!--
  Cluster heatmap — a SUMMARY plot (server-aggregated, drawn by the generic PlotChart): cluster
  signature = clusters × the features the run used (mean / z-score), via the shared matrix
  aggregation (POST /api/plot_data, chartType=matrix/profile, category=clusters.{suffix}). Wrapped in
  CanvasPanel like any plot. Non-interactive — contrast UmapView (a WebGL interactive view).

  Features default to exactly the columns the clustering run used (the {props}.clustfeatures sidecar,
  surfaced as `featureOptions`), pickable in the controls. Channel measures are selected/aggregated by
  RAW name (mean_intensity_N) but displayed via `nameMap` (raw → channel display name).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, useTemplateRef } from 'vue'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import PlotChart from '../../components/plots/PlotChart.vue'
import { defaultVis, plotDataToCsv, type BuildOpts } from '../../plots/plot'
import { downloadDataUrl, downloadBlob } from '../../plots/export'
import type { PlotDataResponse } from '../../plots/types'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  popType: 'clust' | 'trackclust'; suffix: string
  featureOptions: string[]; nameMap: Record<string, string>
  // populations shown in the manager: when present, columns become these pops (per-population
  // profile) instead of the raw cluster IDs. Empty → per-cluster. `clusterIds` is carried only so
  // the reload watch fires when the cluster→pop assignment changes (membership changes, same path).
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  state: { features?: string[] }
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()
const log = useLogStore()
const plotRef = useTemplateRef<{ toImageURL(t: 'png' | 'svg'): Promise<string | null> }>('plotRef')

// export the shown heatmap: CSV (the aggregated cells) or PNG/SVG (the rendered chart) — like SummaryPanel
function exportAs(kind: string) {
  const stem = `cluster_heatmap_${props.suffix}`.replace(/[^\w.-]+/g, '_')
  if (kind === 'csv') { if (heatmap.value) downloadBlob(`${stem}.csv`, new Blob([plotDataToCsv(heatmap.value)], { type: 'text/csv' })) }
  else if (kind === 'png' || kind === 'svg') plotRef.value?.toImageURL(kind).then(url => url && downloadDataUrl(`${stem}.${kind}`, url))
}

const granularity = computed(() => (props.popType === 'trackclust' ? 'track' : 'cell'))
const features = computed(() => props.state.features ?? [])
const heatmap = ref<PlotDataResponse | null>(null)
const loading = ref(false)
const err = ref('')
const opts = computed<BuildOpts>(() => ({
  ...defaultVis(), chartType: 'heatmap', byImage: false, normalize: false, errorMetric: 'sd', colorOf: () => '#8888aa',
}))
const label = (raw: string) => props.nameMap[raw] ?? raw

function toggleFeature(f: string) {
  props.state.features = features.value.includes(f) ? features.value.filter(x => x !== f) : [...features.value, f]
}

async function load() {
  err.value = ''
  if (!props.projectUid || !props.imageUids.length) { heatmap.value = null; return }
  if (!features.value.length) { heatmap.value = null; err.value = 'Pick at least one feature for the heatmap.'; return }
  loading.value = true
  try {
    // per-population when pops are shown (category = the pop_df `pop` column over those pops);
    // otherwise per-cluster (category = the cluster column over root).
    const pops = props.shownPops ?? []
    const popMode = pops.length > 0
    const body: Record<string, unknown> = {
      projectUid: props.projectUid, popType: props.popType, granularity: granularity.value,
      chartType: 'matrix', matrixMode: 'profile',
      category: popMode ? 'pop' : `clusters.${props.suffix}`,
      separator: '_', pops: popMode ? pops.map(p => p.path) : ['root'],
      measures: features.value, zscore: true,
    }
    if (props.setUid) { body.setUid = props.setUid; if (props.imageUids.length) body.imageUids = props.imageUids }
    else if (props.imageUids[0]) body.imageUid = props.imageUids[0]
    const res = await fetch('/api/plot_data', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    if (!res.ok) { heatmap.value = null; err.value = (await res.json()).error ?? res.statusText; return }
    const r = await res.json() as PlotDataResponse & { yLabels?: string[]; xLabels?: string[]; cells?: { x: string; y: string }[] }
    // resolve channel row names to display names (matrix aggregates by raw name; we relabel for show)
    if (r.yLabels) r.yLabels = r.yLabels.map(label)
    if (r.cells) for (const c of r.cells) c.y = label(c.y)
    // per-population columns come back as pop PATHS → show the pop name instead
    if (popMode) {
      const nameByPath = Object.fromEntries(pops.map(p => [p.path, p.name]))
      const xlabel = (path: string) => nameByPath[path] ?? path
      if (r.xLabels) r.xLabels = r.xLabels.map(xlabel)
      if (r.cells) for (const c of r.cells) c.x = xlabel(c.x)
    }
    heatmap.value = r
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`Cluster heatmap: ${err.value}`, { source: 'cluster' })
  } finally { loading.value = false }
}

watch([() => props.projectUid, () => props.imageUids.join(','), () => props.setUid, () => props.popType,
       () => props.suffix, () => features.value.join(','),
       () => JSON.stringify((props.shownPops ?? []).map(p => [p.path, p.clusterIds]))], load)
onMounted(load)
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey" title="Heatmap"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <template #actions>
      <details class="feat">
        <summary v-tooltip.bottom="'Features (rows) — the measures this clustering run used'">
          features ({{ features.length }})
        </summary>
        <div class="feat-list">
          <label v-for="f in featureOptions" :key="f" class="feat-row">
            <input type="checkbox" :checked="features.includes(f)" @change="toggleFeature(f)" /> {{ label(f) }}
          </label>
          <p v-if="!featureOptions.length" class="feat-empty">No recorded features — re-run clustering, or this run predates feature tracking.</p>
        </div>
      </details>
      <button class="cc-btn cc-btn-ghost" @click="load"
              v-tooltip.bottom="'Reload (e.g. after re-running clustering at the same suffix)'"><i class="pi pi-refresh" /></button>
    </template>
    <!-- utility actions (duplicate / export) in the footer, like SummaryPanel / the HMM panels -->
    <template #footer>
      <button class="hm-iconbtn" type="button" @click="emit('duplicate')"
              v-tooltip.top="'Duplicate this plot (same features) to tweak one thing'"><i class="pi pi-copy" /></button>
      <select class="hm-export" v-tooltip.top="'Export the shown plot'" :disabled="!heatmap"
              @change="exportAs(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="csv">Data (CSV)</option>
        <option value="png">Image (PNG)</option>
        <option value="svg">Image (SVG)</option>
      </select>
    </template>
    <div class="hm-body">
      <PlotChart v-if="heatmap" ref="plotRef" :data="heatmap" :opts="opts" />
      <div v-else class="hm-empty">
        <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-table']" />
        <p>{{ loading ? 'Loading…' : (err || 'Pick features to build the cluster heatmap.') }}</p>
      </div>
    </div>
  </CanvasPanel>
</template>

<style scoped>
.feat { position: relative; font-size: 12px; }
.feat summary { cursor: pointer; color: var(--cc-text-dim); list-style: none; padding: 2px 6px; border: 1px solid var(--cc-border); border-radius: 4px; }
.feat[open] summary { color: var(--cc-text); }
.feat-list { position: absolute; z-index: 10; top: 1.7rem; left: 0; min-width: 12rem; max-height: 16rem; overflow-y: auto;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 5px; padding: 5px; box-shadow: 0 4px 14px rgba(0,0,0,0.4); }
.feat-row { display: flex; align-items: center; gap: 5px; padding: 2px 3px; color: var(--cc-text); white-space: nowrap; }
.feat-empty { color: var(--cc-text-dim); font-size: 11px; margin: 4px; }
.hm-iconbtn { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.7rem; }
.hm-iconbtn:hover { color: var(--cc-text); border-color: #484f58; }
.hm-export { font-size: 12px; max-width: 7rem; }
.hm-body { display: flex; flex: 1; min-height: 0; padding: 6px; }
.hm-empty { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; color: var(--cc-text-dim); text-align: center; padding: 1rem; }
.hm-empty .pi { font-size: 1.4rem; opacity: 0.6; }
.hm-empty p { margin: 0; font-size: 0.8rem; max-width: 22rem; }
</style>
