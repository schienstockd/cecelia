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
import { ref, computed, watch, onMounted } from 'vue'
import { useLogStore } from '../../stores/log'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import PlotChart from '../../components/plots/PlotChart.vue'
import { defaultVis, type BuildOpts } from '../../plots/plot'
import type { PlotDataResponse } from '../../plots/types'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  popType: 'clust' | 'trackclust'; suffix: string
  featureOptions: string[]; nameMap: Record<string, string>
  state: { features?: string[] }
}>()
const emit = defineEmits<{ activate: [number]; remove: [] }>()
const log = useLogStore()

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
    const body: Record<string, unknown> = {
      projectUid: props.projectUid, popType: props.popType, granularity: granularity.value,
      chartType: 'matrix', matrixMode: 'profile', category: `clusters.${props.suffix}`,
      separator: '_', pops: ['root'], measures: features.value, zscore: true,
    }
    if (props.setUid) { body.setUid = props.setUid; if (props.imageUids.length) body.imageUids = props.imageUids }
    else if (props.imageUids[0]) body.imageUid = props.imageUids[0]
    const res = await fetch('/api/plot_data', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    if (!res.ok) { heatmap.value = null; err.value = (await res.json()).error ?? res.statusText; return }
    const r = await res.json() as PlotDataResponse & { yLabels?: string[]; cells?: { y: string }[] }
    // resolve channel row names to display names (matrix aggregates by raw name; we relabel for show)
    if (r.yLabels) r.yLabels = r.yLabels.map(label)
    if (r.cells) for (const c of r.cells) c.y = label(c.y)
    heatmap.value = r
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`Cluster heatmap: ${err.value}`, { source: 'cluster' })
  } finally { loading.value = false }
}

watch([() => props.projectUid, () => props.imageUids.join(','), () => props.setUid, () => props.popType,
       () => props.suffix, () => features.value.join(',')], load)
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
      <button class="cc-btn cc-btn-ghost" @click="load" v-tooltip.bottom="'Reload'"><i class="pi pi-refresh" /></button>
    </template>
    <div class="hm-body">
      <PlotChart v-if="heatmap" :data="heatmap" :opts="opts" />
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
.hm-body { display: flex; flex: 1; min-height: 0; padding: 6px; }
.hm-empty { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; color: var(--cc-text-dim); text-align: center; padding: 1rem; }
.hm-empty .pi { font-size: 1.4rem; opacity: 0.6; }
.hm-empty p { margin: 0; font-size: 0.8rem; max-width: 22rem; }
</style>
