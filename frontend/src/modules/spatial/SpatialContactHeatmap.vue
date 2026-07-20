<!--
  CODEX pairwise cell-type contact heatmap (log-odds of observed vs expected contacts; Goltsev 2018).
  A SUMMARY-style plot drawn by the shared PlotChart (chartType 'heatmap') — the data is the precomputed
  spatialAnalysis.neighbourStats matrix (GET /api/plots/contact_matrix), not a pop_df aggregation, so it
  has a small read-only route instead of /api/plot_data, but the RENDERER is the generic one (no bespoke
  heatmap). Positive = association, negative = avoidance. SPATIAL_REGIONS_PLAN Decision 16.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useDataRefresh } from '../../composables/useDataRefresh'
import PlotChart from '../../components/plots/PlotChart.vue'
import { defaultVis, type BuildOpts } from '../../plots/plot'
import { contactMatrixToPlotData, hasContactMatrix, type ContactMatrixResponse } from '../../utils/contactHeatmap'

const props = defineProps<{ imageUids: string[] }>()

const meta = useProjectMetaStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)

const resp = ref<ContactMatrixResponse | null>(null)
const suffix = ref('')
const loading = ref(false)

const data = computed(() => (resp.value && hasContactMatrix(resp.value)) ? contactMatrixToPlotData(resp.value) : null)
// diverging around 0 reads best for +/- log-odds; values printed so it's legible regardless of scale.
const opts = computed<BuildOpts>(() => ({
  ...defaultVis(), chartType: 'heatmap', heatmapScale: 'zscore', heatmapValues: true,
  byImage: false, normalize: false, errorMetric: 'sd', colorOf: () => '#8888aa',
}))

async function load() {
  resp.value = null
  if (!projectUid.value || !imageUid.value) return
  loading.value = true
  try {
    const q = new URLSearchParams({ projectUid: projectUid.value, imageUid: imageUid.value })
    if (suffix.value) q.set('suffix', suffix.value)
    const res = await fetch(`/api/plots/contact_matrix?${q}`)
    if (!res.ok) return
    const r = await res.json() as ContactMatrixResponse
    resp.value = r
    if (!suffix.value && r.suffix) suffix.value = r.suffix   // adopt the run the backend picked
  } finally { loading.value = false }
}

watch(() => [imageUid.value, suffix.value, projectUid.value], load, { immediate: true })
useDataRefresh(() => props.imageUids, load)   // refetch when a spatial task finishes on these images
</script>

<template>
  <div class="contact-heatmap">
    <div v-if="resp && resp.suffixes.length > 1" class="ch-controls">
      <label>Run
        <select :value="suffix" @change="suffix = ($event.target as HTMLSelectElement).value">
          <option v-for="s in resp.suffixes" :key="s" :value="s">{{ s }}</option>
        </select>
      </label>
      <span v-if="resp" class="ch-meta">{{ resp.nCells }} cells · {{ resp.nEdges }} contacts</span>
    </div>
    <PlotChart v-if="data" :data="data" :opts="opts" />
    <div v-else-if="loading" class="ch-empty">Loading…</div>
    <div v-else class="ch-empty">No contact statistics — run “Contact statistics” for this image first.</div>
  </div>
</template>

<style scoped>
.contact-heatmap { display: flex; flex-direction: column; gap: 0.5rem; }
.ch-controls { display: flex; align-items: center; gap: 1rem; font-size: 0.85rem; }
.ch-meta { color: var(--text-muted, #888); }
.ch-empty { padding: 1rem; color: var(--text-muted, #888); font-size: 0.9rem; }
</style>
