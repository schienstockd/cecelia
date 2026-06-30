<!--
  Cluster workspace below the image table — the ONE cluster canvas, reused for cell clustering
  (Cluster cells, popType="clust") and track clustering (Cluster tracks, popType="trackclust"). Built
  on the shared canvas shell (useCanvasPanels + CanvasPanel; docs/UI.md), like gating/summary.

  Plots come in two families, both added from the one "+ Plot" picker and routed by family:
   • INTERACTIVE (registry `INTERACTIVE_VIEWS`, e.g. UMAP) → generic InteractivePanel. Adding another
     interactive plot = a new view component + one registry line; no change here.
   • SUMMARY (server-aggregated, PlotChart) → here the cluster heatmap (ClusterHeatmapPanel).

  Page-level: the clustering-run (suffix) dropdown — you view one run at a time, like picking a
  segmentation. Clustering is set-scope, so plots pool across the selected images (shared UMAP space +
  cluster numbering; the heatmap pools via setUid).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useProjectStore } from '../../stores/project'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useViewState } from '../../composables/useViewState'
import { useLogStore } from '../../stores/log'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import { INTERACTIVE_VIEWS, isInteractiveView } from '../../components/canvas/interactiveViews'
import ClusterHeatmapPanel from './ClusterHeatmapPanel.vue'

// index signature so a panel's state is assignable to the generic InteractivePanel's
// `Record<string, unknown>` state (views read their own keys: umap→labels, heatmap→features)
interface ClusterPanelState { [key: string]: unknown; kind: string; features?: string[]; labels?: boolean }

const props = defineProps<{ imageUids: string[]; popType: 'clust' | 'trackclust' }>()
const meta = useProjectMetaStore()
const project = useProjectStore()
const log = useLogStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

const canvasRef = ref<HTMLElement | null>(null)
const { panels, activeId, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<ClusterPanelState>(canvasRef, () => ({ kind: 'umap', features: [], labels: true }),
    `clust:${props.popType}`)

// suffix is PAGE-LEVEL (you view one clustering run at a time, like picking a segmentation) — a
// dropdown of the discovered clusters.{suffix} columns, persisted per canvas via the shared bag.
const { suffix } = useViewState(shared, { suffix: 'default' })
const suffixes = ref<string[]>([])

// plot types in the "+ Plot" picker: every registered interactive view + the summary heatmap.
const plotTypes = computed(() => [
  ...Object.entries(INTERACTIVE_VIEWS).map(([kind, v]) => ({ kind, label: v.label })),
  { kind: 'heatmap', label: 'Heatmap' },
])
function addKind(kind: string) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state.kind = kind
  activeId.value = id
}

// per-suffix feature list each run actually used (clusters' interpretive columns), from the channels
// endpoint's clusterFeatures sidecar; fall back to markers/motility if a run predates feature
// tracking. RAW column names — the matrix selects by raw name and we relabel channels via nameMap.
const clusterFeatures = ref<Record<string, string[]>>({})
const featureFallback = ref<string[]>([])
const nameMap = ref<Record<string, string>>({})   // raw channel column → display name
const featureOptions = computed<string[]>(() => clusterFeatures.value[suffix.value] ?? featureFallback.value)

async function loadFeatures() {
  if (!projectUid.value || !props.imageUids[0]) { clusterFeatures.value = {}; featureFallback.value = []; return }
  const q = new URLSearchParams({ projectUid: projectUid.value, imageUid: props.imageUids[0] })
  if (props.popType === 'trackclust') q.set('popType', 'track')
  try {
    const r = await fetch(`/api/gating/channels?${q}`)
    if (!r.ok) { clusterFeatures.value = {}; featureFallback.value = []; return }
    const d = await r.json()
    clusterFeatures.value = d.clusterFeatures ?? {}
    featureFallback.value = props.popType === 'trackclust' ? (d.columns ?? []) : (d.channels ?? [])
    // raw channel column → display name (matrix aggregates by raw; heatmap relabels for display)
    const chans: string[] = d.channels ?? [], names: string[] = d.channelNames ?? []
    nameMap.value = Object.fromEntries(chans.map((c, i) => [c, names[i] ?? c]))
    // discovered cluster runs → page-level suffix dropdown; self-heal if the stored one is gone
    suffixes.value = d.clusterSuffixes ?? []
    if (suffixes.value.length && !suffixes.value.includes(suffix.value)) suffix.value = suffixes.value[0]
  } catch (e) {
    log.error(`Cluster features: ${e instanceof Error ? e.message : String(e)}`, { source: 'cluster' })
    clusterFeatures.value = {}; featureFallback.value = []
  }
}

// default a heatmap panel's features to the run's full feature set once known (don't clobber a pick)
watch([featureOptions, () => panels.value.length], () => {
  if (!featureOptions.value.length) return
  for (const p of panels.value) if (p.state.kind === 'heatmap' && !(p.state.features?.length)) p.state.features = [...featureOptions.value]
})
watch([projectUid, () => props.imageUids.join(','), () => props.popType], loadFeatures)

// the generic context every interactive view receives (project/images/popType/suffix)
const viewContext = computed(() => ({
  projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value,
  popType: props.popType, suffix: suffix.value,
}))

onMounted(() => {
  loadFeatures()
  // seed a UMAP + a heatmap ONLY when this canvas is empty (panels persist per `clust:${popType}`
  // across navigation — an unconditional add() would stack more on every remount).
  if (panels.value.length === 0) { addKind('umap'); addKind('heatmap') }
})
</script>

<template>
  <div class="cluster-plots">
    <div v-if="!imageUids.length" class="cp-empty">Select clustered image(s) above to explore clusters.</div>
    <template v-else>
      <div class="cp-bar">
        <label>clustering
          <select v-model="suffix" v-tooltip.bottom="'Which clustering run (clusters.&lt;suffix&gt;) to show'">
            <option v-if="!suffixes.length" :value="suffix">{{ suffix }}</option>
            <option v-for="s in suffixes" :key="s" :value="s">{{ s }}</option>
          </select>
        </label>
        <select class="cp-add" v-tooltip.bottom="'Add a plot'"
                @change="addKind(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="" disabled selected>+ Plot…</option>
          <option v-for="t in plotTypes" :key="t.kind" :value="t.kind">{{ t.label }}</option>
        </select>
        <div class="seg" v-tooltip.bottom="'Arrange windows'">
          <button v-tooltip.bottom="'Tile in a grid'" @click="arrangeGrid"><i class="pi pi-th-large" /></button>
          <button v-tooltip.bottom="'Cascade windows'" @click="arrangeCascade"><i class="pi pi-clone" /></button>
        </div>
        <span class="cp-hint">drag plots by their title · resize from the corner</span>
      </div>
      <div ref="canvasRef" class="cp-canvas">
        <template v-for="(p, i) in panels" :key="p.id">
          <!-- interactive (UMAP, …) → generic InteractivePanel -->
          <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="viewContext" :state="p.state"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" />
          <!-- summary (heatmap) -->
          <ClusterHeatmapPanel v-else-if="p.state.kind === 'heatmap'" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :project-uid="projectUid" :set-uid="setUid"
                            :image-uids="imageUids" :pop-type="popType" :suffix="suffix"
                            :feature-options="featureOptions" :name-map="nameMap" :state="p.state"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" />
        </template>
      </div>
    </template>
  </div>
</template>

<style scoped>
.cluster-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.cp-empty { padding: 20px; color: var(--cc-text-dim); }
.cp-bar { display: flex; align-items: center; gap: 14px; padding: 8px 4px; font-size: 12px; flex-shrink: 0; }
.cp-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.cp-bar select { min-width: 7rem; }
.cp-hint { color: var(--cc-text-dim); font-size: 11px; opacity: 0.7; }
.cp-add { font-size: 12px; padding: 4px 8px; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 5px 9px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.cp-canvas { position: relative; flex: 1; min-height: 70vh; }
</style>
