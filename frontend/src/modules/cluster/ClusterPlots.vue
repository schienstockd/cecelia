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
import { useGatingStore } from '../../stores/gating'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useViewState } from '../../composables/useViewState'
import { useLogStore } from '../../stores/log'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import { INTERACTIVE_VIEWS, isInteractiveView } from '../../components/canvas/interactiveViews'
import ClusterHeatmapPanel from './ClusterHeatmapPanel.vue'
import ClusterHmmStatesPanel from './ClusterHmmStatesPanel.vue'
import ClusterHmmTransitionsPanel from './ClusterHmmTransitionsPanel.vue'
import PopulationManager from '../../components/canvas/PopulationManager.vue'
import { defaultVis, type VisProps } from '../../plots/plot'

// index signature so a panel's state is assignable to the generic InteractivePanel's
// `Record<string, unknown>` state (views read their own keys: umap→labels, heatmap→features).
// `hl` = this panel's LOCAL highlighted pops (used when scope==='local'; see GatingPlots).
// `vis` = this panel's LOCAL plot styling (used when scope==='local'; else the canvas-global vis).
interface ClusterPanelState { [key: string]: unknown; kind: string; features?: string[]; labels?: boolean; hl?: string[]; measure?: string; vis?: VisProps }

const props = defineProps<{
  imageUids: string[]; popType: 'clust' | 'trackclust'
  selectUids?: (uids: string[]) => void   // drive the image selection (from ModuleLayout)
}>()
const meta = useProjectMetaStore()
const project = useProjectStore()
const g = useGatingStore()
const log = useLogStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

const canvasRef = ref<HTMLElement | null>(null)
const { panels, activeId, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<ClusterPanelState>(canvasRef, () => ({ kind: 'umap', features: [], labels: true, hl: [] }),
    `clust:${props.popType}`)
const activePanel = computed(() => panels.value.find(p => p.id === activeId.value) ?? null)

// suffix is PAGE-LEVEL (you view one clustering run at a time, like picking a segmentation).
// Highlighting the eye shows a pop on the UMAP (its colour, other clusters greyed) + breaks the
// heatmap out per-population. `scope` mirrors gating: GLOBAL = one highlight set for every plot;
// LOCAL = each plot (panel) has its own (state.hl). All persisted per canvas via the shared bag.
const { suffix, highlighted, scope, vis: gVis } = useViewState(shared, {
  suffix: 'default', highlighted: [] as string[], scope: 'global' as 'global' | 'local',
  vis: defaultVis() as VisProps })
const suffixes = ref<string[]>([])

const toggle = (arr: string[], v: string) => arr.includes(v) ? arr.filter(x => x !== v) : [...arr, v]
function toggleHighlight(path: string) {
  if (scope.value === 'global') highlighted.value = toggle(highlighted.value, path)
  else if (activePanel.value) activePanel.value.state.hl = toggle(activePanel.value.state.hl ?? [], path)
}
// effective highlight set for a panel, and the set the manager shows/edits (the active scope's)
const panelHL = (s: ClusterPanelState) => scope.value === 'global' ? highlighted.value : (s.hl ?? [])
const activeHL = computed(() =>
  scope.value === 'global' ? highlighted.value : (activePanel.value?.state.hl ?? []))

// plot styling (VisProps) follows the SAME global/local scope as the highlights (like the summary
// canvas): GLOBAL = one styling bag for every plot; LOCAL = the active plot's own. The pop manager
// edits the active scope's bag; each panel renders with its own effective bag.
const panelVis = (s: ClusterPanelState) => scope.value === 'global' ? gVis.value : (s.vis ?? defaultVis())
const activeVis = computed(() =>
  scope.value === 'global' ? gVis.value : (activePanel.value?.state.vis ?? defaultVis()))
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activePanel.value) activePanel.value.state.vis = { ...(activePanel.value.state.vis ?? defaultVis()), ...patch }
}

// duplicate a panel: a new panel with a deep copy of the source's state (so the user can tweak one
// thing — measure, styling — without disturbing the original). Mirrors SummaryCanvas.duplicatePanel.
function duplicatePanel(s: ClusterPanelState) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state = { ...s, features: s.features ? [...s.features] : undefined,
                     hl: s.hl ? [...s.hl] : undefined, vis: s.vis ? { ...s.vis } : undefined }
  activeId.value = id
}

// resolve a highlight set to shown populations (colour + owned cluster IDs) against the store tree
const shownPopsFor = (hl: string[]) => g.flat
  .filter(p => hl.includes(p.path))
  .map(p => ({ path: p.path, name: p.name, colour: p.colour,
               clusterIds: Array.isArray(p.filter?.values) ? (p.filter!.values as unknown[]).map(Number) : [] }))

// drop stale highlights (global + each panel's local) as pops are deleted/renamed
watch(() => g.flat.map(p => p.path).join('\n'), () => {
  const exist = new Set(g.flat.map(p => p.path))
  highlighted.value = highlighted.value.filter(p => exist.has(p))
  for (const p of panels.value) if (p.state.hl) p.state.hl = p.state.hl.filter(x => exist.has(x))
})

// plot types in the "+ Plot" picker: every registered interactive view + the summary heatmap, plus
// (track clustering only, when HMM columns exist) the HMM state / transition behaviour plots.
const plotTypes = computed(() => [
  ...Object.entries(INTERACTIVE_VIEWS).map(([kind, v]) => ({ kind, label: v.label })),
  { kind: 'heatmap', label: 'Heatmap' },
  ...(props.popType === 'trackclust' && hmmStateCols.value.length ? [{ kind: 'hmm-states', label: 'HMM states' }] : []),
  ...(props.popType === 'trackclust' && hmmTransitionCols.value.length ? [{ kind: 'hmm-transitions', label: 'HMM transitions' }] : []),
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
// HMM behaviour columns (state / transition frequencies) are categorical-behaviour features — they
// belong in the dedicated HMM plot types, NOT the numeric heatmap (they don't render there). Keep
// them out of the heatmap's feature options.
const HMM_RE = /live\.cell\.hmm\.(state|transitions)\b/
const allFeatures = computed<string[]>(() => clusterFeatures.value[suffix.value] ?? featureFallback.value)
const featureOptions = computed<string[]>(() => allFeatures.value.filter(f => !HMM_RE.test(f)))
// per-suffix: the tickable cluster IDs (universe) and the uIDs the run clustered together (partOf)
const clusterIds = ref<Record<string, number[]>>({})
const clusterMembers = ref<Record<string, string[]>>({})
const resolvedVn = ref('default')                  // segmentation value_name the channels resolved to
// HMM behaviour cell-obs columns → the dedicated HMM plot types (track clustering only).
const hmmStateCols = ref<string[]>([])             // live.cell.hmm.state.<name>
const hmmTransitionCols = ref<string[]>([])        // live.cell.hmm.transitions.<name>

async function loadFeatures() {
  if (!projectUid.value || !props.imageUids[0]) { clusterFeatures.value = {}; featureFallback.value = []; return }
  const q = new URLSearchParams({ projectUid: projectUid.value, imageUid: props.imageUids[0], popType: props.popType })
  try {
    const r = await fetch(`/api/gating/channels?${q}`)
    if (!r.ok) { clusterFeatures.value = {}; featureFallback.value = []; return }
    const d = await r.json()
    clusterFeatures.value = d.clusterFeatures ?? {}
    clusterIds.value = d.clusterIds ?? {}
    clusterMembers.value = d.clusterMembers ?? {}
    resolvedVn.value = d.valueName ?? 'default'
    featureFallback.value = props.popType === 'trackclust' ? (d.columns ?? []) : (d.channels ?? [])
    // raw channel column → display name (matrix aggregates by raw; heatmap relabels for display)
    const chans: string[] = d.channels ?? [], names: string[] = d.channelNames ?? []
    nameMap.value = Object.fromEntries(chans.map((c, i) => [c, names[i] ?? c]))
    // HMM behaviour columns (track branch cellObsMeasures) → the HMM plot types
    const obs: string[] = d.cellObsMeasures ?? []
    hmmStateCols.value = obs.filter(c => c.startsWith('live.cell.hmm.state.'))
    hmmTransitionCols.value = obs.filter(c => c.startsWith('live.cell.hmm.transitions.'))
    // discovered cluster runs → page-level suffix dropdown; self-heal if the stored one is gone
    suffixes.value = d.clusterSuffixes ?? []
    if (suffixes.value.length && !suffixes.value.includes(suffix.value)) suffix.value = suffixes.value[0]
  } catch (e) {
    log.error(`Cluster features: ${e instanceof Error ? e.message : String(e)}`, { source: 'cluster' })
    clusterFeatures.value = {}; featureFallback.value = []
  }
}

// Cluster pops can only be defined for images IN THE RUN (those carrying clusters.{suffix} — the
// recorded `partOf`). Restrict writes to that subset; surface the mismatch so the user can fix the
// selection. If a run predates partOf recording (empty members), don't block — treat all selected.
const runMembers = computed<string[]>(() => clusterMembers.value[suffix.value] ?? [])
const validUids = computed<string[]>(() =>
  runMembers.value.length ? props.imageUids.filter(u => runMembers.value.includes(u)) : props.imageUids)
const strayUids = computed<string[]>(() =>
  runMembers.value.length ? props.imageUids.filter(u => !runMembers.value.includes(u)) : [])
const missingUids = computed<string[]>(() =>
  runMembers.value.filter(u => !props.imageUids.includes(u)))
const nameOf = (uid: string) =>
  project.activeSet()?.images.find(i => i.uid === uid)?.name ?? uid

// drive the (shared, pop_type-agnostic) gating store for the pop tree: primary = first valid image,
// the rest mirror every mutation so cluster pops land set-wide. Re-sync on selection/suffix change.
watch([validUids, suffix, () => props.popType, projectUid], () => {
  if (!projectUid.value || !validUids.value.length) return
  g.selectImage(validUids.value[0], resolvedVn.value, props.popType).then(() => {
    g.mirrorUids = validUids.value.slice(1)
  })
}, { immediate: true })

// default a heatmap panel's features to the run's full feature set once known (don't clobber a pick)
watch([featureOptions, () => panels.value.length], () => {
  if (!featureOptions.value.length) return
  for (const p of panels.value) if (p.state.kind === 'heatmap' && !(p.state.features?.length)) p.state.features = [...featureOptions.value]
})
watch([projectUid, () => props.imageUids.join(','), () => props.popType], loadFeatures)

// the generic context an interactive view receives, per panel (so LOCAL scope can show different
// pops per panel). Plots run on the run-MEMBER images (validUids), not the raw selection — a cluster
// pop only exists on member images, so querying a non-member errors ("pop_membership: not found").
const ctxFor = (s: ClusterPanelState) => ({
  projectUid: projectUid.value, imageUids: validUids.value, setUid: setUid.value,
  popType: props.popType, suffix: suffix.value, shownPops: shownPopsFor(panelHL(s)),
  vis: panelVis(s),   // canvas styling (dark-theme etc.) — interactive views read it if they theme
})

// the population row the manager treats as selected (clicking a row); cluster ticking is per-row,
// so this is just for the highlight affordance — kept local and unused by the cluster plots for now.
const selectedPop = ref('')

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

      <!-- membership: cluster pops only apply to images that were in the run (carry clusters.{suffix}).
           Tell the user when their selection doesn't match, so they can fix it. -->
      <div v-if="strayUids.length || missingUids.length" class="cp-members">
        <i class="pi pi-info-circle" />
        <span>
          Clustering run “{{ suffix }}” covers {{ runMembers.length }} image{{ runMembers.length === 1 ? '' : 's' }}.
          <template v-if="strayUids.length">
            {{ strayUids.length }} selected ({{ strayUids.map(nameOf).join(', ') }})
            {{ strayUids.length === 1 ? 'is' : 'are' }} not in it — pops won’t be written there.
          </template>
          <template v-if="missingUids.length">
            Also in the run but not selected: {{ missingUids.map(nameOf).join(', ') }}.
          </template>
        </span>
        <button v-if="selectUids && runMembers.length" class="cp-fix"
                @click="selectUids(runMembers)"
                v-tooltip.bottom="'Set the image selection to exactly this run’s images'">
          Select clustered images
        </button>
      </div>

      <div ref="canvasRef" class="cp-canvas">
        <PopulationManager v-if="validUids.length" :selected="selectedPop" :highlighted="activeHL" :scope="scope"
                           :line-width="1" :gate-labels="false" :axis-from-zero="false"
                           :pop-type="popType" :cluster-ids="clusterIds[suffix] ?? []" :suffix="suffix"
                           :vis="activeVis"
                           @update:selected="selectedPop = $event" @update:scope="scope = $event"
                           @update:vis="setVis" @toggle-highlight="toggleHighlight" />
        <template v-for="(p, i) in panels" :key="p.id">
          <!-- interactive (UMAP, …) → generic InteractivePanel -->
          <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="ctxFor(p.state)" :state="p.state" :duplicable="true"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
          <!-- summary (heatmap) -->
          <ClusterHeatmapPanel v-else-if="p.state.kind === 'heatmap'" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :project-uid="projectUid" :set-uid="setUid"
                            :image-uids="validUids" :pop-type="popType" :suffix="suffix"
                            :feature-options="featureOptions" :name-map="nameMap" :shown-pops="shownPopsFor(panelHL(p.state))"
                            :vis="panelVis(p.state)" :state="p.state"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
          <!-- HMM behaviour plots (track clustering) -->
          <ClusterHmmStatesPanel v-else-if="p.state.kind === 'hmm-states'" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :project-uid="projectUid" :set-uid="setUid"
                            :image-uids="validUids" :suffix="suffix" :hmm-cols="hmmStateCols"
                            :shown-pops="shownPopsFor(panelHL(p.state))" :vis="panelVis(p.state)" :state="p.state"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
          <ClusterHmmTransitionsPanel v-else-if="p.state.kind === 'hmm-transitions'" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :project-uid="projectUid" :set-uid="setUid"
                            :image-uids="validUids" :suffix="suffix" :hmm-cols="hmmTransitionCols"
                            :shown-pops="shownPopsFor(panelHL(p.state))" :vis="panelVis(p.state)" :state="p.state"
                            :persist-key="`clust:${popType}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
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
.cp-members { display: flex; align-items: flex-start; gap: 6px; margin: 0 4px 6px; padding: 6px 9px;
  font-size: 11px; color: #fcd34d; background: #78350f22; border: 1px solid #b4530933; border-radius: 5px; }
.cp-members .pi { margin-top: 1px; }
.cp-fix { flex-shrink: 0; margin-left: auto; align-self: center; font-size: 11px; padding: 3px 9px;
  border: 1px solid #b45309; border-radius: 4px; background: #78350f44; color: #fcd34d; cursor: pointer; white-space: nowrap; }
.cp-fix:hover { background: #78350f88; }
.cp-add { font-size: 12px; padding: 4px 8px; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 5px 9px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.cp-canvas { position: relative; flex: 1; min-height: 70vh; }
</style>
