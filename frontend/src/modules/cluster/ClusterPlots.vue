<!--
  Cluster workspace below the image table — the ONE cluster canvas, reused for cell clustering
  (Cluster cells, popType="clust") and track clustering (Cluster tracks, popType="trackclust"). Built
  on the shared canvas shell (useCanvasPanels + CanvasPanel; docs/UI.md), like gating/summary.

  Plots come in two families, both added from the one "+ Plot" picker and rendered GENERICALLY from
  their registry — the SAME mechanism the Analysis board uses (see LayoutCanvas), so there is one way
  to host a plot, not one per surface:
   • INTERACTIVE (registry `INTERACTIVE_VIEWS`, e.g. UMAP) → generic InteractivePanel.
   • CLUSTER PANELS (registry `CLUSTER_PANELS`, e.g. heatmap, HMM behaviour) → generic <component :is>.
  Adding a cluster plot = a new component (to the contract) + one registry line; no change here.

  Page-level: the clustering-run (suffix) dropdown — you view one run at a time, like picking a
  segmentation. Clustering is set-scope, so plots pool across the selected images (shared UMAP space +
  cluster numbering; the heatmap pools via setUid).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useProjectStore } from '../../stores/project'
import { useGatingStore } from '../../stores/gating'
import { useCanvasPanels, type CanvasItem } from '../../composables/useCanvasPanels'
import { useViewState } from '../../composables/useViewState'
import { useClusterContext } from '../../composables/useClusterContext'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import { INTERACTIVE_VIEWS, isInteractiveView } from '../../components/canvas/interactiveViews'
import { CLUSTER_PANELS, isClusterPanel } from './clusterPanels'
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
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

const canvasRef = ref<HTMLElement | null>(null)
// NB: no `features: []` default — leave it undefined so the heatmap panel self-seeds its features
// from the run (its seed watch only fires when `features === undefined`, to avoid clobbering a
// deliberate empty pick). Seeding `[]` here silently blocked that → heatmap never rendered on the page.
const { panels, activeId, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<ClusterPanelState>(canvasRef, () => ({ kind: 'umap', labels: true, hl: [] }),
    `clust:${props.popType}`)
const activePanel = computed(() => panels.value.find(p => p.id === activeId.value) ?? null)

// migrate persisted panel kinds to the CLUSTER_PANELS registry keys (legacy hyphenated → camelCase),
// so old canvases keep working now that the page renders panels generically from the registry.
const KIND_ALIASES: Record<string, string> = { 'hmm-states': 'hmmStates', 'hmm-transitions': 'hmmTransitions' }
for (const p of panels.value) { const a = KIND_ALIASES[p.state.kind]; if (a) p.state.kind = a }

// suffix is PAGE-LEVEL (you view one clustering run at a time, like picking a segmentation).
// Highlighting the eye shows a pop on the UMAP (its colour, other clusters greyed) + breaks the
// heatmap out per-population. `scope` mirrors gating: GLOBAL = one highlight set for every plot;
// LOCAL = each plot (panel) has its own (state.hl). All persisted per canvas via the shared bag.
const { suffix, highlighted, scope, vis: gVis } = useViewState(shared, {
  suffix: 'default', highlighted: [] as string[], scope: 'global' as 'global' | 'local',
  vis: defaultVis() as VisProps })

// run list + per-run features/cluster metadata + valid-image resolution + the gating-store drive +
// highlight→shownPops resolution (shared with the Analysis board via useClusterContext).
const {
  suffixes, nameMap, clusterIds, featureOptions, hmmStateCols, hmmTransitionCols,
  runMembers, validUids, strayUids, missingUids, shownPopsFor,
} = useClusterContext({
  projectUid, imageUids: computed(() => props.imageUids),
  popType: computed(() => props.popType), suffix,
})

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

// drop stale highlights (global + each panel's local) as pops are deleted/renamed
watch(() => g.flat.map(p => p.path).join('\n'), () => {
  const exist = new Set(g.flat.map(p => p.path))
  highlighted.value = highlighted.value.filter(p => exist.has(p))
  for (const p of panels.value) if (p.state.hl) p.state.hl = p.state.hl.filter(x => exist.has(x))
})

// plot types in the "+ Plot" picker, discovered from the SAME two registries the Analysis board uses
// (no per-plot wiring): cluster-page interactive views (UMAP) + the CLUSTER_PANELS (heatmap, and — for
// track clustering with the right obs columns — the HMM behaviour plots).
const plotTypes = computed(() => [
  // only cluster-page interactive views (UMAP) — gatingStrategy/filmstrip are Analysis-board-only
  ...Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => v.clusterPage).map(([kind, v]) => ({ kind, label: v.label })),
  ...Object.entries(CLUSTER_PANELS).filter(([, def]) => {
    if (def.trackOnly && props.popType !== 'trackclust') return false
    if (def.needsCols === 'hmmState' && !hmmStateCols.value.length) return false
    if (def.needsCols === 'hmmTransition' && !hmmTransitionCols.value.length) return false
    return true
  }).map(([kind, def]) => ({ kind, label: def.label })),
])
function addKind(kind: string) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state.kind = kind
  activeId.value = id
}

const nameOf = (uid: string) =>
  project.activeSet()?.images.find(i => i.uid === uid)?.name ?? uid

// props for a cluster PANEL (CLUSTER_PANELS): the common bag + the registry entry's panel-specific
// props mapped from the shared cluster context — so the page renders every cluster panel with one
// generic <component v-bind>, exactly like the Analysis board (LayoutCanvas.clusterPanelProps). Panels
// self-seed their own defaults (e.g. heatmap features), so no host-side seeding is needed.
function clusterPanelProps(p: CanvasItem<ClusterPanelState>) {
  const ctx = { featureOptions: featureOptions.value, nameMap: nameMap.value,
                hmmStateCols: hmmStateCols.value, hmmTransitionCols: hmmTransitionCols.value }
  return {
    projectUid: projectUid.value, setUid: setUid.value, imageUids: validUids.value,
    popType: props.popType, suffix: suffix.value,
    shownPops: shownPopsFor(panelHL(p.state)), vis: panelVis(p.state), state: p.state,
    ...(CLUSTER_PANELS[p.state.kind].props?.(ctx) ?? {}),
  }
}

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
          <!-- cluster panels (heatmap / HMM behaviour) → GENERIC render from the CLUSTER_PANELS
               registry — the SAME mechanism the Analysis board uses (LayoutCanvas). Adding a cluster
               plot is one registry line; no per-plot branch here. -->
          <component v-else-if="isClusterPanel(p.state.kind)" :is="CLUSTER_PANELS[p.state.kind].component"
                            :index="i" :arrange="p.arrange" :active="p.id === activeId"
                            v-bind="clusterPanelProps(p)" :persist-key="`clust:${popType}:${p.id}`"
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
