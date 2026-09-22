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
import { toggleSelected } from '../../utils/selection'
import { ref, computed, watch } from 'vue'
import CanvasArrangeButtons from '../../components/canvas/CanvasArrangeButtons.vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import CanvasZoomControl from '../../components/canvas/CanvasZoomControl.vue'
import { useProjectStore } from '../../stores/project'
import { useGatingStore } from '../../stores/gating'
import type { CanvasItem } from '../../composables/useCanvasPanels'
import { useFloatingCanvas } from '../../composables/useFloatingCanvas'
import FloatingCanvasHost from '../../components/canvas/FloatingCanvasHost.vue'
import { useViewState } from '../../composables/useViewState'
import { useClusterContext } from '../../composables/useClusterContext'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import { isInteractiveView, pageViews } from '../../components/canvas/interactiveViews'
import { CLUSTER_PANELS, isClusterPanel } from './clusterPanels'
import PopulationManager from '../../components/canvas/PopulationManager.vue'
import { defaultVis, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import CanvasSelectionOverlay from '../../components/canvas/CanvasSelectionOverlay.vue'
import FrameAnnotator from '../../components/FrameAnnotator.vue'
import CaptureViewSurface from '../../components/CaptureViewSurface.vue'
import { useCaptureReshowStore } from '../../stores/captureReshow'
import type { CaptureAddress } from '../../utils/captureAddress'
import type { CaptureEnvelope } from '../../utils/kiwiCaptures'
import { useCanvasShare, type SharePanelExported } from '../../composables/useCanvasShare'

// index signature so a panel's state is assignable to the generic InteractivePanel's
// `Record<string, unknown>` state (views read their own keys: umap→labels, heatmap→features).
// `hl` = this panel's LOCAL highlighted pops (used when scope==='local'; see GatingPlots).
// `vis` = this panel's LOCAL plot styling (used when scope==='local'; else the canvas-global vis).
interface ClusterPanelState { [key: string]: unknown; kind: string; features?: string[]; labels?: boolean; hl?: string[]; measure?: string; vis?: VisProps }

const props = defineProps<{
  imageUids: string[]; popType: 'clust' | 'trackclust' | 'region'
  selectUids?: (uids: string[]) => void   // drive the image selection (from ModuleLayout)
}>()
const meta = useProjectMetaStore()
const project = useProjectStore()
const g = useGatingStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

// Clustering is SET-scope (plots pool across the set's images), so persist per-set — rebinds when the
// active set changes. (Gating/summary key per-image; cluster per-set is the "where it makes sense".)
const ckey = computed(() => `clust:${props.popType}:${setUid.value ?? 'none'}`)
// Workspace shell + zoomable panels + zoom provided to plot components inside. See
// useFloatingCanvas — the wiring that used to be four hand-rolled statements here (and identically
// in SummaryCanvas + GatingPlots).
// NB: no `features: []` default — leave it undefined so the heatmap panel self-seeds its features
// from the run (its seed watch only fires when `features === undefined`, to avoid clobbering a
// deliberate empty pick). Seeding `[]` here silently blocked that → heatmap never rendered on the page.
const {
  bindCanvas, bindZoom, workspaceStyle,
  panels, activeId, activePanel, shared,
  add, remove, removeAll, arrangeGrid, arrangeCascade,
  zoom, fitWidth, fitHeight, setZoom, resetZoom,
} = useFloatingCanvas<ClusterPanelState>(
  ckey,
  () => ({ kind: 'umap', labels: true, hl: [] }),
)

// migrate persisted panel kinds to the CLUSTER_PANELS registry keys (legacy hyphenated → camelCase),
// so old canvases keep working now that the page renders panels generically from the registry.
const KIND_ALIASES: Record<string, string> = { 'hmm-states': 'hmmStates', 'hmm-transitions': 'hmmTransitions' }
for (const p of panels.value) { const a = KIND_ALIASES[p.state.kind]; if (a) p.state.kind = a }

// suffix is PAGE-LEVEL (you view one clustering run at a time, like picking a segmentation).
// Highlighting the eye shows a pop on the UMAP (its colour, other clusters greyed) + breaks the
// heatmap out per-population. `scope` mirrors gating: GLOBAL = one highlight set for every plot;
// LOCAL = each plot (panel) has its own (state.hl). All persisted per canvas via the shared bag.
const { suffix, highlighted, scope, vis: gVis, showManager, tileCols } = useViewState(shared, {
  suffix: 'default', highlighted: [] as string[], scope: 'global' as 'global' | 'local',
  vis: defaultVis() as VisProps, showManager: true,
  // Tile Columns knob (0 = Auto) — persisted per canvas; see CanvasArrangeButtons
  tileCols: 0 })

// run list + per-run features/cluster metadata + valid-image resolution + the gating-store drive +
// highlight→shownPops resolution (shared with the Analysis board via useClusterContext).
const {
  suffixes, labelMap, clusterIds, featureOptions, hmmStateCols, hmmTransitionCols,
  runMembers, validUids, strayUids, missingUids, shownPopsFor,
} = useClusterContext({
  projectUid, imageUids: computed(() => props.imageUids),
  popType: computed(() => props.popType), suffix,
})

// the ONE selection toggle (utils/selection.ts) — four hosts had a copy of it each
const toggle = (arr: string[], v: string) => toggleSelected(arr, v)
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
const panelVis = (s: ClusterPanelState) => scope.value === 'global' ? gVis.value : (s.vis ?? DEFAULT_VIS)
const activeVis = computed(() =>
  scope.value === 'global' ? gVis.value : (activePanel.value?.state.vis ?? DEFAULT_VIS))
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
  ...pageViews('clusterPage').map(v => ({ kind: v.key, label: v.label })),
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
  const ctx = { featureOptions: featureOptions.value, nameMap: labelMap.value,
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

// Seed a UMAP + a heatmap for any set that has none yet — on first bind AND after a set switch (the
// reactive per-set key rebinds to a fresh entry; the component doesn't remount). Restored canvases
// come back non-empty, so they're left as-is; persisted per set, so no stacking on remount.
watch(ckey, () => { if (panels.value.length === 0) { addKind('umap'); addKind('heatmap') } }, { immediate: true })

// ── Canvas Share (Kiwi's canvas Share button) ──────────────────────────────
// Same shape as SummaryCanvas — the composable owns the Phase 1→2 machinery + POST + toast; this
// host provides the cluster-panel envelope shape (kind + full state on plotRef.ui) + reshow.
// The `module` tag on the address is `clusterPage:{popType}` so a Kiwi refocus routes back to the
// right page (cluster cells vs cluster tracks).
const panelsSnapshot = panels
const reshown = ref<CaptureEnvelope | null>(null)
const reshowStore = useCaptureReshowStore()
const moduleTag = computed(() => `clusterPage:${props.popType}`)
watch(() => reshowStore.pending, () => {
  const env = reshowStore.consumeFor(moduleTag.value)
  if (env) reshown.value = env
}, { immediate: true })
const share = useCanvasShare({
  projectUid: () => projectUid.value,
  canvasKey: () => ckey.value,
  panels: () => panels.value,
  label: () => `cluster · ${props.popType === 'trackclust' ? 'tracks' : props.popType} · plot canvas`,
  buildPlotSpec: ({ panelCount }) => ({ specId: 'cluster-multi-panel',
    params: { popType: props.popType, suffix: suffix.value, panelCount } }),
  buildEnvelope: ({ selected, workspaceOrigin }) => {
    // Cluster panels carry their full state (kind + panel-specific bag: features / hl / vis / measure)
    // — mirror SummaryCanvas's plotRef.ui shape so a zoom-to-source can rehydrate the panel exactly.
    const { x: x0, y: y0 } = workspaceOrigin
    return selected.map((e: SharePanelExported) => {
      const panel = panelsSnapshot.value.find(pp => pp.id === e.id)
      const st = panel?.state
      const specId = st?.kind ? String(st.kind) : ''
      const plotRef: Record<string, unknown> = { specId }
      if (st) plotRef.ui = { ...st }
      return {
        panelId: String(e.id),
        position: { x: e.geom.x - x0, y: e.geom.y - y0, w: e.geom.w, h: e.geom.h },
        plotRef,
        dataSlice: {
          imageUids: validUids.value,
          setUid: setUid.value ?? null,
          popType: props.popType,
          suffix: suffix.value,
          shownPops: st ? shownPopsFor(panelHL(st)) : [],
        },
      }
    })
  },
  onSaveSuccess: (env) => { reshown.value = env },
})
const { shareSel, sharePanelHits, pendingShare, shareBusy, shareToast,
        onShareCancel, onShareConfirm, onAnnotateCancel, onAnnotateSave, dismissShareToast } = share
watch(() => shareSel.active.value, on => { if (on) reshown.value = null })

// Reshow surface bindings — Kiwi refocus / Blackboard click mounts CaptureViewSurface over the
// canvas. Cluster-page zoom-to-source is a follow-up: the panel state shape is on the wire
// (`plotRef.ui`) but the cluster canvas has no `restorePanelsFromCapture` equivalent yet
// (SummaryCanvas has `reresolvePops` for its per-pop rebinding; the cluster page's `hl` array
// carries paths that don't need the uid two-pass). For now the reshow surface shows the frozen
// composite + marks; zoom-to-source is disabled.
const reshownAddress = computed<CaptureAddress>(() => {
  const a = reshown.value?.address as CaptureAddress | null | undefined
  return a ?? { projectUid: projectUid.value }
})
const reshownAddressLine = computed(() => {
  const panelsN = (reshown.value?.panels?.length) ?? 0
  return panelsN > 0 ? `cluster · ${props.popType} · ${panelsN} panels`
                     : `cluster · ${props.popType} · plot canvas`
})
function onReshowClose() { reshown.value = null }
function onReshowReannotate(payload: { captureId: string; frameDataUrl: string; overlay: unknown; notes: string }) {
  if (!reshown.value) return
  reshown.value = { ...reshown.value,
    captureId: payload.captureId, frame: payload.frameDataUrl,
    overlay: payload.overlay as CaptureEnvelope['overlay'], notes: payload.notes }
}
</script>

<template>
  <div class="cluster-plots">
    <div v-if="!imageUids.length" class="cp-empty cc-empty-inline">Select clustered image(s) above to explore clusters.</div>
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
        <CanvasArrangeButtons :count="panels.length" :cols="tileCols"
                              @update:cols="tileCols = $event"
                              @tile="arrangeGrid(tileCols)" @cascade="arrangeCascade"
                              @close-all="removeAll" />
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" data-guide="cluster.popManager"
                  :class="{ 'cc-btn-on cc-btn-on-tint': showManager }"
                  @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the population manager' : 'Show the population manager'">
            <i class="pi pi-sitemap" />
          </button>
        </div>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
        <span class="cp-hint cc-muted cc-fs-xs">drag plots by their title · resize from the corner</span>
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

      <FloatingCanvasHost :bind-canvas="bindCanvas" :bind-zoom="bindZoom" :workspace-style="workspaceStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <!-- interactive (UMAP, …) → generic InteractivePanel -->
          <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="ctxFor(p.state)" :state="p.state" :duplicable="true"
                            :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
          <!-- cluster panels (heatmap / HMM behaviour) → GENERIC render from the CLUSTER_PANELS
               registry — the SAME mechanism the Analysis board uses (LayoutCanvas). Adding a cluster
               plot is one registry line; no per-plot branch here. -->
          <component v-else-if="isClusterPanel(p.state.kind)" :is="CLUSTER_PANELS[p.state.kind].component"
                            :index="i" :arrange="p.arrange" :active="p.id === activeId"
                            v-bind="clusterPanelProps(p)" :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" @duplicate="duplicatePanel(p.state)" />
        </template>
        <!-- Share mode: dim veil + drag/click selection + toolbar. Same shape as SummaryCanvas. -->
        <CanvasSelectionOverlay v-if="shareSel.active.value"
                                :panels="sharePanelHits"
                                :selection="shareSel"
                                :address-line="`cluster · ${popType === 'trackclust' ? 'tracks' : popType} · plot canvas`"
                                @cancel="onShareCancel" @share="onShareConfirm" />
        <!-- Annotate (Phase 2): frozen composite + DrawSurface. Save fires the POST. -->
        <FrameAnnotator v-if="pendingShare && !reshown"
                        :frame-data-url="pendingShare.composite"
                        :address-line="`cluster · ${popType === 'trackclust' ? 'tracks' : popType} · ${pendingShare.panels.length} panels`"
                        :busy="shareBusy"
                        @save="onAnnotateSave" @cancel="onAnnotateCancel" />
        <!-- Reshow (Kiwi refocus / Blackboard click). Zoom-to-source disabled — cluster canvas has
             no restore path yet (SummaryCanvas has `reresolvePops`; the cluster page's `hl` array
             carries paths without the uid two-pass); the frame + marks still show. -->
        <CaptureViewSurface v-if="reshown && !pendingShare"
                            :project-uid="projectUid"
                            :capture-id="reshown.captureId"
                            :frame-data-url="reshown.frame"
                            :overlay="reshown.overlay"
                            :notes="reshown.notes"
                            :address="reshownAddress"
                            :address-line="reshownAddressLine"
                            surface="plot"
                            :extra-post-fields="reshown.panels ? { panels: reshown.panels } : {}"
                            :show-zoom-to-source="false"
                            @close="onReshowClose"
                            @reannotate="onReshowReannotate" />
        <!-- Absolute-positioned CanvasSidePanel needs a positioned ancestor, so the manager goes
             in the host's `overlay` slot (inside `.floating-canvas`, outside the zoom transform)
             — same pattern as GatingPlots' rail and SummaryCanvas's share toast. -->
        <template #overlay>
          <div v-if="shareToast" class="cp-share-chip"
               :class="{ 'cp-share-chip-error': shareToast.kind === 'fail' }">
            <i :class="['pi', shareToast.kind === 'ok' ? 'pi-clipboard' : 'pi-exclamation-triangle',
                        'cp-share-chip-icon']" />
            <span>{{ shareToast.message }}</span>
            <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro cp-share-chip-dismiss"
                    @click="dismissShareToast" v-tooltip.top="'Dismiss'"
                    aria-label="Dismiss share notice"><i class="pi pi-times" /></button>
          </div>
          <PopulationManager v-if="showManager && validUids.length" :selected="selectedPop" :highlighted="activeHL" :scope="scope"
                             :line-width="1" :gate-labels="false" :axis-from-zero="false"
                             :pop-type="popType" :cluster-ids="clusterIds[suffix] ?? []" :suffix="suffix"
                             :vis="activeVis"
                             @update:selected="selectedPop = $event" @update:scope="scope = $event"
                             @update:vis="setVis" @toggle-highlight="toggleHighlight" />
        </template>
      </FloatingCanvasHost>
    </template>
  </div>
</template>

<style scoped>
.cluster-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.cp-empty { padding: 20px; }   /* + .cc-empty-inline (row/colour) */
.cp-bar { display: flex; align-items: center; gap: 14px; padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }
.cp-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.cp-bar select { min-width: 7rem; }
.cp-hint { opacity: 0.7; }
.cp-members { display: flex; align-items: flex-start; gap: 6px; margin: 0 4px 6px; padding: 6px 9px;
  font-size: var(--cc-fs-xs); color: #fcd34d; background: #78350f22; border: 1px solid #b4530933; border-radius: var(--cc-radius-sm); }
.cp-members .pi { margin-top: 1px; }
.cp-fix { flex-shrink: 0; margin-left: auto; align-self: center; font-size: var(--cc-fs-xs); padding: 3px 9px;
  border: 1px solid #b45309; border-radius: var(--cc-radius-xs); background: #78350f44; color: #fcd34d; cursor: pointer; white-space: nowrap; }
.cp-fix:hover { background: #78350f88; }
.cp-add { padding: 4px 8px; }

/* Share-outcome chip — mirrors `.sc-share-chip` in SummaryCanvas so the two surfaces present the
   same look on the same push branch. Bottom-left, above CaptureViewSurface. */
.cp-share-chip {
  position: absolute; left: 0.75rem; bottom: 0.75rem; z-index: 50;
  padding: 0.3rem 0.55rem; border-radius: var(--cc-radius-xs);
  background: rgba(0, 0, 0, 0.78); color: #fff;
  font-size: var(--cc-fs-xs); pointer-events: auto;
  display: inline-flex; align-items: center; gap: 0.35rem; max-width: calc(100% - 1.5rem);
}
.cp-share-chip-error { color: var(--cc-sev-fail); }
.cp-share-chip-icon { font-size: 1em; }
.cp-share-chip-dismiss { margin-left: 0.15rem; color: #fff; opacity: 0.8; }
.cp-share-chip-dismiss:hover { opacity: 1; }
</style>
