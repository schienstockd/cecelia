<!--
  Optical-flow workspace below the image table — the module page's own plot canvas, built on the
  shared shell (useFloatingCanvas + FloatingCanvasHost + InteractivePanel), exactly like the cluster and
  summary canvases. This is what "the module page convention" means: the plots live in ModuleLayout's
  `#plots` slot, so they collapse, persist and zoom the same way as every other page's.

  Plots come from the ONE interactive registry, filtered on this page's `opticalFlowPage` flag — the
  same mechanism the cluster page and the Analysis board use. Adding a flow plot is a component + one
  registry line; nothing here changes.

  Scope is per-IMAGE (a flow model is inspected on one movie at one timepoint), so the canvas key
  carries the first selected image and the panels rebind when the selection moves.

  The model vault rides along on the canvas and is toggled from the bar, exactly like the population
  manager on the cluster/gating canvases — same `CanvasSidePanel` chrome, same "outside the zoom
  layer so it stays full-size" placement. It is deliberately NOT a top-level `FloatingPanel`: that
  would put a canvas-scoped manager in the app's window layer, competing with the Viewer and the Lab
  log for the same corner.

  And it drives the plots the way the pop manager does, down to the SCOPE: the picked model is the
  canvas's under `global`, the active plot's under `local`. Same bag, same footer control, same
  meaning — a module page should not need re-learning because the thing being picked is a model
  rather than a population.
-->
<script setup lang="ts">
import { scopeSetter } from '../../utils/scopeSwitch'
import { computed, watch } from 'vue'
import CanvasArrangeButtons from '../../components/canvas/CanvasArrangeButtons.vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useFloatingCanvas } from '../../composables/useFloatingCanvas'
import FloatingCanvasHost from '../../components/canvas/FloatingCanvasHost.vue'
import { useViewState } from '../../composables/useViewState'
import CanvasZoomControl from '../../components/canvas/CanvasZoomControl.vue'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import ModelVault from './ModelVault.vue'
import { INTERACTIVE_VIEWS, isInteractiveView, pageViews } from '../../components/canvas/interactiveViews'
import PlotPicker from '../../components/canvas/PlotPicker.vue'
import { defaultVis } from '../../plots/plot'
import type { VaultKind } from '../../utils/modelVaultKinds'

// `model` is the panel's LOCAL pick, used only when the vault's scope is 'local' (mirrors the
// cluster canvas's per-panel `hl`).
interface FlowPanelState { [key: string]: unknown; kind: string; model?: string }

const props = defineProps<{ imageUids: string[] }>()
const project = useProjectStore()
const meta = useProjectMetaStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

// Per-image, like the summary/gating canvases (`flow:` is registered in the canvasPanels store's
// MODULE_PREFIXES, so the panels persist with the image at 1/{uid}/moduleCanvases.json).
const ckey = computed(() => `flow:model:${props.imageUids[0] ?? 'none'}`)
// Workspace shell + zoomable panels + zoom provided to the plots — the same wiring as the cluster,
// gating and summary canvases (useFloatingCanvas + FloatingCanvasHost).
const {
  bindCanvas, bindZoom, workspaceStyle, hostStyle,
  panels, activeId, shared, add, duplicate, remove, removeAll, arrangeGrid, arrangeCascade,
  zoom, fitWidth, fitHeight, setZoom, resetZoom,
} = useFloatingCanvas<FlowPanelState>(ckey, () => ({ kind: 'flowMetrics' }))

// persisted per canvas (a bare ref() would reset on navigation — docs/UI.md → Persisting view state).
// `model` + `scope` are the vault's selection, in exactly the shape the population manager's
// highlight set uses: GLOBAL = one pick for every plot, LOCAL = the active plot's own.
// `vaultKind` is which vault is being browsed — persisted for the same reason: a user who trains a
// denoise model, flips the chip, then navigates away expects the chip on the training convergence
// readout still on `denoiseModels` when they come back.
const { showManager, scope, model, tileCols, vaultKind } = useViewState(shared, {
  showManager: true, scope: 'global' as 'global' | 'local', model: '',
  vaultKind: 'flowModels' as VaultKind,
  // Tile Columns knob (0 = Auto) — persisted per canvas; see CanvasArrangeButtons
  tileCols: 0 })

// migrate persisted panel kinds to the current registry keys, like ClusterPlots does — a restored
// canvas holding a renamed kind renders nothing at all, silently.
const KIND_ALIASES: Record<string, string> = { flowModel: 'flowMetrics' }
for (const p of panels.value) { const a = KIND_ALIASES[p.state.kind]; if (a) p.state.kind = a }


const activePanel = computed(() => panels.value.find(p => p.id === activeId.value) ?? null)
// the model a given panel is showing, and the one the vault edits — the two halves of the scope
const panelModel = (st: FlowPanelState) =>
  scope.value === 'global' ? model.value : (st.model ?? model.value)
const activeModel = computed(() =>
  scope.value === 'global' ? model.value : (activePanel.value?.state.model ?? model.value))
// global → local keeps what's drawn: every plot's own model starts as the global one
const setScope = scopeSetter(scope, () => { for (const p of panels.value) p.state.model = model.value })
function setModel(v: string) {
  if (scope.value === 'global') model.value = v
  else if (activePanel.value) activePanel.value.state.model = v
}

const plotTypes = computed(() => pageViews('opticalFlowPage').map(v => ({ ...v, value: v.key })))
function addKind(kind: string) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state = { ...(INTERACTIVE_VIEWS[kind]?.initialState?.() ?? {}), kind }
  activeId.value = id
}
function duplicatePanel(srcId: number, s: FlowPanelState) {
  const id = duplicate(srcId)   // same size as the source, offset
  const p = panels.value.find(x => x.id === id)
  if (p) p.state = JSON.parse(JSON.stringify(s))
  activeId.value = id
}

// the standard plot bag every interactive view receives (docs/UI.md → generic plot-integration
// interface). Per PANEL, not one shared object, so `local` scope can show a different model in each.
// The view picks its own image out of `imageUids`.
const ctxFor = (st: FlowPanelState) => ({
  projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value,
  model: panelModel(st), vis: defaultVis(),
})

// Seed one flow-model plot for an image that has no canvas yet — on first bind AND after the
// selection moves (the reactive key rebinds without remounting). Only when EMPTY, so a restored
// canvas is left alone and remounts don't stack duplicates.
watch(ckey, () => { if (panels.value.length === 0) addKind('flowMetrics') }, { immediate: true })
</script>

<template>
  <div class="flow-plots">
    <div v-if="!imageUids.length" class="cc-empty-inline cc-empty-pane">Select an image above to inspect a flow model</div>
    <template v-else>
      <div class="cc-row cc-row-loose fp-bar">
        <PlotPicker :options="plotTypes" @pick="addKind" />
        <CanvasArrangeButtons :count="panels.length" :cols="tileCols"
                              @update:cols="tileCols = $event"
                              @tile="arrangeGrid(tileCols)" @cascade="arrangeCascade"
                              @close-all="removeAll" />
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': showManager }"
                  @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the model vault' : 'Show the model vault'">
            <i class="pi pi-database" />
          </button>
        </div>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth"
                           @fit-height="fitHeight" @reset="resetZoom" />
        <span class="fp-hint cc-muted cc-fs-xs">drag plots by their title · resize from the corner</span>
      </div>

      <FloatingCanvasHost :bind-canvas="bindCanvas" :bind-zoom="bindZoom" :workspace-style="workspaceStyle" :host-style="hostStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="ctxFor(p.state)" :state="p.state" :duplicable="true"
                            :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)"
                            @duplicate="duplicatePanel(p.id, p.state)" />
        </template>
        <!-- outside the zoom layer, like the population manager: the vault stays full-size -->
        <template #overlay>
          <ModelVault v-if="showManager" :selected="activeModel" :scope="scope" :kind="vaultKind"
                      @update:selected="setModel" @update:scope="setScope"
                      @update:kind="vaultKind = $event" />
        </template>
      </FloatingCanvasHost>
    </template>
  </div>
</template>

<style scoped>
.flow-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
/* a `.cc-row`, so it wraps on a narrow window instead of running off the right edge (as Gate's) */
.fp-bar { padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }
.fp-hint { opacity: 0.7; white-space: nowrap; }
</style>
