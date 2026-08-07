<!--
  Optical-flow workspace below the image table — the module page's own plot canvas, built on the
  shared shell (useCanvasPanels + InteractivePanel + CanvasZoomControl), exactly like the cluster and
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
-->
<script setup lang="ts">
import { ref, computed, watch, provide } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useCanvasWorkspace } from '../../composables/useCanvasWorkspace'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import { useViewState } from '../../composables/useViewState'
import CanvasZoomControl from '../../components/canvas/CanvasZoomControl.vue'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import FlowModelVault from './FlowModelVault.vue'
import { INTERACTIVE_VIEWS, isInteractiveView, pageViews } from '../../components/canvas/interactiveViews'
import { defaultVis } from '../../plots/plot'

interface FlowPanelState { [key: string]: unknown; kind: string }

const props = defineProps<{ imageUids: string[] }>()
const project = useProjectStore()
const meta = useProjectMetaStore()
const projectUid = computed(() => meta.current?.uid ?? '')
const setUid = computed(() => project.activeSetUid)

const canvasRef = ref<HTMLElement | null>(null)   // the visible viewport (zoom + fit measure it)
const zoomRef = ref<HTMLElement | null>(null)     // the scaled workspace (panels' offsetParent)
// Per-image, like the summary/gating canvases (`flow:` is registered in the canvasPanels store's
// MODULE_PREFIXES, so the panels persist with the image at 1/{uid}/moduleCanvases.json).
const ckey = computed(() => `flow:model:${props.imageUids[0] ?? 'none'}`)
const { panels, activeId, shared, add, remove, arrangeGrid, arrangeCascade, contentBounds } =
  useCanvasPanels<FlowPanelState>(zoomRef, () => ({ kind: 'flowMetrics' }), ckey)

// persisted per canvas (a bare ref() would reset on navigation — docs/UI.md → Persisting view state)
const { showVault } = useViewState(shared, { showVault: true })

// migrate persisted panel kinds to the current registry keys, like ClusterPlots does — a restored
// canvas holding a renamed kind renders nothing at all, silently.
const KIND_ALIASES: Record<string, string> = { flowModel: 'flowMetrics' }
for (const p of panels.value) { const a = KIND_ALIASES[p.state.kind]; if (a) p.state.kind = a }

const { zoom, fitWidth, fitHeight, setZoom, reset: resetZoom } = useCanvasZoom(canvasRef,
  () => ({ w: contentBounds.value.w || null, h: contentBounds.value.h }))
provide(CANVAS_ZOOM_KEY, zoom)
const { workspaceStyle } = useCanvasWorkspace(canvasRef, zoom)

const plotTypes = computed(() => pageViews('opticalFlowPage'))
function addKind(kind: string) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state = { ...(INTERACTIVE_VIEWS[kind]?.initialState?.() ?? {}), kind }
  activeId.value = id
}
function duplicatePanel(s: FlowPanelState) {
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (p) p.state = JSON.parse(JSON.stringify(s))
  activeId.value = id
}

// the standard plot bag every interactive view receives (docs/UI.md → generic plot-integration
// interface). The view picks its own image out of `imageUids`.
const ctx = computed(() => ({
  projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value, vis: defaultVis(),
}))

// Seed one flow-model plot for an image that has no canvas yet — on first bind AND after the
// selection moves (the reactive key rebinds without remounting). Only when EMPTY, so a restored
// canvas is left alone and remounts don't stack duplicates.
watch(ckey, () => { if (panels.value.length === 0) addKind('flowMetrics') }, { immediate: true })
</script>

<template>
  <div class="flow-plots">
    <div v-if="!imageUids.length" class="fp-empty cc-empty-inline">
      Select an image above to inspect a flow model.
    </div>
    <template v-else>
      <div class="fp-bar">
        <select class="fp-add" v-tooltip.bottom="'Add a plot'"
                @change="addKind(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="" disabled selected>+ Plot…</option>
          <option v-for="t in plotTypes" :key="t.key" :value="t.key">{{ t.label }}</option>
        </select>
        <div class="cc-btn-group" v-tooltip.bottom="'Arrange windows'">
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Tile in a grid'"
                  @click="arrangeGrid"><i class="pi pi-th-large" /></button>
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Cascade windows'"
                  @click="arrangeCascade"><i class="pi pi-clone" /></button>
        </div>
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': showVault }"
                  @click="showVault = !showVault"
                  v-tooltip.bottom="showVault ? 'Hide the model vault' : 'Show the model vault'">
            <i class="pi pi-database" />
          </button>
        </div>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth"
                           @fit-height="fitHeight" @reset="resetZoom" />
        <span class="fp-hint cc-muted cc-fs-xs">drag plots by their title · resize from the corner</span>
      </div>

      <div ref="canvasRef" class="fp-canvas">
        <!-- outside the zoom layer, like the population manager: the manager stays full-size -->
        <FlowModelVault v-if="showVault" />
        <div ref="zoomRef" class="fp-zoom" :style="workspaceStyle">
          <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
            <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                              :active="p.id === activeId" :view="p.state.kind"
                              :context="ctx" :state="p.state" :duplicable="true"
                              :persist-key="`${ckey}:${p.id}`"
                              @activate="activeId = p.id" @remove="remove(p.id)"
                              @duplicate="duplicatePanel(p.state)" />
          </template>
        </div>
      </div>
    </template>
  </div>
</template>

<style scoped>
.flow-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.fp-empty { padding: 20px; }   /* + .cc-empty-inline (row/colour) */
.fp-bar { display: flex; align-items: center; gap: 14px; padding: 8px 4px;
          font-size: var(--cc-fs-sm); flex-shrink: 0; }
.fp-add { padding: 4px 8px; }
.fp-hint { opacity: 0.7; }
.fp-canvas { position: relative; flex: 1; min-height: 70vh; }
/* scaled workspace (offsetParent for the panels); size + transform set inline by useCanvasWorkspace */
.fp-zoom { position: absolute; top: 0; left: 0; min-width: 100%; min-height: 100%; }
</style>
