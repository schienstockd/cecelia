<!--
  Summary-plot canvas (the analysis-plot surface). Same shell as the gating workspace
  (useCanvasPanels + CanvasPanel), but the panels are Vega-Lite SummaryPanels chosen from the
  plot-spec registry (GET /api/plots/definitions, filtered by `module`) and the population picker
  is the read-only, cross-segmentation SeriesPicker (not the gating PopulationManager).

  The user selects one or more images above the canvas; this canvas shows ALL populations available
  across those images, GROUPED BY SEGMENTATION (GET /api/plots/populations). Eye-selecting a
  population makes it a plot series — so populations from different images AND different
  segmentations can be overlaid on one plot. The "compare" control pools across the selected images
  (per image / pooled); chart type is a per-panel choice, independent of the data source.

  This is the per-module canvas; the universal canvas (Phase 4) is the same component with the
  module filter off.
-->
<script setup lang="ts">
import { toggleSelected, narrowToSingle } from '../../utils/selection'
import { computed, ref, watch, provide, useTemplateRef } from 'vue'
import CanvasArrangeButtons from './CanvasArrangeButtons.vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useCanvasWorkspace } from '../../composables/useCanvasWorkspace'
import { useSummaryData } from '../../composables/useSummaryData'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import SeriesPicker from './SeriesPicker.vue'
import SummaryPanel from './SummaryPanel.vue'
import InteractivePanel from './InteractivePanel.vue'
import { INTERACTIVE_VIEWS, isPluginView, railFor, popTypesFor, popTypeSpecFor, singlePopFor } from './interactiveViews'
import CanvasZoomControl from './CanvasZoomControl.vue'
import { tkey, parseTkey, seriesMemo } from '../../plots/series'
import { defaultVis, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import type { SeriesTarget, ChartType } from '../../plots/types'
import { migrateSpecId, isPrecomputedSpec } from '../../plots/popTypes'
import { emptyReadout, type PlotReadout } from '../../plots/plotReadout'
import CcToggle from '../CcToggle.vue'
import PlotNotice from './PlotNotice.vue'

// `canvasKey` OPTIONALLY overrides the persistence namespace (default `summary:{module|universal}`).
// The tabbed Analysis board passes `analysis:{projectUid}:tab:{id}` per tab so each board persists
// independently; parents that switch the key MUST also `:key` this component by it so setup re-runs.
// `views` = interactive plots this page ALSO offers, named by their stable registry id. A plugin
// declares them in `plugin.json` → `contributions.views` (PLUGINS_PLAN Decision 11) and the custom
// module page passes them through; every other host passes none and is unchanged. What this makes
// public is view IDS, not components — renaming `trackPaths` breaks installed plugins, rewriting
// `TrackPathsView.vue` does not.
export interface DeclaredView { view: string; label?: string; plugin?: string }
const props = defineProps<{ imageUids: string[]; module?: string | null; canvasKey?: string
                            views?: DeclaredView[] }>()
const project = useProjectStore()
const meta = useProjectMetaStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)   // drives "this image" plots
const setUid = computed(() => project.activeSetUid)

// Persistence key: an explicit override (the Analysis board) wins; otherwise per-module + PER SET.
//
// It used to be per IMAGE, which coupled two unrelated things to whichever image happened to be first
// in the selection: not just what was plotted, but WHICH SAVED CANVAS you were looking at. Ticking a
// different set of images silently swapped your whole plot layout, and ticking five images showed the
// layout of the first. Summary plots are set-aware by design — per-image vs pooled vs by-attribute is
// exactly what the `compare` control decides — so the layout has no business being image-scoped on top
// of that. `ClusterPlots` was already `clust:{popType}:{setUid}`, and `objectOf` persists a set-keyed
// canvas to the SET's own moduleCanvases.json, so this is an existing path, not a new one.
//
// Canvases saved under the old per-image keys are deliberately NOT ported (agreed 2026-08-15): there is
// no honest merge from N per-image layouts into one, so they are simply no longer read.
const ckey = computed(() => props.canvasKey ?? `summary:${props.module ?? 'universal'}:${setUid.value ?? 'none'}`)

// per-plot state (edited inside SummaryPanel; persists in the panel objects). Canvas-level view state
// + all shared data (specs/pops/attrs, compare/scope/global sel+vis) come from useSummaryData below.
interface PanelState {
  // Index signature so a panel's state is assignable to the generic InteractivePanel's
  // `Record<string, unknown>` — the same shape GatingPlots' panel state carries, for the same reason.
  [key: string]: unknown
  specId: string; sel: string[]; vis: VisProps
  // set → this panel is an INTERACTIVE view (interactiveViews.ts) rather than a summary spec. Every
  // panel keeps `sel`/`vis` regardless, so the selection/prune/duplicate helpers below need no
  // interactive-only branches; the picker simply says the eye toggles do nothing on such a panel.
  kind?: string
  popType?: string        // which population family this plot shows (specs that offer a choice)
  chartType?: ChartType; measure?: string; bins?: number; normalize?: boolean; errorMetric?: 'sd' | 'sem' | 'ci95'
  groupBy?: string; smooth?: number; interval?: boolean
  matrixMode?: 'profile' | 'crosstab'; zscore?: boolean; heatmapValues?: boolean; matrixNormalize?: 'none' | 'row' | 'col' | 'total'
}
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')   // the visible viewport (zoom + fit measure it)
const zoomRef = useTemplateRef<HTMLElement>('zoomRef')       // the scaled workspace (panels' offsetParent)
const { panels, activeId, activePanel, shared, add, remove, removeAll, arrangeGrid, arrangeCascade, contentBounds } =
  useCanvasPanels<PanelState>(zoomRef, () => ({ specId: specs.value[0]?.id ?? '', sel: [], vis: defaultVis() }),
    // tileBox: the grid is sized to the VIEWPORT, not to the workspace it grew (utils/tileGrid.ts)
    ckey, { tileBox: () => workspaceBase.value })
// show/hide the floating population picker — persisted per canvas in the `shared` bag (default shown)
const showManager = computed<boolean>({ get: () => (shared.value.showManager as boolean) ?? true, set: v => (shared.value.showManager = v) })

// ── visual zoom (shared control) — scale the free-floating workspace to see everything at once. Fit
// fits the actual plot bounding box; drag is zoom-corrected via the injected zoom (CanvasPanel →
// useFloatingPanel). The workspace GROWS when zoomed out (useCanvasWorkspace) so the whole page stays
// usable; the population picker sits OUTSIDE the zoom layer so the control panel stays full-size.
const { zoom, fitWidth, fitHeight, setZoom, reset: resetZoom } = useCanvasZoom(canvasRef,
  () => ({ w: contentBounds.value.w || null, h: contentBounds.value.h }))
provide(CANVAS_ZOOM_KEY, zoom)
const { workspaceStyle, workspaceBase } = useCanvasWorkspace(canvasRef, zoom,
  // grow the workspace to hold the plots (a tall Tile grid scrolls instead of spilling);
  // a getter, so it may name `contentBounds` from the line above
  () => contentBounds.value)
// shared summary-plot data + canvas-level view-state (identical whether plots float or sit in a grid)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys, popType,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({ projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module, shared,
  // The population picker follows the ACTIVE plot's spec, exactly as on the Analysis board. This used
  // to be board-only, on the assumption that a module page's specs all share one popType — which stopped
  // being true once the per-poptype population summaries moved onto the Explore pages (Phenotype hosts
  // flow + clust, Behaviour live + trackclust, Spatial region). Without it the picker falls back to
  // `specs[0]`, and since /api/plots/definitions just walks readdir, "first" is filename order — so the
  // page would silently offer the wrong population family for the selected plot.
  activeSpecId: computed(() => activePanel.value?.state.specId ?? null),
  // the active plot's chosen population family — the manager lists THAT family (one control, on the plot)
  activePopType: computed(() => activePanel.value?.state.popType ?? null),
  // An INTERACTIVE panel that slices by population declares its families on its registry entry, so the
  // rail lists THAT plot's family rather than whichever one `specs[0]` happens to carry. Same
  // resolution the board uses — a second path here could disagree about what the plot is showing.
  activeFamily: computed(() => {
    const k = activePanel.value?.state.kind
    return k ? popTypeSpecFor(String(k)) : null
  }) })

// Migrate canvases persisted before the four per-popType population summaries collapsed into one spec
// with a family picker. Without this a saved panel's specId no longer resolves and the panel silently
// renders nothing (`v-if="specById[...]"` below). Mirrors ClusterPlots' KIND_ALIASES.
for (const p of panels.value) if (!p.state.kind) migrateSpecId(p.state)

// ── interactive views (PLUGINS_PLAN Decision 11) ──────────────────────────────────────────────────
// A declared id is resolved against the registry HERE, because the registry is a frontend module and
// the manifest that names it is read in Julia. Two ways it can fail to resolve, deliberately reported
// as one line: the id does not exist in this Cecelia, or it exists but is not offered to plugins
// (`pluginPage` — e.g. `trackCorrection`, which mutates, or a view needing a rail this canvas does not
// render). Either way it must be SAID: a view silently missing from the picker is exactly the
// blank-panel failure this codebase keeps producing. (A panel already holding an unusable kind is
// covered too — InteractivePanel renders "Unknown interactive plot".)
const VIEW_OPT_PREFIX = 'view:'   // option values are namespaced so a view id cannot collide with a spec id
const declaredViews = computed(() => (props.views ?? []).filter(v => isPluginView(v.view)))
const unusableViews = computed(() => (props.views ?? []).filter(v => !isPluginView(v.view)))
const viewLabel = (v: DeclaredView) => v.label || INTERACTIVE_VIEWS[v.view]?.label || v.view
const unusableViewText = computed(() =>
  `Plot not available here: ` +
  unusableViews.value.map(v => v.plugin ? `${v.view} (${v.plugin})` : v.view).join(', '))
// The bag an interactive view receives (docs/UI.md → generic plot-integration interface). Views pick
// their own image out of `imageUids` and keep the rest in their panel state.
//
// A view on the POPULATION rail is part of THIS canvas's comparison and gets the same four things a
// SummaryPanel gets — selection, compare mode + its attributes, pool toggle — exactly as the board
// builds it (`LayoutCanvas.ctxFor`). A self-contained view declares `rail: 'none'` and never sees them.
// Both branches exist because #593 moved the two track plots onto the pops rail: before it, every
// plugin-nameable view was self-contained and this was one object.
const viewContext = (id: number, st: PanelState) => {
  const base = { projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value,
                 vis: panelVis(st) }
  return railFor(String(st.kind)) === 'pops'
    ? { ...base, series: panelSeries(id, st), popTypes: popTypesFor(String(st.kind)),
        compareMode: compareMode.value, groupAttr: panelGroupAttr.value, poolGroups: poolGroups.value }
    : base
}

// global/local scope governs BOTH the eye-selection AND the visual properties (like the gating
// PopulationManager): global = one value shared by every plot, local = the active plot's own.
const panelSel = (s: PanelState) => scope.value === 'global' ? gSel.value : s.sel
const activeSel = computed(() => scope.value === 'global' ? gSel.value : (activePanel.value?.state.sel ?? []))
// the SAME registry policy the other two hosts read (`singlePopFor`). No plugin-nameable view declares
// it today, but a policy honoured by two hosts out of three is how the three drift apart.
const activeSinglePop = computed(() => {
  const k = activePanel.value?.state.kind
  return !!k && singlePopFor(String(k))
})
const panelVis = (s: PanelState) => scope.value === 'global' ? gVis.value : s.vis
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activePanel.value?.state.vis ?? DEFAULT_VIS))
// the stats test each panel's last result actually ran (`auto` resolves it server-side from the group
// count) — the picker shows the ACTIVE plot's, so the user can see what `auto` chose. Not persisted:
// it's a readout of the current result, not a setting.
const readouts = ref<Record<number, PlotReadout>>({})
const activeReadout = computed<PlotReadout>(() => readouts.value[activeId.value] ?? emptyReadout())
// the active plot is PRECOMPUTED — its populations come from an analysis run, so the picker says so
// instead of offering eye toggles that do nothing (see isPrecomputedSpec)
const activeIsPrecomputed = computed(() => {
  // An interactive view on the POPS rail consumes the eye-selection (the two track plots do). One on
  // any other rail brings its own data and controls, so the picker says the selection is unused
  // rather than offering toggles that do nothing.
  const kind = activePanel.value?.state.kind
  if (kind) return railFor(String(kind)) !== 'pops' 
  const id = activePanel.value?.state.specId
  const spec = id ? specById.value[id] : null
  return !!spec && isPrecomputedSpec(spec)
})
function removePanel(id: number) { remove(id); delete readouts.value[id] }
// Close all must drop the readouts too — they are keyed by panel id, and a stale entry would be
// re-adopted by the next panel that reuses a freed id (`activeReadout` reads this map by id).
function removeAllPanels() { removeAll(); readouts.value = {} }
function toggleTarget(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  const next = (cur: string[]) => toggleSelected(cur, k, { single: activeSinglePop.value })
  if (scope.value === 'global') gSel.value = next(gSel.value)
  else if (activePanel.value) activePanel.value.state.sel = next(activePanel.value.state.sel)
}
// the policy can change under an existing selection when the ACTIVE panel changes — narrow rather than
// let a single-population plot draw one of several and say nothing
watch(activeSinglePop, single => {
  if (!single) return
  if (scope.value === 'global') gSel.value = narrowToSingle(gSel.value)
  else if (activePanel.value) activePanel.value.state.sel = narrowToSingle(activePanel.value.state.sel)
})
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activePanel.value) activePanel.value.state.vis = { ...activePanel.value.state.vis, ...patch }
}
// a panel's series = its selected target keys parsed back into {valueName, pop}
// keyed by PANEL ID (panels are free-floating, not slot-indexed) — see seriesMemo
const memoSeries = seriesMemo<number>()
const panelSeries = (id: number, s: PanelState): SeriesTarget[] => memoSeries(id, panelSel(s))

function addPanel(value: string) {
  if (!value) return
  add()
  const p = panels.value.at(-1)
  if (!p) return
  if (value.startsWith(VIEW_OPT_PREFIX)) {
    const kind = value.slice(VIEW_OPT_PREFIX.length)
    p.state = { ...p.state, ...(INTERACTIVE_VIEWS[kind]?.initialState?.() ?? {}), kind }
  } else p.state.specId = value
}

// Duplicate a panel: new panel with a deep copy of the source's state (spec, series selection,
// chart type/measure/groupBy/vis) — so the user can change one thing (e.g. measure speed → angle).
function duplicatePanel(src: { state: PanelState }) {
  add()
  const p = panels.value.at(-1)
  if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis } }
}

// "Show series": explode this panel by measurement — one duplicate per selected measure, so all the
// track measurements (speed, displacement, straightness, …) are visible side by side instead of
// flipping the single measure dropdown. Same deep-clone as duplicatePanel, overriding `measure`.
function explodePanel(src: { state: PanelState }, measures: string[]) {
  for (const m of measures) {
    add()
    const p = panels.value.at(-1)
    if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis }, measure: m }
  }
  arrangeGrid()   // tile them so the whole set is visible at once (the point of "show series")
}

// useSummaryData prunes the GLOBAL selection when pops vanish; prune each panel's LOCAL selection here.
// popType-aware (keep other-popType keys) for parity with the board's mixed-popType prune.
watch(segPops, () => {
  const valid = validSelKeys.value, pt = popType.value
  const keep = (k: string) => parseTkey(k).popType !== pt || valid.has(k)
  for (const p of panels.value) p.state.sel = p.state.sel.filter(keep)
})
</script>

<template>
  <div class="summary-canvas">
    <div v-if="!imageUid" class="sc-empty cc-muted">Select one or more images above to plot.</div>
    <template v-else>
      <div class="sc-bar cc-row cc-row-loose">
        <select class="sc-add" v-tooltip.bottom="'Add a plot'"
                @change="addPanel(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="">+ Plot…</option>
          <option v-for="s in specs" :key="s.id" :value="s.id">{{ s.label }}</option>
          <!-- Interactive views a plugin asked for on this page. In their own group so it is obvious
               they are a different kind of plot: they fetch their own data and carry their own
               controls, so the population picker does not drive them. -->
          <optgroup v-if="declaredViews.length" label="Interactive">
            <option v-for="v in declaredViews" :key="v.view" :value="`${VIEW_OPT_PREFIX}${v.view}`">
              {{ viewLabel(v) }}
            </option>
          </optgroup>
        </select>
        <!-- compare cluster: mode + (by attribute) its attribute selects, kept tight in one group -->
        <!-- the row's tip belongs on the mode select (the control it describes, and the only one here
             without its own); on the container it fired over the attribute selects too -->
        <div v-if="canCompare" class="sc-compare">
          <span class="sc-lbl">compare</span>
          <select v-model="compareMode" class="sc-cmp"
                  v-tooltip.bottom="'How to compare across the selected images'">
            <option value="image">this image</option>
            <option value="per_image">per image</option>
            <option value="summarised">pooled</option>
            <option value="by_attr" :disabled="!setAttrs.length">by attribute</option>
          </select>
          <template v-if="compareMode === 'by_attr'">
            <select v-model="compareAttr" class="sc-attr" v-tooltip.bottom="'Attribute to group by'">
              <option v-for="a in setAttrs" :key="a.name" :value="a.name">{{ a.name }}</option>
            </select>
            <template v-if="attrOptions2.length">
              <span class="sc-x">×</span>
              <select v-model="compareAttr2" class="sc-attr" v-tooltip.bottom="'Optional second attribute to combine (e.g. Treatment × Mouse)'">
                <option value="">none</option>
                <option v-for="a in attrOptions2" :key="a.name" :value="a.name">{{ a.name }}</option>
              </select>
            </template>
          </template>
        </div>
        <CcToggle class="sc-pool" v-model="poolGroups" label="pool to groups"
          v-tooltip.bottom="'Pool populations and images — one series per Split-by group'" />
        <CanvasArrangeButtons :count="panels.length" @tile="arrangeGrid" @cascade="arrangeCascade"
                              @close-all="removeAllPanels" />
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': showManager }"
                  @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the population picker' : 'Show the population picker'">
            <i class="pi pi-sitemap" />
          </button>
        </div>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
        <!-- A declared view that resolves to nothing: say which id, and which plugin asked for it.
             Silently dropping it from the picker is the failure Decision 11 exists to avoid. -->
        <PlotNotice v-if="unusableViews.length" :text="unusableViewText"
                    tip="The plugin names a plot this Cecelia does not offer — update the plugin, or ask its author." />
        <span v-if="!specs.length && !declaredViews.length" class="sc-hint cc-muted cc-fs-xs">No plot types available for this module yet.</span>
        <span v-else-if="specs.length" class="sc-hint cc-muted cc-fs-xs">eye-select populations to plot · drag plots by their title</span>
      </div>
      <div class="sc-canvas">
        <!-- scroll viewport (measured): the workspace inside it may be TALLER than the
             visible box, so the plots scroll. The rail is a sibling BELOW, outside this
             box, so it stays put instead of scrolling away with them. -->
        <div ref="canvasRef" class="sc-scroll">
        <!-- scaled workspace: the panels zoom together; the population picker stays full-size (below) -->
        <div ref="zoomRef" class="sc-zoom" :style="workspaceStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <InteractivePanel v-if="p.state.kind" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="viewContext(p.id, p.state)" :state="p.state" :duplicable="true"
                            :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="removePanel(p.id)"
                            @duplicate="duplicatePanel(p)" />
          <SummaryPanel v-else-if="specById[p.state.specId]" :index="i" :arrange="p.arrange"
                        :active="p.id === activeId" :spec="specById[p.state.specId]"
                        :project-uid="projectUid" :image-uid="imageUid"
                        :set-uid="panelSetUid" :image-uids="panelImageUids" :scope="panelScope"
                        :group-attr="panelGroupAttr"
                        :series="panelSeries(p.id, p.state)" :series-color="seriesColor" :vis="panelVis(p.state)"
                        :ui="p.state" :collapse-series="poolGroups"
                        :reload-token="reloadToken" :persist-key="`${ckey}:${p.id}`"
                        @activate="activeId = p.id" @remove="removePanel(p.id)"
                        @duplicate="duplicatePanel(p)" @explode="explodePanel(p, $event)"
                        @readout="readouts[p.id] = $event" />
        </template>
        </div>
        </div>
        <SeriesPicker v-if="showManager" :groups="segPops" :selected="activeSel" :scope="scope" :vis="activeVis"
                      :readout="activeReadout" :selection-unused="activeIsPrecomputed"
                      @toggle="toggleTarget" @update:scope="scope = $event" @update:vis="setVis" />
      </div>
    </template>
  </div>
</template>

<style scoped>
.summary-canvas { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.sc-empty { padding: 20px; }   /* + .cc-muted */
.sc-bar { padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }
.sc-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.sc-add { min-width: 8rem; }
/* compare cluster: one tight group so the mode + attribute selects read as a unit */
.sc-compare { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); }
.sc-lbl { font-size: var(--cc-fs-xs); opacity: 0.8; }
.sc-cmp { min-width: 8rem; }
.sc-attr { min-width: 5.5rem; max-width: 8rem; }   /* short attribute names — no need for 9rem */
.sc-x { opacity: 0.6; }
.sc-hint { opacity: 0.7; margin-left: auto; }
.sc-canvas { position: relative; flex: 1; min-height: 70vh; }
/* the scaled workspace (offsetParent for the floating panels); size + transform set inline by
   useCanvasWorkspace — grows to viewport/zoom when zoomed out so the whole page stays usable */
/* min 100% so the workspace always at least fills the viewport (like the old inset:0) even before the
   JS size lands — else a 0 measurement collapses it and the panels' offsetParent is ~0-wide, pinning
   drag to the top-left. useCanvasWorkspace only EXTENDS it (width/height) when zoomed out. */
/* the measured viewport: the workspace it holds can be taller than this box (useCanvasWorkspace
   grows it to fit the plots), so overflow scrolls here rather than escaping the canvas. */
.sc-scroll { position: absolute; inset: 0; overflow: auto; }
.sc-zoom { position: absolute; top: 0; left: 0; min-width: 100%; min-height: 100%; }
</style>
