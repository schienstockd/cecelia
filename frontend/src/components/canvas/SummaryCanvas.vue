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
import { computed, watch, useTemplateRef } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useSummaryData } from '../../composables/useSummaryData'
import SeriesPicker from './SeriesPicker.vue'
import SummaryPanel from './SummaryPanel.vue'
import { tkey, parseTkey } from '../../plots/series'
import { defaultVis, type VisProps } from '../../plots/plot'
import type { SeriesTarget, ChartType } from '../../plots/types'

// `canvasKey` OPTIONALLY overrides the persistence namespace (default `summary:{module|universal}`).
// The tabbed Analysis board passes `analysis:{projectUid}:tab:{id}` per tab so each board persists
// independently; parents that switch the key MUST also `:key` this component by it so setup re-runs.
const props = defineProps<{ imageUids: string[]; module?: string | null; canvasKey?: string }>()
const ckey = props.canvasKey ?? `summary:${props.module ?? 'universal'}`
const project = useProjectStore()
const meta = useProjectMetaStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)   // drives "this image" plots
const setUid = computed(() => project.activeSetUid)

// per-plot state (edited inside SummaryPanel; persists in the panel objects). Canvas-level view state
// + all shared data (specs/pops/attrs, compare/scope/global sel+vis) come from useSummaryData below.
interface PanelState {
  specId: string; sel: string[]; vis: VisProps
  chartType?: ChartType; measure?: string; bins?: number; normalize?: boolean; errorMetric?: 'sd' | 'sem' | 'ci95'
  groupBy?: string
  matrixMode?: 'profile' | 'crosstab'; zscore?: boolean; matrixNormalize?: 'none' | 'row' | 'col' | 'total'
}
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')
const { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<PanelState>(canvasRef, () => ({ specId: specs.value[0]?.id ?? '', sel: [], vis: defaultVis() }),
    ckey)
// shared summary-plot data + canvas-level view-state (identical whether plots float or sit in a grid)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({ projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module, shared })

// global/local scope governs BOTH the eye-selection AND the visual properties (like the gating
// PopulationManager): global = one value shared by every plot, local = the active plot's own.
const panelSel = (s: PanelState) => scope.value === 'global' ? gSel.value : s.sel
const activeSel = computed(() => scope.value === 'global' ? gSel.value : (activePanel.value?.state.sel ?? []))
const panelVis = (s: PanelState) => scope.value === 'global' ? gVis.value : s.vis
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activePanel.value?.state.vis ?? defaultVis()))
const toggle = (arr: string[], v: string) => arr.includes(v) ? arr.filter(x => x !== v) : [...arr, v]
function toggleTarget(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  if (scope.value === 'global') gSel.value = toggle(gSel.value, k)
  else if (activePanel.value) activePanel.value.state.sel = toggle(activePanel.value.state.sel, k)
}
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activePanel.value) activePanel.value.state.vis = { ...activePanel.value.state.vis, ...patch }
}
// a panel's series = its selected target keys parsed back into {valueName, pop}
const panelSeries = (s: PanelState): SeriesTarget[] => panelSel(s).map(parseTkey)

function addPanel(specId: string) { if (specId) { add(); const p = panels.value.at(-1); if (p) p.state.specId = specId } }

// Duplicate a panel: new panel with a deep copy of the source's state (spec, series selection,
// chart type/measure/groupBy/vis) — so the user can change one thing (e.g. measure speed → angle).
function duplicatePanel(src: { state: PanelState }) {
  add()
  const p = panels.value.at(-1)
  if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis } }
}

// useSummaryData prunes the GLOBAL selection when pops vanish; prune each panel's LOCAL selection here.
watch(segPops, () => { for (const p of panels.value) p.state.sel = p.state.sel.filter(k => validSelKeys.value.has(k)) })
</script>

<template>
  <div class="summary-canvas">
    <div v-if="!imageUid" class="sc-empty">Select one or more images above to plot.</div>
    <template v-else>
      <div class="sc-bar">
        <select class="sc-add" v-tooltip.bottom="'Add a plot'"
                @change="addPanel(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="">+ Plot…</option>
          <option v-for="s in specs" :key="s.id" :value="s.id">{{ s.label }}</option>
        </select>
        <!-- compare cluster: mode + (by attribute) its attribute selects, kept tight in one group -->
        <div v-if="canCompare" class="sc-compare"
             v-tooltip.bottom="'Compare across the selected images: this image, one series per image, pooled, or grouped by an image attribute (e.g. Treatment × Mouse)'">
          <span class="sc-lbl">compare</span>
          <select v-model="compareMode" class="sc-cmp">
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
        <label class="sc-pool" v-tooltip.bottom="'Pool across populations and images so each plot shows one series per Split-by group only (no separation by population or image)'">
          <input type="checkbox" v-model="poolGroups" /> pool to groups
        </label>
        <div class="seg" v-tooltip.bottom="'Arrange windows'">
          <button v-tooltip.bottom="'Tile in a grid'" @click="arrangeGrid"><i class="pi pi-th-large" /></button>
          <button v-tooltip.bottom="'Cascade windows'" @click="arrangeCascade"><i class="pi pi-clone" /></button>
        </div>
        <span v-if="!specs.length" class="sc-hint">No plot types available for this module yet.</span>
        <span v-else class="sc-hint">eye-select populations to plot · drag plots by their title</span>
      </div>
      <div ref="canvasRef" class="sc-canvas">
        <template v-for="(p, i) in panels" :key="p.id">
          <SummaryPanel v-if="specById[p.state.specId]" :index="i" :arrange="p.arrange"
                        :active="p.id === activeId" :spec="specById[p.state.specId]"
                        :project-uid="projectUid" :image-uid="imageUid"
                        :set-uid="panelSetUid" :image-uids="panelImageUids" :scope="panelScope"
                        :group-attr="panelGroupAttr"
                        :series="panelSeries(p.state)" :series-color="seriesColor" :vis="panelVis(p.state)"
                        :ui="p.state" :collapse-series="poolGroups"
                        :reload-token="reloadToken" :persist-key="`${ckey}:${p.id}`"
                        @activate="activeId = p.id" @remove="remove(p.id)"
                        @duplicate="duplicatePanel(p)" />
        </template>
        <SeriesPicker :groups="segPops" :selected="activeSel" :scope="scope" :vis="activeVis"
                      @toggle="toggleTarget" @update:scope="scope = $event" @update:vis="setVis" />
      </div>
    </template>
  </div>
</template>

<style scoped>
.summary-canvas { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.sc-empty { padding: 20px; color: var(--cc-text-dim); }
.sc-bar { display: flex; align-items: center; gap: 12px; padding: 8px 4px; font-size: 12px; flex-shrink: 0; flex-wrap: wrap; }
.sc-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.sc-add { min-width: 8rem; }
/* compare cluster: one tight group so the mode + attribute selects read as a unit */
.sc-compare { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: 6px; }
.sc-lbl { font-size: 11px; opacity: 0.8; }
.sc-cmp { min-width: 8rem; }
.sc-attr { min-width: 5.5rem; max-width: 8rem; }   /* short attribute names — no need for 9rem */
.sc-x { opacity: 0.6; }
.sc-hint { color: var(--cc-text-dim); font-size: 11px; opacity: 0.7; margin-left: auto; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 5px 9px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.sc-canvas { position: relative; flex: 1; min-height: 70vh; }
</style>
