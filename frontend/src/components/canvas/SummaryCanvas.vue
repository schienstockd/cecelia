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
import { ref, computed, watch, onMounted, onUnmounted, useTemplateRef } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useWsStore } from '../../stores/ws'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useViewState } from '../../composables/useViewState'
import SeriesPicker from './SeriesPicker.vue'
import SummaryPanel from './SummaryPanel.vue'
import { tkey, parseTkey } from '../../plots/series'
import { defaultVis, type VisProps } from '../../plots/plot'
import type { PlotSpec, PlotSeries, SegmentationPops, SeriesTarget, ChartType } from '../../plots/types'

const props = defineProps<{ imageUids: string[]; module?: string | null }>()
const project = useProjectStore()
const meta = useProjectMetaStore()
const ws = useWsStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)   // drives "this image" plots
const setUid = computed(() => project.activeSetUid)

// ── canvas state: panels (per-plot) + canvas-level view options (persisted) ─────────────────────
// Per-panel state lives in the panel objects; canvas-LEVEL options live in the `shared` bag via
// useViewState — declare every option in the defaults below and it persists across navigation with
// NO per-field wiring (the forget-proof, Shiny-reactiveValues-style mechanism; see useViewState.ts).
// PanelState carries the per-PLOT options too (chartType/measure/bins/… — edited inside SummaryPanel)
// so those persist as well.
interface PanelState {
  specId: string; sel: string[]; vis: VisProps
  chartType?: ChartType; measure?: string; bins?: number; normalize?: boolean; errorMetric?: 'sd' | 'sem' | 'ci95'
  groupBy?: string
  matrixMode?: 'profile' | 'crosstab'; zscore?: boolean; matrixNormalize?: 'none' | 'row' | 'col' | 'total'
}
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')
const { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade } =
  useCanvasPanels<PanelState>(canvasRef, () => ({ specId: specs.value[0]?.id ?? '', sel: [], vis: defaultVis() }),
    `summary:${props.module ?? 'universal'}`)
const { compareMode, compareAttr, compareAttr2, scope, sel: gSel, vis: gVis, poolGroups } = useViewState(shared, {
  // compare mode: 'image' = first selected image; 'per_image' = one series per image; 'summarised' =
  // pooled; 'by_attr' = group images by the `compareAttr` attribute (one series per attribute value)
  compareMode: 'image' as 'image' | 'per_image' | 'summarised' | 'by_attr',
  compareAttr: '' as string,                  // primary image attribute to group by (when by_attr)
  compareAttr2: '' as string,                 // optional interaction attribute (combined as primary.secondary)
  scope: 'global' as 'global' | 'local',     // global = one value for every plot; local = active plot only
  sel: [] as string[],                        // global-scope eye-selected target keys (tkey)
  vis: defaultVis() as VisProps,              // global-scope visual properties
  // pool across populations AND images so series form only by a plot's "Split by" level (one series
  // per group, e.g. per HMM state) — generic, applies to every plot on the canvas.
  poolGroups: false as boolean,
})
const canCompare = computed(() => !!setUid.value && props.imageUids.length > 1)
const crossImage = computed(() => compareMode.value !== 'image' && canCompare.value)
const panelSetUid = computed(() => crossImage.value ? setUid.value : null)
const panelImageUids = computed(() => crossImage.value ? props.imageUids : undefined)
const panelScope = computed<'per_image' | 'summarised'>(() =>
  compareMode.value === 'summarised' ? 'summarised' : 'per_image')

// image attributes available across the set (for the "by attribute" compare mode); the chosen one is
// sent as `groupAttr` so images sharing a value pool into one series labelled by the value.
const setAttrs = ref<{ name: string; values: string[] }[]>([])
async function loadAttrs() {
  if (!setUid.value || !canCompare.value) { setAttrs.value = []; return }
  const p = new URLSearchParams({ projectUid: projectUid.value, setUid: setUid.value })
  if (props.imageUids.length) p.set('imageUids', props.imageUids.join(','))
  try { setAttrs.value = (await (await fetch(`/api/plots/attrs?${p}`)).json()).attrs ?? [] }
  catch { setAttrs.value = [] }
}
// default the attribute once when entering by_attr / when the list loads; keep the interaction valid
watch([compareMode, setAttrs, compareAttr], () => {
  if (compareMode.value === 'by_attr' && !compareAttr.value && setAttrs.value.length)
    compareAttr.value = setAttrs.value[0].name
  if (compareAttr2.value && (compareAttr2.value === compareAttr.value
      || !setAttrs.value.some(a => a.name === compareAttr2.value)))
    compareAttr2.value = ''
})
// the chosen attribute(s) to group by — primary + optional interaction (combined as primary.secondary,
// like the old R paste0(axisX, ".", interaction)). Empty when not in by_attr mode.
const panelGroupAttr = computed<string[]>(() =>
  compareMode.value === 'by_attr' && crossImage.value
    ? [compareAttr.value, compareAttr2.value].filter(Boolean) : [])
// interaction options exclude the primary (no "A.A")
const attrOptions2 = computed(() => setAttrs.value.filter(a => a.name !== compareAttr.value))

// ── available plot types (per-module registry; null module = universal canvas → all specs) ──────
const specs = ref<PlotSpec[]>([])
const specById = computed(() => Object.fromEntries(specs.value.map(s => [s.id, s])))
// the populations available depend on the pop_type; the per-module specs share one (e.g. behaviour
// → "live"). Use the first spec's pop_type for the picker (default "live").
const popType = computed(() => specs.value[0]?.dataSource.popType ?? 'live')
// track-granularity plots union live + track pops in the picker (see /api/plots/populations). Use
// "track" if ANY spec in this module is track-granularity (the module can mix cell + track plots,
// and specs[0] may be a cell plot) so track gates are always available where a track plot exists.
const granularity = computed(() => specs.value.some(s => s.dataSource.granularity === 'track') ? 'track' : 'cell')
async function loadSpecs() {
  const q = props.module ? `?module=${encodeURIComponent(props.module)}` : ''
  try { specs.value = await (await fetch(`/api/plots/definitions${q}`)).json() } catch { specs.value = [] }
}

// ── populations available across the selected images, grouped by segmentation ───────────────────
const segPops = ref<SegmentationPops[]>([])
// colour by population, keyed by `value_name + path` (= the plot series' `pop` id) — colour is per
// population and independent of pop_type, so this key omits popType (unlike the selection tkey).
const popColors = computed(() => {
  const m = new Map<string, string>()
  for (const g of segPops.value) for (const p of g.populations) m.set(`${g.valueName}${p.path}`, p.colour)
  return m
})
async function loadPops() {
  if (!imageUid.value && !props.imageUids.length) { segPops.value = []; return }
  const p = new URLSearchParams({ projectUid: projectUid.value, popType: popType.value, granularity: granularity.value })
  if (setUid.value) { p.set('setUid', setUid.value); if (props.imageUids.length) p.set('imageUids', props.imageUids.join(',')) }
  else if (imageUid.value) p.set('imageUid', imageUid.value)
  try { segPops.value = await (await fetch(`/api/plots/populations?${p}`)).json() }
  catch { segPops.value = [] }
}

// live updates: the server broadcasts `gating:popmap` after any gate mutation (gate page, napari,
// other clients). A `live` population's gates live in the `flow` map, so refresh regardless of the
// broadcast's popType — refetch the population list AND bump a token so the panels re-pull data
// (membership may have changed without any prop changing).
const reloadToken = ref(0)
function onPopmap(d: unknown) {
  const m = d as { imageUid?: string }
  if (m.imageUid && props.imageUids.length && !props.imageUids.includes(m.imageUid)) return
  loadPops(); reloadToken.value++
}

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

// series colour: always the POPULATION colour, so the same population reads identically across
// images (the user compares populations, not images — facet/x-position separates the images). When
// the population manager colour isn't available, the panel's palette option (distinct/Okabe-Ito/…)
// reassigns by series order. `s.pop` is the value_name+path id.
function seriesColor(s: PlotSeries): string {
  return popColors.value.get(s.pop) ?? '#7c93b8'
}

function addPanel(specId: string) { if (specId) { add(); const p = panels.value.at(-1); if (p) p.state.specId = specId } }

// Duplicate a panel: new panel with a deep copy of the source's state (spec, series selection,
// chart type/measure/groupBy/vis) — so the user can change one thing (e.g. measure speed → angle).
function duplicatePanel(src: { state: PanelState }) {
  add()
  const p = panels.value.at(-1)
  if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis } }
}

// reload populations when the image selection / pop_type changes; prune selections for pops that
// vanished (deselected image, changed segmentation set).
watch([() => props.imageUids.join(','), popType, setUid], () => { loadPops(); loadAttrs() })
watch(() => segPops.value, () => {
  const exist = new Set<string>()
  for (const g of segPops.value) for (const p of g.populations) exist.add(tkey(p.popType, g.valueName, p.path))
  gSel.value = gSel.value.filter(k => exist.has(k))
  for (const p of panels.value) p.state.sel = p.state.sel.filter(k => exist.has(k))
})
onMounted(async () => {
  ws.on('gating:popmap', onPopmap)
  await loadSpecs(); await loadPops(); await loadAttrs()
})
onUnmounted(() => ws.off('gating:popmap', onPopmap))
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
                        :reload-token="reloadToken" :persist-key="`summary:${props.module ?? 'universal'}:${p.id}`"
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
