<!--
  Grid LAYOUT canvas for one Analysis tab (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A2). A template
  (uniform N×M or a rectangular "comic plate") defines SLOTS; each slot holds one plot. This is the
  READ-ONLY analysis surface (project_analysis_canvas_readonly): plots are chosen from the full spec
  catalog and populations are picked in the DOCKED right-rail SeriesPicker — no gate/pop mutation.

  Reuse: the plot data + view-state come from useSummaryData (shared with the free-floating
  SummaryCanvas); each filled slot renders a DOCKED SummaryPanel (fills the slot, no float/drag). Only
  the container differs from SummaryCanvas — the panels/picker are the same components in `docked` mode.

  Persistence: the template + per-slot content + the shared view bag live in the analysisLayout store
  under this tab's `canvasKey`; the parent (TabbedCanvas) :keys us by it so a tab switch rebinds.
-->
<script setup lang="ts">
import { computed, watch, ref, useTemplateRef } from 'vue'
import { plotHostToImageURL } from '../../plots/export'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useAnalysisLayoutStore, type SlotContent } from '../../stores/analysisLayout'
import { useSummaryData } from '../../composables/useSummaryData'
import { tkey, parseTkey } from '../../plots/series'
import { defaultVis, type VisProps } from '../../plots/plot'
import { UNIFORM_PRESETS, COMIC_PRESETS, uniform } from '../../plots/layoutTemplates'
import type { SeriesTarget } from '../../plots/types'
import SummaryPanel from './SummaryPanel.vue'
import InteractivePanel from './InteractivePanel.vue'
import { INTERACTIVE_VIEWS } from './interactiveViews'
import SeriesPicker from './SeriesPicker.vue'

const props = defineProps<{ imageUids: string[]; module?: string | null; canvasKey: string }>()

const project = useProjectStore()
const meta = useProjectMetaStore()
const layout = useAnalysisLayoutStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)
const setUid = computed(() => project.activeSetUid)

layout.ensure(props.canvasKey)
const entry = computed(() => layout.entries[props.canvasKey])
// minmax(0, 1fr) — NOT bare 1fr (= minmax(auto,1fr)). Bare 1fr lets a cell grow to its content's
// min-size, and a plot's ResizeObserver then re-renders taller → the grid keeps growing. minmax(0,1fr)
// pins every track to an equal share of the (bounded) grid height, so content clips/scrolls instead.
// board-level slot height: rows share a fixed total height (rowHeight × rows); the board scrolls in the
// page if it's taller than the viewport. Default 320px/row.
const rowHeight = computed({ get: () => entry.value.rowHeight ?? 320, set: v => (entry.value.rowHeight = v) })
const gridStyle = computed(() => ({
  gridTemplateColumns: entry.value.colTracks ?? `repeat(${entry.value.cols}, minmax(0, 1fr))`,
  gridTemplateRows: entry.value.rowTracks ?? `repeat(${entry.value.rows}, minmax(0, 1fr))`,
  height: `${rowHeight.value * entry.value.rows + 8 * (entry.value.rows - 1)}px`,
}))

// shared summary-plot data + view-state (same composable the free-floating canvas uses)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({
  projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module,
  shared: computed(() => entry.value.shared),
})

// ── slot content: active slot, add/clear, drag-swap ──────────────────────────────────────────────
const activeContent = computed<SlotContent | null>(() => entry.value.contents[entry.value.activeIndex] ?? null)
type PState = { sel: string[]; vis: VisProps; [k: string]: unknown }
const st = (c: SlotContent): PState => c.state as PState

// interactive views offerable here today (self-contained context). UMAP / cluster heatmap need
// per-slot clustering context (a suffix picker) — a follow-up; only list the analysis-ready ones.
const ANALYSIS_VIEWS = ['gatingStrategy']
const interactiveOptions = computed(() => ANALYSIS_VIEWS.filter(k => k in INTERACTIVE_VIEWS).map(k => ({ key: k, label: INTERACTIVE_VIEWS[k].label })))
// image-content views (napari screenshot slots) — grouped separately in the picker
const IMAGE_VIEWS = ['filmstrip']
const imageOptions = computed(() => IMAGE_VIEWS.filter(k => k in INTERACTIVE_VIEWS).map(k => ({ key: k, label: INTERACTIVE_VIEWS[k].label })))

// the "+ Plot" value is "summary:<specId>" or "interactive:<viewKey>"
function addPlot(i: number, val: string) {
  if (!val) return
  const sep = val.indexOf(':'); const kind = val.slice(0, sep); const ref = val.slice(sep + 1)
  if (kind === 'summary') layout.setContent(props.canvasKey, i, { kind: 'summary', ref, state: { specId: ref, sel: [], vis: defaultVis() } })
  else if (kind === 'interactive') layout.setContent(props.canvasKey, i, { kind: 'interactive', ref, state: {} })
  layout.setActive(props.canvasKey, i)
}
function clearSlot(i: number) { layout.setContent(props.canvasKey, i, null) }

// generic context handed to an interactive view in a slot (self-contained views read what they need)
const ctxFor = () => ({ projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value, vis: activeVis.value })

// duplicate a slot's plot into the NEXT EMPTY slot (deep-copy its state so you can tweak one thing);
// no-op if the grid is full.
function nextEmpty(from: number): number {
  const c = entry.value.contents
  for (let k = 1; k <= c.length; k++) { const j = (from + k) % c.length; if (!c[j]) return j }
  return -1
}
function duplicateSlot(i: number) {
  const src = entry.value.contents[i]
  if (!src) return
  const j = nextEmpty(i)
  if (j < 0) return
  layout.setContent(props.canvasKey, j, { kind: src.kind, ref: src.ref, state: JSON.parse(JSON.stringify(src.state)) })
  layout.setActive(props.canvasKey, j)
}

// drag reorder via the slot's GRIP handle only (so interacting with the plot itself never starts a
// drag); drop swaps the two slots' contents.
const dragFrom = { i: -1 }
function onDrop(i: number) { if (dragFrom.i >= 0) layout.swap(props.canvasKey, dragFrom.i, i); dragFrom.i = -1 }

// ── global/local scope (drives eye-selection + vis), targeting the ACTIVE slot when local ─────────
const panelSel = (c: SlotContent) => scope.value === 'global' ? gSel.value : (st(c).sel ?? [])
const panelVis = (c: SlotContent) => scope.value === 'global' ? gVis.value : (st(c).vis ?? defaultVis())
const activeSel = computed(() => scope.value === 'global' ? gSel.value : (activeContent.value ? st(activeContent.value).sel : []))
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activeContent.value ? st(activeContent.value).vis : defaultVis()))
const toggle = (arr: string[], v: string) => arr.includes(v) ? arr.filter(x => x !== v) : [...arr, v]
function toggleTarget(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  if (scope.value === 'global') gSel.value = toggle(gSel.value, k)
  else if (activeContent.value) st(activeContent.value).sel = toggle(st(activeContent.value).sel ?? [], k)
}
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activeContent.value) st(activeContent.value).vis = { ...(st(activeContent.value).vis ?? defaultVis()), ...patch }
}
const panelSeries = (c: SlotContent): SeriesTarget[] => panelSel(c).map(parseTkey)

// prune vanished pops from every slot's local selection (the composable prunes the global one). Guard on
// a non-empty segPops — it's transiently [] during load/image-switch, and pruning then would wipe (and
// then persist-empty) a restored per-slot selection.
watch(segPops, () => {
  if (!segPops.value.length) return
  for (const c of entry.value.contents) if (c && Array.isArray(st(c).sel)) st(c).sel = st(c).sel.filter(k => validSelKeys.value.has(k))
})

// ── PDF export: capture each filled slot to a PNG (hiding the drag grips), keyed by its grid-area ──
const gridRef = useTemplateRef<HTMLElement>('gridRef')
const capturing = ref(false)
function labelFor(c: SlotContent): string {
  return c.kind === 'summary' ? (specById.value[c.ref]?.label ?? c.ref) : (INTERACTIVE_VIEWS[c.ref]?.label ?? c.ref)
}
// panel instances by slot index, so we can ask each for a PLOT-ONLY, LIGHT-theme image (no chrome) and
// pull the summary plot's aggregated CSV. Both panel types expose exportImage(); summary also getCsv().
type SummaryRef = { getCsv(): string | null; exportImage(): Promise<string | null> }
type ExportRef = { exportImage(): Promise<string | null> }
const summaryRefs = new Map<number, SummaryRef>()
const interactiveRefs = new Map<number, ExportRef>()
function setSummaryRef(i: number, el: unknown) { if (el) summaryRefs.set(i, el as SummaryRef); else summaryRefs.delete(i) }
function setInteractiveRef(i: number, el: unknown) { if (el) interactiveRefs.set(i, el as ExportRef); else interactiveRefs.delete(i) }

type PdfSlotOut = { rect: { x: number; y: number; w: number; h: number }; png: string | null; name: string; csv?: string | null }
async function capturePage() {
  const gridEl = gridRef.value
  if (!gridEl) return { aspect: 1, slots: [] as PdfSlotOut[] }
  const slotEls = Array.from(gridEl.querySelectorAll('.lc-slot')) as HTMLElement[]
  capturing.value = true
  await new Promise(r => requestAnimationFrame(() => r(null)))   // let the grip-hide take effect
  // measure the grid + each slot so the PDF reproduces the ON-SCREEN layout (spans, plates, row height,
  // gaps) exactly — the board IS the layout guide, so slots land at their real proportions/positions.
  const gr = gridEl.getBoundingClientRect()
  const slots: PdfSlotOut[] = []
  try {
    for (let i = 0; i < entry.value.contents.length; i++) {
      const c = entry.value.contents[i]
      const el = slotEls[i]
      if (!c || !el) continue
      // prefer the panel's plot-only light-theme export; fall back to a white-ground DOM snapshot
      // (e.g. filmstrip/image slots, which are already screenshots) with the chrome hidden.
      let png: string | null = null
      if (c.kind === 'summary') png = await summaryRefs.get(i)?.exportImage() ?? null
      else if (c.kind === 'interactive') png = await interactiveRefs.get(i)?.exportImage?.() ?? null
      if (!png) png = await plotHostToImageURL(el, '#ffffff')
      const csv = c.kind === 'summary' ? (summaryRefs.get(i)?.getCsv() ?? null) : null
      const sr = el.getBoundingClientRect()
      const rect = { x: (sr.left - gr.left) / gr.width, y: (sr.top - gr.top) / gr.height,
                     w: sr.width / gr.width, h: sr.height / gr.height }
      slots.push({ rect, png, name: labelFor(c), csv })
    }
  } finally { capturing.value = false }
  return { aspect: gr.width / Math.max(1, gr.height), slots }
}
// the shown (aggregated) data for every summary slot — for the standalone CSV export (data → Prism)
function collectCsvs(): { name: string; csv: string | null }[] {
  const out: { name: string; csv: string | null }[] = []
  for (let i = 0; i < entry.value.contents.length; i++) {
    const c = entry.value.contents[i]
    if (c?.kind === 'summary') out.push({ name: labelFor(c), csv: summaryRefs.get(i)?.getCsv() ?? null })
  }
  return out
}
defineExpose({ capturePage, collectCsvs })
</script>

<template>
  <div class="layout-canvas">
    <div v-if="!imageUid" class="lc-empty">Select one or more images above to plot.</div>
    <template v-else>
      <!-- controls, grouped so nothing crowds: Layout row (uniform + custom sliders), Plates row
           (varied presets, wraps to two lines), then the data/compare row. -->
      <div class="lc-bar">
        <div class="lc-row">
          <span class="lc-lbl">Layout</span>
          <div class="seg" v-tooltip.bottom="'Uniform grids'">
            <button v-for="t in UNIFORM_PRESETS" :key="t.id"
                    @click="layout.applyTemplate(canvasKey, t)"
                    :class="{ on: entry.cols === t.cols && entry.rows === t.rows && entry.slotAreas.length === t.slots.length }">
              {{ t.label }}
            </button>
          </div>
          <div class="lc-nm" v-tooltip.bottom="'Custom uniform grid'">
            <span class="lc-lbl">cols</span>
            <input type="range" min="1" max="6" :value="entry.cols"
                   @input="layout.applyTemplate(canvasKey, uniform(+($event.target as HTMLInputElement).value, entry.rows))" />
            <span class="lc-val">{{ entry.cols }}</span>
            <span class="lc-lbl">rows</span>
            <input type="range" min="1" max="6" :value="entry.rows"
                   @input="layout.applyTemplate(canvasKey, uniform(entry.cols, +($event.target as HTMLInputElement).value))" />
            <span class="lc-val">{{ entry.rows }}</span>
          </div>
          <div class="lc-nm" v-tooltip.bottom="'Slot height (applies to all slots on the board)'">
            <span class="lc-lbl">height</span>
            <input type="range" min="160" max="720" step="10" :value="rowHeight"
                   @input="rowHeight = +($event.target as HTMLInputElement).value" />
          </div>
          <!-- compare + pool sit at the right of the layout row -->
          <div class="lc-right">
            <div v-if="canCompare" class="lc-compare"
                 v-tooltip.bottom="'Compare across the selected images'">
              <span class="lc-lbl">compare</span>
              <select v-model="compareMode">
                <option value="image">this image</option>
                <option value="per_image">per image</option>
                <option value="summarised">pooled</option>
                <option value="by_attr" :disabled="!setAttrs.length">by attribute</option>
              </select>
              <template v-if="compareMode === 'by_attr'">
                <select v-model="compareAttr"><option v-for="a in setAttrs" :key="a.name" :value="a.name">{{ a.name }}</option></select>
                <template v-if="attrOptions2.length">
                  <span class="lc-x">×</span>
                  <select v-model="compareAttr2">
                    <option value="">none</option>
                    <option v-for="a in attrOptions2" :key="a.name" :value="a.name">{{ a.name }}</option>
                  </select>
                </template>
              </template>
            </div>
            <label class="lc-pool" v-tooltip.bottom="'Pool across populations and images so each plot shows one series per Split-by group only'">
              <input type="checkbox" v-model="poolGroups" /> pool to groups
            </label>
          </div>
        </div>
        <div class="lc-row">
          <span class="lc-lbl">Plates</span>
          <div class="seg seg-wrap" v-tooltip.bottom="'Comic plates — varied-size panels'">
            <button v-for="t in COMIC_PRESETS" :key="t.id" @click="layout.applyTemplate(canvasKey, t)"
                    :class="{ on: entry.slotAreas.join('|') === t.slots.join('|') }">{{ t.label }}</button>
          </div>
        </div>
      </div>

      <div class="lc-body">
        <!-- the grid of slots -->
        <div ref="gridRef" class="lc-grid" :class="{ capturing }" :style="gridStyle">
          <div v-for="(area, i) in entry.slotAreas" :key="i" class="lc-slot"
               :class="{ active: i === entry.activeIndex, filled: !!entry.contents[i] }"
               :style="{ gridArea: area }"
               @dragover.prevent @drop.prevent="onDrop(i)"
               @mousedown="layout.setActive(canvasKey, i)">
            <!-- grip: the ONLY drag source, so dragging inside the plot never starts a reorder -->
            <div v-if="entry.contents[i]" class="lc-grip" draggable="true"
                 @dragstart="dragFrom.i = i" @dragend="dragFrom.i = -1"
                 v-tooltip.bottom="'Drag to move / swap'"><i class="pi pi-arrows-alt" /></div>
            <!-- summary plot -->
            <SummaryPanel v-if="entry.contents[i]?.kind === 'summary' && specById[entry.contents[i]!.ref]"
                          :ref="el => setSummaryRef(i, el)"
                          :index="i" :active="i === entry.activeIndex" :docked="true" :arrange="null"
                          :spec="specById[entry.contents[i]!.ref]"
                          :project-uid="projectUid" :image-uid="imageUid"
                          :set-uid="panelSetUid" :image-uids="panelImageUids" :scope="panelScope"
                          :group-attr="panelGroupAttr" :series="panelSeries(entry.contents[i]!)" :series-color="seriesColor"
                          :vis="panelVis(entry.contents[i]!)" :ui="entry.contents[i]!.state" :collapse-series="poolGroups"
                          :reload-token="reloadToken" :persist-key="`${canvasKey}:slot:${i}`"
                          @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)" />
            <!-- interactive plot (UMAP / gating-strategy / …) -->
            <InteractivePanel v-else-if="entry.contents[i]?.kind === 'interactive' && INTERACTIVE_VIEWS[entry.contents[i]!.ref]"
                              :ref="el => setInteractiveRef(i, el)"
                              :index="i" :active="i === entry.activeIndex" :docked="true"
                              :view="entry.contents[i]!.ref" :context="ctxFor()" :state="entry.contents[i]!.state"
                              :duplicable="true" :persist-key="`${canvasKey}:slot:${i}`"
                              @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)" />
            <!-- empty slot: add a plot (summary spec or interactive view) -->
            <div v-else class="lc-add">
              <select v-tooltip.bottom="'Add a plot to this slot'"
                      @change="addPlot(i, ($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
                <option value="">+ Plot…</option>
                <optgroup label="Summary">
                  <option v-for="s in specs" :key="s.id" :value="`summary:${s.id}`">{{ s.label }}</option>
                </optgroup>
                <optgroup v-if="interactiveOptions.length" label="Interactive">
                  <option v-for="v in interactiveOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
                <optgroup v-if="imageOptions.length" label="Image">
                  <option v-for="v in imageOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
              </select>
              <span class="lc-add-hint">empty slot</span>
            </div>
          </div>
        </div>

        <!-- docked pop picker (control, not content — excluded from PDF) -->
        <div class="lc-rail">
          <SeriesPicker :groups="segPops" :selected="activeSel" :scope="scope" :vis="activeVis" :docked="true"
                        @toggle="toggleTarget" @update:scope="scope = $event" @update:vis="setVis" />
        </div>
      </div>
    </template>
  </div>
</template>

<style scoped>
.layout-canvas { display: flex; flex-direction: column; }
.lc-empty { padding: 20px; color: var(--cc-text-dim); }
.lc-bar { display: flex; flex-direction: column; gap: 6px; padding: 8px 4px; font-size: 12px; flex-shrink: 0; }
.lc-row { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
/* sits right after the sliders (NOT pushed to the far right) so it doesn't shift when the compare
   dropdown changes width (e.g. "by attribute" adds selects) */
.lc-right { display: flex; align-items: center; gap: 10px; }
.lc-lbl { color: var(--cc-text-dim); font-size: 11px; text-transform: uppercase; letter-spacing: 0.04em; }
.lc-sep { width: 1px; height: 1.4rem; background: var(--cc-border); }
.lc-nm { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.lc-nm input[type="range"] { width: 5rem; }
.lc-val { min-width: 0.9rem; text-align: center; font-weight: 700; color: var(--cc-text); }
.seg-wrap { flex-wrap: wrap; }
/* wrapped seg buttons keep separators between rows too */
.seg-wrap button { border-left: 1px solid var(--cc-border); border-top: 1px solid var(--cc-border); }
.lc-compare { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: 6px; }
.lc-x { opacity: 0.6; }
.lc-pool { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 4px 8px; cursor: pointer; font-size: 11px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.seg button.on { background: #2d1b69; color: #c4b5fd; }
/* the board sizes to its grid (rowHeight × rows); the page's panel-scroll handles overflow */
.lc-body { display: flex; align-items: flex-start; gap: 8px; }
.lc-grid { flex: 1; display: grid; gap: 8px; padding: 4px; overflow: hidden; }
.lc-slot { position: relative; border: 1px dashed var(--cc-border); border-radius: 6px; overflow: hidden;
  display: flex; min-width: 0; min-height: 0; background: var(--cc-bg); }
.lc-slot.filled { border-style: solid; }
.lc-slot.active { border-color: #7c3aed; }
/* drag grip: sits in the docked panel header's empty spacer (left of the remove button) */
.lc-grip { position: absolute; top: 5px; right: 2.3rem; z-index: 7; width: 1.4rem; height: 1.4rem;
  display: flex; align-items: center; justify-content: center; cursor: grab; font-size: 0.62rem;
  color: var(--cc-text-dim); background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: 4px; opacity: 0.4; transition: opacity 0.1s, color 0.1s; }
.lc-slot:hover .lc-grip { opacity: 1; }
.lc-grip:hover { color: var(--cc-text); border-color: #7c3aed; }
.lc-grip:active { cursor: grabbing; }
/* hide the drag grips while capturing so they don't appear in the exported PDF */
.lc-grid.capturing .lc-grip { display: none; }
.lc-add { flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; }
.lc-add-hint { color: var(--cc-text-dim); font-size: 11px; opacity: 0.6; }
.lc-rail { flex-shrink: 0; width: 300px; overflow-y: auto; }
</style>
