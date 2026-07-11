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
import { computed, watch, ref, provide, nextTick, useTemplateRef, onMounted, onUnmounted } from 'vue'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import CanvasZoomControl from './CanvasZoomControl.vue'
import { plotHostToImageURL } from '../../plots/export'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useAnalysisLayoutStore, type SlotContent } from '../../stores/analysisLayout'
import { useSummaryData } from '../../composables/useSummaryData'
import { useClusterContext } from '../../composables/useClusterContext'
import { tkey, parseTkey } from '../../plots/series'
import { defaultVis, type VisProps } from '../../plots/plot'
import { UNIFORM_PRESETS, COMIC_PRESETS, uniform, A4_PORTRAIT_ASPECT, A4_LANDSCAPE_ASPECT } from '../../plots/layoutTemplates'
import type { SeriesTarget } from '../../plots/types'
import SummaryPanel from './SummaryPanel.vue'
import InteractivePanel from './InteractivePanel.vue'
import PlateBuilder from './PlateBuilder.vue'
import type { LayoutTemplate } from '../../plots/layoutTemplates'
import { INTERACTIVE_VIEWS } from './interactiveViews'
import SeriesPicker from './SeriesPicker.vue'
import PopulationManager from './PopulationManager.vue'
import { CLUSTER_PANELS, isClusterPanel } from '../../modules/cluster/clusterPanels'

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

// A4 sheet lock — undefined (older boards) reads as portrait so the "board is too wide" fix applies
// retroactively. In an A4 mode the board's WIDTH is derived from its height × the page aspect, so the
// on-screen layout matches the exported PDF page exactly (capturePage's measured aspect becomes exact).
const sheet = computed<'free' | 'a4-portrait' | 'a4-landscape'>({
  get: () => entry.value.sheet ?? 'a4-portrait', set: v => (entry.value.sheet = v) })
// only the plates that suit the current sheet orientation (all when Free); uniform grids are neutral
const platePresets = computed(() => {
  if (sheet.value === 'free') return COMIC_PRESETS
  const want = sheet.value === 'a4-portrait' ? 'portrait' : 'landscape'
  return COMIC_PRESETS.filter(t => (t.orient ?? 'any') === want || t.orient === 'any')
})

// grid size + height controls live in a ⚙ popover so the board bar doesn't crowd; close on outside click
const optsOpen = ref(false)
const optsRef = useTemplateRef<HTMLElement>('optsRef')
// custom plate builder popover (Phase 3)
const builderOpen = ref(false)
const builderRef = useTemplateRef<HTMLElement>('builderRef')
function applyCustomPlate(t: LayoutTemplate) { layout.applyTemplate(props.canvasKey, t); builderOpen.value = false }
function onDocClick(e: MouseEvent) {
  const t = e.target as Node
  if (optsOpen.value && optsRef.value && !optsRef.value.contains(t)) optsOpen.value = false
  if (builderOpen.value && builderRef.value && !builderRef.value.contains(t)) builderOpen.value = false
}
onMounted(() => document.addEventListener('mousedown', onDocClick))
onUnmounted(() => document.removeEventListener('mousedown', onDocClick))
// board natural (unscaled) size — height from rows×rowHeight; width from the A4 page aspect (null in
// Free mode, where the grid fills the available width).
const boardH = computed(() => rowHeight.value * entry.value.rows + 8 * (entry.value.rows - 1))
const boardW = computed<number | null>(() => {
  if (sheet.value === 'a4-portrait') return boardH.value * A4_PORTRAIT_ASPECT
  if (sheet.value === 'a4-landscape') return boardH.value * A4_LANDSCAPE_ASPECT
  return null
})
const gridStyle = computed(() => {
  const base: Record<string, string> = {
    gridTemplateColumns: entry.value.colTracks ?? `repeat(${entry.value.cols}, minmax(0, 1fr))`,
    gridTemplateRows: entry.value.rowTracks ?? `repeat(${entry.value.rows}, minmax(0, 1fr))`,
    height: `${boardH.value}px`,
  }
  // A4: lock width to height × page aspect so the box IS the page. Free: leave width to CSS (fills).
  if (boardW.value != null) { base.width = `${boardW.value}px`; base.flex = 'none' }
  return base
})

// ── visual zoom (fit-to-view, Word/Illustrator style) — A4 modes only (Free already fills width) ──
const canvasWrapRef = useTemplateRef<HTMLElement>('canvasWrapRef')
const { zoom, fitWidth, fitHeight, fitWidthIfOverflow, setZoom, reset: resetZoom } =
  useCanvasZoom(canvasWrapRef, () => ({ w: boardW.value, h: boardH.value }))
provide(CANVAS_ZOOM_KEY, zoom)   // docked panels don't pixel-drag, but keep the contract uniform
// neutralise zoom during PDF capture so the measured slot rects are at full 1:1 size (the transform
// would otherwise scale getBoundingClientRect and throw off the hi-res composite)
const effZoom = computed(() => (capturing.value ? 1 : zoom.value))
const zoomWrapStyle = computed(() => boardW.value != null
  ? { width: `${boardW.value * effZoom.value}px`, height: `${boardH.value * effZoom.value}px`, margin: '0 auto' }
  : { width: '100%' })
const gridZoomStyle = computed(() => (boardW.value != null && effZoom.value !== 1)
  ? { transform: `scale(${effZoom.value})`, transformOrigin: 'top left' } : {})
// first render (and image switch): fit-to-width if the board would overflow, so the whole board is
// visible without hiding the sidebar; a board that already fits stays at 100%.
watch(imageUid, () => nextTick(fitWidthIfOverflow), { immediate: true })

// shared summary-plot data + view-state (same composable the free-floating canvas uses)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys, popType,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({
  projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module,
  shared: computed(() => entry.value.shared),
  // the board is mixed-popType: point the picker at the ACTIVE summary slot's spec so it surfaces that
  // plot's popType (each population-summary spec carries one popType — split per module page).
  activeSpecId: computed(() => {
    const c = entry.value.contents[entry.value.activeIndex]
    return c && c.kind === 'summary' ? c.ref : null
  }),
})

// ── slot content: active slot, add/clear, drag-swap ──────────────────────────────────────────────
const activeContent = computed<SlotContent | null>(() => entry.value.contents[entry.value.activeIndex] ?? null)
type PState = { sel: string[]; vis: VisProps; [k: string]: unknown }
const st = (c: SlotContent): PState => c.state as PState

// self-contained interactive views (read their own context / pops)
const ANALYSIS_VIEWS = ['gatingStrategy']
const interactiveOptions = computed(() => ANALYSIS_VIEWS.filter(k => k in INTERACTIVE_VIEWS).map(k => ({ key: k, label: INTERACTIVE_VIEWS[k].label })))
// CLUSTERING plots — one clustering run per board (see useClusterContext / ANALYSIS_CANVAS_PLAN Phase G):
// the interactive UMAP (INTERACTIVE_VIEWS) + the cluster panels (CLUSTER_PANELS registry). Discovered
// GENERICALLY via each registry's `analysisBoard` flag — no per-plot wiring here. A cluster slot is any
// interactive slot whose ref is `umap` OR a registered cluster panel.
const isClusterSlot = (c: SlotContent | null): boolean =>
  !!c && c.kind === 'interactive' && (c.ref === 'umap' || isClusterPanel(c.ref))
const clusterOptions = computed(() => {
  const out: { key: string; label: string }[] = []
  if (INTERACTIVE_VIEWS.umap?.analysisBoard) out.push({ key: 'umap', label: INTERACTIVE_VIEWS.umap.label })
  for (const [key, def] of Object.entries(CLUSTER_PANELS)) {
    if (!def.analysisBoard) continue
    if (def.trackOnly && clustPopType.value !== 'trackclust') continue          // HMM = track runs only
    if (def.needsCols === 'hmmState' && !clustHmmStateCols.value.length) continue
    if (def.needsCols === 'hmmTransition' && !clustHmmTransitionCols.value.length) continue
    out.push({ key, label: def.label })
  }
  return out
})
// image-content views (napari screenshot slots) — grouped separately in the picker
const IMAGE_VIEWS = ['filmstrip']
const imageOptions = computed(() => IMAGE_VIEWS.filter(k => k in INTERACTIVE_VIEWS).map(k => ({ key: k, label: INTERACTIVE_VIEWS[k].label })))

// the "+ Plot" value is "summary:<specId>" or "interactive:<viewKey>"
function addPlot(i: number, val: string) {
  if (!val) return
  const sep = val.indexOf(':'); const kind = val.slice(0, sep); const ref = val.slice(sep + 1)
  if (kind === 'summary') layout.setContent(props.canvasKey, i, { kind: 'summary', ref, state: { specId: ref, sel: [], vis: defaultVis() } })
  else if (kind === 'interactive') {
    // cluster slots carry a `hl` (highlight) bag; panels self-seed the rest (e.g. heatmap features)
    const state = ref === 'umap' ? { labels: true, hl: [] } : isClusterPanel(ref) ? { hl: [] } : {}
    layout.setContent(props.canvasKey, i, { kind: 'interactive', ref, state })
  }
  layout.setActive(props.canvasKey, i)
}
function clearSlot(i: number) { layout.setContent(props.canvasKey, i, null) }

// per-slot title (figure caption) — persisted in the slot's own state bag (survives navigation with the
// rest of the layout). Empty by default; drawn above the plot in the PDF export.
const slotTitle = (i: number): string => (entry.value.contents[i]?.state.title as string) ?? ''
function setSlotTitle(i: number, v: string) { const c = entry.value.contents[i]; if (c) c.state.title = v }

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
// fall back to defaultVis() when the active slot has no local vis yet (matches ClusterPlots) — else the
// pop manager's `vis` is undefined and the whole PlotOptions styling block is hidden (the "cluster-tracks
// manager has no plot params" bug).
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activeContent.value ? (st(activeContent.value).vis ?? defaultVis()) : defaultVis()))
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

// ── cluster context: ONE clustering run per board (board-level popType + suffix in the shared bag) so
// the singleton gating store is driven unambiguously; only active when a cluster slot exists. ─────────
const hasClusterSlot = computed(() => entry.value.contents.some(isClusterSlot))
const clustPopType = computed<'clust' | 'trackclust'>({
  get: () => (entry.value.shared.clustPopType as 'clust' | 'trackclust') ?? 'clust',
  set: v => (entry.value.shared.clustPopType = v) })
const clustSuffix = computed<string>({
  get: () => (entry.value.shared.clustSuffix as string) ?? 'default',
  set: v => (entry.value.shared.clustSuffix = v) })
const { suffixes: clustSuffixes, clusterIds: clustClusterIds, validUids: clustValidUids,
        featureOptions: clustFeatureOptions, nameMap: clustNameMap,
        hmmStateCols: clustHmmStateCols, hmmTransitionCols: clustHmmTransitionCols, shownPopsFor } =
  useClusterContext({ projectUid, imageUids: computed(() => props.imageUids),
                      popType: clustPopType, suffix: clustSuffix, enabled: hasClusterSlot })

// cluster HIGHLIGHT — global (shared, one run per board) or per-slot (local), same scope as summary
const clustHl = computed<string[]>({ get: () => (entry.value.shared.clustHl as string[]) ?? [], set: v => (entry.value.shared.clustHl = v) })
const clustHlOf = (c: SlotContent): string[] => (st(c).hl as string[]) ?? []
const panelClustHl = (c: SlotContent) => scope.value === 'global' ? clustHl.value : clustHlOf(c)
const activeClustHl = computed(() => scope.value === 'global' ? clustHl.value
  : (activeContent.value ? clustHlOf(activeContent.value) : []))
const activeIsCluster = computed(() => isClusterSlot(activeContent.value))
function toggleClustHl(path: string) {
  if (scope.value === 'global') clustHl.value = toggle(clustHl.value, path)
  else if (activeContent.value) st(activeContent.value).hl = toggle(clustHlOf(activeContent.value), path)
}

// context handed to an interactive slot: cluster views get the board cluster run + shown pops; the
// self-contained views (gating strategy, filmstrip) just get the image/project context.
function ctxFor(c: SlotContent) {
  if (isClusterSlot(c)) return {
    projectUid: projectUid.value, imageUids: clustValidUids.value, setUid: setUid.value,
    popType: clustPopType.value, suffix: clustSuffix.value,
    shownPops: shownPopsFor(panelClustHl(c)), vis: panelVis(c),
  }
  return { projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value, vis: panelVis(c) }
}

// props for a cluster PANEL slot (CLUSTER_PANELS): the common bag + the panel-specific props its registry
// entry maps from the shared cluster context — so the slot renders with one generic <component v-bind>.
function clusterPanelProps(i: number) {
  const c = entry.value.contents[i]!
  const ctx = { featureOptions: clustFeatureOptions.value, nameMap: clustNameMap.value,
                hmmStateCols: clustHmmStateCols.value, hmmTransitionCols: clustHmmTransitionCols.value }
  return {
    index: i, active: i === entry.value.activeIndex, arrange: null, docked: true,
    projectUid: projectUid.value, setUid: setUid.value, imageUids: clustValidUids.value,
    popType: clustPopType.value, suffix: clustSuffix.value,
    shownPops: shownPopsFor(panelClustHl(c)), vis: panelVis(c), state: c.state,
    persistKey: `${props.canvasKey}:slot:${i}`,
    ...(CLUSTER_PANELS[c.ref].props?.(ctx) ?? {}),
  }
}

// prune vanished pops from every slot's local selection (the composable prunes the global one). Guard on
// a non-empty segPops — it's transiently [] during load/image-switch, and pruning then would wipe (and
// then persist-empty) a restored per-slot selection. popType-AWARE (mixed board): segPops holds only the
// active slot's popType, so only prune keys of THAT popType — else selecting e.g. a trackclust slot would
// wipe the live/track selections of the other (track-measure) plots.
watch(segPops, () => {
  if (!segPops.value.length) return
  const valid = validSelKeys.value, pt = popType.value
  const keep = (k: string) => parseTkey(k).popType !== pt || valid.has(k)
  for (const c of entry.value.contents) if (c && Array.isArray(st(c).sel)) st(c).sel = st(c).sel.filter(keep)
})

// ── PDF export: capture each filled slot to a PNG (hiding the drag grips), keyed by its grid-area ──
const gridRef = useTemplateRef<HTMLElement>('gridRef')
const capturing = ref(false)
function labelFor(c: SlotContent): string {
  if (c.kind === 'summary') return specById.value[c.ref]?.label ?? c.ref
  if (isClusterPanel(c.ref)) return CLUSTER_PANELS[c.ref].label
  return INTERACTIVE_VIEWS[c.ref]?.label ?? c.ref
}
// panel instances by slot index, so we can ask each for a PLOT-ONLY, LIGHT-theme image (no chrome) and
// pull the summary plot's aggregated CSV. Both panel types expose exportImage(); summary also getCsv().
type SummaryRef = { getCsv(): string | null; exportImage(): Promise<string | null> }
type ExportRef = { exportImage(): Promise<string | null> }
const summaryRefs = new Map<number, SummaryRef>()
const interactiveRefs = new Map<number, ExportRef>()
function setSummaryRef(i: number, el: unknown) { if (el) summaryRefs.set(i, el as SummaryRef); else summaryRefs.delete(i) }
function setInteractiveRef(i: number, el: unknown) { if (el) interactiveRefs.set(i, el as ExportRef); else interactiveRefs.delete(i) }

type PdfSlotOut = { rect: { x: number; y: number; w: number; h: number }; png: string | null; name: string; title?: string; csv?: string | null }
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
      // summary + cluster-heatmap panels expose exportImage()/getCsv() (summaryRefs); other interactive
      // views expose exportImage() (interactiveRefs); anything else → white-ground DOM snapshot.
      const summaryLike = c.kind === 'summary' || isClusterPanel(c.ref)
      let png: string | null = null
      if (summaryLike) png = await summaryRefs.get(i)?.exportImage() ?? null
      else if (c.kind === 'interactive') png = await interactiveRefs.get(i)?.exportImage?.() ?? null
      if (!png) png = await plotHostToImageURL(el, '#ffffff')
      const csv = summaryLike ? (summaryRefs.get(i)?.getCsv() ?? null) : null
      const sr = el.getBoundingClientRect()
      const rect = { x: (sr.left - gr.left) / gr.width, y: (sr.top - gr.top) / gr.height,
                     w: sr.width / gr.width, h: sr.height / gr.height }
      slots.push({ rect, png, name: labelFor(c), title: (c.state.title as string) || undefined, csv })
    }
  } finally { capturing.value = false }
  return { aspect: gr.width / Math.max(1, gr.height), slots }
}
// the shown (aggregated) data for every summary slot — for the standalone CSV export (data → Prism)
function collectCsvs(): { name: string; csv: string | null }[] {
  const out: { name: string; csv: string | null }[] = []
  for (let i = 0; i < entry.value.contents.length; i++) {
    const c = entry.value.contents[i]
    if (c && (c.kind === 'summary' || isClusterPanel(c.ref))) out.push({ name: labelFor(c), csv: summaryRefs.get(i)?.getCsv() ?? null })
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
          <!-- custom grid size + slot height, tucked into a ⚙ popover to keep the bar tidy -->
          <div ref="optsRef" class="lc-opts">
            <button class="lc-gear" :class="{ on: optsOpen }" @click="optsOpen = !optsOpen"
                    v-tooltip.bottom="'Grid size & slot height'"><i class="pi pi-sliders-h" /></button>
            <div v-if="optsOpen" class="lc-pop">
              <label class="lc-pop-row"><span>cols</span>
                <input type="range" min="1" max="6" :value="entry.cols"
                       @input="layout.applyTemplate(canvasKey, uniform(+($event.target as HTMLInputElement).value, entry.rows))" />
                <span class="lc-val">{{ entry.cols }}</span></label>
              <label class="lc-pop-row"><span>rows</span>
                <input type="range" min="1" max="6" :value="entry.rows"
                       @input="layout.applyTemplate(canvasKey, uniform(entry.cols, +($event.target as HTMLInputElement).value))" />
                <span class="lc-val">{{ entry.rows }}</span></label>
              <label class="lc-pop-row"><span>height</span>
                <input type="range" min="160" max="720" step="10" :value="rowHeight"
                       @input="rowHeight = +($event.target as HTMLInputElement).value" />
                <span class="lc-val">{{ rowHeight }}</span></label>
            </div>
          </div>
          <!-- A4 sheet lock: keep the board at page proportions (WYSIWYG with the PDF) or let it fill -->
          <div class="seg" v-tooltip.bottom="'Sheet — A4 locks the board to page proportions (what you see is the exported page); Free fills the width'">
            <button :class="{ on: sheet === 'a4-portrait' }" @click="sheet = 'a4-portrait'">A4 ↕</button>
            <button :class="{ on: sheet === 'a4-landscape' }" @click="sheet = 'a4-landscape'">A4 ↔</button>
            <button :class="{ on: sheet === 'free' }" @click="sheet = 'free'">Free</button>
          </div>
          <!-- fit-to-view zoom (visual only; the exported page is unchanged) -->
          <CanvasZoomControl v-if="sheet !== 'free'" :zoom="zoom"
                             @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
          <!-- clustering run: ONE per board (drives all cluster slots + the cluster pop manager) -->
          <div v-if="hasClusterSlot" class="lc-clust" v-tooltip.bottom="'Clustering run shown by this board’s cluster plots'">
            <span class="lc-lbl">cluster</span>
            <select v-model="clustPopType">
              <option value="clust">cells</option>
              <option value="trackclust">tracks</option>
            </select>
            <select v-model="clustSuffix">
              <option v-if="!clustSuffixes.length" :value="clustSuffix">{{ clustSuffix }}</option>
              <option v-for="s in clustSuffixes" :key="s" :value="s">{{ s }}</option>
            </select>
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
          <div class="seg seg-wrap" v-tooltip.bottom="'Comic plates — varied-size panels, matched to the sheet orientation'">
            <button v-for="t in platePresets" :key="t.id" @click="layout.applyTemplate(canvasKey, t)"
                    :class="{ on: entry.slotAreas.join('|') === t.slots.join('|') }">{{ t.label }}</button>
          </div>
          <!-- custom plate builder: drag cells to merge into varied-size panels -->
          <div ref="builderRef" class="lc-opts">
            <button class="cc-btn cc-btn-ghost lc-custom" :class="{ on: builderOpen }" @click="builderOpen = !builderOpen"
                    v-tooltip.bottom="'Build a custom plate — drag cells to merge, click a merge to split'">
              <i class="pi pi-th-large" /> Custom…</button>
            <div v-if="builderOpen" class="lc-pop">
              <PlateBuilder :cols="entry.cols" :rows="entry.rows" :slot-areas="entry.slotAreas"
                            @apply="applyCustomPlate" @cancel="builderOpen = false" />
            </div>
          </div>
        </div>
      </div>

      <div class="lc-body">
        <!-- scroll viewport → .lc-zoom (scaled footprint, centred) → the grid (visually scaled) -->
        <div ref="canvasWrapRef" class="lc-canvas-wrap">
        <div class="lc-zoom" :style="zoomWrapStyle">
        <div ref="gridRef" class="lc-grid" :class="{ capturing }" :style="[gridStyle, gridZoomStyle]">
          <!-- reorder drag: the drag SOURCE is the panel header's drag icon (CanvasPanel, docked);
               its native dragstart bubbles here, so the grip lives IN the header (aligned with the
               other buttons) instead of a fragile absolute overlay that collided with the pin. -->
          <div v-for="(area, i) in entry.slotAreas" :key="i" class="lc-slot"
               :class="{ active: i === entry.activeIndex, filled: !!entry.contents[i] }"
               :style="{ gridArea: area }"
               @dragstart="dragFrom.i = i" @dragend="dragFrom.i = -1"
               @dragover.prevent @drop.prevent="onDrop(i)"
               @mousedown="layout.setActive(canvasKey, i)">
            <!-- per-slot title (figure caption) — persisted in the slot's state, drawn above the plot
                 in the PDF export (pdf.ts). Only for filled slots. -->
            <input v-if="entry.contents[i]" class="lc-slot-cap" :value="slotTitle(i)"
                   @input="setSlotTitle(i, ($event.target as HTMLInputElement).value)"
                   @mousedown.stop placeholder="Add a title…" />
            <div class="lc-slot-plot">
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
            <!-- cluster PANEL (heatmap / HMM …) — rendered GENERICALLY from the CLUSTER_PANELS registry
                 (docked, board's single cluster run); no per-plot branch -->
            <component v-else-if="entry.contents[i] && isClusterPanel(entry.contents[i]!.ref)"
                       :is="CLUSTER_PANELS[entry.contents[i]!.ref].component" :ref="(el: unknown) => setSummaryRef(i, el)"
                       v-bind="clusterPanelProps(i)"
                       @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)" />
            <!-- interactive plot (UMAP / gating-strategy / …) -->
            <InteractivePanel v-else-if="entry.contents[i]?.kind === 'interactive' && INTERACTIVE_VIEWS[entry.contents[i]!.ref]"
                              :ref="el => setInteractiveRef(i, el)"
                              :index="i" :active="i === entry.activeIndex" :docked="true"
                              :view="entry.contents[i]!.ref" :context="ctxFor(entry.contents[i]!)" :state="entry.contents[i]!.state"
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
                <optgroup v-if="clusterOptions.length" label="Clustering">
                  <option v-for="v in clusterOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
                <optgroup v-if="imageOptions.length" label="Image">
                  <option v-for="v in imageOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
              </select>
              <span class="lc-add-hint">empty slot</span>
            </div>
            </div>
          </div>
        </div>
        </div>
        </div>

        <!-- docked pop manager (control, not content — excluded from PDF). Follows the ACTIVE slot:
             cluster PopulationManager for a cluster slot, else the summary SeriesPicker. -->
        <div class="lc-rail">
          <PopulationManager v-if="activeIsCluster" :docked="true" :readonly="true"
                             :selected="''" :highlighted="activeClustHl" :scope="scope"
                             :line-width="1" :gate-labels="false" :axis-from-zero="false"
                             :pop-type="clustPopType" :cluster-ids="clustClusterIds[clustSuffix] ?? []"
                             :suffix="clustSuffix" :vis="activeVis"
                             @update:scope="scope = $event" @update:vis="setVis" @toggle-highlight="toggleClustHl" />
          <SeriesPicker v-else :groups="segPops" :selected="activeSel" :scope="scope" :vis="activeVis" :docked="true"
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
.lc-clust { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: 6px; }
.lc-clust select { font-size: 11px; }
/* ⚙ grid-size / height popover */
.lc-opts { position: relative; display: inline-flex; }
.lc-gear { display: inline-flex; align-items: center; justify-content: center; width: 1.7rem; height: 1.6rem;
  border: 1px solid var(--cc-border); border-radius: 5px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.72rem; }
.lc-gear:hover, .lc-gear.on { color: var(--cc-text); border-color: #7c3aed; }
.lc-custom { font-size: 11px; padding: 0.22rem 0.55rem; }
.lc-custom.on { color: var(--cc-text); border-color: #7c3aed; }
.lc-pop { position: absolute; top: calc(100% + 4px); left: 0; z-index: 20; min-width: 13rem;
  display: flex; flex-direction: column; gap: 8px; padding: 10px; background: var(--cc-surface-1);
  border: 1px solid var(--cc-border); border-radius: 6px; box-shadow: 0 6px 18px rgba(0,0,0,0.35); }
.lc-pop-row { display: flex; align-items: center; gap: 8px; font-size: 12px; color: var(--cc-text-dim); }
.lc-pop-row span:first-child { width: 3rem; }
.lc-pop-row input[type="range"] { flex: 1; }
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
/* scroll viewport for the board. .lc-zoom holds the (visually scaled) footprint: fixed-size + centred
   via margin auto for an A4 board, full-width for a Free board (styled inline via zoomWrapStyle). */
.lc-canvas-wrap { flex: 1; min-width: 0; overflow: auto; }
.lc-zoom { display: block; }
.lc-grid { flex: 1; display: grid; gap: 8px; padding: 4px; overflow: hidden; }
.lc-slot { position: relative; border: 1px dashed var(--cc-border); border-radius: 6px; overflow: hidden;
  display: flex; flex-direction: column; min-width: 0; min-height: 0; background: var(--cc-bg); }
/* per-slot title (figure caption): a plain-looking, centred, editable line above the plot */
.lc-slot-cap { flex: 0 0 auto; width: 100%; box-sizing: border-box; border: none; background: transparent;
  color: var(--cc-text); font-size: 12px; font-weight: 600; text-align: center; padding: 3px 6px 1px; }
.lc-slot-cap::placeholder { color: var(--cc-text-dim); font-weight: 400; opacity: 0.55; }
.lc-slot-cap:focus { outline: none; background: var(--cc-surface-2); }
/* the plot area fills the rest of the slot (was the slot itself before the caption was added) */
.lc-slot-plot { flex: 1; min-width: 0; min-height: 0; display: flex; position: relative; }
.lc-slot.filled { border-style: solid; }
/* selection = amber, matching CanvasPanel .panel.active and every module page (was a clashing violet).
   A FILLED slot's panel already draws the amber border + glow, so don't double it there — only an empty
   slot needs its own amber selection border. */
.lc-slot.active { border-color: #ff8c1a; }
.lc-slot.filled.active { border-color: var(--cc-border); }
/* reorder drag handle now lives IN the panel header (CanvasPanel docked drag icon); its native
   dragstart bubbles to .lc-slot (@dragstart above). No absolute overlay grip here anymore. */
.lc-add { flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; }
.lc-add-hint { color: var(--cc-text-dim); font-size: 11px; opacity: 0.6; }
/* stick to the top of the scroll viewport so the pop manager stays reachable as the (tall) board
   scrolls past — otherwise you must scroll back up to change the selection. align-self so the sticky
   box hugs the top of the flex row; its own overflow-y scrolls a manager taller than the viewport. */
.lc-rail { flex-shrink: 0; width: 300px; overflow-y: auto; padding-right: 10px; box-sizing: content-box;
  position: sticky; top: 8px; align-self: flex-start; max-height: calc(100vh - 16px); }
</style>
