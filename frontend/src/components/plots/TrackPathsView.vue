<!--
  The napari tracks layer, as a plot: every track's path, in µm, optionally coloured by one of its
  own properties — for a COHORT, not one image.

  Until now tracks could only be LOOKED at, in napari — `lib/tips.ts` said so outright. That is fine
  for judging one cell and useless for a figure: a viewer screenshot cannot be recoloured by speed,
  cannot be put beside the same field from another mouse, and cannot leave the app as vectors. This
  view is the plot half of that pair, so the Analysis board can carry tracks next to the populations
  they came from.

  **It compares like every other plot on the board.** The board's `compare` selector (this image / per
  image / pooled / by attribute) and its population rail drive this plot exactly as they drive a summary
  plot: one GROUP per (images × population) cell. Nothing here is a private control —
  `plots/trackGroups.ts` translates the board's own controls into the query, and the server resolves the
  grouping once (`app/src/tracking/track_cohort.jl`). Before that, a two-condition figure meant two
  screenshots.

  **Three modes, because "the tracks" is three different questions.**
  - *Paths* — where the cells actually were. The spatial picture, the one napari shows.
  - *Star* — every track translated to a common origin (celltrackR's `plotTracks`/rose family, Wortel
    et al. 2021, doi:10.1016/j.crmeth.2021.100006). Position is discarded and SHAPE survives, which
    is what you compare between conditions: directed migration fans out, random walk fills a disc.
  - *Rose* — one straight arrow per track, start to end. Net displacement, when hundreds of paths
    have turned the star into a scribble.

  **A cohort always SPLITS, never overlays.** One cell per group, named by a title — never a colour and a
  legend. Two conditions in one box is unreadable in all three modes (paths have unrelated coordinate
  frames; a star/rose fan turns to scribble), and identifying a group by colour would spend the colour
  channel that `colorBy` exists for. So `facetPlan` splits whatever `Facet by` says, and says so.

  The axes are always SQUARE (`pathDomain` + `facetBox`) — a track plot stretched to fill a panel turns a
  straight run into a diagonal, which destroys the only thing these modes exist to show. Facets share
  ONE domain for the same reason: two conditions on two scales are not a comparison, and their cells stay
  square in a grid (`plots/facetGrid.ts`) rather than becoming a row of slivers.

  Geometry comes from `GET /api/tracking/paths` in the same wire shape the correction worklist reads
  (`plots/trackPaths.ts` parses either). The colour-by vocabulary is NOT a second list: it comes from
  `/api/gating/channels?popType=track`, the same call the track-gating axes use, so the two pickers
  cannot drift apart. Colouring is limited to columns the per-track table holds directly (the motility
  measures) — a cell measure would need an aggregate chosen first, and this plot has nowhere to ask.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import PlotNotice from '../canvas/PlotNotice.vue'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf } from '../../plots/export'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { usePlotResize } from '../../composables/usePlotResize'
import { distinctColors, facetMode, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import { popTypeOptions, popTypeLabel, resolvePopType, type PopTypeOption } from '../../plots/popTypes'
import { facetGrid, facetSlot, facetBox } from '../../plots/facetGrid'
import {
  pathDomain, displacementVectors, pathCsvRows, groupedPathPoints,
  type GroupedPathPoint, type PathGroup,
} from '../../plots/trackPaths'
import {
  cohortParams, cohortKey, facetPlan, groupLabel, cohortNote,
  type CompareMode, type TrackGroupMeta,
} from '../../plots/trackGroups'
import { resolveTrackValueName } from '../../plots/trackDiagnostics'
import type { SeriesTarget } from '../../plots/types'

type Mode = 'paths' | 'star' | 'rose'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // the board's comparison context — absent on a module page, which is then exactly the old behaviour
  vis?: VisProps
  series?: SeriesTarget[]
  compareMode?: CompareMode
  groupAttr?: string[]
  poolGroups?: boolean
  popTypes?: PopTypeOption[]
  // every user-settable option lives in the panel's persisted bag, not a bare ref
  state: { valueName?: string; mode?: Mode; colorBy?: string; limit?: number; popType?: string }
}>()

// the FIRST selected image answers "which segmentations / which columns" — the pickers are about the
// vocabulary, and asking every image for it would offer columns half the cohort lacks
const imageUid = computed(() => props.imageUids[0] ?? '')
// a TRACKED segmentation, never 'default' or the active one — see resolveTrackValueName
const trackedNames = ref<string[]>([])
const activeName = ref('')            // the segmentation the rest of the app is pointed at
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})
const mode = computed({ get: () => props.state.mode ?? 'paths',
                        set: v => (props.state.mode = v) })
const colorBy = computed({ get: () => props.state.colorBy ?? '',
                           set: v => (props.state.colorBy = v) })
const limit = computed({ get: () => props.state.limit ?? 500,
                         set: v => (props.state.limit = v) })
// the population FAMILY, one per plot (docs/PLOTS.md) — the rail lists whichever this is, so the pick
// resolves through the same `resolvePopType` the rail uses and cannot disagree with it
const familyOptions = computed<PopTypeOption[]>(() =>
  props.popTypes?.length ? popTypeOptions({ dataSource: { popTypes: props.popTypes } }) : [])
const popType = computed({
  get: () => (familyOptions.value.length
    ? resolvePopType({ dataSource: { popTypes: familyOptions.value } }, props.state.popType)
    : 'live'),
  set: v => (props.state.popType = v),
})
const vis = computed(() => props.vis ?? DEFAULT_VIS)

interface PathGroupResponse extends TrackGroupMeta {
  total: number; shown: number; stepScale: number
  colorBy: string; colorKind: 'numeric' | 'categorical' | 'none'
  values: Record<string, number | string | null>
  paths: PathGroup['paths']
}
interface PathsResponse {
  valueName: string; tracked: boolean; dropped: number
  total: number; shown: number
  colorBy: string; colorKind: 'numeric' | 'categorical' | 'none'
  groups: PathGroupResponse[]
}

const data = ref<PathsResponse | null>(null)
const valueNames = ref<string[]>([])
const colorOptions = ref<string[]>([])
const loading = ref(false)
const error = ref('')

const MODES: ChipOption[] = [
  { value: 'paths', label: 'Paths' },
  { value: 'star', label: 'Star' },
  { value: 'rose', label: 'Rose' },
]
const LIMITS = [100, 500, 2000]

/** The track vocabulary — value names + the per-track columns, from the track-gating source. */
async function loadColumns() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; colorOptions.value = []; return }
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}&popType=track` +
              (valueName.value ? `&valueName=${encodeURIComponent(valueName.value)}` : '')
    const r = await fetch(`/api/gating/channels?${q}`)
    if (!r.ok) throw new Error(`HTTP ${r.status}`)
    const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]
                                  valueName?: string; columns?: string[] }
    valueNames.value = d.valueNames ?? []
    trackedNames.value = d.trackedValueNames ?? []
    activeName.value = d.valueName ?? ''
    // `columns` for popType=track IS the motility set (speed, displacement, straightness, …) — one
    // value per track already. NOT `trackAggregates`, which is the list of aggregate SUFFIXES
    // ("mean", "median", …); offering those as columns would have put "mean" in this picker.
    colorOptions.value = d.columns ?? []
    if (colorBy.value && !colorOptions.value.includes(colorBy.value)) colorBy.value = ''
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
}

/** The cohort query: the board's compare/population context, translated once (plots/trackGroups.ts). */
const cohort = computed(() => ({
  imageUids: props.imageUids, compareMode: props.compareMode, groupAttr: props.groupAttr,
  poolGroups: props.poolGroups, series: props.series, popType: popType.value,
}))

async function load() {
  if (!props.projectUid || !imageUid.value) { data.value = null; return }
  loading.value = true; error.value = ''
  try {
    const p = cohortParams(cohort.value)
    p.set('projectUid', props.projectUid)
    if (valueName.value) p.set('valueName', valueName.value)
    p.set('colorBy', colorBy.value)
    p.set('limit', String(limit.value))
    const r = await fetch(`/api/tracking/paths?${p}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    data.value = d as PathsResponse
    // the server falls a column this cohort lacks back to uncoloured — follow it rather than keeping
    // a picker that claims a colour the plot is not using
    if (data.value.colorBy !== colorBy.value) colorBy.value = data.value.colorBy
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    data.value = null
  } finally {
    loading.value = false
    await nextTick(); plotBox.redraw()
  }
}

onMounted(async () => { await loadColumns(); await load() })
// a tracking or correction run rewrites exactly what this draws — the ONE refresh chokepoint, so the
// global autoRefreshOnTask setting governs it like every other plot
useDataRefresh(() => props.imageUids, () => { loadColumns(); load() })
watch([() => props.projectUid, imageUid], async () => { await loadColumns(); await load() })
// one watcher over the whole cohort query, so a compare-mode / population / family change refetches
// without a per-prop list that can fall behind the params it builds
watch([() => cohortKey(cohort.value), valueName, colorBy, limit], load)

// ── drawing ───────────────────────────────────────────────────────────────────
const host = useTemplateRef<HTMLElement>('host')
const forceLight = ref(false)
let Plot: typeof import('@observablehq/plot') | null = null
let node: SVGElement | null = null

const groups = computed<PathGroupResponse[]>(() => data.value?.groups ?? [])
/** Position is discarded for star and rose — both start every track from a common origin. */
const normalise = computed(() => mode.value !== 'paths')
const rows = computed<GroupedPathPoint[]>(() => groupedPathPoints(
  groups.value.map(g => ({ key: g.key, label: g.label, paths: g.paths, values: g.values })),
  { normalise: normalise.value }))
const coloured = computed(() => data.value?.colorKind === 'numeric' || data.value?.colorKind === 'categorical')
// a plan, not a boolean: a cohort always splits, and the plan carries the note that says so when the
// `Facet by` control asked for one box
const plan = computed(() => facetPlan(facetMode(vis.value), groups.value.length))
const slots = computed(() => {
  const grid = facetGrid(groups.value.length)
  const m = new Map<string, { fx: number; fy: number }>()
  groups.value.forEach((g, i) => m.set(g.key, facetSlot(i, grid.cols)))
  return m
})
const note = computed(() => cohortNote(data.value?.shown ?? 0, data.value?.total ?? 0,
                                       data.value?.dropped ?? 0, groups.value.length))

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  // the host is emptied and refilled in ONE step at the end (`replaceChildren`). Removing the old node
  // up front and appending the new one after an `await` lets two renders interleave — the resize
  // observer fires while a data-driven render is mid-import — and leaves TWO figures in the box, which
  // reads as one plot drawn with two different colour scales.
  const pts = rows.value
  if (!pts.length) { host.value.replaceChildren(); node = null; return }

  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : 'white'
  const facet = plan.value.facet
  const grid = facetGrid(facet ? groups.value.length : 1)
  const ML = 44, MB = 34, MT = 12, MR = 12
  const avail = Math.max(160, Math.min(host.value.clientWidth || 320, host.value.clientHeight || 320))
  const box = facetBox({ cols: grid.cols, rows: grid.rows, w: host.value.clientWidth || avail,
                         h: host.value.clientHeight || avail, mx: ML + MR, my: MB + MT })

  // rose reduces each track to its net vector; the domain must be computed on what is drawn
  const vectors = mode.value === 'rose' ? displacementVectors(pts) : []
  const domain = pathDomain(mode.value === 'rose'
    ? vectors.flatMap(v => ([v, { ...v, x: 0, y: 0 }]))
    : pts)

  // the facet channels, when faceting: a (column, row) pair per group so six conditions are a grid and
  // not six slivers — and a text mark per cell, since a facet HEADER is per column and cannot name one
  const fxOf = (r: { g: string }) => slots.value.get(r.g)?.fx ?? 0
  const fyOf = (r: { g: string }) => slots.value.get(r.g)?.fy ?? 0
  const fch = facet ? { fx: fxOf, fy: fyOf } : {}

  // TWO colour vocabularies, and the group is not one of them: the facet title names the group, so the
  // colour channel belongs entirely to `colorBy` (and to per-track identity when nothing is picked).
  const stroke = coloured.value ? 'v' : 'track'
  const tracks = [...new Set(pts.map(p => p.track))]
  const colourScale = coloured.value
    ? (data.value?.colorKind === 'numeric'
        ? { scheme: 'turbo' as const, legend: true as const, label: colorBy.value }
        : { legend: true as const, label: colorBy.value })
    // one colour per track and a legend of 500 entries is not a legend
    : { domain: tracks, range: distinctColors(tracks.length), legend: false as const }

  const marks = mode.value === 'rose'
    ? [
        Plot.link(vectors, { x1: 0, y1: 0, x2: 'x', y2: 'y', stroke,
                             strokeWidth: 1.2, markerEnd: 'arrow', ...fch }),
      ]
    : [
        Plot.line(pts, { x: 'x', y: 'y', z: 'track', stroke, strokeWidth: 1.2, ...fch }),
        // where each track STARTS — without it a path is a line with no direction
        Plot.dot(pts.filter(p => p.i === 0), { x: 'x', y: 'y', fill: stroke, r: 1.8, ...fch }),
      ]
  if (facet) {
    marks.push(Plot.text(groups.value.map(g => ({ ...slots.value.get(g.key)!, t: groupLabel(g) })),
                         { fx: 'fx', fy: 'fy', text: 't', frameAnchor: 'top-left',
                           dx: 4, dy: 4, fill: fg, fontSize: 10 }))
  }

  node = Plot.plot({
    width: box.width, height: box.height,
    marginLeft: ML, marginBottom: MB, marginTop: MT, marginRight: MR,
    style: { background: bg, color: fg, fontSize: '11px' },
    // µm on both axes, same span — see the header: a stretched track plot is a wrong track plot, and
    // facets share ONE domain so the small multiple is a comparison
    x: { domain: domain?.x, label: 'x (µm)', grid: true },
    y: { domain: domain?.y, label: 'y (µm)', grid: true },
    color: colourScale,
    ...(facet ? { fx: { axis: null }, fy: { axis: null } } : {}),
    marks,
  }) as SVGElement
  host.value.replaceChildren(node)
}

// the observer's callback appends into the element it observes — see usePlotResize for why
// that loops, and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { node?.remove(); node = null })
watch([mode, rows, plan], () => nextTick(() => plotBox.redraw()))

// ── export (the generic panel contract — plots/export.ts, same helpers as the other views) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `tracks_${mode.value}_${valueName.value || 'default'}`.replace(/[^\w.-]+/g, '_'))
const csv = () => (rows.value.length
  ? rowsToCsv(pathCsvRows(rows.value, {}, colorBy.value || 'value'))
  : null)
function exportAs(kind: string) {
  if (kind === 'csv') {
    const text = csv()
    if (text) downloadBlob(`${stem.value}.csv`, new Blob([text], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    elementToImageURL(host.value, kind, '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.${kind}`, url))
  }
}
// board PDF/CSV/SVG: a plot-only LIGHT re-render, per the contract in docs/UI.md
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const url = await elementToImageURL(host.value, 'png', '#ffffff')
  forceLight.value = false; await render()
  return url
}
async function exportSvg(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const svg = svgOf(host.value)?.outerHTML ?? null
  forceLight.value = false; await render()
  return svg
}
defineExpose({ exportFormats, exportAs, exportImage, exportSvg, getCsv: csv })
</script>

<template>
  <div class="tpv">
    <div class="tpv-ctrl cc-panel-controls">
      <div class="cc-row">
        <ChipSelect :options="MODES" :model-value="mode" variant="segmented" aria-label="Track plot mode"
                    v-tooltip.top="'Paths: where the cells were · Star: same origin · Rose: net displacement'"
                    @update:model-value="v => (mode = v as Mode)" />
        <span class="tpv-spacer" />
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="loading" @click="load">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>
      <div class="cc-row">
        <select v-if="familyOptions.length > 1" :value="popType" v-tooltip.top="'Which populations'"
                aria-label="Population family"
                @change="popType = ($event.target as HTMLSelectElement).value">
          <option v-for="o in familyOptions" :key="o.popType" :value="o.popType">{{ popTypeLabel(o) }}</option>
        </select>
        <select v-if="valueNames.length > 1" v-model="valueName"
                v-tooltip.top="'Which segmentation'" aria-label="Segmentation">
          <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
        <select v-model="colorBy" v-tooltip.top="'Colour each track by one of its properties'"
                aria-label="Colour by">
          <option value="">colour: track</option>
          <option v-for="c in colorOptions" :key="c" :value="c">{{ c }}</option>
        </select>
        <select v-model.number="limit" v-tooltip.top="'Cap on how many tracks to draw, per group, longest first'"
                aria-label="Max tracks">
          <option v-for="n in LIMITS" :key="n" :value="n">max {{ n }}</option>
        </select>
      </div>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="data && !data.tracked" class="cc-muted">Not tracked — run Track cells first.</p>
    <p v-else-if="data && data.total === 0" class="cc-muted">No tracks.</p>

    <PlotNotice v-if="plan.note" variant="banner" tone="muted" :text="plan.note"
                tip="Overlaid tracks from two images are not comparable." />
    <div ref="host" class="tpv-host" />
    <PlotSpinner v-if="loading" label="Reading tracks" />
    <span v-if="note" class="tpv-note cc-muted cc-fs-2xs">{{ note }}</span>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .tpv-ctrl (.cc-panel-controls) anchors to the plot box */
.tpv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.tpv-ctrl { display: flex; flex-direction: column; gap: 0.35rem; padding: 4px 6px; }
.tpv-spacer { flex: 1; }
/* overflow:hidden so a plot sized to its own floor cannot GROW this box and
   re-trigger the resize observer — see usePlotResize */
.tpv-host { flex: 1; min-height: 0; overflow: hidden; display: flex; align-items: center; justify-content: center; }
.tpv-note { position: absolute; right: 6px; bottom: 4px; }
</style>
