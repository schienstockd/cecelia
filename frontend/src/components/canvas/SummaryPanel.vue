<!--
  One summary plot, rendered with Vega-Lite (theme_classic look). Wraps the shared CanvasPanel chrome
  (drag/resize/arrange/remove). Generic across modules — no module-specific logic.

  SERIES are the host's selected (segmentation, pop) targets; in CROSS-IMAGE mode (`setUid`) the data
  is pooled across the selected images (`scope` per_image = one series per image, summarised = pooled).
  The MEASURE TYPE (numeric/categorical), returned by /api/plot_data, decides which chart types the
  panel offers; the Vega spec itself is built by plots/vega.ts (one builder per chart type). Data is
  server-aggregated (binning/counts/stats/downsampled raw points) — docs/API.md, docs/PLOTS.md.
  Rendered with Observable Plot (plots/plot.ts builds the options; PlotChart.vue renders + resizes).
-->
<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted, useTemplateRef } from 'vue'
import CanvasPanel from './CanvasPanel.vue'
import PlotChart from '../plots/PlotChart.vue'
import PlotSpinner from '../plots/PlotSpinner.vue'
import { useDelayedLoading } from '../../composables/useDelayedLoading'
import { backendChart, chartsForMeasure, plotDataToCsv, defaultVis, type VisProps, type BuildOpts } from '../../plots/plot'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import type { PlotSpec, PlotDataResponse, PlotSeries, ChartType, SeriesTarget } from '../../plots/types'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null
  spec: PlotSpec
  projectUid: string; imageUid: string | null
  setUid?: string | null               // cross-image: pool across this set's images
  imageUids?: string[]                 // cross-image: optional image subset (default = whole set)
  scope?: 'per_image' | 'summarised'   // cross-image scope (default per_image)
  groupAttr?: string[]                 // group cross-image series by these image attributes (combined; else per-image)
  series: SeriesTarget[]               // selected (segmentation, pop) targets — each one a series
  seriesColor: (s: PlotSeries) => string
  vis?: VisProps                       // visual properties (log scale, legend, point size/opacity)
  // per-panel chart options, PERSISTED in the host's panel state (so chart type/measure/bins survive
  // navigation). Seeded lazily from the spec's defaults; written back on user change.
  ui: { chartType?: ChartType; measure?: string; bins?: number; normalize?: boolean; errorMetric?: 'sd' | 'sem' | 'ci95'; groupBy?: string;
        matrixMode?: 'profile' | 'crosstab'; zscore?: boolean; matrixNormalize?: 'none' | 'row' | 'col' | 'total'; smooth?: number; interval?: boolean }
  collapseSeries?: boolean             // pool across pops & images → series by the groupBy level only
  reloadToken?: number                 // bumped by the host to force a refetch (live gate updates)
  persistKey?: string                  // CanvasPanel geometry persistence key
  docked?: boolean                     // fill a grid slot (Analysis board) instead of free-floating
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()
const plotRef = useTemplateRef<{ toImageURL(t: 'png' | 'svg', light?: boolean): Promise<string | null> }>('plotRef')

const param = (k: string, d: unknown) => props.spec.params?.find(p => p.key === k)?.default ?? d
// the columns actually present on the selected image+segmentation (loaded below), so we never offer a
// measure that doesn't exist — regionprops sets differ (2D vs 3D, version), so e.g. `aspect_ratio`
// may be absent and requesting it would 400 ("column not found"). Before the columns load, `present`
// is empty and we fall back to the spec's full list (the default measure is always valid).
// discovered columns of the selected image+segmentation (loaded in loadObsCols below). Declared up
// here — BEFORE measureOpts/its watch — because `watch(measureOpts)` evaluates its getter at setup,
// which reads these refs; declaring them later would be a temporal-dead-zone crash.
// structural regionprops that are not QC morphology measures (bounding box, centroid, raw inertia
// eigvals, the label index). New measurements no longer save bbox; this also hides it in existing data.
const STRUCTURAL_COL = /^(label$|bbox[-_]|centroid[-_]|inertia_tensor)/
const varCols = ref<string[]>([])        // var columns (morphology + intensity)
const channelCols = ref<string[]>([])    // intensity var columns specifically (mean_intensity_* / renamed)
const obsCols = ref<string[]>([])        // obs columns (live.cell.*, hmm.state, cluster ids, …)
const temporalCols = ref<string[]>([])   // obsm temporal col(s) (e.g. "t") — groupable but not in obs
// which (imageUid, valueName) the loaded columns belong to. On an image/segmentation switch the col
// refs are NOT cleared mid-load (that would reset the user's measure pick and "cycle") — so they still
// describe the PREVIOUS image until loadObsCols resolves. `colsReady` tells the fetch to wait for the
// current image's columns, so it never requests the previous image's measure (see fetchData guard).
const colsKey = () => `${props.imageUid ?? ''}|${props.series[0]?.valueName ?? ''}`
const colsFor = ref('')
const colsReady = computed(() => colsFor.value === colsKey())
const measureOpts = computed(() => {
  const ds = props.spec.dataSource
  // measuresFromData: offer every MORPHOLOGY measurement present — all var columns EXCEPT the
  // per-channel intensities (mean_intensity_*) — so segmentation QC lists the shape descriptors
  // (area, extent, eccentricity, solidity, axis lengths, aspect_ratio, …) it actually has, not a
  // curated subset and not the intensity channels. Default measure first.
  if (ds.measuresFromData && varCols.value.length) {
    const intensity = new Set(channelCols.value)
    // structural regionprops (bounding box, centroid, raw inertia eigvals, the label index) are not
    // QC morphology measures. New measurements no longer save bbox at all (measure_utils), but existing
    // h5ads still carry `bbox-*` — filter them here so the list stays to actual shape descriptors.
    const morph = varCols.value.filter(c => !intensity.has(c) && !STRUCTURAL_COL.test(c))
    const rest = morph.filter(c => c !== ds.measure).sort((a, b) => a.localeCompare(b))
    return morph.includes(ds.measure) ? [ds.measure, ...rest] : rest
  }
  // otherwise the static list, narrowed to what's actually present (avoids a 400 "column not found")
  const all = ds.measureOptions ?? [ds.measure]
  const present = new Set([...varCols.value, ...obsCols.value])
  if (!present.size) return all
  const ok = all.filter(m => present.has(m))
  return ok.length ? ok : all
})
// each option reads the persisted panel state, falling back to the spec default; writing persists it.
const measure = computed<string>({ get: () => props.ui.measure ?? props.spec.dataSource.measure, set: v => (props.ui.measure = v) })
// drop a persisted/selected measure that isn't available for the current data (avoids a 400 fetch).
// Only act once columns have loaded (measuresFromData needs varCols; otherwise the transient fallback
// list would reset the user's pick mid-load and it'd "cycle"). No-op writes are skipped by the guard.
const colsLoaded = computed(() => varCols.value.length > 0 || obsCols.value.length > 0)
watch([measureOpts, colsLoaded], () => {
  if (colsLoaded.value && measureOpts.value.length && !measureOpts.value.includes(measure.value))
    measure.value = measureOpts.value[0]
})
const chartType = computed<ChartType>({ get: () => props.ui.chartType ?? props.spec.chartTypes[0], set: v => (props.ui.chartType = v) })
const bins = computed<number>({ get: () => props.ui.bins ?? Number(param('bins', 30)), set: v => (props.ui.bins = v) })
const normalize = computed<boolean>({ get: () => props.ui.normalize ?? Boolean(param('normalize', true)), set: v => (props.ui.normalize = v) })
const errorMetric = computed<'sd' | 'sem' | 'ci95'>({ get: () => props.ui.errorMetric ?? 'ci95', set: v => (props.ui.errorMetric = v) })
// groupBy: split the measure by a categorical column (generic sub-axis, e.g. HMM state). '' = none.
// Options are DISCOVERED from the actual obs columns of the selected image+segmentation (so we never
// offer a column that doesn't exist — that produced an "ignoring unknown columns" warning and an
// empty split), filtered to categorical-looking names; a spec may add explicit hints that exist.
const CATEGORICAL_OBS = /(\.hmm\.state\.|\.hmm\.transitions\.|\.clusters?\.|track_generation|track_state)/
async function loadObsCols() {
  const key = colsKey()
  const vn = props.series[0]?.valueName
  if (!props.imageUid || !vn) { obsCols.value = []; temporalCols.value = []; colsFor.value = key; return }
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${props.imageUid}&valueName=${encodeURIComponent(vn)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    const j = res.ok ? (await res.json() as { columns?: string[]; channels?: string[]; obsColumns?: string[]; temporalColumns?: string[] }) : {}
    if (colsKey() !== key) return          // image/segmentation switched mid-load — discard the stale response
    varCols.value = j.columns ?? []
    channelCols.value = j.channels ?? []
    obsCols.value = j.obsColumns ?? []
    temporalCols.value = j.temporalColumns ?? []
    colsFor.value = key
  } catch {
    if (colsKey() !== key) return
    varCols.value = []; channelCols.value = []; obsCols.value = []; temporalCols.value = []
    colsFor.value = key
  }
}
watch([() => props.imageUid, () => props.series.map(t => t.valueName).join(',')], loadObsCols, { immediate: true })
const groupByOpts = computed<string[]>(() => {
  // temporal cols live in obsm (not obs) but ARE groupable (the per-timepoint QC/consistency view),
  // so they count as "present" for spec hints alongside the discovered categorical obs columns.
  const present = new Set([...obsCols.value, ...temporalCols.value])
  const hints = (props.spec.dataSource.groupByOptions ?? []).filter(c => present.has(c))
  const discovered = obsCols.value.filter(c => CATEGORICAL_OBS.test(c))
  return [...new Set([...hints, ...discovered])]
})
const groupBy = computed<string>({ get: () => props.ui.groupBy ?? '', set: v => (props.ui.groupBy = v) })
// drop a persisted groupBy that isn't available for the current data (avoids requesting a missing col).
// Only once the columns have actually loaded (groupByOpts non-empty) — otherwise the transient empty
// list during an async reload would wipe a valid selection (e.g. per-timepoint `t`) and it'd "cycle".
watch(groupByOpts, opts => { if (opts.length && groupBy.value && !opts.includes(groupBy.value)) groupBy.value = '' })

// TIME SERIES: grouping by a TEMPORAL column (t) → a geom_smooth LOESS line over time (thousands of
// frames would be thousands of bars/boxes otherwise). Applies to count and to any measure — the
// per-frame mean is fitted, so the box/violin selection collapses to a clean trend. Declared here
// (before the fetch watch, which lists it as a source) to avoid a temporal-dead-zone crash at setup.
const timeSeries = computed(() => !!groupBy.value && temporalCols.value.includes(groupBy.value))
// LOESS span as a % of points (geom_smooth `span`); the CI ribbon toggle. Render-only, persisted.
const smooth = computed<number>({ get: () => props.ui.smooth ?? 30, set: v => (props.ui.smooth = v) })
const interval = computed<boolean>({ get: () => props.ui.interval ?? true, set: v => (props.ui.interval = v) })

// ── heatmap (matrix) options — generic profile / crosstab grid (docs/PLOTS.md §9) ──
// profile = measures × category (the discovered categorical column) → "signature" (z-scorable);
// crosstab = a "from_to" categorical (e.g. HMM transitions) → transition matrix. The category reuses
// the discovered groupBy options; measures (profile rows) are the spec's measureOptions.
// a preset that pins its mode (state signature / transition matrix) hides the Mode toggle, so the two
// plot types keep distinct purposes; a generic heatmap (no pinned mode) shows the toggle.
const specPinnedMode = computed(() => !!props.spec.dataSource.matrix?.mode)
const matrixMode = computed<'profile' | 'crosstab'>({
  get: () => (specPinnedMode.value ? props.spec.dataSource.matrix!.mode! : (props.ui.matrixMode ?? 'profile')),
  set: v => (props.ui.matrixMode = v) })
const zscore = computed<boolean>({ get: () => props.ui.zscore ?? true, set: v => (props.ui.zscore = v) })
const matrixNormalize = computed<'none' | 'row' | 'col' | 'total'>({
  get: () => props.ui.matrixNormalize ?? 'row', set: v => (props.ui.matrixNormalize = v) })
// effective category: the user's chosen column, else the spec's pinned hint (if available), else a
// sensible default for the mode. Crosstab needs a "from_to" transitions column; profile needs a real
// multi-level state column — NB `track_state` also contains "state" but is usually constant, so match
// `.hmm.state.` specifically and never fall back to a bare /state/ (that picked the constant
// `track_state` → a single "5" column). Transitions are pair-encoded, so they're excluded from the
// profile fallback.
const matrixCategory = computed<string>(() => {
  if (groupBy.value) return groupBy.value
  const opts = groupByOpts.value
  if (!opts.length) return ''
  // the pinned hint only applies in the spec's intended mode (a state column makes no sense for
  // crosstab; a transitions column makes no sense for profile) — otherwise fall through to the pick.
  const hint = props.spec.dataSource.matrix?.category
  const specMode = props.spec.dataSource.matrix?.mode ?? 'profile'
  if (hint && opts.includes(hint) && matrixMode.value === specMode) return hint
  const pick = (re: RegExp) => opts.find(o => re.test(o))
  return matrixMode.value === 'crosstab'
    ? (pick(/\.hmm\.transitions\.|transition/i) ?? opts[0])
    : (pick(/\.hmm\.state\./i) ?? opts.find(o => !/transition/i.test(o)) ?? opts[0])
})
// the Category dropdown shows the effective default until the user picks one (then it persists).
const categorySel = computed<string>({ get: () => matrixCategory.value, set: v => (groupBy.value = v) })

// secondary-options popover (Split by + chart-specific control) — keeps the header bar uncluttered
const optsOpen = ref(false)
const optsRef = useTemplateRef<HTMLElement>('optsRef')
const hasOpts = computed(() => groupByOpts.value.length > 0
  || chartType.value === 'heatmap'
  || (['histogram', 'bar', 'frequency', 'count'] as ChartType[]).includes(chartType.value))
// The options popover MUST escape the panel's `overflow: hidden` (the card clips the plot area), so it
// is `position: fixed`, positioned from the trigger button on open and clamped to the viewport — never
// clipped, regardless of where the (draggable) panel sits or which edge it's near. Any new plot popover
// should follow this pattern rather than a plain absolute child of the panel. See docs/PLOTS.md §0.
const popStyle = ref<Record<string, string>>({})
watch(optsOpen, async open => {
  if (!open) { popStyle.value = {}; return }
  // stay OUT OF FLOW while measuring (fixed) so the popover never inflates the wrap — otherwise the
  // anchor rect grows by the popover's own size and the placement is thrown off.
  popStyle.value = { position: 'fixed', visibility: 'hidden' }
  await nextTick()
  const wrap = optsRef.value                        // inline-flex around just the button (popover is fixed)
  const pop = wrap?.querySelector('.sp-pop') as HTMLElement | null
  if (!wrap || !pop) return
  const a = wrap.getBoundingClientRect(), w = pop.offsetWidth, h = pop.offsetHeight
  const vw = window.innerWidth, vh = window.innerHeight
  let left = a.right - w                            // right-aligned to the button…
  if (left < 4) left = a.left                       // …flipped rightward if that clips the left edge
  left = Math.max(4, Math.min(left, vw - w - 4))
  let top = a.bottom + 4
  if (top + h > vh - 4) top = Math.max(4, a.top - h - 4)   // open above if no room below
  popStyle.value = { position: 'fixed', top: `${top}px`, left: `${left}px`, right: 'auto', visibility: 'visible' }
})
function onDocClick(e: MouseEvent) {
  if (optsOpen.value && optsRef.value && !optsRef.value.contains(e.target as Node)) optsOpen.value = false
}
onMounted(() => document.addEventListener('mousedown', onDocClick))
onUnmounted(() => document.removeEventListener('mousedown', onDocClick))
// friendly menu labels (the internal ChartType value stays as-is, e.g. 'strip' renders a beeswarm)
const CHART_LABELS: Partial<Record<ChartType, string>> = { strip: 'beeswarm', stacked100: '100% stacked', trend: 'trend (mean/t)', count: 'count' }
const chartLabel = (c: ChartType) => CHART_LABELS[c] ?? c

const crossImage = computed(() => !!props.setUid)
const result = ref<PlotDataResponse | null>(null)
const loading = ref(false)
// delayed spinner — only shows if a fetch runs past the threshold, so quick plots never flash it
const showSpinner = useDelayedLoading(loading)
const error = ref('')
// Fetch coordination. Rapid setting changes cascade through several watchers (measure → validCharts →
// chartType, groupBy → timeSeries, …); without this those fired overlapping requests whose responses
// landed out of order — the panel showed a stale result and the chart-type/measure selects appeared to
// "cycle". A short debounce coalesces a burst into one request; a monotonic sequence token means only
// the LATEST request may write `result` (older in-flight responses are discarded).
let fetchSeq = 0
let fetchTimer: ReturnType<typeof setTimeout> | null = null
function scheduleFetch() { if (fetchTimer) clearTimeout(fetchTimer); fetchTimer = setTimeout(fetchData, 60) }

// applicable chart types = the spec's allowed set ∩ the charts valid for the detected measure type
// (docs/PLOTS.md §2). Before the first response (no measureType) just show the spec's set.
const validCharts = computed<ChartType[]>(() => {
  // TIME SERIES (grouped by a temporal column, t): the only sensible charts are a trend LINE over
  // time — measure mean per frame (`trend`, geom_smooth/LOESS) or cell count per frame (`count`).
  // Distribution charts (boxplot/histogram/violin) would be thousands of per-frame boxes, so they're
  // dropped — which is why selecting `t` swaps the menu to [trend, count] and the reset watch below
  // moves the selection onto `trend` (no more "boxplot that secretly renders a smooth line").
  if (timeSeries.value) return ['trend', 'count']
  const mt = result.value?.measureType
  if (!mt) return props.spec.chartTypes
  const ok = new Set(chartsForMeasure(mt))
  // heatmap (pooled grid) and count (row count) are measure-independent — always keep them.
  const v = props.spec.chartTypes.filter(c => ok.has(c) || c === 'heatmap' || c === 'count')
  return v.length ? v : props.spec.chartTypes
})
watch(validCharts, v => { if (!v.includes(chartType.value)) chartType.value = v[0] })

// image/set selector shared by every request body (single image vs cross-image set + scope)
function applyImageSelector(body: Record<string, unknown>) {
  if (crossImage.value) {
    body.setUid = props.setUid
    if (props.imageUids?.length) body.imageUids = props.imageUids
    body.scope = props.scope ?? 'per_image'
  } else {
    body.imageUid = props.imageUid
  }
}

async function fetchData() {
  if (fetchTimer) { clearTimeout(fetchTimer); fetchTimer = null }
  const seq = ++fetchSeq                                   // only this call may write `result` (see below)
  if (!props.series.length) { result.value = null; return }
  if (!crossImage.value && !props.imageUid) { result.value = null; return }
  // measuresFromData offers measures built from THIS image's columns, so a fetch fired before the new
  // image's columns have loaded would request the previous image's measure (e.g. 3D-only `euler_number`
  // against a 2D image) → a backend "ignoring unknown columns" warning + a transient empty plot. Defer
  // until loadObsCols reports the current (imageUid, valueName)'s columns; the `colsReady` fetch-watch
  // re-fires this, by which point the measure-reset watch has moved `measure` onto a valid option.
  if (props.spec.dataSource.measuresFromData && !colsReady.value) return
  loading.value = true; error.value = ''

  // matrix/heatmap pools the whole frame into ONE grid, so it's a single request (not the per-popType
  // series merge below). All targets go under the first target's popType — behaviour heatmaps are
  // live-only; a target whose pop only exists under another popType would simply contribute no cells.
  if (chartType.value === 'heatmap') {
    try {
      const cat = matrixCategory.value
      if (!cat) { result.value = null; error.value = 'Pick a Category column (plot options) for the heatmap.'; return }
      const body: Record<string, unknown> = {
        projectUid: props.projectUid,
        popType: props.series[0].popType, granularity: props.spec.dataSource.granularity,
        chartType: 'matrix', matrixMode: matrixMode.value, category: cat, separator: '_',
        series: props.series.map(t => ({ valueName: t.valueName, pop: t.pop })),
        ...(matrixMode.value === 'profile'
          ? { measures: measureOpts.value, zscore: zscore.value }
          : { matrixNormalize: matrixNormalize.value }),
      }
      applyImageSelector(body)
      const res = await fetch('/api/plot_data', {
        method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
      })
      if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
      const r = await res.json() as PlotDataResponse
      if (seq !== fetchSeq) return                         // superseded by a newer request — discard
      result.value = r
    } catch (e) {
      if (seq !== fetchSeq) return
      error.value = e instanceof Error ? e.message : String(e); result.value = null
    } finally { if (seq === fetchSeq) loading.value = false }
    return
  }

  // several charts share one server aggregation. For a TIME SERIES (grouped by t) we fit a LOESS line
  // client-side, so we only need the per-frame AGGREGATE: `count` for the count chart, else the MEAN
  // (the `bar` aggregation) — the box/violin selection collapses to the trend.
  const be = timeSeries.value
    ? { chartType: chartType.value === 'count' ? 'count' : 'bar', rawPoints: false }
    : backendChart(chartType.value)
  try {
    // Group targets by pop_type — each /api/plot_data request resolves all its series under ONE
    // pop_type, so a plot mixing `live` (/_tracked) with `track` gates needs one request per group.
    // The series arrays are concatenated into a single response. (NB: for `histogram` the bin edges
    // come from the first group; mixing pop_types on a histogram can misalign bars — box/violin/bar/
    // beeswarm are per-series and unaffected. Tracked as a follow-up if it matters.)
    const byType = new Map<string, SeriesTarget[]>()
    for (const t of props.series) (byType.get(t.popType) ?? byType.set(t.popType, []).get(t.popType)!).push(t)

    const requests = [...byType.entries()].map(async ([pt, targets]) => {
      const body: Record<string, unknown> = {
        projectUid: props.projectUid,
        popType: pt, granularity: props.spec.dataSource.granularity,
        chartType: be.chartType,
        // count is a row count — no measure. Sending one would just fetch an unused column.
        ...(chartType.value === 'count' ? {} : { measure: measure.value }),
        series: targets.map(t => ({ valueName: t.valueName, pop: t.pop })),
        bins: bins.value,
        normalize: be.normalize ?? normalize.value,
        rawPoints: be.rawPoints ?? false,
        ...(groupBy.value ? { groupBy: groupBy.value } : {}),
        ...(props.collapseSeries ? { collapseSeries: true } : {}),
        ...(props.groupAttr?.length && crossImage.value ? { groupAttr: props.groupAttr } : {}),
      }
      applyImageSelector(body)
      const res = await fetch('/api/plot_data', {
        method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
      })
      if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
      return await res.json() as PlotDataResponse
    })

    const parts = await Promise.all(requests)
    if (seq !== fetchSeq) return                           // superseded by a newer request — discard
    result.value = { ...parts[0], series: parts.flatMap(p => p.series) }
  } catch (e) {
    if (seq !== fetchSeq) return
    error.value = e instanceof Error ? e.message : String(e); result.value = null
  } finally { if (seq === fetchSeq) loading.value = false }
}

// errorMetric is render-only (the bar response carries sd/sem/ci95) → not a fetch trigger.
// matrixCategory is included because for heatmaps the category resolves ASYNC (from obsCols loaded on
// mount) — without it the heatmap's first fetch bails ("pick a category") and never re-runs until the
// user re-picks. (Vue batches multi-source watches, so a user pick that changes groupBy + category
// still fires once.)
// Watch STABLE primitive keys (not the series array by identity — the parent rebuilds it every render,
// which fired spurious refetches). `timeSeries` is included because it selects the aggregation
// (count/bar) but isn't otherwise a fetch input; matrixCategory drives the heatmap category.
watch([() => props.series.map(t => `${t.popType}:${t.valueName}${t.pop}`).join('|'),
       measure, chartType, bins, normalize, groupBy, timeSeries, () => props.collapseSeries,
       matrixMode, zscore, matrixNormalize, matrixCategory, colsReady,
       () => props.imageUid, () => props.setUid, () => (props.groupAttr ?? []).join(','),
       () => (props.imageUids ?? []).join(','), () => props.scope, () => props.reloadToken],
      scheduleFetch)
onMounted(scheduleFetch)

const byImage = computed(() => crossImage.value && (props.scope ?? 'per_image') === 'per_image')

const vis = computed<VisProps>(() => props.vis ?? defaultVis())
const hasData = computed(() => {
  const r = result.value
  if (!r) return false
  return r.chartType === 'matrix' ? (r.cells?.length ?? 0) > 0 : r.series.length > 0
})

// the build options handed to PlotChart (which lazy-loads Plot and renders). Render-only inputs
// (chart type, error metric, vis props) recompute here without a refetch.
const buildOpts = computed<BuildOpts>(() => ({
  chartType: chartType.value, byImage: byImage.value, normalize: normalize.value,
  errorMetric: errorMetric.value, colorOf: props.seriesColor,
  nonNegative: true,               // the measures plotted here are non-negative
  trend: timeSeries.value, smooth: smooth.value, interval: interval.value,
  ...vis.value,                    // logScale, legend, pointSize, pointOpacity
}))

// ── export: the shown DATA as CSV, or the rendered chart as PNG / SVG (like the R version) ──
function downloadBlob(name: string, blob: Blob) {
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a'); a.href = url; a.download = name; a.click()
  URL.revokeObjectURL(url)
}
function exportAs(kind: string) {
  const stem = `${props.spec.id}_${measure.value}`.replace(/[^\w.-]+/g, '_')
  if (kind === 'csv') {
    if (result.value) downloadBlob(`${stem}.csv`, new Blob([plotDataToCsv(result.value)], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    plotRef.value?.toImageURL(kind).then(url => {
      if (!url) return
      const a = document.createElement('a'); a.href = url; a.download = `${stem}.${kind}`; a.click()
    })
  }
}
// the shown (aggregated) data as a CSV string — for embedding into the PDF export as an attachment
function getCsv(): string | null { return result.value ? plotDataToCsv(result.value) : null }
// a plot-only, LIGHT-theme PNG for the PDF export (no panel chrome; dark theme is on-screen only)
async function exportImage(): Promise<string | null> { return (await plotRef.value?.toImageURL('png', true)) ?? null }
defineExpose({ getCsv, exportImage })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :title="spec.label"
               :persist-key="persistKey" :docked="docked"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <template #actions>
      <!-- primary: what to plot + how (the single-measure picker is irrelevant for the matrix grid and
           for a row count) -->
      <select v-if="chartType !== 'heatmap' && chartType !== 'count'" v-model="measure" class="sp-measure" v-tooltip.bottom="'Measure to plot'">
        <option v-for="m in measureOpts" :key="m" :value="m">{{ m }}</option>
      </select>
      <select v-if="validCharts.length > 1" v-model="chartType" class="sp-chart"
              v-tooltip.bottom="'Chart type'">
        <option v-for="c in validCharts" :key="c" :value="c">{{ chartLabel(c) }}</option>
      </select>

      <!-- secondary options (split-by + chart-specific) tucked into a popover to keep the bar tidy -->
      <div v-if="hasOpts" ref="optsRef" class="sp-pop-wrap">
        <button class="sp-iconbtn" type="button" :class="{ on: optsOpen }" @click.stop="optsOpen = !optsOpen"
                v-tooltip.bottom="'Plot options'">
          <i class="pi pi-sliders-h" />
        </button>
        <div v-if="optsOpen" class="sp-pop" :style="popStyle" @click.stop>
          <!-- generic split-by (sub-axis) for the per-series charts; the heatmap uses Category below -->
          <label v-if="chartType !== 'heatmap' && groupByOpts.length" class="sp-pop-row"
                 v-tooltip.left="'Split the measure by a categorical column (e.g. HMM state)'">
            <span>Split by</span>
            <select v-model="groupBy">
              <option value="">none</option>
              <option v-for="g in groupByOpts" :key="g" :value="g">{{ g }}</option>
            </select>
          </label>
          <!-- heatmap (matrix) controls: mode · category · z-score (profile) / normalize (crosstab).
               The Mode toggle is shown only for a GENERIC heatmap; the behaviour presets
               (state signature = profile, transition matrix = crosstab) pin their mode, so the two
               plots stay distinct rather than each being able to become the other. -->
          <template v-if="chartType === 'heatmap'">
            <label v-if="!specPinnedMode" class="sp-pop-row" v-tooltip.left="'profile = measures × category (signature); crosstab = a from_to column → transition matrix'">
              <span>Mode</span>
              <select v-model="matrixMode">
                <option value="profile">profile</option>
                <option value="crosstab">crosstab</option>
              </select>
            </label>
            <label v-if="groupByOpts.length" class="sp-pop-row"
                   v-tooltip.left="'Categorical column: profile columns / crosstab from_to pairs'">
              <span>Category</span>
              <select v-model="categorySel">
                <option v-for="g in groupByOpts" :key="g" :value="g">{{ g }}</option>
              </select>
            </label>
            <label v-if="matrixMode === 'profile'" class="sp-pop-row"
                   v-tooltip.left="'Standardise each measure row across categories (comparable signature)'">
              <span>Z-score rows</span>
              <input type="checkbox" v-model="zscore" />
            </label>
            <label v-else class="sp-pop-row" v-tooltip.left="'Normalise the transition matrix'">
              <span>Normalize</span>
              <select v-model="matrixNormalize">
                <option value="row">row · P(to|from)</option>
                <option value="col">col · P(from|to)</option>
                <option value="total">total</option>
                <option value="none">counts</option>
              </select>
            </label>
          </template>
          <label v-if="chartType === 'histogram'" class="sp-pop-row">
            <span>Bins</span>
            <input type="number" min="5" max="100" step="5" v-model.number="bins" />
          </label>
          <label v-else-if="chartType === 'bar'" class="sp-pop-row">
            <span>Error</span>
            <select v-model="errorMetric">
              <option value="ci95">95% CI</option>
              <option value="sem">SEM</option>
              <option value="sd">SD</option>
            </select>
          </label>
          <label v-else-if="chartType === 'frequency'" class="sp-pop-row">
            <span>Proportion</span>
            <input type="checkbox" v-model="normalize" />
          </label>
          <template v-if="timeSeries">
            <label class="sp-pop-row" v-tooltip.left="'LOESS span — % of points in each local fit (geom_smooth span)'">
              <span>Smooth span</span>
              <input type="number" min="5" max="100" step="5" v-model.number="smooth" />
            </label>
            <label class="sp-pop-row" v-tooltip.left="'Show the ±95% confidence ribbon of the fit'">
              <span>Interval</span>
              <input type="checkbox" v-model="interval" />
            </label>
          </template>
        </div>
      </div>

    </template>

    <!-- utility actions live in the footer so the header/controls never clip -->
    <template #footer>
      <button class="sp-iconbtn" type="button" @click="emit('duplicate')"
              v-tooltip.top="docked ? 'Duplicate this plot into the next empty slot' : 'Duplicate this plot (same series + settings) to tweak one thing'">
        <i class="pi pi-copy" />
      </button>
      <!-- per-plot export is dropped in a slot (the whole page exports to PDF); keep it when floating -->
      <select v-if="!docked" class="sp-export" v-tooltip.top="'Export the shown plot'" :disabled="!result"
              @change="exportAs(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="csv">Data (CSV)</option>
        <option value="png">Image (PNG)</option>
        <option value="svg">Image (SVG)</option>
      </select>
    </template>

    <div class="sp-body">
      <div v-if="!series.length" class="sp-msg">Select one or more populations (eye icon) to plot.</div>
      <div v-else-if="error" class="sp-msg sp-err">{{ error }}</div>
      <div v-else-if="!hasData && !loading" class="sp-msg">No data for the selected populations.</div>
      <PlotChart v-else-if="hasData" ref="plotRef" :data="result" :opts="buildOpts" />
      <PlotSpinner v-if="showSpinner" label="Loading…" />
    </div>
  </CanvasPanel>
</template>

<style scoped>
.sp-measure { font-size: 12px; max-width: 12rem; }
.sp-chart { font-size: 12px; max-width: 8rem; }
.sp-export { font-size: 12px; max-width: 7rem; }

/* compact icon buttons (options / duplicate) */
.sp-iconbtn { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.7rem; }
.sp-iconbtn:hover { color: var(--cc-text); border-color: #484f58; }
.sp-iconbtn.on { color: var(--cc-text); border-color: var(--cc-accent); }

/* options popover */
.sp-pop-wrap { position: relative; display: inline-flex; }
/* position: fixed so the popover escapes the panel's overflow:hidden and never clips; the exact
   top/left are set inline on open (see the popStyle watcher). fixed here (not just inline) keeps it out
   of flow on the first render frame too, so it never inflates the anchor wrap. Only the box look + this
   positioning mode live here. New plot popovers should reuse this pattern (docs/PLOTS.md §0). */
.sp-pop { position: fixed; z-index: 40; min-width: 11rem;
  display: flex; flex-direction: column; gap: 6px; padding: 8px; border: 1px solid var(--cc-border);
  border-radius: 6px; background: var(--cc-surface-2); box-shadow: 0 6px 18px rgba(0,0,0,0.4); }
.sp-pop-row { display: flex; align-items: center; justify-content: space-between; gap: 8px;
  font-size: 12px; color: var(--cc-text-dim); }
.sp-pop-row select { font-size: 12px; max-width: 7rem; }
.sp-pop-row input[type="number"] { width: 3.6rem; font-size: 12px; padding: 2px 4px; }
.sp-body { position: relative; flex: 1; min-height: 200px; padding: 8px; overflow: hidden; }
.sp-msg { display: flex; align-items: center; justify-content: center; height: 100%; color: var(--cc-text-dim); font-size: 12px; text-align: center; padding: 12px; }
.sp-err { color: var(--cc-danger, #f87171); }
</style>
