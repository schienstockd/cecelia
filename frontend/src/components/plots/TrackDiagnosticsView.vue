<!--
  Can this tracking result be trusted, and what kind of motion is in it — the celltrackR quality-control
  battery as four plots (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100048; the 32.7° plane
  expectation is Beltman et al. 2009, doi:10.1038/nri2638).

  **Why this exists at all.** The pair diagnostics were ported months ago and shipped reachable from
  nothing: exported Julia functions, no task, no route, no view. A diagnostic nobody can open is a
  diagnostic nobody has. So the battery now arrives two ways from ONE package roll-up — this panel, and
  QC findings that `tracking.track_measures` banks on every run whether or not anyone opens it.

  **It compares like every other plot on the board.** One cell per GROUP — per image, per treatment
  (the board's `compare by attribute`), per population — so "is WT's motion different from MerTK's" is
  one plot instead of two screenshots. A group's images are POOLED, which is exactly right for every
  diagnostic here (MSD at a lag is the mean over every segment of that lag, from whichever movie it came)
  and is handled where it belongs: the pair scan never pairs two tracks from different movies
  (`app/src/tracking/track_cohort.jl`).

  **A cohort SPLITS, it does not overlay** — the same rule as the track plot (`facetPlan`), and for the
  same two reasons: identifying a group by colour needs a swatch legend the house style rules out (Plot's
  inline legend wraps the svg in a `<figure>` that clips the bottom axis — `plots/plot.ts`), and the
  curves' own colours (blue for the measurement, amber for the fit it is read against) mean something
  already. The group's name is its facet title; the header line and the findings name it too.

  **The verdicts come from the server.** "Drifting", "confined", "artefact near the edge" are computed
  in `track_diagnostics` and shipped as `findings`, the same objects the QC doc holds. Nothing here
  re-derives a threshold from the curves — that is how a panel ends up disagreeing with the QC line
  about the same image.

  Four modes, four questions:
  - *Displacement* — MSD against lag, log-log. The slope IS the answer: 1 random walk, 2 directed,
    below 1 confined. The fitted line uses the server's slope, so the line and the printed number
    cannot differ.
  - *Persistence* — velocity autocorrelation against lag, with 1/e marked. How long a cell remembers
    its direction; flat at zero means it doesn't.
  - *Volume edge* — step angle against distance to the lower z boundary, with 32.7° marked. Angles that
    sag ONLY near the edge are a tracking artefact there. 3D only, so the mode is absent for 2D data
    rather than an empty box — offered if ANY group has it, since a mixed cohort has one arm that can answer.
  - *Track pairs* — angle between two tracks' paths against how far apart they were. Unrelated cells
    average 90°; a lower average among the FAR pairs means the whole field moves together.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { usePlotResize } from '../../composables/usePlotResize'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf } from '../../plots/export'
import { facetMode, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import type { PopTypeOption } from '../../plots/popTypes'
import { usePopFamily } from '../../composables/usePopFamily'
import PopFamilySelect from './PopFamilySelect.vue'
import { facetGrid, facetSlot, facetBox } from '../../plots/facetGrid'
import {
  availableModes, resolveMode, modeHint, referenceLine, axisLabels,
  cohortSummary, cohortFindings, pairCapNote, diagnosticsCsvRows, resolveTrackValueName,
  diagGroups, diagCurveRows, diagCloudRows, diagFitRows, DIAG_LABEL,
  type DiagnosticsResponse, type DiagMode,
} from '../../plots/trackDiagnostics'
import {
  cohortParams, cohortKey, facetPlan, groupLabel, type CompareMode,
} from '../../plots/trackGroups'
import type { SeriesTarget } from '../../plots/types'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // the board's comparison context — absent on a module page, which is then exactly the old behaviour
  vis?: VisProps
  series?: SeriesTarget[]
  compareMode?: CompareMode
  groupAttr?: string[]
  poolGroups?: boolean
  popTypes?: PopTypeOption[]
  state: { valueName?: string; mode?: string; popType?: string }
}>()

// the FIRST selected image answers "which segmentations" — the picker is about the vocabulary
const imageUid = computed(() => props.imageUids[0] ?? '')
// a TRACKED segmentation, never 'default' or the active one — see resolveTrackValueName
const trackedNames = ref<string[]>([])
const activeName = ref('')            // the segmentation the rest of the app is pointed at
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})
// the population FAMILY, one per plot (docs/PLOTS.md) — resolved through the same helper the rail uses
const { options: familyOptions, popType } =
  usePopFamily(() => props.popTypes, () => props.state.popType, v => (props.state.popType = v))
const vis = computed(() => props.vis ?? DEFAULT_VIS)

const data = ref<DiagnosticsResponse | null>(null)
const valueNames = ref<string[]>([])
const loading = ref(false)
const error = ref('')

const mode = computed<DiagMode | null>(() => resolveMode(data.value, props.state.mode))
const modeOptions = computed<ChipOption[]>(() =>
  availableModes(data.value).map(m => ({ value: m, label: DIAG_LABEL[m] })))
const summary = computed(() => cohortSummary(data.value))
const capNote = computed(() => (mode.value === 'pairs' ? pairCapNote(data.value) : ''))
const findings = computed(() => cohortFindings(data.value))
const groups = computed(() => diagGroups(data.value))
const dropped = computed(() => data.value?.dropped ?? 0)

/** Which segmentations this image has — the same list the track-gating axes read. */
async function loadValueNames() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; return }
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}&popType=track` +
              (valueName.value ? `&valueName=${encodeURIComponent(valueName.value)}` : '')
    const r = await fetch(`/api/gating/channels?${q}`)
    if (!r.ok) return
    const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]
                                  valueName?: string }
    valueNames.value = d.valueNames ?? []
    trackedNames.value = d.trackedValueNames ?? []
    activeName.value = d.valueName ?? ''
  } catch { /* the diagnostics request reports its own failure */ }
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
    const r = await fetch(`/api/tracking/diagnostics?${p}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    data.value = d as DiagnosticsResponse
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    data.value = null
  } finally {
    loading.value = false
    await nextTick(); plotBox.redraw()
  }
}

onMounted(async () => { await loadValueNames(); await load() })
// tracking, correction and re-measuring all change what this judges — the ONE refresh chokepoint
useDataRefresh(() => props.imageUids, () => { loadValueNames(); load() })
watch([() => props.projectUid, imageUid], async () => { await loadValueNames(); await load() })
// one watcher over the whole cohort query — see TrackPathsView for why it is the params and not a
// per-prop list
watch([() => cohortKey(cohort.value), valueName], load)

// ── drawing ───────────────────────────────────────────────────────────────────
const host = useTemplateRef<HTMLElement>('host')
const forceLight = ref(false)
let Plot: typeof import('@observablehq/plot') | null = null
let node: SVGElement | null = null

// curves and clouds share one domain across facets — two conditions on two scales are not a comparison
const plan = computed(() => facetPlan(facetMode(vis.value), groups.value.length))
const slots = computed(() => {
  const grid = facetGrid(groups.value.length)
  const m = new Map<string, { fx: number; fy: number }>()
  groups.value.forEach((g, i) => m.set(g.key, facetSlot(i, grid.cols)))
  return m
})

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  // emptied and refilled in ONE step at the end (`replaceChildren`) — see TrackPathsView for the
  // interleaved-render ghost this prevents
  const m = mode.value
  const d = data.value
  if (!m || !d || !groups.value.length) { host.value.replaceChildren(); node = null; return }

  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : 'white'
  const facet = plan.value.facet
  const grid = facetGrid(facet ? groups.value.length : 1)
  const ML = 54, MB = 36, MT = 10, MR = 12
  const box = facetBox({ cols: grid.cols, rows: grid.rows,
                         w: Math.max(220, host.value.clientWidth || 360),
                         h: Math.max(160, host.value.clientHeight || 240),
                         mx: ML + MR, my: MB + MT, square: false })
  const [xLabel, yLabel] = axisLabels(m)
  const refLine = referenceLine(m, d)

  const fxOf = (r: { g: string }) => slots.value.get(r.g)?.fx ?? 0
  const fyOf = (r: { g: string }) => slots.value.get(r.g)?.fy ?? 0
  const fch = facet ? { fx: fxOf, fy: fyOf } : {}
  // ONE colour vocabulary, whatever the group count: blue is the measurement, amber the expectation it
  // is read against. Groups are told apart by their facet title, not by a colour and a legend.
  const stroke = '#5aa9e6'
  const fill = '#5aa9e6'

  const marks: unknown[] = []
  let xOpts: Record<string, unknown> = { label: xLabel, grid: true }
  let yOpts: Record<string, unknown> = { label: yLabel, grid: true }

  if (m === 'msd' || m === 'acor') {
    const pts = diagCurveRows(d, m)
    if (m === 'msd') {
      // log-log, because the SLOPE is the readout — on linear axes a power law is just "a curve"
      xOpts = { ...xOpts, type: 'log' }
      yOpts = { ...yOpts, type: 'log' }
      const fit = diagFitRows(d)
      if (fit.length) marks.push(Plot.line(fit, { x: 'lag', y: 'value', z: 'g',
                                                 stroke: '#e8a33d', strokeDasharray: '4,3', ...fch }))
    } else {
      // the sign matters here (a negative lag-1 is the jitter signature), so zero stays on screen
      yOpts = { ...yOpts, domain: [Math.min(-0.2, ...pts.map(p => p.value)), 1] }
    }
    if (pts.some(p => p.sem !== null))
      marks.push(Plot.ruleX(pts.filter(p => p.sem !== null),
        { x: 'lag', y1: p => p.value - (p.sem ?? 0), y2: p => p.value + (p.sem ?? 0),
          stroke: fg, strokeOpacity: 0.35, ...fch }))
    marks.push(Plot.line(pts, { x: 'lag', y: 'value', z: 'g', stroke, strokeWidth: 1.5, ...fch }))
    marks.push(Plot.dot(pts, { x: 'lag', y: 'value', fill, r: 2.5, tip: true, ...fch,
                               channels: { group: 'label' } }))
  } else {
    const cloud = diagCloudRows(d, m)
    marks.push(Plot.dot(cloud, { x: 'distance', y: 'angle', fill, r: 1.3, fillOpacity: 0.35, ...fch }))
    // the trend is what the vignette reads off this plot, not the individual points
    if (cloud.length > 5)
      marks.push(Plot.line(cloud, { x: 'distance', y: 'angle', z: 'g', stroke: '#e8a33d',
                                    strokeWidth: 1.5, curve: 'basis', sort: 'distance', ...fch }))
    yOpts = { ...yOpts, domain: [0, 90] }
  }

  if (refLine) {
    marks.push(Plot.ruleY([refLine.value], { stroke: '#d9534f', strokeDasharray: '3,3' }))
    // the line needs to say WHAT it is, or it is a red line with no meaning
    marks.push(Plot.text([refLine], { y: 'value', text: 'label', frameAnchor: 'right',
                                      dx: -4, dy: -6, fill: '#d9534f', fontSize: 10 }))
  }
  if (facet) {
    // a facet HEADER is per column and cannot name a cell of a grid — so the title is a mark
    marks.push(Plot.text(groups.value.map(g => ({ ...slots.value.get(g.key)!, t: groupLabel(g) })),
                         { fx: 'fx', fy: 'fy', text: 't', frameAnchor: 'top-left',
                           dx: 4, dy: 4, fill: fg, fontSize: 10 }))
  }

  node = Plot.plot({
    width: box.width, height: box.height,
    marginLeft: ML, marginBottom: MB, marginTop: MT, marginRight: MR,
    style: { background: bg, color: fg, fontSize: '11px' },
    x: xOpts, y: yOpts,
    ...(facet ? { fx: { axis: null }, fy: { axis: null } } : {}),
    marks: marks as never[],
  }) as SVGElement
  host.value.replaceChildren(node)
}

// the observer's callback appends into the element it observes — see usePlotResize for why
// that loops, and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { node?.remove(); node = null })
watch([mode, data, plan], () => nextTick(() => plotBox.redraw()))

// ── export (the generic panel contract — plots/export.ts) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `track_${mode.value ?? 'diagnostics'}_${valueName.value || 'default'}`
  .replace(/[^\w.-]+/g, '_'))
function exportAs(kind: string) {
  if (kind === 'csv') {
    const rows = diagnosticsCsvRows(mode.value ?? 'msd', data.value)
    if (rows.length) downloadBlob(`${stem.value}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    elementToImageURL(host.value, kind, '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.${kind}`, url))
  }
}
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
defineExpose({ exportFormats, exportAs, exportImage, exportSvg })
</script>

<template>
  <div class="tdv">
    <div class="tdv-ctrl cc-panel-controls">
      <div class="cc-row">
        <ChipSelect v-if="modeOptions.length" :options="modeOptions" :model-value="mode ?? ''"
                    variant="segmented" aria-label="Diagnostic"
                    v-tooltip.top="mode ? modeHint(mode) : 'Which diagnostic to show'"
                    @update:model-value="v => (state.mode = v as string)" />
        <span class="tdv-spacer" />
        <PopFamilySelect :options="familyOptions" v-model="popType" />
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Re-run the checks'"
                :disabled="loading" @click="load">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>
      <span v-if="summary" class="cc-muted cc-fs-xs">{{ summary }}</span>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="data && !data.tracked" class="cc-muted">Not tracked — run Track cells first.</p>

    <!-- what the run already concluded, in its own words. Not re-derived here. -->
    <div v-if="findings.length" class="cc-row tdv-findings">
      <span v-for="(f, i) in findings" :key="`${f.group}:${f.code}:${i}`"
            class="cc-muted-warn cc-fs-xs tdv-finding" v-tooltip.bottom="f.long">
        <i class="pi pi-exclamation-triangle" /> {{ f.group ? `${f.group}: ${f.short}` : f.short }}
      </span>
    </div>

    <div ref="host" class="tdv-host" />
    <PlotSpinner v-if="loading" label="Running the checks" />
    <span v-if="capNote || dropped" class="tdv-note cc-muted cc-fs-2xs">
      {{ [capNote, dropped ? `${dropped} more group${dropped === 1 ? '' : 's'} not shown` : ''].filter(Boolean).join(' · ') }}
    </span>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .tdv-ctrl (.cc-panel-controls) anchors to the plot box */
.tdv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.tdv-ctrl { display: flex; flex-direction: column; gap: 0.3rem; padding: 4px 6px; }
.tdv-spacer { flex: 1; }
/* .tdv-findings → .cc-row for the flex row; only the wrap and inset are its own */
.tdv-findings { flex-wrap: wrap; padding: 0 6px; }
.tdv-finding { white-space: nowrap; }
/* overflow:hidden so a plot sized to its own floor cannot GROW this box and
   re-trigger the resize observer — see usePlotResize */
.tdv-host { flex: 1; min-height: 0; overflow: hidden; }
.tdv-note { position: absolute; right: 6px; bottom: 4px; }
</style>
