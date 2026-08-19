<!--
  Can this tracking result be trusted, and what kind of motion is in it — the celltrackR quality-control
  battery as four plots (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100048; the 32.7° plane
  expectation is Beltman et al. 2009, doi:10.1038/nri2638).

  **Why this exists at all.** The pair diagnostics were ported months ago and shipped reachable from
  nothing: exported Julia functions, no task, no route, no view. A diagnostic nobody can open is a
  diagnostic nobody has. So the battery now arrives two ways from ONE package roll-up — this panel, and
  QC findings that `tracking.track_measures` banks on every run whether or not anyone opens it.

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
    rather than an empty box.
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
import {
  availableModes, resolveMode, curvePoints, msdFitLine, modeHint, referenceLine, axisLabels,
  diagnosticsSummary, pairCapNote, diagnosticsCsvRows, resolveTrackValueName, DIAG_LABEL,
  type DiagnosticsResponse, type DiagMode,
} from '../../plots/trackDiagnostics'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { imageUid?: string; valueName?: string; mode?: string }
}>()

const imageUid = computed(() => (props.state.imageUid && props.imageUids.includes(props.state.imageUid))
  ? props.state.imageUid : (props.imageUids[0] ?? ''))
// a TRACKED segmentation, never 'default' or the active one — see resolveTrackValueName
const trackedNames = ref<string[]>([])
const activeName = ref('')            // the segmentation the rest of the app is pointed at
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})

const data = ref<DiagnosticsResponse | null>(null)
const valueNames = ref<string[]>([])
const loading = ref(false)
const error = ref('')

const mode = computed<DiagMode | null>(() => resolveMode(data.value, props.state.mode))
const modeOptions = computed<ChipOption[]>(() =>
  availableModes(data.value).map(m => ({ value: m, label: DIAG_LABEL[m] })))
const summary = computed(() => diagnosticsSummary(data.value))
const capNote = computed(() => (mode.value === 'pairs' ? pairCapNote(data.value) : ''))
const findings = computed(() => data.value?.findings ?? [])

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

async function load() {
  if (!props.projectUid || !imageUid.value) { data.value = null; return }
  loading.value = true; error.value = ''
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              (valueName.value ? `&valueName=${encodeURIComponent(valueName.value)}` : '')
    const r = await fetch(`/api/tracking/diagnostics?${q}`)
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
useDataRefresh(() => (imageUid.value ? [imageUid.value] : []), () => { loadValueNames(); load() })
watch([() => props.projectUid, imageUid], async () => { await loadValueNames(); await load() })
watch(valueName, load)

// ── drawing ───────────────────────────────────────────────────────────────────
const host = useTemplateRef<HTMLElement>('host')
const forceLight = ref(false)
let Plot: typeof import('@observablehq/plot') | null = null
let node: SVGElement | null = null

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  node?.remove(); node = null
  const m = mode.value
  const d = data.value
  if (!m || !d) return

  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : 'white'
  const w = Math.max(220, host.value.clientWidth || 360)
  const h = Math.max(160, host.value.clientHeight || 240)
  const [xLabel, yLabel] = axisLabels(m)
  const refLine = referenceLine(m, d)

  const marks: unknown[] = []
  let xOpts: Record<string, unknown> = { label: xLabel, grid: true }
  let yOpts: Record<string, unknown> = { label: yLabel, grid: true }

  if (m === 'msd' || m === 'acor') {
    const pts = curvePoints(m === 'msd' ? d.msd : d.acor)
    if (m === 'msd') {
      // log-log, because the SLOPE is the readout — on linear axes a power law is just "a curve"
      xOpts = { ...xOpts, type: 'log' }
      yOpts = { ...yOpts, type: 'log' }
      const fit = msdFitLine(pts, d.summary?.msdSlope)
      if (fit) marks.push(Plot.line(fit, { x: 'lag', y: 'value', stroke: '#e8a33d', strokeDasharray: '4,3' }))
    } else {
      // the sign matters here (a negative lag-1 is the jitter signature), so zero stays on screen
      yOpts = { ...yOpts, domain: [Math.min(-0.2, ...pts.map(p => p.value)), 1] }
    }
    if (pts.some(p => p.sem !== null))
      marks.push(Plot.ruleX(pts.filter(p => p.sem !== null),
        { x: 'lag', y1: p => p.value - (p.sem ?? 0), y2: p => p.value + (p.sem ?? 0), stroke: fg, strokeOpacity: 0.35 }))
    marks.push(Plot.line(pts, { x: 'lag', y: 'value', stroke: '#5aa9e6', strokeWidth: 1.5 }))
    marks.push(Plot.dot(pts, { x: 'lag', y: 'value', fill: '#5aa9e6', r: 2.5, tip: true }))
  } else {
    const cloud = m === 'plane'
      ? (d.plane?.distance ?? []).map((distance, i) => ({ distance, angle: d.plane!.angle[i] }))
      : (d.pairs?.distance ?? []).map((distance, i) => ({ distance, angle: d.pairs!.angle[i] }))
    marks.push(Plot.dot(cloud, { x: 'distance', y: 'angle', fill: '#5aa9e6', r: 1.3, fillOpacity: 0.35 }))
    // the trend is what the vignette reads off this plot, not the individual points
    if (cloud.length > 5)
      marks.push(Plot.line(cloud, { x: 'distance', y: 'angle', stroke: '#e8a33d', strokeWidth: 1.5,
                                    curve: 'basis', sort: 'distance' }))
    yOpts = { ...yOpts, domain: [0, 90] }
  }

  if (refLine) {
    marks.push(Plot.ruleY([refLine.value], { stroke: '#d9534f', strokeDasharray: '3,3' }))
    // the line needs to say WHAT it is, or it is a red line with no meaning
    marks.push(Plot.text([refLine], { y: 'value', text: 'label', frameAnchor: 'right',
                                      dx: -4, dy: -6, fill: '#d9534f', fontSize: 10 }))
  }

  node = Plot.plot({
    width: w, height: h, marginLeft: 54, marginBottom: 36, marginTop: 10, marginRight: 12,
    style: { background: bg, color: fg, fontSize: '11px' },
    x: xOpts, y: yOpts,
    marks: marks as never[],
  }) as SVGElement
  host.value.append(node)
}

// the observer's callback appends into the element it observes — see usePlotResize for why
// that loops, and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { node?.remove(); node = null })
watch([mode, data], () => nextTick(() => plotBox.redraw()))

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
        <select v-if="valueNames.length > 1" v-model="valueName"
                v-tooltip.top="'Which segmentation'" aria-label="Segmentation">
          <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
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
      <span v-for="f in findings" :key="f.code" class="cc-muted-warn cc-fs-xs tdv-finding"
            v-tooltip.bottom="f.long">
        <i class="pi pi-exclamation-triangle" /> {{ f.short }}
      </span>
    </div>

    <div ref="host" class="tdv-host" />
    <PlotSpinner v-if="loading" label="Running the checks" />
    <span v-if="capNote" class="tdv-note cc-muted cc-fs-2xs">{{ capNote }}</span>
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
