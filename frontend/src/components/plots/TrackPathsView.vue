<!--
  The napari tracks layer, as a plot: every track's path, in µm, optionally coloured by one of its
  own properties.

  Until now tracks could only be LOOKED at, in napari — `lib/tips.ts` said so outright. That is fine
  for judging one cell and useless for a figure: a viewer screenshot cannot be recoloured by speed,
  cannot be put beside the same field from another mouse, and cannot leave the app as vectors. This
  view is the plot half of that pair, so the Analysis board can carry tracks next to the populations
  they came from.

  **Three modes, because "the tracks" is three different questions.**
  - *Paths* — where the cells actually were. The spatial picture, the one napari shows.
  - *Star* — every track translated to a common origin (celltrackR's `plotTracks`/rose family, Wortel
    et al. 2021, doi:10.1016/j.crmeth.2021.100006). Position is discarded and SHAPE survives, which
    is what you compare between conditions: directed migration fans out, random walk fills a disc.
  - *Rose* — one straight arrow per track, start to end. Net displacement, when hundreds of paths
    have turned the star into a scribble.

  The axes are always SQUARE (`pathDomain`) — a track plot stretched to fill a panel turns a straight
  run into a diagonal, which destroys the only thing these modes exist to show.

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
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf } from '../../plots/export'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { debouncedLatest } from '../../utils/debouncedLatest'
import { followSelection, selectionMissed, EMPTY_TRACK_SELECTION,
         type CanvasTrackSelection } from '../../lib/trackSelection'
import { usePlotResize } from '../../composables/usePlotResize'
import { distinctColors } from '../../plots/plot'
import {
  pathPoints, pathDomain, normalizeTracks, displacementVectors, pathCsvRows, trackCountNote,
  trackEndpoints,
  type TrackPathMap, type PathPoint,
} from '../../plots/trackPaths'
import { resolveTrackValueName } from '../../plots/trackDiagnostics'

type Mode = 'paths' | 'star' | 'rose'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // THE CROSS-PANEL LINK (canvas `shared` bag, provided by GatingPlots). When the timeline selects
  // lanes, this plot draws exactly those tracks instead of the top-N — which is the whole point of
  // having both panels open: the timeline answers WHEN, this one answers WHERE, about the same cells.
  // Optional, so a host that does not share a selection (a board slot) still gets a working plot.
  trackSel?: CanvasTrackSelection
  setTrackSel?: (v: CanvasTrackSelection) => void
  // every user-settable option lives in the panel's persisted bag, not a bare ref
  state: { imageUid?: string; valueName?: string; mode?: Mode; colorBy?: string; limit?: number }
}>()

// `ids=` bypasses the endpoint's cap by NAMING the tracks — exactly what it was added for. Without it
// a selected track outside the top-N would silently not be drawn, which reads as "that track has no
// path" rather than "it is past the limit".
const follow = computed(() =>
  followSelection(props.trackSel ?? EMPTY_TRACK_SELECTION, imageUid.value))
const pinned = computed(() => follow.value?.ids ?? [])
// The selection ADOPTS this panel's segmentation to its own. A track id only means something within
// one label set: with this panel on `memTom` (396 tracks) and the timeline on `importTest2` (314),
// selecting lane 277 asked memTom for a track 277 it does not have and drew an empty box reading
// "0 selected tracks of 396". Following the scope is what makes "select there, see it here" true.
const effectiveValueName = computed(() => follow.value?.valueName || valueName.value)

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
const mode = computed({ get: () => props.state.mode ?? 'paths',
                        set: v => (props.state.mode = v) })
const colorBy = computed({ get: () => props.state.colorBy ?? '',
                           set: v => (props.state.colorBy = v) })
const limit = computed({ get: () => props.state.limit ?? 500,
                         set: v => (props.state.limit = v) })

interface PathsResponse {
  valueName: string; tracked: boolean
  total: number; shown: number; timeStep: number; stepScale: number
  colorBy: string; colorKind: 'numeric' | 'categorical' | 'none'
  values: Record<string, number | string | null>
  paths: TrackPathMap
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

/**
 * LAST REQUEST WINS. Selecting tracks in the timeline fires one `load` per click, and each is a
 * different request (`ids=80` then `ids=80,277`) — so without this the older, smaller response can
 * land last and the plot draws ONE track while two are selected. Dominik hit exactly that: the
 * endpoint returned both (verified: `ids=80,277` → `shown 2`), the panel reported "1 selected track".
 *
 * `debouncedLatest` is the canonical scheduler for a REQUEST (docs/UI.md → Continuous controls) and is
 * placed at the SINK, so every existing caller of `load()` is protected rather than each call site
 * having to remember. `isCurrent()` after the await is the part a plain debounce misses: a burst
 * collapses, but an in-flight response must also be refused once superseded.
 */
const runLoad = debouncedLatest<void>(async (_arg, isCurrent) => {
  if (!props.projectUid || !imageUid.value) { data.value = null; return }
  error.value = ''
  try {
    const vn = effectiveValueName.value
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              (vn ? `&valueName=${encodeURIComponent(vn)}` : '') +
              `&colorBy=${encodeURIComponent(colorBy.value)}` +
              (pinned.value.length ? `&ids=${encodeURIComponent(pinned.value.join(','))}`
                                   : `&limit=${limit.value}`)
    const r = await fetch(`/api/tracking/paths?${q}`)
    const d = await r.json()
    if (!isCurrent()) return                       // a newer selection is already on its way
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    data.value = d as PathsResponse
    // the server falls a column this image lacks back to uncoloured — follow it rather than keeping
    // a picker that claims a colour the plot is not using
    if (data.value.colorBy !== colorBy.value) colorBy.value = data.value.colorBy
  } catch (e) {
    if (!isCurrent()) return
    error.value = e instanceof Error ? e.message : String(e)
    data.value = null
  } finally {
    if (isCurrent()) { await nextTick(); plotBox.redraw() }
  }
}, {
  // short: a click burst is milliseconds apart, and anything longer reads as the plot lagging
  wait: 60,
  onState: st => (loading.value = st !== 'idle'),
  onError: e => (error.value = e instanceof Error ? e.message : String(e)),
})

function load() { runLoad.schedule() }

onMounted(async () => { await loadColumns(); await load() })
// a tracking or correction run rewrites exactly what this draws — the ONE refresh chokepoint, so the
// global autoRefreshOnTask setting governs it like every other plot
useDataRefresh(() => (imageUid.value ? [imageUid.value] : []), () => { loadColumns(); load() })
watch([() => props.projectUid, imageUid], async () => { await loadColumns(); await load() })
watch([valueName, colorBy, limit], load)
// a selection change is a different REQUEST (ids= vs limit=), not just a redraw
watch([pinned, effectiveValueName], load)

// ── drawing ───────────────────────────────────────────────────────────────────
const host = useTemplateRef<HTMLElement>('host')
const forceLight = ref(false)
let Plot: typeof import('@observablehq/plot') | null = null
let node: SVGElement | null = null

const paths = computed<TrackPathMap>(() => data.value?.paths ?? {})
const ids = computed(() => Object.keys(paths.value))
const raw = computed<PathPoint[]>(() => pathPoints(paths.value, ids.value))
/** Paths draws where the cells were; star and rose both start from a common origin. */
const shownPoints = computed<PathPoint[]>(() =>
  mode.value === 'paths' ? raw.value : normalizeTracks(raw.value))
const values = computed(() => data.value?.values ?? {})
// A capped plot that says nothing is a plot that lies — and so is one following a selection while
// claiming "longest first", which is what `trackCountNote` would say. Two different truths.
const missed = computed(() =>
  selectionMissed(props.trackSel ?? EMPTY_TRACK_SELECTION, data.value?.shown ?? 0))
const note = computed(() => {
  if (!pinned.value.length) return trackCountNote(data.value?.shown ?? 0, data.value?.total ?? 0)
  // an empty box under a selection is the worst outcome — it reads as "those tracks have no path"
  if (missed.value) return `None of the ${pinned.value.length} selected tracks are in ${effectiveValueName.value}`
  const n = data.value?.shown ?? 0
  return `${n} selected track${n === 1 ? '' : 's'} of ${data.value?.total ?? 0}`
})

/** The colour value for a point's track — undefined when the plot is coloured by track identity. */
const valueOf = (track: string) => values.value[track] ?? null

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  node?.remove(); node = null
  const pts = shownPoints.value
  if (!pts.length) return

  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : 'white'
  const box = Math.max(160, Math.min(host.value.clientWidth || 320, host.value.clientHeight || 320))
  const coloured = data.value?.colorKind === 'numeric' || data.value?.colorKind === 'categorical'

  // rose reduces each track to its net vector; the domain must be computed on what is drawn
  const vectors = mode.value === 'rose' ? displacementVectors(pts) : []
  const domain = pathDomain(mode.value === 'rose'
    ? vectors.flatMap(v => ([{ ...v, t: 0, i: 0, label: 0, track: v.track },
                             { track: v.track, t: 0, i: 0, label: 0, x: 0, y: 0 }] as PathPoint[]))
    : pts)

  const stroke = coloured ? 'v' : 'track'
  const endpoints = trackEndpoints(pts)
  const withValue = <T extends { track: string }>(rows: T[]) =>
    rows.map(r => ({ ...r, v: valueOf(r.track) }))

  const colourScale = coloured
    ? (data.value?.colorKind === 'numeric'
        ? { scheme: 'turbo' as const, legend: true as const, label: colorBy.value }
        : { legend: true as const, label: colorBy.value })
    // one colour per track and a legend of 500 entries is not a legend
    : { domain: ids.value, range: distinctColors(ids.value.length), legend: false as const }

  const marks = mode.value === 'rose'
    ? [
        Plot.link(withValue(vectors), { x1: 0, y1: 0, x2: 'x', y2: 'y', stroke,
                                        strokeWidth: 1.2, markerEnd: 'arrow' }),
      ]
    : [
        // START and END, told apart at a glance. A polyline says where a cell went and not which way
        // along it, and the old filled dot at the start was the same colour and nearly the same size
        // as the line — so it read as a bend, not a beginning. Now: a HOLLOW CIRCLE where the track
        // starts, and an ARROWHEAD on the line itself where it ends (`markerEnd`, oriented by the
        // final segment, so it also shows the heading). A single-point track gets its circle and no
        // arrow, because there is no segment to put one on.
        Plot.line(withValue(pts), { x: 'x', y: 'y', z: 'track', stroke, strokeWidth: 1.2,
                                    markerEnd: 'arrow' }),
        Plot.dot(withValue(endpoints.starts), { x: 'x', y: 'y', stroke, fill: 'none',
                                                r: 6, strokeWidth: 1.6 }),
      ]

  node = Plot.plot({
    width: box, height: box, marginLeft: 44, marginBottom: 34, marginTop: 12, marginRight: 12,
    style: { background: bg, color: fg, fontSize: '11px' },
    // µm on both axes, same span — see the header: a stretched track plot is a wrong track plot
    x: { domain: domain?.x, label: 'x (µm)', grid: true },
    // Y GROWS DOWNWARD. `centroid_y` is an IMAGE coordinate: row 0 is the top of the frame, which is
    // what napari draws and what every pixel index in the pipeline means. A plot with y increasing
    // upward is a MIRROR of the image — a cell moving down-screen appears to move up, and comparing
    // this plot with the viewer silently means comparing a shape with its reflection.
    y: { domain: domain?.y, label: 'y (µm)', grid: true, reverse: true },
    color: colourScale,
    marks,
  }) as SVGElement
  host.value.append(node)
}

// the observer's callback appends into the element it observes — see usePlotResize for why
// that loops, and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { node?.remove(); node = null })
watch([mode, shownPoints], () => nextTick(() => plotBox.redraw()))

// ── export (the generic panel contract — plots/export.ts, same helpers as the other views) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `tracks_${mode.value}_${valueName.value || 'default'}`.replace(/[^\w.-]+/g, '_'))
const csv = () => (shownPoints.value.length
  ? rowsToCsv(pathCsvRows(shownPoints.value, values.value, colorBy.value || 'value'))
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
        <select v-if="valueNames.length > 1" v-model="valueName"
                v-tooltip.top="'Which segmentation'" aria-label="Segmentation">
          <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
        <select v-model="colorBy" v-tooltip.top="'Colour each track by one of its properties'"
                aria-label="Colour by">
          <option value="">colour: track</option>
          <option v-for="c in colorOptions" :key="c" :value="c">{{ c }}</option>
        </select>
        <select v-model.number="limit" v-tooltip.top="'Cap on how many tracks to draw, longest first'"
                aria-label="Max tracks">
          <option v-for="n in LIMITS" :key="n" :value="n">max {{ n }}</option>
        </select>
      </div>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="data && !data.tracked" class="cc-muted">Not tracked — run Track cells first.</p>
    <p v-else-if="data && data.total === 0" class="cc-muted">No tracks.</p>

    <div ref="host" class="tpv-host" />
    <PlotSpinner v-if="loading" label="Reading tracks" />
    <span v-if="note" class="tpv-note cc-muted cc-fs-2xs">
      {{ note }}
      <button v-if="pinned.length && setTrackSel" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              v-tooltip.left="'Show all tracks again'"
              @click="setTrackSel({ ...EMPTY_TRACK_SELECTION })">
        <i class="pi pi-times" />
      </button>
    </span>
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
