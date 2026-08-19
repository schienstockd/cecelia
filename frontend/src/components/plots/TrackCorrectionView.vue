<!--
  The tracking-correction worklist — one entry in the interactive-view registry.

  This INVERTS the old R version. There you found the wrong track yourself, across hundreds of them,
  and then said how to fix it; the app helped with neither. Here the backend ranks what looks wrong
  and pre-picks the fix (`GET /api/tracking/issues`), each row carries a ready-to-submit op, and the
  user only judges it — which is the part a human is actually needed for.

  **Why each row draws its geometry.** The question a candidate poses cannot be answered by its
  numbers. Two track ends 2 µm apart are ONE cell if the first was heading toward where the second
  starts, and TWO cells if it was heading away — same distance, opposite answer. So the row shows the
  paths (`plots/trackPaths.ts`, Observable Plot) with the decision point marked, purely so an obvious
  non-issue can be rejected without leaving the list. napari remains where you look at the image;
  "Show" flies it to the candidate's own coordinate and frame.

  **Nothing is written until Apply.** Judging a row queues its op; the queue is submitted as ONE
  `tracking.correct_measures` run (Decision 3b). That is old R's `-mod` staging, without the file: it
  wrote a whole second `.h5ad` per edit because reticulate had no other channel.

  Registry-hosted so it gets the panel chrome, zoom and export for free — but page-scoped ONLY. It
  MUTATES, and the Analysis board is read-only (docs/ANALYSIS.md).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, nextTick, useTemplateRef } from 'vue'
import SelectionTable from '../SelectionTable.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import ConfirmButton from '../ConfirmButton.vue'
import CollapsibleSection from '../CollapsibleSection.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useFieldDraft } from '../../composables/useFieldDraft'
import { usePlotResize } from '../../composables/usePlotResize'
import { useLogStore } from '../../stores/log'
import { useTaskStore } from '../../stores/tasks'
import { useProjectStore } from '../../stores/project'
import { useWsStore } from '../../stores/ws'
import {
  visibleIssues, worklistSummary, opDescription, issueKey, undoLast, worklistCsvRows,
  KIND_LABEL, trackRows, manualActions, thresholdQuery, thresholdsChanged, THRESHOLD_FIELDS,
  suggestedOps,
  selectionSummary, selectedTracks, parseTrackIds, buildUntrackOp,
  type TrackIssue, type TrackOp, type IssuesResponse, type TrackRow, type TrackThresholds,
  type TrackSelection,
} from '../../lib/trackCorrection'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL } from '../../plots/export'
import {
  pathPoints, pathDomain, focusPoint, gapGeometry, gapHint, type TrackPathMap,
} from '../../plots/trackPaths'
import { resolveTrackValueName } from '../../plots/trackDiagnostics'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // `valueName` is the tracked label set; `kinds` filters the worklist; `pending` is the uncommitted
  // op queue and `skipped` the rows judged not-a-problem. All persisted, so navigating away and back
  // does not lose a half-reviewed movie.
  state: { imageUid?: string; valueName?: string; kinds?: string[]
           pending?: TrackOp[]; skipped?: string[]
           // `mode` is Suggested vs All tracks (P4d — a track the detector never flagged);
           // `thr` holds only the knobs the user MOVED (P4e), so the server keeps owning the defaults
           mode?: string; thr?: TrackThresholds; picked?: number[]; splitAt?: number | null
           lookup?: string; rowSel?: string[] }
}>()

const log = useLogStore()
const tasks = useTaskStore()
const project = useProjectStore()
const ws = useWsStore()

const imageUid = computed(() => (props.state.imageUid && props.imageUids.includes(props.state.imageUid))
  ? props.state.imageUid : (props.imageUids[0] ?? ''))
// NEVER default to 'default' (or to the ACTIVE segmentation): both are routinely untracked, and this
// panel then reports "nothing to review" for an image that has candidates — see resolveTrackValueName.
const valueNames = ref<string[]>([])
const trackedNames = ref<string[]>([])
const activeName = ref('')            // the segmentation the rest of the app is pointed at
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})
const kinds = computed({ get: () => props.state.kinds ?? [], set: v => (props.state.kinds = v) })
const pending = computed({ get: () => props.state.pending ?? [], set: v => (props.state.pending = v) })
const skipped = computed({ get: () => props.state.skipped ?? [], set: v => (props.state.skipped = v) })
type Mode = 'suggested' | 'all'
const mode = computed<Mode>({ get: () => (props.state.mode === 'all' ? 'all' : 'suggested'),
                              set: v => (props.state.mode = v) })
const thr = computed({ get: () => props.state.thr ?? {}, set: v => (props.state.thr = v) })
const picked = computed({ get: () => props.state.picked ?? [], set: v => (props.state.picked = v) })
const splitAt = computed({ get: () => props.state.splitAt ?? null,
                           set: v => (props.state.splitAt = v) })
const lookup = computed({ get: () => props.state.lookup ?? '', set: v => (props.state.lookup = v) })
// which WORKLIST rows are ticked (issue keys). Drives the shared plot, exactly as `picked` does in
// All-tracks mode — one selection concept, two vocabularies (a candidate vs a track).
const rowSel = computed({ get: () => props.state.rowSel ?? [], set: v => (props.state.rowSel = v) })

const MODES: ChipOption[] = [
  { value: 'suggested', label: 'Suggested' },
  { value: 'all', label: 'All tracks' },
]

const data = ref<IssuesResponse | null>(null)
const loading = ref(false)
const error = ref('')
/** What the server ACTUALLY used — the panel seeds its knobs from this, never from its own copy. */
const serverThresholds = computed<TrackThresholds>(() => data.value?.thresholds ?? {})
const knobs = computed<TrackThresholds>(() => ({ ...serverThresholds.value, ...thr.value }))
const knobsChanged = computed(() => thresholdsChanged(thr.value, serverThresholds.value))
function setKnob(key: keyof TrackThresholds, raw: string) {
  const v = Number(raw)
  if (Number.isFinite(v)) thr.value = { ...thr.value, [key]: v }
}
function resetKnobs() { thr.value = {}; load() }
// One draft per knob, keyed — a `:value` + `@change` number field is uncontrolled while focused, and
// Vue force-patches `value` against the DOM on every patch, so a re-render mid-typing discards the
// digits just typed (utils/continuousControls.test.ts pins the rule).
const knobDrafts = Object.fromEntries(
  THRESHOLD_FIELDS.map(f => [f.key, useFieldDraft(() => knobs.value[f.key])]),
) as Record<string, ReturnType<typeof useFieldDraft<number | undefined>>>

const paths = computed<TrackPathMap>(() => (data.value?.paths ?? {}) as TrackPathMap)
const rows = computed(() => visibleIssues(data.value?.issues ?? [], {
  kinds: kinds.value, pending: pending.value, skipped: skipped.value,
}))
const summary = computed(() => worklistSummary(data.value, pending.value.length))

const kindOptions = computed<ChipOption[]>(() =>
  Object.entries(data.value?.counts ?? {})
    .map(([k, n]) => ({ value: k, label: `${KIND_LABEL[k] ?? k} ${n}` })))

/** Which segmentations this image has, and which of them are tracked. */
async function loadValueNames() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; trackedNames.value = []; return }
  try {
    const r = await fetch(`/api/gating/channels?projectUid=${props.projectUid}` +
                          `&imageUid=${imageUid.value}&popType=track`)
    if (!r.ok) return
    const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]
                                  valueName?: string }
    valueNames.value = d.valueNames ?? []
    trackedNames.value = d.trackedValueNames ?? []
    activeName.value = d.valueName ?? ''    
  } catch { /* the issues request reports its own failure */ }
}

async function load() {
  if (!props.projectUid || !imageUid.value) { data.value = null; return }
  loading.value = true; error.value = ''
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              `&valueName=${encodeURIComponent(valueName.value)}` +
              thresholdQuery(thr.value, serverThresholds.value)
    const r = await fetch(`/api/tracking/issues?${q}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    data.value = d as IssuesResponse
  } catch (e) {
    error.value = String((e as Error)?.message ?? e)
    data.value = null
  } finally {
    loading.value = false
  }
}
onMounted(async () => { await loadValueNames(); await load() })
watch([() => props.projectUid, imageUid], async () => { await loadValueNames(); await load() })
watch(valueName, load)
// applying corrections re-runs the detector's own input, and `commit` promises the user this list
// comes back updated — that promise is this line (subject to the global autoRefreshOnTask setting)
useDataRefresh(() => (imageUid.value ? [imageUid.value] : []), load)

// ── ALL TRACKS (P4d) ──────────────────────────────────────────────────────────
// The worklist can only offer what a signature caught. A swap, a mid-track mis-link, a gap wider than
// `gapFrames` — the user SEES those and, until now, had no way to say so; old R at least let you name
// the tracks. This mode lists every track and turns a selection into the same op objects the detector
// emits, so a hand-authored edit and a suggested one are indistinguishable downstream: one queue, one
// task run, one journal.
const allPaths = ref<TrackPathMap>({})
const allTotal = ref(0)
const loadingAll = ref(false)

async function loadAll() {
  if (!props.projectUid || !imageUid.value) { allPaths.value = {}; return }
  loadingAll.value = true
  try {
    const ids = parseTrackIds(lookup.value)
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              `&valueName=${encodeURIComponent(valueName.value)}` +
              (ids.length ? `&ids=${ids.join(',')}` : '&limit=2000')
    const r = await fetch(`/api/tracking/paths?${q}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    // The route is COHORT-shaped (one entry per images × population group, for the board's comparison —
    // docs/TRACKING.md). This is a single-image, no-population call, so there is exactly one group, and
    // the worklist wants exactly that one: it is about editing THIS image, not comparing anything.
    const g = (d.groups ?? [])[0]
    allPaths.value = ((g?.paths ?? {}) as TrackPathMap)
    allTotal.value = Number(g?.total ?? 0)
    // a named track that does not exist is worth saying — silently showing an empty table reads as
    // "this image has no tracks"
    const missing = ids.filter(i => !(String(i) in allPaths.value))
    if (missing.length) log.warn(`No track ${missing.join(', ')} in ${valueName.value}`, { source: 'correct' })
  } catch (e) {
    error.value = String((e as Error)?.message ?? e)
  } finally {
    loadingAll.value = false
  }
}
// fetched only when the mode is actually opened — the picker is a second request over the same image
watch([mode, valueName, imageUid], () => { if (mode.value === 'all') loadAll() }, { immediate: true })

const allRows = computed<TrackRow[]>(() => trackRows(allPaths.value))

// ── From napari (P4d) ─────────────────────────────────────────────────────────
// The other half of "fix a track the detector missed": draw around it in the viewer instead of
// hunting for its id in a table. Drawing already stores the enclosed labels as the transient napari
// selection; `GET /api/tracking/selection` resolves those to TRACKS, which is the vocabulary the ops
// speak. Nothing new is asked of the bridge.
const selection = ref<TrackSelection | null>(null)
const selSummary = computed(() => selectionSummary(selection.value))

async function readSelection() {
  if (!props.projectUid || !imageUid.value) return
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              `&valueName=${encodeURIComponent(valueName.value)}`
    const r = await fetch(`/api/tracking/selection?${q}`)
    if (!r.ok) return
    selection.value = await r.json() as TrackSelection
  } catch { /* nothing drawn is the common case, not an error */ }
}

/** Ask napari for the drawing layer, then poll once — the bridge posts the labels back to the API. */
async function drawInNapari() {
  try {
    await fetch('/api/napari/start-selection', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, imageUid: imageUid.value,
                             valueName: valueName.value }),
    })
    log.info('Draw a region in napari, then press Read selection.', { source: 'correct' })
  } catch (e) {
    log.warn(`Could not start the napari selection: ${e}`, { source: 'correct' })
  }
}

/** Preselect the tracks the drawn region touched — then Join/Split/Remove act on them. */
function pickSelected() {
  const t = selectedTracks(selection.value)
  if (!t.length) return
  picked.value = t
  // the named tracks may be outside the picker's cap, so fetch them explicitly
  const missing = t.filter(id => !(String(id) in allPaths.value))
  if (missing.length) { lookup.value = t.join(' '); loadAll() }
}

/** Untrack the selected DETECTIONS — the cell-level op, which no track-level button can express. */
function untrackSelection() {
  const labels = selection.value?.labels ?? []
  if (!labels.length) return
  queueManual(buildUntrackOp(labels))
  selection.value = null
}
const pickedIds = computed({
  get: () => picked.value.map(String),
  set: (v: string[]) => (picked.value = v.map(Number)),
})
/** Rows for whatever geometry the current mode holds — the actions need spans, not just ids. */
const plotRows = computed<TrackRow[]>(() => trackRows(plotPaths.value))
// The actions act on what is TICKED, in either mode: a candidate row selects its tracks, and from
// there Join/Split/Remove is the same edit as one authored from the full list. Two modes, one verb set.
const actions = computed(() => manualActions(plotTracks.value, plotRows.value, splitAt.value))
const splitDraft = useFieldDraft(() => splitAt.value)
const lookupDraft = useFieldDraft(() => lookup.value)
/** Frames the picked track spans — the split input's bounds, shown so the number is not a guess. */
const pickedRange = computed(() => {
  if (plotTracks.value.length !== 1) return null
  return plotRows.value.find(r => r.track === plotTracks.value[0]) ?? null
})

function queueManual(op: TrackOp | null) {
  if (!op) return
  pending.value = [...pending.value, op]
  picked.value = []
  rowSel.value = []
  splitAt.value = null
}

/** Fly napari to a picked track's first frame — the same affordance a suggested row has. */
async function showTrack(track: number) {
  // whichever map the current mode holds — the worklist ships only its candidates' geometry
  const p = plotPaths.value[String(track)]
  if (!p?.t.length) return
  try {
    await fetch('/api/napari/centre', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ pos: [p.x[0], p.y[0]], tp: Math.round(p.t[0]) }),
    })
  } catch (e) {
    log.warn(`Could not move the viewer: ${e}`, { source: 'correct' })
  }
}

const ALL_COLUMNS = [
  { key: 'track',       label: 'Track',  width: 70 },
  { key: 'nFrames',     label: 'Frames', width: 70 },
  { key: 'span',        label: 'Span',   width: 90 },
  { key: 'netDistance', label: 'Net µm', width: 80 },
]

// ── Judging a row ─────────────────────────────────────────────────────────────
// Queue, don't write. The op goes on the stack exactly as the detector emitted it — nothing here
// translates a suggestion into an edit, which is the property that makes the worklist trustworthy.
/** The detector's own fix for every ticked candidate — the one-click path. */
const fixable = computed(() => suggestedOps(rowSel.value, rows.value))
function applySuggested() {
  if (!fixable.value.length) return
  pending.value = [...pending.value, ...fixable.value]
  rowSel.value = []
}

/** Dismiss the ticked candidates — the worklist's own verb, not a track edit. */
function dismissSelected() {
  if (!rowSel.value.length) return
  skipped.value = [...skipped.value, ...rowSel.value]
  rowSel.value = []
}
function undo() {
  pending.value = undoLast(pending.value)
}

// ── Committing ────────────────────────────────────────────────────────────────
// ONE task run for the whole queue, through the composite that also recomputes the track measures —
// so a correction can never leave `live.*` describing the previous assignment.
const FUN = 'tracking.correct_measures'
function commit() {
  if (!pending.value.length || !imageUid.value) return
  const params = { valueName: valueName.value, trackOps: JSON.stringify(pending.value) }
  const img = project.imageByUid(imageUid.value)
  const task = tasks.add({
    module: 'tracking', label: 'Correct tracks', imageUid: imageUid.value,
    imageName: img?.name || imageUid.value,
    status: 'queued' as const, taskName: 'trackCorrectMeasures', funName: FUN,
    params, projectUid: props.projectUid,
  })
  ws.send({ type: 'task:run', taskId: task.id, funName: FUN, params,
            imageUid: imageUid.value, projectUid: props.projectUid, setUid: props.setUid,
            poolName: 'cpu' })
  log.info(`Applying ${pending.value.length} correction(s) — the worklist refreshes when it finishes.`,
           { source: 'correct' })
  pending.value = []
  skipped.value = []
}

// the panel root — the PNG export captures it whole (table, plot and all)
const tcRoot = useTemplateRef<HTMLElement>('tcRoot')

/** One tooltip per row: what to do, plus the heading read when there is one. */
function rowTip(i: TrackIssue): string {
  const h = hintFor(i)
  return [i.advice ?? '', h].filter(Boolean).join(' — ')
}

/** The heading hint for a gap — the discriminator the row's numbers cannot express. */
function hintFor(i: TrackIssue): string {
  if (i.kind !== 'gap' || i.trackIds.length < 2) return ''
  const g = gapGeometry(plotPaths.value, i.trackIds[0], i.trackIds[1])
  return g ? gapHint(g.cosine) : ''
}

// ── ONE shared plot, driven by the selection ──────────────────────────────────
// The first version drew a 132px thumbnail per row. It was the wrong picture: a track on its own tells
// you almost nothing, and the question — which of these should be merged, which split — is about how
// they sit RELATIVE TO EACH OTHER. The old R version plotted the selected tracks together for exactly
// that reason. So selection drives one plot: tick rows, see those tracks on the same axes, then act.
const PlotMod = ref<typeof import('@observablehq/plot') | null>(null)
onMounted(async () => { PlotMod.value = await import('@observablehq/plot') })

const plotHost = useTemplateRef<HTMLElement>('plotHost')
/** Where the geometry comes from depends on the mode — the worklist ships only its candidates' tracks. */
const plotPaths = computed<TrackPathMap>(() => (mode.value === 'all' ? allPaths.value : paths.value))
/** The tracks on the plot: whatever is ticked, in either mode. */
const plotTracks = computed<number[]>(() => {
  if (mode.value === 'all') return picked.value
  const byKey = new Map(rows.value.map(i => [issueKey(i), i]))
  const out: number[] = []
  for (const k of rowSel.value) for (const t of byKey.get(k)?.trackIds ?? []) if (!out.includes(t)) out.push(t)
  return out
})
/** The decision point to mark — only meaningful with exactly one candidate ticked. */
const plotFocus = computed(() => {
  if (mode.value === 'all' || rowSel.value.length !== 1) return null
  return rows.value.find(i => issueKey(i) === rowSel.value[0]) ?? null
})

function drawPlot() {
  const P = PlotMod.value
  const el = plotHost.value
  if (!P || !el) return
  el.replaceChildren()
  const pts = pathPoints(plotPaths.value, plotTracks.value)
  if (!pts.length) return
  const dom = pathDomain(pts)
  const w = Math.max(120, el.clientWidth)
  const h = Math.max(100, el.clientHeight)
  const marks = [
    P.line(pts, { x: 'x', y: 'y', z: 'track', stroke: 'track', strokeWidth: 1.5 }),
    // where each track STARTS, and its id beside it — with several tracks on one pair of axes,
    // "which one is 41" is the question the plot has to answer before any button makes sense
    P.dot(pts.filter(p => p.i === 0), { x: 'x', y: 'y', fill: 'track', r: 3 }),
    P.text(pts.filter(p => p.i === 0), { x: 'x', y: 'y', text: 'track', fill: 'track',
                                         dx: 6, dy: -6, fontSize: 10 }),
  ]
  const f = plotFocus.value
  if (f) {
    const focus = focusPoint(pts, f.atT, f.trackIds[0])
    if (focus) marks.push(P.dot([focus], { x: 'x', y: 'y', stroke: 'currentColor', r: 6, strokeWidth: 1.5 }))
  }
  el.append(P.plot({
    width: w, height: h, marginLeft: 40, marginBottom: 28, marginTop: 8, marginRight: 8,
    x: { domain: dom?.x, label: 'x (µm)' }, y: { domain: dom?.y, label: 'y (µm)' },
    color: { legend: false }, style: { background: 'transparent', fontSize: '10px' },
    marks,
  }))
}
const plotBox = usePlotResize(plotHost, drawPlot)
watch([plotTracks, plotPaths, plotFocus, PlotMod], () => nextTick(() => plotBox.redraw()))

const columns = [
  { key: 'reason', label: 'What looks wrong' },
]
/** Rows carry a stable id so the table can multi-select them (the plot follows the ticks). */
const keyedRows = computed(() => rows.value.map(i => ({ ...i, key: issueKey(i) })))

// ── export (the generic panel contract — plots/export.ts) ──
// CSV is the point of exporting a worklist: a correction is a change to the data that no figure
// shows, so the record of what was found and what was decided has to be able to leave the app.
// The PNG is the review itself, thumbnails included (foreignObject capture, like the HMM panels).
// No SVG: the picture here is a TABLE, and a vector table is a screenshot with extra steps.
const exportFormats = ['csv', 'png']
const stem = computed(() => `track_worklist_${valueName.value}`.replace(/[^\w.-]+/g, '_'))
function exportAs(kind: string) {
  if (kind === 'csv') {
    const rows = worklistCsvRows(data.value?.issues ?? [], pending.value, skipped.value)
    if (rows.length) downloadBlob(`${stem.value}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  } else if (kind === 'png') {
    elementToImageURL(tcRoot.value, 'png', '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.png`, url))
  }
}
defineExpose({ exportFormats, exportAs })
</script>

<template>
  <div ref="tcRoot" class="tc">
    <div class="cc-row tc-head">
      <ChipSelect :options="MODES" :model-value="mode" variant="segmented" aria-label="Correction mode"
                  v-tooltip="'Suggested: what the detector found · All tracks: fix one it missed'"
                  @update:model-value="v => (mode = v as Mode)" />
      <span class="cc-muted cc-fs-sm">
        <i class="pi pi-flag" /> {{ mode === 'all' ? `${allRows.length} tracks` : summary }}
      </span>
      <ChipSelect v-if="mode === 'suggested' && kindOptions.length" v-model="kinds" :options="kindOptions"
                  multiple v-tooltip="'Show only these kinds'" />
      <span class="tc-spacer" />
      <select v-if="valueNames.length > 1" v-model="valueName"
              v-tooltip="'Which tracked segmentation'" aria-label="Segmentation">
        <option v-for="vn in valueNames" :key="vn" :value="vn"
                :disabled="trackedNames.length > 0 && !trackedNames.includes(vn)">{{ vn }}</option>
      </select>
      <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip="'Re-scan the tracks'" @click="load">
        <i class="pi pi-refresh" />
      </button>
    </div>


    <div class="cc-row tc-actions">
      <button v-if="mode === 'suggested'" class="cc-btn cc-btn-primary cc-btn-dense"
              :disabled="!fixable.length"
              v-tooltip="fixable.length ? 'Queue the suggested fix for each ticked row'
                                        : 'Tick a row — its fix is already chosen'"
              @click="applySuggested">Fix {{ fixable.length || '' }}</button>
      <button v-for="a in actions" :key="a.key" class="cc-btn cc-btn-dense"
              :class="a.blocked ? 'cc-btn-ghost' : 'cc-btn-primary'"
              :disabled="!!a.blocked" v-tooltip="a.blocked ?? opDescription(a.op!)"
              @click="queueManual(a.op)">{{ a.label }}</button>
      <label v-if="pickedRange" class="cc-row-group cc-fs-xs"
             v-tooltip="'Frame to split at — the new track starts here'">
        <span class="cc-muted">at frame</span>
        <input type="number" :min="pickedRange.t0 + 1" :max="pickedRange.t1"
               v-model="splitDraft" @change="splitAt = Number(splitDraft)" />
      </label>
      <button v-if="plotTracks.length === 1" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense"
              v-tooltip="'Show it in napari'" @click="showTrack(plotTracks[0])">
        <i class="pi pi-map-marker" />
      </button>
      <button v-if="mode === 'suggested'" class="cc-btn cc-btn-dense"
              :class="rowSel.length ? 'cc-btn-ghost' : 'cc-btn-ghost'" :disabled="!rowSel.length"
              v-tooltip="rowSel.length ? 'Not a problem — hide these rows' : 'Tick rows to dismiss them'"
              @click="dismissSelected">Dismiss</button>
      <span class="tc-spacer" />
      <span v-if="mode === 'all' && allTotal > allRows.length" class="cc-muted cc-fs-2xs">
        {{ allRows.length }} of {{ allTotal }} — longest first
      </span>
    </div>


    <!-- PLOT LEFT, LIST RIGHT. Stacked, the list was a two-row sliver under the plot: both halves are
         needed at once — you tick in one and read the answer in the other. -->
    <div class="cc-row tc-body">
    <div v-if="plotTracks.length" ref="plotHost" class="tc-plot" />
    <p v-else class="cc-muted cc-fs-xs tc-plot tc-plothint">Tick tracks to see them together</p>

    <div class="tc-side">
      <!-- P4e: the detector's own thresholds. Collapsed, because the defaults are measured — but a
           worklist someone abandons is one whose sensitivity they could not change. -->
      <CollapsibleSection v-if="mode === 'suggested' && data?.tracked" label="Sensitivity"
                          storage-key="tc:sensitivity" :default-open="false">
        <div class="cc-row tc-knobs">
          <label v-for="f in THRESHOLD_FIELDS" :key="f.key" class="cc-row-group cc-fs-xs"
                 v-tooltip="f.tip">
            <span class="cc-muted">{{ f.label }}</span>
            <input type="number" :step="f.step" v-model="knobDrafts[f.key].value"
                   @change="setKnob(f.key, knobDrafts[f.key].value)" />
          </label>
          <button v-if="knobsChanged" class="cc-btn cc-btn-ghost cc-btn-dense"
                  v-tooltip="'Back to the measured defaults'" @click="resetKnobs">Reset</button>
          <button class="cc-btn cc-btn-primary cc-btn-dense" v-tooltip="'Re-scan with these'"
                  :disabled="loading" @click="load">Re-scan</button>
        </div>
      </CollapsibleSection>

    <!-- ALL TRACKS (P4d): pick tracks, then say what is wrong with them -->
    <template v-if="mode === 'all'">
      <PlotSpinner v-if="loadingAll" label="Reading tracks" />
      <p v-else-if="!allRows.length" class="cc-muted cc-fs-sm">No tracks.</p>
      <template v-else>
        <!-- from napari: draw around the track you can SEE, instead of hunting for its id -->
        <div class="cc-row tc-napari">
          <button class="cc-btn cc-btn-dense" v-tooltip="'Draw a region in napari around the cells'"
                  @click="drawInNapari"><i class="pi pi-pencil" /> Draw</button>
          <button class="cc-btn cc-btn-dense" v-tooltip="'Read what is drawn in napari'"
                  @click="readSelection"><i class="pi pi-download" /> Read selection</button>
          <span v-if="selSummary" class="cc-muted cc-fs-xs">{{ selSummary }}</span>
          <button v-if="selection?.tracks.length" class="cc-btn cc-btn-primary cc-btn-dense"
                  v-tooltip="'Select those tracks in the list below'" @click="pickSelected">
            Pick {{ selection.tracks.length }}
          </button>
          <button v-if="selection?.labels.length" class="cc-btn cc-btn-dense"
                  v-tooltip="'Queue: untrack these detections, leaving the rest of their tracks'"
                  @click="untrackSelection">Untrack cells</button>
          <span class="tc-spacer" />
          <label class="cc-row-group cc-fs-xs" v-tooltip="'Show these track ids, past the list cap'">
            <span class="cc-muted">find</span>
            <input type="text" v-model="lookupDraft" placeholder="id(s)"
                   @change="lookup = lookupDraft; loadAll()" />
          </label>
        </div>
        <div class="tc-scroll">
        <SelectionTable :rows="allRows" :columns="ALL_COLUMNS" selection-mode="multi"
                        :selected="pickedIds" id-key="track" density="compact"
                        sort-storage-key="tc:allTracks"
                        @update:selected="pickedIds = $event">
          <template #cell-span="{ row }">
            {{ (row as unknown as TrackRow).t0 }}–{{ (row as unknown as TrackRow).t1 }}
          </template>
          <template #cell-netDistance="{ row }">
            {{ (row as unknown as TrackRow).netDistance.toFixed(1) }}
          </template>
        </SelectionTable>
        </div>
      </template>
    </template>

    <div v-else class="tc-scroll">
    <PlotSpinner v-if="loading" />
    <p v-else-if="error" class="cc-muted cc-fs-sm">{{ error }}</p>
    <p v-else-if="!rows.length" class="cc-muted cc-fs-sm">{{ summary || 'Nothing to review.' }}</p>

    <SelectionTable
      v-else
      :rows="keyedRows"
      :columns="columns"
      selection-mode="multi"
      :selected="rowSel"
      id-key="key"
      density="compact"
      @update:selected="rowSel = $event"
    >
      <template #cell-reason="{ row }">
        <div class="cc-row cc-row-tight tc-reason" v-tooltip="rowTip(row as TrackIssue)">
          <span class="cc-module-tag">{{ KIND_LABEL[(row as TrackIssue).kind] }}</span>
          <span class="tc-why">{{ (row as TrackIssue).reason }}</span>
          <!-- a MARKER, not a second tooltip: one tip per row, or the two fight (uiCopy ratchet) -->
          <i v-if="hintFor(row as TrackIssue)" class="pi pi-directions cc-muted" />
        </div>
      </template>
    </SelectionTable>
    </div>
    </div>
    </div>


    <div v-if="pending.length" class="cc-row tc-foot">
      <span class="cc-muted cc-fs-sm">{{ pending.length }} queued</span>
      <button class="cc-btn cc-btn-ghost cc-btn-dense" v-tooltip="'Take back the last one'" @click="undo">
        <i class="pi pi-undo" /> Undo
      </button>
      <ConfirmButton class="cc-btn cc-btn-primary cc-btn-dense"
                     :label="`Apply ${pending.length}`"
                     confirm-label="Apply — this rewrites the tracks"
                     @confirm="commit">
        <i class="pi pi-save" /> Apply {{ pending.length }}
      </ConfirmButton>
    </div>
  </div>
</template>

<style scoped>
.tc { display: flex; flex-direction: column; gap: 0.5rem; height: 100%; min-height: 0; }
/* `.cc-row` carries the flex/align/wrap/gap; only the bits it does not own live here. */
/* THE scrolling region. `.panel-main` is `overflow: hidden`, so without this the list is simply
   clipped at the panel edge with no way to reach the rest of it — which is what shipped. `min-height: 0`
   is the load-bearing half: a flex item defaults to `min-height: auto` and refuses to shrink below its
   content, so `overflow-y: auto` alone would have done nothing. */
.tc-scroll { flex: 1; min-height: 0; overflow-y: auto; }
/* the queue stays visible while the list scrolls — Apply must never scroll out of reach */
.tc-foot { margin-top: auto; }
.tc-napari { flex-wrap: wrap; }
.tc-napari input { width: 6rem; }
.tc-knobs { flex-wrap: wrap; }
.tc-knobs input { width: 4.5rem; }
.tc-actions { flex-wrap: wrap; }
.tc-actions input { width: 4.5rem; }
.tc-spacer { flex: 1; }
.tc-plothint { margin: 0 6px; }
/* Two columns that must both be visible at once: you tick in the list and read the answer in the
   plot. `.cc-row` carries the flex; nowrap + stretch are this layout's own, and `min-width: 0` on the
   plot is what lets it shrink instead of pushing the list off the panel. */
.tc-body { flex: 1; min-height: 0; align-items: stretch; flex-wrap: nowrap; }
.tc-plot { flex: 1 1 60%; min-width: 160px; min-height: 0; overflow: hidden; }
.tc-side { flex: 0 1 340px; min-width: 190px; min-height: 0; display: flex; flex-direction: column; }
.tc-why { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.tc-hint { display: inline-flex; align-items: center; gap: 0.25rem; }
</style>
