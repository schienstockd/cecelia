<!--
  The track scheme — every track as a LANE over frames (docs/todo/TRACK_SCHEME_PLAN.md, Phase 1).

  **What it is for.** "Should these two tracks be joined, split, or dropped" is a question about TIME,
  and the surface that shipped in #590 asked it on a pair of spatial axes. The verdict on that,
  verbatim: "I selected two tracks that look almost identical. join and split are both greyed out. am
  I supposed to remove them? no clue what's happening. are they both from the same timepoints?" Here
  time is the x axis, so that last question is answered by looking.

  **A lane is one rect per contiguous run** (Decision 3). A gap is therefore the ABSENCE of a rect,
  not a marker drawn over one — which is what makes the picture load-bearing rather than decorative:
  two tracks that can be joined read as two bars that do not overlap in x, and two that cannot read as
  two bars side by side over the same frames. The overlap that will block a join in P2 is drawn in red
  the moment both tracks are selected, so a refusal is visible BEFORE any button is pressed
  (Decision 4).

  **One workspace, no modes** (Decision 2). "Work the ranked candidates" and "browse and spot it
  yourself" are this same screen with a different FILTER — there is no mode switch, and no branch in
  here that asks which one is on. That is the direct answer to "can't we let the user decide. I like
  the idea of working through the candidates. but some other might prefer to just look at the tracks
  and immediately see, aah, that's wrong."

  **Read-only, for now.** Phase 1 draws and selects; nothing here mutates. That also means it carries
  no `analysisBoard` flag: P2 makes it an editor, and the board is read-only (docs/ANALYSIS.md).

  **Why hand-rolled SVG rather than Observable Plot.** docs/PLOTS.md already draws this line — the
  gating scatter is a 2D canvas rather than a Plot because "gate drawing is the only interaction". The
  same applies: this is an authoring surface, not a chart. Every pixel has to map back to a (track,
  frame) the user will click to author an edit, and that mapping lives in `plots/trackScheme.ts` where
  it is unit-tested. Rendering through Plot would mean a second coordinate system — Plot's scales —
  that hit-testing would have to agree with. SVG also gives the board's vector export for free.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import CollapsibleSection from '../CollapsibleSection.vue'
import PlotSpinner from './PlotSpinner.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useFieldDraft } from '../../composables/useFieldDraft'
import { useLogStore } from '../../stores/log'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useViewerStore } from '../../stores/viewer'
import { openViewerWindow } from '../../utils/viewerWindow'
import { showTracksInViewer } from '../../utils/viewer/showTracksInViewer'
import { usePlotResize } from '../../composables/usePlotResize'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf, svgDoc, svgEsc }
  from '../../plots/export'
import { resolveTrackValueName } from '../../plots/trackDiagnostics'
import { cohortParams, type CompareMode } from '../../plots/trackGroups'
import type { PopTypeOption } from '../../plots/popTypes'
import { usePopFamily } from '../../composables/usePopFamily'
import PopFamilySelect from './PopFamilySelect.vue'
import type { SeriesTarget } from '../../plots/types'
import { EMPTY_TRACK_SELECTION, type CanvasTrackSelection } from '../../lib/trackSelection'
import {
  issueKey, KIND_LABEL, trackRows, manualActions, undoLast, opDescription,
  thresholdQuery, thresholdsChanged, THRESHOLD_FIELDS, selectionSummary, selectedTracks,
  type TrackIssue, type TrackOp, type TrackThresholds, type TrackSelection,
} from '../../lib/trackCorrection'
import { submitTrackOps } from '../../lib/trackOpsRun'
import { useTrackOpsQueueStore, trackOpsKey } from '../../stores/trackOpsQueue'
import { useCorrectionCockpitStore } from '../../stores/correctionCockpit'
import type { TrackPathMap } from '../../plots/trackPaths'
import { keyToAction, KEY_HINT, type TrackSchemeAction } from '../../utils/trackSchemeKeymap'

/** One entry of the grouped paths response — the timeline uses its own image's group. */
interface PathsGroup {
  key: string; label: string; imageUids?: string[]; valueName?: string
  tracked?: boolean; total?: number; shown?: number; timeStep?: number | null
  paths?: TrackPathMap
}
import {
  buildLanes, frameDomain, orderLanes, filterLanes, laneWindow, windowNote,
  issueMarkers, laneSeverity, candidateTracks, selectionOverlaps,
  joinPairs, orderLanesByPair, joinLinks, sharedFrames,
  frameToX, laneY, runRects, hitTest, frameTicks, laneSummary, schemeCsvRows,
  buildDetectionsLane, detectionRects, detectionHit,
  ORDER_LABEL, DEFAULT_LANE_H, DEFAULT_BAR_H,
  type LaneOrder, type SchemeGeom, type DetectionsLane, type DetectionFrame,
} from '../../plots/trackScheme'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // THE CROSS-PANEL LINK. When the host canvas provides these, the selection lives on the CANVAS, so
  // picking lanes here is the same act as choosing what the x/y track plot draws. Optional, because a
  // host that does not offer them (a board slot) must still get a working panel — it then falls back
  // to the panel's own state and simply talks to nobody.
  trackSel?: CanvasTrackSelection
  setTrackSel?: (v: CanvasTrackSelection) => void
  // THE CANVAS'S POPULATION MANAGER drives which tracks this shows — the same `series` vocabulary the
  // Analysis board hands its cohort plots. A track POPULATION is what a user picks; the segmentation
  // is the storage detail underneath it, and a private picker for it was a second control for a job
  // this canvas already has a canonical one for.
  series?: SeriesTarget[]
  popTypes?: PopTypeOption[]
  compareMode?: CompareMode
  poolGroups?: boolean
  state: {
    imageUid?: string; valueName?: string; order?: string
    candidatesOnly?: boolean; gapsOnly?: boolean; offset?: number; sel?: string[]
    popType?: string
    // `pending` is LEGACY ONLY — the uncommitted queue moved to `stores/trackOpsQueue.ts`, keyed by the
    // (image, segmentation) the ops edit rather than by the canvas the panel sits on. Read once, to
    // adopt what a canvas persisted mid-edit, then cleared. Do not write it.
    pending?: TrackOp[]; splitAt?: number | null
    thr?: TrackThresholds
    // P3: show the untracked-detections strip at the top of the panel. On by default because the
    // op it feeds (`points.add`) has no other UI — hiding it by default would hide the affordance.
    showUntracked?: boolean
    // P3: the untracked FRAME the user picked in the strip — the source of a queued `points.add`.
    // Persisted for the same reason as `sel`: a canvas rebind must not discard an authoring gesture.
    detSel?: { frame: number; labels: number[] } | null
  }
}>()

const imageUid = computed(() => (props.state.imageUid && props.imageUids.includes(props.state.imageUid))
  ? props.state.imageUid : (props.imageUids[0] ?? ''))
// a TRACKED segmentation, never 'default' or merely the active one. The worklist shipped with
// `valueName ?? 'default'` hardcoded and reported "nothing to review" on an image with 31 candidates,
// because `default` was untracked and the tracks lived under `memTom` — see resolveTrackValueName.
const trackedNames = ref<string[]>([])
const activeName = ref('')
const valueNames = ref<string[]>([])
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})

// The population FAMILY, through the same helper the rail and its two sibling views use. WRITABLE, and
// that is the fix: it was a read-only computed, so this panel was pinned to whichever family the
// registry listed first (`live`) while the canvas offers `track` populations — `filterSeriesToPopType`
// then correctly dropped every population the user had ticked, and the timeline silently showed the
// whole segmentation. A picker the user cannot move is not a resolution, it is a hardcoded default.
const { options: familyOptions, popType } =
  usePopFamily(() => props.popTypes, () => props.state.popType, v => (props.state.popType = v))

const order = computed<LaneOrder>(() => (props.state.order as LaneOrder) ?? 'pair')
const candidatesOnly = computed(() => !!props.state.candidatesOnly)
const gapsOnly = computed(() => !!props.state.gapsOnly)
// Only adopt the canvas selection when it is about THIS panel's segmentation — otherwise two
// timelines on different label sets would highlight each other's ids (see lib/trackSelection.ts).
const shared = computed(() => !!props.setTrackSel)
const selected = computed(() => {
  if (!shared.value) return new Set(props.state.sel ?? [])
  const sel = props.trackSel ?? EMPTY_TRACK_SELECTION
  const mine = (!sel.imageUid || sel.imageUid === imageUid.value)
            && (!sel.valueName || sel.valueName === valueName.value)
  return new Set(mine ? sel.ids : [])
})
function setSelected(ids: string[]) {
  // the scope travels WITH the ids — the receiving panel cannot reconstruct it
  if (props.setTrackSel) props.setTrackSel({ imageUid: imageUid.value, valueName: valueName.value, ids })
  else props.state.sel = ids
  // Clearing the LANE selection releases the viewer highlight too. A user's mental model here is
  // "these lanes ↔ what the viewer is showing" — leaving the highlight alone after Clear made the
  // viewer keep narrowing to tracks that were no longer selected, and the ribbons never came back.
  // The footer × still exists for the rare "keep highlight, clear lanes" case (users who want it
  // will pick a different lane first; Show then replaces the highlight).
  if (!ids.length && viewerStore.trackHighlight) viewerStore.setTrackHighlight(null)
  // Split cursor only applies when exactly one lane is selected. Any change out of that state
  // leaves the armed frame stale — a later Split against a fresh single-lane selection would
  // fire at whatever the last two-lane click armed. Drop it here so all clearing pathways benefit.
  if (ids.length !== 1) props.state.splitAt = null
}

const paths = ref<TrackPathMap>({})
const meta = ref<{ tracked: boolean; total: number; shown: number; timeStep?: number | null } | null>(null)
const issues = ref<TrackIssue[]>([])
// The untracked-detections strip (P3). Empty lane while loading — the render draws nothing then, so
// the strip only appears when there is something in it. Kept as its own ref because the wire is a
// separate route (`/api/tracking/detections`) and a failure there must not blank the tracks below.
const detFrames = ref<DetectionFrame[]>([])
const detLane = computed<DetectionsLane>(() => buildDetectionsLane(detFrames.value))
const showUntracked = computed(() => props.state.showUntracked !== false)
const untrackedVisible = computed(() => showUntracked.value && detLane.value.frames.length > 0)
const detSel = computed<{ frame: number; labels: number[] } | null>({
  get: () => props.state.detSel ?? null,
  set: v => (props.state.detSel = v),
})
const loading = ref(false)
const error = ref('')

// ── the lane set, in four steps: build → filter → order → window ───────────────
// Each step is a pure function from `plots/trackScheme.ts`; none of this arithmetic lives here.
const allLanes = computed(() => buildLanes(paths.value))
const severity = computed(() => laneSeverity(issues.value))
const candidates = computed(() => candidateTracks(issues.value))

const pairs = computed(() => joinPairs(issues.value, issueKey))
const kept = computed(() => filterLanes(allLanes.value, {
  tracks: candidatesOnly.value ? candidates.value : null,
  gapsOnly: gapsOnly.value,
}))
// 'Join candidates' is a GROUPING, not a comparator: it puts the two halves of each proposed join on
// neighbouring rows so the one comparison this panel exists for — clean hole, or overlapping bars? —
// is two rows apart instead of a scroll apart.
const filtered = computed(() => order.value === 'pair'
  ? orderLanesByPair(kept.value, pairs.value)
  : orderLanes(kept.value, order.value, severity.value))

// How many lanes FIT, rather than a fixed number — Open question 2 in the plan is "how many stay
// legible", and the honest answer is "as many as the panel is tall". A panel dragged taller shows
// more; nothing here caps it at a guess.
const perPage = ref(20)
const win = computed(() => laneWindow(filtered.value, props.state.offset ?? 0, perPage.value))
const note = computed(() => windowNote(win.value, order.value))
const markers = computed(() => issueMarkers(issues.value, win.value.lanes, issueKey))
const links = computed(() => joinLinks(win.value.lanes, pairs.value))

// The pairwise overlaps inside the selection — Decision 4 made visible. Named pairs, not a boolean:
// "these two of your four share frames" is useful and "your selection overlaps" is not.
const overlaps = computed(() => selectionOverlaps(win.value.lanes, selected.value))
const selSummary = computed(() => {
  const n = selected.value.size
  if (!n) return ''
  if (n === 1) return `Track ${[...selected.value][0]} selected`
  if (!overlaps.value.length) return `${n} tracks selected — no shared frames`
  const o = overlaps.value[0]
  const span = o.spans[0].t0 === o.spans[0].t1 ? `frame ${o.spans[0].t0}`
    : `frames ${o.spans[0].t0}–${o.spans[0].t1}`
  return `${n} tracks selected — ${o.a} and ${o.b} both exist at ${span}`
})

// The two counts in the header measure DIFFERENT things and a user will assume they are the same, so
// hovering says which is which. `summary` stays terse (docs/UI.md → UI copy); this is the long half,
// the same split `QC_TEXT` uses.
const summaryTip = computed(() => {
  const flagged = issues.value.length
  const gappy = allLanes.value.filter(l => l.nGaps > 0).length
  if (!flagged && !gappy) return ''
  return `Flagged = tracks an automatic scan thinks need a join, split or removal (${flagged}). `
       + `With gaps = tracks missing a detection in some frame (${gappy}) — most are not flagged.`
})

const orderOptions: ChipOption[] = (Object.keys(ORDER_LABEL) as LaneOrder[])
  .map(k => ({ value: k, label: ORDER_LABEL[k] }))

const summary = computed(() => {
  const m = meta.value
  if (!m?.tracked) return ''
  const parts = [`${m.total} tracks`]
  if (issues.value.length) parts.push(`${issues.value.length} flagged`)
  // COUNT OF TRACKS, not of holes, and shown beside the flagged count on purpose: the two are
  // different things and a user will assume they are the same, which is what `summaryTip` spells out. A
  // FLAGGED gap is two track ids the scan thinks are one cell; a gap in a LANE is a frame inside one
  // track where the cell was never detected. On the reference image (zolIMa/fXgbTl, memTom) that is 23 candidates against 306 of 396
  // tracks — so the majority of what a user might want to fix is invisible to the detector, which is
  // the whole reason the untracked lane and `points.add` are Phase 3.
  const gappy = allLanes.value.filter(l => l.nGaps > 0).length
  if (gappy) parts.push(`${gappy} with gaps`)
  if (detLane.value.nDetections) parts.push(`${detLane.value.nDetections} untracked`)
  return parts.join(' · ')
})

// ── data ──────────────────────────────────────────────────────────────────────
async function loadValueNames() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; return }
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}&popType=track` +
              (valueName.value ? `&valueName=${encodeURIComponent(valueName.value)}` : '')
    const r = await fetch(`/api/gating/channels?${q}`)
    if (!r.ok) return
    const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]; valueName?: string }
    valueNames.value = d.valueNames ?? []
    trackedNames.value = d.trackedValueNames ?? []
    activeName.value = d.valueName ?? ''
  } catch { /* the paths request reports its own failure */ }
}

/**
 * Occupancy and candidates, in parallel.
 *
 * The scheme needs only the `t` arrays, and `GET /api/tracking/paths` already sends exactly the run
 * structure (Decision 9) — so there is no new route for Phase 1. The limit is raised well past the
 * plot default because a scheme is a LIST: 374 tracks is the reference image, and a windowed list of
 * 500 is not the hairball a 500-track XY plot would be. `total` still reports the truth if the cap bites.
 *
 * A candidates failure is NOT fatal. The scheme's own job — showing when each track existed — needs
 * no detector at all, and refusing to draw because the ranking is unavailable would make a
 * degraded-but-useful surface into a blank one.
 */
async function load() {
  if (!props.projectUid || !imageUid.value) { paths.value = {}; meta.value = null; return }
  loading.value = true; error.value = ''
  // the same request shape as the other two track views (`cohortParams`), so the population manager's
  // selection means the same thing on every panel of this canvas
  const cp = cohortParams({ imageUids: [imageUid.value], compareMode: props.compareMode ?? 'image',
                            poolGroups: props.poolGroups, series: props.series,
                            popType: popType.value })
  cp.set('projectUid', props.projectUid)
  if (valueName.value) cp.set('valueName', valueName.value)
  const q = cp.toString()
  try {
    // `occupancy=1` → timepoints only, ~a third the bytes, because this panel reads nothing but `t`.
    // The explicit high `limit` is sent as well so an API server that predates the flag still returns
    // every track (heavier, but complete) instead of falling back to its 500 default.
    //
    // WHY NO SENSIBLE CAP: a path plot caps because 5000 polylines are a hairball; a lane list caps
    // into a LIE. "Pick track 2001" has no answer if track 2001 was never sent, and the panel cannot
    // even tell you it exists.
    const r = await fetch(`/api/tracking/paths?${q}&occupancy=1&limit=20000`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    // THE RESPONSE IS GROUPED (one entry per images × population — see docs/TRACKING.md). The
    // timeline edits ONE image's tracks, so it takes the group that matches its own image and falls
    // back to the first: a track id is only unique within a segmentation, and merging groups here
    // would put two different cells on one lane. Reading the old flat `paths` would report "no
    // tracks" on an image full of them — the same bug the worklist had to be fixed for.
    const gs = (d.groups ?? []) as PathsGroup[]
    const g = gs.find(x => (x.imageUids ?? []).includes(imageUid.value)) ?? gs[0]
    paths.value = (g?.paths ?? {}) as TrackPathMap
    meta.value = { tracked: !!g?.tracked, total: g?.total ?? 0, shown: g?.shown ?? 0,
                   timeStep: g?.timeStep }
    // the server resolves which segmentation a group actually came from; follow it rather than
    // keeping a picker that names one the data did not come from
    if (g?.valueName && g.valueName !== valueName.value) props.state.valueName = g.valueName
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    paths.value = {}; meta.value = null
  }
  try {
    // THE SAME SCOPE AS THE LANES. The detector has no cohort shape — it reports track ids and an op
    // built from one — so it still takes ONE resolved image (`imageUid`), but it takes the POPULATION
    // too: with the lanes narrowed to a population and the ranking left on the whole segmentation, a
    // candidate could name a track that is not on screen, and the two counts this panel prints beside
    // each other would be tallied over two different track sets.
    const iq = new URLSearchParams(cp)
    iq.set('imageUid', imageUid.value)
    const r = await fetch(`/api/tracking/issues?${iq}${thresholdQuery(thr.value, serverThresholds.value)}`)
    const d = await r.json()
    issues.value = r.ok ? ((d.issues ?? []) as TrackIssue[]) : []
    // what the server ACTUALLY used — the panel seeds its knobs from this rather than from a copy of
    // the defaults, so the two can never drift apart
    if (r.ok && d.thresholds) serverThresholds.value = d.thresholds as TrackThresholds
  } catch { issues.value = [] }
  // ── untracked lane (P3) ──
  // Separate try: a detections failure MUST NOT blank the tracks below it (same discipline as the
  // issues fetch — a degraded panel is more useful than a hidden one). Same `pops` scoping as
  // issues, so the strip matches the lanes it sits above.
  try {
    const dq = new URLSearchParams(cp)
    dq.set('imageUid', imageUid.value)
    const rd = await fetch(`/api/tracking/detections?${dq}`)
    const dd = await rd.json()
    detFrames.value = rd.ok ? ((dd.frames ?? []) as DetectionFrame[]) : []
  } catch { detFrames.value = [] }
  loading.value = false
  await nextTick(); plotBox.redraw()
}

onMounted(async () => { await loadValueNames(); await load() })
// tracking, correction and re-measuring all change what this draws — the ONE refresh chokepoint
useDataRefresh(() => (imageUid.value ? [imageUid.value] : []), () => { loadValueNames(); load() })
watch([() => props.projectUid, imageUid], async () => { await loadValueNames(); await load() })
watch(valueName, load)
// the canvas's population manager IS the picker for this panel — a change there is a different
// request, not just a redraw
watch(() => [props.series, props.compareMode, props.poolGroups, popType.value], load, { deep: true })
// a narrower lane set can leave the window past the end; `laneWindow` clamps, and this writes the
// clamped value back so the pager and the note agree with what is drawn
watch(filtered, () => { if (win.value.offset !== (props.state.offset ?? 0)) props.state.offset = win.value.offset })

// ── the detector's thresholds ────────────────────────────────────────────────
//
// "Tracks that the user might want to correct and are not detected are real, and we have to expose
// the knob of that detector." The DEFAULTS are never copied into TypeScript — the server reports what
// it used and the panel seeds from that, so the measured numbers stay on the Julia constants where
// they belong. Only what the user moved is sent.
const log = useLogStore()
// Which image the VIEWER holds, and the one canonical way to change it (`openViewerWindow` — pops
// the browser viewer window on the target) — a track panel must not grow a second open path.
const project = useProjectStore()
const projectMeta = useProjectMetaStore()
const settings = useSettingsStore()
const viewerStore = useViewerStore()
const serverThresholds = ref<TrackThresholds>({})
const thr = computed<TrackThresholds>({
  get: () => props.state.thr ?? {}, set: v => (props.state.thr = v),
})
const knobs = computed<TrackThresholds>(() => ({ ...serverThresholds.value, ...thr.value }))
const knobsChanged = computed(() => thresholdsChanged(thr.value, serverThresholds.value))
function setKnob(key: keyof TrackThresholds, raw: string) {
  const v = Number(raw)
  if (Number.isFinite(v)) thr.value = { ...thr.value, [key]: v }
}
function resetKnobs() { thr.value = {}; load() }
// One draft per knob: a `:value` + `@change` number field is uncontrolled while focused, and Vue
// force-patches `value` on every patch, so a re-render mid-typing discards the digits just typed
// (utils/continuousControls.test.ts pins this).
const knobDrafts = Object.fromEntries(
  THRESHOLD_FIELDS.map(f => [f.key, useFieldDraft(() => knobs.value[f.key])]),
) as Record<string, ReturnType<typeof useFieldDraft<number | undefined>>>

// ── from viewer ───────────────────────────────────────────────────────────────
//
// The other half of "fix a track the detector missed": draw around it in the viewer rather than hunt
// for its id. Drawing stores the enclosed labels as the transient viewer selection; `GET
// /api/tracking/selection` resolves those to TRACKS, the vocabulary the ops speak.
const viewerSel = ref<TrackSelection | null>(null)
const viewerSummary = computed(() => selectionSummary(viewerSel.value))
// What the viewer is currently narrowing to (independent of LANE selection). Scoped to this
// panel's (imageUid, valueName) so a highlight authored elsewhere doesn't advertise here.
const viewerHighlightSummary = computed(() => {
  const hl = viewerStore.trackHighlight
  if (!hl || !hl.trackIds.length) return ''
  if (hl.imageUid !== imageUid.value || hl.valueName !== valueName.value) return ''
  return `Viewer: ${hl.trackIds.length} highlighted`
})

async function enterSelectMode() {
  // Same reason as `showInViewer`: a region drawn on the image ON SCREEN would resolve against this
  // panel's labels, which is only meaningful when they are the same image.
  if (!(await ensureViewerImage())) return
  settings.viewerSelectMode = 'select'
  log.info('Drag a rectangle on the viewer, then press Read.', { source: 'tracks' })
}

/** Read what was drawn and SELECT those lanes — the timeline then shows when each of them existed. */
async function readSelection() {
  if (!props.projectUid || !imageUid.value) return
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              `&valueName=${encodeURIComponent(valueName.value)}`
    const r = await fetch(`/api/tracking/selection?${q}`)
    if (!r.ok) return
    viewerSel.value = await r.json() as TrackSelection
    const t = selectedTracks(viewerSel.value).map(String)
    if (t.length) {
      setSelected(t)
      // a drawn track can be outside the lane window — jump to the first one rather than select
      // something the user cannot see
      const i = filtered.value.findIndex(l => l.track === t[0])
      if (i >= 0) props.state.offset = Math.max(0, i - 1)
    }
  } catch { /* nothing drawn is the common case, not an error */ }
}

// ── editing (Phase 2) ─────────────────────────────────────────────────────────
//
// The ops and the queue are the SAME ones the worklist used (`lib/trackCorrection.ts`): nothing
// downstream knows this surface exists, and a hand-authored edit is indistinguishable from a
// suggested one — one queue, one `tracking.correct_measures` run, one journal (Decision 5).

// THE QUEUE LIVES IN A STORE, keyed by what the ops edit (project, image, segmentation) — not in this
// panel's state, which is keyed by the CANVAS. The Track canvas keys itself on the page-level
// segmentation select, so changing it rebound the canvas and took this panel and its queued edits out of
// view: an un-run task draft was being stored as a view option (`stores/trackOpsQueue.ts`).
const opsQueue = useTrackOpsQueueStore()
const cockpit = useCorrectionCockpitStore()
const opsKey = computed(() => trackOpsKey(props.projectUid, imageUid.value, valueName.value))
const pending = computed<TrackOp[]>({
  get: () => opsQueue.get(opsKey.value),
  set: v => opsQueue.set(opsKey.value, v),
})
// Ops queued against the OLD storage, so a canvas persisted mid-edit does not drop them on this upgrade.
// Waits for a REAL key rather than running on mount: `valueName` resolves through `resolveTrackValueName`,
// which needs the tracked-names fetch, so at mount the key can still be empty — and filing carried ops
// under the wrong segmentation would be worse than the bug being fixed. One-shot, then the panel state's
// copy is dropped so the store is the only one.
const carriedDone = ref(false)
watch(opsKey, k => {
  if (carriedDone.value || !k) return
  carriedDone.value = true
  const carried = props.state.pending
  if (carried?.length) opsQueue.set(k, [...opsQueue.get(k), ...carried])
  props.state.pending = undefined
}, { immediate: true })

// Publish this panel's selection / split-frame / untracked-detection pick to the shared correction
// cockpit store, so the app-global cockpit floating panel can act on what the timeline sees. One-way
// (timeline → cockpit) in Phase 1b — Phase 1c will make the store the source of truth in both
// directions when the timeline's action row is removed. Guarded on a real key so we don't publish
// state against a partly-resolved (image, valueName) that the queue would refuse anyway.
watch([opsKey, selected], ([k, sel]) => {
  if (k) cockpit.setSelectedTracks(k, [...sel])
}, { immediate: true })
watch([opsKey, () => props.state.splitAt], ([k, f]) => {
  if (k) cockpit.setSplitFrame(k, (f ?? null) as number | null)
}, { immediate: true })
watch([opsKey, detSel], ([k, d]) => {
  if (k) cockpit.setDetSelection(k, d as { frame: number; labels: number[] } | null)
}, { immediate: true })
// Actions + addAction are declared later in the file; wire their watchers there so the reference
// order stays clean. See "Cockpit action-publish" below.

/** The frame the user last clicked — where a Split would cut. */
const splitAt = computed<number | null>({
  get: () => props.state.splitAt ?? null,
  set: v => (props.state.splitAt = v),
})

const rows = computed(() => trackRows(paths.value))
const laneByTrack = computed(() => new Map(allLanes.value.map(l => [l.track, l])))

/**
 * Which edits the selection allows, and why the others do not.
 *
 * The fourth argument is the point: it hands `manualActions` the EXACT frames two tracks share, from
 * the runs this panel already drew. Without it the lib falls back to comparing t0/t1 ranges, which is
 * conservative — on the reference image that refuses 395 pairs the engine would happily join. The red
 * band on screen and the disabled Join button are now the same fact, computed once.
 */
const actions = computed(() => manualActions(
  [...selected.value].map(Number).filter(Number.isFinite),
  rows.value, splitAt.value,
  (a, b) => {
    const la = laneByTrack.value.get(String(a))
    const lb = laneByTrack.value.get(String(b))
    return la && lb ? sharedFrames(la, lb) : []
  },
))

/**
 * The queued edits, by track — so the PICTURE says an edit is pending.
 *
 * Clicking Join appeared to do nothing: the op went on the queue, the selection cleared, and the only
 * evidence was the words "1 queued" in the far corner. A correction is deliberately submitted as ONE
 * task run rather than one per click (CORRECTION_PLAN.md → Decision 3b), so the gap between "I said
 * join" and "the bars moved" is by design — which makes showing that intermediate state the surface's
 * job, not an extra.
 */
const pendingTracks = computed(() => {
  const m = new Map<string, string>()
  for (const op of pending.value) {
    for (const id of [...(op.trackIds ?? []), ...(op.trackId !== undefined ? [op.trackId] : [])])
      m.set(String(id), op.op)
  }
  return m
})

/** What was queued last, in the user's words — kept until the next action rather than timed out. */
const lastQueued = ref('')

function queue(op: TrackOp | null) {
  if (!op) return
  pending.value = [...pending.value, op]
  lastQueued.value = opDescription(op)
  setSelected([])
  splitAt.value = null
  detSel.value = null   // a queued points.add "uses up" the picked untracked cell — clear the strip
}

// Fix (queue the detector's suggested ops for all selected) moved out with the action row —
// re-add in the cockpit when Phase 2 introduces a "suggested fixes" verb; the underlying
// `suggestedOps` and `manualActions` helpers stay in lib/trackCorrection so the cockpit can
// import them without a second copy.

function apply() {
  if (submitTrackOps({
    projectUid: props.projectUid, setUid: props.setUid, imageUid: imageUid.value,
    valueName: valueName.value, ops: pending.value, source: 'tracks',
  })) { pending.value = []; lastQueued.value = 'Applying — the timeline refreshes when it finishes' }
}

// ── keybindings (P6, docs/todo/TRACK_SCHEME_PLAN.md; utils/trackSchemeKeymap.ts) ─────────────────
// Scoped to the panel — the listener sits on the .tsv root (tabindex=0), so a key only fires when
// the panel or one of its children has focus. Nothing at page scope: `l` also means Q in the
// animation timeline, and we do not want two panels racing for the same letter. `preventDefault`
// only where the browser has its own binding for the key (Backspace on a page = navigate back on
// old browsers, Enter = form submit if we ever land inside one) — plain letters we leave alone.
function dispatchAction(a: TrackSchemeAction) {
  const act = actions.value.find(x => x.key === a)
  if (act && act.op) queue(act.op)
  else if (a === 'add' && addAction.value.op) queue(addAction.value.op)
  else if (a === 'undo' && pending.value.length) pending.value = undoLast(pending.value)
  else if (a === 'apply' && pending.value.length) apply()
  else if (a === 'clearSel' && selected.value.size) { setSelected([]); detSel.value = null }
}

// ── the Add action (P3, points.add) ────────────────────────────────────────────
//
// Not part of `manualActions` in `lib/trackCorrection.ts` because it is driven by a DIFFERENT
// selection (an untracked frame, not two track lanes). Its own tiny state machine, mirrored on the
// Join/Split/Remove ones so the button behaves identically: `blocked` names why, `op` is the
// submittable op, `label` is the button text.
//
// Multi-cell frames: attaches ONLY the first label. The engine `_add_points!` refuses more than
// one cell at the target's existing time, but two untracked cells at the SAME frame going into a
// track that has a HOLE there would succeed — and give the target two cells at one timepoint,
// which makes `dt` in track_measures zero and its speeds infinite. So the safe rule is one-per-op,
// and the multi-cell picker is a follow-up (called out in the tooltip so the choice is honest).
const addAction = computed(() => {
  const d = detSel.value
  if (!d) return { blocked: 'Pick a frame in the Untracked strip first',
                   op: null as TrackOp | null, label: 'Add' }
  if (selected.value.size > 1)
    return { blocked: 'Select at most one track to attach to (or none to create a new track)',
             op: null as TrackOp | null, label: 'Add' }
  if (!d.labels.length) return { blocked: 'This frame has no untracked cell',
                                 op: null as TrackOp | null, label: 'Add' }
  const trackId = selected.value.size === 1 ? Number([...selected.value][0]) : undefined
  const op: TrackOp = trackId !== undefined && Number.isFinite(trackId)
    ? { op: 'points.add', labels: [d.labels[0]], trackId }
    : { op: 'points.add', labels: [d.labels[0]] }
  const suffix = d.labels.length > 1 ? ` (first of ${d.labels.length} at this frame)` : ''
  return { blocked: '', op,
           label: trackId !== undefined ? `Add${suffix}` : `Add as new track${suffix}` }
})

// Cockpit action-publish. Timeline computes the Join/Split/Remove/Add ops with their `blocked`
// reasons; the app-global correction cockpit renders the same set as buttons without refetching
// the paths. Behind opsKey so a partly-resolved (image, valueName) never publishes.
watch([opsKey, actions], ([k, as]) => {
  if (k) cockpit.setActions(k, [...as])
}, { immediate: true })
watch([opsKey, addAction], ([k, a]) => {
  if (!k) return
  cockpit.setAddAction(k, { label: a.label, blocked: a.blocked || null, op: a.op })
}, { immediate: true })
function onKey(e: KeyboardEvent) {
  const a = keyToAction(e)
  if (!a) return
  if (a === 'undo' || a === 'apply' || a === 'clearSel') e.preventDefault()
  dispatchAction(a)
}

// Hover-to-focus: the panel under the cursor gets keys. Mirrors napari-vizsla's mental model
// (`bind_key` on the layer that's active) and matches what the user already assumes when they
// wheel-zoom over a panel — the mouse position IS the focus. Skipped if a text field is focused
// (they're typing), so moving the mouse across a plot does not eat someone's typing in another
// panel. `pointerenter` rather than `mouseenter` so a stylus/touch also focuses.
function onPointerEnter() {
  const el = tsvRoot.value
  if (!el || el.contains(document.activeElement)) return
  const active = document.activeElement as { tagName?: string; isContentEditable?: boolean } | null
  const tag = active?.tagName?.toUpperCase()
  if (tag === 'INPUT' || tag === 'TEXTAREA' || active?.isContentEditable === true) return
  el.focus({ preventScroll: true })
}
const tsvRoot = useTemplateRef<HTMLElement>('tsvRoot')

// ── drawing ───────────────────────────────────────────────────────────────────
const host = useTemplateRef<HTMLElement>('host')
const forceLight = ref(false)

const GUTTER = 52          // track-id labels
// The frame ruler goes at the BOTTOM. `.cc-panel-controls` is `position: absolute; top: 0` — it
// OVERLAYS the plot box rather than sitting above it (that is what makes board panels fill their
// slot) — so an axis drawn at the top is hidden behind the control strip whenever it is shown or
// pinned. The first live look at this panel had no visible x axis at all for exactly that reason,
// which made the timeline unreadable. A time axis belongs at the bottom anyway.
const AXIS_H = 18
const PAD_R = 10
// The untracked strip (P3) is pinned above the scrolling lanes. Same pitch as a lane so the
// picture reads as "one extra row that doesn't scroll", but its own bar height so the rects can
// carry an intensity (untracked count normalised) without confusing the eye with the tracked ones.
const UNTRACKED_STRIP_H = DEFAULT_LANE_H
const UNTRACKED_BAR_H = DEFAULT_BAR_H

/** The geometry the render and the hit-test SHARE — one object, so a click cannot disagree with a bar. */
let geom: SchemeGeom | null = null

function render() {
  const el = host.value
  if (!el) return
  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : '#ffffff'
  const muted = dark ? '#8b949e' : '#666'
  const grid = dark ? '#30363d' : '#ddd'

  const w = Math.max(240, el.clientWidth || 360)
  const h = Math.max(80, el.clientHeight || 200)
  const stripH = untrackedVisible.value ? UNTRACKED_STRIP_H : 0
  // GUARDED: `render` runs inside the ResizeObserver's delivery, and an unconditional reactive write
  // here re-enters the whole chain (perPage → win → the watcher → redraw → render) on every single
  // delivery. The browser reports that as "ResizeObserver loop completed with undelivered
  // notifications" — the same warning `usePlotResize` was written for, arriving by a second route
  // that its size guard cannot see, because the state being changed is not the size.
  const fit = Math.max(1, Math.floor((h - AXIS_H - 4 - stripH) / DEFAULT_LANE_H))
  if (fit !== perPage.value) perPage.value = fit
  const laneBottom = h - AXIS_H

  const lanes = win.value.lanes
  const dom = untrackedVisible.value
    ? (frameDomain(allLanes.value) ?? [detLane.value.t0, detLane.value.t1])
    : frameDomain(allLanes.value)
  if (!dom || (!lanes.length && !untrackedVisible.value)) {
    el.innerHTML = ''; geom = null; return
  }

  // Track lanes start below the untracked strip. The strip itself is drawn at `y = 2` with height
  // UNTRACKED_STRIP_H; lanes start at `y = 2 + stripH` so a hit-test against `geom` never returns
  // an "untracked" lane (Decision 6: the untracked lane is not a `Lane`).
  const stripY = 2
  geom = { x0: GUTTER, x1: w - PAD_R, y0: 2 + stripH, laneH: DEFAULT_LANE_H, barH: DEFAULT_BAR_H,
           t0: dom[0], t1: dom[1] }
  const g = geom
  const parts: string[] = []

  // ── the untracked-detections strip, pinned above the lanes (P3) ──
  //
  // One rect per frame with any untracked cell, sized to that frame and shaded by count. Same
  // frameToX as the track bars, so a busy untracked frame lines up EXACTLY with the tracks' hole
  // that might absorb it — that visual alignment IS the affordance for `points.add`.
  //
  // Colour: `--cc-sev-warn` orange to say "this is unlinked", never blue (that reads as a tracked
  // bar). Opacity carries the count so a lone stray reads as light and a burst reads as saturated.
  if (untrackedVisible.value) {
    parts.push(`<text x="${GUTTER - 6}" y="${stripY + UNTRACKED_BAR_H - 1}" text-anchor="end" ` +
               `font-size="10" fill="${muted}"><title>Cells with no track_id, per frame — ` +
               `drag onto a track bar to add (Phase 3)</title>Untracked</text>`)
    // baseline, so an empty span reads as "no untracked cells" rather than as nothing
    parts.push(`<line x1="${g.x0}" y1="${stripY + UNTRACKED_BAR_H / 2}" x2="${g.x1}" ` +
               `y2="${stripY + UNTRACKED_BAR_H / 2}" stroke="${grid}" stroke-width="1" ` +
               `stroke-dasharray="2,2"/>`)
    const pickedFrame = detSel.value?.frame ?? null
    for (const r of detectionRects(detLane.value, g, stripY, UNTRACKED_BAR_H)) {
      // opacity floor of 0.35 so a single detection is still visible; ceiling implicit at 1
      const op = 0.35 + 0.6 * r.intensity
      const s = r.count === 1 ? `1 untracked at frame ${r.frame} — pick a track and Add`
                              : `${r.count} untracked at frame ${r.frame} — Add attaches the first`
      const picked = pickedFrame === r.frame
      parts.push(`<rect x="${r.x}" y="${r.y}" width="${r.w}" height="${r.h}" rx="1.5" ` +
                 `fill="#e8a33d" fill-opacity="${op.toFixed(2)}"` +
                 (picked ? ` stroke="${fg}" stroke-width="1.4"` : '') +
                 `><title>${esc(s)}</title></rect>`)
    }
    // a thin separator between the strip and the scrolling lanes, so the pinning is visible
    parts.push(`<line x1="0" y1="${stripY + UNTRACKED_STRIP_H - 1}" x2="${w}" ` +
               `y2="${stripY + UNTRACKED_STRIP_H - 1}" stroke="${grid}" stroke-width="0.5"/>`)
  }

  // ── frame ruler, along the bottom ──
  for (const t of frameTicks(g.t0, g.t1, Math.max(2, Math.round((g.x1 - g.x0) / 90)))) {
    const x = frameToX(g, t)
    parts.push(`<line x1="${x}" y1="0" x2="${x}" y2="${laneBottom}" stroke="${grid}" stroke-width="1"/>`)
    parts.push(`<text x="${x + 2}" y="${h - 5}" fill="${muted}" font-size="9">${t}</text>`)
  }
  parts.push(`<line x1="${g.x0}" y1="${laneBottom}" x2="${g.x1}" y2="${laneBottom}" stroke="${grid}"/>`)
  parts.push(`<text x="2" y="${h - 5}" fill="${muted}" font-size="9">frame</text>`)

  // ── the overlap bands, UNDER the bars: the reason a join is refused, drawn before it is asked ──
  const laneIndex = new Map(lanes.map((l, i) => [l.track, i]))
  for (const o of overlaps.value) {
    for (const s of o.spans) {
      const x = frameToX(g, s.t0)
      const x2 = Math.max(x + 2, frameToX(g, s.t1 + 1))
      for (const track of [o.a, o.b]) {
        const i = laneIndex.get(track)
        if (i === undefined) continue
        parts.push(`<rect x="${x}" y="${laneY(g, i) - 1}" width="${x2 - x}" height="${g.barH + 2}" ` +
                   `fill="#d9534f" fill-opacity="0.28"/>`)
      }
    }
  }

  // ── lane labels + the runs ──
  lanes.forEach((lane, i) => {
    const y = laneY(g, i)
    const on = selected.value.has(lane.track)
    parts.push(`<text x="${GUTTER - 6}" y="${y + g.barH - 1}" text-anchor="end" font-size="10" ` +
               `fill="${on ? fg : muted}" font-weight="${on ? 600 : 400}">${esc(lane.track)}` +
               `<title>${esc(laneSummary(lane))}</title></text>`)
    // the lane's own baseline: shows the extent a track SPANS, so a hole reads as a hole rather than
    // as the end of the track
    const bx = frameToX(g, lane.t0)
    const bx2 = frameToX(g, lane.t1 + 1)
    parts.push(`<line x1="${bx}" y1="${y + g.barH / 2}" x2="${bx2}" y2="${y + g.barH / 2}" ` +
               `stroke="${grid}" stroke-width="1" stroke-dasharray="2,2"/>`)
  })

  for (const r of runRects(lanes, g)) {
    const on = selected.value.has(r.track)
    // a QUEUED lane is green and outlined: the edit is decided but not yet run, and that state has to
    // be visible or clicking Join reads as nothing having happened
    const q = pendingTracks.value.get(r.track)
    const fill = q ? '#4a9d5f' : on ? '#e8a33d' : '#5aa9e6'
    parts.push(`<rect x="${r.x}" y="${r.y}" width="${r.w}" height="${r.h}" rx="2" ` +
               `fill="${fill}" fill-opacity="${q ? 0.9 : on ? 0.95 : 0.75}"` +
               (q ? ` stroke="#8fe0a3" stroke-width="1" stroke-dasharray="3,2"`
                  : on ? ` stroke="${fg}" stroke-width="1"` : '') +
               (q ? `><title>Queued: ${esc(q)}</title></rect>` : '/>'))
  }

  // ── the PROPOSED JOIN, drawn as a link between the two lanes ──
  //
  // This is what makes the panel readable rather than merely informative. Two bars and an amber tick
  // say "something is odd here"; a line from the end of one bar to the start of the other says "these
  // two are one cell, and this is the hole". Solid amber when the join is possible, red and crossed
  // when the two tracks share frames — the engine's own refusal, drawn before there is a button.
  for (const k of links.value) {
    const x1 = frameToX(g, k.fromT + 1)
    const x2 = frameToX(g, k.toT)
    const y1 = laneY(g, k.laneA) + g.barH / 2
    const y2 = laneY(g, k.laneB) + g.barH / 2
    const col = k.blocked ? '#d9534f' : '#e8a33d'
    const mx = (x1 + x2) / 2
    parts.push(`<path d="M${x1} ${y1} L${mx} ${y1} L${mx} ${y2} L${x2} ${y2}" fill="none" ` +
               `stroke="${col}" stroke-width="1.2"${k.blocked ? ' stroke-dasharray="3,2"' : ''}>` +
               `<title>${esc(k.blocked
                 ? `Tracks ${k.a} and ${k.b} exist at the same frames — cannot be joined`
                 : `Join track ${k.b} into ${k.a} across frames ${k.fromT + 1}\u2013${k.toT - 1}`)}` +
               `</title></path>`)
    if (!k.blocked)
      parts.push(`<circle cx="${mx}" cy="${(y1 + y2) / 2}" r="2.4" fill="${col}"/>`)
  }

  // ── the split cursor: WHERE the cut would land ──
  // The one thing a timeline can show that a worklist cannot. The worklist made you read the frame out
  // of a sentence and type it into a number box; here you click the bar and the cut is drawn on it.
  if (splitAt.value !== null && selected.value.size === 1) {
    const track = [...selected.value][0]
    const i = laneIndex.get(track)
    if (i !== undefined) {
      const x = frameToX(g, splitAt.value)
      const y = laneY(g, i)
      parts.push(`<line x1="${x}" y1="${y - 2}" x2="${x}" y2="${y + g.barH + 2}" ` +
                 `stroke="#e6e6e6" stroke-width="1.5"><title>Split track ${esc(track)} at frame ` +
                 `${splitAt.value}</title></line>`)
    }
  }

  // ── candidate marks: a tick UNDER the bar at the frame the detector is talking about ──
  for (const m of markers.value) {
    const i = laneIndex.get(m.track)
    if (i === undefined) continue
    const x = frameToX(g, m.t) + 1
    const y = laneY(g, i) + g.barH
    parts.push(`<path d="M${x - 3} ${y + 4} L${x + 3} ${y + 4} L${x} ${y} Z" fill="#e8a33d">` +
               `<title>${esc(KIND_LABEL[m.kind] ?? m.kind)} — track ${esc(m.track)}` +
               `${m.partners.length ? ` with ${esc(m.partners.join(', '))}` : ''}</title></path>`)
  }

  // `svgDoc`, not a hand-rolled <svg> root: it declares the xlink namespace the export path needs, and
  // `export.test.ts` fails a second root on sight — one document builder, like every other primitive here
  el.innerHTML = svgDoc({ width: w, height: h, background: bg, body: parts.join('') })
}

const esc = svgEsc

// the observer's callback writes into the element it observes — see usePlotResize for why that
// loops and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { if (host.value) host.value.innerHTML = '' })
watch([win, markers, links, overlaps, selected, splitAt, pendingTracks, detLane, untrackedVisible, detSel],
      () => nextTick(() => plotBox.redraw()))
// If a reload lost the frame the user had picked (a `points.add` was applied and that frame's
// only untracked cell became tracked, or a `pops` change shrank the scope), clear the selection.
// A ghost `detSel` naming a frame that no longer has an untracked cell would enable the Add
// button against nothing, and the queued op would fail server-side.
watch(detLane, l => {
  if (!detSel.value) return
  if (!l.frames.some(f => f.t === detSel.value!.frame)) detSel.value = null
})

// ── interaction ───────────────────────────────────────────────────────────────
/**
 * Click a lane to select it; shift-click to add.
 *
 * Selecting is the whole of Phase 1's interaction, and it is not idle: two selected lanes draw their
 * shared frames in red, which is the answer to "are they both from the same timepoints?" — asked and
 * unanswered by the surface this replaces. Phase 2 turns the same selection into Join / Split /
 * Remove.
 */
function onClick(ev: MouseEvent) {
  if (!geom || !host.value) return
  const box = host.value.getBoundingClientRect()
  const px = ev.clientX - box.left, py = ev.clientY - box.top
  // The untracked strip sits above `geom.y0`; test it first so a click there never falls through to
  // the lane hit-tester (which would return `null` for that Y band and quietly do nothing).
  if (untrackedVisible.value) {
    const dh = detectionHit(detLane.value, geom, 2, UNTRACKED_BAR_H, px, py)
    if (dh) {
      // Toggle: click the same frame to clear the selection. Multi-select is deliberately NOT
      // offered — one `points.add` op names one target track, so picking two source frames is
      // ambiguous. A user who wants both queues one Add, then picks the second.
      detSel.value = (detSel.value?.frame === dh.frame) ? null
                                                        : { frame: dh.frame, labels: [...dh.labels] }
      return
    }
  }
  const hit = hitTest(win.value.lanes, geom, px, py)
  if (!hit) return
  const next = new Set(ev.shiftKey ? selected.value : [])
  if (selected.value.has(hit.track) && (!ev.shiftKey || selected.value.size === 1)) next.delete(hit.track)
  else next.add(hit.track)
  setSelected([...next])
  // The clicked frame IS the split point — the worklist made you read it out of a sentence and type
  // it into a box, which is the single clearest thing a timeline removes. But only arm when the
  // resulting selection is exactly this one lane, so a two-lane Join gesture doesn't silently
  // arm a split-frame that Split would later fire against.
  if (hit.occupied && next.size === 1 && next.has(hit.track)) props.state.splitAt = hit.frame
}

/**
 * Point the viewer at THIS panel's image, if it is not there already.
 *
 * `false` when there is nothing to point: no viewer open, which is deliberately not force-launched —
 * the same rule the canvas's prev/next navigation follows ("don't force-launch viewer when it isn't
 * open"). Says so rather than failing three calls later inside the bridge.
 */
async function ensureViewerImage(): Promise<boolean> {
  if (!imageUid.value) return false
  if (!project.openImageUid) {
    log.info('Open this image in the viewer first.', { source: 'tracks' })
    return false
  }
  if (project.openImageUid === imageUid.value) return true
  openViewerWindow({
    projectUid: projectMeta.current?.uid ?? '',
    imageUid: imageUid.value,
    valueName: valueName.value || undefined,
  })
  return true
}

/**
 * Show the selected tracks in the viewer, and fly the camera to the first one.
 *
 * Restores the pre-napari-retire behaviour that regressed in P9 slice 4 (commit 842d8d36 dropped
 * `showTracksInNapari` + `centreNapariOnTrack` without a browser-viewer equivalent). The two
 * primitives that make this work here:
 *   1. **Highlight** — `viewerStore.setTrackHighlight({imageUid, valueName, trackIds})` narrows
 *      the viewer's per-vn track source to just these ids via `filterPayloadByTracks`.
 *   2. **Focus**    — `buildFocusViewState(current, {t, cx, cy, cz})` + `setPendingViewState`
 *      flies the camera to the first selected track's first detection.
 *
 * Uses ONE fetch (paths?ids=<first>) + ONE fetch (geometry, for voxelUm µm→L0 pixel). No
 * timelines-side conversion of stepScale (it's a normalisation factor, not a voxel size).
 */
async function showInViewer() {
  const ids = [...selected.value].map(Number).filter(Number.isFinite)
  if (!ids.length) return
  await showTracksInViewer(props.projectUid, imageUid.value, valueName.value, ids, 'tracks')
}


/** Hover readout — which track, which frame, and whether the cell is even there. */
const hover = ref('')
function onMove(ev: MouseEvent) {
  if (!geom || !host.value) { hover.value = ''; return }
  const box = host.value.getBoundingClientRect()
  const px = ev.clientX - box.left, py = ev.clientY - box.top
  // Try the untracked strip first — it is drawn above the track lanes and its Y band is disjoint
  // from `geom.y0`, so a hit there is unambiguous.
  if (untrackedVisible.value) {
    const dh = detectionHit(detLane.value, geom, 2, UNTRACKED_BAR_H, px, py)
    if (dh) {
      const n = dh.labels.length
      hover.value = `Untracked · frame ${dh.frame} · ${n} cell${n === 1 ? '' : 's'}`
      return
    }
  }
  const hit = hitTest(win.value.lanes, geom, px, py)
  hover.value = hit
    ? `Track ${hit.track} · frame ${hit.frame}${hit.occupied ? '' : ' · no detection'}`
    : ''
}

/** The wheel pages the lane window — the lanes are a list, and a list scrolls. */
function onWheel(ev: WheelEvent) {
  if (!win.value.total) return
  const step = ev.deltaY > 0 ? 3 : -3
  const next = Math.max(0, Math.min((props.state.offset ?? 0) + step,
                                    Math.max(0, win.value.total - perPage.value)))
  if (next === (props.state.offset ?? 0)) return      // at an end: let the page scroll instead
  ev.preventDefault()
  props.state.offset = next
}
const page = (dir: number) => {
  props.state.offset = Math.max(0, Math.min((props.state.offset ?? 0) + dir * perPage.value,
                                            Math.max(0, win.value.total - perPage.value)))
}
const atStart = computed(() => win.value.offset === 0)
const atEnd = computed(() => win.value.offset + win.value.lanes.length >= win.value.total)

// ── export (the generic panel contract — plots/export.ts) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `track_scheme_${valueName.value || 'default'}`.replace(/[^\w.-]+/g, '_'))
function exportAs(kind: string) {
  if (kind === 'csv') {
    // the FILTERED, ORDERED set — not just the visible window. The window is a viewport; exporting
    // only what happens to be on screen would make the file depend on the panel's height.
    const rows = schemeCsvRows(filtered.value)
    if (rows.length) downloadBlob(`${stem.value}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    elementToImageURL(host.value, kind, '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.${kind}`, url))
  }
}
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); render()
  const url = await elementToImageURL(host.value, 'png', '#ffffff')
  forceLight.value = false; render()
  return url
}
async function exportSvg(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); render()
  const svg = svgOf(host.value)?.outerHTML ?? null
  forceLight.value = false; render()
  return svg
}
defineExpose({ exportFormats, exportAs, exportImage, exportSvg })
</script>

<template>
  <!-- tabindex=0 so the panel can receive focus and @keydown fires. Focus is only visible in
       the outline the browser draws for keyboard users (:focus-visible), never on mouse click,
       so the panel does not gain a permanent ring when clicked. `@pointerenter` claims focus so
       the panel under the cursor is the one that receives shortcuts — same mental model as
       napari-vizsla and matches how wheel-zoom already scopes to the panel under the mouse. -->
  <div ref="tsvRoot" class="tsv" tabindex="0" @keydown="onKey" @pointerenter="onPointerEnter">
    <div class="tsv-ctrl cc-panel-controls">
      <div class="cc-row">
        <ChipSelect :options="orderOptions" :model-value="order" variant="segmented" aria-label="Lane order"
                    v-tooltip.top="'How the lanes are sorted'"
                    @update:model-value="v => (state.order = v as string)" />
        <span class="tsv-spacer" />
        <!-- the candidate queue is a FILTER, not a second screen (Decision 2) -->
        <button class="cc-btn cc-btn-bare cc-btn-dense" :class="{ 'cc-btn-on': candidatesOnly }"
                v-tooltip.top="'Only tracks flagged as a possible join, split or removal'"
                :disabled="!issues.length"
                @click="state.candidatesOnly = !candidatesOnly">Flagged</button>
        <button class="cc-btn cc-btn-bare cc-btn-dense" :class="{ 'cc-btn-on': gapsOnly }"
                v-tooltip.top="'Tracks missing a detection in some frame'"
                @click="state.gapsOnly = !gapsOnly">Gaps</button>
        <!-- P3: the untracked-detections strip. On by default because its op (`points.add`) has
             no other authoring surface; hiding it is a display-only choice, so the button is
             `cc-btn-bare` and does not refetch. -->
        <button class="cc-btn cc-btn-bare cc-btn-dense" :class="{ 'cc-btn-on': showUntracked }"
                v-tooltip.top="'Show untracked detections along the top — the source for Add'"
                :disabled="!detLane.frames.length"
                @click="state.showUntracked = !showUntracked">Untracked</button>
        <PopFamilySelect :options="familyOptions" v-model="popType" />
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload the tracks'"
                :disabled="loading" @click="load">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>
      <span v-if="summary" class="cc-muted cc-fs-xs"
            v-tooltip.bottom="summaryTip || 'Tracks in this set'">{{ summary }}</span>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="meta && !meta.tracked" class="cc-muted">Not tracked — run Track cells first.</p>
    <p v-else-if="meta && !filtered.length" class="cc-muted">No tracks match this filter.</p>

    <!-- The detector's own thresholds. Collapsed, because the defaults are measured — but a queue
         someone abandons is one whose sensitivity they could not change. -->
    <CollapsibleSection v-if="meta?.tracked" label="Flagging"
                        tip="How readily a track is flagged — re-scans when you apply"
                        storage-key="tsv:sensitivity" :default-open="false">
      <div class="cc-row tsv-knobs">
        <label v-for="f in THRESHOLD_FIELDS" :key="f.key" class="cc-row-group cc-fs-xs"
               v-tooltip.top="f.tip">
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

    <div ref="host" class="tsv-host" @click="onClick" @mousemove="onMove"
         @mouseleave="hover = ''" @wheel="onWheel" />
    <PlotSpinner v-if="loading" label="Loading tracks" />

    <!-- Timeline canvas-affordances only. The correction verbs (Join / Split / Remove / Fix / Add)
         and the queue tail (Undo / Apply) live in the Correction cockpit floating panel — one
         authoring surface for tracks and (Phase 2) labels, so the two surfaces cannot disagree
         about what's Joinable. This row keeps the viewer-brush pair (Draw / Read) and the
         selection utilities (Show / Clear) because they act ON the canvas the user is looking at.
         Hotkeys (Join / Split / Remove / Add / Undo / Apply) still work from here — the keydown
         handler drives the same functions the cockpit does. -->
    <div class="tsv-act cc-row">
      <div class="cc-btn-group">
        <button class="cc-btn cc-btn-bare cc-btn-dense"
                v-tooltip.top="'Select tracks by dragging a rectangle in the viewer'"
                @click="enterSelectMode">
          <i class="pi pi-pencil" /> Draw
        </button>
        <button class="cc-btn cc-btn-bare cc-btn-dense"
                v-tooltip.top="'Select the tracks inside the drawn region'"
                @click="readSelection">Read</button>
        <button class="cc-btn cc-btn-bare cc-btn-dense" :disabled="!selected.size"
                v-tooltip.top="'Turn this segmentation\'s ribbons on in the viewer'"
                @click="showInViewer"><i class="pi pi-eye" /> Show</button>
        <button class="cc-btn cc-btn-bare cc-btn-dense" :disabled="!selected.size"
                v-tooltip.top="`Clear the selection (${KEY_HINT.clearSel})`" @click="setSelected([])">
          <i class="pi pi-times" />
        </button>
      </div>
    </div>

    <div class="tsv-foot cc-row">
      <!-- Persistent state slot: the answer to "did my click do anything". Hover no longer
           overwrites this — the transient hover readout lives in its own slot to the right. -->
      <span class="cc-muted cc-fs-2xs tsv-foot-state">{{ lastQueued || selSummary }}</span>
      <span v-if="hover" class="cc-muted cc-fs-2xs tsv-foot-hover">{{ hover }}</span>
      <span v-if="viewerSummary" class="cc-muted cc-fs-2xs">{{ viewerSummary }}</span>
      <!-- When the viewer highlight is active, show it here — the release path is the Clear
           Selection X in the row above (which also clears the highlight). -->
      <span v-if="viewerHighlightSummary" class="cc-muted cc-fs-2xs">{{ viewerHighlightSummary }}</span>
      <span class="tsv-spacer" />
      <span v-if="note" class="cc-muted cc-fs-2xs">{{ note }}</span>
      <button v-if="note" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" :disabled="atStart"
              v-tooltip.top="'Earlier lanes'" @click="page(-1)"><i class="pi pi-chevron-up" /></button>
      <button v-if="note" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" :disabled="atEnd"
              v-tooltip.top="'Later lanes'" @click="page(1)"><i class="pi pi-chevron-down" /></button>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .tsv-ctrl (.cc-panel-controls) anchors to the plot box */
.tsv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
/* tabindex=0 makes the panel focusable so @keydown can fire; the default browser :focus outline
   would draw a permanent ring the moment the user clicked in — hide it for mouse users and only
   show a keyboard-nav ring. */
.tsv:focus { outline: none; }
.tsv:focus-visible { outline: 2px solid var(--cc-accent); outline-offset: -2px; }
.tsv-ctrl { display: flex; flex-direction: column; gap: 0.3rem; padding: 4px 6px; }
.tsv-spacer { flex: 1; }
/* overflow:hidden so an svg sized to its own floor cannot GROW this box and re-trigger the resize
   observer — see usePlotResize */
.tsv-host { flex: 1; min-height: 0; overflow: hidden; cursor: pointer; }
/* svgDoc emits no style attribute, and an inline <svg> is display:inline — which leaves a
   descender gap under it that the resize observer then reads as a size change */
.tsv-host :deep(svg) { display: block; }
/* fixed height + nowrap: the hover readout changes on every mousemove, and a footer that
   REWRAPS would resize the plot host under it — another ResizeObserver loop, driven by the
   pointer. `min-width: 0` lets the readout ellipsise instead of pushing the row wider. */
.tsv-foot { padding: 2px 6px 4px; align-items: center; gap: 0.25rem;
            flex-wrap: nowrap; height: 22px; overflow: hidden; }
/* both textual readouts ellipsise. State keeps its width (first slot); hover shrinks first when
   the row is tight — a transient readout losing chars is fine, the persistent state must not. */
.tsv-foot-state { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.tsv-foot-hover { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
                  flex-shrink: 2; font-style: italic; }
.tsv-act { padding: 2px 6px; align-items: center; gap: 0.3rem; flex-wrap: wrap; }
.tsv-knobs { padding: 0 6px 4px; flex-wrap: wrap; gap: 0.4rem; }
.tsv-knobs input { width: 4.5rem; }
</style>
