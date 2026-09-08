<!--
  Correction cockpit — one panel to drive all track and label corrections.

  Phase 1 landed the shell + Tracks mode (Show / Add / Join / Split / Remove) and cut the
  timeline's action row so this is the sole authoring surface. Phase 2 added Labels mode:
  Merge + Remove verbs against whatever the viewer has picked, submitted as one
  `segment.correct_measures` task run. Phase 3 (this) adds Review — pager over a chosen label
  set, prev/next, fly-to-viewer, inline Remove (Merge is a Labels-mode op that needs 2+ ids).
  Phase 4 lands the Split op (server-side `label.split`) and — as follow-up — raster brush
  tools (Draw / Erase / Fill / Pick).

  Three modes today, two independent queues. Track ops and label ops don't compose — they
  address different data and run through different composite tasks — so `useTrackOpsQueueStore`
  and `useLabelOpsQueueStore` are peers, and everything on this panel that reads/writes the
  queue switches by the mode. Review mode drives the label queue too (its Remove/Split verbs
  are label ops).

  How Tracks mode cooperates with TrackSchemeView. The `useCorrectionCockpitStore` holds the
  shared `(image, valueName)`-keyed state (selected tracks, split-frame, det-selection, computed
  Join/Split/Remove/Add ops); TSV publishes on selection changes, the cockpit reads.

  How Labels mode cooperates with the viewer. The viewer's pick-cell path already writes a
  transient pop `/Pick selection` into the gating map (`_set_pick_selection!` in gating_api.jl),
  so the cockpit just watches that pop's membership. The `t` an op targets is the viewer's
  CURRENT timepoint (Decision 6b: label edits are frame-local), read from
  `viewerStore.viewState.dims.current_step[0]`. Picks made at a different t get queued at the
  current t; the Julia validator refuses out-of-range t and the Python runner reports 0-pixel ops
  when an id isn't present, so a mis-scoped pick is loud rather than silent.

  Scope: `openImageUid` (viewer's currently-focused image) + a valueName picked here or defaulted
  by `useTrackValueNames`. When either is missing the cockpit reads as "open an image", every
  button disables — nothing does a partial write.
-->
<script setup lang="ts">
import { computed, ref, watch, onBeforeUnmount } from 'vue'
import FloatingPanel from '../FloatingPanel.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useTrackOpsQueueStore, trackOpsKey } from '../../stores/trackOpsQueue'
import { useLabelOpsQueueStore, labelOpsKey } from '../../stores/labelOpsQueue'
import { useCorrectionCockpitStore } from '../../stores/correctionCockpit'
import { useViewerStore } from '../../stores/viewer'
import { useTrackValueNames } from '../../composables/useTrackValueNames'
import { undoLast as undoTrack, opDescription as trackOpDescription,
         type TrackOp } from '../../lib/trackCorrection'
import { undoLast as undoLabel, opDescription as labelOpDescription,
         labelActions, buildRemoveOp, buildCentroidSplitOp,
         type LabelOp, type LabelAction } from '../../lib/labelCorrection'
import { submitTrackOps } from '../../lib/trackOpsRun'
import { submitLabelOps } from '../../utils/labelOpsRun'
import { showTracksInViewer } from '../../utils/viewer/showTracksInViewer'
import { armViewerSelectMode, readTrackSelection } from '../../utils/viewer/trackSelectionFromViewer'
import { selectedTracks as resolvedTracks } from '../../lib/trackCorrection'
import { buildFocusViewState } from '../../utils/viewer/focusOnCell'
import { overlaysUrl, type OverlayPayload } from '../../utils/viewerOverlays'
import {
  sortLabels, nextIndex, prevIndex, clampIndex, pageSummary,
  type ReviewLabel, type ReviewSort,
} from '../../utils/reviewPager'
import {
  selectionKind, labelSelectionSummary, trackSelectionSummary,
  labelChipOptions, trackChipOptions, parseLabelChipValues,
} from '../../utils/correctionSelections'

const emit = defineEmits<{ close: [] }>()

const project = useProjectStore()
const settings = useSettingsStore()
const trackQueueStore = useTrackOpsQueueStore()
const labelQueueStore = useLabelOpsQueueStore()
const cockpit = useCorrectionCockpitStore()
const viewerStore = useViewerStore()

const projectUid = computed(() => project.loadedProjectUid ?? '')
const imageUid = computed(() => project.openImageUid ?? '')
const setUid = computed(() => imageUid.value ? (project.setUidOfImage(imageUid.value) ?? null) : null)

// Picker state — persisted; useTrackValueNames.resolved() picks the fallback (active segmentation
// if tracked, else first tracked, else first-of-anything). Labels mode wants a broader picker
// (any labels set, not just tracked) — handled below in `valueNameOptions`.
const wantedValueName = computed<string>({
  get: () => settings.correctionCockpitValueName,
  set: v => { settings.correctionCockpitValueName = v },
})
const { valueNames, trackedNames, resolved } = useTrackValueNames(projectUid, imageUid, wantedValueName)
const valueName = computed(() => resolved())

const mode = computed<'tracks' | 'labels' | 'review'>({
  get: () => settings.correctionCockpitMode,
  set: v => { settings.correctionCockpitMode = v },
})

const MODES: ChipOption[] = [
  { value: 'tracks', label: 'Tracks', tip: 'Join, split, remove, points' },
  { value: 'labels', label: 'Labels', tip: 'Merge, remove — click cells in the viewer to pick' },
  { value: 'review', label: 'Review', tip: 'Step through picked labels one at a time' },
]

const valueNameOptions = computed<ChipOption[]>(() => {
  // Tracks mode: tracked names first (the ones you can author against), others afterwards but
  // labelled "Not tracked" so a user doesn't wonder where their segmentation went.
  // Labels mode: every label set is fair game — merge/remove edits the labels store itself, and a
  // tracked one is still an ordinary labels store underneath.
  const tracked = new Set(trackedNames.value)
  if (mode.value === 'tracks') {
    const trackedList = valueNames.value.filter(n => tracked.has(n))
    const otherList = valueNames.value.filter(n => !tracked.has(n))
    return [
      ...trackedList.map(n => ({ value: n, label: n })),
      ...otherList.map(n => ({ value: n, label: n, tip: 'Not tracked' })),
    ]
  }
  return valueNames.value.map(n => ({ value: n, label: n }))
})

// ── Keys + pending queue — switch by mode ─────────────────────────────────────────

const trackKey = computed(() => trackOpsKey(projectUid.value, imageUid.value, valueName.value))
const labelKey = computed(() => labelOpsKey(projectUid.value, imageUid.value, valueName.value))
const trackScope = computed(() => cockpit.state(trackKey.value))

// Labels + Review both drive the label queue — Review's Remove is a label op.
const pending = computed<readonly (TrackOp | LabelOp)[]>(() =>
  (mode.value === 'labels' || mode.value === 'review')
    ? labelQueueStore.get(labelKey.value)
    : trackQueueStore.get(trackKey.value)
)
const pendingCount = computed(() => pending.value.length)

// ── Labels mode: viewer timepoint + picked labels ────────────────────────────────

/** Current viewer t (from viewState). null when the viewer hasn't published a state yet. */
const currentT = computed<number | null>(() => {
  const step = viewerStore.viewState?.dims?.current_step
  const t = step && step.length > 0 ? Number(step[0]) : NaN
  return Number.isFinite(t) ? Math.floor(t) : null
})

/** Labels the viewer's pick selection currently holds — polled from the gating membership API
 *  on every pop-map change (see the pick-selection lifecycle in api/src/gating_api.jl). Empty
 *  when nothing is picked or when the fetch fails. */
const pickedLabels = ref<number[]>([])
let pickReq = 0

async function reloadPicked(): Promise<void> {
  // Both Labels AND Review page over `/Pick selection` — Review's pager is a rank over the same
  // picks Labels' verbs would act on. Only clear the local mirror when we've left both modes.
  const needsPicks = mode.value === 'labels' || mode.value === 'review'
  if (!needsPicks || !projectUid.value || !imageUid.value || !valueName.value) {
    pickedLabels.value = []
    return
  }
  const seq = ++pickReq
  try {
    const q = `projectUid=${encodeURIComponent(projectUid.value)}` +
              `&imageUid=${encodeURIComponent(imageUid.value)}` +
              `&valueName=${encodeURIComponent(valueName.value)}` +
              `&pops=${encodeURIComponent('/Pick selection')}`
    const r = await fetch(`/api/gating/membership?${q}`)
    if (!r.ok) { if (seq === pickReq) pickedLabels.value = []; return }
    const d = await r.json() as { membership?: Record<string, number[]> }
    const labs = d.membership?.['/Pick selection'] ?? []
    if (seq === pickReq) pickedLabels.value = labs.map(Number).filter(Number.isFinite)
  } catch { if (seq === pickReq) pickedLabels.value = [] }
}

// The gating store broadcasts `gating:popmap` on every mutation (incl. viewer picks). Watch a cheap
// cross-window signal — pickCellAt writes localStorage `cc.pickSelectionTick`, or the popmap
// change bumps `viewerStore.viewState` transitively. Cheapest that's reactive: watch mode + key.
watch([mode, labelKey], () => { void reloadPicked() }, { immediate: true })
// Poll on localStorage tick so a pick from the popup viewer window refreshes here too.
function onStorage(e: StorageEvent) {
  if (e.key === 'cc.pickSelectionTick' || e.key === 'cc.gatingPopmapTick') void reloadPicked()
}
if (typeof window !== 'undefined') window.addEventListener('storage', onStorage)
onBeforeUnmount(() => { if (typeof window !== 'undefined') window.removeEventListener('storage', onStorage) })

const labelActionsAtT = computed<LabelAction[]>(() =>
  currentT.value === null ? labelActions(0, []) : labelActions(currentT.value, pickedLabels.value)
)

// ── Review mode: pager over the picked labels, fly-to on focus change ───────────
//
// Sources the label ids from the same `/Pick selection` pop Labels mode uses (`pickedLabels`), so
// what the user picks in the viewer IS the review scope — no separate pop picker needed for MVP.
// Coordinates come from `/api/viewer/overlays` fetched once per (image, valueName); the payload is
// small (see `viewerOverlays.ts` — largest measured is 2 MB). A shrinking pick set clamps the
// cursor rather than jumping past the end.

const reviewSort = ref<ReviewSort>('id-asc')
const reviewIndex = ref<number>(0)
const overlays = ref<OverlayPayload | null>(null)
let overlayReq = 0

async function reloadOverlays(): Promise<void> {
  if (!projectUid.value || !imageUid.value || !valueName.value) {
    overlays.value = null
    return
  }
  const seq = ++overlayReq
  try {
    const url = overlaysUrl({ projectUid: projectUid.value, imageUid: imageUid.value,
                              valueName: valueName.value })
    const r = await fetch(url)
    if (!r.ok) { if (seq === overlayReq) overlays.value = null; return }
    const j = await r.json() as OverlayPayload
    if (seq === overlayReq) overlays.value = j
  } catch { if (seq === overlayReq) overlays.value = null }
}
// Fetch once when Review mode is opened (or the labels change). Cheap enough to not need a cache
// across mode switches — a stale overlay after a correction Apply would show old coords, which is
// worse than a re-fetch.
watch([mode, labelKey], () => { if (mode.value === 'review') void reloadOverlays() },
      { immediate: true })

/** label id → first-t centroid, built from the overlays payload. */
const labelCoords = computed<Map<number, ReviewLabel>>(() => {
  const map = new Map<number, ReviewLabel>()
  const o = overlays.value
  if (!o?.cells?.label?.length) return map
  const L = o.cells.label, T = o.cells.t ?? [], X = o.cells.x ?? [], Y = o.cells.y ?? [],
        Z = o.cells.z ?? []
  for (let i = 0; i < L.length; i++) {
    const lab = Number(L[i])
    if (!Number.isFinite(lab) || lab <= 0 || map.has(lab)) continue
    map.set(lab, {
      label: lab,
      t: Number.isFinite(T[i]) ? Math.floor(Number(T[i])) : 0,
      x: Number(X[i]),
      y: Number(Y[i]),
      z: Z[i] !== undefined ? Number(Z[i]) : undefined,
    })
  }
  return map
})

/** The sorted list Review pages over — the picked labels, resolved to coords, in the chosen order. */
const reviewList = computed<ReviewLabel[]>(() => {
  const coords = labelCoords.value
  const list: ReviewLabel[] = []
  for (const id of pickedLabels.value) {
    const c = coords.get(id)
    if (c) list.push(c)
    else list.push({ label: id, t: currentT.value ?? 0, x: NaN, y: NaN })
  }
  return sortLabels(list, reviewSort.value)
})

// Clamp the cursor whenever the list shrinks — a Remove that pops the current label off the end
// would otherwise leave the pager pointing past `total`.
watch(() => reviewList.value.length, n => { reviewIndex.value = clampIndex(reviewIndex.value, n) })

const reviewFocused = computed<ReviewLabel | null>(() => {
  const list = reviewList.value
  const i = reviewIndex.value
  return (i >= 0 && i < list.length) ? list[i] : null
})

const reviewSummary = computed(() => pageSummary(reviewIndex.value, reviewList.value.length))

/**
 * Fly the viewer to the focused label — pans (never zooms in), sets t to the label's first
 * occurrence. Only fires when we have a real coord (a label with no overlay entry can't be flown
 * to; a warning surfaces via the summary text). Same delivery mechanism as `showTracksInViewer` +
 * TrackSchemeView's Show: `viewerStore.setPendingViewState` → popup viewer's watcher applies.
 */
function focusReviewLabel(target: ReviewLabel | null): void {
  if (!target || !Number.isFinite(target.x) || !Number.isFinite(target.y)) return
  const current = viewerStore.viewState
  if (!current) return
  const next = buildFocusViewState(current, {
    t: target.t, cx: target.x, cy: target.y, cz: target.z,
  })
  if (next) viewerStore.setPendingViewState(next)
}

// Auto-fly on any focus change — the whole point of the pager is that a Prev/Next step immediately
// lights up the label in the viewer. When the cockpit mounts before the viewer publishes its
// first viewState, the initial focus fires with `current` null and silently drops. Watch both:
// the focused label AND the viewer viewState — whichever arrives second retries the fly.
watch(reviewFocused, target => { if (mode.value === 'review') focusReviewLabel(target) })
watch(() => viewerStore.viewState, (vs, prev) => {
  // Only fire the retry when viewState transitions from null → present (the mount race). Later
  // viewState changes (t-scrub, camera edits) must NOT re-fly the focused label — the user has
  // to be able to pan away while Review is open without being yanked back.
  if (vs && !prev && mode.value === 'review') focusReviewLabel(reviewFocused.value)
}, { flush: 'post' })

function reviewNext(): void { reviewIndex.value = nextIndex(reviewIndex.value, reviewList.value.length) }
function reviewPrev(): void { reviewIndex.value = prevIndex(reviewIndex.value, reviewList.value.length) }

/** Queue a Remove for the focused label — one-cell op, no `into` needed. */
function reviewRemoveFocused(): void {
  const f = reviewFocused.value
  if (!f) return
  const op = buildRemoveOp(f.t, [f.label])
  if (op) queueLabelOp(op)
}

/** Queue a centroid-anchored Split — horizontal or vertical cut through the focused label. The
 *  runner clips the cut to the label's mask, so an overshoot is harmless. A cut that doesn't
 *  divide the label surfaces as a warn line + zero-pixel op (see `_apply_split_inplace`). */
function reviewSplitFocused(axis: 'horizontal' | 'vertical'): void {
  const f = reviewFocused.value
  if (!f || !Number.isFinite(f.x) || !Number.isFinite(f.y)) return
  const op = buildCentroidSplitOp(f.t, f.label, f.x, f.y, axis)
  if (op) queueLabelOp(op)
}

// ── Summaries + chip strips ─────────────────────────────────────────────────────

// Which selection this mode reads — labels (Labels + Review both page over `/Pick selection`),
// tracks (Tracks). Was a bug pre-vis-PR: Review fell through to the tracks branch and reported
// "N tracks selected" while the user was picking labels (screenshot 2026-09-08).
const stripKind = computed(() => selectionKind(mode.value))

const selectedSummary = computed(() =>
  stripKind.value === 'labels'
    ? labelSelectionSummary(pickedLabels.value, currentT.value)
    : trackSelectionSummary(trackScope.value.selectedTracks, trackScope.value.splitFrame))

const chipOptions = computed(() =>
  stripKind.value === 'labels'
    ? labelChipOptions(pickedLabels.value, reviewFocused.value?.label ?? null)
    : trackChipOptions(trackScope.value.selectedTracks))

const chipModel = computed<string[]>(() =>
  stripKind.value === 'labels'
    ? pickedLabels.value.map(String)
    : trackScope.value.selectedTracks.slice())

/** Click on a chip in the strip — ChipSelect emits the updated `string[]`; POST to the server so
 *  every downstream consumer (plots, viewer overlay, the other cockpit mode) reflects the change,
 *  then explicitly reload from `/api/gating/membership` so the local mirror is the SERVER's truth,
 *  not our best-effort echo. Same idempotent-reconcile pattern the popmap tick uses for other
 *  cross-window changes — doing it explicitly here removes "the fetch didn't broadcast" as a
 *  possible failure mode. */
async function onStripUpdate(next: string[] | string): Promise<void> {
  const arr = Array.isArray(next) ? next : [next]
  if (stripKind.value === 'labels') {
    if (!projectUid.value || !imageUid.value || !valueName.value) return
    const labs = parseLabelChipValues(arr)
    // Optimistic paint so the click feels instant; the reload below is authoritative.
    pickedLabels.value = labs
    try {
      await fetch('/api/viewer/pick-set', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectUid: projectUid.value, imageUid: imageUid.value,
          valueName: valueName.value, popType: 'flow', labels: labs,
        }),
      })
    } catch { /* fall through to reload — the reconciler is the source of truth */ }
    await reloadPicked()
  } else {
    writeSelectedTracks(arr)
  }
}

/**
 * Write a new tracks-selection through the same convention `TrackSchemeView.setSelected` uses:
 * cockpit store + viewer highlight + split cursor stay in lock-step. Bypassing this via
 * `cockpit.setSelectedTracks` alone was the "I can't get rid of the selected tracks" bug —
 * clearing the store leaves `viewerStore.trackHighlight` (persisted to localStorage) still
 * highlighting the last set, so the viewer keeps drawing them even after every UI count is 0.
 */
function writeSelectedTracks(ids: string[]): void {
  if (!trackKey.value) return
  cockpit.setSelectedTracks(trackKey.value, ids)
  if (!ids.length && viewerStore.trackHighlight) viewerStore.setTrackHighlight(null)
  // Split cursor only makes sense on a lone selection; any other cardinality invalidates it.
  if (ids.length !== 1 && trackScope.value.splitFrame !== null) {
    cockpit.setSplitFrame(trackKey.value, null)
  }
}

/** Clear the whole selection for the current mode. Labels + Review call `/api/viewer/pick-clear`
 *  then reconcile; Tracks writes an empty list to the client store. Explicit-clear affordance
 *  was the "selections hang around for eternity" complaint (screenshot 2026-09-08). */
async function clearSelection(): Promise<void> {
  if (stripKind.value === 'labels') {
    if (!projectUid.value || !imageUid.value || !valueName.value) return
    pickedLabels.value = []
    try {
      await fetch('/api/viewer/pick-clear', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectUid: projectUid.value, imageUid: imageUid.value,
          valueName: valueName.value, popType: 'flow',
        }),
      })
    } catch { /* fall through to reload */ }
    await reloadPicked()
  } else {
    // Always clear the store AND the viewer highlight — even when the store is already empty.
    // The persisted `cc.viewer.trackHighlight` outlives a page reload, so a user can inherit a
    // stale highlight (Dominik's "I still can't get rid of the selected tracks in fXgbTl",
    // 2026-09-08) that no in-cockpit count reflects. The ✕ button must be the one that always
    // works, so it addresses BOTH signals unconditionally.
    writeSelectedTracks([])
    if (viewerStore.trackHighlight) viewerStore.setTrackHighlight(null)
  }
}

const canClear = computed(() =>
  // Tracks mode: enabled when EITHER the store carries picks OR the viewer highlight is set —
  // the second is the "stale highlight from before" case; the button must be reachable to clear
  // it even when the local counts already read zero.
  stripKind.value === 'labels'
    ? pickedLabels.value.length > 0
    : (trackScope.value.selectedTracks.length > 0 || viewerStore.trackHighlight !== null))

const detSummary = computed(() => {
  if (mode.value !== 'tracks') return ''
  const d = trackScope.value.detSelection
  if (!d) return ''
  return `Untracked frame ${d.frame} · ${d.labels.length} label${d.labels.length === 1 ? '' : 's'}`
})

// ── Op-queue helpers ─────────────────────────────────────────────────────────────

function queueTrackOp(op: TrackOp | null): void {
  if (!op || !trackKey.value) return
  trackQueueStore.set(trackKey.value, [...trackQueueStore.get(trackKey.value), op])
}
function queueLabelOp(op: LabelOp | null): void {
  if (!op || !labelKey.value) return
  labelQueueStore.set(labelKey.value, [...labelQueueStore.get(labelKey.value), op])
}

async function showInViewer(): Promise<void> {
  const ids = trackScope.value.selectedTracks.map(Number).filter(Number.isFinite)
  if (!ids.length) return
  await showTracksInViewer(projectUid.value, imageUid.value, valueName.value, ids, 'cockpit')
}

/**
 * Draw / Read — the viewer→tracks bridge. The COCKPIT is the sole authoring surface: Draw arms
 * the viewer rectangle mode, Read resolves the pick to tracks and writes them to the shared
 * cockpit store, Show reads back from the same store. The timeline (TrackSchemeView) reads AND
 * writes the same store too, so a Cockpit Read immediately lights the TSV lanes and a TSV lane
 * click immediately updates the cockpit summary. That symmetry is why Draw / Read are NOT
 * duplicated on the timeline — a second copy running against a divergent selection state was
 * the exact "Cockpit Read is a no-op" / "Show highlights different tracks" bug reported by
 * Dominik on 2026-09-07.
 */
async function drawInViewer(): Promise<void> {
  if (!imageUid.value) return
  armViewerSelectMode('cockpit')
}
async function readFromViewer(): Promise<void> {
  if (!projectUid.value || !imageUid.value || !valueName.value || !trackKey.value) return
  const sel = await readTrackSelection({
    projectUid: projectUid.value, imageUid: imageUid.value,
    valueName: valueName.value, source: 'cockpit',
  })
  if (!sel) return
  const ids = resolvedTracks(sel).map(String)
  // We deliberately write EVEN AN EMPTY read to the store — a Read that resolved to zero tracks
  // (all-untracked, or nothing picked) should clear a stale selection rather than leave the
  // previous ids hanging under a summary that no longer matches what the viewer shows. Route via
  // `writeSelectedTracks` so a Read-to-empty also drops the viewer highlight (see the helper).
  writeSelectedTracks(ids)
}

const isLabelMode = computed(() => mode.value === 'labels' || mode.value === 'review')

function onUndo(): void {
  if (!pendingCount.value) return
  if (isLabelMode.value) labelQueueStore.set(labelKey.value, undoLabel(labelQueueStore.get(labelKey.value)))
  else                   trackQueueStore.set(trackKey.value, undoTrack(trackQueueStore.get(trackKey.value)))
}
function onClear(): void {
  if (isLabelMode.value) labelQueueStore.clear(labelKey.value)
  else                   trackQueueStore.clear(trackKey.value)
}
function onApply(): void {
  if (!pendingCount.value) return
  if (isLabelMode.value) {
    const ok = submitLabelOps({
      projectUid: projectUid.value, setUid: setUid.value, imageUid: imageUid.value,
      valueName: valueName.value, ops: labelQueueStore.get(labelKey.value),
      source: mode.value === 'review' ? 'review' : 'cockpit',
    })
    if (ok) labelQueueStore.clear(labelKey.value)
  } else {
    const ok = submitTrackOps({
      projectUid: projectUid.value, setUid: setUid.value, imageUid: imageUid.value,
      valueName: valueName.value, ops: trackQueueStore.get(trackKey.value), source: 'cockpit',
    })
    if (ok) trackQueueStore.clear(trackKey.value)
  }
}

function shortId(uid: string): string { return uid.length > 8 ? uid.slice(0, 6) + '…' : uid }

// ── Tools per mode ───────────────────────────────────────────────────────────────

type ToolRow = {
  key: string; label: string; icon?: string
  blocked: string; tooltip: string; danger?: boolean; run: () => void
}

const tracksTools = computed<ToolRow[]>(() => {
  // Draw + Read + Show are the viewer-brush trio — kept first, in flow order (arm → read →
  // highlight). The mutating verbs (Add / Join / Split / Remove) come after; they operate on the
  // selection this trio populates.
  const rows: ToolRow[] = [{
    key: 'draw', label: 'Draw', icon: 'pi-pencil',
    blocked: imageUid.value ? '' : 'Open an image in the viewer first',
    tooltip: 'Select tracks by dragging a rectangle in the viewer',
    run: drawInViewer,
  }, {
    key: 'read', label: 'Read',
    blocked: (projectUid.value && imageUid.value && valueName.value) ? ''
             : 'Waiting for the viewer to publish an image',
    tooltip: 'Resolve the drawn selection to tracks',
    run: readFromViewer,
  }, {
    key: 'show', label: 'Show', icon: 'pi-eye',
    blocked: trackScope.value.selectedTracks.length ? '' : 'Pick at least one track first',
    tooltip: 'Show the selected tracks in the viewer',
    run: showInViewer,
  }]
  const add = trackScope.value.addAction
  rows.push({
    key: 'add', label: add.label, icon: 'pi-plus',
    blocked: add.blocked || '',
    tooltip: add.blocked || (add.op ? trackOpDescription(add.op) : 'Add'),
    run: () => queueTrackOp(add.op),
  })
  for (const a of trackScope.value.actions) {
    rows.push({
      key: a.key, label: a.label,
      icon: a.key === 'remove' ? 'pi-trash' : undefined,
      blocked: a.blocked || '',
      tooltip: a.blocked || (a.op ? trackOpDescription(a.op) : a.label),
      danger: a.key === 'remove',
      run: () => queueTrackOp(a.op),
    })
  }
  return rows
})

const labelsTools = computed<ToolRow[]>(() =>
  labelActionsAtT.value.map(a => ({
    key: a.key, label: a.label,
    icon: a.key === 'remove' ? 'pi-trash' : undefined,   // Merge: no glyph, per CORRECTION_PLAN.md §440
    blocked: a.blocked || '',
    tooltip: a.blocked || (a.op ? labelOpDescription(a.op) : a.label),
    danger: a.key === 'remove',
    run: () => queueLabelOp(a.op),
  }))
)

// ── Which key drives disable states for the mode ─────────────────────────────────
const currentKey = computed(() => isLabelMode.value ? labelKey.value : trackKey.value)
</script>

<template>
  <FloatingPanel
    title="Correction"
    icon="pi-wrench"
    storage-key="correction-cockpit"
    accent="var(--cc-accent)"
    :default-x="620" :default-y="120" :default-w="340" :default-h="440"
    @close="emit('close')"
  >
    <div class="cockpit">
      <div class="cockpit-mode">
        <ChipSelect variant="pill" :options="MODES" :model-value="mode"
                    @update:model-value="v => mode = (v as 'tracks' | 'labels' | 'review')" />
      </div>

      <div class="cockpit-scope cc-row cc-fs-2xs cc-muted">
        <template v-if="!imageUid">Open an image in the viewer to begin</template>
        <template v-else>
          <span v-tooltip.top="imageUid">Image {{ shortId(imageUid) }}</span>
          <span class="cockpit-scope-sep">·</span>
          <ChipSelect v-if="valueNameOptions.length" variant="pill" :options="valueNameOptions"
                      :model-value="valueName"
                      @update:model-value="v => wantedValueName = String(v ?? '')" />
          <span v-else class="cc-muted">no value name available</span>
        </template>
      </div>

      <div class="cockpit-tools">
        <template v-if="mode === 'tracks'">
          <div v-if="!currentKey" class="cockpit-placeholder cc-fs-sm cc-muted">
            Pick a tracked segmentation to enable the verbs.
          </div>
          <div v-else class="cc-btn-group cockpit-toolbar">
            <button v-for="t in tracksTools" :key="t.key"
                    class="cc-btn cc-btn-dense"
                    :class="[t.blocked ? 'cc-btn-bare' : (t.danger ? 'cc-btn-danger-ghost' : 'cc-btn-primary')]"
                    :disabled="!!t.blocked"
                    v-tooltip.top="t.tooltip"
                    @click="t.run()">
              <i v-if="t.icon" :class="['pi', t.icon]" />
              <span>{{ t.label }}</span>
            </button>
          </div>
        </template>

        <template v-else-if="mode === 'labels'">
          <div v-if="!currentKey" class="cockpit-placeholder cc-fs-sm cc-muted">
            Pick a labels set to enable the verbs.
          </div>
          <div v-else class="cc-btn-group cockpit-toolbar">
            <button v-for="t in labelsTools" :key="t.key"
                    class="cc-btn cc-btn-dense"
                    :class="[t.blocked ? 'cc-btn-bare' : (t.danger ? 'cc-btn-danger-ghost' : 'cc-btn-primary')]"
                    :disabled="!!t.blocked"
                    v-tooltip.top="t.tooltip"
                    @click="t.run()">
              <i v-if="t.icon" :class="['pi', t.icon]" />
              <span>{{ t.label }}</span>
            </button>
          </div>
        </template>

        <template v-else>
          <div v-if="!currentKey" class="cockpit-placeholder cc-fs-sm cc-muted">
            Pick a labels set to enable Review.
          </div>
          <div v-else-if="!reviewList.length" class="cockpit-placeholder cc-fs-sm cc-muted">
            Pick cells in the viewer to build a review list.
          </div>
          <div v-else class="cockpit-review">
            <div class="cc-btn-group cockpit-toolbar">
              <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense"
                      v-tooltip.top="'Previous label'" @click="reviewPrev">
                <i class="pi pi-chevron-left" />
              </button>
              <span class="cockpit-review-counter cc-fs-xs">{{ reviewSummary }}</span>
              <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense"
                      v-tooltip.top="'Next label'" @click="reviewNext">
                <i class="pi pi-chevron-right" />
              </button>
              <span class="cockpit-spring" />
              <button class="cc-btn cc-btn-bare cc-btn-dense"
                      :class="{ 'cc-btn-primary': reviewSort === 'id-asc' }"
                      v-tooltip.top="'Sort by label id, ascending'"
                      @click="reviewSort = 'id-asc'">Id ↑</button>
              <button class="cc-btn cc-btn-bare cc-btn-dense"
                      :class="{ 'cc-btn-primary': reviewSort === 'id-desc' }"
                      v-tooltip.top="'Sort by label id, descending'"
                      @click="reviewSort = 'id-desc'">Id ↓</button>
            </div>
            <div v-if="reviewFocused" class="cockpit-review-card cc-fs-2xs cc-muted">
              <div>Label {{ reviewFocused.label }} @ frame {{ reviewFocused.t }}</div>
              <div v-if="Number.isFinite(reviewFocused.x)">
                Centroid ({{ Math.round(reviewFocused.x) }}, {{ Math.round(reviewFocused.y) }}<template
                  v-if="reviewFocused.z !== undefined">, z {{ Math.round(reviewFocused.z) }}</template>)
              </div>
              <div v-else class="cc-muted-warn">No centroid — this label has no overlay row (re-measure?)</div>
            </div>
            <div class="cc-btn-group cockpit-review-verbs">
              <button class="cc-btn cc-btn-bare cc-btn-dense"
                      v-tooltip.top="'Re-centre viewer on this label'"
                      :disabled="!reviewFocused || !Number.isFinite(reviewFocused?.x ?? NaN)"
                      @click="focusReviewLabel(reviewFocused)">
                <i class="pi pi-map-marker" /><span>Show</span>
              </button>
              <button class="cc-btn cc-btn-danger-ghost cc-btn-dense"
                      v-tooltip.top="'Queue a Remove for the focused label'"
                      :disabled="!reviewFocused"
                      @click="reviewRemoveFocused">
                <i class="pi pi-trash" /><span>Remove</span>
              </button>
              <button class="cc-btn cc-btn-bare cc-btn-dense"
                      v-tooltip.top="'Split horizontally through the centroid — runner clips to the label'"
                      :disabled="!reviewFocused || !Number.isFinite(reviewFocused?.x ?? NaN)"
                      @click="reviewSplitFocused('horizontal')">
                <span>Split ↔</span>
              </button>
              <button class="cc-btn cc-btn-bare cc-btn-dense"
                      v-tooltip.top="'Split vertically through the centroid — runner clips to the label'"
                      :disabled="!reviewFocused || !Number.isFinite(reviewFocused?.x ?? NaN)"
                      @click="reviewSplitFocused('vertical')">
                <span>Split ↕</span>
              </button>
            </div>
          </div>
        </template>
      </div>

      <div class="cockpit-spacer" />

      <div class="cockpit-selection cc-fs-2xs">
        <!-- Chip strip: the "what is picked" visualiser. One chip per picked track (Tracks) or
             picked label (Labels + Review). Clicking a chip drops that id — same DE-select
             mechanic ChipSelect exposes everywhere else. Empty = no strip; the summary line
             below still names the state. Review's focused label gets the accent so the pager's
             cursor is visible in the strip too. -->
        <div v-if="chipOptions.length" class="cockpit-chip-strip"
             v-tooltip.top="stripKind === 'labels'
               ? 'Picked labels — click one to drop it'
               : 'Selected tracks — click one to drop it'">
          <ChipSelect variant="pill" multiple :options="chipOptions"
                      :model-value="chipModel"
                      @update:model-value="onStripUpdate" />
        </div>
        <div class="cockpit-selection-row">
          <span class="cc-muted">{{ selectedSummary }}</span>
          <span class="cockpit-spring" />
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense"
                  :disabled="!canClear"
                  v-tooltip.top="stripKind === 'labels'
                    ? 'Clear picked labels'
                    : 'Clear selected tracks'"
                  @click="clearSelection">
            <i class="pi pi-times" />
          </button>
        </div>
        <div v-if="detSummary" class="cc-muted">{{ detSummary }}</div>
      </div>

      <div class="cockpit-queue cc-row">
        <span class="cc-fs-2xs cc-muted">
          {{ pendingCount ? `${pendingCount} queued` : 'Nothing queued' }}
        </span>
        <span class="cockpit-spring" />
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" :disabled="!pendingCount"
                v-tooltip.top="'Undo the last queued edit'" @click="onUndo">
          <i class="pi pi-undo" />
        </button>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" :disabled="!pendingCount"
                v-tooltip.top="'Clear all queued edits'" @click="onClear">
          <i class="pi pi-times" />
        </button>
        <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="!pendingCount"
                :class="{ 'cc-btn-bare': !pendingCount }"
                v-tooltip.top="'Apply all queued edits as one correction'" @click="onApply">
          {{ pendingCount ? `Apply ${pendingCount}` : 'Apply' }}
        </button>
      </div>
    </div>
  </FloatingPanel>
</template>

<style scoped>
/* fp-body carries no padding by convention (LabLogPanel, ViewerPanel, CorrectionPlanPanel all pad
   their own root). Same here. */
.cockpit { display: flex; flex-direction: column; height: 100%; padding: 8px 10px; gap: 8px;
           min-height: 0; }
.cockpit-mode { display: flex; }
/* .cockpit-scope wraps with `cc-row` (canonical row primitive); this rule adds only chrome. */
.cockpit-scope-sep { opacity: 0.5; }
.cockpit-tools { padding: 4px 0; }
/* Compact toolbar: buttons size to content, wrap only if the panel is truly narrow. */
.cockpit-toolbar { flex-wrap: wrap; gap: 0.3rem; }
.cockpit-placeholder { line-height: 1.5; }
.cockpit-spacer { flex: 1; min-height: 0; }
.cockpit-selection { padding: 4px 0; border-top: 1px solid var(--cc-border);
                     border-bottom: 1px solid var(--cc-border); display: flex; flex-direction: column;
                     gap: 2px; }
.cockpit-queue { align-items: center; gap: 0.3rem; padding-top: 2px; }
.cockpit-spring { flex: 1; }
.cockpit-review { display: flex; flex-direction: column; gap: 6px; }
.cockpit-review-counter { min-width: 5.5rem; text-align: center; }
.cockpit-review-card { padding: 4px 6px; border-left: 2px solid var(--cc-accent);
                       display: flex; flex-direction: column; gap: 2px; }
.cockpit-review-verbs { gap: 0.3rem; flex-wrap: wrap; }
.cockpit-chip-strip { padding: 2px 0; max-height: 5.5rem; overflow-y: auto; }
.cockpit-selection-row { display: flex; align-items: center; gap: 0.3rem; }
</style>
