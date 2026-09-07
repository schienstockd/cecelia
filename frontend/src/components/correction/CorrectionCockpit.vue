<!--
  Correction cockpit — one panel to drive all track and label corrections.

  Phase 1 landed the shell + Tracks mode (Show / Add / Join / Split / Remove) and cut the
  timeline's action row so this is the sole authoring surface. Phase 2 (this) adds Labels mode:
  Merge + Remove verbs against whatever the viewer has picked, submitted as one
  `segment.correct_measures` task run. Phase 3 will add Review (sort + pager + Details);
  Phase 4 will add raster brush tools (Draw / Erase / Fill / Pick — the shipment that lets Split
  become a label op instead of a Phase-4-only-brush-op).

  Two modes today, two independent queues. Track ops and label ops don't compose — they address
  different data and run through different composite tasks — so `useTrackOpsQueueStore` and
  `useLabelOpsQueueStore` are peers, and everything on this panel that reads/writes the queue
  switches by the mode.

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
         labelActions, type LabelOp, type LabelAction } from '../../lib/labelCorrection'
import { submitTrackOps } from '../../lib/trackOpsRun'
import { submitLabelOps } from '../../utils/labelOpsRun'
import { showTracksInViewer } from '../../utils/viewer/showTracksInViewer'

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
  { value: 'review', label: 'Review', tip: 'Sort + prev/next + Details — Phase 3', disabled: true },
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

const pending = computed<readonly (TrackOp | LabelOp)[]>(() =>
  mode.value === 'labels' ? labelQueueStore.get(labelKey.value)
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
  if (mode.value !== 'labels' || !projectUid.value || !imageUid.value || !valueName.value) {
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

// ── Summaries ───────────────────────────────────────────────────────────────────

const selectedSummary = computed(() => {
  if (mode.value === 'labels') {
    const n = pickedLabels.value.length
    if (currentT.value === null) return 'Viewer not ready'
    if (!n) return `No label picked (frame ${currentT.value})`
    if (n === 1) return `Label ${pickedLabels.value[0]} @ frame ${currentT.value}`
    if (n <= 4) return `Labels ${pickedLabels.value.join(', ')} @ frame ${currentT.value}`
    return `${n} labels picked @ frame ${currentT.value}`
  }
  const ids = trackScope.value.selectedTracks
  if (!ids.length) return 'No track selected'
  if (ids.length === 1) return `Track ${ids[0]}${trackScope.value.splitFrame !== null ? ` @ frame ${trackScope.value.splitFrame}` : ''}`
  if (ids.length === 2) return `Tracks ${ids.join(' + ')}`
  return `${ids.length} tracks selected`
})

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

function onUndo(): void {
  if (!pendingCount.value) return
  if (mode.value === 'labels') labelQueueStore.set(labelKey.value, undoLabel(labelQueueStore.get(labelKey.value)))
  else                         trackQueueStore.set(trackKey.value, undoTrack(trackQueueStore.get(trackKey.value)))
}
function onClear(): void {
  if (mode.value === 'labels') labelQueueStore.clear(labelKey.value)
  else                         trackQueueStore.clear(trackKey.value)
}
function onApply(): void {
  if (!pendingCount.value) return
  if (mode.value === 'labels') {
    const ok = submitLabelOps({
      projectUid: projectUid.value, setUid: setUid.value, imageUid: imageUid.value,
      valueName: valueName.value, ops: labelQueueStore.get(labelKey.value), source: 'cockpit',
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
  const rows: ToolRow[] = [{
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
const currentKey = computed(() => mode.value === 'labels' ? labelKey.value : trackKey.value)
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

        <div v-else class="cockpit-placeholder cc-fs-sm cc-muted">
          Phase 3: sort by criterion, prev/next pager,<br />
          per-object Details montage.
        </div>
      </div>

      <div class="cockpit-spacer" />

      <div class="cockpit-selection cc-fs-2xs">
        <div class="cc-muted">{{ selectedSummary }}</div>
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
</style>
